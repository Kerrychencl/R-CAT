# ==============================================================================
# CAT 模擬研究--平行運算（完整重構版：模擬 + Excel 輸出 + 繪圖）
# 目標：復現與擴充 Some New Item Selection Criteria for Adaptive Testing
#       (Veerkamp & Berger, 1997) - MFI / MFII / MLWI
# 結構：
#   1. 參數設定（情境切換只需改此區，所有輸出檔名自動連動）
#   2. 核心向量化 IRT / CAT 模擬引擎與統計彙整函式
#   3. 題庫與作答反應生成（向量化）與輸出
#   4. 分塊平行運算執行 MFI / MFII / MLWI 並輸出 Excel
#   5. 繪製並輸出 5 項指標 x 7 個能力水準之 PNG 圖檔
# ==============================================================================

library(parallel)
library(doSNOW)
library(foreach)
library(writexl)

# ==============================================================================
# 1. 模擬參數與輸出設定（切換情境只需改這裡）
# ==============================================================================
set.seed(20250101)        # 固定隨機種子以確保可重現

R <- 1000L                                              # 每個能力點重複次數
theta_levels <- c(-3, -2, -1, 0, 1, 2, 3)               # 7 個真實能力水準
theta_true <- rep(theta_levels, each = R)               # 受試者總數 np = 7000
theta_level_idx <- rep(seq_along(theta_levels), each = R)
np <- length(theta_true)

pool_choice  <- 1L        # 1: 400 題庫；其他數字: 200 題庫
distribution <- 1L        # 1: 均勻分布；其他數字: 常態分布
TL           <- 60L       # 測驗題數長度

# 自動組合檔名前綴（例如 unif400 / norm400 / unif200 / norm200）
ni <- if (pool_choice == 1L) 400L else 200L
dist_label <- if (distribution == 1L) "unif" else "norm"
file_prefix <- sprintf("%s%d", dist_label, ni)

# 輸出選項（若只需彙整表與圖檔、想省下大型原始表寫入 Excel 的時間，可改為 FALSE）
save_raw_xlsx <- TRUE
output_dir    <- "."      # 預設輸出在目前工作目錄，與原版一致

# 數值積分與 MFII 參數（與 catR 預設對齊：lower = -4, upper = 4, nqp = 33, D = 1）
Q_nodes       <- 33L
theta_lower   <- -4
theta_upper   <- 4
mfii_z        <- 1.96
mfii_n_points <- 20L
mfii_bounds   <- c(-4, 4)

# ==============================================================================
# 2. 核心向量化 IRT / CAT 輔助函式
# ==============================================================================

# 單一純量 theta 對多題 (a, b, c) 的 3PL Fisher 訊息量（D = 1）
info_3pl_vec <- function(th, a, b, c_par) {
  P_star <- plogis(a * (th - b))
  P <- c_par + (1 - c_par) * P_star
  (a^2 * (1 - c_par) * P_star^2 * (1 - P_star)) / P
}

# 時間格式化工具
format_elapsed <- function(secs) {
  days <- floor(secs / 86400)
  hours <- floor((secs %% 86400) / 3600)
  minutes <- floor((secs %% 3600) / 60)
  rem_secs <- secs %% 60
  parts <- c(
    if (days > 0) sprintf("%d 天", days),
    if (hours > 0) sprintf("%02d 小時", hours),
    if (minutes > 0) sprintf("%02d 分鐘", minutes),
    sprintf("%.2f 秒", rem_secs)
  )
  paste(parts, collapse = " ")
}

# 組合輸出路徑
out_path <- function(filename) {
  if (output_dir == "." || output_dir == "") filename else file.path(output_dir, filename)
}

# 將 7 x 56 彙整矩陣轉為帶 theta 欄位的 data.frame（避免 write_xlsx 遺失 row.names）
to_summary_df <- function(mat, theta_levels) {
  cbind(
    data.frame(theta = theta_levels),
    as.data.frame(mat, check.names = FALSE)
  )
}

# 預計算共用矩陣（整個模擬僅執行一次）
precompute_bank <- function(item_pool, theta_levels, Q = 33L,
                            lower = -4, upper = 4) {
  a <- item_pool[, 1]
  b <- item_pool[, 2]
  c_par <- item_pool[, 3]
  one_minus_c <- 1 - c_par
  a2_1mc <- a^2 * one_minus_c

  theta_q <- seq(lower, upper, length.out = Q)
  delta_q <- (upper - lower) / (Q - 1L)
  w_trap  <- c(0.5, rep(1, Q - 2L), 0.5) * delta_q
  prior_w <- dnorm(theta_q) * w_trap

  diff_q   <- -outer(b, theta_q, "-")
  P_star_q <- plogis(diff_q * a)
  P_mat    <- c_par + one_minus_c * P_star_q
  logP_mat <- log(pmax(P_mat, 1e-300))
  logQ_mat <- log(pmax(1 - P_mat, 1e-300))
  I_mat    <- (a2_1mc * P_star_q^2 * (1 - P_star_q)) / P_mat

  diff_true  <- -outer(b, theta_levels, "-")
  P_star_t   <- plogis(diff_true * a)
  P_true     <- c_par + one_minus_c * P_star_t
  I_true_mat <- (a2_1mc * P_star_t^2 * (1 - P_star_t)) / P_true

  list(
    a = a, b = b, c_par = c_par,
    one_minus_c = one_minus_c, a2_1mc = a2_1mc,
    theta_q = theta_q, w_trap = w_trap, prior_w = prior_w,
    logP_mat = logP_mat, logQ_mat = logQ_mat,
    I_mat = I_mat, I_true_mat = I_true_mat
  )
}

# 單一分塊（Chunk）的 CAT 模擬引擎（支援 MFI / MFII / MLWI）
simulate_chunk <- function(resp_chunk, level_idx_chunk, criterion, TL, bank,
                           mfii_z = 1.96, mfii_n_points = 20L,
                           mfii_bounds = c(-4, 4)) {
  n_sub <- nrow(resp_chunk)
  ni <- length(bank$a)
  Q <- length(bank$theta_q)

  a <- bank$a
  b <- bank$b
  c_par <- bank$c_par
  one_minus_c <- bank$one_minus_c
  a2_1mc <- bank$a2_1mc
  theta_q <- bank$theta_q
  w_trap <- bank$w_trap
  prior_w <- bank$prior_w
  logP_mat <- bank$logP_mat
  logQ_mat <- bank$logQ_mat
  I_mat <- bank$I_mat
  I_true_mat <- bank$I_true_mat

  w_mfii <- c(0.5, rep(1, mfii_n_points - 2L), 0.5)

  ever_used_mat <- matrix(0L, nrow = n_sub, ncol = TL)
  theta_est_mat <- matrix(0, nrow = n_sub, ncol = TL)
  est_info_mat  <- matrix(0, nrow = n_sub, ncol = TL)
  true_info_mat <- matrix(0, nrow = n_sub, ncol = TL)

  for (idx in seq_len(n_sub)) {
    resp_row <- resp_chunk[idx, ]
    lvl_i <- level_idx_chunk[idx]

    used <- logical(ni)
    ever_used <- integer(TL)
    theta_est <- numeric(TL)
    est_info <- numeric(TL)
    true_info <- numeric(TL)

    logL <- numeric(Q)
    theta_in <- 0
    true_info_sum <- 0

    for (j in seq_len(TL)) {
      if (criterion == "MFI") {
        scores <- info_3pl_vec(theta_in, a, b, c_par)
      } else if (criterion == "MFII") {
        if (j == 1L) {
          theta_L <- max(mfii_bounds[1], theta_in - mfii_z)
          theta_R <- min(mfii_bounds[2], theta_in + mfii_z)
        } else {
          adm <- ever_used[seq_len(j - 1L)]
          test_info <- sum(info_3pl_vec(theta_in, a[adm], b[adm], c_par[adm]))
          se_theta <- 1 / sqrt(max(test_info, 1e-12))
          theta_L <- max(mfii_bounds[1], theta_in - mfii_z * se_theta)
          theta_R <- min(mfii_bounds[2], theta_in + mfii_z * se_theta)
        }
        grid_20 <- seq(theta_L, theta_R, length.out = mfii_n_points)
        diff_20 <- -outer(b, grid_20, "-")
        P_star_20 <- plogis(diff_20 * a)
        P_20 <- c_par + one_minus_c * P_star_20
        I_20 <- (a2_1mc * P_star_20^2 * (1 - P_star_20)) / P_20
        scores <- as.vector(I_20 %*% w_mfii)
      } else if (criterion == "MLWI") {
        L_vec <- exp(logL - max(logL))
        scores <- as.vector(I_mat %*% (L_vec * w_trap))
      } else {
        stop("不支援的選題準則：", criterion)
      }

      scores[used] <- -Inf
      item_id <- which.max(scores)

      used[item_id] <- TRUE
      ever_used[j] <- item_id

      if (resp_row[item_id] == 1L) {
        logL <- logL + logP_mat[item_id, ]
      } else {
        logL <- logL + logQ_mat[item_id, ]
      }

      post <- exp(logL - max(logL)) * prior_w
      theta_in <- sum(theta_q * post) / sum(post)
      theta_est[j] <- theta_in

      adm_now <- ever_used[seq_len(j)]
      est_info[j] <- mean(info_3pl_vec(theta_in, a[adm_now], b[adm_now], c_par[adm_now]))
      true_info_sum <- true_info_sum + I_true_mat[item_id, lvl_i]
      true_info[j] <- true_info_sum / j
    }

    ever_used_mat[idx, ] <- ever_used
    theta_est_mat[idx, ] <- theta_est
    est_info_mat[idx, ]  <- est_info
    true_info_mat[idx, ] <- true_info
  }

  list(
    ever_used = ever_used_mat,
    theta_est = theta_est_mat,
    est_info  = est_info_mat,
    true_info = true_info_mat
  )
}

# 統一執行單一準則：平行模擬 -> 彙整指標 -> 輸出 Excel
run_and_export_criterion <- function(criterion, response, theta_level_idx, theta_levels,
                                     R, TL, bank, cl, n_chunks, file_prefix,
                                     save_raw_xlsx = TRUE,
                                     mfii_z = 1.96, mfii_n_points = 20L,
                                     mfii_bounds = c(-4, 4)) {
  np <- nrow(response)
  chunk_ids <- split(seq_len(np), cut(seq_len(np), breaks = n_chunks, labels = FALSE))

  cat(sprintf("\n>>> 開始執行 %s 模擬（np = %d, TL = %d, chunks = %d）...\n",
              criterion, np, TL, n_chunks))
  t0 <- proc.time()

  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)
  on.exit(close(pb), add = TRUE)
  opts <- list(progress = function(n) setTxtProgressBar(pb, n))

  chunk_results <- foreach(
    k = seq_len(n_chunks),
    .options.snow = opts,
    .export = c("simulate_chunk", "info_3pl_vec")
  ) %dopar% {
    idx <- chunk_ids[[k]]
    simulate_chunk(
      resp_chunk      = response[idx, , drop = FALSE],
      level_idx_chunk = theta_level_idx[idx],
      criterion       = criterion,
      TL              = TL,
      bank            = bank,
      mfii_z          = mfii_z,
      mfii_n_points   = mfii_n_points,
      mfii_bounds     = mfii_bounds
    )
  }

  ever_used <- do.call(rbind, lapply(chunk_results, `[[`, "ever_used"))
  theta_est <- do.call(rbind, lapply(chunk_results, `[[`, "theta_est"))
  est_info  <- do.call(rbind, lapply(chunk_results, `[[`, "est_info"))
  true_info <- do.call(rbind, lapply(chunk_results, `[[`, "true_info"))

  # 計算第 5 題到第 TL 題的彙整指標（7 x (TL - 4)）
  item_cols <- 5:TL
  n_lvl <- length(theta_levels)
  n_col <- length(item_cols)
  row_names <- as.character(theta_levels)
  col_names <- paste0("第", item_cols, "題")

  mk_mat <- function() {
    matrix(0, nrow = n_lvl, ncol = n_col, dimnames = list(row_names, col_names))
  }
  df_bias <- mk_mat()
  df_var  <- mk_mat()
  df_MSE  <- mk_mat()
  df_info_est  <- mk_mat()
  df_info_true <- mk_mat()

  for (lvl in seq_len(n_lvl)) {
    rows <- ((lvl - 1L) * R + 1L):(lvl * R)
    th_mat <- theta_est[rows, item_cols, drop = FALSE]
    th_true_val <- theta_levels[lvl]

    df_bias[lvl, ]      <- th_true_val - colMeans(th_mat)
    df_var[lvl, ]       <- apply(th_mat, 2L, var)
    df_MSE[lvl, ]       <- colMeans((th_true_val - th_mat)^2)
    df_info_est[lvl, ]  <- colMeans(est_info[rows, item_cols, drop = FALSE])
    df_info_true[lvl, ] <- colMeans(true_info[rows, item_cols, drop = FALSE])
  }

  sim_elapsed <- (proc.time() - t0)[["elapsed"]]

  # 輸出 Excel（保留與原程式一致的檔案命名格式）
  t_io <- proc.time()
  tag <- sprintf("%s_%s", file_prefix, criterion)

  if (save_raw_xlsx) {
    write_xlsx(as.data.frame(ever_used), out_path(sprintf("%s_ever_used.xlsx", tag)))
    write_xlsx(as.data.frame(theta_est), out_path(sprintf("%s_theta_table.xlsx", tag)))
    write_xlsx(as.data.frame(est_info),  out_path(sprintf("%s_est_info_table.xlsx", tag)))
    write_xlsx(as.data.frame(true_info), out_path(sprintf("%s_true_info_table.xlsx", tag)))
  }

  write_xlsx(to_summary_df(df_bias, theta_levels),      out_path(sprintf("%s_bias.xlsx", tag)))
  write_xlsx(to_summary_df(df_var, theta_levels),       out_path(sprintf("%s_var.xlsx", tag)))
  write_xlsx(to_summary_df(df_MSE, theta_levels),       out_path(sprintf("%s_MSE.xlsx", tag)))
  write_xlsx(to_summary_df(df_info_est, theta_levels),  out_path(sprintf("%s_info_mean_est.xlsx", tag)))
  write_xlsx(to_summary_df(df_info_true, theta_levels), out_path(sprintf("%s_info_mean_true.xlsx", tag)))

  io_elapsed <- (proc.time() - t_io)[["elapsed"]]
  cat(sprintf("\n%s 完成！模擬耗時：%s | Excel 輸出耗時：%s\n",
              criterion, format_elapsed(sim_elapsed), format_elapsed(io_elapsed)))

  list(
    criterion   = criterion,
    sim_elapsed = sim_elapsed,
    io_elapsed  = io_elapsed,
    summary = list(
      bias      = df_bias,
      var       = df_var,
      MSE       = df_MSE,
      info_est  = df_info_est,
      info_true = df_info_true
    )
  )
}

# 繪圖輸出函式（藍: MFI, 綠: MFII, 紅: MLWI，檔名與原版完全一致）
export_metric_plots <- function(mfi_mat, mfii_mat, mlwi_mat,
                                theta_levels, TL,
                                file_stem, ylab_text, ylim_range) {
  x_vals <- 5:TL
  for (i in seq_along(theta_levels)) {
    th_val <- theta_levels[i]
    png(filename = out_path(sprintf("%s_ability_%d.png", file_stem, th_val)),
        width = 800, height = 600)
    plot(x_vals, as.numeric(mfi_mat[i, ]), type = "l", col = "blue", lwd = 3,
         xlab = "item length", ylab = ylab_text,
         main = paste0("ability = ", th_val),
         xlim = c(1, TL), ylim = ylim_range)
    lines(x_vals, as.numeric(mfii_mat[i, ]), col = "green", lwd = 3)
    lines(x_vals, as.numeric(mlwi_mat[i, ]), col = "red", lwd = 3)
    legend("topright", legend = c("MFI", "MFII", "MLWI"),
           col = c("blue", "green", "red"), lwd = 3, bty = "n")
    dev.off()
  }
}

# ==============================================================================
# 3. 題庫與作答反應資料生成（完全向量化）與輸出
# ==============================================================================
if (output_dir != "." && output_dir != "" && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

t_total_start <- proc.time()
t_gen <- proc.time()

item_pool <- matrix(0, nrow = ni, ncol = 4L)
item_pool[, 1] <- runif(ni, min = 0.5, max = 2.0)    # 鑑別度 a
item_pool[, 2] <- if (distribution == 1L) runif(ni, min = -3.0, max = 3.0) else rnorm(ni)  # 難度 b
item_pool[, 3] <- runif(ni, min = 0.15, max = 0.30)  # 猜測度 c
item_pool[, 4] <- 1                                  # 上漸近線 d = 1

P_star_resp <- plogis(outer(theta_true, item_pool[, 2], "-") * rep(item_pool[, 1], each = np))
P_resp <- rep(item_pool[, 3], each = np) + rep(1 - item_pool[, 3], each = np) * P_star_resp
response <- (P_resp >= matrix(runif(np * ni), nrow = np, ncol = ni)) * 1L

bank <- precompute_bank(item_pool, theta_levels, Q = Q_nodes,
                        lower = theta_lower, upper = theta_upper)

write_xlsx(as.data.frame(item_pool), out_path(sprintf("%s_item_pool.xlsx", file_prefix)))
if (save_raw_xlsx) {
  write_xlsx(as.data.frame(response), out_path(sprintf("%s_response.xlsx", file_prefix)))
}

gen_elapsed <- (proc.time() - t_gen)[["elapsed"]]
cat(sprintf("資料生成、預計算與題庫輸出耗時（情境：%s）：%s\n",
            file_prefix, format_elapsed(gen_elapsed)))

# ==============================================================================
# 4. 啟動平行運算叢集並執行 MFI / MFII / MLWI
# ==============================================================================
num_cores <- max(1L, detectCores() - 4L)
n_chunks  <- num_cores * 4L

cl <- makeCluster(num_cores)
registerDoSNOW(cl)

cat(sprintf("平行叢集啟動：使用 %d 核心（分塊數：%d）\n", num_cores, n_chunks))

tryCatch({
  res_MFI  <- run_and_export_criterion("MFI",  response, theta_level_idx, theta_levels,
                                       R, TL, bank, cl, n_chunks, file_prefix,
                                       save_raw_xlsx = save_raw_xlsx)
  res_MFII <- run_and_export_criterion("MFII", response, theta_level_idx, theta_levels,
                                       R, TL, bank, cl, n_chunks, file_prefix,
                                       save_raw_xlsx = save_raw_xlsx,
                                       mfii_z = mfii_z, mfii_n_points = mfii_n_points,
                                       mfii_bounds = mfii_bounds)
  res_MLWI <- run_and_export_criterion("MLWI", response, theta_level_idx, theta_levels,
                                       R, TL, bank, cl, n_chunks, file_prefix,
                                       save_raw_xlsx = save_raw_xlsx)
}, finally = {
  stopCluster(cl)
})

# ==============================================================================
# 5. 繪製並輸出 5 項指標 x 7 個能力水準 PNG 圖檔（共 35 張）
# ==============================================================================
t_plot <- proc.time()

export_metric_plots(res_MFI$summary$bias,      res_MFII$summary$bias,      res_MLWI$summary$bias,
                    theta_levels, TL, "bias",      "bias",            c(-1.6, 1.6))
export_metric_plots(res_MFI$summary$var,       res_MFII$summary$var,       res_MLWI$summary$var,
                    theta_levels, TL, "var",       "variance",        c(0, 0.4))
export_metric_plots(res_MFI$summary$MSE,       res_MFII$summary$MSE,       res_MLWI$summary$MSE,
                    theta_levels, TL, "MSE",       "MSE",             c(0, 2))
export_metric_plots(res_MFI$summary$info_true, res_MFII$summary$info_true, res_MLWI$summary$info_true,
                    theta_levels, TL, "info_true", "mean info(true)", c(0, 0.5))
export_metric_plots(res_MFI$summary$info_est,  res_MFII$summary$info_est,  res_MLWI$summary$info_est,
                    theta_levels, TL, "info_est",  "mean info(est)",  c(0, 0.5))

plot_elapsed <- (proc.time() - t_plot)[["elapsed"]]
total_elapsed <- (proc.time() - t_total_start)[["elapsed"]]

# ==============================================================================
# 6. 輸出總耗時彙整表
# ==============================================================================
cat("\n==================== 執行耗時總結 ====================\n")
timing_table <- data.frame(
  Stage = c("Data Gen + Export", "MFI (Sim)", "MFI (Excel)",
            "MFII (Sim)", "MFII (Excel)", "MLWI (Sim)", "MLWI (Excel)",
            "Plotting (35 PNGs)", "Total"),
  Seconds = round(c(gen_elapsed,
                    res_MFI$sim_elapsed,  res_MFI$io_elapsed,
                    res_MFII$sim_elapsed, res_MFII$io_elapsed,
                    res_MLWI$sim_elapsed, res_MLWI$io_elapsed,
                    plot_elapsed, total_elapsed), 2),
  Formatted = c(
    format_elapsed(gen_elapsed),
    format_elapsed(res_MFI$sim_elapsed),  format_elapsed(res_MFI$io_elapsed),
    format_elapsed(res_MFII$sim_elapsed), format_elapsed(res_MFII$io_elapsed),
    format_elapsed(res_MLWI$sim_elapsed), format_elapsed(res_MLWI$io_elapsed),
    format_elapsed(plot_elapsed), format_elapsed(total_elapsed)
  )
)
print(timing_table, row.names = FALSE)
