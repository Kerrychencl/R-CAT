# ==============================================================================
# CAT 模擬研究--畫圖（重構模組化版）
# 功能：讀取模擬輸出的 .xlsx 彙整表，繪製 5 項指標 x 7 個能力水準折線圖
# 相容性：
#   - 自動相容新版（含 theta 首欄）與舊版（不含 theta 欄）的 .xlsx 檔案
#   - 支援「全準則（MFI / MFII / MLWI）」與「二準則（MFI / MFII）」一鍵切換
#   - 切換情境（unif400 / norm400 / unif200 / norm200）只需改第 16 行 file_prefix
# ==============================================================================

library(readxl)

# ==============================================================================
# 1. 繪圖參數設定（需要調整時只需改這裡）
# ==============================================================================
file_prefix  <- "unif400"                    # 情境前綴：unif400 / norm400 / unif200 / norm200
input_dir    <- "."                          # Excel 檔案所在資料夾
output_dir   <- "."                          # PNG 圖檔輸出資料夾
TL           <- 60L                          # 測驗題數長度
theta_levels <- c(-3, -2, -1, 0, 1, 2, 3)    # 7 個能力水準
include_mlwi <- TRUE                         # TRUE: 畫三準則；FALSE: 只畫 MFI 與 MFII
save_png     <- TRUE                         # TRUE: 輸出 PNG 檔；FALSE: 直接在 RStudio 預覽

# 5 項指標的繪圖規格（檔名後綴、輸出前綴、Y 軸標籤、Y 軸範圍）
metric_specs <- list(
  list(xlsx_suffix = "bias",           out_stem = "bias",      ylab = "bias",            ylim = c(-1.6, 1.6)),
  list(xlsx_suffix = "var",            out_stem = "var",       ylab = "variance",        ylim = c(0, 0.4)),
  list(xlsx_suffix = "MSE",            out_stem = "MSE",       ylab = "MSE",             ylim = c(0, 2)),
  list(xlsx_suffix = "info_mean_true", out_stem = "info_true", ylab = "mean info(true)", ylim = c(0, 0.6)),
  list(xlsx_suffix = "info_mean_est",  out_stem = "info_est",  ylab = "mean info(est)",  ylim = c(0, 0.6))
)

# ==============================================================================
# 2. 讀取與繪圖輔助函式
# ==============================================================================

# 讀取彙整表 Excel 並轉成純數值矩陣（自動略過首欄 theta，並避免 tibble 繪圖相容問題）
read_metric_matrix <- function(criterion, xlsx_suffix, file_prefix, input_dir) {
  fname <- sprintf("%s_%s_%s.xlsx", file_prefix, criterion, xlsx_suffix)
  fpath <- if (input_dir == "." || input_dir == "") fname else file.path(input_dir, fname)
  df <- read_excel(fpath)

  # 若第一欄名稱為 theta（新版輸出），自動去掉該欄只保留第 5 題～第 TL 題數值
  if (tolower(names(df)[1]) == "theta") {
    df <- df[, -1, drop = FALSE]
  }
  mat <- as.matrix(df)
  storage.mode(mat) <- "double"
  mat
}

# 繪製單一指標在 7 個能力水準下的比較折線圖
plot_metric <- function(spec, mfi_mat, mfii_mat, mlwi_mat = NULL,
                        theta_levels, TL, output_dir = ".", save_png = TRUE) {
  x_vals <- 5:TL
  has_mlwi <- !is.null(mlwi_mat)

  legend_labels <- if (has_mlwi) c("MFI", "MFII", "MLWI") else c("MFI", "MFII")
  legend_cols   <- if (has_mlwi) c("blue", "green", "red") else c("blue", "green")

  for (i in seq_along(theta_levels)) {
    th_val <- theta_levels[i]

    if (save_png) {
      png_name <- sprintf("%s_ability_%d.png", spec$out_stem, th_val)
      png_path <- if (output_dir == "." || output_dir == "") png_name else file.path(output_dir, png_name)
      png(filename = png_path, width = 800, height = 600)
    }

    plot(x_vals, mfi_mat[i, ], type = "l", col = "blue", lwd = 3,
         xlab = "item length", ylab = spec$ylab,
         main = paste0("ability = ", th_val),
         xlim = c(1, TL), ylim = spec$ylim)
    lines(x_vals, mfii_mat[i, ], col = "green", lwd = 3)
    if (has_mlwi) {
      lines(x_vals, mlwi_mat[i, ], col = "red", lwd = 3)
    }
    legend("topright", legend = legend_labels, col = legend_cols, lwd = 3, bty = "n")

    if (save_png) {
      dev.off()
    }
  }
}

# ==============================================================================
# 3. 批次讀取所有指標並輸出圖檔
# ==============================================================================
if (save_png && output_dir != "." && output_dir != "" && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (spec in metric_specs) {
  mfi_mat  <- read_metric_matrix("MFI",  spec$xlsx_suffix, file_prefix, input_dir)
  mfii_mat <- read_metric_matrix("MFII", spec$xlsx_suffix, file_prefix, input_dir)
  mlwi_mat <- if (include_mlwi) {
    read_metric_matrix("MLWI", spec$xlsx_suffix, file_prefix, input_dir)
  } else {
    NULL
  }

  plot_metric(
    spec         = spec,
    mfi_mat      = mfi_mat,
    mfii_mat     = mfii_mat,
    mlwi_mat     = mlwi_mat,
    theta_levels = theta_levels,
    TL           = TL,
    output_dir   = output_dir,
    save_png     = save_png
  )
}

cat(sprintf("繪圖完成！模式：%s | 情境：%s | 輸出位置：%s\n",
            if (include_mlwi) "全準則 (MFI/MFII/MLWI)" else "二準則 (MFI/MFII)",
            file_prefix, output_dir))
