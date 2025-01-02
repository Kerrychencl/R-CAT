# 啟用必要的套件
library(pacman)
p_load(parallel, catR, progress, doSNOW)

###---------------------數據生成---------------------
## 受試者能力生成
R <- 3  # 將能力值重複R遍
theta.true <- rep(c(-3, -2, -1, 0, 1, 2, 3), each = R)  # theta.true生成

##參數設定 (受測者數量、題庫和測驗長度)
# 受測者數量(np)：1400
np <- length(theta.true)

# 題庫大小的選擇(ni)(1：400，其他數字：200)
pool <- 0
if (pool == 1) ni = 400 else ni = 200  # 試題數量(選其中一個題庫)：400 and 200

# 測驗訂定的題數長度(TL)：60
TL <- 10

## 題庫生成的選擇(1：均勻分布，其他數字：常態分布)
distribution <- 1
### 題庫生成
item_pool <- matrix(nrow = ni, ncol = 4)  #創建空矩陣，用於裝取題目參數
if (distribution == 1) {
  ## 試題參數生成(均勻分布)
  item_pool[, 1] <- runif(ni, min = 0.5, max = 2.0)  # 鑑別度
  item_pool[, 2] <- runif(ni, min = -3.0, max = 3.0)  # 難度
  item_pool[, 3] <- runif(ni, min = 0.15, max = 0.30)  # 猜測度
  item_pool[, 4] <- 1  # inattention value 
} else {
  ## 試題參數生成(常態分布)
  item_pool[, 1] <- runif(ni, min = 0.5, max = 2.0)  # 鑑別度
  item_pool[, 2] <- rnorm(ni)  # 難度
  item_pool[, 1] <- runif(ni, min = 0.15, max = 0.30)  # 猜測度
  item_pool[, 4] <- 1  # inattention value 
}


#### 作答反應資料產生
response <- matrix(nrow = np, ncol = ni)  # 創建作答反應矩陣，用於裝取作答反應
for (i in 1:np){
  for (j in 1:ni){
    # 3PL的答對率公式
    p = item_pool[j, 3] + (1 - item_pool[j, 3] ) * exp(item_pool[j, 1] * (theta.true[i] - item_pool[j, 2])) / (
      1 + exp(item_pool[j, 1] * (theta.true[i] - item_pool[j, 2])))
    # 隨機生成一個連續的機率資料 (最小值為0，最大值為1)
    r = runif(1)
    # 如果答對率高於隨機機率，就代表達對，反之則為錯
    if (p >= r) response[i, j] = 1 else response[i, j] = 0
  }
}

##---------------------平行運算前置設定---------------------
# 確定可用核心數量(平行計算前置設定)
num_cores <- detectCores() - 1  # 預留一個核心給系統
cl <- makeCluster(num_cores)
registerDoSNOW(cl)

## 設置進度條
pb <- txtProgressBar(max = np, style = 3)
progress <- function(n) {
  setTxtProgressBar(pb, n)
  cat(sprintf("\r任務進度 %d/%d |", n, np))  # 顯示當前進度
}
opts <- list(progress = progress)
###---------------------MFICAT(平行計算)---------------------------
### CAT相關矩陣宣告
MFI_true_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄真實能力值下，計算的訊息量
MFI_est_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄當前能力值下，計算的訊息量
MFI_theta_table <-  matrix(NA, nrow = np, ncol = TL) # 矩陣，紀錄當前的能力估計值
MFI_ever_used <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄已經選取過的題目

# 紀錄測試時間(使用系統的時間)
start_time_MFI <- Sys.time()  # 紀錄測試時間(使用系統的時間)

# 用foreach換掉for，代表
CAT_data <- foreach(i = 1:np, .combine = rbind, .packages = "catR", .options.snow = opts) %dopar% {
  # 因為np個人是同時進行的，所以原本矩陣nrow是np的，都變為TL長度的向量即可
  ever.used <- numeric(TL)  # 向量，用於紀錄已經選取過的題目
  theta.est_eap <- numeric(TL)  # 向量，用於紀錄當前的能力值
  est_info <- numeric(TL)  # 向量，用於紀錄當前能力值下的訊息量
  true_info <- numeric(TL)  # 向量，用於紀錄真實能力值下的訊息量
  eap.para <- matrix(nrow = TL, ncol = 4)  # 矩陣，用以記錄作答過的題目參數
  admin.x <- c()  # 建立一個空向量 (裝取被選到的題目的題號)
  theta_est_in <- c(0)  # MFI要使用的當前能力值
  
  # 每一個人作答情況的循環
  for (j in 1:TL) {
    ## 題目選取邏輯
    MFI_criterion <- nextItem(item_pool, theta = theta_est_in, criterion = "MFI", out = ever.used, x = admin.x)
    
    ## 更新作答紀錄(紀錄作答過的題號、該題的作答反應 and 作答過的題目的試題參數)
    ever.used[j] <- MFI_criterion$item  # 紀錄作答過的題號
    admin.x[j] <- response[i, MFI_criterion$item]  # 當前題目的作答反應
    eap.para[j, ] <- MFI_criterion$par  # 當前題目的試題參數
    
    ## 估計能力估計值 
    theta.est_eap[j] <- eapEst(eap.para[1:j, ], admin.x[1:j])  # 循環外查看用的能力估計值
    theta_est_in <- eapEst(eap.para[1:j, ], admin.x[1:j])  # 循環內用的能力估計直
    
    ## 紀錄當前能力值下的訊息量 and 當前真實能力值下的訊息量
    est_info[j] <- mean(Ii(theta.est_eap, eap.para[1:j, ])$Ii)
    true_info[j] <- mean(Ii(theta.true[i], eap.para[1:j, ])$Ii)
  }
  # 將數據轉為 data.frame 格式返回
  data.frame(
    id = i,
    MFI_ever_used = ever.used,
    theta_est = theta.est_eap,
    est_info = est_info,
    true_info = true_info
  )
} 

# 將CAT_data轉為4個matrix
for (i in 1:np) {
  MFI_ever_used[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 2]
  MFI_theta_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 3]
  MFI_est_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 4]
  MFI_true_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 5]
}

# 紀錄作答結束的時間
end_time_MFI <- Sys.time()

# 計算模擬時間
exec_timeCAT_MFI <- end_time_MFI - start_time_MFI

# 將時間差轉換為天、時、分、秒
total_seconds <- as.numeric(exec_timeCAT_MFI, units = "secs")
days <- floor(total_seconds / (24 * 3600))
hours <- floor((total_seconds %% (24 * 3600)) / 3600)
minutes <- floor((total_seconds %% 3600) / 60)
seconds <- round(total_seconds %% 60, 2)  # 可選保留小數

# 動態構建報告字符串
formatted_time_MFI <- trimws(paste(  # trimws()去除空格
  if (days > 0) sprintf("%d 天", days) else "",
  if (hours > 0) sprintf("%02d 小時", hours) else "",
  if (minutes > 0) sprintf("%02d 分鐘", minutes) else "",
  if (seconds > 0) sprintf("%.2f 秒", seconds) else ""
))

# 輸出模擬時間
cat("MFI模擬時間：", formatted_time_MFI, "\n")


###----------------------MFI_data.frame生成結果與輸出----------------------
## 數據框的建立
# 數據框名字建立
row_name <- c("-3", "-2", "-1", "0", "1", "2", "3")  # 列的名字(即，左側)
col_name <- c()  # 行的名字(即，上側)
for (i in 1 : (TL - 4)) {col_name[i] <- paste0("第", i + 4, "題")}
# bias數據框
MFI_df_bias <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFI_df_bias) <- row_name
colnames(MFI_df_bias) <- col_name
# variance數據框
MFI_df_var <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFI_df_var) <- row_name
colnames(MFI_df_var) <- col_name
# MSE數據框
MFI_df_MSE <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFI_df_MSE) <- row_name
colnames(MFI_df_MSE) <- col_name
# est_info數據框
MFI_df_info_mean_est <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFI_df_info_mean_est) <- row_name
colnames(MFI_df_info_mean_est) <- col_name
# ture_info數據框
MFI_df_info_mean_true <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFI_df_info_mean_true) <- row_name
colnames(MFI_df_info_mean_true) <- col_name



## 計算bias, variance, MSE, info_mean_est and info_mean_true
for (i in 1:7) {
  for (j in 1:(TL - 4)) {
    MFI_df_bias[i, j] <- theta.true[i * R] - mean(MFI_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # bias
    MFI_df_var[i, j] <- var(MFI_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # variance
    MFI_df_MSE[i, j] <- mean((theta.true[i * R] - MFI_theta_table[((i - 1) * R + 1):(i * R), j + 4]) ^ 2)  # MSE
    MFI_df_info_mean_est[i, j] <- mean(MFI_est_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_est
    MFI_df_info_mean_true[i, j] <- mean(MFI_true_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_true
  }
}


## MFI資料輸出
write_xlsx(MFI_df_bias, "unif400_MFI_bias.xlsx")
write_xlsx(MFI_df_var, "unif400_MFI_var.xlsx")
write_xlsx(MFI_df_MSE, "unif400_MFI_MSE.xlsx")
write_xlsx(MFI_df_info_mean_est, "unif400_MFI_info_mean_est.xlsx")
write_xlsx(MFI_df_info_mean_true, "unif400_MFI_info_mean_true.xlsx")


###---------------------MFIICAT(平行計算)---------------------------
### CAT相關矩陣宣告
MFII_true_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄真實能力值下，計算的訊息量
MFII_est_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄當前能力值下，計算的訊息量
MFII_theta_table <-  matrix(NA, nrow = np, ncol = TL) # 矩陣，紀錄當前的能力估計值
MFII_ever_used <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄已經選取過的題目

## 紀錄測試時間(使用系統的時間)
start_time_MFII <- Sys.time()  # 紀錄測試時間(使用系統的時間)

# 平行運算
CAT_data <- foreach(i = 1:np, .combine = rbind, .packages = "catR", .options.snow = opts) %dopar% {
  # 因為np個人是同時進行的，所以原本矩陣nrow是np的，都變為TL長度的向量即可
  ever.used <- numeric(TL)  # 向量，用於紀錄已經選取過的題目
  theta.est_eap <- numeric(TL)  # 向量，用於紀錄當前的能力值
  est_info <- numeric(TL)  # 向量，用於紀錄當前能力值下的訊息量
  true_info <- numeric(TL)  # 向量，用於紀錄真實能力值下的訊息量
  eap.para <- matrix(nrow = TL, ncol = 4)  # 矩陣，用以記錄作答過的題目參數
  admin.x <- c()  # 建立一個空向量 (裝取被選到的題目的題號)
  ever.used_in <- numeric(ni)  # 查看過去的以經選過的題目
  theta_est_in <- c(0)  # MFI需要的當前能力值，裝取用來尋找下一題的能力值
  
  # 每一個人作答情況的循環
  for (j in 1:TL) {
    ## 題目選取邏輯
    # 計算出對應theta所有試題訊息量的interval(SE)
    SE <- 1.96 * (sqrt(1 / Ii(theta_est_in, item_pool)$Ii))
    # 拉出interval內，20個點的能力值
    FI_Interval_20point <- matrix(nrow = ni, ncol = 20)
    for (k in 1:ni){
      FI_Interval_20point[k, ] <- seq(theta_est_in - SE[k] ,theta_est_in + SE[k] , length.out = 20)
    }
    # 計算每一題20個能力值對應的訊息量
    info_matrix <- matrix(nrow = ni, ncol = 20)  # 矩陣，用來裝取20個能力值對應的訊息量
    for (item in 1:ni){
      for (point in 1:20){
        info_matrix[item, point] <- Ii(FI_Interval_20point[item, point], item_pool[item, ])$Ii
      }
    }
    # 計算20個訊息量的總和
    infosum_20point <- c()  # 向量，用以裝取每一題20個訊息量的總和
    for (item in 1:ni) {
      infosum_20point[item] <- sum(info_matrix[item, ])
    }
    # 找出未用過的題目中，訊息量總和最大的位置
    infosum_20point[ever.used_in != 0] <- 0 # 屏蔽先前已經使用的試題
    admin.item <- which.max(infosum_20point) # 找出訊息量最大值(返回位置的數字)
    
    ## 更新作答紀錄(紀錄作答過的題號、該題的作答反應 and 作答過的題目的試題參數)
    ever.used_in[admin.item] <- 1 # 標記第n題已經施測(用於109行)
    ever.used[j] <- admin.item  # 裝取已經選過的題目的題號(用於查看整體)
    admin.x[j] <- response[i, admin.item]  # 當前題目的作答反應
    eap.para[j, ] <- item_pool[admin.item, ]  # 當前題目的試題參數
    
    ## 估計能力估計值 
    theta.est_eap[j] <- eapEst(eap.para[1:j, ], admin.x[1:j])  # 能力估計值，查看整體用的
    theta_est_in <- eapEst(eap.para[1:j, ], admin.x[1:j])  # 循環內用的能力估計直
    
    ## 紀錄當前能力值下的訊息量 and 當前真實能力值下的訊息量
    est_info[j] <- mean(Ii(theta.est_eap, eap.para[1:j, ])$Ii)
    true_info[j] <- mean(Ii(theta.true[i], eap.para[1:j, ])$Ii)
  }
  # 將數據轉為 data.frame 格式返回
  data.frame(
    id = i,
    MFII_ever_used = ever.used,
    theta_est = theta.est_eap,
    est_info = est_info,
    true_info = true_info
  )
} 

# 將CAT_data轉為4個matrix
for (i in 1:np) {
  MFII_ever_used[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 2]
  MFII_theta_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 3]
  MFII_est_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 4]
  MFII_true_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 5]
}

# 紀錄作答結束的時間
end_time_MFII <- Sys.time()

# 計算模擬時間
exec_timeCAT_MFII <- end_time_MFII - start_time_MFII

# 將時間差轉換為天、時、分、秒
total_seconds <- as.numeric(exec_timeCAT_MFII, units = "secs")
days <- floor(total_seconds / (24 * 3600))
hours <- floor((total_seconds %% (24 * 3600)) / 3600)
minutes <- floor((total_seconds %% 3600) / 60)
seconds <- round(total_seconds %% 60)  # 可選保留小數秒

# 動態構建報告字符串
formatted_time_MFII <- trimws(paste(  # trimws()去除空格
  if (days > 0) sprintf("%d 天", days) else "",
  if (hours > 0) sprintf("%02d 小時", hours) else "",
  if (minutes > 0) sprintf("%02d 分鐘", minutes) else "",
  if (seconds > 0) sprintf("%d 秒", seconds) else ""  # 可不去除小數點%.2f
))

# 輸出模擬時間
cat("MFII模擬時間：", formatted_time_MFII, "\n")


###----------------------MFII_data.frame生成結果----------------------
## 數據框的建立
# 數據框名字建立
row_name <- c("-3", "-2", "-1", "0", "1", "2", "3")  # 列的名字(即，左側)
col_name <- c()  # 行的名字(即，上側)
for (i in 1 : (TL - 4)) {col_name[i] <- paste0("第", i + 4, "題")}
# bias數據框
MFII_df_bias <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFII_df_bias) <- row_name
colnames(MFII_df_bias) <- col_name
# variance數據框
MFII_df_var <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFII_df_var) <- row_name
colnames(MFII_df_var) <- col_name
# MSE數據框
MFII_df_MSE <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFII_df_MSE) <- row_name
colnames(MFII_df_MSE) <- col_name
# est_info數據框
MFII_df_info_mean_est <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFII_df_info_mean_est) <- row_name
colnames(MFII_df_info_mean_est) <- col_name
# ture_info數據框
MFII_df_info_mean_true <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MFII_df_info_mean_true) <- row_name
colnames(MFII_df_info_mean_true) <- col_name



## 計算bias, variance, MSE, info_mean_est and info_mean_true
for (i in 1:7) {
  for (j in 1:(TL - 4)) {
    MFII_df_bias[i, j] <- theta.true[i * R] - mean(MFII_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # bias
    MFII_df_var[i, j] <- var(MFII_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # variance
    MFII_df_MSE[i, j] <- mean((theta.true[i * R] - MFII_theta_table[((i - 1) * R + 1):(i * R), j + 4]) ^ 2)  # MSE
    MFII_df_info_mean_est[i, j] <- mean(MFII_est_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_est
    MFII_df_info_mean_true[i, j] <- mean(MFII_true_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_true
  }
}

## MFII資料輸出
write_xlsx(MFII_df_bias, "unif400_MFII_bias.xlsx")
write_xlsx(MFII_df_var, "unif400_MFII_var.xlsx")
write_xlsx(MFII_df_MSE, "unif400_MFII_MSE.xlsx")
write_xlsx(MFII_df_info_mean_est, "unif400_MFII_info_mean_est.xlsx")
write_xlsx(MFII_df_info_mean_true, "unif400_MFII_info_mean_true.xlsx")

###---------------------MLWICAT(平行計算)---------------------------
### CAT相關矩陣宣告
MLWI_true_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄真實能力值下，計算的訊息量
MLWI_est_info_table <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄當前能力值下，計算的訊息量
MLWI_theta_table <-  matrix(NA, nrow = np, ncol = TL) # 矩陣，紀錄當前的能力估計值
MLWI_ever_used <- matrix(NA, nrow = np, ncol = TL)  # 矩陣，用於紀錄已經選取過的題目

## # 紀錄測試時間(使用系統的時間)
start_time_MLWI <- Sys.time()  

# 平行運算
CAT_data <- foreach(i = 1:np, .combine = rbind, .packages = "catR", .options.snow = opts) %dopar% {
  # 因為np個人是同時進行的，所以原本矩陣nrow是np的，都變為TL長度的向量即可
  ever.used <- numeric(TL)  # 向量，用於紀錄已經選取過的題目
  theta.est_eap <- numeric(TL)  # 向量，用於紀錄當前的能力值
  est_info <- numeric(TL)  # 向量，用於紀錄當前能力值下的訊息量
  true_info <- numeric(TL)  # 向量，用於紀錄真實能力值下的訊息量
  eap.para <- matrix(nrow = TL, ncol = 4)  # 矩陣，用以記錄作答過的題目參數
  admin.x <- c()  # 建立一個空向量 (裝取被選到的題目的題號)
  
  # 一個人的作答情況的循環
  for (j in 1:TL) {
    ## 題目選取邏輯
    MLWI_criterion <- nextItem(item_pool, criterion = "MLWI", out = ever.used, x = admin.x)
    
    ## 更新作答紀錄(紀錄作答過的題號、該題的作答反應 and 作答過的題目的試題參數)
    ever.used[j] <- MLWI_criterion$item
    admin.x[j] <- response[i, MLWI_criterion$item]
    eap.para[j, ] <- MLWI_criterion$par
    
    ## 估計能力估計值 
    theta.est_eap[j] <- eapEst(eap.para[1:j, ], admin.x[1:j])
    
    ## 紀錄當前能力值下的訊息量 and 當前真實能力值下的訊息量
    est_info[j] <- mean(Ii(theta.est_eap, eap.para[1:j, ])$Ii)
    true_info[j] <- mean(Ii(theta.true[i], eap.para[1:j, ])$Ii)
  }
  # 將數據轉為 data.frame 格式返回
  data.frame(
    id = i,
    MLWI_ever_used = ever.used,
    theta_est = theta.est_eap,
    est_info = est_info,
    true_info = true_info
  )
} 

# 將CAT_data轉為4個matrix
for (i in 1:np) {
  MLWI_ever_used[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 2]
  MLWI_theta_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 3]
  MLWI_est_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 4]
  MLWI_true_info_table[i, ] <- CAT_data[((i - 1) * TL + 1):(i * TL), 5]
}

# 紀錄作答結束的時間
end_time_MLWI <- Sys.time()

# 計算模擬時間
exec_timeCAT_MLWI <- end_time_MLWI - start_time_MLWI

# 將時間差轉換為天、時、分、秒
total_seconds <- as.numeric(exec_timeCAT_MLWI, units = "secs")
days <- floor(total_seconds / (24 * 3600))
hours <- floor((total_seconds %% (24 * 3600)) / 3600)
minutes <- floor((total_seconds %% 3600) / 60)
seconds <- round(total_seconds %% 60)  # 可選保留小數秒

# 動態構建報告字符串
formatted_time_MLWI <- trimws(paste(  # trimws()去除空格
  if (days > 0) sprintf("%d 天", days) else "",
  if (hours > 0) sprintf("%02d 小時", hours) else "",
  if (minutes > 0) sprintf("%02d 分鐘", minutes) else "",
  if (seconds > 0) sprintf("%d 秒", seconds) else ""  # 可不去除小數點%.2f
))

# 輸出模擬時間
cat("MLWI模擬時間：", formatted_time_MLWI, "\n")

# 關閉平行運算設置
close(pb)
stopCluster(cl) 


###----------------------MLWI_data.frame生成結果----------------------
## 數據框的建立
# 數據框名字建立
row_name <- c("-3", "-2", "-1", "0", "1", "2", "3")  # 列的名字(即，左側)
col_name <- c()  # 行的名字(即，上側)
for (i in 1 : (TL - 4)) {col_name[i] <- paste0("第", i + 4, "題")}
# bias數據框
MLWI_df_bias <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MLWI_df_bias) <- row_name
colnames(MLWI_df_bias) <- col_name
# variance數據框
MLWI_df_var <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MLWI_df_var) <- row_name
colnames(MLWI_df_var) <- col_name
# MSE數據框
MLWI_df_MSE <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MLWI_df_MSE) <- row_name
colnames(MLWI_df_MSE) <- col_name
# est_info數據框
MLWI_df_info_mean_est <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MLWI_df_info_mean_est) <- row_name
colnames(MLWI_df_info_mean_est) <- col_name
# ture_info數據框
MLWI_df_info_mean_true <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(MLWI_df_info_mean_true) <- row_name
colnames(MLWI_df_info_mean_true) <- col_name


## 計算bias, variance, MSE, info_mean_est and info_mean_true
for (i in 1:7) {
  for (j in 1:(TL - 4)) {
    MLWI_df_bias[i, j] <- theta.true[i * R] - mean(MLWI_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # bias
    MLWI_df_var[i, j] <- var(MLWI_theta_table[((i - 1) * R + 1):(i * R), j + 4])  # variance
    MLWI_df_MSE[i, j] <- mean((theta.true[i * R] - MLWI_theta_table[((i - 1) * R + 1):(i * R), j + 4]) ^ 2)  # MSE
    MLWI_df_info_mean_est[i, j] <- mean(MLWI_est_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_est
    MLWI_df_info_mean_true[i, j] <- mean(MLWI_true_info_table[((i - 1) * R + 1):(i * R), j + 4])  # info_mean_true
  }
}

## MLWI資料輸出
write_xlsx(MLWI_df_bias, "unif400_MLWI_bias.xlsx")
write_xlsx(MLWI_df_var, "unif400_MLWI_var.xlsx")
write_xlsx(MLWI_df_MSE, "unif400_MLWI_MSE.xlsx")
write_xlsx(MLWI_df_info_mean_est, "unif400_MLWI_info_mean_est.xlsx")
write_xlsx(MLWI_df_info_mean_true, "unif400_MLWI_info_mean_true.xlsx")


###----------------------展示結果(圖片)----------------------
# 繪製bias # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  plot(5:TL, MFI_df_bias[i, ], type = "l", col = "blue", lwd = 3, xlab = "itemlength", ylab = "bias",
       main = paste("ability = ",i-4), xlim = c(1, TL), ylim = c(-1.6, 1.6)) # ylim 自行調整
  lines(5:TL, MFII_df_bias[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_bias[i, ], col = "red", lwd = 3)
  legend("topright", legend = c("MFI", "MFII", "MLWI"), col = c("blue", "green", "red"), lty = 1, lwd = 2)
}

# 繪製var # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  plot(5:TL, MFI_df_var[i, ], type = "l", col = "blue", lwd = 3, xlab = "itemlength", ylab = "varience",
       main = paste("ability = ",i-4), xlim = c(1, TL), ylim = c(0, .4)) # ylim 自行調整
  lines(5:TL, MFII_df_var[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_var[i, ], col = "red", lwd = 3)
}

# 繪製MSE # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  plot(5:TL, MFI_df_MSE[i, ], type = "l", col = "blue", lwd = 3, xlab = "itemlength", ylab = "MSE",
       main = paste("ability = ",i-4), xlim = c(1, TL), ylim = c(0, 2)) # ylim 自行調整
  lines(5:TL, MFII_df_MSE[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_MSE[i, ], col = "red", lwd = 3)
}

# 繪製 mean_info(基於真實能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  plot(5:TL, MFI_df_info_mean_true[i, ], type = "l", col = "blue", lwd = 3, xlab = "itemlength", ylab = "mean_info(真實)",
       main = paste("ability = ",i-4), xlim = c(1, TL), ylim = c(0, .5)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_true[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_info_mean_true[i, ], col = "red", lwd = 3)
}

# 繪製 mean_info(基於估計能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  plot(5:TL, MFI_df_info_mean_est[i, ], type = "l", col = "blue", lwd = 3, xlab = "itemlength", ylab = "mean_info(估計)",
       main = paste("ability = ",i-4), xlim = c(1, TL), ylim = c(0, .5)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_est[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_info_mean_est[i, ], col = "red", lwd = 3)
}
