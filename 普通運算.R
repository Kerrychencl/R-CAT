###模擬練習--電腦適性測驗
library(catR)


## 受試者能力生成
R <- 2  # 將能力值重複R遍
theta.true <- rep(c(-3, -2, -1, 0, 1, 2, 3), each = R)  # theta.true生成

##參數設定 (受測者數量、題庫和測驗長度)
# 受測者數量(np)：1400
np <- length(theta.true)
# 題庫大小的選擇(ni)(1：，0：)
pool <- 1
if (pool == 1) ni = 75 else ni = 10  # 試題數量(選其中一個題庫)：400 and 200
# 測驗訂定的題數長度(TL)：60
TL <- 20  

# 題庫生成的選擇(1：均勻分布，其他數字：常態分布)
distribution <- 1
### 題庫生成
item_pool <- matrix(nrow = ni, ncol = 4)  #創建空矩陣，用於裝取題目參數
if (distribution == 1) {
  ## 試題參數生成(均勻分布)
  set.seed(123)  # 設置隨機種子以確保結果可重現
  item_pool[,1] <- runif(ni, min = 0.5, max = 2.0)  # 鑑別度
  item_pool[,2] <- runif(ni, min = -3.0, max = 3.0)  # 難度
  item_pool[,3] <- runif(ni, min = 0.15, max = 0.30)  # 猜測度
  item_pool[,4] <- 1  # inattention value 
} else {
  ## 試題參數生成(常態分布)
  set.seed(168)  # 設置隨機種子以確保結果可重現
  item_pool[,1] <- rnorm(ni, 1, 0.1)  # 鑑別度
  item_pool[,2] <- rnorm(ni)  # 難度
  item_pool[,3] <- rnorm(ni, 0, 0.2)  # 猜測度
  item_pool[,4] <- 1  # inattention value 
}


#### 作答反應資料產生
response <- matrix(nrow = np, ncol = ni)  # 創建作答反應矩陣，用於裝取作答反應
for (i in 1 : np){
  for (j in 1 : ni){
    # 3PL的答對率公式
    p = item_pool[j, 3] + (1 - item_pool[j, 3] ) * exp(item_pool[j, 1] * (theta.true[i] - item_pool[j, 2])) / (
      1 + exp(item_pool[j, 1] * (theta.true[i] - item_pool[j, 2])))
    # 隨機生成一個連續的機率資料 (最小值為0，最大值為1)
    r = runif(1)
    # 如果答對率高於隨機機率，就代表達對，反之則為錯
    if (p >= r) response[i, j] = 1 else response[i, j] = 0
  }
}

#---------------------CAT---------------------------
##CAT相關矩陣宣告
true_info_table <- matrix(nrow = np, ncol = TL)  # 矩陣，用於紀錄真實能力值下，計算的訊息量
est_info_table <- matrix(nrow = np, ncol = TL)  # 矩陣，用於紀錄當前能力值下，計算的訊息量
theta_table <-  matrix(nrow = np, ncol = TL) # 矩陣，紀錄當題的能力估計值
ever.used <- matrix(nrow = np, ncol = TL)  # 矩陣，用於紀錄已經選取過的題目
eap.para <- matrix(nrow = TL, ncol = 4)  # 矩陣，用以記錄作答過的題目參數
theta.est_eap <- numeric(length = np)  # 向量，且每個元素被標為0，用以裝取eap能力估計值
#theta.est_ML <- numeric(length = np)  # 向量，且每個元素被標為0，用以裝取MLE能力估計值

##CAT施測
start_time <- Sys.time()  # 紀錄測試時間(使用系統的時間)
for (i in 1 : np) {
  ever.used_in <- c()  # 建立一個空向量 (裝取被選到的題目的題號)
  admin.x <- c()  # 建立一個空向量 (裝取被選到的題目的作答反應)
  ###序列式施測
  for (j in 1 : TL){
    # 題目選取邏輯(MLWI)
    MLWI_criterion <- nextItem(item_pool, criterion = "MLWI", out = ever.used_in, x = admin.x)
    # 更新作答紀錄(紀錄作答過的題號 and 該題的作答反應)
    ever.used[i, j] <- MLWI_criterion$item   # 將使用過的題號記錄到ever.used中(結束時查看用)
    ever.used_in[j] <- MLWI_criterion$item   # 將使用過的題號記錄到ever.used_in中(循環內用)
    admin.x[j] <- response[i, MLWI_criterion$item]  # 儲存第j題為選到的第n題的作答反應
    # 新增作答的試題參數
    eap.para[j,] <- MLWI_criterion$par  # 紀錄第n題的試題參數
    # 更新能力估計(EAP)
    theta.est_eap[i] <- eapEst(eap.para[1 : j,], admin.x[1 : j])
    # 更新能力估計(MLE)
    #theta.est_ML[i] <- thetaEst(matrix(eap.para[1 : j,], nrow = j), admin.x[1 : j], method = "ML")
    # 紀錄能力估計值 (當前題目的能力值)
    theta_table[i,j] <- theta.est_eap[i]
    # 紀錄當前能力值下的訊息量
    est_info_table[i, j] <- mean(Ii(theta.est_eap[i], eap.para[1 : j, ])$Ii)
    # 紀錄當真實能力值下的訊息量
    true_info_table[i, j] <- mean(Ii(theta.true[i], eap.para[1 : j, ])$Ii)
    # 用於暫停並調試
    #if (j == 10) {
        #browser()  # 在第 5 次迴圈時進入調試
      #}
  }
  # 顯示進度，\n為分行報告
  message(paste0("目前施測到第", i, "人\n"))
}
# 紀錄作答結束的時間
end_time <- Sys.time()
# 紀錄使用的時間
exec_timeCAT <- end_time - start_time
# 報告使用的時間
cat("模擬時間：", exec_timeCAT, "\n")


###----------------------data.frame展示結果----------------------
## 數據框的建立
# 數據框名字建立
row_name <- c("-3", "-2", "-1", "0", "1", "2", "3")  # 列的名字(即，左側)
col_name <- c()  # 行的名字(即，上側)
for (i in 1 : (TL - 4)) {col_name[i] <- paste0("第", i + 4, "題")}
# bias數據框
df_bias <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(df_bias) <- row_name
colnames(df_bias) <- col_name
# variance數據框
df_var <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(df_var) <- row_name
colnames(df_var) <- col_name
# MSE數據框
df_MSE <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(df_MSE) <- row_name
colnames(df_MSE) <- col_name
# est_info數據框
df_info_mean_est <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(df_info_mean_est) <- row_name
colnames(df_info_mean_est) <- col_name
# ture_info數據框
df_info_mean_true <- data.frame(matrix(0, nrow = 7, ncol = TL - 4))
row.names(df_info_mean_true) <- row_name
colnames(df_info_mean_true) <- col_name



## 計算bias, variance, MSE, info_mean_est and info_mean_true
for (i in 1 : 7) {
  for (j in 1 : (TL - 4)) {
    df_bias[i, j] <- theta.true[i * R] - mean(theta_table[((i - 1) * R + 1) : (i * R), j + 4])  # bias
    # 用於暫停並調試
    #if (j == (TL-3)) {
    #browser()  # 在第 5 次迴圈時進入調試
    #}
    df_var[i, j] <- var(theta_table[((i - 1) * R + 1) : (i * R), j + 4])  # variance
    df_MSE[i, j] <- mean((theta.true[i * R] - theta_table[((i - 1) * R + 1) : (i * R), j + 4]) ^ 2)  # MSE
    df_info_mean_est[i, j] <- mean(est_info_table[((i - 1) * R + 1) : (i * R), j + 4])  # info_mean_est
    df_info_mean_true[i, j] <- mean(true_info_table[((i - 1) * R + 1) : (i * R), j + 4])  # info_mean_true
  }
}

## 以數據框的形式報告bias, variance, MSE, info_mean_est and info_mean_true
df_bias  # 偏誤
df_var  # 變異數
df_MSE  # 均方誤
df_info_mean_true  # 平均訊息量(基於真實能力值)
df_info_mean_est  # 平均訊息量(基於估計能力值)


###----------------------用ggplot展示結果的圖片----------------------


