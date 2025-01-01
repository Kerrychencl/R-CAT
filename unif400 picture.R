# 載入需要的套件
library(readxl)

###----------------------讀取資料----------------------
TL <- 60
# MFI
MFI_df_bias <- read_excel("unif400_MFI_bias.xlsx")
MFI_df_var <- read_excel("unif400_MFI_var.xlsx")
MFI_df_MSE <- read_excel("unif400_MFI_MSE.xlsx")
MFI_df_info_mean_est <- read_excel("unif400_MFI_info_mean_est.xlsx")
MFI_df_info_mean_true <- read_excel("unif400_MFI_info_mean_true.xlsx")

# MFII
MFII_df_bias <- read_excel("unif400_MFII_bias.xlsx")
MFII_df_var <- read_excel("unif400_MFII_var.xlsx")
MFII_df_MSE <- read_excel("unif400_MFII_MSE.xlsx")
MFII_df_info_mean_est <- read_excel("unif400_MFII_info_mean_est.xlsx")
MFII_df_info_mean_true <- read_excel("unif400_MFII_info_mean_true.xlsx")
###----------------------MLWI資料----------------------
# MLWI
MLWI_df_bias <- read_excel("unif400_MLWI_bias.xlsx")
MLWI_df_var <- read_excel("unif400_MLWI_var.xlsx")
MLWI_df_MSE <- read_excel("unif400_MLWI_MSE.xlsx")
MLWI_df_info_mean_est <- read_excel("unif400_MLWI_info_mean_est.xlsx")
MLWI_df_info_mean_true <- read_excel("unif400_MLWI_info_mean_true.xlsx")

###----------------------test_plot畫圖與輸出(2準則)----------------------
# 繪製bias # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 繪圖
  plot(5:TL, MFI_df_bias[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "bias",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(-1.6, 1.6)) # ylim 自行調整
  lines(5:TL, MFII_df_bias[i, ], col = "green", lwd = 3)
}
# 繪製var # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 繪圖
  plot(5:TL, MFI_df_var[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "varience",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, 0.4)) # ylim 自行調整
  lines(5:TL, MFII_df_var[i, ], col = "green", lwd = 3)
}
# 繪製MSE # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 繪圖
  plot(5:TL, MFI_df_MSE[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "MSE",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, 2)) # ylim 自行調整
  lines(5:TL, MFII_df_MSE[i, ], col = "green", lwd = 2)
}

# 繪製 mean_info(基於真實能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 繪圖
  plot(5:TL, MFI_df_info_mean_true[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(true)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .6)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_true[i, ], col = "green", lwd = 3)
}

# 繪製 mean_info(基於估計能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 繪圖
  plot(5:TL, MFI_df_info_mean_est[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(est)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .6)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_est[i, ], col = "green", lwd = 3)
}
###----------------------plot畫圖與輸出(2準則)----------------------
# 繪製bias # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("bias_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_bias[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "bias",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(-1.6, 1.6)) # ylim 自行調整
  lines(5:TL, MFII_df_bias[i, ], col = "green", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}
# 繪製var # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("var_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_var[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "varience",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, 0.4)) # ylim 自行調整
  lines(5:TL, MFII_df_var[i, ], col = "green", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}
# 繪製MSE # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("MSE_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_MSE[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "MSE",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, 2)) # ylim 自行調整
  lines(5:TL, MFII_df_MSE[i, ], col = "green", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}

# 繪製 mean_info(基於真實能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("info_true_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_info_mean_true[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(true)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .5)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_true[i, ], col = "green", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}

# 繪製 mean_info(基於估計能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("info_est_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_info_mean_est[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(est)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .5)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_est[i, ], col = "green", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}

###----------------------plot畫圖與輸出(全準則)----------------------
# 繪製bias # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("bias_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_bias[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "bias",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(-1.6, 1.6)) # ylim 自行調整
  lines(5:TL, MFII_df_bias[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_bias[i, ], col = "red", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
}

# 繪製var # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("var_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_var[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "varience",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .4)) # ylim 自行調整
  lines(5:TL, MFII_df_var[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_var[i, ], col = "red", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
  }

# 繪製MSE # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("MSE_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_MSE[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "MSE",
       main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, 2)) # ylim 自行調整
  lines(5:TL, MFII_df_MSE[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_MSE[i, ], col = "red", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
  }

# 繪製 mean_info(基於真實能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("info_true_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_info_mean_true[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(true)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .6)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_true[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_info_mean_true[i, ], col = "red", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
  }

# 繪製 mean_info(基於估計能力) # 藍:MFI 綠:MFII 紅:MLWI
for (i in 1:7) {
  # 開啟圖形設備，設置保存文件的路徑和文件名
  png(filename = paste0("info_est_ability_", i - 4, ".png"), width = 800, height = 600)
  
  # 繪圖
  plot(5:TL, MFI_df_info_mean_est[i, ], type = "l", col = "blue", lwd = 3, xlab = "item length", ylab = "mean info(est)"
       , main = paste0("ability = ", i - 4), xlim = c(1, TL), ylim = c(0, .6)) # ylim 自行調整
  lines(5:TL, MFII_df_info_mean_est[i, ], col = "green", lwd = 3)
  lines(5:TL, MLWI_df_info_mean_est[i, ], col = "red", lwd = 3)
  
  # 關閉圖形設備
  dev.off()
  }

legend("topright", legend = c("MFI", "MFII", "MLWI"), col = c("blue", "green", "red"), lty = 1, lwd = 2)
