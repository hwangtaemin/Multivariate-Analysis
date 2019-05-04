#Assignment 3
library(moments)
library(corrplot)
library(dplyr)
library(psych)

perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)
rownames(perf_mat) <- c("House","House_7")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")

#[Q1]
House <- read.csv("kc_house_data.csv")
House_revised <- House[,-c(1,2,17)]
row <- nrow(House_revised)
col <- ncol(House_revised)
#[Q2]
options(scipen = 100)
House_statistic <- data.frame()
for(i in (1:col)){
  House_statistic[i,1] <- mean(House_revised[,i])
  House_statistic[i,2] <- sqrt(var(House_revised[,i]))
  House_statistic[i,3] <- skewness(House_revised[,i])
  House_statistic[i,4] <- kurtosis(House_revised[,i])
}
rownames(House_statistic) <- colnames(House_revised)
colnames(House_statistic)[1] <- "Mean"
colnames(House_statistic)[2] <- "Standard Deviation"
colnames(House_statistic)[3] <- "Skewness"
colnames(House_statistic)[4] <- "Kurtosis"
House_statistic
par(mfrow = c(2,4))
for (i in 1:18){
  boxplot(House_revised[,i],main=colnames(House_revised)[i])
}
dev.off()
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#[Q3]
House_revised[,2] <- remove_outliers(House_revised[,2])
House_revised[,3] <- remove_outliers(House_revised[,3])
House_revised[,4] <- remove_outliers(House_revised[,4])
House_revised[,5] <- remove_outliers(House_revised[,5])
House_revised[,6] <- remove_outliers(House_revised[,6])
House_revised[,9] <- remove_outliers(House_revised[,9])
House_revised[,10] <- remove_outliers(House_revised[,10])
House_revised[,11] <- remove_outliers(House_revised[,11])
House_revised[,12] <- remove_outliers(House_revised[,12])
House_revised[,13] <- remove_outliers(House_revised[,13])
House_revised[,15] <- remove_outliers(House_revised[,15])
House_revised[,16] <- remove_outliers(House_revised[,16])
House_revised[,17] <- remove_outliers(House_revised[,17])
House_revised[,18] <- remove_outliers(House_revised[,18])
House_revised <- na.omit(House_revised)

#[Q4]
data.cor = House_revised[,-1]
plot(data.cor[sample(1:nrow(data.cor),500),])
cormat <- round(cor(data.cor),3)
corrplot(cormat)

#[Q5&6]
set.seed(12345) 
row_revised <- nrow(House_revised)
House_trn_idx <- sample(1:row_revised, round(0.7*row_revised))
House_trn_data <- House_revised[House_trn_idx,]
House_val_data <- House_revised[-House_trn_idx,]
mlr_House <- lm(price ~ ., data = House_trn_data)
mlr_House
summary(mlr_House)
plot(mlr_House)

#[Q7]
mlr_House_haty <- predict(mlr_House, newdata = House_val_data)
perf_mat[1,] <- perf_eval_reg(House_val_data$price, mlr_House_haty)
perf_mat

#[Q8&9]
House_trn_data <- House_trn_data[,c(1,4,7,8,9,10,13,15)]
mlr_housing <- lm(price ~., data = House_trn_data)
summary(mlr_housing)
pred <- predict(mlr_housing, newdata = House_val_data)
perf_mat[2,] <- perf_eval_reg(House_val_data$price,pred)
perf_mat

#[Extra Question]
pairs.panels(cormat)
