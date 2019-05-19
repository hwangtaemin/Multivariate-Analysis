#Assignment 4
library(moments)
library(corrplot)
library(plyr)
library(ggplot2)

# Performance Evaluation Function
perf_eval2 <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Initialize the performance matrix
perf_mat <- matrix(0, 1, 6)
colnames(perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

#Load Dataset
Admission <- read.csv("Admission_Predict.csv")

#[Q1]
input_idx <- c(2:8)
target_idx <- 9
Admission_input <- Admission[,input_idx]
Admission_target <- Admission[,target_idx]

#[Q2]
options(scipen = 100)
Admission_statistic <- data.frame()
for(i in (1:7)){
  Admission_statistic[i,1] <- mean(Admission_input[,i])
  Admission_statistic[i,2] <- sqrt(var(Admission_input[,i]))
  Admission_statistic[i,3] <- skewness(Admission_input[,i])
  Admission_statistic[i,4] <- kurtosis(Admission_input[,i])
}
rownames(Admission_statistic) <- colnames(Admission_input)
colnames(Admission_statistic)[1] <- "Mean"
colnames(Admission_statistic)[2] <- "Standard Deviation"
colnames(Admission_statistic)[3] <- "Skewness"
colnames(Admission_statistic)[4] <- "Kurtosis"
Admission_statistic
par(mfrow = c(1,2))
for (i in 1:7){
  boxplot(Admission_input[,i],main=colnames(Admission_input)[i])
}
dev.off()

#[Q3]
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
Admission_removed <- data.frame()
for(i in 1:7){
  Admission_input[,i] <- remove_outliers(Admission_input[,i])
}
Admission_removed <- na.omit(Admission_input)

#[Q4]
Admission.cor = Admission_removed
plot(Admission.cor)
cormat <- round(cor(Admission.cor),3)
corrplot(cormat)

#[Q5]
Admission_target_binary <- data.frame()
for(i in 1:398){
  if(Admission_target[i]>0.8){
    Admission_target_binary <- rbind(Admission_target_binary, 1)
  }
  else Admission_target_binary <- rbind(Admission_target_binary, 0)
}
#Normalization
Admission_removed_scaled <- scale(Admission_removed, center = TRUE, scale = TRUE)
Admission_scaled_data <- data.frame(Admission_removed_scaled, Admission_target_binary)
colnames(Admission_scaled_data)[8] <- "Chance.of.Admit"

set.seed(12345)
trn_idx <- sample(1:nrow(Admission_removed), round(0.7*nrow(Admission_removed)))
Admission_trn <- Admission_scaled_data[trn_idx,]
Admission_tst <- Admission_scaled_data[-trn_idx,]

full_lr <- glm(Chance.of.Admit ~ ., family=binomial, Admission_trn)
summary(full_lr)

#[Q6]
lr_response <- predict(full_lr, type = "response", newdata = Admission_tst)
lr_target <- Admission_tst$Chance.of.Admit
response_table <- data.frame(lr_response,lr_target)
response_table
#cut off value = 0.5
lr_predicted1 <- rep(0, length(lr_target))
lr_predicted1[which(lr_response >= 0.5)] <- 1
cm_full1 <- table(lr_target, lr_predicted1)
cm_full1

perf_mat[1,] <- perf_eval2(cm_full1)
perf_mat
#cut off value = 0.7
lr_predicted2 <- rep(0, length(lr_target))
lr_predicted2[which(lr_response >= 0.7)] <- 1
cm_full2 <- table(lr_target, lr_predicted2)
cm_full2

perf_mat[1,] <- perf_eval2(cm_full2)
perf_mat
#cut off value = 0.3
lr_predicted3 <- rep(0, length(lr_target))
lr_predicted3[which(lr_response >= 0.3)] <- 1
cm_full3 <- table(lr_target, lr_predicted3)
cm_full3

perf_mat[1,] <- perf_eval2(cm_full3)
perf_mat

#[Q7]
ROC <- function(prob_table){
  prob_table <- prob_table[order(-prob_table[,1]),]
  ROC_TPR <- rep(0,nrow(prob_table))
  ROC_FPR <- rep(0,nrow(prob_table))
  ROC_actualNG <- length(which(prob_table[,2]==1))
  ROC_actualG <- length(which(prob_table[,2]==0))
  NG_count <- 0
  G_count <- 0
  ROC_table <- data.frame(prob_table, ROC_TPR, ROC_FPR)
  for(i in (1:nrow(ROC_table))){
    if(ROC_table[i,2]==1){
      NG_count = NG_count + 1
      ROC_table[i,3] = NG_count/ROC_actualNG
    }
    else
      NG_count = NG_count
      ROC_table[i,3] = NG_count/ROC_actualNG
  }
  for(i in (1:nrow(ROC_table))){
    if(ROC_table[i,2]==0){
      G_count = G_count + 1
      ROC_table[i,4] = G_count/ROC_actualG
    }
    else
      G_count = G_count
    ROC_table[i,4] = G_count/ROC_actualG
  }
  return(ROC_table)
}
AUROC <-function(roc_table){
  ROC_actualG <- length(which(roc_table[,2]==0))
  AUROC <- 0
  for(i in (1:(nrow(roc_table)-1)))
    if(roc_table[i,4]!=roc_table[i+1,4])
      AUROC = AUROC + roc_table[i,3]*(1/ROC_actualG)
    else
      AUROC = AUROC
  return(AUROC)
}

#iteration 5
AUROC_mat <- matrix(0,5,1)
colnames(AUROC_mat) <- "AUROC"
for(i in (1:5)){
  set.seed(2000+5*i)
  trn_idx <- sample(1:nrow(Admission_removed), round(0.7*nrow(Admission_removed)))
  Admission_trn <- Admission_scaled_data[trn_idx,]
  Admission_tst <- Admission_scaled_data[-trn_idx,]
  full_lr <- glm(Chance.of.Admit ~ ., family=binomial, Admission_trn)
  lr_response <- predict(full_lr, type = "response", newdata = Admission_tst)
  lr_target <- Admission_tst$Chance.of.Admit
  response_table <- data.frame(lr_response,lr_target)
  roc_table <- ROC(response_table)
  AUROC_mat[i,]<- AUROC(roc_table)
}
AUROC_mat
#ROC Curve
ggplot(data=roc_table,aes(x=roc_table[,4],y=roc_table[,3]))+geom_path()+
  labs(x="1-specificity",y="sensitivity",title="ROC curve of count")





