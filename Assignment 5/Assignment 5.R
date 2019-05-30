#Assignment 5
library(glmnet)
library(GA)
library(moments)

# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

# Initialize a performance summary
Perf_Table <- matrix(0, nrow = 6, ncol = 3)
rownames(Perf_Table) <- c("All", "Exhaustive", "Forward", "Backward", "Stepwise", "GA")
colnames(Perf_Table) <- c("RMSE", "MAE", "MAPE")
Perf_Table

# Load the datasets
weather <- read.csv("Weather_Ankara.csv")

# Split the data into the training/validation sets
set.seed(12345)
nweather <- nrow(weather)
weather_trn_idx <- sample(1:nweather, round(0.778*nweather))
weather_trn_data <- weather[weather_trn_idx,]
weather_val_data <- weather[-weather_trn_idx,]

#[Q1] ALL
full_model_weather <- lm( Mean_temperature ~ ., data = weather_trn_data)
full_model_weather
summary(full_model_weather)

full_model_haty <- predict(full_model_weather, newdata = weather_val_data)
Perf_Table[1,] <- perf_eval_reg(weather_val_data$Mean_temperature, full_model_haty)
Perf_Table

#[Q2] Exhaustive Search
iMatrix <<- 1:(length(weather)-1)    
dfs <- function(idx,arr){
  if(idx==length(arr)+1){
    iMatrix <<- cbind(iMatrix,arr)
    return()
  }
  arr[idx] <- 0
  dfs(idx+1, arr)
  arr[idx] <- 1
  dfs(idx+1, arr)
}
dfs(1, 1:(length(weather)-1))
iMatrix <- data.frame(iMatrix[,-c(1,2)]) 

exhaustive_search <- function(){
  start_time<-Sys.time()
  r.squared <- rep(0:9)
  names(r.squared) <- c("var1","var2","var3","var4","var5","var6","var7","var8","var9","ADJ R")
  for(i in 1:511){
    tmp_x <- paste(colnames(weather[,-10])[which(iMatrix[i]==1)],collapse=" + ")
    string <- paste("Mean_temperature ~ ", tmp_x, collapse = "")
    model <- lm(as.formula(string), data = weather_trn_data)
    r.squared <- rbind(r.squared, c(iMatrix[,i],round(summary(model)$adj.r.squared,5)))
  }
  end_time<-Sys.time()
  print(end_time - start_time)
  return(r.squared)
}
result <- exhaustive_search()
result <- result[-1,]

es_model_weather <- lm( Mean_temperature ~ Max_termperature + Min_temperature + Dewpoint +
                          Sea_level_pressure + Standard_pressure + Visibility + Wind_speed
                        , data = weather_trn_data)
es_model_weather
summary(es_model_weather)

es_model_haty <- predict(es_model_weather, newdata = weather_val_data)
Perf_Table[2,] <- perf_eval_reg(weather_val_data$Mean_temperature, es_model_haty)
Perf_Table
#[Q3] F.S, B.E, S.S
#Forward Selection
start_time <- Sys.time()
forward_model <- step(lm(Mean_temperature ~ 1, data = weather_trn_data), 
                      scope = list(upper = full_model_weather, lower = Mean_temperature ~ 1), 
                      direction="forward", trace = 1)
end_time <- Sys.time()
print(end_time-start_time)
summary(forward_model)

forward_model_haty <- predict(forward_model, newdata = weather_val_data)
Perf_Table[3,] <- perf_eval_reg(weather_val_data$Mean_temperature, forward_model_haty)
Perf_Table

#Backward Elimination
start_time <- Sys.time()
backward_model <- step(full_model_weather, 
                       scope = list(upper = full_model_weather, lower = Mean_temperature ~ 1),
                       direction = "backward", trace = 1)
end_time <- Sys.time()
print(end_time-start_time)
summary(backward_model)

backward_model_haty <- predict(backward_model, newdata = weather_val_data)
Perf_Table[4,] <- perf_eval_reg(weather_val_data$Mean_temperature, backward_model_haty)
Perf_Table

#Stepwise Selection
start_time <- Sys.time()
stepwise_model <- step(lm( Mean_temperature ~ 1, data = weather_trn_data), 
                       scope = list(upper = full_model_weather, lower = Mean_temperature ~ 1), 
                       direction="both", trace = 1)
end_time <- Sys.time()
print(end_time-start_time)
summary(stepwise_model)

stepwise_model_haty <- predict(stepwise_model, newdata = weather_val_data)
Perf_Table[5,] <- perf_eval_reg(weather_val_data$Mean_temperature, stepwise_model_haty)
Perf_Table

#[Q4] G.A
# Fitness function: Adjusted R-squared
fit_R <- function(string){
  sel_var_idx <- which(string == 1)
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- lm(y ~ ., data = xy)
  summary(GA_lr)$adj.r.squared
  return(summary(GA_lr)$adj.r.squared)
}

x <- as.matrix(weather_trn_data[,-10])
y <- weather_trn_data[,10]

# Variable selection by Genetic Algorithm
start_time <- Sys.time()
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
            names = colnames(x), popSize = 50, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
end_time <- Sys.time()
end_time - start_time

best_var_idx <- which(GA_R@solution == 1)

GA_trn_data <- weather_trn_data[,c(best_var_idx, 10)]
GA_tst_data <- weather_val_data[,c(best_var_idx, 10)]

GA_model <- lm( Mean_temperature ~ ., GA_trn_data)
summary(GA_model)

GA_model_haty <- predict(GA_model, newdata = GA_tst_data)
Perf_Table[6,] <- perf_eval_reg(GA_tst_data$Mean_temperature, GA_model_haty)
Perf_Table

#[Q5]
#cross over rate changes
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.25, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)

best_var_idx2 <- which(GA_R@solution == 1)
best_var_idx2

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.75, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)

best_var_idx3 <- which(GA_R@solution == 1)
best_var_idx3

#mutation rate changes
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.1, maxiter = 100, elitism = 2, seed = 123)

best_var_idx4 <- which(GA_R@solution == 1)
best_var_idx4

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.5, maxiter = 100, elitism = 2, seed = 123)

best_var_idx5 <- which(GA_R@solution == 1)
best_var_idx5

#population size changes
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 25, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)

best_var_idx6 <- which(GA_R@solution == 1)
best_var_idx6

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 5, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)

best_var_idx7 <- which(GA_R@solution == 1)
best_var_idx7

#max iteration changes
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 50, elitism = 2, seed = 123)

best_var_idx8 <- which(GA_R@solution == 1)
best_var_idx8

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 10, elitism = 2, seed = 123)

best_var_idx9 <- which(GA_R@solution == 1)
best_var_idx9

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 5, elitism = 2, seed = 123)

best_var_idx10 <- which(GA_R@solution == 1)
best_var_idx10

#extreme changes
GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 10, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 5, elitism = 2, seed = 123)

best_var_idx11 <- which(GA_R@solution == 1)
best_var_idx11

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 10, pcrossover = 0.5, 
           pmutation = 0.5, maxiter = 5, elitism = 2, seed = 123)

best_var_idx12 <- which(GA_R@solution == 1)
best_var_idx12

GA_R <- ga(type = "binary", fitness = fit_R, nBits = ncol(x), 
           names = colnames(x), popSize = 10, pcrossover = 0.75, 
           pmutation = 0.01, maxiter = 5, elitism = 2, seed = 123)

best_var_idx13 <- which(GA_R@solution == 1)
best_var_idx13
