#Assignment 6 Decision Tree
# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
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

# Performance table
Perf.Table <- matrix(0, nrow = 5, ncol = 6)
rownames(Perf.Table) <- c("Tree", "Tree_pruned","rpart","party","evtree")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
heart <- read.csv("heart.csv")
target.idx <- 14

heart.input <- heart[,-target.idx]
heart.target <- as.factor(heart[,target.idx])

heart.data <- data.frame(heart.input, heart.target)

# Split the data into the training/validation sets
set.seed(12345)
nheart <- nrow(heart)
trn.idx <- sample(1:nheart, round(0.66*nheart))

# Classification and Regression Tree (CART) -------------------------------
#[Q1] Full Tree
library(tree)

CART.trn <- data.frame(heart.input[trn.idx,], heartYN = heart.target[trn.idx])
CART.tst <- data.frame(heart.input[-trn.idx,], heartYN = heart.target[-trn.idx])

# Training the tree
CART.model <- tree(heartYN ~ ., CART.trn)
summary(CART.model)

# Plot the tree
plot(CART.model)
text(CART.model, pretty = 1)

# Full tree prediction
CART.prey <- predict(CART.model, CART.tst, type = "class")
CART.cfm <- table(CART.tst$heartYN, CART.prey)
CART.cfm

Perf.Table[1,] <- perf_eval(CART.cfm)
Perf.Table

#[Q2] Pruning
# Find the best tree
CART.model.cv <- cv.tree(CART.model, FUN = prune.misclass)

# Plot the pruning result
plot(CART.model.cv$size, CART.model.cv$dev, type = "b")
CART.model.cv

# Select the final model
CART.model.pruned <- prune.misclass(CART.model, best = 4)
summary(CART.model.pruned)
plot(CART.model.pruned)
text(CART.model.pruned, pretty = 1)

# Prediction
CART.prey.pruned <- predict(CART.model.pruned, CART.tst, type = "class")
CART.cfm2 <- table(CART.tst$heartYN, CART.prey.pruned)
CART.cfm2

Perf.Table[2,] <- perf_eval(CART.cfm2)
Perf.Table

#[Q3] 'rpart' ---------------------------------------------------
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01)
prp(rpart.model, type=4, extra=2, digits = 2)

# minsplit variation
rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01, minsplit=4)
prp(rpart.model, type=4, extra=2, digits = 2)

# minbucket variation
rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01, minbucket=5)
prp(rpart.model, type=4, extra=2, digits = 2)

rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01, minbucket=15)
prp(rpart.model, type=4, extra=2, digits = 2)

# maxdepth variation
rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01, maxdepth=3)
prp(rpart.model, type=4, extra=2, digits = 2)

rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01, maxdepth=2)
prp(rpart.model, type=4, extra=2, digits = 2)

# Pruning
rpart.model <- rpart(heartYN ~ ., CART.trn, xval=10, cp=0.01)
printcp(rpart.model)
plotcp(rpart.model)

rpart.model.pruned <- prune(rpart.model, cp= rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),"CP"])
prp(rpart.model.pruned, type=4, extra=2, digits = 2, roundint = FALSE)

# [Q3-4] plotting
par(mfrow = c(2,2))
prp(rpart.model.pruned, type=5, extra=2, digits = 2)
prp(rpart.model.pruned, type=3, extra=2, digits = 2)
prp(rpart.model.pruned, type=2, extra=2, digits = 2)
prp(rpart.model.pruned, type=1, extra=2, digits = 2)
dev.off()
# Prediction
rpart.prey.pruned <- predict(rpart.model.pruned, CART.tst, type = "class")
CART.cfm3 <- table(CART.tst$heartYN, rpart.prey.pruned)
CART.cfm3

Perf.Table[3,] <- perf_eval(CART.cfm3)
Perf.Table

#[Q3] 'party' -----------------------------------------------------
install.packages("party")
library(party)

party.model <- ctree(heartYN ~ ., CART.trn)
plot(party.model)

# options variation
party.model <- ctree(heartYN ~ ., CART.trn, control = ctree_control(mincriterion = 0.99))
plot(party.model)
party.model <- ctree(heartYN ~ ., CART.trn, control = ctree_control(mtry = 3))
plot(party.model)
party.model <- ctree(heartYN ~ ., CART.trn, control = ctree_control(mtry = 4))
plot(party.model)

#[Q3-4]
party.model <- ctree(heartYN ~ ., CART.trn)
plot(party.model, type = "simple")

# Prediction
party.prey <- predict(party.model, CART.tst)
CART.cfm4 <- table(CART.tst$heartYN, party.prey)
CART.cfm4

Perf.Table[4,] <- perf_eval(CART.cfm4)
Perf.Table

#[Q3] 'evtree' ---------------------------------------------
install.packages("evtree")
library(evtree)

evtree.model <- evtree(heartYN ~ ., CART.trn)
plot(evtree.model)

#[Q3-3] option variation
evtree.model <- evtree(heartYN ~ ., CART.trn)
plot(evtree.model)
evtree.model <- evtree(heartYN ~ ., CART.trn, niterations=50)
plot(evtree.model)
evtree.model <- evtree(heartYN ~ ., CART.trn, ntrees = 10)
plot(evtree.model)
evtree.model <- evtree(heartYN ~ ., CART.trn, operatorprob = list(pmutatemajor = 0.5, pmutateminor = 0.2,
                                                                  pcrossover = 0.01))
plot(evtree.model)


#[Q3-4] Plotting
evtree.model <- evtree(heartYN ~ ., CART.trn)
plot(evtree.model, type ="simple")

# Prediction
evtree.prey <- predict(evtree.model, CART.tst)
CART.cfm5 <- table(CART.tst$heartYN, evtree.prey)
CART.cfm5

Perf.Table[5,] <- perf_eval(CART.cfm5)
Perf.Table
