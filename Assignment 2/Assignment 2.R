#Assignment 2
library(ISLR)
library(ggplot2)
library(gplots)
library(pheatmap)
library(RColorBrewer)
library(clValid)
library(plotrix)
library(cluster)

data(College)
View(College)

College_class <- College[,1]
College_x <- College[,-1]

#K-Means Clustering
#[Q1-1]
College_x_scaled <- scale(College_x, center = TRUE, scale = TRUE)
start_time <- Sys.time()
College_clValid <- clValid(College_x_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"))
y
summary(College_clValid)
end_time <- Sys.time()
end_time - start_time
#[Q1-2]
for(i in (1:10)){
  College_kmc <- kmeans(College_x_scaled,3)
  print(College_kmc$centers)
  print(College_kmc$size)
}
#[Q1-3]
for(i in (1:10)){
  College_kmc <- kmeans(College_x_scaled,10)
  print(College_kmc$centers)
  print(College_kmc$size)
}
#[Q1-4]
College_kmc <- kmeans(College_x_scaled,3)
cluster_kmc <- data.frame(College_x_scaled, clusterID = as.factor(College_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(College_x)
kmc_summary

par(mfrow = c(1,3))
  for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()
#[Q1-5]
kmc_cluster1 <- College_x[College_kmc$cluster == 1,]
kmc_cluster2 <- College_x[College_kmc$cluster == 2,]
kmc_cluster3 <- College_x[College_kmc$cluster == 3,]

kmc_t_result1 <- data.frame()
kmc_t_result2 <- data.frame()
kmc_t_result3 <- data.frame()
for (i in 1:17){
  
  kmc_t_result1[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result1[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result1[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}
for (i in 1:17){
  
  kmc_t_result2[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result2[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result2[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "less")$p.value
}
for (i in 1:17){
  
  kmc_t_result3[i,1] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result3[i,2] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result3[i,3] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "less")$p.value
}
rownames(kmc_t_result1) <- colnames(College_x)
rownames(kmc_t_result2) <- colnames(College_x)
rownames(kmc_t_result3) <- colnames(College_x)
kmc_t_result1
kmc_t_result2
kmc_t_result3
#[Q1-6]
fviz_cluster(College_kmc, College_x_scaled, geom = "text")

#Hierarchical clustering
#[Q2-1]
start_time <- Sys.time()
College_clValid <- clValid(College_x_scaled, 2:10, clMethods = "hierarchical", 
                           validation = c("internal", "stability"))
y
summary(College_clValid)
end_time <- Sys.time()
end_time - start_time
#[Q2-2]
cor_Mat <- cor(t(College_x_scaled), method = "spearman")
dist_College <- as.dist(1-cor_Mat)
hr <- hclust(dist_College, method = "complete", members = NULL)
plot(hr)
hr <- hclust(dist_College, method = "single", members = NULL)
plot(hr)
hr <- hclust(dist_College, method = "ward.D", members = NULL)
plot(hr)
hr <- hclust(dist_College, method = "average", members = NULL)
plot(hr)
hr <- hclust(dist_College, method = "centroid", members = NULL)
plot(hr)
#[Q2-3]
hr <- hclust(dist_College, method = "complete", members = NULL)
mycl <- cutree(hr, k=10)
mycl
plot(hr)
rect.hclust(hr, k=10, border = "red")

College_hc <- data.frame(College_x_scaled, clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(College_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(College_hc[,i], College_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- c(colnames(College_x))
hc_summary

par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

hc_cluster3 <- College_hc[College_hc$clusterID == 3,]
hc_cluster8 <- College_hc[College_hc$clusterID == 8,]
hc_cluster2 <- College_hc[College_hc$clusterID == 2,]
hc_cluster7 <- College_hc[College_hc$clusterID == 7,]
hc_t_result1 <- data.frame()
hc_t_result2 <- data.frame()
for (i in 1:17){
  
  hc_t_result1[i,1] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                               alternative = "two.sided")$p.value
  
  hc_t_result1[i,2] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                               alternative = "greater")$p.value
  
  hc_t_result1[i,3] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                               alternative = "less")$p.value
}
for (i in 1:17){
  
  hc_t_result2[i,1] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                              alternative = "two.sided")$p.value
  
  hc_t_result2[i,2] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                              alternative = "greater")$p.value
  
  hc_t_result2[i,3] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                              alternative = "less")$p.value
}
rownames(hc_t_result1) <- colnames(College_x)
rownames(hc_t_result2) <- colnames(College_x)
hc_t_result1
hc_t_result2
#[Q2-4]
pheatmap(cor_Mat, color = brewer.pal(7,"RdYlBu"), fontsize = 0.2)

#DBSCAN
#[Q3-1]
library(factoextra)
library(dbscan)

ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]
ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)
for(i in (1:5)){
  for(j in (1:5)){
    DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2 + 0.5*i, minPts = 3 + j)
    print(DBSCAN_ploan)
  }
}

#[Q3-2]
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 4)
cluster_dbscan <- data.frame(ploan_x_scaled, clusterID = as.factor(DBSCAN_ploan$cluster))
dbscan_summary <- data.frame()

for (i in 1:(ncol(cluster_dbscan)-1)){
  dbscan_summary = rbind(dbscan_summary, 
                      tapply(cluster_dbscan[,i], cluster_dbscan$clusterID, mean))
}
colnames(dbscan_summary)[1] <- "noise"
colnames(dbscan_summary)[2:5] <- paste("cluster", c(1:4))
rownames(dbscan_summary) <- colnames(ploan_x)
dbscan_summary

DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 5)
par(mfrow = c(1,4))
for (i in 2:5){
  plot_title <- paste("Radar Chart for Cluster", i-1, sep=" ")
  radial.plot(dbscan_summary[,i], labels = rownames(dbscan_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 4, show.grid.labels=1)
}
dev.off()
#[Q3-3]
fviz_cluster(DBSCAN_ploan, ploan_x_scaled, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)

#[Extra Question]
data("USArrests")
help("USArrests")
USArrests
df <- scale(USArrests)
res.fanny <- fanny(df, 2)
head(res.fanny$clustering)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")
fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())
