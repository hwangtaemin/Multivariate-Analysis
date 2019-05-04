#Assignment 1
library(arules)
library(arulesViz)
library(wordcloud)

# step 1
mooc_dataset<-read.csv("big_student_clear_third_version.csv")
Institute<-mooc_dataset$institute
Course<-mooc_dataset$course_id
Region<-mooc_dataset$final_cc_cname_DI
Degree<-mooc_dataset$LoE_DI
Region<-gsub(" ","",Region)
RawTransactions<-paste(Institute, Course, Region, Degree, sep='_')
MOOC_transactions<-paste(mooc_dataset$userid_DI, RawTransactions, sep=' ')
write.table(MOOC_transactions, file="MOOC_User_Course.csv", col.names = FALSE, row.names = FALSE, quote = FALSE)

# step 2
tmp_single <- read.transactions("MOOC_User_Course.csv", format = "single", cols = c(1,2), rm.duplicates = TRUE)
summary(tmp_single)
itemName <- itemLabels(tmp_single)
itemCount <- itemFrequency(tmp_single)*nrow(tmp_single)
col <- brewer.pal(9, "Reds")
wordcloud(words = itemName, freq = itemCount, min.freq = 500, scale = c(1, 0.2), col = col, random.order = FALSE)
itemFrequencyPlot(tmp_single, support = 0.01, cex.names=0.8)
itemFrequencyPlot(tmp_single, support = 0.01, cex.names=0.8, topN = 5)

# step 3
rules <- apriori(tmp_single, parameter=list(support=0.0005, confidence=0.05))
rules <- apriori(tmp_single, parameter=list(support=0.0005, confidence=0.1))
rules <- apriori(tmp_single, parameter=list(support=0.0005, confidence=0.15))

rules <- apriori(tmp_single, parameter=list(support=0.001, confidence=0.05))
rules <- apriori(tmp_single, parameter=list(support=0.001, confidence=0.1))
rules <- apriori(tmp_single, parameter=list(support=0.001, confidence=0.15))

rules <- apriori(tmp_single, parameter=list(support=0.0015, confidence=0.05))
rules <- apriori(tmp_single, parameter=list(support=0.0015, confidence=0.1))
rules <- apriori(tmp_single, parameter=list(support=0.0015, confidence=0.15))

rules <- apriori(tmp_single, parameter=list(support=0.001, confidence=0.05))
inspect(rules)
inspect(sort(rules, by="support"))
inspect(sort(rules, by="confidence"))
inspect(sort(rules, by="lift"))
write.csv(as(rules, "data.frame"), "MOOC_rules.csv", row.names = FALSE)

df_rules=data.frame( lhs = labels(lhs(rules)), rhs = labels(rhs(rules)), rules@quality)
new_criteria=c(df_rules$support*df_rules$confidence*df_rules$lift)
df_rules=cbind(df_rules,new_criteria)
head(df_rules[order(-df_rules$new_criteria),],3)

plot(rules, method = "graph")
plot(rules, method = "graph", engine = "interactive")

# [Extra Question]
plot(rules, method="matrix", engine = "3d")
plot(rules, method="matrix", shading=c("lift", "confidence"))
plot(rules, method="graph", engine="htmlwidget", 
     igraphLayout = "layout_in_circle")
plot(rules, method = "grouped", gp_labels = gpar(cex=0.4))
