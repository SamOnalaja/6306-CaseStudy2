library(pROC)
library(ROCR)
library(randomForest)
library(ggplot2)
library(reshape2)
library(caret)
# Add libraries
library(dplyr)
library(RColorBrewer)
#library(lattice)
#library(sm)
# Load Data
df <- read.csv("CaseStudy2data.csv")
dfCat <- df[,names(base::Filter(is.factor,df))]
dfCon <- df[,c("Attrition",names(base::Filter(is.integer,df)))]
head(dfCat)
summary(df)



# NA per Field
NACount <- sapply(df,function(x) sum(is.na(x)))
NACount <- as.data.frame(NACount)
colnames(NACount)=c("CountOfNAs")
NACount


#remove reduntant fields

#remove reduntant fields
# 9 - Employee Count
# 10 - Employee Number
# 22 - Over18
# 27 - Standard Hours
cleanAttDf <- df[,c(-9,-10,-22,-27)]
colnames(cleanAttDf)

# change variables as factors
cleanAttDf$JobLevel <- as.factor(cleanAttDf$JobLevel)
cleanAttDf$JobInvolvement <- as.factor(cleanAttDf$JobInvolvement)
cleanAttDf$EnvironmentSatisfaction <- as.factor(cleanAttDf$EnvironmentSatisfaction)
cleanAttDf$Education <- as.factor(cleanAttDf$Education)
cleanAttDf$JobSatisfaction <- as.factor(cleanAttDf$JobSatisfaction)
cleanAttDf$RelationshipSatisfaction <- as.factor(cleanAttDf$RelationshipSatisfaction)
cleanAttDf$StockOptionLevel <- as.factor(cleanAttDf$StockOptionLevel)
cleanAttDf$PerformanceRating <- as.factor(cleanAttDf$PerformanceRating)
cleanAttDf$WorkLifeBalance <- as.factor(cleanAttDf$WorkLifeBalance)

##### Branch for categorical histograms
orig_cleanAttDf <- cleanAttDf
orig_dfCat <- orig_cleanAttDf[,names(base::Filter(is.factor,orig_cleanAttDf))]


# Downsample the 'Yes' Attrition
set.seed(7)
cleanAttDfDs <- downSample(x=cleanAttDf, y=cleanAttDf$Attrition)

cleanAttDfDs_cat <- cleanAttDfDs[,names(base::Filter(is.factor,cleanAttDfDs))]
cleanAttDfDs_con <- cleanAttDfDs[,c("Attrition",names(base::Filter(is.integer,cleanAttDfDs)))]

cleanAttDfDs_conCorr <- cor(cleanAttDfDs_con[,2:15])

# Correlation heatmap
ggplot(data=melt(cleanAttDfDs_conCorr), aes(x=Var1,y=Var2,fill=value)) + geom_tile(color="white") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_gradient2(low="purple",high="Red",mid="#fcfcfc" ,
                       midpoint = 0, limit=c(-1,1), space = "Lab",
                       name="Pearson \nCorrelation") +
  ggtitle("Correlation Matrix for Continuous Variables") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#splom(cleanAttDfDs_con[,2:15], groups=cleanAttDfDs_con$Attrition, data=cleanAttDfDs_con, panel=panel.superpose)

#pairs(cleanAttDfDs_con[,2:15],col=cleanAttDfDs_con$Attrition)

#histograms for categorical data using cleaned original files

barPlot1 <- ggplot(data = melt(orig_dfCat[,1:7], id=c("Attrition")), mapping= aes(x=value, fill=Attrition)) + geom_histogram(stat="count")
barPlot1 <- barPlot1 + facet_grid(Attrition~variable, scales = "free")+ theme(axis.text.x = element_text(angle=90, hjust = 1)) + xlab("Categories") 

barPlot2 <- ggplot(data = melt(orig_dfCat[,c(1,8:13)], id=c("Attrition")), mapping= aes(x=value, fill=Attrition) ) + geom_histogram(stat="count")  
barPlot2 <- barPlot2 + facet_grid(Attrition~variable, scales = "free")+ theme(axis.text.x = element_text(angle=90, hjust = 1)) + xlab("Categories") 

barPlot3 <- ggplot(data = melt(orig_dfCat[,c(1,14:17)], id=c("Attrition")), mapping= aes(x=value, fill=Attrition) ) +geom_histogram(stat="count")  
barPlot3 <- barPlot3 + facet_grid(Attrition~variable, scales = "free")+ theme(axis.text.x = element_text(angle=90, hjust = 1)) + xlab("Categories")

barPlot1
barPlot2
barPlot3


### Model


#use the clean data set that is down sampled.
cleanAttDfDs
ncol(cleanAttDfDs)

clf_1 <- randomForest(cleanAttDfDs[,c(1,3:31)],cleanAttDfDs[,2])
impClf_1<- clf_1$importance
impClf_1

barplot(sort(impClf_1[,1]), horiz = "False", col=impClf_1)


#creates attractive random forest graph
clfdf<-cbind(rownames(impClf_1),impClf_1)
colnames(clfdf)<-c("variable","score")
clfdf<-as.data.frame(clfdf,row.names = F)
clfdf$score<-as.numeric(levels(clfdf$score))[clfdf$score]
clfdf <- transform(clfdf, variable=reorder(clfdf$variable, -clfdf$score) ) 

colourCount = length(unique(clfdf$variable))
getPalette = colorRampPalette(brewer.pal(11, "RdYlGn"))

ggplot(data=clfdf, aes(x=reorder(clfdf$variable,clfdf$score),y=clfdf$score,fill=clfdf$variable)) + geom_bar(stat="identity") + coord_flip() +
  labs(title="Random Forest model predictive factors",x="Predictor",y="Score") + 
  theme(plot.title = element_text(hjust = 0.3), legend.position ="none") + scale_fill_manual(values = getPalette(colourCount))




#Test ROC Random Forest
num_obs=nrow(cleanAttDfDs)

train_idx <- sample(c(1:num_obs),size=num_obs*0.7,replace = FALSE)

clf_2 = randomForest(cleanAttDfDs[train_idx,c(1,3:31)],cleanAttDfDs[train_idx,2])
predicAtt <- predict(clf_2,cleanAttDfDs[-train_idx,c(1,3:31)], type="prob" )
plot(roc(cleanAttDfDs[-train_idx,2], as.numeric(predicAtt[,1])))

auc.clf2 <- roc(cleanAttDfDs[-train_idx,2], as.numeric(predicAtt[,1]),auc=TRUE)
auc.clf2$auc

nloops <- 100
cv.aucs <-c()
cv.top3pred <- c()
for (i in 1:nloops){
  train_idx <- sample(c(1:num_obs),size=num_obs*0.7,replace = FALSE)
  clf_3 = randomForest(cleanAttDfDs[train_idx,c(1,3:31)],cleanAttDfDs[train_idx,2])
  predicAtt <- predict(clf_3,cleanAttDfDs[-train_idx,c(1,3:31)], type="prob" )
  auc.clf3 <- roc(cleanAttDfDs[-train_idx,2], as.numeric(predicAtt[,1]),auc=TRUE)
  #plot(roc(cleanAttDfDs[-train_idx,2], as.numeric(predicAtt[,1])))
  cv.aucs[i]<-as.numeric(auc.clf3$auc)
  Pred_Results <- clf_3$importance
  sortedResults <- as.matrix(Pred_Results[order(Pred_Results[,1], decreasing = "TRUE"),])
  Top3Pred <- rownames(sortedResults)
  cv.top3pred<-rbind(cv.top3pred,Top3Pred[1:3])
}
cv.top3pred

ggplot(melt(cv.top3pred), mapping = aes(x=value,fill="count")) + geom_histogram(stat = "count") + 
  theme(axis.text.x = element_text(angle=90, hjust = 1),legend.position = "none") + 
  labs(title="Random Forest model top 3 predictor occurences",x="Predictor",y="Count")


qplot(cv.aucs,geom="histogram",binwidth=.008,main = "Histogram for random forest model", fill=I("Blue"), col=I("Gray"), alpha=I(.8)) + geom_density(col=2)
cv.aucs

summary(cv.aucs)

# Result plot

sortedResultsHist <- as.data.frame(sortedResults)
sortedResultsHist <- rename(x = sortedResultsHist, replace = "V1")
sortedResultsHist["Factor"] <- rownames(sortedResultsHist)
sortedResultsTop3 <- dplyr::slice(sortedResultsHist, 1:3)

# ggplot Top 3 Factors 
# Barplot

ggplot(sortedResultsTop3, aes(x=Factor, fill=(sortedResultsTop3$Factor), y=V1)) + 
   labs(title="Top 3 predictor results",x="Predictor",y="Importance") + geom_bar(stat = "identity") + 
   theme(plot.title = element_text(hjust = 0.4), legend.position ="none") + scale_fill_manual(values=c("#5F9EA0","#4682B4","#00BFFF"))



cleanAttDf$MonthlyIncome



# Generate property summary for variable
MonthlyIncomeTable <- table(cleanAttDf$MonthlyIncome, cleanAttDf$Attrition)
PTdf <- as.data.frame(prop.table(MonthlyIncomeTable))
PTdf <- plyr::rename(PTdf, c("Var1"= "MonthlyIncome", "Var2"= "Attrition"))
df <- as.data.frame(prop.table(MonthlyIncomeTable))
df <- plyr::rename(df, c("Var1"= "MonthlyIncome", "Var2"= "Attrition"))

kable(df, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
# Plot variable vs Attrition Percent
g <- ggplot(PTdf, aes(x = (MonthlyIncome), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
MonthlyIncomeBP <- g + xlab("MonthlyIncome") + ylab("Percent") + ggtitle("Attrition vs. MonthlyIncome")
MonthlyIncomeBP

# Generate property summary for variable


##########



## Subset Total Working Years by 5, categorical vector
cleanAttDf$YearsAtCompanyby5 <- findInterval(cleanAttDf$YearsAtCompany, c(0, 5, 10, 15, 20, 25, 30, 35))
cleanAttDf$YearsAtCompanyby5 <- mapvalues(cleanAttDf$YearsAtCompanyby5, from=c(1, 2, 3, 4, 5, 6, 7, 8), to=c('0-04', '05-09', '10-14', '15-19', '20-24', '25-29', '30-34', '35-40'))
cleanAttDf$YearsAtCompanyby5 <- as.factor(cleanAttDf$YearsAtCompanyby5)

# Generate property summary for variable
YearsAtCompanyby5Table <- table(cleanAttDf$YearsAtCompanyby5, cleanAttDf$Attrition)
PTdf <- as.data.frame(prop.table(YearsAtCompanyby5Table))
PTdf <- plyr::rename(PTdf, c("Var1"= "YearsAtCompanyby5", "Var2"= "Attrition"))
df <- as.data.frame(prop.table(YearsAtCompanyby5Table))
df <- plyr::rename(df, c("Var1"= "YearsAtCompanyby5", "Var2"= "Attrition"))

kable(df, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Plot variable vs Attrition Percent
g <- ggplot(PTdf, aes(x = YearsAtCompanyby5, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
YearsAtCompanyby5BP <- g + xlab("Years At Company") + ylab("Percent") + ggtitle("Attrition vs. Years At Company")
YearsAtCompanyby5BP





########

binasdf<-seq(0,30000,by=1000)
binasdf
## Subset Total Working Years by 5, categorical vector
cleanAttDf$MonthlyIncomeBy1000 <- findInterval(cleanAttDf$MonthlyIncome, binasdf)
cleanAttDf$MonthlyIncomeBy1000 <- mapvalues(cleanAttDf$MonthlyIncomeBy1000, from=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), to=c('$2000','$3000','$4000','$5000','$6000','$7000','$8000','$9000','$10000','$11000','$12000','$13000','$14000','$15000','$16000','$17000','$18000','$19000','$20000'))
cleanAttDf$MonthlyIncomeBy1000 <- as.factor(cleanAttDf$MonthlyIncomeBy1000)

# Generate property summary for variable
MI1000Table <- table(cleanAttDf$MonthlyIncomeBy1000, cleanAttDf$Attrition)
PTdf <- as.data.frame(prop.table(MI1000Table))
PTdf <- plyr::rename(PTdf, c("Var1"= "MonthlyIncomeBy1000", "Var2"= "Attrition"))
df <- as.data.frame(prop.table(MI1000Table))
df <- plyr::rename(df, c("Var1"= "MonthlyIncomeBy1000", "Var2"= "Attrition"))

kable(df, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Plot variable vs Attrition Percent
g <- ggplot(PTdf, aes(x = MonthlyIncomeBy1000 , y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
MI1000BP <- g + xlab("Monthly income") + ylab("Percent") + ggtitle("Attrition vs. Years At Company")
MI1000BP


qplot(cleanAttDf$MonthlyIncome,geom="histogram",binwidth=1000,main = "Histogram for monthly income", fill=I("Blue"), col=I("Gray"), alpha=I(.8))






