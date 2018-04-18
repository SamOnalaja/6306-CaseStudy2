library(data.table)
library(readxl)
library(doBy)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(colorRamps)

# Read xlsx file
rawdata <- read_excel("CaseStudy2data.xlsx")
str(rawdata)

# write out header as list for review
header <- colnames(rawdata, do.NULL = TRUE, prefix = "col")

# Clean data

# Convert numeric categorical variables to character 

# ggplot Bar Plots of categorical variables in percentages
ggplot(rawdata, aes(x = Gender)) + geom_bar(aes(fill=Gender, y=100*(..count..)/sum(..count..))) + xlab("Gender") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Attrition)
ggplot(rawdata, aes(x = Education)) + geom_bar(aes(fill=Education, y=100*(..count..)/sum(..count..)))

# experiments
ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition, y=100*(..count..)/sum(..count..))) + xlab("Attrition") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Gender)
ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition, y=100*(..count..)/sum(..count..))) + xlab("Attrition") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Gender)







ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Employees by dept") + facet_grid(~Department)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Job role attrition") + facet_grid(~JobRole)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Travel level") + facet_grid(~BusinessTravel)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Overtime eligible") + facet_grid(~OverTime)

#ignoring percent salary hike

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("PerformanceRating") + facet_grid(~PerformanceRating)

#remake as stacked bar
ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Training events over last year") + facet_grid(~TrainingTimesLastYear)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Years at company") + facet_grid(~YearsAtCompany)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Years in role") + facet_grid(~YearsInCurrentRole)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Years since promotion") + facet_grid(~YearsSinceLastPromotion)

ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition)) + 
  xlab("Attrition") + ylab("count") + ggtitle("Years with manager") + facet_grid(~YearsWithCurrManager)



#start dem graphs


asdf<-as.data.frame.table(table(rawdata$Attrition,rawdata$YearsAtCompany))
colnames(asdf)<-c("Attrition","YearsAtCompany","Count")

ggplot(asdf,aes(x = YearsAtCompany,y = Count,fill = Attrition)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) + ggtitle("years at company")

asdf<-as.data.frame.table(table(rawdata$Attrition,rawdata$YearsAtCompany))
colnames(asdf)<-c("Attrition","YearsAtCompany","Count")

ggplot(asdf,aes(x = YearsAtCompany,y = Count,fill = Attrition)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) + ggtitle("years at company")

asdf2<-as.data.frame.table(table(rawdata$Attrition,rawdata$TrainingTimesLastYear))
colnames(asdf2)<-c("Attrition","TrainingTimes","Count")

ggplot(asdf2,aes(x = TrainingTimes,y = Count,fill = Attrition)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) + ggtitle("Training Times Last year")

asdf3<-as.data.frame.table(table(rawdata$Attrition,rawdata$TrainingTimesLastYear))
colnames(asdf2)<-c("Attrition","TrainingTimes","Count")

ggplot(asdf3,aes(x = TrainingTimes,y = Count,fill = Attrition)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) + ggtitle("Training Times Last year")







