rm(list=ls(all=TRUE))

setwd("J:/SMU/MSDS 6306 Doing Data Science/Lecture Assignments/Live Session Case Study 2")
setwd("Q:/SMU/MSDS 6306 Doing Data Science/Lecture Assignments/Live Session Case Study 2")
setwd("I:/SMU/MSDS 6306 Doing Data Science/Lecture Assignments/Live Session Case Study 2")
setwd("P:/SMU/MSDS 6306 Doing Data Science/Lecture Assignments/Live Session Case Study 2")

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
utils::View(rawdata)
str(rawdata)

# write out header as list for review
header <- colnames(rawdata, do.NULL = TRUE, prefix = "col")
utils::View(header)

# Clean data

# Convert numeric categorical variables to character 
rawdata$Education <- mapvalues(rawdata$Education, from=c(1, 2, 3, 4, 5), to=c('Below College', 'College', 'Bachelor', 'Master', 'Doctor'))
rawdata$EnvironmentSatisfaction <- mapvalues(rawdata$EnvironmentSatisfaction, from=c(1, 2, 3, 4), to=c('Low', 'Medium', 'High', 'Very High'))
rawdata$JobInvolvement <- mapvalues(rawdata$JobInvolvement, from=c(1, 2, 3, 4), to=c('Low', 'Medium', 'High', 'Very High'))
rawdata$JobSatisfaction <- mapvalues(rawdata$JobSatisfaction, from=c(1, 2, 3, 4), to=c('Low', 'Medium', 'High', 'Very High'))
rawdata$PerformanceRating <- mapvalues(rawdata$PerformanceRating, from=c(3, 4), to=c('Excellent', 'Outstanding'))
rawdata$RelationshipSatisfaction <- mapvalues(rawdata$RelationshipSatisfaction, from=c(1, 2, 3, 4), to=c('Low', 'Medium', 'High', 'Very High'))
rawdata$WorkLifeBalance <- mapvalues(rawdata$WorkLifeBalance, from=c(1, 2, 3, 4), to=c('Bad', 'Good', 'Better', 'Best'))

# Convert character data to factor data
rawdata$Attrition <- as.factor(rawdata$Attrition)
rawdata$BusinessTravel <- as.factor(rawdata$BusinessTravel)
rawdata$Department <- as.factor(rawdata$Department)
rawdata$Education <- as.factor(rawdata$Education)
rawdata$EducationField <- as.factor(rawdata$EducationField)
rawdata$EnvironmentSatisfaction <- as.factor(rawdata$EnvironmentSatisfaction)
rawdata$JobInvolvement <- as.factor(rawdata$JobInvolvement)
rawdata$Gender <- as.factor(rawdata$Gender)
rawdata$JobRole <- as.factor(rawdata$JobRole)
rawdata$JobSatisfaction <- as.factor(rawdata$JobSatisfaction)
rawdata$MaritalStatus <- as.factor(rawdata$MaritalStatus)
rawdata$Over18 <- as.factor(rawdata$Over18)
rawdata$OverTime <- as.factor(rawdata$OverTime)
rawdata$PerformanceRating <- as.factor(rawdata$PerformanceRating)
rawdata$RelationshipSatisfaction <- as.factor(rawdata$RelationshipSatisfaction)
rawdata$WorkLifeBalance <- as.factor(rawdata$WorkLifeBalance)

utils::View(rawdata)
str(rawdata)

# ==============================================================================================================

# Bar Plots of variables
barplot(table(rawdata$Age), xlab = "Age", ylab = "Frequency", main = "Employee Age Distribution", col = "red")
barplot(table(rawdata$Gender), xlab = "Gender", ylab = "Frequency", main = "Employee Gender", col = "blue")
barplot(table(rawdata$Education), xlab = "Education Level", ylab = "Frequency", main = "Employee Education Level", col = "orange") 
barplot(table(rawdata$EducationField), xlab = "Education Field", ylab = "Frequency", main = "Employee Education Field", col = "green")

# ==============================================================================================================

# ggplot Bar Plots of categorical variables in percentages
ggplot(rawdata, aes(x = Gender)) + geom_bar(aes(fill=Gender, y=100*(..count..)/sum(..count..))) + xlab("Gender") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Attrition)
ggplot(rawdata, aes(x = Education)) + geom_bar(aes(fill=Education, y=100*(..count..)/sum(..count..)))

# experiments
ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition, y=100*(..count..)/sum(..count..))) + xlab("Attrition") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Gender)
ggplot(rawdata, aes(x = Attrition)) + geom_bar(aes(fill=Attrition, y=100*(..count..)/sum(..count..))) + xlab("Attrition") + ylab("Percent") + ggtitle("Employees by Gender") + facet_grid(~Gender)

