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
library(readxl)
library(pROC)
library(randomForest)
library(reshape2)
library(forcats)


#  Clean file name (Edwardo) - cleanAttDf


# Read xlsx file
rawdata <- read_excel("CaseStudy2data.xlsx")
utils::View(rawdata)
str(rawdata)

# Data cleaning for Analysis

# NA per Field
NACount <- sapply(rawdata,function(x) sum(is.na(x)))
NACount <- as.data.frame(NACount)
colnames(NACount)=c("CountOfNAs")
NACount


#remove reduntant fields
# 9 - Employee Count
# 10 - Employee Number
# 22 - Over18
# 27 - Standard Hours

rawdata <- rawdata[,c(-9,-10,-22,-27)]
colnames(rawdata)

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
rawdata$OverTime <- as.factor(rawdata$OverTime)
rawdata$PerformanceRating <- as.factor(rawdata$PerformanceRating)
rawdata$RelationshipSatisfaction <- as.factor(rawdata$RelationshipSatisfaction)
rawdata$WorkLifeBalance <- as.factor(rawdata$WorkLifeBalance)

cleanAttDf <- rawdata

# write out header as list for review
header <- colnames(cleanAttDf, do.NULL = TRUE, prefix = "col")
utils::View(header)




# Exploratory Analysis ======================================================================================================



AttritionTable <- table(cleanAttDf$Attrition)
PT <- prop.table(AttritionTable)
PT

# ggplot Bar Plot (percentage)
g <- ggplot(cleanAttDf, aes(x = Attrition)) + geom_bar(aes(fill=Attrition, y=100*(..count..)/sum(..count..))) 
AttritionBCP <- g + xlab("Attrition") + ylab("Percent") + ggtitle("Total Attrition") 
AttritionBCP

## Attrition rate: 16%

# ===Age=====================================================================================================================================================

## Subset Age by 5, categorical vector
# 18-22,23-27,28-32,33-37,38-42,43-47,48-52,53+
cleanAttDf$Ageby5 <- findInterval(cleanAttDf$Age, c(18, 23, 28, 33, 38, 43, 48, 53))
cleanAttDf$Ageby5 <- mapvalues(cleanAttDf$Ageby5, from=c(1, 2, 3, 4, 5, 6, 7, 8), to=c('18-22', '23-27', '28-32', '33-37', '38-42', '43-47','48-52','53+'))
cleanAttDf$Ageby5 <- as.factor(cleanAttDf$Ageby5)

# Generate property summary for variable
AgeTable <- table(cleanAttDf$Ageby5, cleanAttDf$Attrition)
PT <- prop.table(AgeTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "Age" = Var1, "Attrition" = Var2)
PT

# Plot Age Range vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = Age, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
AgeBP <- g + xlab("Age") + ylab("Percent") + ggtitle("Attrition by Age")
AgeBP

## Notes
# 47% Attrition in the 18-23 age bracket

# ===Business Travel=====================================================================================================================================================

# Generate property summary for variable
BusinessTravelTable <- table(cleanAttDf$BusinessTravel, cleanAttDf$Attrition)
PT <- prop.table(BusinessTravelTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "BusinessTravel" = Var1, "Attrition" = Var2)
PT

# Plot Marital Status vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(BusinessTravel, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
BusinessTravelBP <- g + xlab("Business Travel") + ylab("Percent") + ggtitle("Attrition vs. Business Travel")
BusinessTravelBP

## Notes
# Minimal Factor

# ===DailyRate=====================================================================================================================================================

## Subset Daily Rate by 200, categorical vector
cleanAttDf$DailyRateby200 <- findInterval(cleanAttDf$DailyRate, c(0,200,400,600,800,1000,1200,1400))
cleanAttDf$DailyRateby200 <- mapvalues(cleanAttDf$DailyRateby200, from=c(1, 2, 3, 4, 5, 6, 7, 8), to=c('0-199' ,'200-399', '400-599', '600-799', '800-999', '1000-1199', '1200-1399','1400+'))
cleanAttDf$DailyRateby200 <- as.factor(cleanAttDf$DailyRateby200)

# Generate property summary for variable
DailyRateby200Table <- table(cleanAttDf$DailyRateby200, cleanAttDf$Attrition)
PT <- prop.table(DailyRateby200Table)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "DailyRateby200" = Var1, "Attrition" = Var2)
levels(PTdf$DailyRateby200) <- c('0-199' ,'200-399', '400-599', '600-799', '800-999', '1000-1199', '1200-1399','1400+')
PT

# Plot Age Range vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = (DailyRateby200), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
DailyRateby200BP <- g + xlab("Daily Rate ($)") + ylab("Percent") + ggtitle("Attrition vs.Daily Rate")
DailyRateby200BP

## Notes
# Daily Rate not a big factor

# ===Department=====================================================================================================================================================

# Generate property summary for variable
DepartmentTable <- table(cleanAttDf$Department, cleanAttDf$Attrition)
PT <- prop.table(DepartmentTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "Department" = Var1, "Attrition" = Var2)
PT

# Plot Department vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(Department, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
DepartmentBP <- g + xlab("Department") + ylab("Percent") + ggtitle("Attrition by Department")
DepartmentBP

# ===Distance From Home============================================================================================
DistanceFromHomeTable <- table(cleanAttDf$DistanceFromHome)
prop.table(DistanceFromHomeTable)

## Subset Distance by 5, categorical vector
# '1-5', '6-10', '11-15', '16-20', '21-25', '25+' 
cleanAttDf$DistanceFromHomeby5 <- findInterval(cleanAttDf$DistanceFromHome, c(0, 5, 10, 15, 20, 25))
cleanAttDf$DistanceFromHomeby5 <- mapvalues(cleanAttDf$DistanceFromHomeby5, from=c(1, 2, 3, 4, 5, 6), to=c('01-05', '06-10', '11-15', '16-20', '21-25', '25+'))
cleanAttDf$DistanceFromHomeby5 <- as.factor(cleanAttDf$DistanceFromHomeby5)

# Generate property summary for variable
DistanceFromHomeby5Table <- table(cleanAttDf$DistanceFromHomeby5, cleanAttDf$Attrition)
PT <- prop.table(DistanceFromHomeby5Table)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "DistanceFromHomeby5" = Var1, "Attrition" = Var2)
PT

# Plot Distance From Home vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = DistanceFromHomeby5, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
DistanceFromHomeby5BP <- g + xlab("Distance From Home") + ylab("Percent") + ggtitle("Attrition vs. Distance From Home")
DistanceFromHomeby5BP

## Notes
# Most very close - over half within 7 miles
# Poor factor 


# ===Education============================================================================================

## Education (39%B/12%BC/19%C/3%D/27%M)

# Generate property summary for variable
EducationTable <- table(cleanAttDf$Education, cleanAttDf$Attrition)
PT <- prop.table(EducationTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "Education" = Var1, "Attrition" = Var2)
PT

# Plot Education vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(Education, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
EducationLevelBP <- g + xlab("Education Level") + ylab("Percent") + ggtitle("Attrition by Education Level")
EducationLevelBP

## Notes
# 39% Bachelor, 27% Masters, 19% College, 12% Below College, 3% Doctorate
# Highly educated workforce (69% degreed employees)
# Flat response across Education Levels, except lower for Doctors 


# ===Education Field============================================================================================

## Education Field (41%LS/32%Med/11%Mar/9%Tech/5%Other/2%HR)

# Generate property summary for variable
EducationFieldTable <- table(cleanAttDf$EducationField, cleanAttDf$Attrition)
PT <- prop.table(EducationFieldTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "EducationField" = Var1, "Attrition" = Var2)
PT

# Plot Education Field vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(EducationField, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
EducationFieldBP <- g + xlab("Education Field") + ylab("Percent") + ggtitle("Attrition by Education Field")
EducationFieldBP

## Notes
# 41% Life Science, 32% Medical, 11% Marketing, 9% Technical Degree, 5% Other, 3% HR
# Fairly even distribution


# ===Environment Satisfaction============================================================================================

# Generate property summary for variable
EnvironmentSatisfactionTable <- table(cleanAttDf$EnvironmentSatisfaction, cleanAttDf$Attrition)
PT <- prop.table(EnvironmentSatisfactionTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "EnvironmentSatisfaction" = Var1, "Attrition" = Var2)
PT

# Plot Environment Satisfaction vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(EnvironmentSatisfaction, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
EnvironmentSatisfactionBP <- g + xlab("Environment Satisfaction") + ylab("Percent") + ggtitle("Attrition vs. Environment Satisfaction ")
EnvironmentSatisfactionBP

## Notes
# Fairly even distribution

# ===Gender============================================================================================

# Generate property summary for variable
GenderTable <- table(cleanAttDf$Gender, cleanAttDf$Attrition)
PT <- prop.table(GenderTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "Gender" = Var1, "Attrition" = Var2)
PT

# Plot Gender vs, Attrition Percent
g <- ggplot(PTdf, aes(x = Gender, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
GenderBP <- g + xlab("Gender") + ylab("Percent") + ggtitle("Attrition by Gender")
GenderBP

## Notes
# 60% Male / 40% Female

# ===Hourly Rate============================================================================================

## Subset Daily Rate by 200, categorical vector
cleanAttDf$HourlyRateby10 <- findInterval(cleanAttDf$HourlyRate, c(30, 40, 50, 60, 70, 80, 90))
cleanAttDf$HourlyRateby10 <- mapvalues(cleanAttDf$HourlyRateby10, from=c(1, 2, 3, 4, 5, 6, 7), to=c('30-39' ,'40-49', '50-59', '60-69', '70-79', '80-89', '90-100'))
cleanAttDf$HourlyRateby10 <- as.factor(cleanAttDf$HourlyRateby10)

# Generate property summary for variable
HourlyRateby10Table <- table(cleanAttDf$HourlyRateby10, cleanAttDf$Attrition)
PT <- prop.table(HourlyRateby10Table)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "HourlyRateby10" = Var1, "Attrition" = Var2)
PT

# Plot Age Range vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = HourlyRateby10, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
HourlyRateby10BP <- g + xlab("Hourly Rate") + ylab("Percent") + ggtitle("Attrition vs. Hourly Rate ")
HourlyRateby10BP

## Notes
# Hourly Rate not a big factor

# ===JobInvolvement============================================================================================

# Generate property summary for variable
JobInvolvementTable <- table(cleanAttDf$JobInvolvement, cleanAttDf$Attrition)
PT <- prop.table(JobInvolvementTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "JobInvolvement" = Var1, "Attrition" = Var2)
PT

# Plot Job Involvement vs. Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(JobInvolvement, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
JobInvolvementBP <- g + xlab("Job Involvement") + ylab("Percent") + ggtitle("Attrition vs. Job Involvement")
JobInvolvementBP

# ===Job Level============================================================================================
# Generate property summary for variable
JobLevelTable <- table(cleanAttDf$JobLevel, cleanAttDf$Attrition)
PT <- prop.table(JobLevelTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "JobLevel" = Var1, "Attrition" = Var2)
PT

# Plot Job Level vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(JobLevel, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
JobLevelBP <- g + xlab("Job Level") + ylab("Percent") + ggtitle("Attrition vs. Job Level")
JobLevelBP

# Job level 1 has higher attrition levels

# ===Job Role============================================================================================
# Generate property summary for variable
JobRoleTable <- table(cleanAttDf$JobRole, cleanAttDf$Attrition)
PT <- prop.table(JobRoleTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "JobRole" = Var1, "Attrition" = Var2)
PT

# Plot Job Role vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(JobRole, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
g <- g + xlab("Job Role") + ylab("Percent") + ggtitle("Attrition vs. Job Role")
JobLevelBP <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
JobLevelBP

# ===Job Satisfaction============================================================================================

# Generate property summary for variable
JobSatisfactionTable <- table(cleanAttDf$JobSatisfaction, cleanAttDf$Attrition)
PT <- prop.table(JobSatisfactionTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "JobSatisfaction" = Var1, "Attrition" = Var2)
PT

# Plot Job Satisfaction vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(JobSatisfaction, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
JobLevelBP <- g + xlab("Job Satisfaction") + ylab("Percent") + ggtitle("Attrition vs Job Satisfaction")
JobLevelBP

# ===Marital Status============================================================================================

# Generate property summary for variable
MaritalStatusTable <- table(cleanAttDf$MaritalStatus, cleanAttDf$Attrition)
PT <- prop.table(MaritalStatusTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "MaritalStatus" = Var1, "Attrition" = Var2)
PT

# Plot Marital Status vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(MaritalStatus, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
MaritalStatusBP <- g + xlab("Marital Status") + ylab("Percent") + ggtitle("Attrition by Marital Status")
MaritalStatusBP

## Notes
# 46% married, 32% single, 22% divorced
# single population may be a factor


# ===Number of Companies Worked============================================================================================

## Number of Companies Worked

# Generate property summary for variable
NumCompaniesWorkedTable <- table(cleanAttDf$NumCompaniesWorked, cleanAttDf$Attrition)
PT <- prop.table(NumCompaniesWorkedTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "NumCompaniesWorked" = Var1, "Attrition" = Var2)
PT

# Plot Number of Companies Worked vs, Attrition Percent
g <- ggplot(PTdf, aes(x = NumCompaniesWorked, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
g + xlab("Number of Companies Worked") + ylab("Percent") + ggtitle("Attrition vs. Number of Companies Worked")


## Notes
# Many employees from one other company

# ===OverTime============================================================================================

# Generate property summary for variable
OverTimeTable <- table(cleanAttDf$OverTime, cleanAttDf$Attrition)
PT <- prop.table(OverTimeTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "OverTime" = Var1, "Attrition" = Var2)
PT

# Plot Over Time vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(OverTime, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
OverTimeBP <- g + xlab("Overtime") + ylab("Percent") + ggtitle("Attrition vs. Overtime")
OverTimeBP

## Notes
# Attrition higher with more overtime

# ===PercentSalaryHike============================================================================================

## Percent Salary Hike

# Generate property summary for variable
PercentSalaryHikeTable <- table(cleanAttDf$PercentSalaryHike, cleanAttDf$Attrition)
PT <- prop.table(PercentSalaryHikeTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "PercentSalaryHike" = Var1, "Attrition" = Var2)
PT

# Plot Number of Companies Worked vs, Attrition Percent
g <- ggplot(PTdf, aes(x = PercentSalaryHike, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
PercentSalaryHikeBP <- g + xlab("Percent Salary Hike") + ylab("Percent") + ggtitle("Attrition vs. Percent Salary Hike")
PercentSalaryHikeBP

## Notes
# Many employees from one other company


# ===PerformanceRating============================================================================================

# Generate property summary for variable
PerformanceRatingTable <- table(cleanAttDf$PerformanceRating, cleanAttDf$Attrition)
PT <- prop.table(PerformanceRatingTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "PerformanceRating" = Var1, "Attrition" = Var2)
PT

# Plot Gender vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(PerformanceRating, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
PerformanceRatingBP <- g + xlab("Performance Rating") + ylab("Percent") + ggtitle("Attrition vs. Performance Rating")
PerformanceRatingBP

## Notes
# Attrition noticably lower for outstanding performance rating


# ===RelationshipSatisfaction============================================================================================

# Generate property summary for variable
RelationshipSatisfactionTable <- table(cleanAttDf$RelationshipSatisfaction, cleanAttDf$Attrition)
PT <- prop.table(RelationshipSatisfactionTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "RelationshipSatisfaction" = Var1, "Attrition" = Var2)
PT

# Plot Environment Satisfaction vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(RelationshipSatisfaction, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
RelationshipSatisfactionBP <- g + xlab("Relationship Satisfaction") + ylab("Percent") + ggtitle("Attrition vs. Relationship Satisfaction")
RelationshipSatisfactionBP


# ===StockOptionLevel============================================================================================

# Generate property summary for variable
StockOptionLevelTable <- table(cleanAttDf$StockOptionLevel, cleanAttDf$Attrition)
PT <- prop.table(StockOptionLevelTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "StockOptionLevel" = Var1, "Attrition" = Var2)
PT

# Plot Gender vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(StockOptionLevel, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
StockOptionLevelBP <- g + xlab("Stock Option Level") + ylab("Percent") + ggtitle("Attrition vs. Stock Option Level")
StockOptionLevelBP


# ===Total Working Years============================================================================================

## Total Working Years
## Subset Total Working Years by 5, categorical vector
# 0-4,5-9,10-14,15-19,20-24,25-29,30-34,35-40
cleanAttDf$TotalWorkingYearsby5 <- findInterval(cleanAttDf$TotalWorkingYears, c(0, 5, 10, 20, 25, 30, 35, 40))
cleanAttDf$TotalWorkingYearsby5 <- as.factor(cleanAttDf$TotalWorkingYearsby5)
cleanAttDf$TotalWorkingYearsby5 <- mapvalues(cleanAttDf$TotalWorkingYearsby5, from=c(1, 2, 3, 4, 5, 6, 7, 8), to=c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29','30-34','35-40'))

# Generate property summary for variable
TotalWorkingYearsby5Table <- table(cleanAttDf$TotalWorkingYearsby5, cleanAttDf$Attrition)
PT <- prop.table(TotalWorkingYearsby5Table)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "TotalWorkingYearsby5" = Var1, "Attrition" = Var2)
PT

# Plot Total Working Years vs, Attrition Percent
g <- ggplot(PTdf, aes(x = TotalWorkingYearsby5, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
g + xlab("Total Working Years") + ylab("Percent") + ggtitle("Attrition vs. Total Working Years")

## Notes
# Under 10 years experience more likely to leave 
# Attrition after 29 working years statistically insignificant and may be related to retirement


# ===TrainingTimesLastYear============================================================================================

# Generate property summary for variable
TrainingTimesLastYearTable <- table(cleanAttDf$TrainingTimesLastYear, cleanAttDf$Attrition)
PT <- prop.table(TrainingTimesLastYearTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "TrainingTimesLastYear" = Var1, "Attrition" = Var2)
PT

# Plot Gender vs, Attrition Percent
g <- ggplot(PTdf, aes(x = TrainingTimesLastYear, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
TrainingTimesLastYearBP <- g + xlab("Training Times Last Year") + ylab("Percent") + ggtitle("Attrition vs. Training Times Last Year")
TrainingTimesLastYearBP

# Largely irrelevant

# ===WorkLifeBalance============================================================================================

# Generate property summary for variable
WorkLifeBalanceTable <- table(cleanAttDf$WorkLifeBalance, cleanAttDf$Attrition)
PT <- prop.table(WorkLifeBalanceTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "WorkLifeBalance" = Var1, "Attrition" = Var2)
PT

# Plot Work Life Balance vs, Attrition Percent
g <- ggplot(PTdf, aes(x = fct_reorder(WorkLifeBalance, Freq), y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
WorkLifeBalanceBP <- g + xlab("Work Life Balance") + ylab("Percent") + ggtitle("Attrition vs. Work Life Balance")
WorkLifeBalanceBP

# ===YearsAtCompany============================================================================================

## Subset Years At Company by 5, categorical vector
# 18-22,23-27,28-32,33-37,38-42,43-47,48-52,53+

cleanAttDf$YearsAtCompanyby5 <- findInterval(cleanAttDf$YearsAtCompany, c(0, 5, 10, 15, 20, 25, 30, 35))
cleanAttDf$YearsAtCompanyby5 <- mapvalues(cleanAttDf$YearsAtCompanyby5, from=c(1, 2, 3, 4, 5, 6, 7, 8), to=c('0-04', '05-09', '10-14', '15-19', '20-24', '25-29', '30-34', '35-40'))
cleanAttDf$YearsAtCompanyby5 <- as.factor(cleanAttDf$YearsAtCompanyby5)

# Generate property summary for variable
YearsAtCompanyby5Table <- table(cleanAttDf$YearsAtCompanyby5, cleanAttDf$Attrition)
PT <- prop.table(YearsAtCompanyby5Table)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "YearsAtCompanyby5" = Var1, "Attrition" = Var2)
PT

# Plot Years At Company vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = YearsAtCompanyby5, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
YearsAtCompanyby5BP <- g + xlab("Years At Company") + ylab("Percent") + ggtitle("Attrition vs. Years At Company")
YearsAtCompanyby5BP

# This is a major predictor

# ===YearsInCurrentRole============================================================================================

# Generate property summary for variable
YearsInCurrentRoleTable <- table(cleanAttDf$YearsInCurrentRole, cleanAttDf$Attrition)
PT <- prop.table(YearsInCurrentRoleTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "YearsInCurrentRole" = Var1, "Attrition" = Var2)
PT

# Plot Years In Current Role vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = YearsInCurrentRole, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
YearsInCurrentRoleBP <- g + xlab("Years In Current Role") + ylab("Percent") + ggtitle("Attrition vs. Years In Current Role")
YearsInCurrentRoleBP

# This is a major predictor

# ===YearsSinceLastPromotion============================================================================================

# Generate property summary for variable
YearsInCurrentRoleTable <- table(cleanAttDf$YearsInCurrentRole, cleanAttDf$Attrition)
PT <- prop.table(YearsInCurrentRoleTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "YearsInCurrentRole" = Var1, "Attrition" = Var2)
PT

# Plot Years Since Last Promotion vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = YearsInCurrentRole, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
YearsInCurrentRoleBP <- g + xlab("Years In Current Role") + ylab("Percent") + ggtitle("Attrition vs. Years In Current Role")
YearsInCurrentRoleBP

# ===YearsWithCurrManager============================================================================================

# Generate property summary for variable
YearsWithCurrManagerTable <- table(cleanAttDf$YearsWithCurrManager, cleanAttDf$Attrition)
PT <- prop.table(YearsWithCurrManagerTable)
PTdf <- as.data.frame(PT)
PTdf <- rename(PTdf, "YearsWithCurrManager" = Var1, "Attrition" = Var2)
PT

# Plot Years With Current Manager vs, Percent Attrition 
g <- ggplot(PTdf, aes(x = YearsWithCurrManager, y = (100*Freq), fill=Attrition)) + geom_bar(stat = "identity")
YearsWithCurrManagerBP <- g + xlab("Years With Current Manager") + ylab("Percent") + ggtitle("Attrition vs. Years With Current Manager")
YearsWithCurrManagerBP

# ========================================================================================================================================








