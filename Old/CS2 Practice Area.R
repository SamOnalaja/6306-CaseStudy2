#start

install.packages("readxl")
library(readxl)
library(randomForest)

st<-read_excel("./CaseStudy2data.xlsx", col_names = T,trim_ws = T)
df<-st



####Start converting to numerical values####

#attition yes as 1
df$Attrition<-as.numeric(df$Attrition == "Yes")
#Business travel as 2 Frequent, none 0
###DOES THIS NEED TO BE MULTIPLE COLLUMNS?
df$BusinessTravel<-factor(df$BusinessTravel, c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), labels = c(0, 1, 2))

###EDUCATION (could this be a match criteria??? ie 1 value for match in field???)


#Gender male =1 
df$Gender<-as.numeric(df$Gender == "Male")

#overtime yes as 1
df$OverTime<-as.numeric(df$OverTime == "Yes")

#creates binary fields for single married and divorced
df$single<-as.numeric(df$MaritalStatus == "Single")
df$married<-as.numeric(df$MaritalStatus == "Married")
df$divorced<-as.numeric(df$MaritalStatus == "Divorced")

#human resources match
#sales /marketing


#unique(df$JobRole)
#unique(df$EducationField)


####atempts fit analysis by degree
#lifesci fit
df$LifeSciFit<-as.numeric(df$EducationField == "Life Sciences" & 
                            (df$JobRole == "Research Scientist" | df$JobRole == "Laboratory Technician" | df$JobRole == "Healthcare Representative"| df$JobRole == "Manager"))

#medical fit
df$MedicalFit<-as.numeric(df$EducationField == "Medical" & 
                            (df$JobRole == "Research Scientist" | df$JobRole == "Laboratory Technician" | df$JobRole == "Healthcare Representative"| df$JobRole == "Manager"))
#marketing fit
df$MarketingFit<-as.numeric(df$EducationField == "Marketing" & 
                              (df$JobRole == "Sales Executive" | df$JobRole == "Sales Representative" | df$JobRole == "Manager"))
#technical fit
df$technicalFit<-as.numeric(df$EducationField == "Technical Degree" & 
                              (df$JobRole == "Research Scientist" | df$JobRole == "Research Director"| df$JobRole == "Manufacturing Director" | df$JobRole == "Laboratory Technician" | df$JobRole == "Manager"))
#Human Resources
df$HRFit<-as.numeric(df$EducationField == "Human Resources" & 
                              (df$JobRole == "Manager" | df$JobRole == "Human Resources" ))


####backout text data for randomforest
df[,c(5,8,16,18,22,27)]
prerun<-df[,c(-5,-8,-16,-18,-22,-27)]



###atempt random forest
clf1 <- randomForest(x=prerun[,-2],as.factor(prerun[,2]==1))
clf1$importance
clf1$confusion
summary(clf1)


# Test ROC RF
num_exmps = nrow(prerun)
L = replace(integer(num_exmps), prerun[,2]==1, 1)
M <- prerun[,-2]

train_idx <- sample(c(1:num_exmps), size = num_exmps * 0.7, replace = FALSE)
length(train_idx)

#runs on training data
clf2 <- randomForest(M[train_idx,],as.factor(L[train_idx]))

#runs on not trained data/validation
pred <- predict(clf2, M[-train_idx,],type="prob")
plot(roc(L[-train_idx], as.numeric(pred[,1])))

clf2$importance
clf2$confusion
summary(clf2)




