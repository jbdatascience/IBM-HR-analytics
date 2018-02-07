employee<-read.csv("D:/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(employee)
str(employee)
dim(employee)
colnames(employee)[1]="Age"

library("caTools")
set.seed(12345)
emp <- sample.split(employee$Attrition,SplitRatio = 0.75)
emp_train <- subset(employee,emp==TRUE)
emp_test <- subset(employee,emp==FALSE)
View(emp_train)
summary(is.na(emp_train))
colSums(is.na(employee))

library("ggplot2")
ggplot(data=emp_train,mapping=aes(Attrition,fill=Attrition))+geom_bar()
prop.table(table(emp_train$Attrition))

ggplot(data=emp_train,mapping=aes(x=Age,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(BusinessTravel,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(x=DailyRate,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(Department,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(x=DistanceFromHome,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(Education,fill=Attrition))+geom_bar()
tt<-table(Train1$Attrition,Train1$BusinessTravel)
chisq.test(tt)
ggplot(data=emp_train,mapping=aes(EducationField,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(Gender,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(HourlyRate,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(JobInvolvement,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(JobLevel,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(JobRole,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(JobSatisfaction,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(MaritalStatus,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(MonthlyIncome,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(MonthlyRate,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(NumCompaniesWorked,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(OverTime,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(PercentSalaryHike,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(PerformanceRating,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(RelationshipSatisfaction,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(StockOptionLevel,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(TotalWorkingYears,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(TrainingTimesLastYear,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(WorkLifeBalance,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(YearsAtCompany,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(YearsInCurrentRole,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(YearsSinceLastPromotion,fill=Attrition))+geom_bar()
ggplot(data=emp_train,mapping=aes(YearsWithCurrManager,fill=Attrition))+geom_bar()
summary(emp_train$DistanceFromHome)
emp_train$EducationField[emp_train$EducationField=="Marketing"]
library(ROSE)
abcd <- ROSE(Attrition ~ ., data = emp_train, seed = 1,p=0.45)$data
table(abcd$Attrition)
View(emp_train)
as.integer(emp_train1$Age)
nrow(abcd)
abcd

library("DMwR")
emp_train <- SMOTE(Attrition ~ ., data = emp_train,perc.over = 100, perc.under=200)
table(emp_train$Attrition)

#Feature Engg
emp_train$TenurePerJob<-ifelse(emp_train$NumCompaniesWorked!=0, emp_train$TotalWorkingYears/emp_train$NumCompaniesWorked,0)
emp_train$YearWithoutChange <- emp_train$YearsInCurrentRole - emp_train$YearsSinceLastPromotion
emp_train$YearsWithoutChange2 <- emp_train$TotalWorkingYears - emp_train$YearsSinceLastPromotion

ggplot(data=emp_train,mapping=aes(TenurePerJob,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(YearWithoutChange,fill=Attrition))+geom_histogram()
ggplot(data=emp_train,mapping=aes(YearsWithoutChange2,fill=Attrition))+geom_histogram()

Med_HR <- median(emp_train[emp_train$Department == 'Human Resources',]$MonthlyIncome)
Med_RnD <- median(emp_train[emp_train$Department == 'Research & Development',]$MonthlyIncome)
Med_Sales <- median(emp_train[emp_train$Department == 'Sales',]$MonthlyIncome)

emp_train$CompaRatioDep <- ifelse(emp_train$Department == 'Human Resources',
                                  emp_train$MonthlyIncome/Med_HR,
                                  ifelse(emp_train$Department=='Research & Development',emp_train$MonthlyIncome/Med_RnD,emp_train$MonthlyIncome/Med_Sales))

ggplot(data=emp_train,mapping=aes(CompaRatioDep,fill=Attrition))+geom_histogram()

#####
emp_test$TenurePerJob<-ifelse(emp_test$NumCompaniesWorked!=0, emp_test$TotalWorkingYears/emp_test$NumCompaniesWorked,0)
emp_test$YearWithoutChange <- emp_test$YearsInCurrentRole - emp_test$YearsSinceLastPromotion
emp_test$YearsWithoutChange2 <- emp_test$TotalWorkingYears - emp_test$YearsSinceLastPromotion
emp_test$CompaRatioDep <- ifelse(emp_test$Department == 'Human Resources',emp_test$MonthlyIncome/Med_HR,ifelse(emp_test$Department=='Research & Development',emp_test$MonthlyIncome/Med_RnD,emp_test$MonthlyIncome/Med_Sales))

#Binning
emp_train$AgeGroup <- with(emp_train,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1))))))))

emp_train$DistanceGroup <- with(emp_train,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1))))))
emp_train$YearsWithManagerGroup <- with(emp_train,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1)))))

#emp_train$YearsWithManagerGroup <- with(emp_train,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1)))))


emp_train$TenureGroup <- with(emp_train,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1)))))))))

emp_train$Change2Group <- with(emp_train,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1)))

emp_train$Change1Group <- with(emp_train,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1)))


emp_train$WorkYearGroup <- with(emp_train,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

emp_train$NumCompGroup <- with(emp_train,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) 

#Testing

#emp_test$tenureperjob<-ifelse(emp_test$NumCompaniesWorked!=0, emp_test$TotalWorkingYears/emp_test$NumCompaniesWorked,0)
#emp_test$YearWithoutChange <- emp_test$YearsInCurrentRole - emp_test$YearsSinceLastPromotion
#emp_test$YearsWithoutChange2 <- emp_test$TotalWorkingYears - emp_test$YearsSinceLastPromotion

emp_test$AgeGroup <- with(emp_test,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1))))))))

emp_test$DistanceGroup <- with(emp_test,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1))))))
emp_test$YearsWithManagerGroup <- with(emp_test,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1)))))

#emp_test$YearsWithManagerGroup <- with(emp_test,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1)))))


emp_test$TenureGroup <- with(emp_test,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1)))))))))

emp_test$Change2Group <- with(emp_test,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1)))

emp_test$Change1Group <- with(emp_test,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1)))


emp_test$WorkYearGroup <- with(emp_test,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

emp_test$NumCompGroup <- with(emp_test,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) 

colnames(emp_train)
colnames(emp_test)
##
install.packages("corrplot")
library(corrplot)
library(psych)

str(emp_train)

for(i in 1:ncol(emp_train)){
  
  emp_train[,i]<- as.integer(emp_train[,i])
}

corrplot(cor(emp_train))
##
#Correlation

cor(emp_train[,c(1,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28:38)])

Train <- emp_train[,c(3,5,7,8,12,14,15,16,17,18,21,23,24,26,28,29,30,31,40:47,2)]


Test=emp_test[,c(2,3,5,7,8,12,14,15,16,17,18,21,23,24,26,28,29,30,31,40:47)]
#Test=Test[,-1]

Train$BusinessTravel <- as.integer(Train$BusinessTravel)
Train$Department <- as.integer(Train$Department)
Train$Gender <- as.integer(Train$Gender)
Train$MaritalStatus <- as.integer(Train$MaritalStatus)
Train$OverTime <- as.integer(Train$OverTime)
Train$JobRole <- as.integer(Train$JobRole)
Train$EducationField <- as.integer(Train$EducationField)

Test$BusinessTravel <- as.integer(Test$BusinessTravel)
Test$Department <- as.integer(Test$Department)
Test$Gender <- as.integer(Test$Gender)
Test$MaritalStatus <- as.integer(Test$MaritalStatus)
Test$OverTime <- as.integer(Test$OverTime)
Test$JobRole <- as.integer(Test$JobRole)
Test$EducationField <- as.integer(Test$EducationField)
Train1 <- Train
Test1<-Test
Test1=Test1[,-1]
for(i in 1:ncol(Train1)){
  Train1[,i] <- as.factor(Train1[,i])
}

for(i in 1:ncol(Test1)){
  Test1[,i] <- as.factor(Test1[,i])
}

fit_rpart <- train(Attrition ~.,Train1,method = 'rpart', trControl = trainControl(method = 'cv',number = 3))
pred_rpart=predict(fit_rpart,newdata=Test1)

#auc <- roc(testSplit$target, pred)
#print(auc)

set.seed(123)
fit_rf <- train(Attrition ~.,Train1,method = 'rf', trControl = trainControl(method = 'repeatedcv',number = 3))
pred_rf<-predict(fit_rf,newdata=Test1)

library("xgboost")
xgbGrid <- expand.grid(nrounds = 300,
                       max_depth = 1,
                       eta = 0.3,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9)

set.seed(12)
fit_xgb <- train(Attrition ~.,Train1,method = 'xgbTree',tuneGrid = xgbGrid,trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE))
pred_xgb<-predict(fit_xgb,newdata=Test1)
confusionMatrix(pred_rpart,Test$Attrition)
confusionMatrix(pred_rf,Test$Attrition)
confusionMatrix(pred_xgb,Test$Attrition)

varImp(fit_rf)

awb<-data.frame(Emp_No=emp_test$EmployeeNumber,Employee_churn=pred_xgb)
View(awb)
write.csv(awb,file = "Employee churn.csv")

library("caret")
k=train(x=Train1[,-1],y=Train1[,1],method='knn',trControl=cntrl,tuneGrid=expand.grid(k=1:31))
pred_k=predict(k,newdata = Test1)
confusionMatrix(Test1[,1],pred_k)

fit_logistic<-glm(Attrition~.,data=Train1,family = binomial)

#anova(fit_rpart, test="Chisq")