source("C:/R/MAXIM/Tools/FunctionList.R")
library(tm)

setwd("C:/R/MAXIM/MLwR/Chapter 07")

letters <- read.csv("letterdata.csv")

str(letters)

letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

library(kernlab)

letter_classifier <- ksvm(letter ~., data = letters_train, kernel = "vanilladot")

letter_classifier

letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

#Show how the misidentification went
table(letter_predictions,letters_test$letter)

agreement <- letter_predictions == letters_test$letter

table(agreement)

prop.table(table(agreement))

letter_classifier_rbf <- ksvm(letter~.,data=letters_train, kernel = "rbfdot")

letters_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letters_predictions_rbf == letters_test$letter

table(agreement_rbf)

prop.table(table(agreement_rbf))

####
library(C50)
data(churn)
str(churnTrain)

#remove state, area_code, and account_length which are not approapriate for class features
churnTrain = churnTrain[,!names(churnTrain)%in% c("state","area_code","account_length")]
set.seed(2)
ind = sample(2,nrow(churnTrain), replace = TRUE, prob=c(0.7,0.3))
trainset = churnTrain[ind==1,]
testset = churnTrain[ind==2,]
dim(trainset); dim(testset)

library(rpart)
churn.rp = rpart(churn ~.,data=trainset)
churn.rp
#Get cost of complexity parameter
printcp(churn.rp)
plotcp(churn.rp)
summary(churn.rp)
plot(churn.rp, margin = 0.1)
text(churn.rp, all=TRUE, use.n=TRUE)

#Measure predictive performance of recursive partitioning tree
predictions = predict(churn.rp, testset, type="class")
table(testset$churn, predictions)
library(caret)
library(e1071)
confusionMatrix(table(predictions, testset$churn))

#Prune Tree
min(churn.rp$cptable[,"xerror"]) #find min x-validation error of class treemodel
which.min(churn.rp$cptable[,"xerror"])#locate error with min XV parameter
churn.cp = churn.rp$cptable[7,"CP"]#get the cost completxity parameter of the record with min CV

prune.tree <- prune(churn.rp, cp= churn.cp)
plot(prune.tree,margin=0.1)
text(prune.tree,all=TRUE,use.n=TRUE)

predictions <- predict(prune.tree, testset, type="class")
table(testset$churn, predictions)
confusionMatrix(table(predictions,testset$churn))

##Conditional Inference Tree - adapt sig. test to select variables rather than by max measures (Gini used in rpart)

library(party)
ctree.model <- ctree(churn ~ .,data=trainset)
ctree.model
plot(ctree.model)

#obtain a simple conditional inf tree, one can reduce with less input features
daycharge.model <- ctree(churn ~ total_day_charge, data = trainset)
plot(daycharge.model)

########

concrete <- read.csv("C:/R/MAXIM/MLwR/Chapter 07/concrete.csv")
str(concrete)
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}

concrete_norm <- as.data.frame(lapply(concrete,normalize))
summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]
library(neuralnet)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)
plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
MAE(predicted_strength, concrete_test$strength)
cor(predicted_strength, concrete_test$strength)
