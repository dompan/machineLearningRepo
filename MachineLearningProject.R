library(caret)
library(rpart)
library(rattle)

training<-read.csv("pml-training.csv",header=TRUE,sep=",",na.strings=c("","NA","NULL"))
testing<-read.csv("pml-testing.csv",header=TRUE,sep=",",na.strings=c("","NA","NULL"))

training_clean <- training[, colSums(is.na(training))==FALSE]
testing_clean <- testing[, colSums(is.na(testing))==FALSE]

# the first 7 column doesn't contain info from sensors

training_clean <- training_clean[, c(8:60)]
testing_clean <- testing_clean[, c(8:60)]

inTrain <- createDataPartition(y=training_clean$classe, p=.75, list=FALSE)
train <- training_clean[inTrain,]
test <- training_clean[-inTrain,]

modFit_tree<-train(classe ~ .,method="rpart",data=train)
fancyRpartPlot(modFit_tree$finalModel)
#Accuratezza del 72% non molto buono
confusionMatrix( predict(modFit_tree, newdata = test) , test$classe)

# I tried to use rand forest with caret package but it nevr ends
#modFit_rf<-train(classe ~ .,method="rf",data=train)

#Random forest work so better
modFit_rf <- randomForest(classe ~ ., data=train)
confusionMatrix( predict(modFit_rf, newdata = test) , test$classe)

answers<-predict(modFit_rf, testing_clean)