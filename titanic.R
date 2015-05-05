setwd("~/Desktop/kaggle/")
train <- read.csv("~/Downloads/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("~/Downloads/test.csv", header = TRUE, stringsAsFactors = FALSE)
#part 1
train$Age[is.na(train$Age) == TRUE] <- mean(train$Age,na.rm=TRUE) 
test$Age[is.na(test$Age) == TRUE] <- mean(test$Age,na.rm=TRUE) 
na.omit(train)
na.omit(test)

library(rpart)
#part 2
Survivedtrain <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                       data=train, method= "class")
printcp(Survivedtrain)
plotcp(Survivedtrain)
plot(Survivedtrain, uniform=TRUE, main= "Decision Tree for Train")
text(Survivedtrain, use.n=TRUE, cex=.8)
post(Survivedtrain, title = "Survived Titanic")

#part 3
library(caret)
set.seed(998)
inTraining <- createDataPartition(train$Survived, p=.75, list=FALSE)
training <- train[inTraining,]
testing <- train[-inTraining,]

fitControl <- trainControl(method = "cv", number=10, repeats= 1, verbose= TRUE)
set.seed(825)

library(gbm)
gbmFit1 <- train(Survived ~ Pclass+Sex+Age, data=train,
                 method= "gbm", trControl = fitControl, verbose= FALSE)
gbmFit1
gbmGrid <- expand.grid(interaction.depth = c(2,3,4), n.trees=(1:50)*200, 
                       shrinkage= c(.1,.05,.01))
nrow(gbmGrid)
set.seed(825)
gbmFit2 <- train(Survived ~ Pclass + Sex + Age, 
                 data=train, method= "gbm", trControl = fitControl, verbose= FALSE, 
                 tuneGrid=gbmGrid)

gbmFit2

trellis.par.set(caretTheme())
plot(gbmFit2)
summary(gbmFit2)

test$Survived=1
Prediction<- predict(gbmFit2, test)

#submission

submit <- data.frame(cbind(PassengerId = test$PassengerId, Survived=Prediction))
write.csv(submit, file = "titanic.csv", row.names = FALSE)



