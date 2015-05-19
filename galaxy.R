setwd("~/Desktop/kaggle/images_training_rev")
galaxytrain <- read.csv("~/Desktop/Kaggle/galaxy_train (1).csv")
library(jpeg)
library(EBImage)
library(ripa)

images <- list.files()

data <- data.frame(GalaxyID = integer(length(images)),
                     Mean = double(length(images)),
                     Variance = double(length(images)),
                     q10 = double(length(images)),
                     q25 = double(length(images)),
                     q75 = double(length(images)),
                     q90 = double(length(images)))

for(i in 1:length(images))
{  
  images2 <- as.matrix(resize(rgb2grey(readJPEG(images[i])), 50, 50))
  GalaxyID <- gsub(".jpg", "", images[i])
  data$GalaxyID[i] <- GalaxyID
  data$Mean[i] <- mean(images2)
  data$Variance[i] <- var(as.vector(images2))
  data$q10[i] <- quantile(images2, 0.1)
  data$q25[i] <- quantile(images2, 0.25)
  data$q75[i] <- quantile(images2, 0.75)
  data$q90[i] <- quantile(images2, 0.9)
}

data2 <- cbind(data, new_features)
data3 <- merge(data2, galaxytrain, by= "GalaxyID", all=TRUE)
train <- subset(data3, !is.na(Prob_Smooth))
test <- subset(data3, is.na(Prob_Smooth))

library(caret)
trainsplit <- createFolds(train$GalaxyID, k=2)
train1 <- train[trainsplit[[1]],]
train2 <- train[trainsplit[[2]],]

set.seed(998)
glmFit <- glm(Prob_Smooth ~ Mean + Variance + q10 + q25 + q75 + q90, data= train1, 
              family= "gaussian")
Predict <- predict(glmFit, newdata = train2)
RMSE <- RMSE(train2$Prob_Smooth, Predict)

install.packages("neuralnet")
library(neuralnet)

library(gbm)
inTraining <- createDataPartition(train$Prob_Smooth, p= .7, list = FALSE)
training <- train[inTraining,]
testing <- train[-inTraining,]
fitControl <- trainControl(method = "cv", number=2, repeats= 1, verbose= TRUE)
gbmGrid <- expand.grid(interaction.depth = c(2,3,4), n.trees=(1:50)*200, 
                       shrinkage= c(.1,.05,.01))

gbmFit1 <- train(Prob_Smooth ~ Mean + Variance + q10 + q25 + q75 + q90, data=train,
                 method= "nnet", trControl = fitControl, verbose= FALSE)
prediction <- predict(gbmFit1, newdata = test)

submit <- data.frame(cbind(GalaxyID = test$GalaxyID, Prob_Smooth=prediction))
write.csv(submit, file = "Galaxy.csv", row.names = FALSE)


