library(caret)
library(randomForest)
library(ggthemes)
library(gridExtra)
library(ggplot2)
library(grid)
library(e1071)

rm(list = ls())
if (!file.exists("pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
}
train <- read.csv("pml-training.csv", sep = ",", na.strings = c("", "NA"))
str(train)
train_used <- train[,c(8:11,37:49,60:68,84:86,102,113:124,140,151:160)]
Plot1 = ggplot() + geom_density(aes(x=gyros_belt_x), colour="red", data = train_used) + geom_density(aes(x=gyros_belt_y), colour="green", data = train_used) + geom_density(aes(x=gyros_belt_z), colour="blue", data = train_used) + theme_few() + xlab("Gyro Belt (xyz)")

Plot2 = ggplot() + geom_density(aes(x=roll_belt), colour="red", data = train_used) + geom_density(aes(x=pitch_belt), colour="green", data = train_used) + geom_density(aes(x=yaw_belt), colour="blue", data = train_used) + theme_few() + xlab("Pitch Belt (xyz)")

Plot3 = ggplot() + geom_density(aes(x=magnet_belt_x), colour="red", data = train_used) + geom_density(aes(x=magnet_belt_y), colour="green", data = train_used) + geom_density(aes(x=magnet_belt_z), colour="blue", data = train_used) + theme_few() + xlab("Magnet Belt (xyz)")

Plot4 = ggplot() + geom_density(aes(x=roll_dumbbell), colour="red", data = train_used) + geom_density(aes(x=pitch_dumbbell), colour="green", data = train_used) + geom_density(aes(x=yaw_dumbbell), colour="blue", data = train_used) + theme_few() + xlab("Dumbbell movement (yaw, pitch, roll")

Dplots <- arrangeGrob(Plot1, Plot2, Plot3, Plot4, nrow=2, ncol=2)
grid.draw(Dplots)

train_part <- createDataPartition(train_used$classe, p = 0.6, list = FALSE)
training <- train_used[train_part,]
testing <- train_used[-train_part,]

set.seed(1777)
random_forest <- randomForest(classe~., data=training, ntree=500, importance=TRUE)
random_forest
plot(random_forest, main="Random Forest: Error Rate Vs. Number of Trees")

imp <- importance(random_forest)
impL <- imp[,c(6,7)]
imp.ma <- as.matrix(impL)
imp.df <- data.frame(imp.ma)

write.csv(imp.df, "imp.df.csv", row.names = TRUE)
imp.df.csv <- read.csv("imp.df.csv", header = TRUE)

colnames(imp.df.csv) <-  c("Variable", "MeanDecreaseAccuracy", "MeanDecreaseGini")
imp.sort <- imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),]

imp.sort = transform(imp.df.csv, Variable = reorder(Variable, MeanDecreaseAccuracy))

VIP <- ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + ylab("Mean Decrease Accuracy") + xlab("") + geom_bar(stat="identity", fill="orange", alpha=0.8, width=0.75) + coord_flip() + theme_few()

imp.sort.Gini = transform(imp.df.csv, Variable = reorder(Variable, MeanDecreaseGini))

VIP.Gini <- ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) + ylab("Mean Decrease Gini") + xlab("") + geom_bar(stat="identity", fill="orange", alpha=0.8, width=0.75) + coord_flip() + theme_few()

VarImpPlot <- arrangeGrob(VIP, VIP.Gini, ncol=2)
grid.draw(VarImpPlot)

test_predictions <- predict(random_forest, newdata=testing)
CM <- confusionMatrix(test_predictions, testing$classe)
CM

CV <- trainControl(method = "CV", number = 5, allowParallel = T, verboseIter = F)
CVModel = train(classe~., data = training, method = "rf", prox = F, trControl = CV)
CVModel

predsCVModel <- predict(CVModel, newdata = testing)

confMatrix <- confusionMatrix(predsCVModel, testing$classe)
confMatrix
