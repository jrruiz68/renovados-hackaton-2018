#Knn- Iris

summary(iris)

str(iris)

table(iris$Species)

round(prop.table(table(iris$Species)) * 100,digits = 1)


ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))

trainData <-  iris[ind==1,]
testData <- iris[ind==2,]

head(trainData)

trainData1 <- trainData[-5]
testData1 <- testData[-5]

head(trainData1)


dim(trainData)
dim(trainData1)
dim(testData)
dim(testData1)

iris_train_labels <- trainData$Species
dim(iris_train_labels)

class(iris_train_labels)

iris_test_labels <- testData$Species
dim(iris_test_labels)

class(iris_test_labels)


library(class)

iris_test_pred1 <- knn(train = trainData1, test = testData1, cl= iris_train_labels,k = 3,prob=TRUE) 

library(gmodels)


CrossTable(x = iris_test_labels, y = iris_test_pred1,prop.chisq=FALSE) 
