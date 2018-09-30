##File ubication of the dataset
file <- "/Users/julietarodriguez/Documents/Hackatón 2018/covtype.csv"

## variable mydata as the dataset in csv format
mydata <- read.csv(file)

##Summary, with median, mean, moda, 1st and 3rd quartil, minimun and maximum of every column
summary(mydata)

##First five rows of the dataset
head(mydata)

##Type of data per feature
sapply (mydata, class)

##List of classification types
columns <- c('Soil_Type1','Soil_Type2','Soil_Type3','Soil_Type4','Soil_Type5','Soil_Type6','Soil_Type7','Soil_Type8','Soil_Type9',
             'Soil_Type_Diez','Soil_Type_Once','Soil_Type_Doce','Soil_Type_Trece','Soil_Type_Catorce','Soil_Type_Quince',
             'Soil_Type_Dieciseis','Soil_Type_Diecisiete','Soil_Type_Dieciocho','Soil_Type_Diecinueve','Soil_Type_Veinte',
             'Soil_Type_Veintiuno','Soil_Type_Veintidos','Soil_Type_Veintitres','Soil_Type_Veinticuatro','Soil_Type_Veinticinco',
             'Soil_Type_Veintiseis','Soil_Type_Veintisiete','Soil_Type_Veintiocho','Soil_Type_Veintinueve','Soil_Type_Treinta',
             'Soil_Type_Treintiuno','Soil_Type_Treintidos','Soil_Type_Treintitres','Soil_Type_Treinticuatro','Soil_Type_Treinticinco',
             'Soil_Type_Treintiseis','Soil_Type_Treintisiete','Soil_Type_Treintiocho','Soil_Type_Treintinueve','Soil_Type_Cuarenta')

##Unify all the forty columns of classification in one
mydata$Classification <- apply( mydata[, columns],1,paste,collapse="")

##Quit columns of the dataset
mydata <- mydata[ , !( names( mydata ) %in% columns ) ]

sapply(mydata, class)

##Change type character to factor of classification column
mydata$Classification <- as.factor(mydata$Classification)

##Structure of the dataset
str(mydata)

##Total rows per type of land
table(mydata$Classification)

##Percentage per type of land
round(prop.table(table(mydata$Classification)) * 100,digits = 1)

##Separate train dataset to test dataset
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.7, 0.3))

trainData <-  mydata[ind==1,]
testData <- mydata[ind==2,]

head(testData)

##Removing the feature to predict
trainData1 <- trainData[-16]
testData1 <- testData[-16]

head(trainData1)

##Checking dimensions of the different dataset made.
dim(trainData)
dim(trainData1)
dim(testData)
dim(testData1)

##Target variable for training
mydata_train_labels <- trainData$Classification
dim(mydata_train_labels)

class(mydata_train_labels)

##Target variable fot testing
mydata_test_labels <- testData$Classification
dim(mydata_test_labels)

class(mydata_test_labels)

library(class)

sapply(mydata, class)

##Knn entrenamiento con Euclides k=10
mydata_test_pred1 <- knn(train = trainData1, test = testData1, cl= mydata_train_labels,k = 10,prob=TRUE) 


library(gmodels)

##TRUE-FALSE Table 
CrossTable(x = mydata_test_labels, y = mydata_test_pred1,prop.chisq=FALSE) 


plot.df = data.frame(testData1, predicted = mydata_test_pred1)

# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Elevation,
                      y = plot.df$Slope, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Elevation, Slope, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


str(mydata)
