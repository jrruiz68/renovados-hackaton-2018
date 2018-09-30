
##File ubication
file <- "/Users/julietarodriguez/Documents/Hackatón 2018/covtype.csv"

mydata <- read.csv(file)


summary(mydata)

head(mydata)

sapply (mydata, class)

# your starting data..
data <- data.frame('a' = 1:3, 'b' = c('a','b','c'), 'c' = c('d', 'e', 'f'), 'd' = c('g', 'h', 'i')) 

# columns to paste together
cols <- c( 'b' , 'c' , 'd' )

# create a new column `x` with the three columns collapsed together
data$x <- apply( data[ , cols ] , 1 , paste , collapse = "-" )

# remove the unnecessary columns
data <- data[ , !( names( data ) %in% cols ) ]
head(data)

columns <- c('Soil_Type1','Soil_Type2 ','Soil_Type3','Soil_Type4','Soil_Type5','Soil_Type6','Soil_Type7','Soil_Type8','Soil_Type9',
             'Soil_Type_Diez','Soil_Type_Once','Soil_Type_Doce','Soil_Type_Trece','Soil_Type_Catorce','Soil_Type_Quince',
             'Soil_Type_Dieciseis','Soil_Type_Diecisiete','Soil_Type_Dieciocho','Soil_Type_Diecinueve','Soil_Type_Veinte',
             'Soil_Type_Veintiuno','Soil_Type_Veintidos','Soil_Type_Veintitres','Soil_Type_Veinticuatro','Soil_Type_Veinticinco',
             'Soil_Type_Veintiseis','Soil_Type_Veintisiete','Soil_Type_Veintiocho','Soil_Type_Veintinueve','Soil_Type_Treinta',
             'Soil_Type_Treintiuno','Soil_Type_Treintidos','Soil_Type_Treintitres','Soil_Type_Treinticuatro','Soil_Type_Treinticinco',
             'Soil_Type_Treintiseis','Soil_Type_Treintisiete','Soil_Type_Treintiocho','Soil_Type_Treintinueve','Soil_Type_Cuarenta')

columns <- c('Soil_Type1','Soil_Type2','Soil_Type3','Soil_Type4','Soil_Type5','Soil_Type6','Soil_Type7','Soil_Type8','Soil_Type9',
             'Soil_Type_Diez','Soil_Type_Once','Soil_Type_Doce','Soil_Type_Trece','Soil_Type_Catorce','Soil_Type_Quince',
             'Soil_Type_Dieciseis','Soil_Type_Diecisiete','Soil_Type_Dieciocho','Soil_Type_Diecinueve','Soil_Type_Veinte',
             'Soil_Type_Veintiuno','Soil_Type_Veintidos','Soil_Type_Veintitres','Soil_Type_Veinticuatro','Soil_Type_Veinticinco',
             'Soil_Type_Veintiseis','Soil_Type_Veintisiete','Soil_Type_Veintiocho','Soil_Type_Veintinueve','Soil_Type_Treinta',
             'Soil_Type_Treintiuno','Soil_Type_Treintidos','Soil_Type_Treintitres','Soil_Type_Treinticuatro','Soil_Type_Treinticinco',
             'Soil_Type_Treintiseis','Soil_Type_Treintisiete','Soil_Type_Treintiocho','Soil_Type_Treintinueve','Soil_Type_Cuarenta')

mydata$clasification <- apply( mydata[, columns],1,paste,collapse="")

head(mydata$clasification)
tail(mydata$clasification)
tail(mydata)

mydata <- mydata[ , !( names( mydata ) %in% columns ) ]

sapply(mydata, class)

mydata$classitifaction <- NULL

sapply(mydata, class)

colnames(mydata) <- c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways",
                      "Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Wilderness_Area1","Wilderness_Area2",
                      "Wilderness_Area3","Wilderness_Area4","Cover_Type","Classification")

mydata$Classification <- as.factor(mydata$Classification)

str(mydata)

table(mydata$Classification)

round(prop.table(table(mydata$Classification)) * 100,digits = 1)


ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.7, 0.3))

trainData <-  mydata[ind==1,]
testData <- mydata[ind==2,]

head(testData)

trainData1 <- trainData[-16]
testData1 <- testData[-16]

head(trainData1)


dim(trainData)
dim(trainData1)
dim(testData)
dim(testData1)

mydata_train_labels <- trainData$Classification
dim(mydata_train_labels)

class(mydata_train_labels)

mydata_test_labels <- testData$Classification
dim(mydata_test_labels)

class(mydata_test_labels)

library(class)

sapply(mydata, class)
mydata_test_pred1 <- knn(train = trainData1, test = testData1, cl= mydata_train_labels,k = 3,prob=TRUE) 

fit <- knn(train = trainData1, test = testData1, cl= mydata_train_labels)
fit

library(gmodels)


CrossTable(x = mydata_test_labels, y = mydata_test_pred1,prop.chisq=FALSE) 

# Create a dataframe to simplify charting
plot.df = data.frame(testData1, predicted = fit)

# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Elevation,
                      y = plot.df$Aspect, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Elevation, Aspect, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Horizontal_Distance_To_Hydrology,
                      y = plot.df$Vertical_Distance_To_Hydrology, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Slope,
                      y = plot.df$Hillshade_Noon, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Slope, Hillshade_Noon, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Aspect,
                      y = plot.df$Cover_Type, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Aspect, Cover_Type, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Horizontal_Distance_To_Fire_Points,
                      y = plot.df$Wilderness_Area2, 
                      predicted = plot.df$predicted)

library(plyr)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

library(ggplot2)

ggplot(plot.df, aes(Wilderness_Area2, Wilderness_Area1, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)



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

fit

sapply(mydata, class)

#k=10
mydata_test_pred1 <- knn(train = trainData1, test = testData1, cl= mydata_train_labels,k = 10,prob=TRUE) 


