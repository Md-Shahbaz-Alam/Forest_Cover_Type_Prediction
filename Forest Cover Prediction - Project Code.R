## Forest Cover Prediction Competition

## Loading required libraries
library(ggplot2); library(corrgram); library(MASS)
library(caret)
library(randomForest)

## Reading the Data Set in the directory
train <- read.csv("train.csv", header = TRUE, sep = ",")


## Checking for the details of the Data Set
dim(train)
summary(train)
str(train)
table(is.na(train))


## After looking at the Data Set, there are Dummy column created for the soil_type
## We need to covert the dummy soil_type to soil_type


## Getting the index of soil_type dummies
grep("Soil_Type", colnames(train))


## Creating column for soil type which will vary from 1 to 40, each number for
train$Soil_Type <- 0

## Each soil type, thus soil type essentially becomes a categorical variable

for(i in 1:dim(train)[1]){
  for(j in 1:40){
    train[i,c("Soil_Type")] <- train[i,c(57)] + 
      train[i,c(grep("Soil_Type", 
                      colnames(train))[j])]*j
  }
}


## Similarly the wilderness_area feature was also in binary format, need to aggregate the binary values to 
## create one column with different levels

train$Wilderness_Area <- 0

## vary from 1 to 4, each number for each wilderness_area, thus soil type 
## essentially becomes a categorical variable

for(i in 1:dim(train)[1]){
  for(j in 1:4){
    train[i,c("Wilderness_Area")] <- train[i,c(57)] + 
      train[i,c(grep("Wilderness_Area", colnames(train))[j])]*j
  }
}


## Converting the columns from integer to factor
train$Soil_Type <- as.factor(train$Soil_Type)
train$Wilderness_Area <- as.factor(train$Wilderness_Area)
train$Cover_Type <- as.factor(train$Cover_Type)
str(train)

# Extracting important features i.e. excluding the dummy variables in new train set
train1 <- train[,c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology", 
                   "Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways",
                   "Hillshade_9am","Hillshade_Noon", "Hillshade_3pm","Horizontal_Distance_To_Fire_Points",
                   "Wilderness_Area","Soil_Type","Cover_Type")]

# Performing EDA for the Data Set

parameters <- names(train1)

for(i in 1:dim(train1)[2]){
  p <- ggplot(train1, aes(factor(Cover_Type), train1[,parameters[i]]))
  print(p + geom_boxplot() + ylab(parameters[i]))
  print(i)
}

# Checking for NA values in the data set

table(is.na(train1))

## As their is no outlier and NAs in the dataset, we can know use the data set for the analysis

## We need to check for the correlation of the Independent variables with the dependent variables for 
## getting the significance of the column which can used for modeling.

# Checking the correlation of the continous variables with the target variable 
corrgram.data <- train1[,c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology", 
                            "Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways",
                            "Hillshade_9am","Hillshade_Noon", "Hillshade_3pm","Horizontal_Distance_To_Fire_Points")]

# Generate correlogram
corrgram.vars <- c("Elevation","Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
                   "Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways",
                   "Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points")

corrgram(corrgram.data[,corrgram.vars], order=FALSE, lower.panel=NULL,
         upper.panel=panel.pie, 
         text.panel=panel.txt, main="Forest Data")

## Although some variables were correlated but they cannot be removed based on domain significane

# Checking for the correlation of categorical variables with the target variable

tbl = table(train$Soil_Type, train$Wilderness_Area)
chisq.test(tbl) 

## The variables comes out to be independant of each other


# Checking for the dependance of Categorical variable on continuous variables

for (i in 1:10) {
  fit <- aov(train1[,i] ~ Soil_Type, data=train1)
  print(summary(fit) )
}

for (i in 1:10) {
  fit <- aov(train1[,i] ~ Wilderness_Area, data=train1)
  print(summary(fit))
}

## All the variables were dependant on each other

## CONCLUSION - ALL THE VARIABLES WILL BE USED FOR MODELLING GIVEN THERE DEPENDANCY AND IMPORTANCE

## Spliting the data set into train and test using stratified sampling technique

set.seed(1)
inTraining <- createDataPartition(train1$Cover_Type, p = .8, list = FALSE)
train.data <- train1[ inTraining,]
test.data <- train1[-inTraining,]

## For modeling of the data we will use Random Forest modeling technique

## Search grid

cvCtrl = trainControl(method = "repeatedcv", number = 3, repeats = 3, verboseIter = TRUE)
newGrid = expand.grid(mtry = c(4,6,8))

## try with trees = 500, 1000, 1500, 2000, 2500

fit_rf1 <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
                                + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
                                + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
                                data=train.data, trControl = cvCtrl, method = "rf", weights = NULL,
                                tuneGrid = newGrid, n.trees = 500, verbose=TRUE)

fit_rf2 <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
                                + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
                                + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
                                data=train.data, trControl = cvCtrl, method = "rf", weights = NULL,
                                tuneGrid = newGrid, n.trees = 1000)

fit_rf3 <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
                                + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
                                + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
                                data=train.data, trControl = cvCtrl, method = "rf", weights = NULL,
                                tuneGrid = newGrid, n.trees = 1500)

fit_rf4 <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
                                + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
                                + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
                                data=train.data, trControl = cvCtrl, method = "rf", weights = NULL,
                                tuneGrid = newGrid, n.trees = 2000)

fit_rf5 <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
                                + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
                                + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
                                data=train.data, trControl = cvCtrl, method = "rf", weights = NULL,
                                tuneGrid = newGrid, n.trees = 2500)

## Given these models, we can make statistical 
## statements about their performance differences

resamps <- resamples(list(RF1 = fit_rf1,
                          RF2 = fit_rf2,
                          RF3 = fit_rf3,
                          RF4 = fit_rf4,
                          RF5 = fit_rf5))
resamps


# Visualizing the performance of different data set
bwplot(resamps)
xyplot(resamps)


## p-value adjustment: bonferroni 
## Upper diagonal: estimates of the difference
## Lower diagonal: p-value for H0: difference = 0


difValues <- diff(resamps)
difValues
summary(difValues)

## We get best result from RF3, beyond that its leading to overfitting

# Predicting using random forest model

pred.rf.test <- predict(fit_rf3, newdata = test.data)

table(pred.rf.test)

confusionMatrix(pred.rf.test, test.data$Cover_Type)

## THE MODEL WITH N.TREE = 2000 WITH MTRY = 8 gave the best result among random forest

## Using the gbm for modeling the data set

gbm_grid <-  expand.grid(n.trees = seq(500,2501,500),
                        interaction.depth = c(4, 6),
                        shrinkage = c(0.1, 0.05),
                        n.minobsinnode = 10)

fit_gbm <- train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology
             + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon
             + Hillshade_3pm + Horizontal_Distance_To_Fire_Points +Wilderness_Area + Soil_Type,
             data = train.data, method = "gbm", trControl = cvCtrl, 
             verbose = TRUE, tuneGrid = gbm_grid)

### Predicting using gbm method

pred.gbm.test <- predict(fit_gbm, newdata = test.data)

table(pred.gbm.test)

confusionMatrix(pred.gbm.test, test.data$Cover_Type)

resamps <- resamples(list(GB = fit_gbm,
                          RF = fit_rf4))
resamps


# visualizing the performance of different data set
bwplot(resamps)
xyplot(resamps)


## Upper diagonal: estimates of the difference
## Lower diagonal: p-value for H0: difference = 0

difValues <- diff(resamps)
difValues
summary(difValues)


## best result is given by RF4 i.e. the random forest with 2000 trees
## HENCE WE CONCLUDE THAT RANDOM FOREST IS GIVING BEST RESULT WITH 2000 TREES
