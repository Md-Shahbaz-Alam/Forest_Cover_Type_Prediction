# Forest_Cover_Type_Prediction

## Kaggle Competition | Forest Cover Type

In this competition you are asked to predict the forest cover type (the predominant kind of tree cover) from cartographic variables. The actual forest cover type for a given 30 x 30 meter cell was determined from US Forest Service (USFS) Region 2 Resource Information System data. Independent variables were then derived from data obtained from the US Geological Survey and USFS. The data is in raw form and contains binary columns of data for qualitative independent variables such as wilderness areas and soil type.

This study area includes four wilderness areas located in the Roosevelt National Forest of northern Colorado. These areas represent forests with minimal human-caused disturbances, so that existing forest cover types are more a result of ecological processes rather than forest management practices.

From the competition [homepage](https://www.kaggle.com/c/forest-cover-type-kernels-only)

## Implemented Approach

Used cartographic variables to classify forest categories for a given 30 x 30 meter cell

1. Applied regression analysis to determine the statistical significance of features to be used for the classification of forest cover type of an area

2. Performed data preprocessing and feature selection using EDA, Correlation Matrix, Chi square test and ANOVA to train the model

3. Applied Random Forest for classification, tuned parameters of rf model to obtain test accuracy of 77% with 1000 trees

4. Improved test accuracy to 82.5% using Gradient Boosting Machine with 1500 trees

