#______________________________________________________________________________
# Course name: Introduction to R programming  
# Student name: Razan Muhammed Aljuhani
# Student Email: razanmaljuhani@gmail.com
# Project 1 - Analysis of Sales Report of a Clothes Manufacturing Outlet
#______________________________________________________________________________

# import libraries
library(plyr) # used for splitting, applying and combining data. 
library(caTools) # used for moving window statistics, GIF and other. 
library(e1071) # used for analysis and prediction. use of Naive Bayes and Support Vector Machine.
library(caret)  # used for classification and liner regression of data.
library(readxl) # used for get data from excel file.
library(randomForest) # used for create random forests(decision trees).

# Take datasets
Attribute_DataSet <- read_excel("Attribute DataSet.xlsx")
Dress_Sales <- read_excel("Dress Sales.xlsx")

# View original datasets
View(Attribute_DataSet)
View(Dress_Sales)

# Delete Dress_ID column from datasets
Attribute_DataSet_1 <- Attribute_DataSet[2:14] 
Dress_Sales_1 <- Dress_Sales[2:24]

# View updated datasets
View(Attribute_DataSet_1)
View(Dress_Sales_1)

#_____________________________________________________________________________________________
# Checking of uniqueness of values for each columns in Attribute_DataSet_1 

# Style attributes 
Attribute_DataSet_1$Style[Attribute_DataSet_1$Style == 'sexy'] = 'Sexy'

# Price attributes
Attribute_DataSet_1$Price[Attribute_DataSet_1$Price == 'low'] = 'Low'
Attribute_DataSet_1$Price[Attribute_DataSet_1$Price == 'high'] = 'High'

# Size attributes
Attribute_DataSet_1$Size[Attribute_DataSet_1$Size == 's'] = 'S' 
Attribute_DataSet_1$Size[Attribute_DataSet_1$Size == 'small'] = 'S'

# Season attributes
Attribute_DataSet_1$Season[Attribute_DataSet_1$Season == 'spring'] = 'Spring'
Attribute_DataSet_1$Season[Attribute_DataSet_1$Season == 'summer'] = 'Summer'
Attribute_DataSet_1$Season[Attribute_DataSet_1$Season == 'Automn'] = 'Autumn'
Attribute_DataSet_1$Season[Attribute_DataSet_1$Season == 'winter'] = 'Winter'

# NeckLine attributes
Attribute_DataSet_1$NeckLine[Attribute_DataSet_1$NeckLine == 'sweetheart'] = 'Sweetheart'

# SleeveLength attributes
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'sleevless'] = 'sleeveless' 
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'sleeevless'] = 'sleeveless' 
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'sleveless'] = 'sleeveless' 
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'threequater'] = 'threequarter' 
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'thressqatar'] = 'threequarter' 
Attribute_DataSet_1$SleeveLength[Attribute_DataSet_1$SleeveLength == 'urndowncollor'] = 'turndowncollar' 

# FabricType attributes
Attribute_DataSet_1$FabricType[Attribute_DataSet_1$FabricType == 'shiffon'] = 'chiffon'
Attribute_DataSet_1$FabricType[Attribute_DataSet_1$FabricType == 'sattin'] = 'satin'
Attribute_DataSet_1$FabricType[Attribute_DataSet_1$FabricType == 'wollen'] = 'woolen'
Attribute_DataSet_1$FabricType[Attribute_DataSet_1$FabricType == 'flannael'] = 'flannel'
Attribute_DataSet_1$FabricType[Attribute_DataSet_1$FabricType == 'knitting'] = 'knitted'

# Decoration attributes
Attribute_DataSet_1$Decoration[Attribute_DataSet_1$Decoration == 'embroidary'] = 'embroidery'
Attribute_DataSet_1$Decoration[Attribute_DataSet_1$Decoration == 'sequined'] = 'sequins'
Attribute_DataSet_1$Decoration[Attribute_DataSet_1$Decoration == 'ruched'] = 'ruche'
Attribute_DataSet_1$Decoration[Attribute_DataSet_1$Decoration == 'none'] = 'null'

# Pattern Type attributes
Attribute_DataSet_1$'Pattern Type'[Attribute_DataSet_1$'Pattern Type' == 'none'] = 'null' 
Attribute_DataSet_1$'Pattern Type'[Attribute_DataSet_1$'Pattern Type' == 'leapord'] = 'leopard'

#_____________________________________________________________________________________________
# Factoring of data attributes. 

# Style attributes 
Attribute_DataSet_1$Style = factor(Attribute_DataSet_1$Style, 
                          levels = c('Sexy', 'Casual', 'vintage', 'Brief', 'cute', 'bohemian', 'Novelty', 'Flare', 'party', 'work', 'OL', 'fashion'),
                          labels = c(0,1,2,3,4,5,6,7,8,9,10,11))
# Price attributes 
Attribute_DataSet_1$Price = factor(Attribute_DataSet_1$Price, 
                          levels = c('Low', 'High', 'Average', 'Medium', 'very-high'),
                          labels = c(0,1,2,3,4))
# Size attributes 
Attribute_DataSet_1$Size = factor(Attribute_DataSet_1$Size, 
                         levels = c('M', 'L', 'XL', 'free', 'S'),
                         labels = c(0,1,2,3,4))
# Season attributes
Attribute_DataSet_1$Season = factor(Attribute_DataSet_1$Season, 
                           levels = c('Summer', 'Autumn', 'Spring', 'Winter'),
                           labels = c(0,1,2,3))
# NeckLine attributes
Attribute_DataSet_1$NeckLine = factor(Attribute_DataSet_1$NeckLine, 
                             levels = c('o-neck', 'v-neck', 'boat-neck', 'peterpan-collor', 'ruffled', 'turndowncollor', 'slash-neck', 'mandarin-collor', 'open', 'sqare-collor', 'Sweetheart', 'Scoop', 'halter', 'backless', 'bowneck', 'NULL'),
                             labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

# SleeveLength attributes
Attribute_DataSet_1$SleeveLength = factor(Attribute_DataSet_1$SleeveLength, 
                                 levels = c('sleeveless', 'Petal', 'full', 'butterfly', 'short', 'threequarter', 'halfsleeve', 'cap-sleeves', 'turndowncollor', 'capsleeves', 'half', 'turndowncollar', 'NULL'),
                                 labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
# waiseline attributes
Attribute_DataSet_1$waiseline = factor(Attribute_DataSet_1$waiseline, 
                              levels = c('empire', 'natural', 'null', 'princess', 'dropped'),
                              labels = c(0,1,2,3,4))
# Material attributes
Attribute_DataSet_1$Material = factor(Attribute_DataSet_1$Material, 
                             levels = c('null', 'microfiber', 'polyster', 'silk', 'chiffonfabric', 'cotton', 'nylon', 'other', 'milksilk', 'linen', 'rayon', 'lycra', 'mix', 'acrylic', 'spandex', 'lace', 'modal', 'cashmere', 'viscos', 'knitting', 'sill', 'wool', 'model', 'shiffon'),
                             labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
# FabricType attributes
Attribute_DataSet_1$FabricType = factor(Attribute_DataSet_1$FabricType, 
                               levels = c('chiffon', 'null', 'broadcloth', 'jersey', 'other', 'batik', 'satin', 'flannel', 'worsted', 'woolen', 'poplin', 'dobby', 'knitted', 'tulle', 'organza', 'lace', 'Corduroy', 'terry'),
                               labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
# Decoration attributes
Attribute_DataSet_1$Decoration = factor(Attribute_DataSet_1$Decoration, 
                               levels = c('ruffles', 'null', 'embroidery', 'bow', 'lace', 'beading', 'sashes', 'hollowout', 'pockets', 'sequins', 'applique', 'button', 'Tiered', 'rivet', 'feathers', 'flowers', 'pearls', 'pleat', 'crystal', 'ruche', 'draped', 'tassel', 'plain', 'cascading'),
                               labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
# `Pattern Type` attributes
Attribute_DataSet_1$`Pattern Type` = factor(Attribute_DataSet_1$`Pattern Type`, 
                                   levels = c('animal', 'print', 'dot', 'solid', 'null', 'patchwork', 'striped', 'geometric', 'plaid', 'leopard', 'floral', 'character', 'splice'),
                                   labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
# Recommendation attributes 
Attribute_DataSet_1$Recommendation = sapply(Attribute_DataSet_1$Recommendation, factor)

#_____________________________________________________________________________________________
# Calculate the number of missing values in (attribset_)  which is without Dress_ID column 
colSums(is.na(Attribute_DataSet_1))

#_____________________________________________________________________________________________
# Create the getMode function and fill the missing values of attributes .
getMode <- function(value) {
  uniqueValue <- unique(value)
  uniqueValue[which.max(tabulate(match(value, uniqueValue)))]
}

# The missing values will filled with mode
# Price
Attribute_DataSet_1$Price[is.na(Attribute_DataSet_1$Price) ==TRUE] <- getMode(Attribute_DataSet_1$Price)
# Season
Attribute_DataSet_1$Season[is.na(Attribute_DataSet_1$Season) ==TRUE] <- getMode(Attribute_DataSet_1$Season)
# NeckLine
Attribute_DataSet_1$NeckLine[is.na(Attribute_DataSet_1$NeckLine) ==TRUE] <- getMode(Attribute_DataSet_1$NeckLine)
# waiseline
Attribute_DataSet_1$waiseline[is.na(Attribute_DataSet_1$waiseline) ==TRUE] <- getMode(Attribute_DataSet_1$waiseline)
# Material
Attribute_DataSet_1$Material[is.na(Attribute_DataSet_1$Material) ==TRUE] <- getMode(Attribute_DataSet_1$Material)
# FabricType
Attribute_DataSet_1$FabricType[is.na(Attribute_DataSet_1$FabricType) ==TRUE] <- getMode(Attribute_DataSet_1$FabricType)
# Decoration
Attribute_DataSet_1$Decoration[is.na(Attribute_DataSet_1$Decoration) ==TRUE] <- getMode(Attribute_DataSet_1$Decoration)
# `Pattern Type`
Attribute_DataSet_1$`Pattern Type`[is.na(Attribute_DataSet_1$`Pattern Type`) ==TRUE] <- getMode(Attribute_DataSet_1$`Pattern Type`)

# Checking 
attribsetOfData <- data.frame(Attribute_DataSet_1)
str(attribsetOfData)

#_____________________________________________________________________________________________
# Change columns name in Dress_Sales_1 dataset

Dress_Sales_1 = rename(Dress_Sales_1,c('41314'='2/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41373'='4/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41434'='6/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41495'='8/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41556'='10/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41617'='12/9/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41315'='2/10/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41374'='4/10/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41435'='6/10/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('40400'='8/10/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41557'='10/10/2013'))
Dress_Sales_1 = rename(Dress_Sales_1,c('41618'='12/10/2013'))

# Convering all variable types in Dress_Sales_1 to numeric
Dress_Sales_1 <- as.data.frame(apply(Dress_Sales_1, 2, as.numeric))

# Calculating the mean row 
Dress_Sales_1 = as.matrix(Dress_Sales_1)
i <- which(is.na(Dress_Sales_1), arr.ind=TRUE)
Dress_Sales_1[i] <- rowMeans(Dress_Sales_1, na.rm=TRUE)[i[,1]]
Dress_Sales_1 = as.data.frame(Dress_Sales_1)

# Calculating the sum of all values on row on total_sales
Dress_Sales_1$total_sales = rowSums(Dress_Sales_1)
head(Dress_Sales_1)

#_____________________________________________________________________________________________
# Marging of data of the two datasets  
Marged_Data <- data.frame(Attribute_DataSet_1 ,Dress_Sales_1)
head(Marged_Data)

# Chaecking
str(Marged_Data)

#_____________________________________________________________________________________________
# Spliting dataset 
set.seed(100)
spl <- sample.split(Marged_Data$Recommendation, SplitRatio = 0.7)
train <- subset(Marged_Data, spl==TRUE)
test <- subset(Marged_Data, spl==FALSE)
#Printing 
print(dim(train)); print(dim(test))

#_____________________________________________________________________________________________
# Classification, predict of Recommendation attributes using 3 models
# 1. naive bayes model.
# Estabilish model
Naive_Model = naiveBayes(Recommendation ~.,data = train) 
# Create confusion Matrix
confusionMatrix(train$Recommendation,predict(Naive_Model,train),positive = '1') 
print('______________________________')
# Predict the test set using Naive_Model
naivePredict = predict(Naive_Model,test) 
# Create the table
table(naivePredict,test$Recommendation) 

#_________________________________
# 2. Support vector machine model.
# Estabilishing model
Support_Vector_Machine_Model = svm(Recommendation ~.,train) 
# Create confusion Matrix
confusionMatrix(train$Recommendation,predict(Support_Vector_Machine_Model),positive = '1')
print('______________________________')
# Predict the test set using svm model
svmPredict = predict(Support_Vector_Machine_Model,test) 
# Create table
table(svmPredict,test$Recommendation) 

#_________________________________
# 3. Random Forest model
# Estabilising model
randomForest_Model = randomForest(x = train, y = train$Recommendation,ntree =800)
# Create confusion Matrix
confusionMatrix(train$Recommendation,predict(randomForest_Model),positive = '1') 
print('______________________________')
# predict the test set using randomForest model
randomForestPredict = predict(randomForest_Model,test) 
# Create table
table(randomForestPredict,test$Recommendation )
#_____________________________________________________________________________________________
# Regression model 
# 1. Regression of (total sales and (Style+Season+Material+Price))
# Estabilishing model
Regressor_Sales = lm(formula = total_sales ~ Style+Season+Material+Price, data = train) 
# printing model summary
summary(Regressor_Sales) 
# Plotting the results
plot(Regressor_Sales, pch = 16, col = "blue") 
# Adding of regression line
abline(Regressor_Sales) 

#_____________________________________________________________________________________________
# Evaluatio 
basic = test$total_sales
pred = predict(Regressor_Rating,test)
predicted = pred
M = basic-predicted

# MSE
mse = mean((M)^2) 
# MAE
mae = mean(abs(M))
# RMSE
rmse = sqrt(mse) 
#R^2
R2 = 1-(sum((M)^2)/sum((basic-mean(basic))^2))

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "R-squared:", R2)

# end ^-^
