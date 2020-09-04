#install.packages("corrplot")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("gclus")
#install.packages("tree")
#install.packages("GGally")
#install.packages("factoextra")
#install.packages("useful")
library(tidyverse)
library(ggplot2)
library(ggplot2)
library(corrplot)
library(rpart) #used for regression and the various resudual plots
library(rpart.plot)
library(gclus)
library(tree) #used for generating the regression tree
library(GGally) #used for the kmeans clustering
library(cluster)
library(factoextra) #to include the fvizcluster function
library(useful) # by Jared Lander

asiacrop <- read_csv(file="C:/Users/chand/Documents/ChandanaNarla/chandana/CourseWork/Sem_1/Assignments/Stat515/FinalProject/Production_Crops_E_Asia.csv")

#asiacrop=read.csv(file = "Production_Crops_E_Asia.csv",header = TRUE)
#asiacrop
summary(asiacrop)
sum(is.na(asiacrop))


#Here we need the data from 2008 to 2013 for predicting the crops production yeild in 2014
#hence considering the required column for analysis
asia_subset1<-asiacrop[,c(1:7,102,104,106,108,110,112,114)]
#datacleaning
sum(is.na(asia_subset1))
#omitting the null values as the mean of the values is large
asia_subset1<-na.omit(asia_subset1)
#check whether the null values are removed
sum(is.na(asia_subset1))



#Subsetting the data in terms of production
subset_4<-pivot_wider(data=asia_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c(Area,Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
subset_4=na.omit(subset_4)

#Pairscorrelation plot
#cparis(subset_4)
pairs(subset_4[3:9],colour='blue')

#Linear Regression Model 1 for predicting the Production of Year 2014 using year 2013
set.seed(100)
trainingIndex1<- sample(1:nrow(subset_4),0.8*nrow(subset_4))
trainingData1<-subset_4[trainingIndex1,]
testData1<-subset_4[-trainingIndex1,]

lmModel1<-lm(Y2014_Production~Y2013_Production, data=trainingData1)
#predict_2 = predict(lmModel1,data.frame(Y2013_Production = 4223))
ElementPredict1<- predict(lmModel1,testData1)
summary(lmModel1)
summary(ElementPredict1)

actual_Predict<-data.frame(cbind(actuals=testData1$Y2014_Production,predicteds=ElementPredict1))
corr_accuracy<-cor(actual_Predict)


#Linear Regression Model 2 for predicting the Production of Year 2014 using years from 2013-2008
trainingIndex<- sample(1:nrow(subset_4),0.8*nrow(subset_4))
trainingData<-subset_4[trainingIndex,]
testData<-subset_4[-trainingIndex,]
lmModel2<-lm(Y2014_Production~Y2013_Production+Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production, data=trainingData)
#predict_2 = predict(lmModel2,data.frame(Y2013_Production = 4223,Y2013_Production,Y2013_Production,Y2013_Production,Y2013_Production))
ElementPredict2<- predict(lmModel2,testData)
summary(lmModel2)
par(mfrow=c(2,2))
plot(lmModel2)

actual_Predict1<-data.frame(cbind(actuals=testData$Y2014_Production,predicteds=ElementPredict2))
corr_accuracy1<-cor(actual_Predict1)

#Plot showing the Linear relationship between the predictors.
ggplot(data=subset_4,aes(x=Y2013_Production,y=Y2014_Production))+geom_point()+stat_smooth(method = 'lm',col="orange")+
  geom_line()


#Regression Trees

#We install the Tree package for the Regression Tree
tree_item=tree(Y2014_Production~Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production,subset_4,subset=trainingIndex)
summary(tree_item)
plot(tree_item)
text(tree_item,pretty=0,cex=0.9)
tree_item

#Cross validation plot for REgression tree
cv_treeitem<-cv.tree(tree_item)
plot(cv_treeitem)

plot(cv_treeitem$size,cv_treeitem$dev,type="b")
plot(cv_treeitem$k,cv_treeitem$dev,type="b")

#KMeans Clustering

cropTrain <- select(subset_4, -Area, -Item)
cropTrain

ggscatmat(cropTrain)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(100)
cropTrain = scale(cropTrain)
head(cropTrain)
crop <- kmeans(x=cropTrain, centers=3)
# crop


plot.kmeans(crop, data=cropTrain)

fviz_cluster(crop, data = cropTrain)

#Now creating another subset to include Area Code (Region) and item code (Various items)
subset_5<-pivot_wider(data=asia_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c('Area Code',Area,'Item Code',Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
subset_5=na.omit(subset_5)

cropTrain1 <- select(subset_5, -Area, -Item)
cropTrain1
ggscatmat(cropTrain1)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(200)
cropTrain1 = scale(cropTrain1)
head(cropTrain1)
crop1 <- kmeans(x=cropTrain1, centers=3)
crop1
plot.kmeans(crop1, data=cropTrain1)
fviz_cluster(crop1, data = cropTrain1)


#Other Continents comparision


##################################   AFRICA   #############################################################

africacrop <- read_csv(file="C:/Users/chand/Documents/ChandanaNarla/chandana/CourseWork/Sem_1/Assignments/Stat515/FinalProject/Production_Crops_E_Africa.csv")
#africacrop=read_csv(file = "Production_Crops_E_Africa.csv",header = TRUE)
#africacrop
summary(africacrop)
sum(is.na(africacrop))


#Here we need the data from 2008 to 2013 for predicting the crops production yeild in 2014
#hence considering the required column for analysis
africa_subset1<-africacrop[,c(1:7,102,104,106,108,110,112,114)]
#datacleaning
sum(is.na(africa_subset1))
#omitting the null values as the mean of the values is large
africa_subset1<-na.omit(africa_subset1)
#check whether the null values are removed
sum(is.na(africa_subset1))


#Subsetting the data in terms of production
africa_subset_4<-pivot_wider(data=africa_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c(Area,Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
africa_subset_4=na.omit(africa_subset_4)

#Linear Regression Model 1 for predicting the Production of Year 2014 using year 2013
set.seed(100)
trainingIndex3<- sample(1:nrow(africa_subset_4),0.8*nrow(africa_subset_4))
trainingData3<-africa_subset_4[trainingIndex3,]
testData3<-africa_subset_4[-trainingIndex3,]

lmModel3<-lm(Y2014_Production~Y2013_Production, data=trainingData3)
ElementPredict3<- predict(lmModel3,testData3)
summary(lmModel3)
summary(ElementPredict3)

actual_Predict<-data.frame(cbind(actuals=testData3$Y2014_Production,predicteds=ElementPredict3))
corr_accuracy<-cor(actual_Predict)


#Linear Regression Model 2 for predicting the Production of Year 2014 using years from 2013-2008
trainingIndex4<- sample(1:nrow(africa_subset_4),0.8*nrow(africa_subset_4))
trainingData4<-subset_4[trainingIndex4,]
testData4<-africa_subset_4[-trainingIndex4,]
lmModel4<-lm(Y2014_Production~Y2013_Production+Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production, data=trainingData)
ElementPredict4<- predict(lmModel4,testData4)
summary(lmModel4)
par(mfrow=c(2,2))
plot(lmModel4)

actual_Predict4<-data.frame(cbind(actuals=testData4$Y2014_Production,predicteds=ElementPredict4))
corr_accuracy4<-cor(actual_Predict4)

#Plot showing the Linear relationship between the predictors.
ggplot(data=africa_subset_4,aes(x=Y2013_Production,y=Y2014_Production))+geom_point()+stat_smooth(method = 'lm',col="orange")+
  geom_line()


#Regression Trees

#We install the Tree package for the Regression Tree
tree_item1=tree(Y2014_Production~Y2013_Production+Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production,subset_4,subset=trainingIndex)
summary(tree_item1)
plot(tree_item1)
text(tree_item1,pretty=0,cex=0.9)
tree_item1


#KMeans Clustering

cropTrain2 <- select(africa_subset_4, -Area, -Item)
cropTrain2

ggscatmat(cropTrain2)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(100)
cropTrain2 = scale(cropTrain2)
head(cropTrain)
crop2 <- kmeans(x=cropTrain2, centers=3)
crop2
plot.kmeans(crop2, data=cropTrain2)
fviz_cluster(crop2, data = cropTrain2)
#Now creating another subset to include Area Code (Region) and item code (Various items)
subset_6<-pivot_wider(data=africa_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c('Area Code',Area,'Item Code',Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
subset_6=na.omit(subset_6)

cropTrain3 <- select(subset_5, -Area, -Item)
cropTrain3
ggscatmat(cropTrain3)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(200)
cropTrain3 = scale(cropTrain3)
head(cropTrain3)
crop3 <- kmeans(x=cropTrain3, centers=3)
crop3

plot.kmeans(crop3, data=cropTrain3)

fviz_cluster(crop3, data = cropTrain3)


###############################   EUROPE  ################################################################

Europecrop <- read_csv(file="C:/Users/chand/Documents/ChandanaNarla/chandana/CourseWork/Sem_1/Assignments/Stat515/FinalProject/Production_Crops_E_Europe.csv")
#Europecrop
summary(Europecrop)
sum(is.na(Europecrop))


#Here we need the data from 2008 to 2013 for predicting the crops production yeild in 2014
#hence considering the required column for analysis
Europe_subset1<-Europecrop[,c(1:7,102,104,106,108,110,112,114)]
#datacleaning
sum(is.na(Europe_subset1))
#omitting the null values as the mean of the values is large
Europe_subset1<-na.omit(Europe_subset1)
#check whether the null values are removed
sum(is.na(Europe_subset1))


#Subsetting the data in terms of production
Europecrop_subset_4<-pivot_wider(data=Europe_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c(Area,Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
Europecrop_subset_4=na.omit(Europecrop_subset_4)

#Linear Regression Model 1 for predicting the Production of Year 2014 using year 2013
set.seed(100)
trainingIndex5<- sample(1:nrow(Europecrop_subset_4),0.8*nrow(Europecrop_subset_4))
trainingData5<-Europecrop_subset_4[trainingIndex5,]
testData5<-Europecrop_subset_4[-trainingIndex1,]

lmModel5<-lm(Y2014_Production~Y2013_Production, data=trainingData5)
ElementPredict5<- predict(lmModel5,testData5)
summary(lmModel5)
summary(ElementPredict5)

#Linear Regression Model 2 for predicting the Production of Year 2014 using years from 2013-2008
trainingIndex6<- sample(1:nrow(Europecrop_subset_4),0.8*nrow(Europecrop_subset_4))
trainingData6<-Europecrop_subset_4[trainingIndex6,]
testData6<-Europecrop_subset_4[-trainingIndex6,]
lmModel6<-lm(Y2014_Production~Y2013_Production+Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production, data=trainingData)
ElementPredict6<- predict(lmModel6,testData6)
summary(lmModel6)
par(mfrow=c(2,2))
plot(lmModel6)

actual_Predict6<-data.frame(cbind(actuals=testData6$Y2014_Production,predicteds=ElementPredict6))
corr_accuracy6<-cor(actual_Predict6)

#Plot showing the Linear relationship between the predictors.
ggplot(data=Europecrop_subset_4,aes(x=Y2013_Production,y=Y2014_Production))+geom_point()+stat_smooth(method = 'lm',col="orange")+
  geom_line()


#Regression Trees

#We install the Tree package for the Regression Tree
tree_item2=tree(Y2014_Production~Y2012_Production+Y2011_Production+Y2010_Production+Y2009_Production+Y2008_Production,subset_4,subset=trainingIndex)
summary(tree_item2)
plot(tree_item2)
text(tree_item2,pretty=0,cex=0.9)
tree_item2

#KMeans Clustering

cropTrain4 <- select(Europecrop_subset_4, -Area, -Item)
cropTrain4

ggscatmat(cropTrain4)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(100)
cropTrain4 = scale(cropTrain4)
head(cropTrain4)
crop4 <- kmeans(x=cropTrain4, centers=3)
crop4


plot.kmeans(crop4, data=cropTrain4)

fviz_cluster(crop4, data = cropTrain4)

#Now creating another subset to include Area Code (Region) and item code (Various items)
subset_6<-pivot_wider(data=Europe_subset1,names_from = Element,values_from = c(Y2014,Y2013,Y2012,Y2011,Y2010,Y2009,Y2008))%>%select(c('Area Code',Area,'Item Code',Item,Y2014_Production,Y2013_Production,Y2012_Production,Y2011_Production,Y2010_Production,Y2009_Production,Y2008_Production))
subset_6=na.omit(subset_6)

cropTrain5 <- select(subset_6, -Area, -Item)
cropTrain5
ggscatmat(cropTrain5)

RNGkind(sample.kind="Rounding") # To obtain same results as RFE 
set.seed(200)
cropTrain5 = scale(cropTrain5)
head(cropTrain5)
crop5 <- kmeans(x=cropTrain5, centers=3)
crop5

plot.kmeans(crop5, data=cropTrain5)

fviz_cluster(crop5, data = cropTrain5)








