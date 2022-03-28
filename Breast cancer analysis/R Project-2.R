#Installing Packages
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("corrplot")
install.packages("caret")
install.packages("e1071")
install.packages("ggcorrplot")

#Reading Data 
df <- read.csv("B_Cancer.csv")
head(df)

#Removing ID from dataset
df <- subset (df, select = -id)
head(df)

#Library
library(lattice)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(e1071)
library(ggcorrplot)

glimpse(df)

#Data Transformation
df1 <- df %>%
  mutate (diagnosis = if_else(diagnosis == 1,'Malignant', 'Benign')
  ) %>%
  mutate_if(is.character, as.factor) 

head(df1)

#Validation Dataset
#We will split the loaded dataset into two, 80% of which we will use to train our models 
#and 20% that we will hold back as a validation dataset.
validation_index <- createDataPartition(df1$diagnosis, p=0.80, list=FALSE)
#20% for data validation
validation <- df1[-validation_index,]
#using 80% of data to training & testing models
df1 <- df1[validation_index,]

#Splitting data into 10 parts using K fold CV
vald_10 <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

#Models
#1 KNN
set.seed(7) # sets the starting number used to generate a sequence of random numbers 
fit.knn <- train(diagnosis~., data=df1, method="knn", metric=metric, trControl=vald_10)
#2 Random Forest
set.seed(7)
fit.rf <- train(diagnosis~., data=df1, method="rf", metric=metric, trControl=vald_10)

#Accuracy of models
results <- resamples(list(knn=fit.knn, rf=fit.rf))
summary(results)

#Comparing Accuracy of Models
dotplot(results)

#Prediction
#using rf for prediction
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$diagnosis)

#Data Visualization

#1 Bar Plot for target
ggplot(df1, aes(x=diagnosis, fill= diagnosis))+
  geom_bar()+
  xlab("Diagnosis")+
  ylab("Count")+
  ggtitle("Ratio of Diagnosis for Breast Cancer")+
  scale_fill_discrete(name= "Diagnosis", labels= c("Benign", "Malignant"))

prop.table(table(df1$diagnosis))


#2 Random Forest prediction in test set
ggplot(validation, aes(x=diagnosis, fill= diagnosis))+
  geom_bar()+
  xlab("Prediction")+
  ylab("Count")+
  ggtitle("Random Forest Prediction")+
  scale_fill_discrete(name= "Diagnosis", labels= c("Benign", "Malignant"))

prop.table(table(validation$diagnosis))

#3 radius_mean vs Frequency
df1 %>%
  ggplot(aes(x=radius_mean, y=diagnosis))+
  geom_boxplot(fill = 'blue')+
  xlab("radius_mean")+
  ylab("Diagnosis")


#4 radius_mean vs area_mean
df1 %>%
  ggplot(aes(x=radius_mean, y=area_worst, colour=as.factor(diagnosis), shape=as.factor(diagnosis)))+
  geom_jitter()+
  scale_colour_manual(values = c("turquoise","red"))+
  xlab("radius_mean")+
  ylab("area_mean")


#5 Correlation Matrix
cor_bc <- cor(df1[, 2:11])
corr = round(cor(cor_bc),1)
corr

corrplot(corr, method="square", type="lower") 





