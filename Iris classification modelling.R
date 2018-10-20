install.packages(c("dmm","dplyr","plyr","reshape","data.table"))
install.packages(c("gglot2","psych","scales","gplots"))
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
# Load Libraries
library("ggplot2")
library("gplots")
library("psych")
library("scales")
setwd("/Users/ad/Desktop/edWisor/Projects/Iris_Dataclassification")
getwd()
iris_data= read.csv("iris data.csv")
df= iris_data[,1:5]
# Data exporation on iris dataset
colnames(df)
str(df)
dim(df)
head(df,5)
unique(df$class)
table(df$class)
summary(df)

# Outlier Analysis
## Box-plot distribution and outlier check
numeric_index= sapply(df, is.numeric)
# Selecting only numeric variables
numeric_data=df[,numeric_index]
cnames= colnames(numeric_data)

for (i in 1:length(cnames)) 
  {
  assign(paste0("gn",i), ggplot(aes_string(y= cnames[i], x= "class"), data= subset(df))+
           stat_boxplot(geom= "errorbar", width= 0.5)+
           geom_boxplot(outlier.colour= "red", fill= "grey", outlier.shape= 18, outlier.size= 1, notch= FALSE)+
         theme(legend.position="bottom")+
           labs(y= cnames[i], x= "class")+
         ggtitle(paste("Box plot for", cnames[i])))}
#Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=1)

rmExcept("iris_data")
# Divide dataset in train and test data using stratified sampling
set.seed(1234)
train.index= createDataPartition(df$class, p=0.7, list = FALSE)
train= df[train.index,]
test= df[-train.index,]

# Decision Tree for classification
# Develop model on training data
C50_model= C5.0(class~.,train, trials= 100, rules= TRUE)

# Summary of Decision Tree model
summary(C50_model)

# Write rules into disk
write(capture.output(summary(C50_model)),"c50rules.txt")

#Predict on to test cases
C50_predictions= predict(C50_model, test[,-5], type= "class")

# Evaluate performance of classification model
Confusion_C50= table(test$class, C50_predictions)
confusionMatrix(Confusion_C50)

#Accuracy
#93.33%
#False negative rate
#6.66%

# KNN Implementation

library(class)

# Predict test cases by KNN method

KNN_Predictions= knn(train[,1:4], test[,1:4], train$class, k=7)
conf_matrix= table(KNN_Predictions, test$class)

#Accuracy= 91.11%(k=3), 93.33%(k=5), 97.77%(k=7)
sum(diag(conf_matrix))/nrow(test)
#FNR= FN/FN+TP= 8.88%(k=3), 6.66(k=5), 2.22%(k=7)


