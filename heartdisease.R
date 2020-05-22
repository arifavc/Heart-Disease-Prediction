# install.packages('ggplot2')


#Load libraries

knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(corrplot)
library(caret)
library(GGally)
library(ggthemes)
library(broom)
library(dplyr)
library(bindrcpp)
library(caTools)
library(rattle)
library(RColorBrewer)
library(nnet)
library(rpart.plot)


#Set Directory
getwd()
setwd('C:/Users/YOGA/Desktop/RProje')

#Read data
data <- read.csv("heart.csv")



#Overview of heart disease dataset
dim(data)#data dimension
str(data) # To check the structure of our data
head(data) # we use head function when we want to see and check the first six observation of our data.
summary(data) # To see the summary of our data




# According to above observation we implementing the changes:
colnames(data)[1]<-"age"
data$sex<-as.factor(data$sex)
data$cp<-as.factor(data$cp)
data$fbs<-as.factor(data$fbs)
data$exang<-as.factor(data$exang)
data$restecg<-as.factor(data$restecg)
data$slope<-as.factor(data$slope)
data$thal<-as.factor(data$thal)
data$target<-as.factor(data$target)
str(data)
levels(data$sex)[levels(data$sex)==0] <- "Female"
levels(data$sex)[levels(data$sex)==1] <- "Male"
levels(data$fbs)[levels(data$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(data$fbs)[levels(data$fbs)==1] <- "Fasting Blood Sugar > 120"
levels(data$thal)[levels(data$thal)==0] <- "No Thalassemia"
levels(data$thal)[levels(data$thal)==1] <- "Normal Thalassemia"
levels(data$thal)[levels(data$thal)==2] <- "Fixed Defect Thalassemia"
levels(data$thal)[levels(data$thal)==3] <- "Reversible Defect Thalassemia"
levels(data$target)[levels(data$target)==0] <- "Healthy"
levels(data$target)[levels(data$target)==1] <- "Heart Disease"
levels(data$exang)[levels(data$exang)==1] <- "Exercise Induced Angina"
levels(data$exang)[levels(data$exang)==0] <- "No Exercise Induced Angina"
levels(data$cp)[levels(data$cp)==0] <- "Chest Pain Type 0"
levels(data$cp)[levels(data$cp)==1] <- "Chest Pain Type 1"
levels(data$cp)[levels(data$cp)==2] <- "Chest Pain Type 2"
levels(data$cp)[levels(data$cp)==3] <- "Chest Pain Type 3"
levels(data$restecg)[levels(data$restecg)==0] <- "Rest ECG 0"
levels(data$restecg)[levels(data$restecg)==1] <- "Rest ECG 1"
levels(data$restecg)[levels(data$restecg)==2] <- "Rest ECG 2"
levels(data$slope)[levels(data$slope)==0] <- "Peak Excercise ST Slope 0"
levels(data$slope)[levels(data$slope)==1] <- "Peak Excercise ST Slope 1"
levels(data$slope)[levels(data$slope)==2] <- "Peak Excercise ST Slope 2"


# Checking whether the above changes are implemented or not:
str(data)
summary(data)
##########################

sum(is.na(data))# Our data is clean , there is no NA


############### Exploratory Data Analysis ####################
# The target variable contains the information about the disease status (1: Diagnosed heart diseases ;0: No diagnosed heart disease). Lets analyze this varaible
# Number of observations: Healthy and Heart Disease cases
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) 

# As we can see above that there are almost equal number of individuals who are normal and have heart problem.

# Age can be one of the most important contributing factor in the heart diseases. So lets check it out. Lets first check the overall age distribution of the individuals present in the dataset.
summary(data$age)
sd(data$age)
boxplot(data$age ~ data$target,
        main="Age Distribution",
        ylab="Age",xlab="Heart disease")


#Heart Disease is uniformly spread out across Age
ggplot(data,aes(age, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  xlab("Age") +
  ylab("Density / Count") +
  ggtitle("Age Histogram")



ggplot(data,aes(x = age)) + geom_histogram(bins =30,fill ="dodgerblue4") + theme_bw() + theme_classic() +
  ggtitle("age distribution") +ylab("number of people")


ggplot(data,aes(x = age)) + geom_density(fill ="dodgerblue4") +
  theme_bw() + theme_classic() +ggtitle("age distribution") +
  ylab("number of people")


#Sex
mosaicplot(data$sex ~ data$target,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")


ggplot(data,aes(x =sex)) + geom_bar(width = 0.2,fill ="gray") +
  geom_text(stat = 'count',aes(label =..count..),vjust =-0.5) +
  theme_bw() + theme_classic() +ylab("number of count") +
  ggtitle("sex") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=sex)) + geom_bar(fill="green") + facet_wrap(~target)


#No major difference in Rest ECG for Healthy and Heart Disease patients
ggplot(data,aes(trestbps, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(90, 200, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  xlab("Resting Blood Pressure (in mm Hg on admission to the hospital") +
  ylab("Density / Count") +
  ggtitle("Rest ECG Histogram")



#More Heart Disease patients seem to have between 200 and 250 mg/dl
ggplot(data,aes(chol, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(100, 600, by=25), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  xlab("Serum Cholestoral in mg/dl") +
  ylab("Density / Count") +
  ggtitle("Cholestoral Histogram")


#Heart Disease patients have higher maximum heart rate than healthy patients
ggplot(data,aes(thalach, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(70, 205, by=10), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  xlab("Maximum Heart Rate Achieved") +
  ylab("Density / Count") +
  ggtitle("Max Heart Rate Histogram")


#More Heart Disease patients have ST depression of 0.1
ggplot(data,aes(oldpeak, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 7, by=0.1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("ST Depression Histogram") +
  xlab("ST Depression Induced by Exercise Relative to Rest") +
  ylab("Density / Count")



#More females have Heart Disease
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~sex, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) 


#More Heart Disease patients have chest pain type 1 or 2
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~cp, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) 


#No difference in fasting blood sugar

ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~fbs, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) 


# Patients with Rest ECG 1 have more Heart Diseases
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~restecg, ncol=3,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))


# Patients with no exercise induced angina have more Heart Disease
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~exang, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))


# Fixed defect thalasemia has more Heart Disease
ggplot(data,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~thal, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))



# correlations
correlations <- cor(data[,1:11])
print(correlations)
corrplot(correlations,method="circle")





  # We can see below that only a few of the parameters significantly has an effect on Heart Disease
# The plots for categorical and numeric variables suggest the 
# following conditions are associated with increased prevalence of heart 
# disease:
# Gender 
# Chest Pain Type
# Excercise Induced Angina
# ST Depression & No. of vessels observed by fluroscopy 
# are the only variables that has a significant effect on Heart Disease
# The rest of the parameters can be excluded and dropped from our analysis
# This provides a nice phase gate to let us proceed with the analysis.


log<-glm(target~., data=data, family=binomial)
summary(log)
log.df<-tidy(log)


# Taking only the significant variables and summarising
d<-data[,c(2,3,9,10,12,14)]
summary(d)


# Plotting the coefficients
log.df %>%
  mutate(term=reorder(term,estimate)) %>%
  ggplot( aes(term,estimate, fill=estimate)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="firebrick2", high="springgreen2") +
  theme_economist() +
  geom_hline(yintercept=0) +
  coord_flip()
# If the patient has chest pain type 2 or 3, the probability of them having Heart Disease rises
# More number of blood vessels that are visible by fluroscopy, the lower the chances of Heart Disease
# More number of blood vessels that are visible by fluroscopy, the lower the chances of Heart Disease
# Also, higher the ST Depression, lower the chances of Heart Disease
# If a patient has excercise induced angina, then the probability of them having Heart Disease reduces
# If male, then also less chances of Heart Disease





data<-d
set.seed(1237)
train <- sample(nrow(data), .8*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)



TrainSet$target<-make.names(TrainSet$target)
set.seed(142)
TrainSet$target<-as.factor(TrainSet$target)


# Training with random forest
# Best ROC = 89%
# Sensitivity = 78%
# Specificity = 86%

gbm.ada.1 <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "rf", 
                          trControl = fitControl,
                          metric="ROC")

gbm.ada.1


#Variable importance. 
varImp(gbm.ada.1)



# Predicting on the Test Set
# The confusion matrix and the accuracy on the test set is output
# Accuracy = 81%
# p-Value is highly significant
pred <- predict(gbm.ada.1,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(ValidSet$target, pred)
t.df<-as.data.frame(t)
res<-caret::confusionMatrix(t, positive="Heart Disease")
res



# Confusion Matrix for Random Forest
# A confusion matrix is a visual way to display the results of the model’s 
# predictions. It’s not just the ability to predict the presence of heart 
# disease that is of interest – we also want to know the number of times the 
# model successfully predicts the absence of heart disease. Likewise, we want 
# to know the number of false positives and false negatives. 
# The confusion matrix captures all these metrics nicely.
ggplot(data = t.df, aes(x = Var1, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="firebrick2", high="springgreen2") +
  theme_economist() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Random Forest")

dim(data)
dim(TrainSet)
dim(ValidSet)



