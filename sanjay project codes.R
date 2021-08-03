#Analysis Tasks: Analyze the historical data and determine the key drivers for admission.
getwd()
dataset <- read.csv("College_admission.csv")
View(dataset)

#A->predictive tasks
#task1 -to find missing value in data set.?if found perform missing value treatment.
x=is.na(dataset)
View(x)
#there is no missing value in the data set.


#task2-Find outliers (if any, then perform outlier treatment)
#although admit,gpa,ses,Gender_Male,Race,rank are cetagorical data still as per said lets check it out for ouliers.
boxplot(dataset$admit,horizontal = T)
boxplot(dataset$ses,horizontal = T)
boxplot(dataset$Gender_Male,horizontal = T)
boxplot(dataset$Race,horizontal = T)
boxplot(dataset$rank,horizontal = T)
#gre and gra is a numerical/intiger data its important to find outliers here if any
OutVals = boxplot(dataset$gre,horizontal = T)$out
OutVals
#there are 4 outliers in dataset$gre or gre column
outvals1=boxplot(dataset$gpa,horizontal = T)$out
outvals1
#there are 1 outliers in dataset$gpa or gpa column
#oulier treatment













#task3-Find the structure of the data set and if required, transform the numeric data type to factor and vice-versa.
str(dataset)
#admit,ses,Gender_Male, Race,rank are cetagorical detatype thay need to be factor but in structure of deta set it is numeric/integer.
dataset$ses=as.factor(dataset$ses)
dataset$Gender_Male=as.factor(dataset$Gender_Male)
dataset$Race=as.factor(dataset$Race)
dataset$rank=as.factor(dataset$rank)
str(dataset)


#task4-Find whether the data is normally distributed or not. Use the plot to determine the same. 
mean(dataset$gre)
median(dataset$gre)
#here mean is not equal to median in case of gre column of dataset hance data is not normaly distributed
#by visualization 
plot(density(dataset$gre))
abline(v = c(mean(dataset$gre),
             median(dataset$gre)),
       col = c('green', 'steelblue'))
#in visualization also mean is not equal to median in case of gre column of dataset hance data is not normaly distributed 
mean(dataset$gpa)
median(dataset$gpa)
#here mean is not equal to median in case of gpa column of dataset hance data is not normaly distributed
#by visualization 
plot(density(dataset$gpa))
abline(v = c(mean(dataset$gpa),
             median(dataset$gpa)),
       col = c('green', 'steelblue'))
#in visualization also mean is not equal to median in case of gpa column of dataset hance data is not normaly distributed 


#task5-Normalize the data if not normally distributed.
#computing The skewness coefficient
#install.packages("moments")
library(moments)
skewness(dataset$gre, na.rm = TRUE)
skewness(dataset$gpa, na.rm = TRUE)
#both gre and gpa distribution have negative skewness.
#install.packages("ggpubr")
library(ggplot2)
library(magrittr)
library(ggpubr)
dataset$gre = sqrt(dataset$gre)
dataset$gpa = sqrt(dataset$gpa)
plot(density(dataset$gre))
abline(v = c(mean(dataset$gre),
             median(dataset$gre)),
       col = c('green', 'steelblue'))
plot(density(dataset$gpa))
abline(v = c(mean(dataset$gpa),
             median(dataset$gpa)),
       col = c('green', 'steelblue'))
mean(dataset$gre)
median(dataset$gre)
mean(dataset$gpa)
median(dataset$gpa)
#now the data is normally distributed


#task6-Use variable reduction techniques to identify significant variables.
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(125)
split = sample.split(dataset$admit,SplitRatio = 0.8)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = admit ~.,data = training_set)
summary(regressor)
#backward elimination for eliminating 
regressor = lm(formula = admit ~gre+gpa+rank+Race+ses+Gender_Male,data = training_set)
summary(regressor)

regressor = lm(formula = admit ~gre+gpa+rank+Race+ses,data = training_set)
summary(regressor)

regressor = lm(formula = admit ~gre+gpa+rank+ses,data = training_set)
summary(regressor)

regressor = lm(formula = admit ~gre+gpa+rank,data = training_set)
summary(regressor)

regressor = lm(formula = admit ~gre+rank,data = training_set)
summary(regressor)

regressor = lm(formula = admit ~rank,data = training_set)
summary(regressor)
#by the Use variable reduction techniques we identified significant variables.which are gre and rank.


#task7-Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables)
#Encoding the target feature as factor
dataset$admit = factor(dataset$admit,)
class(dataset$admit)
levels(dataset$admit)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(167)
split = sample.split(dataset$admit,SplitRatio = 0.75)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)
# Fitting Logistic Regression to the Training set
classifier = glm(formula = admit ~.,
                 family = binomial,
                 data = training_set)
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response',newdata = test_set) 
y_pred = ifelse(prob_pred>0.5 ,1 ,0)
#making confusion matrix for test set
m  = table(test_set[,1],y_pred>0.5)
#checking accuracy of the model over test set (acc0rding to task 8)
accuracy =(66+11)/(66+21+11+2)*100

classifier = glm(formula = admit ~gre+gpa+rank+Race+ses+Gender_Male,
                 family = binomial,
                 data = training_set)
summary(classifier)

classifier = glm(formula = admit ~gre+gpa+rank+Race+ses,
                 family = binomial,
                 data = training_set)
summary(classifier)

classifier = glm(formula = admit ~gre+gpa+rank+Race,
                 family = binomial,
                 data = training_set)
summary(classifier)

classifier = glm(formula = admit ~gre+gpa+rank,
                 family = binomial,
                 data = training_set)
summary(classifier)

classifier = glm(formula = admit ~gre+rank,
                 family = binomial,
                 data = training_set)
summary(classifier)

classifier1 = glm(formula = admit ~rank,
                  family = binomial,
                  data = training_set)
summary(classifier1)



#task 8-Calculate the accuracy of the model and run validation techniques
# Predicting the Test set results
prob_pred1 = predict(classifier1, type = 'response',newdata = test_set) 
y_pred1 = ifelse(prob_pred1>0.5 ,1 ,0)
#making confusion matrix for test set
m  = table(test_set[,1],y_pred>0.5)
accuracy_of_logistic_regression = ((66+11)/(66+21+11+2))*100
#accuracy of model have no effect by adding or removing insignificant factors from the model.



#task 9-Try other modelling techniques like decision tree and SVM and select a champion model
#1.svm
# Fitting SVM to the Training set, with kernel as linear
library(e1071)
classifier = svm(formula = admit~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# Predicting the Test set results
y_pred = predict(classifier,newdata = test_set[-1])
# Making the Confusion Matrix
cm  = table(test_set[,1],y_pred)
accuracy_of_svm= (68/100*100)
classifier = svm(formula = admit~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# Predicting the Test set results
y_pred = predict(classifier,newdata = test_set[-1])
# Making the Confusion Matrix
cm  = table(test_set[,1],y_pred)
# Fitting SVM to the Training set, with kernel as redial 
classifier = svm(formula = admit~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')
# Predicting the Test set results
y_pred = predict(classifier,newdata = test_set[-1])
# Making the Confusion Matrix
cm  = table(test_set[,1],y_pred)
accuracy =68/100*100

#2.knn
# Fitting KNN to the Training set
library(class)
y_pred = knn(train = training_set[,-1],
             test = test_set[,-1],
             cl = training_set[,1],
             k = 5,
             prob = T)  #3,5,7,9
# Making the Confusion Matrix
cm  = table(test_set[,1],y_pred)
accuracy_of_knn = (58+9)/(23+58+10+9)*100


#3-decision tree
# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = admit~ .,
                   data = training_set)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-1],type = 'class')
# Making the Confusion Matrix
cm = table(test_set[,1], y_pred)
accuracy_of_decision_tree = (62+9)/(62+9+23+6)*100
# Choosing the number of trees
plot(classifier)

#task10-Determine the accuracy rates for each kind of model.
accuracy_of_logistic_regression = ((66+11)/(66+21+11+2))*100 #77%
accuracy_of_svm= (68+0) /(68+32+0+0)*100                     #68%
accuracy_of_knn = (58+9)/(23+58+10+9)*100                    #68%
accuracy_of_decision_tree = (62+9)/(62+9+23+6)*100           #71%
#hance we chose logistic regression model and it is the chempion model.


#task11->Identify other Machine learning or statistical techniques














#B->Descriptive: 
#Categorize the average of grade point into High, Medium, and Low (with admission probability percentages) and plot it on a point chart.Cross grid for admission variables with GRE Categorization is shown below:
#GRE	Categorized
#0-440	Low
#440-580	Medium
#580+	High
