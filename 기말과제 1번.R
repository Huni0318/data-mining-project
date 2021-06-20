setwd('C:/Users/신상훈/Desktop/데이터 (1)/데이터')
#install.packages('mltools')
#install.packages('data.table')
install.packages('dummies')
library(data.table)
library(mltools)
library(dplyr)
library(caret)
library(RANN)
library(earth)
library(dummies)

data = read.csv('adult.txt',stringsAsFactors = FALSE,strip.white = TRUE,header = F)
head(data)
str(data)
dim(data)

str(data)
names(data) <- c('age','workclass','fnlwgt','education','education.num','marital.status','occupation','relationship','race','sex','capital.gain','capital.loss','hours.per.week','native.country','income')
str(data)
head(data)
colSums(is.na(data))

summary(data)
cor(data$age, data$capital.gain)
cor(data$capital.loss,data$age)


### 
unique(data$workclass)
unique(data$occupation)
unique(data$native.country)
unique(data$income)
colSums(data=="?")

data$workclass[data$workclass == "?"] = mode(data$workclass)
data$occupation[data$occupation == "?"] = mode(data$occupation)
data$native.country[data$native.country == "?"] = mode(data$native.country)
colSums(data=="?")


data$workclass <- as.factor(data$workclass)
data$education <- as.factor(data$education)
data$marital.status <- as.factor(data$marital.status)
data$occupation <- as.factor(data$occupation)
data$relationship <- as.factor(data$relationship)
data$race <- as.factor(data$race)
data$sex <- as.factor(data$sex)
data$native.country <- as.factor(data$native.country)
data$income <- as.factor(data$income)
str(data)

### EDA
library(ggplot2)
barplot(table(data$income),col='red')
barplot(table(data$workclass),
        col='blue')
barplot(table(data$education),
        col='blue')
barplot(table(data$marital.status),
        col='blue')
barplot(table(data$race),
        col='blue')
barplot(table(data$sex),
        col='blue')
barplot(table(data$occupation),
        col='blue')
barplot(table(data$relationship),
        col='blue')
data$education.num

ggplot(data, aes(age)) + geom_histogram(aes(fill = income), color = "black",
                                         binwidth = 1)

ggplot(data, aes(hours.per.week)) + geom_histogram()
library(data.table)

# Reorder factor levels by count
region.ordered <- reorder(data$native.country, data$native.country, length)
region.ordered <- factor(region.ordered, levels = rev(levels(region.ordered)))

ggplot(data, aes(region.ordered)) + geom_bar(aes(fill = income), color = "black")

intrain<-createDataPartition(y=data$income, p=0.7, list=FALSE) 
train<-data[intrain, ]
test<-data[-intrain, ]

### logistic
b =glm(income~. ,data = data, family=binomial())
summary(b)



library(randomForest)
forest_m = randomForest(income ~., data = train,ntree=100)
forest_m
forest_m$predicted
sort(forest_m$importance,decreasing = T)
forest_m$mtry
forest_m$ntree
plot(forest_m)

#### tree

library(e1071)
library(tree)
library(rpart)
train$income = as.factor(train$income)
rpartmod<-rpart(income~. , data=train, method="class")
plot(rpartmod)
text(rpartmod)

printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
rpartpred<-predict(ptree, test, type='class')
str(test$y)
test$income = as.factor(test$income)
dim(test)
confusionMatrix(rpartpred, test$income)


