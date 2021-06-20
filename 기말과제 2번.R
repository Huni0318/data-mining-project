setwd('C:/Users/신상훈/Desktop/데이터 (1)/데이터')
data = read.csv("Teaching Assistant Evaluation.txt",header = F)
str(data)
dim(data)

names(data) <- c('y','instructor','course','semester','class.size','attribute')

head(data)
colSums(is.na(data))
summary(data)
str(data)

data$y <- as.factor(data$y)
data$instructor <- as.factor(data$instructor)
data$course <- as.factor(data$course)
data$semester <- as.factor(data$semester)
data$attribute <- as.factor(data$attribute)
#####EDA
barplot(table(data$y),,col='red')
barplot(table(data$instructor),,
        col='blue',)
barplot(table(data$course),,
        col='blue',)
barplot(table(data$semester),,
        col='blue')
barplot(table(data$attribute),,
        col='blue')


ggplot(data, aes(class.size)) + geom_histogram(aes(fill = y), color = "black",
                                        binwidth = 1)

ggplot(data, aes(class.size)) + geom_histogram()

library(data.table)

# Reorder factor levels by count

ggplot(data, aes(instructor)) + geom_bar(aes(fill = y), color = "black")
ggplot(data, aes(course)) + geom_bar(aes(fill = y), color = "black")
ggplot(data, aes(semester)) + geom_bar(aes(fill = y), color = "black")
ggplot(data, aes(attribute)) + geom_bar(aes(fill = y), color = "black")



####
intrain<-createDataPartition(y=data$y, p=0.7, list=FALSE) 
train<-data[intrain, ]
test<-data[-intrain, ]

### logistic

b =glm(y~. ,data = train, family=binomial())
summary(b)



###rf
library(randomForest)
forest_m = randomForest(y ~., data = train)
forest_m
forest_m$predicted
forest_m$importance
forest_m$mtry
forest_m$ntree
plot(forest_m)
#### tree

library(e1071)
library(tree)
library(rpart)
train$y = as.factor(train$y)
rpartmod<-rpart(y~. , data=train, method="class")
plot(rpartmod)
text(rpartmod)

printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
rpartpred<-predict(ptree, test, type='class')
str(test$y)
test$y = as.factor(test$y)
dim(test)
confusionMatrix(rpartpred, test$y)


































