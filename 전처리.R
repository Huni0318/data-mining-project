setwd('C:/Users/신상훈/Documents/GitHub/data-mining-project/데이터 및 조사설계서 (1)/데이터 및 조사설계서')
Sys.setlocale(category = "LC_CTYPE", locale = "C")
data = read.csv("데이터.csv",header = TRUE)
Sys.setlocale(category = 'LC_ALL',locale='korean')
library(dplyr)
library(caret)
library(RANN)
library(earth)
dim(data)
data = data[data$h10_g4 < 1990, ] # 90년대 이전 출생자 뽑기
dim(data)
data = data %>% filter(p1003_12 %in% c(1,2,4,5)) # p1003_12에서 1,2,4,5만 뽑기
dim(data)
data$p1003_12

data$y = data$p1003_12 # y로 변환
data = subset( data, select = -c(p1003_12))
dim(data)
data$y
# 전반적 만족도에 p1003_5~p1003_11 이 정보를 어느정도 포함하고 있으므로 삭제한다.

data = subset( data, select = -c(p1003_5,p1003_6,p1003_7,p1003_8,p1003_9,p1003_10,p1003_11))
dim(data)
data$y
data = as.data.frame(data)
dim(data)
str(data$y)
# 개수 확인
sum(na.omit(data$y == 1))
sum(na.omit(data$y == 2))
sum(na.omit(data$y == 4))
sum(na.omit(data$y == 5))


data$y[data$y == 2] = 1 # 변환
data$y[data$y == 5] = 4
data$y[data$y == 4] = 2
dim(data)

colSums(is.na(data)) # na가 극단적으로 많은 값과 적은 값들로 이루어져 있으므로 비율이 0.5 기준으로 이상인 값들의 열을 제거
colSums(is.na(data))/7923 < 0.5


x = data[colSums(is.na(data))/7923 < 0.5] # na가 0.5 이상인 열은 제거

dim(x)
colSums(is.na(x))
str(x)


colSums(is.na(x)) # 

x = subset( x, select = -c(X...h10_id,h10_ind,h10_sn,h10_merkey,h1013_4aq15)) # 가구 패널 ID와 관련된 정보는 특별한 의미가 없다고 생각되어 제거, h1013_4aq15 값이 없으므로 제거
dim(x)
str(x)
sort(colSums(is.na(x)),decreasing = T) # 결측치 값들을 크기 순으로 정렬
sort(colSums(is.na(x)),decreasing = T)[1:10]

x$y = as.factor(x$y)
imp.x = preProcess(x, method = c("bagImpute")) # 결측값을 bagImputer로 채움
dim(imp.x)
sum(is.na(x))

imp.2 <- predict(imp.x, x)
dim(imp.2)

sum(is.na(imp.2))

set.seed(1996) 

intrain<-createDataPartition(y=imp.2$y, p=0.7, list=FALSE) 
train<-imp.2[intrain, ]
test<-imp.2[-intrain, ]


#### random forest
library(randomForest)

forest_m = randomForest(y~., data = train)
forest_m
forest_m$predicted
forest_m$importance
forest_m$mtry
forest_m$ntree


sort(forest_m$importance)

p1002_8aq13 
p1002_8aq14
p1002_8aq11
p1002_8aq8 
p1002_8aq9
p10_cp 
h1010_aq17
h1008_6aq1
h1006_39
h10_flag

train = subset( train, select = -c(p1002_8aq13,p1002_8aq14,p1002_8aq11,p1002_8aq8,p1002_8aq9,p10_cp,h1010_aq17,h1008_6aq1,h1006_39,h10_flag))
test = subset( test, select = -c(p1002_8aq13,p1002_8aq14,p1002_8aq11,p1002_8aq8,p1002_8aq9,p10_cp,h1010_aq17,h1008_6aq1,h1006_39,h10_flag))
dim(train)

forest_m = randomForest(y~., data = train)
forest_m
forest_m$predicted
forest_m$importance
forest_m$mtry
forest_m$ntree

prd_v <- predict(forest_m, newdata = test, type = 'class')
sum(prd_v == test$y) / nrow(test) * 100


prd_v2 <- predict(forest_m, newdata = train, type = 'class')
sum(prd_v2 == train$y) / nrow(train) * 100

##### graph
layout(matrix(c(1,2),nrow=1),width=c(4,1)) 
par(mar=c(5,4,4,0)) # 오른쪽 마진 제거 
plot(forest_m)
par(mar=c(5,0,4,2)) # 왼쪽 마진 제거 
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(forest_m$err.rate),col=1:4,cex=0.8,fill=1:4)



### parameter tuning
ntree_test <- c()

for (i in 300:500) {
  
  m <- randomForest(y ~ . , data=train, ntree = i)
  
  val_var <- predict(m, newdata=test, type="class") 
  
  ntree_test <- c(ntree_test, sum(val_var == test$y)/nrow(test) * 100)
  
}

ntree_test



# train data에 대한 score 

ntree_train <- c()

for (i in 300:500) {
  
  m <- randomForest(y ~ . , data=train, ntree = i)
  
  val_var <- predict(m, newdata=train, type="class") 
  
  ntree_train <- c(ntree_train, sum(val_var == train$y)/nrow(train) * 100)
  
}

ntree_train




# 2) mtry 변화

# test data에 대한 score

mtry_test <- c()

for (i in 1:(length(train)-1)) {
  
  m <- randomForest(y ~ . , data=train, mtry = i)
  
  val_var <- predict(m, newdata=test, type="class") 
  
  mtry_test <- c(mtry_test, sum(val_var == test$y)/nrow(test) * 100)
  
}

mtry_test

forest_m1 = randomForest(y~., data = train, mtry=20, ntree = 300)
prd_v <- predict(forest_m1, newdata = test, type = 'class')



# train data에 대한 score 

mtry_train <- c()

for (i in 1:(length(train)-1)) {
  
  m <- randomForest(y ~ . , data=train, mtry = i)
  
  val_var <- predict(m, newdata=train, type="class") 
  
  mtry_train <- c(mtry_train, sum(val_var == train$y)/nrow(train) * 100)
  
}

mtry_train


plot(1:length(ntree_test), ntree_test, type = 'l', xlab = 'ntree(size of tree)', col='red', ylim = c(95,100))

lines(1:length(ntree_train), ntree_train, type = 'l')

plot(1:length(mtry_test)-1, mtry_test, type = 'b', xlab = 'mtry', col='red', ylim = c(95,100))

lines(1:length(mtry_train)-1, mtry_train, type = 'b')


forest_m1 = randomForest(y~., data = train, mtry=20, ntree = 300)
prd_v <- predict(forest_m1, newdata = test, type = 'class')


library(prediction)

### decision tree를 이용한 분석
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






TN = 311
FN = 34
FP = 270
TP = 4932
Specificity= TP / (FP+TP)
Specificity

Sensitivity = FP/(FN+TP)
Sensitivity

ACCURACY = (TP + TN)/(TN + FN + TP + FP)

ACCURACY
