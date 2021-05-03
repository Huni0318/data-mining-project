setwd('C:/Users/신상훈/Documents/GitHub/data-mining-project/데이터 및 조사설계서 (1)/데이터 및 조사설계서')
Sys.setlocale(category = "LC_CTYPE", locale = "C")
data = read.csv("데이터.csv",header = TRUE)
Sys.setlocale(category = 'LC_ALL',locale='korean')
library(dplyr)

sum(na.omit(data$y == 4))

data = data[data$h10_g4 < 1990, ]
dim(data)
data = data %>% filter(p1003_12 %in% c(1,2,4,5))
dim(data)
data$p1003_12




data$y = data$p1003_12
data = subset( data, select = -c(p1003_12))
dim(data)

# 전반적 만족도에 p1003_5~p1003_11 이 정보를 어느정도 포함하고 있으므로 삭제한다.

data = subset( data, select = -c(p1003_5,p1003_6,p1003_7,p1003_8,p1003_9,p1003_10,p1003_11))
dim(data)
data$y
data = as.data.frame(data)
dim(data)
str(data$y)
data$y[data$y == 2] = 1
data$y[data$y == 5] = 4
data$y[data$y == 4] = 2
dim(data)

colSums(is.na(data))/7923 < 0.5

data
x = data[colSums(is.na(data))/7923 < 0.5] # na가 0.5 이상인 열은 제거

dim(x)

str(x)
colSums(is.na(x)) 
