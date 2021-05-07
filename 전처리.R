setwd('C:/Users/신상훈/Documents/GitHub/data-mining-project/데이터 및 조사설계서 (1)/데이터 및 조사설계서')
Sys.setlocale(category = "LC_CTYPE", locale = "C")
data = read.csv("데이터.csv",header = TRUE)
Sys.setlocale(category = 'LC_ALL',locale='korean')
library(dplyr)


data = data[data$h10_g4 < 1990, ] # 90년대 이전 출생자 뽑기
dim(data)
data = data %>% filter(p1003_12 %in% c(1,2,4,5)) # p1003_12에서 1,2,4,5만 뽑기
dim(data)
data$p1003_12

data$y = data$p1003_12 # y로 변환
data = subset( data, select = -c(p1003_12))
dim(data)

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

x = subset( x, select = -c(X...h10_id,h10_ind,h10_sn,h10_merkey)) # 가구 패널 ID와 관련된 정보는 특별한 의미가 없다고 생각되어 제거
dim(x)
str(x)
sort(colSums(is.na(x))) # 결측치 값들을 크기 순으로 정렬
sort(colSums(is.na(x)),decreasing = T)[1:10]












