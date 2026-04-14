#중심 경향 측정
#산술평균:점수, 키, 데이터값 등등
#가중평균:수량, 개수, 비중, 학점 평균
#기하평균:성장률, 증가.감소율, 수익률,비율 변화, CAGR 
#조화평균:속도, 시간, 거리 +)같은 거리에 다른 속도 이동

data <- c(10,25,30,35,30,15,20,25,25,10)
mean_value <-mean(data) #평균
median_value <-median(data) #중앙값
max_value <-max(data) #최대값
min_value <-min(data) #최소값
mode_value <-find_mode(data) #최빈값을 찾아서 mode_value에 넣어라


#1 산술평균
a <-c((50*70)+(50*50)+(40*60)+(60*80)/50+50+40+60)

#2  기하평균-성장률 구할 때
x <- c(542, 674, 841, 966, 1026)

cagr <- (x[length(x)] / x[1])^(1/(length(x)-1)) - 1
cagr

#3 조화평균, HM = 1/역수들의 평균
x <-c(100, 50)

HM <- length(x)/sum(1/x)

#가중 조화평균
price <- c(10,20,40,30,50)
weight  <-c(100,50,50,40,70)

hm <- sum(weight) / sum(weight/price)

p <- c(0.1, 0.4, 0.8, 0.75, 0.71)
w <- c(1,1,1,1,1)

H_M <- sum(w)/sum(w/p)

#예제1 
a_m <- c(75,88,91,68,82)
a_f <- c(60,87,55,47,92)
avg_a_m <- sum(a_m)/5
avg_a_f <- sum(a_f)/5
#예제2 
am <- c(75,88,82)
af <- c(60,87,55)
avg_am <- sum(am)/3
avg_af <- sum(af)/3
#예제3
a_m <- c(75,88,91,68,82)
a_f <- c(60,87,55,47,92)
score <- a_m*0.5 + a_f*0.3 
rank <- rank(-score)
result <- data.frame(a_m,a_f,score,rank)
result[order(result$rank)]

#예제4(기하평균)

carg <- (10/1)^(1/4)-1


#예제5 (조화평균)
p <- c(15,25,35,45,55)
w <- c(80, 100, 60, 40, 20)

h_m <- sum(w)/sum(w/p)

#사분위수
x <- c(100,200,300,400,200,200,100,400,250,230,70,80,90)
quantile(x)

Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
#사분위수 Q3에서 Q1 사이 범위
iqr_value <- Q3- Q1














