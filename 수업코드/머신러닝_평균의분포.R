#예제1
m <- 160
sigma <- 7
n <- 10 
se <- sigma/sqrt(n)
pnorm(157, mean = m, sd = se )
#예제2
m<- 15
var<- 100
n<- 20
se <- sqrt(var/n)

pnorm(20, mean = m, sd = se)
#예제3
m<- 25
sigma<- 12
n<- 35 
se<- sigma/sqrt(n)
pnorm(15, mean = m, sd = se)
#4
m<- 18
sigma<- 3
n<- 36
se <- sigma/sqrt(n)

1 - pnorm(17, mean = n, sd = se)
#5
m<-85
sigma<- 10
n<-30
se <- sigma/sqrt(n)
z <- (90 - m)/se

1 - pnorm(z)
#6 
m<- 85
sigma<- 10
n<- 30
se<- sigma/sqrt(n)
z1<- (90 - m)/se
z2<- (93 - m)/se
pnorm(z2)-pnorm(z1)
#7
m<- 175
sigma<- 15
n<- 49
se<- sigma/sqrt(n)
z1<-(173-m)/se #정규분포는 좌우대칭
z2<- (178-m)/se
pnorm(z2)-pnorm(z1)

#모집단이 두 개일 때 
m1 <- 4
var1 <- 16
n1 <- 64

m2<-12
var2<-48
n2<-75

se <- sqrt((var1/n1)+(var2/n2))
z <- (-6-(m1-m2))/se
z
pnorm(z)

#2
m1 <- 10
var1 <- 25
n1 <- 50
m2 <- 20
var2 <- 45
n2 <- 60

se <- sqrt((var1/n1)+(var2/n2))
z <- (m1-m2)/se
z 
pnorm(z)

#사과의 무게
Apple_weights <- c(150,152,147,160,155,153,158,162,149,154,159,157)
#샘플사이즈와 시행 횟수
sample_size <- 5
num_samples <-1000

get_sample_mean <- function() {
  sample <- sample(apple_weights,sample_size, replace = TRUE)
  return(mean(smaple))
}

sample_means <- replicate(num_samples, get_sample_mean())
   
  
  
#Z-test
x <- 50 #표본평균
mu <- 45 #모집단 평균
sigma <- 10 #표준편차
n <- 100

z<- (x - mu)/(sigma/sqrt(n))

#p_value 양측 검정
p_value <- 2*(1 - pnorm(abs(z)))

z
p_value

#t-test / t=0, p_value =1 맞다라는 의미
x <-c(48,52,50,47,49,51,53)

t.test(x, mu = 50) #mu = 기준평균(검정하고 싶은 값)

  
#문제1
m<- 60
sigma <- 10
n <- 40
se <- sigma/sqrt(n)
z <-(55 - m)/se
pnorm(z)

#문제2
m <- 150
sigma <- 20
n <- 36
se <- sigma/sqrt(n)
z <- (143-m)/se
pnorm(z)














