
data <- c(10,25,30,35,30,15,20,25,25,10)
mean_value <-mean(data) 
median_value <-median(data) 
max_value <-max(data) 
min_value <-min(data) 
mode_value <-find_mode(data)
mean(x)
mean(x, trim=0.25)
weightedmean(x,w)
exp(mean(log(x)))

a <-c((50*70)+(50*50)+(40*60)+(60*80)/50+50+40+60)

x <- c(542, 674, 841, 966, 1026)

cagr <- (x[length(x)] / x[1])^(1/(length(x)-1)) - 1
cagr


x <-c(100, 50)
HM <- length(x)/sum(1/x)


price <- c(10,20,40,30,50)
weight  <-c(100,50,50,40,70)
hm <- sum(weight) / sum(weight/price)


p <- c(0.1, 0.4, 0.8, 0.75, 0.71)
w <- c(1,1,1,1,1)
H_M <- sum(w)/sum(w/p)


a_m <- c(75,88,91,68,82)
a_f <- c(60,87,55,47,92)
avg_a_m <- sum(a_m)/5
avg_a_f <- sum(a_f)/5

am <- c(75,88,82)
af <- c(60,87,55)
avg_am <- sum(am)/3
avg_af <- sum(af)/3

a_m <- c(75,88,91,68,82)
a_f <- c(60,87,55,47,92)
score <- a_m*0.5 + a_f*0.3 
rank <- rank(-score)
result <- data.frame(a_m,a_f,score,rank)
result[order(result$rank)]


vi <- 542
vf <- 1026
n <- 4
carg <- (vf/vi)^(1/n)-1
carg



p <- c(15,25,35,45,55)
w <- c(80, 100, 60, 40, 20)

h_m <- sum(w)/sum(w/p)



x <- c(100,200,300,400,200,200,100,400,250,230,70,80,90)
quantile(x)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
iqr_value <- Q3- Q1

city <- c("seoul","Busan","Deagu","Ulsan")
pm25 <-c(18,21,21,17,8,11,25)

df <- data.frame(city = city, pm25 = pm25)

id<-c(1:5)
gender <-c("M","F","F","M","M")
major<-c("eng","math","com","eng","busi")
salary<-c(2500,2800,2500,3000,2600)
survey<-data.frame(ID=id, Gender=gender, Major=major, Salary=salary, srtingsAsFactors = FALSE)
survey

head(survey, n=3)
tail(survey, n=3)
survey$Salary
survey[["Salary"]]
survey["Major"]
survey[2]
survey[[2]]
survey$score=c(200,300,400,500,600)
survey
survey$ss=survey$Salary+survey$score
survey
survey[c(1,2)]
survey[c(-1,-2)]
survey[survey$Gender=="F",]
survey[survey$Major=="eng"&survey$Salary>2600,]
nrow(survey)
ncol(survey)
dim(survey)
names(survey)
rownames(survey)
colnames(survey)
survey[c(order(survey$Salary)),]
survey1 <- data.frame(ID=id[1:3],Gender= gender[1:3],Major=major[1:3],Salary=salary[1:3],stringsAsFactors =  FALSE)
survey1
survey2 <- data.frame(ID=id[4:5],Gender= gender[4:5],Major=major[4:5],Salary=salary[4:5],stringsAsFactors =  FALSE)
survey2
survey3 <-rbind(survey1, survey2)
survey3
job <- c("office","profession","technician")
survey4 <-cbind(survey1, job)
survey4
subset(survey, Major == "com", c(Gender, Salary))
survey$Gender <- NULL
survey

name_age_df <- data.frame(
  Name = c("kim Cheol-soo", "Lee Cheol-soo","Kim young-hee","Lee young-hee","kim min-jun","Pack min-jun","kim ji-young","Pack ji-young"),
  Age = c(20,24,21,23,35,40,34,35),
  stringsAsFactors = FALSE
)
name_age_df <- separate(name_age_df, col = "Name", into = c("LastName","FirstName"), sep = "-")
print(name_age_df)

df <- fread("경로")
df_clean <- na,omit(df)
imputed_data <- mine(sub_df_numeric, m=5, maxit=10, method="pmm", seed = 1235)

time <- seq(1, 24) 
temperature <- c(22,21,20,19,18,19,20,22,24,26,28,29,28,27,25,24,24,22,22,21,21,20,19)

d_f <- data.frame(time=time, temp=temperature)

ggplot(data, aes(x=time, y=temp))+
  geom_line() + 
  labs(title = "시간에 따른 온도 변화")+
  xlab("시간 ") + 
  ylab("온도 ")


df <- data.frame(x =c(1,2,3,4,5), y =c(6,8,5,9,7))

ggplot(df, aes(x=x, y=y))+
  geom_point(color = "blue", size = 3)+
  labs(title = "Scatter Plot")+
  xlab("X")+
  ylab("Y")

df <- data.frame(x =c(1,2,3,4,5), y =c(6,8,5,9,7))
df2 <- data.frame(x=c(5,6,7,8,9), y=c(18,12,16,77,63))

ggplot() +
  geom_point(data = df, aes(x=x, y=y), color = "blue", size = 3)+
  geom_line(data = df, aes(x=x, y=y, color = "Connected Points"), size = 0.5)+
  geom_point(data = df2, aes(x=x, y=y), color = "blue", size = 3)+
  geom_line(data = df2, aes(x=x, y=y, color = "Connected Points2"), size = 0.5)+
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y")



df <- data.frame(x = c(1,2,3,4,5), y = c(6,8,5,9,7))

ggplot(df, aes(x=x, y=y)) +
  geom_point(color = "blue",size = 3)+
  geom_smooth(method = "lm", se = FALSE, aes(color= "Trendline"))+
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y")



df <- data.frame(
  group = c(rep("Group 1", 60), rep("Group 2", 60)),
  values = c(rnorm(60, mean = 0, sd = 1), rnorm(60, mean = 2, sd =1)))

ggpolt(df, aes(x = group, y = values)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red") +
  labs(title = "Boxplot Example") +
  xlab("Group") +
  ylab("Values")


city <-c("seoul","Busan","Deagu","seoul","Busan","Deagu","Ulsan")
pm25 <-c(18,21,21,17,8,11,25)

df<-data.frame(city=city, pm25=pm25)

ggplot(df, aes(x = city, y= pm25, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "지역별 초미세먼지 농도")+
  xlab("city")+
  ylab("농도")


city <- c("Seoul","Busan","Deagu","seoul","Busan","Deagu","Ulsan")
vari <- c("오전","오후","오전","오후","오전","오후","오후")
pm25 <-c(18,21,21,17,8,11,25)

df <- data.frame(city=city,pm25=pm25, vari=vari)

ggplot(df, aes(x= city,y= pm25, fill = vari)) +
  geom_bar(stat = "identity")+
  labs(title = "지역별 초미세먼지 농도") +
  xlab("City")+
  ylab("농도")



df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30))

ggplot(df, aes(x = values))+
  geom_histogram(aes(y = ..density..),binwidth = 5, fill = "steelblue", color = "white")+
  xlab("Values")+
  ylab("Density")



df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30))

ggplot(df, aes(x = values))+
  geom_histogram(aes(y = ..density..),binwidth = 5, fill = "steelblue", color = "white")+
  geom_density(alpha = 0.3, fill = "red")+
  labs(title = "Histogram with Density Plot")
xlab("Values")+
  ylab("Density")



city <- c("seoul","Busan","Deagu","Incheon","Gwangju","Daejeon","Ulsan")
pm25 <- c(18,21,21,17,8,11,25)
colours()
colors <- c("red","orange","yellow","green","lightblue","blue","violet")

df <- data.frame(city=city, pm25 = pm25, colors = colors)

ggplot(df,aes(x = "",y = pm25, fill = city))+
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  labs(title = "Concentration of Ultrafine Dust by Region") +
  xlab("")+
  ylab("")



lidrary(reshape2)


students <- paste("Student", 1:10)#10명의 학생
subjects <- c("Math", "Science","Engilsh")
grades <- maxtrix(sample(50:100, 30, replace = TRUE), nrow = 10, ncol =3, dimnames = list(students, subjects))


grades_melted <- melt(grades, id.vars = rownames(grades))
grades_melted

colnames(grades_melted) <-c("Student","Subject","Grade")


ggplot(grades_melted, aes(x = subject, y = student, fill = Grade))+
  geom_tile(color = "white")+
  scale_fill_gradient(low = "green", high = "red")+
  labs(title = "학생별 과목 성적 Heatmap")+
  xlab("과목")+
  ylab("학생")


m <- 160
sigma <- 7
n <- 10 
se <- sigma/sqrt(n)
z <-(157-m)/se
pnorm(z)

m<- 15
var<- 100
n<- 20
se <- sqrt(var/n)
pnorm(20, mean = m, sd = se)

m<- 25
sigma<- 12
n<- 35 
se<- sigma/sqrt(n)
pnorm(15, mean = m, sd = se)

m<- 18
sigma<- 3
n<- 36
se <- sigma/sqrt(n)
1 - pnorm(17, mean = n, sd = se)

m<-85
sigma<- 10
n<-30
se <- sigma/sqrt(n)
z <- (90 - m)/se
1 - pnorm(z)

m<- 85
sigma<- 10
n<- 30
se<- sigma/sqrt(n)
z1<- (90 - m)/se
z2<- (93 - m)/se
pnorm(z2)-pnorm(z1)

m<- 175
sigma<- 15
n<- 49
se<- sigma/sqrt(n)
z1<-(173-m)/se 
z2<- (178-m)/se
pnorm(z2)-pnorm(z1)


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


Apple_weights <- c(150,152,147,160,155,153,158,162,149,154,159,157)
sample_size <- 5
num_samples <-1000
get_sample_mean <- function() {
  sample <- sample(apple_weights,sample_size, replace = TRUE)
  return(mean(smaple))
}

sample_means <- replicate(num_samples, get_sample_mean())



x <- 50 
mu <- 45 
sigma <- 10
n <- 100
z<- (x - mu)/(sigma/sqrt(n))
p_value <- 2*(1 - pnorm(abs(z)))

z
p_value


x <-c(48,52,50,47,49,51,53)
t.test(x, mu = 50)


m<- 60
sigma <- 10
n <- 40
se <- sigma/sqrt(n)
z <-(55 - m)/se
pnorm(z)


m <- 150
sigma <- 20
n <- 36
se <- sigma/sqrt(n)
z <- (143-m)/se
pnorm(z)


data <-read.csv("C:/Users/asdf0/OneDrive/바탕 화면/t_test.csv")

t.test(x1, x2)
t.test(x1,x2,alternative = "two.sided")
t.test(x2,x1,alternative = "greater")


data <-read.csv("C:/Users/asdf0/OneDrive/바탕 화면/z_test.csv")
x1 <- data$groupA
x2 <- data$groupB

mean1 <- mean(x1)
mean2 <- mean(x2)

n1 <- length(x1)
n2 <- length(x2)

sigma1 <- sd(x1)
sigma2 <- sd(x2)

z <-(mean1-mean2)/sqrt((sigma1^2/n1)+(sigma2^2/n2))

z

#P_value
p_value <- 2*(1-pnorm(abs(z)))
p_value



















