time <- seq(1, 24)#24시간 
temperature <- c(22,21,20,19,18,19,20,22,24,26,28,29,28,27,25,24,24,22,22,21,21,20,19)

d_f <- data.frame(time=time, temp=temperature)

ggplot(data, aes(x=time, y=temp))+
  geom_line() + 
  labs(title = "시간에 따른 온도 변화")+
  xlab("시간 ") + 
  ylab("온도 ")

#산점도 
df <- data.frame(x =c(1,2,3,4,5), y =c(6,8,5,9,7))

ggplot(df, aes(x=x, y=y))+
  geom_point(color = "blue", size = 3)+
  labs(title = "Scatter Plot")+
  xlab("X")+
  ylab("Y")
#두 변수간의 관계
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
  
#직교좌표계 통해 두 변수간 관계

df <- data.frame(x = c(1,2,3,4,5), y = c(6,8,5,9,7))

ggplot(df, aes(x=x, y=y)) +
  geom_point(color = "blue",size = 3)+
  geom_smooth(method = "lm", se = FALSE, aes(color= "Trendline"))+
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y")
  
#상자수염그림Boxplot

df <- data.frame(
  group = c(rep("Group 1", 60), rep("Group 2", 60)),
  values = c(rnorm(60, mean = 0, sd = 1), rnorm(60, mean = 2, sd =1)))

ggpolt(df, aes(x = group, y = values)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red") +
  labs(title = "Boxplot Example") +
  xlab("Group") +
  ylab("Values")

#막대그래프bar_chart
city <-c("seoul","Busan","Deagu","seoul","Busan","Deagu","Ulsan")
pm25 <-c(18,21,21,17,8,11,25)

df<-data.frame(city=city, pm25=pm25)

ggplot(df, aes(x = city, y= pm25, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "지역별 초미세먼지 농도")+
  xlab("city")+
  ylab("농도")
  
#막대그래프_표현 값 비례 
city <- c("Seoul","Busan","Deagu","seoul","Busan","Deagu","Ulsan")
vari <- c("오전","오후","오전","오후","오전","오후","오후")
pm25 <-c(18,21,21,17,8,11,25)

df <- data.frame(city=city,pm25=pm25, vari=vari)

ggplot(df, aes(x= city,y= pm25, fill = vari)) +
  geom_bar(stat = "identity")+
  labs(title = "지역별 초미세먼지 농도") +
  xlab("City")+
  ylab("농도")

#히스토그램 

df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30))

ggplot(df, aes(x = values))+
  geom_histogram(aes(y = ..density..),binwidth = 5, fill = "steelblue", color = "white")+
  xlab("Values")+
  ylab("Density")

#히스토그램+주요정보

df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30))

ggplot(df, aes(x = values))+
  geom_histogram(aes(y = ..density..),binwidth = 5, fill = "steelblue", color = "white")+
  geom_density(alpha = 0.3, fill = "red")+
  labs(title = "Histogram with Density Plot")
  xlab("Values")+
  ylab("Density")

#파이차트pie_chart

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

#히트맵

lidrary(reshape2)

#가상의 성적 데이터 생성
students <- paste("Student", 1:10)#10명의 학생
subjects <- c("Math", "Science","Engilsh")
grades <- maxtrix(sample(50:100, 30, replace = TRUE), nrow = 10, ncol =3, dimnames = list(students, subjects))

#데이터 프레임으로 변환 
grades_melted <- melt(grades, id.vars = rownames(grades))
grades_melted

colnames(grades_melted) <-c("Student","Subject","Grade")

#HEATMAP
ggplot(grades_melted, aes(x = subject, y = student, fill = Grade))+
  geom_tile(color = "white")+
  scale_fill_gradient(low = "green", high = "red")+
  labs(title = "학생별 과목 성적 Heatmap")+
  xlab("과목")+
  ylab("학생")







  







