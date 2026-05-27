set_a <-c(10.1,10.2,10.3,10.0, 10.1, 10.2,10.3, 10.0, 10.1, 10.2)
set_b <-c(9.8,10.5, 10.2, 9.7, 10.4, 10.3, 9.6, 10.6, 9.9, 10.7)

#분산 동질성 검정(양측)
result <- var.test(set_a, set_b)
result

var.test(set_a, set_b, alternative = "less") #a집단은 b집단보다 분산이 작다

var.test(set_a, set_b, alternative = "greater") #a집단은 b집단보다 분산이 크다

#set_a/set_b = 0.07494647 값이 1보다 작으므로 set_a가 set_b보다 작다
#P-value 값이 

df= read.csv("C:/Users/asdf0/OneDrive/바탕 화면/Machine_set.csv")

s_a <- subset(df, machine == "A")$value
s_b <- subset(df, machine == "B")$value

result <- var.test(s_a, s_b)
result

var.test(s_a, s_b, alternative = "less")
var.test(s_a, s_b, alternative = "greater")

#s_a/s_b =7.187954이므로 s_a값이 더 큼

#일원분산분석 
score <- c(90, 85, 77,
           75, 80, 71,
           82, 76, 88)

worker <- c("A", "A", "A",
            "B", "B", "B",
            "C", "C", "C")

data <- data.frame(score, worker)

print(data)

aggregate(score ~ worker, data=data, mean)

result <- aov(score ~ worker, data=data)
summary(result)

#F값이 1.866으로 1보다 크지만 엄청 크진 않아서 작업자 간 차이 크다고 보기 어렵다
#p_value 0.234이므로 0.05보다 큼 귀무가설을 기각하지 않음 
#작업자에 따른 생산량 차이가 있다고 보기 어렵다 

#이원분산분석 
grow <-read.csv("C:/Users/asdf0/OneDrive/바탕 화면/growth.csv")

#독립변수들간 상호작용X
anova_result <- aov(growth ~fertilizer + water, data = grow)
summary(anova_result)
#독립변수들간 상호작용
anova_resultv<- aov(growth ~fertilizer * water, data = grow)
summary(anova_result)

#등분산검정
install.packages("car")
library(car)

growth <-c(30, 35, 37, 28, 29, 32, 33, 36, 35, 37)
fertilizer <-c("A","A","A","A","B","B","B","C","C","C")
water <-c("Low","High","Low","High","Low","High","Low","High","Low","High")

data <- data.frame(growth, fertilizer, water)
print(data)

leveneTest(growth ~ fertilizer, data = data)
leveneTest(growth ~ interaction(fertilizer,water), data = data)
#F = 3.7181e+31, P-value =  2.2e-16
#귀무가설 기각, 그룹 간 분산이 서로 같다고 보기 어렵, 분산 동질성 만족되지 않음


#사후검증
install.packages("rstatix")
install.packages("multcomp")
library(rstatix)
library(multcomp)

grow <-read.csv("C:/Users/asdf0/OneDrive/바탕 화면/growth.csv")
grow$fertilizer <- as.factor(grow$fertilizer)
grow$water <- as.factor(grow$water)

print(grow)

#이원분산분석
anova_result <- aov(growth ~ fertilizer + water, data = grow)
summary(anova_result)

# 사후검정 fertilitzer
tukey_fertilizer <- glht(anova_result, linfct = mcp(fertilizer = "Tukey"))
summary(tukey_fertilizer)
games_howell_test(grow, growth ~ fertilizer)

#사후검정 water
tukey_water <- glht(anova_result,linfct = mcp(water = "Tukey"))
summary(tukey_water)
games_howell_test(grow,growth ~ water)


#cafe
cafe =read.csv("C:/Users/asdf0/OneDrive/바탕 화면/cafe.csv")

#범주형변수로 변경
data$CoffeeType <- as.factor(data$CoffeeType)
data$CafeMood <- as.factor(data$CafeMood)
data$SeatLocation <- as.factor(data$SeatLocation)

# CoffeeType에 대한 등분산 검정
leveneTest(Satisfaction ~ CoffeeType, data = data)
# CafeMood에 대한 등분산 검정
leveneTest(Satisfaction ~ CafeMood, data = data)
# SeatLocation에 대한 등분산 검정
leveneTest(Satisfaction ~ SeatLocation, data = data)

# CoffeeType에 따른 만족도 차이
anova_coffee <- aov(Satisfaction ~ CoffeeType, data = data)
summary(anova_coffee)
# CafeMood에 따른 만족도 차이
anova_mood <- aov(Satisfaction ~ CafeMood, data = data)
summary(anova_mood)
# SeatLocation에 따른 만족도 차이
anova_seat <- aov(Satisfaction ~ SeatLocation, data = data)
summary(anova_seat)

# CoffeeType 사후검정

tukey_coffee <- glht(anova_result,linfct = mcp(CoffeeType = "Tukey"))
summary(tukey_coffee)
games_howell_test(data,Satisfaction ~ CoffeeType)

# CafeMood 사후검정
tukey_mood <- glht(anova_result,linfct = mcp(CafeMood = "Tukey"))
summary(tukey_mood)
games_howell_test(data,Satisfaction ~ CafeMood)

# SeatLocation 사후검정
tukey_seat <- glht(anova_result,linfct = mcp(SeatLocation = "Tukey"))
summary(tukey_seat)
games_howell_test(data,Satisfaction ~ SeatLocation)

