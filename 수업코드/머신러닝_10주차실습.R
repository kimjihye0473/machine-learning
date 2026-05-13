data = read.csv("C:/Users/User/Desktop/Gaming_Academic.csv")

data <- data.frame(gaming_hours = data$gaming_hours, 
                   grades = data$grades)

group_a <- data[data$gaming_hours <= 4, ]$grades
group_b <- data[data$gaming_hours > 4, ]$grades


t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
t_test_result
t_test_result <- t.test(group_a, group_b, alternative = "less")
t_test_result
t_test_result <- t.test(group_a, group_b, alternative = "greater")
t_test_result



df <- data.frame(
  Variable = c(rep("gaming_hours", length(group_a)), rep("grades", length(group_b))),
  Value = c(group_a, group_b)
)
ggplot(df, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red") +
  labs(title = "gaming_hours_grades") +
  xlab("Variable") +
  ylab("Value")


#z-test
data2 = read.csv("C:/Users/User/Desktop/predictive_maintenance.csv")

# 필요한 변수만 추출
df <- data.frame(
  Type = data$Type,
  Torque = data$Torque..Nm.
)

# Torque 기준으로 그룹 분리
group_under <- df[df$Torque <= 40, ]$Type
group_over  <- df[df$Torque > 40, ]$Type

# 문자 데이터를 숫자로 변환
convert_type <- function(x) {
  ifelse(x == "L", 25,
         ifelse(x == "M", 65, 90))
}

group_under_num <- convert_type(group_under)
group_over_num  <- convert_type(group_over)

# 표준편차 계산
sd_under <- sd(group_under_num)
sd_over  <- sd(group_over_num)

# Z-test 수행
library(BSDA)

z_result <- z.test(
  x = group_under_num,
  y = group_over_num,
  sigma.x = sd_under,
  sigma.y = sd_over,
  alternative = "two.sided"
)

# 결과 출력
z_result








