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


csv <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/predictive_maintenance.csv")
data <- data.frame(Type = z_csv$Type,
                     Torque = z_csv$Torque..Nm.)

group_under <- data[z_data$Torque <= 40, ]$Type
group_over <- data[z_data$Torque > 40, ]$Type


group_under_num <- ifelse(group_under == "L", 25, 
                          ifelse(group_under == "M", 65, 90))
group_over_num <- ifelse(group_over == "L", 25, 
                         ifelse(group_over == "M", 65, 90))

df2 <- data.frame(
  variable = c(rep("group_a", length(group_under_num)), #하루 4시간 이하
               rep("group_b", length(group_over_num))), #하루 4시간 초과
  value = c(group_under_num, group_over_num)
)

ggplot(df2, aes(x = value, y = variable)) + 
  geom_boxplot(fill = c("lightblue", "lightgreen"),
               outlier.color = "red") + 
  coord_flip()+
  labs(title = "Boxplot of Two Variable") +
  xlab("variable") +
  ylab("value")

sd_a = sd(group_under_num)
sd_b = sd(group_over_num)
result <- z.test(x = group_under_num, y = group_over_num, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")
result  







