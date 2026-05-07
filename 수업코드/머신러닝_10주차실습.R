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
  Variable = c(rep("var1", length(group_a)), rep("var2", length(group_b))),
  Value = c(group_a, group_b)
)
ggplot(df, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red") +
  labs(title = "Boxplot of Two Variables") +
  xlab("Variable") +
  ylab("Value")



data2 = read.csv()




