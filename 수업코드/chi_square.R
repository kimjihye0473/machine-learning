O <- c(45,47,59,49)
E <- c(50, 50, 50, 50)
chi <- sum((O - E)^2 / E)
chi

candy_data <- data.frame(
  Color = c("Red","Blue","Green"),
  Observed =c(30,50,20)
)

total_candies <- sum(candy_data$Observed)
expected <- rep(total_candies/3, 3)
test_result <- chisq.test(candy_data$Observed, p = expected/sum(expected))
print(test_result)

#X-squared(카이제곱통계량) = 14, df(자유도) = 2, p_value = 0.0009119
#0.0009119 -> 0.091 > 0.05 (유의하지 않음, 귀무가설 기각)
#귀무가설을 기각하므로 사탕 색 비율은 균등하지 않다

std_residuals <- (candy_data$Odserved - expected) / sqrt(expected)
chi_square_values <- std_residuals^2
p_value <- 1 - pchisq(chi_square_values, df = 2)

candy_data$Residuals <- std_residuals
candy_data$Chi_square <- chi_square_values
candy_data$p_value <- p_values

print(candy_data)


#적합도 검정
observed <- table(데이터$변수)
expected <- rep(sum(observed)/length(observed), length(observed))
test_result <- chisq.test(x = observed, p = expected/sum(expected))

print(test_result)


observed <- matrix(c(25,15,20,30,35,25,15,20,15), nrow = 3, byrow = TRUE)

rownames(observed) <- c("Red","Blue","Green")
colnames(observed) <- c("chocolate","vanilla","strabreey")
chi_squared <- chisq.test(observed)
print(chi_squared)
#X-squared(카이제곱통계량) = 4.0212, df(자유도) = 4, p_value = 0.4031


data <- data.frame(Gender = c("Male","Female","Male","Male","Female","Female","Male","Male","Female","Female"), 
                   Food = c("국밥","마라탕","국밥","피자","피자","피자","국밥","국밥","마라탕","피자"))

cross_tab <- table(data$Gender, data$Food)

chi_square_test_result <- chisq.test(cross_tab)
print(chi_square_test_result)
#X-squared(카이제곱통계량) = 7, df(자유도) = 2, p_value = 0.0302


install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
cross_tab <- table(data$Gender, data$Food)
cross_tab

results <- chisq.posthoc.test(cross_tab, method = "bonferroni")
results



#부부집안일예제
#카이제곱 분석
data2=read.csv("C:/Users/asdf0/OneDrive/바탕 화면/housetasks.csv", row.names = 1)
chi_square_test_result <- chisq.test(data2)
print(chi_square_test_result)

#사후검증
results <- chisq.posthoc.test(data2, method = "bonferroni")
results

#시각화
install.packages("corrplot")
library(corrplot)
corrplot(chi_square_test_result$residuals, is.cor = FALSE)















