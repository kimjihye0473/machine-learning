drink_data <- data.frame(
  Color = c("coke","sprite","orangejuice"),
  Observed =c(40,35,25)
)

total_drinks <- sum(drink_data$Observed)
expected <- rep(total_drinks/3, 3)
test_result <- chisq.test(drink_data$Observed, p = expected/sum(expected))
print(test_result)

observed =c(5,8,12,15,10,10)
chisq.test(observed)

#기대비율이 균등하지 않을경우
observed <- c(20,30,50)

chisq.test(observed, p = c(0.1,0.5,0.4)) #기대비율 합은 1

#독립성 검정
data <- matrix(c(30,10,
                 15,25),
               nrow = 2,
               byrow = TRUE)
rownames(data) <- c("남학생","여학생")
colnames(data) <- c("수학","영어")
data

result <- chisq.test(data)


data2 <- matrix(c(20,30,
                   40,10),
                 nrow = 2,
                 byrow = TRUE)
rownames(data2) <- c("Male","Female")
colnames(data2) <- c("coffee","Tea")
data2

result <- chisq.test(data2)
result

















