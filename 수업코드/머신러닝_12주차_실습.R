#문제1
data <- read.csv("C:/Users/asdf0/OneDrive/바탕 화면/TalkFile_Life_case.csv")
data

data_subset <- data[, c("Person_A","Person_B")]
data_subset

result <- chisq.test(data_subset)
result

data$Difference <- abs(data$Person_A - data$Person_B)
sorted_data <- data[order(data$Difference), ]

top2 <- head(sorted_data[, c("Case","Person_A","Person_B")], 2)
top2

#문제2

activity_data <- data[, c("Activity","Person_A", "Person_B")]

# 카이제곱 검정
activity_matrix <- as.matrix(activity_data[, c("Person_A", "Person_B")])
chi_result <- chisq.test(activity_matrix)
chi_result

#차이 계산
data$Difference <- abs(data$Person_A - data$Person_B)

# 차이가 큰 순으로 정렬
sorted_data <- data[order(-data$Difference), ]
# Top 5 추출
top5 <- head(sorted_data[, c(
                             "Activity",
                             "Person_A",
                             "Person_B",
                             "Difference")], 5)
print(top5)