observed <- c(45, 47, 59, 49)

# 기대비율
# 4종류가 동일한 비율이라고 가정
expected_prob <- c(0.25, 0.25, 0.25, 0.25)

# 카이제곱 적합도 검정
result <- chisq.test(observed, p = expected_prob)

# 결과 출력
result

# 관찰값
O <- c(45, 47, 59, 49)

# 기대값
E <- c(50, 50, 50, 50)

# 카이제곱 값 계산
chi_square <- sum((O - E)^2 / E)

# 출력
chi_square
