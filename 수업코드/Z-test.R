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
