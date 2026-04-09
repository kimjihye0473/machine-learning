m <- 160
sigma <- 7
n <- 10 
se <- sigma/sqrt(n)
pnorm(157, mean = m, sd = se)

m <- 15 
var <- 100
n <- 25
se <- sqrt(var/n)
pnorm(20, mean = m, sd = se)

#예제
m <- 6 
sigma <- 2 
n <- 36
se <- sigma/sqrt(n)
pnorm(0.5, mean=m, sd=se)

m <- 1200
sigma <- 180
n  <- 64
se <- sigma/sqrt(n)
(1-pnorm(1170, mean=m, sd=se))-pnorm(1230, mean=m, sd=se)

m <- 72 
sigma <- 8
n <- 100 
se <- sigma/sqrt(n)
pnorm(73.5, mean = m, sd = se)-pnorm(70.5, mean = m, sd = se)


#모집단 2개일 때
n1 <- 64 
m1 <- 4 
var1 <- 16
n2 <- 75
m2 <- 12 
var2 <- 48

se <- sqrt((var1/n1)+(var2/n2))
pnorm(-6, mean = m1 - m2, sd = se)


m1 <- 10
var1 <- 25 
n1 <- 50
m2 <- 20
var2 <- 45
n2 <- 60
z <- ((m1 - m2)/ sqrt((var1/n1)+(var2/n2)))
1- pnorm 

#t-test 
group_a <- c(85,88,90,92,91,87,89,86,84,83)
group_b <- c(78,82,80,85,84,87,83,81,80,79)

mean_a <- mean(group_a)
mean_b <- mean(group_b)
sd_a <- sd(group_a)
sd_b <- sd(group_b)
t_statistic <- (mean_a - mean_b) / sqrt((sd_a^2)/length(group_a)) + (sd_b^2)/length(group_b)))

df <- length(group_a)+length(group_b) - 2


#Z-test 
library(BSDA)

group_a <- c(85,88,90,92,91,87,89,86,84,83,85,88,90,92,91,87,89,86,84,83,85,88,90,92,91,87,89,86,84,83,85,88,90,92,91,87,89,86,84,83)
group_b <- c(78,82,80,85,84,87,83,81,80,79,78,82,80,85,84,87,83,81,80,79,78,82,80,85,84,87,83,81,80,79,78,82,80,85,84,87,83,81,80,79)

sd_a =sd(group_a)
sd_b =sd(group_b)

result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.t = sd_b, alternative = "two.sided")

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")



























