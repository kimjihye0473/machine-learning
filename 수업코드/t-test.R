data <-read.csv("C:/Users/asdf0/OneDrive/바탕 화면/t_test.csv")

t.test(x1, x2)
#groupA < groupB ?
t.test(x1,x2,alternative = "less")


