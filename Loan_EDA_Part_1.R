#Question 1
loan <- read.csv("~/Documents/Maryville University/DSCI_502/loan.csv")
View(loan)

#Question 3
min(loan$loan_amnt)

max(loan$loan_amnt)

mean(loan$loan_amnt)

median(loan$loan_amnt)

sd(loan$loan_amnt)

quantile(loan$loan_amnt, probs = c(0.25, 0.5, 0.75))

#Question 4
min(loan$int_rate)

max(loan$int_rate)

mean(loan$int_rate)

median(loan$int_rate)

sd(loan$int_rate)

quantile(loan$int_rate, probs = c(0.25, 0.5, 0.75))

#Question 5
cor(loan$int_rate, loan$installment)

#Question 6
loan$term <- as.factor(loan$term)

table(loan$term)

names(sort(-table(loan$term)))[1]

#Question 7
loan$loan_status <- as.factor(loan$loan_status)

prop.table(table(loan$loan_status))

names(sort(-prop.table(table(loan$loan_status))))[1]

#Question 8
xtabs.term.loanstatus <- xtabs(~ term + loan_status, data = loan)
print(xtabs.term.loanstatus)

prop.table(xtabs.term.loanstatus, margin = 1)

prop.table(xtabs.term.loanstatus, margin = 2)

#Question 9
loan$id <- as.factor(loan$id)
loan$grade <- as.factor(loan$grade)
loan$emp_length <- as.factor(loan$emp_length)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$verification_status <- as.factor(loan$verification_status)

summary(loan)








