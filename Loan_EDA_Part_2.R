#Question 1
loan <- read.csv("~/Documents/Maryville University/DSCI_502/loan.csv")
View(loan)

#Question 2
#Histogram
hist(loan$loan_amnt, 
     main = "Histogram of Loan Amounts", 
     xlab= "Loan Amounts", ylab = "Count", 
     xlim= c(0, 35000), 
     ylim = c(0,120000))
#Density
dloan_amnt <- density(loan$loan_amnt)
plot(dloan_amnt, 
     main = "Density Plot of Loan Amounts")
#Histogram and Density Plots on Same Axis
hist(loan$loan_amnt, 
     main = "Histogram and Density Plots of Loan Amounts", 
     prob = TRUE,
     xlab= "Loan Amounts", 
     ylab = "Probability")
lines(density(loan$loan_amnt),
      lwd = 2,
      col = "red")

#Question 3
ggplot(data = loan, 
       aes(x=loan_amnt)) +
geom_histogram(aes(y=..density..), 
               colour="black", 
               fill="white") +
geom_density(alpha=.2, 
             fill="blue") +
geom_vline(aes(xintercept=mean(loan_amnt)), 
           color="green", 
           linetype="dashed", 
           size=1) +
labs(title = "Histogram and Density Plots w/Vertical Line as Mean",
     x = "Loan Amount",
     y = "Density")

#Question 4
plot(loan$annual_inc, 
     loan$loan_amnt, 
     main = "Scatter Plot of Annual Income vs Loan Amount", 
     xlab = "Annual Income", 
     ylab = "Loan Amount")
abline(lm(loan_amnt~annual_inc,data = loan), lwd = 2, col="red")

#Question 5
ggplot(data = loan, 
       aes(x = annual_inc, y = loan_amnt)) +
geom_point() + 
geom_smooth() +
labs(title = "Scatter Plot of Annual Income vs Loan Amount",
     x = "Annual Income",
     y = "Loan Amount")

#Question 6
loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)
tbl_term_grade <- xtabs(~term + grade, data = loan)
barplot(tbl_term_grade, 
        main = "Bar Plots for Term and Grade", 
        xlab = "Grade", 
        ylab= "Count", 
        ylim = c(0, 250000),
        legend = rownames(tbl_term_grade), 
        beside = TRUE)

#Question 7
ggplot(data = loan, aes(x = grade, y= ..count..)) + 
geom_bar(aes(fill = term), position = "dodge") +
labs(title = "Bar Plots for Term and Grade", 
     x = "Grade", 
     y = "Count")

#Question 8
jpeg("C:\\Users\\symphonyhopkins\\Documents\\Maryville University\\DSCI_502\\loanterm.jpg")
boxplot(loan_amnt ~ term, 
        data = loan, 
        notch = TRUE, 
        col=c("green"),
        main="Boxplots by Term", xlab="Term")
#close the file
dev.off()

#Question 9
ggplot(data = loan, aes(x = term, y = loan_amnt)) + 
geom_boxplot(notch = TRUE) +
labs(title = "Boxplots by Term", x = "Term", y = "Loan Amount")
ggsave("C:\\Users\\symphonyhopkins\\Documents\\Maryville University\\DSCI_502\\loanterm.jpg")

