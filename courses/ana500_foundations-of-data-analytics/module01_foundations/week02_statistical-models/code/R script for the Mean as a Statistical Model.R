#The Mean as a Statistical Model

#We are going to create a variable
#Exam scores on statistics final for 12 students

examscore <- c(81, 95, 79, 80, 73, 87, 81, 90, 54, 86, 71, 62)

#I want a variable to identify the students

student <- 1:12

#Let's put these variable together in a single dataframe

stat_exam <- data.frame(student=student, examscore=examscore)

print(stat_exam)

#Let's plot the exam score data

plot(student, examscore, xlab="Student", ylab="Exam Score")

#We can describe exam scores with:
#Exam Score = Mean Exam Score + Error

#Outcome = Model + Error

mean(examscore)

#We are going to add a horizontal line at the mean exam score

plot(student, examscore, xlab="Student", ylab="Exam Score", 
     abline(h=mean(examscore), col="red"))

#Let's calculate deviance, and variance, and standard deviation

stat_exam$devsq <- (stat_exam$examscore - 78.25)^2

print(stat_exam)

#Let's add up all of the deviances squared
sumdevsq <- sum(stat_exam$devsq)

#Let's calculate the variance by dividing sum of deviances squared by n-1
var <- sumdevsq/11
print(var)

#Let's calculate the standard deviation by taking square root of variance

stndev <- sqrt(var)
print(stndev)

var(stat_exam$examscore)
sd(stat_exam$examscore)

#Let's calculate the standard error for the mean
stnderror <- stndev / sqrt(12)
print(stnderror)

#95% confidence interval for a mean:
# Sample mean +- 1.96*standard error

#With a small sample (n<30) we need to use the t-distribution with n-1
#degrees of freedom to find the appropriate critical values for confidence interval

tcrit <- qt(0.975, 11, lower.tail=TRUE)

print(tcrit)

ub <- 78.25 + (tcrit*stnderror)
lb <- 78.25 - (tcrit*stnderror)
print(lb)
print(ub)




