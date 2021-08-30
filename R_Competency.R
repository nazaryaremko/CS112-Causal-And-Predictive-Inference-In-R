########## PROBLEM 1 #############

#creating the linear equation and putting the variables into the data frame
set.seed(123)
skip <- rnorm(998, 80, 1)
income <- 5000 - skip*3 + rnorm(998, 400, 100)
data <- data.frame(skip, income)

#creating two outliers and adding them to the data to create the second dataset
out1 <- c(87, 26430)
out2 <- c(94, 28439)
data2 <- rbind(data, out1)
data2 <- rbind(data2, out2)

#Creating linear models and checking their summaries to make sure that the slope has changed
lm.998 <- lm(income ~ skip, data = data)
summary(lm.998)
lm.1000 <- lm(income ~ skip, data = data2)
summary(lm.1000)
coefficients(lm(income ~ skip, data = data2))

#plotting the data, adding the regression lines and adding the legend
plot(data2, xlab = "The number of time an individual \n skipped high school/college", ylab = 'Income 5 years after graduating college')

abline(a = coef(lm.998)[1],
       b = coef(lm.998)[2],
       col = "blue")
abline(a = coef(lm.1000)[1],
       b = coef(lm.1000)[2],
       col = "red")

legend(77, 28000, legend=c("Line 1000", "Line 998"),
       col=c("red", "blue"), cex=0.8, lty=1:1)
       
########## PROBLEM 2 #############

#importing the library
library(Matching)
library(arm)
data(lalonde)

#A & B
#Creating two linear models: one for the control units and one of the treated units
lm_cont <- lm(re78 ~ age + I(age**2) + educ + re74 + re75, data = lalonde)
lm_treat <- lm(re78 ~ age + I(age**2) + educ + treat + treat*age + re74 + re75, data = lalonde)

#setting the number of iterations and running the simulation to obtain the coefficients from both models
iterations <- 10000
sim.lalonde_cont <- sim(lm_cont, n.sims = iterations)
sim.lalonde_treat <- sim(lm_treat, n.sims = iterations)

#creating empty matrix for control and treatment units
simulated.ys_cont <- matrix(NA, nrow = iterations, ncol = length(17:55))
simulated.ys_treat <- matrix(NA, nrow = iterations, ncol = length(17:55))

#separating the data to the treatment group and for the control group
lalonde_treat <- lalonde[which(lalonde$treat == 1), ]
lalonde_cont <- lalonde[which(lalonde$treat == 0), ]

#calculating means for each group
mean.educ <- mean(lalonde_cont$educ)
mean.re74 <- mean(lalonde_cont$re74)
mean.re75 <- mean(lalonde_cont$re75)

mean2.educ <- mean(lalonde_treat$educ)
mean2.re74 <- mean(lalonde_treat$re74)
mean2.re75 <- mean(lalonde_treat$re75)

#Filling in the matrix with the expected values for the control group by multiplying the Xs with the coefficients we obtained via simulation
for (age in 17:55) {
  Xs <- c(1, age, age**2, mean.educ, mean.re74, mean.re75)
  for (i in 1:iterations) {
    simulated.ys_cont[i, age + 1 - min(lalonde$age)] <- sum(Xs*sim.lalonde_cont@coef[i,])
  }
}

#Filling in the matrix with the expected values for the treatment group by multiplying the Xs with the coefficients we obtained via simulation
for (age in 17:55) {
  Xs2 <- c(1, age, age**2, mean2.educ, 1, mean2.re74, mean2.re75, 1*age)
  for (i in 1:iterations) {
    simulated.ys_treat[i, age + 1 - min(lalonde_treat$age)] <- sum(Xs2*sim.lalonde_treat@coef[i,])
  }
}

#C
#Finding the tretment effect by subtracting the control group expected values from the treatment group expected values
treat_effect <- simulated.ys_treat - simulated.ys_cont

#D
#Finding medians for different variables
median.educ <- median(lalonde_cont$educ)
median.re74 <- median(lalonde_cont$re74)
median.re75 <- median(lalonde_cont$re75)

median2.educ <- median(lalonde_treat$educ)
median2.re74 <- median(lalonde_treat$re74)
median2.re75 <- median(lalonde_treat$re75)

#Creating an empty matrix for the last problem
simulated.ys_d <- matrix(NA, nrow = iterations, ncol = length(17:55))

#Filling in the matrix with the treatment effect values and including simulated sigmas
for (age in 17:55) {
  Xs_cont <- c(1, age, age**2, median.educ, median.re74, median.re75)
  Xs_treat <- c(1, age, age**2, median2.educ, 1, median2.re74, median2.re75, 1*age)
  for (i in 1:iterations) {
    simulated.ys_d[i, age + 1 - min(lalonde_treat$age)] <- sum(Xs_treat*sim.lalonde_treat@coef[i,]) - sum(Xs_cont*sim.lalonde_cont@coef[i,]) + rnorm(1, 0, sim.lalonde_cont@sigma[i]) + rnorm(1, 0, sim.lalonde_treat@sigma[i])
  }
}

#Creating a fucntion to produce a summary table of the confidence intervals for the expected values
summary_table <- function(matrix, educ, re.74, re.75) {
  conf.int <- apply(matrix, 2, quantile, probs = c(0.025, 0.975))
  table <- t(data.frame(conf.int))
  colnames(table) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
  table <- data.frame(table, educ, re.74, re.75)
  rownames(table) <- 17:55
  View(table)
}

summary_table(simulated.ys_cont, mean.educ, mean.re74, mean.re75)
summary_table(simulated.ys_treat, mean2.educ, mean2.re74, mean2.re75)
summary_table(treat_effect, NA, NA, NA)
summary_table(simulated.ys_d, median.educ, median.re74, median.re75)

#Creating a function to plot the confidence intervals 
plot_conf <- function(x, title, limit) {
  plot(x = c(1:100), y = c(1:100), type = "n", 
       xlim = c(min(lalonde_cont$age),max(lalonde_cont$age)), 
       ylim = limit, 
       main = title, xlab = "Age", 
       ylab = "Real Earnings in 1978 (re78) ")
  conf.int <- apply(x, 2, quantile, probs = c(0.025, 0.975))
  for (age in 17:55) {
    segments(
      x0 = age,
      y0 = conf.int[1, age - 17 + 1],
      x1 = age,
      y1 = conf.int[2, age - 17 + 1],
      lwd = 2)
  }
}

plot_conf(simulated.ys_cont, "Confidence intervals of the expected values of re78 \n by age for control units with predictors held at their means", c(-5000,15000))
plot_conf(simulated.ys_treat, "Confidence intervals of the expected values of re78 \n by age for treatment units with predictors held at their means", c(-5000,20000))
plot_conf(treat_effect, "Confidence intervals of the treatment effect by age", c(-10000,15000))
plot_conf(simulated.ys_d, "Confidence intervals of the treatment effect \n by age with predictors held at the medians \n and simulated sigmas included", c(-25000,25000))

########## PROBLEM 3 #############

#Importing data and excluding NA values
lol <- read.csv(url("https://tinyurl.com/y2prc9xq"))
foo <- na.omit(lol)

#Creating a model and the function which returns coefficients
mod3 <- lm(MATH_SCORE ~ TREATMENT, data = foo)
boot.fn <- function(data, index) return(coef(lm(MATH_SCORE ~ TREATMENT, data = data, subset = index)))

#stting the number of iterations and creating an empty storage
iterations2 = 5000
storage <- c()

#Runnign the function 5000 times and adding the valeues the the storage vector
set.seed(123)
for (i in 1:iterations2) {
  storage[i] <- boot.fn(foo, sample(nrow(foo), nrow(foo), replace = TRUE))[2]
}

#Creating a histrogram with the bootstrap sample results
hist(storage, main="", xlab = "Bootstrapped treatment coefficient", ylab = "Number of occurrences in the bootstap sample")
title(main="Histogram of the bootstrap sample results")
axis(1, at=c(1.5,2.5,3.5,4.5,5.5,6.5))

#Calculating the confidence interval for the bootstrap sample
quantile(storage, probs = c(0.025, 0.975))
confint(mod3)

########## PROBLEM 4 #############

#function
rsquared <- function(ytrue, ypred) {
  RSS <- sum((ytrue - ypred)**2)
  TOT <- sum((ytrue - mean(ytrue))**2)
  return(1 - RSS/TOT)
}

#creating a storage vector
storage2 <- c()

#running the function 5000 times 
for (i in 1:iterations2) {
  subset <- sample(nrow(foo), nrow(foo), replace = TRUE)
  storage2 <- rsquared(foo$MATH_SCORE[subset], mod3$fitted.values[subset])
}

#prining the mean R-squared out of 5000 boostrapped results
mean(storage2)
summary(mod3)

########## PROBLEM 5 #############

#Importing the data and creating a vector with the rows of the test set
foo2 <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
set.seed(12345)
test_set_rows <- sample(1:length(foo2$age), 2000, replace = FALSE)

#separating the test data from the training data
test <- foo2[test_set_rows,]
train <- foo2[-test_set_rows,]



#Fitting 5 models of different complexities
model1 <- glm(treat~.-re78, data = train)
model2 <- glm(treat~age+education+re74+re75+I(age**2)+education*re74, data = train)
model3 <- glm(treat~re74+re75, data=train)
model4 <- glm(treat~education*re74, data=train)
model5 <- glm(treat~.-re78 +I(age**2)+age*education, data=train)

#Creating a function which returns the MSE using LOOCV and Validation-set method
MSE_summary <- function(model) {
  model.err <- cv.glm(train, model)
  print(model.err$delta[1])
  
  mean((test$treat - predict.lm(model, test)) **2)
}

#Running the fucntion on 5 different models
#1
MSE_summary(model1)

#2
MSE_summary(model2)

#3
MSE_summary(model3)

#4
MSE_summary(model4)

#5
MSE_summary(model5)