#QUESTION 2
#Obtaining and cleaning data from missing values
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
foo <- foo[c(-19, -47), ]
which(is.na(foo) == TRUE)

#Creating logistic models for two different ourcomes: peacekeeping value.
glm1 <- glm(foo$pbs2s3~foo$wartype+foo$logcost+ foo$wardur +foo$factnum +foo$factnum2+ foo$trnsfcap + foo$treaty +foo$develop +foo$exp +foo$decade+ foo$untype, data=foo, family=binomial)
glm2 <- glm(foo$pbs2s3~foo$wartype+foo$logcost+ foo$wardur +foo$factnum +foo$factnum2+ foo$trnsfcap + foo$treaty +foo$develop +foo$exp +foo$decade+ foo$untype + I(foo$exp*foo$untype4)+ I(foo$wardur*foo$logcost), data=foo, family=binomial)
summary(glm1)

#Finding mean values for every covariate except for wardur to use them in a for loop later and see how the outcome changes as the function of war dur.
mean.wartype <- mean(foo$wartype)
mean.logcost <- mean(foo$logcost)
mean.factnum <- mean(foo$factnum)
mean.factnum2 <- mean(foo$factnum2)
mean.trnsfcap <- mean(foo$trnsfcap)
mean.develop <- mean(foo$develop)
mean.exp <- mean(foo$exp)
mean.decade <- mean(foo$decade)
mean.treaty <- mean(foo$treaty)

#function which find an outcome (UN peacekeeping effect) given values off all of the variables and cooefficients from the logistic model, and then transforms it into a value between 0 and 1
get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

#Creating two empty storage vectors
storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)

#For loop that returns a vector with all of treatment effectss for each value of wardur, holding other variables at their means (without the interactiion terms)
for (wardur in 1:315) {
  
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
              mean.trnsfcap, mean.treaty, mean.develop, mean.exp, mean.decade, 1)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.treaty, mean.develop, mean.exp, mean.decade, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm1))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm1))
}

#finding treatment effects for each wardur value
original_y <- storage.original.treat - storage.original.control

#Creating two empty storage vectors
storage.new.treat <- rep(NA, 315)
storage.new.control <- rep(NA, 315)


#For loop that returns a vector with all of treatment effectss for each value of wardur, holding other variables at their means (with the interactiion terms)
for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.treaty, mean.develop, mean.exp, mean.decade, 1, mean.exp*1, wardur*mean.logcost)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.treaty, mean.develop, mean.exp, mean.decade, 0, mean.exp*0, wardur*mean.logcost)
  storage.new.treat[wardur]  <- get_logit(X.treat, coef(glm2))
  storage.new.control[wardur]  <- get_logit(X.control, coef(glm2))
}
logcost_y <- storage.new.treat - storage.new.control

#plotting two lines - the original one and the one with the interaction terms, both representing peacekeeping success as a function of wardur
plot(1:315, original_y, type = "l", ylim = c(0, 0.8), ylab = "Marginal effects of UN peacekeeping operations", xlab="Duration of war in months", axes = FALSE, lty=2)
lines(1:315, logcost_y, col = "blue", ylim = c(0, 0.8))
ticks1 = c(5, 20, 35, 50, 65, 80, 95, 115, 140, 165, 190, 215, 240, 265, 290, 315)
ticks2 = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
axis(side = 1, at = ticks1)
axis(side = 2, at = ticks2)
legend(5, 0.15, legend=c("Original model", "Model with interaction terms"),
       col=c("black", "blue"), lty=2:1, cex=0.85)
box()

#QUESTION 4

#PART 1: CALCULATIONS FOR THE SUCCESS AFTER 2 YEARS

library(Matching)
#Importing data and cleaning it from the missing variables
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-19, -47), ]
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != "None")] <- 1
foo$Tr <- Tr

#Creating a logistic model for the peacekeeping success after 2 years
glm2l <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty + Tr, 
             data = foo, family = "binomial")

#logistic regression - creating counterfactuals for both treated and control units, preding the outcomes and subtracting them from the orignial outcomes to find out treatment effects for each unit
foo.counter_factual <- foo
foo.counter_factual$Tr <- rep(1, nrow(foo)) - foo$Tr
counter.factuals <- predict(glm2l, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo))
mask <- foo$Tr == 1
unit_treat_effects[mask] <- glm2l$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm2l$fitted.values[!mask]
mean(unit_treat_effects)
sd(unit_treat_effects)

#PROPENSITY SCORES
#Creating a logisitc model for to find the propensity scores
glmTr <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty, 
             data = foo, family = "binomial")
X <- glmTr$fitted.values

#Specifying Y
Y1 <- foo$pbs2l

#Propensity scores matching: Matching the units with similar propensity scores and then checking the balance for the data
set.seed(145)
m1  <- Match(Y=Y1, Tr=Tr, X=X, M=1)
summary(m1)
mb1 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m1, nboots=500)

#Creating another X for all of the variables AND propensity scores+ just variables
attach(foo)
X1 = cbind(wartype, logcost, wardur, factnum, 
           factnum2, trnsfcap, develop, exp, decade, treaty)
BalMat1 = cbind(glmTr$fitted.values, wartype, logcost, wardur, factnum, 
                factnum2, trnsfcap, develop, exp, decade, treaty, I(wardur*logcost), I(decade**2))
BalMat2 = cbind(wartype, logcost, wardur, factnum, 
                factnum2, trnsfcap, develop, decade, treaty, I(wardur*logcost), I(decade**2))
detach(foo) 

#Estimating ATT with all the variables and propensity scores + Matching and finding the balance of the covariates
set.seed(123)
genout <- GenMatch(Tr=Tr, X=X1, M=1, BalanceMatrix = BalMat1,
                  pop.size=200, max.generations=10, wait.generations=25)
m3  <- Match(Y=Y1, Tr=Tr, X=X1, M=1, Weight.matrix = genout)
summary(m3)
mb3 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap +
                       develop + exp + decade + treaty, data = foo, 
                    match.out = m3, nboots=500

#Estimating ATT with all the variables but with NO propensisty scores + Matching and finding the balance of the covariates
set.seed(123)
genout2 <- GenMatch(Tr=Tr, X=X1, M=1, BalanceMatrix = BalMat2,
                   pop.size=200, max.generations=10, wait.generations=25)
m4  <- Match(Y=Y1, Tr=Tr, X=X1, M=1, Weight.matrix = genout2)
summary(m4)
mb4 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m4, nboots=500)


#PART2: CALCULATIONS FOR THE SUCCESS AFTER 5 YEARS

#Creating a logistic model for the peacekeeping success after 5 years 
glm5l <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty + Tr, 
             data = foo, family = "binomial")
  
#Specifying Y and finding missing data
Y2 <- foo$pbs5l
mask <- which(!is.na(Y2))
NAs <- is.na(foo$pbs5l)

#logistic regression - creating counterfactuals for both treated and control units, preding the outcomes and subtracting them from the orignial outcomes
foo.counter_factual1 <- foo[!NAs,]
foo.counter_factual1$Tr <- 1 - foo$Tr[!NAs]
counter.factuals1 <- predict(glm5l, newdata=foo.counter_factual1, type="response")
unit_treat_effects1 <- rep(NA, nrow(foo[!NAs,]))
mask1 <- foo[!NAs,]$Tr == 1
unit_treat_effects1[mask1] <- glm5l$fitted.values[mask1] - counter.factuals1[mask1]
unit_treat_effects1[!mask1] <- counter.factuals1[!mask1] - glm5l$fitted.values[!mask1]
mean(unit_treat_effects1)
sd(unit_treat_effects1)

#Propensity score matching
sed.seed(123)
m7  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask], M=1)
summary(m7)
mb7 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m7, nboots=500)
mb7$AMsmallestVarName
mb7$AMsmallest.p.value

#Estimating ATT with all the variables and propensity scores + Matching and finding the balance of the covariates
set.seed(123)
genout5 <- GenMatch(Tr=Tr[mask], X=X1[mask,], M=1, BalanceMatrix = BalMat1[mask,],
                     pop.size=200, max.generations=10, wait.generations=25)
m5  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X1[mask,], M=1, Weight.matrix = genout5)
summary(m5)
mb5 <- MatchBalance(Tr ~ wartype + logcost + wardur+ factnum + factnum2 + 
                        trnsfcap + develop + exp + decade + treaty, data = foo, 
                      match.out = m5, nboots=500)
  
#Estimating ATT with all the variables but with NO propensisty scores + Matching and finding the balance of the covariates
set.seed(123)
genout6 <- GenMatch(Tr=Tr[mask], X=X1[mask,], M=1 , BalanceMatrix = BalMat2[mask,],
                      pop.size=200, max.generations=10, wait.generations=25)
m6  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X1[mask,], M=1, Weight.matrix = genout6)
summary(m6)
mb6 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                        trnsfcap + develop + exp + decade + treaty, data = foo, 
                      match.out = m6, nboots=500)
