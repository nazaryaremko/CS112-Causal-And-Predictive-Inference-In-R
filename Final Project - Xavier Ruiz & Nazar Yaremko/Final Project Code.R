#Loaded relevant libraries
library(interplot)
library(Matching)
library(rbounds)
library(zoo)

#Loaded relevant data
load("repData.rda")
foo <- kdata
foo <- foo[,c(2:14,17)]

sum(is.na(kdata))
182/18000

#Replaced string values with corresponding numeric values
Tr <- rep(0,length(foo$att_party.bin))
Tr[which(foo$victim == "Victim")] <- 1
foo$victim <- Tr

ns <- rep(NA,length(foo$neighsec))
ns[which(foo$neighsec=="Worsened a lot/somewhat")] <- 0
ns[which(foo$neighsec=="Improved a lot/somewhat or same")] <- 1
foo$neighsec <- ns

prty <- rep(NA,length(foo$party))
prty[which(foo$party=="R does not identify with PAN")] <- 0
prty[which(foo$party=="R identifies with PAN")] <- 1
foo$party <- prty

#Filled NA values in dataframe with column means
foo <- na.aggregate(foo)


#STEP 1 - Balance Between Groups Evaluation
#Found the leximin p-value between control and tmt groups
mb1 <- MatchBalance(victim ~ kinship + tenure + age
                    + women + edu + neighsec  + party 
                    + vote + rural, data=foo)


#STEP 2 - Matching Process
#Defined the predictors and relevant outcomes for genetic matching
attach(foo)
X <- cbind(kinship, tenure, age, women, edu, neighsec, party, 
vote, rural, I(kinship*victim))
Y <- foo$att_party.bin
detach(foo)

#Performed genetic matching with victimization as treatment on the predictors above
genout <- GenMatch(Tr=Tr, X=X, M=1, pop.size=200, max=200,
                    max.generations=10, wait.generations=25)

#Matched units based on the weight matrix generated above and evaluated the effect estimate
mout <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix = genout)
summary(mout)

#Checked for balance between control and tmt groups post-matching process
mb2 <- MatchBalance(victim ~ kinship + tenure + age
                    + women + edu + neighsec  + party 
                    + vote + rural, data=foo, match.out=mout)


#STEP 3 - Sensitivity Analysis
#Performed a sensitivity test using the rbounds library
psens(mout, Gamma=1.5, GammaInc=.1)


#STEP 4 - Figure Replication
#Extracted tmt and control indexes from mout, as well as weights of the groups
t_units <- mout$index.treated
c_units <- mout$index.control
ws <- c(mout$weights,mout$weights)
  
#Created a new dataframe using the matched units
new_foo <- rbind(foo[t_units,],foo[c_units,])

#Re-created the original logistic model using the post-match dataset
new_model<- glm(att_party.bin ~ victim + kinship + tenure + age + women + edu 
                + neighsec  + party + vote + rural + victim*kinship,
                family='binomial', weights=ws,data=new_foo)

#Replicating the original figure using the interplot function
p<- interplot(m = new_model, var1 = "victim", var2 = "kinship") +
  facet_null() +
  xlab("Kinship Density") +
  ylab("Estimated Coefficient for Victimization") + 
  ggtitle("Estimated Coefficient of Victimization
          on Participation by Kinship Density") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")
p



###ADDITIONAL CODE FOR EVALUATING THE ADDITIONAL OUTCOMES

##FOR att_neigh.bin
#Step 1
attach(foo)
X <- cbind(kinship, tenure, age, women, edu, neighsec, party, 
           vote, rural, I(kinship*victim))
Y <- foo$att_neigh.bin
detach(foo)

genout2 <- GenMatch(Tr=Tr, X=X, M=1, pop.size=200, max=200,
                   max.generations=10, wait.generations=25)

mout2 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix = genout2)
summary(mout2)

mb3 <- MatchBalance(victim ~ kinship + tenure + age
                    + women + edu + neighsec  + party 
                    + vote + rural, data=foo, match.out=mout2)

#Step 3
psens(mout2, Gamma=1.5, GammaInc=.1)

#Step 4
t_units2 <- mout2$index.treated
c_units2 <- mout2$index.control
ws2 <- c(mout2$weights,mout2$weights)

new_foo2 <- rbind(foo[t_units2,],foo[c_units2,])

new_model2<- glm(att_neigh.bin ~ victim + kinship + tenure + age + women + edu 
                + neighsec  + party + vote + rural + victim*kinship,
                family='binomial', weights=ws2,data=new_foo2)

p<- interplot(m = new_model2, var1 = "victim", var2 = "kinship") +
  facet_null() +
  xlab("Kinship Density") +
  ylab("Estimated Coefficient for Victimization") + 
  ggtitle("Estimated Coefficient of Victimization
          on Participation by Kinship Density") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")
p


##FOR att_ngo.bin
#Step 1
attach(foo)
X <- cbind(kinship, tenure, age, women, edu, neighsec, party, 
           vote, rural, I(kinship*victim))
Y <- foo$att_ngo.bin
detach(foo)

genout3 <- GenMatch(Tr=Tr, X=X, M=1, pop.size=200, max=200,
                    max.generations=10, wait.generations=25)

mout3 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix = genout3)
summary(mout3)

mb4 <- MatchBalance(victim ~ kinship + tenure + age
                    + women + edu + neighsec  + party 
                    + vote + rural, data=foo, match.out=mout3)

#Step 3
psens(mout3, Gamma=1.5, GammaInc=.1)

#Step 4
t_units3 <- mout3$index.treated
c_units3 <- mout3$index.control
ws3 <- c(mout3$weights,mout3$weights)

new_foo3 <- rbind(foo[t_units3,],foo[c_units3,])

new_model3 <- glm(att_ngo.bin ~ victim + kinship + tenure + age + women + edu 
                 + neighsec  + party + vote + rural + victim*kinship,
                 family='binomial', weights=ws3,data=new_foo3)

p<- interplot(m = new_model3, var1 = "victim", var2 = "kinship") +
  facet_null() +
  xlab("Kinship Density") +
  ylab("Estimated Coefficient for Victimization") + 
  ggtitle("Estimated Coefficient of Victimization
          on Participation by Kinship Density") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")
p

##FOR att_union.bin
#Step 1
attach(foo)
X <- cbind(kinship, tenure, age, women, edu, neighsec, party, 
           vote, rural, I(kinship*victim))
Y <- foo$att_union.bin
detach(foo)

genout4 <- GenMatch(Tr=Tr, X=X, M=1, pop.size=200, max=200,
                    max.generations=10, wait.generations=25)

mout4 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix = genout4)
summary(mout4)

mb5 <- MatchBalance(victim ~ kinship + tenure + age
                    + women + edu + neighsec  + party 
                    + vote + rural, data=foo, match.out=mout4)

#Step 3
psens(mout4, Gamma=1.5, GammaInc=.1)

#Step 4
t_units4 <- mout4$index.treated
c_units4 <- mout4$index.control
ws4 <- c(mout4$weights,mout4$weights)

new_foo4 <- rbind(foo[t_units4,],foo[c_units4,])

new_model4<- glm(att_union.bin ~ victim + kinship + tenure + age + women + edu 
                 + neighsec  + party + vote + rural + victim*kinship,
                 family='binomial', weights=ws4,data=new_foo4)

p<- interplot(m = new_model4, var1 = "victim", var2 = "kinship") +
  facet_null() +
  xlab("Kinship Density") +
  ylab("Estimated Coefficient for Victimization") + 
  ggtitle("Estimated Coefficient of Victimization
          on Participation by Kinship Density") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")
p