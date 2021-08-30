foo <- read.csv("https://tinyurl.com/yb4phxx8")

names(foo)
head(foo)
dim(foo)

date.columns <- c(11 , 12 , 14 , 15 , 16 , 17 , 18 , 25)
for (i in date.columns) {
  which_values_are_missing <- which(as.character(foo[,i]) == "")
  foo[ which_values_are_missing,i] <- NA
  foo[,i] <- as.Date(as.character(foo[,i ]))
}

#installing the packages to use "filter" function
install.packages("dplyr")
library(dplyr)

#filtering the data and adding only the rows with CirculationDate higher or equal to 2009-01-01
analyzefoo <- filter(foo, CirculationDate >= "2009-01-01")

#calculating the mean number of month given for each project; na.rm part allows us to ignore any  NA values which are in the data set
mean(analyzefoo$OriginalCompletionDate - analyzefoo$ApprovalDate, na.rm = TRUE)/30.5

#identifying what Circulation Data was in the middle to separate the "earlier" and "later" projects
median(analyzefoo$CirculationDate)

#separating the "earlier" and "later" projects with the filter() function
circearlier <- filter(analyzefoo, CirculationDate < "2014-02-01")
circlater <- filter(analyzefoo, CirculationDate >= "2014-02-01")

#creating a function which calculates a mean, median and quantile so we did not have to type it many times
descriptivestats <- function(x){
  print(paste("The mean is:", mean(x, na.rm = TRUE)), digits = NULL)
  print(paste("The median is:", median(x, na.rm = TRUE)), digits = NULL)
  quantile(x, na.rm = TRUE)
}


#calculating the descriptive stats for the earlier projects
descriptivestats(circearlier$RevisedCompletionDate - circearlier$OriginalCompletionDate)

#calculating the descriptive stats for the later projects
descriptivestats(circlater$RevisedCompletionDate - circlater$OriginalCompletionDate)

#calculating the descriptive stats of the original durations of the projects
descriptivestats(analyzefoo$OriginalCompletionDate - analyzefoo$ApprovalDate)

#calculating the descriptive stats of the actual durations of the projects
descriptivestats(analyzefoo$RevisedCompletionDate - analyzefoo$ApprovalDate)

#separating the projects which have been completed after 2010
rateanalysis <- filter(analyzefoo, RevisedCompletionDate >= "2010-01-01")

#creating a table with the ratings for all of the projects in the created data frame
round((table(rateanalysis$Rating)/(length(rateanalysis$Rating)-sum(is.na(rateanalysis$Rating))))*100, digits = 0)

#separating the projects with the PATA type
PATAanalysis <- filter(rateanalysis, Type == "PATA")

#creating a table with the ratings for all of the projects in the created PATA data frame
round((table(PATAanalysis$Rating)/(length(PATAanalysis$Rating)-sum(is.na(PATAanalysis$Rating))))*100, digits = 2)

#ordering the data (Revised amount from min to max) to get the top and bottom 10%
ordered1 <- analyzefoo[order(analyzefoo$RevisedAmount),]

#creating dataframes with only top and bottom 10%
bottom10 <- ordered1[1:(nrow(ordered1)/10),]
top10 <- ordered1[((nrow(ordered1)/10)*9):(nrow(ordered1)),]

#calculating descriptive stats for top10% projects
mean(top10$Rating, na.rm  = TRUE)
median(top10$Rating, na.rm  = TRUE)
table(top10$Rating)

#calculating descriptive stats for bottom10% projects
mean(bottom10$Rating, na.rm = TRUE)
median(bottom10$Rating, na.rm = TRUE)
table(bottom10$Rating)

#checking other characteristis of top and bottom projects to identify biases in the data
table(bottom10$Country)
table(top10$Country)
table(bottom10$Dept)
table(top10$Dept)
table(bottom10$Cluster)
table(top10$Cluster)