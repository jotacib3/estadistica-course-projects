rm(list=ls()) # Clean the global environment

data <- read.csv(file = "9 - Bank Marketing/bank-full.csv" , header = TRUE)

vars <- c("job","age","education")

job <- data$job
age <- as.numeric(data$age)
education <- data$education

calcMode <- function(sample) {
  uniqv <- unique(sample)
  return(uniqv[which.max(tabulate(match(sample,uniqv)))])
}


statisticInfo = function(sample) {
  variance <- var(sample)
  return (c(
    min = min(sample),
    max = max(sample),
    mean = mean(sample), 
    median = median(sample),
    mode=calcMode(sample),
    variance = variance,
    standDesv = sd(sample),
    coefVar =  variance/mean(sample)
    ))
}

#STATISTICS RESULTS

"General Statics"
summary(data[vars])

"Age"
statisticInfo(age)
quantile(age)

"Job"
table(job)
prop.table(table(job))

"Education"
table(education)
prop.table(table(education))

#HISTOGRAMS   
hist(age, main = "Histogram Age")

#BOX PLOTS
fivenum(age)
boxplot(age, main = "Box Plot Age")

