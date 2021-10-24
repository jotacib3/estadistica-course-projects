rm(list=ls()) # Clean the global environment

descriptiveStatistics = function(sample) {
  variance <- var(sample)
  
  return (c(min = min(sample),
            max = max(sample),
            mean = mean(sample),
            median = median(sample),
            variance = var(sample),
            standDesv = sd(sample),
            coefVar =  variance/mean(sample)))
}

generatePopulation <- function(size=500) {
  return(rnorm(size))
}

analyzeSample <- function(selectedSample, replace=FALSE) {
  print(paste("Analyze for size ", length(selectedSample)))
  
  print("Calculation of Measures")
  print(descriptiveStatistics(selectedSample))
  print("Quantile")
  print(quantile(selectedSample))
  
  hist(selectedSample, main=paste("Histogram for Sample with size", length(selectedSample), "and Replace", replace))
  boxplot(selectedSample, main = paste("Box Plot Sample with size", length(selectedSample), "and Replace", replace))
  boxplot(selectedSample, main = paste("Box Plot Sample with size", length(selectedSample), "and Replace", replace))
}

meanConfidenceInterval = function(sample, variance = NULL)
{
  porcent <- 0.95
  alpha <- 1 - porcent
  i <- 1 - alpha/2.0
  n <- length(sample)
  sqrt.n = sqrt(n)
  z <- qnorm(i)
  x <- mean(sample)
  
  if(!is.null(variance))
  {
    e <- z*(sqrt(variance)/sqrt.n)
    return (c(x - e, x + e))
  }
  else if(n > 30)
  {
    ds <- sd(sample)
    e <- z*(ds/sqrt.n)
    return (c(x - e, x + e))
  }
  else
  {
    ds <- sd(sample)
    t = qt(p = i, df = n-1) # t distribution
    e = t*(ds/sqrt.n)
    return (c(x - e, x + e))
  }
}

varianceConfidenceInterval = function(sample)
{
  porcent <- 0.95
  alpha <- 1 - porcent
  size <- length(sample)
  variance <- var(sample)
  chiL <- qchisq(1 - alpha/2.0, size - 1)
  chiR <- qchisq(alpha/2.0, size - 1)
  return (c( (size - 1)* variance / chiL, (size - 1) * variance / chiR))
}

#-----------------------POPULATION--------------------------------------------
population <- generatePopulation()
"Population results"
descriptiveStatistics(population)
fivenum(population)
quantile(population)
boxplot(population, main = "Population Box Plot")

#-----------------------SAMPLES-------------------------------------------------
# Selecting samples
"Calculation of Measures"


"Sample for size=25 and REPLACE=TRUE"
samplesWithReplacement1 <- sample(population, 25, TRUE)
descriptiveStatistics(samplesWithReplacement1)
summary(samplesWithReplacement1)
quantile(samplesWithReplacement1)

"Sample for size=25 and REPLACE=FALSE"
samplesWithoutReplacement1 <- sample(population, 25)
descriptiveStatistics(samplesWithoutReplacement1)
quantile(samplesWithoutReplacement1)


"Sample for size=60 and REPLACE=TRUE"
samplesWithReplacement2 <- sample(population, 60, TRUE)
descriptiveStatistics(samplesWithReplacement2)
quantile(samplesWithReplacement2)

"Sample for size=60 and REPLACE=FALSE"
samplesWithoutReplacement2 <- sample(population, 60)
descriptiveStatistics(samplesWithoutReplacement2)
quantile(samplesWithoutReplacement2)

"Sample for size=129 and REPLACE=TRUE"
samplesWithReplacement3 <- sample(population, 129, TRUE)
descriptiveStatistics(samplesWithReplacement3)
quantile(samplesWithReplacement3)

"Sample for size=129 and REPLACE=FALSE"
samplesWithoutReplacement3 <- sample(population, 129)
descriptiveStatistics(samplesWithoutReplacement3)
quantile(samplesWithoutReplacement3)

"Sample for size=350 and REPLACE=TRUE"
samplesWithReplacement4 <- sample(population, 350, TRUE)
descriptiveStatistics(samplesWithReplacement4)
quantile(samplesWithReplacement4)

"Sample for size=350 and REPLACE=FALSE"
samplesWithoutReplacement4 <- sample(population, 350)
descriptiveStatistics(samplesWithoutReplacement4)
quantile(samplesWithoutReplacement4)


# HISTOGRAMS
hist(population, main="Population Histogram")
par(mfrow=c(4,2), mar=c(2, 2, 1, 1))

hist(samplesWithReplacement1, main=paste("Histogram for Sample with size", length(samplesWithReplacement1), "and Replace TRUE"))
hist(samplesWithoutReplacement1, main=paste("Histogram for Sample with size", length(samplesWithoutReplacement1), "and Replace FALSE"))

hist(samplesWithReplacement2, main=paste("Histogram for Sample with size", length(samplesWithReplacement2), "and Replace TRUE"))
hist(samplesWithoutReplacement2, main=paste("Histogram for Sample with size", length(samplesWithoutReplacement2), "and Replace FALSE"))

hist(samplesWithReplacement3, main=paste("Histogram for Sample with size", length(samplesWithReplacement3), "and Replace TRUE"))
hist(samplesWithoutReplacement3, main=paste("Histogram for Sample with size", length(samplesWithoutReplacement3), "and Replace FALSE"))

hist(samplesWithReplacement4, main=paste("Histogram for Sample with size", length(samplesWithReplacement4), "and Replace TRUE"))
hist(samplesWithoutReplacement4, main=paste("Histogram for Sample with size", length(samplesWithoutReplacement4), "and Replace FALSE"))

# BOX PLOTS
par(mfrow=c(4,2), mar=c(2, 2, 1, 1))

boxplot(samplesWithReplacement1, main = paste("Box Plot Sample with size", length(samplesWithReplacement1), "and Replace TRUE"))
boxplot(samplesWithoutReplacement1, main = paste("Box Plot Sample with size", length(samplesWithoutReplacement1), "and Replace FALSE"))

boxplot(samplesWithReplacement2, main = paste("Box Plot Sample with size", length(samplesWithReplacement2), "and Replace TRUE"))
boxplot(samplesWithoutReplacement2, main = paste("Box Plot Sample with size", length(samplesWithoutReplacement2), "and Replace FALSE"))

boxplot(samplesWithReplacement3, main = paste("Box Plot Sample with size", length(samplesWithReplacement3), "and Replace TRUE"))
boxplot(samplesWithoutReplacement3, main = paste("Box Plot Sample with size", length(samplesWithoutReplacement3), "and Replace FALSE"))

boxplot(samplesWithReplacement4, main = paste("Box Plot Sample with size", length(samplesWithReplacement4), "and Replace TRUE"))
boxplot(samplesWithoutReplacement4, main = paste("Box Plot Sample with size", length(samplesWithoutReplacement4), "and Replace FALSE"))

# CONFIDENCE INTERVAL
paste("Confidence Intervals for Sample with size", length(samplesWithReplacement1), "and Replace TRUE")
c("Mean:", meanConfidenceInterval(samplesWithReplacement1))
c("Variance: ", varianceConfidenceInterval(samplesWithReplacement1))

paste("Confidence Intervals for Sample with size", length(samplesWithoutReplacement1), "and Replace FALSE")
c("Mean:", meanConfidenceInterval(samplesWithoutReplacement1))
c("Variance: ", varianceConfidenceInterval(samplesWithoutReplacement1))

paste("Confidence Intervals for Sample with size", length(samplesWithReplacement2), "and Replace TRUE")
c("Mean:", meanConfidenceInterval(samplesWithReplacement2))
c("Variance: ", varianceConfidenceInterval(samplesWithReplacement2))

paste("Confidence Intervals for Sample with size", length(samplesWithoutReplacement2), "and Replace FALSE")
c("Mean:", meanConfidenceInterval(samplesWithoutReplacement2))
c("Variance: ", varianceConfidenceInterval(samplesWithoutReplacement2))

paste("Confidence Intervals for Sample with size", length(samplesWithReplacement3), "and Replace TRUE")
c("Mean:", meanConfidenceInterval(samplesWithReplacement3))
c("Variance: ", varianceConfidenceInterval(samplesWithReplacement3))

paste("Confidence Intervals for Sample with size", length(samplesWithoutReplacement3), "and Replace FALSE")
c("Mean:", meanConfidenceInterval(samplesWithoutReplacement3))
c("Variance: ", varianceConfidenceInterval(samplesWithoutReplacement3))

paste("Confidence Intervals for Sample with size", length(samplesWithReplacement4), "and Replace TRUE")
c("Mean:", meanConfidenceInterval(samplesWithReplacement4))
c("Variance: ", varianceConfidenceInterval(samplesWithReplacement4))

paste("Confidence Intervals for Sample with size", length(samplesWithoutReplacement4), "and Replace FALSE")
c("Mean:", meanConfidenceInterval(samplesWithoutReplacement4))
c("Variance: ", varianceConfidenceInterval(samplesWithoutReplacement4))