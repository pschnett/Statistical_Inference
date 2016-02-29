##Comparing the simulated mean and variance with the theoretical values

#We will run 1000 rounds of simulation of 40 exponentials with $\lambda = 0.2$, using 
#a fixed seed, and comparing the distribution of the simulated mean and variance with 
#the theoretical value of $1 / \lambda$:

install.packages("pander")
library(pander)
nSim <- 1000
nVals <- 40
lambda <- 0.2
set.seed(456)

#Create exponential distribution and transpose into a matrix 
simMatrix <- t(replicate(nSim, rexp(nVals, lambda)))

#Create a datafrom consisting of the Mean and variance
df <- data.frame(
        Mean=c(mean(rowMeans(simMatrix)), 1/lambda),
        Variance=c(mean(apply(simMatrix, 1, var)), 1/lambda^2))
rownames(df) <- c("Sample", "Theoretical")
pander(df, round=2)

#Assessing if the simulated values are approximately normal

#Also, according to the CLT, the distribution of the simulated means should be 
#approximately normal. To illustrate this we will normalize the vectors and compare 
#it to a $N(0,1)$ distribution.

library(ggplot2)
meanVals <- rowMeans(simMatrix)
zMean <- (meanVals - mean(meanVals)) / sd(meanVals)
qplot(zMean, geom = "blank") +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth=.35) +
  geom_vline(xintercept=0, colour="red", linetype="longdash") +
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
  ylab("Density") + xlab("z") + ggtitle("Distribution of means of 40 Exponential Distribution Lambda of 0.2") +
  theme_bw() + theme(legend.position = c(0.85, 0.85))

#Evaluating the coverage of the confidence interval

#Theoretically, a 95% confidence interval should contain, if we simulate a big number of them, 
#the mean value for the exponential distribution ($1 / \lambda$) 95% of the time

set.seed(456)
lambda <- 0.2
# checks for each simulation if the mean is in the confidence interval
inconfint <- function(lambda, nSim) {
  expd <- rexp(nSim, lambda)
  se <- sd(expd)/sqrt(nSim)
  llim <- mean(expd) - 1.96 * se
  ulim <- mean(expd) + 1.96 * se
  (llim < 1/lambda & ulim > 1/lambda)
}
# estimate the coverage in each round of simulations
coverage <- function(lambda) {
  covvals <- replicate(100, inconfint(lambda))
  mean(covvals)
}
# perform the simulation
simres <- replicate(100, coverage(lambda))
mean(simres)