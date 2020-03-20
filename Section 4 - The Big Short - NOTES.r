# Yannique Hecht
# Harvardx: PH125.3 - (3) Data Science: Probability
# SECTION 4: THE BIG SHORT
# CLASS NOTES


# # # SECTION 4.1: THE BIG SHORT

# # THE BIG SHORT: INTEREST RATES EXPLAINED

# KEY POINTS
# Interest rates for loans are set using the probability of loan defaults to calculate a rate that minimizes the probability of losing money.
# We can define the outcome of loans as a random variable. We can also define the sum of outcomes of many loans as a random variable.
#The Central Limit Theorem can be applied to fit a normal distribution to the sum of profits over many loans. We can use properties of the normal distribution to calculate the interest rate needed to ensure a certain probability of losing money for a given probability of default.

# INTEREST RATE MONTE CARLO SIMULATION
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# INTEREST RATE MONTE CARLO SIMULATION
B <- 10000
losses <- replicate(B, {
    defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# PLOTTING EXPECTED LOSSES
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# EXPECTED VALUE AND STANDARD ERROR OF THE SUM OF 1,000 LOANS
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# CALCULATING INTEREST RATES FOR EXPECTED VALUE OF 0
# We can calculate the amount  𝑥  to add to each loan so that the expected value is 0 using the equation  𝑙𝑝+𝑥(1−𝑝)=0 . Note that this equation is the definition of expected value given a loss per foreclosure  𝑙  with foreclosure probability  𝑝  and profit  𝑥  if there is no foreclosure (probability  1−𝑝 ).
# We solve for  𝑥=−𝑙𝑝1−𝑝  and calculate  𝑥 :
x = - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000

# CALCULATING INTEREST RATE FOR 1% PROBABILITY OF LOSING MONEY
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x    # required profit when loan is not a foreclosure
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# MONTE CARLO SIMULATION FOR 1% PROBABILITY OF LOSING MONEY
B <- 100000
profit <- replicate(B, {
    draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob=c(1-p, p), replace = TRUE) 
    sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money


# # THE BIG SHORT

# KEY POINTS
# The Central Limit Theorem states that the sum of independent draws of a random variable follows a normal distribution. However, when the draws are not independent, this assumption does not hold.
# If an event changes the probability of default for all borrowers, then the probability of the bank losing money changes.
# Monte Carlo simulations can be used to model the effects of unknown changes in the probability of default.

# EXPECTED VALUE WITH HIGHER DEFAULT RATE AND INTEREST RATE
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# CALCULATING NUMBER OF LOANS FOR DESIRED PROBABILITY OF LOSING MONEY
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# MONTE CARLO SIMULATION WITH KNOWN DEFAULT PROBABILITY
# This Monte Carlo simulation estimates the expected profit given a known probability of default  𝑝=0.04 . Note that your results will differ from the video because the seed is not set.
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob=c(1-p, p), replace = TRUE) 
    sum(draws)
})
mean(profit)

# MONTE CARLO SIMULATION WITH UNKNOWN DEFAULT PROBABILITY
# This Monte Carlo simulation estimates the expected profit given an unknown probability of default  0.03≤𝑝≤0.05 , modeling the situation where an event changes the probability of default for all borrowers simultaneously. Note that your results will differ from the video because the seed is not set.
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
    new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
    draws <- sample( c(x, loss_per_foreclosure), n, 
                        prob=c(1-new_p, new_p), replace = TRUE) 
    sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million