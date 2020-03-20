# Yannique Hecht
# Harvardx: PH125.3 - (3) Data Science: Probability
# SECTION 3: RANDOM VARIABLES, SAMPLING MODELS, & CENTRAL LIMIT THEOREM
# CLASS NOTES


# # # SECTION 3.1: RANDOM VARIABLES & SAMPLING MODELS

# # RANDOM VARIABLES

# KEY POINTS
# Random variables are numeric outcomes resulting from random processes.
# Inference offers a framework for quantifying uncertainty due to randomness.

# MODELING A RANDOM VARIABLE
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)
# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


# # SAMPLING MODELS

# KEY POINTS
# A sampling model models the random behavior of a process as the sampling of draws from an urn.
# The probability distribution of a random variable is the probability of the observed value falling in any given interval.
# We can define a CDF  𝐹(𝑎)=Pr(𝑆≤𝑎)  to answer questions related to the probability of S being in any interval.
# The average of many draws of a random variable is called its expected value.
# The standard deviation of many draws of a random variable is called its standard error.

# MONTE CARLO SIMULATION: CHANCE OF CASINO LOSING MONEY ON ROULETTE
#build a sampling model for the random variable  𝑆  that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)    # 1000 draws from urn, -1 if red, else +1
X[1:10]    # first 10 outcomes

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 roulette spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

# We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


# # DISTRIBUTION VS PROBABILITY DISTRIBUTIONS
# KEY POINTS
# A random variable  𝑋  has a probability distribution function  𝐹(𝑎)  that defines  Pr(𝑋≤𝑎)  over all values of  𝑎 .
# Any list of numbers has a distribution. The probability distribution function of a random variable is defined mathematically and does not depend on a list of numbers.
# The results of a Monte Carlo simulation with a large enough number of observations will approximate the probability distribution of  𝑋 .

# If a random variable is defined as draws from an urn:
# - The probability distribution function of the random variable is defined as the distribution of the list of values in the urn.
# - The expected value of the random variable is the average of values in the urn.
# - The standard error of one draw of the random variable is the standard deviation of the values of the urn.


# # NOTATION FOR RANDOM VARIABLES
# KEY POINTS
# Capital letters denote random variables ( 𝑋 ) and lowercase letters denote observed values ( 𝑥 ).
# In the notation  Pr(𝑋=𝑥) , we are asking how frequently the random variable  𝑋  is equal to the value  𝑥 . For example, if  𝑥=6 , this statement becomes  Pr(𝑋=6).


# # CENTRAL LIMIT THEOREM

# KEY POINTS
# The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.
# The expected value of a random variable,  E[𝑋]=𝜇 , is the average of the values in the urn. This represents the expectation of one draw. 
# The standard error of one draw of a random variable is the standard deviation of the values in the urn.
# The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
# The standard error of the sum of independent draws of a random variable is the square root of the number of draws times the standard deviation of the urn. 


# # # SECTION 3.2: CENTRAL LIMIT THEOREM

# # AVERAGES & PROPORTIONS

# KEY POINTS
# Random variable times a constant
# - The expected value of a random variable multiplied by a constant is that constant times its original expected value: E[𝑎𝑋]=𝑎𝜇 
# - The standard error of a random variable multiplied by a constant is that constant times its original standard error: SE[𝑎𝑋]=𝑎𝜎 
# Average of multiple draws of a random variable
# - The expected value of average of multiple draws from an urn is the expected value of the urn ( 𝜇 ).
# - The standard deviation of the average of multiple draws from an urn is the standard deviation of the urn divided by the square root of the number of draws ( 𝜎/𝑛√ ).
# The sum of multiple draws of a random variable
# - The expected value of the sum of  𝑛  draws of random variable is  𝑛  times its original expected value: E[𝑛𝑋]=𝑛𝜇 
# - The standard error of the sum of  𝑛  draws of random variable is  𝑛√  times its original standard error: SE[𝑛𝑋]=𝑛√𝜎 
# The sum of multiple different random variables
# - The expected value of the sum of different random variables is the sum of the individual expected values for each random variable: E[𝑋1+𝑋2+⋯+𝑋𝑛]=𝜇1+𝜇2+⋯+𝜇𝑛 
# - The standard error of the sum of different random variables is the square root of the sum of squares of the individual standard errors: SE[𝑋1+𝑋2+⋯+𝑋𝑛]=√‾‾ 𝜎21+𝜎22+⋯+𝜎2𝑛‾‾
# Transformation of random variables
# - If  𝑋  is a normally distributed random variable and  𝑎  and  𝑏  are non-random constants, then  𝑎𝑋+𝑏  is also a normally distributed random variable.


# # LAW OF LARGE NUMBERS

# KEY POINTS
# The law of large numbers states that as  𝑛  increases, the standard error of the average of a random variable decreases. In other words, when  𝑛  is large, the average of the draws converges to the average of the urn.
# The law of large numbers is also known as the law of averages.
# The law of averages only applies when  𝑛  is very large and events are independent. It is often misused to make predictions about an event being "due" because it has happened less frequently than expected in a small sample size.


# # HOW LARGE IS LARGE IN CLT?

# KEY POINTS
# The sample size required for the Central Limit Theorem and Law of Large Numbers to apply differs based on the probability of success.
# - If the probability of success is high, then relatively few observations are needed.
# - As the probability of success decreases, more observations are needed.
# If the probability of success is extremely low, such as winning a lottery, then the Central Limit Theorem may not apply even with extremely large sample sizes. The normal distribution is not a good approximation in these cases, and other distributions such as the Poisson distribution (not discussed in these courses) may be more appropriate.