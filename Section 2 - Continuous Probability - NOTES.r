# Yannique Hecht
# Harvardx: PH125.3 - (3) Data Science: Probability
# SECTION 2: CONTINUOUS PROBABILITY
# CLASS NOTES


# # # SECTION 2.1: CONTINUOUS PROBABILITY

# # CONTINUOUS PROBABILITY

# KEY POINTS
# The cumulative distribution function (CDF) is a distribution function for continuous data  𝑥  that reports the proportion of the data below  𝑎  for all values of  𝑎 :
# 𝐹(𝑎)=Pr(𝑥≤𝑎) 
#The CDF is the probability distribution function for continuous variables. For example, to determine the probability that a male student is taller than 70.5 inches given a vector of male heights  𝑥 , we can use the CDF:
Pr(𝑥>70.5)=1−Pr(𝑥≤70.5)=1−𝐹(70.5) 
#The probability that an observation is in between two values  𝑎,𝑏  is  𝐹(𝑏)−𝐹(𝑎) .

# CUMULATIVE DISTRIBUTION FUNCTION
# Define x as male heights from the dslabs dataset:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches


# # THEORETICAL DISTRIBTUION

# KEY POINTS
# pnorm(a, avg, s) gives the value of the cumulative distribution function  𝐹(𝑎)  for the normal distribution defined by average avg and standard deviation s.
# We say that a random quantity is normally distributed with average avg and standard deviation s if the approximation pnorm(a, avg, s) holds for all values of a.
# If we are willing to use the normal approximation for height, we can estimate the distribution simply from the mean and standard deviation of our values.
# If we treat the height data as discrete rather than categorical, we see that the data are not very useful because integer values are more common than expected due to rounding. This is called discretization.
# With rounded data, the normal approximation is particularly useful when computing probabilities of intervals of length 1 that include exactly one integer.

# USING PNORM TO CALCULATE PROBABILITIES

# Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# We can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))

# DISCRETIZATION AND THE NORMAL APPROXIMATION
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# # PROBABILITY DENSITY

# KEY POINTS
# The probability of a single value is not defined for a continuous distribution.
# The quantity with the most similar interpretation to the probability of a single value is the probability density function  𝑓(𝑥) .
# The probability density  𝑓(𝑥)  is defined such that the integral of  𝑓(𝑥)  over a range gives the CDF of that range.
# 𝐹(𝑎)=Pr(𝑋≤𝑎)=∫𝑎−∞𝑓(𝑥)𝑑𝑥 
# In R, the probability density function for the normal distribution is given by dnorm. We will see uses of dnorm in the future.
# Note that dnorm gives the density function and pnorm gives the distribution function, which is the integral of the density function.


# # MONTE CARLO SIMULATIONS
# KEY POINTS
# rnorm(n, avg, s) generates n random numbers from the normal distribution with average avg and standard deviation s.
# By generating random numbers from the normal distribution, we can simulate height data with similar properties to our dataset. Here we generate simulated height data using the normal distribution.

# GENERATING NORMALLY DISTRIBUTED RANDOM NUMBERS FOR MONTE CARLO SIMULATIONS
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
    ggplot(aes(simulated_heights)) +
    geom_histogram(color="black", binwidth = 2)

# MONTE CARLO SIMULATION OF PROBABILITY OF TALLEST PERSON BEING OVER 7 FEET
B <- 10000
tallest <- replicate(B, {
    simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
    max(simulated_data)    # determine the tallest height 
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)


# # OTHER CONTINUOUS DISTRIBUTIONS

# KEY POINTS
# You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
# R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for many of these distributions.
# Each distribution has a matching abbreviation (for example, norm or t) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
# For example, use rt to generate random numbers for a Monte Carlo simulation using the Student t distribution.

# PLOTTING THE NORMAL DISTRIBUTION WITH DNORM
# Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm):
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
    ggplot(aes(x,f)) +
    geom_line()