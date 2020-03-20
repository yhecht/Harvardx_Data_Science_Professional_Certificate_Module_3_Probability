# Yannique Hecht
# Harvardx: PH125.3 - (3) Data Science: Probability
# SECTION 1: DISCRETE PROBABILITY
# CLASS NOTES


# # # SECTION 1.1: INTRODUCTION TO DISCRETE PROBABILITY

# # DISCRETE PROBABILITY

# Pr(𝐴)=probability of event A


# # MONTE CARLO DISTRIBUTIONS

# THE REP FUNCTION & SAMPLE FUNCTION
# sample function draws random outcomes from a set of options.
# replicate function repeats lines of code a set number of times. It is used with sample and similar functions to run Monte Carlo simulations.
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object

sample(beads, 1)    # sample 1 bead at random

# MONTE CARLO SIMULATION
B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions


# # SETTING THE RANDOM SEED

set.seed(1986) 

# learn more about setting the seed by looking at the documentation:
?set.seed

# revert to the original seed setting behavior by adding the argument sample.kind="Rounding". For example:
set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

# Using the sample.kind="Rounding" argument will generate a message:
non-uniform 'Rounding' sampler used


# # USING THE MEAN FUNCTION FOR PROBABILITY

# Suppose you have the vector beads from a previous video:
beads <- rep(c("red", "blue"), times = c(2,3))
beads
[1] "red" "red" "blue" "blue" "blue"
# To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")
[1] 0.6


# # PROBABILITY DISTRIBUTIONS


# # INDEPENDENCE

# Conditional probabilities compute the probability that an event occurs given information about dependent events. For example, the probability of drawing a second king given that the first draw is a king is:
# Pr(Card 2 is a king∣Card 1 is a king)=3/51
# If two events  𝐴  and  𝐵  are independent,  Pr(𝐴∣𝐵)=Pr(𝐴) .

# EQUATIONS
# The multiplication rule for independent events is:
# Pr(𝐴 and 𝐵 and 𝐶)=Pr(𝐴)×Pr(𝐵)×Pr(𝐶) 
# The multiplication rule for dependent events considers the conditional probability of both events occurring:
# Pr(𝐴 and 𝐵)=Pr(𝐴)×Pr(𝐵∣𝐴) 
# We can expand the multiplication rule for dependent events to more than 2 events:
# Pr(𝐴 and 𝐵 and 𝐶)=Pr(𝐴)×Pr(𝐵∣𝐴)×Pr(𝐶∣𝐴 and 𝐵)



# # # SECTION 1.2: COMBINATIONS & PERMUTATIONS

# # COMBINATIONS & PERMUTATIONS

# KEY POINTS
# paste joins two strings and inserts a space in between.
# expand.grid gives the combinations of 2 vectors or lists.
# permutations(n,r) from the gtools package lists the different ways that r items can be selected from a set of n options when order matters.
# combinations(n,r) from the gtools package lists the different ways that r items can be selected from a set of n options when order does not matter.

# INTRODUCING PASTE & EXPAND.GRID
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# GENERATING A DECK OF CARDS
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

# PERMUTATIONS & COMBINATIONS
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]
permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# PROBABILITY OF DRAWING A SECOND KING GIVEN THAT ONE KING IS DRAWN
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# PROBABILITY OF A NATURAL 21 IN BLACKJACK
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v=deck) # all possible hands
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# MONTE CARLO SIMULATION OF NATURAL 21 IN BLACKJACK
# code for one hand of blackjack
hand <- sample(deck, 2)
hand
# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


# # THE BIRTHDAY PROBLEM

# KEY POINTS
# duplicated takes a vector and returns a vector of the same length with TRUE for any elements that have appeared previously in that vector.

# BIRTHDAY PROBLEM

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


# # SAPPLY

# KEY POINTS
# Some functions automatically apply element-wise to vectors, such as sqrt and *.
# However, other functions do not operate element-wise by default. This includes functions we define ourselves.
# The function sapply(x, f) allows any other function f to be applied element-wise to the vector x.
# The probability of an event happening is 1 minus the probability of that event not happening: 
# Pr(event)=1−Pr(no event) 
# e can compute the probability of shared birthdays mathematically:
# Pr(shared birthdays)=1−Pr(no shared birthdays)=1−(1×364365×363365×...×365−𝑛+1365)

# FUNCTION FOR CALCULATING BIRTHDAY PROBLEM MONTE CARLO SIMULATIONS FOR ANY VALUE OF N
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace = TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}

n <- seq(1, 60)

# ELEMENT-WISE OPERATION OVER VECTORS AND SAPPLY
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# COMPUTING BIRTHDAY PROBLEM PROBABILITIES WITH SAPPLY
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
    prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
    1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


# # HOW MANY MONTE CARLO EXPERIMENTS ARE ENOUGH?

# KEY POINTS
# The larger the number of Monte Carlo replicates  𝐵 , the more accurate the estimate.
# Determining the appropriate size for  𝐵  can require advanced statistics.
# One practical approach is to try many sizes for  𝐵  and look for sizes that provide stable estimates.

# ESTIMATING A PRACTICAL VALUE OF B
# This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. When B is large enough that the estimated probability stays stable, then we have selected a useful value of B. 

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace = TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}
prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 



# # # SECTION 1.3: ADDITION RULE & MONTY HALL

# # THE ADDITION RULE

# KEY POINTS
# The addition rule states that the probability of event  𝐴  or event  𝐵  happening is the probability of event  𝐴  plus the probability of event  𝐵  minus the probability of both events  𝐴  and  𝐵  happening together.
# Pr(𝐴 or 𝐵)=Pr(𝐴)+Pr(𝐵)−Pr(𝐴 and 𝐵) 
# Note that  (𝐴 or 𝐵)  is equivalent to  (𝐴|𝐵)


# # THE MONTY HALL PROBLEM

# KEY POINTS
# Monte Carlo simulations can be used to simulate random outcomes, which makes them useful when exploring ambiguous or less intuitive problems like the Monty Hall problem.
# In the Monty Hall problem, contestants choose one of three doors that may contain a prize. Then, one of the doors that was not chosen by the contestant and does not contain a prize is revealed. The contestant can then choose whether to stick with the original choice or switch to the remaining unopened door.
# Although it may seem intuitively like the contestant has a 1 in 2 chance of winning regardless of whether they stick or switch, Monte Carlo simulations demonstrate that the actual probability of winning is 1 in 3 with the stick strategy and 2 in 3 with the switch strategy.

# MONTE CARLO SIMULATION OF STICK STRATEGY
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# MONTE CARLO SIMULATION OF SWITCH STRATEGY
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching






