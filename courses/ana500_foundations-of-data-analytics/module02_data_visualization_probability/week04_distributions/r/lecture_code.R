##################################################################
## Week 4: Lecture code
##################################################################

# PMF: probability of EXACTLY 5 aces in 20 trials
#   x    = number of successes we're asking about
#   size = number of trials (n) in one experiment
#   prob = probability of success on a single trial (p)
prob = dbinom(x=5, size=20, prob=0.0769)
print(sprintf("probabiliy of pulling an ace 5 times out of 20 trials: %.2f%%", prob * 100))

# CDF: probability of 5 OR FEWER aces in 20 trials
#   q = the upper bound of the "less-than-or-equal" region
prob = pbinom(q=5, size=20, prob=0.0769)
print(sprintf("probabiliy of pulling an ace '5 times or less'  out of 20 trials: %.2f%%", prob * 100))

# ---------- Geometric PMF ----------
# dgeom(x = number of failures BEFORE the first success, prob = probability of success)
# For the first ace on the 7th trial, there are 6 failures before it.
dgeom(x = 6, prob = 0.0769)

# ---------- Geometric CDF ----------
# Probability of picking the first ace on any of the first 7 trials
pgeom(q = 6, prob = 0.0769)
# [1] 0.429...



# Example: Drawing 5 cards from a 52-card deck, without replacement. 
# What is the probability of getting exactly 3 aces?
# ---------- Hypergeometric PMF ----------
# dhyper(x = number of successes in sample,
#        m = number of successes in population, # number of aces in the deck in this case
#        n = number of failures in population, # number of non-aces in the deck
#        k = sample size) # size of the sample we have drawn, 5 cards
dhyper(x = 3, m = 4, n = 48, k = 5)
# [1] 0.0017...

# ---------- Hypergeometric CDF ----------
# Probability of getting 3 or fewer aces when selecting 5 cards
phyper(q = 3, m = 4, n = 48, k = 5)
# Almost certain — quite likely we would get 3 or fewer aces.


## Poisson Distribution: used to model the number of event occurs over a fixed interval of time or space
# mean is assumed to equal variance
## Example: Mean number of car accidents per hour in a city is 6. 
# What is the probability of 11 accidents in a given hour?
# ---------- Poisson PMF ----------
# dpois(x = number of occurrences, lambda = mean number of occurrences)
dpois(x = 11, lambda = 6)
# [1] 0.0225...

# ---------- Poisson CDF ----------
# Probability of 5 or fewer accidents in a given hour
ppois(q = 5, lambda = 6)
# [1] 0.446...
