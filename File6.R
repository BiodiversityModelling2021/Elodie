#DAY 7: Bayesian Stats

# Initial exercise: CHICK SURVIVAL - In-class grid search
# Step 0: Initial code

set.seed(1859)
x_obs <- rbinom(10, 6, 0.6)
x_obs

# Step 1: make a vector of probabilities for p using seq(from = 0, to = 1, length.out = n)

theta <- seq(from = 0.01, to = 0.99, by = 0.01)

# Step 2: Think about the prior you might use. Look at curve(dbeta(x)).

prior_unif <- dunif(theta, 0, 1)

pr_like <- sapply(X = theta, function(X))
  exp(sum(dbinom(x_obs, size = 6, prob = X, log = TRUE)))
  
pr_like <- sapply(X = theta, function(X))
  prod(dbinom(x_obs, size = 6, prob = X))
  
plot(theta,pr_lik)

numerator <- pr_like*prior_unif

denom <- sum(numerator)
posterior <- numerator/denom
sum(posterior)
plot(theta,posterior)
points(theta,prior_unif)

#NORMALIZING
xx <- 1:8
sum(xx/sum(xx))

  
  # size fixed : birds lie exactly 6 eggs
  # perform operation for prob x
  # add log = multiply proba
  # should be tiny number

curve(dbeta(x, 2, 2))
curve(dbeta(x, .3*10, (1-.3)*10))
curve(dbeta(x,2,2))

# Step 3: Find the probability of each of these values of p via dbeta

prior_dens <- dbeta(p,2,2)
plot(p, prior_dens)

# Step 4: find the likelihood for each of these values of p via dbinom

log_lik <- function(x)(sum(dbinom(x_obs, size = 6, prob = .5, log=TRUE)))

# Step 5: multiply these two columns together

pr_lik <- sapply(X = p_v, function(X) 
  exp(sum(dbinom(x_obs, size = 6, prob = X, log = T)))) 

prior_unif <- dunif(p_v, 0, 1)

par(mfrow = c(1,3))
plot(pr_lik, type = "p")
plot(prior_unif, type = "l")
plot(pr_lik*prior_unif, type = "b", col = "red")

prior_beta <- dbeta(p_v, 2, 2)
par(mfrow = c(1,3))
plot(pr_lik, type = "p")
plot(prior_beta, type = "l")
plot(pr_lik*prior_beta, type = "b", col = "red")


# Step 6: normalize (divide by the sum)


