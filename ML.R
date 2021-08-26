# VERY BASIC EXAMPLE - Max Likelihood

# First: minimizing sum of squares (tradi lin models)
ss <- function(X,u){
  res <- (X-u)*(X-u)
  return(sum(res))
}
X <- c(4,2,10,5,8,4)
ss(X,5)

ss(X,5.5)

# Then: max likelihood, assume normal distr of residuals

ll <- function(X,u,sd){
  sum(dnorm(X,u,sd, log=TRUE))
}
ll(X,5,1)
ll(X,5.5,2.95)

# Exercise: bees

X <- c(0,1,1,0,1,1,0,0,0,0)
loglik <- function(X,p){
  sum(dbern(X,p,log=TRUE))
}

library(purrr)
P <- seq(from = 0.01, to=0.99, by=0.01)
loglik_P <- P %>% map_dbl(loglik, X=X)
plot(P,loglik_P)
P_maxLL <- P(loglik_P == max(loglik_P))


# Exercise 2.2 maple distribution at Sutton