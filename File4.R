---
  title: 'Day 5: Optimization Algorithms'
author: "Nicola Love" + "Pierre Rogy" + "Elodie Bebronne"
date: "20/08/2021"
output: html_document
---
  # Brute force
  ## Grid Search
  ```{r}
X <- runif(n = 10, min = 0, max = 10)
Y <- rnorm(n = 10, mean = 0.5 + 1.2*X, sd = 1.5)
ll <- function(X,Y,a,b, sd) {
  pred <- a + b*X
  sum(dnorm(Y, pred, sd, log = TRUE))
}
a_seq <- seq(-5,5,length.out = 1000)
b_seq <- seq(0,5,length.out = 1000)
grid <- expand.grid(a_seq,b_seq)
res <- numeric(1000*1000)
for(i in 1:nrow(grid)) 
  res[i] <- ll(X,Y,grid[i,1],grid[i,2],sd=2)
z <- matrix(res, nr = 1000, nc = 1000)
which.max(z) # 248502
z[248502]
contour(a_seq,b_seq,z, xlab = "Intercept", 
        ylab = "Slope",nlevels=50)
```
### Exercise 3.1
```{r}
eq <- function(x, y){
  h = (x*sin(20*y)+y*sin(20*x))^2*
    cosh(sin(10*x)*x)+(x*cos(10*y)-y*sin(10*x))^2*cosh(cos(20*y)*y)
}
runs <- 10
xseq <- seq(0, 100, length.out = runs)
yseq <- seq(0, 100, length.out = runs)
egrid <- expand.grid(xseq, yseq)
result <- numeric(runs*runs)
for(i in 1:nrow(grid)){
  result[i] <- eq(egrid[i,1], egrid[i,2])
}
```
# Simulated Annealing
## Exercise 2.2
DEFINE function to optimize h(X) -> temp?
  DEFINE the sampling function c(X) -> random walk
DEFINE temperature sequence -> what

REPEAT
DRAW sample X from c(x)
COMPUTE difference diff = h(X) - h(X_0)
IF diff > 0 ACCEPT X
ELSE 
COMPUTE acceptance probability p = exp(diff/T)
DRAW value P from random uniform on (0,1)
IF P < p
ACCEPT X
ELSE
REJECT X
UPDATE temperature
UNTIL nsteps is reached

DEFINE function to optimize h(X) -> temp
```{r}
#curve(exp(-3/x), xlim = c(0, 6))
fake_xs <- rnorm(42, mean = 19, sd = 4)
loglike_numbers <- function(numbers, mu){
  sum(dnorm(x = numbers, mean = mu, sd = 4, log = TRUE))
}
loglike_numbers(fake_xs, 7)
```
DEFINE the sampling function c(X) -> random walk
```{r}
# drunkards walk
c_ofx <- function(x, sigma) {
  x + rnorm(1, sd = sigma)
}
x0 <- 30
#check
xx <- numeric(40)
x0 <- 30
for(i in 2:40){
  xx[i] <- c_ofx(xx[i-1], sigma = 1)
}
plot(xx, type = "l")
```
DEFINE temperature sequence: negative exponential shape , initial temp and rate and which temp decreases
This is our prior distribution
```{r}
temp0 <- 30
temp_line <- function(runs, tstart, tcool){
  exp(log(tstart)+log(tcool)*0:(runs-1))
}
```
REPEAT
DRAW sample X from c(x) 
COMPUTE difference diff = h(X) - h(X_0)
IF diff > 0 ACCEPT X
ELSE 
COMPUTE acceptance probability p = exp(diff/Tt)
DRAW value P from random uniform on (0,1)
IF P < p
ACCEPT X
ELSE
REJECT X
UPDATE temperature
UNTIL nsteps is reached

```{r}
nsteps <- 100
diffvec <- numeric(nsteps)
history <- numeric(nsteps)
x0 <- 30
temp0 <- 30
mean0 <- 10
mytemps <- temp_line(runs = nsteps, temp0, .9)
for(i in 2:nsteps){
  X <- c_ofx(x0, sigma = 4)
  pot_diffvec<- loglike_numbers(fake_xs, X) - loglike_numbers(fake_xs, x0) #COMPUTE difference diff = h(X) - h(X_0)
  if(pot_diffvec > 0){
    diffvec[i] <- pot_diffvec
    x0 <- X
  } else if(runif(1, 0, 1) < exp(pot_diffvec/mytemps[i])) {
    diffvec[i] <- pot_diffvec
    x0 <- X
  } 
  history[i] <- x0
}
plot(history)
```
