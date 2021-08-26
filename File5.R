# 1. Write a function to generate a random matrix for pre-specified S and C

S <- 50
C <- 0.1
sigma = 0.5



creatematrix <- function(S, C, sigma=0.5){
  L <- matrix(0, nr = S, nc = S)
  int <- matrix(rnorm(S^2, mean = 0, 
                      sd = sigma), nr = S, nc = S)
  rand <- matrix(runif(S^2,0,1),
                 nr=S,nc=S)
  L[rand<C] = int[rand<C]
  return(L)
}
creatematrix(20,0.1)

# 2. Write a function to compute the real part of the largest eigen value of a matrix L

eigen(creatematrix(S=20,C=0.1),symmetric = FALSE, only.values = TRUE)
realpart <- function (matrix){
  eigenvalues <- eigen(matrix, symmetric = FALSE, only.values = TRUE)$values
  real<- Re(eigenvalues)
  maxeigen <- max(real)
  return(maxeigen)
}
matrix <- creatematrix(S=20,C=0.1) 
realpart(creatematrix(S=20,C=0.1))


# 3. Write a function to perform simulated annealing with a matrix L as input and returning an optimized matrix Lopt as output

# Set conditions for the simulated annealing sequence
T_fn <- function(T0, alpha, step) T0*exp(alpha*step)

T0 <- 10
alpha = -0.001
nsteps <- 10000

# Sampling function : c of x
c_x <- function(x, sigma) {
  x + rnorm(1, sd = sigma)
}
x0 <- 30
#check
xx <- numeric(40)
x0 <- 30
for(i in 2:40){
  xx[i] <- c_x(xx[i-1], sigma = 1)
}
plot(xx, type = "l")


# Function to optimize : h of x : Input L creatematrix: see l 9-18


# Prepare an object to store the result: Output L OPT
Lopt <- matrix(nr = nsteps, nc = 5)

# Initiate the algorithm
a0 <- 1
s0 <- 1000
sd0 <- 10
pars0 <- c(1, 100, 10)
pars_lo <- c(0, 1, 1)
pars_hi <- c(1000, 1000, 100)

# Main loop
for(step in 1:nsteps) {
  
  for(j in 1:3) {
    
    # Draw new value for parameter j
    pars1 <- pars0
    pars1[j] <- c_x(pars_lo[j], pars_hi[j])
    
    # Evaluate the function
    h1 <- h(obs, L, pars1)
    h0 <- h(obs, L, pars0)
    
    # Compute the difference
    diff <- h1 - h0
    
    # Accept if improvement
    if(diff > 0) pars0 <- pars1
    
    # Accept wrong candidates 
    else {  
      p <- exp(diff/T_fn(T0, alpha, step))
      if(runif(1)<p) pars0 <- pars1
    }
  }
  
  # Record values
  Lopt[step,] <- c(step,pars0,h(obs,L,pars0)) 
}

#Plot the results
plot(c(1:nsteps), Lopt[,5], type = "l", xlab = "Time step", ylab = "h(x)",cex = 2, log = "x")

# 4. Run the optimization procedure for a gradient of species richness and record L and Lopt
# 5. Evaluate if the "potential for stability" ($L-L_{opt}$) relates to complexity

