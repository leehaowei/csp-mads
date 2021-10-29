library(rethinking)

# Constant
p_grid <- seq( from=0, to=1, length.out=1000 )
prior <- rep( 1 , 1000 )

# Problem 1
likelihood <- dbinom( 6 , size=9 , prob=p_grid ) 
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# sampling
set.seed(215)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# 1-a
sum( samples < 0.2 ) / 1e4

# 1-b
sum( samples > 0.8 ) / 1e4

# 1-c
sum( samples > 0.2 & samples < 0.8 ) / 1e4

# 1-d
quantile( samples , 0.2 )


# Problem 2
# Put graphs in 1 row and 2 columns
par(mfrow = c(2, 2))

# 2-a
prior_a <- rep( 1 , 1000 )
prob_data <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- prob_data * prior_a
posterior <- posterior / sum(posterior)

plot( p_grid , prior_a)
mtext( "flat prior" )
plot( p_grid , posterior , type="l" , 
      xlab="proportion water" , ylab="posterior probability" )
mtext( "posterior distribution a" )


# 2-b
prior_b <- ifelse( p_grid < 0.5 , 0 , 1 )
prob_data_b <- dbinom( 8 , size=15 , prob=p_grid )
posterior_b <- prob_data_b * prior_b
posterior_b <- posterior_b / sum(posterior_b)

plot( p_grid , prior_b)
mtext( "1 if p>0.5 else 0" )
plot( p_grid , posterior_b , type="l" , 
      xlab="proportion water" , ylab="posterior probability" )
mtext( "posterior distribution b" )


# Problem 3
# define a function compute_pi_width 
# that calculates the difference between the upper and lower bound of a interval
# variables (N: number of data, true_p: true value of p)
# return the difference and the lower and upper bound

compute_pi_width <- function(N, true_p) {
  likelihood <- dbinom( round(N*true_p), size=N, prob=p_grid ) 
  posterior <- likelihood * prior
  posterior <- posterior / sum(posterior)
  samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE )
  interval <- PI(samples, prob=0.99) # specify the interval to contains 99% mass
  dif <- diff( interval )         # difference between the upper and lower bound
  names(dif) <- "difference" 
  return (c(dif,interval))
}

true_p <- 0.7  # set the true p to 0.7 as the example in the book

number_of_data = 1:2000
diff_bounds = c()

for (val in number_of_data)
{
  N <- val
  ans <- compute_pi_width(N, true_p)

  diff_bounds <- c(diff_bounds, ans[1])
}

plot(number_of_data, diff_bounds, cex = 0.09)
mtext( "mass of 99%")

compute_pi_width(2000, true_p)
