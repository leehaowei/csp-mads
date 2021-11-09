# import packages and read in the Howell1 data
library(rethinking)
data(Howell1)
d <- Howell1

# PS2-1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight)

# model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) , 
    mu <- a + b*( weight - xbar ),  
    a ~ dnorm( 178 , 20 ),
    b ~ dlnorm( 0 , 1 ),
    sigma ~ dunif( 0 , 50 ) 
  ), data=d2 )

weights = c(46, 61, 35, 52, 56)            # define target weights

# use link function to link a model to calculate distribution of mu
mu <- link(m4.3, 
           data=data.frame(weight=weights))# mu is a N x m matrix (1000 x 5)
mu.mean <- apply(mu, 2, mean)           
mu.PI <- apply(mu, 2 ,PI, prob=0.89)       # set the credibility interval to 89% 

# Assignm variables to create a dataframe
Individual <- c(1,2,3,4,5)
expected_height = mu.mean
lower_bound_5 <- mu.PI[1,]   # first column in PI is the lower bound
upper_bound_94 <- mu.PI[2,]  # second column in PI is the upper bound 

# create and print out the dataframe
df <- data.frame(Individual, weights, expected_height, 
                 lower_bound_5, upper_bound_94)
print (df)



# PS2-2a
# standardize weight and weight^2
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2

# polynomial model from Chapter 4
poly_model <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ), data=d )

# define a function to plot n lines
plot_curves <- function(a, b1, b2, n) 
{
  # set the limitation of the figure
  plot( NULL, xlim=range(d$weight), ylim=c(-100, 400),
        xlab="weight", ylab="height")
  
  # plot n curve lines 
  for (i in 1:n) curve( a[i] + b1[i] * x + b2[i] * x^2,
                        from=min(d$weight), 
                        to=max(d$weight), 
                        add  = TRUE,
                        col=col.alpha("black", 0.2))
}

set.seed(10)

# extract priors from the polynomial model
priors <- extract.prior( poly_model )
a <- priors$a
b1 <- priors$b1
b2 <- priors$b2
sigma <- priors$sigma
plot_curves(a, b1, b2, 100)


# PS2-2b-1
N <- 1000
b2 <- rexp(N, 20)
plot_curves(a, b1, b2, 100)


# PS2-2b-2
n <- 1000
a = rnorm(n, -50, 3)
b1 = rnorm(n, 11, 0.18)
b2 = runif(n, -0.12, -0.1)
plot_curves(a, b1, b2, 100)



# PS2-3a
library(dagitty)

mad_dag <- dagitty("dag{ M -> A -> D}")
impliedConditionalIndependencies(mad_dag)