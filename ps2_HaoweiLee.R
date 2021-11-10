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
lower_bound_5.5 <- mu.PI[1,]   # first row in PI is the lower bound
upper_bound_94.5 <- mu.PI[2,]  # second row in PI is the upper bound 

# create and print out the dataframe
df <- data.frame(Individual, weights, expected_height, 
                 lower_bound_5.5, upper_bound_94.5)
print (df)


# PS2-2a
# standardize weight and weight^2
xsd <- sd(d2$weight)
d2$weight_s <- ( d2$weight - mean(d2$weight) )/xsd
d2$weight_s2 <- d2$weight_s^2


# polynomial model from Chapter 4
poly_model <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ), data=d2 )


# define a function to plot n lines for Standerdised
plot_poly_sd <- function(a, b1, b2, n) 
{
  # plot the figure and customize settings
  plot( NULL, xlim=range(d2$weight), ylim=c(0, 300),
        xlab="weight", ylab="height")
  
  # plot n curve lines 
  for (i in 1:n) curve( a[i] + b1[i] * (x-xbar)/xsd + b2[i] * ((x-xbar)/xsd)^2,
                        from=min(d2$weight), 
                        to=max(d2$weight), 
                        add  = TRUE,
                        col=rangi2)
}

# define a function to plot n lines for Unstanderdised
plot_poly_unsd <- function(a, b1, b2, n) 
{
  # plot the figure and customize settings
  plot( NULL, xlim=range(d2$weight), ylim=c(0, 300),
        xlab="weight", ylab="height")
  
  # plot n curve lines 
  for (i in 1:n) curve( a[i] + b1[i] * x + b2[i] * x^2,
                        from=min(d2$weight), 
                        to=max(d2$weight), 
                        add  = TRUE,
                        col=rangi2)
}
set.seed(10)

# extract priors from the polynomial model
priors <- extract.prior( poly_model )
a <- priors$a           # a ~ dnorm( 178 , 20 )
b1 <- priors$b1         # b1 ~ dlnorm( 0 , 1 ) 
b2 <- priors$b2         # b2 ~ dnorm( 0 , 1 ) 
sigma <- priors$sigma   #   sigma ~ dunif( 0 , 50 )
plot_poly_sd(a, b1, b2, 50)
mtext( "Standarised" )

plot_poly_unsd(a, b1, b2, 50)
mtext( "Unstandarised" )


# PS2-2b-1 (first try)
N <- 1000   # number of samples
b2 <- rexp(N, 20)
plot_poly_sd(a, b1, b2, 50)
mtext( "Exponancial" )


# PS2-2b-2 (second try)
N <- 1000   # number of samples
a <- rnorm(N, 172, 18)
b1 <- rlnorm(N, 2.1, 0.3)
b2 <- rlnorm(N, 1, 0.05)
plot_poly_sd(a, b1, -b2, 50)   # add negetive to b2
mtext( "Final_Version" )


# Compare
par(mfrow = c(1, 2))
# left
priors <- extract.prior( poly_model )
a <- priors$a           # a ~ dnorm( 178 , 20 )
b1 <- priors$b1         # b1 ~ dlnorm( 0 , 1 ) 
b2 <- priors$b2         # b2 ~ dnorm( 0 , 1 ) 
sigma <- priors$sigma   #   sigma ~ dunif( 0 , 50 )
plot_poly_sd(a, b1, b2, 50)
mtext( "Original_Standarised" )

# right
N <- 1000   # number of samples
a <- rnorm(N, 172, 18)
b1 <- rlnorm(N, 2.1, 0.3)
b2 <- rlnorm(N, 1, 0.05)
plot_poly_sd(a, b1, -b2, 50)   # add negetive to b2
mtext( "Final_Version" )


# PS2-3a
library(dagitty)

mad_dag <- dagitty("dag{ M -> A -> D}")
impliedConditionalIndependencies(mad_dag)