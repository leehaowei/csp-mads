# Import libraries
library(rethinking)
library(ggplot2)

# 1
# Import and inspect foxes data
data(foxes)
d <- foxes
precis(d)

# 1-a
# standardize the variables
d$w_s <- standardize(d$weight)
d$a_s <- standardize(d$area)

# build a linear model
modelA <- quap(
  alist(
    w_s ~ dnorm( mu , sigma ),  # weight
    mu <- a + b_area * a_s,
    a ~ dnorm(0,0.2),
    b_area ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )

precis(modelA)   # inspect the result

set.seed(2971)
N <- 500
priors <- extract.prior( modelA )
a <- priors$a # a ~ dnorm( 178 , 20 )
b_area <- priors$b_area # b1 ~ dlnorm( 0 , 1 )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) ,
      xlab="area" , ylab="weight" )
# mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d$w_s)
for ( i in 1:N ) curve( a[i] + b_area[i]*(x - xbar) ,
                        from=min(d$w_s) , to=max(d$w_s) , add=TRUE ,
                        col=col.alpha("black",0.2) )

# 1-b
d$f_s <- standardize(d$avgfood)
modelB <- quap(
  alist(
    w_s ~ dnorm( mu , sigma ),
    mu <- a + b_food * f_s,
    a ~ dnorm(0,0.2),
    b_food ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )

precis(modelB)   # inspect the result

# 1-c
d$g_s <- standardize(d$groupsize)
modelC <- quap(
  alist(
    w_s ~ dnorm( mu , sigma ),
    mu <- a + b_food * f_s + b_group * g_s,
    a ~ dnorm(0,0.2),
    b_food ~ dnorm(0,0.5),
    b_group ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d )

precis(modelC)   # inspect the result


# 3
# load the data
data(foxes)
d <- foxes
d$area <- scale(d$area)
d$avgfood <- scale(d$avgfood)
d$weight <- scale(d$weight)
d$groupsize <- scale(d$groupsize)

# Construct the models
# (1) avgfood + groupsize + area
m1 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood + bGroup * groupsize + bArea * area,
    a ~ dnorm(0, 0.2),
    c(bFood, bGroup, bArea) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (2) avgfood + groupsize
m2 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood + bGroup * groupsize,
    a ~ dnorm(0, 0.2),
    c(bFood, bGroup) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (3) avgfood + area
m3 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood + bArea * area,
    a ~ dnorm(0, 0.2),
    c(bFood, bArea) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (4) avgfood
m4 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood,
    a ~ dnorm(0, 0.2),
    bFood ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (5) area
m5 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bArea * area,
    a ~ dnorm(0, 0.2),
    bArea ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

compare(m1, m2, m3, m4, m5)
## Comparison plot
plot(compare(m1, m2, m3, m4, m5))
