
# installations -----------------------------------------------------------

# run once


install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
cmdstanr::install_cmdstan()
devtools::install_github("rmcelreath/rethinking")


# start
glmsummary::install_load_packages(c(
  "coda",
  "mvtnorm",
  "devtools",
  "loo",
  "dagitty",
  "shape",
  "cmdstanr",
  "rethinking"
))

p_grid <- seq(0, 1, length.out = 1000)
# a flat prior?
prob_p <- rep(1, 1000)
# likelihood
prob_data <- dbinom(6, size = 9, prob = p_grid)
# posterior
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
sapply(list(p_grid, prob_p, prob_data, posterior), plot)
# sample from posterior pedictive distribution
s <- sample(p_grid, size=1e4, prob = posterior, replace = TRUE)
w <- dbinom(1e4, 9, prob = s)
plot(w)


# chap3: Linear regression ------------------------------------------------

# data generating mechanism: H ---> W
alpha <- 0; beta <- 0.5; sigma <- 5; n <- 100
# generate heights
h <- runif(n, 130, 170)
mu <- alpha + beta * h
# generate weight
w <- rnorm(n, mu, sigma)

plot(h, w)

#  anatomy of linear models

# yi ~ Normal(ui, d); ui ~ ai + BXi

# priors:


#  a ~ Normal(0, 1)
# B ~ Normal(0, 1)
# d ~ uniform(0, 1): sigmas are scale parameters and are positive. you need 
# to give them a positive distribution as well

# sampling from the prior distribution

n_samples <- 10

alpha <- rnorm(n_samples, 0, 1)
beta <- rnorm(n_samples, 0, 1)


plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2),
     xlab = "x", ylab = "y")
for(i in seq(n_samples)){
  abline(alpha[i], beta[i], lwd = 4, col = 2)
}    


# statistical  model for H -> W

# Wi ~ Normal(ui, d)
# ui ~ a + B(Hi - H_bar): changes interpretation of intercept to average weight
# when an individual is of average height of the population

# alpha: average adult weight
# B: kilograms per centimeter

# priors:

# a ~ Normal(60, 10)
# B ~ Normal(0, 10)
# sigma ~ Uniform(0, 10)


# sampled regression lines

n <- 10; alpha <- rnorm(n, 60, 10); beta <- rnorm(n, 0, 10)

Hbar <- 150
Hseq <- seq(from = 130, to = 170, len = 30)

plot(NULL, xlim = c(130, 170), ylim = c(10, 100),
     xlab = "height (cm)",  ylab = "weight (kg)")
for (i in seq(n)) {
  lines(Hseq, alpha[i] + beta[i] * (Hseq - Hbar),
        lwd =3, col =2)
}

# a lot of the lines go in wrong directions coz we made prior for 
# beta centered around 0
#  on average relationship between height and weight is positive,
# so we use a positive distribution for prior of beta

# beta <- lognormal (0, 1)

n <- 10; alpha <- rnorm(n, 60, 10); beta <- rlnorm(n, 0, 1)

Hbar <- 150
Hseq <- seq(from = 130, to = 170, len = 30)

plot(NULL, xlim = c(130, 170), ylim = c(10, 100),
     xlab = "height (cm)",  ylab = "weight (kg)")
for (i in seq(n)) {
  lines(Hseq, alpha[i] + beta[i] * (Hseq - Hbar),
        lwd =3, col =2)
}

# Linear regression Owl:
# 1). Question/goal/estimand
# 2). Scientific model
# 3). Statistical model(s)
# 4). Validate model
# 5). Analyze data


# Model formula

data("Howell1")
# a <- dnorm(60, 10); b <- dlnorm(0, 1); sigma <- dunif(0, 10)
# mu <- a + b*(Hseq - Hbar)
# w <- dnorm(mu, sigma)

# Simulation-Based Calibration is how you verify that numerical algorithms are
# working as expected in a Bayesian context



# first validate the simulation -------------------------------------------

alpha <- 70; beta <-5; sigma <- 5; n_indiv <- 100
H <- runif(n_indiv, 130, 170)
mu <- alpha + beta * (H - mean(H))
W <- rnorm(n_indiv, mu, sigma)

dat <- list(H = H, W = W, Hbar = mean(H))


m_validate <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b *(H - Hbar),
    a ~ dnorm(60, 10),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dat
)

# summary
precis(m_validate)


# real data

d <- Howell1[Howell1$age >= 18, ]

dat <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height)
)

m_adults <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b * (H - Hbar),
    a ~ dnorm(60, 10),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dat
)
precis(m_adults, corr = TRUE)



# posterior predictive distribution ---------------------------------------

# the distributionn of possible unobserved values conditional on observed values

# 1). plot the sample
# 2). plot the posterior line - most plausible line
# 3). plot uncertainty of the mean
# 4). plot uncertainty of predictions


#1  plot sample
#  see section 4.4.3 from page 98 in book

cols2 <- col.alpha(2, 0.8)
plot(d$height, d$weight, col = cols2, lwd = 3,
     cex = 1.2, xlab = "height (cm)", ylab = "weight (kg)")

#2. plot the expectation with 99% compatibility interval

xseq <- seq(from = 130, to = 190, len = 50)
mu <- link(m_adults, data = list(H=xseq, Hbar = mean(d$height)))
lines(xseq, apply(mu, 2, mean), lwd =4)
shade(apply(mu, 2, PI, prob = 0.99), xseq,
      col = col.alpha(2, 0.5))
# 89% prediction interval using sim
# sim simulates weights as though from a generative model
W_sim <- sim(m_adults, data = list(H = xseq, Hbar = mean(d$height)))
shade(apply(W_sim, 2, PI, prob = 0.89), xseq, col = col.alpha(1, 0.3))
