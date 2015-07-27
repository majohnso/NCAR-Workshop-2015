// rat tumor example
// Carpenter et al. (20XX)


data {
  int<lower=0> J;    // number of experiments
  int<lower=0> y[J]; // numbers of tumors
  int<lower=0> n[J]; // numbers of rats
}
parameters {
  real<lower=0,upper=1> theta[J]; // tumor probabilities
  real<lower=0,upper=1> lambda;   // prior mean of tumor probabilities
  real<lower=0.1> kappa;          // prior count
}
transformed parameters {
  real<lower=0> alpha;   // prior tumor count
  real<lower=0> beta;    // prior non-tumor count
  alpha <- lambda * kappa;
  beta <- (1 - lambda) * kappa;
}
model {
  lambda ~ uniform(0,1);    // hyperprior
  kappa ~ pareto(0.1,1.5);  // hyperprior
  theta ~ beta(alpha,beta); // prior
  y ~ binomial(n,theta);    // likelihood
}
generated quantities {
  real<lower=0,upper=1> avg; // avg tumor probability
  avg <- mean(theta);
}