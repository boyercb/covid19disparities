data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] x;
  int deaths[N];
  // int cases[N];
  vector[N] off;
}

parameters {
  vector[K] beta;
  real<lower=0> phi;
}

transformed parameters {
  vector[K] mu;
  mu = exp(x * beta);
}

model {
  beta ~ normal(0, 1);
  phi ~ cauchy(0, 1);
  deaths ~ neg_binomial_2(mu + off, 1/phi);
  // cases ~ poisson(mu + off);
  
}
