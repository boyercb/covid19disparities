data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] x;
  int deaths[N];
  vector[N] off;
}

parameters {
  vector[K] beta;
}

transformed parameters {
  vector[K] mu;
  mu = exp(x * beta);
}

model {
  beta ~ normal(0, 1);
  deaths ~ poisson(mu + off);
}
