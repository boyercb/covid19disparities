functions {
  // count number times elem appears in test set
  int count_elem(vector test, int elem) {
    int count;
    count = 0;
    for(i in 1:num_elements(test))
      if(test[i] == elem)
        count = count + 1;
    return(count);
  }
  
  // find elements in test which are equal to elem
  int[] which_elem(vector test, int elem) {
    int res[count_elem(test, elem)];
    int ci;
    ci = 1;
    for(i in 1:num_elements(test))
      if(test[i] == elem) {
        res[ci] = i;
        ci = ci + 1;
      }
    return(res);
  }
  
  real calculate_rr_rng(int[] num, matrix x, int[] outcome, vector coef, int idy, int idy_ref) {
    int N;
    int N_ref;
    int idx[count_elem(x[:, idy], 1)];
    int idx_ref[count_elem(x[:, idy_ref], 1)];
    int events_hat[count_elem(x[:, idy], 1)];
    int events_hat_ref[count_elem(x[:, idy_ref], 1)];
    real pr = 0;
    real pr_ref = 0;
    
    N = count_elem(x[:, idy], 1);
    N_ref = count_elem(x[:, idy_ref], 1);
    
    idx = which_elem(x[:, idy], 1);
    idx_ref = which_elem(x[:, idy_ref], 1);
    
    events_hat = binomial_rng(num[idx], inv_logit(x[idx] * coef));
    events_hat_ref = binomial_rng(num[idx_ref], inv_logit(x[idx_ref] * coef));
    
    for (i in 1:N) {
      pr += 1.0 * events_hat[i] / num[idx][i];
    }
    
    for (i in 1:N_ref) {
      pr_ref += 1.0 * events_hat_ref[i] / num[idx_ref][i];
    }
    
    return((pr / N) / (pr_ref / N_ref));
  }
}

data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] x;
  int deaths[N];
  int pop[N];
  int cases[N];
  int tests[N];
}

parameters {
  vector[K] beta;
  vector[K] gamma;
  vector[K] alpha;
}

transformed parameters {
  vector[K] p;
  vector[K] q;
  vector[K] r;
  p = inv_logit(x * gamma);
  q = inv_logit(x * beta);
  r = inv_logit(x * alpha);
}

model {
  beta ~ normal(0, 10);
  gamma ~ normal(0, 10);
  alpha ~ normal(0, 10);
  cases ~ binomial(pop, p);
  deaths ~ binomial(cases, q);
  tests ~ binomial(pop, r);
}

generated quantities {
  real<lower=0> rr_case_aian;
  real<lower=0> rr_test_aian;
  real<lower=0> rr_death_aian;
  real<lower=0> rr_case_asian;
  real<lower=0> rr_test_asian;
  real<lower=0> rr_death_asian;
  real<lower=0> rr_case_black;
  real<lower=0> rr_test_black;
  real<lower=0> rr_death_black;
  real<lower=0> rr_case_latinx;
  real<lower=0> rr_test_latinx;
  real<lower=0> rr_death_latinx;
  real<lower=0> rr_case_nhpi;
  real<lower=0> rr_test_nhpi;
  real<lower=0> rr_death_nhpi;
  
  rr_case_aian = calculate_rr_rng(pop, x, cases, gamma, 1, 6);
  rr_test_aian = calculate_rr_rng(pop, x, tests, alpha, 1, 6);
  rr_death_aian = calculate_rr_rng(cases, x, deaths, beta, 1, 6);
  rr_case_asian = calculate_rr_rng(pop, x, cases, gamma, 2, 6);
  rr_test_asian = calculate_rr_rng(pop, x, tests, alpha, 2, 6);
  rr_death_asian = calculate_rr_rng(cases, x, deaths, beta, 2, 6);
  rr_case_black = calculate_rr_rng(pop, x, cases, gamma, 3, 6);
  rr_test_black = calculate_rr_rng(pop, x, tests, alpha, 3, 6);
  rr_death_black = calculate_rr_rng(cases, x, deaths, beta, 3, 6);
  rr_case_latinx = calculate_rr_rng(pop, x, cases, gamma, 4, 6);
  rr_test_latinx = calculate_rr_rng(pop, x, tests, alpha, 4, 6);
  rr_death_latinx = calculate_rr_rng(cases, x, deaths, beta, 4, 6);
  rr_case_nhpi = calculate_rr_rng(pop, x, cases, gamma, 5, 6);
  rr_test_nhpi = calculate_rr_rng(pop, x, tests, alpha, 5, 6);
  rr_death_nhpi = calculate_rr_rng(cases, x, deaths, beta, 5, 6);
}