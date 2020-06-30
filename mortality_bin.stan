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
}

data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] x;
  int deaths[N];
  int pop[N];
  int cases[N];
}

parameters {
  vector[K] beta;
  vector[K] gamma;
}

transformed parameters {
  vector[K] p;
  vector[K] q;
  p = inv_logit(x * gamma);
  q = inv_logit(x * beta);
}

model {
  beta ~ normal(0, 10);
  gamma ~ normal(0, 10);
  cases ~ binomial(pop, p);
  deaths ~ binomial(cases, q);
}

generated quantities {
  real<lower=0> rr_case;
  real<lower=0> rr_death;
  int N_black;
  int N_white;

  
  N_black = count_elem(x[:,3], 1);
  N_white = count_elem(x[:,6], 1);
  
  {
    int idx_black[N_black];
    int idx_white[N_white];
    int cases_hat_black[N_black];
    int cases_hat_white[N_white];
    int deaths_hat_black[N_black];
    int deaths_hat_white[N_white];
    real pr_case_black;
    real pr_case_white;
    real pr_death_black;
    real pr_death_white;
    
    idx_black = which_elem(x[:,3], 1);
    idx_white = which_elem(x[:,6], 1);
    
    cases_hat_black = binomial_rng(pop[idx_black], inv_logit(x[idx_black] * gamma));
    deaths_hat_black = binomial_rng(cases[idx_black], inv_logit(x[idx_black] * beta));
    
    cases_hat_white = binomial_rng(pop[idx_white], inv_logit(x[idx_white] * gamma));
    deaths_hat_white = binomial_rng(cases[idx_white], inv_logit(x[idx_white] * beta));
  
    pr_case_black = sum(cases_hat_black);
    pr_case_black = pr_case_black / (N_black * sum(pop[idx_black]) * 1.0);
    pr_death_black = sum(deaths_hat_black);
    pr_death_black = pr_death_black / (N_black * sum(cases[idx_black]) * 1.0);
    
    pr_case_white = sum(cases_hat_white);
    pr_case_white = pr_case_white / (N_white * sum(pop[idx_white]) * 1.0);
    pr_death_white = sum(deaths_hat_white);
    pr_death_white = pr_death_white / (N_white * sum(cases[idx_white]) * 1.0);
    
    rr_case = pr_case_black / pr_case_white;
    rr_death = pr_death_black / pr_death_white;
  }
}