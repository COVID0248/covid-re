data {
  int<lower=0> Nlags;             // number of lags
  int<lower=0> Nedges;            // number of edges
  int<lower=0> Nareas;            // number of areas
  int<lower=0> Ntimes;            // number of times/periods
  int<lower=0> Npreds;            // number of predictors
  real<lower=0> i[Nareas];        // area id
  real<lower=0> t[Ntimes];        // time id
  int<lower=0> Y[Ntimes, Nareas]; // observed counts.
  matrix[Ntimes, Npreds] X;       // design matrix
  int<lower=1> edge1[Nedges];     // 1st node of the edges
  int<lower=1> edge2[Nedges];     // 2nd node of the edges
  simplex[Nlags] w;               // infectivity rates
}

transformed data {
  row_vector[Npreds] ones;
  for (k in 1:Npreds) {
    ones[k] = 1.0;
  }
}

parameters {
  vector[Nareas] u;
  matrix[Ntimes, Nareas] e;
  matrix[Nareas, Npreds] V;
  real<lower=0> tu; // 1 / std(u).
  real<lower=0> te; // 1 / std(e).
  real<lower=0> tV; // 1 / std(V).
}

transformed parameters {
  real<lower=0> oV = 1.0 / sqrt(tV);
  real<lower=0> ou = 1.0 / sqrt(tu);
  matrix[Nareas, Npreds] B = ou * u * ones + oV * V;
}

model {
  for (j in 1:Ntimes) {
    Y[j] ~ poisson(ou);
  }
  for (k in 1:Npreds) {
    target += -0.5 * dot_self(V[edge1, k] - V[edge2, k]);
    sum(col(V, k)) ~ normal(0, 0.001 * Nareas);
  }

  // Specify the role of the other parameters in the model
  for (j in 1:Ntimes) {
    e[j]  ~ normal(0, 1);
    Y[j] ~ poisson_log(B * X[j]' + e[j]');
  }
  // u  ~ normal(0, 1);
  // te ~ gamma(3.2761, 1.81);
  // tu ~ gamma(3.2761, 1.81); // Carlin WinBUGS priors.
  // tV ~ gamma(1.0000, 1.00); // Carlin WinBUGS priors.
  // w  ~ gamma(1, 1);
  // w  ~ dirichlet(w_hp);
}
