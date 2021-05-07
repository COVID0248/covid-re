data {
  int<lower=0> Nlags;             // number of lags
  int<lower=0> Nedges;            // number of edges
  int<lower=0> Nareas;            // number of areas
  int<lower=0> Ntimes;            // number of times/periods
  int<lower=0> Npreds;            // number of predictors
  real<lower=0> i[Nareas];        // area id
  real<lower=0> t[Ntimes];        // time id
  int<lower=0> Y[Ntimes, Nareas]; // observed counts.
  real X[Ntimes, Npreds];         // design matrix
  int<lower=1> edge1[Nedges];     // 1st node of the edges
  int<lower=1> edge2[Nedges];     // 2nd node of the edges
  simplex[Nlags] w;               // infectivity rates
}

parameters {
  // matrix[Nareas, Npreds] B;
  // vector[Nareas] u;
  // vector[Nareas] v;.
  real<lower=0> tu; // 1 / std(u).
  // real<lower=0> tv; // 1 / std(v).
}

transformed parameters {
  real<lower=0> ou = 1.0 / sqrt(tu);
  // real<lower=0> ov = 1.0 / sqrt(tv);
}

model {
  // target += -0.5 * dot_self(v[edge1] - v[edge2]);
  // sum(v) ~ normal(0, 0.001 * Nareas);

  // Specify the role of the other parameters in the model
  for (j in 1:Ntimes) {
    Y[j] ~ poisson(ou);
  }
  // for (j in (Nlags + 1):Ntimes) {
  //   y[j] ~ poisson_log(log(Ylags[j] * w) + B * X[j] + ou * u + ov * v);
  // }
  // u  ~ normal(0, 1);
  // b  ~ normal(0, 5);
  // tu ~ gamma(3.2761, 1.81); // Carlin WinBUGS priors.
  // tv ~ gamma(1.0000, 1.00); // Carlin WinBUGS priors.
  // w  ~ gamma(1, 1);
  // w  ~ dirichlet(w_hp);
}
