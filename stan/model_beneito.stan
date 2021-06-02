data {
  int<lower=0> Nlags;             // number of lags
  int<lower=0> Nedges;            // number of edges
  int<lower=0> Nareas;            // number of areas
  int<lower=0> Ntimes;            // number of times/periods
  int<lower=0> Npreds;            // number of predictors
  int<lower=0> y[Ntimes, Nareas]; // observed counts.
  int<lower=0> n[Nareas];         // population sizes.
  matrix[Ntimes, Npreds] x;       // design matrix
  int<lower=1> edge1[Nedges];     // 1st node of the edges
  int<lower=1> edge2[Nedges];     // 2nd node of the edges
  simplex[Nlags] w;               // infectivity rates
}

transformed data {
  vector[Npreds] X[Ntimes];
  vector[Nareas] logn;
  for (t in 1:Ntimes) {
    for (k in 1:Npreds) {
      X[t][k] = x[t, k];
    }
  }
  for (i in 1:Nareas) {
    logn[i] = log(n[i]);
  }
}

parameters {
  real<lower=0> tU; // 1 / std(U).
  real<lower=0> tV; // 1 / std(V).
  vector[Nareas] V[Npreds];
  vector[Nareas] U;
}

transformed parameters {
  matrix[Nareas, Npreds] B;
  real<lower=0> oV = 1.0 / sqrt(tV);
  real<lower=0> oU = 1.0 / sqrt(tU);
  for (i in 1:Nareas) {
    for (k in 1:Npreds) {
      B[i, k] = oV * V[k][i] + oU * U[k];
    }
  }
}

model {
  for (k in 1:Npreds) {
    target += -0.5 * dot_self(V[k][edge1] - V[k][edge2]);
    sum(V[k]) ~ normal(0, 0.001 * Nareas);
  }
  for (t in 1:Ntimes) {
    y[t] ~ poisson_log(logn + B * X[t]);
  }
  U ~ normal(0, 1);
  tU ~ gamma(3.2761, 1.81); // Carlin WinBUGS priors.
  tV ~ gamma(1.0000, 1.00); // Carlin WinBUGS priors.
}

// generated quantities {
//   matrix[Nareas, Ntimes] R;
//   R = exp(B * x');
//   for (i in 1:Nareas){
//     for (t in (1+Nlags):Ntimes) {
//       real weighted_sum = 0.0;
//       for (lag in 1:Nlags) {
//         weighted_sum = weighted_sum + w[lag] * R[i, t - lag];
//       }
//       R[i, t] = R[i, t] / weighted_sum;
//     }
//   }
// }
