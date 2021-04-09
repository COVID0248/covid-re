data {
  int<lower=0> gamma;
  real<lower=0> sigma;
  int<lower=0> Ntimes;
  int<lower=0> I[Ntimes];
}

parameters {
  real R[Ntimes];
}

model {
  R[1] ~ normal(1.0, sigma);
  for (t in 2:Ntimes) {
    I[t] ~ poisson_log(log(I[t - 1]) + gamma * (R[t] - 1));
    R[t] ~ normal(R[t - 1], sigma);
  }
}
