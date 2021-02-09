data {
  int<lower=0> gamma;
  int<lower=0> Ntimes;
  int<lower=0> Nareas;
  int<lower=0> I[Ntimes, Nareas];
}

parameters {
  real<lower=0> sigma;
  real R[Ntimes, Nareas];
}

model {
  for (j in 2:Ntimes) {
    for (i in 1:Nareas) {
      I[j, i] ~ poisson_log(log(I[j - 1, i]) + gamma * (R[j, i] - 1));
      R[j, i] ~ normal(R[j, i], sigma);
    }
  }
}
