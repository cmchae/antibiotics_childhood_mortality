functions {
  real binom_interval_lp(int lb, int ub, int n, real p) {
    int K = ub - lb + 1;
    vector[K] lps;
    for (k in lb:ub) {
      lps[k - lb + 1] = binomial_lpmf(k | n, p);
    }
    return log_sum_exp(lps);
  }
}

data {
  int<lower=1> I;            // number of study strata (pop_id)
  int<lower=1> K;            // number of predictors

  matrix[I, K] X;            // predictors for observed pop_id
  vector[I] x_mixed;         // 0 = U5-only, 1 = mixed age-band (includes >5 or all-ages)

  int<lower=1> S;                            // number of studies
  array[I] int<lower=1, upper=S> study_idx; // study index for each pop_id

  array[I] int<lower=0, upper=1> B_def_excl_M;   // bacterial stratum excludes malaria (by design)
  array[I] int<lower=0, upper=1> B_def_excl_L;   // bacterial stratum excludes LRTI (by design)

  // Bacterial
  array[I] int<lower=0, upper=1> has_B_exact;
  array[I] int<lower=0, upper=1> has_B_int;
  array[I] int<lower=0> N_B;
  array[I] int<lower=0> y_B;
  array[I] int<lower=0> yB_lb;
  array[I] int<lower=0> yB_ub;

  // Malaria
  array[I] int<lower=0, upper=1> has_M_exact;
  array[I] int<lower=0, upper=1> has_M_int;
  array[I] int<lower=0> N_M;
  array[I] int<lower=0> y_M;
  array[I] int<lower=0> yM_lb;
  array[I] int<lower=0> yM_ub;

  // LRTI
  array[I] int<lower=0, upper=1> has_L_exact;
  array[I] int<lower=0, upper=1> has_L_int;
  array[I] int<lower=0> N_L;
  array[I] int<lower=0> y_L;
  array[I] int<lower=0> yL_lb;
  array[I] int<lower=0> yL_ub;

  // B ∩ M (optional)
  array[I] int<lower=0, upper=1> has_BM_exact;
  array[I] int<lower=0, upper=1> has_BM_int;
  array[I] int<lower=0> N_BM;
  array[I] int<lower=0> y_BM;
  array[I] int<lower=0> yBM_lb;
  array[I] int<lower=0> yBM_ub;

  // Prediction rows
  int<lower=0> N_new;
  matrix[N_new, K] X_new;
}

parameters {
  // Fixed effects
  real mu_B;
  real mu_M;
  real mu_L;

  vector[K] beta_B;
  vector[K] beta_M;
  vector[K] beta_L;

  real gammaB;
  real gammaM;
  real gammaL;

  real beta_BM;
  real beta_BL;
  real beta_ML;

  // Study-level random intercept SDs (common across studies, outcome-specific)
  real<lower=0> sigma_u_B;
  real<lower=0> sigma_u_M;
  real<lower=0> sigma_u_L;

  // Non-centered study effects
  vector[S] z_u_B;
  vector[S] z_u_M;
  vector[S] z_u_L;
}

transformed parameters {
  vector[S] u_B;
  vector[S] u_M;
  vector[S] u_L;

  // Center random effects to reduce confounding with intercepts
  u_B = sigma_u_B * (z_u_B - mean(z_u_B));
  u_M = sigma_u_M * (z_u_M - mean(z_u_M));
  u_L = sigma_u_L * (z_u_L - mean(z_u_L));
}

model {
  // Priors for fixed effects
  mu_B ~ normal(0, 1.5);
  mu_M ~ normal(0, 1.5);
  mu_L ~ normal(0, 1.5);

  beta_B ~ normal(0, 0.5);
  beta_M ~ normal(0, 0.5);
  beta_L ~ normal(0, 0.5);

  gammaB ~ normal(0, 0.5);
  gammaM ~ normal(0, 0.5);
  gammaL ~ normal(0, 0.5);

  beta_BM ~ normal(0, 0.75);
  beta_BL ~ normal(0, 0.75);
  beta_ML ~ normal(0, 0.75);

  // Priors for study heterogeneity
  // These are intentionally fairly tight because too much heterogeneity
  // made the previous model unstable.
  sigma_u_B ~ normal(0, 0.3);
  sigma_u_M ~ normal(0, 0.3);
  sigma_u_L ~ normal(0, 0.3);

  z_u_B ~ std_normal();
  z_u_M ~ std_normal();
  z_u_L ~ std_normal();

  for (i in 1:I) {
    // Linear predictors for this pop_id
    real aB = mu_B + (X[i] * beta_B) + gammaB * x_mixed[i] + u_B[study_idx[i]];
    real aM = mu_M + (X[i] * beta_M) + gammaM * x_mixed[i] + u_M[study_idx[i]];
    real aL = mu_L + (X[i] * beta_L) + gammaL * x_mixed[i] + u_L[study_idx[i]];

    // 8-cell probabilities for (B,M,L) in {0,1}^3
    // idx = 1 + 4*B + 2*M + 1*L
    vector[8] log_w;
    for (b in 0:1) {
      for (m in 0:1) {
        for (l in 0:1) {
          int idx = 1 + 4*b + 2*m + l;
          log_w[idx] =
            b * aB +
            m * aM +
            l * aL +
            (b * m) * beta_BM +
            (b * l) * beta_BL +
            (m * l) * beta_ML;
        }
      }
    }
    vector[8] p = softmax(log_w);

    real pB  = p[5] + p[6] + p[7] + p[8];
    real pM  = p[3] + p[4] + p[7] + p[8];
    real pL  = p[2] + p[4] + p[6] + p[8];
    real pBM = p[7] + p[8];

    // Conditionals for excluded-by-design strata
    real p_notM = 1 - pM;
    real p_notL = 1 - pL;

    real pB_and_notM = p[5] + p[6];      // B=1, M=0 (sum over L)
    real pB_given_notM = pB_and_notM / fmax(p_notM, 1e-12);

    real pB_and_notL = p[5] + p[7];      // B=1, L=0 (sum over M)
    real pB_given_notL = pB_and_notL / fmax(p_notL, 1e-12);

    real p_notM_notL = p[1] + p[5];      // M=0, L=0 (sum over B)
    real pB_notM_notL = p[5];            // B=1, M=0, L=0
    real pB_given_notM_notL = pB_notM_notL / fmax(p_notM_notL, 1e-12);

    // Malaria likelihood
    if (has_M_exact[i] == 1) {
      y_M[i] ~ binomial(N_M[i], pM);
    } else if (has_M_int[i] == 1) {
      target += binom_interval_lp(yM_lb[i], yM_ub[i], N_M[i], pM);
    }

    // LRTI likelihood
    if (has_L_exact[i] == 1) {
      y_L[i] ~ binomial(N_L[i], pL);
    } else if (has_L_int[i] == 1) {
      target += binom_interval_lp(yL_lb[i], yL_ub[i], N_L[i], pL);
    }

    // B ∩ M likelihood
    if (has_BM_exact[i] == 1) {
      y_BM[i] ~ binomial(N_BM[i], pBM);
    } else if (has_BM_int[i] == 1) {
      target += binom_interval_lp(yBM_lb[i], yBM_ub[i], N_BM[i], pBM);
    }

    // Bacterial likelihood
    if (has_B_exact[i] == 1) {
      if (B_def_excl_M[i] == 1 && B_def_excl_L[i] == 1) {
        y_B[i] ~ binomial(N_B[i], pB_given_notM_notL);
      } else if (B_def_excl_M[i] == 1) {
        y_B[i] ~ binomial(N_B[i], pB_given_notM);
      } else if (B_def_excl_L[i] == 1) {
        y_B[i] ~ binomial(N_B[i], pB_given_notL);
      } else {
        y_B[i] ~ binomial(N_B[i], pB);
      }
    } else if (has_B_int[i] == 1) {
      if (B_def_excl_M[i] == 1 && B_def_excl_L[i] == 1) {
        target += binom_interval_lp(yB_lb[i], yB_ub[i], N_B[i], pB_given_notM_notL);
      } else if (B_def_excl_M[i] == 1) {
        target += binom_interval_lp(yB_lb[i], yB_ub[i], N_B[i], pB_given_notM);
      } else if (B_def_excl_L[i] == 1) {
        target += binom_interval_lp(yB_lb[i], yB_ub[i], N_B[i], pB_given_notL);
      } else {
        target += binom_interval_lp(yB_lb[i], yB_ub[i], N_B[i], pB);
      }
    }
  }
}

generated quantities {
  vector[N_new] p_target_u5_new;

  for (j in 1:N_new) {
    // Population-level U5 prediction:
    // no mixed-age effect and no study-specific random effect
    real aB = mu_B + (X_new[j] * beta_B);
    real aM = mu_M + (X_new[j] * beta_M);
    real aL = mu_L + (X_new[j] * beta_L);

    vector[8] log_w;
    for (b in 0:1) {
      for (m in 0:1) {
        for (l in 0:1) {
          int idx = 1 + 4*b + 2*m + l;
          log_w[idx] =
            b * aB +
            m * aM +
            l * aL +
            (b * m) * beta_BM +
            (b * l) * beta_BL +
            (m * l) * beta_ML;
        }
      }
    }
    vector[8] p = softmax(log_w);

  // Target: P(B=1 | L=0)
  real denom = p[1] + p[3] + p[5] + p[7];
  p_target_u5_new[j] = (p[5] + p[7]) / fmax(denom, 1e-12);
  }
}
