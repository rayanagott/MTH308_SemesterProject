data {
  int<lower=1> N;  // Number of observations
  int<lower=1> K;  // Number of outcome categories
  int<lower=1,upper=K> position[N];  // Ordered outcome
  int<lower=1> driver_id[N];  // Grouping for driver
  int<lower=1> constructor_id[N];  // Grouping for constructor
  int<lower=1> year[N];  // Grouping for year
  real wet_race[N];  // Predictor
  real permanent_circuit[N];  // Predictor
}

parameters {
  vector[K - 1] cutpoints;  // Cutpoints for cumulative logit
  real wet_race_coef;
  real permanent_circuit_coef;
  vector<lower=0>[N] driver_sd;  // Random effects for drivers
  vector<lower=0>[N] constructor_sd;  // Random effects for constructors
  vector<lower=0>[N] year_sd;  // Random effects for year
}

model {
  wet_race_coef ~ normal(0, 5);
  permanent_circuit_coef ~ normal(0, 5);
  
  position ~ ordered_logistic( 
    wet_race_coef * wet_race + permanent_circuit_coef * permanent_circuit + cutpoints 
  );
}
