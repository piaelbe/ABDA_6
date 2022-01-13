// Nesting levels, first adult/child next level is participant
// adjlt    muy[0] tau[0]                      child my[1], tau[1]
// participant 1, ... N(mean_p, sigma)        participant 2, .... 

data {
  int<lower=0> J; //observations
  int<lower=0> P; //participants
  int<lower=0> child_c[P];
  //  int<lower=0> child_c; //child or adult
  vector[J] y;                    // Vector of observations.
  int<lower = 1, upper = P> g[J]; // Vector of group assignments. 
  //  int<lower=0> C;
}

parameters {
  real mu;
  real<lower=0> tau;
  vector[P] theta;                 // Vector of group intercepts.
  real<lower=0> sigma;
//  real phi;
  real phi;
}

model { 
  // Hyperpriors.
  mu ~ normal(0, 1);
  phi ~ normal(0, 1);
  tau ~ normal(0, 1); // 1 means child

    // Prior.
  sigma ~ normal(0, 1);
 
  for (p in 1:P) {
    theta[p] ~ normal(mu + phi*child_c[p], tau); //added phi the effect of being a child. the intercept is mu and the slope is phi.
  }

  // Population model and likelihood.
  for (j in 1:J) {
    y[j] ~ normal(theta[g[j]], sigma); //different beta for each participant
  }
}

generated quantities {
  real theta_predicted_adult = normal_rng(mu, tau);
  real theta_predicted_child = normal_rng(mu + phi, tau);
  real logy_predicted_child = normal_rng(theta_predicted_child,sigma); //If we do know that it is a child (question 4)
  real logy_predicted_adult = normal_rng(theta_predicted_adult,sigma); //If we know that it is an adult

}
