data {
int<lower=0> N; // sample size
int<lower=1> J;// Number of groups
real y[N];
int causa[N];
}

parameters {
  //hyperparameters
  real<lower=0, upper=2.5> sigma_mu_m;
  //parameters
  real<lower=0,upper=0.60> alpha[J];
  real mu_m_tilde[J];
  real<lower=0,upper=20> sigma_m[J];
  real<lower=-20,upper=20> mu_M_tilde;
  real<lower=0,upper=20> sigma_M;
  real<lower=-0.80403,upper=0.80400> gamma_M;
}

transformed parameters {
  real<upper=65> mu_m[J];
  real mu_M;
  for(j in 1:J){
    mu_m[j]=45+sigma_mu_m*mu_m_tilde[j];
  }
  mu_M=65+4*mu_M_tilde;
}


model {
  // We use the centered parametrization of skew normals
  // local variables


  real c_M;
  real muz_M;
  real csi_M;
  real omega_M;
  real lambda_M;


    
  


  if(gamma_M<0){
    c_M=((-1)*pow((2*(-gamma_M))*inv(4-pi()),0.3333333));
  }
  else{
    c_M=((1)*pow((2*(gamma_M))*inv(4-pi()),0.3333333));
  }

  muz_M=c_M*inv_sqrt(1+square(c_M));
  lambda_M=(muz_M*sqrt(pi()*0.5))*inv_sqrt(1-square(muz_M)*pi()*0.5);
  omega_M=sigma_M*inv_sqrt(1-square(muz_M));
  csi_M=mu_M-omega_M*muz_M;

  //MODELLO GERARCHICO

  //DISTRIBUZIONI A PRIORI

  //iperparametri comuni

  sigma_mu_m ~ uniform(0,2.5);

  //group-varying parameters
  for(j in 1:J){
    alpha[j] ~ uniform(0,0.60);
    //mu_m[j] ~ normal(60,sigma_mu_m)T[ , 75];
    mu_m_tilde[j] ~ normal(0,1);
    sigma_m[j] ~ uniform(0,20);
  }

  //Common parameters (second SN)
  //mu_m ~ normal(87,2);
  mu_M_tilde ~ normal(0,1);
  sigma_M ~ uniform(0,9);
  gamma_M ~ skew_normal(-1,0.5,1)T[-0.995,0.995];

  // Mixture model
  for(n in 1:N){
    target += log_sum_exp(log(alpha[causa[n]])
			  +normal_lpdf(y[n] | mu_m[causa[n]],sigma_m[causa[n]]),
				log1m(alpha[causa[n]])
			  +skew_normal_lpdf(y[n] | csi_M, omega_M,lambda_M));
  }
}






    
