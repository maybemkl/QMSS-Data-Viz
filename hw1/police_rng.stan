functions {
      int[ , , ] police_rng(int D, int R, int[ , ] stops) {
        // prior predictions stored as Departments by Race by {Searches, Hits}
        int out[D, R, 2] = rep_array(0, D, R, 2); // initialize everything to 0
        // fill in the rest, which will involve many loops and random draws
        // weakly informative priors
        real sigma_phi = fabs(normal_rng(0, 2))
        real sigma_lambda = fabs(normal_rng(0, 2))
        
        //hyperpriors
        real mu_phi = normal_rng(0,2)
        real mu_lambda = normal_rng(0,2)

        for(d in 1:D) {
          real phi_d = normal_rng(mu_phi, sigma_phi)
          real lambda_d = normal_rng(mu_lambda, sigma_lambda)
          
          for(r in 1:R) {
            real mu_t_r = normal_rng(0,2)
            real sigma_t_r = fabs(normal_rng(0, 2))
            real phi_r = normal_rng(0,2)
            real lambda_r = normal_rng(0,2)
        
            for(s in stops[d,r]) {
              real phi_rd = inv_logit(phi_r + phi_d)
              real lambda = exp(lambda_r + lambda_r)
              real p_i = beta_rng(phi_rd, lambda_rd)
              real t_rd = normal_rng(mu_t_r, sigma_t_r)
              if(p_i >= t_rd) {
                out[d, r, 1] += 1 
                out[d, r, 2] += bernoulli(p_i)
              }
              
            }
            
          }
        }
        
        return out;
      }
} 