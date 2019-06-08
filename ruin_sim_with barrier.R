ruin_sim_with_barrier <- function(num, c, t_s, u,  
                            claim_amount_f, claim_amount_pdf, a, b, k,
                            claim_interval_f, claim_interval_cdf, barrier) {
  # Simulation of insurance process
  # Args:
  #                num: number of simulation of insurance process
  #                  c: policy price
  #                  u: initial money
  #                t_s: stop time
  #     claim_amount_f: random number generator of claim amount
  #   claim_amount_pdf: estimated pdf of claim_amount distribution
  #            a, b, k: args of claim_amount_f
  #   claim_interval_f: random number generator of claim interval
  # claim_interval_cdf: estimated cdf of claim interval distribution
  #            barrier: bonus barrier
  
  # Assume claim amount is a continuous rv
  #        claim interval is a discrete rv
  
  K_s <- numeric(length(t_s))
  
  for (m in 1:num) {
    t_policy <- 1 # time when insurance policy comes
    t_claim <- claim_interval_f(claim_interval_cdf) # time when claim happens
    S <- u # initial asset
    t <- max(t_s) # running time before the sim stop
    idx <- 1
    
    while (min(c(t_policy, t_claim)) <= t) { 
      t_first <- min(c(t_policy, t_claim)) # time when first thing happen
      if (t_first > t_s[idx]) {
        idx <- idx + 1
      }
      if (t_policy <= t_claim) { # if policy comes first
        S <- S + c    # earn policy price 
        if (S > barrier) {
          S <- barrier
        }
        t_policy <- t_first + 1
      } else {
        S <- S - claim_amount_f(claim_amount_pdf, a, b, k) # minus claim amount
        t_claim <- t_first + claim_interval_f(claim_interval_cdf)
        if (S < 0) { # if broken
          K_s[idx] <- K_s[idx] + 1 # number of broken add one
          break  # stop this sim process
        }
      }
    }
  }
  cumsum(K_s) / num
}