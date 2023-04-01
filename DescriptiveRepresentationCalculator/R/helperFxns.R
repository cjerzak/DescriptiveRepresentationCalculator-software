theoretical_L1deviance_affine <- function(pop_share, bodyN, a = -1/2, b = 1){
  if(length(pop_share) > 1){
    theoretical_means_log <- log(2) +
      (bodyN - floor(bodyN*pop_share))*log(1 - pop_share) +
      (floor(bodyN*pop_share)+1)*log(pop_share) +
      log(floor(bodyN*pop_share)+1) +
      lchoose(bodyN,floor(bodyN*pop_share)+1)
    theoretical_means <- exp(  theoretical_means_log )
    theoretical_mean <- sum( theoretical_means / bodyN )
  }
  if(length(pop_share) == 1){theoretical_mean <- 0}
  if(abs(max(pop_share) - 1) < 10^(-10)){
    theoretical_means <- 2 * (1 - pop_share)^(bodyN - floor(bodyN*pop_share)) *
      pop_share^(floor(bodyN*pop_share)+1)*
      (floor(bodyN*pop_share)+1)*
      choose(bodyN,floor(bodyN*pop_share)+1)
    theoretical_mean <- sum( theoretical_means / bodyN )
  }
  return( a * theoretical_mean + b )
}
