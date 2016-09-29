#' @description Compute Economic Value following Richardson definitions. For further details cfr Richardson, Economic Value & Skills, 2003
#! @param pp A vector containing probabilistic forecast
#! @param pp_ref A vector of an observed binary event (0,1) 
#! @param COST the vector of the COST/LOSS ratio to consider. The default is seq(0.1, 0.9, 0.05)
#! @param method NA or 'bootstrap' to compute a bootstrap for the significativity 
#' @export
#' @author M. De Felice
getEconomicValue = function(pp, pp_ref, COST = c(), method = NA, NREP = 100) {
  
  
  if (length(COST) == 0) {
    SCALE = seq(0.1, 0.9, by = 0.05)
  } else {
    SCALE = COST
  }
  
  N = length(pp)
  pp_ref = as.logical(pp_ref) #
  V = rep(NA, length(SCALE))
  # Discretizing probabilistic forecast
  # Assuming that optimal disc. threshold is equal to C/L -> C
  for (i in 1:length(SCALE)) {
    pp_disc = (pp > SCALE[i])
    # Setting L (Loss) as 1 we explore various values of C (cost)
    
    C = SCALE[i]
    s = mean(pp_ref * 1)
    Eperfect = s*C
    Eclimate = min(C, s)
    
    a = sum(1*(pp_disc & pp_ref)) / N
    b = sum(1*(pp_disc & !pp_ref)) / N
    c = sum(1*(!pp_disc & pp_ref)) / N
    d = sum(1*(!pp_disc & !pp_ref)) / N
    Eforecast = a*C +b*C+c # Eq. (8.6)
    
    V[i] = (Eclimate - Eforecast)/(Eclimate - Eperfect)
    # print(sprintf('C: %2.2f: Eclim: %2.2f Eforec: %2.2f Eperf: %2.2f', 
#                   C, Eclimate, Eforecast, Eperfect))
  }
  # Bootstrap
  if (!is.na(method)) {
    bsV = matrix(NA, nrow = length(SCALE), ncol = NREP)
    for (n in 1:NREP) {
      for (i in 1:length(SCALE)) {
        bspp = sample(pp, length(pp), replace = T)
        pp_disc = (bspp > SCALE[i])
        # Setting L (Loss) as 1 we explore various values of C (cost)
        C = SCALE[i]
        s = mean(pp_ref * 1)
        Eperfect = s*C
        Eclimate = min(C, s)
        
        a = sum(1*(pp_disc & pp_ref)) / N
        b = sum(1*(pp_disc & !pp_ref)) / N
        c = sum(1*(!pp_disc & pp_ref)) / N
        d = sum(1*(!pp_disc & !pp_ref)) / N
        Eforecast = a*C +b*C+c # Eq. (8.6)
        
        bsV[i,n] = (Eclimate - Eforecast)/(Eclimate - Eperfect)
      }
    }
    # Compute significativity
    QQs = c(0.01, 0.025, 0.05)
    SIGs = rep(NA, length(SCALE))
    for (l in 1:length(SCALE)) {
      for (q in QQs) {
        qq = quantile(bsV[l,], c(q, 1-q))
        if (V[i] <= qq[1] || V[i]>=qq[2]) {
          SIGs[l] = q
          break
        }
      }
    }
    # If bootstrap is enable this function exits here
    return(list(x = SCALE, v = V, SIG = SIGs))
  } else {
    return(list(x = SCALE, v = V))
  }
  
}
