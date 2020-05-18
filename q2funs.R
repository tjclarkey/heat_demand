# 2b
fnb <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")]
  }
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, sep = "")] <- exp(t[, paste("v_i", j, sep = "")])
  }
  t <- mutate(t, sum_exp_v_ij = rowSums(t[, 23:27]))
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- (t[, paste("exp_v_i", j, sep = "")]) / (t$sum_exp_v_ij)
  }
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  logprob <- log(p)
  return(-sum(logprob))
}


#2biii
fnb3 <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")]
  }
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, sep = "")] <- exp(t[, paste("v_i", j, sep = "")])
  }
  t <- mutate(t, sum_exp_v_ij = rowSums(t[, 23:27]))
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- (t[, paste("exp_v_i", j, sep = "")]) / (t$sum_exp_v_ij)
  }
  
  for (j in unique(t$choice)) {
    t[, paste("elast_ic", j, sep = "_")] <- b1 * (c(1) - t[, paste("P_i", j, sep = "")]) * t[, paste("ic", j, sep = "_")]
    t[, paste("elast_oc", j, sep = "_")] <- b2 * (c(1) - t[, paste("P_i", j, sep = "")]) * t[, paste("oc", j, sep = "_")]
  }
  
  t %>% 
    select(contains("elast")) %>% 
    select(contains("ic"), everything()) %>% 
    mutate_all(funs(average = mean(.))) %>% 
    select(contains("average")) %>% 
    slice(1L)
}


#2c
fnc <- function(beta) {
  t <- heat_data 
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b4 <- beta[4]
  b5 <- beta[5]
  b6 <- beta[6]
  
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0))))
  }
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, sep = "")] <- exp(t[, paste("v_i", j, sep = "")])
  }
  t <- mutate(t, sum_exp_v_ij = rowSums(t[, 23:27]))
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- (t[, paste("exp_v_i", j, sep = "")]) / (t$sum_exp_v_ij)
  }
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  logprob <- log(p)
  return(-sum(logprob))
}


#2d
fnd <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b4 <- beta[4]
  b5 <- beta[5]
  b6 <- beta[6]
  b7 <- beta[7]
  b8 <- beta[8]
  
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0))))
    
  }
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, sep = "")] <- exp(t[, paste("v_i", j, sep = "")])
  }
  t <- mutate(t, sum_exp_v_ij = rowSums(t[, 23:27]))
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- (t[, paste("exp_v_i", j, sep = "")]) / (t$sum_exp_v_ij)
  }
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  logprob <- log(p)
  return(-sum(logprob))
}


#2e
fne <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b9 <- beta[9]
  b4 <-  beta[4]
  b10 <- beta[10]
  b5 <- beta[5]
  b11 <- beta[11]
  b6 <- beta[6]
  b12 <- beta[12]
  b7 <- beta[7]
  b8 <- beta[8]
  
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0)))) +
      ifelse(t$choice == "gc", beta[9], 
             ifelse(t$choice == "gr", beta[10], 
                    ifelse(t$choice == "ec", beta[11], 
                           ifelse(t$choice == "er", beta[12], 0)))) * t$income
    
  }
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, sep = "")] <- exp(t[, paste("v_i", j, sep = "")])
  }
  t <- mutate(t, sum_exp_v_ij = rowSums(t[, 23:27]))
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- (t[, paste("exp_v_i", j, sep = "")]) / (t$sum_exp_v_ij)
  }
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  logprob <- log(p)
  return(-sum(logprob))
}


#2f
fnf <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b9 <- beta[9]
  b4 <-  beta[4]
  b10 <- beta[10]
  b5 <- beta[5]
  b11 <- beta[11]
  b6 <- beta[6]
  b12 <- beta[12]
  b7 <- beta[7]
  b8 <- beta[8]
  l1 <- beta[13]
  l2 <- beta[14]
  
  # Generating the observable utility function
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0)))) +
      ifelse(t$choice == "gc", beta[9], 
             ifelse(t$choice == "gr", beta[10], 
                    ifelse(t$choice == "ec", beta[11], 
                           ifelse(t$choice == "er", beta[12], 0)))) * t$income
    
  }
  
  # Here I create 5 new variables. Each variable represents the utility that an individual 
  # receives if they choose product j, divided by the weight for the nest of that product.
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, "_lambdaNest", sep = "")] <- 
      t[, paste("v_i", j, sep = "")] / ifelse(t$choice == "gc", l1, 
                                              ifelse(t$choice == "gr", l1,
                                                     ifelse(t$choice == "ec", l2,
                                                            ifelse(t$choice == "er", l2, l2))))
  } 
  
  # Here I create an additional 5 variables. Each variable is simply e raised to the power 
  # of the utility divided by the nest preference weight, lambda (l1 & l2)
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, "_lambdaNest", sep = "")] <- exp(t[, paste("v_i", j, "_lambdaNest", sep = "")])
  }
  
  # This code creates two new columns, each expressing the summation used in the numerator 
  # of the probability that inidivudal i chooses system j, for each nest
  t <- mutate(t, 
              numSumGas = t$exp_v_igc_lambdaNest + t$exp_v_igr_lambdaNest,
              numSumElec = t$exp_v_ier_lambdaNest + t$exp_v_ihp_lambdaNest + t$exp_v_iec_lambdaNest)
  
  
  # I create a variable for each heating system. This variable represents the probability that an individual
  # chooses that product. I take the product of the exponential of the individual's utility over the weight 
  # and the sum of the exponential terms within that nest, 
  # raised to the weight of that nest subtract 1. This product is divided by the sum of the exponential terms 
  # for both nests raised to the power of their respective weights.
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- 
      ((t[, paste("exp_v_i", j, "_lambdaNest", sep = "")]) * 
         (ifelse(t$choice == "gc", t$numSumGas^(l1-1), 
                 ifelse(t$choice == "gr", t$numSumGas^(l1-1), 
                        ifelse(t$choice == "ec", t$numSumElec^(l2-1),
                               ifelse(t$choice == "er", t$numSumElec^(l2-1), t$numSumElec^(l2-1))))))
       ) / (t$numSumGas^l1 + t$numSumElec^l2)
  }
  
  # This finds the p values which are fed into the likelihood function. It only pulls the probability that
  # an individual chooses the heating system which they actually did choose.
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  
  # Log of the likelihood function results in taking the sum of the log of p. We return the negation of this 
  # since optim, by default, finds the values which minimize the function that is called within it. Since we 
  # are performing Maximum Likelihood Estimation, we want to do the opposite of this.
  logprob <- log(p)
  return(-sum(logprob))
}


#2fi
fnfi <- function(beta) {
  t <- heat_data
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b9 <- beta[9]
  b4 <-  beta[4]
  b10 <- beta[10]
  b5 <- beta[5]
  b11 <- beta[11]
  b6 <- beta[6]
  b12 <- beta[12]
  b7 <- beta[7]
  b8 <- beta[8]
  l1 <- beta[13]
  l2 <- beta[14]
  
  # Generating the observable utility function
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0)))) +
      ifelse(t$choice == "gc", beta[9], 
             ifelse(t$choice == "gr", beta[10], 
                    ifelse(t$choice == "ec", beta[11], 
                           ifelse(t$choice == "er", beta[12], 0)))) * t$income
    
  }
  
  # Here I create 5 new variables. Each variable represents the utility that an individual 
  # receives if they choose product j, divided by the weight for the nest of that product.
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, "_lambdaNest", sep = "")] <- 
      t[, paste("v_i", j, sep = "")] / ifelse(t$choice == "gc", l1, 
                                              ifelse(t$choice == "ec", l1,
                                                     ifelse(t$choice == "hp", l1,
                                                            ifelse(t$choice == "gr", l2, l2))))
  } 
  
  # Here I create an additional 5 variables. Each variable is simply e raised to the power 
  # of the utility divided by the nest preference weight, lambda (l1 & l2)
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, "_lambdaNest", sep = "")] <- exp(t[, paste("v_i", j, "_lambdaNest", sep = "")])
  }
  
  # This code creates two new columns, each expressing the summation used in the numerator 
  # of the probability that inidivudal i chooses system j, for each nest
  t <- mutate(t, 
              numSumCentral = t$exp_v_igc_lambdaNest + t$exp_v_iec_lambdaNest + t$exp_v_ihp_lambdaNest,
              numSumRoom = t$exp_v_igr_lambdaNest + t$exp_v_ier_lambdaNest)
  
  
  # I create a variable for each heating system. This variable represents the probability that an individual
  # chooses that product. I take the product of the exponential of the individual's utility over the weight 
  # and the sum of the exponential terms within that nest, 
  # raised to the weight of that nest subtract 1. This product is divided by the sum of the exponential terms 
  # for both nests raised to the power of their respective weights.
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- 
      ((t[, paste("exp_v_i", j, "_lambdaNest", sep = "")]) * 
         (ifelse(t$choice == "gc", t$numSumCentral^(l1-1), 
                 ifelse(t$choice == "ec", t$numSumCentral^(l1-1), 
                        ifelse(t$choice == "hp", t$numSumCentral^(l1-1),
                               ifelse(t$choice == "gr", t$numSumRoom^(l2-1), t$numSumRoom^(l2-1))))))
      ) / (t$numSumCentral^l1 + t$numSumRoom^l2)
  }
  
  # This finds the p values which are fed into the likelihood function. It only pulls the probability that
  # an individual chooses the heating system which they actually did choose.
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  
  # Log of the likelihood function results in taking the sum of the log of p. We return the negation of this 
  # since optim, by default, finds the values which minimize the function that is called within it. Since we 
  # are performing Maximum Likelihood Estimation, we want to do the opposite of this.
  logprob <- log(p)
  return(-sum(logprob))
}

#2fii
coefficient_descriptions <- c("Change in utility for a $1 increase in installation cost",
                             "Change in utility for a $1 increase in operation cost",
                             "Utility of choosing Gas Central over Heat Pump",
                             "Utility of choosing Gas Room over Heat Pump",
                             "Utility of choosing Electric Central over Heat Pump",
                             "Utility of choosing Electric Room over Heat Pump",
                             "Change in sensitivity to installation cost for a unit ^ in income",
                             "Change in sensitivity to operation cost for a a unit ^ in income",
                             "Change in utility of Gas Central over HP for a unit ^ in income",
                             "Change in utility of Gas Room over HP for a unit ^ in income",
                             "Change in utility of Electric Central over HP for a unit ^ in income",
                             "Change in utility of Electric Room over HP for a unit ^ in income",
                             "The relative weight of preference for nest 1",
                             "The relative weight of preference for nest 2"
)


#2g
fng <- function(beta) {
  t <- rebate
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b9 <- beta[9]
  b4 <-  beta[4]
  b10 <- beta[10]
  b5 <- beta[5]
  b11 <- beta[11]
  b6 <- beta[6]
  b12 <- beta[12]
  b7 <- beta[7]
  b8 <- beta[8]
  l1 <- beta[13]
  l2 <- beta[14]
  
  # Generating the observable utility function
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", beta[3], 
             ifelse(t$choice == "gr", beta[4], 
                    ifelse(t$choice == "ec", beta[5], 
                           ifelse(t$choice == "er", beta[6], 0)))) +
      ifelse(t$choice == "gc", beta[9], 
             ifelse(t$choice == "gr", beta[10], 
                    ifelse(t$choice == "ec", beta[11], 
                           ifelse(t$choice == "er", beta[12], 0)))) * t$income
    
  }
  
  # Here I create 5 new variables. Each variable represents the utility that an individual 
  # receives if they choose product j, divided by the weight for the nest of that product.
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, "_lambdaNest", sep = "")] <- 
      t[, paste("v_i", j, sep = "")] / ifelse(t$choice == "gc", l1, 
                                              ifelse(t$choice == "ec", l1,
                                                     ifelse(t$choice == "hp", l1,
                                                            ifelse(t$choice == "gr", l2, l2))))
  } 
  
  # Here I create an additional 5 variables. Each variable is simply e raised to the power 
  # of the utility divided by the nest preference weight, lambda (l1 & l2)
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, "_lambdaNest", sep = "")] <- exp(t[, paste("v_i", j, "_lambdaNest", sep = "")])
  }
  
  # This code creates two new columns, each expressing the summation used in the numerator 
  # of the probability that inidivudal i chooses system j, for each nest
  t <- mutate(t, 
              numSumCentral = t$exp_v_igc_lambdaNest + t$exp_v_iec_lambdaNest + t$exp_v_ihp_lambdaNest,
              numSumRoom = t$exp_v_igr_lambdaNest + t$exp_v_ier_lambdaNest)
  
  
  # I create a variable for each heating system. This variable represents the probability that an individual
  # chooses that product. I take the product of the exponential of the individual's utility over the weight 
  # and the sum of the exponential terms within that nest, 
  # raised to the weight of that nest subtract 1. This product is divided by the sum of the exponential terms 
  # for both nests raised to the power of their respective weights.
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- 
      ((t[, paste("exp_v_i", j, "_lambdaNest", sep = "")]) * 
         (ifelse(t$choice == "gc", t$numSumCentral^(l1-1), 
                 ifelse(t$choice == "ec", t$numSumCentral^(l1-1), 
                        ifelse(t$choice == "hp", t$numSumCentral^(l1-1),
                               ifelse(t$choice == "gr", t$numSumRoom^(l2-1), t$numSumRoom^(l2-1))))))
      ) / (t$numSumCentral^l1 + t$numSumRoom^l2)
  }
  
  # This finds the p values which are fed into the likelihood function. It only pulls the probability that
  # an individual chooses the heating system which they actually did choose.
  p = ifelse(t$choice == "gc", t$P_igc, 
             ifelse(t$choice == "gr", t$P_igr, 
                    ifelse(t$choice == "hp", t$P_ihp, 
                           ifelse(t$choice == "ec", t$P_iec, t$P_ier))))
  
  # Log of the likelihood function results in taking the sum of the log of p. We return the negation of this 
  # since optim, by default, finds the values which minimize the function that is called within it. Since we 
  # are performing Maximum Likelihood Estimation, we want to do the opposite of this.
  logprob <- log(p)
  return(-sum(logprob))
}



predictMarketShare <- function(beta, data) {
  
  t <- data
  b1 <- beta[1]
  b2 <- beta[2]
  b3 <- beta[3]
  b9 <- beta[9]
  b4 <-  beta[4]
  b10 <- beta[10]
  b5 <- beta[5]
  b11 <- beta[11]
  b6 <- beta[6]
  b12 <- beta[12]
  b7 <- beta[7]
  b8 <- beta[8]
  l1 <- beta[13]
  l2 <- beta[14]
  
  # Generating the observable utility function
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, sep = "")] <- 
      b1 * t[, paste("ic", j, sep = "_")] + b2 * t[, paste("oc", j, sep = "_")] + 
      b7 * t[, paste("ic", j, sep = "_")] * t[,"income"] + b8 * t[, paste("oc", j, sep = "_")] * t[,"income"] +
      ifelse(t$choice == "gc", b3, 
             ifelse(t$choice == "gr", b4, 
                    ifelse(t$choice == "ec", b5, 
                           ifelse(t$choice == "er", b6, 0)))) +
      ifelse(t$choice == "gc", b9, 
             ifelse(t$choice == "gr", b10, 
                    ifelse(t$choice == "ec", b11, 
                           ifelse(t$choice == "er", b12, 0)))) * t$income
    
  }
  
  # Here I create 5 new variables. Each variable represents the utility that an individual 
  # receives if they choose product j, divided by the weight for the nest of that product.
  for (j in unique(t$choice)) {
    t[, paste("v_i", j, "_lambdaNest", sep = "")] <- 
      t[, paste("v_i", j, sep = "")] / ifelse(t$choice == "gc", l1, 
                                              ifelse(t$choice == "ec", l1,
                                                     ifelse(t$choice == "hp", l1,
                                                            ifelse(t$choice == "gr", l2, l2))))
  } 
  
  # Here I create an additional 5 variables. Each variable is simply e raised to the power 
  # of the utility divided by the nest preference weight, lambda (l1 & l2)
  for (j in unique(t$choice)) {
    t[, paste("exp_v_i", j, "_lambdaNest", sep = "")] <- exp(t[, paste("v_i", j, "_lambdaNest", sep = "")])
  }
  
  # This code creates two new columns, each expressing the summation used in the numerator 
  # of the probability that inidivudal i chooses system j, for each nest
  t <- mutate(t, 
              numSumCentral = t$exp_v_igc_lambdaNest + t$exp_v_iec_lambdaNest + t$exp_v_ihp_lambdaNest,
              numSumRoom = t$exp_v_igr_lambdaNest + t$exp_v_ier_lambdaNest)
  
  
  # I create a variable for each heating system. This variable represents the probability that an individual
  # chooses that product. I take the product of the exponential of the individual's utility over the weight 
  # and the sum of the exponential terms within that nest, 
  # raised to the weight of that nest subtract 1. This product is divided by the sum of the exponential terms 
  # for both nests raised to the power of their respective weights.
  for (j in unique(t$choice)) {
    t[, paste("P_i", j, sep = "")] <- 
      ((t[, paste("exp_v_i", j, "_lambdaNest", sep = "")]) * 
         (ifelse(t$choice == "gc", t$numSumCentral^(l1-1), 
                 ifelse(t$choice == "ec", t$numSumCentral^(l1-1), 
                        ifelse(t$choice == "hp", t$numSumCentral^(l1-1),
                               ifelse(t$choice == "gr", t$numSumRoom^(l2-1), t$numSumRoom^(l2-1))))))
      ) / (t$numSumCentral^l1 + t$numSumRoom^l2)
  }
  t <- t %>% 
    select(P_ihp) %>% 
    filter(P_ihp <= 1) 
  return(mean(t$P_ihp))
} 

