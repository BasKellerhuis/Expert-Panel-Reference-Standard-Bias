SimTest <- function(disease_status, sens, spec){
  # Function takes arguments:
  #   disease_status: Vector of disease status
  #   sens: Vector of sensitivities for the tests to simulate
  #   spec: Vector of specificities for the tests to simulate
  #
  # Function returns:
  #   test_results: Matrix of test results, one column per test
  
  n_sample <- length(disease_status)
  D <- as.logical(disease_status)
  n_test <- length(sens)
  
  if (length(sens) != length(spec)) {
    stop("Argument sens and spec are required to be of the same length.")
  }
  
  # Simulate test results
  test_results <- matrix(nrow = n_sample,
                         ncol = n_test)
  
  for (i in 1:n_test){
    test_results[D, i] <- rbinom(n = sum(disease_status),
                                 size = 1,
                                 prob = sens[i])
    test_results[!D, i] <- rbinom(n = n_sample - sum(disease_status),
                                  size = 1,
                                  prob = 1 - spec[i])
  }
  
  return(test_results)
}

SensSpecRecalc <- function(true_class, predicted_class, sens, spec){
  # Function takes arguments:
  #   true_class: Vector of true disease status
  #   predicted_class: Vector of predicted disease status
  #   sens: Vector of sensitivities for the tests to simulate
  #   spec: Vector of specificities for the tests to simulate
  #
  # Function returns:
  #   Data frame containing apparent sensitivity and specificity
  
  if (length(true_class) != length(predicted_class)) {
    stop("Argument true_class and predicted_class are required to be of the same length.")
  }  
  if (length(sens) != length(spec)) {
    stop("Argument sens and spec are required to be of the same length.")
  }
  
  n_test <- length(sens)
  
  recalc_test <- SimTest(disease_status = true_class,
                         sens = sens,
                         spec = spec)
  
  sens_recalc <- numeric(n_test)
  spec_recalc <- numeric(n_test)
  
  for (i in 1:n_test){
    conf_mat <- table(factor(predicted_class, levels = c(0,1)),
                      factor(recalc_test[, i], levels = c(0,1)))
    
    sens_recalc[i] <- conf_mat[2, 2] / sum(conf_mat[2, ])
    spec_recalc[i] <- conf_mat[1, 1] / sum(conf_mat[1, ])
  }

  
  return(data.frame("Sensitivity recalculated" = sens_recalc,
                    "Specificity recalculated" = spec_recalc,
                    "Sensitivity index" = sens,
                    "Specificity index" = spec))
}

BTrule <- function(test_results, prevalence, sens, spec, n_exp = 0, bias = F, variance = F, cowboy = F, specialisation = list(), bias_1_exp_down = F, bias_1_exp_up = F){
  
  # Function takes arguments:
  #   test_results: Matrix of test results
  #   prevalence: Value between 0 and 1 expressing the prevalence of the disease
  #   sens: Vector of sensitivities for the tests
  #   spec: Vector of specificities for the tests
  #   n_exp: Scalar of number of experts
  #   bias: Logical for whether to simulate bias
  #   variance: Logical for whether to simulate variance
  #   specialisation: Optional list of vectors for component tests that experts use
  #   
  # Function returns:
  #   prob: Matrix of probabilities for disease status
  
  if ((bias || variance) && (n_exp < 1)) {
    stop("If bias or variance is true, n_exp must be at least 1.")
  }
  if (length(sens) != length(spec)) {
    stop("Argument sens and spec are required to be of the same length.")
  }
  if (length(specialisation) != n_exp && length(specialisation) != 0){
    stop("Argument specialisation is optional. If specified it must match n_exp.")
  }
  
  test_results_list <- vector(mode = "list", length = n_exp)
  sens_list <- vector(mode = "list", length = n_exp)
  spec_list <- vector(mode = "list", length = n_exp)
  n_obs <- nrow(test_results)
  
  if (length(specialisation) == 0){
    for (expert in 1:n_exp){
      test_results_list[[expert]] <- test_results
      sens_list[[expert]] <- sens
      spec_list[[expert]] <- spec
    }
  }
  
  if (length(specialisation) == n_exp){
    for (expert in 1:n_exp){
      comp_test_expert <- specialisation[[expert]]
      test_results_list[[expert]] <- test_results[, comp_test_expert]
      sens_list[[expert]] <- sens[comp_test_expert]
      spec_list[[expert]] <- spec[comp_test_expert]
    }
  }
  
  prob_matrix <- matrix(nrow = n_obs, ncol = 0) #to bind results to
  
  for (k in 1:n_exp){
    test_results <- test_results_list[[k]]
    sens <- sens_list[[k]]
    spec <- spec_list[[k]]
    n_test <- length(sens)
    
    # Combine component test results into one disease classification
    potential_outcomes <- t(expand.grid(replicate(0:1, n = n_test, simplify = F)))
    
    # Probability of test pattern given disease present
    P_test_given_D1 <- sens^potential_outcomes *
      (1-sens)^(1-potential_outcomes)
    
    P_given_D1 <- prevalence * apply(P_test_given_D1, 2, prod)
    
    # Probability of test pattern given disease not present
    P_test_given_D0 <- spec^(1-potential_outcomes) *
      (1-spec)^potential_outcomes
    
    P_given_D0 <- (1 - prevalence) * apply(P_test_given_D0, 2, prod)
    
    # Bayes' theorem
    P_D1_given_pattern <- P_given_D1 / (P_given_D1 + P_given_D0)
    
    # Test outcomes can be considered binary number for test pattern
    BinToDec <- function(x) {
      # Function by Julius Vainora https://stackoverflow.com/a/12892614
      sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
    }
    
    # For function to choose properly, reverse the binary pattern
    binary_patterns <- test_results[, ncol(test_results):1]
    
    pattern_no <- apply(binary_patterns, 1, BinToDec) + 1
    
    if(!bias && !variance) { 
      P_D1_calibrated <- as.matrix(P_D1_given_pattern[pattern_no])
      
      prob <- P_D1_calibrated
    }
    
    if(bias){
      groups <- table(pattern_no)
      n_groups <- length(groups)
      group_to_pattern <- as.numeric(names(groups))
      
      P_D1_bias_exp <- matrix(nrow = n_exp, ncol = n_groups)
      
      for (i in 1:n_groups){
        group_prob <- P_D1_given_pattern[group_to_pattern[i]]
        P_D1_bias_exp[, i] <- rbeta(n = n_exp, 
                                    shape1 = n_obs*group_prob, 
                                    shape2 = n_obs*(1-group_prob))
      }
      if(!variance){  
        P_D1_bias <- t(P_D1_bias_exp[, match(pattern_no, group_to_pattern)])
        
        prob <- P_D1_bias
      }
    }
    
    if(bias_1_exp_down){
      groups <- table(pattern_no)
      n_groups <- length(groups)
      group_to_pattern <- as.numeric(names(groups))
      
      P_D1_bias_exp <- matrix(nrow = n_exp, ncol = n_groups)
      
      for (i in 1:n_groups){
        group_prob <- P_D1_given_pattern[group_to_pattern[i]]
        
        P_D1_bias_exp[1, i] <- qbeta(p=abs(runif(1) - 0.5), 
                                     shape1 = n_obs*group_prob, 
                                     shape2 = n_obs*(1-group_prob))
        
        P_D1_bias_exp[2:nrow(P_D1_bias_exp), i] <- group_prob
      }
      if(!variance){  
        P_D1_bias <- t(P_D1_bias_exp[, match(pattern_no, group_to_pattern)])
        
        prob <- P_D1_bias
      }
    }
    
    if(bias_1_exp_up){
      groups <- table(pattern_no)
      n_groups <- length(groups)
      group_to_pattern <- as.numeric(names(groups))
      
      P_D1_bias_exp <- matrix(nrow = n_exp, ncol = n_groups)
      
      for (i in 1:n_groups){
        group_prob <- P_D1_given_pattern[group_to_pattern[i]]
        
        P_D1_bias_exp[1, i] <- qbeta(p=(abs(runif(1) - 0.5) + 0.5), 
                                     shape1 = n_obs*group_prob, 
                                     shape2 = n_obs*(1-group_prob))
        
        P_D1_bias_exp[2:nrow(P_D1_bias_exp), i] <- group_prob
      }
      if(!variance){  
        P_D1_bias <- t(P_D1_bias_exp[, match(pattern_no, group_to_pattern)])
        
        prob <- P_D1_bias
      }
    }
    
    if(variance){
      groups <- table(pattern_no)
      n_groups <- length(groups)
      if(bias) {P_D1 <- P_D1_bias_exp} else {P_D1 <- rbind(t(replicate(n_exp, P_D1_given_pattern)))[, as.numeric(names(groups))]}
      
      group_estimates <- matrix(nrow = n_obs, ncol = 1)
      
      for (i in 1:n_groups){
        group_exp_prob <- P_D1[k, i]
        
        group_estimates[which(pattern_no == names(groups)[i]), 1] <- rbeta(n = groups[i], 
                                                                           shape1 = n_obs*group_exp_prob, 
                                                                           shape2 = n_obs*(1-group_exp_prob))
        
      }
      P_D1_variance <- group_estimates
      prob <- P_D1_variance
    }
    
    if(cowboy){
      if(!bias && !variance){P_D1_cowboy <- P_D1_calibrated}
      if(bias && !variance){P_D1_cowboy <- P_D1_bias}
      if(variance){P_D1_cowboy <- P_D1_variance}
      
      cowboy_exp <- sample(1:ncol(P_D1_cowboy), 1)
      
      P_D1_cowboy[, cowboy_exp] <- apply(matrix(P_D1_cowboy[ , cowboy_exp]), 1, cowboyAdjustment)
      
      prob <- P_D1_cowboy
      
    }
    prob_matrix <- cbind(prob_matrix, prob)
  }
  
  prob_mean <- apply(prob_matrix, 1, FUN = mean)
  prob_med <- apply(prob_matrix, 1, FUN = median)
  prob_min <- apply(prob_matrix, 1, FUN = min)
  prob_max <- apply(prob_matrix, 1, FUN = max)
  
  prob <- cbind(prob_mean,
                prob_med,
                prob_min,
                prob_max)
  
  return(prob)
}

cowboyAdjustment <- function(estimate){
  # Helper function to provide the cowboy adjustment in BTrule
  if (estimate > 0.5){result <- (estimate + 1)/2}
  else {result <- estimate/2}
  
  return(result)
}

getProb <- function(test_results, prevalence, sens, spec){
  
  # Function takes arguments:
  #   test_results: Matrix of test results
  #   prevalence: Value between 0 and 1 expressing the prevalence of the disease
  #   sens: Vector of sensitivities for the tests
  #   spec: Vector of specificities for the tests
  #   
  # Function returns:
  #   Data frame containing probabilities for test outcomes given disease status
  
  if (length(sens) != length(spec)) {
    stop("Argument sens and spec are required to be of the same length.")
  }
  n_test <- length(sens)
  
  # Combine component test results into one disease classification
  potential_outcomes <- t(expand.grid(replicate(0:1, n = n_test, simplify = F)))
  
  # Probability of test pattern if disease present
  P_test_given_D1 <- sens^potential_outcomes *
    (1-sens)^(1-potential_outcomes)
  
  # Probability of test pattern if disease not present
  P_test_given_D0 <- spec^(1-potential_outcomes) *
    (1-spec)^potential_outcomes
  
  return(data.frame("P_test_given_D0" = apply(P_test_given_D0, 2, prod),
                    "P_test_given_D1" = apply(P_test_given_D1, 2, prod)))
}
