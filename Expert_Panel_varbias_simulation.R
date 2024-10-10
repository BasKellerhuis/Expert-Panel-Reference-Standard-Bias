source("Expert_Panel_Functions.R")

set.seed(1)
n_sim <- 10^3 # Number of simulations per scenario

comp_test_sens <- list(rep(0.7, 4), rep(0.8, 4), c(0.6, 0.7, 0.8, 0.9), c(0.6, 0.7, 0.8, 0.9))
comp_test_spec <- list(rep(0.7, 4), rep(0.8, 4), c(0.9, 0.8, 0.7, 0.6), c(0.6, 0.7, 0.8, 0.9))
sens_idx <- 0.8 # Sensitivity of index test
spec_idx <- 0.8 # Specificity of index test
recalc_variable <- seq(0, 1, 0.05) # Range of values for which to recalculate
sens_recalc_fixed <- rep(sens_idx, length(recalc_variable))
spec_recalc_fixed <- rep(spec_idx, length(recalc_variable))
#specialisation_experts <- list(list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4)))
#specialisation_experts <- list(list(c(1,2,3), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(2,3,4)),
#                               list(c(1,2,3), c(2,3,4), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(2,3,4), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(1,2,3), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3)),
#                               list(c(2,3,4), c(2,3,4), c(2,3,4), c(2,3,4), c(2,3,4))
#                               )

#specialisation_experts <- list(list(c(1,2,3), c(1,2,3), c(2,3,4), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(1,2,3), c(2,3,4)),
#                               list(c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3)))

grid.full.factorial <- expand.grid(n_obs = c(100, 360, 1000),
                    prevalence = c(0.2, 0.4, 0.5),
                    threshold = c(0.5, 0.7, 0.8),
                    n_exp = c(2, 3, 10),
                    sens_spec_comp = 1:length(comp_test_sens),
                    bias = c(F, T),
                    variance = c(F, T),
                    cowboy = c(F, T)#,
#                    specialisation = 1:length(specialisation_experts)
                    )

grid <- grid.full.factorial

for (counter in 1:nrow(grid)){
  scenario <- grid[counter, ]
  n_obs <- scenario$n_obs # Number of observations per simulation
  prevalence <- scenario$prevalence
  threshold <- scenario$threshold # Classification threshold
  n_exp <- scenario$n_exp #length(specialisation_experts[[scenario$specialisation]])#scenario$n_exp # Number of experts to include in calculation
  sens_comp <- comp_test_sens[[scenario$sens_spec_comp]]# Sensitivities of component tests
  spec_comp <- comp_test_spec[[scenario$sens_spec_comp]]# Specificities of component tests
  bias <- scenario$bias # Logical indicating whether bias is simulated
  variance <- scenario$variance # Logical indicating whether variance is simulated
  cowboy <- scenario$cowboy # Logical indicating whether an expert should tend towards extreme estimates
#  specialisation <- specialisation_experts[[scenario$specialisation]]
  
  # Declare result variables
  seed <- list()
  sens_results <- list()
  spec_results <- list()
  
  # Filepath to store results
  fp_results <- paste0("EPsim_",
                   "bias", bias,
                   "variance", variance,
                   "cowboy", cowboy,
                   "obs", n_obs,
                   "prev", prevalence*100,
                   "thresh", threshold*100,
                   "experts", n_exp,
                   "sens", sens_comp[1]*100,
                   "spec", spec_comp[1]*100,
#                   "specialisation", scenario$specialisation,
                   ".RDat")
  
  for (sim in 1:n_sim){
    # The state is in .Random.seed and can be stored
    seed[[sim]] <- .Random.seed
    
    # Sample disease status from Bernoulli distribution
    disease_status <- rbinom(n = n_obs,
                             size = 1,
                             prob = prevalence)
    
    # Simulate test results
    comp_test_results <- SimTest(disease_status = disease_status, 
                                 sens = sens_comp, 
                                 spec = spec_comp)
    
    idx_test_results <- SimTest(disease_status = disease_status,
                                sens = sens_idx,
                                spec = spec_idx)
    
    # Simulate expert decision
    EPprob <- BTrule(test_results = comp_test_results,
                     sens = sens_comp,
                     spec = spec_comp, 
                     prevalence = prevalence,
                     n_exp = n_exp,
                     bias = bias,
                     variance = variance,
                     cowboy = cowboy,
#                     specialisation = specialisation
                     )
    
    # Sens and spec recalculation
    sens_recalc <- list()
    spec_recalc <- list()
    
    for (i in 1:ncol(EPprob)){
      sens_recalc[[i]] <- SensSpecRecalc(true_class = disease_status,
                                    predicted_class = ((EPprob[, i] > threshold)*1),
                                    sens = recalc_variable,
                                    spec = spec_recalc_fixed)
      
      spec_recalc[[i]] <- SensSpecRecalc(true_class = disease_status,
                                    predicted_class = ((EPprob[, i] > threshold)*1),
                                    sens = sens_recalc_fixed,
                                    spec = recalc_variable)
    }
    sens_results[[sim]] <- sens_recalc
    spec_results[[sim]] <- spec_recalc
  }
  
  # Storing results
  save(list = c("sens_results", "spec_results", "seed"), 
       file = fp_results)
  
  message("Number: ", counter, "/", nrow(grid), " finished.")
}


# Single biased expert in panel

source("Expert_Panel_Functions.R")

set.seed(1)
n_sim <- 10^3 # Number of simulations per scenario

comp_test_sens <- list(rep(0.7, 4), rep(0.8, 4), c(0.6, 0.7, 0.8, 0.9), c(0.6, 0.7, 0.8, 0.9))
comp_test_spec <- list(rep(0.7, 4), rep(0.8, 4), c(0.9, 0.8, 0.7, 0.6), c(0.6, 0.7, 0.8, 0.9))
sens_idx <- 0.8 # Sensitivity of index test
spec_idx <- 0.8 # Specificity of index test
recalc_variable <- seq(0, 1, 0.05) # Range of values for which to recalculate
sens_recalc_fixed <- rep(sens_idx, length(recalc_variable))
spec_recalc_fixed <- rep(spec_idx, length(recalc_variable))

grid_bias_down <- expand.grid(n_obs = c(360, 1000),
                                   prevalence = c(0.2, 0.4, 0.5),
                                   threshold = c(0.2, 0.3, 0.5),
                                   n_exp = c(2, 3, 10),
                                   sens_spec_comp = 1:length(comp_test_sens),
                                   bias_down = c(T),
                                   bias_up = c(F))

grid_bias_up <- expand.grid(n_obs = c(360, 1000),
                              prevalence = c(0.2, 0.4, 0.5),
                              threshold = c(0.2, 0.3, 0.5),
                              n_exp = c(2, 3, 10),
                              sens_spec_comp = 1:length(comp_test_sens),
                              bias_down = c(F),
                              bias_up = c(T))

grid <- rbind(grid_bias_down, grid_bias_up)

for (counter in 1:nrow(grid)){
  scenario <- grid[counter, ]
  n_obs <- scenario$n_obs # Number of observations per simulation
  prevalence <- scenario$prevalence
  threshold <- scenario$threshold # Classification threshold
  n_exp <- scenario$n_exp #scenario$n_exp # Number of experts to include in calculation
  sens_comp <- comp_test_sens[[scenario$sens_spec_comp]]# Sensitivities of component tests
  spec_comp <- comp_test_spec[[scenario$sens_spec_comp]]# Specificities of component tests
  bias_down <- scenario$bias_down # Logical indicating whether bias is simulated
  bias_up <- scenario$bias_up # Logical indicating whether bias is simulated
  
  # Declare result variables
  seed <- list()
  sens_results <- list()
  spec_results <- list()
  
  # Filepath to store results
  fp_results <- paste0("EPsim_",
                       "bias_down", bias_down,
                       "bias_up", bias_up,
                       "obs", n_obs,
                       "prev", prevalence*100,
                       "thresh", threshold*100,
                       "experts", n_exp,
                       "sens", sens_comp[1]*100,
                       "spec", spec_comp[1]*100,
                       ".RDat")
  
  for (sim in 1:n_sim){
    # The state is in .Random.seed and can be stored
    seed[[sim]] <- .Random.seed
    
    # Sample disease status from Bernoulli distribution
    disease_status <- rbinom(n = n_obs,
                             size = 1,
                             prob = prevalence)
    
    # Simulate test results
    comp_test_results <- SimTest(disease_status = disease_status, 
                                 sens = sens_comp, 
                                 spec = spec_comp)
    
    idx_test_results <- SimTest(disease_status = disease_status,
                                sens = sens_idx,
                                spec = spec_idx)
    
    # Simulate expert decision
    EPprob <- BTrule(test_results = comp_test_results,
                     sens = sens_comp,
                     spec = spec_comp, 
                     prevalence = prevalence,
                     n_exp = n_exp,
                     bias_1_exp_down = bias_down,
                     bias_1_exp_up = bias_up)
    
    # Sens and spec recalculation
    sens_recalc <- list()
    spec_recalc <- list()
    
    for (i in 1:ncol(EPprob)){
      sens_recalc[[i]] <- SensSpecRecalc(true_class = disease_status,
                                         predicted_class = ((EPprob[, i] > threshold)*1),
                                         sens = recalc_variable,
                                         spec = spec_recalc_fixed)
      
      spec_recalc[[i]] <- SensSpecRecalc(true_class = disease_status,
                                         predicted_class = ((EPprob[, i] > threshold)*1),
                                         sens = sens_recalc_fixed,
                                         spec = recalc_variable)
    }
    sens_results[[sim]] <- sens_recalc
    spec_results[[sim]] <- spec_recalc
  }
  
  # Storing results
  save(list = c("sens_results", "spec_results", "seed"), 
       file = fp_results)
  
  message("Number: ", counter, "/", nrow(grid), " finished.")
}
