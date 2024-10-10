library(ggplot2)
library(gridExtra)

comp_test_sens <- list(rep(0.7, 4), rep(0.8, 4), c(0.6, 0.7, 0.8, 0.9))
comp_test_spec <- list(rep(0.7, 4), rep(0.8, 4), c(0.9, 0.8, 0.7, 0.6))

# specialisation_experts <- list(list(c(1,2,3), c(1,2,3), c(1,3,4), c(1,3,4)),
#                                list(c(1,2,3), c(1,2,3), c(1,2,3), c(1,3,4)),
#                                list(c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3)))

# Grid of scenarios without specialisation
grid <- expand.grid(n_obs = c(100, 360, 1000),
                    prevalence = c(0.2, 0.4, 0.5),
                    threshold = c(0.2, 0.5, 0.8),
                    n_exp = c(2, 3, 10),
                    sens_spec_comp = 1:length(comp_test_sens),
                    bias = c(F, T),
                    variance = c(F, T),
                    cowboy = c(F, T))

# Scenarios for illustrating low/high MSE
# low
# n_obs <- 360
# prevalence <- 0.5
# threshold <- 0.5
# n_exp <- 3
# sens_comp <- comp_test_sens[[2]]
# spec_comp <- comp_test_sens[[2]]
# bias <- T
# variance <- F
# cowboy <- F

# high
# n_obs <- 360
# prevalence <- 0.2
# threshold <- 0.5
# n_exp <- 3
# sens_comp <- comp_test_sens[[2]]
# spec_comp <- comp_test_sens[[2]]
# bias <- T
# variance <- T
# cowboy <- F

 # Grid of scenarios with specialisation
 grid <- expand.grid(n_obs = c(360, 1000),
                     prevalence = c(0.2, 0.4, 0.5),
                     threshold = c(0.5, 0.7, 0.8),
                     n_exp = c(2, 3, 10),
                     sens_spec_comp = 1:length(comp_test_sens),
                     bias = c(F, T),
                     variance = c(F, T),
                     cowboy = c(F, T),
                     specialisation = 1:length(specialisation_experts))
# 
# # Grid of scenarios with exactly one biased expert
# 
# comp_test_sens <- list(rep(0.7, 4), rep(0.8, 4), c(0.6, 0.7, 0.8, 0.9), c(0.6, 0.7, 0.8, 0.9))
# comp_test_spec <- list(rep(0.7, 4), rep(0.8, 4), c(0.9, 0.8, 0.7, 0.6), c(0.6, 0.7, 0.8, 0.9))
# sens_idx <- 0.8 # Sensitivity of index test
# spec_idx <- 0.8 # Specificity of index test
# recalc_variable <- seq(0, 1, 0.05) # Range of values for which to recalculate
# sens_recalc_fixed <- rep(sens_idx, length(recalc_variable))
# spec_recalc_fixed <- rep(spec_idx, length(recalc_variable))
# 
# grid_bias_down <- expand.grid(n_obs = c(360, 1000),
#                               prevalence = c(0.2, 0.4, 0.5),
#                               threshold = c(0.2, 0.3, 0.5),
#                               n_exp = c(2, 3, 10),
#                               sens_spec_comp = 1:length(comp_test_sens),
#                               bias_down = c(T),
#                               bias_up = c(F))
# 
# grid_bias_up <- expand.grid(n_obs = c(360, 1000),
#                             prevalence = c(0.2, 0.4, 0.5),
#                             threshold = c(0.2, 0.3, 0.5),
#                             n_exp = c(2, 3, 10),
#                             sens_spec_comp = 1:length(comp_test_sens),
#                             bias_down = c(F),
#                             bias_up = c(T))
# 
#grid <- rbind(grid_bias_down, grid_bias_up)

scenario_list <- list()
MSE_results <- matrix(nrow = 0, ncol = 16)
  
for (counter in 1:nrow(grid)){
  scenario <- grid[counter, ]
  n_obs <- scenario$n_obs # Number of observations per simulation
  prevalence <- scenario$prevalence
  threshold <- scenario$threshold # Classification threshold
  n_exp <- scenario$n_exp #length(specialisation_experts[[scenario$specialisation]]) # Number of experts to include in calculation
  sens_comp <- comp_test_sens[[scenario$sens_spec_comp]]# Sensitivities of component tests
  spec_comp <- comp_test_spec[[scenario$sens_spec_comp]]# Specificities of component tests

  bias <- scenario$bias # Logical indicating whether bias is simulated
  variance <- scenario$variance # Logical indicating whether variance is simulated
  cowboy <- scenario$cowboy # Logical indicating whether an expert should tend towards extreme estimates

  #  specialisation <- specialisation_experts[[scenario$specialisation]]

  #bias_down <- scenario$bias_down
  #bias_up <- scenario$bias_up

# Filepath for scenarios with or without specialisation
  fp <- paste0("EPsim_",
               "bias", bias,
               "variance", variance,
               "cowboy", cowboy,
               "obs", n_obs,
               "prev", prevalence*100,
               "thresh", threshold*100,
               "experts", n_exp,
               "sens", sens_comp[1]*100,
               "spec", spec_comp[1]*100,
#               "specialisation", scenario$specialisation,
               ".RDat")
  
  # # Filepath for exactly one biased expert
  # fp <- paste0("EPsim_",
  #              "bias_down", bias_down,
  #              "bias_up", bias_up,
  #              "obs", n_obs,
  #              "prev", prevalence*100,
  #              "thresh", threshold*100,
  #              "experts", n_exp,
  #              "sens", sens_comp[1]*100,
  #              "spec", spec_comp[1]*100,
  #              ".RDat")
  
  load(fp)
  
  n_sim <- length(seed)
  
  mean_sens <- numeric(21 * n_sim)
  median_sens <- numeric(21 * n_sim)
  min_sens <- numeric(21 * n_sim)
  max_sens <- numeric(21 * n_sim)
  
  for (df in 1:n_sim){
    mean_sens[((21 * (df-1))+1):((21 * (df-1))+21)] <- sens_results[[df]][[1]][[1]]
    median_sens[((21 * (df-1))+1):((21 * (df-1))+21)] <- sens_results[[df]][[2]][[1]]
    min_sens[((21 * (df-1))+1):((21 * (df-1))+21)] <- sens_results[[df]][[3]][[1]]
    max_sens[((21 * (df-1))+1):((21 * (df-1))+21)] <- sens_results[[df]][[4]][[1]]
  }
  
  mean_spec <- numeric(21 * n_sim)
  median_spec <- numeric(21 * n_sim)
  min_spec <- numeric(21 * n_sim)
  max_spec <- numeric(21 * n_sim)
  
  for (df in 1:n_sim){
    mean_spec[((21 * (df-1))+1):((21 * (df-1))+21)] <- spec_results[[df]][[1]][[2]]
    median_spec[((21 * (df-1))+1):((21 * (df-1))+21)] <- spec_results[[df]][[2]][[2]]
    min_spec[((21 * (df-1))+1):((21 * (df-1))+21)] <- spec_results[[df]][[3]][[2]]
    max_spec[((21 * (df-1))+1):((21 * (df-1))+21)] <- spec_results[[df]][[4]][[2]]
  }
  
  mean_dat_sens <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    mean_dat_sens[i, c(1,3)] <- quantile(na.omit(mean_sens[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    mean_dat_sens[i, 2] <- summary(na.omit(mean_sens[selection]))[4]
  }
  
  median_dat_sens <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    median_dat_sens[i, c(1,3)] <- quantile(na.omit(median_sens[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    median_dat_sens[i, 2] <- summary(na.omit(median_sens[selection]))[4]
  }
  
  min_dat_sens <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    min_dat_sens[i, c(1,3)] <- quantile(na.omit(min_sens[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    min_dat_sens[i, 2] <- summary(na.omit(min_sens[selection]))[4]
  }
  
  max_dat_sens <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    max_dat_sens[i, c(1,3)] <- quantile(na.omit(max_sens[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    max_dat_sens[i, 2] <- summary(na.omit(max_sens[selection]))[4]
  }
  
  mean_dat_spec <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    mean_dat_spec[i, c(1,3)] <- quantile(na.omit(mean_spec[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    mean_dat_spec[i, 2] <- summary(na.omit(mean_spec[selection]))[4]
  }
  
  median_dat_spec <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    median_dat_spec[i, c(1,3)] <- quantile(na.omit(median_spec[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    median_dat_spec[i, 2] <- summary(na.omit(median_spec[selection]))[4]
  }
  
  min_dat_spec <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    min_dat_spec[i, c(1,3)] <- quantile(na.omit(min_spec[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    min_dat_spec[i, 2] <- summary(na.omit(min_spec[selection]))[4]
  }
  
  max_dat_spec <- matrix(nrow = 21, ncol = 3)
  for (i in 1:21){
    selection <- (0:(n_sim-1) * 21) + i
    max_dat_spec[i, c(1,3)] <- quantile(na.omit(max_spec[selection]), probs = c(0.025, 0.5, 0.975))[c(1,3)]
    max_dat_spec[i, 2] <- summary(na.omit(max_spec[selection]))[4]
  }
  
  sens_dat <- data.frame(rbind(mean_dat_sens, median_dat_sens, min_dat_sens, max_dat_sens))
  spec_dat <- data.frame(rbind(mean_dat_spec, median_dat_spec, min_dat_spec, max_dat_spec))
  sens_dat$group <- c(rep("Mean", 21), rep("Median", 21), 
                      rep("Minimum", 21), rep("Maximum", 21))
  spec_dat$group <- c(rep("Mean", 21), rep("Median", 21), 
                      rep("Minimum", 21), rep("Maximum", 21))
  sens_dat$index <- c(rep(seq(0,1,0.05), 4))
  spec_dat$index <- c(rep(seq(0,1,0.05), 4))
  
  # Calculate results (MSE, bias, etc) and bind to a results array
  SE_sens <- (sens_dat$X2 - sens_dat$index)^2
  SE_spec <- (spec_dat$X2 - spec_dat$index)^2
  
  MSE_sens <- aggregate(SE_sens, sens_dat["group"], mean)[c(2,3,4,1),]
  MSE_spec <- aggregate(SE_spec, spec_dat["group"], mean)[c(2,3,4,1),]
  
  # Calculate upper bounds for results and bind to a results array
  consensusmethods <- c("Mean", "Median", "Minimum", "Maximum")
  SE_sens_UB <- numeric(4)
  SE_spec_UB <- numeric(4)
  for (i in 1:4){
    SE_sens_UB[i] <- quantile(SE_sens[sens_dat["group"] == consensusmethods[i]], probs = 0.975)
    SE_spec_UB[i] <- quantile(SE_spec[spec_dat["group"] == consensusmethods[i]], probs = 0.975)
  }

  
  scenario_list <- c(scenario_list, fp)
  MSE_results <- rbind(MSE_results, c(MSE_sens$x, MSE_spec$x, SE_sens_UB, SE_spec_UB))
  
}

colnames(MSE_results) <- c("Sens Mean", "Sens Median", "Sens Min", "Sens Max", "Spec Mean", "Spec Median", "Spec Min", "Spec Max", "Sens Mean Upper Bound", "Sens Median Upper Bound", "Sens Min Upper Bound", "Sens Max Upper Bound", "Spec Mean Upper Bound", "Spec Median Upper Bound", "Spec Min Upper Bound", "Spec Max Upper Bound")

MSE_results <- cbind(MSE_results, grid)

# Nested loop plot, code adapted from Rucker and Schwarzer 2014
nldata <- data.frame(MSE_results)
nldata$bias <- (nldata$bias)*1
nldata$variance <- (nldata$variance)*1
nldata$cowboy <- (nldata$cowboy)*1

## No systematic and no random differences
nldata_selection <- nldata[nldata$bias == F & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.5, 0.8),
                              labels = c("20%", "50%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.08), bty="n",
     xlab="3 x 2 x 3 x 3 x 3 x 3 = 486 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(350, 0.08,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Systematic differences, no random differences
nldata_selection <- nldata[nldata$bias == T & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.5, 0.8),
                              labels = c("20%", "50%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.08), bty="n",
     xlab="3 x 2 x 3 x 3 x 3 x 3 = 486 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(350, 0.08,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Systematic differences and random differences
nldata_selection <- nldata[nldata$bias == T & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.5, 0.8),
                              labels = c("20%", "50%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.08), bty="n",
     xlab="3 x 2 x 3 x 3 x 3 x 3 = 486 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(350, 0.08,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Random differences, no systematic differences
nldata_selection <- nldata[nldata$bias == F & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.5, 0.8),
                              labels = c("20%", "50%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.08), bty="n",
     xlab="3 x 2 x 3 x 3 x 3 x 3 = 486 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(350, 0.08,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()
















##########################################################
results <- data.frame("MSE" = c(as.matrix(MSE_results[, 1:8])),
                      "scenario" = rep(1:nrow(MSE_results), ncol(MSE_results)),
                      "mechanism" = c(rep("Sens mean", 1296), rep("Sens med", 1296), 
                                      rep("Sens min", 1296), rep("Sens max", 1296),
                                      rep("Spec mean", 1296), rep("Spec med", 1296), 
                                      rep("Spec min", 1296), rep("Spec max", 1296)))

results <- cbind(results, grid)

#save(results, file = "ExpertPanelSimResults.RData")
#load("ExpertPanelSimResults.RData")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = mechanism)) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = factor(n_obs))) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = factor(prevalence))) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = factor(threshold))) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = factor(n_exp))) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")

ggplot(results[1:5184, ], aes(x = scenario, y = MSE, color = factor(sens_spec_comp))) +
  geom_point(alpha = 0.25) + ggtitle("Sensitivity")


# Reorder for plots
results_sens <- results[1:5184, ]
results_mechanism <- results_sens[order(results_sens$mechanism), ]
results_n_obs <- results_sens[order(results_sens$n_obs), ]
results_prevalence <- results_sens[order(results_sens$prevalence), ]
results_threshold <- results_sens[order(results_sens$threshold), ]
results_n_exp <- results_sens[order(results_sens$n_exp), ]
results_sens_spec_comp <- results_sens[order(results_sens$sens_spec_comp), ]


results_spec <- results[5185:10368, ]
results_spec_mechanism <- results_spec[order(results_spec$mechanism), ]
results_spec_n_obs <- results_spec[order(results_spec$n_obs), ]
results_spec_prevalence <- results_spec[order(results_spec$prevalence), ]
results_spec_threshold <- results_spec[order(results_spec$threshold), ]
results_spec_n_exp <- results_spec[order(results_spec$n_exp), ]
results_spec_sens_spec_comp <- results_spec[order(results_spec$sens_spec_comp), ]

## MSE difference calculations
# Number of experts
results_sens_2_exp <- results_sens[results_sens$n_exp==2,]$MSE
results_sens_3_exp <- results_sens[results_sens$n_exp==3,]$MSE
results_sens_10_exp <- results_sens[results_sens$n_exp==10,]$MSE

results_spec_2_exp <- results_spec[results_spec$n_exp==2,]$MSE
results_spec_3_exp <- results_spec[results_spec$n_exp==3,]$MSE
results_spec_10_exp <- results_spec[results_spec$n_exp==10,]$MSE

quantile(results_sens_2_exp - results_sens_3_exp, probs = c(0.025, 0.5, 0.975))
quantile(results_sens_2_exp - results_sens_10_exp, probs = c(0.025, 0.5, 0.975))

quantile(results_spec_2_exp - results_spec_3_exp, probs = c(0.025, 0.5, 0.975))
quantile(results_spec_2_exp - results_spec_10_exp, probs = c(0.025, 0.5, 0.975))

# Number of participants
results_sens_360_obs <- results_sens[results_sens$n_obs==360,]$MSE
results_sens_1000_obs <- results_sens[results_sens$n_obs==1000,]$MSE

results_spec_360_obs <- results_spec[results_spec$n_obs==360,]$MSE
results_spec_1000_obs <- results_spec[results_spec$n_obs==1000,]$MSE

quantile(results_sens_360_obs - results_sens_1000_obs, probs = c(0.025, 0.5, 0.975))

quantile(results_spec_360_obs - results_spec_1000_obs, probs = c(0.025, 0.5, 0.975))

# Classification threshold
results_sens_thresh_20 <- results_sens[results_sens$threshold==0.2,]$MSE
results_sens_thresh_30 <- results_sens[results_sens$threshold==0.3,]$MSE
results_sens_thresh_50 <- results_sens[results_sens$threshold==0.5,]$MSE

results_spec_thresh_20 <- results_spec[results_spec$threshold==0.2,]$MSE
results_spec_thresh_30 <- results_spec[results_spec$threshold==0.3,]$MSE
results_spec_thresh_50 <- results_spec[results_spec$threshold==0.5,]$MSE

quantile(results_sens_thresh_50 - results_sens_thresh_20, probs = c(0.025, 0.5, 0.975))
quantile(results_sens_thresh_50 - results_sens_thresh_30, probs = c(0.025, 0.5, 0.975))

quantile(results_spec_thresh_50 - results_spec_thresh_20, probs = c(0.025, 0.5, 0.975))
quantile(results_spec_thresh_50 - results_spec_thresh_30, probs = c(0.025, 0.5, 0.975))

# Prevalence
results_sens_prev_20 <- results_sens[results_sens$prevalence==0.2,]$MSE
results_sens_prev_40 <- results_sens[results_sens$prevalence==0.4,]$MSE
results_sens_prev_50 <- results_sens[results_sens$prevalence==0.5,]$MSE

results_spec_prev_20 <- results_spec[results_spec$prevalence==0.2,]$MSE
results_spec_prev_40 <- results_spec[results_spec$prevalence==0.4,]$MSE
results_spec_prev_50 <- results_spec[results_spec$prevalence==0.5,]$MSE

quantile(results_sens_prev_50 - results_sens_prev_20, probs = c(0.025, 0.5, 0.975))
quantile(results_sens_prev_50 - results_sens_prev_40, probs = c(0.025, 0.5, 0.975))

quantile(results_spec_prev_50 - results_spec_prev_20, probs = c(0.025, 0.5, 0.975))
quantile(results_spec_prev_50 - results_spec_prev_40, probs = c(0.025, 0.5, 0.975))










# MSE plot for the consensus mechanism
plot_mechanism_var <- ggplot(results_mechanism[results_mechanism$variance == T, ], aes(x = 1:nrow(results_mechanism[results_mechanism$variance == T, ]), y = MSE, color = mechanism)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by consensus mechanism)") + ylab("MSE") + labs(col = "Consensus mechanism")

plot_mechanism_novar <- ggplot(results_mechanism[results_mechanism$variance == F, ], aes(x = 1:nrow(results_mechanism[results_mechanism$variance == F, ]), y = MSE, color = mechanism)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by consensus mechanism)") + ylab("MSE") + labs(col = "Consensus mechanism")

grid.arrange(plot_mechanism_var, plot_mechanism_novar)

# MSE plot for the number of observations
plot_n_obs_var <- ggplot(results_n_obs[results_n_obs$variance == T, ], aes(x = 1:nrow(results_n_obs[results_n_obs$variance == T, ]), y = MSE, color = factor(n_obs))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by number of observations)") + ylab("MSE") + labs(col = "# of observations")

plot_n_obs_novar <- ggplot(results_n_obs[results_n_obs$variance == F, ], aes(x = 1:nrow(results_n_obs[results_n_obs$variance == F, ]), y = MSE, color = factor(n_obs))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by number of observations)") + ylab("MSE") + labs(col = "# of observations")

grid.arrange(plot_n_obs_var, plot_n_obs_novar)

# MSE plot for the prevalence
plot_prevalence_var <- ggplot(results_prevalence[results_prevalence$variance == T, ], aes(x = 1:nrow(results_prevalence[results_prevalence$variance == T, ]), y = MSE, color = factor(prevalence))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by prevalence)") + ylab("MSE") + labs(col = "Prevalence")

plot_prevalence_novar <- ggplot(results_prevalence[results_prevalence$variance == F, ], aes(x = 1:nrow(results_prevalence[results_prevalence$variance == F, ]), y = MSE, color = factor(prevalence))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by prevalence)") + ylab("MSE") + labs(col = "Prevalence")

grid.arrange(plot_prevalence_var, plot_prevalence_novar)

# MSE plot for the classification threshold
plot_threshold_var <- ggplot(results_threshold[results_threshold$variance == T, ], aes(x = 1:nrow(results_threshold[results_threshold$variance == T, ]), y = MSE, color = factor(threshold))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by classification threshold)") + ylab("MSE") + labs(col = "Threshold")

plot_threshold_novar <- ggplot(results_threshold[results_threshold$variance == F, ], aes(x = 1:nrow(results_threshold[results_threshold$variance == F, ]), y = MSE, color = factor(threshold))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by classification threshold)") + ylab("MSE") + labs(col = "Threshold")

grid.arrange(plot_threshold_var, plot_threshold_novar)

# MSE Plot for the number of experts
plot_n_exp_var <- ggplot(results_n_exp[results_n_exp$variance == T, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == T, ]), y = MSE, color = factor(n_exp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_n_exp_novar <- ggplot(results_n_exp[results_n_exp$variance == F, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == F, ]), y = MSE, color = factor(n_exp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

grid.arrange(plot_n_exp_var, plot_n_exp_novar)

# MSE plot for sensitivity and specificity
plot_sens_spec_comp_var <- ggplot(results_sens_spec_comp[results_sens_spec_comp$variance == T, ], aes(x = 1:nrow(results_sens_spec_comp[results_sens_spec_comp$variance == T, ]), y = MSE, color = factor(sens_spec_comp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by sensitivity and specificity)") + ylab("MSE") + labs(col = "Sensitivity and specificity")

plot_sens_spec_comp_novar <- ggplot(results_sens_spec_comp[results_sens_spec_comp$variance == F, ], aes(x = 1:nrow(results_sens_spec_comp[results_sens_spec_comp$variance == F, ]), y = MSE, color = factor(sens_spec_comp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by sensitivity and specificity)") + ylab("MSE") + labs(col = "Sensitivity and specificity")

grid.arrange(plot_sens_spec_comp_var, plot_sens_spec_comp_novar)

# MSE plot for specialisation
ggplot(results[results$variance == T, ], aes(x = 1:nrow(results[results$variance == T, ]), y = Max, color = factor(specialisation))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Maximum probability)") + xlab("Simulation (ordered by specialisation)") + ylab("MSE") + labs(col = "Specialisation") + ylim(c(0.0445, 0.046))

# MSE plot for specialisation (highlights)
ggplot(results[1:24, ], aes(x = rep(1:8, 3), y = Median, color = factor(specialisation))) +
  geom_point(size = 3, alpha = 0.5) + ggtitle("Error in sensitivity (Median probability)") + xlab("Simulation") + ylab("MSE") + labs(col = "Specialisation")

# Plot one scenario
ggplot_se <- ggplot(sens_dat, aes(x = index, y = X2, group= group)) +
  geom_point(aes(group=group, colour=group)) +
  geom_line(aes(group=group, colour=group)) +
  geom_abline(slope=1,intercept=0, colour="black") +
  geom_ribbon(aes(ymin = X1, ymax = X3), alpha = 0.1) +
  xlim(0,1) +
  ylim(0,1) +
  xlab("True sensitivity") +
  ylab("Observed sensitivity")

ggplot_sp <- ggplot(spec_dat,
       aes(x = index,
           y = X2,
           group= group)) +
  geom_point(aes(group=group, colour=group)) +
  geom_line(aes(group=group, colour=group)) +
  geom_abline(slope=1,intercept=0, colour="black") +
  geom_ribbon(aes(ymin = X1, ymax = X3), alpha = 0.1) +
  xlim(0,1) +
  ylim(0,1) +
  xlab("True specificity") +
  ylab("Observed specificity")

grid.arrange(ggplot_se, ggplot_sp, ncol = 2)

# Separate plots for different levels

pdf("Figure_Old.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")

# MSE Plot for the number of experts
plot_n_exp_var <- ggplot(results_n_exp[results_n_exp$variance == T, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == T, ]), y = MSE, color = factor(n_exp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_n_exp_novar <- ggplot(results_n_exp[results_n_exp$variance == F, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == F, ]), y = MSE, color = factor(n_exp))) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

grid.arrange(plot_n_exp_var, plot_n_exp_novar)

dev.off()

pdf("Figure_Separate.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")

plot_2_exp_var <- ggplot(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 2, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 2, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only, 2 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_2_exp_novar <- ggplot(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 2, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 2, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded, 2 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_3_exp_var <- ggplot(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 3, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 3, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only, 3 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_3_exp_novar <- ggplot(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 3, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 3, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded, 3 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_10_exp_var <- ggplot(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 10, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == T & results_n_exp$n_exp == 10, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations only, 10 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")

plot_10_exp_novar <- ggplot(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 10, ], aes(x = 1:nrow(results_n_exp[results_n_exp$variance == F & results_n_exp$n_exp == 10, ]), y = MSE)) +
  geom_point(alpha = 0.5) + ggtitle("Sensitivity (Variance simulations excluded, 10 experts)") + xlab("Simulation (ordered by number of experts in panel)") + ylab("MSE") + labs(col = "# of experts")


grid.arrange(plot_2_exp_var, plot_3_exp_var, plot_10_exp_var)
grid.arrange(plot_2_exp_novar, plot_3_exp_novar, plot_10_exp_novar)
dev.off()

# Nested loop plot, code adapted from Rucker and Schwarzer 2014

## No systematic and no random differences
nldata <- data.frame(MSE_results)
nldata$bias <- (nldata$bias)*1
nldata$variance <- (nldata$variance)*1
nldata$cowboy <- (nldata$cowboy)*1

nldata_selection <- nldata[nldata$bias == F & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.5, 0.7, 0.8),
                              labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot_no_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.05), bty="n",
     xlab="4 x 3 x 3 x 3 x 3 x 2 = 648 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(500, 0.05,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Systematic differences, no random differences
nldata_selection <- nldata[nldata$bias == T & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.5, 0.7, 0.8),
                              labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot_yes_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.05), bty="n",
     xlab="4 x 3 x 3 x 3 x 3 x 2 = 648 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(500, 0.05,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Systematic differences and random differences
nldata_selection <- nldata[nldata$bias == T & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.5, 0.7, 0.8),
                              labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot_yes_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.05), bty="n",
     xlab="4 x 3 x 3 x 3 x 3 x 2 = 648 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(500, 0.05,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

# Random differences, no systematic differences
nldata_selection <- nldata[nldata$bias == F & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.5, 0.7, 0.8),
                              labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot_no_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.05), bty="n",
     xlab="4 x 3 x 3 x 3 x 3 x 2 = 648 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.035, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(500, 0.05,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

############# More nested loop plots
plotdata1 <- nestedloop(nldata,
                     varnames=c("variance", "bias", "sens_spec_comp", "n_exp", "prevalence", "threshold", "n_obs", "cowboy"),
                     varlabels=
                       c("Random Error", "Systematic error",
                         "Sensitivity and specificity of component tests", "# of experts",
                         "Target condition prevalence", "Classification threshold", 
                         "# of participants", "Charismatic expert"),
                     sign=c(1, 1, 1, 1, 1, 1, 1, 1))

plotdata1$bias <- factor(plotdata1$bias,
                             levels = c(0, 1),
                             labels = c("No", "Yes"))

plotdata1$variance <- factor(plotdata1$variance,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

plotdata1$cowboy <- factor(plotdata1$cowboy,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                         levels = c(0.2, 0.4, 0.5),
                         labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                               levels = c(0.5, 0.7, 0.8),
                               labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                               levels = c(1, 2, 3, 4),
                               labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     log=c("y"), type="n",
     ylim=c(0.000001, 0.12), bty="n",
     xlab="2 x 2 x 4 x 3 x 3 x 3 x 3 x 2 = 2592 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"))
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=0.000001, ymax.refline=0.00005,
      cex.ref=0.7)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s")   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s")    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s")   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s")      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty = "dashed")           # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty = "dashed")      # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty = "dashed")      # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty = "dashed")     # Specificity estimate, maximum
##
legend(1000, 0.0005,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum"
         )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()

# Nested loop plots, split by bias and variance
nldata <- data.frame(MSE_results)
nldata$bias <- (nldata$bias)*1
nldata$variance <- (nldata$variance)*1
nldata$cowboy <- (nldata$cowboy)*1

nldata_selection <- nldata[nldata$bias == F & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_no_variance.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.12), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.045, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Sens.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.12,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == T & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_no_variance.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.30), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Sens.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.30,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == F & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_yes_variance.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.09, 0.45), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.09, ymax.refline=0.03,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Sens.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.5,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == T & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_yes_variance.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.09, 0.45), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.09, ymax.refline=0.03,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Sens.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.5,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()

# Nested loop plots, split by bias and variance
nldata_selection <- nldata[nldata$bias == F & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_no_variance_spec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Spec.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.07), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.045, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Spec.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.07,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == T & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_no_variance_spec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.05, 0.07), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Spec.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.07,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == F & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_yes_variance_spec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.05, 0.2), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0.0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Spec.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.2,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()


nldata_selection <- nldata[nldata$bias == T & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_yes_variance_spec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Spec.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.05, 0.2), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0.0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Spec.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Spec.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Sensitivity estimate, median
lines(plotdata1$Spec.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Spec.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Sensitivity estimate, maximum
#lines(plotdata1$Spec.Mean, col="red", type="s")           # Specificity estimate, mean
#lines(plotdata1$Spec.Median, col="violet", type="s")      # Specificity estimate, median
#lines(plotdata1$Spec.Min, col="gold", type="s")      # Specificity estimate, minimum
#lines(plotdata1$Spec.Max, col="purple", type="s")     # Specificity estimate, maximum
##
legend(0, 0.2,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green",
             "blue"), #, "darkgray", "violet", "gold", "purple"
       cex=0.9,
       bty="n",
       c("Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum"
       )) #, "Specificity, mean", "Specificity, median", "Specificity, minimum", "Specificity, maximum"
##
dev.off()

nldata_selection <- nldata[nldata$bias == T & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.5, 0.7, 0.8),
                              labels = c("50%", "70%", "80%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("70%", "80%", "Mirrored", "Growing"))


pdf("Figure_Nested_Loop_plot_yes_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.10), bty="n",
     xlab="4 x 3 x 3 x 3 x 3 x 2 = 648 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(250, 0.10,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

pdf("Figure_Nested_Loop_plot_yes_bias_no_variance_sensspec_UB.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.30), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean.Upper.Bound, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median.Upper.Bound, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min.Upper.Bound, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max.Upper.Bound, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean.Upper.Bound, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median.Upper.Bound, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min.Upper.Bound, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max.Upper.Bound, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(0, 0.30,
       lwd=c(2, rep(1, 6)),
       col=c("black", "red", "green","blue"),
       cex=0.9,
       bty="n",
       c("Mean", "Median",
         "Minimum", "Maximum")
       )

dev.off()




nldata_selection <- nldata[nldata$bias == F & nldata$variance == F, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))



pdf("Figure_Nested_Loop_plot_no_bias_no_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.035, 0.05), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.04, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(250, 0.05,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

nldata_selection <- nldata[nldata$bias == F & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_no_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.13), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(250, 0.13,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

nldata_selection <- nldata[nldata$bias == T & nldata$variance == T, ]

plotdata1 <- nestedloop(nldata_selection,
                        varnames=c("sens_spec_comp", "cowboy", "prevalence", "threshold", "n_obs", "n_exp"),
                        varlabels=
                          c(
                            "Sensitivity and specificity of component tests", "Overconfident expert",
                            "Target condition prevalence", "Classification threshold", 
                            "# of participants", "# of experts"),
                        sign=c(1, 1, 1, 1, 1, 1))


plotdata1$cowboy <- factor(plotdata1$cowboy,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

plotdata1$prevalence <- factor(plotdata1$prevalence,
                               levels = c(0.2, 0.4, 0.5),
                               labels = c("20%", "40%", "50%"))

plotdata1$threshold <- factor(plotdata1$threshold,
                              levels = c(0.2, 0.3, 0.5),
                              labels = c("20%", "30%", "50%"))

plotdata1$sens_spec_comp <- factor(plotdata1$sens_spec_comp,
                                   levels = c(1, 2, 3),
                                   labels = c("70%", "80%", "Mirrored"))


pdf("Figure_Nested_Loop_plot_yes_bias_yes_variance_sensspec.pdf", paper="a4r", width=18, height=15)
##
par(pty="m")
##
## Create skeleton of nested-loop plot using standard R plot function
##
plot(plotdata1$Sens.Mean,
     #log=c("y"), 
     type="n",
     ylim=c(-0.045, 0.13), bty="n",
     xlab="3 x 3 x 3 x 3 x 2 x 2 = 324 ordered scenarios",
     ylab="MSE in accuracy estimate",
     las=1, xaxt="n")
##
## Add vertical lines (using R function lines.nestedloop)
##
lines(plotdata1, col=c("#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE"), log = F)
##
## Add reference lines (using R function lines.nestedloop)
##
lines(plotdata1, which="r",
      ymin.refline=-0.05, ymax.refline=0,
      cex.ref=0.7, log = F)
##
## Estimates and legend (using standard R functions lines and legend)
##
lines(plotdata1$Sens.Mean, col="black", type="s", log = F)   # Sensitivity estimate, mean
lines(plotdata1$Sens.Median, col="red", type="s", log = F)    # Sensitivity estimate, median
lines(plotdata1$Sens.Min, col="green", type="s", log = F)   # Sensitivity estimate, minimum
lines(plotdata1$Sens.Max, col="blue", type="s", log = F)      # Sensitivity estimate, maximum
lines(plotdata1$Spec.Mean, col="black", lty="dashed", log = F)   # Specificity estimate, mean
lines(plotdata1$Spec.Median, col="red", lty="dashed", log = F)    # Specificity estimate, median
lines(plotdata1$Spec.Min, col="green", lty="dashed", log = F)   # Specificity estimate, minimum
lines(plotdata1$Spec.Max, col="blue", lty="dashed", log = F)      # Specificity estimate, maximum
##
legend(250, 0.13,
       lwd=c(1),
       col=c("black", "red", "green","blue", "black", "red", "green","blue"),
       cex=0.9,
       lty=c("solid", "solid", "solid", "solid", "dashed", "dashed", "dashed", "dashed"),
       bty="n",
       c("Sensitivity, mean", "Sensitivity, median",
         "Sensitivity, minimum", "Sensitivity, maximum", 
         "Specificity, mean", "Specificity, median",
         "Specificity, minimum", "Specificity, maximum")
)
##
dev.off()

