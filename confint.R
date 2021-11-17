# Function for calculating CIs of retention1 and retention7.
# This function will not work correctly for sum_gamerounds.

confint <- function(variable){
  # Argument: Insert variable name of choice (either retention_1 or retention_7)
  library(tidyverse) # Import Tidyverse here, in case not imported some other place. 
  
  conversion_subset_control <- data %>% filter(version == "gate_30" & variable == 1)
  conversions_control <- nrow(conversion_subset_control)
  users_control <- nrow(data %>% filter(version == "gate_30"))
  conv_rate_control <-  (conversions_control/users_control)

  conversion_subset_treat <- data %>% filter(version == "gate_40" & variable == 1)
  conversions_treat <- nrow(conversion_subset_treat)
  users_treat <- nrow(data %>% filter(version == "gate_40"))
  conv_rate_treat <-  (conversions_treat/users_treat)
  
  # Compute CIs.
  # c(-qnorm(.975), qnorm(.975))    #95% confidence interval
  X_hat_treat <- conversions_treat/users_treat
  se_hat_treat <- sqrt(X_hat_treat*(1-X_hat_treat)/users_treat)
  
  X_hat_control <- conversions_control/users_control
  se_hat_control <- sqrt(X_hat_control*(1-X_hat_control)/users_control)
  
  ci_treat <- c(X_hat_treat - qnorm(0.975)*se_hat_treat, X_hat_treat + qnorm(0.975)*se_hat_treat)
  ci_control <- c(X_hat_control - qnorm(0.975)*se_hat_control, X_hat_control + qnorm(0.975)*se_hat_control)
  
  return(cbind(ci_treat, ci_control))
}
