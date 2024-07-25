#------------------------------------------------------------------------------
# Change scores Analysis
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Simulation 1: Measuring correlation coefficient [E(r)] between either of the
#               parent variables [Y_0/Y_1] and their difference [Y_1 - Y_0]
#               ---------------------------------------------------------------
#               Scenario 1: similar variances
#               Scenario 2: dissimilar variances
#------------------------------------------------------------------------------


gendat <- function(N, mean1, sd1, mean2, sd2, k, 
                   type="corr")
{
  correlation_coeffs <- numeric(k)
  correlation_coeffsLR <- numeric(k)
  
  for (i in 1:k)
  {
    y0 <- rnorm(N, mean=mean1, sd=sd1)
    y1 <- rnorm(N, mean=mean2, sd=sd2)

    correlation_coeffs[i] <- cor(y0, y1-y0)
    
    change <- y1-y0
    scaled_change <- scale(change)
    scaled_y0 <- scale(y0)
    correlation_coeffsLR[i] <- coef(lm(scaled_change ~ scaled_y0))["scaled_y0"] 
    
  }
  
  summary(correlation_coeffs)                      ###>>> aceast output nu va fi afisat cand chemi functia (plot-ul de mai jos insa da)
  plot(density(correlation_coeffs))                
  if(type=="corr"){return(correlation_coeffs)}
  if(type=="LR"){return(correlation_coeffsLR)}
}


#------------------------------------------------------------------------------
# Scenario 1: similar variances
#             obs: r ~= -1/sqrt(2)
#                  negative correlation between baseline value and change
#------------------------------------------------------------------------------

set.seed(123)
corr <- gendat(1000, 50, 2, 15, 2, 10000)

set.seed(123) ###>>>acelasi seed ca sa am exact aceleasi date generate la fiecare iteratie. difera doar modul de calcul al corelatiei
LR <- gendat(1000, 50, 2, 15, 2, 10000,type="LR")

summary(corr-LR)

#------------------------------------------------------------------------------
# Scenario 2: dissimilar variances
#             obs: r != +-1/sqrt(2)
#------------------------------------------------------------------------------

corr <- gendat(1000, 50, 2.5, 15, 2, 10000)
