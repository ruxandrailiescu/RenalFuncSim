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


gendat <- function(N, mean1, sd1, mean2, sd2, k)
{
  correlation_coeffs <- numeric(k)
  
  for (i in 1:k)
  {
    y0 <- rnorm(N, mean=mean1, sd=sd1)
    y1 <- rnorm(N, mean=mean2, sd=sd2)
    
    correlation_coeffs[i] <- cor(y0, y1-y0)
  }
  
  summary(correlation_coeffs)
  plot(density(correlation_coeffs))
  return(correlation_coeffs)
}


#------------------------------------------------------------------------------
# Scenario 1: similar variances
#             obs: r ~= -1/sqrt(2)
#                  negative correlation between baseline value and change
#------------------------------------------------------------------------------

set.seed(123)
corr <- gendat(1000, 50, 2, 15, 2, 10000)

#------------------------------------------------------------------------------
# Scenario 2: dissimilar variances
#             obs: r != +-1/sqrt(2)
#------------------------------------------------------------------------------

corr <- gendat(1000, 50, 2.5, 15, 2, 10000)
