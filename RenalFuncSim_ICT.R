#------------------------------------------------------------------------------
# Data generation
#------------------------------------------------------------------------------

#timeVect <- c(shape, scale, cens)
#ageVect <- c(interc, coef, shape1, shape2)
#comorbidVect <- c(intercLog, coefLog)
#severVect <- c(intercLog, coefAge, coefCov, coefComorbid)
#fnrenInVect <- c(interc, coefAge, coefComorbid)
#fnrenVect <- c(coefAge, coefCov, coefComorbid, coefSever)
#------------------------------------------------------------------------------

gendat <- function(N, timeVect, ageVect, comorbidVect, severVect,
                   fnrenInVect, fnrenVect)
{
  t <- rgamma(N, timeVect[1], timeVect[2])
  cov <- as.numeric(t <= timeVect[3])
  
  age <- ageVect[1] + ageVect[2]*rbeta(N, ageVect[3], ageVect[4])
  comorbid <- rbinom(N, 1, plogis(comorbidVect[1] + comorbidVect[2]*age))
  sever <- ifelse(cov==0, 0, rbinom(N, 1, plogis(severVect[1]
                                                    + severVect[2]*age
                                                    + severVect[3]*comorbid)))
  fnrenIn <- rnorm(N, mean=fnrenInVect[1]
                   + fnrenInVect[2]*age 
                   + fnrenInVect[3]*comorbid)
  fnren <- rnorm(N, mean=fnrenVect[1]
                 + fnrenVect[2]*fnrenIn
                 + fnrenVect[3]*age
                 + fnrenVect[4]*cov
                 + fnrenVect[5]*comorbid
                 + fnrenVect[6]*sever)
  
  return(data.frame(fnren=fnren,
                    fnrenIn=fnrenIn,
                    cov=cov,
                    age=age,
                    comorbid=comorbid,
                    sever=sever,
                    change=fnren-fnrenIn))
}

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

descriptive <- function(dat)
{
  plot(density(dat$fnren[dat$cov==1]), col="magenta", 
       ylim=c(0, 0.4),
       xlim=c(65, 80),
       main="fnren")
  lines(density(dat$fnren[dat$cov==0]), col="blue")
  lines(density(dat$fnrenIn[dat$cov==1]), col="green")
  lines(density(dat$fnrenIn[dat$cov==0]), col="red")
  legend(x="topright", cex=1.2,
         legend=c("pre, cov-", "pre, cov+", "post, cov-", "post, cov+"),
         fill=c("red", "green", "blue", "magenta"))
  
  plot(density((dat$fnrenIn-dat$fnren)[dat$cov==1]), col="blue",
       ylim=c(0, 0.35),
       xlim=c(-10, 10),
       main="fnren pre-post")
  lines(density((dat$fnrenIn-dat$fnren)[dat$cov==0]), col="orange")
  legend(x="topright", cex=1.2,
         legend=c("cov-", "cov+"),
         fill=c("orange", "blue"))
}

#------------------------------------------------------------------------------

set.seed(9898)
dat <- gendat(N=10^4, timeVect=c(2, 0.005, 200), 
              ageVect=c(-7.5, 92, 23.31, 3.33),
              comorbidVect=c(-5.3, 0.1), 
              severVect=c(-3.2, 0.1, 0.13),
              fnrenInVect=c(80, -0.1, -0.3), 
              fnrenVect=c(52, 0.3, -0.01, -0.4, -0.2, -0.3))
descriptive(dat)

