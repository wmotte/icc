#!/usr/bin/env Rscript
#
# Wim Otte
#
# One-sample intraclass correlation (ICC)
#
# INFO: https://bookdown.org/anshul302/HE902-MGHIHP-Spring2020/Random.html
#
################################################################################

library( "lme4" )
library( "merDeriv" ) # req. to determine 95% CI's for glmer random effects
library( "parameters" )

################################################################################
# BEGIN FUNCTIONS
################################################################################

###
# Get data.
##
get_data <- function()
{
    # make data
    study <- c(50,53,55,53,57,60,63,65,63,67,70,73,75,73,77)
    mcat <- c(77,76,77,78,79,73,74,73,75,75,63,64,65,66,70)
    classroom <- c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C")
    tqual <- c(8,8,8,8,8,7,7,7,7,7,2,2,2,2,2)
    mcat_bin <- rep( 0, length( mcat ) )
    mcat_bin[1:9] <- 1
    
    # df
    prep <- data.frame( study, mcat, mcat_bin, classroom, tqual )
    
    return( prep )
}

###
# ICC rho for logistic regression models
##
get_icc_rho_logistic <- function( group_var )
{
    # residual variance for logistic regression is fixed.
    # Check => Contemp Clin Trials. 2012 Sep;33(5):869-80. doi: 10.1016/j.cct.2012.05.004.
    resid_var <- ( pi ^ 2 ) / 3
    
    # The intraclass correlation ρ : the expected correlation between two randomly drawn units that are in the same group.
    icc_rho <- round( group_var / ( group_var + resid_var ), 3 )
    
    return( icc_rho )
}

################################################################################
# END FUNCTIONS
################################################################################

# outdir
outdir <- 'out.00.icc'
dir.create( outdir, showWarnings = FALSE )

# data
data <- get_data()

# null-model
#randnull <- lme4::lmer( mcat ~ 1 + ( 1 | classroom ), data = prep )
#sum <- summary( randnull )

# get group variance
#group_var <- attr( sum$varcor$classroom, 'stddev' )^2
#resid_var <- attr( sum$varcor, 'sc' )^2

# The intraclass correlation ρ : the expected correlation between two randomly drawn units that are in the same group.
#icc_rho <- round( group_var / ( group_var + resid_var ), 3 )


## BINARY ##


# fit generalized linear mixed model
summary( model <- lme4::glmer( mcat_bin ~ 1 + ( 1 | classroom ), data = data, family = 'binomial' ) )


# fixed and random parameters with 95% CI interval computed using a Wald z-distribution approximation.
parameters <- parameters::model_parameters( model )

# get group variance
group_var_mean <- ( parameters$Coefficient^2 )[ 2 ]
group_var_ci_low <- ( parameters$CI_low^2 )[ 2 ]
group_var_ci_high <- ( parameters$CI_high^2 )[ 2 ]

# get icc with 95% CI
icc <- data.frame( rho = get_icc_rho_logistic( group_var_mean ),
                    rho_CI_low = get_icc_rho_logistic( group_var_ci_low ),
                    rho_CI_high = get_icc_rho_logistic( group_var_ci_high ) )

