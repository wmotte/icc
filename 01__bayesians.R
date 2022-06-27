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
#library( "merDeriv" ) # req. to determine 95% CI's for glmer random effects
library( "parameters" )
library( "ggplot2" )

################################################################################
# BEGIN FUNCTIONS
################################################################################

###
# Get example data.
##
get_example_data <- function()
{
    # surgeon n=3
    surgeon <- c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C")
    # outcome 0/1
    outcome <- rep( 0, length( surgeon ) )
    outcome[1:9] <- 1
    
    # df
    data <- data.frame( surgeon, outcome )
    
    return( data )
}

###
# Get data.
##
get_data <- function()
{
    # get data
    raw <- read.csv( 'data/20220627_preprocessed_df_inclsurgeon.csv' )
    
    # surgeon
    surgeon <- as.factor( raw$Surgeon_num_f )
    levels( surgeon ) <- c( 'A', 'B', 'C' )
    
    # binary outcome
    outcome <- rep( 0, nrow( raw ) )
    outcome[ raw$freedom == 'yes' ] <- 1
    
    # prepare data
    data <- data.frame( surgeon, outcome ) 
    
    return( data )
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
    icc_rho <- round( group_var / ( group_var + resid_var ), 30 )
    
    return( icc_rho )
}

################################################################################
# END FUNCTIONS
################################################################################

# outdir
outdir <- 'out.00.icc'
dir.create( outdir, showWarnings = FALSE )

# data
table( data <- get_example_data() )
table( data <- get_data() )

# plot data
ggplot( data = data, aes( x = surgeon ) ) + 
    geom_histogram( stat = 'count' ) +
    facet_wrap( ~outcome, nrow = 2 ) 


## BINARY ##

# fit generalized linear mixed model
summary( model <- lme4::glmer( outcome ~ 1 + ( 1 | surgeon ), data = data, family = 'binomial' ) )

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



library( 'brms' )

# fit bayesian model
set.seed( 123 )
bmodel <- brms::brm( outcome ~ 1 + ( 1 | surgeon ), data = data, 
                              family = bernoulli(),
                              warmup = 2500, 
                              iter   = 20000, 
                              chains = 2 )

# https://easystats.github.io/performance/index.html
# Intraclass Correlation Coefficient
# Adjusted ICC: 0.141
# Unadjusted ICC: 0.141
performance::icc( bmodel )

## Variances of Posterior Predicted Distribution
# Conditioned on rand. effects: 0.17  Credible Interval (CI) 95%: [0.09-0.23]
performance::variance_decomposition( bmodel )


