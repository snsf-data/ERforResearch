<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# ERforResearch 4.0.0

- (!!!) switch from `rjags` to `runjags` for `get_mcmc_samples()`.   
- all function and documentations were updated due to the change to `runjags`.  
- (!!!) important naming changes to align all package-communications: from `application` to `proposal`, `voter` to `assessor` and `section` to `panel`.  
- colored points in `plot_er_distribution()`.


# ERforResearch 3.1.1.9000

- added an option in the ER plotting function to allow for not having the PM, and skipping it in the plot


# ERforResearch 3.1.1

- added an option in the ER plotting function to allow for not having the PM, and skipping it in the plot
- documentation updates


# ERforResearch 3.1.0

- documentation updates
- added parameter `rhat_threshold` in `get_mcmc_samples()` to allow the user to change the threshold used to decide whether the chains converged.
- added option `maximal_testing` in mcmc function
- add a default continuous model with a nine point scale
- added na.rm = TRUE whenever max(rhat) calculated


# ERforResearch 3.0.0.9000

* nothing to add


# ERforResearch 3.0.0

Include Reviewer comments:  

- changed name of `get_inits_overdispersed_two_chains()` to `get_inits_overdispersed_four_chains()` and implemented the new default of **four** chains with different RNGs.  
- `get_mcmc_samples()` function defaulted to four chains with four different samplers and fixed (random but dependent on seed) starting values.  
- updated the test of `get_mcmc_samples()` function:  
  - changed Rhat threshold to 1.01 instead of 1.1  
  - update the updating loop (the multiplier of n.adapt, burnin and iterations)  
- the `get_max_rhat()` function was deleted  
- added option to compute effective sample size and MCMC error in `get_mcmc_sample()`    
- the default variables in `get_mcmc_sample()` were changed to being able to sample from all the models discussed in the paper (Heyard et al (2021)), with the default model  
- rankability computation etc was deleted an documentation of `get_er_from_jags()` updated


# ERforResearch 2.0.0

- add max_iter option in `get_er_form_jags()`
- new option to not add the rank based on pm in the `get_er_from_jags()` function
- fix `get_mcmc_samples()`
- added maximum number of iterations to `get_mcmc_samples()`
- update `get_sankey_plot_br()`
- added `get_sankey_plot_br()` function
- `get_mcmc_samples()` now tests whether chains converge, if not, the adaptation, burnin and iteration phases are increases.


# ERforResearch 1.0.0

- `get_mcmc_samples()` now tests whether chains converge, if not, the adaptation, burnin and iteration phases are increases.
- all functions were adapted accordingly
- possibility to add overdispersed initial values in `get_mcmc_samples()`
- functions for convergence diagnostic: `get_inits_overdispersed_two_chains()` and `get_max_rhat()`
- default model can now also have an ordinal outcome variable and a heterogenous residual structure


# ERforResearch 0.1.0.9009

- allow model to account for heterogenous residuals


# ERforResearch 0.1.0.9008

- allow model to account for heterogenous residuals
- extended `get_er_from_jags()` to be used with ordinal outcomes
- added ordinal extension in `get_mcmc_samples()` function



# ERforResearch 0.1.0.9007

- added an option  to choose between inner and outer CrI for funding decision
- update  `get_right_data_format()`


