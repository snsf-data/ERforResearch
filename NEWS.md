<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

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


