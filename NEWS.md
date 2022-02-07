# nterval 0.1.0.9000 (development version)

## Bug fixes

## New functionality

## Function changes

## Documentation

# nterval 0.1.0

Includes the following functions:

* `find_n_ksigma()` uses a bisection search algorithm to identify the minimum required sample size 
  to calculate a parametric sample interval for a Normally-distributed population (i.e., "k-sigma" 
  interval) with coverage that falls within pre-specified proximity limits with the desired 
  reliability.
  
* `estimate_reliability()` is a helper function called by `find_n_ksigma()` that uses Monte
  Carlo simulation to estimate the proportion of k-sigma interval coverages that will fall within
  the proximity limits for a given sample size.
  
* `plot_reliability()` is a helper function called by `find_n_ksigma()` that plots the simulated
  sampling distribution for k-sigma interval coverages.
  
* Additional utility functions.
