---
output:
  html_document: default
  pdf_document: default
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# nterval 0.0.0.9000

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nterval)](https://CRAN.R-project.org/package=nterval)
<!-- badges: end -->

The goal of nterval is to provide an approach for calculating the
minimum required sample size for estimation of population coverage
intervals (e.g., k-sigma intervals).

## Installation

Install the package using `devtools`:

``` r
# To install the most recent stable release of the package from Github
devtools::install_github("dygobeng/nterval@*release")

# To install the latest (development) version of the package from Github
devtools::install_github("dygobeng/nterval")
```

## Definitions

Before exploring an example, a few definitions:

-   The **target coverage** is the expected proportion of a
    Normally-distributed population that should be contained within the
    k-sigma parametric sample interval (e.g., a 3-sigma sample interval
    is expected to cover 99.73% of Normally-distributed observations).

-   The **sample coverage** is the proportion of a Normally-distributed
    population that *is* contained within the k-sigma interval estimated
    using a sample from the population.

-   The **upper and lower proximity limits** define the distance from
    the target coverage that an individual sample’s coverage can fall
    and still be considered “reasonably close” to the target. These
    bounds are useful for dialing acceptable

    -   *producer’s risk* by ensuring that the lower proximity limit
        does not move so low as to result in an unacceptable false
        positive rate (probability of flagging common cause variation),
        and

    -   \_consumer’s risk” by ensuring that the upper proximity limit
        does not move so high as to result in an unacceptable false
        negative rate (probability of failing to flag special cause
        variation)

-   The **reliability** is the proportion of individual sample coverages
    expected to fall within the proximity limits.

## Example

A researcher would like to determine the minimum sample size required to
provide that a 2-sigma sample interval (with a targeted coverage of
95.44% when calculated using data that can be reasonably approximated by
a Normal distribution) will cover no less than 93% of the population and
no more than 97% of the population with 70% reliability.

We’ll use the `find_n_ksigma()` function to determine the required
sample size. The main arguments accept values for `k` (based on the
target coverage), the `proximity_range`, and the targeted `reliability`:
