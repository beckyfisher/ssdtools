---
title: 'ssdtools: An R package to fit Species Sensitivity Distributions (SSDs)'
authors:
- affiliation: 1
  name: Joe Thorley
  orcid: 0000-0002-7683-4592
- affiliation: 2
  name: Carl Schwarz
date: '2018-10-30'
output: pdf_document
bibliography: paper.bib
tags:
- ssd
- R
- maximum likelihood
- hazard concentration
affiliations:
- index: 1
  name: Poisson Consulting Ltd., British Columbia
- index: 2
  name: Simon Fraser University, British Columbia
---

# Summary

Species sensitivity distributions (SSDs) are cumulative probability distributions which are fitted to toxicity concentrations for multiple species (Figure 1). 
They are used for the derivation of environmental quality criteria and ecological risk assessment for contaminated ecosystems [@posthuma_species_2001].

`ssdtools` is an R package [@r] to fit log-normal (lnorm), log-logistic (llog), gompertz, log-gumbel (lgumbel), gamma or weibull distributions to species concentration data.

![Species sensitivity distributions for example species concentration values](dists.png)

The user can also define their own distributions.
Multiple distributions can be averaged using Information Criteria [@burnham_model_2002].
The available Information Criteria are the Akaike Information Criterion (AIC), the Akaike Information Criterion corrected for small sample size (AICc) and Bayesian Information Criterion (BIC).
Confidence intervals can be calculated for fits or specific hazard concentrations (HCs).
The confidence intervals are currently produced by parametric bootstrap resampling.

`ssdtools` loads the `fitdistrplus` R package [@fitdistrplus] upon which it depends for model fitting, calculation of AIC and bootstrapping.
`ssdtools` also loads the `ggplot2` R package [@ggplot2] which it extends by defining `ssdfit`, `xribbon` and `hcintersect` geoms to allow the user to produce custom SSD plots.

Development of `ssdtools` was funded by the Ministry of Environment, British Columbia.

The software archive is at <https://github.com/bcgov/ssdtools>.

# References