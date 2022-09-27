---
title: "cran-comments"
author: "Nils Petras"
date: "2022 M09 27"
output: html_document
---

## Submission
New features and fixed bugs

## Test environments
* local Windows 11, release (4.2.1), devel (2022-09-26 r82921 ucrt)
* Linux (x86_64-pc-linux-gnu (64-bit); via rhub), devel  (4.2.1 (2022-06-23))

## R CMD check results
There were no ERRORs, WARNINGs.

There was 2 NOTEs:

* checking for future file timestamps ... NOTE
  unable to verify current time
  
This is likely due to the unavailability of worldclockapi.com, see https://stackoverflow.com/questions/63613301/

*  checking examples ... [112s] OK (1m 52.6s)
   Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
   ipv_est       82.56   6.98   89.63
   item_overview 18.89   0.62   19.53

Reaching a time below 5s proved only feasible by removing lots of indicator variables of the factor models, making them uninterpretable. The computation time is high due to model estimation via the lavaan package.

## Downstream dependencies
There are currently no downstream dependencies for this package.
