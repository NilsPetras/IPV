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

There were 2 NOTEs:

* checking for future file timestamps ... NOTE
  unable to verify current time
  
This is likely due to the unavailability of worldclockapi.com, see https://stackoverflow.com/questions/63613301/

*  checking examples ... [51s] OK (51.2s)
   Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
   ipv_est       26.94   1.51   28.45
   item_overview 18.54   0.61   19.16

Reaching a time below 5s proved only feasible by removing lots of indicator variables of the factor models, which makes them uninterpretable. We accepted this in an earlier version, but think we erred in the wrong direction. The computation time is almost entirely spent in necessary calls to lavaan (model estimation and standardization of estimation results), which I checked via profvis::profvis. In this resubmission, using a cropped dataset was implemented where it saves a meaningful amount of time.

## Downstream dependencies
There are currently no downstream dependencies for this package.
