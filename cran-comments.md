---
title: "cran-comments"
author: "Nils Petras"
date: "2021 M12 08"
output: html_document
---

## Submission
New features and fixed bugs

## Test environments
* local Windows 10, release (4.1.2), devel (2021-12-04 r81290)
* Windows 10, devel (via devtools::check_win_devel; 2021-12-03 r81290)
* Ubuntu Linux 16.04.7 LTS (via travis-ci.com)

## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE:

Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
item_overview 15.6    0.1   15.92

This is the slowest test run (via check_win_devel), on other machines this runs
faster (~1.9s on local laptop). It is a much reduced version of the actual
example on the full data (40% of variables, 5% of cases). Further simplification
lead to irregular estimates.


## Downstream dependencies
There are currently no downstream dependencies for this package.
