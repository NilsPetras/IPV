# example for input using excel

## initializing
rm(list = ls())
library("readxl")
devtools::load_all()

# read data for a single factor by ignoring the "global" parameter of input_excel
DSSEI <- input_excel(factors="inst/extdata/DSSEI.xlsx")
SMTQ <- input_excel(factors = "inst/extdata/IPV_SMTQ.xlsx")

# read data for a nested factor model
# note that excel files need to be split as in the example to contain only
# one general factor and one correlated factor model for the same set of items
global <- "inst/extdata/IPV_global.xlsx"
factors <- c("inst/extdata/IPV_DSSEI.xlsx","inst/extdata/IPV_SMTQ.xlsx","inst/extdata/IPV_RSES.xlsx")
self_confidence <- input_excel(global = global,factors = factors)
