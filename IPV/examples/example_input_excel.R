# example for input using excel (standard workflow)

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
library("readxl")
# loadfonts(device = "win")
devtools::load_all()

# one factor
DSSEI <- "Michael/DSSEI.xlsx"
mydata <- fbrv.input(factors=DSSEI)
save(mydata,file = "Michael/DSSEI.RData")

# nested
global <- "Michael/IPV_global.xlsx"
factors <- c("Michael/IPV_DSSEI.xlsx","Michael/IPV_SMTQ.xlsx","Michael/IPV_RSES.xlsx")
myglobaldata <- fbrv.input(global = global,factors = factors)
save(myglobaldata,file = "Michael/IPV.RData")
