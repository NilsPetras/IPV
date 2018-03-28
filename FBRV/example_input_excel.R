# example for input using excel (standard workflow)

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
library("readxl")
# loadfonts(device = "win")
sink(lapply(list.files(pattern = "fbrv\\."),source))


# ## as comparizon
# load(file = "data/fbrv_example_positive_self.RData")
# mycompdata <- mydata
# rm(mydata)


## using excel input (standard workflow)

# global <- "test_input/formated_data/Mental_DSSEI_RSES_f.xlsx"
# factors <- as.list(c("test_input/formated_data/DSSEI_f.xlsx","test_input/formated_data/Mental_toughness_f.xlsx","test_input/formated_data/RSES_f.xlsx"))
# mydata <- fbrv.input(global=global,factors=factors)
# save(mydata,file = "Data/fbrv_example_SelfConfidence_long.RData")
DSSEI <- "Michael/DSSEI.xlsx"
mydata <- fbrv.input(factors=DSSEI)
save(mydata,file = "Michael/DSSEI.RData")

global <- "Michael/IPV_global.xlsx"
factors <- c("Michael/IPV_DSSEI.xlsx","Michael/IPV_SMTQ.xlsx","Michael/IPV_RSES.xlsx")
myglobaldata <- fbrv.input(global = global,factors = factors)
save(myglobaldata,file = "Michael/IPV.RData")