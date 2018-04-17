# example on how to generate an item plot

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
devtools::load_all()

# creating plots is a two step process, using model_ and plot_ functions:
coord <- model_items(DSSEI)
DSSEI_item_plot <- plot_items(coord,filename = "DSSEI_items")
