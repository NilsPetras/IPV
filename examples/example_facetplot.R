# example on how to generate a facet plot

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
devtools::load_all()

# creating plots is a two step process, using model_ and plot_ functions:
coord <- model_facets(DSSEI,subradius = .5)
DSSEI_item_plot <- plot_facets(coord,filename = "DSSEI_facets")
