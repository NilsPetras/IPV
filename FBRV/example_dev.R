# example for current development

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
sink(lapply(list.files(pattern = "fbrv\\."),source))




# # dev fbrv.calc using data with item information on global scale 08.03.2018
# load(file = "Data/fbrv_example_SelfConfidence_long.RData")
# 
# # itemcoor <- fbrv.model.items(mydata$global)
# # global_itemplot <- fbrv.plot.items(itemcoor,file_name = "test_output/global_itemplot")
# coor <- fbrv.calc(mydata,subradius = .5)
# fbrv.draw(coor,file_name = "test_output/nested_new",cor_labels = T,subfactor_cor_labels = T)
# # subcoor <- fbrv.model(mydata$factors$DSSEI,subradius = .5)
