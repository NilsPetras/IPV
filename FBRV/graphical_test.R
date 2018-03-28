# testing new graphical parameters 05.03.2018


rm(list = ls())
library("ggplot2")
library("ggforce") # or circles
library("extrafont") # for fonts
# loadfonts(device = "win")
sink(lapply(list.files(pattern = "fbrv\\."),source))
load(file = "Data/fbrv_example_SelfConfidence_long.RData")

# item plot
# default
DSSEI_item_coordinates <- fbrv.model.items(mydata$factors$DSSEI)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_default")
# par 1: size_title
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_title",
                                   size_title=2)
# par 2: size_axes_labels
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_axes_labels",
                                   size_axes_labels=2)
# par 3: width_axes
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_width_axes",
                                   width_axes=2)
# par 4: size_arrow_heads
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_arrow_heads",
                                   size_arrow_heads=2)
# par 5: width_items
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_width_items",
                                   width_items=2)
# par 6: size_grid
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_grid",
                                   size_grid=2)
# par 7: size_tick_label
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_tick_label",
                                   size_tick_label=2)
# par 8: size_center_dot
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_size_center_dot",
                                   size_center_dot=2)


# factor plot
# default
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,subradius = .5)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_default")
# par 1: size_title
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_title",
                        size_title=2)
# par 2: size_subfactor_labels
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_subfactor_labels",
                        size_subfactor_labels=2)
# par 3: width_axes
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_width_axes",
                        width_axes=2)
# par 4: width_circles
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_width_circles",
                        width_circles=2)
# par 5: size_tick
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_tick",
                        size_tick=2)
# par 6: size_tick_label
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_tick_label",
                        size_tick_label=2)
# par 7: size_cor_labels
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_cor_labels",
                        size_cor_labels=2)
# par 8: size_center_dot
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_size_center_dot",
                        size_center_dot=2)

# nested plot
# default
sc_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
sc_coordinates <- fbrv.calc(mydata,subradius = .5,extra_arrows = sc_arrows)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_default",subfactor_cor_labels = T,extra_arrows = T)
# par 1: size_title
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_title",
                     size_title=2)
# par 2: size_subfactor_labels
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_subfactor_labels",
                     size_subfactor_labels=2)
# par 2a: size_subfactor_labels_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_subfactor_labels_inner",
                     size_subfactor_labels_inner=2)
# par 3: width_axes
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_width_axes",
                     width_axes=2)
# par 3a: width_axes_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_width_axes_inner",
                     width_axes_inner=2)
# par 4: width_circles
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_width_circles",
                     width_circles=2)
# par 4a: width_circles_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_width_circles_inner",
                     width_circles_inner=2)
# par 5: size_tick
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_tick",
                     size_tick=2)
# par 5a: size_tick_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_tick_inner",
                     size_tick_inner=2)
# par 6: size_tick_label
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_tick_label",
                     size_tick_label=2)
# par 7: size_cor_labels
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_cor_labels",
                     size_cor_labels=2)
# par 7a: size_cor_labels_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_cor_labels_inner",
                     size_cor_labels_inner=2,subfactor_cor_labels = T)
# par 8: size_center_dot
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_center_dot",
                     size_center_dot=2)
# par 8a: size_center_dot_inner
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_center_dot",
                     size_center_dot=2)
# par 9: size_extra_arrows
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_extra_arrows",
                     size_extra_arrows=2,extra_arrows = T)
# par 10: size_extra_arrow_heads
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_extra_arrow_heads",
                     size_extra_arrow_heads=2,extra_arrows = T)
# par 11: size_extra_labels
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_size_extra_labels",
                     size_extra_labels=2,extra_arrows = T)

# testing smart scaling with "size" 06.03.2018
DSSEI_item_coordinates <- fbrv.model.items(mydata$factors$DSSEI)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_double",size=2)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "test_output/graphical_test/items_half",size=0.5)
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,subradius = .5)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_double",size = 2)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_half",size = 0.5)
sc_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
sc_coordinates <- fbrv.calc(mydata,subradius = .5,extra_arrows = sc_arrows)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_double",subfactor_cor_labels = T,extra_arrows = T,size=2)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_half",subfactor_cor_labels = T,extra_arrows = T,size=0.5)

# testing the positional dependencies of the axis tick label 07.03.2018
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,subradius = .5,rotate = 1)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_rotated")
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "test_output/graphical_test/factor_rotated_higher_tick",tick = .3)
sc_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
sc_coordinates <- fbrv.calc(mydata,subradius = .5,extra_arrows = sc_arrows,rotate = 1)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_rotated",subfactor_cor_labels = T,extra_arrows = T)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "test_output/graphical_test/nested_rotated_higher_tick",subfactor_cor_labels = T,extra_arrows = T,tick = .3)
