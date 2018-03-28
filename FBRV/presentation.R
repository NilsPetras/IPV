# FRBV Presentation 07.02.2018


    ### design principles ###

# contrast
# layering
# positioning
# dependency
# smart adaptation
# dealing with "special features"



    ### header ###

rm(list = ls())
library("ggplot2")
library("ggforce") # or circles
library("extrafont") # for fonts
# loadfonts(device = "win")
sink(lapply(list.files(pattern = "fbrv\\."),source))
load(file = "Data/fbrv_example_positive_self.RData")



    ### creating an item plot ###

# standard
mydata$factors$DSSEI
DSSEI_item_coordinates <- fbrv.model.items(mydata$factors$DSSEI)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "Presentation/DSSEI1")

# changing color
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "Presentation/DSSEI2",
                                  colour = "springgreen4")

# rotating (continuous)
DSSEI_item_coordinates <- fbrv.model.items(mydata$factors$DSSEI,
                                  rotate = 2.843753)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "Presentation/DSSEI3",colour = "springgreen4")

# changing the size of things
DSSEI_item_coordinates <- fbrv.model.items(mydata$factors$DSSEI,rotate = 2.843753)
DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "Presentation/DSSEI4",colour = "springgreen4",
                                   size = 1.924282)

# rm(DSSEI_item_coordinates,DSSEI_item_plot)




    ### creating a facet plot ###

# standard
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,subradius = .5)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "Presentation/DSSEI5")

# changing circle radius
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,
                                subradius = .148232)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "Presentation/DSSEI6",
                        size=.5)

# changing color (again)
DSSEI_coordinates <- fbrv.model(mydata$factors$DSSEI,subradius = .148232)
DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "Presentation/DSSEI7",size=.5,
                        colour="darkorchid3")



    ### creating a nested plot ###

# standard
mydata$global
sc_coordinates <- fbrv.calc(mydata,subradius = .5)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC1")

# relative scaling of overall plot and subplots
sc_coordinates <- fbrv.calc(mydata,subradius=.5,
                            relative_scaling = 3)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC2")

# rotating, resizing and using color
sc_coordinates <- fbrv.calc(mydata,subradius=.5,relative_scaling = 3,
                            rotate=pi)
sc_plot <- fbrv.draw(sc_coordinates,file_name="Presentation/SC3",
                     colour="darkred",subcolour="red",size = 1.5)

# without cors
sc_coordinates <- fbrv.calc(mydata,subradius=.5,rotate=pi,relative_scaling = 3,
                            cors = F)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC4",colour="darkred",subcolour="red",size=1.5,
                     cor_labels = F)

# rotating the subplots
sc_coordinates <- fbrv.calc(mydata,subradius=.5,rotate=pi,relative_scaling = 3,
                            subrotate = c(-.1,1,0))
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC5",colour="darkred",subcolour="red",size=1.5)

# showing cors in subplots (omit arrows)
sc_coordinates <- fbrv.calc(mydata,subradius=.5,rotate=pi,relative_scaling = 3,subrotate = c(-1,1,3))
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC6",colour="darkred",subcolour="red",size=1.5,
                     subfactor_cor_labels = T)

# adding extra arrows
sc_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
sc_arrows

sc_coordinates <- fbrv.calc(mydata,subradius=.5,rotate=pi,relative_scaling = 3,subrotate = c(-1,1,3),
                            extra_arrows = sc_arrows)
sc_plot <- fbrv.draw(sc_coordinates,file_name = "Presentation/SC7",colour="darkred",subcolour="red",size=1.5,subfactor_cor_labels = "all",
                     extra_arrows = T)



    ### for advanced users ###

# creating the plots is always a two step process:
# 1) calculating the coordinates
# 2) plotting with custom options

# the coordinates/labels/values can be subject to change
# sc_coordinates

# layers can be added to the plot
# sc_plot+geom_abline()