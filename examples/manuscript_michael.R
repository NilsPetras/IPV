
# Abbildungen Michael Manuskript 05.03.2018
# alle schwarzweiﬂ

rm(list = ls())
library("ggplot2")
library("ggforce") # or circles
library("extrafont") # for fonts
sink(lapply(list.files(pattern = "fbrv\\."),source))
load(file = "Michael/DSSEI.RData")
load(file = "Michael/IPV.RData")

# #DSSEI Items
# DSSEI_item_coordinates <- fbrv.model.items(mydata)
# DSSEI_item_plot <- fbrv.plot.items(DSSEI_item_coordinates,file_name = "Michael/DSSEI_Items",colour2 = "grey40",font = "Helvetica")

#DSSEI Facetten
# DSSEI_coordinates <- fbrv.model(mydata,subradius = .5)
# DSSEI_plot <- fbrv.plot(DSSEI_coordinates,file_name = "Michael/DSSEI_Facetten", font = "Helvetica",size_tick_label = 1.75)

#Gesamt
# myglobaldata$global$center_distances$factor <- as.factor("Self Confidence")
sc_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
sc_coordinates <- fbrv.calc(myglobaldata,subradius=.6, relative_scaling = 3, rotate = pi,subrotate = c(0,pi/2,0),rotate_title=pi/16,cor_spacing = .4,extra_arrows = sc_arrows)
sc_plot <- fbrv.draw(sc_coordinates,size = 1.5,file_name = "Michael/SelfConfidence",subfactor_cor_labels = T,font = "Helvetica",extra_arrows = T,
                     size_extra_labels = 1.125,size_cor_labels = 0.9,size_subfactor_labels_inner = 1.25,size_cor_labels_inner = 1.5,size_tick_label = 1.3,size_title = 8/9)


