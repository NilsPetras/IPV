# example on how to create a nested plot

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
devtools::load_all()

# creating plots is a two step process, using model_ and plot_ functions:
coord <- model_nested(self_confidence,subradius = .6)
sc_plot <- plot_nested(coord,filename = "self_confidence_nested")

# adding extra arrows
sc_arrows <- data.frame(V1_factor=rep(NA,3),
                        V1_subfactor=rep(NA,3),
                        V2_factor=rep(NA,3),
                        V2_subfactor=rep(NA,3),
                        value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
coord <- model_nested(self_confidence,subradius = .6,extra_arrows = sc_arrows)
sc_plot <- plot_nested(coord,filename = "self_confidence_nested",extra_arrows = T)

# rotating the nested facet plots one by one
coord <- model_nested(self_confidence,subradius = .6,subrotate = c(0,pi/2,0))
sc_plot <- plot_nested(coord,filename = "self_confidence_nested")

# changing the size of everything at once (adaptive for each part, some not linear)
coord <- model_nested(self_confidence,subradius = .6)
sc_plot <- plot_nested(coord,filename = "self_confidence_nested",size = 1.5)
