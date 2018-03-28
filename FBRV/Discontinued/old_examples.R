# examples for development of fbrv functions

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
# loadfonts(device = "win")
sink(lapply(list.files(pattern = "fbrv\\."),source))

## examples to use
# load(file = "fbrv_example_Gesicht.RData")
# load(file = "fbrv_example_I-S-T.RData")
load(file = "Data/fbrv_example_positive_self.RData")

## creating extra arrow options for nested plot
# extra_arrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
# extra_arrows[1,] <- c("groß","Ohr","klein","Ohr",".56")
# extra_arrows[2,] <- c("mittel","Ohr","klein","Ohr",".43")
# extra_arrows[3,] <- c("mittel","Nase","klein","Ohr",".94")
psarrows <- data.frame(V1_factor=rep(NA,3),V1_subfactor=rep(NA,3),V2_factor=rep(NA,3),V2_subfactor=rep(NA,3),value=rep(NA,3))
psarrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
psarrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
psarrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")

## function calls

#   # calculation of coordinates
pscoor <- fbrv.calc(data = mydata,subradius = .5,cor_spacing = .4,relative_scaling = 3,subrotate = c(0,0,pi/2),rotate = pi,extra_arrows = psarrows,items = T) # 
# psitems <- fbrv.model.items(mydata$factors$RSES,rotate = pi/2)
# DSSEIcoor <- fbrv.model(data = mydata$factors$DSSEI,subradius = .5)
# RSEScoor <- fbrv.model(data = mydata$factors$RSES,subradius = .2)
# SMTQcoor <- fbrv.model(data = mydata$factors$SMTQ,subradius = .2)
# istcoor <- fbrv.model(data,subradius = .3,rotate = pi/2)
# mycoor <- fbrv.model(data$factors$klein,subradius = .2,rotate = 2)
# mycoor <- fbrv.calc(data,0.25,subrotate = c(1.1,1.9,3.5))
# mycoor <- fbrv.calc(data,.25)
# fbrv_coor_items <- fbrv.model.items(data,pi/6)

  # drawing of plot
psitemplot <- fbrv.plot.items(coor = pscoor$items$SMTQ,size = 1,file_name = "test_output/item_plots/item_test_3",colour = "blue")
# DSSEIplot <- fbrv.plot(coor = DSSEIcoor,size = 1,file_name = "myDSSEIplot2",colour = "blue",cor_labels = T)
# psplot <- fbrv.draw(coor = pscoor,size = 1.5,file_name = "mypsplot5",subfactor_cor_labels = T,colour = "darkblue",subcolour = "blue",extra_arrows = F)
# istplot <- fbrv.plot(istcoor,size = 1, file_name = "myfbrvplot4",cor_labels = F)
# myplot <- fbrv.draw(mycoor,size = 1.5,file_name = "fbrvglobalplot39",cor_labels="all",subfactor_cor_labels = T,subfactor_cor_arrows=F,colour = "darkblue",subcolour="darkred",extra_arrows=T)
# myfbrv <- fbrv.plot(mycoor,size = .5 ,file_name = "myfbrvplot32",colour = "darkred",cor_arrows=F,cor_labels = T)
# myfbrvitems <- fbrv.plot.items(fbrv_coor_items,1,file_name = "fbrvitemplot12",colour = "blue")

  # coordinates
# myfbrvitems
# data
# mycoor$factor$groß$inner_cors

  # other
# shifted <- fbrv.shift(mycoor$factor$groß,1,1)