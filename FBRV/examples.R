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
# pscoor <- fbrv.calc(data = mydata,subrad = .5,correlation_spacing = .4,relative_scaling = 3,rotate = pi,extra_arrows = psarrows,subrotate = c(0,0,pi/2))
# psitems <- fbrv.model.items(mydata$factors$DSSEI)
# DSSEIcoor <- fbrv.model(data = mydata$factors$DSSEI,subrad = .5)
# RSEScoor <- fbrv.model(data = mydata$factors$RSES,subrad = .2)
# SMTQcoor <- fbrv.model(data = mydata$factors$SMTQ,subrad = .2)
# istcoor <- fbrv.model(data,subrad = .3,rotate = pi/2)
# mycoor <- fbrv.model(data$factors$klein,subrad = .2,rotate = 2)
# mycoor <- fbrv.calc(data,0.25,subrotate = c(1.1,1.9,3.5))
# mycoor <- fbrv.calc(data,.25)
# fbrv_coor_items <- fbrv.model.items(data,pi/6)

  # drawing of plot
# psitemplot <- fbrv.plot.items(coor_items = psitems,size = 1,outfilename = "mypsitemplot1",mycol = "blue")
# DSSEIplot <- fbrv.plot(coor = DSSEIcoor,size = 1,outfilename = "myDSSEIplot2",mycol = "blue",correlation_labels = "all")
# psplot <- fbrv.draw(coor = pscoor,size = 1.5,outfilename = "mypsplot5",subfactor_correlation_labels = "all",mycol = "darkblue",mysubcol = "blue",extra_arrows = F)
# istplot <- fbrv.plot(istcoor,size = 1, outfilename = "myfbrvplot4",correlation_labels = "none",correlation_arrows = T)
# myplot <- fbrv.draw(mycoor,size = 1.5,outfilename = "fbrvglobalplot39",correlation_labels="all",subfactor_correlation_labels = "all",subfactor_correlation_arrows=F,mycol = "darkblue",mysubcol="darkred",extra_arrows=T)
# myfbrv <- fbrv.plot(mycoor,size = .5 ,outfilename = "myfbrvplot32",mycol = "darkred",correlation_arrows=F,correlation_labels = "all")
# myfbrvitems <- fbrv.plot.items(fbrv_coor_items,1,outfilename = "fbrvitemplot12",mycol = "blue")

  # coordinates
# myfbrvitems
# data
# mycoor$factor$groß$inner_cors

  # other
# shifted <- fbrv.shift(mycoor$factor$groß,1,1)