# examples for development of the input function

## initializing
rm(list = ls())
library("ggplot2")
library("ggforce")
library("extrafont")
library("readxl")
# loadfonts(device = "win")
sink(lapply(list.files(pattern = "fbrv\\."),source))


## as comparizon
load(file = "fbrv_example_Gesicht.RData")
mycompdata <- data
rm(data)

## creating input for presentation example

  # structure dataframe
mytitle <- as.factor(rep("SelfConfidence",44))
myfactor <- as.factor(c(rep("DSSEI",20),rep("SMTQ",14),rep("RSES",10)))
mysubfactor <- as.factor(c(rep("So",5),rep("Ph",5),rep("Pb",5),rep("Ab",5),
                 rep("Cf",6),rep("Cs",4),rep("Ct",4),
                 rep("Ns",5),rep("Ps",5)))
myitem <- as.factor(c(1,5,9,13,17,3,7,11,15,19,16,4,12,8,20,2,6,10,14,18,
                         13,5,11,6,14,1,3,12,8,10,2,4,9,7,
                         6,8,2,9,5,7,1,3,4,10))

structure <- data.frame(title=mytitle,factor=myfactor,subfactor=mysubfactor,item=myitem)


mydata <- fbrv.input(mode="manual",structure=structure)

  # manual version for presentation
mydata$global$center_distances$center_distance <- c(1.094,1.469,1.334)
mydata$global$center_distances$center_distance <- mydata$global$center_distances$center_distance-1
mydata$global$subfactor_cors[1,] <- c(1,.62,.73)
mydata$global$subfactor_cors[2,] <- c(.62,1,.75)
mydata$global$subfactor_cors[3,] <- c(.73,.75,1)

mydata$factors$DSSEI$center_distances$center_distance <- c(1.611,1.452,1.416,.575,.911,2.026,3.184,2.077,2,1.599,
                                                           2.282,1.712,1.461,1.761,1.614,1.16,1.24,1.467,1.426,1.313)
mydata$factors$DSSEI$center_distances$center_distance <- mydata$factors$DSSEI$center_distances$center_distance-1
mydata$factors$DSSEI$center_distances$mean_center_distance <- c(rep(1.193,5),rep(2.177,5),rep(1.766,5),rep(1.321,5))
mydata$factors$DSSEI$center_distances$mean_center_distance <- mydata$factors$DSSEI$center_distances$mean_center_distance-1
mydata$factors$DSSEI$subfactor_cors[1,] <- c(1,.66,.49,.76)
mydata$factors$DSSEI$subfactor_cors[2,] <- c(.66,1,.37,.52)
mydata$factors$DSSEI$subfactor_cors[3,] <- c(.49,.37,1,.54)
mydata$factors$DSSEI$subfactor_cors[4,] <- c(.76,.52,.54,1)

mydata$factors$RSES$center_distances$center_distance <- c(1.228,1.141,1.209,1.078,1.034,1.44,1.16,1.481,1.472,1.182)
mydata$factors$RSES$center_distances$center_distance <- mydata$factors$RSES$center_distances$center_distance-1
mydata$factors$RSES$center_distances$mean_center_distance <- c(rep(1.138,5),rep(1.347,5))
mydata$factors$RSES$center_distances$mean_center_distance <- mydata$factors$RSES$center_distances$mean_center_distance-1
mydata$factors$RSES$subfactor_cors[1,] <- c(1,.69)
mydata$factors$RSES$subfactor_cors[2,] <- c(.69,1)

mydata$factors$SMTQ$center_distances$center_distance <- c(1.235,1.01,1.138,1.249,1.214,.989,
                                                          1.491,1.387,1.483,1.787,1.859,1.486,1.359,2.387)
mydata$factors$SMTQ$center_distances$center_distance <- mydata$factors$SMTQ$center_distances$center_distance-1
mydata$factors$SMTQ$center_distances$mean_center_distance <- c(rep(1.139,6),rep(1.537,4),rep(1.773,4))
mydata$factors$SMTQ$center_distances$mean_center_distance <- mydata$factors$SMTQ$center_distances$mean_center_distance-1
mydata$factors$SMTQ$subfactor_cors[1,] <- c(1,.69,.61)
mydata$factors$SMTQ$subfactor_cors[2,] <- c(.69,1,.61)
mydata$factors$SMTQ$subfactor_cors[3,] <- c(.61,.61,1)

      # setting negative values to 0
mydata$factors$DSSEI$center_distances$center_distance[mydata$factors$DSSEI$center_distances$center_distance<0] <- 0
mydata$factors$SMTQ$center_distances$center_distance[mydata$factors$SMTQ$center_distances$center_distance<0] <- 0

  # loading and preparing excel data

# filelist <- list.files(path="Data",pattern='*NP.xlsx',full.names = T)
# exceldata <- lapply(filelist,read_excel,sheet=1,trim_ws=T)
# names(exceldata) <- c("DSSEI","PositiveSelf","SMTQ","RSES")
# mycolumns <- function(x){
#   x <- x[,-c(2,3,5,6)]
#   names(x) <- c("item","subfactor","center_distance")
#   return(x)
# }
# exceldata <- lapply(exceldata,FUN = mycolumns)
# exceldata$RSES$subfactor[exceldata$RSES$subfactor==1] <- 
# exceldata$RSES$subfactor[exceldata$RSES$subfactor==2] <- 

# calcmeancd <- function(x){
#   x["mean_center_distance"] <- 
# }

  # filling in data values

  # saving to file
save(mydata,file = "Data/fbrv_example_positive_self.RData")
