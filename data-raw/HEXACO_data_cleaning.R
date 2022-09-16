### Preparation of HEXACO example data for IPV

# HEXACO data (https://openpsychometrics.org/_rawdata/ 16.9.2022)
HEXACO <- read.csv(file = "data-raw/data.csv", sep = "\t")

# number of cases in raw data:
# dim(HEXACO) # 22786

# dismiss cases that did not at least "agree" to both checks
HEXACO <- HEXACO[which((HEXACO$V1 > 5) & (HEXACO$V2 > 5)), ]
# missing data coded "0" in raw data
HEXACO[HEXACO == 0] <- NA

# number of cases in cleaned up data:
# dim(HEXACO) # 20365

# recode reversely keyed items based on codebook
# H
rh <- c(paste("HSinc", 2:10, sep = ""),
        paste("HFair", 6:10, sep = ""),
        paste("HGree", 3:10, sep = ""),
        paste("HMode", 5:10, sep = ""))
HEXACO[ ,rh] <- 8 - HEXACO[ ,rh]

# E
re <- c(paste("EFear", 6:10, sep = ""),
        paste("EAnxi", 6:10, sep = ""),
        paste("ESent", 6:10, sep = ""))
HEXACO[ ,re] <- 8 - HEXACO[ ,re]

# X
rx <- c(paste("XExpr", 6:10, sep = ""),
        paste("XSocB", 6:10, sep = ""),
        paste("XSoci", 6:10, sep = ""),
        paste("XLive", 9:10, sep = ""))
HEXACO[ ,rx] <- 8 - HEXACO[ ,rx]

# A
ra <- c(paste("AForg", 5:10, sep = ""),
        paste("AGent", 5:10, sep = ""),
        paste("AFlex", 3:10, sep = ""),
        paste("APati", 6:10, sep = ""))
HEXACO[ ,ra] <- 8 - HEXACO[ ,ra]

# C
rc <- c(paste("COrga", 6:10, sep = ""),
        paste("CDili", 6:10, sep = ""),
        paste("CPerf", 9:10, sep = ""),
        paste("CPrud", 4:10, sep = ""))
HEXACO[ ,rc] <- 8 - HEXACO[ ,rc]

# O
ro <- c(paste("OAesA", 6:10, sep = ""),
        paste("OInqu", 7:10, sep = ""),
        paste("OCrea", 7:10, sep = ""),
        paste("OUnco", 6:10, sep = ""))
HEXACO[ ,ro] <- 8 - HEXACO[ ,ro]

# reformat variable names
## first 240 variables are 240 HEXACO items
## scheme: construct_test_item
## long form assures unique item names at the level of the test
names(HEXACO)[1:240] <- paste(
  substr(names(HEXACO)[1:240], 1,1),
  substr(names(HEXACO)[1:240], 2,5),
  substr(names(HEXACO)[1:240], 2,7),
  sep = "_")
