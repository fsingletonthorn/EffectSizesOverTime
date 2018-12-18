
# setting paths for library
.libPaths("~RLib/")

lib = .libPaths()[1]

# Installing packages 
install.packages(c("metafor", "tidyverse", "readr"),
                 lib = lib,
                 repos = "https://cran.ms.unimelb.edu.au/")

library(metafor); library(tidyverse); library(readr)

# read data 
data <- read_csv2(file = "data/150211FullFile_AllStatcheckData_Automatic1Tail.csv")

# exluding all articles where the articles are not appliable (i.e., samples don't go back to 1985)
includedJournals <- c("DP", "FP", "JAP", "JCCP", "JEPG", "JPSP",  "PS")
dat <- data[data$journals.jour. %in% includedJournals,]

# Renamming for clarity
names(dat)[18] <- "journal"
names(dat)[19] <- "year"
names(dat)[1] <- "id"
names(dat)[2] <- "article"

# Adapted from https://osf.io/z7aux/
esComp <- function(x,df1,df2,esType) {
  esComp <- ifelse(esType=="t",sqrt((x^2*(1 / df2)) / (((x^2*1) / df2) + 1)), 
                   ifelse(esType=="F",sqrt((x*(df1 / df2)) / (((x*df1) / df2) + 1))*sqrt(1/df1),
                          ifelse(esType=="r",x,
                                 ifelse(esType=="Chi2",sqrt(x/(df2 + 2)),
                                        ifelse(esType == "Z",NA,NA)))))
  return(esComp)
}

# calculating pseudo correlation coefficents
dat$r <- esComp(x = dat$Value, df1 = dat$df1, df2 =  dat$df2, esType = dat$Statistic)

# setting to positive
dat$r <- abs(dat$r)

# transforming to fisher's z
dat$z <- 0.5*log((1 + dat$r) / (1 - dat$r)) 

# Standard error for z (sample size taken as df2 - 2, se as sqrt(1/(sampleSize-3))
dat$SEz <- sqrt(1/(dat$df2 - 5))

# Binary for valid standard error
dat$vaidSE <- !((dat$Statistic=="Chi2" | (dat$Statistic=="F" & dat$df1 > 1) | (dat$Statistic == "Z")) | dat$df2 < 6 | is.na(dat$z) | dat$z == Inf)

# extrating data for main meta-analysis
datMeta <- filter(dat, dat$vaidSE)

# Model
mod <- rma.mv(z, V = SEz^2, random = ~  factor(article) | journal, mods = (year - mean(year)), data = datMeta)
saveRDS(mod, file = "mod.rds")














