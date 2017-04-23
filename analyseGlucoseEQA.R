library(plyr)
library(tidyverse)
library(magrittr)
library(stringr)
source("C:/Users/andi/Dropbox/Papers/graphsForPublications.R")


base.dir <- '~/RCode/Glucose-EQA/'
colors.status <- c('fail' = "#F8766D",
                  'poor' = "#619CFF",
                  'good' =   "#00BA38")

load(paste0(base.dir, 'data/eqa.RData'))
load(paste0(base.dir, 'data/lots.RData'))
load(paste0(base.dir, 'data/sharedDevs.RData'))

source(paste0(base.dir, 'R/calcDeviations.R'))
source(paste0(base.dir, 'R/algAFuncs.R'))

source(paste0(base.dir, 'R/description.R'))
source(paste0(base.dir, 'R/frequencyOfDeviations.R'))
source(paste0(base.dir, 'R/factorsForPerformance.R'))
source(paste0(base.dir, 'R/pathway.R'))
