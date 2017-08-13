library(plyr)
library(tidyverse)
library(rlang)
library(magrittr)
library(stringr)
library(here)
library(ggpub)
library(scales)
library(robustbase)
library(boot)

source("C:/Users/andi/Dropbox/Papers/graphsForPublications.R")


base.dir <- paste0(here::here(), '/')
colors.status <- c('failed' = "#d7191c",
                  'poor' = "#fdae61",
                  'acceptable' = '#2b83ba',
                  'good' =   "#abdda4")

load(here::here('data', 'eqaAll.RData'))

load(here::here('data', 'sharedDevs.RData'))

source(here::here('R', 'calcDeviations.R'))
source(here::here('R', 'algAFuncs.R'))

source(here::here('R', 'description.R'))
source(here::here('R', 'frequencyOfDeviations.R'))
source(here::here('R', 'charFunction.R'))
source(here::here('R', 'factorsForPerformance.R'))
source(here::here('R', 'pathway.R'))
source(here::here('R', 'lots.R'))
source(here::here("R", "biasBudget.R"))
