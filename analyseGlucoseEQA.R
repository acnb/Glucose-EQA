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
library(forcats)
library(rtf)

mmolConvFactor <- 18.01806

load(here::here('data', 'eqaAll.RData'))

load(here::here('data', 'sharedDevs.RData'))

source(here::here('R', 'helpersForGraphics.R'))
source(here::here('R', 'calcDeviations.R'))
source(here::here('R', 'algAFuncs.R'))

source(here::here('R', 'description.R'))
source(here::here('R', 'frequencyOfDeviations.R'))
source(here::here('R', 'factorsForPerformance.R'))
source(here::here('R', 'charFunction.R'))
source(here::here('R', 'pathway.R'))
source(here::here('R', 'lots.R'))
source(here::here('R', 'bias.R'))
source(here::here("R", "biasBudget.R")) # needs results from 'charFunction.R'
