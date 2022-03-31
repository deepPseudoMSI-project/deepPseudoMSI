library(tidyverse)
library(tidyr)
library(plyr)
library(masstools)
library(VennDiagram)
library(randomForest)
library(Boruta)

cal_rmse <- function(predicted, true){
  sqrt(mean((predicted-true)^2))
}

cal_r2 <- function(predicted, true){
  1 - sum((predicted-true)^2)/sum((mean(true) - true)^2)
}