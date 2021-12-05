
library(tidyverse)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


DS <- read.delim("../Data/dataset.csv",sep = ";")

names(DS) # to see all column names

str(DS) # to see all the columns with their type + dimensions of the data

length(which(is.na(DS))) # to check how many NA vlaues there are -> none
