# test if it works

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

load("NN_output.RData")
