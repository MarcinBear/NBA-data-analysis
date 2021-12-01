library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
df <- read.csv("../data/cleaned_nba_dataset.csv")  # the same df as in main.rnw after cleaning