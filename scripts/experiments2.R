library(ggplot2)
library(dplyr)
library(car)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
df <- read.csv("../data/cleaned_nba_dataset.csv")  # the same df as in main.rnw after cleaning

qqPlot(df$PTS, distribution="gamma", shape=1.5)

ks.test(x=df$REB, y="pgamma", 1.5, 1)
