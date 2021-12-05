library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
df <- read.csv("../data/cleaned_nba_dataset.csv")  # the same df as in main.rnw after cleaning


existing_teams <- c('Lakers', 'Clippers', 'Kings', 'Rockets', 'Nuggets', 'Thunder',
                    'Trail Blazers', 'Bucks', 'Warriors', 'Knicks', 'Magic', 'Suns',
                    'Pistons', 'Pelicans', 'Heat', 'Nets', 'Cavaliers', 'Mavericks',
                    'Celtics', 'Raptors', '76ers', 'Spurs', 'Wizards', 'Grizzlies',
                    'Bulls', 'Pacers', 'Jazz', 'Hornets', 'Hawks', 'Timberwolves')

df <- df[df$TEAM %in% existing_teams, ]  # save only existing teams

df1 <- df[!is.na(df$AST), ]

A <- aggregate(df1[, c("from_USA", "AST")], list(from_USA=df$from_USA), mean)
A <- data.frame(A[1], A[3])
df["from_USA"] <- df$COUNTRY == "USA"
  
  
# geom_bar(stat='summary', fun.y = mean)
