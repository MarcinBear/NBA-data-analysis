library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
df <- read.csv("../data/cleaned_nba_dataset.csv")  # the same df as in main.rnw after cleaning
summary(df)
MY_PALETTE1 = c("#ff0000", "#ffa200", "#007bff", "#ff00fb", "#241aa5")

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


ggplot(data=df, aes(x=SEASON_EXP, y=PTS, group=SEASON_EXP, fill=SEASON_EXP)) +
  geom_boxplot(color="#703100", alpha=0.5, lwd=1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom='point', shape=18, color="#ff7300", size=6, na.rm = TRUE, show.legend = TRUE) +
  scale_fill_gradient(low = "#fcba03", high = "#fc3003", aesthetics = "fill") + 
  scale_shape_manual("", values=c("średnia"="x")) +
  guides(fill="none") +
  labs(x = "liczba pełnych sezonów lidze (doświadczenie zawodnika)", 
       y = "średnia liczba punktów na mecz", 
       title = "Liczba punktów vs. doświadczenie w lidze")#+
  # theme(text = element_text(size=35))

df2 <- df[!(df$POSITION == ""), ]

ggplot(df2, aes(AST, REB, shape=POSITION, color=POSITION, na.rm=TRUE, label=PLAYER)) +
  geom_point(size=4, stroke=1.4, alpha=0.4, na.rm=TRUE) +
  geom_smooth(method = "lm", show.legend = FALSE, na.rm=TRUE) +
  geom_label_repel(data = subset(df2, AST*REB > 80), box.padding = 5, 
                   point.padding = 0) +
  
  scale_color_manual(values=MY_PALETTE1[1:3]) +
  scale_shape_manual(values=c(1, 2, 5)) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = "asysty / mecz",
       y = "zbiórki / mecz",
       title = "Zbiórki vs. asysty",
       subtitle = sprintf("Wykres rozrzutu z naniesionymi liniami trendu"))
