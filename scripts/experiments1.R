library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
df <- read.csv("../data/cleaned_nba_dataset.csv")  # the same df as in main.rnw after cleaning
summary(df)


MY_PALETTE1 = c("#ff0000", "#ffa200", "#007bff", "#ff00fb", "#241aa5")
df2 <- df[!(df$POSITION == ""), ]

ggplot(df2, aes(AST, REB, na.rm=TRUE, label=PLAYER)) +
  geom_point(size=4, stroke=1.4, alpha=0.4, na.rm=TRUE) +
  geom_smooth(method = "lm", show.legend = FALSE, na.rm=TRUE) +
  scale_color_manual(values=MY_PALETTE1[1:3]) +
  scale_shape_manual(values=c(1, 2, 5)) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  labs(x = "asysty",
       y = "zbiórki",
       colour="Pozycja", 
       shape="Pozycja",
       title = "Zbiórki vs. asysty",
       subtitle = sprintf("Wykres rozrzutu z naniesionymi liniami trendu")) + 
  theme(text = element_text(size=40))


MY_PALETTE1 = c("#ff0000", "#ffa200", "#007bff", "#ff00fb", "#241aa5")

existing_teams <- c('Lakers', 'Clippers', 'Kings', 'Rockets', 'Nuggets', 'Thunder',
                    'Trail Blazers', 'Bucks', 'Warriors', 'Knicks', 'Magic', 'Suns',
                    'Pistons', 'Pelicans', 'Heat', 'Nets', 'Cavaliers', 'Mavericks',
                    'Celtics', 'Raptors', '76ers', 'Spurs', 'Wizards', 'Grizzlies',
                    'Bulls', 'Pacers', 'Jazz', 'Hornets', 'Hawks', 'Timberwolves')

medPTS <- median(df$PTS, na.rm = TRUE)
medAST <- median(df$AST, na.rm = TRUE)
medREB <- median(df$REB, na.rm = TRUE)

avgPTS <- mean(df$PTS, na.rm = TRUE)
avgAST <- mean(df$AST, na.rm = TRUE)
avgREB <- mean(df$REB, na.rm = TRUE)
# df <- df[df$TEAM %in% existing_teams, ]  # save only existing teams
# 
# df1 <- df[!is.na(df$AST), ]
# 
# A <- aggregate(df1[, c("from_USA", "AST")], list(from_USA=df$from_USA), mean)
# A <- data.frame(A[1], A[3])
# df["from_USA"] <- df$COUNTRY == "USA"
  
# ggplot(df, aes(x=COUNTRY, y=AST)) +
# geom_bar(stat='summary', fun.y = mean)
# df3 <- df[!(df$COUNTRY == 'USA'), ]

treshold <- function(vec) {quantile(vec, na.rm = TRUE)[4]}



df4 <- df[!(df$TEAM == ''), ]
df4$Status <- ifelse(df4$TEAM %in% existing_teams, "aktywna", "rozwiązana")

df5 <- df4[(df4$PTS > avgPTS) & (df4$AST > avgAST) & (df4$REB > avgREB), ]
df5 <- df5[complete.cases(df5), ]

df5$stats <- ifelse((df5$PTS > 2*avgPTS) & (df5$AST > 2*avgAST) & (df5$REB > 2*avgREB), "powyżej Q3", "powyżej Q2")

ggplot(df5, aes(x=reorder(TEAM, TEAM, function(x)+length(x))), na.rm = TRUE) +
  geom_bar(aes(alpha=stats, fill=Status), na.rm = TRUE) +
  scale_fill_manual("Status drużyny", values=c("#007bff", "#ffa200")) + 
  scale_alpha_discrete("Punkty, asysty i zbiórki", range=c(.5,1)) +
  labs(x = "Drużyna",
       y = "Liczba zawodników",
       title = "Liczba wyjątkowych zawodników w historii klubu") +
  coord_flip() +
  theme(text = element_text(size=10),
        legend.position = c(0.7, 0.2),
        legend.spacing.y = unit(1, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill=alpha('white', 0.5)))






library(ggcorrplot)


D <- df[, c(4,6,9,10,11)]
D <- D[complete.cases(D), ]
ggcorrplot(cor(D), type = "lower",
           outline.col = "white",
           colors = c("#241aa5", "#ffefe5", "#ff6f00"),
           lab=TRUE)












# ggplot(data=df, aes(x=SEASON_EXP, y=PTS, group=SEASON_EXP, fill=SEASON_EXP)) +
#   geom_boxplot(color="#703100", alpha=0.5, lwd=1.5, na.rm = TRUE) +
#   stat_summary(fun = mean, geom='point', shape=18, color="#ff7300", size=6, na.rm = TRUE, show.legend = TRUE) +
#   scale_fill_gradient(low = "#fcba03", high = "#fc3003", aesthetics = "fill") + 
#   scale_shape_manual("", values=c("średnia"="x")) +
#   guides(fill="none") +
#   labs(x = "liczba pełnych sezonów lidze (doświadczenie zawodnika)", 
#        y = "średnia liczba punktów na mecz", 
#        title = "Liczba punktów vs. doświadczenie w lidze")#+
#   # theme(text = element_text(size=35))
# 
# df2 <- df[!(df$POSITION == ""), ]
# 
# ggplot(df2, aes(AST, REB, shape=POSITION, color=POSITION, na.rm=TRUE, label=PLAYER)) +
#   geom_point(size=4, stroke=1.4, alpha=0.4, na.rm=TRUE) +
#   geom_smooth(method = "lm", show.legend = FALSE, na.rm=TRUE) +
#   geom_label_repel(data = subset(df2, AST*REB > 80), box.padding = 5, 
#                    point.padding = 0) +
#   
#   scale_color_manual(values=MY_PALETTE1[1:3]) +
#   scale_shape_manual(values=c(1, 2, 5)) +
#   guides(color = guide_legend(override.aes = list(size = 10))) +
#   labs(x = "asysty / mecz",
#        y = "zbiórki / mecz",
#        title = "Zbiórki vs. asysty",
#        subtitle = sprintf("Wykres rozrzutu z naniesionymi liniami trendu"))
