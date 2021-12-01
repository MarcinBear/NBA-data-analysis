library(RSQLite)

# Run to save data to csv file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set working dir to current folder
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite, "../data/basketball.sqlite")
df <- dbGetQuery(conn, "SELECT * FROM Player_Attributes")
write.csv(df, file = "../data/nba_dataset.csv")
dbDisconnect(conn)
