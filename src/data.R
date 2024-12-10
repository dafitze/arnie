library(tidyverse)
library(readxl)
library(janitor)

path = "/Users/dafitze/Dropbox/arnie.xlsx"

d = read_excel(path) %>%
  clean_names() %>%
  mutate(date_watched = as.Date(date_watched, format = '%d-%m-%Y'),
         # movie_overall = movie_overall * 10,
         # arnies_performance = arnies_performance * 10,
         # story = story * 10,
         # style = style * 10,
         rotten_tomato_ranking = as.numeric(rotten_tomato_ranking)/10)

write_rds(d, "/Users/dafitze/ristretti/GIT/arnie/data/arnie.rds")
