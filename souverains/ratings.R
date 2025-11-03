library(rvest)
library(tidyverse)

# setwd("contenu/graphiques/finance")

# On récupère les données table -----

source("taux_souverains.R")

temp <- "https://tradingeconomics.com/country-list/rating" |>
  rvest::read_html() |>
  rvest::html_table(header = T, fill = T)

ratings <- temp[[1]] |>
  rename(country = 1)

ratings_legende <- temp[[2]]

save(ratings, file = "ratings.rds")
save(ratings_legende, file = "ratings_legende.rds")

# Merge with current rates

load("taux_souverains.rds")

ratings_taux <- taux_souverains %>%
  filter(duration %in% c("5Y", "10Y", "30Y")) %>%
  mutate(country = ifelse(country == "Cote d'Ivoire", "Ivory Coast", country)) %>%
  select(country, duration, Yield) %>%
  spread(duration, Yield) %>%
  left_join(ratings, by = "country")

save(ratings_taux, file = "ratings_taux.rds")

return(ratings_taux)