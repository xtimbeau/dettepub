library(rvest)
library(tidyverse)

# setwd("finance")

# On récupère les données table -----

temp <- "https://www.investing.com/rates-bonds/world-government-bonds" |>
  rvest::read_html() |>
  rvest::html_table(header = T, fill = T)

# length(temp) - 4 car les.4 dernières tibble ne sont pas des données de taux souverains
# on enlève les warnings pour la transformation de Chg. en numeric

taux_souverains <- tibble(num = 1:(length(temp)-4)) %>%
  mutate(data = map(num, ~ temp[[.]] %>%
                      select(2:7) %>%
                      mutate(`Chg.` = suppressWarnings(as.numeric(`Chg.`))))) %>%
  unnest(cols = c(data)) %>%
  select(-num) %>%
  mutate(duration = str_extract(Name, "\\d+[YM]$"),
         country = str_trim(str_extract(Name, "^[\\p{L} .'’]+")),
         country = case_when(country == "U.S." ~ "United States",
                             country == "U.K." ~ "United Kingdom",
                             T ~ country)) %>%
  select(-Name) %>%
  select(country, duration, everything())

save(taux_souverains, file = "taux_souverains.rds")
