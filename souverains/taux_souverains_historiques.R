library(tidyverse)
library(rvest)
library(vroom)

if(file.exists("taux_souverains_historiques.csv"))
  tsh <- vroom::vroom("taux_souverains_historiques.csv") else {
    ff <- list.files(pattern = "france.*\\.csv")
    france <- map_dfr(ff, ~vroom(.x, locale = vroom::locale(decimal_mark=",")) %>%
                         transmute(date = dmy(Date),
                                   france = Dernier) ) |>
      distinct(date, .keep_all = TRUE)
    ff <- list.files(pattern = "germany.*\\.csv")
    germany <- map_dfr(ff, ~vroom(.x, locale = vroom::locale(decimal_mark=",")) %>%
      transmute(date = dmy(Date),
                germany = Dernier) ) |>
        distinct(date, .keep_all = TRUE)
    ff <- list.files(pattern = "italy.*\\.csv")
    italy <- map_dfr(ff, ~vroom(.x, locale = vroom::locale(decimal_mark=",")) %>%
                         transmute(date = dmy(Date),
                                   italy = Dernier) ) |>
      distinct(date, .keep_all = TRUE)

    tsh <- germany |>
      left_join(france, by = "date")  |>
      left_join(italy, by = "date")  |>
      transmute(date,
                france,
                germany,
                italy)  |>
      pivot_longer(-date, names_to = "pays", values_to = "taux")

  }

taux_souverains_historiques_MAJ <- tibble(country = c("france", "germany", "italy")) %>%
  mutate(data = map(country, ~ paste0("https://fr.investing.com/rates-bonds/", .,
                                      "-10-year-bond-yield-historical-data") %>%
                      read_html() %>%
                      html_table() %>%
                      .[[1]] %>%
                      transmute(date = as.Date(Date, format = "%d/%m/%Y"),
                                value = as.numeric(gsub(",", ".", Dernier))))) %>%
  unnest(cols = data) %>%
  spread(country, value) %>%
  transmute(date,
            france,
            germany,
            italy) %>%
  pivot_longer(-date, names_to = "pays", values_to = "taux")

min_date <- min(taux_souverains_historiques_MAJ$date)

tsh <- tsh %>%
  filter(date < min_date) %>%
  bind_rows(taux_souverains_historiques_MAJ) %>%
  unique()

vroom::vroom_write(tsh, file = "taux_souverains_historiques.csv")

return(tsh)
