library(tidyquant)
library(tidyverse)
library(lubridate)
brent_yf <- tq_get("BZ=F", from = "1990-01-01")
brent_yf |> arrange(date) |> readr::write_csv("brent 2007-2026.csv")

brent_yf <- brent_yf |>
  mutate(ukr = date - ymd("2022-02-24"),
         iran = date - ymd("2026-02-28"),
         twelve = date - ymd("2025-06-13"))

brent <- brent_yf |>
  select(date, high, low, open, close,  ukr, iran, twelve) |>
  pivot_longer(cols = c(ukr, iran, twelve), names_to = "event", values_to = "days")
ggplot(brent) +
  geom_errorbar(aes(x=days, ymax = high,  ymin = low,  color = event, group = event )) +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_y_log10(limits = c(50, 150)) +
  ofce::theme_ofce()
