library(tidyquant)
library(tidyverse)
library(lubridate)
brent_yf <- tq_get("BZ=F", from = "1990-01-01") |>
  mutate(com = "brent")
dow_yf <- tq_get("YM=F", from = "1990-01-01") |>
  mutate(com = "dow")
gaz_yf <- tq_get("NG=F", from = "1990-01-01") |>
  mutate(com = "gaz")
oil_yf <- tq_get("CL=F", from = "1990-01-01") |>
  mutate(com = "oil")
gold_yf <- tq_get("GC=F", from = "1990-01-01") |>
  mutate(com = "gold")
bills_yf <- tq_get("ZN=F", from = "1990-01-01") |>
  mutate(com = "10y bills")
fert_yf <- tq_get("YAR.OL", from = "1990-01-01") |>
  mutate(com = "fertilizers")
data <- bind_rows(brent_yf, dow_yf, gaz_yf, oil_yf, gold_yf, fert_yf) |>
  mutate(ukr = date - ymd("2022-02-24"),
         iran = date - ymd("2026-02-27"),
         twelve = date - ymd("2025-06-13")) |>
  select(com, date, high, low, open, close,  ukr, iran, twelve) |>
  pivot_longer(cols = c(ukr, iran, twelve), names_to = "event", values_to = "days") |>
  group_by(com, event) |>
  mutate(across(c(high, low, open, close), ~.x/.x[days==0])) |>
  ungroup() |>
  mutate(days = as.numeric(days),
         event = factor(event, c("iran", "twelve", "ukr"))) |>
  filter(between(days, -60, 60))

ggplot(data) +
  geom_line(aes(x=days, y = close,  color = event, group = event )) +
  scale_x_continuous(limits = c(-30, 60)) +
  geom_vline(xintercept = 0, linewidth = 0.1, color = "black", linetype = "11") +
  scale_y_log10() +
  scale_color_manual(values = c("iran" = "purple3", "twelve"="pink1", "ukr" = "pink3")) +
  facet_wrap(vars(com), scales = "free_y") +
  ofce::theme_ofce()
