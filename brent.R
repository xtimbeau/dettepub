library(tidyquant)
library(tidyverse)
library(lubridate)
library(ggiraph)
brent_yf <- tq_get("BZ=F", from = "1990-01-01") |>
  mutate(com = "brent") |>
  add_row(symbol = "BZ=F", date = ymd("2026-03-09"), close = 104.3, com = "brent")
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
euro_yf <- tq_get("EURUSD=X", from = "1990-01-01") |>
  mutate(com = "euro dollar")
bitcoin_yf <- tq_get("BTC-USD", from = "1990-01-01") |>
  mutate(com = "bitcoin")
fert_yf <- tq_get("YAR.OL", from = "1990-01-01") |>
  mutate(com = "fertilizers")
corn_yf <- tq_get("ZC=F", from = "1990-01-01") |>
  mutate(com = "corn")
data <- bind_rows(brent_yf, dow_yf, gaz_yf, corn_yf, gold_yf, fert_yf, bitcoin_yf, euro_yf, bills_yf) |>
  mutate(ukr = date - ymd("2022-02-24"),
         iran = date - ymd("2026-02-28"),
         twelve = date - ymd("2025-06-13")) |>
  select(com, date, high, low, open, close,  ukr, iran, twelve) |>
  pivot_longer(cols = c(ukr, iran, twelve), names_to = "event", values_to = "days") |>
  group_by(com, event) |>
  mutate(
    across(c(high, low, open, close), ~.x/.x[days == max(days[days <=0])], .names = "{.col}_r")) |>
  ungroup() |>
  mutate(days = as.numeric(days),
         event = factor(event, c("iran", "twelve", "ukr")),
         tooltip = glue::glue("day {ifelse(days>0, '+', '')}{days}
                        {com} = {round(100*(close_r-1), 1)}% ({round(close)})")) |>
  filter(between(days, -60, 60))

gg1 <- ggplot(data |> filter(com %in% c("brent", "gaz", "fertilizers", "corn"))) +
  aes(x=days, y = (open_r + close_r)/2,  color = event, group = event ) +
  geom_line() +
  geom_pointrange_interactive(
    aes(
      ymin = open_r, ymax= close_r,
      tooltip = tooltip, data_id = days),
    hover_nearest = TRUE, size = 0.1, linewidth = 0.2) +
  scale_x_continuous(limits = c(-10, 60)) +
  geom_vline(xintercept = 0, linewidth = 0.1, color = "black", linetype = "11") +
  geom_hline(yintercept = 1, linewidth = 0.1, color = "black", linetype = "11") +
  scale_y_log10(limits = c(0.8, 1.5),
                breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3),
                labels = ~str_c(round((.x-1)*100), "%"),
                oob = scales::oob_keep) +
  xlab("Days since begining of conflict")+
  scale_color_manual(
    name = "",
    labels = c("Guerre US/Israël vs Iran", "Guerre des 12 jours", "Invasion de l'Ukraine par la Russie"),
    values = c("iran" = "purple3", "twelve"="pink1", "ukr" = "pink3")) +
  facet_wrap(vars(com), scales = "fixed") +
  ofce::theme_ofce(
    legend.position = "top",
    legend.direction = "horizontal")
 ofce::girafy(gg1, 1.25)

 gg2 <- ggplot(data |> filter(com %in% c("euro dollar", "10y bills", "gold", "dow"))) +
   aes(x=days, y = (open_r + close_r)/2,  color = event, group = event ) +
   geom_line() +
   geom_pointrange_interactive(
     aes(
       ymin = open_r, ymax= close_r,
       tooltip = tooltip, data_id = days),
     hover_nearest = TRUE, size = 0.5) +
   scale_x_continuous(limits = c(-10, 60)) +
   geom_vline(xintercept = 0, linewidth = 0.1, color = "black", linetype = "11") +
   geom_hline(yintercept = 1, linewidth = 0.1, color = "black", linetype = "11") +
   scale_y_log10(limits = c(0.9, 1.1),
                 breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3),
                 labels = ~str_c(round((.x-1)*100), "%"),
                 oob = scales::oob_keep) +
   xlab("Days since begining of conflict")+
   scale_color_manual(
     name = "",
     labels = c("Guerre US/Israël vs Iran", "Guerre des 12 jours", "Invasion de l'Ukraine par la Russie"),
     values = c("iran" = "purple3", "twelve"="pink1", "ukr" = "pink3")) +
   facet_wrap(vars(com), scales = "fixed") +
   ofce::theme_ofce(
     legend.position = "top",
     legend.direction = "horizontal")
 ofce::girafy(gg2, 1.25)
