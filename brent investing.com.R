library(tidyquant)
library(tidyverse)
library(lubridate)
library(ggiraph)

commodities <- c(
  "Brent" = "https://fr.investing.com/commodities/brent-oil-historical-data",
  "Gaz" = "https://fr.investing.com/commodities/natural-gas-historical-data"
)

data <- imap_dfr(
  commodities,
  ~ .x |>
    read_html() |>
    html_table() |>
    pluck(2) |>
    transmute(
      date = as.Date(Date, format = "%d/%m/%Y"),
      close = as.numeric(gsub(",", ".", Dernier)),
      open = as.numeric(gsub(",", ".", Ouv.)),
      high = as.numeric(gsub(",", ".", `Plus Haut`)),
      low = as.numeric(gsub(",", ".", `Plus Bas`)),
      commodity = .y
    )
)
elab <- c(
  iran = "US/Israël vs Iran war",
  twelve = "12 days war",
  ukr = "Russian agression of Ukraine",
  GWI = "Gulf War I",
  GWII = "Gulf War II"
)
data_h <- imap_dfr(
  commodities,
  ~ {
    files <- list.files("data", pattern = .y, full.names = TRUE)
    map(files, \(.f) {
      read_delim(
        .f,
        delim = ",",
        col_types = cols(
          Date = col_character(),
          Dernier = col_character(),
          `Ouv.` = col_character(),
          ` Plus Haut` = col_character(),
          `Plus Bas` = col_character(),
          `Vol.` = col_character(),
          `Variation %` = col_character()
        )
      )
    }) |>
      bind_rows() |>
      mutate(commodity = .y) |>
      distinct(Date, .keep_all = TRUE) |>
      transmute(
        date = as.Date(Date, format = "%d/%m/%Y"),
        close = as.numeric(gsub(",", ".", Dernier)),
        open = as.numeric(gsub(",", ".", Ouv.)),
        high = as.numeric(gsub(",", ".", ` Plus Haut`)),
        low = as.numeric(gsub(",", ".", `Plus Bas`)),
        commodity = .y
      )
  }
) |>
  bind_rows(data) |>
  distinct(date, commodity, .keep_all = TRUE) |>
  mutate(
    ukr = date - ymd("2022-02-24"),
    iran = date - ymd("2026-02-28"),
    twelve = date - ymd("2025-06-13"),
    GWI = date - ymd("1990-08-02"),
    GWII = date - ymd("2003-03-20")
  ) |>
  select(
    commodity,
    date,
    high,
    low,
    open,
    close,
    ukr,
    iran,
    twelve,
    GWI,
    GWII
  ) |>
  pivot_longer(
    cols = c(ukr, iran, twelve, GWI, GWII),
    names_to = "event",
    values_to = "days"
  ) |>
  group_by(commodity, event) |>
  mutate(
    across(
      c(high, low, open, close),
      ~ .x / .x[days == max(days[days <= 0])],
      .names = "{.col}_r"
    )
  ) |>
  ungroup() |>
  mutate(
    days = as.numeric(days),
    event = factor(event, names(elab)),
    event_label = elab[event],
    unit = case_match(
      commodity,
      "Brent" ~ "$/barrel",
      "Gaz" ~ "$/MWh"
    ),
    conversion = case_match(
      commodity,
      "Brent" ~ 1,
      "Gaz" ~ 0.00000029307 * 1000 * 10000
    ),
    cent = (close + open) / 2 * conversion,
    cent_r = (close_r + open_r) / 2,
    tooltip = glue::glue(
      "
      <b>{event_label}</b> day {ifelse(days>0, '+', '')}{days}
                        {commodity} = {round(100*(cent_r-1), 1)}% ({round(cent, 1)}{unit})"
    )
  ) |>
  filter(between(days, -60, 60))

gg1 <- ggplot(
  data_h |> filter(commodity %in% c("Brent", "Gaz"))
) +
  aes(
    x = days,
    y = (open_r + close_r) / 2,
    color = event,
    group = event,
    linewidth = event,
    size = event
  ) +
  geom_line() +
  geom_pointrange_interactive(
    aes(
      ymin = open_r,
      ymax = close_r,
      tooltip = tooltip,
      data_id = days
    ),
    hover_nearest = TRUE,
    linewidth = 0.05
  ) +
  scale_x_continuous(limits = c(-10, 60)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.1,
    color = "black",
    linetype = "11"
  ) +
  geom_hline(
    yintercept = 1,
    linewidth = 0.1,
    color = "black",
    linetype = "11"
  ) +
  scale_y_log10(
    limits = c(0.8, 1.5),
    breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3),
    labels = ~ str_c(round((.x - 1) * 100), "%"),
    oob = scales::oob_keep
  ) +
  xlab("Days since begining of conflict") +
  ylab("(Open+Close)/2") +
  scale_color_manual(
    name = "",
    labels = c(
      "US/Israël vs Iran war",
      "12 days war",
      "Russian agression of Ukraine",
      "Gulf War I",
      "Gulf War II"
    ),
    values = c(
      "iran" = "purple3",
      "twelve" = "mediumorchid1",
      "ukr" = "pink3",
      "GWI" = "cyan3",
      "GWII" = "cyan4"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "iran" = .5,
      "twelve" = .1,
      "ukr" = .1,
      "GWI" = .1,
      "GWII" = .1
    )
  ) +
  scale_size_manual(
    values = c(
      "iran" = .2,
      "twelve" = .025,
      "ukr" = .025,
      "GWI" = .025,
      "GWII" = .025
    )
  ) +
  facet_wrap(vars(commodity), scales = "fixed") +
  guides(
    color = guide_legend(nrow = 2),
    linewidth = "none",
    fill = "none",
    shape = "none",
    size = "none"
  ) +
  ofce::theme_ofce(
    marquee = TRUE,
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  ofce::ofce_caption(
    wrap = 0,
    ofce = FALSE,
    source = "Investing.com, calculs XT",
    note = "Les données sont quotidiennes, généralement les jours ouvrés ; la barre marque le plus haut et le plus bas ; la valeur centrale est la moyenne entre l'ouverture et la cloture. Les évolutions sont calculées par rapport à la dernière cotation disponible avant le déclenchement du conflit"
  )
ofce::girafy(gg1, 1.25)
