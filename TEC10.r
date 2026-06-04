library(httr2)
library(readr)
library(dplyr)

# ------------------------------------------------------------
# TEC10 France (quotidien, depuis nov. 2004)
# Source : Banque de France / CNO-TEC via Webstat
# ------------------------------------------------------------
get_tec10_bdf <- function() {
  url <- "https://webstat.banque-france.fr/export/csv-columns/fr/selection/5385693"

  raw <- request(url) |>
    req_perform() |>
    resp_body_string(encoding = "UTF-8")

  read_delim(
    I(raw),
    delim      = ";",
    skip       = 6,
    col_names  = c("date", "TEC1", "TEC10", "TEC15", "TEC2", "TEC20",
                   "TEC25", "TEC3", "TEC30", "TEC5", "TEC7"),
    locale     = locale(decimal_mark = ",", date_format = "%Y-%m-%d"),
    na         = c("-", ""),
    show_col_types = FALSE
  ) |>
    mutate(date = as.Date(date)) |>
    filter(!is.na(date), !is.na(TEC10)) |>
    arrange(date)
}

# ------------------------------------------------------------
# Bund Allemagne proxy (quotidien, depuis sept. 2004)
# Source : BCE - Courbe AAA zone euro (modèle Svensson)
# Note : agrège les souverains AAA de la zone euro (dominé par
#        l'Allemagne). Proxy standard du taux sans risque EUR.
# ------------------------------------------------------------
get_bund_ecb <- function() {
  url <- paste0(
    "https://data-api.ecb.europa.eu/service/data/",
    "YC/B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y",
    "?format=csvdata&startPeriod=2004-01-01"
  )

  raw <- request(url) |>
    req_perform() |>
    resp_body_string()

  read_csv(I(raw), show_col_types = FALSE) |>
    select(date = TIME_PERIOD, Bund = OBS_VALUE) |>
    mutate(date = as.Date(date)) |>
    filter(!is.na(Bund)) |>
    arrange(date)
}

# ------------------------------------------------------------
# US 10 ans (quotidien, depuis janv. 1962)
# Source : FRED - DGS10 (Fed)
# ------------------------------------------------------------
get_us10y_fred <- function() {
  read_csv(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DGS10",
    show_col_types = FALSE
  ) |>
    rename(date = observation_date, US10Y = DGS10) |>
    filter(!is.na(US10Y)) |>
    arrange(date)
}

# ------------------------------------------------------------
# Chargement
# ------------------------------------------------------------
tec10 <- get_tec10_bdf()
bund  <- get_bund_ecb()
us10y <- get_us10y_fred()

return(list(tec10 = tec10, bund = bund, us10y = us10y))
