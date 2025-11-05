library(tidyverse)
library(ameco)
library(insee)
library(melodi)
library(ofce)
library(slider)
library(scales)
library(patchwork)

set_theme(theme_ofce())

nego <- get_idbank_list("DETTE-NEGOCIABLE-ETAT") |>
  filter(NATURE == "ENCOURS") |>
  pull(idbank) |>
  get_insee_idbank() |>
  mutate(
    code = case_match(
      IDBANK,
      "001711531" ~ "totale",
      "001711532" ~ "matm1ae",
      "001711533" ~ "matp1ae",
      "001719708" ~ "totalf",
      "001719709" ~ "matm1af",
      "001719710" ~ "matp1af",
      "001738853" ~ "fixe",
      "001738854" ~ "inflation",
      "001739081" ~ "total")) |>
  select(time = DATE, code, value = OBS_VALUE) |>
  pivot_wider(names_from = code, values_from = value ) |>
  mutate(across(-time, ~.x/total))

# nego |> ggplot() +geom_line(aes(x=time, y = inflation, group=1)) + scale_ofce_date()

pib <-  "CNT-2020-PIB-EQB-RF" |>
  get_idbank_list() |>
  filter(OPERATION == "PIB",
         NATURE == "VALEUR_ABSOLUE",
         VALORISATION == "V") |>
  pull(idbank) |>
  get_insee_idbank() |>
  select(time = DATE, pib = OBS_VALUE)

d41 <- "CNT-2020-CSI" |>
  get_idbank_list() |>
  filter(OPERATION == "D41", SECT_INST == "S13") |>
  pull(idbank) |>
  get_insee_idbank() |>
  transmute(
    time=DATE, value=OBS_VALUE,
    code = case_match(
      IDBANK,
      "011794584" ~ "verse",
      "011794585" ~ "recu")) |>
  pivot_wider(names_from = code) |>
  mutate(net = verse-recu)

taux_eu <- "irt_lt_mcby_m" |>
  eurostat::get_eurostat(
    filters = list(geo = "FR", int_rt = "MCBY") ) |>
  select(time, r10ans = values) |>
  mutate(y = year(time), m = month(time)) |>
  mutate(time = floor_date(time, "quarter")) |>
  group_by(time) |>
  summarize(r10ans= mean(r10ans))

apu_a <- melodi::get_all_data("DD_CNA_APU") |>
  as_tibble() |>
  filter(STO =="_MA_PPIB",
         REF_SECTOR == "S13")|>
  drop_na(OBS_VALUE) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated = 2),
    def_ma = OBS_VALUE/100) |>
  bind_rows(tibble(time = ymd("2025-01-01"), def_ma = -5.4/100)) |>
  cross_join(tibble(q = 1:4)) |>
  mutate(time = ymd(str_c(year(time),"-", (q-1)*3+1, "-01"))) |>
  arrange(time) |>
  select(-q)

dette_trim <- "DETTE-TRIM-APU-2020" |>
  get_idbank_list() |>
  filter(
    SECT_INST  == "S13",
    INDICATEUR %in% c("DETTE_MAASTRICHT", "DETTE_NETTE"),
    NATURE == "VALEUR_ABSOLUE",
    DETTE_MAASTRICHT_INTRUMENTS == "F") |>
  pull(idbank) |>
  get_insee_idbank() |>
  select(time = DATE, IDBANK, value = OBS_VALUE) |>
  transmute(
    time, value,
    code = case_match(
      IDBANK,
      "010777616" ~ "maastricht",
      "010777611" ~ "nette")) |>
  pivot_wider(names_from = code) |>
  left_join(pib, by="time") |>
  left_join(d41 |> select(time, d41 =net), by = "time") |>
  left_join(apu_a, by = "time") |>
  left_join(taux_eu, by = "time") |>
  arrange(time) |>
  mutate(
    d414 = slider::slide_dbl(d41/1000, ~mean(.x)*4, .before=3),
    rapp = d414/maastricht,
    pib4 = slider::slide_dbl(pib/1000, ~mean(.x)*4, .before=3),
    d41 = d41/pib,
    g = pib/lag(pib) - 1,
    g4 = pib4/lag(pib4, 4) - 1,
    g_bck2 = (pib/lag(pib, 8))^(1/7)-1,
    g_fwd = head(slider::slide_dbl(c(pib, last(pib)*cumprod(rep(1+last(g_bck2), 39))),
                           ~(last(.x)/first(.x))^(1/9)-1,
                           .after=40), -39),
    dm = maastricht/pib4,
    dn = nette/pib4 ) |>
  arrange(time) |>
  fill(g4, .direction = "up") |>
  mutate(
    ec_app = rapp-g_fwd,
    ec = r10ans/100-g_fwd,
    ss = -lag(dm)* g4*(1-r10ans/100)/(1+g4),
    ss_app = -lag(dm)* g4*(1-rapp)/(1+g4),
    ssp = -lag(dm)* (g4-r10ans/100)/(1+g4),
    ssp_app = -lag(dm)* (g4-rapp)/(1+g4),
    ecp = ifelse(ec>0, "p", "n"),
    sppp = ifelse(ss>def_ma, "p", "n"),
    time = factor(time, time))


return(list(trim = dette_trim, nego = nego))
