library(insee)
library(tidyverse)
library(scales)
library(readr)
library(janitor)
library(ggplot2)
library(plotly)
library(lubridate)
library(patchwork)
library(slider)
library(ameco)

source("bdf.R")
bdf <- get_bdf("FM.D.FR.EUR.FR2.BB.FRMOYTEC10.HSTA") |>
  rename(time = time_period) |>
  mutate(time=ymd(time),
         time = floor_date(time, unit="month")) |>
  group_by(time) |>
  summarise(r10ans = mean(obs_value , na.rm=TRUE)) |>
  mutate(y = year(time),
         m = month(time)) |>
  select(time, y, m, r10ans)

taux_eu <- "irt_lt_mcby_m" |>
  eurostat::get_eurostat(
    filters = list(geo = "FR", int_rt = "MCBY") ) |>
  select(time, r10ans = values) |>
  mutate(y = year(time), m = month(time))

frigit <- c("20.0%", "10.2%", "9.5%", "9.8%", "9.2%", "8.8%", "7.9%", "6.5%", "6.1%", "6.5%", "6.4%", "6.4%",
            "6.3%", "7.5%", "8.2%", "8.3%", "8.9%", "8.1%", "7.3%", "7.5%", "6.8%", "6.0%", "5.7%", "5.9%",
            "5.1%", "5.1%", "4.6%", "4.3%", "4.3%", "3.8%", "4.1%", "5.1%", "4.5%", "4.0%", "4.0%", "3.8%",
            "3.8%", "3.8%", "3.8%", "3.8%", "3.8%", "3.9%", "3.8%", "3.8%", "3.7%", "3.6%", "3.7%", "4.0%",
            "6.6%", "5.7%", "5.4%", "5.4%", "4.1%", "3.9%", "4.4%", "4.6%", "4.4%", "4.5%", "4.3%", "4.5%",
            "4.4%", "4.5%", "4.4%", "4.5%", "4.6%", "4.5%", "4.5%", "4.4%", "4.4%", "4.3%", "4.8%", "5.7%",
            "5.6%", "5.4%", "5.0%", "4.7%", "4.5%", "4.3%", "4.1%", "3.8%", "3.6%", "3.6%", "3.7%", "3.9%",
            "3.9%", "3.8%", "3.7%", "3.8%", "3.7%", "3.6%", "3.3%", "3.2%", "3.1%", "3.1%", "3.0%", "3.0%",
            "3.0%", "2.9%", "3.0%", "3.0%", "3.0%", "3.0%", "3.0%", "3.1%", "3.1%", "3.1%", "3.1%", "3.2%",
            "3.2%", "3.1%", "3.1%", "3.2%", "3.3%", "3.5%", "3.7%", "4.4%", "4.9%", "5.0%", "5.1%", "5.2%",
            "5.9%", "6.7%", "6.4%", "6.3%", "6.7%", "7.6%", "8.9%", "7.4%", "6.0%", "5.4%", "4.7%", "4.7%",
            "5.2%", "5.8%", "6.1%", "5.7%", "6.3%", "6.7%", "6.9%", "5.7%", "5.9%", "4.5%", "3.8%", "3.8%",
            "3.8%", "3.6%", "4.4%", "5.0%", "5.9%", "6.8%", "7.0%", "7.0%", "6.7%", "6.4%", "6.4%", "6.0%",
            "6.3%", "6.5%", "6.7%", "5.9%", "5.8%", "5.6%", "5.7%", "5.3%", "5.5%", "6.2%", "6.6%", "6.8%",
            "7.2%", "8.3%", "8.8%", "8.5%", "8.1%", "8.9%", "10.9%", "10.5%", "10.4%", "11.0%", "10.7%",
            "10.6%", "13.9%", "16.4%", "16.6%", "14.6%", "13.3%", "11.7%", "8.8%", "9.6%", "9.1%", "8.8%",
            "9.9%", "9.0%", "8.6%", "6.9%", "7.4%", "7.6%", "6.4%", "5.6%", "4.7%", "4.7%", "5.5%", "5.0%",
            "4.9%", "4.2%", "4.2%", "3.5%", "3.9%", "4.4%", "4.3%", "3.7%", "3.2%", "3.4%", "2.6%", "2.3%",
            "1.7%", "0.9%", "0.5%", "0.8%", "0.8%", "0.2%", "-0.1%")

frigit <-  tibble( y=1800:2020, r_frig = frigit |> str_remove("%") |> as.numeric())

taux1 <- expand_grid(y=1800:2024, m=1:12) |>
  left_join(frigit, by='y') |>
  left_join(taux_eu, by=c("y","m")) |>
  arrange(desc(y),desc(m)) |>
  mutate(time = ym(str_c(y," ",m)))

taux.a <- taux1 |>
  group_by(y) |>
  summarize(r_frig=mean(r_frig, na.rm=TRUE),
            r10ans=mean(r10ans, na.rm=TRUE)) |>
  mutate(r = if_else(is.na(r10ans), r_frig, r10ans),
         year = y)
# ggplotly(ggplot(taux |> filter(y>1949))+geom_line(aes(x=time, y=r10ans))+geom_line(data=~.x |> filter(y<1987),aes(x=time, y=r_frig)) + theme_minimal())

variables <- c(
  pibpot = "OVGDP", og = "AVGDGP", ppib = "PVGD", vpib = "UVGD", qpib = "OVGD",
  dette = "UDGG", oneoff = "UDGGS", ci = "UYIGE", dep_prim = "UUCGI", rec_po = "UTTT",
  sprim = "UBLGI", solde = "UBLG", ira = "AYIGD", irl = "ILN", irs = "ISN" )
ameco <- ameco() |>
  filter(code %in% variables, geo == "FRA")

pib <- get_idbank_list("CNA-2020-PIB") |>
  filter(INDICATEUR=="CNA_PIB", PRIX_REF =="VAL") |>
  pull(idbank) |>
  get_insee_idbank() |>
  mutate(year = as.numeric(TIME_PERIOD),
         pib = as.numeric(OBS_VALUE)) |>
  select(year, pib) |>
  arrange(year) |>
  mutate(
    g = pib/lag(pib)-1,
    g_bck = (lag(pib)/lag(pib, 6))^(1/5)-1,
    g_bck3 = (lag(pib)/lag(pib, 3))^(1/2)-1,
    g_fwd = head(
      slide_dbl(c(pib, last(pib)*(last(g_bck3))^(1:9)),~(last(.x)/first(.x))^(1/length(.x))-1, .after=9),
      -9) )

dette_eu <- "gov_10dd_edpt1" |>
  eurostat::get_eurostat(
    filters = list(geo = "FR", unit = "MIO_EUR", sector = c("S1", "S13"),
                   na_item  = c("B1GQ", "P51G", "B9", "GD", "D41PAY", "B5GQ"))) |>
  select(geo, time, na_item, values) |>
  drop_na(values) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  mutate(
    d = GD/B1GQ,
    r = D41PAY/GD,
    ir = D41PAY/B1GQ,
    s = B9/B1GQ) |>
  arrange(desc(time))

pib_cna14 <- get_idbank_list("CNA-2014-PIB") |>
  filter(INDICATEUR=="CNA_PIB", PRIX_REF =="VAL") |>
  pull(idbank) |>
  get_insee_idbank() |>
  transmute(year = as.numeric(TIME_PERIOD),
         pib = as.numeric(OBS_VALUE)) |>
  select(year, pib)
interet_cna14 <- get_idbank_list("CNA-2014-CSI") |>
    filter(OPERATION %in%c("D41"), SECT_INST == "S13") |>
    pull(idbank) |>
    get_insee_idbank() |>
    transmute(year = TIME_PERIOD, d41  = OBS_VALUE, ae  = IDBANK) |>
    pivot_wider(names_from = ae, values_from = d41) |>
    transmute(year, d41 = `010563497`-`010563498`) |>
    left_join(pib_cna14, by="year") |>
    transmute(
      time = ymd(year, truncated=2),
      year = as.numeric(year),
      d41 = d41/pib)  |>
    arrange(desc(year))

dettet_eu <- "gov_10q_ggdebt" |>
  eurostat::get_eurostat(
    filters = list(geo = "FR", unit = c("PC_GDP"), sector = "S13",
                   na_item  = c("GD"))) |>
  select(geo, time, na_item, values) |>
  drop_na(values) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  arrange(desc(time))


apu <- melodi::get_all_data("DD_CNA_APU") |>
  as_tibble() |>
  filter(STO == c("D41"),
         CONSOLIDATION == "N",
         REF_SECTOR == "S13") |>
  drop_na(OBS_VALUE) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated = 2),
    value = OBS_VALUE,
    ACCOUNTING_ENTRY) |>
  pivot_wider(names_from = ACCOUNTING_ENTRY, values_from = value) |>
  mutate(intnet = D - C) |>
  arrange(desc(time))

# interets <- apu |>
#   filter(OPERATION %in%c("D41", "B9NF"), SECT_INST == "S13") |>
#   pull(idbank) |>
#   get_insee_idbank() |>
#   mutate(year = TIME_PERIOD) |>
#   left_join(pib, by="year") |>
#   mutate(interets = OBS_VALUE/pib,
#          year = TIME_PERIOD) |>
#   select(year, interets, pib, g, g_bck, g_fwd, IDBANK) |>
#   mutate(type = case_when(
#     IDBANK=="010563497" ~ "verses",
#     IDBANK=="010563498" ~ "recus",
#     IDBANK=="010563277" ~ "cf"),
#     year = as.numeric(year)) |>
#   select(-IDBANK) |>
#   pivot_wider(names_from = type, values_from = interets) |>
#   mutate(defp = -cf - verses,
#          time = lubridate::ym(str_c(year, " 01"))) |>
#   arrange(desc(time))

# ids <- get_idbank_list("CNA-2014-DETTE-APU") |>
#   filter(SECT_INST == "S13", PRIX_REF =="PCT_PIB") |>
#   pull(idbank, name=INDICATEUR)
#
# dette <- 	ids |>
#   get_insee_idbank() |>
#   mutate(IDBANK = names(ids)[map_int(IDBANK, ~which(.x==ids))]) |>
#   select(year = TIME_PERIOD,
#          dette = OBS_VALUE,
#          var = IDBANK) |>
#   mutate(var = str_remove(var, "CNA_FINANCES_")) |>
#   pivot_wider(names_from = var, values_from = dette) |>
#   rename(dette = DETTE, dette_nette = DETTENETTE, deficit = DEFICIT) |>
#   left_join(pib, by="year") |>
#   mutate(dstar = g_bck/(1+g_bck)*dette/100,
#          year = as.numeric(year),
#          time = lubridate::ym(str_c(year, " 01")))

dette_ameco <- ameco |>
  filter(code %in% c("UDGG", "UBLG", "UVGD", "UYIGE" ,"ILN"), unit_code==0) |>
  select(code, year, value) |>
  pivot_wider(names_from = code, values_from = value) |>
  rename(pib = UVGD, dette = UDGG, interets = UYIGE, deficit = UBLG, r10 = ILN) |>
  mutate(
    time = ym(str_c(year, '-01')),
    dette = dette / pib * 100,
    verses = interets/pib * 100,
    deficit = deficit / pib * 100,
    g = pib/lag(pib)-1,
    g_bck = (lag(pib)/lag(pib, 6))^(1/5)-1,
    g_bck3 = (lag(pib)/lag(pib, 3))^(1/2)-1,
    g_fwd = head(slide_dbl(c(pib, last(pib)*(1+last(g_bck3))^(1:9)), ~(last(.x)/first(.x))^(1/length(.x))-1, .after=9), -9),
    dstar = g_fwd/(1+g_fwd)*dette/100) |>
  relocate(time, year) |>
  full_join(interet_cna14, by = c("time")) |>
  mutate(verses = ifelse(is.na(verses), 100*d41, verses)) |>
  select(-starts_with("year")) |>
  mutate(year = year(time)) |>
  arrange(year)

# dette historique de la France, source FMI (https://www.imf.org/external/pubs/ft/wp/2010/wp10245.pdf & https://data.imf.org/?sk=806ED027-520D-497F-9052-63EC199F5E63)
HPDD <- read_csv("./data/HPDD_04-26-2020 18-59-27-12.csv") |>
  janitor::clean_names() |>
  rename(year=time_period, dette_IMF=value, country = country_name) |>
  filter(country=="France") |>
  select(year, dette_IMF) |>
  full_join(dette_ameco, by="year") |>
  arrange(year) |>
  left_join(taux.a, by="year") |>
  mutate(
    dette_IMF = ifelse(is.na(dette_IMF),
                       lag(dette_IMF)/(1+g)-deficit,
                       dette_IMF) ) |>
  mutate(
    dette_IMF = ifelse(is.na(dette_IMF),
                       lag(dette_IMF)/(1+g)-deficit,
                       dette_IMF) ) |>
  left_join(pib |> select(year, g_fwd.pib = g_fwd), by = "year") |>
  mutate(g_fwd = ifelse(is.na(g_fwd), g_fwd.pib, g_fwd)) |>
  mutate(rapp2=verses/dette_IMF,
         rapp1=verses/dette,
         time = ym(str_c(year," 01")),
         ec_app = if_else(year<1995, rapp2-g_fwd, rapp1-g_fwd),
         ec_app_p = if_else(ec_app>=0, ec_app, NA_real_),
         ec_app_m  = if_else(ec_app<0, ec_app, NA_real_),
         ec = r/100-g_fwd,
         ec_p = if_else(ec>=0, ec, NA_real_),
         ec_m  = if_else(ec<0, ec, NA_real_))  |>
  arrange((year))

taux <- left_join(taux1, dette_ameco |> select(year, r10), by=c("y"="year"))

gdette <- ggplot(HPDD |> filter(year>=1949))+
  geom_step(data=~filter(.x,year<1995),aes(x=time, y=dette_IMF), color="dodgerblue4", direction="mid")+
  geom_step(aes(x=time, y=dette), col="dodgerblue1", direction="mid")+
  ylim(c(0,120))+
  geom_segment(aes(y=0, yend=0, x=ym("1949 01"), xend=ym("1995 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=1, x=ym("1949 01"), xend=ym("1949 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=1, x=ym("1995 01"), xend=ym("1995 01")), size=0.1, col="grey25")+
  annotate(geom="text", y=1, x=ym("1949 2"),
           label = "IMF historical public debt database", hjust=0, vjust = 0, size=1.5)+
  geom_segment(aes(y=0, yend=0, x=ym("1996 01"), xend=ym("2020 06")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=1, x=ym("1996 01"), xend=ym("1996 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=1, x=ym("2020 06"), xend=ym("2020 06")), size=0.1, col="grey25")+
  annotate(geom="text", y=1, x=ym("1996 2"), label = "Eurostat", hjust=0, vjust = 0, size=1.5)+
  scale_x_date(breaks = ym(str_c(c(1949, seq(1960, 2010, 10), 2024), " 01")), date_labels = "%Y", limits=c(ym("1949 01"), ym("2024 12")))+
  labs(title="Dette publique en % du PIB")

gtaux <- ggplot(taux |> filter(y>=1949))+
  geom_step(data=~filter(.x, y>= 1987), aes(x=time, y=r10ans), col="dodgerblue1", direction="mid")+
  geom_step(data=~filter(.x, y<1987),aes(x=time, y=r_frig), col="dodgerblue4", direction="mid")+
  geom_segment(aes(y=0, yend=0, x=ym("1949 01"), xend=ym("1987 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.1, x=ym("1949 01"), xend=ym("1949 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.1, x=ym("1987 01"), xend=ym("1987 01")), size=0.1, col="grey25")+
  annotate(geom="text", y=0.1, x=ym("1949 2"), label = "CN 2014 INSEE et CGEDD J.Friggit", hjust=0, vjust = 0, size=1.5)+
  geom_segment(aes(y=0, yend=0, x=ym("1987 01"), xend=ym("2024 12")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=.1, x=ym("1987 01"), xend=ym("1987 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=.1, x=ym("2021 11"), xend=ym("2024 12")), size=0.1, col="grey25")+
  annotate(geom="text", y=0.1, x=ym("1988 2"), label = "Eurostat", hjust=0, vjust = 0, size=1.5)+
  scale_x_date(breaks = ym(str_c(c(1949, seq(1960, 2010, 10), 2024), " 01")), date_labels = "%Y", limits=c(ym("1949 01"), ym("2024 12")))+
  labs(title="Taux souverain (10 ans, en %/an)")

gcharge <- ggplot(dette_ameco)+
  geom_step(data = ~.x |> filter(year>=1978), aes(x=time, y=verses), col="dodgerblue1", direction="mid")+
  geom_step(data = ~.x |> filter(year<1978), aes(x=time, y=verses), col="dodgerblue4", direction="mid")+
  geom_segment(aes(y=0, yend=0, x=ym("1949 01"), xend=ym("1978 04")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0, x=ym("1978 08"), xend=ym("2024 06")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.05, x=ym("1949 01"), xend=ym("1949 01")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.05, x=ym("1978 04"), xend=ym("1978 04")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.05, x=ym("1978 08"), xend=ym("1978 08")), size=0.1, col="grey25")+
  geom_segment(aes(y=0, yend=0.05, x=ym("2024 06"), xend=ym("2024 06")), size=0.1, col="grey25")+
  annotate(geom="text", y=0.05, x=ym("1949 2"), label = "CN 2014 INSEE", hjust=0, vjust = 0, size=1.5)+
  annotate(geom="text", y=0.05, x=ym("1987 09"), label = "CN 2020 INSEE", hjust=0, vjust = 0, size=1.5)+
  scale_x_date(breaks = ym(str_c(c(1949, seq(1960, 2010, 10), 2024), " 01")), date_labels = "%Y", limits=c(ym("1949 01"), ym("2024 12")))+
  ylim(c(0,4))+
  labs(title="Charge d'intérêts en % du PIB")

(gecartc <- ggplot(HPDD |> filter(year>=1949))+
    geom_step(data=~filter(.x,year<1987),aes(x=time, y=100*ec), color="dodgerblue4", direction="mid")+
    geom_step(data=~filter(.x,year>=1987),aes(x=time, y=100*ec), color="dodgerblue1", direction="mid")+
    geom_col(aes(x=time, y=100*ec_p), fill="red", alpha=0.1, col=NA, width=years(1)/days(1),)+
    geom_col(aes(x=time, y=100*ec_m), fill="green", alpha=0.1, width=years(1)/days(1), col=NA)+
    geom_segment(aes(y=0, yend=0, x=ym("1949 01"), xend=ym("1986 01")), size=0.1, col="grey25")+
    geom_segment(aes(y=0, yend=0.1, x=ym("1949 01"), xend=ym("1949 01")), size=0.1, col="grey25")+
    geom_segment(aes(y=0, yend=0.1, x=ym("1986 01"), xend=ym("1986 01")), size=0.1, col="grey25")+
    annotate(geom="text", y=0.1, x=ym("1949 2"), label = "CN 2020 INSEE/CGEDD", hjust=0, vjust = 0, size=1.5)+
    geom_segment(aes(y=0, yend=0, x=ym("1987 01"), xend=ym("2024 06")), size=0.1, col="grey25")+
    geom_segment(aes(y=0, yend=.1, x=ym("1987 01"), xend=ym("1987 01")), size=0.1, col="grey25")+
    geom_segment(aes(y=0, yend=.1, x=ym("2024 01"), xend=ym("2024 06")), size=0.1, col="grey25")+
    annotate(geom="text", y=-0.1, x=ym("1988 2"), label = "CN 2020 INSEE", hjust=0, vjust = 1, size=1.5)+
    scale_x_date(breaks = ym(str_c(c(1949, seq(1960, 2010, 10), 2024), " 01")), date_labels = "%Y", limits=c(ym("1949 01"), ym("2024 12")))+
    labs(title="r - g (en %/an)"))

g_altereco  <- (gdette + gtaux)/(gecartc+gcharge) +
  plot_annotation(caption="Sources: INSEE, Banque de France, FMI, J. Friggit/CGEDD \n code: github.com/OFCE/AE_11-21") &
  ylab("") &
  xlab("") &
  theme_minimal(base_family = "Nunito") &
  theme(plot.title = element_text(size=10, face = "plain", hjust = 0.5),
        plot.caption = element_text(size=7, face = "plain", hjust = 0),
        axis.text =  element_text(size=6),
        panel.grid.major = element_line(color="grey90", size=0.25),
        panel.grid.minor = element_line(color="grey90", size=0.25))
ggsave(plot=g_altereco, filename="altereco.svg", width = 180, height = 180, units = "mm")
