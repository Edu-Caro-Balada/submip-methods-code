# ============================================================
# SubMIP Methods Code (R)
# Autor: Edu Caro Balada
# ============================================================


# ----------------------------
# Librerías
# ----------------------------
library(readxl)
library(tidyverse)
library(signal)
library(data.table)
library(gt)
library(zoo)
library(readr)
library(tidyr)
library(dplyr)


# ----------------------------
# Configuración de rutas
# ----------------------------

raw_folder <- "PATH_A_TUS_CSV/"
mip_reference_file <- "PATH_A_MIP.xlsx"

# Salida 
output_file <- "BBDD_SubMIP.csv"


# ----------------------------
# Lectura de múltiples CSV + filename
# ----------------------------
df_full <- list.files(path = raw_folder, pattern = ".csv", full.names = T) %>%
  map_df(function(x) fread(x, sep = ",", header = FALSE) %>%
           mutate(filename = gsub(" .csv", "", basename(x))))

names(df_full) <- c(
  "Player_Id", "Player_displ", "Time", "Elap_Time", "Lat", "Lon", "Speedkmh", "Inst_acc", "HR",
  "V1", "filename"
)


# ----------------------------
# Conversión velocidad y selección
# ----------------------------
df_full <- df_full %>%
  group_by(filename, Player_displ) %>%
  mutate(Speed_ms = Speedkmh / 3.6) %>%
  select(Time, Speed_ms, filename, Player_displ, Player_Id)


# ----------------------------
# Filtros Butterworth
# ----------------------------
bf <- butter(n = 4, W = 0.15, type = "low", plane = "z")
bfacc <- butter(n = 1, W = 0.65, type = "low", plane = "z")

df_full <- df_full %>%
  group_by(filename, Player_displ) %>%
  mutate(Speed_msf3 = filtfilt(bf, Speed_ms)) %>%
  mutate(Speed_msf3 = ifelse((Speed_msf3 < 0), 0, Speed_msf3))


# ----------------------------
# Cálculo de variables
# ----------------------------
df_full <- df_full %>%
  group_by(filename, Player_displ) %>%
  mutate(Acc = ((Speed_msf3) - lag(Speed_msf3)) / ((0.1))) %>%
  mutate(Accf = filtfilt(bfacc, Acc)) %>%
  mutate(ES = (0.0037 * (Speed_msf3)^2 / 9.81) + Accf / 9.81) %>% #cambiado parentesis antes de cuadrado a despues
  mutate(EM = sqrt(Accf^2 + 9.81^2) / 9.81) %>%
  mutate(EC = ifelse(
    Accf > 0,
    ((155.4 * ES^5) - (30.4 * ES^4) - (43.3 * ES^3) + (46.3 * ES^2) + (19.5 * ES) + (3.6 * 1.29)) * EM + (0.01 * (Speed_msf3)^2),
    ((-30.4 * ES^4) - (5.0975 * ES^3) + (46.3 * ES^2) + (17.696 * ES) + (3.6 * 1.29)) * EM + (0.01 * (Speed_msf3)^2)
  )) %>%
  mutate(MetPowf = EC * Speed_msf3) %>%
  mutate(dist = Speed_msf3 * 0.1) %>%
  mutate(Accdens = abs(Accf)) %>%
  mutate(
    "SpeedHS" = case_when(
      Speed_msf3 <= 5.5 ~ 0,
      Speed_msf3 > 5.5  ~ Speed_msf3
    ),
    "SpeedSD" = case_when(
      Speed_msf3 <= 7 ~ 0,
      Speed_msf3 > 7  ~ Speed_msf3
    )
  ) %>%
  mutate(
    Dist = Speed_msf3 * 0.1,
    Dist_HS = SpeedHS * 0.1,
    Dist_SD = SpeedSD * 0.1
  ) %>%
  mutate("HMLD" = case_when(
    MetPowf <= 25.5 ~ 0,
    MetPowf > 25.5  ~ Speed_msf3 * 60
  )) %>%
  mutate(
    Speedmmin = Speed_msf3 * 60,
    HSR = SpeedHS * 60,
    Sprint = SpeedSD * 60
  ) %>%
  select(Time, Speedmmin, Accf, MetPowf, dist, filename, Player_displ, Accdens, HSR, Sprint, HMLD, Player_Id)


# ----------------------------
# Rolling average (ventana 60s a 10 Hz = 600)
# ----------------------------
df <- df_full %>%
  group_by(filename, Player_displ) %>%
  select(Time, Speedmmin, Accf, MetPowf, dist, filename, Player_displ, Player_Id, Accdens, HSR, Sprint, HMLD) %>%
  mutate(AccdensVen = frollmean(Accdens, 600, fill = NA)) %>%
  mutate(AccdensVen = ifelse(AccdensVen < 0, 0, AccdensVen)) %>%
  mutate(MetPowVen = frollmean(MetPowf, 600, fill = NA)) %>%
  mutate(MetPowVen = ifelse(MetPowVen < 0, 0, MetPowVen)) %>%
  mutate(Dist_HSVen = frollmean(HSR, 600, fill = NA)) %>%
  mutate(Dist_HSVen = ifelse(Dist_HSVen < 0, 0, Dist_HSVen)) %>%
  mutate(Dist_SDVen = frollmean(Sprint, 600, fill = NA)) %>%
  mutate(Dist_SDVen = ifelse(Dist_SDVen < 0, 0, Dist_SDVen)) %>%
  mutate(HMLDVen = frollmean(HMLD, 600, fill = NA)) %>%
  mutate(HMLDVen = ifelse(HMLDVen < 0, 0, HMLDVen)) %>%
  mutate(DistVen = frollmean(Speedmmin, 600, fill = NA)) %>%
  mutate(DistVen = ifelse(DistVen < 0, 0, DistVen)) %>%
  ungroup() %>%
  mutate(sec = seq(0, nrow(df_full) / 10, 0.1)) %>% #o poner 0.1 en inicio
  select(Time, Speedmmin, Accf, MetPowf, dist, Player_Id, Accdens, AccdensVen, MetPowVen, Dist_HSVen, Dist_SDVen, HMLDVen, DistVen, sec, filename, Player_displ)


# ----------------------------
# Cargar MIPs referencia y unir
# ----------------------------
referencia <- read_excel(mip_reference_file)
colnames(referencia) <- c("Player_displ", "MIP_met", "MIP_HMLD", "MIP_HSR", "MIP_sprint", "MIP_dist", "MIP_accdens")

df <- df %>% left_join(referencia, by = "Player_displ")


# ----------------------------
# Umbral (85%)
# ----------------------------
umb <- 85 * 0.01


# ============================================================
# EVENTOS POR VARIABLE
# ============================================================

# ACCELERATION DENSITY -------------------------------------------------
dfdens <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = AccdensVen > (umb * MIP_accdens), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit() %>%
  ungroup()

dfdens2 <- dfdens %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>% #o dur_ven o dur/2 por el rowmean
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPACD <- dfdens2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "AccDens"
  )

SM_ACD <- dfdens2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "AccDens"
  )

aACD <- right_join(SubMIPACD, SM_ACD, by = c("filename", "Player_displ"))
aACD <- aACD[1:22]
names(aACD) <- names(SubMIPACD)

final_ACD <- aACD %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "AccDens")

final_ACD[is.na(final_ACD)] <- 0


# MetPow -------------------------------------------------
dfmet <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = MetPowVen > (umb * MIP_met), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit()

dfmet2 <- dfmet %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>%
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPmet <- dfmet2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "MetPow"
  )

SM_met <- dfmet2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "MetPow"
  )

amet <- right_join(SubMIPmet, SM_met, by = c("filename", "Player_displ"))
amet <- amet[1:22]
names(amet) <- names(SubMIPmet)

final_met <- amet %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "MetPow")

final_met[is.na(final_met)] <- 0


# Dist -------------------------------------------------
dfdist <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = DistVen > (umb * MIP_dist), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit()

dfdist2 <- dfdist %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>% #o dur_ven o dur/2 por el rowmean
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPdist <- dfdist2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "Dist"
  )

SM_dist <- dfdist2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "Dist"
  )

adist <- right_join(SubMIPdist, SM_dist, by = c("filename", "Player_displ"))
adist <- adist[1:22]
names(adist) <- names(SubMIPdist)

final_dist <- adist %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "Dist")

final_dist[is.na(final_dist)] <- 0


# HMLD -------------------------------------------------
dfHMLD <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = HMLDVen > (umb * MIP_HMLD), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit()

dfHMLD2 <- dfHMLD %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>% #o dur_ven o dur/2 por el rowmean
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPHMLD <- dfHMLD2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "HMLD"
  )

SM_HMLD <- dfHMLD2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "HMLD"
  )

aHMLD <- right_join(SubMIPHMLD, SM_HMLD, by = c("filename", "Player_displ"))
aHMLD <- aHMLD[1:22]
names(aHMLD) <- names(SubMIPHMLD)

final_HMLD <- aHMLD %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "HMLD")

final_HMLD[is.na(final_HMLD)] <- 0


# HSR -------------------------------------------------
dfHSR <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = Dist_HSVen > (umb * MIP_HSR), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit()

dfHSR2 <- dfHSR %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>% #o dur_ven o dur/2 por el rowmean
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPHSR <- dfHSR2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "HSR"
  )

SM_HSR <- dfHSR2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "HSR"
  )

ahsr <- right_join(SubMIPHSR, SM_HSR, by = c("filename", "Player_displ"))
ahsr <- ahsr[1:22]
names(ahsr) <- names(SubMIPHSR)

final_hsr <- ahsr %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "HSR")

final_hsr[is.na(final_hsr)] <- 0


# SP -------------------------------------------------
dfSP <- df %>%
  group_by(filename, Player_displ) %>%
  mutate(x = Dist_SDVen > (umb * MIP_sprint), id = rleid(x)) %>%
  mutate(tiempocorresp = lag(Time, 599)) %>%
  mutate(tiempocorrespfin = Time) %>%
  na.omit()

dfSP2 <- dfSP %>%
  group_by(filename, Player_displ) %>%
  mutate(group_id = cumsum(x != lag(x, default = 0))) %>%
  ungroup() %>%
  group_by(filename, Player_displ, group_id) %>%
  mutate(x = x | n() < 600) %>% #o dur_ven o dur/2 por el rowmean
  ungroup() %>%
  mutate(id2 = rleid(x)) %>%
  ungroup()

SubMIPSP <- dfSP2 %>%
  dplyr::filter(x) %>%
  group_by(filename, Player_displ, id2) %>%
  summarise(
    x = first(x), event_duration = (last(sec) - first(sec)) + 60, intAcc = mean(AccdensVen), maxAcc = max(AccdensVen),
    Timeini = first(tiempocorresp), Timefin = last(tiempocorrespfin),
    intMet = mean(MetPowVen), maxMet = max(MetPowVen),
    intHSR = mean(Dist_HSVen), maxHSR = max(Dist_HSVen),
    intSP = mean(Dist_SDVen), maxSP = max(Dist_SDVen),
    avgHMLD = mean(HMLDVen), maxHMLD = max(HMLDVen),
    avgDist = mean(DistVen), maxDist = max(DistVen), Player = first(Player_displ), Partido = first(filename), Variable = "SP"
  )

SM_SP <- dfSP2 %>%
  dplyr::filter(!x) %>%
  group_by(filename, Player_displ) %>%
  summarise(
    id2 = first(id2), x = first(x), event_duration = 0, intAcc = 0, maxAcc = 0,
    Timeini = as.character(0), Timefin = as.character(0),
    intMet = 0, maxMet = 0,
    intHSR = 0, maxHSR = 0,
    intSP = 0, maxSP = 0,
    avgHMLD = 0, maxHMLD = 0,
    avgDist = 0, maxDist = 0, Player = first(Player_displ), Partido = first(filename), Variable = "SP"
  )

asp <- right_join(SubMIPSP, SM_SP, by = c("filename", "Player_displ"))
asp <- asp[1:22]
names(asp) <- names(SubMIPSP)

final_sp <- asp %>%
  group_by(filename, Player_displ) %>%
  summarise(num = sum(x), dur = sum(event_duration)) %>%
  mutate(variable = "SP")

final_sp[is.na(final_sp)] <- 0


# ============================================================
# UNIÓN FINAL + METADATOS DESDE filename
# ============================================================
data_completa <- final_dist %>%
  left_join(final_ACD, by = c("filename", "Player_displ")) %>%
  left_join(final_HMLD, by = c("filename", "Player_displ")) %>%
  left_join(final_hsr, by = c("filename", "Player_displ")) %>%
  left_join(final_met, by = c("filename", "Player_displ")) %>%
  left_join(final_sp, by = c("filename", "Player_displ"))

data_completa <- data_completa %>%
  mutate(
    tipo_dia = str_extract(filename, "^[a-z]+-\\d+"),
    fecha = str_extract(filename, "\\d{1,2}-\\d{1,2}-\\d{4}"),
    jugadores_info = str_extract(filename, "\\d+[vV]\\d+\\+?\\d*"),
    num_jugadores = ifelse(!is.na(jugadores_info),
                           sapply(strsplit(jugadores_info, "[vV+]+"), function(x) sum(as.numeric(x))),
                           NA),
    dimensiones = str_extract(filename, "\\d+[xX]\\d+"),
    espacio_total = ifelse(!is.na(dimensiones),
                           as.numeric(str_extract(dimensiones, "^[0-9]+")) * as.numeric(str_extract(dimensiones, "(?<=[xX])[0-9]+")),
                           NA),
    espacio_relativo = ifelse(!is.na(espacio_total) & !is.na(num_jugadores), espacio_total / num_jugadores, NA),
    tipo_objetivo = ifelse(str_detect(filename, "\\([A-Za-z0-9]+\\)"),
                           str_extract(filename, "(?<=\\()\\w+(?=\\))"), "POS"),
    num_serie = ifelse(str_detect(filename, "_\\d+\\.csv$"),
                       as.numeric(unlist(str_extract_all(filename, "(?<=_)\\d+(?=\\.csv$)"))),
                       1)
  ) %>%
  mutate(
    num_jugadores = ifelse(tipo_objetivo == "PG", num_jugadores + 2, num_jugadores),
    espacio_relativo = ifelse(!is.na(espacio_total) & !is.na(num_jugadores), espacio_total / num_jugadores, NA)
  )

names(data_completa) <- c(
  "filename", "Player_name", "events_dist", "dur_dist", "dist",
  "events_accdens", "dur_accdens", "accdens", "events_HMLD", "dur_HMLD", "HMLD",
  "events_HSR", "dur_HSR", "HSR", "events_MetPow", "dur_MetPow", "MetPow",
  "events_SP", "dur_SP", "SP", "tipo_dia", "fecha", "jugadores_info", "num_jugadores",
  "dimensiones", "espacio_total", "espacio_relativo", "tipo_objetivo", "num_serie"
)

write.csv(data_completa, output_file)
