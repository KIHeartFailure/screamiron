
# Outcomes hospitalizations ----------------------------------------------

svpatreg <- patreg %>%
  filter(sos_source == "sv")

svpatregrsdata <- left_join(rsdatalab %>%
  select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, sos_out_deathcv, censdtm),
svpatreg,
by = "LopNr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM <= censdtm)

tmpsos <- svpatregrsdata %>%
  mutate(sos_out_hosphfrec = stringr::str_detect(HDIA, " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57")) %>%
  filter(sos_out_hosphfrec) %>%
  select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, sos_out_deathcv, censdtm, sos_outtime, sos_out_hosphfrec)

rsdatalab_rec <- bind_rows(
  rsdatalab %>%
    select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, sos_out_deathcv, censdtm),
  tmpsos
) %>%
  mutate(
    sos_out_hosphfrec = if_else(is.na(sos_out_hosphfrec), 0, 1),
    sos_outtime = if_else(is.na(sos_outtime), difftime(censdtm, shf_indexdtm, units = "days"), sos_outtime)
  )

rsdatalab_rec <- rsdatalab_rec %>%
  group_by(LopNr, shf_indexdtm, sos_outtime) %>%
  arrange(desc(sos_out_hosphfrec)) %>%
  slice(1) %>%
  ungroup()

# Outcomes hospitalizations + CVD --------------------------------------------

rsdatalab_rec <- rsdatalab_rec %>%
  mutate(sos_out_deathcvhosphfrec = if_else(sos_out_deathcv == "Yes", 1, sos_out_hosphfrec))

# fix for events at day of censoring

tmpadd <- rsdatalab_rec %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(sos_outtime) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(sos_out_deathcvhosphfrec == 1) %>%
  mutate(
    sos_outtime = sos_outtime + 1,
    sos_out_deathcvhosphfrec = 0, 
    sos_out_hosphfrec = 0, 
  )

rsdatalab_rec <- bind_rows(
  rsdatalab_rec,
  tmpadd
) %>%
  arrange(LopNr, shf_indexdtm, sos_outtime, desc(sos_out_deathcvhosphfrec))
