
# Outcomes hospitalizations ----------------------------------------------

svpatreg <- patreg %>%
  filter(sos_source == "sv")

svpatregrsdata <- left_join(rsdatalab %>%
  select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, censdtm),
svpatreg,
by = "LopNr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM < censdtm)

tmpsos <- svpatregrsdata %>%
  mutate(sos_out = stringr::str_detect(HDIA, " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57")) %>%
  filter(sos_out) %>%
  select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, censdtm, sos_outtime, sos_out)

rsdatalab_rec_hosphf <- bind_rows(
  rsdatalab %>%
    select(LopNr, shf_indexdtm, starts_with("scream_"), sos_outtime_death, sos_out_death, censdtm),
  tmpsos
) %>%
  mutate(
    sos_out = if_else(is.na(sos_out), 0, 1),
    sos_outtime = if_else(is.na(sos_outtime), difftime(censdtm, shf_indexdtm, units = "days"), sos_outtime)
  )

rsdatalab_rec_hosphf <- rsdatalab_rec_hosphf %>%
  group_by(LopNr, shf_indexdtm, sos_outtime) %>%
  arrange(desc(sos_out)) %>%
  slice(1) %>%
  ungroup()
