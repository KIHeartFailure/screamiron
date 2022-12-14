

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- shfdata001 %>%
  rename(LopNr = lopnr)
flow <- c("Number of posts in SwedeHF in SCREAM2", nrow(rsdata))

# already removed duplicated indexdates

rsdata <- left_join(rsdata,
  pininfo,
  by = "LopNr"
)
rsdata <- rsdata %>%
  filter(scb_reusedpin == 0 & scb_changedpin == 0) %>% # reused/changed personr
  select(-scb_reusedpin, -scb_changedpin)
flow <- rbind(flow, c("Exclude posts with reused or changed PINs", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_age >= 18 & !is.na(shf_age))
flow <- rbind(flow, c("Exclude posts < 18 years", nrow(rsdata)))

rsdata <- left_join(rsdata,
  dors %>%
    filter(diag_no == "ULORSAK") %>%
    rename(sos_deathcause = diagnos) %>%
    select(LopNr, dodsdat, sos_deathcause),
  by = "LopNr"
) %>%
  mutate(sos_deathdtm = ymd(dodsdat)) %>%
  select(-dodsdat)

rsdata <- rsdata %>%
  filter((shf_indexdtm < sos_deathdtm | is.na(sos_deathdtm))) # enddate prior to indexdate
flow <- rbind(flow, c("Exclude posts with end of follow-up <= index date (died in hospital)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2006-01-01"))
flow <- rbind(flow, c("Exclude posts with index date < 2006-01-01", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm <= ymd("2018-12-31"))
flow <- rbind(flow, c("Exclude posts with index date > 2018-12-31", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("Exclude posts with missing EF", nrow(rsdata)))

tmp_rsdataflytt <- inner_join(rsdata %>% select(LopNr, shf_indexdtm),
  flytt %>% mutate(flyttdtm = ymd(hdat)) %>% select(-hdat, -hdat_c),
  by = "LopNr"
)
tmp_rsdataflytt <- tmp_rsdataflytt %>%
  filter(shf_indexdtm > flyttdtm) %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(flyttdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(hkod == "U")

rsdata <- left_join(rsdata,
  tmp_rsdataflytt,
  by = c("LopNr", "shf_indexdtm")
) %>%
  filter(is.na(hkod)) %>%
  select(-flyttdtm, -hkod)

flow <- rbind(flow, c("Exclude posts emigrated from Stockholm prior to indexdate in SwedeHF", nrow(rsdata)))

# check lab inclusion criteria
tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm),
  lab_avg,
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(shf_indexdtm - labdtm)) %>%
  filter(diff >= 0 & diff < 90 &
    !is.na(ferritin) &
    !is.na(transf_sat) &
    !is.na(hb)) %>%
  group_by(LopNr) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(hbtfindex = 1)

rsdata <- left_join(
  rsdata,
  tmp_rsdatalab %>% select(LopNr, shf_indexdtm, hbtfindex),
  by = c("LopNr", "shf_indexdtm")
)

rsdata <- rsdata %>%
  group_by(LopNr) %>%
  arrange(desc(hbtfindex), shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post with hb, ferritin, transferrin at index / patient* - Full population", nrow(rsdata)))

flow <- rbind(flow, c("Hb, ferritin, transferrin within 3 months PRIOR to indexdate in SwedeHF (on same date) - Iron population", nrow(rsdata %>% filter(hbtfindex == 1))))

colnames(flow) <- c("Criteria", "N")
