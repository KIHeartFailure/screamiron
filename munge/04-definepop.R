

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- shfdata001 %>%
  rename(LopNr = lopnr)
flow <- c("Number of posts in SwedeHF - SCREAM", nrow(rsdata))

# already removed duplicated indexdates

rsdata <- left_join(rsdata,
  pininfo,
  by = "LopNr"
)
rsdata <- rsdata %>%
  filter(scb_reusedpin == 0 & scb_changedpin == 0) %>% # reused/changed personr
  select(-scb_reusedpin, -scb_changedpin)
flow <- rbind(flow, c("Remove posts with reused or changed PINs", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_age >= 18 & !is.na(shf_age))
flow <- rbind(flow, c("Remove posts < 18 years", nrow(rsdata)))

rsdata <- rsdata %>%
  filter((shf_indexdtm < shf_deathdtm | is.na(shf_deathdtm))) # enddate prior to indexdate
flow <- rbind(flow, c("Remove posts with end of follow-up <= index date (died in hospital)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2006-01-01"))
flow <- rbind(flow, c("Remove posts with with index date < 2006-01-01", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm <= ymd("2018-12-31"))
flow <- rbind(flow, c("Remove posts with with index date > 2018-12-31", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("Non-missing EF (necessary????)", nrow(rsdata)))

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

flow <- rbind(flow, c("Not emigrated from Stockholm prior to indexdate in SwedeHF", nrow(rsdata)))

# check lab inclusion criteria
tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm), lab_avg, by = "LopNr") %>%
  mutate(diff = as.numeric(shf_indexdtm - labdtm)) %>%
  filter(diff >= 0, diff < 90) #%>%
  #filter(!is.na(hb) & !is.na(transf_sat) & !is.na(ferritin))

 tmp_rsdatalab2 <- tmp_rsdatalab %>%
  group_by(LopNr, shf_indexdtm) %>%
  summarise(hb_any = mean(hb, na.rm = TRUE),
            transf_any = mean(transf_sat, na.rm = TRUE),
            ferritin_any = mean(ferritin, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(hb_any) & !is.na(transf_any) & !is.na(ferritin_any))

rsdata <- left_join(tmp_rsdatalab2 %>% select(LopNr, shf_indexdtm),
  rsdata,
  by = c("LopNr", "shf_indexdtm")
)

flow <- rbind(flow, c("Hb, ferritin, transferrin within 3 months PRIOR to indexdate in SwedeHF (not necessesarily on same date)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

colnames(flow) <- c("Criteria", "N")
