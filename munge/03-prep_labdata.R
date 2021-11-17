lab_avg <- lab %>%
  filter(test %in% c(
    "bnp",
    "potas",
    "sodium",
    "transf_sat",
    "regular Hb",
    "crea",
    "ferritin"
  )) %>%
  group_by(LopNr, test, datum) %>%
  summarise(result = mean(result, na.rm = TRUE), .groups = "drop") %>%
  mutate(labdtm = ymd(datum)) %>%
  select(-datum)

lab_avg <- lab_avg %>% 
  pivot_wider(id_cols = c(LopNr, labdtm), names_from = test, values_from = result) %>%
  rename(hb = `regular Hb`)
