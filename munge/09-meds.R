
# oral supplements ----------------------------------------------

meds2 <- meds %>%
  mutate(ddr_ironoralsupp = stringr::str_detect(ATC, "B03AA|B03AB|B03AD|B03AE")) %>%
  filter(ddr_ironoralsupp) %>%
  mutate(edtm = ymd(EDATUM))

meds2 <- inner_join(
  rsdata %>% select(LopNr, shf_indexdtm),
  meds2 %>% select(LopNr, edtm, ddr_ironoralsupp),
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(edtm - shf_indexdtm))

## index
meds2 <- meds2 %>%
  filter(diff <= 14 & diff >= -30 * 5) %>%
  group_by(LopNr) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr, shf_indexdtm, ddr_ironoralsupp)

rsdata <- left_join(
  rsdata,
  meds2,
  by = c("LopNr", "shf_indexdtm")
) %>%
  mutate(ddr_ironoralsupp = factor(if_else(is.na(ddr_ironoralsupp), 0, 1), levels = 0:1, labels = c("No", "Yes")))
