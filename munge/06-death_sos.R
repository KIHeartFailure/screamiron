
# koll <- migration %>%
#  filter(Posttyp == "Utv") %>%
#  group_by(lopnr) %>%
#  slice(2) %>%
#  ungroup() %>%
#  count()


tmp_rsdataflytt <- inner_join(rsdata %>%
  select(LopNr, shf_indexdtm),
flytt %>%
  mutate(flyttdtm = ymd(hdat)) %>%
  select(-hdat, -hdat_c) %>%
  filter(hkod == "U"),
by = "LopNr"
) %>%
  filter(
    flyttdtm > shf_indexdtm,
    flyttdtm <= ymd("2019-12-31")
  ) %>%
  group_by(LopNr, shf_indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr, shf_indexdtm, flyttdtm)

rsdata <- left_join(rsdata,
  tmp_rsdataflytt,
  by = c("LopNr", "shf_indexdtm")
)


# koll finns ulorsak för alla döda
# koll <- dors %>%
#  group_by(LopNr) %>%
#  slice(1) %>%
#  ungroup()
# koll2 <- dors %>%
#  filter(diag_no == "ULORSAK") %>%
#  group_by(LopNr) %>%
#  slice(1) %>%
#  ungroup()

rsdata <- rsdata %>%
  mutate(
    censdtm = pmin(sos_deathdtm, flyttdtm, na.rm = TRUE),
    censdtm = pmin(censdtm, ymd("2019-12-31"), na.rm = TRUE),
    censdtm = pmin(censdtm, shf_indexdtm + 365 * 3, na.rm = TRUE),
    sos_out_death = factor(if_else(censdtm == sos_deathdtm & !is.na(sos_deathdtm), 1, 0), 
                           levels = 0:1, 
                           labels = c("No", "Yes")),
    sos_outtime_death = as.numeric(censdtm - shf_indexdtm)
  ) %>%
  select(-shf_deathdtm)

rsdata <- create_deathvar(
  cohortdata = rsdata,
  indexdate = shf_indexdtm,
  censdate = censdtm,
  deathdate = sos_deathdtm,
  name = "cv",
  orsakvar = sos_deathcause,
  orsakkod = "I|J81|K761|R57|G45",
  valsclass = "fac",
  warnings = FALSE
)