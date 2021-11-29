
# check lab inclusion criteria
tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm), lab_avg, by = "LopNr") %>%
  mutate(diff = as.numeric(shf_indexdtm - labdtm)) %>%
  filter(diff >= 0, diff < 90)

## for primary inclusion transferrin, ferritin and hb need to be on SAME date
tmp_rsdatalab2 <- tmp_rsdatalab %>%
  filter(
    !is.na(ferritin) &
      !is.na(transf_sat) &
      !is.na(hb)
  ) %>%
  group_by(LopNr) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(transf_sat = transf_sat * 100) %>% # transform transferrin
  rename(
    scream_ferritin = ferritin,
    scream_transferrin = transf_sat,
    scream_hb = hb
  ) %>%
  select(LopNr, shf_indexdtm, starts_with("scream_"))

rsdata <- left_join(rsdata,
  tmp_rsdatalab2,
  by = c("LopNr", "shf_indexdtm")
)


## other labs can be on different dates within 3 mo from index

labfunc <- function(lab, labname) {
  labname <- paste0("scream_", labname)
  tmp_rsdatalab2 <- tmp_rsdatalab %>%
    filter(!is.na(!!sym(lab))) %>%
    group_by(LopNr) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    rename(!!labname := !!sym(lab)) %>%
    select(LopNr, shf_indexdtm, !!sym(labname))

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab2,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfunc("bnp", "ntprobnp")
labfunc("crea", "creatinine")
labfunc("sodium", "sodium")
labfunc("potas", "potassium")

# Follow-up transferrin/ferritin -------------------------------------------

tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm),
  lab_avg,
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(labdtm - shf_indexdtm)) %>%
  filter(diff >= 0 &
    !is.na(ferritin) &
    !is.na(transf_sat) &
    !is.na(hb))

labfuncfollow <- function(time, timename) {
  ferrname <- paste0("scream_ferritin", timename)
  transname <- paste0("scream_transferrin", timename)
  hbname <- paste0("scream_hb", timename)

  tmp_rsdatalab2 <- tmp_rsdatalab %>%
    filter(
      diff >= time - 91 & # +/- 3 mo from resp time point
        diff <= time + 91
    ) %>%
    group_by(LopNr) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(transf_sat = transf_sat * 100) %>% # transform transferrin
    rename(
      !!ferrname := ferritin,
      !!transname := transf_sat,
      !!hbname := hb
    ) %>%
    select(
      LopNr, shf_indexdtm,
      !!sym(ferrname),
      !!sym(transname),
      !!sym(hbname)
    )

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab2,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfuncfollow(182, "6mo")
labfuncfollow(365, "1yr")
labfuncfollow(365 * 3, "3yr")
