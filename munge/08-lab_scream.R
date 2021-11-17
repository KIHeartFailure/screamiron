
# check lab inclusion criteria
tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm), lab_avg, by = "LopNr") %>%
  mutate(diff = as.numeric(shf_indexdtm - labdtm)) %>%
  filter(diff >= 0, diff < 90)

labfunc <- function(lab, labname, unittransform = NULL) {
  labname <- paste0("scream_", labname)
  tmp_rsdatalab2 <- tmp_rsdatalab %>%
    filter(!is.na(!!sym(lab))) %>%
    group_by(LopNr) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    rename(!!labname := !!sym(lab)) %>%
    select(LopNr, shf_indexdtm, !!sym(labname))

  if (!is.null(unittransform)) {
    tmp_rsdatalab2 <- tmp_rsdatalab2 %>%
      mutate(!!labname := !!sym(labname) * unittransform)
  }

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab2,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfunc("bnp", "ntprobnp")
labfunc("crea", "creatinine")
labfunc("sodium", "sodium")
labfunc("potas", "potassium")
labfunc("ferritin", "ferritin")
labfunc("transf_sat", "transferrin", 100)
labfunc("hb", "hb")


# Follow-up transferrin/ferritin -------------------------------------------

tmp_rsdatalab <- left_join(rsdata %>% select(LopNr, shf_indexdtm), lab_avg, by = "LopNr") %>%
  mutate(diff = as.numeric(labdtm - shf_indexdtm)) %>%
  filter(diff >= 0)

labfuncfollow <- function(lab, labname, time, timename, unittransform = NULL) {
  labname <- paste0("scream_", labname, timename)
  tmp_rsdatalab2 <- tmp_rsdatalab %>%
    filter(!is.na(!!sym(lab)) &
      diff >= time - 6 * 30.5 &
      diff <= time + 6 * 30.5) %>%
    group_by(LopNr) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    rename(!!labname := !!sym(lab)) %>%
    select(LopNr, shf_indexdtm, !!sym(labname))


  if (!is.null(unittransform)) {
    tmp_rsdatalab2 <- tmp_rsdatalab2 %>%
      mutate(!!labname := !!sym(labname) * unittransform)
  }

  rsdata <<- left_join(rsdata,
    tmp_rsdatalab2,
    by = c("LopNr", "shf_indexdtm")
  )
}

labfuncfollow("ferritin", "ferritin", 365, "1yr")
labfuncfollow("transf_sat", "transferrin", 365, "1yr", 100)
labfuncfollow("hb", "hb", 365, "1yr")

labfuncfollow("ferritin", "ferritin", 365 * 3, "3yr")
labfuncfollow("transf_sat", "transferrin", 365 * 3, "3yr", 100)
labfuncfollow("hb", "hb", 365 * 3, "3yr")
