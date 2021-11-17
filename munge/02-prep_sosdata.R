

# In-patient data ---------------------------------------------------------

# Transpose data ----------------------------------------------------------

svt <- sv %>%
  mutate(INDATUM = ymd(indat), 
         UTDATUM = ymd(utdat)) %>%
  select(-indat, -utdat) %>%
  group_by(LopNr, INDATUM, UTDATUM, diag_no) %>%
  mutate(no = 1:n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(LopNr, INDATUM, UTDATUM, no), names_from = diag_no, values_from = diagnosis) %>%
  mutate(HDIA = paste0(" ", diag1)) %>%
  mutate(across(starts_with("diag"), ~ tidyr::replace_na(.x, ""))) %>%
  tidyr::unite(DIA_all, starts_with("diag", ignore.case = FALSE), sep = " ", remove = TRUE) %>%
  mutate(DIA_all = paste0(" ", stringr::str_squish(DIA_all))) %>%
  select(-no)


# Link hospital visits ----------------------------------------------------
sv2 <- svt %>%
  filter(!is.na(INDATUM)) %>% # 0 obs have missing INDATUM
  mutate(UTDATUM = case_when(
    is.na(UTDATUM) ~ INDATUM, # 598 obs have missing UTDATUM (did not parse)
    INDATUM > UTDATUM ~ INDATUM,
    TRUE ~ UTDATUM
  )) %>% # 0 obs have indatum AFTER utdatum, set utdatum to indatum
  group_by(LopNr) %>%
  arrange(INDATUM, UTDATUM) %>%
  mutate(
    n = row_number(),
    link = case_when(
      INDATUM <= dplyr::lag(UTDATUM) + 1 ~ 1,
      UTDATUM + 1 >= lead(INDATUM) ~ 1
    )
  ) %>%
  ungroup() %>%
  arrange(LopNr, INDATUM, UTDATUM)

svlink <- sv2 %>%
  filter(!is.na(link)) %>%
  group_by(LopNr) %>%
  arrange(INDATUM, UTDATUM) %>%
  mutate(link2 = case_when(
    INDATUM > dplyr::lag(UTDATUM) + 1 ~ row_number(),
    row_number() == 1 ~ row_number()
  )) %>%
  ungroup() %>%
  arrange(LopNr, INDATUM, UTDATUM) %>%
  mutate(link2 = zoo::na.locf(link2))

svlink <- svlink %>%
  group_by(LopNr, link2) %>%
  summarize(
    HDIA = paste0(" ", stringr::str_squish(str_c(HDIA, collapse = " "))),
    DIA_all = paste0(" ", stringr::str_squish(str_c(DIA_all, collapse = " "))),
    INDATUM = min(INDATUM),
    UTDATUM = max(UTDATUM),
    .groups = "drop"
  )

svlink <- bind_rows(
  sv2 %>% filter(is.na(link)) %>% mutate(sos_hosplinked = 0),
  svlink %>% mutate(sos_hosplinked = 1)
) %>%
  select(-link, -link2, -n)


# Out-patient, surgery and primary care data ------------------------------

svopt <- svop %>%
  mutate(INDATUM = ymd(datum_c)) %>%
  select(-datum_c, datum) %>%
  group_by(LopNr, INDATUM) %>%
  mutate(no = paste0("op", 1:n())) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(LopNr, INDATUM), names_from = no, values_from = opk) %>%
  mutate(across(starts_with("op"), ~ tidyr::replace_na(.x, ""))) %>%
  tidyr::unite(OP_all, starts_with("op", ignore.case = FALSE), sep = " ", remove = TRUE) %>%
  mutate(OP_all = paste0(" ", stringr::str_squish(OP_all)))

ovopt <- ovop %>%
  mutate(INDATUM = ymd(datum_c)) %>%
  select(-datum_c, datum) %>%
  group_by(LopNr, INDATUM) %>%
  mutate(no = paste0("op", 1:n())) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(LopNr, INDATUM), names_from = no, values_from = opk) %>%
  mutate(across(starts_with("op"), ~ tidyr::replace_na(.x, ""))) %>%
  tidyr::unite(OP_all, starts_with("op", ignore.case = FALSE), sep = " ", remove = TRUE) %>%
  mutate(OP_all = paste0(" ", stringr::str_squish(OP_all)))

ovt <- ov %>%
  mutate(INDATUM = ymd(bdat_c)) %>%
  select(-bdat_c, bdat) %>%
  group_by(LopNr, INDATUM, diag_no) %>%
  mutate(no = 1:n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(LopNr, INDATUM, no), names_from = diag_no, values_from = diagnosis) %>%
  mutate(across(starts_with("diag"), ~ tidyr::replace_na(.x, ""))) %>%
  tidyr::unite(DIA_all, starts_with("diag", ignore.case = FALSE), sep = " ", remove = TRUE) %>%
  mutate(DIA_all = paste0(" ", stringr::str_squish(DIA_all))) %>%
  select(-no)

pct <- pc %>%
  mutate(INDATUM = ymd(bdat_c)) %>%
  select(-bdat_c, bdat) %>%
  group_by(LopNr, INDATUM, diag_no) %>%
  mutate(no = 1:n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(LopNr, INDATUM, no), names_from = diag_no, values_from = diagnosis) %>%
  mutate(across(starts_with("diag"), ~ tidyr::replace_na(.x, ""))) %>%
  tidyr::unite(DIA_all, starts_with("diag", ignore.case = FALSE), sep = " ", remove = TRUE) %>%
  mutate(DIA_all = paste0(" ", stringr::str_squish(DIA_all))) %>%
  select(-no)

 
# Merge sos data ----------------------------------------------------------

patreg <- bind_rows(
  svlink %>% mutate(sos_source = "sv"),
  svopt %>% mutate(sos_source = "sv"),
  ovopt %>% mutate(sos_source = "ov"),
  ovt %>% mutate(sos_source = "ov"),
  pct %>% mutate(sos_source = "pc")
)

rm(list = ls()[grepl("sv|ov|pc", ls())]) # delete to save workspace
