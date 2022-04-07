

# Fix variables ------------------------------------

rsdata <- rsdata %>%
  mutate(
    scream_anemia = case_when(
      is.na(scream_hb) ~ NA_character_,
      shf_sex == "Female" & scream_hb < 120 | shf_sex == "Male" & scream_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),
    scream_hb_cat = factor(case_when(
      scream_hb < 110 ~ 1,
      scream_hb < 120 ~ 2,
      scream_hb < 130 ~ 3,
      scream_hb < 140 ~ 4,
      scream_hb < 150 ~ 5,
      scream_hb >= 150 ~ 6
    ),
    levels = 1:6,
    labels = c(
      "<110", "110-119",
      "120-129", "130-139", "140-149", ">=150"
    )
    ),

    # iron def
    scream_id = case_when(
      is.na(scream_ferritin) | is.na(scream_transferrin) ~ NA_character_,
      scream_ferritin < 100 ~ "Yes",
      scream_ferritin <= 299 & scream_transferrin < 20 ~ "Yes",
      TRUE ~ "No"
    ),
    scream_aid = factor(case_when(
      scream_id == "Yes" & scream_anemia == "Yes" ~ 4,
      scream_id == "Yes" & scream_anemia == "No" ~ 3,
      scream_id == "No" & scream_anemia == "Yes" ~ 2,
      scream_id == "No" & scream_anemia == "No" ~ 1
    ),
    levels = 1:4,
    labels = c("A-/ID-", "A+/ID-", "A-/ID+", "A+/ID+")
    ),
    scream_ferritin_cat = factor(case_when(
      is.na(scream_ferritin) ~ NA_real_,
      scream_ferritin < 100 ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c(">=100", "<100")),
    scream_ferritin_cat300 = factor(case_when(
      is.na(scream_ferritin) ~ NA_real_,
      scream_ferritin < 300 ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c(">=300", "<300")),
    scream_transferrin_cat = factor(case_when(
      is.na(scream_transferrin) ~ NA_real_,
      scream_transferrin < 20 ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c(">=20", "<20")),
    iron_need = factor(case_when(
      is.na(shf_weight) | is.na(scream_hb) ~ NA_real_,
      shf_weight >= 35 & shf_weight <= 70 & scream_hb < 100 ~ 1500,
      shf_weight >= 35 & shf_weight <= 70 & scream_hb >= 100 & scream_hb < 140 ~ 1000,
      shf_weight >= 35 & shf_weight <= 70 & scream_hb >= 140 & scream_hb <= 150 ~ 500,
      shf_weight > 70 & scream_hb < 100 ~ 2000,
      shf_weight > 70 & scream_hb >= 100 & scream_hb < 140 ~ 1500,
      shf_weight > 70 & scream_hb >= 140 & scream_hb <= 150 ~ 500
    )),
    shf_ferrocarboxymaltosisimp = case_when(
      shf_indexyear <= 2016 & is.na(shf_ferrocarboxymaltosis) ~ "No",
      TRUE ~ as.character(shf_ferrocarboxymaltosis)
    ),
    shf_ferrocarboxymaltosisdose = factor(shf_ferrocarboxymaltosisdose),
    shf_ferrocarboxymaltosis_priorinc2016 = if_else(shf_indexyear <= 2016, shf_ferrocarboxymaltosis, factor(NA_character_)),
    shf_ferrocarboxymaltosis_postinc2017 = if_else(shf_indexyear >= 2017, shf_ferrocarboxymaltosis, factor(NA_character_)),
    ddr_ironoralsupp_priorinc2016 = if_else(shf_indexyear <= 2016, ddr_ironoralsupp, factor(NA_character_)),
    ddr_ironoralsupp_postinc2017 = if_else(shf_indexyear >= 2017, ddr_ironoralsupp, factor(NA_character_)),
    fcm_ironoral = factor(case_when(
      is.na(shf_ferrocarboxymaltosis) | is.na(ddr_ironoralsupp) ~ NA_real_,
      shf_ferrocarboxymaltosis == "Yes" | ddr_ironoralsupp == "Yes" ~ 1,
      TRUE ~ 0
    ), levels = 0:1, labels = c("No", "Yes")),
    fcm_ironoral_postinc2017 = if_else(shf_indexyear >= 2017, fcm_ironoral, factor(NA_character_)),

    ## lab follow-up
    ### 6 mo
    scream_anemia6mo = case_when(
      is.na(scream_hb6mo) ~ NA_character_,
      shf_sex == "Female" & scream_hb6mo < 120 | shf_sex == "Male" & scream_hb6mo < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # iron def
    scream_id6mo = case_when(
      is.na(scream_ferritin6mo) | is.na(scream_transferrin6mo) ~ NA_character_,
      scream_ferritin6mo < 100 ~ "Yes",
      scream_ferritin6mo <= 299 & scream_transferrin6mo < 20 ~ "Yes",
      TRUE ~ "No"
    ),
    scream_aid6mo = factor(case_when(
      scream_id6mo == "Yes" & scream_anemia6mo == "Yes" ~ 4,
      scream_id6mo == "Yes" & scream_anemia6mo == "No" ~ 3,
      scream_id6mo == "No" & scream_anemia6mo == "Yes" ~ 2,
      scream_id6mo == "No" & scream_anemia6mo == "No" ~ 1
    ),
    levels = 1:4,
    labels = c("A-/ID-", "A+/ID-", "A-/ID+", "A+/ID+")
    ),

    ### 1 year
    scream_anemia1yr = case_when(
      is.na(scream_hb1yr) ~ NA_character_,
      shf_sex == "Female" & scream_hb1yr < 120 | shf_sex == "Male" & scream_hb1yr < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # iron def
    scream_id1yr = case_when(
      is.na(scream_ferritin1yr) | is.na(scream_transferrin1yr) ~ NA_character_,
      scream_ferritin1yr < 100 ~ "Yes",
      scream_ferritin1yr <= 299 & scream_transferrin1yr < 20 ~ "Yes",
      TRUE ~ "No"
    ),
    scream_aid1yr = factor(case_when(
      scream_id1yr == "Yes" & scream_anemia1yr == "Yes" ~ 4,
      scream_id1yr == "Yes" & scream_anemia1yr == "No" ~ 3,
      scream_id1yr == "No" & scream_anemia1yr == "Yes" ~ 2,
      scream_id1yr == "No" & scream_anemia1yr == "No" ~ 1
    ),
    levels = 1:4,
    labels = c("A-/ID-", "A+/ID-", "A-/ID+", "A+/ID+")
    ),

    ### 3 years
    scream_anemia3yr = case_when(
      is.na(scream_hb3yr) ~ NA_character_,
      shf_sex == "Female" & scream_hb3yr < 120 | shf_sex == "Male" & scream_hb3yr < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # iron def
    scream_id3yr = case_when(
      is.na(scream_ferritin3yr) | is.na(scream_transferrin3yr) ~ NA_character_,
      scream_ferritin3yr < 100 ~ "Yes",
      scream_ferritin3yr <= 299 & scream_transferrin3yr < 20 ~ "Yes",
      TRUE ~ "No"
    ),
    scream_aid3yr = factor(case_when(
      scream_id3yr == "Yes" & scream_anemia3yr == "Yes" ~ 4,
      scream_id3yr == "Yes" & scream_anemia3yr == "No" ~ 3,
      scream_id3yr == "No" & scream_anemia3yr == "Yes" ~ 2,
      scream_id3yr == "No" & scream_anemia3yr == "No" ~ 1
    ),
    levels = 1:4,
    labels = c("A-/ID-", "A+/ID-", "A-/ID+", "A+/ID+")
    ),
    shf_indexyear_cat = case_when(
      shf_indexyear <= 2010 ~ "2006-2010",
      shf_indexyear <= 2016 ~ "2011-2016",
      shf_indexyear <= 2018 ~ "2017-2018"
    ),
    shf_indexyear_cat2 = case_when(
      shf_indexyear <= 2016 ~ "2006-2016",
      shf_indexyear <= 2018 ~ "2017-2018"
    ),
    shf_nyha_cat = case_when(
      shf_nyha %in% c("I", "II") ~ "I - II",
      shf_nyha %in% c("III", "IV") ~ "III-IV"
    ),
    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),
    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 3,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 1
    ),
    labels = c("HFrEF", "HFmrEF", "HFpEF"),
    levels = 1:3
    ),
    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 0,
      shf_smoking %in% c("Current") ~ 1
    ),
    labels = c("No", "Yes"),
    levels = 0:1
    ),
    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),
    scream_potassium_cat = factor(
      case_when(
        is.na(scream_potassium) ~ NA_real_,
        scream_potassium < 3.5 ~ 2,
        scream_potassium <= 5 ~ 1,
        scream_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),
    scream_sodium_cat = factor(
      case_when(
        is.na(scream_sodium) ~ NA_real_,
        scream_sodium <= 135 ~ 1,
        scream_sodium > 135 ~ 2
      ),
      labels = c("<=135", ">135"),
      levels = 1:2
    ),
    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),
    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),
    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(shf_sex == "Female", 0.7, 0.9),
    tmp_a = if_else(shf_sex == "Female", -0.241, -0.302),
    tmp_add = if_else(shf_sex == "Female", 1.012, 1),
    scream_gfrckdepi = 142 * pmin(scream_creatinine / 88.4 / tmp_k, 1)^tmp_a * pmax(scream_creatinine / 88.4 / tmp_k, 1)^-1.200 * 0.9938^shf_age * tmp_add,
    scream_gfrckdepi_cat = factor(case_when(
      is.na(scream_gfrckdepi) ~ NA_real_,
      scream_gfrckdepi >= 60 ~ 1,
      scream_gfrckdepi < 60 ~ 2,
    ),
    labels = c(">=60", "<60"),
    levels = 1:2
    ),
    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_valvular = case_when(
      shf_valvedisease == "Yes" |
        sos_com_valvular == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_followuplocation_cat = if_else(shf_followuplocation %in% c("Primary care", "Other"), "Primary care/Other",
      as.character(shf_followuplocation)
    ),
    scb_education_cat = case_when(
      scb_education %in% c("Compulsory school", "Secondary school") ~ "No university",
      scb_education %in% c("University") ~ "University"
    ),

    ## Outomes
    ### combined outcomes
    sos_out_deathcvhosphf = if_else(sos_out_deathcv == "Yes" | sos_out_hosphf == "Yes", "Yes", "No"),
    sos_out_deathcvnohosphf = if_else(sos_out_deathcv == "Yes", sos_out_nohosphf + 1, sos_out_nohosphf),

    #### comp risk
    sos_out_deathcvhosphf_cr = create_crevent(sos_out_deathcvhosphf, sos_out_death),
    sos_out_deathcv_cr = create_crevent(sos_out_deathcv, sos_out_death),
    sos_out_hosphf_cr = create_crevent(sos_out_hosphf, sos_out_death)
  ) %>%
  select(-starts_with("tmp_"))


# ntprobnp

ntprobnp <- rsdata %>%
  group_by(shf_ef_cat) %>%
  summarise(
    ntmed = quantile(scream_ntprobnp,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

rsdata <- left_join(
  rsdata,
  ntprobnp,
  by = c("shf_ef_cat")
) %>%
  mutate(
    scream_ntprobnp_cat = case_when(
      scream_ntprobnp < ntmed ~ 1,
      scream_ntprobnp >= ntmed ~ 2
    ),
    scream_ntprobnp_cat = factor(scream_ntprobnp_cat,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-ntmed)

# income

inc <- rsdata %>%
  group_by(shf_indexyear) %>%
  summarise(
    incmed = quantile(scb_dispincome,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

rsdata <- left_join(
  rsdata,
  inc,
  by = c("shf_indexyear")
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-incmed)

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))


# Create data with hb, ferritin and transferrin at index ------------------

rsdatalab <- rsdata %>%
  filter(hbtfindex == 1)
