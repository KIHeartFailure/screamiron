

# Variables for tabs/mods -------------------------------------------------

tabvars <- c(
  # demo
  "shf_location",
  "shf_sex",
  "shf_age",
  "shf_age_cat",
  "shf_indexyear_cat",
  "shf_indexyear_cat2",
  "shf_indexyear",

  "iron_need",
  "scream_hb",
  "scream_hb_cat",
  "scream_anemia",
  "scream_id",
  "scream_ferritin",
  "scream_ferritin_cat",
  "scream_ferritin_cat300",
  "scream_transferrin",
  "scream_transferrin_cat",
  
  # clinical factors and lab measurments
  "shf_ef_cat",
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_map",
  "shf_map_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "scream_sodium",
  "scream_sodium_cat",
  "scream_potassium",
  "scream_potassium_cat",
  "scream_gfrckdepi",
  "scream_gfrckdepi_cat",
  "scream_ntprobnp",
  "scream_ntprobnp_cat",

  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_af",
  "shf_sos_com_ihd",
  "shf_sos_com_hypertension",
  "sos_com_peripheralartery",
  "sos_com_stroketia",
  "shf_sos_com_valvular",
  "sos_com_renal",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_muscoloskeletal3y", 
  "sos_com_dementia",
  "sos_com_copd",
  "sos_com_bleed",

  # treatments
  "shf_rasarni",
  "shf_bbl",
  "shf_mra",
  "shf_diuretic",
  "shf_device_cat",
  "shf_digoxin",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_nitrate",
  "shf_ferrocarboxymaltosis",
  "shf_ferrocarboxymaltosisdose",
  "ddr_ironoralsupp",

  # organizational
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # socec
  "scb_fam",
  "scb_education_cat",
  "scb_dispincome_cat2"
)

tabvars_not_in_mod <- c(
  "shf_indexyear",
  "shf_indexyear_cat2",
  "shf_age",
  
  "iron_need",
  "scream_hb",
  "scream_hb_cat",
  "scream_ferritin",
  "scream_ferritin_cat",
  "scream_ferritin_cat300",
  "scream_transferrin",
  "scream_transferrin_cat",
  
  "shf_ferrocarboxymaltosis",
  "shf_ferrocarboxymaltosis_priorinc2016",
  "shf_ferrocarboxymaltosis_postinc2017",
  "shf_ferrocarboxymaltosisdose",
  "ddr_ironoralsupp",
  "ddr_ironoralsupp_priorinc2016",
  "ddr_ironoralsupp_postinc2017",
  
  "shf_nyha",
  "shf_map",
  "shf_bpsys",
  "shf_bpdia",
  "shf_heartrate",
  "shf_bmi",
  "shf_hb",
  "scream_sodium",
  "scream_potassium",
  "scream_gfrckdepi",
  "scream_ntprobnp", 
  
  "sos_com_renal", 
  "sos_com_bleed" 
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]
modvarsfcm <- c(modvars, "shf_ferrocarboxymaltosisimp")

nomultmodvars <- c("sos_com_dementia", "shf_sos_com_hypertension")