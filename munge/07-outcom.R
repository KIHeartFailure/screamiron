

# Comorbidities -----------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "diabetes",
  diakod = " E1[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "af",
  diakod = " I48",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "ihd",
  diakod = " I2[0-5]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "hypertension",
  diakod = " I1[0-5]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "peripheralartery",
  diakod = " I7[0-3]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "pci",
  opkod = " FNG",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out)"
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "cabg",
  diakod = " Z951| Z955",
  opkod = " FNA| FNB| FNC| FND| FNE| FNF| FNH",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "stroketia",
  diakod = " I6[0-4]| I69[0-4]| G45",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "valvular",
  diakod = " I0[5-8]| I3[4-9]| Q22| Q23[0-3]| Z95[2-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "renal",
  diakod = " N1[7-9]| Z491| Z492",
  opkod = " KAS00| KAS10| KAS20| DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "copd",
  diakod = " J4[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "liver",
  diakod = " B18| I85| I864| I982| K70| K710| K711| K71[3-7]| K7[2-4]| K760| K76[2-9]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "dementia",
  diakod = " F0[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "com",
  name = "cancer3y",
  diakod = " C",
  stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "muscoloskeletal3y",
  diakod = " M",
  stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "bleed",
  diakod = " S064| S065| S066| I850| I983| K226| K250| K252| K254| K256| K260| K262| K264| K266| K270| K272| K274| K276| K280| K284| K286| K290| K625| K661| K920| K921| K922| H431| N02| R04| R58| T810| D629",
  opkod = " DR029",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in, out, pc)"
)

# Outcomes ----------------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphf",
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "VAL/KON (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "nohosphf",
  noof = TRUE, 
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  warnings = FALSE, 
  meta_reg = "VAL/KON (in)"
)
