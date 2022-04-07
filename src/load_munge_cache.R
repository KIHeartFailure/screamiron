# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("flow")

ProjectTemplate::cache("metaout")

ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("rsdatalab")
ProjectTemplate::cache("rsdatalab_rec_hosphf")
ProjectTemplate::cache("rsdatalab_rec_deathcvhosphf")

ProjectTemplate::cache("imprsdatalab")
ProjectTemplate::cache("imprsdatalabfcm")

ProjectTemplate::cache("lab_iron")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarsfcm")
ProjectTemplate::cache("nomultmodvars")

ProjectTemplate::cache("meta.variables.Sheet.1")