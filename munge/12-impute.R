

# Impute missing values ---------------------------------------------------

modvarsimp <- modvars[modvars != "shf_bmi_cat"]
modvarsimp <- c(modvarsimp, "shf_weight", "shf_height")

noimpvars <- names(rsdatalab)[!names(rsdatalab) %in% modvarsimp]

ini <- mice(rsdatalab, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "scb_dispincome")] <- "polr"
meth[noimpvars] <- ""

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 2
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 4
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imprsdatalab <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "rsdatalab"),
    .packages = "mice"
  ) %dopar% {
    mice(rsdatalab,
      m = m_2_use, maxit = 20, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# keep original imputed data (rsdataimp) just in case
imprsdatalab.org <- imprsdatalab

# Convert to Long
long <- mice::complete(imprsdatalab, action = "long", include = TRUE)

long <- long %>%
  mutate(
    shf_bmi = round(shf_weight / (shf_height / 100)^2, 1),
    shf_bmi_cat = factor(case_when(
      is.na(shf_bmi) ~ NA_real_,
      shf_bmi < 30 ~ 1,
      shf_bmi >= 30 ~ 2
    ), levels = 1:2, labels = c("<30", ">=30"))
  )

# Convert back to mids object
imprsdatalab <- as.mids(long)


# Impute with fcm (<=2016 all imputed with No) ----------------------------

modvarsfcmimp <- modvarsfcm[modvarsfcm != "shf_bmi_cat"]
modvarsfcmimp <- c(modvarsfcmimp, "shf_weight", "shf_height")

noimpvars <- names(rsdatalab)[!names(rsdatalab) %in% modvarsfcmimp]

ini <- mice(rsdatalab, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "scb_dispincome")] <- "polr"
meth[noimpvars] <- ""

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imprsdatalabfcm <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "rsdatalab"),
    .packages = "mice"
  ) %dopar% {
    mice(rsdatalab,
      m = m_2_use, maxit = 20, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# keep original imputed data (rsdataimp) just in case
imprsdatalabfcm.org <- imprsdatalabfcm

# Convert to Long
long <- mice::complete(imprsdatalabfcm, action = "long", include = TRUE)

long <- long %>%
  mutate(
    shf_bmi = round(shf_weight / (shf_height / 100)^2, 1),
    shf_bmi_cat = factor(case_when(
      is.na(shf_bmi) ~ NA_real_,
      shf_bmi < 30 ~ 1,
      shf_bmi >= 30 ~ 2
    ), levels = 1:2, labels = c("<30", ">=30"))
  )

# Convert back to mids object
imprsdatalabfcm <- as.mids(long)
