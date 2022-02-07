
# Creating variables for competing risk analysis in imputed dataset ------------

# keep org imp
imp.org <- imprsdatalab

# Convert to Long
long <- mice::complete(imprsdatalab, action = "long", include = TRUE)

## Create numeric variables needed for comp risk model
long <- create_crvar(long, "scream_aid")

for (i in seq_along(modvars)) {
  long <- create_crvar(long, modvars[i])
}

# Convert back to Mids
imput.short <- as.mids(long)
imprsdatalab <- imput.short



# keep org imp
imp.org <- imprsdatalabfcm

# Convert to Long
long <- mice::complete(imprsdatalabfcm, action = "long", include = TRUE)

## Create numeric variables needed for comp risk model
long <- create_crvar(long, "scream_aid")

for (i in seq_along(modvarsfcm)) {
  long <- create_crvar(long, modvarsfcm[i])
}

# Convert back to Mids
imput.short <- as.mids(long)
imprsdatalabfcm <- imput.short