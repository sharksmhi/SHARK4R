# Load helper functions
source("helpers.R")

# Clear cache
clean_shark4r_cache(days = 1, verbose = FALSE)

# Translate datatype names
type_lookup <- c(
  "Bacterioplankton"       = "Bacterioplankton",
  "Chlorophyll"            = "Chlorophyll",
  "Epibenthos"             = "Epibenthos",
  "Grey seal"              = "GreySeal",
  "Harbour Porpoise"       = "HarbourPorpoise",
  "Harbour seal"           = "HarbourSeal",
  "Physical and Chemical"  = "PhysicalChemical",
  "Phytoplankton"          = "Phytoplankton",
  "Picoplankton"           = "Picoplankton",
  "Plankton Barcoding"     = "PlanktonBarcoding",
  "Plankton Imaging"       = "PlanktonImaging",
  "Primary production"     = "PrimaryProduction",
  "Profile"                = "Profile",
  "Ringed seal"            = "RingedSeal",
  "Seal pathology"         = "SealPathology",
  "Sedimentation"          = "Sedimentation",
  "Zoobenthos"             = "Zoobenthos",
  "Zooplankton"            = "Zooplankton"
)

shark_codes <- get_shark_codes()
