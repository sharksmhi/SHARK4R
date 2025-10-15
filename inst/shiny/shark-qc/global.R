library(SHARK4R)

# Load helper functions
source("helpers.R")

# Clear cache
clean_shark4r_cache(days = 1, verbose = FALSE)

# Translate datatype names
type_lookup <- SHARK4R:::.type_lookup

# Load data
shark_codes <- get_shark_codes()
shark_fields <- load_shark4r_fields(verbose = FALSE)
