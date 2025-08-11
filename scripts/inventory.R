# ==============================================================================
# minimal_inventory.R -
# Purpose  = Generate a list of modules present in all PH ENNS survey years
#            and create an inventory table of year × module × data/dictionary
# Author   = Erika Salvador ’28  <esalvador28@amherst.edu>
# Project  = Empirical Evidence of the Q-Q Trade off
# ==============================================================================

library(dplyr)
library(stringr)
library(tidyr)

# ==============================================================================
# STEP 1: Locate project root and data directory
# ==============================================================================
cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
root <- if (basename(cwd) == "scripts") dirname(cwd) else cwd
data_dir <- file.path(root, "data")

# Create outputs folder
out_dir <- file.path(root, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

outfile  <- file.path(out_dir, "inventory_and_mutual.txt")

if (!dir.exists(data_dir)) stop("No 'data' folder found in: ", root)

# ==============================================================================
# STEP 2: List all CSV files in data directory
# ==============================================================================
files <- list.files(data_dir, pattern = "\\.csv$", full.names = FALSE)
if (!length(files)) stop("No .csv files found in: ", data_dir)

# ==============================================================================
# STEP 3: Parse filenames to extract year(s), module name, and type
# ==============================================================================
df <- tibble(file = files) |>
  mutate(
    stem    = str_remove(file, "\\.csv$"),
    years   = str_extract(stem, "^[0-9]{4}(?:_[0-9]{4})*"),
    module  = str_remove(stem, "^[0-9]{4}(?:_[0-9]{4})*_"),
    is_dict = str_detect(module, "_dictionary"),
    module  = str_remove(module, "_dictionary$"),
    module  = if_else(str_starts(module, "infant-and-young-child-feeding"),
                      "infant_young_child_feeding", module)
  ) |>
  separate_rows(years, sep = "_", convert = TRUE) |>
  rename(year = years) |>
  mutate(kind = if_else(is_dict, "dictionary", "data")) |>
  distinct(year, module, kind)

# ==============================================================================
# STEP 4: Create inventory table (year × module × data/dictionary)
# ==============================================================================
inventory <- df |>
  mutate(flag = 1) |>
  pivot_wider(id_cols = c(year, module),
              names_from = kind, values_from = flag, values_fill = 0) |>
  arrange(year, module)

# ==============================================================================
# STEP 5: Identify mutual modules present in ALL years (strict criteria)
# ==============================================================================

all_years <- sort(unique(inventory$year))

mutual <- df |>
  mutate(present = 1L) |>
  complete(year = all_years, module, kind = c("data","dictionary"),
           fill = list(present = 0L)) |>
  group_by(module, year) |>
  summarise(has_both = all(present == 1L), .groups = "drop") |>
  group_by(module) |>
  summarise(ok_all_years = all(has_both), .groups = "drop") |>
  filter(ok_all_years) |>
  arrange(module) |>
  pull(module)


# ==============================================================================
# STEP 6: Write results to outputs/ and print to console
# ==============================================================================
con <- file(outfile, "w")
writeLines("# Mutual modules across all years:", con)
writeLines(paste(" -", mutual), con)
writeLines("\n# Inventory:", con)
write.table(inventory, con, sep = "\t", row.names = FALSE, quote = FALSE)
close(con)

cat("# Mutual modules across all years:\n", paste(" -", mutual, collapse = "\n"), "\n")
cat("\nWrote:", outfile, "\n")