#===============================================================================
# DURANTON & PUGA (2023) - UK APPLICATION
# Refactored version with:
#   - Constants defined at top
#   - Chained data transformations
#   - Minimal global assignment
#   - Modular counterfactual functions
#===============================================================================

rm(list = ls())

setwd("/Users/joshmuthu/urb_growth_eng/2100282 EC331 - Code")

#_______________________________________________________________________________
# PACKAGES ####
#_______________________________________________________________________________

packages <- c("readr", "tidyverse", "httr", "jsonlite", "readxl", "ggrepel", "stargazer")

install_if_missing <- function(pkg) {
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
invisible(lapply(packages, install_if_missing))

#_______________________________________________________________________________
# CONSTANTS ####
#_______________________________________________________________________________

# Control flow
SKIP_DATA_CONSTRUCTION <- FALSE

# US parameter values (from DP 2023, Table I p.2238 and Table II p.2242)
# gamma: elasticity of commuting cost w.r.t. distance
# theta: (negative) elasticity of travel speed w.r.t. city population  
# sigma: short-term agglomeration elasticity
# beta: learning/experience agglomeration elasticity
# lambda: land share in rural production (Valentinyi & Herrendorf 2008)
PARAMS_USA <- list(
  tau_1980 = 1,
  tau_2010 = 1.569,
  gamma = 0.07,
  theta = 0.04,
  sigma = 0.04,
  beta = 0.04,
  lambda = 0.18
)

# City population threshold for "medium" BUA
# Source: ONS Census 2021 BUA definitions
CITY_POP_THRESHOLD <- 20000

# Geographic constraint variable choice
GEOGRAPHIC_CONSTRAINT_OPTIONS <- c("mean_z_i", "median_z_i", "min_z_i")
GEOGRAPHIC_CONSTRAINT_VAR <- "mean_z_i"

# England population totals
# Source: ONS mid-year estimates
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop
POP_ENGLAND <- list(
  y2001 = 49449700,
  y2021 = 56554900
)

# Population growth factors for imputation
# Source: Data/eng_wales_pop_over_time.xlsx
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
POP_GROWTH_FACTORS <- list(
  ew_2001_to_2011 = 1.07396242237523,
  ew_2011_to_2021 = 1.06492008640114,
  ew_2001_to_2021 = 1.14368415562741
)

# UK real GDP per capita (CVM, 2012 prices)
# Source: ONS regional GDP
RGDP_PER_CAPITA <- list(
  y2001 = 30316,
  y2021 = 36465
)

# Data file parameters
PERM_DATA_START_ROW <- 33128  # row where 2001 data begins in PS2 file
PERM_DATA_END_YEAR <- 2021

# Counterfactual parameters
PERCENTILE_FOR_PERM_RATE <- 0.98

# Austin, TX comparison data
# Source: https://data.austintexas.gov/Building-and-Development/New-Residential-Units-Summary-by-Calendar-Year-and/2y79-8diw
AUSTIN_DATA_FILE <- "Data/Issued_Construction_Permits_20250927.csv"
AUSTIN_2001_POP <- 669693

# Permitting rate change factors for sensitivity analysis
PERM_RATE_CHANGE_FACTORS <- c(0.05, 0.1, 0.25, 0.3, 0.5)

# London BUA codes for aggregation
LONDON_BUAS <- tibble(
  BUA22NM = c(
    "City and County of the City of London", "Barking and Dagenham", "Barnet",
    "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield",
    "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow",
    "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea",
    "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham",
    "Redbridge", "Richmond upon Thames", "Southwark", "Sutton (Sutton)",
    "Tower Hamlets", "Waltham Forest", "Wandsworth", "City of Westminster"
  ),
  BUA22CD = c(
    "E63004906", "E63004859", "E63004747", "E63004992", "E63004844",
    "E63005189", "E63004858", "E63005267", "E63004894", "E63004679",
    "E63004986", "E63004850", "E63004944", "E63004793", "E63004781",
    "E63004796", "E63004882", "E63005014", "E63004860", "E63004950",
    "E63005164", "E63005063", "E63005035", "E63005121", "E63004881",
    "E63004790", "E63005073", "E63004965", "E63005250", "E63004898",
    "E63004797", "E63005033", "E63004916"
  )
)

# Bordering LADs for imputation of missing permit data
BORDERING_LADS <- list(
  E06000061 = c("E06000017", "E07000141", "E06000031", "E07000011", 
                "E06000055", "E06000042", "E06000062", "E07000131"),
  E06000062 = c("E07000131", "E06000061", "E06000042", "E06000060",
                "E07000177", "E07000221", "E07000220")
)

#_______________________________________________________________________________
# AUSTIN PERMITTING RATE CALCULATION ####
#_______________________________________________________________________________

calculate_austin_perm_rate <- function(filepath = AUSTIN_DATA_FILE) {
  
  meaningful_residence_names <- c(
    "residence", "condo", "duplex", "apartment", "family", "home", 
    "residential", "dwelling", "finish-out", "finish out", "story", "stories"
  )
  meaningful_pattern <- paste(c(meaningful_residence_names, paste0(meaningful_residence_names, "s")), collapse = "|")
  
  other_residence_names <- c("res", "apt", "hm", "sty", "stry")
  other_pattern <- paste(paste0("\\b", c(other_residence_names, paste0(other_residence_names, "s")), "\\b"), collapse = "|")
  
  delete_conditions <- c("clubhouse", "new garage", "new parking garage", "maintenance", "kiosk", "new 2 level parking garage")
  
  total_permits <- read_csv(filepath, show_col_types = FALSE) %>%
    select(`Calendar Year Issued`, `Housing Units`, `Status Current`, Description, `Number Of Floors`) %>%
    filter(
      `Calendar Year Issued` >= 2001,
      `Calendar Year Issued` <= 2021,
      `Status Current` == "Final"
    ) %>%
    mutate(
      `Housing Units` = if_else(`Housing Units` > 1000, `Housing Units` - 1000, `Housing Units`),
      Description = tolower(Description)
    ) %>%
    filter(str_detect(Description, meaningful_pattern) | str_detect(Description, other_pattern)) %>%
    filter(!str_detect(Description, paste(delete_conditions, collapse = "|"))) %>%
    mutate(
      `Housing Units` = if_else(str_detect(Description, "multi"), pmax(5, `Number Of Floors`), 1)
    ) %>%
    summarise(total = sum(`Housing Units`, na.rm = TRUE)) %>%
    pull(total)
  
  rate <- total_permits / AUSTIN_2001_POP
  message(sprintf("Austin permitting rate (2001-2021): %.6f", rate))
  
  rate
}

#_______________________________________________________________________________
# DATA CONSTRUCTION ####
#_______________________________________________________________________________

if (SKIP_DATA_CONSTRUCTION) {
  
  message("Loading pre-existing data, skipping construction...")
  
  city_data <- read_csv("Data/agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv")
  
  # Compute derived population values
  urb_01_pop <- sum(city_data$bua_01_pop)
  urb_21_pop <- sum(city_data$bua_21_pop)
  rur_01_pop <- POP_ENGLAND$y2001 - urb_01_pop
  rur_21_pop <- POP_ENGLAND$y2021 - urb_21_pop
  
} else {
  
  message("Running full data construction pipeline...")
  
  #-----------------------------------------------------------------------------
  # Import raw data
  #-----------------------------------------------------------------------------
  
  lsoa_land_use_2022_raw <- read_csv("Data/lsoa_land_use_2022.csv")
  lsoa_pop_2001_raw <- read_csv("Data/lsoa_pop_2001.csv")
  lsoa_pop_2011_raw <- read_csv("Data/lsoa_pop_2011.csv")
  lsoa_pop_2021_raw <- read_csv("Data/lsoa_pop_2021.csv")
  lsoa_lookup_2001_to_2011_raw <- read_csv("Data/LSOA_(2001)_to_LSOA_(2011)_to_LAD_(2011)_Lookup_in_England_and_Wales.csv")
  lsoa_lookup_2011_to_2021_raw <- read_csv("Data/LSOA_(2011)_to_LSOA_(2021)_to_LAD_(2022)_Best_Fit_Lookup_for_EW_(V2).csv")
  lsoa_bua_lookup_2021_raw <- read_csv("Data/LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv")
  
  perm_rates_raw <- read_csv("Data/PS2_data_-_open_data_table__202409_.csv", skip = 2) %>%
    select(2:4, 34, 112) %>%
    slice(PERM_DATA_START_ROW:n())
  
  #-----------------------------------------------------------------------------
  # 2001-2011 LSOA merging (chained)
  #-----------------------------------------------------------------------------
  
  # Join 2001 pop to 2011 LSOAs, handling merges/splits
  lsoa_pops_2001_2011 <- lsoa_pop_2001_raw %>%
    full_join(lsoa_lookup_2001_to_2011_raw, by = c("lsoa_2001_code" = "LSOA01CD")) %>%
    select(2:7) %>%
    # Aggregate any 2001 LSOAs that merged into single 2011 LSOAs
    group_by(LSOA11CD) %>%
    summarize(
      pop_2001_lsoa_2011_nosplit = sum(usual_resident_pop_2001),
      LSOA11NM = first(LSOA11NM),
      CHGIND = first(CHGIND),
      .groups = "drop"
    ) %>%
    # Join with 2011 population
    left_join(lsoa_pop_2011_raw, by = c("LSOA11CD" = "lsoa_code_2011")) %>%
    select(-lsoa_name_2011) %>%
    # Impute 2001 pop for split LSOAs using national growth rate
    mutate(
      pop_2001_imputed = if_else(
        CHGIND == "S",
        usual_resident_pop_2011 / POP_GROWTH_FACTORS$ew_2001_to_2011,
        pop_2001_lsoa_2011_nosplit
      )
    ) %>%
    select(-pop_2001_lsoa_2011_nosplit)
  
  # Handle 2011 LSOAs that weren't in the 2001-2011 lookup (new LSOAs)
  missing_2011_lsoas <- lsoa_pop_2011_raw %>%
    anti_join(lsoa_pops_2001_2011, by = c("lsoa_code_2011" = "LSOA11CD")) %>%
    transmute(
      LSOA11CD = lsoa_code_2011,
      LSOA11NM = lsoa_name_2011,
      CHGIND = "N",  # newly added
      usual_resident_pop_2011 = usual_resident_pop_2011,
      pop_2001_imputed = usual_resident_pop_2011 / POP_GROWTH_FACTORS$ew_2001_to_2011
    )
  
  lsoa_pops_2001_2011 <- bind_rows(lsoa_pops_2001_2011, missing_2011_lsoas)
  
  #-----------------------------------------------------------------------------
  # Diagnostic: how much population is in split LSOAs?
  #-----------------------------------------------------------------------------
  
  split_diagnostic <- lsoa_pops_2001_2011 %>%
    summarise(
      n_split = sum(CHGIND == "S", na.rm = TRUE),
      n_merged = sum(CHGIND == "M", na.rm = TRUE),
      n_unchanged = sum(CHGIND == "U", na.rm = TRUE),
      n_new = sum(CHGIND == "N", na.rm = TRUE),
      n_total = n(),
      pop_2011_split = sum(usual_resident_pop_2011[CHGIND == "S"], na.rm = TRUE),
      pop_2011_merged = sum(usual_resident_pop_2011[CHGIND == "M"], na.rm = TRUE),
      pop_2011_unchanged = sum(usual_resident_pop_2011[CHGIND == "U"], na.rm = TRUE),
      pop_2011_new = sum(usual_resident_pop_2011[CHGIND == "N"], na.rm = TRUE),
      pop_2011_total = sum(usual_resident_pop_2011, na.rm = TRUE)
    ) %>%
    mutate(
      pct_lsoas_split = 100 * n_split / n_total,
      pct_pop_split = 100 * pop_2011_split / pop_2011_total,
      pct_lsoas_new = 100 * n_new / n_total,
      pct_pop_new = 100 * pop_2011_new / pop_2011_total
    )
  
  message("\n=== LSOA Split/Merge Diagnostic (2001→2011) ===")
  message(sprintf("Total 2011 LSOAs: %d", split_diagnostic$n_total))
  message(sprintf("  - Unchanged (U): %d (%.1f%% of LSOAs, %.1f%% of 2011 pop)",
                  split_diagnostic$n_unchanged,
                  100 * split_diagnostic$n_unchanged / split_diagnostic$n_total,
                  100 * split_diagnostic$pop_2011_unchanged / split_diagnostic$pop_2011_total))
  message(sprintf("  - Split (S):     %d (%.1f%% of LSOAs, %.1f%% of 2011 pop) ← 2001 POP IMPUTED",
                  split_diagnostic$n_split,
                  split_diagnostic$pct_lsoas_split,
                  split_diagnostic$pct_pop_split))
  message(sprintf("  - Merged (M):    %d (%.1f%% of LSOAs, %.1f%% of 2011 pop)",
                  split_diagnostic$n_merged,
                  100 * split_diagnostic$n_merged / split_diagnostic$n_total,
                  100 * split_diagnostic$pop_2011_merged / split_diagnostic$pop_2011_total))
  message(sprintf("  - New (N):       %d (%.1f%% of LSOAs, %.1f%% of 2011 pop) ← 2001 POP IMPUTED",
                  split_diagnostic$n_new,
                  split_diagnostic$pct_lsoas_new,
                  split_diagnostic$pct_pop_new))
  message(sprintf("LSOAs with imputed 2001 pop: %.0f people in 2011 (%.1f%% of 2011 total)",
                  split_diagnostic$pop_2011_split + split_diagnostic$pop_2011_new,
                  split_diagnostic$pct_pop_split + split_diagnostic$pct_pop_new))
  message("================================================\n")
  
  #-----------------------------------------------------------------------------
  # 2011-2021 LSOA merging (chained)
  #-----------------------------------------------------------------------------
  
  lsoa_pops_all_years <- lsoa_pops_2001_2011 %>%
    full_join(
      lsoa_lookup_2011_to_2021_raw %>% select(2:7),
      by = "LSOA11CD"
    ) %>%
    group_by(LSOA21CD) %>%
    summarize(
      pop_2011_final = sum(usual_resident_pop_2011, na.rm = TRUE),
      pop_2001_final = sum(pop_2001_imputed, na.rm = TRUE),
      LSOA21NM = first(LSOA21NM),
      LAD22CD = first(LAD22CD),
      LAD22NM = first(LAD22NM),
      .groups = "drop"
    ) %>%
    full_join(lsoa_pop_2021_raw, by = c("LSOA21CD" = "lsoa_21_code")) %>%
    select(-LSOA21NM) %>%
    mutate(
      pop_2011_final = if_else(
        is.na(pop_2011_final) | pop_2011_final == 0,
        total_lsoa_pop_21 / POP_GROWTH_FACTORS$ew_2011_to_2021,
        pop_2011_final
      ),
      pop_2001_final = if_else(
        is.na(pop_2001_final) | pop_2001_final == 0,
        total_lsoa_pop_21 / POP_GROWTH_FACTORS$ew_2001_to_2021,
        pop_2001_final
      )
    ) %>%
    filter(startsWith(LSOA21CD, "E"))
  
  #-----------------------------------------------------------------------------
  # Join land use data
  #-----------------------------------------------------------------------------
  
  lsoa_with_land_use <- lsoa_pops_all_years %>%
    left_join(lsoa_land_use_2022_raw, by = c("LSOA21CD" = "lsoa_2021_code")) %>%
    select(-lsoa_2021_name)
  
  #-----------------------------------------------------------------------------
  # Fix missing LAD codes
  #-----------------------------------------------------------------------------
  
  lads_to_add <- read_csv("Data/lads_to_add_backup.csv")
  
  lsoa_with_lad <- lsoa_with_land_use %>%
    left_join(lads_to_add, by = "LSOA21CD") %>%
    mutate(LAD22CD = coalesce(LAD22CD.x, LAD22CD.y)) %>%
    select(-LAD22CD.x, -LAD22CD.y)
  
  valid_lad_codes <- lsoa_with_lad %>%
    filter(!is.na(LAD22CD)) %>%
    distinct(LAD22CD) %>%
    pull(LAD22CD)
  
  #-----------------------------------------------------------------------------
  # Process permitting data
  #-----------------------------------------------------------------------------
  
  perm_rates_by_year <- perm_rates_raw %>%
    filter(LPACD %in% valid_lad_codes) %>%
    mutate(
      major_dwellings = as.numeric(na_if(`Total granted; major dwellings (all)`, "..")),
      minor_dwellings = as.numeric(na_if(`Total granted; minor dwellings (all)`, "..")),
      year = as.numeric(substr(Quarter, 1, 4))
    ) %>%
    group_by(LPACD, LPANM, year) %>%
    summarise(
      total_dwellings = sum(major_dwellings, na.rm = TRUE) + sum(minor_dwellings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(year <= PERM_DATA_END_YEAR)
  
  impute_permits_lm <- function(data) {
    if (sum(!is.na(data$total_dwellings)) < 2) return(data)
    
    model <- lm(total_dwellings ~ year, data = data[!is.na(data$total_dwellings), ])
    data$total_dwellings_imputed <- pmax(predict(model, newdata = data), 0)
    data$total_dwellings_final <- if_else(
      is.na(data$total_dwellings),
      data$total_dwellings_imputed,
      data$total_dwellings
    )
    data
  }
  
  lpacd_cant_impute <- perm_rates_by_year %>%
    group_by(LPACD) %>%
    summarise(n_nonmiss = sum(!is.na(total_dwellings)), .groups = "drop") %>%
    filter(n_nonmiss <= 1) %>%
    pull(LPACD)
  
  perm_rates_imputed <- perm_rates_by_year %>%
    group_by(LPACD, LPANM) %>%
    group_modify(~impute_permits_lm(.x)) %>%
    ungroup()
  
  impute_from_bordering <- function(target_lpacd, bordering_lpacds, data) {
    valid_bordering <- bordering_lpacds[bordering_lpacds %in% setdiff(unique(data$LPACD), lpacd_cant_impute)]
    if (length(valid_bordering) == 0) return(NULL)
    
    data %>%
      filter(LPACD %in% valid_bordering) %>%
      group_by(year) %>%
      summarise(total_dwellings_final = median(total_dwellings_final, na.rm = TRUE), .groups = "drop") %>%
      mutate(LPACD = target_lpacd)
  }
  
  bordering_imputed <- map2_dfr(
    names(BORDERING_LADS),
    BORDERING_LADS,
    ~impute_from_bordering(.x, .y, perm_rates_imputed)
  )
  
  perm_rates_final <- perm_rates_imputed %>%
    filter(!LPACD %in% lpacd_cant_impute) %>%
    select(LPACD, LPANM, year, total_dwellings_final) %>%
    bind_rows(bordering_imputed) %>%
    group_by(LPACD, year) %>%
    summarise(total_dwellings_final = sum(total_dwellings_final, na.rm = TRUE), .groups = "drop")
  
  perm_rates_summary <- perm_rates_final %>%
    group_by(LPACD) %>%
    summarise(
      sum_perms_01_11 = sum(total_dwellings_final[year >= 2001 & year <= 2011], na.rm = TRUE),
      sum_perms_01_21 = sum(total_dwellings_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  #-----------------------------------------------------------------------------
  # LSOA to BUA mapping
  #-----------------------------------------------------------------------------
  
  buas_to_add <- read_csv("Data/buas_to_add_backup.csv")
  
  lsoa_bua_lookup <- lsoa_bua_lookup_2021_raw %>%
    select(-ends_with("W"), -ObjectId) %>%
    filter(startsWith(LSOA21CD, "E")) %>%
    left_join(buas_to_add, by = "LSOA21CD") %>%
    mutate(BUA22CD = coalesce(BUA22CD.x, BUA22CD.y)) %>%
    select(-BUA22CD.x, -BUA22CD.y)
  
  #-----------------------------------------------------------------------------
  # Join all LSOA data together
  #-----------------------------------------------------------------------------
  
  lsoa_full <- lsoa_with_lad %>%
    left_join(lsoa_bua_lookup %>% select(LSOA21CD, BUA22CD, BUA22NM, RGN22CD, RGN22NM), by = "LSOA21CD") %>%
    filter(BUA22CD != "E63999999") %>%
    mutate(
      BUA22NM = if_else(LSOA21CD == "E01002946", "Kingston upon Thames", BUA22NM),
      BUA22CD = if_else(LSOA21CD == "E01002946", "E63005164", BUA22CD)
    )
  
  #-----------------------------------------------------------------------------
  # Distribute LAD permits to BUAs by population share
  #-----------------------------------------------------------------------------
  
  lad_bua_pop_shares <- lsoa_full %>%
    filter(!is.na(BUA22CD)) %>%
    group_by(LAD22CD, BUA22CD) %>%
    summarise(bua_in_lad_pop = sum(pop_2001_final, na.rm = TRUE), .groups = "drop") %>%
    group_by(LAD22CD) %>%
    mutate(
      lad_total_pop = sum(bua_in_lad_pop),
      pop_share = bua_in_lad_pop / lad_total_pop
    ) %>%
    ungroup()
  
  bua_permits <- lad_bua_pop_shares %>%
    left_join(perm_rates_summary, by = c("LAD22CD" = "LPACD")) %>%
    mutate(
      bua_perms_01_11 = sum_perms_01_11 * pop_share,
      bua_perms_01_21 = sum_perms_01_21 * pop_share
    ) %>%
    group_by(BUA22CD) %>%
    summarise(
      bua_pop_01 = sum(bua_in_lad_pop, na.rm = TRUE),
      sum_bua_perms_01_11 = sum(bua_perms_01_11, na.rm = TRUE),
      sum_bua_perms_01_21 = sum(bua_perms_01_21, na.rm = TRUE),
      .groups = "drop"
    )
  
  #-----------------------------------------------------------------------------
  # Aggregate to BUA level with London consolidation
  #-----------------------------------------------------------------------------
  
  bua_permits_london <- bua_permits %>%
    mutate(BUA22CD = if_else(BUA22CD %in% LONDON_BUAS$BUA22CD, "LON999999", BUA22CD)) %>%
    group_by(BUA22CD) %>%
    summarise(
      bua_pop_01 = sum(bua_pop_01),
      sum_bua_perms_01_11 = sum(sum_bua_perms_01_11),
      sum_bua_perms_01_21 = sum(sum_bua_perms_01_21),
      .groups = "drop"
    ) %>%
    mutate(
      bua_perm_rate_01_11 = sum_bua_perms_01_11 / bua_pop_01,
      bua_perm_rate_01_21 = sum_bua_perms_01_21 / bua_pop_01
    )
  
  city_data <- lsoa_full %>%
    mutate(
      BUA22CD = if_else(BUA22CD %in% LONDON_BUAS$BUA22CD, "LON999999", BUA22CD),
      BUA22NM = if_else(BUA22NM %in% LONDON_BUAS$BUA22NM, "Greater London", BUA22NM)
    ) %>%
    group_by(BUA22CD) %>%
    summarise(
      BUA22NM = first(na.omit(BUA22NM)),
      RGN22CD = first(na.omit(RGN22CD)),
      RGN22NM = first(na.omit(RGN22NM)),
      bua_01_pop = sum(pop_2001_final),
      bua_11_pop = sum(pop_2011_final),
      bua_21_pop = sum(total_lsoa_pop_21),
      mean_z_i = 1 / (1 - mean(pct_forest_open_land_water) / 100),
      median_z_i = 1 / (1 - median(pct_forest_open_land_water) / 100),
      min_z_i = 1 / (1 - min(pct_forest_open_land_water) / 100),
      .groups = "drop"
    ) %>%
    left_join(bua_permits_london %>% select(BUA22CD, bua_perm_rate_01_11, bua_perm_rate_01_21), by = "BUA22CD") %>%
    filter(bua_21_pop >= CITY_POP_THRESHOLD) %>%
    mutate(across(c(bua_01_pop, bua_11_pop, bua_21_pop), round)) %>%
    select(-all_of(setdiff(GEOGRAPHIC_CONSTRAINT_OPTIONS, GEOGRAPHIC_CONSTRAINT_VAR))) %>%
    rename(geographic_constraint = all_of(GEOGRAPHIC_CONSTRAINT_VAR)) %>%
    arrange(desc(bua_21_pop))
  
  write_csv(city_data, "Data/agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv")
  
  urb_01_pop <- sum(city_data$bua_01_pop)
  urb_21_pop <- sum(city_data$bua_21_pop)
  rur_01_pop <- POP_ENGLAND$y2001 - urb_01_pop
  rur_21_pop <- POP_ENGLAND$y2021 - urb_21_pop
  
  message("Data construction complete.")
}


#===============================================================================
# COUNTERFACTUAL ANALYSIS FUNCTIONS
#===============================================================================

#-------------------------------------------------------------------------------
# Prepare data for counterfactual (returns list, no global assignment)
#-------------------------------------------------------------------------------
prepare_counterfactual_data <- function(
    city_data,
    cities_in_cf,
    params,  # list with gamma, theta, sigma, beta, lambda
    pop_totals  # list with tot_01, tot_21, urb_01, urb_21, rur_01, rur_21
) {
  gamma <- params$gamma
  theta <- params$theta
  sigma <- params$sigma
  beta <- params$beta
  lambda <- params$lambda
  
  # Compute income and consumption coefficients (DP equations 8, 21, 22)
  cf_data <- city_data %>%
    mutate(
      # Coefficient for rho*A*h (eq 21 divided by tau)
      eq_rho_A_h_coeff = (gamma + theta) / ((sigma + beta) * (gamma + 1)) *
        geographic_constraint^gamma,
      eq_rho_A_h_coeff_01 = eq_rho_A_h_coeff * bua_01_pop^(gamma + theta - sigma - beta),
      eq_rho_A_h_coeff_21 = eq_rho_A_h_coeff * bua_21_pop^(gamma + theta - sigma - beta),
      
      # Income / tau (eq 8)
      income_div_tau_01 = eq_rho_A_h_coeff_01 * bua_01_pop^(sigma + beta),
      income_div_tau_21 = eq_rho_A_h_coeff_21 * bua_21_pop^(sigma + beta),
      
      # Consumption / tau (eq 22)
      cons_coeff = (gamma + theta - sigma - beta) / ((sigma + beta) * (gamma + 1)) *
        geographic_constraint^gamma,
      cons_div_tau_01 = cons_coeff * bua_01_pop^(gamma + theta),
      cons_div_tau_21 = cons_coeff * bua_21_pop^(gamma + theta)
    ) %>%
    select(-eq_rho_A_h_coeff, -cons_coeff)
  
  # Impute tau from GDP growth
  rgdp_multiple <- RGDP_PER_CAPITA$y2021 / RGDP_PER_CAPITA$y2001
  
  sum_income_pc_01_div_tau <- sum(cf_data$income_div_tau_01 * cf_data$bua_01_pop) / pop_totals$urb_01
  sum_income_pc_21_div_tau <- sum(cf_data$income_div_tau_21 * cf_data$bua_21_pop) / pop_totals$urb_21
  
  tau_01 <- 1  # numeraire
  tau_21 <- rgdp_multiple / (sum_income_pc_21_div_tau / sum_income_pc_01_div_tau)
  
  # Compute actual income and consumption values
  cf_data <- cf_data %>%
    mutate(
      rho_A_h_01 = eq_rho_A_h_coeff_01 * tau_01,
      rho_A_h_21 = eq_rho_A_h_coeff_21 * tau_21,
      y_01 = income_div_tau_01 * tau_01,
      y_21 = income_div_tau_21 * tau_21,
      c_01 = cons_div_tau_01 * tau_01,
      c_21 = cons_div_tau_21 * tau_21
    ) %>%
    select(-ends_with("_div_tau"), -starts_with("eq_rho_A_h_coeff"))
  
  # Rural income = consumption in marginal city
  rur_01_income <- min(cf_data$c_01)
  rur_21_income <- min(cf_data$c_21)
  
  # Rural productivity (from eq 11/40)
  rur_01_prod <- rur_01_income / (pop_totals$rur_01^(-lambda))
  rur_21_prod <- rur_21_income / (pop_totals$rur_21^(-lambda))
  
  # Add permitting costs and counterfactual indicator
  cf_data <- cf_data %>%
    mutate(
      perm_01 = c_01 - rur_01_income,
      perm_21 = c_21 - rur_21_income,
      in_counterfact = as.integer(BUA22NM %in% cities_in_cf),
      pop_21_incumb = pmin(bua_01_pop, bua_21_pop)
    ) %>%
    arrange(desc(bua_01_pop))
  
  rur_21_pop_incumb <- pop_totals$tot_21 - sum(cf_data$pop_21_incumb)
  
  list(
    data = cf_data,
    tau_01 = tau_01,
    tau_21 = tau_21,
    rur_01_income = rur_01_income,
    rur_21_income = rur_21_income,
    rur_01_prod = rur_01_prod,
    rur_21_prod = rur_21_prod,
    rur_21_pop_incumb = rur_21_pop_incumb,
    params = params,
    pop_totals = pop_totals
  )
}


#-------------------------------------------------------------------------------
# Compute counterfactual populations given permitting rate changes
#-------------------------------------------------------------------------------
compute_cf_populations <- function(cf_prep, perm_rate_cf) {
  cf_prep$data %>%
    mutate(
      perm_rate_counterfact = perm_rate_cf,
      bua_21_pop_counterfact = if_else(
        in_counterfact == 1,
        (perm_rate_counterfact / bua_perm_rate_01_21) * (bua_21_pop - bua_01_pop) + bua_01_pop,
        as.numeric(bua_21_pop)
      )
    )
}


#-------------------------------------------------------------------------------
# Find marginal city
#-------------------------------------------------------------------------------
find_marginal_city <- function(cf_data, cf_prep) {
  params <- cf_prep$params
  pop_totals <- cf_prep$pop_totals
  tau_21 <- cf_prep$tau_21
  rur_21_prod <- cf_prep$rur_21_prod
  
  gamma <- params$gamma
  theta <- params$theta
  sigma <- params$sigma
  beta <- params$beta
  
  num_cities <- nrow(cf_data)
  marginal_city_index <- num_cities
  
  for (i in 1:num_cities) {
    temp_data <- cf_data %>%
      mutate(
        pop_21_cf = bua_21_pop_counterfact,
        y_21_cf = (y_21 / bua_21_pop^(sigma + beta)) * pop_21_cf^(sigma + beta),
        c_21_cf = y_21_cf - (1 / (gamma + 1)) * tau_21 * geographic_constraint^gamma * pop_21_cf^(gamma + theta)
      ) %>%
      arrange(desc(c_21_cf)) %>%
      mutate(
        city_order = row_number(),
        cumpop = cumsum(pop_21_cf)
      )
    
    temp_data <- temp_data %>%
      mutate(
        pop_21_cf = if_else(cumpop > pop_totals$tot_21 | city_order > marginal_city_index, NA_real_, pop_21_cf),
        c_21_rural_threshold = if_else(
          cumpop >= pop_totals$tot_21,
          Inf,
          rur_21_prod * (pop_totals$tot_21 - cumpop)^(-params$lambda)
        )
      )
    
    marg_city <- temp_data %>% filter(city_order == marginal_city_index)
    
    if (nrow(marg_city) == 0 || is.na(marg_city$pop_21_cf)) {
      marginal_city_index <- marginal_city_index - 1
    } else if (marg_city$c_21_rural_threshold > marg_city$c_21_cf) {
      marginal_city_index <- marginal_city_index - 1
    } else {
      break
    }
    
    if (marginal_city_index < 1) break
  }
  
  final_data <- cf_data %>%
    mutate(
      pop_21_cf = bua_21_pop_counterfact,
      y_21_cf = (y_21 / bua_21_pop^(sigma + beta)) * pop_21_cf^(sigma + beta),
      c_21_cf = y_21_cf - (1 / (gamma + 1)) * tau_21 * geographic_constraint^gamma * pop_21_cf^(gamma + theta)
    ) %>%
    arrange(desc(c_21_cf)) %>%
    mutate(
      city_order = row_number(),
      cumpop = cumsum(pop_21_cf)
    ) %>%
    mutate(
      pop_21_cf = if_else(city_order > marginal_city_index, NA_real_, pop_21_cf),
      y_21_cf = if_else(is.na(pop_21_cf), NA_real_, y_21_cf),
      c_21_cf = if_else(is.na(pop_21_cf), NA_real_, c_21_cf)
    )
  
  pop_cum_final <- final_data %>% 
    filter(!is.na(pop_21_cf)) %>%
    summarise(total = sum(pop_21_cf)) %>%
    pull(total)
  
  c_21_rural_threshold <- rur_21_prod * (pop_totals$tot_21 - pop_cum_final)^(-params$lambda)
  
  final_data <- final_data %>%
    mutate(
      c_21_rural_threshold = c_21_rural_threshold,
      perm_21_counterfact = c_21_cf - c_21_rural_threshold,
      pop_21_incumb_counterfact = pmin(pop_21_cf, pop_21_incumb)
    )
  
  list(
    data = final_data,
    marginal_city_index = marginal_city_index,
    c_21_rur_counterfact = c_21_rural_threshold,
    pop_21_rur_counterfact = pop_totals$tot_21 - pop_cum_final
  )
}

#-------------------------------------------------------------------------------
# Compute welfare metrics
#-------------------------------------------------------------------------------
compute_welfare_metrics <- function(marg_result, cf_prep) {
  final_data <- marg_result$data
  pop_totals <- cf_prep$pop_totals
  rur_21_income <- cf_prep$rur_21_income
  rur_21_pop_incumb <- cf_prep$rur_21_pop_incumb
  c_21_rur_cf <- marg_result$c_21_rur_counterfact
  pop_21_rur_cf <- marg_result$pop_21_rur_counterfact
  
  # Baseline values
  y_21_baseline <- sum(final_data$y_21 * (final_data$bua_21_pop / pop_totals$tot_21), na.rm = TRUE) +
    rur_21_income * (pop_totals$rur_21 / pop_totals$tot_21)
  
  c_21_baseline <- sum(final_data$c_21 * (final_data$pop_21_incumb / pop_totals$tot_21), na.rm = TRUE) +
    rur_21_income * (rur_21_pop_incumb / pop_totals$tot_21)
  
  # Counterfactual values
  y_21_cf <- sum(final_data$y_21_cf * (final_data$pop_21_cf / pop_totals$tot_21), na.rm = TRUE) +
    c_21_rur_cf * (pop_21_rur_cf / pop_totals$tot_21)
  
  pop_21_rur_incumb_cf <- pop_totals$tot_21 - sum(final_data$pop_21_incumb_counterfact, na.rm = TRUE)
  
  c_21_cf <- sum(final_data$c_21_cf * (final_data$pop_21_incumb_counterfact / pop_totals$tot_21), na.rm = TRUE) +
    c_21_rur_cf * (pop_21_rur_incumb_cf / pop_totals$tot_21)
  
  # Percentage changes
  pct_chg_y <- 100 * (y_21_cf - y_21_baseline) / y_21_baseline
  pct_chg_c <- 100 * (c_21_cf - c_21_baseline) / c_21_baseline
  pct_chg_c_rur <- 100 * (c_21_rur_cf - rur_21_income) / rur_21_income
  pct_chg_pop_rur <- 100 * (pop_21_rur_cf - pop_totals$rur_21) / pop_totals$rur_21
  
  list(
    y_21_baseline = y_21_baseline,
    c_21_baseline = c_21_baseline,
    y_21_counterfactual = y_21_cf,
    c_21_counterfactual = c_21_cf,
    pct_chg_y_tot = pct_chg_y,
    pct_chg_c_tot = pct_chg_c,
    pct_chg_c_rur = pct_chg_c_rur,
    pct_chg_pop_rur = pct_chg_pop_rur,
    c_21_rur_counterfact = c_21_rur_cf,
    pop_21_rur_counterfact = pop_21_rur_cf
  )
}


#-------------------------------------------------------------------------------
# Run single counterfactual scenario (streamlined for sweep)
#-------------------------------------------------------------------------------
run_single_counterfactual <- function(cf_prep, perm_rate_cf) {
  cf_pop_data <- compute_cf_populations(cf_prep, perm_rate_cf)
  marg_result <- find_marginal_city(cf_pop_data, cf_prep)
  welfare <- compute_welfare_metrics(marg_result, cf_prep)
  
  final_data <- marg_result$data %>%
    mutate(
      pct_chg_y = 100 * (y_21_cf - y_21) / y_21,
      pct_chg_c = 100 * (c_21_cf - c_21) / c_21
    )
  
  list(
    data = final_data,
    welfare = welfare
  )
}

#===============================================================================
# PARAMETER SWEEP AND FAN CHART ANALYSIS (OPTIMIZED)
#===============================================================================

message("\n=== SETTING UP PARAMETER SWEEP ===\n")

#-------------------------------------------------------------------------------
# Population totals
#-------------------------------------------------------------------------------
pop_totals <- list(
  tot_01 = POP_ENGLAND$y2001,
  tot_21 = POP_ENGLAND$y2021,
  urb_01 = urb_01_pop,
  urb_21 = urb_21_pop,
  rur_01 = rur_01_pop,
  rur_21 = rur_21_pop
)

#-------------------------------------------------------------------------------
# Parameter grids
#-------------------------------------------------------------------------------
param_grid <- expand.grid(
  gamma = c(0.03, 0.05, 0.08),
  theta = c(0.05, 0.06, 0.07),
  sigma = c(0.008, 0.025, 0.038),
  beta  = c(0.01, 0.015, 0.03),
  lambda = c(0.10, 0.12, 0.16),
  stringsAsFactors = FALSE
)

central_params <- list(gamma = 0.05, theta = 0.06, sigma = 0.025, beta = 0.015, lambda = 0.12)

message(sprintf("Parameter grid has %d combinations", nrow(param_grid)))

#-------------------------------------------------------------------------------
# City sets
#-------------------------------------------------------------------------------
cities_sets <- list(
  london_only = city_data$BUA22NM[1],
  top_4 = city_data$BUA22NM[1:4],
  top_6 = city_data$BUA22NM[1:6],
  university_cities = c("Greater London", "Oxford", "Cambridge (Cambridge)"),
  top_10 = city_data$BUA22NM[1:10]
)

message("\nCity sets:")
for (nm in names(cities_sets)) {
  message(sprintf("  %s: %s", nm, paste(cities_sets[[nm]], collapse = ", ")))
}

#-------------------------------------------------------------------------------
# Calculate reference rates
#-------------------------------------------------------------------------------
austin_rate <- calculate_austin_perm_rate()
all_perm_rates <- city_data$bua_perm_rate_01_21

pct_75 <- quantile(all_perm_rates, 0.75)
pct_95 <- quantile(all_perm_rates, 0.95)
max_uk_rate <- max(all_perm_rates)

city_at_75 <- city_data %>% 
  filter(bua_perm_rate_01_21 >= pct_75) %>% 
  slice_min(bua_perm_rate_01_21, n = 1, with_ties = TRUE) %>%
  slice_max(bua_21_pop, n = 1, with_ties = FALSE) %>% 
  pull(BUA22NM)

city_at_95 <- city_data %>% 
  filter(bua_perm_rate_01_21 >= pct_95) %>% 
  slice_min(bua_perm_rate_01_21, n = 1, with_ties = TRUE) %>%
  slice_max(bua_21_pop, n = 1, with_ties = FALSE) %>% 
  pull(BUA22NM)

city_at_max <- city_data %>% 
  filter(bua_perm_rate_01_21 == max_uk_rate) %>% 
  slice_max(bua_21_pop, n = 1, with_ties = FALSE) %>% 
  pull(BUA22NM)

reference_rates <- tibble(
  label = c(
    sprintf("75th pct (%s)", city_at_75),
    sprintf("95th pct (%s)", city_at_95),
    sprintf("UK Max (%s)", city_at_max),
    "Austin, TX"
  ),
  rate = c(pct_75, pct_95, max_uk_rate, austin_rate)
)

message("\nReference rates:")
print(reference_rates)

#-------------------------------------------------------------------------------
# Permitting rate sweep range
#-------------------------------------------------------------------------------
rate_start <- floor(pct_75 * 100) / 100
rate_end <- 0.13
rate_increment <- 0.001
rate_sequence <- seq(rate_start, rate_end, by = rate_increment)

message(sprintf("\nPermitting rate sweep: %.4f to %.4f by %.4f (%d values)", 
                rate_start, rate_end, rate_increment, length(rate_sequence)))

#-------------------------------------------------------------------------------
# STEP 1: Find extreme parameter combinations at a single test rate
#-------------------------------------------------------------------------------
identify_extreme_params <- function(cities_in_cf, param_grid, test_rate, pop_totals) {
  
  results <- map_dfr(1:nrow(param_grid), function(p) {
    params <- list(
      gamma = param_grid$gamma[p],
      theta = param_grid$theta[p],
      sigma = param_grid$sigma[p],
      beta = param_grid$beta[p],
      lambda = param_grid$lambda[p]
    )
    
    cf_prep <- prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals)
    
    perm_rate_cf <- cf_prep$data %>%
      mutate(rate = if_else(in_counterfact == 1, pmax(bua_perm_rate_01_21, test_rate), bua_perm_rate_01_21)) %>%
      pull(rate)
    
    result <- run_single_counterfactual(cf_prep, perm_rate_cf)
    
    treated <- result$data %>% filter(in_counterfact == 1, !is.na(pct_chg_y))
    pct_chg_y_cities <- if (nrow(treated) > 0) {
      sum(treated$pct_chg_y * treated$bua_21_pop) / sum(treated$bua_21_pop)
    } else NA_real_
    
    tibble(
      param_idx = p,
      pct_chg_newcomer_cons = result$welfare$pct_chg_c_rur,
      pct_chg_national_gdp = result$welfare$pct_chg_y_tot,
      pct_chg_city_income = pct_chg_y_cities
    )
  })
  
  # Identify which param_idx gives max/min for each metric
  list(
    newcomer_cons_max_idx = results$param_idx[which.max(results$pct_chg_newcomer_cons)],
    newcomer_cons_min_idx = results$param_idx[which.min(results$pct_chg_newcomer_cons)],
    national_gdp_max_idx = results$param_idx[which.max(results$pct_chg_national_gdp)],
    national_gdp_min_idx = results$param_idx[which.min(results$pct_chg_national_gdp)],
    city_income_max_idx = results$param_idx[which.max(results$pct_chg_city_income)],
    city_income_min_idx = results$param_idx[which.min(results$pct_chg_city_income)]
  )
}

#-------------------------------------------------------------------------------
# STEP 2: Run sweep for just 3 param sets (central, max, min) per metric
#-------------------------------------------------------------------------------
run_optimized_sweep <- function(city_set_name, cities_in_cf, param_grid, rate_sequence, 
                                pop_totals, extreme_indices) {
  
  message(sprintf("\n=== Running optimized sweep for: %s ===", city_set_name))
  
  # Unique param indices needed (central + extremes for all metrics)
  central_idx <- which(
    param_grid$gamma == central_params$gamma &
      param_grid$theta == central_params$theta &
      param_grid$sigma == central_params$sigma &
      param_grid$beta == central_params$beta &
      param_grid$lambda == central_params$lambda
  )
  
  unique_indices <- unique(c(
    central_idx,
    extreme_indices$newcomer_cons_max_idx,
    extreme_indices$newcomer_cons_min_idx,
    extreme_indices$national_gdp_max_idx,
    extreme_indices$national_gdp_min_idx,
    extreme_indices$city_income_max_idx,
    extreme_indices$city_income_min_idx
  ))
  
  message(sprintf("  Running %d unique parameter sets × %d rates = %d evaluations",
                  length(unique_indices), length(rate_sequence), 
                  length(unique_indices) * length(rate_sequence)))
  
  results <- vector("list", length(unique_indices) * length(rate_sequence))
  idx <- 0
  
  pb <- txtProgressBar(min = 0, max = length(unique_indices) * length(rate_sequence), style = 3)
  
  for (p in unique_indices) {
    params <- list(
      gamma = param_grid$gamma[p],
      theta = param_grid$theta[p],
      sigma = param_grid$sigma[p],
      beta = param_grid$beta[p],
      lambda = param_grid$lambda[p]
    )
    
    cf_prep <- prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals)
    
    for (r in seq_along(rate_sequence)) {
      target_rate <- rate_sequence[r]
      
      perm_rate_cf <- cf_prep$data %>%
        mutate(rate = if_else(in_counterfact == 1, pmax(bua_perm_rate_01_21, target_rate), bua_perm_rate_01_21)) %>%
        pull(rate)
      
      result <- run_single_counterfactual(cf_prep, perm_rate_cf)
      
      treated <- result$data %>% filter(in_counterfact == 1, !is.na(pct_chg_y))
      pct_chg_y_cities <- if (nrow(treated) > 0) {
        sum(treated$pct_chg_y * treated$bua_21_pop) / sum(treated$bua_21_pop)
      } else NA_real_
      
      idx <- idx + 1
      results[[idx]] <- tibble(
        param_idx = p,
        is_central = (p == central_idx),
        target_rate = target_rate,
        pct_chg_newcomer_cons = result$welfare$pct_chg_c_rur,
        pct_chg_national_gdp = result$welfare$pct_chg_y_tot,
        pct_chg_city_income = pct_chg_y_cities
      )
      
      setTxtProgressBar(pb, idx)
    }
  }
  close(pb)
  
  all_results <- bind_rows(results)
  
  # Reshape into fan format
  fan_data <- tibble(target_rate = rate_sequence) %>%
    left_join(
      all_results %>% filter(is_central) %>% 
        select(target_rate, nc_central = pct_chg_newcomer_cons, 
               gdp_central = pct_chg_national_gdp, ci_central = pct_chg_city_income),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$newcomer_cons_max_idx) %>%
        select(target_rate, nc_max = pct_chg_newcomer_cons),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$newcomer_cons_min_idx) %>%
        select(target_rate, nc_min = pct_chg_newcomer_cons),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$national_gdp_max_idx) %>%
        select(target_rate, gdp_max = pct_chg_national_gdp),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$national_gdp_min_idx) %>%
        select(target_rate, gdp_min = pct_chg_national_gdp),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$city_income_max_idx) %>%
        select(target_rate, ci_max = pct_chg_city_income),
      by = "target_rate"
    ) %>%
    left_join(
      all_results %>% filter(param_idx == extreme_indices$city_income_min_idx) %>%
        select(target_rate, ci_min = pct_chg_city_income),
      by = "target_rate"
    ) %>%
    mutate(city_set = city_set_name)
  
  list(
    fan_data = fan_data,
    extreme_indices = extreme_indices,
    central_idx = central_idx
  )
}

#-------------------------------------------------------------------------------
# Run for all city sets
#-------------------------------------------------------------------------------
test_rate <- mean(c(rate_start, rate_end))  # midpoint for identification

all_fan_data <- list()
all_extreme_params <- list()

for (cs in names(cities_sets)) {
  message(sprintf("\n>>> Processing city set: %s", cs))
  
  # Step 1: identify extremes
  message("  Identifying extreme parameter combinations...")
  extreme_idx <- identify_extreme_params(cities_sets[[cs]], param_grid, test_rate, pop_totals)
  all_extreme_params[[cs]] <- extreme_idx
  
  # Log the extreme params
  message(sprintf("  Newcomer cons max: γ=%.2f,θ=%.2f,σ=%.3f,β=%.2f,λ=%.2f",
                  param_grid$gamma[extreme_idx$newcomer_cons_max_idx],
                  param_grid$theta[extreme_idx$newcomer_cons_max_idx],
                  param_grid$sigma[extreme_idx$newcomer_cons_max_idx],
                  param_grid$beta[extreme_idx$newcomer_cons_max_idx],
                  param_grid$lambda[extreme_idx$newcomer_cons_max_idx]))
  message(sprintf("  Newcomer cons min: γ=%.2f,θ=%.2f,σ=%.3f,β=%.2f,λ=%.2f",
                  param_grid$gamma[extreme_idx$newcomer_cons_min_idx],
                  param_grid$theta[extreme_idx$newcomer_cons_min_idx],
                  param_grid$sigma[extreme_idx$newcomer_cons_min_idx],
                  param_grid$beta[extreme_idx$newcomer_cons_min_idx],
                  param_grid$lambda[extreme_idx$newcomer_cons_min_idx]))
  # quick check
  message("GDP max: ", paste(param_grid[extreme_idx$national_gdp_max_idx, ], collapse=", "))
  message("GDP min: ", paste(param_grid[extreme_idx$national_gdp_min_idx, ], collapse=", "))
  message("City income max: ", paste(param_grid[extreme_idx$city_income_max_idx, ], collapse=", "))
  message("City income min: ", paste(param_grid[extreme_idx$city_income_min_idx, ], collapse=", "))
  
  # Step 2: run optimized sweep
  sweep_result <- run_optimized_sweep(cs, cities_sets[[cs]], param_grid, rate_sequence, 
                                      pop_totals, extreme_idx)
  all_fan_data[[cs]] <- sweep_result$fan_data
}

fan_summary <- bind_rows(all_fan_data)

message("\nSweep complete. Total observations: ", nrow(fan_summary))

write_csv(fan_summary, "Outputs/fan_chart_summary.csv")

#===============================================================================
# PLOTTING
#===============================================================================

message("\n=== GENERATING FAN CHARTS ===\n")

fan_color <- "#1f77b4"

city_set_labels <- c(
  london_only = "London only",
  top_4 = "Top 4 cities by population",
  top_6 = "Top 6 cities by population",
  university_cities = "London, Oxford & Cambridge"
)

#-------------------------------------------------------------------------------
# Fan chart function
#-------------------------------------------------------------------------------
create_fan_chart <- function(data, city_set_name, y_min, y_max, y_central, 
                             ylabel, title_metric, filename_suffix) {
  
  plot_data <- data %>% filter(city_set == city_set_name)
  
  p <- ggplot(plot_data, aes(x = target_rate)) +
    # Fan (ribbon) - 60% transparent
    geom_ribbon(aes(ymin = .data[[y_min]], ymax = .data[[y_max]]), 
                fill = fan_color, alpha = 0.6) +
    # Central line - same color, opaque
    geom_line(aes(y = .data[[y_central]]), color = fan_color, linewidth = 1) +
    # Reference lines
    geom_vline(data = reference_rates, aes(xintercept = rate), 
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_text(data = reference_rates, aes(x = rate, y = Inf, label = label),
              angle = 90, hjust = 1.1, vjust = -0.3, size = 2.5, color = "gray30") +
    scale_x_continuous(
      name = "Counterfactual permitting rate",
      labels = scales::number_format(accuracy = 0.01)
    ) +
    scale_y_continuous(name = ylabel) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray40")
    ) +
    labs(
      title = sprintf("%s: %s", title_metric, city_set_labels[city_set_name]),
      subtitle = "Shaded fan: parameter uncertainty | Solid line: central estimates",
      caption = sprintf("Central: γ=%.2f, θ=%.2f, σ=%.3f, β=%.2f, λ=%.2f",
                        central_params$gamma, central_params$theta, 
                        central_params$sigma, central_params$beta, central_params$lambda)
    )
  
  ggsave(
    sprintf("Outputs/fan_%s_%s.png", filename_suffix, city_set_name),
    p, width = 24, height = 14, units = "cm", dpi = 320
  )
  
  p
}

#-------------------------------------------------------------------------------
# Generate all charts
#-------------------------------------------------------------------------------
for (cs in names(cities_sets)) {
  message(sprintf("Creating charts for: %s", cs))
  
  create_fan_chart(fan_summary, cs, "nc_min", "nc_max", "nc_central",
                   "% change in newcomer consumption",
                   "Change in newcomer consumption",
                   "newcomer_cons")
  
  create_fan_chart(fan_summary, cs, "gdp_min", "gdp_max", "gdp_central",
                   "% change in national GDP",
                   "Change in national GDP",
                   "national_gdp")
  
  create_fan_chart(fan_summary, cs, "ci_min", "ci_max", "ci_central",
                   "% change in city income (treated cities)",
                   "Change in city income",
                   "city_income")
}

message("\n=== ALL CHARTS GENERATED ===\n")

write_csv(reference_rates, "Outputs/reference_rates.csv")

message("Results saved to Outputs/")
message("=== ANALYSIS COMPLETE ===")