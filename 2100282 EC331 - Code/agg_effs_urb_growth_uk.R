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
  ew_2001_to_2011_alt = 1.07278362492818,  # slightly different calc for some imputes
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

# Permitting rate change factors for sensitivity analysis
PERM_RATE_CHANGE_FACTORS <- c(0.05, 0.1, 0.25, 0.3, 0.5)

# R&D employment shares by region
# Source: ERC Insight report p.11
# https://www.enterpriseresearch.ac.uk/wp-content/uploads/2021/09/ERC-Insight-The-UK%E2%80%99s-business-RD-workforce-Belt.Ri_.Akinremi.pdf
RD_LABOUR_SHARES <- list(
  "North East" = 0.0052,
  "North West" = 0.0060,
  "Yorkshire and The Humber" = 0.0056,
  "East Midlands" = 0.0088,
  "West Midlands" = 0.0097,
  "East of England" = 0.0143,
  "London" = 0.0069,
  "South East" = 0.013,
  "South West" = 0.0083
)

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

# Alternative parameter combinations for robustness (from DP Table IV)
ALT_PARAMS <- tibble(
  sigma = c(0.02, 0.03, 0.04, 0.02, 0.03, 0.02, 0.03, 0.04),
  beta  = c(0.02, 0.03, 0.04, 0.02, 0.03, 0.02, 0.03, 0.04),
  gamma = c(0.06, 0.06, 0.06, 0.07, 0.07, 0.08, 0.08, 0.08),
  theta = c(0.03, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05, 0.05)
)


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
      pop_2001_imputed = usual_resident_pop_2011 / POP_GROWTH_FACTORS$ew_2001_to_2011_alt
    )
  
  lsoa_pops_2001_2011 <- bind_rows(lsoa_pops_2001_2011, missing_2011_lsoas)
  
  #-----------------------------------------------------------------------------
  # 2011-2021 LSOA merging (chained)
  #-----------------------------------------------------------------------------
  
  lsoa_pops_all_years <- lsoa_pops_2001_2011 %>%
    # Join to 2011-2021 lookup
    full_join(
      lsoa_lookup_2011_to_2021_raw %>% select(2:7),
      by = "LSOA11CD"
    ) %>%
    # Aggregate to 2021 LSOA level
    group_by(LSOA21CD) %>%
    summarize(
      pop_2011_final = sum(usual_resident_pop_2011, na.rm = TRUE),
      pop_2001_final = sum(pop_2001_imputed, na.rm = TRUE),
      LSOA21NM = first(LSOA21NM),
      LAD22CD = first(LAD22CD),
      LAD22NM = first(LAD22NM),
      .groups = "drop"
    ) %>%
    # Join 2021 population
    full_join(lsoa_pop_2021_raw, by = c("LSOA21CD" = "lsoa_21_code")) %>%
    select(-LSOA21NM) %>%
    # Impute missing 2001/2011 values for new 2021 LSOAs
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
    # Keep only English LSOAs
    filter(startsWith(LSOA21CD, "E"))
  
  #-----------------------------------------------------------------------------
  # Join land use data
  #-----------------------------------------------------------------------------
  
  lsoa_with_land_use <- lsoa_pops_all_years %>%
    left_join(lsoa_land_use_2022_raw, by = c("LSOA21CD" = "lsoa_2021_code")) %>%
    select(-lsoa_2021_name)
  
  #-----------------------------------------------------------------------------
  # Fix missing LAD codes (load from pre-computed API results)
  #-----------------------------------------------------------------------------
  
  lads_to_add <- read_csv("Data/lads_to_add_backup.csv")
  
  lsoa_with_lad <- lsoa_with_land_use %>%
    left_join(lads_to_add, by = "LSOA21CD") %>%
    mutate(LAD22CD = coalesce(LAD22CD.x, LAD22CD.y)) %>%
    select(-LAD22CD.x, -LAD22CD.y)
  
  # Get list of valid LAD codes
  valid_lad_codes <- lsoa_with_lad %>%
    filter(!is.na(LAD22CD)) %>%
    distinct(LAD22CD) %>%
    pull(LAD22CD)
  
  #-----------------------------------------------------------------------------
  # Process permitting data (chained)
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
  
  # Function to impute missing permit values via linear regression
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
  
  # Identify LPACDs that can't be imputed (all missing or only 1 obs)
  lpacd_cant_impute <- perm_rates_by_year %>%
    group_by(LPACD) %>%
    summarise(n_nonmiss = sum(!is.na(total_dwellings)), .groups = "drop") %>%
    filter(n_nonmiss <= 1) %>%
    pull(LPACD)
  
  # Apply linear imputation
  perm_rates_imputed <- perm_rates_by_year %>%
    group_by(LPACD, LPANM) %>%
    group_modify(~impute_permits_lm(.x)) %>%
    ungroup()
  
  # For LPACDs that couldn't be imputed, use median of bordering LADs
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
  
  # Combine and aggregate to summary
  perm_rates_final <- perm_rates_imputed %>%
    filter(!LPACD %in% lpacd_cant_impute) %>%
    select(LPACD, LPANM, year, total_dwellings_final) %>%
    bind_rows(bordering_imputed) %>%
    # Handle any duplicates by summing
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
  # LSOA to BUA mapping (chained)
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
    # Remove invalid BUA code
    filter(BUA22CD != "E63999999") %>%
    # Fix Kingston upon Thames coding error
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
  
  # Aggregate permits with London consolidation
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
  
  # Aggregate LSOA data to BUA level with London consolidation
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
      # Geographic constraints: z_i = 1/(1 - fraction constrained)
      mean_z_i = 1 / (1 - mean(pct_forest_open_land_water) / 100),
      median_z_i = 1 / (1 - median(pct_forest_open_land_water) / 100),
      min_z_i = 1 / (1 - min(pct_forest_open_land_water) / 100),
      .groups = "drop"
    ) %>%
    # Join permit data
    left_join(bua_permits_london %>% select(BUA22CD, bua_perm_rate_01_11, bua_perm_rate_01_21), by = "BUA22CD") %>%
    # Apply population threshold
    filter(bua_21_pop >= CITY_POP_THRESHOLD) %>%
    # Round populations
    mutate(across(c(bua_01_pop, bua_11_pop, bua_21_pop), round)) %>%
    # Add R&D labour shares
    mutate(l_i = recode(RGN22NM, !!!RD_LABOUR_SHARES)) %>%
    # Keep only selected geographic constraint
    select(-all_of(setdiff(GEOGRAPHIC_CONSTRAINT_OPTIONS, GEOGRAPHIC_CONSTRAINT_VAR))) %>%
    rename(geographic_constraint = all_of(GEOGRAPHIC_CONSTRAINT_VAR)) %>%
    arrange(desc(bua_21_pop))
  
  # Save intermediate data
  write_csv(city_data, "Data/agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv")
  
  # Compute derived population values
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
        (geographic_constraint^gamma / (1 - l_i)),
      eq_rho_A_h_coeff_01 = eq_rho_A_h_coeff * bua_01_pop^(gamma + theta - sigma - beta),
      eq_rho_A_h_coeff_21 = eq_rho_A_h_coeff * bua_21_pop^(gamma + theta - sigma - beta),
      
      # Income / tau (eq 8)
      income_div_tau_01 = eq_rho_A_h_coeff_01 * (1 - l_i) * bua_01_pop^(sigma + beta),
      income_div_tau_21 = eq_rho_A_h_coeff_21 * (1 - l_i) * bua_21_pop^(sigma + beta),
      
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
  # perm_rate_cf is a vector of counterfactual permitting rates (same length as cf_prep$data)
  
  cf_prep$data %>%
    mutate(
      perm_rate_counterfact = perm_rate_cf,
      # DP's formula: N_cf = (perm_rate_cf / perm_rate_actual) * (N_21 - N_01) + N_01
      bua_21_pop_counterfact = if_else(
        in_counterfact == 1,
        (perm_rate_counterfact / bua_perm_rate_01_21) * (bua_21_pop - bua_01_pop) + bua_01_pop,
        as.numeric(bua_21_pop)
      )
    )
}


#-------------------------------------------------------------------------------
# Find marginal city (the while loop extracted)
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
    # Compute counterfactual income and consumption
    temp_data <- cf_data %>%
      mutate(
        pop_21_cf = bua_21_pop_counterfact,
        y_21_cf = (y_21 / bua_21_pop^(sigma + beta)) * pop_21_cf^(sigma + beta),
        c_21_cf = y_21_cf - (1 / (gamma + 1)) * tau_21 * geographic_constraint^gamma * pop_21_cf^(gamma + theta)
      ) %>%
      arrange(desc(c_21_cf)) %>%
      mutate(city_order = row_number())
    
    # Cumulative urban population
    pop_cum <- sum(temp_data$pop_21_cf)
    
    temp_data <- temp_data %>%
      mutate(
        pop_21_cf = if_else(pop_cum > pop_totals$tot_21 | city_order > marginal_city_index, NA_real_, pop_21_cf),
        c_21_rural_threshold = rur_21_prod * (pop_totals$tot_21 - pop_cum)^(-params$lambda)
      )
    
    # Check if marginal city is still viable
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
  
  # Final computation with correct marginal city
  final_data <- cf_data %>%
    mutate(
      pop_21_cf = bua_21_pop_counterfact,
      y_21_cf = (y_21 / bua_21_pop^(sigma + beta)) * pop_21_cf^(sigma + beta),
      c_21_cf = y_21_cf - (1 / (gamma + 1)) * tau_21 * geographic_constraint^gamma * pop_21_cf^(gamma + theta)
    ) %>%
    arrange(desc(c_21_cf)) %>%
    mutate(city_order = row_number()) %>%
    mutate(
      pop_21_cf = if_else(city_order > marginal_city_index, NA_real_, pop_21_cf),
      y_21_cf = if_else(is.na(pop_21_cf), NA_real_, y_21_cf),
      c_21_cf = if_else(is.na(pop_21_cf), NA_real_, c_21_cf)
    )
  
  pop_cum_final <- sum(final_data$pop_21_cf, na.rm = TRUE)
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
  
  # Baseline (actual) values
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
# Run single counterfactual scenario
#-------------------------------------------------------------------------------
run_single_counterfactual <- function(cf_prep, perm_rate_cf, factor_value, factor_change = TRUE) {
  # Compute counterfactual populations
  cf_pop_data <- compute_cf_populations(cf_prep, perm_rate_cf)
  
  # Find marginal city
  marg_result <- find_marginal_city(cf_pop_data, cf_prep)
  
  # Compute welfare
  welfare <- compute_welfare_metrics(marg_result, cf_prep)
  
  # Add city-level percentage changes
  final_data <- marg_result$data %>%
    mutate(
      pct_chg_y = 100 * (y_21_cf - y_21) / y_21,
      pct_chg_c = 100 * (c_21_cf - c_21) / c_21,
      pct_chg_perm_rate = 100 * (perm_rate_counterfact - bua_perm_rate_01_21) / bua_perm_rate_01_21
    )
  
  list(
    data = final_data,
    welfare = welfare,
    params = cf_prep$params,
    factor_value = factor_value,
    factor_change = factor_change,
    rur_21_income = cf_prep$rur_21_income
  )
}


#-------------------------------------------------------------------------------
# Main counterfactual function (orchestrates multiple scenarios)
#-------------------------------------------------------------------------------
run_counterfactuals <- function(
    city_data,
    cities_in_cf,
    params,
    pop_totals,
    perm_rate_factors = NULL,
    use_percentile = FALSE,
    percentile = PERCENTILE_FOR_PERM_RATE,
    store_results = FALSE
) {
  # Prepare base data
  cf_prep <- prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals)
  
  results_list <- list()
  
  if (use_percentile) {
    # Single scenario: raise to percentile
    pct_value <- quantile(cf_prep$data$bua_perm_rate_01_21, percentile)
    message(sprintf("Permitting rate at %.0f%% percentile: %.6f", percentile * 100, pct_value))
    
    perm_rate_cf <- cf_prep$data %>%
      mutate(
        rate = if_else(
          in_counterfact == 1,
          pmax(bua_perm_rate_01_21, pct_value),
          bua_perm_rate_01_21
        )
      ) %>%
      pull(rate)
    
    result <- run_single_counterfactual(cf_prep, perm_rate_cf, pct_value, factor_change = FALSE)
    results_list[["percentile"]] <- result
    
    message(sprintf("Percentile %.0f%% scenario: Δy=%.2f%%, Δc=%.2f%%",
                    percentile * 100, result$welfare$pct_chg_y_tot, result$welfare$pct_chg_c_tot))
    
  } else if (!is.null(perm_rate_factors)) {
    # Multiple scenarios: multiplicative factors
    for (factor in perm_rate_factors) {
      perm_rate_cf <- cf_prep$data %>%
        mutate(
          rate = if_else(
            in_counterfact == 1,
            bua_perm_rate_01_21 * (1 + factor),
            bua_perm_rate_01_21
          )
        ) %>%
        pull(rate)
      
      result <- run_single_counterfactual(cf_prep, perm_rate_cf, factor, factor_change = TRUE)
      results_list[[as.character(factor)]] <- result
      
      message(sprintf("Factor %.0f%% scenario: Δy=%.2f%%, Δc=%.2f%%",
                      factor * 100, result$welfare$pct_chg_y_tot, result$welfare$pct_chg_c_tot))
    }
  }
  
  # Optionally save
  if (store_results) {
    save_counterfactual_results(results_list, cities_in_cf, params)
  }
  
  results_list
}


#-------------------------------------------------------------------------------
# Save results helper
#-------------------------------------------------------------------------------
save_counterfactual_results <- function(results_list, cities_in_cf, params) {
  for (name in names(results_list)) {
    result <- results_list[[name]]
    
    # Create presentable table
    tbl <- result$data %>%
      filter(in_counterfact == 1) %>%
      select(BUA22NM, pct_chg_perm_rate, bua_21_pop, bua_21_pop_counterfact, pct_chg_y, pct_chg_c) %>%
      mutate(across(starts_with("pct_"), ~ . / 100)) %>%
      mutate(pct_chg_c_newcomers = result$welfare$pct_chg_c_rur / 100)
    
    # Add rural row
    tbl <- bind_rows(tbl, tibble(
      BUA22NM = "Rural areas",
      pct_chg_perm_rate = NA,
      bua_21_pop = result$welfare$pop_21_rur_counterfact,
      bua_21_pop_counterfact = result$welfare$pop_21_rur_counterfact,
      pct_chg_y = result$welfare$pct_chg_c_rur / 100,
      pct_chg_c = NA,
      pct_chg_c_newcomers = NA
    ))
    
    # Save
    filename <- sprintf(
      "Outputs/tbl_%s_cf_%d_cities_sb%.0f_gt%.0f_l%.0f.csv",
      if (result$factor_change) paste0("fact_", result$factor_value) else "q98",
      length(cities_in_cf),
      (params$sigma + params$beta) * 100,
      (params$gamma + params$theta) * 100,
      params$lambda * 100
    )
    
    write_csv(tbl, filename)
  }
}


#===============================================================================
# RUN ANALYSIS
#===============================================================================

# Population totals (computed during data construction or loading)
pop_totals <- list(
  tot_01 = POP_ENGLAND$y2001,
  tot_21 = POP_ENGLAND$y2021,
  urb_01 = urb_01_pop,
  urb_21 = urb_21_pop,
  rur_01 = rur_01_pop,
  rur_21 = rur_21_pop
)

# Default parameters
params_default <- list(
  gamma = PARAMS_USA$gamma,
  theta = PARAMS_USA$theta,
  sigma = PARAMS_USA$sigma,
  beta = PARAMS_USA$beta,
  lambda = PARAMS_USA$lambda
)

# Cities to evaluate
cities_sets <- list(
  top_10 = city_data$BUA22NM[1:10],
  top_5 = city_data$BUA22NM[1:5],
  top_2 = city_data$BUA22NM[1:2],
  london_only = city_data$BUA22NM[1],
  oxbridge = c("Greater London", "Oxford", "Cambridge (Cambridge)")
)

#-------------------------------------------------------------------------------
# Main analysis with default parameters
#-------------------------------------------------------------------------------

message("\n=== Running main counterfactual with default parameters ===\n")

# Prepare base data (save for plotting)
cf_prep_main <- prepare_counterfactual_data(city_data, cities_sets$top_10, params_default, pop_totals)
write_csv(cf_prep_main$data, "Outputs/dp_original_params_counterfactual_data.csv")

# Run with percentile approach
results_main <- run_counterfactuals(
  city_data = city_data,
  cities_in_cf = cities_sets$top_10,
  params = params_default,
  pop_totals = pop_totals,
  use_percentile = TRUE,
  store_results = TRUE
)

# save metadata
write_csv(
  tibble(
    percentile_used = PERCENTILE_FOR_PERM_RATE,
    perm_rate_at_percentile = results_main$percentile$factor_value
  ),
  "Outputs/counterfactual_metadata.csv"
)

#-------------------------------------------------------------------------------
# Vary cities
#-------------------------------------------------------------------------------

message("\n=== Varying cities ===\n")

results_by_cities <- map(cities_sets, function(cities) {
  run_counterfactuals(
    city_data = city_data,
    cities_in_cf = cities,
    params = params_default,
    pop_totals = pop_totals,
    use_percentile = TRUE,
    store_results = TRUE
  )
})

#-------------------------------------------------------------------------------
# Vary factors
#-------------------------------------------------------------------------------

message("\n=== Varying permitting rate factors ===\n")

results_by_factor <- run_counterfactuals(
  city_data = city_data,
  cities_in_cf = cities_sets$top_10,
  params = params_default,
  pop_totals = pop_totals,
  perm_rate_factors = PERM_RATE_CHANGE_FACTORS,
  store_results = TRUE
)

#-------------------------------------------------------------------------------
# Vary parameters
#-------------------------------------------------------------------------------

message("\n=== Varying model parameters ===\n")

results_by_params <- map(1:nrow(ALT_PARAMS), function(i) {
  alt_params <- list(
    gamma = ALT_PARAMS$gamma[i],
    theta = ALT_PARAMS$theta[i],
    sigma = ALT_PARAMS$sigma[i],
    beta = ALT_PARAMS$beta[i],
    lambda = PARAMS_USA$lambda
  )
  
  run_counterfactuals(
    city_data = city_data,
    cities_in_cf = cities_sets$top_10,
    params = alt_params,
    pop_totals = pop_totals,
    use_percentile = TRUE,
    store_results = TRUE
  )
})


#===============================================================================
# PLOTS AND REGRESSIONS
#===============================================================================

message("\n=== Generating plots and regressions ===\n")

plot_data <- cf_prep_main$data

top_10_buas <- plot_data %>%
  arrange(desc(bua_21_pop)) %>%
  slice_head(n = 10)

# Plot 1: Permitting rate vs city population
perm_rate_vs_city_pop <- ggplot(plot_data, aes(x = bua_21_pop, y = 1/bua_perm_rate_01_21)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text_repel(
    data = top_10_buas,
    aes(label = BUA22NM),
    size = 3.5, box.padding = 0.5, point.padding = 0.2, force = 10
  ) +
  labs(
    x = "2021 population (log scale)",
    y = "Reciprocal of permitting rate\n(2001-2021)"
  ) +
  theme_minimal() +
  scale_x_log10()

# Plot 2: Geographic constraint vs permitting rate
geog_constr_vs_perm_rate <- ggplot(plot_data, aes(x = 1 - 1/geographic_constraint, y = 1/bua_perm_rate_01_21)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text_repel(
    data = top_10_buas,
    aes(label = BUA22NM),
    size = 3.5, box.padding = 0.5, point.padding = 0.2, force = 10
  ) +
  labs(
    x = "Estimated % geographically-constrained land (log scale)",
    y = "Reciprocal of permitting rate\n(2001-2021)"
  ) +
  theme_minimal() +
  scale_x_log10(labels = scales::percent)

ggsave("Outputs/perm_rate_vs_city_pop.png", perm_rate_vs_city_pop, units = "cm", width = 16, height = 12, dpi = 320)
ggsave("Outputs/geog_constr_vs_perm_rate.png", geog_constr_vs_perm_rate, units = "cm", width = 16, height = 12, dpi = 320)

# Regressions
reg1 <- lm(I(1/bua_perm_rate_01_21) ~ log(bua_21_pop), data = plot_data)
reg2 <- lm(I(1/bua_perm_rate_01_21) ~ log(1 - 1/geographic_constraint), data = plot_data)

stargazer(
  reg1, reg2,
  title = "Empirically testing model predictions",
  dep.var.labels = "Reciprocal of permitting rate (2001-2021)",
  covariate.labels = c("Log population", "Log \\% of geographically constrained land", "Constant"),
  align = TRUE,
  model.numbers = TRUE,
  label = "tab:reg_tab",
  type = "latex",
  out = "Outputs/regression_results.tex"
)

message("\n=== Analysis complete ===\n")