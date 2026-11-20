#===============================================================================
# DURANTON & PUGA (2023) - UK APPLICATION
# Simplified version: single central-estimate sweep, line charts (no fan bands)
#===============================================================================

rm(list = ls())

#_______________________________________________________________________________
# PACKAGES ####
#_______________________________________________________________________________

packages <- c("readr", "tidyverse", "httr", "jsonlite", "readxl", "ggrepel", "stargazer", "shiny", "plotly")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
invisible(lapply(packages, install_if_missing))

source("counterfactual_functions.R")

#_______________________________________________________________________________
# CONSTANTS ####
#_______________________________________________________________________________

SKIP_DATA_CONSTRUCTION <- FALSE

# Central UK parameter estimates
# See parameter_estimates.csv for min/max ranges and full sourcing
PARAMS_CENTRAL <- list(
  gamma  = 0.07,   # commuting cost elasticity w.r.t. distance
  theta  = 0.06,   # congestion elasticity w.r.t. city population
  sigma  = 0.015,  # short-run (static) agglomeration elasticity
  beta   = 0.035,  # learning/experience agglomeration elasticity
  lambda = 0.18    # land share in rural production
)

# Permit window matched to the census dates (29 April 2001, 27 March 2011, 21 March 2021).
# PS2 quarters are calendar quarters, so "census year" t runs from Q2 of t to Q1 of t+1:
# 2001-2021 uses 2001 Q2 to 2021 Q1 (20 years); 2001-2011 uses 2001 Q2 to 2011 Q1 (10 years).
PERM_FIRST_CENSUS_YEAR <- 2001
PERM_LAST_CENSUS_YEAR_01_11 <- 2010
PERM_LAST_CENSUS_YEAR_01_21 <- 2020

# How each LAD's permits are shared out among BUAs:
#   "lad_population" - in proportion to each BUA's share of the LAD's whole 2001 population
#                      (so permits per head are uniform within a LAD, including outside BUAs).
#   "bua_population" - in proportion to each BUA's share of the LAD's population living in BUAs
#                      (all of a LAD's permits are assumed to fall inside BUAs; the pre-November
#                      2026 behaviour).
PERMIT_ALLOCATION <- "lad_population"

PERCENTILE_FOR_PERM_RATE <- 0.98

PERM_RATE_CHANGE_FACTORS <- c(0.05, 0.1, 0.25, 0.3, 0.5)

#_______________________________________________________________________________
# DATA CONSTRUCTION ####
#_______________________________________________________________________________

if (SKIP_DATA_CONSTRUCTION) {
  
  message("Loading pre-existing data, skipping construction...")
  city_data <- read_csv(file.path("Data", "agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv"))
  
  urb_01_pop <- sum(city_data$bua_01_pop)
  urb_21_pop <- sum(city_data$bua_21_pop)
  rur_01_pop <- POP_ENGLAND$y2001 - urb_01_pop
  rur_21_pop <- POP_ENGLAND$y2021 - urb_21_pop
  
} else {
  
  message("Running full data construction pipeline...")
  
  # Import raw data
  lsoa_land_use_2022_raw <- read_csv(file.path("Data", "lsoa_land_use_2022.csv"))
  lsoa_pop_2001_raw <- read_csv(file.path("Data", "lsoa_pop_2001.csv"))
  lsoa_pop_2011_raw <- read_csv(file.path("Data", "lsoa_pop_2011.csv"))
  lsoa_pop_2021_raw <- read_csv(file.path("Data", "lsoa_pop_2021.csv"))
  # Census household counts (occupied dwellings) by LSOA, from Nomis: KS020 (2001), TS041 (2021)
  lsoa_hh_2001_raw <- read_csv(file.path("Data", "lsoa_households_2001_KS020.csv")) %>%
    distinct(GEOGRAPHY_CODE, .keep_all = TRUE)
  lsoa_hh_2021_raw <- read_csv(file.path("Data", "lsoa_households_2021_TS041.csv")) %>%
    distinct(GEOGRAPHY_CODE, .keep_all = TRUE)
  lsoa_lookup_2001_to_2011_raw <- read_csv(file.path("Data", "LSOA_(2001)_to_LSOA_(2011)_to_LAD_(2011)_Lookup_in_England_and_Wales.csv"))
  # Exact-fit lookup: one row per (2011 LSOA, 2021 LSOA) pair, so split 2011 LSOAs appear once
  # for each of their 2021 children. (The best-fit lookup used previously has one row per 2011
  # LSOA, which left the other children of a split with no 2011 parent; see the note below.)
  lsoa_lookup_2011_to_2021_raw <- read_csv(file.path("Data", "LSOA_(2011)_to_LSOA_(2021)_to_LAD_(2022)_Exact_Fit_Lookup_for_EW_(V3).csv"))
  lsoa_bua_lookup_2021_raw <- read_csv(file.path("Data", "LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv"))

  # See https://assets.publishing.service.gov.uk/media/68516a97dc640d9b20f3e92a/District_Matters_-_Data_dictionary.pdf
  # For Planning Application Statistics Data Dictionary
  perm_rates_raw <- read_csv(file.path("Data", "PS2_data_-_open_data_table__202409_.csv"), skip = 2,
                             locale = locale(encoding = "latin1")) %>%
    select(LPANM, LPACD, Quarter,
           `Total granted; major dwellings (all)`, `Total granted; minor dwellings (all)`)

  # Planning authorities whose codes are not 2022 LADs: districts abolished in the 2009-2021
  # reorganisations and development corporations (including LLDC and OPDC in London). Each is
  # mapped to the LAD22 that now covers it. National park authorities are not mapped.
  lpa_to_lad22 <- read_csv(file.path("Data", "lpa_to_lad22_crosswalk.csv"))

  #-----------------------------------------------------------------------------
  # Population and households on consistent 2021 LSOA boundaries
  #-----------------------------------------------------------------------------
  # Each earlier LSOA's population (and 2001 households) is shared among the later LSOAs it
  # maps to, in proportion to their later populations. Unchanged (U) and merged (M) LSOAs map one-to-one or many-to-one,
  # so this is a straight transfer. Split (S) and irregular (X) LSOAs divide their known
  # population among their children. Totals are preserved exactly at every step.
  apportion_pop <- function(lookup, from_code, to_code, from_pop, to_pop) {
    lookup %>%
      select(from = all_of(from_code), to = all_of(to_code)) %>%
      distinct() %>%
      inner_join(from_pop, by = "from") %>%
      left_join(to_pop %>% select(to, to_weight = pop), by = "to") %>%
      group_by(from) %>%
      mutate(w = to_weight / sum(to_weight)) %>%
      ungroup() %>%
      group_by(to) %>%
      summarise(across(-c(from, to_weight, w), ~ sum(.x * w)), .groups = "drop")
  }

  pop01 <- lsoa_pop_2001_raw %>%
    transmute(from = lsoa_2001_code, pop_2001 = usual_resident_pop_2001) %>%
    left_join(lsoa_hh_2001_raw %>% transmute(from = GEOGRAPHY_CODE, hh_2001 = OBS_VALUE), by = "from")
  pop11 <- lsoa_pop_2011_raw %>% transmute(LSOA11CD = lsoa_code_2011, pop = usual_resident_pop_2011)
  pop21 <- lsoa_pop_2021_raw %>% transmute(LSOA21CD = lsoa_21_code, pop = total_lsoa_pop_21)

  # Four 2011 LSOAs (in split or irregular groups in Nottingham, Hartlepool, Leeds and Greenwich)
  # have no row in the 2001-2011 lookup. Attach each to the 2001 LSOAs that feed the changed 2011
  # LSOAs in the same MSOA, so the group's 2001 population is shared among all its 2011 LSOAs.
  orphans_2011 <- lsoa_pop_2011_raw %>%
    filter(!lsoa_code_2011 %in% lsoa_lookup_2001_to_2011_raw$LSOA11CD) %>%
    transmute(LSOA11CD = lsoa_code_2011, msoa = str_sub(lsoa_name_2011, 1, -2))
  orphan_rows <- lsoa_lookup_2001_to_2011_raw %>%
    filter(CHGIND != "U") %>%
    mutate(msoa = str_sub(LSOA11NM, 1, -2)) %>%
    distinct(LSOA01CD, msoa) %>%
    inner_join(orphans_2011, by = "msoa", relationship = "many-to-many") %>%
    select(LSOA01CD, LSOA11CD)
  lookup_2001_to_2011 <- bind_rows(
    lsoa_lookup_2001_to_2011_raw %>% select(LSOA01CD, LSOA11CD),
    orphan_rows
  )

  stopifnot(all(orphans_2011$LSOA11CD %in% orphan_rows$LSOA11CD),
            all(pop01$from %in% lookup_2001_to_2011$LSOA01CD),
            all(pop11$LSOA11CD %in% lookup_2001_to_2011$LSOA11CD),
            all(pop11$LSOA11CD %in% lsoa_lookup_2011_to_2021_raw$LSOA11CD),
            all(pop21$LSOA21CD %in% lsoa_lookup_2011_to_2021_raw$LSOA21CD))

  # 2001 -> 2011 boundaries
  lsoa_pops_2001_2011 <- apportion_pop(
    lookup_2001_to_2011, "LSOA01CD", "LSOA11CD",
    pop01, pop11 %>% rename(to = LSOA11CD)
  ) %>%
    rename(LSOA11CD = to) %>%
    left_join(pop11 %>% rename(pop_2011 = pop), by = "LSOA11CD")

  # 2011 -> 2021 boundaries (carries both the 2011 count and the 2001 count already on 2011 LSOAs)
  lsoa_pops_all_years <- apportion_pop(
    lsoa_lookup_2011_to_2021_raw, "LSOA11CD", "LSOA21CD",
    lsoa_pops_2001_2011 %>% rename(from = LSOA11CD), pop21 %>% rename(to = LSOA21CD)
  ) %>%
    rename(LSOA21CD = to, pop_2001_final = pop_2001, pop_2011_final = pop_2011) %>%
    left_join(lsoa_lookup_2011_to_2021_raw %>% distinct(LSOA21CD, LAD22CD, LAD22NM), by = "LSOA21CD") %>%
    full_join(lsoa_pop_2021_raw, by = c("LSOA21CD" = "lsoa_21_code")) %>%
    left_join(lsoa_hh_2021_raw %>% transmute(LSOA21CD = GEOGRAPHY_CODE, hh_2021 = OBS_VALUE), by = "LSOA21CD") %>%
    filter(startsWith(LSOA21CD, "E"))

  stopifnot(!anyNA(lsoa_pops_all_years$LAD22CD), !anyNA(lsoa_pops_all_years$pop_2001_final),
            !anyNA(lsoa_pops_all_years$hh_2001), !anyNA(lsoa_pops_all_years$hh_2021),
            n_distinct(lsoa_pops_all_years$LSOA21CD) == nrow(lsoa_pops_all_years))

  message(sprintf(
    "England population on 2021 LSOAs: 2001 %s (census %s), 2011 %s (census %s), 2021 %s",
    format(round(sum(lsoa_pops_all_years$pop_2001_final)), big.mark = ","),
    format(sum(lsoa_pop_2001_raw$usual_resident_pop_2001[startsWith(lsoa_pop_2001_raw$lsoa_2001_code, "E")]), big.mark = ","),
    format(round(sum(lsoa_pops_all_years$pop_2011_final)), big.mark = ","),
    format(sum(lsoa_pop_2011_raw$usual_resident_pop_2011[startsWith(lsoa_pop_2011_raw$lsoa_code_2011, "E")]), big.mark = ","),
    format(sum(lsoa_pops_all_years$total_lsoa_pop_21), big.mark = ",")
  ))
  message(sprintf("England households on 2021 LSOAs: 2001 %s, 2021 %s",
                  format(round(sum(lsoa_pops_all_years$hh_2001)), big.mark = ","),
                  format(sum(lsoa_pops_all_years$hh_2021), big.mark = ",")))

  # Join land use data
  lsoa_with_lad <- lsoa_pops_all_years %>%
    left_join(lsoa_land_use_2022_raw, by = c("LSOA21CD" = "lsoa_2021_code")) %>%
    select(-lsoa_2021_name)

  valid_lad_codes <- lsoa_with_lad %>%
    distinct(LAD22CD) %>%
    pull(LAD22CD)

  #-----------------------------------------------------------------------------
  # Planning permissions by LAD22, census-to-census windows
  #-----------------------------------------------------------------------------
  lpa_map <- bind_rows(
    tibble(LPACD = valid_lad_codes, LAD22CD = valid_lad_codes, share = 1),
    lpa_to_lad22 %>% select(LPACD, LAD22CD, share)
  )

  perm_quarters <- perm_rates_raw %>%
    mutate(
      year = as.numeric(substr(Quarter, 1, 4)),
      q = as.numeric(substr(Quarter, nchar(Quarter), nchar(Quarter))),
      census_year = if_else(q >= 2, year, year - 1),
      # Major dwellings developments have >= 10 houses, so I multiply by 10 as a lower bound
      major_dwellings = suppressWarnings(as.numeric(`Total granted; major dwellings (all)`)) * 10,
      minor_dwellings = suppressWarnings(as.numeric(`Total granted; minor dwellings (all)`)),
      total_dwellings = major_dwellings + minor_dwellings
    ) %>%
    filter(census_year >= PERM_FIRST_CENSUS_YEAR, census_year <= PERM_LAST_CENSUS_YEAR_01_21)

  unmapped <- perm_quarters %>% filter(!LPACD %in% lpa_map$LPACD)
  message(sprintf("Permissions not mapped to a LAD (national parks): %s major and %s minor dwelling decisions",
                  sum(unmapped$major_dwellings / 10, na.rm = TRUE), sum(unmapped$minor_dwellings, na.rm = TRUE)))

  # A quarter suppressed in the source ("..") is filled with the authority's mean for the other
  # quarters of the same census year.
  perm_quarters <- perm_quarters %>%
    filter(LPACD %in% lpa_map$LPACD) %>%
    group_by(LPACD, census_year) %>%
    mutate(total_dwellings = if_else(is.na(total_dwellings), mean(total_dwellings, na.rm = TRUE), total_dwellings)) %>%
    ungroup()

  perm_rates_summary <- perm_quarters %>%
    inner_join(lpa_map, by = "LPACD", relationship = "many-to-many") %>%
    group_by(LAD22CD) %>%
    summarise(
      n_quarters = n_distinct(Quarter),
      sum_perms_01_11 = sum(total_dwellings[census_year <= PERM_LAST_CENSUS_YEAR_01_11] * share[census_year <= PERM_LAST_CENSUS_YEAR_01_11]),
      sum_perms_01_21 = sum(total_dwellings * share),
      .groups = "drop"
    ) %>%
    rename(LPACD = LAD22CD)

  n_expected_quarters <- 4 * (PERM_LAST_CENSUS_YEAR_01_21 - PERM_FIRST_CENSUS_YEAR + 1)
  stopifnot(all(valid_lad_codes %in% perm_rates_summary$LPACD),
            all(perm_rates_summary$n_quarters == n_expected_quarters))
  
  # LSOA to BUA mapping
  buas_to_add <- read_csv(file.path("Data", "buas_to_add_backup.csv"))
  
  lsoa_bua_lookup <- lsoa_bua_lookup_2021_raw %>%
    select(-ends_with("W"), -ObjectId) %>%
    filter(startsWith(LSOA21CD, "E")) %>%
    left_join(buas_to_add, by = "LSOA21CD") %>%
    mutate(BUA22CD = coalesce(BUA22CD.x, BUA22CD.y)) %>%
    select(-BUA22CD.x, -BUA22CD.y)
  
  # Join all LSOA data
  lsoa_full <- lsoa_with_lad %>%
    left_join(lsoa_bua_lookup %>% select(LSOA21CD, BUA22CD, BUA22NM, RGN22CD, RGN22NM), by = "LSOA21CD") %>%
    filter(BUA22CD != "E63999999") %>%
    mutate(
      BUA22NM = if_else(LSOA21CD == "E01002946", "Kingston upon Thames", BUA22NM),
      BUA22CD = if_else(LSOA21CD == "E01002946", "E63005164", BUA22CD)
    )
  
  # Distribute LAD permits to BUAs by 2001 population share (see PERMIT_ALLOCATION)
  lad_pop_01 <- lsoa_with_lad %>%
    group_by(LAD22CD) %>%
    summarise(lad_all_pop = sum(pop_2001_final), .groups = "drop")

  lad_bua_pop_shares <- lsoa_full %>%
    filter(!is.na(BUA22CD)) %>%
    group_by(LAD22CD, BUA22CD) %>%
    summarise(bua_in_lad_pop = sum(pop_2001_final, na.rm = TRUE), .groups = "drop") %>%
    left_join(lad_pop_01, by = "LAD22CD") %>%
    group_by(LAD22CD) %>%
    mutate(
      lad_total_pop = case_when(
        PERMIT_ALLOCATION == "lad_population" ~ lad_all_pop,
        PERMIT_ALLOCATION == "bua_population" ~ sum(bua_in_lad_pop)
      ),
      pop_share = bua_in_lad_pop / lad_total_pop
    ) %>%
    ungroup()
  stopifnot(!anyNA(lad_bua_pop_shares$pop_share))
  
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
  
  # Aggregate to BUA level with London consolidation
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
      bua_01_hh = sum(hh_2001),
      bua_21_hh = sum(hh_2021),
      mean_z_i = 1 / (1 - mean(pct_forest_open_land_water) / 100),
      median_z_i = 1 / (1 - median(pct_forest_open_land_water) / 100),
      min_z_i = 1 / (1 - min(pct_forest_open_land_water) / 100),
      .groups = "drop"
    ) %>%
    left_join(bua_permits_london %>% select(BUA22CD, bua_perm_rate_01_11, bua_perm_rate_01_21), by = "BUA22CD") %>%
    filter(bua_21_pop >= CITY_POP_THRESHOLD) %>%
    mutate(across(c(bua_01_pop, bua_11_pop, bua_21_pop, bua_01_hh, bua_21_hh), round),
           # Net growth in occupied dwellings per initial household: an alternative to the
           # permitting rate that counts homes directly (used as a robustness check)
           bua_hh_growth_01_21 = (bua_21_hh - bua_01_hh) / bua_01_hh) %>%
    select(-all_of(setdiff(GEOGRAPHIC_CONSTRAINT_OPTIONS, GEOGRAPHIC_CONSTRAINT_VAR))) %>%
    rename(geographic_constraint = all_of(GEOGRAPHIC_CONSTRAINT_VAR)) %>%
    arrange(desc(bua_21_pop))
  
  write_csv(city_data, file.path("Data", "agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv"))
  
  urb_01_pop <- sum(city_data$bua_01_pop)
  urb_21_pop <- sum(city_data$bua_21_pop)
  rur_01_pop <- POP_ENGLAND$y2001 - urb_01_pop
  rur_21_pop <- POP_ENGLAND$y2021 - urb_21_pop
  
  message("Data construction complete.")
}


#===============================================================================
# SINGLE CENTRAL-ESTIMATE SWEEP
#===============================================================================

message("\n=== SETTING UP CENTRAL-ESTIMATE SWEEP ===\n")

pop_totals <- list(
  tot_01 = POP_ENGLAND$y2001,
  tot_21 = POP_ENGLAND$y2021,
  urb_01 = urb_01_pop, urb_21 = urb_21_pop,
  rur_01 = rur_01_pop, rur_21 = rur_21_pop
)

# City sets
cities_sets <- list(
  london_only = city_data$BUA22NM[1],
  top_4 = city_data$BUA22NM[1:4],
  top_6 = city_data$BUA22NM[1:6],
  university_cities = c("Greater London", "Oxford", "Cambridge (Cambridge)"),
  top_10 = city_data$BUA22NM[1:10]
)

message("City sets:")
for (nm in names(cities_sets)) {
  message(sprintf("  %s: %s", nm, paste(cities_sets[[nm]], collapse = ", ")))
}

# Reference rates
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

# Austin's rate (new dwellings permitted per 2001 resident) is reported for reference only. At more
# than twice the English maximum it lies far outside the range of English rates over which
# observed growth is scaled, so it is not used as a counterfactual target or plotted.
reference_rates <- tibble(
  label = c(
    sprintf("75th percentile (%s)", city_at_75),
    sprintf("95th percentile (%s)", city_at_95),
    sprintf("Max UK permitting rate (%s)", city_at_max)
  ),
  rate = c(pct_75, pct_95, max_uk_rate)
)

message("\nReference rates:")
print(reference_rates)
message(sprintf("Austin, TX permitting rate (reference only, not plotted): %.4f", austin_rate))

# Permitting rate sweep: from just below the 75th percentile to just above the English maximum
rate_start <- floor((pct_75 - 0.01) * 100) / 100
rate_end <- ceiling((max_uk_rate + 0.01) * 100) / 100
rate_increment <- 0.001
rate_sequence <- seq(rate_start, rate_end, by = rate_increment)

message(sprintf("\nRate sweep: %.4f to %.4f by %.4f (%d values)",
                rate_start, rate_end, rate_increment, length(rate_sequence)))

#-------------------------------------------------------------------------------
# Run sweep for each city set (central params only)
#-------------------------------------------------------------------------------
all_results <- lapply(names(cities_sets), function(cs) {
  message(sprintf("\n=== Running sweep for: %s ===", cs))
  run_sweep(cs, cities_sets[[cs]], rate_sequence, pop_totals, PARAMS_CENTRAL, city_data)
})
names(all_results) <- names(cities_sets)

agg_summary <- bind_rows(map(all_results, "agg"))
city_income_summary <- bind_rows(map(all_results, "city_income"))
city_cons_summary <- bind_rows(map(all_results, "city_cons"))

message("\nSweep complete.")
message(sprintf("  Aggregate observations: %d", nrow(agg_summary)))
message(sprintf("  City income observations: %d", nrow(city_income_summary)))
message(sprintf("  City consumption observations: %d", nrow(city_cons_summary)))

write_csv(agg_summary, file.path("Outputs", "central_sweep_summary.csv"))
write_csv(city_income_summary, file.path("Outputs", "city_income_results.csv"))
write_csv(city_cons_summary, file.path("Outputs", "city_consumption_results.csv"))


#===============================================================================
# PLOTTING (LINE CHARTS, NO FAN BANDS)
#===============================================================================

message("\n=== GENERATING LINE CHARTS ===\n")

line_color <- custom_palette[1]

param_caption <- sprintf("Parameters: \u03B3=%.2f, \u03B8=%.2f, \u03C3=%.3f, \u03B2=%.3f, \u03BB=%.3f",
                         PARAMS_CENTRAL$gamma, PARAMS_CENTRAL$theta,
                         PARAMS_CENTRAL$sigma, PARAMS_CENTRAL$beta, PARAMS_CENTRAL$lambda)

#-------------------------------------------------------------------------------
# Aggregate metric line chart (newcomer cons, national income, incumbent cons)
#-------------------------------------------------------------------------------
create_line_chart <- function(data, city_set_name, y_var, ylabel, title_metric, filename_suffix) {
  
  plot_data <- data %>% filter(city_set == city_set_name)
  
  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]])) +
    geom_line(color = line_color, linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_text(data = reference_rates, aes(x = rate, y = Inf, label = label),
              angle = 90, hjust = 1.1, vjust = -0.3, size = 2.5, color = "gray30") +
    scale_x_continuous(
      name = "Counterfactual permitting rate",
      breaks = seq(0, 0.20, by = 0.02),
      labels = scales::number_format(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(name = ylabel, expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray40")
    ) +
    labs(
      title = sprintf("%s: %s", title_metric, city_set_labels[city_set_name]),
      subtitle = "Central parameter estimates",
      caption = param_caption
    )
  
  ggsave(
    file.path("Outputs", sprintf("%s_%s.png", filename_suffix, city_set_name)),
    p, width = 24, height = 14, units = "cm", dpi = 320
  )
  p
}

#-------------------------------------------------------------------------------
# City-level multi-line chart (income or consumption)
#-------------------------------------------------------------------------------
create_city_line_chart <- function(city_data, city_set_name, y_var, ylabel,
                                   title_metric, filename_suffix) {
  
  plot_data <- city_data %>% filter(city_set == city_set_name)
  plot_data$BUA22NM <- sub("Cambridge \\(Cambridge\\)", "Cambridge", plot_data$BUA22NM)

  city_order <- plot_data %>%
    distinct(BUA22NM, bua_21_pop) %>%
    arrange(desc(bua_21_pop)) %>%
    pull(BUA22NM)
  
  plot_data <- plot_data %>%
    mutate(BUA22NM = factor(BUA22NM, levels = city_order))
  
  n_cities <- length(city_order)
  colors <- custom_palette[1:n_cities]
  names(colors) <- city_order
  
  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]], color = BUA22NM)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_text(data = reference_rates, aes(x = rate, y = Inf, label = label),
              angle = 90, hjust = 1.1, vjust = -0.3, size = 2.5, color = "gray30",
              inherit.aes = FALSE) +
    scale_x_continuous(
      name = "Counterfactual permitting rate",
      breaks = seq(0, 0.20, by = 0.02),
      labels = scales::number_format(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(name = ylabel, expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = colors, name = "City") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray40"),
      legend.position = "right"
    ) +
    labs(
      title = sprintf("%s: %s", title_metric, city_set_labels[city_set_name]),
      subtitle = "Central parameter estimates",
      caption = param_caption
    )
  
  ggsave(
    file.path("Outputs", sprintf("%s_%s.png", filename_suffix, city_set_name)),
    p, width = 26, height = 14, units = "cm", dpi = 320
  )
  p
}

#-------------------------------------------------------------------------------
# All-sets overlay chart (all 5 city sets on one chart)
#-------------------------------------------------------------------------------
create_all_sets_chart <- function(data, y_var, ylabel, title_metric, filename_suffix) {

  plot_data <- data
  plot_data$city_set_label <- city_set_labels[plot_data$city_set]
  plot_data$city_set_label <- factor(plot_data$city_set_label,
                                     levels = city_set_labels)

  n_sets <- length(city_set_labels)
  colors <- custom_palette[1:n_sets]
  names(colors) <- city_set_labels

  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]],
                             color = city_set_label, group = city_set_label)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_text(data = reference_rates, aes(x = rate, y = Inf, label = label),
              angle = 90, hjust = 1.1, vjust = -0.3, size = 2.5, color = "gray30",
              inherit.aes = FALSE) +
    scale_x_continuous(
      name = "Counterfactual permitting rate",
      breaks = seq(0, 0.20, by = 0.02),
      labels = scales::number_format(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(name = ylabel, expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = colors, name = "City set") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray40"),
      legend.position = "right"
    ) +
    labs(
      title = title_metric,
      subtitle = "Central parameter estimates",
      caption = param_caption
    )

  ggsave(
    file.path("Outputs", sprintf("%s_all_sets.png", filename_suffix)),
    p, width = 26, height = 14, units = "cm", dpi = 320
  )
  p
}

#-------------------------------------------------------------------------------
# Generate all charts
#-------------------------------------------------------------------------------
for (cs in names(cities_sets)) {
  message(sprintf("Creating charts for: %s", cs))
  
  create_line_chart(agg_summary, cs, "pct_chg_newcomer_cons",
                    "% change in consumption",
                    "Change in newcomer consumption",
                    "newcomer_cons")
  
  create_line_chart(agg_summary, cs, "pct_chg_national_income_pc",
                    "% change in income per capita",
                    "Change in national income per capita",
                    "national_income_pc")
  
  # D&P incumbent-weighted consumption (only incumbents receive c_i)
  create_line_chart(agg_summary, cs, "pct_chg_cons_incumb",
                    "% change in consumption",
                    "Change in national consumption p.c. (incumbent-weighted, D&P)",
                    "national_cons_pc_incumb")
  
  # Social planner consumption (all city residents receive c_i)
  create_line_chart(agg_summary, cs, "pct_chg_cons_social",
                    "% change in consumption",
                    "Change in national consumption p.c. (population-weighted)",
                    "national_cons_pc_social")
  
  create_city_line_chart(city_income_summary, cs, "pct_chg_y",
                         "% change in income per capita",
                         "Change in city income per capita",
                         "city_income_pc")
  
  create_city_line_chart(city_cons_summary, cs, "pct_chg_c",
                         "% change in consumption",
                         "Change in incumbent consumption by city",
                         "city_incumbent_cons")
}

#-------------------------------------------------------------------------------
# Generate all-sets overlay charts
#-------------------------------------------------------------------------------
message("Creating all-sets overlay charts")

create_all_sets_chart(agg_summary, "pct_chg_newcomer_cons",
                      "% change in consumption",
                      "Change in newcomer consumption",
                      "newcomer_cons")

create_all_sets_chart(agg_summary, "pct_chg_national_income_pc",
                      "% change in income per capita",
                      "Change in national income per capita",
                      "national_income_pc")

# D&P incumbent-weighted consumption
create_all_sets_chart(agg_summary, "pct_chg_cons_incumb",
                      "% change in consumption",
                      "Change in national consumption p.c. (incumbent-weighted, D&P)",
                      "national_cons_pc_incumb")

# Social planner consumption
create_all_sets_chart(agg_summary, "pct_chg_cons_social",
                      "% change in consumption",
                      "Change in national consumption p.c. (population-weighted)",
                      "national_cons_pc_social")

message("\n=== ALL CHARTS GENERATED ===\n")

write_csv(reference_rates, file.path("Outputs", "reference_rates.csv"))

message("Results saved to Outputs/")
message("=== ANALYSIS COMPLETE ===")