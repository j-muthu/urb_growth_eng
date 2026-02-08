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
  gamma  = 0.05,   # commuting cost elasticity w.r.t. distance
  theta  = 0.06,   # congestion elasticity w.r.t. city population
  sigma  = 0.025,  # short-run agglomeration elasticity
  beta   = 0.015,  # learning/experience agglomeration elasticity
  lambda = 0.12    # land share in rural production
)

PERM_DATA_START_ROW <- 33128
PERM_DATA_END_YEAR <- 2021

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
  lsoa_lookup_2001_to_2011_raw <- read_csv(file.path("Data", "LSOA_(2001)_to_LSOA_(2011)_to_LAD_(2011)_Lookup_in_England_and_Wales.csv"))
  lsoa_lookup_2011_to_2021_raw <- read_csv(file.path("Data", "LSOA_(2011)_to_LSOA_(2021)_to_LAD_(2022)_Best_Fit_Lookup_for_EW_(V2).csv"))
  lsoa_bua_lookup_2021_raw <- read_csv(file.path("Data", "LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv"))
  
  perm_rates_raw <- read_csv(file.path("Data", "PS2_data_-_open_data_table__202409_.csv"), skip = 2) %>%
    select(2:4, 34, 112) %>%
    slice(PERM_DATA_START_ROW:n())
  
  # 2001-2011 LSOA merging
  lsoa_pops_2001_2011 <- lsoa_pop_2001_raw %>%
    full_join(lsoa_lookup_2001_to_2011_raw, by = c("lsoa_2001_code" = "LSOA01CD")) %>%
    select(2:7) %>%
    group_by(LSOA11CD) %>%
    summarize(
      pop_2001_lsoa_2011_nosplit = sum(usual_resident_pop_2001),
      LSOA11NM = first(LSOA11NM),
      CHGIND = first(CHGIND),
      .groups = "drop"
    ) %>%
    left_join(lsoa_pop_2011_raw, by = c("LSOA11CD" = "lsoa_code_2011")) %>%
    select(-lsoa_name_2011) %>%
    mutate(
      pop_2001_imputed = if_else(
        CHGIND == "S",
        usual_resident_pop_2011 / POP_GROWTH_FACTORS$ew_2001_to_2011,
        pop_2001_lsoa_2011_nosplit
      )
    ) %>%
    select(-pop_2001_lsoa_2011_nosplit)
  
  missing_2011_lsoas <- lsoa_pop_2011_raw %>%
    anti_join(lsoa_pops_2001_2011, by = c("lsoa_code_2011" = "LSOA11CD")) %>%
    transmute(
      LSOA11CD = lsoa_code_2011,
      LSOA11NM = lsoa_name_2011,
      CHGIND = "N",
      usual_resident_pop_2011 = usual_resident_pop_2011,
      pop_2001_imputed = usual_resident_pop_2011 / POP_GROWTH_FACTORS$ew_2001_to_2011
    )
  
  lsoa_pops_2001_2011 <- bind_rows(lsoa_pops_2001_2011, missing_2011_lsoas)
  
  # Diagnostic
  split_diagnostic <- lsoa_pops_2001_2011 %>%
    summarise(
      n_split = sum(CHGIND == "S", na.rm = TRUE),
      n_merged = sum(CHGIND == "M", na.rm = TRUE),
      n_unchanged = sum(CHGIND == "U", na.rm = TRUE),
      n_new = sum(CHGIND == "N", na.rm = TRUE),
      n_total = n(),
      pop_2011_split = sum(usual_resident_pop_2011[CHGIND == "S"], na.rm = TRUE),
      pop_2011_total = sum(usual_resident_pop_2011, na.rm = TRUE)
    )
  
  message(sprintf("LSOAs with imputed 2001 pop: %.1f%% of 2011 total",
                  100 * split_diagnostic$pop_2011_split / split_diagnostic$pop_2011_total))
  
  # 2011-2021 LSOA merging
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
  
  # Join land use data
  lsoa_with_land_use <- lsoa_pops_all_years %>%
    left_join(lsoa_land_use_2022_raw, by = c("LSOA21CD" = "lsoa_2021_code")) %>%
    select(-lsoa_2021_name)
  
  # Fix missing LAD codes
  lads_to_add <- read_csv(file.path("Data", "lads_to_add_backup.csv"))
  
  lsoa_with_lad <- lsoa_with_land_use %>%
    left_join(lads_to_add, by = "LSOA21CD") %>%
    mutate(LAD22CD = coalesce(LAD22CD.x, LAD22CD.y)) %>%
    select(-LAD22CD.x, -LAD22CD.y)
  
  valid_lad_codes <- lsoa_with_lad %>%
    filter(!is.na(LAD22CD)) %>%
    distinct(LAD22CD) %>%
    pull(LAD22CD)
  
  # Process permitting data
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
  
  # Distribute LAD permits to BUAs by population share
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

# Permitting rate sweep
rate_start <- floor(pct_75 * 100) / 100
rate_end <- 0.13
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

param_caption <- sprintf("Parameters: \u03B3=%.2f, \u03B8=%.2f, \u03C3=%.3f, \u03B2=%.3f, \u03BB=%.2f",
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
      labels = scales::number_format(accuracy = 0.01)
    ) +
    scale_y_continuous(name = ylabel) +
    scale_color_manual(values = colors, name = "City") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
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
  
  create_line_chart(agg_summary, cs, "pct_chg_incumbent_cons",
                    "% change in consumption",
                    "Change in incumbent consumption (aggregate)",
                    "incumbent_cons_agg")
  
  create_city_line_chart(city_income_summary, cs, "pct_chg_y",
                         "% change in income per capita",
                         "Change in city income per capita",
                         "city_income_pc")
  
  create_city_line_chart(city_cons_summary, cs, "pct_chg_c",
                         "% change in consumption",
                         "Change in incumbent consumption by city",
                         "city_incumbent_cons")
}

message("\n=== ALL CHARTS GENERATED ===\n")

write_csv(reference_rates, file.path("Outputs", "reference_rates.csv"))

message("Results saved to Outputs/")
message("=== ANALYSIS COMPLETE ===")