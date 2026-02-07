#===============================================================================
# SHINY APP - GLOBAL STARTUP
#===============================================================================

library(shiny)
library(tidyverse)
library(readr)
library(plotly)

# Source shared computation functions
source(file.path("..", "counterfactual_functions.R"))

#_______________________________________________________________________________
# PARAMETER RANGES (from CSV) ####
#_______________________________________________________________________________

param_ranges <- read_csv(file.path("..", "parameter_estimates.csv"), show_col_types = FALSE)

get_param_range <- function(param_name) {
  row <- param_ranges %>% filter(parameter == param_name)
  list(min = row$min, central = row$central, max = row$max)
}

PARAM_GAMMA  <- get_param_range("gamma")
PARAM_THETA  <- get_param_range("theta")
PARAM_SIGMA  <- get_param_range("sigma")
PARAM_BETA   <- get_param_range("beta")
PARAM_LAMBDA <- get_param_range("lambda")

PARAMS_CENTRAL <- list(
  gamma  = PARAM_GAMMA$central,
  theta  = PARAM_THETA$central,
  sigma  = PARAM_SIGMA$central,
  beta   = PARAM_BETA$central,
  lambda = PARAM_LAMBDA$central
)

#_______________________________________________________________________________
# LOAD PRE-BUILT CITY DATA ####
#_______________________________________________________________________________

city_data <- read_csv(
  file.path("..", "Data", "agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv"),
  show_col_types = FALSE
)

urb_01_pop <- sum(city_data$bua_01_pop)
urb_21_pop <- sum(city_data$bua_21_pop)
rur_01_pop <- POP_ENGLAND$y2001 - urb_01_pop
rur_21_pop <- POP_ENGLAND$y2021 - urb_21_pop

pop_totals <- list(
  tot_01 = POP_ENGLAND$y2001,
  tot_21 = POP_ENGLAND$y2021,
  urb_01 = urb_01_pop, urb_21 = urb_21_pop,
  rur_01 = rur_01_pop, rur_21 = rur_21_pop
)

#_______________________________________________________________________________
# CITY SETS ####
#_______________________________________________________________________________

cities_sets <- list(
  london_only = city_data$BUA22NM[1],
  top_4 = city_data$BUA22NM[1:4],
  top_6 = city_data$BUA22NM[1:6],
  university_cities = c("Greater London", "Oxford", "Cambridge (Cambridge)"),
  top_10 = city_data$BUA22NM[1:10]
)

#_______________________________________________________________________________
# REFERENCE RATES ####
#_______________________________________________________________________________

austin_rate <- calculate_austin_perm_rate(file.path("..", AUSTIN_DATA_FILE))
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

#_______________________________________________________________________________
# RATE SEQUENCE ####
#_______________________________________________________________________________

rate_start <- floor(pct_75 * 100) / 100
rate_end <- 0.13
rate_increment <- 0.001
rate_sequence <- seq(rate_start, rate_end, by = rate_increment)
