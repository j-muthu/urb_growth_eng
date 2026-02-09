#===============================================================================
# COUNTERFACTUAL FUNCTIONS MODULE
# Extracted from agg_effs_urb_growth_uk.R for shared use by Shiny app
#===============================================================================

#_______________________________________________________________________________
# CONSTANTS ####
#_______________________________________________________________________________

CITY_POP_THRESHOLD <- 20000

GEOGRAPHIC_CONSTRAINT_OPTIONS <- c("mean_z_i", "median_z_i", "min_z_i")
GEOGRAPHIC_CONSTRAINT_VAR <- "mean_z_i"

POP_ENGLAND <- list(y2001 = 49449700, y2021 = 56554900)

POP_GROWTH_FACTORS <- list(
  ew_2001_to_2011 = 1.07396242237523,
  ew_2011_to_2021 = 1.06492008640114,
  ew_2001_to_2021 = 1.14368415562741
)

RGDP_PER_CAPITA <- list(y2001 = 30316, y2021 = 36465)

AUSTIN_DATA_FILE <- file.path("Data", "Issued_Construction_Permits_20250927.csv")
AUSTIN_2001_POP <- 669693

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

BORDERING_LADS <- list(
  E06000061 = c("E06000017", "E07000141", "E06000031", "E07000011",
                "E06000055", "E06000042", "E06000062", "E07000131"),
  E06000062 = c("E07000131", "E06000061", "E06000042", "E06000060",
                "E07000177", "E07000221", "E07000220")
)

custom_palette <- c(
  '#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231',
  '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4',
  '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000',
  '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9'
)

city_set_labels <- c(
  london_only = "London only",
  top_4 = "4 largest cities (London, Birmingham, Leeds, Liverpool)",
  top_6 = "6 largest cities (London - Manchester)",
  university_cities = "University cities (London, Oxford, Cambridge)",
  top_10 = "10 largest cities (London - Bradford)"
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

  total_permits <- readr::read_csv(filepath, show_col_types = FALSE) %>%
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

  total_permits / AUSTIN_2001_POP
}

#_______________________________________________________________________________
# COUNTERFACTUAL ANALYSIS FUNCTIONS ####
#_______________________________________________________________________________

prepare_counterfactual_data <- function(city_data, cities_in_cf, params, pop_totals) {
  gamma <- params$gamma
  theta <- params$theta
  sigma <- params$sigma
  beta  <- params$beta
  lambda <- params$lambda

  cf_data <- city_data %>%
    mutate(
      eq_rho_A_h_coeff = (gamma + theta) / ((sigma + beta) * (gamma + 1)) *
        geographic_constraint^gamma,
      eq_rho_A_h_coeff_01 = eq_rho_A_h_coeff * bua_01_pop^(gamma + theta - sigma - beta),
      eq_rho_A_h_coeff_21 = eq_rho_A_h_coeff * bua_21_pop^(gamma + theta - sigma - beta),
      income_div_tau_01 = eq_rho_A_h_coeff_01 * bua_01_pop^(sigma + beta),
      income_div_tau_21 = eq_rho_A_h_coeff_21 * bua_21_pop^(sigma + beta),
      cons_coeff = (gamma + theta - sigma - beta) / ((sigma + beta) * (gamma + 1)) *
        geographic_constraint^gamma,
      cons_div_tau_01 = cons_coeff * bua_01_pop^(gamma + theta),
      cons_div_tau_21 = cons_coeff * bua_21_pop^(gamma + theta)
    ) %>%
    select(-eq_rho_A_h_coeff, -cons_coeff)

  rgdp_multiple <- RGDP_PER_CAPITA$y2021 / RGDP_PER_CAPITA$y2001

  sum_income_pc_01_div_tau <- sum(cf_data$income_div_tau_01 * cf_data$bua_01_pop) / pop_totals$urb_01
  sum_income_pc_21_div_tau <- sum(cf_data$income_div_tau_21 * cf_data$bua_21_pop) / pop_totals$urb_21

  tau_01 <- 1
  tau_21 <- rgdp_multiple / (sum_income_pc_21_div_tau / sum_income_pc_01_div_tau)

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

  rur_01_income <- min(cf_data$c_01)
  rur_21_income <- min(cf_data$c_21)

  rur_01_prod <- rur_01_income / (pop_totals$rur_01^(-lambda))
  rur_21_prod <- rur_21_income / (pop_totals$rur_21^(-lambda))

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
    tau_01 = tau_01, tau_21 = tau_21,
    rur_01_income = rur_01_income, rur_21_income = rur_21_income,
    rur_01_prod = rur_01_prod, rur_21_prod = rur_21_prod,
    rur_21_pop_incumb = rur_21_pop_incumb,
    params = params, pop_totals = pop_totals
  )
}

compute_cf_populations <- function(cf_prep, perm_rate_cf) {
  d <- cf_prep$data
  d$perm_rate_counterfact <- perm_rate_cf
  d$bua_21_pop_counterfact <- d$bua_21_pop
  mask <- d$in_counterfact == 1L
  d$bua_21_pop_counterfact[mask] <-
    (perm_rate_cf[mask] / d$bua_perm_rate_01_21[mask]) *
    (d$bua_21_pop[mask] - d$bua_01_pop[mask]) + d$bua_01_pop[mask]
  d
}

find_marginal_city <- function(cf_data, cf_prep) {
  params <- cf_prep$params
  pop_totals <- cf_prep$pop_totals
  tau_21 <- cf_prep$tau_21
  rur_21_prod <- cf_prep$rur_21_prod

  gamma <- params$gamma; theta <- params$theta
  sigma <- params$sigma; beta <- params$beta

  sorted_data <- cf_data
  sorted_data$pop_21_cf <- sorted_data$bua_21_pop_counterfact
  sorted_data$y_21_cf <- (sorted_data$y_21 / sorted_data$bua_21_pop^(sigma + beta)) *
    sorted_data$pop_21_cf^(sigma + beta)
  sorted_data$c_21_cf <- sorted_data$y_21_cf - (1 / (gamma + 1)) * tau_21 *
    sorted_data$geographic_constraint^gamma * sorted_data$pop_21_cf^(gamma + theta)

  ord <- order(sorted_data$c_21_cf, decreasing = TRUE)
  sorted_data <- sorted_data[ord, ]
  sorted_data$city_order <- seq_len(nrow(sorted_data))
  sorted_data$cumpop <- cumsum(sorted_data$pop_21_cf)

  marginal_city_index <- nrow(sorted_data)

  while (marginal_city_index >= 1) {
    if (sorted_data$cumpop[marginal_city_index] > pop_totals$tot_21) {
      marginal_city_index <- marginal_city_index - 1
      next
    }

    urban_pop <- sorted_data$cumpop[marginal_city_index]
    rural_pop <- pop_totals$tot_21 - urban_pop
    c_21_rural_threshold <- rur_21_prod * rural_pop^(-params$lambda)

    if (c_21_rural_threshold > sorted_data$c_21_cf[marginal_city_index]) {
      marginal_city_index <- marginal_city_index - 1
    } else {
      break
    }
  }

  pop_cum_final <- if (marginal_city_index >= 1) {
    sorted_data$cumpop[marginal_city_index]
  } else {
    0
  }

  c_21_rural_threshold <- rur_21_prod * (pop_totals$tot_21 - pop_cum_final)^(-params$lambda)

  excluded <- sorted_data$city_order > marginal_city_index
  sorted_data$pop_21_cf[excluded] <- NA_real_
  sorted_data$y_21_cf[excluded] <- NA_real_
  sorted_data$c_21_cf[excluded] <- NA_real_
  sorted_data$c_21_rural_threshold <- c_21_rural_threshold
  sorted_data$perm_21_counterfact <- sorted_data$c_21_cf - c_21_rural_threshold
  sorted_data$pop_21_incumb_counterfact <- pmin(sorted_data$pop_21_cf, sorted_data$pop_21_incumb)
  final_data <- sorted_data

  list(
    data = final_data,
    marginal_city_index = marginal_city_index,
    c_21_rur_counterfact = c_21_rural_threshold,
    pop_21_rur_counterfact = pop_totals$tot_21 - pop_cum_final
  )
}

compute_welfare_metrics <- function(marg_result, cf_prep) {
  final_data <- marg_result$data
  pop_totals <- cf_prep$pop_totals
  rur_21_income <- cf_prep$rur_21_income
  rur_21_pop_incumb <- cf_prep$rur_21_pop_incumb
  c_21_rur_cf <- marg_result$c_21_rur_counterfact
  pop_21_rur_cf <- marg_result$pop_21_rur_counterfact

  y_21_baseline <- sum(final_data$y_21 * (final_data$bua_21_pop / pop_totals$tot_21), na.rm = TRUE) +
    rur_21_income * (pop_totals$rur_21 / pop_totals$tot_21)

  c_21_baseline <- sum(final_data$c_21 * (final_data$pop_21_incumb / pop_totals$tot_21), na.rm = TRUE) +
    rur_21_income * (rur_21_pop_incumb / pop_totals$tot_21)

  y_21_cf <- sum(final_data$y_21_cf * (final_data$pop_21_cf / pop_totals$tot_21), na.rm = TRUE) +
    c_21_rur_cf * (pop_21_rur_cf / pop_totals$tot_21)

  pop_21_rur_incumb_cf <- pop_totals$tot_21 - sum(final_data$pop_21_incumb_counterfact, na.rm = TRUE)

  c_21_cf <- sum(final_data$c_21_cf * (final_data$pop_21_incumb_counterfact / pop_totals$tot_21), na.rm = TRUE) +
    c_21_rur_cf * (pop_21_rur_incumb_cf / pop_totals$tot_21)

  list(
    y_21_baseline = y_21_baseline,
    c_21_baseline = c_21_baseline,
    y_21_counterfactual = y_21_cf,
    c_21_counterfactual = c_21_cf,
    pct_chg_y_tot = 100 * (y_21_cf - y_21_baseline) / y_21_baseline,
    pct_chg_c_tot = 100 * (c_21_cf - c_21_baseline) / c_21_baseline,
    pct_chg_c_rur = 100 * (c_21_rur_cf - rur_21_income) / rur_21_income,
    pct_chg_pop_rur = 100 * (pop_21_rur_cf - pop_totals$rur_21) / pop_totals$rur_21,
    c_21_rur_counterfact = c_21_rur_cf,
    pop_21_rur_counterfact = pop_21_rur_cf
  )
}

run_single_counterfactual <- function(cf_prep, perm_rate_cf) {
  cf_pop_data <- compute_cf_populations(cf_prep, perm_rate_cf)
  marg_result <- find_marginal_city(cf_pop_data, cf_prep)
  welfare <- compute_welfare_metrics(marg_result, cf_prep)

  final_data <- marg_result$data
  final_data$pct_chg_y <- 100 * (final_data$y_21_cf - final_data$y_21) / final_data$y_21
  final_data$pct_chg_c <- 100 * (final_data$c_21_cf - final_data$c_21) / final_data$c_21

  list(data = final_data, welfare = welfare)
}

#_______________________________________________________________________________
# SWEEP FUNCTION ####
#_______________________________________________________________________________

run_sweep <- function(city_set_name, cities_in_cf, rate_sequence,
                      pop_totals, params, city_data,
                      progress_callback = NULL) {

  cf_prep <- prepare_counterfactual_data(city_data, cities_in_cf, params, pop_totals)

  agg_results <- vector("list", length(rate_sequence))
  city_income_results <- vector("list", length(rate_sequence))
  city_cons_results <- vector("list", length(rate_sequence))

  in_cf <- cf_prep$data$in_counterfact == 1L
  base_rate <- cf_prep$data$bua_perm_rate_01_21
  perm_rate_cf <- base_rate

  for (r in seq_along(rate_sequence)) {
    target_rate <- rate_sequence[r]

    perm_rate_cf[in_cf] <- pmax(base_rate[in_cf], target_rate)

    result <- run_single_counterfactual(cf_prep, perm_rate_cf)

    rd <- result$data
    treated_idx <- which(rd$in_counterfact == 1L & !is.na(rd$pct_chg_y))
    pct_chg_y_cities <- if (length(treated_idx) > 0) {
      sum(rd$pct_chg_y[treated_idx] * rd$bua_21_pop[treated_idx]) /
        sum(rd$bua_21_pop[treated_idx])
    } else NA_real_

    agg_results[[r]] <- data.frame(
      target_rate = target_rate,
      pct_chg_newcomer_cons = result$welfare$pct_chg_c_rur,
      pct_chg_cons_total = result$welfare$pct_chg_c_tot,
      pct_chg_national_income_pc = result$welfare$pct_chg_y_tot,
      pct_chg_city_income_pc = pct_chg_y_cities
    )

    city_income_results[[r]] <- data.frame(
      BUA22NM = rd$BUA22NM[treated_idx],
      bua_21_pop = rd$bua_21_pop[treated_idx],
      pct_chg_y = rd$pct_chg_y[treated_idx],
      target_rate = target_rate,
      stringsAsFactors = FALSE
    )

    city_cons_results[[r]] <- data.frame(
      BUA22NM = rd$BUA22NM[treated_idx],
      bua_21_pop = rd$bua_21_pop[treated_idx],
      pct_chg_c = rd$pct_chg_c[treated_idx],
      target_rate = target_rate,
      stringsAsFactors = FALSE
    )

    if (!is.null(progress_callback)) {
      progress_callback(r, length(rate_sequence))
    }
  }

  agg <- do.call(rbind, agg_results)
  agg$city_set <- city_set_name

  city_income <- do.call(rbind, city_income_results)
  city_income$city_set <- city_set_name

  city_cons <- do.call(rbind, city_cons_results)
  city_cons$city_set <- city_set_name

  list(agg = agg, city_income = city_income, city_cons = city_cons)
}

#_______________________________________________________________________________
# CHART FUNCTIONS ####
#_______________________________________________________________________________

create_line_chart <- function(data, city_set_name, y_var, ylabel,
                              title_metric, reference_rates, params) {

  plot_data <- data %>%
    filter(city_set == city_set_name) %>%
    mutate(text = sprintf("Permitting rate: %.3f\n%s: %.2f%%",
                          target_rate, ylabel, .data[[y_var]]))
  line_color <- custom_palette[1]

  param_caption <- sprintf("\u03B3=%.2f, \u03B8=%.2f, \u03C3=%.3f, \u03B2=%.3f, \u03BB=%.2f",
                           params$gamma, params$theta, params$sigma, params$beta, params$lambda)

  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]], text = text, group = 1)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
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
      subtitle = param_caption
    )

  p
}

create_agg_multi_set_chart <- function(data, y_var, ylabel, title_metric,
                                       reference_rates, params) {

  param_caption <- sprintf("\u03B3=%.2f, \u03B8=%.2f, \u03C3=%.3f, \u03B2=%.3f, \u03BB=%.2f",
                           params$gamma, params$theta, params$sigma, params$beta, params$lambda)

  plot_data <- data %>%
    mutate(
      city_set_label = city_set_labels[city_set],
      text = sprintf("City set: %s\nPermitting rate: %.3f\n%s: %.2f%%",
                     city_set_label, target_rate, ylabel, .data[[y_var]])
    )

  set_order <- names(city_set_labels)
  plot_data <- plot_data %>%
    mutate(city_set_label = factor(city_set_label, levels = city_set_labels[set_order]))

  n_sets <- length(unique(plot_data$city_set_label))
  colors <- custom_palette[1:n_sets]
  names(colors) <- levels(plot_data$city_set_label)

  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]],
                             color = city_set_label, group = city_set_label, text = text)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
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
      subtitle = param_caption
    )

  p
}

create_city_line_chart <- function(city_data_plot, city_set_name, y_var, ylabel,
                                   title_metric, reference_rates, params) {

  plot_data <- city_data_plot %>%
    filter(city_set == city_set_name)
  plot_data$BUA22NM <- sub("Cambridge \\(Cambridge\\)", "Cambridge", plot_data$BUA22NM)
  plot_data <- plot_data %>%
    mutate(text = sprintf("City: %s\nPermitting rate: %.3f\n%s: %.2f%%",
                          BUA22NM, target_rate, ylabel, .data[[y_var]]))

  param_caption <- sprintf("\u03B3=%.2f, \u03B8=%.2f, \u03C3=%.3f, \u03B2=%.3f, \u03BB=%.2f",
                           params$gamma, params$theta, params$sigma, params$beta, params$lambda)

  city_order <- plot_data %>%
    distinct(BUA22NM, bua_21_pop) %>%
    arrange(desc(bua_21_pop)) %>%
    pull(BUA22NM)

  plot_data <- plot_data %>%
    mutate(BUA22NM = factor(BUA22NM, levels = city_order))

  n_cities <- length(city_order)
  colors <- custom_palette[1:n_cities]
  names(colors) <- city_order

  p <- ggplot(plot_data, aes(x = target_rate, y = .data[[y_var]], color = BUA22NM, group = BUA22NM, text = text)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.3) +
    geom_vline(data = reference_rates, aes(xintercept = rate),
               linetype = "dashed", color = "gray40", linewidth = 0.5) +
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
      subtitle = param_caption
    )

  p
}
