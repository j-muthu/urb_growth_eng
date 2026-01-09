#_______________________________________________________________________________
# prepping for cfact ####
# dataset names!!!!!

#_______________________________________________________________________________
## function for Austin's permitting rate ####
# https://data.austintexas.gov/Building-and-Development/New-Residential-Units-Summary-by-Calendar-Year-and/2y79-8diw#:~:text=View%20Count%202402-,New%20Residential%20Units%20:%20Summary%20by%20Calendar%20Year%20and%20Type,BP
austin_perm_rate <- function (austin_dataset) 
{
  austin_dataset = read_csv(austin_dataset, show_col_types = FALSE)
  
  austin_dataset = austin_dataset %>%
    select(c("Calendar Year Issued", "Housing Units", "Permit Class", "Status Current", "Description", "Number Of Floors")) %>%
    filter(`Calendar Year Issued` >= 2001, `Calendar Year Issued` <= 2021, `Status Current` == "Final") %>%
    # if housing units is 1201, change to 1
    mutate(`Housing Units` = ifelse(`Housing Units` > 1000, `Housing Units` - 1000, `Housing Units`))
  
  austin_dataset$Description = tolower(austin_dataset$Description)
  
  meaningful_residence_names = c("residence", "condo", "duplex", "apartment", "family", "home", "residential", 
                                 "dwelling", "finish-out", "finish out", "story", "stories")
  meaningful_residence_names = c(meaningful_residence_names, paste0(meaningful_residence_names, "s"))
  
  # other names (abbreviations)
  other_residence_names = c("res", "apt", "hm", "sty", "stry")
  other_residence_names = c(other_residence_names, paste0(other_residence_names, "s"))
  
  # create regex patterns
  meaningful_pattern <- paste(meaningful_residence_names, collapse = "|")
  other_pattern <- paste(paste0("\\b", other_residence_names, "\\b"), collapse = "|")
  
  # filter to keep only rows with residence-related descriptions
  austin_dataset <- austin_dataset %>%
    filter(str_detect(Description, meaningful_pattern) | str_detect(Description, other_pattern))
  
  delete_conditions = c("clubhouse", "new garage", "new parking garage", "maintenance", "kiosk", "new 2 level parking garage")
  # remove any rows that contain any of the delete conditions
  austin_dataset = austin_dataset %>%
    filter(!str_detect(Description, paste(delete_conditions, collapse = "|")))
  
  # if the remaining rows contain the string "multi", then set the housing unit to 
  # the maximum of 5 or Number Of Floors, else assume it's a single housing unit
  austin_dataset = austin_dataset %>%
    mutate(`Housing Units` = ifelse(str_detect(Description, "multi"), 
                                    pmax(5, `Number Of Floors`), 1))
  
  # summarise to get total housing units per year
  austin_summary = austin_dataset %>%
    group_by(`Calendar Year Issued`) %>%
    summarise(total_housing_units = sum(`Housing Units`, na.rm = TRUE)) %>%
    arrange(`Calendar Year Issued`)
  
  # total number of permits issued over 2001-2021
  total_permits = sum(austin_summary$total_housing_units)
  
  # Austin's 2001 population was 669,693
  # https://www.austintexas.gov/sites/default/files/files/Planning/Demographics/population_history_pub_2019.pdf
  austin_2001_pop = 669693
  
  # austin's permitting rate over 2001-2021
  austin_permitting_rate = total_permits / austin_2001_pop
  cat("Austin's permitting rate:", austin_permitting_rate,"\n")
  
  return(austin_permitting_rate)
  # end of fn
}

## finding Austin's permitting rate ####
# 0.1263848
# https://data.austintexas.gov/Building-and-Development/New-Residential-Units-Summary-by-Calendar-Year-and/2y79-8diw
cat("Calculating Austin's permitting rate...\n")
austin_permitting_rate = austin_perm_rate("Data/Issued_Construction_Permits_20250927.csv")






# Apply the counterfactual permitting rates (if Austin's rate is higher)
modified_counterfactual_dataset = counterfactual_dataset %>%
  mutate(perm_rate_counterfact = ifelse(in_counterfact == 1,
                                        pmax(bua_perm_rate_01_21, austin_permitting_rate),
                                        bua_perm_rate_01_21))


