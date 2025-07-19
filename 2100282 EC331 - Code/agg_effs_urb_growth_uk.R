#_______________________________________________________________________________
# initialisation ####


packages = c("readr", "tidyverse", "httr", "jsonlite", "readxl", "ggrepel", "stargazer")

# Installing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  } else {
    message(paste("Package already installed:", pkg))
  }
  library(pkg, character.only = TRUE)
}
invisible(lapply(packages, install_if_missing))

# clean all
rm(list = ls())

## US parameter values ####

tau_1980_usa = 1 # numeraire
tau_2010_usa = 1.569 
# see "C:\Users\joshu\OneDrive - University of Warwick\EC331\non-final work\urb growth appendices.pdf"
# for calculation - calculate this for the UK

gamma_usa = 0.07
theta_usa = 0.04
sigma_usa = 0.04
beta_usa = 0.04
lambda_usa = 0.18

# defining city population threshold (only keep BUAs above this)
# https://www.ons.gov.uk/peoplepopulationandcommunity/housing/articles/townsandcitiescharacteristicsofbuiltupareasenglandandwales/census2021
# ^defines BUAs w pop over 20,000 as "medium" - keep those
city_pop_threshold = 20000

# deciding on geographic constraint variable
geographic_constraint_potential_list = c("mean_z_i","median_z_i","min_z_i")
geographic_constraint = "mean_z_i"

#_______________________________________________________________________________
# data cleaning ####
#_______________________________________________________________________________
## Importing data ####

# import 2022 land use data (using 2021 LSOA boundaries)
# https://www.gov.uk/government/statistics/land-use-in-england-2022
lsoa_land_use_2022_raw = read_csv("Data/lsoa_land_use_2022.csv")

# import LSOA pop data for 2001, 2011, 2021
# https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=1634
# https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=144
# https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2021
for (censusyr in c(2001,2011,2021)) {
  assign(paste0("lsoa_pop_",censusyr,"_raw"), read_csv(paste0("Data/lsoa_pop_",censusyr,".csv")))
}
rm(censusyr)

# import inter-year LSOA lookups
# https://www.data.gov.uk/dataset/4048a518-3eaf-457a-905c-9e04f4fffca8/lsoa-2001-to-lsoa-2011-to-lad-december-2011-best-fit-lookup-in-ew1
lsoa_lookup_2001_to_2011_raw = read_csv("Data/LSOA_(2001)_to_LSOA_(2011)_to_LAD_(2011)_Lookup_in_England_and_Wales.csv")
# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2011-to-lsoa-2021-to-local-authority-district-2022-best-fit-lookup-for-ew-v2/about
lsoa_lookup_2011_to_2021_raw = read_csv("Data/LSOA_(2011)_to_LSOA_(2021)_to_LAD_(2022)_Best_Fit_Lookup_for_EW_(V2).csv")

# import 2021 LSOA to BUA lookup
# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2021-to-bua-to-lad-to-region-december-2022-best-fit-lookup-in-ew-v2/about
lsoa_bua_lookup_2021_raw = read_csv("Data/LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv")

# LAD permitting data
# https://assets.publishing.service.gov.uk/media/67d2ac07886e7770c211e042/PS2_data_-_open_data_table__202412_.csv/preview
perm_rates_raw = read_csv("Data/PS2_data_-_open_data_table__202409_.csv",
                          skip = 2) %>%  # Skip first 2 rows so row 3 becomes headers
  select(2:4,34,112) %>%
  slice((33128):n())  # Filter rows from 33128 (start of 2001 data)

#_______________________________________________________________________________
## 2001-2011 merging ####

# for 2001 LSOAs, find equivalent 2011 LSOAs
lsoa_pop_2001_to_2011 = lsoa_pop_2001_raw %>%
  full_join(lsoa_lookup_2001_to_2011_raw, 
            by = c("lsoa_2001_code" = "LSOA01CD")) %>%
  select(2:7)

rm(lsoa_pop_2001_raw,
   lsoa_lookup_2001_to_2011_raw)

# check if there are any missing values in the 2001-2011 linked dataset
lsoa_pop_2001_to_2011 %>%
  filter(if_any(everything(), is.na))

# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2001-to-lsoa-2011-to-lad-december-2011-best-fit-lookup-in-ew/about
# U means unchanged, so no problems there
# M means merged, so for any 2001 LSOAs that merge into 2011 LSOAs, add their 2001 pops together
# X means irregular relationship. For any 2001 LSOAs 

# Create a subset of rows where LSOA11CD appears more than once
# AND at least one of those rows has "X" in CHGIND
lsoa_pop_2001_to_2011 %>%
  # Group by LSOA11CD to check for duplicates
  group_by(LSOA11CD) %>%
  # Filter groups that have duplicates AND at least one "X"
  filter(n() > 1 & any(CHGIND == "X")) %>%
  # Remove the grouping to get back to regular dataframe
  ungroup()
# there are no 2011 LSOAs that are a mix of both X and not X (i.e. they are only changed irregularly).

# for U, M, X, we merge any 2001 LSOA pops that got merged under the 2011 LSOAs
# if they didn't get merged, the 2001 LSOA pops correspond to the 2011 LSOAs
lsoa_pop_2001_to_2011_merged = lsoa_pop_2001_to_2011 %>%
  group_by(LSOA11CD) %>%
  summarize(pop_2001_lsoa_2011_nosplit = sum(usual_resident_pop_2001),
            LSOA11CD = first(LSOA11CD),
            LSOA11NM = first(LSOA11NM),
            CHGIND = first(CHGIND)
  )

rm(lsoa_pop_2001_to_2011)

# checking the LSOAs that got split (922 rows)
lsoa_pop_2001_to_2011_merged %>%
  filter(CHGIND == "S")

# merging with 2011 LSOA pop data
lsoa_pops_2001_and_2011_split_01_pop = lsoa_pop_2001_to_2011_merged %>%
  left_join(lsoa_pop_2011_raw, by = c("LSOA11CD"="lsoa_code_2011")) %>%
  # drop column that repeats 2011 LSOA names
  select(-"lsoa_name_2011")

rm(lsoa_pop_2001_to_2011_merged)

# the split LSOA 2001 population values will need to be imputed 
# do this by taking the 2011 pop values and decreasing them... 
# ...by the relative population change of E&W between 2001-2011
# England's pop grew by factor of 1.07396242237523 between 2001-2011
# see "Data/eng_wales_pop_over_time.xlsx"
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
lsoa_pops_2001_and_2011 = lsoa_pops_2001_and_2011_split_01_pop %>%
  mutate(pop_2001_imputed_split = ifelse(CHGIND == "S",
                                         usual_resident_pop_2011 / 1.07396242237523, 
                                         pop_2001_lsoa_2011_nosplit
  ))

rm(lsoa_pops_2001_and_2011_split_01_pop)

# checking rows where 2001 LSOAs were split
lsoa_pops_2001_and_2011 %>%
  filter(CHGIND == "S")

# dropping non-imputed 2001 pop column
lsoa_pops_2001_and_2011_imputed_only = lsoa_pops_2001_and_2011 %>%
  select(-"pop_2001_lsoa_2011_nosplit")

rm(lsoa_pops_2001_and_2011)

# checking for any duplicate 2011 LSOAs (there are none - good!)
lsoa_pops_2001_and_2011_imputed_only %>%
  group_by(LSOA11CD) %>%
  filter(n() > 1) %>%
  ungroup()

# the 2011 LSOA raw pop file has 4 more observations than the merged file
# and the original 2001-2011 LSOA lookup file
# finding those missing observations
diff_11_raw_pop_and_merged_df = lsoa_pop_2011_raw %>%
  anti_join(lsoa_pops_2001_and_2011_imputed_only,
            by = c("lsoa_code_2011" = "LSOA11CD")) %>%
  print()

rm(lsoa_pop_2011_raw)

# create a dataframe of these missing observations to add manually to the merged file
diff_11_raw_pop_and_merged_df_for_joining = data.frame(
  LSOA11CD = diff_11_raw_pop_and_merged_df$lsoa_code_2011,
  LSOA11NM = diff_11_raw_pop_and_merged_df$lsoa_name_2011,
  # create new change indicator "N" for "newly joined" for all of these entries
  CHGIND = rep("N", times = nrow(diff_11_raw_pop_and_merged_df)),
  usual_resident_pop_2011 = diff_11_raw_pop_and_merged_df$usual_resident_pop_2011,
  # and imputing 2001 pop in same way as above (using E&W pop growth rate)
  pop_2001_imputed_split = diff_11_raw_pop_and_merged_df$usual_resident_pop_2011 / 1.07278362492818
)

# adding missing observations to merged file
lsoa_pops_2001_and_2011_imputed_only_plus_miss = lsoa_pops_2001_and_2011_imputed_only %>%
  bind_rows(diff_11_raw_pop_and_merged_df_for_joining)

rm(lsoa_pops_2001_and_2011_imputed_only,
   diff_11_raw_pop_and_merged_df,
   diff_11_raw_pop_and_merged_df_for_joining)

#_______________________________________________________________________________
## 2011 to 2021 merging ####

# 2011 to 2021 LSOA lookup
# keeping only relevant cols
# 2011 LSOA, 2021 LSOA, 2022 LAD
lsoa_lookup_2011_to_2021_narrow = lsoa_lookup_2011_to_2021_raw %>%
  select(2:7)

rm(lsoa_lookup_2011_to_2021_raw)

# check for rows where multiple 2011 LSOAs are merged into 2021 LSOAs
lsoa_lookup_2011_to_2021_narrow %>%
  group_by(LSOA21CD) %>%
  filter(n() > 1)

# joining 2001&2011 pop data in 2011 LSOAs to 2011-2021 LSOA lookup
lsoa_pop_01_11_merged_w_21_lsoa = lsoa_pops_2001_and_2011_imputed_only_plus_miss %>%
  full_join(lsoa_lookup_2011_to_2021_narrow,
            by = "LSOA11CD")

rm(lsoa_pops_2001_and_2011_imputed_only_plus_miss,
   lsoa_lookup_2011_to_2021_narrow)

# aggregating pop values by 2021 LSOAs
lsoa_agg_pop_01_11_merged_w_21_lsoa = lsoa_pop_01_11_merged_w_21_lsoa %>%
  group_by(LSOA21CD) %>%
  summarize(pop_2011_final = sum(usual_resident_pop_2011),
            pop_2001_final = sum(pop_2001_imputed_split),
            LSOA21NM = first(LSOA21NM),
            LAD22CD = first(LAD22CD),
            LAD22NM = first(LAD22NM))

rm(lsoa_pop_01_11_merged_w_21_lsoa)

# merging w 2021 pop data
lsoa_pop_01_11_21_merged = lsoa_agg_pop_01_11_merged_w_21_lsoa %>%
  full_join(lsoa_pop_2021_raw,
            by = c("LSOA21CD" = "lsoa_21_code")) %>%
  # dropping repeated LSOA name column
  select(-"LSOA21NM")

rm(lsoa_agg_pop_01_11_merged_w_21_lsoa,
   lsoa_pop_2021_raw)

# identifying 2021 LSOAs with missing 2011 or 2001 pop data
lsoa_pop_01_11_21_merged %>%
  filter(is.na(pop_2011_final) | is.na(pop_2001_final)) %>%
  nrow()
# there are 1044!

# again, use the same impute algorithm as before
# England's pop grew by factor of 1.14368415562741 between 2001-2021
# and by a factor of 1.06492008640114 between 2011-2021
# see "Data/eng_wales_pop_over_time.xlsx"
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
lsoa_pop_01_11_21_miss_imputed = lsoa_pop_01_11_21_merged %>%
  mutate(pop_2011_final = ifelse(is.na(pop_2011_final),
                                 total_lsoa_pop_21 / 1.06492008640114,
                                 pop_2011_final
  )
  ) %>%
  mutate(pop_2001_final = ifelse(is.na(pop_2001_final),
                                 total_lsoa_pop_21 / 1.14368415562741,
                                 pop_2001_final
  )
  )

rm(lsoa_pop_01_11_21_merged)

# the land use statistics are only for English 2021 LSOAs
# checking we don't have any LSOAs not coded for England or Wales
lsoa_pop_01_11_21_miss_imputed %>%
  filter(!startsWith(LSOA21CD, "E") & !startsWith(LSOA21CD, "W"))
# we don't

# checking that the number of English LSOAs in the population dataset matches the number in the land use dataset
lsoa_pop_01_11_21_miss_imputed %>%
  filter(startsWith(LSOA21CD, "E")) %>%
  nrow()
# it does - 33755

# only keeping English LSOAs
lsoa_eng_pop_01_11_21_miss_imputed = lsoa_pop_01_11_21_miss_imputed %>%
  filter(startsWith(LSOA21CD, "E"))

rm(lsoa_pop_01_11_21_miss_imputed)

#_______________________________________________________________________________
## importing land use data ####

# joining with English LSOA land use data
lsoa_eng_pop_01_11_21_imputed_w_land_use = lsoa_eng_pop_01_11_21_miss_imputed %>%
  left_join(lsoa_land_use_2022_raw,
            by = c("LSOA21CD" = "lsoa_2021_code")) %>%
  # dropping duplicated LSOA name col
  select(-"lsoa_2021_name")

#_______________________________________________________________________________
## matching missing LAD codes ####
# checking how many distinct LAD codes there are in this LSOA dataset
lads_in_lsoa_data = lsoa_eng_pop_01_11_21_imputed_w_land_use %>%
  distinct(LAD22CD, LAD22NM)
# 310 - one of these is NA

# splitting by 3-character geocode prefix
# E06, E07, E08, E09 (plus NA)
lad_prefix_counts = lads_in_lsoa_data %>%
  mutate(prefix = str_sub(LAD22CD, 1, 3)) %>%
  group_by(prefix) %>%
  summarise(count = n_distinct(LAD22CD)) %>%
  arrange(prefix)
print(lad_prefix_counts)
# 1 E06       59
# 2 E07      181
# 3 E08       36
# 4 E09       33
# 5 NA         1

# investigating the NA-valued LAD
lsoas_w_missing_lad = lsoa_eng_pop_01_11_21_imputed_w_land_use %>%
  filter(is.na(LAD22CD))

#_______________________________________________________________________________
## ## API call to match unmatched LSOAs to a LAD ####
# 
# # Create an empty dataframe to store results
# lads_to_add = data.frame(
#   LSOA21CD = character(),
#   LAD22CD = character()
# )
# # call the findthatpostcode.uk API to match the LAD22CD based on the LSOA
# # note that my wifi seems to be slow enough to not hit the rate limit...
# # ...if you do start hitting a rate limit, just use a Sys.sleep(`#secs`) command
# for(row in 1:nrow(lsoas_w_missing_lad)) {
#   lsoa_code_val = lsoas_w_missing_lad[[row, 1]]
#   url = paste0("https://findthatpostcode.uk/areas/", lsoa_code_val, ".json")
#   response = GET(url)
# 
#   while (response$status_code == 429) {
#     warning(paste("Rate limit exceeded for LSOA code: ", lsoa_code_val," - waiting 60 secs"))
#     Sys.sleep(60)  # Wait 60 seconds before continuing
#     response = GET(url)
#   }
# 
#   json_data = fromJSON(rawToChar(response$content))
# 
#   lad22_val = json_data[["included"]][["attributes"]][["itl"]][1]
#   
#   print(paste0("LSOA: ",lsoa_code_val," has LAD code: ",lad22_val,". Row ",row," complete"))
#   lads_to_add = lads_to_add %>%
#     add_row(LSOA21CD = lsoa_code_val, LAD22CD = lad22_val)
#   
# }
# 
# print("Rounds complete!")
# 
# # saving backup of API-called dataset
# write_csv(lads_to_add, "Data/lads_to_add_backup.csv")
## UNCOMMENT TO HERE IF YOU WANT TO RE-RUN LAD API CALL ##

## COMMENT OUT THE COMMAND BELOW IF YOU WANT TO RE-RUN LAD API CALL ##
# loading in dataframe with results of API call
# ie LAD codes for entries previously missing them
lads_to_add = read_csv("Data/lads_to_add_backup.csv")

# add these missing LAD codes back into the LSOA-LAD dataset
# note the LAD names are still missing
lsoa_eng_pop_01_11_21_nomiss_lad = lsoa_eng_pop_01_11_21_imputed_w_land_use %>%
  left_join(lads_to_add, by = "LSOA21CD") %>%
  mutate(LAD22CD = coalesce(LAD22CD.x, LAD22CD.y)) %>%
  # dropping redundant code columns
  select(-c("LAD22CD.x","LAD22CD.y"))

# now, after merging, checking how many distinct LAD codes there are in this LSOA dataset
# remember that the merged LAD22CD values don't have LAD22NM values, so we have to add these manually
lads_in_lsoa_data = lsoa_eng_pop_01_11_21_nomiss_lad %>%
  group_by(LAD22CD) %>%
  mutate(LAD22NM = first(LAD22NM[!is.na(LAD22NM)])) %>%
  distinct(LAD22CD, LAD22NM)
# 309 values now - the NA value has been dropped

# splitting by 3-character geocode prefix
# E06, E07, E08, E09 (plus NA)
lad_prefix_counts = lads_in_lsoa_data %>%
  mutate(prefix = str_sub(LAD22CD, 1, 3)) %>%
  group_by(prefix) %>%
  summarise(count = n_distinct(LAD22CD)) %>%
  arrange(prefix)
print(lad_prefix_counts)
# as before:
# 1 E06       59
# 2 E07      181
# 3 E08       36
# 4 E09       33

rm(lsoa_eng_pop_01_11_21_miss_imputed,
   lsoa_land_use_2022_raw)

#_______________________________________________________________________________
## LAD permitting rates ####
# Extract the list of valid LAD codes from lads_in_lsoa_data
valid_lad_codes = lads_in_lsoa_data$LAD22CD

# Find LPACDs that don't meet the filter criteria
filtered_out_lpacd = perm_rates_raw %>%
  filter(!LPACD %in% valid_lad_codes) %>%
  distinct(LPACD) %>%
  pull(LPACD)

# Print the filtered-out LPACD values
print("LPACD values that were filtered out:")
print(filtered_out_lpacd)

perm_rates_data = perm_rates_raw %>%
  # Filter to keep only LPACDs that match the LAD22CD values
  filter(LPACD %in% valid_lad_codes) %>%
  # Replace ".." with NA and then convert to numeric
  mutate(major_dwellings = ifelse(`Total granted; major dwellings (all)` == "..", NA, `Total granted; major dwellings (all)`),
         minor_dwellings = ifelse(`Total granted; minor dwellings (all)` == "..", NA, `Total granted; minor dwellings (all)`),
         major_dwellings = as.numeric(major_dwellings),
         minor_dwellings = as.numeric(minor_dwellings),
         year = as.numeric(substr(Quarter, 1, 4))) %>%
  # Group and sum by LPACD and year
  group_by(LPACD, LPANM, year) %>%
  summarise(total_dwellings = sum(major_dwellings, na.rm = TRUE) + sum(minor_dwellings, na.rm = TRUE),
            .groups = 'drop') %>%
  # Pivot wider to get years as columns
  pivot_wider(names_from = year,
              values_from = total_dwellings,
              names_prefix = "total_maj_and_min_dwellings_") %>%
  # up to and including 2021
  select(-c("total_maj_and_min_dwellings_2022",
            "total_maj_and_min_dwellings_2023",
            "total_maj_and_min_dwellings_2024"))

#_______________________________________________________________________________
## imputing missing permitting data using linear regression ####
# Convert to long format for easier analysis of missing patterns
perm_rates_long = perm_rates_data %>%
  pivot_longer(cols = starts_with("total_maj_and_min_dwellings_"),
               names_to = "year", 
               values_to = "total_dwellings") %>%
  mutate(year = as.numeric(gsub("total_maj_and_min_dwellings_", "", year)))

# Check which LPACDs have all missing values or only 1 non-missing value
missing_data_check = perm_rates_long %>%
  group_by(LPACD) %>%
  summarise(LPANM = first(LPANM),
            n_missing = sum(is.na(total_dwellings)),
            n_total = n(),
            n_non_missing = n_total - n_missing,
            .groups = 'drop') %>%
  filter(n_missing == n_total | n_non_missing <= 1)

# Create the vector of LPACDs that can't be imputed
lpacd_noimp_miss_val = missing_data_check$LPACD

# Function to perform linear imputation for a single LPACD
impute_missing = function(data) {
  # Check if we have any non-missing data
  if(sum(!is.na(data$total_dwellings)) < 2) {
    # If less than 2 non-missing points, we can't do linear regression
    # Return original data
    return(data)
  }
  
  # Fit linear model on non-missing data
  model = lm(total_dwellings ~ year, data = data[!is.na(data$total_dwellings),])
  
  # Predict for all years
  data$total_dwellings_imputed = predict(model, newdata = data)
  
  # Apply zero floor to predictions (replace negative values with zero)
  # so that there are no predicted negative values for the number of permits
  data$total_dwellings_imputed = pmax(data$total_dwellings_imputed, 0)
  
  # Use original values where available, imputed values where missing
  data$total_dwellings_final = ifelse(is.na(data$total_dwellings), 
                                      data$total_dwellings_imputed,
                                      data$total_dwellings)
  return(data)
}

# Now apply imputation
perm_rates_imputed = perm_rates_long %>%
  group_by(LPACD, LPANM) %>%
  group_modify(~impute_missing(.x)) %>%
  ungroup()

# Reshape back to wide format
perm_rates_final = perm_rates_imputed %>%
  select(-total_dwellings, -total_dwellings_imputed) %>%  # Remove intermediate columns
  pivot_wider(names_from = year,
              values_from = total_dwellings_final,
              names_prefix = "total_maj_and_min_dwellings_")

# Print these LPACDs
print("LPACDs with all missing values or only 1 non-missing value (can't perform imputation):")
print(missing_data_check)
# 2 of these

# for the 2 LPACDs with missing (non-imputable) values
# use median values of permitting numbers from bordering LADs in the same region
# https://ons.maps.arcgis.com/apps/webappviewer/index.html?id=5cec9cc7208d418fbc1e7f538cb8745f
# North Northamptonshire E06000061 
lads_bordering_E06000061 = c("E06000017", # Rutland
                             "E07000141", # South Kesteven
                             "E06000031", # Peterborough
                             "E07000011", # Huntingdonshire
                             "E06000055", # Bedford
                             "E06000042", # Milton Keynes
                             "E06000062", # West Northamptonshire
                             "E07000131" # Harborough
)

# West Northamptonshire E06000062 
lads_bordering_E06000062 = c("E07000131", # Harborough
                             "E06000061", # North Northamptonshire
                             "E06000042", # Milton Keynes
                             "E06000060", # Buckinghamshire
                             "E07000177", # Cherwell
                             "E07000221", # Stratford-upon-Avon
                             "E07000220" # Rugby
)

# Function to impute values based on bordering LADs
impute_from_bordering_lads = function(target_lpacd, bordering_lpacds, data_wide, data_long) {
  # Filter bordering LADs that exist in the dataset and aren't in missing_data_check
  valid_bordering_lpacds = bordering_lpacds[bordering_lpacds %in% 
                                              setdiff(unique(data_long$LPACD), 
                                                      lpacd_noimp_miss_val)]
  
  if(length(valid_bordering_lpacds) == 0) {
    warning(paste("No valid bordering LADs found for", target_lpacd))
    return(NULL)
  }
  
  # Get the name of the target LAD
  target_name = missing_data_check$LPANM[missing_data_check$LPACD == target_lpacd]
  
  # Get data for bordering LADs in long format
  bordering_data_long = data_long %>%
    filter(LPACD %in% valid_bordering_lpacds)
  
  # Calculate median values for each year
  imputed_values = bordering_data_long %>%
    group_by(year) %>%
    summarise(total_dwellings_final = median(total_dwellings_final, na.rm = TRUE),
              .groups = 'drop') %>%
    # Add target LPACD and name
    mutate(LPACD = target_lpacd,
           LPANM = target_name)
  
  return(imputed_values)
}

# Create data structure to store all imputed values
bordering_imputed_values = list()

# Apply imputation for each LPACD in missing_data_check
bordering_imputed_values[["E06000061"]] = impute_from_bordering_lads(
  "E06000061", lads_bordering_E06000061, perm_rates_final, perm_rates_imputed)

bordering_imputed_values[["E06000062"]] = impute_from_bordering_lads(
  "E06000062", lads_bordering_E06000062, perm_rates_final, perm_rates_imputed)

# Combine all imputed values into a single dataframe
bordering_imputed_combined = bind_rows(bordering_imputed_values)

# Convert back to wide format for merging with main dataset
bordering_imputed_wide = bordering_imputed_combined %>%
  pivot_wider(id_cols = c(LPACD, LPANM),
              names_from = year,
              values_from = total_dwellings_final,
              names_prefix = "total_maj_and_min_dwellings_")

# Remove the non-imputable LPACDs from the final dataset
perm_rates_final_cleaned = perm_rates_final %>%
  filter(!LPACD %in% lpacd_noimp_miss_val)

# Add the newly imputed values
perm_rates_final_with_bordering = bind_rows(
  perm_rates_final_cleaned,
  bordering_imputed_wide
)

# Print summary of imputation results
cat("\nImputation summary:\n")
cat("Number of LPACDs that couldn't be imputed with linear regression:", length(lpacd_noimp_miss_val), "\n")
cat("Number of LPACDs successfully imputed using bordering LADs:", 
    nrow(bordering_imputed_wide), "\n")

# removing intermediate datasets
rm(list = ls(pattern = "^lads_bordering_E0"))
rm(bordering_imputed_combined,
   bordering_imputed_values,
   bordering_imputed_wide,
   perm_rates_data,
   perm_rates_final,
   perm_rates_final_cleaned,
   perm_rates_imputed,
   perm_rates_long,
   perm_rates_raw)

# Identify which LPACD value is duplicated
duplicated_lpacd = perm_rates_final_with_bordering %>%
  group_by(LPACD) %>%
  filter(n() > 1) %>%
  pull(LPACD) %>%
  unique()

# Confirm it's E07000112 as you mentioned
print(duplicated_lpacd)

# Extract the duplicate rows
duplicate_rows = perm_rates_final_with_bordering %>%
  filter(LPACD == duplicated_lpacd)

# Create the aggregated row:
# 1. Keep LPACD as E07000112
# 2. Keep LPANM as "Folkestone and Hythe"
# 3. Sum all other columns
aggregated_row = duplicate_rows %>%
  summarize(
    LPACD = first(LPACD),
    LPANM = first(LPANM),
    across(where(is.numeric), sum)
  )

# Remove the duplicate rows from the original dataframe
perm_rates_final_with_bordering = perm_rates_final_with_bordering %>%
  filter(LPACD != duplicated_lpacd)

# Add the aggregated row back to the dataframe
perm_rates_final_with_bordering = bind_rows(
  perm_rates_final_with_bordering, 
  aggregated_row
)

# Verify no more duplicates exist
any_duplicates = perm_rates_final_with_bordering %>%
  group_by(LPACD) %>%
  filter(n() > 1) %>%
  nrow()

print(paste0("Duplicates remaining: ", any_duplicates))

# Calculate sum of permits for 2001-2011 and 2001-2021 periods
perm_rates_summary = perm_rates_final_with_bordering %>%
  rowwise() %>%
  mutate(# Sum of permits for 2001-2011
    sum_perms_01_11 = sum(
      c_across(starts_with("total_maj_and_min_dwellings_2001"):starts_with("total_maj_and_min_dwellings_2011")),
      na.rm = TRUE),
    # Sum of permits for 2001-2021
    sum_perms_01_21 = sum(
      c_across(starts_with("total_maj_and_min_dwellings_")),
      na.rm = TRUE)) %>%
  
  ungroup() %>%
  # Keep only LPACD, LPANM, and the new sum columns
  select(LPACD, LPANM, sum_perms_01_11, sum_perms_01_21)

rm(perm_rates_final_with_bordering,
   duplicate_rows)

# # joining LSOA pop & land use data to LSOA-BUA lookup
# lsoa_bua_eng_pop_01_11_21_land_use = lsoa_eng_pop_01_11_21_nomiss_lad %>%
# left_join(lsoa_bua_eng_lookup_2021_complete,
#           by = "LSOA21CD") %>%
# # dropping duplicated LSOA name col
# select(-c("lsoa_21_name")) %>%
# # dropping invalid BUA code (E63999999)
# filter(BUA22CD != "E63999999")

#_______________________________________________________________________________
## LSOA to BUA data ####

# cleaning
lsoa_bua_eng_lookup_2021_narrow = lsoa_bua_lookup_2021_raw %>%
  # removing irrelevant cols from LSOA to BUA lookup
  select(-c(ends_with("W"),"ObjectId")) %>%
  # keeping only English LSOAs
  filter(startsWith(LSOA21CD, "E"))
# 33755 rows - matches number in pop & land use dataframe

rm(lsoa_bua_lookup_2021_raw)

# identifying any rows that have any missing entries
missing_buas = lsoa_bua_eng_lookup_2021_narrow %>%
  filter(if_any(everything(), is.na)) %>%
  # keeping only the LSOA codes
  select("LSOA21CD")
# there are lots (2280)

#_______________________________________________________________________________
## ## API to match unmatched LSOAs to BUAs ####
# 
# library(httr)
# library(jsonlite)
# 
# # Create an empty dataframe to store results
# buas_to_add = data.frame(
#   LSOA21CD = character(),
#   BUA22CD = character()
# )
# # call the findthatpostcode.uk API to match the BUA22CD based on the LSOA
# # note that my wifi seems to be slow enough to not hit the rate limit...
# # ...if you do start hitting a rate limit, just use a Sys.sleep(`#secs`) command
# for(row in 1:nrow(missing_buas)) {
#   lsoa_code_val = missing_buas[[row, 1]]
#   url = paste0("https://findthatpostcode.uk/areas/", lsoa_code_val, ".json")
#   response = GET(url)
#   
#   while (response$status_code == 429) {
#     warning(paste("Rate limit exceeded for LSOA code: ", lsoa_code_val," - waiting 60 secs"))
#     Sys.sleep(60)  # Wait 60 seconds before continuing
#     response = GET(url)
#   }
#   
#   json_data = fromJSON(rawToChar(response$content))
#   
#   # for some reason, the API will sometimes give an invalid BUA code - E63999999.
#   # in some cases, it seems to be legitimate (probably, there's no BUA present)
#   # but in others, the solution seems to be to just keep asking
#   # and eventually, it will return a correct BUA code!
#   E63999999_count = 1
#   max_E63999999_count = 20
#   
#   while (json_data[["included"]][["attributes"]][["bua22"]][1] == "E63999999" && E63999999_count <= max_E63999999_count) {
#     print(paste0("LSOA: ",missing_buas[[row, 1]]," gave E63999999 (invalid code)"))
#     response = GET(url)
#     json_data = fromJSON(rawToChar(response$content))
#     bua22_val = json_data[["included"]][["attributes"]][["bua22"]][1]
#     E63999999_count = E63999999_count + 1
#   }
#   
#   bua22_val = json_data[["included"]][["attributes"]][["bua22"]][1]
#   
#   if (E63999999_count > max_E63999999_count) {
#     print(paste0("LSOA: ",missing_buas[[row, 1]]," kept giving E63999999 (invalid code) even after ",max_E63999999_count," attempts - assigning it E63999999. Row ",row," complete"))
#   } else {
#     print(paste0("LSOA: ",lsoa_code_val," has BUA code: ",bua22_val,". Row ",row," complete"))
#   }
#   buas_to_add = buas_to_add %>%
#     add_row(LSOA21CD = lsoa_code_val, BUA22CD = bua22_val)
# }
# 
# print("Rounds complete!")
# 
# # saving backup of API-called dataset
# write_csv(buas_to_add, "Data/buas_to_add_backup.csv")
# 
# # checking for invalid BUA code (E63999999)
# buas_to_add %>%
#   filter(BUA22CD == "E63999999") %>%
#   nrow
# # 203 LSOAs have invalid corresponding BUA code
## UNCOMMENT TO HERE IF YOU WANT TO RE-RUN BUA API CALL!!!!!!!!!!!!!!!!!!!!! ##

## COMMENT OUT THE COMMAND BELOW IF YOU WANT TO RE-RUN BUA API CALL!!!!!!!!!!!!!!!!!!!!! ##
# loading in dataframe with results of API call
# ie BUA codes for entries previously missing them
buas_to_add = read_csv("Data/buas_to_add_backup.csv")

rm(missing_buas)

#_______________________________________________________________________________
## merging BUA API data with other datasets ####

# add these missing BUA codes back into the LSOA-BUA lookup
# note the BUA names are still missing
lsoa_bua_eng_lookup_2021_complete = lsoa_bua_eng_lookup_2021_narrow %>%
  left_join(buas_to_add, by = "LSOA21CD") %>%
  mutate(BUA22CD = coalesce(BUA22CD.x, BUA22CD.y)) %>%
  # dropping redundant BUA code columns
  select(-c("BUA22CD.x","BUA22CD.y"))

rm(lsoa_bua_eng_lookup_2021_narrow)

# checking again for invalid BUA code (E63999999)
lsoa_bua_eng_lookup_2021_complete %>%
  filter(BUA22CD == "E63999999") %>%
  print(n = 10000)

# joining LSOA pop & land use data to LSOA-BUA lookup
lsoa_bua_eng_pop_01_11_21_land_use = lsoa_eng_pop_01_11_21_nomiss_lad %>%
  left_join(lsoa_bua_eng_lookup_2021_complete,
            by = "LSOA21CD") %>%
  # coalescing repeated columns
  mutate(LAD22NM = coalesce(LAD22NM.x, LAD22NM.y)) %>%
  mutate(LAD22CD = coalesce(LAD22CD.x, LAD22CD.y)) %>%
  # dropping duplicated column names
  select(-c("lsoa_21_name", 
            "LAD22NM.x","LAD22NM.y",
            "LAD22CD.x","LAD22CD.y")) %>%
  # dropping invalid BUA code (E63999999)
  filter(BUA22CD != "E63999999")

rm(lsoa_bua_eng_lookup_2021_complete)

# checking to see if anything is missing
lsoa_bua_eng_pop_01_11_21_land_use %>%
  filter(is.na(BUA22CD))
# nothing else missing - good!

# I noticed an error in one of the LSOA to BUA codings
# rectifying
lsoa_bua_eng_pop_01_11_21_land_use = lsoa_bua_eng_pop_01_11_21_land_use %>%
  mutate(
    BUA22NM = if_else(LSOA21CD == "E01002946", "Kingston upon Thames", BUA22NM),
    BUA22CD = if_else(LSOA21CD == "E01002946", "E63005164", BUA22CD)
  )

#_______________________________________________________________________________
## using LAD permit data to estimate BUA permit data ####
# Create a mapping table
lsoa_mapping = lsoa_bua_eng_pop_01_11_21_land_use %>%
  select(LSOA21CD, LAD22CD, BUA22CD, pop_2001_final) %>%
  # Ensure we have the BUA code for each LSOA
  filter(!is.na(BUA22CD))

# calculating proportion of the LAD population living in each BUA
lad_bua_population = lsoa_mapping %>%
  # Group by LAD and BUA
  group_by(LAD22CD, BUA22CD) %>%
  # Sum population for each LAD-BUA combination
  summarise(bua_in_lad_population = sum(pop_2001_final, na.rm = TRUE),
            .groups = "drop") %>%
  # Calculate total population by LAD
  group_by(LAD22CD) %>%
  mutate(lad_total_population = sum(bua_in_lad_population),
         # Calculate proportion of LAD population in each BUA
         population_proportion = bua_in_lad_population / lad_total_population) %>%
  ungroup()
# checking for any missing rows:
lad_bua_population %>% filter(if_any(everything(), is.na))
# nothing - good

# applying population proportions to distribute LAD-level permitting data to BUAs
bua_permits = lad_bua_population %>%
  # Join with permitting data (assuming you've matched LAD22CD with LPACD)
  left_join(perm_rates_summary, by = c("LAD22CD" = "LPACD")) %>%
  # Calculate BUA-level permits based on population proportion
  mutate(
    sum_bua_perms_01_11 = sum_perms_01_11 * population_proportion,
    sum_bua_perms_01_21 = sum_perms_01_21 * population_proportion
  )

# Aggregate to get total permits for each BUA (summing across LADs)
bua_total_permits = bua_permits %>%
  group_by(BUA22CD) %>%
  summarise(
    bua_pop_01 = sum(bua_in_lad_population, na.rm = TRUE),
    sum_bua_perms_01_11 = sum(sum_bua_perms_01_11, na.rm = TRUE),
    sum_bua_perms_01_21 = sum(sum_bua_perms_01_21, na.rm = TRUE),
    .groups = "drop"
  )

rm(lsoa_eng_pop_01_11_21_nomiss_lad)

#_______________________________________________________________________________
## final joining incl London ####

# aggregating london BUAs
# need to aggregate London BUAs
london_buas = data.frame(
  BUA22NM = c("City and County of the City of London",
              "Barking and Dagenham",
              "Barnet",
              "Bexley",
              "Brent",
              "Bromley",
              "Camden",
              "Croydon",
              "Ealing",
              "Enfield",
              "Greenwich",
              "Hackney",
              "Hammersmith and Fulham",
              "Haringey",
              "Harrow",
              "Havering",
              "Hillingdon",
              "Hounslow",
              "Islington",
              "Kensington and Chelsea",
              "Kingston upon Thames",
              "Lambeth",
              "Lewisham",
              "Merton",
              "Newham",
              "Redbridge",
              "Richmond upon Thames",
              "Southwark",
              "Sutton (Sutton)",
              "Tower Hamlets",
              "Waltham Forest",
              "Wandsworth",
              "City of Westminster"
  ),
  
  BUA22CD = c("E63004906",
              "E63004859",
              "E63004747",
              "E63004992",
              "E63004844",
              "E63005189",
              "E63004858",
              "E63005267",
              "E63004894",
              "E63004679",
              "E63004986",
              "E63004850",
              "E63004944",
              "E63004793",
              "E63004781",
              "E63004796",
              "E63004882",
              "E63005014",
              "E63004860",
              "E63004950",
              "E63005164",
              "E63005063",
              "E63005035",
              "E63005121",
              "E63004881",
              "E63004790",
              "E63005073",
              "E63004965",
              "E63005250",
              "E63004898",
              "E63004797",
              "E63005033",
              "E63004916"
  )
)

## Aggregating London BUA permit data level ####
bua_total_permits_lon = bua_total_permits %>%
  # Replace London BUA codes with a single user-defined code
  mutate(BUA22CD = ifelse(BUA22CD %in% london_buas$BUA22CD,
                          "LON999999", 
                          BUA22CD)) %>%
  # Group by the modified BUA22CD
  group_by(BUA22CD) %>%
  # Create the aggregations
  summarize(bua_pop_01 = sum(bua_pop_01),
            sum_bua_perms_01_11 = sum(sum_bua_perms_01_11),
            sum_bua_perms_01_21 = sum(sum_bua_perms_01_21)) %>%
  # total permits per initial housing unit (i.e. per initial individual)
  # "initial" meaning 2001
  mutate(bua_perm_rate_01_11 = sum_bua_perms_01_11 / bua_pop_01,
         bua_perm_rate_01_21 = sum_bua_perms_01_21 / bua_pop_01)

## Aggregating up to BUA level ####
agg_eng_pop_01_11_21_land_use_w_london = lsoa_bua_eng_pop_01_11_21_land_use %>%
  # Replace London BUA codes with a single user-defined code
  mutate(BUA22CD = ifelse(BUA22CD %in% london_buas$BUA22CD,
                          "LON999999", 
                          BUA22CD)) %>%
  mutate(BUA22NM = ifelse(BUA22NM %in% london_buas$BUA22NM, 
                          "Greater London", 
                          BUA22NM)) %>%
  # Group by the modified BUA22CD
  group_by(BUA22CD) %>%
  # Create the aggregations
  summarize(
    # Keep first non-empty BUA22NM
    BUA22NM = first(na.omit(BUA22NM)),
    RGN22CD = first(na.omit(RGN22CD)),
    RGN22NM = first(na.omit(RGN22NM)),
    bua_01_pop = sum(pop_2001_final),
    bua_11_pop = sum(pop_2011_final),
    bua_21_pop = sum(total_lsoa_pop_21),
    # 1/z_i is fraction of land that's geographically unconstrained
    # so z_i is 1/(1-fraction of land that's geographically *constrained*)
    # calculating different values, averaging/summarising land constraints in different ways
    mean_z_i = 1/(1-(mean(pct_forest_open_land_water) / 100)),
    median_z_i = 1/(1-(median(pct_forest_open_land_water) / 100)),
    min_z_i = 1/(1-(min(pct_forest_open_land_water) / 100))
  )

#_______________________________________________________________________________
## merging permit data with main dataset ####
agg_eng_pop_01_11_21_land_use_w_london = agg_eng_pop_01_11_21_land_use_w_london %>%
  left_join(bua_total_permits_lon,
            by = "BUA22CD") %>%
  select(-c("bua_pop_01"))

rm(lsoa_bua_eng_pop_01_11_21_land_use,
   bua_total_permits_lon,
   london_buas)

# drop any BUAs that are below your city size threshold (20k)
agg_pop_01_11_21_ovr_city_threshold = agg_eng_pop_01_11_21_land_use_w_london %>%
  filter(bua_21_pop >= city_pop_threshold)

rm(agg_eng_pop_01_11_21_land_use_w_london)

# check to see if there are any remaining BUAs with no name
agg_pop_01_11_21_ovr_city_threshold %>%
  filter(is.na(BUA22NM))
# there are none

# rounding population values
agg_pop_01_11_21_ovr_city_threshold_rounded = agg_pop_01_11_21_ovr_city_threshold %>%
  mutate(across(c(bua_01_pop, bua_11_pop, bua_21_pop), round)) %>%
  select(-c("sum_bua_perms_01_11","sum_bua_perms_01_21"))

#_______________________________________________________________________________
## adding in research labour share values (l_i) ####
# from p.11 https://www.enterpriseresearch.ac.uk/wp-content/uploads/2021/09/ERC-Insight-The-UK%E2%80%99s-business-RD-workforce-Belt.Ri_.Akinremi.pdf
agg_pop_01_11_21_ovr_city_threshold_rounded = agg_pop_01_11_21_ovr_city_threshold_rounded %>%
  mutate(l_i = case_when(
    RGN22NM == "North East" ~ 0.0052,
    RGN22NM == "North West" ~ 0.0060,
    RGN22NM == "Yorkshire and The Humber" ~ 0.0056,
    RGN22NM == "East Midlands" ~ 0.0088,
    RGN22NM == "West Midlands" ~ 0.0097,
    RGN22NM == "East of England" ~ 0.0143,
    RGN22NM == "London" ~ 0.0069,
    RGN22NM == "South East" ~ 0.013,
    RGN22NM == "South West" ~ 0.0083
  ))

rm(agg_pop_01_11_21_ovr_city_threshold,
   perm_rates_summary,
   valid_lad_codes,
   filtered_out_lpacd,
   lpacd_noimp_miss_val,
   any_duplicates,
   missing_data_check,
   lsoa_mapping,
   lsoa_eng_pop_01_11_21_imputed_w_land_use,
   lsoas_w_missing_lad,
   lads_in_lsoa_data,
   lads_to_add,
   lad_prefix_counts,
   lad_bua_population,
   buas_to_add,
   bua_total_permits,
   bua_permits,
   aggregated_row)

#_______________________________________________________________________________
# testing the original DP model on data ####

# total population of England
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop
tot_01_pop = 49449700
tot_11_pop = 53107200
tot_21_pop = 56554900

# urban population based on aggregated BUA dataset
urb_01_pop = sum(agg_pop_01_11_21_ovr_city_threshold_rounded$bua_01_pop)
urb_11_pop = sum(agg_pop_01_11_21_ovr_city_threshold_rounded$bua_11_pop)
urb_21_pop = sum(agg_pop_01_11_21_ovr_city_threshold_rounded$bua_21_pop)

# rural pop is difference between total and urban pop
rur_01_pop = tot_01_pop - urb_01_pop
rur_11_pop = tot_11_pop - urb_11_pop
rur_21_pop = tot_21_pop - urb_21_pop

# removing irrelevant geographic constraint variables
geographic_constraint_list_for_removal = geographic_constraint_potential_list[geographic_constraint_potential_list != geographic_constraint]
agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints = agg_pop_01_11_21_ovr_city_threshold_rounded %>%
  select(-all_of(geographic_constraint_list_for_removal)) %>%
  # arranging by descending order of 2021 population
  arrange(desc(bua_21_pop))

# intermediate save
write_csv(agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
          "Data/agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv")

#_______________________________________________________________________________
# intermediate save - can run from here ####
agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints = read_csv("Data/agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints.csv")

rm(
  counterfactual_results_tibble,
  agg_pop_01_11_21_ovr_city_threshold_rounded
)

#_______________________________________________________________________________
# counterfactual & evaluation ####
# _________________________ ####

#_______________________________________________________________________________
# function to prepare data for counterfactual & evaluation ####
data_for_cf_fn = function(
    og_dataset = agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
    cities_in_counterfactual_f = cities_in_counterfactual,
    beta_f = beta,
    gamma_f = gamma,
    lambda_f = lambda,
    sigma_f = sigma,
    theta_f = theta
) {
  
  #_______________________________________________________________________________
  ## getting τ_t coefficient variables ####
  # (rho A h) eqn coeff on τ_t (DP eqn 21 / τ_t)
  replication = og_dataset %>%
    rename(geographic_constraint = .data[[geographic_constraint]]) %>%
    mutate(eq_rho_A_h_coeff_on_tau = (gamma_f + theta_f) / ( (sigma_f + beta_f) * (gamma_f + 1) ) ) %>%
    # dividing by (1-l_i)
    mutate(eq_rho_A_h_coeff_on_tau = eq_rho_A_h_coeff_on_tau * ((geographic_constraint)^(gamma_f)/(1-l_i))) %>%
    # now calculating for each year
    mutate(eq_rho_A_h_coeff_on_tau_01 = eq_rho_A_h_coeff_on_tau * (bua_01_pop)^(gamma_f + theta_f - sigma_f - beta_f)) %>%
    mutate(eq_rho_A_h_coeff_on_tau_11 = eq_rho_A_h_coeff_on_tau * (bua_11_pop)^(gamma_f + theta_f - sigma_f - beta_f)) %>%
    mutate(eq_rho_A_h_coeff_on_tau_21 = eq_rho_A_h_coeff_on_tau * (bua_21_pop)^(gamma_f + theta_f - sigma_f - beta_f)) %>%
    # dropping irrelevant var
    select(-eq_rho_A_h_coeff_on_tau)
  
  # y_{it} / τ_t (DP eqn 8 / τ_t)
  replication = replication %>%
    # calculating for each year - multiplying by (1-l_i)
    mutate(income01_div_tau = eq_rho_A_h_coeff_on_tau_01 * (1-l_i) * (bua_01_pop)^(sigma_f + beta_f) ) %>%
    mutate(income11_div_tau = eq_rho_A_h_coeff_on_tau_11 * (1-l_i) * (bua_11_pop)^(sigma_f + beta_f) ) %>%
    mutate(income21_div_tau = eq_rho_A_h_coeff_on_tau_21 * (1-l_i) * (bua_21_pop)^(sigma_f + beta_f) )
  
  # finding consumption c_{it} / τ_t (eqn 22 / τ_t) - same value for old and new model
  replication = replication %>%
    mutate(cons_coeff_on_tau = (gamma_f + theta_f - sigma_f - beta_f) / ( (sigma_f + beta_f) * (gamma_f + 1) ) ) %>%
    mutate(cons_coeff_on_tau = cons_coeff_on_tau * (geographic_constraint)^(gamma_f) ) %>%
    # now calculating for each year
    mutate(cons_coeff_on_tau_01 = cons_coeff_on_tau * (bua_01_pop)^(gamma_f + theta_f)) %>%
    mutate(cons_coeff_on_tau_11 = cons_coeff_on_tau * (bua_11_pop)^(gamma_f + theta_f)) %>%
    mutate(cons_coeff_on_tau_21 = cons_coeff_on_tau * (bua_21_pop)^(gamma_f + theta_f)) %>%
    # removing irrelevant variable
    select(-cons_coeff_on_tau)
  
  # check no missing values
  missing_vals = replication %>%
    filter(if_any(everything(), is.na))
  print("missing values: ")
  print(missing_vals)
  # none - good
  
  #_______________________________________________________________________________
  ## imputing tau ####
  
  # UK real GDP per capita growth
  # https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions
  # regionalgrossdomesticproductgdpbyallinternationalterritoriallevelitlregions - Copy.xlsx
  rgdp01_cvm <<- 30316
  rgdp11_cvm <<- 33428
  rgdp21_cvm <<- 36465
  
  # gdp per capita factor increase
  rgdp_multiple_01_to_11 <<- rgdp11_cvm/rgdp01_cvm
  rgdp_multiple_01_to_11
  # 1.102652
  rgdp_multiple_01_to_21 <<- rgdp21_cvm/rgdp01_cvm
  rgdp_multiple_01_to_21
  # 1.20283
  # both much lower than US per capita growth 1980-2010:  1.658
  
  # normalise tau01_uk to 1
  tau_01_uk <<- 1
  
  # real gdp_t+1 / real gdp_t = 
  # (sum (total income in each city + rural areas)_t+1 / total pop_t+1) /
  # (sum (total income in each city + rural areas)_t / total pop_t)
  # = τ_t+1 (sum (total income in each city + rural areas div τ_t+1)_t+1 / total pop_t+1) /
  # τ_t (sum (total income in each city + rural areas div τ_t)_t / total pop_t)
  # summed variables
  sum_tot_income01_div_tau = sum(replication$income01_div_tau * replication$bua_01_pop)
  sum_tot_income11_div_tau = sum(replication$income11_div_tau * replication$bua_11_pop)
  sum_tot_income21_div_tau = sum(replication$income21_div_tau * replication$bua_21_pop)
  sum_income_pc_01_div_tau = sum_tot_income01_div_tau / urb_01_pop
  sum_income_pc_11_div_tau = sum_tot_income11_div_tau / urb_11_pop
  sum_income_pc_21_div_tau = sum_tot_income21_div_tau / urb_21_pop
  
  # imputed value of tau
  # note this imputation is the same in both the og and new methodology
  tau_11_uk <<- rgdp_multiple_01_to_11 / (sum_income_pc_11_div_tau / sum_income_pc_01_div_tau)
  # 1.083876 with DP's param values
  tau_21_uk <<- rgdp_multiple_01_to_21 / (sum_income_pc_21_div_tau / sum_income_pc_01_div_tau)
  # 1.175209 with DP's param values
  # both much lower than the 1.596 US value over 1980-2010 
  
  #_______________________________________________________________________________
  ## calculating consumption, income values ####
  consumption_income = replication %>%
    # values for equation 21 (rho, A, h)
    mutate(rho_A_h_01 = eq_rho_A_h_coeff_on_tau_01 * tau_01_uk) %>%
    mutate(rho_A_h_11 = eq_rho_A_h_coeff_on_tau_11 * tau_11_uk) %>%
    mutate(rho_A_h_21 = eq_rho_A_h_coeff_on_tau_21 * tau_21_uk) %>%
    # dropping old columns that were coefficients on tau
    select(-starts_with("eq_rho_A_h_coeff_on_tau")) %>%
    # values for equation 8 - y_{it}
    mutate(y_01 = income01_div_tau * tau_01_uk) %>%
    mutate(y_11 = income11_div_tau * tau_11_uk) %>%
    mutate(y_21 = income21_div_tau * tau_21_uk) %>%
    # dropping old columns that were coefficients on tau
    select(-ends_with("_div_tau")) %>%
    # values for equation 22 - c_{it}
    mutate(c_01 = cons_coeff_on_tau_01 * tau_01_uk) %>%
    mutate(c_11 = cons_coeff_on_tau_11 * tau_11_uk) %>%
    mutate(c_21 = cons_coeff_on_tau_21 * tau_21_uk) %>%
    # dropping old columns that were coefficients on tau
    select(-starts_with("cons_coeff_on_tau")) %>%
    
    # calculating growth rate of income over that period
    mutate(y_11_ovr_y_01 = y_11 / y_01) %>%
    mutate(y_21_ovr_y_01 = y_21 / y_01)
  
  # generating variables for consumption as share of income
  c_ovr_y_data = consumption_income %>%
    mutate(
      c_ovr_y_01 = c_01 / y_01,
      c_ovr_y_11 = c_11 / y_11,
      c_ovr_y_21 = c_21 / y_21
    )
  assign("c_ovr_y_data", c_ovr_y_data, envir = .GlobalEnv)
  
  summed_c_y_vars = consumption_income %>%  
    summarise(
      c_all_cities_01 = sum(c_01),
      c_all_cities_11 = sum(c_11),
      c_all_cities_21 = sum(c_21),
      y_all_cities_01 = sum(y_01),
      y_all_cities_11 = sum(y_11),
      y_all_cities_21 = sum(y_21)
    )
  c_ovr_y_01 <<- summed_c_y_vars$c_all_cities_01 / summed_c_y_vars$y_all_cities_01
  print(paste0("c_ovr_y_01: ", c_ovr_y_01))
  c_ovr_y_11 <<- summed_c_y_vars$c_all_cities_11 / summed_c_y_vars$y_all_cities_11
  print(paste0("c_ovr_y_11: ", c_ovr_y_11))
  c_ovr_y_21 <<- summed_c_y_vars$c_all_cities_21 / summed_c_y_vars$y_all_cities_21
  print(paste0("c_ovr_y_21: ", c_ovr_y_21))
  
  # rural income
  # y_{rt} is income in marginal populated city (eqn 22)
  # for marginal city, c_{it} = c_{rt} = y_{rt}
  # so c_{rt} / y_{rt} = 1 for all t
  rur_01_income <<- min(consumption_income$c_01)
  rur_11_income <<- min(consumption_income$c_11)
  rur_21_income <<- min(consumption_income$c_21)
  
  # calculating permitting costs and ordering by 2001 population
  consumption_income_w_permitting_ordered = consumption_income %>%
    mutate(perm_01 = c_01 - rur_01_income) %>%
    mutate(perm_11 = c_11 - rur_11_income) %>%
    mutate(perm_21 = c_21 - rur_21_income) %>%
    # sorting by largest 2001 population first
    arrange(desc(bua_01_pop)) %>%
    # now generating variables permitting costs as share of income
    mutate(perm_ovr_y_01 = perm_01 / y_01) %>%
    mutate(perm_ovr_y_11 = perm_11 / y_11) %>%
    mutate(perm_ovr_y_21 = perm_21 / y_21)
  
  # creating another version of dataset with only 2001 and 2021 values
  consumption_income_w_permitting_01_21_only = consumption_income_w_permitting_ordered %>%
    select(-contains("_11"))
  
  # rural productivity
  # rearranging equation 40 <=> eqn 11
  rur_01_prod <<- rur_01_income / ((rur_01_pop)^(-lambda_f))
  rur_11_prod <<- rur_11_income / ((rur_11_pop)^(-lambda_f))
  rur_21_prod <<- rur_21_income / ((rur_21_pop)^(-lambda_f))
  
  start_counterfactual_data <<- consumption_income_w_permitting_01_21_only %>%
    # creating indicator variable for the cities to be counterfactually evaluated
    mutate(in_counterfact = as.integer(BUA22NM %in% cities_in_counterfactual_f)) %>%
    # incumbent population in each city (min of old generation (i.e. 2001() pop or current (i.e. 2021) pop)
    mutate(pop_21_incumb = pmin(bua_01_pop, bua_21_pop))
  
  # summarising permitting rate data
  summary(start_counterfactual_data$bua_perm_rate_01_21)
  
  # incumbent rural population
  rur_21_pop_incumb <<- tot_21_pop - sum(start_counterfactual_data$pop_21_incumb)
  # note by construction, total incumbent population is the 2021 total UK pop 
  
  # end of function
}


#_______________________________________________________________________________
# function to generate counterfactual income, consumption, etc. #####
counterfactual_function = function(
    counterfactual_dataset = start_counterfactual_data,
    cities_in_counterfactual_vector = cities_in_counterfactual,
    perm_rate_change_factor_vector = perm_rate_change_factor,
    beta_f = beta,
    gamma_f = gamma,
    lambda_f = lambda,
    sigma_f = sigma,
    theta_f = theta,
    # whether we're changing based on the factor list or not 
    # if not, we increase permitting rates up to the 75th percentile value 
    factor_change = TRUE,
    # save results to global environment?
    store_results = FALSE
) {
  
  # Create empty lists to store results
  cfact_fct_results_list = list()
  
  # Calculate the 75th percentile of bua_perm_rate_01_21 if needed
  percentile_75th_perm_rate = quantile(counterfactual_dataset$bua_perm_rate_01_21, 0.75)
  
  # Define factors to process
  if (factor_change) {
    # If using factor change, use the provided vector
    factors_to_process = perm_rate_change_factor_vector
  } else {
    # If using 75th percentile, only need to do one calculation
    # Just use a single placeholder factor (we'll ignore its value)
    factors_to_process = 0  # Just a placeholder
  }
  
  # creating counterfactual 2001 permitting costs
  for (factor in factors_to_process) {
    print(paste0("start of factor: ", factor))
    
    if (factor_change) {
      # Get the original values for the cities in the counterfactual
      affected_cities = counterfactual_dataset %>% 
        filter(in_counterfact == 1) %>%
        select(BUA22NM, bua_perm_rate_01_21)
      
      print("Original permitting rates for affected cities:")
      print(affected_cities)
      
      # Original calculation using factor - increase by factor amount
      modified_counterfactual_dataset = counterfactual_dataset %>%
        mutate(perm_rate_counterfact = ifelse(in_counterfact == 1,
                                              bua_perm_rate_01_21 * (1 + factor),
                                              bua_perm_rate_01_21))
      
      # Get the new values for the cities in the counterfactual
      affected_cities_after = modified_counterfactual_dataset %>% 
        filter(in_counterfact == 1) %>%
        select(BUA22NM, bua_perm_rate_01_21, perm_rate_counterfact)
      
      print("New permitting rates for affected cities:")
      print(affected_cities_after)
    } else {
      # Use 75th percentile when factor_change is FALSE
      # Set perm_rate_counterfact to max of current value and 75th percentile
      # Output the 75th percentile value being used
      print(paste0("75th percentile of permitting rates: ", percentile_75th_perm_rate))
      
      # Get the original values for the cities in the counterfactual
      affected_cities = counterfactual_dataset %>% 
        filter(in_counterfact == 1) %>%
        select(BUA22NM, bua_perm_rate_01_21)
      
      print("Original permitting rates for affected cities:")
      print(affected_cities)
      
      # Apply the counterfactual permitting rates
      modified_counterfactual_dataset = counterfactual_dataset %>%
        mutate(perm_rate_counterfact = ifelse(in_counterfact == 1,
                                              pmax(bua_perm_rate_01_21, percentile_75th_perm_rate),
                                              bua_perm_rate_01_21))
      
      # Get the new values for the cities in the counterfactual
      affected_cities_after = modified_counterfactual_dataset %>% 
        filter(in_counterfact == 1) %>%
        select(BUA22NM, bua_perm_rate_01_21, perm_rate_counterfact)
      
      print("New permitting rates for affected cities:")
      print(affected_cities_after)
    }
    
    # creating counterfactual population
    # DP's stata code is 
    # replace N = int((`permit_rate_cfact' / permit_rate_1980_2010) * (pop2010 - pop1980) + pop1980) if affected_cfact == 1
    # where `permit_rate_...` is the total number of permits issued between 1980-2010 per initial (1980) housing unit 
    modified_counterfactual_dataset = modified_counterfactual_dataset %>%
      mutate(bua_21_pop_counterfact = ifelse(in_counterfact == 1,
                                             (perm_rate_counterfact / bua_perm_rate_01_21)*(bua_21_pop - bua_01_pop) + bua_01_pop,
                                             bua_21_pop))
    
    # dropping cities one-by-one to find the new marginal city ####
    
    # initialise counting variables
    num_cities = nrow(modified_counterfactual_dataset)
    marginal_city_index = num_cities
    consumption_in_current_marginal_city_loop = rur_21_income
    consumption_in_prev_marginal_city_loop = rur_21_income + 1
    i = 1
    
    # looping to iteratively drop cities
    while (i < num_cities && marginal_city_index > 1) {
      
      # initialising vectors
      counterfactual_data_temp_marginal_city_calc = modified_counterfactual_dataset %>%
        
        # Reset counterfactual population
        mutate(pop_21_counterfact_loop = bua_21_pop_counterfact) %>%
        
        # Counterfactual income and incumbent consumption
        mutate(y_21_counterfact_loop = ( y_21 / (bua_21_pop)^(sigma_f+beta_f) ) * (pop_21_counterfact_loop)^(sigma_f+beta_f)) %>%
        mutate(c_21_counterfact_loop = y_21_counterfact_loop - (1 / (gamma_f + 1)) * tau_21_uk * geographic_constraint^gamma_f * (pop_21_counterfact_loop^(gamma_f + theta_f))) %>%
        
        # Sort cities from highest to lowest incumbent consumption
        arrange(desc(c_21_counterfact_loop)) %>%
        mutate(city_order = rank(-c_21_counterfact_loop))
      
      # Cumulative urban population
      pop_cum_21_counterfact_loop = sum(counterfactual_data_temp_marginal_city_calc$pop_21_counterfact_loop)
      
      counterfactual_data_temp_marginal_city_calc = counterfactual_data_temp_marginal_city_calc %>%
        
        # Urban population cannot exceed total pop and only cities above current marginal try are populated
        mutate(pop_21_counterfact_loop = ifelse(pop_cum_21_counterfact_loop > tot_21_pop | city_order > marginal_city_index,
                                                NA,
                                                pop_21_counterfact_loop)) %>%
        
        # Calculate rural consumption if each city were the marginal one
        mutate(c_21_rural_threshold = rur_21_prod * ((tot_21_pop - pop_cum_21_counterfact_loop)^(-lambda_f)))
      
      # Update incumbent consumption in marginal city
      consumption_in_prev_marginal_city_loop = consumption_in_current_marginal_city_loop
      consumption_in_current_marginal_city_loop = counterfactual_data_temp_marginal_city_calc %>%
        filter(city_order == marginal_city_index) %>%
        pull(c_21_counterfact_loop) %>%
        mean(na.rm = TRUE)
      
      # print info
      print(paste0("iteration: ",i,
                   ", current cons: ",consumption_in_current_marginal_city_loop,
                   ", prev cons: ",consumption_in_prev_marginal_city_loop,
                   ", cities: ",marginal_city_index))
      
      # Update marginal city consumption
      consumption_in_prev_marginal_city_loop = consumption_in_current_marginal_city_loop
      
      # if marginal city implies:
      # urb pop > UK pop
      # or rural cons > marg city cons
      # take out 1 more city
      marginal_city_index = with(counterfactual_data_temp_marginal_city_calc, {
        if (is.na(pop_21_counterfact_loop[marginal_city_index])) {
          marginal_city_index - 1
        } else if (c_21_rural_threshold[marginal_city_index] > 
                   c_21_counterfact_loop[marginal_city_index]) {
          marginal_city_index - 1
        } else {
          0
        }
      })
      
      i = i + 1
      
      # end of while loop through cities and marginal consumption
    }
    
    # set empty cities
    counterfactual_data_temp_marginal_city_calc = counterfactual_data_temp_marginal_city_calc %>%
      mutate(y_21_counterfact_loop = ifelse(is.na(pop_21_counterfact_loop),
                                            NA,
                                            y_21_counterfact_loop)) %>%
      mutate(c_21_counterfact_loop = ifelse(is.na(pop_21_counterfact_loop),
                                            NA,
                                            c_21_counterfact_loop))
    
    # cf rural consumption per capita
    # (rural income per capita = consumption)
    # as per DP, cf rur cons is maximum rural consumption value based on marginal city
    c_21_rur_counterfact = max(counterfactual_data_temp_marginal_city_calc$c_21_rural_threshold, na.rm = TRUE)
    print(paste0("c_21_rur_counterfact: ", c_21_rur_counterfact))
    print(paste0("rur_21_income: ", rur_21_income))
    
    # percentage change in consumption for rural and newcomers
    pct_chg_c_rur = 100 * (c_21_rur_counterfact - rur_21_income) / rur_21_income
    print(paste0("pct_chg_c_rur: ", pct_chg_c_rur))
    
    # cf permitting costs
    counterfactual_data_temp_marginal_city_calc = counterfactual_data_temp_marginal_city_calc %>%
      mutate(perm_21_counterfact = c_21_counterfact_loop - max(c_21_rural_threshold, na.rm = TRUE)) %>%
      # counterfactual incumbent population 
      # it's minimum of counterfactual and original incumbent pop
      # pmin is the parallel minimum function that compares vectors elementwise, returning a vector of the smallest values at each position
      mutate(pop_21_incumb_counterfact = pmin(pop_21_counterfact_loop, pop_21_incumb))
    
    # cf rural pop
    # rural only
    pop_cum_21_counterfact_loop = sum(counterfactual_data_temp_marginal_city_calc$pop_21_counterfact_loop, na.rm = TRUE)
    pop_21_rur_counterfact = tot_21_pop - pop_cum_21_counterfact_loop
    print(paste0("pop_21_rur_counterfact: ", pop_21_rur_counterfact))
    
    # includes urban newcomers
    pop_cum_21_counterfact_loop = sum(counterfactual_data_temp_marginal_city_calc$pop_21_incumb_counterfact, na.rm = TRUE)
    pop_21_rur_incumb_counterfact = tot_21_pop - pop_cum_21_counterfact_loop
    
    # percentage change in rural population
    pct_chg_pop_rur = 100 * (pop_21_rur_counterfact - rur_21_pop) / rur_21_pop
    print(paste0("pct_chg_pop_rur: ", pct_chg_pop_rur))
    
    ### baseline per-capita income and consumption ####
    # weighted by population
    y_21_baseline = with(counterfactual_data_temp_marginal_city_calc,
                         sum(y_21 * (bua_21_pop / tot_21_pop), na.rm = TRUE)
    ) + rur_21_income * (rur_21_pop / tot_21_pop)
    print(paste0("y_21_baseline: ", y_21_baseline))
    
    # note consumption uses incumbent population that includes urban newcomers
    # note by construction, *total* incumbent pop is just total UK pop in 2021
    c_21_baseline = with(counterfactual_data_temp_marginal_city_calc,
                         sum(c_21 * (pop_21_incumb / tot_21_pop), na.rm = TRUE)
    ) + rur_21_income * (rur_21_pop_incumb / tot_21_pop)
    print(paste0("c_21_baseline: ", c_21_baseline))
    
    ### counterfactual per-capita income (weighted by population) & %chg ####
    y_21_counterfactual = with(counterfactual_data_temp_marginal_city_calc,
                               sum(y_21_counterfact_loop * (pop_21_counterfact_loop / tot_21_pop), na.rm = TRUE)
    ) + c_21_rur_counterfact * (pop_21_rur_counterfact / tot_21_pop)
    print(paste0("y_21_counterfactual: ", y_21_counterfactual))
    
    # percentage change in income
    pct_chg_y_tot = 100 * (y_21_counterfactual - y_21_baseline) / y_21_baseline
    print(paste0("pct_chg_y_tot: ", pct_chg_y_tot))
    
    ### per-capita consumption (weighted by population) & %chg ####
    # note consumption uses incumbent population that includes urban newcomers
    # counterfactual
    c_21_counterfactual = with(counterfactual_data_temp_marginal_city_calc,
                               sum(c_21_counterfact_loop * (pop_21_incumb_counterfact / tot_21_pop), na.rm = TRUE)
    ) + c_21_rur_counterfact * (pop_21_rur_incumb_counterfact / tot_21_pop)
    print(paste0("c_21_counterfactual: ", c_21_counterfactual))
    
    # percentage change in consumption
    pct_chg_c_tot = 100 * (c_21_counterfactual - c_21_baseline) / c_21_baseline
    print(paste0("pct_chg_c_tot: ", pct_chg_c_tot))
    
    # generating variables for consumption as share of income (counterfactual)
    cf_summed_c_y_vars = counterfactual_data_temp_marginal_city_calc %>%
      summarise(
        cf_c_all_cities_01 = sum(c_01),
        cf_c_all_cities_21 = sum(c_21),
        cf_y_all_cities_01 = sum(y_01),
        cf_y_all_cities_21 = sum(y_21)
      )
    cf_c_ovr_y_01 <<- cf_summed_c_y_vars$cf_c_all_cities_01 / cf_summed_c_y_vars$cf_y_all_cities_01
    print(paste0("cf_c_ovr_y_01: ", cf_c_ovr_y_01))
    cf_c_ovr_y_21 <<- cf_summed_c_y_vars$cf_c_all_cities_21 / cf_summed_c_y_vars$cf_y_all_cities_21
    print(paste0("cf_c_ovr_y_21: ", cf_c_ovr_y_21))
    # always just 1 - (sigma + beta)/(gamma + theta)
    
    # percentage changes for individual cities
    counterfactual_data_temp_marginal_city_calc = counterfactual_data_temp_marginal_city_calc %>%
      mutate(pct_chg_y = 100 * (y_21_counterfact_loop - y_21) / y_21) %>%
      mutate(pct_chg_c = 100 * (c_21_counterfact_loop - c_21) / c_21) %>%
      mutate(pct_chg_perm_rate = 100 * (perm_rate_counterfact - bua_perm_rate_01_21) / bua_perm_rate_01_21)
    
    # Track results in global tibble
    append_counterfactual_results(
      sigma_f = sigma_f, 
      beta_f = beta_f, 
      gamma_f = gamma_f, 
      theta_f = theta_f, 
      lambda_f = lambda_f,
      factor_change = factor_change, 
      factor_value = ifelse(factor_change, factor, percentile_75th_perm_rate),
      cities_in_counterfactual_vector = cities_in_counterfactual_vector,
      c_21_rur_counterfact = c_21_rur_counterfact,
      rur_21_income = rur_21_income,
      pop_21_rur_counterfact = pop_21_rur_counterfact,
      pct_chg_pop_rur = pct_chg_pop_rur,
      y_21_baseline = y_21_baseline, 
      c_21_baseline = c_21_baseline,
      y_21_counterfactual = y_21_counterfactual, 
      pct_chg_y_tot = pct_chg_y_tot,
      c_21_counterfactual = c_21_counterfactual,
      pct_chg_c_tot= pct_chg_c_tot,
      cf_c_ovr_y_01 = cf_c_ovr_y_01,
      cf_c_ovr_y_21 = cf_c_ovr_y_21,
      pct_chg_c_rur = pct_chg_c_rur
    )
    
    # Store results for this factor
    cfact_fct_results_list[[as.character(factor)]] = list(
      sigma_f = sigma_f, 
      beta_f = beta_f, 
      gamma_f = gamma_f, 
      theta_f = theta_f, 
      lambda_f = lambda_f,
      counterfactual_data_temp_marginal_city_calc = counterfactual_data_temp_marginal_city_calc,
      cities_in_counterfactual_vector = cities_in_counterfactual_vector,
      c_21_rur_counterfact = c_21_rur_counterfact,
      rur_21_income = rur_21_income,
      pop_21_rur_counterfact = pop_21_rur_counterfact,
      pct_chg_pop_rur = pct_chg_pop_rur,
      y_21_baseline = y_21_baseline, 
      c_21_baseline = c_21_baseline,
      y_21_counterfactual = y_21_counterfactual, 
      pct_chg_y_tot = pct_chg_y_tot,
      c_21_counterfactual = c_21_counterfactual,
      pct_chg_c_tot= pct_chg_c_tot,
      cf_c_ovr_y_01 = cf_c_ovr_y_01,
      cf_c_ovr_y_21 = cf_c_ovr_y_21,
      pct_chg_c_rur = pct_chg_c_rur
    )
    
    ### Store results in presentable table ####
    #___________________________________________________________________________
    presentable_table = counterfactual_data_temp_marginal_city_calc %>%
      filter(in_counterfact == 1) %>%
      select(c(BUA22NM,
               pct_chg_perm_rate,
               bua_21_pop,
               bua_21_pop_counterfact,
               pct_chg_y,
               pct_chg_c
      )) %>%
      # dividing by 100 as excel/sheets, when formatting x into percentage, 
      # treats the conversion as 100x%
      mutate(
        pct_chg_perm_rate = pct_chg_perm_rate/100,
        pct_chg_y = pct_chg_y/100,
        pct_chg_c = pct_chg_c/100
      ) %>%
      # add new column for increase in consumption of newcomers (formerly rural ppl)
      mutate(pct_chg_c_newcomers = pct_chg_c_rur/100)
    
    # Create a new row for rural areas
    rural_row = tibble(
      BUA22NM = "Rural areas",
      pct_chg_perm_rate = NA,
      bua_21_pop = rur_21_pop,
      bua_21_pop_counterfact = pop_21_rur_counterfact,
      pct_chg_y = pct_chg_c_rur/100,
      pct_chg_c = NA,
      pct_chg_c_newcomers = NA
    )
    
    # Combine the original table with the new row
    presentable_table = bind_rows(presentable_table, rural_row)
    
    # end of for loop through cost reduction factors
    print(paste0("end of factor: ", factor))
    
    # Save to global environment
    if (store_results) {
      if (factor_change) {
        assign(
          paste0(
            "fact_",factor,
            "_cf_",
            length(cities_in_counterfactual_vector),"_cities_",
            "_sb",(sigma_f+beta_f)*100,
            "_gt",(gamma_f+theta_f)*100,
            "_l",lambda_f*100), 
          cfact_fct_results_list, 
          envir = .GlobalEnv
        )
        
        assign(
          paste0(
            "tbl_fact_",factor,
            "_cf_",
            length(cities_in_counterfactual_vector),"_cities_",
            "_sb",(sigma_f+beta_f)*100,
            "_gt",(gamma_f+theta_f)*100,
            "_l",lambda_f*100), 
          presentable_table, 
          envir = .GlobalEnv
        )    
        
        write_csv(presentable_table,
                  paste0(
                    "Outputs/tbl_fact_",factor,
                    "_cf_",
                    length(cities_in_counterfactual_vector),"_cities_",
                    "_sb",(sigma_f+beta_f)*100,
                    "_gt",(gamma_f+theta_f)*100,
                    "_l",lambda_f*100,
                    ".csv"
                  ))
      } else {
        assign(
          paste0(
            "q75_cf_",
            length(cities_in_counterfactual_vector),"_cities_",
            "_sb",(sigma_f+beta_f)*100,
            "_gt",(gamma_f+theta_f)*100,
            "_l",lambda_f*100),
          cfact_fct_results_list, 
          envir = .GlobalEnv
        )
        
        assign(
          paste0(
            "tbl_q75_cf_",
            length(cities_in_counterfactual_vector),"_cities_",
            "_sb",(sigma_f+beta_f)*100,
            "_gt",(gamma_f+theta_f)*100,
            "_l",lambda_f*100),
          presentable_table, 
          envir = .GlobalEnv
        )
        
        write_csv(presentable_table,
                  paste0(
                    "Outputs/tbl_q75_cf_",
                    length(cities_in_counterfactual_vector),"_cities_",
                    "_sb",(sigma_f+beta_f)*100,
                    "_gt",(gamma_f+theta_f)*100,
                    "_l",lambda_f*100,
                    ".csv"
                  ))
      }
    }
  }
  
  # end of function
}


#_______________________________________________________________________________
# iterating over parameter values ####
# we can vary the parameters as robustness checks

# varying size of increase in permitting rate
perm_rate_change_factor = c(0.05,
                            0.1,
                            0.25,
                            0.3,
                            0.5)

# varying cities to be counterfactually evaluated
# cities to iterate over
cities_iteration = list(
  agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints$BUA22NM[1:10],
  agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints$BUA22NM[1:5], # top 5
  agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints$BUA22NM[1:2], # top 2 (London, Birmingham)
  agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints$BUA22NM[1], # london only
  c("Greater London", "Oxford", "Cambridge (Cambridge)")
)

cities_in_counterfactual = cities_iteration[[1]]

# DP do:
# γ+θ = 0.09,0.11,0.13
# σ+β = 0.04,0.06,0.08
# note second order condition restrictions:
# γ + θ − σ − β > 0 and σ + β > 0
# using the same combinations as D+P

alt_parameter_options = tibble(
  sigma_options = c(
    0.02, 0.03, 0.04, 0.02, 0.03, 0.02, 0.03, 0.04
  ),
  beta_options = c(
    0.02, 0.03, 0.04, 0.02, 0.03, 0.02, 0.03, 0.04
  ),
  gamma_options = c(
    0.06, 0.06, 0.06, 0.07, 0.07, 0.08, 0.08, 0.08
  ),
  theta_options = c(
    0.03, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05, 0.05
  )
)

# Create a global tibble to store results
if (!exists("counterfactual_results_tibble")) {
  counterfactual_results_tibble = tibble(
    # Parameters
    sigma = numeric(),
    beta = numeric(), 
    gamma = numeric(),
    theta = numeric(),
    lambda = numeric(),
    factor_change = logical(),
    factor_value = numeric(),
    cities_count = numeric(),
    cities_list = list(),
    
    # Results
    c_21_rur_counterfact = numeric(),
    rur_21_income = numeric(),
    pop_21_rur_counterfact = numeric(),
    pct_chg_pop_rur = numeric(),
    y_21_baseline = numeric(), 
    c_21_baseline = numeric(),
    y_21_counterfactual = numeric(),
    pct_chg_y_tot = numeric(), 
    c_21_counterfactual = numeric(),
    pct_chg_c_tot= numeric(),
    cf_c_ovr_y_01 = numeric(),
    cf_c_ovr_y_21 = numeric(),
    pct_chg_c_rur = numeric()
  )
}

# Function to append results to global tibble
append_counterfactual_results = function(
    sigma_f, 
    beta_f, 
    gamma_f, 
    theta_f, 
    lambda_f,
    factor_change, 
    factor_value,
    cities_in_counterfactual_vector,
    c_21_rur_counterfact,
    rur_21_income,
    pop_21_rur_counterfact,
    pct_chg_pop_rur,
    y_21_baseline, 
    c_21_baseline,
    y_21_counterfactual, 
    pct_chg_y_tot,
    c_21_counterfactual,
    pct_chg_c_tot,
    cf_c_ovr_y_01,
    cf_c_ovr_y_21,
    pct_chg_c_rur
) {
  
  # Create new row
  new_row = tibble(
    sigma = sigma_f,
    beta = beta_f,
    gamma = gamma_f,
    theta = theta_f,
    lambda = lambda_f,
    factor_change = factor_change,
    factor_value = factor_value,
    cities_count = length(cities_in_counterfactual_vector),
    cities_list = list(cities_in_counterfactual_vector),
    c_21_rur_counterfact = c_21_rur_counterfact,
    rur_21_income = rur_21_income,
    pop_21_rur_counterfact = pop_21_rur_counterfact,
    pct_chg_pop_rur = pct_chg_pop_rur,
    y_21_baseline = y_21_baseline, 
    c_21_baseline = c_21_baseline,
    y_21_counterfactual = y_21_counterfactual, 
    pct_chg_y_tot = pct_chg_y_tot,
    c_21_counterfactual = c_21_counterfactual,
    pct_chg_c_tot= pct_chg_c_tot,
    cf_c_ovr_y_01 = cf_c_ovr_y_01,
    cf_c_ovr_y_21 = cf_c_ovr_y_21,
    pct_chg_c_rur = pct_chg_c_rur
  )
  
  # Append to global tibble
  counterfactual_results_tibble <<- bind_rows(counterfactual_results_tibble, new_row)
  
  # Print basic results
  message(sprintf("Row added to results tibble: σ+β=%.2f, γ+θ=%.2f, cities=%d", 
                  sigma_f+beta_f, gamma_f+theta_f, length(cities_in_counterfactual_vector)))
  message(paste0("Consumption change: ", pct_chg_c_rur))
  
  return(invisible(NULL))
}

#_______________________________________________________________________________
## main parameter options - DP's original ####
data_for_cf_fn(
  og_dataset = agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
  cities_in_counterfactual_f = cities_in_counterfactual,
  beta_f = beta_usa,
  gamma_f = gamma_usa,
  lambda_f = lambda_usa,
  sigma_f = sigma_usa,
  theta_f = theta_usa
)

write_csv(start_counterfactual_data,
          "Outputs/dp_original_params_counterfactual_data.csv")

dp_original_params_counterfactual_data = start_counterfactual_data

counterfactual_function(
  counterfactual_dataset = start_counterfactual_data,
  cities_in_counterfactual_vector = cities_in_counterfactual,
  perm_rate_change_factor_vector = perm_rate_change_factor,
  beta_f = beta_usa,
  gamma_f = gamma_usa,
  lambda_f = lambda_usa,
  sigma_f = sigma_usa,
  theta_f = theta_usa,
  # whether we're changing based on the factor list or not 
  # if not, we increase permitting rates up to the 75th percentile value 
  factor_change = FALSE,
  # save results to global environment?
  store_results = TRUE
)

#_______________________________________________________________________________
## original parameters - varying cities ####
for (i in cities_iteration) {
  data_for_cf_fn(
    og_dataset = agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
    cities_in_counterfactual_f = i,
    beta_f = beta_usa,
    gamma_f = gamma_usa,
    lambda_f = lambda_usa,
    sigma_f = sigma_usa,
    theta_f = theta_usa
  )
  
  counterfactual_function(
    counterfactual_dataset = start_counterfactual_data,
    cities_in_counterfactual_vector = i,
    perm_rate_change_factor_vector = perm_rate_change_factor,
    beta_f = beta_usa,
    gamma_f = gamma_usa,
    lambda_f = lambda_usa,
    sigma_f = sigma_usa,
    theta_f = theta_usa,
    # whether we're changing based on the factor list or not 
    # if not, we increase permitting rates up to the 75th percentile value 
    factor_change = FALSE,
    # save results to global environment?
    store_results = TRUE
  )
}

#_______________________________________________________________________________
## original parameters - using cost reduction factors ####
data_for_cf_fn(
  og_dataset = agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
  cities_in_counterfactual_f = cities_in_counterfactual,
  beta_f = beta_usa,
  gamma_f = gamma_usa,
  lambda_f = lambda_usa,
  sigma_f = sigma_usa,
  theta_f = theta_usa
)

counterfactual_function(
  counterfactual_dataset = start_counterfactual_data,
  cities_in_counterfactual_vector = cities_in_counterfactual,
  perm_rate_change_factor_vector = perm_rate_change_factor,
  beta_f = beta_usa,
  gamma_f = gamma_usa,
  lambda_f = lambda_usa,
  sigma_f = sigma_usa,
  theta_f = theta_usa,
  # whether we're changing based on the factor list or not 
  # if not, we increase permitting rates up to the 75th percentile value 
  factor_change = TRUE,
  # save results to global environment?
  store_results = TRUE
)


#_______________________________________________________________________________
## varying sigma, beta, gamma, theta #####

for (param_idx in 1:nrow(alt_parameter_options)) {
  # adjusting parameters
  gamma = alt_parameter_options$gamma_options[param_idx]
  theta = alt_parameter_options$theta_options[param_idx]
  sigma = alt_parameter_options$sigma_options[param_idx]
  beta = alt_parameter_options$beta_options[param_idx]
  
  data_for_cf_fn(
    og_dataset = agg_pop_01_11_21_ovr_city_threshold_rounded_rm_geog_constraints,
    cities_in_counterfactual_f = cities_in_counterfactual,
    beta_f = beta,
    gamma_f = gamma,
    lambda_f = lambda_usa,
    sigma_f = sigma,
    theta_f = theta
  )
  
  counterfactual_function(
    counterfactual_dataset = start_counterfactual_data,
    cities_in_counterfactual_vector = cities_in_counterfactual,
    perm_rate_change_factor_vector = perm_rate_change_factor,
    beta_f = beta,
    gamma_f = gamma,
    lambda_f = lambda_usa,
    sigma_f = sigma,
    theta_f = theta,
    # whether we're changing based on the factor list or not 
    # if not, we increase permitting rates up to the 75th percentile value 
    factor_change = FALSE,
    # save results to global environment?
    store_results = TRUE
  )
  
}

# viewing final results
view(counterfactual_results_tibble)

# consumption as share of income
# https://www.perplexity.ai/search/consumption-as-share-of-income-My2BWGVtQGiqmyN8TRgE6g
# https://ifs.org.uk/publications/measuring-living-standards-income-and-consumption-evidence-uk-1
# https://www.stats.gov.cn/english/PressRelease/202410/t20241025_1957142.html
# https://www.bea.gov/system/files/papers/BEA-WP2023-1.pdf


# replicating results of DP's original paper ####
## plots ####
top_10_buas = dp_original_params_counterfactual_data %>%
  arrange(desc(bua_21_pop)) %>%
  slice_head(n = 10)

perm_rate_vs_city_pop = ggplot(dp_original_params_counterfactual_data, 
                aes(x = bua_21_pop, y = 1/bua_perm_rate_01_21)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = F) +
  geom_text_repel(
    data = top_10_buas, 
    aes(label = BUA22NM),
    size = 3.5,
    box.padding = 0.5,
    point.padding = 0.2,
    force = 10
  ) +
  labs(
    # title = "Planning regulations and city population",
    x = "2021 population (log scale)",
    y = "Reciprocal of permitting rate \n(2001-2021)"
  ) +
  theme_minimal() +
  scale_x_log10()

geog_constr_vs_perm_rate = ggplot(dp_original_params_counterfactual_data, 
                aes(x = 1 - 1/geographic_constraint, y = 1/bua_perm_rate_01_21)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = F) +
  geom_text_repel(
    data = top_10_buas, 
    aes(label = BUA22NM),
    size = 3.5,
    box.padding = 0.5,
    point.padding = 0.2,
    force = 10
  ) +
  labs(
    # title = "Planning regulations and geographical constraints",
    x = "Estimated % geographically-constrained land (log scale)",
    y = "Reciprocal of permitting rate \n(2001-2021)"
  ) +
  theme_minimal() +
  scale_x_log10(labels = scales::percent)

print(perm_rate_vs_city_pop)
print(geog_constr_vs_perm_rate)
ggsave("Outputs/perm_rate_vs_city_pop.png", 
       perm_rate_vs_city_pop,
       units = "cm",
       width = 16,
       height = 12,
       dpi = 320)
ggsave("Outputs/geog_constr_vs_perm_rate.png", 
       geog_constr_vs_perm_rate,
       units = "cm",
       width = 16,
       height = 12,
       dpi = 320)

## regressions ####
# Permitting rate vs log population
perm_rate_vs_city_pop_reg = lm(I(1/bua_perm_rate_01_21) ~ log(bua_21_pop), data = dp_original_params_counterfactual_data)

# Permitting rate vs log share of geographically constrained land
geog_constr_vs_perm_rate_reg = lm(I(1/bua_perm_rate_01_21) ~ log(1 - 1/geographic_constraint), data = dp_original_params_counterfactual_data)

# outputting models
summary_perm_rate_vs_city_pop_reg = summary(perm_rate_vs_city_pop_reg)
summary_geog_constr_vs_perm_rate_reg = summary(geog_constr_vs_perm_rate_reg)
summary_perm_rate_vs_city_pop_reg
summary_geog_constr_vs_perm_rate_reg

# creating latex table
stargazer(perm_rate_vs_city_pop_reg, 
          geog_constr_vs_perm_rate_reg, 
          title = "Empirically testing model predictions",
          # column.labels = c("Log 2021 Population", "Log Geographic Constraint"),
          column.separate = c(1, 1),
          dep.var.labels = "Reciprocal of permitting rate (2001-2021)",
          covariate.labels = c("Log population", "Log \\% of geographically constrained land", "Constant"),
          align = TRUE,
          model.numbers = TRUE,
          label = "tab:reg tab",
          type = "latex",
          out = "Outputs/regression_results.tex")

