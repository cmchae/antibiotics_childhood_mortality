# _____________________________________________ ----------
# 0. DHS data cleaning ----------------------------------------------------


rm(list = ls()); gc()

requiredPackages = c("rdhs", "dplyr", "tidyr", "gt", "zoo", "haven", "data.table", "labelled", "naniar", "survival", "flexsurv", "MASS", "purrr")

lib.path <- .libPaths()
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p, lib = lib.path)
  library(p,character.only = TRUE)
}

library(purrr)
library(tibble)
library(labelled)
library(stringr)
library(purrr)

set.seed(123); options(scipen = 999, digits = 2)

datasets_dir <- "data/DHS_datasets"
local_files <- list.files(datasets_dir, pattern = "\\.rds$", full.names = TRUE)

# Variables to keep
keep_variables <- c(
  "caseid", "v000", "v001", "v007", "v008", "v191", "bord", "b3", "b4", "b5", "b6", "b7", "b8", "bidx",
  "h22", "h31", "h31b", "h31c", "h32y", "h32z", "h37i", "h37j", "h37n", "h37o", "h37p",
  "v005", "v025", "v106", "v190", "v481",
  "h37a", "h37b", "h37c", "h37d", "h37da", "h37e", "h37aa", "h37ab", "h37f", "h37g", "h37h"
)

# Function to load and filter a dataset
load_and_filter <- function(file) {
  dat <- as.data.table(readRDS(file))
  dat <- dat[, intersect(keep_variables, names(dat)), with = FALSE]
  dat$filename <- tools::file_path_sans_ext(basename(file))
  dat
}

# Load all datasets into a named list: 167 
extract <- lapply(local_files, load_and_filter)
names(extract) <- sapply(local_files, function(x) tools::file_path_sans_ext(basename(x)))

for (i in 1:length(extract)) {
  year_val <- min(unique(extract[[i]]$v007), na.rm = TRUE)
  file_label <- names(extract)[i]
  cat("i =", i, "filename =", file_label, "Year =", year_val, "\n")
}

length(extract) #165 (2003~2023)
datasets_to_remove <- c("ETKR61FL") # remove 2003 year only data (remaining: #164)
extract <- extract[!names(extract) %in% datasets_to_remove]



# assigning correct survey year (Afghanistan and Nepal: assigning survey starting year)
extract$AFKR71FL$v007 <- 2015
extract$NPKR61FL$v007 <- 2011
extract$NPKR7HFL$v007 <- 2016
extract$NPKR82FL$v007 <- 2022

# excluding DHS ver.4: -19 (remaining: 145)
version.4 <- names(extract)[substr(names(extract), 5, 5) == "4"]
length(version.4)
extract <- extract[!names(extract) %in% version.4] # Remove the specified datasets from the "extract" list
length(extract)

## datasets without variables: -6 (remaining: 139)
column_names <- lapply(extract, colnames); length(keep_variables) 
sum(lapply(column_names, length)<30) # Check the variables in datasets.
column_names[lapply(column_names, length)<30]
datasets_to_remove <- c("AMKR54FL", "COKR53FL","EGKR51FL","ETKR51FL", "GNKR52FL", "RWKR53FL") 
extract <- extract[!names(extract) %in% datasets_to_remove]# Remove the specified datasets from the "extract" list
length(extract) #139

## datasets without respiratory symptoms information (h31~h31c): -0 (remaining: 142)
columns_to_check <- c("h22", "h31", "h31b", "h31c")
column_names <- lapply(extract, colnames) # Identify datasets that include all variables in columns_to_check
datasets_with_all_columns <- sapply(column_names, function(cols) {all(columns_to_check %in% cols)}) # Get the names or indices of the datasets that satisfy the condition
which(!datasets_with_all_columns)
length(extract) #139

combined_data <- rbindlist(extract, fill = TRUE, ignore.attr = TRUE)
combined_data <- combined_data %>% dplyr::select(order(names(.))) 
length(unique(combined_data$v000)) 

# DHS survey version from filename
combined_data <- combined_data %>% mutate(survey_v = substr(filename, 5,5))

# Country name
country_name <- c( AF7 = "Afghanistan", AL5 = "Albania", AL7 = "Albania", AM6 = "Armenia", AM7 = "Armenia", AO7 = "Angola", AZ5 = "Azerbaijan", BD5 = "Bangladesh", BD6 = "Bangladesh", BD7 = "Bangladesh", BD8 = "Bangladesh", BF6 = "Burkina Faso", BF8 = "Burkina Faso", BJ5 = "Benin", BJ6 = "Benin", BJ7 = "Benin", BO5 = "Bolivia", CD5 = "Democratic Republic of the Congo", CD6 = "Democratic Republic of the Congo", CG5 = "Republic of the Congo", CG6 = "Republic of the Congo", CI6 = "Cote d'Ivoire", CI8 = "Cote d'Ivoire", CM6 = "Cameroon", CM7 = "Cameroon", CO6 = "Colombia",  CO5 = "Colombia", CO7 = "Colombia", EG5 = "Egypt", EG6 = "Egypt", ET7 = "Ethiopia", ET8 = "Ethiopia", GA6 = "Gabon", GA7 = "Gabon", GH5 = "Ghana", GH6 = "Ghana",GH7 = "Ghana", GH8 = "Ghana", GM6 = "Gambia", GM7 = "Gambia", GM8 = "Gambia", GN6 = "Guinea", GN7 = "Guinea", GY5 = "Guyana", HN5 = "Honduras", HN6 = "Honduras", HT5 = "Haiti", HT6 = "Haiti", HT7 = "Haiti", ID5 = "Indonesia", ID6 = "Indonesia", ID7 = "Indonesia", JO5 = "Jordan", JO6 = "Jordan", JO7 = "Jordan", JO8 = "Jordan", KE5 = "Kenya", KE6 = "Kenya", KE7 = "Kenya", KE8 = "Kenya", KH5 = "Cambodia", KH6 = "Cambodia", KH7 = "Cambodia", KH8 = "Cambodia", KM6 = "Comoros", LS5 = "Lesotho",LS6 = "Lesotho", LS7 = "Lesotho", LS8 = "Lesotho", MD5 = "Madagascar", MD7 = "Madagascar", MD8 = "Madagascar", ML5 = "Mali", ML6 = "Mali", ML7 = "Mali", MM7 = "Myanmar", MR7 = "Mauritania", MV5 = "Maldives", MV7 = "Maldives", MW5 = "Malawi",  MW6 = "Malawi", MW7 = "Malawi", MZ6 = "Mozambique", MZ8 = "Mozambique", NG5 = "Nigeria", NG6 = "Nigeria", NG7 = "Nigeria", NI5 = "Niger", NI6 = "Niger", NP6 = "Nepal", NP7 = "Nepal", NP8 = "Nepal", PE5 = "Peru", PE6 = "Peru", PG7 = "Papua New Guinea", PH5 = "Philippines", PH6 = "Philippines", PH7 = "Philippines", PH8 = "Philippines", PK5 = "Pakistan", PK6 = "Pakistan", PK7 = "Pakistan", RW5 = "Rwanda", RW6 = "Rwanda", RW7 = "Rwanda", RW8 = "Rwanda", SL5 = "Sierra Leone", SL6 = "Sierra Leone", SL7 = "Sierra Leone", SN6 = "Senegal", SN7 = "Senegal", SN8 = "Senegal", ST5 = "Sao Tome and Principe", SZ5 = "Eswatini", TD6 = "Chad", TD7 = "Chad", TG6 = "Togo", TJ6 = "Tajikistan", TJ7 = "Tajikistan", TR5 = "Turkey", TR6 = "Turkey", TZ5 = "Tanzania", TZ6 = "Tanzania", TZ7 = "Tanzania", TZ8 = "Tanzania", UA5 = "Ukraine", UG5 = "Uganda", UG6 = "Uganda", UG7 = "Uganda", YE6 = "Yemen", ZA7 = "South Africa", ZM5 = "Zambia", ZM6 = "Zambia", ZM7 = "Zambia", ZW5 = "Zimbabwe", ZW6 = "Zimbabwe", ZW7 = "Zimbabwe"
)


a <- combined_data %>% dplyr::select(v000, filename) %>% distinct() 
table(a$v000)
combined_data$Country <- country_name[combined_data$v000]
combined_data_unique <- combined_data[!duplicated(combined_data[, c("filename", "Country")]), c("filename", "Country")]
combined_data_unique$filename <- substr(combined_data_unique$filename,1,5)
print(combined_data_unique, nrows = Inf)
length(unique(combined_data$Country)); length(unique(combined_data$v000)) #59 #125


sum(with(combined_data, table(Country, v007))>0) #201 datapoints (Country year)

## b section
combined_data %>% dplyr::select(starts_with("b")) %>% colnames()
b_columns <- combined_data %>% dplyr::select(starts_with("b"))
b_column_tables <- # check the values in variables in b section
  lapply(b_columns, function(column) {table(column, useNA = "always")})
b_column_tables
combined_data <- combined_data %>% # mutate the names of variables in b section
  rename(ch_bth_index = bidx, ch_bth_order = bord, ch_bth_month = b3, ch_sex = b4, ch_alive = b5, ch_dth_age = b6, ch_dth_month = b7, ch_age_yr = b8)
colnames(combined_data)


## v section
combined_data %>% dplyr::select(starts_with("v")) %>% colnames()
v_columns <- combined_data %>% dplyr::select(starts_with("v"))
v_column_tables <- # check the values in variables in v section
  lapply(v_columns, function(column) {table(column, useNA = "always")})
v_column_tables
combined_data <- # mutate the names of variables in v section
  combined_data %>% rename(
    survey_code = v000,
    cluster_code = v001,
    sample_weight = v005,
    Year = v007,
    int_month = v008,
    hh_wealth = v190)

# check the duplication
combined_data <- combined_data %>% mutate(ch_id = paste(filename, caseid, ch_bth_index, sep = ""))
any(duplicated(combined_data$ch_id)) # check duplication of child_id: none
unique(any(duplicated(combined_data$ch_id)))
combined_data <- combined_data %>% dplyr::select(-c("caseid", "ch_bth_index"))
#age in month
combined_data$ch_age_month <- combined_data$int_month - combined_data$ch_bth_month





# _____________________________________________ ----------
# Mortality subgroup analysis by household wealth quintile ---------------------
# Run after combined_data has been created and the b/v variables have been renamed.
# Required combined_data columns:
# Country, Year, filename, ch_id, ch_sex, ch_age_month, ch_alive, ch_dth_age,
# ch_dth_month, sample_weight, survey_code, survey_v, hh_wealth

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(survival)
  library(flexsurv)
  library(MASS)
  library(mgcv)
  library(ranger)
  library(glmnet)
  library(xgboost)
})

set.seed(123)
options(scipen = 999)

stopifnot(exists("combined_data"))
stopifnot(all(c("Country", "Year", "filename", "ch_id", "ch_sex", "ch_age_month", "ch_alive", "ch_dth_age", "ch_dth_month", "sample_weight", "survey_code", "survey_v", "hh_wealth") %in% names(combined_data)))

out_dir <- "outputs/pipelines/mortality_by_wealth_quintile"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_sim <- 1000
wealth_levels <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")

# CHANGE THIS ONE LINE after reviewing the revised main mortality Section 4.
# Use one common mortality extrapolation model for all three age groups and all five wealth scenarios.
selected_mortality_model_key_i <- "lasso"


# _____________________________________________ ----------
# 1. Prepare mortality data and household wealth quintile ----------------------

country_name_map <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Democratic Republic of Congo" = "Congo, Dem. Rep.",
  "Republic of the Congo" = "Congo, Rep.",
  "Congo" = "Congo, Rep.",
  "Congo [DRC]" = "Congo, Dem. Rep.",
  "Congo [Republic]" = "Congo, Rep.",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Iran" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Laos" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "Macedonia [FYROM]" = "North Macedonia",
  "Russia" = "Russian Federation",
  "Swaziland" = "Eswatini",
  "United Republic of Tanzania" = "Tanzania",
  "Türkiye" = "Turkiye",
  "Turkey" = "Turkiye",
  "Vietnam" = "Viet Nam",
  "Yemen" = "Yemen, Rep.",
  "Myanmar [Burma]" = "Myanmar"
)

analysis_data <- combined_data %>%
  transmute(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Year = as.numeric(Year),
    filename = as.character(filename),
    ch_id = as.character(ch_id),
    ch_sex,
    ch_age_month = as.numeric(ch_age_month),
    ch_alive = as.numeric(ch_alive),
    ch_dth_age,
    ch_dth_month,
    sample_weight = as.numeric(sample_weight) / 1000000,
    survey_code,
    survey_v,
    wealth_quintile_code = suppressWarnings(as.integer(hh_wealth))
  ) %>%
  mutate(
    Country = trimws(Country),
    Country = gsub("\\\\", "", Country),
    CountryYear_original = paste(Country, Year, sep = "_"),
    Year = case_when(
      CountryYear_original == "Albania_2008" ~ 2009,
      CountryYear_original == "Albania_2018" ~ 2017,
      CountryYear_original == "Armenia_2015" ~ 2016,
      CountryYear_original == "Cambodia_2011" ~ 2010,
      CountryYear_original == "Cambodia_2022" ~ 2021,
      CountryYear_original == "Cameroon_2019" ~ 2018,
      CountryYear_original == "Gabon_2019" ~ 2020,
      CountryYear_original == "Jordan_2018" ~ 2017,
      CountryYear_original == "Maldives_2016" ~ 2017,
      CountryYear_original == "Sao Tome and Principe_2009" ~ 2008,
      CountryYear_original == "Zambia_2019" ~ 2018,
      TRUE ~ Year
    ),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    CountryYear = paste(Country, Year, sep = "_"),
    hh_wealth_quintile = factor(wealth_quintile_code, levels = 1:5, labels = wealth_levels)
  ) %>%
  filter(
    Year > 2004,
    ch_alive %in% c(0, 1),
    is.finite(ch_age_month),
    is.finite(sample_weight),
    sample_weight > 0,
    wealth_quintile_code %in% 1:5,
    !Country %in% c("Haiti", "Maldives", "Sao Tome and Principe", "Yemen, Rep.")
  ) %>%
  dplyr::select(-CountryYear_original)

wealth_quintile_check <- analysis_data %>%
  group_by(filename, Country, Year, wealth_quintile_code, hh_wealth_quintile) %>%
  summarise(
    n_child = n(),
    weight_sum = sum(sample_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(filename) %>%
  mutate(weighted_share = weight_sum / sum(weight_sum, na.rm = TRUE)) %>%
  ungroup()

write.csv(
  wealth_quintile_check,
  file.path(out_dir, "mortality_wealth_quintile_distribution_check.csv"),
  row.names = FALSE
)




mortality_pc_file_i <- "outputs/pipelines/pc_gdp_2023_mortality.rds"
mortality_mipca_file_i <- "outputs/pipelines/mipca_mortality_m5_bayes.rds"

if(!file.exists(mortality_pc_file_i)){
  stop(
    "Run revised mortality PCA Section 0 first. Missing: ",
    mortality_pc_file_i
  )
}

if(!file.exists(mortality_mipca_file_i)){
  stop(
    "Revised Bayesian mortality MIPCA output was not found. Run revised mortality Section 0 first."
  )
}

# Five imputation-specific PC datasets from the revised Bayesian mortality PCA.
# MIPCA uncertainty is generated in main Section 0; it is not re-estimated here.
UNI_raw <- readRDS(mortality_pc_file_i)

UNI_model <- UNI_raw %>%
  mutate(
    .imp_pc = as.integer(.imp_pc),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2),
    gbd_region = dplyr::recode(
      as.character(gbd_region),
      "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
      .default = as.character(gbd_region)
    )
  ) %>%
  filter(is.finite(Year), is.finite(PC1), is.finite(PC2), !is.na(gbd_region))

stopifnot(
  ".imp_pc" %in% names(UNI_model),
  n_distinct(UNI_model$.imp_pc) == 5
)

pc_grid_check <- UNI_model %>% count(Country, Year, name = "n_pc")
if(any(pc_grid_check$n_pc != 5)) stop("Each mortality target Country-Year must have exactly five PC-imputation rows.")

UNI_base <- UNI_model %>%
  dplyr::select(Country, Year, any_of(c("GDP", "Income", "gbd_region", "Endemic", "Population"))) %>%
  distinct() %>%
  arrange(Country, Year)

stopifnot(
  nrow(UNI_base) == n_distinct(paste(UNI_base$Country, UNI_base$Year)),
  !anyDuplicated(paste(UNI_base$Country, UNI_base$Year))
)

analysis_data <- analysis_data %>%
  inner_join(
    UNI_base %>% dplyr::select(Country, Year, gbd_region),
    by = c("Country", "Year")
  )

# Compatibility object used only by the descriptive plotting code below.
# It intentionally contains one row per Country-Year and no PC scores.
UNI <- UNI_base

# _____________________________________________ ----------
# 1.1 (PDF) Distribution of under-5 children across DHS wealth quintiles, ordered by GDP and Year --------------
# Common data -------------------------------------------------------------------

wealth_quintile_check_u5 <- analysis_data %>%
  filter(ch_alive == 1, ch_age_month >= 0, ch_age_month < 60,
         is.finite(sample_weight), sample_weight > 0, wealth_quintile_code %in% 1:5) %>%
  group_by(filename, Country, Year, wealth_quintile_code, hh_wealth_quintile) %>%
  summarise(n_child = n(), weight_sum = sum(sample_weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(filename, Country, Year) %>%
  mutate(weighted_share = weight_sum / sum(weight_sum, na.rm = TRUE)) %>%
  ungroup()

wealth_quintile_check_u5_summary <- wealth_quintile_check_u5 %>%
  group_by(hh_wealth_quintile) %>%
  summarise(
    median_share = median(weighted_share, na.rm = TRUE),
    q25 = quantile(weighted_share, 0.25, na.rm = TRUE),
    q75 = quantile(weighted_share, 0.75, na.rm = TRUE),
    min_share = min(weighted_share, na.rm = TRUE),
    max_share = max(weighted_share, na.rm = TRUE)
  )

quintile_levels <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")

fill_cols <- c(
  "Poorest" = "#145AA3",
  "Poorer"  = "#3487BF",
  "Middle"  = "#72B2D7",
  "Richer"  = "#B8D3E3",
  "Richest" = "#E8EEF9"
)

legend_data <- wealth_quintile_check_u5_summary %>%
  mutate(hh_wealth_quintile = as.character(hh_wealth_quintile)) %>%
  slice(match(rev(quintile_levels), hh_wealth_quintile)) %>%
  mutate(
    legend_label = sprintf(
      "%s  %.1f%% (%.1f–%.1f%%)",
      hh_wealth_quintile,
      100 * median_share,
      100 * q25,
      100 * q75
    )
  )

legend_data


# A. GDP-ordered data -----------------------------------------------------------

GDP_lookup <- UNI %>%
  dplyr::select(Country, Year, GDP) %>%
  filter(is.finite(GDP), GDP > 0) %>%
  distinct(Country, Year, .keep_all = TRUE)

wealth_plot_gdp <- wealth_quintile_check_u5 %>%
  left_join(GDP_lookup, by = c("Country", "Year")) %>%
  filter(is.finite(GDP), GDP > 0)

bar_order_gdp <- wealth_plot_gdp %>%
  distinct(filename, Country, Year, GDP) %>%
  arrange(GDP, Year, Country, filename) %>%
  mutate(bar_index = row_number())

wealth_plot_gdp <- wealth_plot_gdp %>%
  left_join(bar_order_gdp %>% dplyr::select(filename, Country, Year, bar_index),
            by = c("filename", "Country", "Year")) %>%
  mutate(hh_wealth_quintile = factor(as.character(hh_wealth_quintile), levels = quintile_levels))

duplicate_check_gdp <- wealth_plot_gdp %>%
  count(bar_index, filename, Country, Year, hh_wealth_quintile, name = "n") %>%
  filter(n > 1)

stopifnot(nrow(duplicate_check_gdp) == 0)

plot_mat_gdp_df <- wealth_plot_gdp %>%
  dplyr::select(bar_index, hh_wealth_quintile, weighted_share) %>%
  complete(
    bar_index = bar_order_gdp$bar_index,
    hh_wealth_quintile = factor(quintile_levels, levels = quintile_levels),
    fill = list(weighted_share = 0)
  ) %>%
  pivot_wider(id_cols = bar_index, names_from = hh_wealth_quintile, values_from = weighted_share) %>%
  arrange(bar_index)

plot_mat_gdp <- as.matrix(plot_mat_gdp_df[, quintile_levels, drop = FALSE])
storage.mode(plot_mat_gdp) <- "numeric"
stopifnot(all(abs(rowSums(plot_mat_gdp) - 1) < 1e-8))

gdp_axis_index <- unique(round(seq(1, nrow(bar_order_gdp), length.out = 5)))
gdp_axis_at <- bar_order_gdp$bar_index[gdp_axis_index]
gdp_axis_labels <- format(round(bar_order_gdp$GDP[gdp_axis_index]),
                          big.mark = ",", trim = TRUE, scientific = FALSE)


# B. Year-ordered data ----------------------------------------------------------

bar_order_year <- wealth_quintile_check_u5 %>%
  distinct(filename, Country, Year) %>%
  arrange(Year, Country, filename) %>%
  mutate(bar_index = row_number())

wealth_plot_year <- wealth_quintile_check_u5 %>%
  left_join(bar_order_year %>% dplyr::select(filename, Country, Year, bar_index),
            by = c("filename", "Country", "Year")) %>%
  mutate(hh_wealth_quintile = factor(as.character(hh_wealth_quintile), levels = quintile_levels))

duplicate_check_year <- wealth_plot_year %>%
  count(bar_index, filename, Country, Year, hh_wealth_quintile, name = "n") %>%
  filter(n > 1)

stopifnot(nrow(duplicate_check_year) == 0)

plot_mat_year_df <- wealth_plot_year %>%
  dplyr::select(bar_index, hh_wealth_quintile, weighted_share) %>%
  complete(
    bar_index = bar_order_year$bar_index,
    hh_wealth_quintile = factor(quintile_levels, levels = quintile_levels),
    fill = list(weighted_share = 0)
  ) %>%
  pivot_wider(id_cols = bar_index, names_from = hh_wealth_quintile, values_from = weighted_share) %>%
  arrange(bar_index)

plot_mat_year <- as.matrix(plot_mat_year_df[, quintile_levels, drop = FALSE])
storage.mode(plot_mat_year) <- "numeric"
stopifnot(all(abs(rowSums(plot_mat_year) - 1) < 1e-8))

year_axis_data <- bar_order_year %>%
  group_by(Year) %>%
  summarise(axis_at = mean(bar_index), .groups = "drop") %>%
  arrange(Year)

year_axis_index <- unique(round(seq(1, nrow(year_axis_data), length.out = 4)))
year_axis_at <- year_axis_data$axis_at[year_axis_index]
year_axis_labels <- year_axis_data$Year[year_axis_index]


# Combined plot -----------------------------------------------------------------

dir.create("figures/supplementary/wealth_quintile", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/supplementary/wealth_quintile/under5_wealth_quintile_distribution_GDP_year.pdf",
  width = 6, height = 2, useDingbats = FALSE
)

old_par <- par(no.readonly = TRUE)

# GDP panel | Year panel | Shared legend
layout(matrix(c(0, 1, 2, 3), nrow = 1), widths = c(1.3, 4.0, 4.0, 3.7))


# Panel A: GDP ------------------------------------------------------------------

par(mar = c(4, 1.0, 1.3, 1.0), mgp = c(1.0, 0.3, 0), tcl = -0.25, cex.axis = 0.8)

n_bar_gdp <- nrow(plot_mat_gdp)

plot(NA, xlim = c(0.5, n_bar_gdp + 0.5), ylim = c(0, 1),
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")

for (i in seq_len(n_bar_gdp)) {
  y_bottom <- 0
  for (q in seq_along(quintile_levels)) {
    quintile_i <- quintile_levels[q]
    y_top <- y_bottom + plot_mat_gdp[i, quintile_i]
    rect(i - 0.5, y_bottom, i + 0.5, y_top, col = fill_cols[quintile_i], border = NA)
    y_bottom <- y_top
  }
}

abline(h = c(0.2, 0.4, 0.6, 0.8), col = "white", lwd = 0.8)

axis(1, at = gdp_axis_at, labels = gdp_axis_labels, lwd = 0.8, lwd.ticks = 0.8, cex.axis = 0.75)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"),
     las = 1, lwd = 0.8, lwd.ticks = 0.8, cex.axis = 0.8)

box(bty = "l")
mtext("GDP per capita (PPP)", side = 1, line = 2.5, cex = 0.8)
mtext("Weighted proportion of\nunder-5 children", side = 2, line = 3.0, cex = 0.8)


# Panel B: Survey year ----------------------------------------------------------

par(mar = c(4, 1.0, 1.3, 1.0), mgp = c(1.0, 0.3, 0), tcl = -0.25, cex.axis = 0.8)

n_bar_year <- nrow(plot_mat_year)

plot(NA, xlim = c(0.5, n_bar_year + 0.5), ylim = c(0, 1),
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")

for (i in seq_len(n_bar_year)) {
  y_bottom <- 0
  for (q in seq_along(quintile_levels)) {
    quintile_i <- quintile_levels[q]
    y_top <- y_bottom + plot_mat_year[i, quintile_i]
    rect(i - 0.5, y_bottom, i + 0.5, y_top, col = fill_cols[quintile_i], border = NA)
    y_bottom <- y_top
  }
}

abline(h = c(0.2, 0.4, 0.6, 0.8), col = "white", lwd = 0.8)

axis(1, at = year_axis_at, labels = year_axis_labels, lwd = 0.8, lwd.ticks = 0.8, cex.axis = 0.8)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"),
     las = 1, lwd = 0.8, lwd.ticks = 0.8, cex.axis = 0.8)

box(bty = "l")
mtext("Survey year", side = 1, line = 2.5, cex = 0.8)

# One shared legend -------------------------------------------------------------

legend_data <- wealth_quintile_check_u5_summary %>%
  mutate(
    hh_wealth_quintile = as.character(hh_wealth_quintile),
    value_label = sprintf("%.1f%% (%.1f–%.1f%%)", 100 * median_share, 100 * q25, 100 * q75)
  ) %>%
  slice(match(rev(quintile_levels), hh_wealth_quintile))

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

legend_y <- seq(0.60, 0.40, length.out = 5)

# Title
text(0.05, 0.73, "Household wealth quintile", adj = 0, cex = 1.0, font=2)
text(0.49, 0.65, "Median (IQR)", adj = 0, cex = 0.8)

# Color boxes
rect(
  xleft = 0.05, ybottom = legend_y - 0.018,
  xright = 0.09, ytop = legend_y + 0.018,
  col = fill_cols[legend_data$hh_wealth_quintile],
  border = NA
)

# Wealth-quintile names
text(0.12, legend_y,
     legend_data$hh_wealth_quintile,
     adj = 0, cex = 0.72)

# Median (IQR): same starting x-position for every row
text(0.40, legend_y,
     legend_data$value_label,
     adj = 0, cex = 0.72)

par(old_par)
dev.off()




# _____________________________________________ ----------
# 2. Clean age at death and survival time once ---------------------------------

analysis_data <- analysis_data %>%
  mutate(
    ch_dth_age_raw = suppressWarnings(as.integer(as.character(ch_dth_age))),
    death_event = as.integer(ch_alive == 0),
    death_unit = ch_dth_age_raw %/% 100,
    death_value = ch_dth_age_raw %% 100,
    ch_dth_age_days = case_when(
      death_event == 0 ~ NA_real_,
      ch_dth_age_raw %in% c(
        99, 297, 298, 299, 305, 306, 307, 308, 309, 310, 311, 312, 313,
        314, 318, 323, 324, 326, 327, 336, 340, 397, 399, 997, 998, 999
      ) ~ NA_real_,
      death_unit == 1 & death_value <= 98 ~ as.numeric(death_value),
      death_unit == 2 & death_value <= 59 ~ (death_value + 0.5) * 365.25 / 12,
      death_unit == 3 & death_value <= 4 ~ (death_value + 0.5) * 365.25,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!(death_event == 1 & !is.finite(ch_dth_age_days))) %>%
  mutate(
    age_at_interview_days = ch_age_month * 365.25 / 12,
    time_days = if_else(death_event == 1, ch_dth_age_days, age_at_interview_days),
    time_fixed = pmax(time_days, 0.1)
  ) %>%
  filter(is.finite(time_fixed), time_fixed > 0, !is.na(hh_wealth_quintile))

# Do not recreate or overwrite death_event, time_days, or time_fixed after here.


# _____________________________________________ ----------
# 3. Define five counterfactual household wealth quintile scenarios ------------

wealth_scenario <- tibble(
  wealth_quintile_code = 1:5,
  wealth_level = wealth_levels,
  wealth_tag = paste0("q", 1:5),
  hh_wealth_quintile = factor(wealth_levels, levels = wealth_levels)
)

wealth_design_matrix <- model.matrix(~ hh_wealth_quintile, data = wealth_scenario)
colnames(wealth_design_matrix)[colnames(wealth_design_matrix) == "(Intercept)"] <- "scale"

print(wealth_scenario)
print(wealth_design_matrix)

write.csv(
  wealth_scenario %>% dplyr::select(-hh_wealth_quintile),
  file.path(out_dir, "mortality_wealth_quintile_scenarios.csv"),
  row.names = FALSE
)


# _____________________________________________ ----------
# 4. Fit one wealth-quintile-dependent Weibull model per country-year -----------

cy_list <- sort(unique(analysis_data$CountryYear))
all_fits_wealth_quintile <- list()
fit_status_list <- vector("list", length(cy_list))

for (i in seq_along(cy_list)) {
  cy_i <- cy_list[i]
  message("Weibull fit: ", cy_i, " | ", i, "/", length(cy_list))
  
  sub_data_i <- analysis_data %>%
    filter(CountryYear == cy_i) %>%
    dplyr::select(
      CountryYear, Country, Year, time_fixed, death_event,
      sample_weight, wealth_quintile_code, hh_wealth_quintile
    ) %>%
    mutate(hh_wealth_quintile = factor(hh_wealth_quintile, levels = wealth_levels))
  
  n_child_i <- nrow(sub_data_i)
  n_death_i <- sum(sub_data_i$death_event == 1, na.rm = TRUE)
  wealth_counts_i <- table(sub_data_i$hh_wealth_quintile)
  n_wealth_levels_i <- sum(wealth_counts_i > 0)
  min_wealth_n_i <- if (length(wealth_counts_i) > 0) min(wealth_counts_i) else 0
  
  if (n_child_i < 20 || n_death_i < 3 || n_wealth_levels_i < 5) {
    fit_status_list[[i]] <- tibble(
      CountryYear = cy_i,
      Country = unique(sub_data_i$Country)[1],
      Year = unique(sub_data_i$Year)[1],
      n_child = n_child_i,
      n_death = n_death_i,
      n_wealth_levels = n_wealth_levels_i,
      min_n_per_wealth_quintile = min_wealth_n_i,
      fit_success = FALSE,
      fit_message = "Insufficient children, deaths, or wealth-quintile coverage"
    )
    next
  }
  
  sub_data_i <- sub_data_i %>%
    mutate(norm_weight = sample_weight / mean(sample_weight, na.rm = TRUE))
  
  fit_i <- try(
    flexsurv::flexsurvreg(
      survival::Surv(time_fixed, death_event) ~ hh_wealth_quintile,
      data = sub_data_i,
      dist = "weibull",
      weights = norm_weight
    ),
    silent = TRUE
  )
  
  if (inherits(fit_i, "try-error")) {
    fit_status_list[[i]] <- tibble(
      CountryYear = cy_i,
      Country = unique(sub_data_i$Country)[1],
      Year = unique(sub_data_i$Year)[1],
      n_child = n_child_i,
      n_death = n_death_i,
      n_wealth_levels = n_wealth_levels_i,
      min_n_per_wealth_quintile = min_wealth_n_i,
      fit_success = FALSE,
      fit_message = as.character(fit_i)
    )
    next
  }
  
  required_coef_i <- c("shape", colnames(wealth_design_matrix))
  
  if (
    !all(required_coef_i %in% names(coef(fit_i))) ||
    any(!is.finite(coef(fit_i)[required_coef_i])) ||
    any(!is.finite(vcov(fit_i)[required_coef_i, required_coef_i, drop = FALSE]))
  ) {
    fit_status_list[[i]] <- tibble(
      CountryYear = cy_i,
      Country = unique(sub_data_i$Country)[1],
      Year = unique(sub_data_i$Year)[1],
      n_child = n_child_i,
      n_death = n_death_i,
      n_wealth_levels = n_wealth_levels_i,
      min_n_per_wealth_quintile = min_wealth_n_i,
      fit_success = FALSE,
      fit_message = "Missing or non-finite Weibull parameters"
    )
    next
  }
  
  all_fits_wealth_quintile[[cy_i]] <- fit_i
  
  fit_status_list[[i]] <- tibble(
    CountryYear = cy_i,
    Country = unique(sub_data_i$Country)[1],
    Year = unique(sub_data_i$Year)[1],
    n_child = n_child_i,
    n_death = n_death_i,
    n_wealth_levels = n_wealth_levels_i,
    min_n_per_wealth_quintile = min_wealth_n_i,
    fit_success = TRUE,
    fit_message = "Success"
  )
}

fit_status <- bind_rows(fit_status_list)

write.csv(
  fit_status,
  file.path(out_dir, "mortality_country_year_weibull_fit_status_by_wealth_quintile.csv"),
  row.names = FALSE
)

saveRDS(
  all_fits_wealth_quintile,
  file.path(out_dir, "mortality_country_year_weibull_fits_by_wealth_quintile.rds")
)

if (length(all_fits_wealth_quintile) == 0) {
  stop("No country-year Weibull models were successfully fitted.")
}




# _____________________________________________ ----------
# 5. Draw Weibull parameters and evaluate all wealth-quintile scenarios ----------
# The same Weibull parameter draw is evaluated under q1-q5, preserving pairing.
# draw_id is inherited from the revised main mortality 5-PC x 200-draw design.

mortality_draw_design_file <- "outputs/pipelines/mortality_rate_uncertainty/mortality_draw_design_5PC_200draw.rds"

if(!file.exists(mortality_draw_design_file)){
  stop("Run the revised main mortality analysis through Section 3 first. Missing: ", mortality_draw_design_file)
}

mortality_draw_design <- readRDS(mortality_draw_design_file) %>%
  mutate(
    draw_id = as.integer(draw_id),
    .imp_pc = as.integer(.imp_pc),
    .draw_within_pc = as.integer(.draw_within_pc)
  ) %>%
  arrange(draw_id)

stopifnot(
  nrow(mortality_draw_design) == 1000,
  identical(mortality_draw_design$draw_id, seq_len(1000)),
  n_distinct(mortality_draw_design$.imp_pc) == 5,
  all((mortality_draw_design %>% count(.imp_pc) %>% pull(n)) == 200)
)

n_sim <- nrow(mortality_draw_design)
n_draw_within_pc <- max(mortality_draw_design$.draw_within_pc)

saveRDS(
  mortality_draw_design,
  file.path(out_dir, "mortality_draw_design_5PC_200draw_by_wealth_quintile.rds")
)

simulated_mortality_wealth_list <- vector("list", length(all_fits_wealth_quintile) * nrow(wealth_scenario))
list_index_i <- 1

for(cy_index_i in seq_along(all_fits_wealth_quintile)){
  cy_i <- names(all_fits_wealth_quintile)[cy_index_i]
  message("Weibull uncertainty: ", cy_i)
  
  fit_i <- all_fits_wealth_quintile[[cy_i]]
  
  metadata_i <- fit_status %>%
    filter(CountryYear == cy_i, fit_success) %>%
    slice(1)
  
  set.seed(500000 + cy_index_i)
  parameter_draw_i <- try(
    MASS::mvrnorm(n = n_sim, mu = coef(fit_i), Sigma = vcov(fit_i)),
    silent = TRUE
  )
  
  if(inherits(parameter_draw_i, "try-error")) next
  
  parameter_draw_i <- as.matrix(parameter_draw_i)
  shape_draw_i <- exp(parameter_draw_i[, "shape"])
  scale_coef_names_i <- colnames(wealth_design_matrix)
  
  if(!all(scale_coef_names_i %in% colnames(parameter_draw_i))) next
  
  for(k in seq_len(nrow(wealth_scenario))){
    scenario_vector_i <- wealth_design_matrix[k, scale_coef_names_i]
    log_scale_draw_i <- as.numeric(parameter_draw_i[, scale_coef_names_i, drop = FALSE] %*% scenario_vector_i)
    scale_draw_i <- exp(log_scale_draw_i)
    
    p365_i <- pweibull(365, shape = shape_draw_i, scale = scale_draw_i)
    p730_i <- pweibull(730, shape = shape_draw_i, scale = scale_draw_i)
    p1825_i <- pweibull(1825, shape = shape_draw_i, scale = scale_draw_i)
    
    q_year0_i <- p365_i
    q_year1_i <- (p730_i - p365_i) / pmax(1 - p365_i, 1e-12)
    q_year2to4_i <- (p1825_i - p730_i) / pmax(1 - p730_i, 1e-12)
    
    simulated_mortality_wealth_list[[list_index_i]] <- tibble(
      CountryYear = cy_i,
      Country = metadata_i$Country,
      Year = metadata_i$Year,
      n_child = metadata_i$n_child,
      n_death = metadata_i$n_death,
      wealth_quintile_code = wealth_scenario$wealth_quintile_code[k],
      wealth_level = wealth_scenario$wealth_level[k],
      wealth_tag = wealth_scenario$wealth_tag[k],
      draw_id = seq_len(n_sim),
      q_year0 = pmin(pmax(q_year0_i, 1e-8), 1 - 1e-8),
      q_year1 = pmin(pmax(q_year1_i, 1e-8), 1 - 1e-8),
      q_year2to4 = pmin(pmax(q_year2to4_i, 1e-8), 1 - 1e-8)
    )
    
    list_index_i <- list_index_i + 1
  }
}

simulated_mortality_wealth_list <- simulated_mortality_wealth_list[seq_len(list_index_i - 1)]

simulated_mortality_data_wealth <- bind_rows(simulated_mortality_wealth_list) %>%
  left_join(
    mortality_draw_design %>% dplyr::select(draw_id, .imp_pc, .draw_within_pc),
    by = "draw_id"
  )

stopifnot(
  n_distinct(simulated_mortality_data_wealth$draw_id) == 1000,
  n_distinct(simulated_mortality_data_wealth$.imp_pc) == 5
)

saveRDS(
  simulated_mortality_data_wealth,
  file.path(out_dir, "simulated_country_year_mortality_probabilities_by_wealth_quintile.rds")
)


# _____________________________________________ ----------
# 6. Convert estimated death risks to central mortality rates -------------------

age_interval <- c("q_year0", "q_year1", "q_year2to4")

age_group_label <- c(
  q_year0 = "0-11 months",
  q_year1 = "12-23 months",
  q_year2to4 = "24-59 months"
)

mortality_rate_column <- c(
  q_year0 = "m_year0",
  q_year1 = "m_year1",
  q_year2to4 = "m_year2to4"
)


# 6-1. Estimate infant a0 separately in each PC imputation ----------------------

empirical_a0_base <- analysis_data %>%
  filter(death_event == 1, is.finite(ch_dth_age_days), ch_dth_age_days <= 365.25) %>%
  group_by(Country, Year, wealth_quintile_code, hh_wealth_quintile) %>%
  summarise(
    empirical_a0 = weighted.mean(ch_dth_age_days / 365.25, w = sample_weight, na.rm = TRUE),
    infant_deaths = n(),
    .groups = "drop"
  ) %>%
  filter(
    is.finite(empirical_a0), empirical_a0 > 0, empirical_a0 < 1,
    is.finite(infant_deaths), infant_deaths > 0
  )

pc_imp_values <- sort(unique(mortality_draw_design$.imp_pc))

a0_model_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
a0_coef_draw_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
a0_coef_draw_data_list <- vector("list", length(pc_imp_values))

for(pc_index_i in seq_along(pc_imp_values)){
  pc_imp_i <- pc_imp_values[pc_index_i]
  
  a0_data_i <- empirical_a0_base %>%
    inner_join(
      UNI_model %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(Country, Year, PC1, PC2),
      by = c("Country", "Year")
    ) %>%
    mutate(empirical_a0_logit = qlogis(pmin(pmax(empirical_a0, 1e-6), 1 - 1e-6))) %>%
    filter(is.finite(empirical_a0_logit), is.finite(PC1), is.finite(PC2))
  
  if(nrow(a0_data_i) < 5) stop("Insufficient country-year observations for a0 model in PC imputation ", pc_imp_i)
  
  a0_model_i <- lm(empirical_a0_logit ~ PC1 + PC2 + hh_wealth_quintile, weights = infant_deaths, data = a0_data_i)
  
  if(any(!is.finite(coef(a0_model_i))) || any(!is.finite(vcov(a0_model_i)))){
    stop("Non-finite a0 coefficient/covariance in PC imputation ", pc_imp_i)
  }
  
  set.seed(300000 + pc_imp_i)
  a0_draw_i <- MASS::mvrnorm(
    n = n_draw_within_pc,
    mu = coef(a0_model_i),
    Sigma = vcov(a0_model_i)
  )
  
  a0_draw_i <- as.matrix(a0_draw_i)
  colnames(a0_draw_i) <- names(coef(a0_model_i))
  
  a0_model_list[[as.character(pc_imp_i)]] <- a0_model_i
  a0_coef_draw_list[[as.character(pc_imp_i)]] <- a0_draw_i
  
  a0_coef_draw_data_list[[pc_index_i]] <- a0_draw_i %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    mutate(
      .imp_pc = pc_imp_i,
      .draw_within_pc = seq_len(n_draw_within_pc),
      .before = 1
    )
}

a0_coef_draw_data <- bind_rows(a0_coef_draw_data_list)

saveRDS(a0_model_list, file.path(out_dir, "a0_models_5PC_by_wealth_quintile.rds"))
saveRDS(a0_coef_draw_data, file.path(out_dir, "a0_coefficient_draws_5PC_by_wealth_quintile.rds"))


# Target a0 matrix: one row per target Country-Year x the same 1000 draw IDs.
UNI_base <- UNI_base %>%
  arrange(Country, Year) %>%
  mutate(.row_pred = row_number())
UNI_base_key <- paste(UNI_base$Country, UNI_base$Year, sep = "___")

matrix_a0_list <- setNames(
  lapply(wealth_scenario$wealth_tag, function(x){
    matrix(
      NA_real_,
      nrow = nrow(UNI_base),
      ncol = n_sim,
      dimnames = list(
        paste(UNI_base$Country, UNI_base$Year, sep = "_"),
        paste0("draw_", mortality_draw_design$draw_id)
      )
    )
  }),
  wealth_scenario$wealth_tag
)

for(pc_imp_i in pc_imp_values){

  pred_pc_base_i <- UNI_model %>%
    filter(.imp_pc == pc_imp_i) %>%
    mutate(
      .row_pred = match(
        paste(Country, Year, sep = "___"),
        UNI_base_key
      )
    ) %>%
    arrange(.row_pred)

  if(nrow(pred_pc_base_i) != nrow(UNI_base) ||
     anyNA(pred_pc_base_i$.row_pred)){
    stop("a0 target grid is not aligned for PC imputation ", pc_imp_i)
  }

  a0_draw_i <- a0_coef_draw_list[[as.character(pc_imp_i)]]

  draw_ids_i <- mortality_draw_design %>%
    filter(.imp_pc == pc_imp_i) %>%
    arrange(.draw_within_pc) %>%
    pull(draw_id)

  for(k in seq_len(nrow(wealth_scenario))){

    wealth_level_i <- wealth_scenario$wealth_level[k]
    wealth_tag_i <- wealth_scenario$wealth_tag[k]

    pred_pc_i <- pred_pc_base_i %>%
      mutate(
        hh_wealth_quintile = factor(
          wealth_level_i,
          levels = wealth_levels
        )
      )

    X_pred_a0_i <- model.matrix(
      ~ PC1 + PC2 + hh_wealth_quintile,
      data = pred_pc_i
    )

    X_pred_a0_i <- X_pred_a0_i[
      ,
      colnames(a0_draw_i),
      drop = FALSE
    ]

    matrix_a0_list[[wealth_tag_i]][, draw_ids_i] <-
      plogis(
        data.matrix(X_pred_a0_i) %*%
          t(a0_draw_i)
      )
  }
}

stopifnot(
  all(vapply(
    matrix_a0_list,
    function(x) !anyNA(x),
    logical(1)
  ))
)


a0_prediction_long <- bind_rows(
  lapply(seq_len(nrow(wealth_scenario)), function(k){
    
    wealth_tag_i <- wealth_scenario$wealth_tag[k]
    a0_matrix_i <- matrix_a0_list[[wealth_tag_i]]
    
    tibble(
      .row_pred = rep(
        seq_len(nrow(UNI_base)),
        times = n_sim
      ),
      draw_id = rep(
        mortality_draw_design$draw_id,
        each = nrow(UNI_base)
      ),
      a0 = as.vector(a0_matrix_i)
    ) %>%
      left_join(
        UNI_base %>%
          dplyr::select(
            .row_pred,
            Country,
            Year
          ),
        by = ".row_pred"
      ) %>%
      mutate(
        wealth_tag = wealth_tag_i
      )
  })
)

saveRDS(
  matrix_a0_list,
  file.path(
    out_dir,
    "matrix_a0_all_wealth_quintile_scenarios.rds"
  )
)

# 6-2. Convert wealth-specific death-risk draws to mortality-rate draws ----------
simulated_mortality_rate_data_wealth <-
  simulated_mortality_data_wealth %>%
  inner_join(
    UNI_model %>%
      dplyr::select(
        .imp_pc,
        Country,
        Year,
        PC1,
        PC2,
        gbd_region
      ),
    by = c(
      ".imp_pc",
      "Country",
      "Year"
    )
  ) %>%
  left_join(
    a0_prediction_long %>%
      dplyr::select(
        Country,
        Year,
        wealth_tag,
        draw_id,
        a0
      ),
    by = c(
      "Country",
      "Year",
      "wealth_tag",
      "draw_id"
    )
  ) %>%
  mutate(
    m_year0 =
      q_year0 /
      (1 - (1 - a0) * q_year0),
    
    m_year1 =
      q_year1 /
      (1 - 0.5 * q_year1),
    
    m_year2to4 =
      q_year2to4 /
      (3 - 1.5 * q_year2to4)
  ) %>%
  filter(
    is.finite(a0), a0 > 0, a0 < 1,
    is.finite(m_year0), m_year0 > 0,
    is.finite(m_year1), m_year1 > 0,
    is.finite(m_year2to4), m_year2to4 > 0
  )

stopifnot(
  n_distinct(simulated_mortality_rate_data_wealth$draw_id) == 1000,
  n_distinct(simulated_mortality_rate_data_wealth$.imp_pc) == 5
)

saveRDS(
  simulated_mortality_rate_data_wealth,
  file.path(out_dir, "simulated_country_year_mortality_rates_by_wealth_quintile.rds")
)


# 6-3. Summarize estimated country-year mortality rates before extrapolation -----

estimated_mortality_rate_summary_by_wealth_quintile <- simulated_mortality_rate_data_wealth %>%
  group_by(
    CountryYear, Country, Year, gbd_region,
    wealth_quintile_code, wealth_level, wealth_tag
  ) %>%
  summarise(
    m_year0_median = median(m_year0, na.rm = TRUE),
    m_year0_lower = as.numeric(quantile(m_year0, 0.025, na.rm = TRUE)),
    m_year0_upper = as.numeric(quantile(m_year0, 0.975, na.rm = TRUE)),
    m_year1_median = median(m_year1, na.rm = TRUE),
    m_year1_lower = as.numeric(quantile(m_year1, 0.025, na.rm = TRUE)),
    m_year1_upper = as.numeric(quantile(m_year1, 0.975, na.rm = TRUE)),
    m_year2to4_median = median(m_year2to4, na.rm = TRUE),
    m_year2to4_lower = as.numeric(quantile(m_year2to4, 0.025, na.rm = TRUE)),
    m_year2to4_upper = as.numeric(quantile(m_year2to4, 0.975, na.rm = TRUE)),
    .groups = "drop"
  )

saveRDS(
  estimated_mortality_rate_summary_by_wealth_quintile,
  file.path(out_dir, "estimated_country_year_mortality_rate_summary_by_wealth_quintile.rds")
)


# _____________________________________________ ----------
# 7. Extrapolate wealth-specific mortality rates using the common main model -----
# No subgroup model re-selection is performed.
# The same selected model is applied to all three ages and all five wealth quintiles.
# Corresponding q1-q5 draws share draw_id, .imp_pc, a0 draw, country bootstrap,
# and stochastic model-fitting seed.

model_labels_i <- c(
  lm = "Linear model",
  spline_lm = "Spline linear model",
  gam = "GAM",
  rf_balanced = "Random forest, balanced",
  rf_small_nodes = "Random forest, small nodes",
  extra_trees = "Extremely randomized trees",
  ridge = "Ridge regression",
  enet_025 = "Elastic net alpha 0.25",
  enet_050 = "Elastic net alpha 0.50",
  enet_075 = "Elastic net alpha 0.75",
  lasso = "Lasso regression",
  svm_radial = "Radial SVM",
  xgb_smooth = "XGBoost, smooth",
  xgb_balanced = "XGBoost, balanced",
  xgb_deeper = "XGBoost, deeper"
)

if(!selected_mortality_model_key_i %in% names(model_labels_i)){
  stop("Unsupported selected mortality model: ", selected_mortality_model_key_i)
}

if(selected_mortality_model_key_i == "svm_radial" && !requireNamespace("kernlab", quietly = TRUE)){
  stop("kernlab is required when selected_mortality_model_key_i = 'svm_radial'.")
}

selected_mortality_model_label_i <- unname(model_labels_i[selected_mortality_model_key_i])

main_common_model_file_i <- "outputs/pipelines/mortality_rate_full_model_search_outputs/mortality_common_model_selection.rds"

if(file.exists(main_common_model_file_i)){
  main_common_model_selection_i <- readRDS(main_common_model_file_i)
  if("model_key" %in% names(main_common_model_selection_i)){
    cat("\nMain mortality common-model ranking first model:", as.character(main_common_model_selection_i$model_key[1]), "\n")
  }
}

cat(
  "\nMortality subgroup model used for all ages and wealth scenarios: ",
  selected_mortality_model_key_i, " - ", selected_mortality_model_label_i, "\n",
  sep = ""
)


# 7-1. Create capped country-year model weights --------------------------------

mortality_weight_lookup_wealth <- simulated_mortality_rate_data_wealth %>%
  dplyr::select(Country, Year, n_child) %>%
  distinct()

stopifnot(
  nrow(mortality_weight_lookup_wealth) ==
    dplyr::n_distinct(paste(mortality_weight_lookup_wealth$Country, mortality_weight_lookup_wealth$Year))
)

mortality_weight_cap <- quantile(
  mortality_weight_lookup_wealth$n_child[
    is.finite(mortality_weight_lookup_wealth$n_child) &
      mortality_weight_lookup_wealth$n_child > 0
  ],
  probs = 0.95,
  na.rm = TRUE
)

if(!is.finite(mortality_weight_cap)) mortality_weight_cap <- 1

mortality_weight_lookup_wealth <- mortality_weight_lookup_wealth %>%
  mutate(
    weight_model = if_else(
      is.finite(n_child) & n_child > 0,
      pmin(n_child, mortality_weight_cap),
      1
    )
  )

simulated_mortality_rate_model_data_wealth <- simulated_mortality_rate_data_wealth %>%
  mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
  left_join(
    mortality_weight_lookup_wealth %>% dplyr::select(Country, Year, weight_model),
    by = c("Country", "Year")
  ) %>%
  mutate(weight_model = if_else(is.finite(weight_model) & weight_model > 0, weight_model, 1))

draw_id_values <- sort(unique(simulated_mortality_rate_model_data_wealth$draw_id))
n_sim <- length(draw_id_values)

stopifnot(
  n_sim == 1000,
  identical(draw_id_values, seq_len(1000))
)


# 7-2. One country bootstrap per draw ------------------------------------------
# Exactly the same bootstrap is used across q1-q5 and all three age groups.

mortality_bootstrap_country_levels <- sort(unique(simulated_mortality_rate_model_data_wealth$Country))
n_mortality_bootstrap_countries <- length(mortality_bootstrap_country_levels)

if(n_mortality_bootstrap_countries < 3L) stop("At least three countries are required for mortality extrapolation.")

mortality_country_bootstrap_list <- vector("list", n_sim)
names(mortality_country_bootstrap_list) <- as.character(draw_id_values)

for(draw_id_i in draw_id_values){
  set.seed(1200000 + draw_id_i)
  boot_countries_i <- sample(
    mortality_bootstrap_country_levels,
    n_mortality_bootstrap_countries,
    replace = TRUE
  )
  
  boot_country_count_i <- as.data.frame(table(boot_countries_i), stringsAsFactors = FALSE)
  names(boot_country_count_i) <- c("Country", "bootstrap_count")
  
  mortality_country_bootstrap_list[[as.character(draw_id_i)]] <- boot_country_count_i %>%
    mutate(Country = as.character(Country), bootstrap_count = as.numeric(bootstrap_count)) %>%
    filter(bootstrap_count > 0)
}


# 7-3. Matching target grids for each PC imputation -----------------------------

UNI_base <- UNI_base %>% arrange(Country, Year) %>% mutate(.row_pred = row_number())
UNI_base_key <- paste(UNI_base$Country, UNI_base$Year, sep = "___")

UNI_pred_pc_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))

for(pc_imp_i in pc_imp_values){
  UNI_pred_i <- UNI_model %>%
    filter(.imp_pc == pc_imp_i) %>%
    mutate(.row_pred = match(paste(Country, Year, sep = "___"), UNI_base_key)) %>%
    arrange(.row_pred)
  
  if(nrow(UNI_pred_i) != nrow(UNI_base) || anyNA(UNI_pred_i$.row_pred)){
    stop("Target prediction grid is not aligned for PC imputation ", pc_imp_i)
  }
  
  UNI_pred_pc_list[[as.character(pc_imp_i)]] <- UNI_pred_i
}


# 7-4. Selected-model fitting helper -------------------------------------------

fit_predict_selected_mortality_i <- function(train_i, pred_i, draw_id_i){
  lambda_min_i <- NA_real_
  lambda_1se_i <- NA_real_
  intercept_i <- NA_real_
  PC1_coef_i <- NA_real_
  PC2_coef_i <- NA_real_
  seed_model_i <- 1500000 + draw_id_i
  
  if(selected_mortality_model_key_i == "lm"){
    fit_i <- lm(y_log_rate ~ PC1 + PC2, data = train_i, weights = weight_model_boot)
    pred_log_i <- as.numeric(predict(fit_i, newdata = pred_i))
    coef_i <- coef(fit_i)
    intercept_i <- unname(coef_i["(Intercept)"])
    PC1_coef_i <- unname(coef_i["PC1"])
    PC2_coef_i <- unname(coef_i["PC2"])
    
  } else if(selected_mortality_model_key_i == "spline_lm"){
    fit_i <- lm(
      y_log_rate ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3),
      data = train_i,
      weights = weight_model_boot
    )
    pred_log_i <- as.numeric(predict(fit_i, newdata = pred_i))
    
  } else if(selected_mortality_model_key_i == "gam"){
    fit_i <- mgcv::gam(
      y_log_rate ~ s(PC1, k = 4) + s(PC2, k = 4),
      data = train_i,
      weights = weight_model_boot,
      method = "REML"
    )
    pred_log_i <- as.numeric(predict(fit_i, newdata = pred_i, type = "response"))
    
  } else if(selected_mortality_model_key_i %in% c("rf_balanced", "rf_small_nodes", "extra_trees")){
    if(selected_mortality_model_key_i == "rf_balanced"){
      num_trees_i <- 1000; mtry_i <- 1; min_node_i <- 5; splitrule_i <- "variance"
    } else if(selected_mortality_model_key_i == "rf_small_nodes"){
      num_trees_i <- 1000; mtry_i <- 2; min_node_i <- 2; splitrule_i <- "variance"
    } else {
      num_trees_i <- 1500; mtry_i <- 2; min_node_i <- 3; splitrule_i <- "extratrees"
    }
    
    set.seed(seed_model_i)
    fit_i <- ranger::ranger(
      y_log_rate ~ PC1 + PC2,
      data = train_i,
      case.weights = train_i$weight_model_boot,
      num.trees = num_trees_i,
      mtry = mtry_i,
      min.node.size = min_node_i,
      splitrule = splitrule_i,
      seed = seed_model_i
    )
    pred_log_i <- as.numeric(predict(fit_i, data = pred_i)$predictions)
    
  } else if(selected_mortality_model_key_i %in% c("ridge", "enet_025", "enet_050", "enet_075", "lasso")){
    alpha_map_i <- c(ridge = 0, enet_025 = 0.25, enet_050 = 0.50, enet_075 = 0.75, lasso = 1)
    
    X_train_i <- model.matrix(~ PC1 + PC2, data = train_i)[, -1, drop = FALSE]
    X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_i)[, -1, drop = FALSE]
    
    country_levels_i <- sort(unique(train_i$Country))
    nfolds_i <- min(5L, length(country_levels_i))
    if(nfolds_i < 3L) stop("At least three country-level folds are required.")
    
    set.seed(1400000 + draw_id_i)
    country_fold_i <- sample(rep(seq_len(nfolds_i), length.out = length(country_levels_i)), replace = FALSE)
    names(country_fold_i) <- country_levels_i
    foldid_i <- unname(country_fold_i[train_i$Country])
    
    fit_i <- glmnet::cv.glmnet(
      x = X_train_i,
      y = train_i$y_log_rate,
      weights = train_i$weight_model_boot,
      family = "gaussian",
      alpha = alpha_map_i[selected_mortality_model_key_i],
      foldid = foldid_i,
      type.measure = "mse",
      standardize = TRUE,
      intercept = TRUE
    )
    
    pred_log_i <- as.numeric(predict(fit_i, newx = X_pred_i, s = "lambda.min"))
    lambda_min_i <- fit_i$lambda.min
    lambda_1se_i <- fit_i$lambda.1se
    
    coefficient_i <- as.matrix(coef(fit_i, s = "lambda.min"))
    intercept_i <- coefficient_i["(Intercept)", 1]
    PC1_coef_i <- coefficient_i["PC1", 1]
    PC2_coef_i <- coefficient_i["PC2", 1]
    
  } else if(selected_mortality_model_key_i %in% c("xgb_smooth", "xgb_balanced", "xgb_deeper")){
    X_train_i <- model.matrix(~ PC1 + PC2, data = train_i)[, -1, drop = FALSE]
    X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_i)[, -1, drop = FALSE]
    dtrain_i <- xgboost::xgb.DMatrix(data = X_train_i, label = train_i$y_log_rate, weight = train_i$weight_model_boot)
    
    if(selected_mortality_model_key_i == "xgb_smooth"){
      max_depth_i <- 2; eta_i <- 0.02; subsample_i <- 0.85; colsample_i <- 0.85; min_child_i <- 5; nrounds_i <- 500
    } else if(selected_mortality_model_key_i == "xgb_balanced"){
      max_depth_i <- 3; eta_i <- 0.03; subsample_i <- 0.80; colsample_i <- 0.80; min_child_i <- 3; nrounds_i <- 300
    } else {
      max_depth_i <- 4; eta_i <- 0.02; subsample_i <- 0.75; colsample_i <- 0.75; min_child_i <- 2; nrounds_i <- 450
    }
    
    fit_i <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = max_depth_i,
        eta = eta_i,
        subsample = subsample_i,
        colsample_bytree = colsample_i,
        min_child_weight = min_child_i,
        seed = seed_model_i
      ),
      data = dtrain_i,
      nrounds = nrounds_i,
      verbose = 0
    )
    
    pred_log_i <- as.numeric(predict(fit_i, newdata = X_pred_i))
    
  } else if(selected_mortality_model_key_i == "svm_radial"){
    X_train_i <- model.matrix(~ PC1 + PC2, data = train_i)[, -1, drop = FALSE]
    X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_i)[, -1, drop = FALSE]
    
    fit_i <- kernlab::ksvm(
      x = X_train_i,
      y = train_i$y_log_rate,
      type = "eps-svr",
      kernel = "rbfdot",
      kpar = "automatic",
      C = 2,
      scaled = TRUE
    )
    
    pred_log_i <- as.numeric(kernlab::predict(fit_i, X_pred_i))
  }
  
  list(
    pred_log = pred_log_i,
    lambda_min = lambda_min_i,
    lambda_1se = lambda_1se_i,
    intercept = intercept_i,
    PC1 = PC1_coef_i,
    PC2 = PC2_coef_i
  )
}


# 7-5. Bootstrap-refit the selected model by wealth quintile and age ------------

mortality_death_rate_matrix_list <- list()
mortality_probability_matrix_list <- list()
mortality_rate_selected_model_diagnostic_list <- vector("list", nrow(wealth_scenario) * length(age_interval) * n_sim)
mortality_summary_list <- vector("list", nrow(wealth_scenario) * length(age_interval))
diagnostic_index_i <- 1
summary_index_i <- 1

for(wealth_index_i in seq_len(nrow(wealth_scenario))){
  wealth_tag_i <- wealth_scenario$wealth_tag[wealth_index_i]
  wealth_data_i <- simulated_mortality_rate_model_data_wealth %>% filter(wealth_tag == wealth_tag_i)
  message("Selected-model mortality extrapolation: ", wealth_tag_i)
  
  for(age_index_i in seq_along(age_interval)){
    age_interval_i <- age_interval[age_index_i]
    mortality_rate_column_i <- unname(mortality_rate_column[age_interval_i])
    
    death_rate_matrix_i <- matrix(
      NA_real_,
      nrow = nrow(UNI_base),
      ncol = n_sim,
      dimnames = list(
        paste(UNI_base$Country, UNI_base$Year, sep = "_"),
        paste0("draw_", draw_id_values)
      )
    )
    
    probability_matrix_i <- death_rate_matrix_i
    
    for(draw_id_i in draw_id_values){
      sim_data_i <- wealth_data_i %>%
        filter(draw_id == draw_id_i) %>%
        transmute(
          Country, Year, .imp_pc, .draw_within_pc, PC1, PC2, weight_model,
          y_log_rate = log(pmax(.data[[mortality_rate_column_i]], 1e-12))
        ) %>%
        filter(
          !is.na(Country), is.finite(PC1), is.finite(PC2),
          is.finite(y_log_rate), is.finite(weight_model), weight_model > 0
        )
      
      if(nrow(sim_data_i) < 5 || n_distinct(sim_data_i$Country) < 3){
        stop("Insufficient training data for ", wealth_tag_i, " | ", age_interval_i, " | draw ", draw_id_i)
      }
      
      pc_imp_i <- unique(sim_data_i$.imp_pc)
      if(length(pc_imp_i) != 1) stop("Each mortality subgroup draw must correspond to exactly one PC imputation.")
      
      pred_data_i <- UNI_pred_pc_list[[as.character(pc_imp_i)]]
      boot_country_count_i <- mortality_country_bootstrap_list[[as.character(draw_id_i)]]
      
      sim_data_boot_i <- sim_data_i %>%
        inner_join(boot_country_count_i, by = "Country") %>%
        mutate(weight_model_boot = weight_model * bootstrap_count) %>%
        filter(is.finite(weight_model_boot), weight_model_boot > 0)
      
      if(nrow(sim_data_boot_i) < 5 || n_distinct(sim_data_boot_i$Country) < 3){
        stop("Insufficient bootstrap data for ", wealth_tag_i, " | ", age_interval_i, " | draw ", draw_id_i)
      }
      
      fit_result_i <- try(
        fit_predict_selected_mortality_i(
          train_i = sim_data_boot_i,
          pred_i = pred_data_i,
          draw_id_i = draw_id_i
        ),
        silent = TRUE
      )
      
      if(inherits(fit_result_i, "try-error")){
        stop(
          "Selected mortality model failed for ", wealth_tag_i,
          " | ", age_interval_i, " | draw ", draw_id_i,
          ": ", as.character(fit_result_i)
        )
      }
      
      predicted_log_mortality_rate_i <- fit_result_i$pred_log
      
      if(length(predicted_log_mortality_rate_i) != nrow(UNI_base) ||
         any(!is.finite(predicted_log_mortality_rate_i))){
        stop("Prediction failed for ", wealth_tag_i, " | ", age_interval_i, " | draw ", draw_id_i)
      }
      
      predicted_mortality_rate_i <- exp(predicted_log_mortality_rate_i)
      death_rate_matrix_i[, draw_id_i] <- predicted_mortality_rate_i
      
      if(age_interval_i == "q_year0"){
        predicted_probability_i <- predicted_mortality_rate_i / 
          (1 + (1 - matrix_a0_list[[wealth_tag_i]][, draw_id_i]) * predicted_mortality_rate_i)
        } else if(age_interval_i == "q_year1"){
        predicted_probability_i <- predicted_mortality_rate_i /
          (1 + 0.5 * predicted_mortality_rate_i)
      } else {
        predicted_probability_i <- 3 * predicted_mortality_rate_i /
          (1 + 1.5 * predicted_mortality_rate_i)
      }
      

      
      predicted_probability_i[!is.finite(predicted_probability_i)] <- NA_real_
      predicted_probability_i <- pmin(pmax(predicted_probability_i, 0), 1)
      probability_matrix_i[, draw_id_i] <- predicted_probability_i
      
      mortality_rate_selected_model_diagnostic_list[[diagnostic_index_i]] <- tibble(
        age_interval = age_interval_i,
        age_group = unname(age_group_label[age_interval_i]),
        wealth_quintile_code = wealth_scenario$wealth_quintile_code[wealth_index_i],
        wealth_level = wealth_scenario$wealth_level[wealth_index_i],
        wealth_tag = wealth_tag_i,
        draw_id = draw_id_i,
        .imp_pc = pc_imp_i,
        .draw_within_pc = unique(sim_data_i$.draw_within_pc),
        selected_model_key = selected_mortality_model_key_i,
        selected_model = selected_mortality_model_label_i,
        n_training_country = n_distinct(sim_data_i$Country),
        n_bootstrap_country = n_distinct(sim_data_boot_i$Country),
        bootstrap_country_draws = sum(boot_country_count_i$bootstrap_count),
        lambda_min = fit_result_i$lambda_min,
        lambda_1se = fit_result_i$lambda_1se,
        intercept = fit_result_i$intercept,
        PC1 = fit_result_i$PC1,
        PC2 = fit_result_i$PC2
      )
      
      diagnostic_index_i <- diagnostic_index_i + 1
    }
    
    matrix_key_i <- paste(age_interval_i, wealth_tag_i, sep = "_")
    mortality_death_rate_matrix_list[[matrix_key_i]] <- death_rate_matrix_i
    mortality_probability_matrix_list[[matrix_key_i]] <- probability_matrix_i
    
    mortality_summary_list[[summary_index_i]] <- tibble(
      Country = UNI_base$Country,
      Year = UNI_base$Year,
      gbd_region = UNI_base$gbd_region,
      age_interval = age_interval_i,
      age_group = unname(age_group_label[age_interval_i]),
      wealth_quintile_code = wealth_scenario$wealth_quintile_code[wealth_index_i],
      wealth_level = wealth_scenario$wealth_level[wealth_index_i],
      wealth_tag = wealth_tag_i,
      model_key = selected_mortality_model_key_i,
      model = selected_mortality_model_label_i,
      mortality_probability_median = apply(probability_matrix_i, 1, median, na.rm = TRUE),
      mortality_probability_lower = apply(probability_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      mortality_probability_upper = apply(probability_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      death_rate_median = apply(death_rate_matrix_i, 1, median, na.rm = TRUE),
      death_rate_lower = apply(death_rate_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      death_rate_upper = apply(death_rate_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      n_successful_draws = rowSums(is.finite(death_rate_matrix_i))
    )
    
    age_file_i <- dplyr::recode(
      age_interval_i,
      q_year0 = "year0",
      q_year1 = "year1",
      q_year2to4 = "year2to4"
    )
    
    # Original per-age/per-quintile filenames retained.
    saveRDS(
      death_rate_matrix_i,
      file.path(out_dir, paste0("matrix_death_rate_", age_file_i, "_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      probability_matrix_i,
      file.path(out_dir, paste0("matrix_probability_", age_interval_i, "_", wealth_tag_i, ".rds"))
    )
    
    summary_index_i <- summary_index_i + 1
    message("Finished: ", wealth_tag_i, " | ", age_interval_i)
  }
}


# 7-6. Combine, check, and save final outputs -----------------------------------

mortality_rate_selected_model_diagnostics_by_wealth_quintile <- bind_rows(
  mortality_rate_selected_model_diagnostic_list[seq_len(diagnostic_index_i - 1)]
)

mortality_summary_by_wealth_quintile <- bind_rows(mortality_summary_list)

stopifnot(
  nrow(mortality_rate_selected_model_diagnostics_by_wealth_quintile) ==
    nrow(wealth_scenario) * length(age_interval) * n_sim,
  all(mortality_rate_selected_model_diagnostics_by_wealth_quintile$bootstrap_country_draws ==
        n_mortality_bootstrap_countries),
  all(mortality_summary_by_wealth_quintile$n_successful_draws == n_sim)
)


# Primary long-format mortality-rate draws with explicit PC provenance.
mortality_rate_draws_long_by_wealth_quintile <- bind_rows(
  lapply(seq_len(nrow(wealth_scenario)), function(k){
    wealth_tag_i <- wealth_scenario$wealth_tag[k]
    
    bind_rows(lapply(age_interval, function(age_interval_i){
      matrix_key_i <- paste(age_interval_i, wealth_tag_i, sep = "_")
      matrix_i <- mortality_death_rate_matrix_list[[matrix_key_i]]
      
      as.data.frame(matrix_i) %>%
        mutate(.row_pred = row_number()) %>%
        pivot_longer(cols = starts_with("draw_"), names_to = "draw_name", values_to = "mortality_rate") %>%
        mutate(
          draw_id = as.integer(sub("^draw_", "", draw_name)),
          age_interval = age_interval_i,
          age_group = unname(age_group_label[age_interval_i]),
          wealth_quintile_code = wealth_scenario$wealth_quintile_code[k],
          wealth_level = wealth_scenario$wealth_level[k],
          wealth_tag = wealth_tag_i
        ) %>%
        dplyr::select(-draw_name) %>%
        left_join(
          UNI_base %>% dplyr::select(.row_pred, Country, Year, gbd_region),
          by = ".row_pred"
        ) %>%
        left_join(
          mortality_draw_design %>% dplyr::select(draw_id, .imp_pc, .draw_within_pc),
          by = "draw_id"
        ) %>%
        relocate(
          draw_id, .draw_within_pc, .imp_pc,
          wealth_quintile_code, wealth_level, wealth_tag,
          age_interval, age_group, Country, Year
        )
    }))
  })
)

mortality_probability_draws_long_by_wealth_quintile <- bind_rows(
  lapply(seq_len(nrow(wealth_scenario)), function(k){
    wealth_tag_i <- wealth_scenario$wealth_tag[k]
    
    bind_rows(lapply(age_interval, function(age_interval_i){
      matrix_key_i <- paste(age_interval_i, wealth_tag_i, sep = "_")
      matrix_i <- mortality_probability_matrix_list[[matrix_key_i]]
      
      as.data.frame(matrix_i) %>%
        mutate(.row_pred = row_number()) %>%
        pivot_longer(cols = starts_with("draw_"), names_to = "draw_name", values_to = "mortality_probability") %>%
        mutate(
          draw_id = as.integer(sub("^draw_", "", draw_name)),
          age_interval = age_interval_i,
          age_group = unname(age_group_label[age_interval_i]),
          wealth_quintile_code = wealth_scenario$wealth_quintile_code[k],
          wealth_level = wealth_scenario$wealth_level[k],
          wealth_tag = wealth_tag_i
        ) %>%
        dplyr::select(-draw_name) %>%
        left_join(
          UNI_base %>% dplyr::select(.row_pred, Country, Year, gbd_region),
          by = ".row_pred"
        ) %>%
        left_join(
          mortality_draw_design %>% dplyr::select(draw_id, .imp_pc, .draw_within_pc),
          by = "draw_id"
        ) %>%
        relocate(
          draw_id, .draw_within_pc, .imp_pc,
          wealth_quintile_code, wealth_level, wealth_tag,
          age_interval, age_group, Country, Year
        )
    }))
  })
)

saveRDS(
  mortality_rate_selected_model_diagnostics_by_wealth_quintile,
  file.path(out_dir, "mortality_rate_selected_model_diagnostics_by_wealth_quintile.rds")
)

write.csv(
  mortality_rate_selected_model_diagnostics_by_wealth_quintile,
  file.path(out_dir, "mortality_rate_selected_model_diagnostics_by_wealth_quintile.csv"),
  row.names = FALSE
)

saveRDS(mortality_death_rate_matrix_list, file.path(out_dir, "mortality_death_rate_matrices_by_wealth_quintile.rds"))
saveRDS(mortality_probability_matrix_list, file.path(out_dir, "mortality_probability_matrices_by_wealth_quintile.rds"))
saveRDS(mortality_summary_by_wealth_quintile, file.path(out_dir, "mortality_country_year_summary_by_wealth_quintile.rds"))
write.csv(mortality_summary_by_wealth_quintile, file.path(out_dir, "mortality_country_year_summary_by_wealth_quintile.csv"), row.names = FALSE)
saveRDS(mortality_country_bootstrap_list, file.path(out_dir, "mortality_rate_country_bootstrap_counts_by_wealth_quintile.rds"))

saveRDS(
  mortality_rate_draws_long_by_wealth_quintile,
  file.path(out_dir, "mortality_rate_draws_1000_long_by_wealth_quintile.rds")
)

saveRDS(
  mortality_probability_draws_long_by_wealth_quintile,
  file.path(out_dir, "mortality_probability_draws_1000_long_by_wealth_quintile.rds")
)


# Compatibility outputs retained from the previous subgroup script.
# Their contents now correspond to selected_mortality_model_key_i, not necessarily enet_025.
mortality_rate_enet_025_diagnostics_by_wealth_quintile <- mortality_rate_selected_model_diagnostics_by_wealth_quintile

saveRDS(
  mortality_rate_enet_025_diagnostics_by_wealth_quintile,
  file.path(out_dir, "mortality_rate_enet_025_diagnostics_by_wealth_quintile.rds")
)

write.csv(
  mortality_rate_enet_025_diagnostics_by_wealth_quintile,
  file.path(out_dir, "mortality_rate_enet_025_diagnostics_by_wealth_quintile.csv"),
  row.names = FALSE
)


# 7-7. Selected-model diagnostic summary ---------------------------------------

mortality_rate_selected_model_summary <- mortality_rate_selected_model_diagnostics_by_wealth_quintile %>%
  group_by(age_interval, age_group, wealth_quintile_code, wealth_level, wealth_tag) %>%
  summarise(
    selected_model_key = first(selected_model_key),
    selected_model = first(selected_model),
    n_successful_models = n(),
    proportion_PC1_nonzero = if(all(is.na(PC1))) NA_real_ else mean(PC1 != 0, na.rm = TRUE),
    proportion_PC2_nonzero = if(all(is.na(PC2))) NA_real_ else mean(PC2 != 0, na.rm = TRUE),
    median_lambda_min = if(all(is.na(lambda_min))) NA_real_ else median(lambda_min, na.rm = TRUE),
    median_lambda_1se = if(all(is.na(lambda_1se))) NA_real_ else median(lambda_1se, na.rm = TRUE),
    mean_n_bootstrap_country = mean(n_bootstrap_country),
    .groups = "drop"
  )

saveRDS(
  mortality_rate_selected_model_summary,
  file.path(out_dir, "mortality_rate_selected_model_summary_by_wealth_quintile.rds")
)

write.csv(
  mortality_rate_selected_model_summary,
  file.path(out_dir, "mortality_rate_selected_model_summary_by_wealth_quintile.csv"),
  row.names = FALSE
)


# Old enet_025 summary filename retained for downstream compatibility.
mortality_rate_enet_025_selection_summary <- mortality_rate_selected_model_summary

saveRDS(
  mortality_rate_enet_025_selection_summary,
  file.path(out_dir, "mortality_rate_enet_025_selection_summary_by_wealth_quintile.rds")
)

write.csv(
  mortality_rate_enet_025_selection_summary,
  file.path(out_dir, "mortality_rate_enet_025_selection_summary_by_wealth_quintile.csv"),
  row.names = FALSE
)

print(mortality_rate_selected_model_summary, n = Inf)


# _____________________________________________ ----------
# 8. Apply main-analysis bacterial-death proportions ----------------------------
# The main CHAMPS bacterial-death fraction models do not include household wealth.
# Therefore, the same country-year-age bacterial fractions are applied to q1-q5.
# Because both main and subgroup mortality use the same draw_id -> .imp_pc design,
# bacterial fractions are matched by draw_id rather than multiplied positionally.

main_pipeline_dir <- "outputs/pipelines"

matrix_prop_bact_LRTI_deaths_Age_0 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_LRTI_deaths_Age_0.rds")
))

matrix_prop_bact_LRTI_deaths_Age_1 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_LRTI_deaths_Age_1.rds")
))

matrix_prop_bact_LRTI_deaths_Age_2to4 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_LRTI_deaths_Age_2to4.rds")
))

matrix_prop_bact_fever_deaths_Age_0 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_fever_deaths_Age_0.rds")
))

matrix_prop_bact_fever_deaths_Age_1 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_fever_deaths_Age_1.rds")
))

matrix_prop_bact_fever_deaths_Age_2to4 <- as.matrix(readRDS(
  file.path(main_pipeline_dir, "matrix_matrix_prop_bact_fever_deaths_Age_2to4.rds")
))


target_row_key <- paste(UNI_base$Country, UNI_base$Year, sep = "_")
draw_names <- paste0("draw_", mortality_draw_design$draw_id)

main_bacterial_matrix_list <- list(
  LRTI_Age_0 = matrix_prop_bact_LRTI_deaths_Age_0,
  LRTI_Age_1 = matrix_prop_bact_LRTI_deaths_Age_1,
  LRTI_Age_2to4 = matrix_prop_bact_LRTI_deaths_Age_2to4,
  fever_Age_0 = matrix_prop_bact_fever_deaths_Age_0,
  fever_Age_1 = matrix_prop_bact_fever_deaths_Age_1,
  fever_Age_2to4 = matrix_prop_bact_fever_deaths_Age_2to4
)


align_main_bacterial_matrix_i <- function(mat_i, matrix_name_i){
  if(nrow(mat_i) != nrow(UNI_base)){
    stop(matrix_name_i, " does not have the same number of target Country-Year rows as UNI_base.")
  }
  
  if(is.null(rownames(mat_i))){
    stop(matrix_name_i, " has no row names; explicit Country-Year alignment is required.")
  }
  
  row_index_i <- match(target_row_key, rownames(mat_i))
  if(anyNA(row_index_i)){
    stop(matrix_name_i, " could not be aligned to the subgroup target Country-Year grid.")
  }
  
  if(is.null(colnames(mat_i))){
    if(ncol(mat_i) != n_sim) stop(matrix_name_i, " has no draw names and does not contain exactly 1000 columns.")
    colnames(mat_i) <- paste0("draw_", seq_len(ncol(mat_i)))
  }
  
  col_index_i <- match(draw_names, colnames(mat_i))
  if(anyNA(col_index_i)){
    stop(matrix_name_i, " does not contain all required draw_id columns from the mortality draw design.")
  }
  
  mat_i <- mat_i[row_index_i, col_index_i, drop = FALSE]
  rownames(mat_i) <- target_row_key
  colnames(mat_i) <- draw_names
  mat_i
}


main_bacterial_matrix_list <- lapply(
  names(main_bacterial_matrix_list),
  function(x) align_main_bacterial_matrix_i(main_bacterial_matrix_list[[x]], x)
)
names(main_bacterial_matrix_list) <- c(
  "LRTI_Age_0", "LRTI_Age_1", "LRTI_Age_2to4",
  "fever_Age_0", "fever_Age_1", "fever_Age_2to4"
)

matrix_prop_bact_LRTI_deaths_Age_0 <- main_bacterial_matrix_list$LRTI_Age_0
matrix_prop_bact_LRTI_deaths_Age_1 <- main_bacterial_matrix_list$LRTI_Age_1
matrix_prop_bact_LRTI_deaths_Age_2to4 <- main_bacterial_matrix_list$LRTI_Age_2to4
matrix_prop_bact_fever_deaths_Age_0 <- main_bacterial_matrix_list$fever_Age_0
matrix_prop_bact_fever_deaths_Age_1 <- main_bacterial_matrix_list$fever_Age_1
matrix_prop_bact_fever_deaths_Age_2to4 <- main_bacterial_matrix_list$fever_Age_2to4


bacterial_proportion_LRTI_matrix_list <- list(
  q_year0 = matrix_prop_bact_LRTI_deaths_Age_0,
  q_year1 = matrix_prop_bact_LRTI_deaths_Age_1,
  q_year2to4 = matrix_prop_bact_LRTI_deaths_Age_2to4
)

bacterial_proportion_fever_matrix_list <- list(
  q_year0 = matrix_prop_bact_fever_deaths_Age_0,
  q_year1 = matrix_prop_bact_fever_deaths_Age_1,
  q_year2to4 = matrix_prop_bact_fever_deaths_Age_2to4
)


# Check that bacterial-proportion draws have the same PC provenance as mortality.
bacterial_draw_design_check <- mortality_draw_design %>%
  count(.imp_pc, name = "n_draw")

stopifnot(all(bacterial_draw_design_check$n_draw == 200))

bacterial_proportion_check_list <- vector("list", length(age_interval))

for(j in seq_along(age_interval)){
  age_interval_i <- age_interval[j]
  prop_LRTI_matrix_i <- bacterial_proportion_LRTI_matrix_list[[age_interval_i]]
  prop_fever_matrix_i <- bacterial_proportion_fever_matrix_list[[age_interval_i]]
  prop_combined_matrix_i <- prop_LRTI_matrix_i + prop_fever_matrix_i
  
  bacterial_proportion_check_list[[j]] <- tibble(
    age_interval = age_interval_i,
    age_group = unname(age_group_label[age_interval_i]),
    min_prop_bact_LRTI = min(prop_LRTI_matrix_i, na.rm = TRUE),
    max_prop_bact_LRTI = max(prop_LRTI_matrix_i, na.rm = TRUE),
    min_prop_bact_fever = min(prop_fever_matrix_i, na.rm = TRUE),
    max_prop_bact_fever = max(prop_fever_matrix_i, na.rm = TRUE),
    max_combined_bacterial_proportion = max(prop_combined_matrix_i, na.rm = TRUE),
    n_combined_proportion_above_1 = sum(prop_combined_matrix_i > 1, na.rm = TRUE)
  )
}

bacterial_proportion_check <- bind_rows(bacterial_proportion_check_list)

write.csv(
  bacterial_proportion_check,
  file.path(out_dir, "bacterial_death_proportion_alignment_check.csv"),
  row.names = FALSE
)


bacterial_LRTI_death_rate_matrix_list <- list()
bacterial_fever_death_rate_matrix_list <- list()
bacterial_combined_death_rate_matrix_list <- list()

bacterial_mortality_summary_list <- vector(
  "list",
  nrow(wealth_scenario) * length(age_interval)
)

bacterial_summary_index_i <- 1

for(k in seq_len(nrow(wealth_scenario))){
  wealth_tag_i <- wealth_scenario$wealth_tag[k]
  
  for(j in seq_along(age_interval)){
    age_interval_i <- age_interval[j]
    rate_key_i <- paste(age_interval_i, wealth_tag_i, sep = "_")
    
    all_cause_death_rate_matrix_i <- mortality_death_rate_matrix_list[[rate_key_i]]
    prop_LRTI_matrix_i <- bacterial_proportion_LRTI_matrix_list[[age_interval_i]]
    prop_fever_matrix_i <- bacterial_proportion_fever_matrix_list[[age_interval_i]]
    
    if(is.null(all_cause_death_rate_matrix_i)){
      stop("All-cause death-rate matrix was not found for ", rate_key_i)
    }
    
    all_cause_death_rate_matrix_i <- as.matrix(all_cause_death_rate_matrix_i)
    
    all_cause_row_index_i <- match(target_row_key, rownames(all_cause_death_rate_matrix_i))
    all_cause_col_index_i <- match(draw_names, colnames(all_cause_death_rate_matrix_i))
    
    if(anyNA(all_cause_row_index_i) || anyNA(all_cause_col_index_i)){
      stop("The all-cause death-rate matrix could not be aligned by Country-Year and draw_id for ", rate_key_i)
    }
    
    all_cause_death_rate_matrix_i <- all_cause_death_rate_matrix_i[
      all_cause_row_index_i,
      all_cause_col_index_i,
      drop = FALSE
    ]
    
    rownames(all_cause_death_rate_matrix_i) <- target_row_key
    colnames(all_cause_death_rate_matrix_i) <- draw_names
    
    stopifnot(
      identical(dim(all_cause_death_rate_matrix_i), dim(prop_LRTI_matrix_i)),
      identical(dim(all_cause_death_rate_matrix_i), dim(prop_fever_matrix_i)),
      identical(rownames(all_cause_death_rate_matrix_i), rownames(prop_LRTI_matrix_i)),
      identical(colnames(all_cause_death_rate_matrix_i), colnames(prop_LRTI_matrix_i))
    )
    
    bacterial_LRTI_death_rate_matrix_i <- all_cause_death_rate_matrix_i * prop_LRTI_matrix_i
    bacterial_fever_death_rate_matrix_i <- all_cause_death_rate_matrix_i * prop_fever_matrix_i
    bacterial_combined_death_rate_matrix_i <- bacterial_LRTI_death_rate_matrix_i + bacterial_fever_death_rate_matrix_i
    
    bacterial_LRTI_death_rate_matrix_i[
      !is.finite(bacterial_LRTI_death_rate_matrix_i) |
        bacterial_LRTI_death_rate_matrix_i < 0
    ] <- NA_real_
    
    bacterial_fever_death_rate_matrix_i[
      !is.finite(bacterial_fever_death_rate_matrix_i) |
        bacterial_fever_death_rate_matrix_i < 0
    ] <- NA_real_
    
    bacterial_combined_death_rate_matrix_i[
      !is.finite(bacterial_combined_death_rate_matrix_i) |
        bacterial_combined_death_rate_matrix_i < 0
    ] <- NA_real_
    
    bacterial_LRTI_death_rate_matrix_list[[rate_key_i]] <- bacterial_LRTI_death_rate_matrix_i
    bacterial_fever_death_rate_matrix_list[[rate_key_i]] <- bacterial_fever_death_rate_matrix_i
    bacterial_combined_death_rate_matrix_list[[rate_key_i]] <- bacterial_combined_death_rate_matrix_i
    
    bacterial_mortality_summary_list[[bacterial_summary_index_i]] <- tibble(
      Country = UNI_base$Country,
      Year = UNI_base$Year,
      gbd_region = UNI_base$gbd_region,
      age_interval = age_interval_i,
      age_group = unname(age_group_label[age_interval_i]),
      wealth_quintile_code = wealth_scenario$wealth_quintile_code[k],
      wealth_level = wealth_scenario$wealth_level[k],
      wealth_tag = wealth_tag_i,
      prop_bact_LRTI_median = apply(prop_LRTI_matrix_i, 1, median, na.rm = TRUE),
      prop_bact_LRTI_lower = apply(prop_LRTI_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      prop_bact_LRTI_upper = apply(prop_LRTI_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      prop_bact_fever_median = apply(prop_fever_matrix_i, 1, median, na.rm = TRUE),
      prop_bact_fever_lower = apply(prop_fever_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      prop_bact_fever_upper = apply(prop_fever_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      bacterial_LRTI_death_rate_median = apply(bacterial_LRTI_death_rate_matrix_i, 1, median, na.rm = TRUE),
      bacterial_LRTI_death_rate_lower = apply(bacterial_LRTI_death_rate_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      bacterial_LRTI_death_rate_upper = apply(bacterial_LRTI_death_rate_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      bacterial_fever_death_rate_median = apply(bacterial_fever_death_rate_matrix_i, 1, median, na.rm = TRUE),
      bacterial_fever_death_rate_lower = apply(bacterial_fever_death_rate_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      bacterial_fever_death_rate_upper = apply(bacterial_fever_death_rate_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      bacterial_combined_death_rate_median = apply(bacterial_combined_death_rate_matrix_i, 1, median, na.rm = TRUE),
      bacterial_combined_death_rate_lower = apply(bacterial_combined_death_rate_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
      bacterial_combined_death_rate_upper = apply(bacterial_combined_death_rate_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE),
      n_successful_bacterial_draws = rowSums(is.finite(bacterial_combined_death_rate_matrix_i))
    )
    
    age_file_i <- dplyr::recode(
      age_interval_i,
      q_year0 = "Age_0",
      q_year1 = "Age_1",
      q_year2to4 = "Age_2to4"
    )
    
    # Original bacterial subgroup filenames retained.
    saveRDS(
      bacterial_LRTI_death_rate_matrix_i,
      file.path(out_dir, paste0("matrix_bact_death_rate_LRTI_", age_file_i, "_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      bacterial_fever_death_rate_matrix_i,
      file.path(out_dir, paste0("matrix_bact_death_rate_fever_", age_file_i, "_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      bacterial_combined_death_rate_matrix_i,
      file.path(out_dir, paste0("matrix_bact_death_rate_combined_", age_file_i, "_", wealth_tag_i, ".rds"))
    )
    
    bacterial_summary_index_i <- bacterial_summary_index_i + 1
  }
}


bacterial_mortality_summary_by_wealth_quintile <- bind_rows(bacterial_mortality_summary_list)

summary_join_keys <- c(
  "Country", "Year", "gbd_region", "age_interval", "age_group",
  "wealth_quintile_code", "wealth_level", "wealth_tag"
)

mortality_bacterial_summary_by_wealth_quintile <- mortality_summary_by_wealth_quintile %>%
  left_join(
    bacterial_mortality_summary_by_wealth_quintile,
    by = summary_join_keys
  )


# Long-format bacterial mortality draws, explicitly retaining draw provenance.
bacterial_mortality_draws_long_by_wealth_quintile <- bind_rows(
  lapply(seq_len(nrow(wealth_scenario)), function(k){
    wealth_tag_i <- wealth_scenario$wealth_tag[k]
    
    bind_rows(lapply(age_interval, function(age_interval_i){
      rate_key_i <- paste(age_interval_i, wealth_tag_i, sep = "_")
      
      LRTI_i <- bacterial_LRTI_death_rate_matrix_list[[rate_key_i]]
      fever_i <- bacterial_fever_death_rate_matrix_list[[rate_key_i]]
      combined_i <- bacterial_combined_death_rate_matrix_list[[rate_key_i]]
      
      tibble(
        .row_pred = rep(seq_len(nrow(UNI_base)), times = n_sim),
        draw_id = rep(draw_id_values, each = nrow(UNI_base)),
        bacterial_LRTI_death_rate = as.vector(LRTI_i),
        bacterial_fever_death_rate = as.vector(fever_i),
        bacterial_combined_death_rate = as.vector(combined_i)
      ) %>%
        left_join(
          UNI_base %>% dplyr::select(.row_pred, Country, Year, gbd_region),
          by = ".row_pred"
        ) %>%
        left_join(
          mortality_draw_design %>% dplyr::select(draw_id, .imp_pc, .draw_within_pc),
          by = "draw_id"
        ) %>%
        mutate(
          age_interval = age_interval_i,
          age_group = unname(age_group_label[age_interval_i]),
          wealth_quintile_code = wealth_scenario$wealth_quintile_code[k],
          wealth_level = wealth_scenario$wealth_level[k],
          wealth_tag = wealth_tag_i
        ) %>%
        relocate(
          draw_id, .draw_within_pc, .imp_pc,
          wealth_quintile_code, wealth_level, wealth_tag,
          age_interval, age_group, Country, Year
        )
    }))
  })
)


saveRDS(
  bacterial_LRTI_death_rate_matrix_list,
  file.path(out_dir, "bacterial_LRTI_death_rate_matrices_by_wealth_quintile.rds")
)

saveRDS(
  bacterial_fever_death_rate_matrix_list,
  file.path(out_dir, "bacterial_fever_death_rate_matrices_by_wealth_quintile.rds")
)

saveRDS(
  bacterial_combined_death_rate_matrix_list,
  file.path(out_dir, "bacterial_combined_death_rate_matrices_by_wealth_quintile.rds")
)

saveRDS(
  bacterial_mortality_summary_by_wealth_quintile,
  file.path(out_dir, "bacterial_mortality_country_year_summary_by_wealth_quintile.rds")
)

saveRDS(
  mortality_bacterial_summary_by_wealth_quintile,
  file.path(out_dir, "mortality_and_bacterial_country_year_summary_by_wealth_quintile.rds")
)

saveRDS(
  bacterial_mortality_draws_long_by_wealth_quintile,
  file.path(out_dir, "bacterial_mortality_draws_1000_long_by_wealth_quintile.rds")
)

write.csv(
  bacterial_mortality_summary_by_wealth_quintile,
  file.path(out_dir, "bacterial_mortality_country_year_summary_by_wealth_quintile.csv"),
  row.names = FALSE
)

write.csv(
  mortality_bacterial_summary_by_wealth_quintile,
  file.path(out_dir, "mortality_and_bacterial_country_year_summary_by_wealth_quintile.csv"),
  row.names = FALSE
)


cat(
  "\nFinal mortality wealth-subgroup uncertainty design:\n",
  "Selected common mortality model = ", selected_mortality_model_key_i,
  "\nWealth scenarios = 5",
  "\nPC imputations = ", n_distinct(mortality_draw_design$.imp_pc),
  "\nConditional draws per PC imputation = ", n_draw_within_pc,
  "\nTotal draws per wealth scenario = ", n_sim,
  "\n",
  sep = ""
)

message("Mortality household-wealth-quintile subgroup analysis completed.")
message("The same matched main-analysis bacterial-death proportions were applied to every wealth quintile.")
message("Outputs saved to: ", normalizePath(out_dir, mustWork = FALSE))
