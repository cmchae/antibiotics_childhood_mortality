# _____________________________________________ ----------
# 1. DHS data cleaning ----------------------------------------------------


rm(list = ls()); gc()

requiredPackages = c("rdhs", "dplyr", "tidyr", "gt", "zoo", "haven", "data.table", "labelled", "naniar")
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
  "caseid", "v000", "v001", "v005", "v006", "v007", "v008", 
  "v024", "v025",
  "h22", "h31", "h31b", "h31c",
  "v106", "v190",
  "v113", "v116", "v119", "v161",
  "v481",
  "hw72",
  "h37i", "h37j", "h37n", "h37o", "h37p",
  "h37a", "h37b", "h37c", "h37d", "h37da", "h37e", "h37aa", "h37ab", "h37f", "h37g", "h37h",
  "ml0", "v460"
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

length(extract) #164 (2003~2023)
datasets_to_remove <- c("ETKR61FL") # remove 2003 year only data
extract <- extract[!names(extract) %in% datasets_to_remove]
# assigning correct survey Year (Afghanistan and Nepal: assigning survey starting Year)
extract$AFKR71FL$v007 <- 2015 # based on the fieldwork period in the DHS dataset website
extract$NPKR61FL$v007 <- 2011 # based on the fieldwork period in the DHS dataset website
extract$NPKR7HFL$v007 <- 2016 # based on the fieldwork period in the DHS dataset website
extract$NPKR82FL$v007 <- 2022 # based on the fieldwork period in the DHS dataset website

# excluding DHS ver.4: -19 (remaining: 145)
version.4 <- names(extract)[substr(names(extract), 5, 5) == "4"]
length(version.4)
extract <- extract[!names(extract) %in% version.4] # Remove the specified datasets from the "extract" list
length(extract)



## datasets without variables: -6 (remaining: 139)
column_names <- lapply(extract, colnames); length(keep_variables) 
sum(lapply(column_names, length)<30) # Check the variables in datasets.
n_vars <- sapply(column_names, length)
sum(n_vars < length(keep_variables))

for (i in which(n_vars < length(keep_variables))) {
  cat("\nDataset index:", i, "\n")
  cat("Number of variables:", n_vars[i], "\n")
  cat("Missing variables:\n")
  print(setdiff(keep_variables, column_names[[i]]))
}

for (i in which(n_vars < length(keep_variables) - 5)) {
  print(names(extract)[i])
}

datasets_to_remove <- c("AMKR54FL", "COKR53FL", "EGKR51FL", "ETKR51FL", "GNKR52FL", "RWKR53FL")
extract <- extract[!names(extract) %in% datasets_to_remove] # Remove the specified datasets from the "extract" list
length(extract) #139

## datasets without respiratory symptoms information (h31~h31c): -0 (remaining: 139)
columns_to_check <- c("h22", "h31", "h31b", "h31c")
column_names <- lapply(extract, colnames) # Identify datasets that include all variables in columns_to_check
datasets_with_all_columns <- sapply(column_names, function(cols) {all(columns_to_check %in% cols)}) # Get the names or indices of the datasets that satisfy the condition
which(!datasets_with_all_columns)
length(extract) #139



# Exclude the datasets with only NA in antibiotic variables: -66 (remaining 73)
datasets_all_na_h37i_h37j <- c()
for (dataset_name in names(extract)) {
 df <- extract[[dataset_name]]
 if (all(c("h37i", "h37j") %in% names(df))) {
   if (all(is.na(df$h37i)) && all(is.na(df$h37j))) {
     datasets_all_na_h37i_h37j <- c(datasets_all_na_h37i_h37j, dataset_name)
   }}}
extract <- extract[!names(extract) %in% datasets_all_na_h37i_h37j]
length(extract) #73

# Antibiotic demand (h37)
# Treated with antibiotics: h37i(pills), h37j(inection), h37(f,g,n,o,p: Country specific antibiotic)
h37_cs <- c( #Country specific antibiotics: label
 "Amoxicillin taken for fever", "Beta lactum taken for fever",
 "Amoxicillin taken for fever/cough","Cotramoxazole (CS) taken for fever/cough",
 "CS other taken for fever/cough: Pen VK", "Macrolides for fever/cough",
 "Cotrimoxazole taken for fever/cough", "CS other for fever/cough: Erithromycin",
 "Bactrim taken for fever/cough", "Aminophyllin for fever/cough",
 "Septrin taken for fever/cough", "Erythromycine(CS) taken for fever/cough",
 "Beta lactum Tab/Syrup taken for fever/cough", "Ampicilin taken for fever/cough",
 "Amoxicillin(CS) taken for fever/cough", "Quinolone for fever/cough",
 "Macrolides Tablets taken for fever/cough", "Quinolone Syrup taken for fever/cough",
 "Other antibiotic taken for fever/cough", "Antibiotic", "Amoxycillin taken for fever/cough")
# finding antimalaria agent from Country specific malarial agent
h37_mal_lab <- c("NA - CS antimalarial taken for fever", "Other antimalarial taken for fever", "CS antimalarial taken for fever/cough", "Other antimalarial taken for fever/cough", "NA - Other antimalarial taken for fever", "NA - CS antimalarial taken for fever/cough", "NA - Other antimalarial taken for fever/cough", "CS antimalarial taken for fever", "CS antimalarial taken for fever/cough (Delagil)", "CS antimalarial taken for fever/cough (Premakhin)", "Artemether-Lumefantrine taken for fever/cough", "Mefloquine (CS) taken for fever/cough", "Artesunate/artinate (CS) taken for fever/cough", "Primaquine antimalarial taken for fever/cough", "CS antimalarial taken for f-NA", "NA - NA-CS antimalarial taken for fever/cough", "malarine: antimalarial taken for fever/cough", "A+M 2,3,4: antimalarial taken for fever/cough", "Other antimalarial taken fo-NA", "Unknown antimalarial taken for fever/cough", "NA-CS antimalarial taken for fever", "NA-CS antimalarial taken for fever/cough")

h37_vars <- c("h37f", "h37g", "h37h", "h37n", "h37o", "h37p")
h37_mal <- c("h37f", "h37g", "h37h")

for (dataset_name in names(extract)) {
 df <- extract[[dataset_name]]
 for (var in h37_mal) {
   if (var %in% names(df)) {
     new_var <- paste0(var, "_mal")   # Create new variable name
     df[[new_var]] <- df[[var]]       # Copy values
     var_label(df[[new_var]]) <- paste0(var_label(df[[var]]), " (mal)") # Optional: label
   }
 }
 extract[[dataset_name]] <- df
}

for (dataset_name in names(extract)) {
 df <- extract[[dataset_name]]
 for (var in h37_vars) {
   if (var %in% names(df)) {
     var_lbl <- var_label(df[[var]])
     if (is.null(var_lbl) || (!(var_lbl %in% h37_cs))) {
       df[[var]] <- NA }}}
 extract[[dataset_name]] <- df
}

h37_new_mal <- c("h37f_mal", "h37g_mal", "h37h_mal")

for (dataset_name in names(extract)) {
 df <- extract[[dataset_name]]
 for (var in h37_new_mal) {
   if (var %in% names(df)) {
     var_lbl <- var_label(df[[var]])
     if (is.null(var_lbl) || (!(var_lbl %in% h37_mal_lab))) {
       df[[var]] <- NA }}}
 extract[[dataset_name]] <- df
}

presence_matrix <-        # h37n~h37r table
 matrix(0, nrow = length(extract), ncol = length(h37_vars), dimnames = list(names(extract), h37_vars))

for (dataset_name in names(extract)) {
 df <- extract[[dataset_name]]
 for (var in h37_vars) {
   if (var %in% names(df) && any(!is.na(df[[var]]))) {
     presence_matrix[dataset_name, var] <- 1
   }}}

as.data.frame(presence_matrix)

# Combining the datasets into one dataframe and align the variables with order by A-Z
combined_data <- rbindlist(extract, fill = TRUE, ignore.attr = TRUE)
combined_data <- combined_data %>% dplyr::select(order(names(.))) 



#making overall antibiotic treatment variable
# have at least 1 -> 1, only have 0 and NA -> 0, only have NA -> NA
determine_value <- function(x) {                      # set the function for Combining healthcare facilitiy variables
  if (any(x == 1, na.rm = TRUE)) {return(1)}          # Check if there is at least one '1', ignoring NAs
  else if (any(x == 0, na.rm = TRUE)) {return(0)}     # Check if there is at least one '0', ignoring NAs
  else {return(NA)}}                                   # If there are no '1's or '0's, return NA

# label check
h37g_labels <- purrr::imap_dfr(
  extract,
  ~ tibble(
    filename = .y,
    h37g_present = "h37g" %in% names(.x),
    h37g_label = if ("h37g" %in% names(.x)) {
      as.character(labelled::var_label(.x$h37g))
    } else {
      NA_character_}))
print(h37g_labels, n = Inf)

combined_data$Ab_treatment <- # Combine the variables related to antibiotics #h37g recode is antimalarial but data label is antbiotics in NPK and PEK
  apply(combined_data[, c("h37i", "h37j", "h37f", "h37g", "h37h", "h37n", "h37o", "h37p")], 1, determine_value)

combined_data$mal_treatment <- # Combine the variables related to antimalarial agent
  apply(combined_data[, c("h37a", "h37b", "h37c", "h37d", "h37e", "h37da", "h37aa", "h37ab", "h37f_mal", "h37g_mal", "h37h_mal")], 1, determine_value)


combined_data <- combined_data %>% dplyr::select(-starts_with("h37"))
table(combined_data$Ab_treatment, useNA = 'always')/nrow(combined_data)       
table(combined_data$mal_treatment, useNA = 'always')/nrow(combined_data)      



# DHS survey version from filename
combined_data <- combined_data %>% mutate(survey_v = substr(filename, 5,5))

# Country name
Country_name <- c( AF7 = "Afghanistan", AL5 = "Albania", AL7 = "Albania", AM6 = "Armenia", AM7 = "Armenia", AO7 = "Angola", AZ5 = "Azerbaijan", BD5 = "Bangladesh", BD6 = "Bangladesh", BD7 = "Bangladesh", BD8 = "Bangladesh", BF6 = "Burkina Faso", BF8 = "Burkina Faso", BJ5 = "Benin", BJ6 = "Benin", BJ7 = "Benin", BO5 = "Bolivia", CD5 = "Democratic Republic of the Congo", CD6 = "Democratic Republic of the Congo", CG5 = "Republic of the Congo", CG6 = "Republic of the Congo", CI6 = "Cote d'Ivoire", CI8 = "Cote d'Ivoire", CM6 = "Cameroon", CM7 = "Cameroon", CO6 = "Colombia", CO7 = "Colombia", EG5 = "Egypt", EG6 = "Egypt", ET7 = "Ethiopia", ET8 = "Ethiopia", GA6 = "Gabon", GA7 = "Gabon", GH5 = "Ghana", GH7 = "Ghana", GH8 = "Ghana", GM6 = "Gambia", GM8 = "Gambia", GN6 = "Guinea", GN7 = "Guinea", GY5 = "Guyana", HN5 = "Honduras", HN6 = "Honduras", HT5 = "Haiti", HT6 = "Haiti", HT7 = "Haiti", ID5 = "Indonesia", ID6 = "Indonesia", ID7 = "Indonesia", JO5 = "Jordan", JO6 = "Jordan", JO7 = "Jordan", JO8 = "Jordan", KE5 = "Kenya", KE6 = "Kenya", KE7 = "Kenya", KE8 = "Kenya", KH5 = "Cambodia", KH6 = "Cambodia", KH7 = "Cambodia", KH8 = "Cambodia", KM6 = "Comoros", LS6 = "Lesotho", LS7 = "Lesotho", LS8 = "Lesotho", MD5 = "Madagascar", MD8 = "Madagascar", ML5 = "Mali", ML6 = "Mali", ML7 = "Mali", MM7 = "Myanmar", MR7 = "Mauritania", MV5 = "Maldives", MV7 = "Maldives", MW6 = "Malawi", MW7 = "Malawi", MZ6 = "Mozambique", MZ8 = "Mozambique", NG5 = "Nigeria", NG6 = "Nigeria", NG7 = "Nigeria", NI5 = "Niger", NI6 = "Niger", NP6 = "Nepal", NP7 = "Nepal", NP8 = "Nepal", PE5 = "Peru", PE6 = "Peru", PG7 = "Papua New Guinea", PH5 = "Philippines", PH6 = "Philippines", PH7 = "Philippines", PH8 = "Philippines", PK5 = "Pakistan", PK6 = "Pakistan", PK7 = "Pakistan", RW5 = "Rwanda", RW6 = "Rwanda", RW7 = "Rwanda", RW8 = "Rwanda", SL5 = "Sierra Leone", SL6 = "Sierra Leone", SL7 = "Sierra Leone", SN6 = "Senegal", SN7 = "Senegal", SN8 = "Senegal", ST5 = "Sao Tome and Principe", SZ5 = "Eswatini", TD7 = "Chad", TG6 = "Togo", TJ6 = "Tajikistan", TJ7 = "Tajikistan", TR5 = "Turkey", TR6 = "Turkey", TZ6 = "Tanzania", TZ7 = "Tanzania", TZ8 = "Tanzania", UA5 = "Ukraine", UG5 = "Uganda", UG6 = "Uganda", UG7 = "Uganda", YE6 = "Yemen", ZA7 = "South Africa", ZM5 = "Zambia", ZM6 = "Zambia", ZM7 = "Zambia", ZW5 = "Zimbabwe", ZW6 = "Zimbabwe", ZW7 = "Zimbabwe"
)

country_phase_all <- unique(
  paste0(
    substr(combined_data$filename, 1, 2),
    substr(combined_data$filename, 5, 5)
  )
)

setdiff(country_phase_all, names(Country_name))
setdiff(names(Country_name), country_phase_all)

combined_data <- combined_data %>%
  mutate(
    country_phase = paste0(
      substr(filename, 1, 2),
      substr(filename, 5, 5)
    ),
    Country = Country_name[country_phase],
    CountryYear = paste0(Country, "_", v007)
  )

setdiff(unique(combined_data$v000), names(Country_name))

combined_data_unique <- combined_data[!duplicated(combined_data[, c("filename", "Country")]), c("filename", "Country")]
length(unique(combined_data$Country)); length(unique(combined_data$v000)) #40 countries, 70 surveys
length(unique(combined_data$Country)); length(unique(combined_data$filename)) #40 countries, 73 dataset
sum(with(combined_data, table(Country, v007))>0) #105 datapoints (Country Year)
combined_data <- combined_data %>% filter(v007 > 2004)
sum(with(combined_data, table(Country, v007))>0) #103 datapoints (Country Year)


# Mutate "I don't know (6, 8, 9)" value in h22 (fever) and h31 (respiratory symptoms) to NA
combined_data[, c("h22","h31","h31b","h31c")] <- lapply(combined_data[, c("h22","h31","h31b","h31c")], function(x) ifelse(x  %in% c(6, 8, 9), NA, x))
combined_data$h31 <- ifelse(combined_data$h31 == 2, 1, combined_data$h31) # making 0, 1, NA format

# converting nutrition value to z-score
combined_data <- combined_data %>%
  mutate(wh_z_score = hw72 / 100, wh_z_score = ifelse(wh_z_score < -6 | wh_z_score > 6, NA, wh_z_score)) %>% dplyr::select(-hw72)

# h section
combined_data <- combined_data %>%
  rename(fever = h22, 
         cough = h31, 
         rapid_breath = h31b, 
         cpain_rnose = h31c)

## b section
combined_data %>% dplyr::select(starts_with("b")) %>% colnames()
b_columns <- combined_data %>% dplyr::select(starts_with("b"))
b_column_tables <- # check the values in variables in b section
  lapply(b_columns, function(column) {table(column, useNA = "always")})
b_column_tables
combined_data <- combined_data %>% # mutate the names of variables in b section
  rename(ch_bth_index = bidx, ch_bth_month = b3, ch_sex = b4, ch_alive = b5, ch_age_yr = b8)
colnames(combined_data)





## v section
combined_data %>% dplyr::select(starts_with("v")) %>% colnames()
v_columns <- combined_data %>% dplyr::select(starts_with("v"))
v_column_tables <- # check the values in variables in v section
  lapply(v_columns, function(column) {table(column, useNA = "always")})
v_column_tables

# mutate and rename the variables in v section
combined_data <- 
  combined_data %>% rename(
    survey = v000,
    cluster = v001,
    sample_weight = v005,
    Year = v007,
    survey_month = v006,
    int_month = v008,
    sub_region = v024,
    rural_urban = v025, 
    maternal_edu = v106,
    water = v113,          
    sanitation = v116,          
    electricity = v119,          
    cooking_fuel = v161,
    health_insurance = v481,
    hh_wealth = v190)

combined_data <- combined_data %>%
  mutate(
    sanitation = as.numeric(sanitation),
    water = as.numeric(water),
    cooking_fuel = as.numeric(cooking_fuel),
    ch_sex = as.numeric(ch_sex),
    rural_urban = as.numeric(rural_urban)
  )

combined_data <- combined_data %>%
  mutate(
    electricity = case_when(
      electricity == 0 ~ 0,
      electricity == 1 ~ 1,
      electricity %in% c(7, 9) ~ NA_real_,
      is.na(electricity) ~ NA_real_
    ),
    electricity = factor(
      electricity,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )


hh_columns <- combined_data %>% dplyr::select(starts_with("hh"))

combined_data[, c("maternal_edu")] <- ## Mutate "I don't know (8, 9)" to NA
  lapply(combined_data[, c("maternal_edu")], function(x) ifelse(x  %in% c(6, 8, 9), NA, x))



# check the duplication
combined_data <- combined_data %>% mutate(ch_id = paste(filename, caseid, ch_bth_index, sep = ""))
any(duplicated(combined_data$ch_id)) # check duplication of child_id: none
unique(any(duplicated(combined_data$ch_id)))
combined_data <- combined_data %>% dplyr::select(-c("caseid", "ch_bth_index"))



# check missing data
missing_counts <- sort(colSums(is.na(combined_data)), decreasing = TRUE)/nrow(combined_data)
missing_counts

## adding GBD regions
gbd_region <- read.csv("data/metadata/gbd_region.csv")
gbd_region[gbd_region$Country == "Côte d'Ivoire", "Country"] <- "Cote d'Ivoire"
gbd_region[gbd_region$Country == "Arab Republic of Egypt", "Country"] <- "Egypt"
gbd_region[gbd_region$Country == "Republic of Yemen", "Country"] <- "Yemen"
gbd_region[gbd_region$Country == "Democratic Republic of Congo", "Country"] <- "Democratic Republic of the Congo"
gbd_region[gbd_region$Country == "Congo", "Country"] <- "Republic of the Congo"
gbd_region[gbd_region$Country == "The Gambia", "Country"] <- "Gambia"
gbd_region[gbd_region$Country == "São Tomé and Príncipe", "Country"] <- "Sao Tome and Principe"

setdiff(combined_data$Country, gbd_region$Country)
combined_data <- combined_data %>% left_join(gbd_region, by = "Country")


## adding malarial endemic countries
mal.endemic.df <- read.csv('data/metadata/WHO2025_malarial_endemic.csv')
a <- unique(mal.endemic.df$Country)
b <- unique(combined_data$Country)
setdiff(a,b)
setdiff(b,a)

country_mapping <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Congo" = "Republic of the Congo",  
  "C\x99te d\xd5Ivoire" = "Cote d'Ivoire",
  "Comoros " = "Comoros",
  "United Republic of Tanzania" = "Tanzania",
  "Niger " = "Niger"
)

mal.endemic.df <- mal.endemic.df %>% mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))

combined_data <- combined_data %>% left_join(mal.endemic.df, by = c("Country" = "Country"))
combined_data <- combined_data %>% mutate(Endemic = ifelse(is.na(Endemic), 0, Endemic))
combined_data <- combined_data %>% mutate(v460 = case_when(Endemic == 0 & is.na(v460) ~ 0, TRUE ~ v460),
                                          ml0 = case_when(Endemic == 0 & is.na(ml0) ~ 0, TRUE ~ ml0))
with(combined_data, table(v460, ml0, Endemic, useNA = "ifany"))

# check the duplication
any(duplicated(combined_data$ch_id)) # check duplication of child_id: none

combined_data <- combined_data %>% mutate(    
  month_sin = sin(2 * pi * survey_month / 12),
  month_cos = cos(2 * pi * survey_month / 12)
)
colnames(combined_data)

#age in month
combined_data$ch_age_month <- combined_data$int_month - combined_data$ch_bth_month
combined_data <- combined_data %>% filter(ch_age_month >= 0, ch_age_month < 60) # exclude exactly 60 months
combined_data <- combined_data %>% filter(ch_alive == 1) # exclude deceased children (do not include information about syndrome)

combined_data <- combined_data %>%
  mutate(
    ch_age_group = case_when(
      ch_age_month >= 0  & ch_age_month <= 11 ~ "0-11 months",
      ch_age_month >= 12 & ch_age_month <= 23 ~ "12-23 months",
      ch_age_month >= 24 & ch_age_month < 60  ~ "24-59 months",
      TRUE ~ NA_character_
    ),
    ch_age_group = factor(
      ch_age_group,
      levels = c("0-11 months", "12-23 months", "24-59 months")
    )
  )



#only alive children have meaningful information
combined_data <- combined_data %>% filter(ch_alive == 1)

##selecting dataset
colnames(combined_data)

analysis_data <- combined_data %>%
  dplyr::select(c("Country", "Year", "gbd_region", "sub_region", "ch_id", "ch_age_group", "ch_sex", "survey_month", "sample_weight", "wh_z_score", "rural_urban", "Endemic", "maternal_edu", "water", "sanitation", "electricity", "health_insurance", "cooking_fuel", "hh_wealth", "fever", "cough", "rapid_breath", "cpain_rnose", "filename", "survey_v", "Ab_treatment", "mal_treatment", "month_sin", "month_cos", "cluster"))


# Remove rows where cough or fever is NA
with(analysis_data, table(cough,fever, useNA = 'always'))/nrow(analysis_data) # any of fever/cough missing 6.7%; both missing 5.2% 
analysis_data <- analysis_data[!is.na(analysis_data$cough) & !is.na(analysis_data$fever), ]


# Check the distribution of symptoms
with(analysis_data, table(cough, useNA='always')); with(analysis_data, table(fever, useNA='always'))
with(analysis_data, table(rapid_breath, useNA='always')); with(analysis_data, table(cpain_rnose, useNA='always'))
with(analysis_data, table(cough, rapid_breath, useNA='always'))
with(analysis_data, table(cough, cpain_rnose, useNA='always'))
with(analysis_data, table(rapid_breath, cpain_rnose, useNA='always'))
with(analysis_data, table(Ab_treatment, useNA = 'always'))
with(analysis_data, table(fever, Ab_treatment, mal_treatment), margin = 1)


# LRTI variable
analysis_data <- analysis_data %>%
  mutate(LRTI = case_when(
    cough == 1 & rapid_breath == 1 & cpain_rnose %in% c(1, 3) ~ 1L,
    cough == 0 ~ 0L,
    cough == 1 & rapid_breath == 0 ~ 0L,
    cough == 1 & rapid_breath == 1 & !(cpain_rnose %in% c(1, 3)) ~ 0L,
    TRUE ~ 0)) # cough = 1, rapid_breat = NA --> lrti = 0

# LRTI ~ Fever table by survey version
with(analysis_data, table(fever, LRTI))/nrow(analysis_data) # LRTI ~ 5.4%, Fever ~ 21% 

# fever variable
analysis_data <- analysis_data %>%
  mutate(
    fever = case_when(
      fever == 1 & LRTI == 1 ~ 0L,
      fever == 1 & LRTI == 0 ~ 1L,
      fever == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

with(analysis_data, table(LRTI, cpain_rnose,cough,rapid_breath))
with(analysis_data, table(fever, LRTI))/nrow(analysis_data) # LRTI ~ 5.4%, nonLRTI-Fever ~ 17.6% 
with(analysis_data, table(Ab_treatment, fever, useNA = 'always'))
with(analysis_data, table(Ab_treatment, LRTI, useNA = 'always'))

analysis_data <- analysis_data %>% mutate(health_insurance = case_when(
  health_insurance == 9 ~ NA_real_,
  health_insurance %in% c(0,1) ~ health_insurance,
  is.na(health_insurance) ~ NA_real_
))


# LRTI ~ Fever table by survey version
as.data.frame(with(analysis_data, table(fever, LRTI, survey_v)))

# LRTI ~ Cough table by survey version
as.data.frame(with(analysis_data, table(cough, LRTI, survey_v)))

# Cough ~ Fever table by survey version
as.data.frame(with(analysis_data, table(fever, cough, survey_v)))


# make Country_Year vLRTIable
with(analysis_data, table(Year, Country, useNA ='always'))
with(analysis_data[analysis_data$LRTI ==1,], table(Year, Country, useNA ='always'))
with(analysis_data[analysis_data$fever ==1,], table(Year, Country, useNA ='always'))

with(analysis_data[analysis_data$Ab_treatment ==1 & analysis_data$LRTI ==1,], table(Year, Country, useNA ='always'))/with(analysis_data[analysis_data$LRTI ==1,], table(Year, Country, useNA ='always'))
with(analysis_data[analysis_data$Ab_treatment ==1 & analysis_data$fever ==1,], table(Year, Country, useNA ='always'))/with(analysis_data[analysis_data$fever ==1,], table(Year, Country, useNA ='always'))


analysis_data$CountryYear <- paste(analysis_data$Country, analysis_data$Year, sep = "_")
unique(analysis_data$CountryYear)

# merging and excluding the CountryYear with very small number of Antibiotics treatment and LRTI data.
with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 39 countries, 102 country-year
with(analysis_data[LRTI == 1], table(CountryYear, Ab_treatment))
with(analysis_data[fever == 1], table(CountryYear, Ab_treatment))

analysis_data$CountryYear[analysis_data$CountryYear == "Albania_2008"] <- "Albania_2009" 
analysis_data$CountryYear[analysis_data$CountryYear == "Albania_2018"] <- "Albania_2017" 
analysis_data$CountryYear[analysis_data$CountryYear == "Armenia_2015"] <- "Armenia_2016" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cambodia_2011"] <- "Cambodia_2010" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cambodia_2022"] <- "Cambodia_2021" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cameroon_2019"] <- "Cameroon_2018" 
analysis_data$CountryYear[analysis_data$CountryYear == "Eswatini_2007"] <- "Eswatini_2006" 
analysis_data$CountryYear[analysis_data$CountryYear == "Gabon_2019"] <- "Gabon_2020" 
analysis_data$CountryYear[analysis_data$CountryYear == "Ghana_2023"] <- "Ghana_2022" 
analysis_data$CountryYear[analysis_data$CountryYear == "Lesotho_2023"] <- "Lesotho_2024" 
analysis_data$CountryYear[analysis_data$CountryYear == "Maldives_2016"] <- "Maldives_2017" 
analysis_data$CountryYear[analysis_data$CountryYear == "Mozambique_2023"] <- "Mozambique_2022" 
analysis_data$CountryYear[analysis_data$CountryYear == "Peru_2005"] <- "Peru_2006" 
analysis_data$CountryYear[analysis_data$CountryYear == "Zambia_2019"] <- "Zambia_2018" 

with(analysis_data[LRTI == 1], table(CountryYear, Ab_treatment))
with(analysis_data[fever == 1], table(CountryYear, Ab_treatment))

analysis_data$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", analysis_data$CountryYear)) # applying same change to Year variable.

with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 39 countries, 99 country-year
nrow(distinct(analysis_data[,c("Country","Year")]))

# add GDP variable
analysis_data[analysis_data$Country == "Egypt",]$Country <- "Egypt, Arab Rep."
analysis_data[analysis_data$Country == "Yemen",]$Country <- "Yemen, Rep."

GDP.PPP <- read.csv("data/metadata/GDP_PPP.csv")
colnames(GDP.PPP)[-1] <- gsub("^X", "", colnames(GDP.PPP)[-1])
GDP.PPP <- GDP.PPP %>% pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP")
GDP.PPP$Year <- as.numeric(GDP.PPP$Year)
a <- unique(GDP.PPP$Country)
b <- unique(analysis_data$Country)
setdiff(b,a)

country_mapping <- c(
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Republic of the Congo",  
  "Gambia, The" = "Gambia",
  "Egypt, Arab Rep." = "Egypt",
  "Yemen, Rep." = "Yemen")

GDP.PPP <- GDP.PPP %>% mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))
GDP.PPP$CountryYear <- paste(GDP.PPP$Country, GDP.PPP$Year, sep = "_")

GDP.PPP[GDP.PPP$Country == "Egypt",]$Country <- "Egypt, Arab Rep."
GDP.PPP[GDP.PPP$Country == "Yemen",]$Country <- "Yemen, Rep."

a <- unique(GDP.PPP$Country)
b <- unique(analysis_data$Country)
setdiff(b,a)

a <- unique(analysis_data$CountryYear)
b <- unique(GDP.PPP$CountryYear)
setdiff(a,b)

analysis_data <- analysis_data %>% inner_join(GDP.PPP, by = c("Country" = "Country", "Year" = "Year", "CountryYear" = "CountryYear"))
unique(analysis_data$GDP)
analysis_data %>% dplyr::select(c(Country, Year, GDP)) %>% distinct()
analysis_data <- analysis_data %>%
  mutate(GDP = ifelse(Country == "Yemen" & Year == 2013 & is.na(GDP), 110.19/95.14*3164, GDP)) # Yemen does not have the value for GDP per capita in 2021 international dollar so I used proxy using current international dollar and US GDP deflation index


# sub-national region
analysis_data <- analysis_data %>% mutate(sub_region_id = paste(Country, Year, sub_region, sep = "_"))

analysis_data <- analysis_data %>%
  dplyr::select(c("Country", "Year", "gbd_region", "sub_region_id", "ch_id", "ch_age_group", "ch_sex", "survey_month", "month_sin", "month_cos", "sample_weight", "wh_z_score", "rural_urban", "Endemic", "maternal_edu", "water", "sanitation", "electricity", "cooking_fuel", "hh_wealth", "fever", "LRTI", "CountryYear", "GDP", "filename", "health_insurance", "Ab_treatment", "cluster"))

check_var_labels_by_file <- function(extract, filenames, varname = "v116", target_codes = NULL) {
  
  label_list <- map_dfr(filenames, function(fn) {
    
    if (!fn %in% names(extract)) {
      return(tibble(
        filename = fn,
        variable = varname,
        code = NA_real_,
        label = NA_character_,
        note = "filename not found in extract"))}
    
    if (!varname %in% names(extract[[fn]])) {
      return(tibble(
        filename = fn,
        variable = varname,
        code = NA_real_,
        label = NA_character_,
        note = "variable not found in dataset"))}
    
    x <- extract[[fn]][[varname]]
    labs <- labelled::val_labels(x)
    
    if (length(labs) == 0) {
      return(tibble(
        filename = fn,
        variable = varname,
        code = NA_real_,
        label = NA_character_,
        note = "no value labels"))}
    
    out <- tibble(
      filename = fn,
      variable = varname,
      label = names(labs),
      code = as.numeric(labs),
      note = NA_character_)
    
    if (!is.null(target_codes)) {
      out <- out %>% filter(code %in% target_codes)} 
    out})
  
  label_list %>%arrange(filename, code)
}

# Build filename-code-label map from original extract list
fuel_label_map <- purrr::map_dfr(names(extract), function(fn) {
  if (!"v161" %in% names(extract[[fn]])) {
    return(tibble::tibble(
      filename = fn,
      cooking_fuel_num = NA_real_,
      fuel_label = NA_character_
    ))
  }
  
  labs <- labelled::val_labels(extract[[fn]]$v161)
  
  tibble::tibble(
    filename = fn,
    fuel_label = names(labs),
    cooking_fuel_num = as.numeric(labs)
  )
}) %>%
  mutate(
    fuel_label_lower = stringr::str_to_lower(fuel_label),
    
    fuel_collapsed_label = case_when(
      stringr::str_detect(
        fuel_label_lower,
        "electric|solar|lpg|liquefied|natural gas|piped natural gas|cylinder gas|compressed gas|biogas|bio gas|biogaz|kerosene|paraffin|alcohol|ethanol|gasoline|diesel"
      ) ~ "Non-solid",
      
      stringr::str_detect(
        fuel_label_lower,
        "coal|lignite|charcoal|wood|straw|shrubs|grass|agricultural|crop|dung|biomass|pellet|woodchip|wood chips|garbage|plastic|sawdust|saw dust|briquette"
      ) ~ "Solid",
      
      stringr::str_detect(
        fuel_label_lower,
        "no food cooked|other|not.*resident|missing"
      ) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(filename, cooking_fuel_num, fuel_collapsed_label)

# check sanitation
sanitation_check_files <- analysis_data %>%
  filter(as.numeric(sanitation) %in% c(16, 17, 24, 25, 32, 43, 44, 45)) %>%
  distinct(filename) %>%
  pull(filename)

sanitation_labels_target <- check_var_labels_by_file(
  extract = extract,
  filenames = sanitation_check_files,
  varname = "v116",
  target_codes = c(16, 17, 24, 25, 32, 43, 44, 45)
)

sanitation_labels_target %>%
  as_tibble() %>%
  print(n = Inf)

# check cooking_fuel
fuel_check_files <- analysis_data %>%
  filter(as.numeric(cooking_fuel) %in% c(2, 13, 14, 15, 16, 17)) %>%
  distinct(filename) %>%
  pull(filename)

fuel_labels_target <- check_var_labels_by_file(
  extract = extract,
  filenames = fuel_check_files,
  varname = "v161",
  target_codes = c(2, 13, 14, 15, 16, 17)
)

fuel_labels_target %>%
  as_tibble() %>%
  print(n = Inf)


analysis_data <- analysis_data %>%
  mutate(
    water_num = as.numeric(water),
    sanitation_num = as.numeric(sanitation),
    cooking_fuel_num = as.numeric(cooking_fuel)
  ) %>%
  left_join(
    fuel_label_map,
    by = c("filename", "cooking_fuel_num")
  ) %>%
  mutate(
    water_collapsed = case_when(
      water_num %in% c(11, 12, 13, 14, 15, 16, 21, 31, 41, 51, 61, 71, 72) ~ "Improved water",
      water_num %in% c(
        22, 23, 32, 33, 34, 35, 36, 37, 38,
        42, 43, 44, 45, 46, 47,
        52, 62, 63, 73, 81, 82, 91, 92, 96
      ) ~ "Unimproved water",
      water_num %in% c(97, 99) ~ NA_character_,
      is.na(water_num) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    sanitation_collapsed = case_when(
      sanitation_num %in% c(11, 12, 13, 21, 22, 41) ~ "Improved sanitation",
      
      filename == "EGKR61FL" & sanitation_num == 16 ~ "Improved sanitation",
      filename %in% c("GHKR8AFL", "GHKR8BFL") & sanitation_num == 16 ~ "Improved sanitation",
      filename == "AFKR71FL" & sanitation_num == 44 ~ "Improved sanitation",
      filename == "UGKR61FL" & sanitation_num == 44 ~ "Improved sanitation",
      filename == "ZAKR71FL" & sanitation_num == 44 ~ "Improved sanitation",
      filename == "HTKR61FL" & sanitation_num == 45 ~ "Improved sanitation",
      filename == "NPKR82FL" & sanitation_num == 45 ~ "Improved sanitation",
      
      sanitation_num %in% c(30, 31, 32) ~ "Open defecation",
      
      sanitation_num %in% c(
        14, 15, 16, 17, 18,
        23, 24, 25,
        42, 43, 44, 45,
        51, 52, 53, 54,
        71,
        96
      ) ~ "Unimproved sanitation",
      
      sanitation_num %in% c(97, 99) ~ NA_character_,
      is.na(sanitation_num) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    fuel_collapsed = fuel_collapsed_label,
    
    water_collapsed = factor(
      water_collapsed,
      levels = c("Improved water", "Unimproved water")
    ),
    
    sanitation_collapsed = factor(
      sanitation_collapsed,
      levels = c("Improved sanitation", "Unimproved sanitation", "Open defecation")
    ),
    
    fuel_collapsed = factor(
      fuel_collapsed,
      levels = c("Non-solid", "Solid")
    )
  ) %>%
  dplyr::select(-fuel_collapsed_label)

analysis_data %>%
  filter(is.na(fuel_collapsed) & !is.na(cooking_fuel_num)) %>%
  count(filename, CountryYear, cooking_fuel_num, sort = TRUE) %>%
  as_tibble() %>%
  print(n = 200)


analysis_data %>%
  filter(is.na(sanitation_collapsed) & !is.na(sanitation_num)) %>%
  count(filename, CountryYear, sanitation_num, sort = TRUE) %>%
  as_tibble() %>%
  print(n = 200)

analysis_data %>%
  filter(is.na(water_collapsed) & !is.na(water_num)) %>%
  count(filename, CountryYear, water_num, sort = TRUE) %>%
  as_tibble() %>%
  print(n = 200)

# check missing data
missing_counts <- sort(colSums(is.na(analysis_data)), decreasing = TRUE)/nrow(analysis_data)

analysis_data_LRTI <- analysis_data %>% filter(LRTI == 1)
sort(colSums(is.na(analysis_data_LRTI)), decreasing = TRUE)/nrow(analysis_data_LRTI)
analysis_data_LRTI <- analysis_data_LRTI[!is.na(analysis_data_LRTI$Ab_treatment),]

analysis_data_Fever <- analysis_data %>% filter(fever == 1)
sort(colSums(is.na(analysis_data_Fever)), decreasing = TRUE)/nrow(analysis_data_Fever)
analysis_data_Fever <- analysis_data_Fever[!is.na(analysis_data_Fever$Ab_treatment),]

saveRDS(analysis_data_LRTI, file ="outputs/pipelines/AA_LRTI_analysis_data.rds")
saveRDS(analysis_data_Fever, file ="outputs/pipelines/AA_Fever_analysis_data.rds")


rm(list = ls())
gc()









# _____________________________________________ ----------
# 4. imputation of missing data -------------------
library(dplyr)
library(mice)
library(lme4)
library(purrr)
library(tibble)
library(labelled)
library(openxlsx)
library(gridExtra)
library(grid)
library(future)
library(furrr)

rm(list = ls()); gc()

analysis_data_LRTI <- readRDS("outputs/pipelines/AA_LRTI_analysis_data.rds")
analysis_data_Fever <- readRDS("outputs/pipelines/AA_Fever_analysis_data.rds")

# Variables to report in the missingness table
missing_vars <- c("fever", "LRTI", "Ab_treatment", 
                  "ch_age_group", "ch_sex",
                  "wh_z_score", 
                  "water_collapsed", "electricity", "sanitation_collapsed", "fuel_collapsed",
                  "rural_urban", "hh_wealth", "maternal_edu", "health_insurance",
                  "gbd_region", "sub_region_id", 
                  "month_sin", "month_cos", "Endemic",
                  "sample_weight")    

# Optional readable labels
var_labels <- c(
  fever = "Fever in previous 2 weeks",
  LRTI = "Lower respiratory tract infection",
  Ab_treatment = "Antibiotic treatment",
  ch_age_group = "Child age group",
  ch_sex = "Child sex",
  wh_z_score = "Weight-for-height z-score",
  water_collapsed = "Drinking water category",
  electricity = "Household electricity",
  sanitation_collapsed = "Sanitation category",
  fuel_collapsed = "Cooking fuel category",
  rural_urban = "Rural or urban residence",
  hh_wealth = "Household wealth quintile",
  maternal_edu = "Maternal education",
  health_insurance = "Health insurance",
  gbd_region = "GBD region",
  GDP = "GDP per capita",
  month_sin = "Seasonality sine term",
  month_cos = "Seasonality cosine term",
  Endemic = "Malaria endemicity",
  sample_weight = "DHS sampling weight"
)

missing_table_LRTI <- analysis_data_LRTI %>%
  summarise(across(all_of(missing_vars), list(n_missing = ~sum(is.na(.x)), pct_missing = ~mean(is.na(.x)) * 100, n_nonmissing = ~sum(!is.na(.x))), .names = "{.col}__{.fn}")) %>%
  tidyr::pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "__") %>%
  mutate(variable_label = unname(var_labels[variable]), n_total = nrow(analysis_data_LRTI), pct_missing = round(pct_missing, 2)) %>%
  dplyr::select(variable, variable_label, n_total, n_nonmissing, n_missing, pct_missing) %>%
  arrange(desc(pct_missing), variable)

missing_table_LRTI

missing_table_Fever <- analysis_data_Fever %>%
  summarise(across(all_of(missing_vars), list(n_missing = ~sum(is.na(.x)), pct_missing = ~mean(is.na(.x)) * 100, n_nonmissing = ~sum(!is.na(.x))), .names = "{.col}__{.fn}")) %>%
  tidyr::pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "__") %>%
  mutate(variable_label = unname(var_labels[variable]), n_total = nrow(analysis_data_Fever), pct_missing = round(pct_missing, 2)) %>%
  dplyr::select(variable, variable_label, n_total, n_nonmissing, n_missing, pct_missing) %>%
  arrange(desc(pct_missing), variable)

missing_table_Fever

write.xlsx(missing_table_LRTI, file = "outputs/supplementary/AA_missingness_table_LRTI.xlsx", overwrite = TRUE)
write.xlsx(missing_table_Fever, file = "outputs/supplementary/AA_missingness_table_fever.xlsx", overwrite = TRUE)

model_data_mi_LRTI <- analysis_data_LRTI %>%
  dplyr::select(
    ch_id,
    filename,
    Country, Year, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    health_insurance, Ab_treatment,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, GDP, Endemic,
    fever, LRTI,
    sub_region_id,
    sample_weight
  ) %>%
  mutate(
    Year_c = Year - 2010,
    log_GDP = log(GDP),
    log_GDP_z = as.numeric(scale(log_GDP)),
    hh_wealth = factor(
      as.integer(hh_wealth),
      levels = 1:5,
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    health_insurance = factor(health_insurance),
    filename = factor(filename),
    Country = factor(Country),
    CountryYear = factor(CountryYear),
    ch_age_group = factor(ch_age_group),
    ch_sex = factor(ch_sex),
    water_collapsed = factor(water_collapsed),
    sanitation_collapsed = factor(sanitation_collapsed),
    electricity = factor(electricity),
    fuel_collapsed = factor(fuel_collapsed),
    rural_urban = factor(rural_urban),
    maternal_edu = factor(maternal_edu),
    gbd_region = factor(gbd_region),
    Endemic = factor(Endemic),
    sub_region_id = factor(sub_region_id),
    sample_weight = sample_weight / 1000000,
    fever = as.integer(fever),
    LRTI = as.integer(LRTI),
    Ab_treatment = as.integer(Ab_treatment)
  ) %>%
  dplyr::select(
    ch_id,
    filename,
    Country, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    health_insurance, Ab_treatment,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, Endemic, log_GDP_z, Year_c,
    fever, LRTI,
    sub_region_id,
    sample_weight
  ) %>%
  as.data.frame()

model_data_mi_Fever <- analysis_data_Fever %>%
  dplyr::select(
    ch_id,
    filename,
    Country, Year, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    health_insurance, Ab_treatment,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, GDP, Endemic,
    fever, LRTI,
    sub_region_id,
    sample_weight
  ) %>%
  mutate(
    Year_c = Year - 2010,
    log_GDP = log(GDP),
    log_GDP_z = as.numeric(scale(log_GDP)),
    hh_wealth = factor(
      as.integer(hh_wealth),
      levels = 1:5,
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    health_insurance = factor(health_insurance),
    filename = factor(filename),
    Country = factor(Country),
    CountryYear = factor(CountryYear),
    ch_age_group = factor(ch_age_group),
    ch_sex = factor(ch_sex),
    water_collapsed = factor(water_collapsed),
    sanitation_collapsed = factor(sanitation_collapsed),
    electricity = factor(electricity),
    fuel_collapsed = factor(fuel_collapsed),
    rural_urban = factor(rural_urban),
    maternal_edu = factor(maternal_edu),
    gbd_region = factor(gbd_region),
    Endemic = factor(Endemic),
    sub_region_id = factor(sub_region_id),
    sample_weight = sample_weight / 1000000,
    fever = as.integer(fever),
    LRTI = as.integer(LRTI),
    Ab_treatment = as.integer(Ab_treatment)
  ) %>%
  dplyr::select(
    ch_id,
    filename,
    Country, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    health_insurance, Ab_treatment,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, Endemic, log_GDP_z, Year_c,
    fever, LRTI,
    sub_region_id,
    sample_weight
  ) %>%
  as.data.frame()

sort(colMeans(is.na(model_data_mi_LRTI)), decreasing = TRUE)
sort(colMeans(is.na(model_data_mi_Fever)), decreasing = TRUE)


#LRTI
ini_LRTI <- mice(model_data_mi_LRTI, maxit = 0)
meth_LRTI <- ini_LRTI$method
pred_LRTI <- ini_LRTI$predictorMatrix

meth_LRTI[] <- ""

meth_LRTI["wh_z_score"] <- "pmm" 
meth_LRTI["health_insurance"] <- "logreg"
meth_LRTI["electricity"] <- "logreg"
meth_LRTI["water_collapsed"] <- "logreg"
meth_LRTI["fuel_collapsed"] <- "logreg"
meth_LRTI["sanitation_collapsed"] <- "polyreg"
meth_LRTI["maternal_edu"] <- "polyreg"

pred_LRTI[,] <- 0

imputed_vars_LRTI <- c(
  "wh_z_score",
  "health_insurance",
  "electricity",
  "water_collapsed",
  "fuel_collapsed",
  "sanitation_collapsed",
  "maternal_edu"
)

predictor_vars_LRTI <- c(
  "Year_c",
  "ch_age_group", "ch_sex",
  "rural_urban",
  "hh_wealth",
  "month_sin", "month_cos",
  "gbd_region", "Endemic", "log_GDP_z",
  "fever", "LRTI", "Ab_treatment",
  "sample_weight"
)

pred_LRTI[imputed_vars_LRTI, predictor_vars_LRTI] <- 1

# Allow imputed variables to predict each other
pred_LRTI[imputed_vars_LRTI, imputed_vars_LRTI] <- 1
diag(pred_LRTI) <- 0

# Do not use identifiers as predictors
pred_LRTI[, c("ch_id", "filename", "CountryYear", "sub_region_id")] <- 0
pred_LRTI[c("ch_id", "filename", "CountryYear", "sub_region_id"), ] <- 0

## Parallel multiple imputation ---------------------------------------------
# Use three workers to limit memory use.
# Each worker holds its own copy of the data and imputation models.
n_core_mi <- min(3L, future::availableCores())

Sys.time()

imp_LRTI <- mice::futuremice(
  data = model_data_mi_LRTI,
  m = 5,
  maxit = 10,
  method = meth_LRTI,
  predictorMatrix = pred_LRTI,
  ridge = 1e-04,
  n.core = n_core_mi,
  parallelseed = 123,
  future.plan = "multisession"
)

Sys.time()
future::plan(future::sequential)

imp_LRTI
summary(imp_LRTI)
plot(imp_LRTI)

complete_data_check_LRTI <- mice::complete(imp_LRTI, 1)

complete_data_check_LRTI %>%
  summarise(
    wh_z_missing = mean(is.na(wh_z_score)),
    water_missing = mean(is.na(water_collapsed)),
    sanitation_missing = mean(is.na(sanitation_collapsed)),
    electricity_missing = mean(is.na(electricity)),
    fuel_missing = mean(is.na(fuel_collapsed)),
    health_insurance_missing = mean(is.na(health_insurance)))

densityplot(imp_LRTI, ~ wh_z_score)
stripplot(imp_LRTI, wh_z_score ~ .imp, pch = 20, cex = 0.3)

if (!is.null(imp_LRTI$loggedEvents)) {
  imp_LRTI$loggedEvents %>%
    as_tibble() %>%
    count(dep, meth, out, sort = TRUE) %>%
    print(n = 500)
}

saveRDS(imp_LRTI, "outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_LRTI.rds")










#Fever
ini_Fever <- mice(model_data_mi_Fever, maxit = 0)
meth_Fever <- ini_Fever$method
pred_Fever <- ini_Fever$predictorMatrix

meth_Fever[] <- ""
meth_Fever["wh_z_score"] <- "pmm" 
meth_Fever["health_insurance"] <- "logreg"
meth_Fever["electricity"] <- "logreg"
meth_Fever["water_collapsed"] <- "logreg"
meth_Fever["fuel_collapsed"] <- "logreg"
meth_Fever["sanitation_collapsed"] <- "polyreg"
meth_Fever["maternal_edu"] <- "polyreg"

pred_Fever[,] <- 0

imputed_vars_Fever <- c(
  "wh_z_score",
  "electricity",
  "water_collapsed",
  "sanitation_collapsed",
  "fuel_collapsed",
  "health_insurance",
  "maternal_edu"
)

predictor_vars_Fever <- c(
  "Year_c",
  "ch_age_group", "ch_sex",
  "rural_urban",
  "hh_wealth", 
  "month_sin", "month_cos",
  "gbd_region", "Endemic", "log_GDP_z",
  "fever", "LRTI", "Ab_treatment",
  "sample_weight"
)

pred_Fever[imputed_vars_Fever, predictor_vars_Fever] <- 1

# Allow imputed variables to predict each other
pred_Fever[imputed_vars_Fever, imputed_vars_Fever] <- 1
diag(pred_Fever) <- 0

# Do not use identifiers as predictors
pred_Fever[, c("ch_id", "filename", "CountryYear", "sub_region_id")] <- 0
pred_Fever[c("ch_id", "filename", "CountryYear", "sub_region_id"), ] <- 0

Sys.time()

imp_Fever <- mice::futuremice(
  data = model_data_mi_Fever,
  m = 5,
  maxit = 10,
  method = meth_Fever,
  predictorMatrix = pred_Fever,
  ridge = 1e-04,
  n.core = n_core_mi,
  parallelseed = 123,
  future.plan = "multisession"
)

Sys.time()
future::plan(future::sequential)

imp_Fever
summary(imp_Fever)
plot(imp_Fever)

complete_data_check_Fever <- mice::complete(imp_Fever, 1)

complete_data_check_Fever %>%
  summarise(
    wh_z_missing = mean(is.na(wh_z_score)),
    water_missing = mean(is.na(water_collapsed)),
    sanitation_missing = mean(is.na(sanitation_collapsed)),
    electricity_missing = mean(is.na(electricity)),
    fuel_missing = mean(is.na(fuel_collapsed)),
    maternal_edu_missing = mean(is.na(maternal_edu)),
    health_insurance_missing = mean(is.na(health_insurance)))

densityplot(imp_Fever, ~ wh_z_score)
stripplot(imp_Fever, wh_z_score ~ .imp, pch = 20, cex = 0.3)

if (!is.null(imp_Fever$loggedEvents)) {
  imp_Fever$loggedEvents %>%
    as_tibble() %>%
    count(dep, meth, out, sort = TRUE) %>%
    print(n = 500)
}

saveRDS(imp_Fever, "outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_Fever.rds")
















# _____________________________________________ ----------
# 5. Estimation of antibiotic treatment among fever and LRTI cases -------------
# Logistic GLMMs with model-based SEs and MI pooling

rm(list = ls()); gc()

library(dplyr)
library(mice)
library(glmmTMB)
library(future)
library(future.apply)
library(stringr)
library(grid)
library(gridExtra)
library(tibble)



imp_LRTI <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_LRTI.rds")
imp_Fever <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_Fever.rds")

plan(multisession, workers = 3)

completed_data_LRTI <- mice::complete(imp_LRTI, "all")
completed_data_Fever <- mice::complete(imp_Fever, "all")

completed_data_LRTI <- lapply(completed_data_LRTI, function(dat) {
  dat %>%
    mutate(
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE),
      sample_weight_scaled = sample_weight / mean(sample_weight, na.rm = TRUE))
})

completed_data_Fever <- lapply(completed_data_Fever, function(dat) {
  dat %>%
    mutate(
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE),
      sample_weight_scaled = sample_weight / mean(sample_weight, na.rm = TRUE))
})

# LRTI models ------------------------------------------------------------------

Sys.time()

fit_lrti_list <- future_lapply(completed_data_LRTI, function(dat) {
  glmmTMB::glmmTMB(
    Ab_treatment ~ ch_age_group + ch_sex + wh_z_score +
      water_collapsed + sanitation_collapsed + electricity + fuel_collapsed +
      health_insurance + rural_urban + hh_wealth + maternal_edu +
      Endemic + Year_c + log_GDP_z +
      (1 | gbd_region) +
      (1 | sub_region_nested_id),
    data = dat,
    family = binomial(link = "logit"),
    weights = sample_weight_scaled,
    REML = FALSE,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2e5, eval.max = 2e5)
    )
  )
}, future.seed = TRUE)

fit_lrti_mi <- mice::as.mira(fit_lrti_list)

# Save fitted models
saveRDS(fit_lrti_mi, "outputs/pipelines/(GBD_RE)AA_fit_lrti_mi.rds")

summary(fit_lrti_list[[1]])

coef_names_lrti <- names(glmmTMB::fixef(fit_lrti_list[[1]])$cond)

stopifnot(all(vapply(fit_lrti_list, function(fit_i) {
  identical(names(glmmTMB::fixef(fit_i)$cond), coef_names_lrti)
}, logical(1))))

beta_lrti <- do.call(cbind, lapply(fit_lrti_list, function(fit_i) {
  glmmTMB::fixef(fit_i)$cond[coef_names_lrti]
}))

# Model-based within-imputation variances
model_var_lrti <- do.call(cbind, lapply(fit_lrti_list, function(fit_i) {
  
  vcov_i <- as.matrix(stats::vcov(fit_i)$cond)
  
  if (!all(coef_names_lrti %in% rownames(vcov_i))) {
    stop("LRTI coefficient names do not match the covariance matrix.")
  }
  
  vcov_i <- vcov_i[coef_names_lrti, coef_names_lrti, drop = FALSE]
  vcov_i <- (vcov_i + t(vcov_i)) / 2
  
  if (any(!is.finite(vcov_i))) {
    stop("Non-finite LRTI covariance values.")
  }
  
  diag(vcov_i)
}))

# Rubin pooling using model-based within-imputation variances
m_lrti <- ncol(beta_lrti)
qbar_lrti <- rowMeans(beta_lrti)
ubar_lrti <- rowMeans(model_var_lrti)
b_lrti <- apply(beta_lrti, 1, var)

total_var_lrti <- ubar_lrti + (1 + 1 / m_lrti) * b_lrti
se_lrti <- sqrt(total_var_lrti)

r_lrti <- (1 + 1 / m_lrti) * b_lrti / ubar_lrti
df_lrti <- (m_lrti - 1) * (1 + 1 / r_lrti)^2
df_lrti[!is.finite(df_lrti)] <- 999999

statistic_lrti <- qbar_lrti / se_lrti
critical_lrti <- qt(0.975, df = df_lrti)


summary_lrti <- tibble(
  term = coef_names_lrti,
  estimate_log_odds = qbar_lrti,
  std_error = se_lrti,
  statistic = statistic_lrti,
  df = df_lrti,
  p.value = 2 * pt(abs(statistic_lrti), df = df_lrti, lower.tail = FALSE),
  estimate = exp(qbar_lrti),
  conf.low = exp(qbar_lrti - critical_lrti * se_lrti),
  conf.high = exp(qbar_lrti + critical_lrti * se_lrti)
)

pooled_lrti <- list(
  m = m_lrti,
  qbar = qbar_lrti,
  ubar = ubar_lrti,
  between_imputation_variance = b_lrti,
  total_variance = total_var_lrti,
  df = df_lrti
)

print(summary_lrti)

saveRDS(
  pooled_lrti,
  "outputs/pipelines/(GBD_RE)AA_pooled_lrti.rds"
)

saveRDS(
  summary_lrti,
  "outputs/pipelines/(GBD_RE)AA_summary_lrti.rds"
)

gc()
Sys.time()

# Fever models -----------------------------------------------------------------

fit_fever_list <- future_lapply(completed_data_Fever, function(dat) {
  glmmTMB::glmmTMB(
    Ab_treatment ~ ch_age_group + ch_sex + wh_z_score +
      water_collapsed + sanitation_collapsed + electricity + fuel_collapsed +
      health_insurance + rural_urban + hh_wealth + maternal_edu +
      Endemic + Year_c + log_GDP_z +
      (1 | gbd_region) +
      (1 | sub_region_nested_id),
    data = dat,
    family = binomial(link = "logit"),
    weights = sample_weight_scaled,
    REML = FALSE,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2e5, eval.max = 2e5)
    )
  )
}, future.seed = TRUE)

fit_fever_mi <- mice::as.mira(fit_fever_list)

# Save fitted models
saveRDS(fit_fever_mi, "outputs/pipelines/(GBD_RE)AA_fit_fever_mi.rds")

summary(fit_fever_list[[1]])

coef_names_fever <- names(glmmTMB::fixef(fit_fever_list[[1]])$cond)

stopifnot(all(vapply(fit_fever_list, function(fit_i) {
  identical(names(glmmTMB::fixef(fit_i)$cond), coef_names_fever)
}, logical(1))))

beta_fever <- do.call(cbind, lapply(fit_fever_list, function(fit_i) {
  glmmTMB::fixef(fit_i)$cond[coef_names_fever]
}))


# Model-based within-imputation variances
model_var_fever <- do.call(cbind, lapply(fit_fever_list, function(fit_i) {
  
  vcov_i <- as.matrix(stats::vcov(fit_i)$cond)
  
  if (!all(coef_names_fever %in% rownames(vcov_i))) {
    stop("Fever coefficient names do not match the covariance matrix.")
  }
  
  vcov_i <- vcov_i[coef_names_fever, coef_names_fever, drop = FALSE]
  vcov_i <- (vcov_i + t(vcov_i)) / 2
  
  if (any(!is.finite(vcov_i))) {
    stop("Non-finite fever covariance values.")
  }
  
  diag(vcov_i)
}))

gc()

# Rubin pooling using model-based within-imputation variances
m_fever <- ncol(beta_fever)
qbar_fever <- rowMeans(beta_fever)
ubar_fever <- rowMeans(model_var_fever)
b_fever <- apply(beta_fever, 1, var)

total_var_fever <- ubar_fever + (1 + 1 / m_fever) * b_fever
se_fever <- sqrt(total_var_fever)

r_fever <- (1 + 1 / m_fever) * b_fever / ubar_fever
df_fever <- (m_fever - 1) * (1 + 1 / r_fever)^2
df_fever[!is.finite(df_fever)] <- 999999

statistic_fever <- qbar_fever / se_fever
critical_fever <- qt(0.975, df = df_fever)

summary_fever <- tibble(
  term = coef_names_fever,
  estimate_log_odds = qbar_fever,
  std_error = se_fever,
  statistic = statistic_fever,
  df = df_fever,
  p.value = 2 * pt(abs(statistic_fever), df = df_fever, lower.tail = FALSE),
  estimate = exp(qbar_fever),
  conf.low = exp(qbar_fever - critical_fever * se_fever),
  conf.high = exp(qbar_fever + critical_fever * se_fever)
)

pooled_fever <- list(
  m = m_fever,
  qbar = qbar_fever,
  ubar = ubar_fever,
  between_imputation_variance = b_fever,
  total_variance = total_var_fever,
  df = df_fever
)

print(summary_fever)

saveRDS(
  pooled_fever,
  "outputs/pipelines/(GBD_RE)AA_pooled_fever.rds"
)

saveRDS(
  summary_fever,
  "outputs/pipelines/(GBD_RE)AA_summary_fever.rds"
)

gc()
Sys.time()

# Regression tables ------------------------------------------------------------

format_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

format_regression_table <- function(summary_dat, outcome_label) {
  summary_dat %>%
    mutate(
      outcome = outcome_label,
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ term
      ),
      term = str_replace_all(term, "_", " "),
      term = str_replace_all(term, "ch age group", "Child age: "),
      term = str_replace_all(term, "ch sex2", "Female child"),
      term = str_replace_all(term, "wh z score", "Weight-for-height z-score"),
      term = str_replace_all(term, "hh wealth", "Household wealth, quintile"),
      term = str_replace_all(term, "health insurance1", "Health insurance"),
      term = str_replace_all(term, "log GDP z", "Log GDP per capita, z-score"),
      term = str_replace_all(term, "Year c", "Survey year, centered"),
      term = str_replace_all(term, "Endemic1", "Malaria endemic"),
      term = str_replace_all(term, "rural urban2", "Rural residence"),
      term = str_replace_all(term, "electricityYes", "Electricity"),
      term = str_replace_all(term, "fuel collapsedSolid", "Solid cooking fuel"),
      term = str_replace_all(term, "water collapsedUnimproved water", "Unimproved water"),
      term = str_replace_all(term, "sanitation collapsedUnimproved sanitation", "Unimproved sanitation"),
      term = str_replace_all(term, "sanitation collapsedOpen defecation", "Open defecation"),
      term = str_replace_all(term, "maternal edu1", "Maternal education: primary"),
      term = str_replace_all(term, "maternal edu2", "Maternal education: secondary"),
      term = str_replace_all(term, "maternal edu3", "Maternal education: higher")
    ) %>%
    transmute(
      Outcome = outcome,
      Term = term,
      OR = sprintf("%.3f", estimate),
      `95% CI` = paste0(
        sprintf("%.3f", conf.low),
        "–",
        sprintf("%.3f", conf.high)
      ),
      `SE, log-odds scale` = sprintf("%.3f", std_error),
      `P value` = format_p(p.value)
    )
}

summary_lrti_pdf <- format_regression_table(summary_lrti, "LRTI")
summary_fever_pdf <- format_regression_table(summary_fever, "Fever")

figure_dir <- "figures/main_analysis/AA_model_diagnostics"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

pdf(
  file.path(
    figure_dir,
    "(GBD_RE)AA_regression_summary_lrti_fever.pdf"
  ),
  width = 13,
  height = 10
)

grid.newpage()

grid.text(
  "P(antibiotic treatment | LRTI)",
  x = 0.5,
  y = 0.97,
  gp = gpar(fontsize = 15, fontface = "bold")
)

grid.table(
  summary_lrti_pdf,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    core = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(fontface = "bold", cex = 0.85))
  ),
  vp = viewport(y = 0.48, height = 0.90)
)

grid.newpage()

grid.text(
  "P(antibiotic treatment | fever)",
  x = 0.5,
  y = 0.97,
  gp = gpar(fontsize = 15, fontface = "bold")
)

grid.table(
  summary_fever_pdf,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    core = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(fontface = "bold", cex = 0.85))
  ),
  vp = viewport(y = 0.48, height = 0.90)
)

dev.off()

# glmmTMB diagnostics -----------------------------------------------------------

lrti_convergence <- bind_rows(lapply(seq_along(fit_lrti_list), function(i) {
  fit_i <- fit_lrti_list[[i]]
  optimizer_message_i <- fit_i$fit$message
  
  tibble(
    imputation = i,
    optimizer_code = fit_i$fit$convergence,
    positive_definite_hessian = isTRUE(fit_i$sdr$pdHess),
    optimizer_message = if (is.null(optimizer_message_i)) {
      NA_character_
    } else {
      as.character(optimizer_message_i)
    }
  )
}))

fever_convergence <- bind_rows(lapply(seq_along(fit_fever_list), function(i) {
  fit_i <- fit_fever_list[[i]]
  optimizer_message_i <- fit_i$fit$message
  
  tibble(
    imputation = i,
    optimizer_code = fit_i$fit$convergence,
    positive_definite_hessian = isTRUE(fit_i$sdr$pdHess),
    optimizer_message = if (is.null(optimizer_message_i)) {
      NA_character_
    } else {
      as.character(optimizer_message_i)
    }
  )
}))

lrti_random_effect_sd <- bind_rows(lapply(seq_along(fit_lrti_list), function(i) {
  varcorr_i <- VarCorr(fit_lrti_list[[i]])$cond
  
  tibble(
    imputation = i,
    gbd_region_sd = attr(varcorr_i$gbd_region, "stddev")[1],
    sub_region_nested_id_sd = attr(
      varcorr_i$sub_region_nested_id,
      "stddev"
    )[1]
  )
}))

fever_random_effect_sd <- bind_rows(lapply(seq_along(fit_fever_list), function(i) {
  varcorr_i <- VarCorr(fit_fever_list[[i]])$cond
  
  tibble(
    imputation = i,
    gbd_region_sd = attr(varcorr_i$gbd_region, "stddev")[1],
    sub_region_nested_id_sd = attr(
      varcorr_i$sub_region_nested_id,
      "stddev"
    )[1]
  )
}))

print(lrti_convergence)
print(fever_convergence)
print(lrti_random_effect_sd)
print(fever_random_effect_sd)

saveRDS(
  lrti_convergence,
  "outputs/pipelines/(GBD_RE)AA_lrti_convergence.rds"
)

saveRDS(
  fever_convergence,
  "outputs/pipelines/(GBD_RE)AA_fever_convergence.rds"
)

saveRDS(
  lrti_random_effect_sd,
  "outputs/pipelines/(GBD_RE)AA_lrti_random_effect_sd.rds"
)

saveRDS(
  fever_random_effect_sd,
  "outputs/pipelines/(GBD_RE)AA_fever_random_effect_sd.rds"
)

plan(sequential)
gc()

Sys.time()
















# _____________________________________________ ----------
# 6. Prediction and Aggregation to country-year specific antibiotic treatment -----------------------
rm(list = ls()); gc()

# 0. Setup
library(dplyr)
library(tidyr)
library(mice)
library(glmmTMB)
library(purrr)
library(tibble)
library(openxlsx)
library(gridExtra)
library(grid)


LRTI_imp <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_LRTI.rds")
LRTI_fit <- readRDS("outputs/pipelines/(GBD_RE)AA_fit_lrti_mi.rds")
fever_imp <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_Fever.rds")
fever_fit <- readRDS("outputs/pipelines/(GBD_RE)AA_fit_fever_mi.rds")

completed_data_LRTI <- mice::complete(LRTI_imp, "all")
completed_data_fever <- mice::complete(fever_imp, "all")
fit_LRTI <- LRTI_fit$analyses
fit_fever <- fever_fit$analyses

stopifnot(length(completed_data_LRTI) == length(fit_LRTI))
stopifnot(length(completed_data_fever) == length(fit_fever))
stopifnot(length(completed_data_LRTI) == 5)
stopifnot(length(completed_data_fever) == 5)



## 1. LRTI marginal RE0 predictions --------------------------------------------

completed_data_LRTI_prepared <- purrr::imap(completed_data_LRTI, \(dat, i) {
  dat %>%
    mutate(
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE),
      sample_weight_scaled = sample_weight / mean(sample_weight, na.rm = TRUE),
      age_group = as.character(ch_age_group),
      Year = as.numeric(Year_c + 2010),
      .imp_dhs = i,
      outcome = "LRTI"
    )
})

completed_data_LRTI_pred <- purrr::map2(completed_data_LRTI_prepared, fit_LRTI, \(dat, fit) {
  dat$pred_proportion <- predict(fit, newdata = dat, type = "response", re.form = NA)
  dat
})


# keep DHS-imputation-specific country-year-age predictions
country_year_lrti_imp_age <- purrr::map_dfr(completed_data_LRTI_pred, \(dat) {
  dat %>%
    filter(!is.na(Ab_treatment), !is.na(sample_weight), sample_weight > 0, !is.na(pred_proportion)) %>%
    group_by(.imp_dhs, outcome, age_group, CountryYear, Country, Year, gbd_region) %>%
    summarise(
      n_child = n(),
      weight_sum = sum(sample_weight, na.rm = TRUE),
      observed_proportion = weighted.mean(Ab_treatment, w = sample_weight, na.rm = TRUE),
      pred_proportion = weighted.mean(pred_proportion, w = sample_weight, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(.imp_dhs, Country, Year, CountryYear, outcome, age_group, gbd_region)


# descriptive pooled estimate across five DHS imputations
country_year_lrti_marginal_age <- country_year_lrti_imp_age %>%
  group_by(outcome, age_group, CountryYear, Country, Year, gbd_region) %>%
  summarise(
    n_imputations = n_distinct(.imp_dhs),
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_proportion_mean = mean(observed_proportion, na.rm = TRUE),
    pred_proportion_mean = mean(pred_proportion, na.rm = TRUE),
    pred_proportion_between_imp_sd = sd(pred_proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(Country, Year, CountryYear, outcome, age_group, gbd_region)



## 2. Fever marginal RE0 predictions -------------------------------------------

completed_data_fever_prepared <- purrr::imap(completed_data_fever, \(dat, i) {
  dat %>%
    mutate(
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE),
      sample_weight_scaled = sample_weight / mean(sample_weight, na.rm = TRUE),
      age_group = as.character(ch_age_group),
      Year = as.numeric(Year_c + 2010),
      .imp_dhs = i,
      outcome = "fever"
    )
})

completed_data_fever_pred <- purrr::map2(completed_data_fever_prepared, fit_fever, \(dat, fit) {
  dat$pred_proportion <- predict(fit, newdata = dat, type = "response", re.form = NA)
  dat
})


# keep DHS-imputation-specific country-year-age predictions
country_year_fever_imp_age <- purrr::map_dfr(completed_data_fever_pred, \(dat) {
  dat %>%
    filter(!is.na(Ab_treatment), !is.na(sample_weight), sample_weight > 0, !is.na(pred_proportion)) %>%
    group_by(.imp_dhs, outcome, age_group, CountryYear, Country, Year, gbd_region) %>%
    summarise(
      n_child = n(),
      weight_sum = sum(sample_weight, na.rm = TRUE),
      observed_proportion = weighted.mean(Ab_treatment, w = sample_weight, na.rm = TRUE),
      pred_proportion = weighted.mean(pred_proportion, w = sample_weight, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(.imp_dhs, Country, Year, CountryYear, outcome, age_group, gbd_region)


# descriptive pooled estimate across five DHS imputations
country_year_fever_marginal_age <- country_year_fever_imp_age %>%
  group_by(outcome, age_group, CountryYear, Country, Year, gbd_region) %>%
  summarise(
    n_imputations = n_distinct(.imp_dhs),
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_proportion_mean = mean(observed_proportion, na.rm = TRUE),
    pred_proportion_mean = mean(pred_proportion, na.rm = TRUE),
    pred_proportion_between_imp_sd = sd(pred_proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(Country, Year, CountryYear, outcome, age_group, gbd_region)



## 2-1. Basic checks ------------------------------------------------------------

stopifnot(
  n_distinct(country_year_lrti_imp_age$.imp_dhs) == 5,
  n_distinct(country_year_fever_imp_age$.imp_dhs) == 5
)

print(country_year_lrti_imp_age %>% count(.imp_dhs))
print(country_year_fever_imp_age %>% count(.imp_dhs))

plot(country_year_lrti_marginal_age$observed_proportion_mean, country_year_lrti_marginal_age$pred_proportion_mean, xlim = c(0,1), ylim = c(0,1))
plot(country_year_fever_marginal_age$observed_proportion_mean, country_year_fever_marginal_age$pred_proportion_mean, xlim = c(0,1), ylim = c(0,1))


# ex) We compared marginal predictions, in which GBD-region and subregion random intercepts were set to zero, with conditional predictions including estimated geographic random intercepts. The difference between these predictions quantified the extent to which absolute incidence estimates were driven by geographic deviations rather than the common fixed-effect component of the model.

# ex) Conditional predictions were [similar to / higher than] marginal predictions, suggesting that the elevated absolute incidence estimates were [not primarily attributable to / partly driven by] region- and subregion-specific random intercepts.





## 3. Add GDP and population ---------------------------------------------------

GDP.PPP <- read.csv("data/metadata/GDP_PPP.csv") # Yemen and Venezuela from IMF data (GDP per capita international dollar)
colnames(GDP.PPP)[-1] <- gsub("^X", "", colnames(GDP.PPP)[-1])
GDP.PPP <- GDP.PPP %>% pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP")
GDP.PPP$Year <- as.numeric(GDP.PPP$Year)

Population <- read.csv("data/metadata/Under_5.csv")
Population <- Population %>% dplyr::select(c(Entity, Year, Population))
colnames(Population) <- c("Country", "Year", "Population")

head(GDP.PPP)
head(Population)

a <- unique(GDP.PPP$Country)
b <- unique(Population$Country)
c <- unique(country_year_lrti_marginal_age$Country)

setdiff(c,a)
setdiff(c,b)

GDP.PPP <- GDP.PPP %>%
  mutate(Country = dplyr::recode(
    Country,
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Congo, Rep." = "Republic of the Congo",
    "Gambia, The" = "Gambia",
    "Egypt" = "Egypt, Arab Rep.",
    "Yemen, Rep." = "Yemen"
  ))

Population <- Population %>%
  mutate(Country = dplyr::recode(
    Country,
    "Democratic Republic of Congo" = "Democratic Republic of the Congo",
    "Congo" = "Republic of the Congo",
    "Egypt" = "Egypt, Arab Rep.",
    "Yemen, Rep." = "Yemen"
  ))

manual_country_year_values <- tibble::tribble(
  ~CountryYear,      ~Population_manual, ~GDP_manual,
  "Haiti_2012",       1243438,             3286,
  "Maldives_2009",      30631,            16163,
  "Maldives_2016",      36815,            19264,
  "Maldives_2017",      36199,            19948,
  "Yemen_2013",       4616819,             3665
)

add_gdp_population <- function(country_year_table) {
  
  country_year_table %>%
    mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
    left_join(
      Population %>%
        mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
        dplyr::select(Country, Year, Population),
      by = c("Country", "Year")
    ) %>%
    left_join(
      GDP.PPP %>%
        mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
        dplyr::select(Country, Year, GDP),
      by = c("Country", "Year")
    ) %>%
    left_join(manual_country_year_values, by = "CountryYear") %>%
    mutate(
      Population = if_else(!is.na(Population_manual), Population_manual, Population),
      GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)
    ) %>%
    dplyr::select(-Population_manual, -GDP_manual)
}


# DHS-imputation-specific tables used in Section 7
country_year_lrti_marginal_age_imp_gdp <- add_gdp_population(country_year_lrti_imp_age)
country_year_fever_marginal_age_imp_gdp <- add_gdp_population(country_year_fever_imp_age)

# descriptive pooled tables used for plots and summaries
country_year_lrti_marginal_age_gdp <- add_gdp_population(country_year_lrti_marginal_age)
country_year_fever_marginal_age_gdp <- add_gdp_population(country_year_fever_marginal_age)


a <- country_year_lrti_marginal_age_imp_gdp[is.na(country_year_lrti_marginal_age_imp_gdp$GDP),]
b <- country_year_fever_marginal_age_imp_gdp[is.na(country_year_fever_marginal_age_imp_gdp$Population),]

a[,c("CountryYear")]
b[,c("CountryYear")]


# Income group is descriptive only and does not depend on PC imputation
UNI_income <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>%
  dplyr::select(Country, Income) %>%
  distinct()

UNI_income <- UNI_income %>%
  bind_rows(
    tibble(
      Country = c("Azerbaijan", "Gabon", "Guyana", "Haiti", "Maldives"),
      Income = c("UM", "UM", "UM", "LM", "UM")
    )
  ) %>%
  distinct(Country, .keep_all = TRUE)

stopifnot(!anyDuplicated(UNI_income$Country))

setdiff(unique(country_year_lrti_marginal_age_gdp$Country), unique(UNI_income$Country))


# add Income to both primary MI-specific and descriptive pooled tables
country_year_lrti_marginal_age_imp_gdp <- country_year_lrti_marginal_age_imp_gdp %>% inner_join(UNI_income, by = "Country")
country_year_fever_marginal_age_imp_gdp <- country_year_fever_marginal_age_imp_gdp %>% inner_join(UNI_income, by = "Country")

country_year_lrti_marginal_age_gdp <- country_year_lrti_marginal_age_gdp %>% inner_join(UNI_income, by = "Country")
country_year_fever_marginal_age_gdp <- country_year_fever_marginal_age_gdp %>% inner_join(UNI_income, by = "Country")


# final MI checks
stopifnot(
  n_distinct(country_year_lrti_marginal_age_imp_gdp$.imp_dhs) == 5,
  n_distinct(country_year_fever_marginal_age_imp_gdp$.imp_dhs) == 5
)

print(country_year_lrti_marginal_age_imp_gdp %>% count(.imp_dhs))
print(country_year_fever_marginal_age_imp_gdp %>% count(.imp_dhs))



## 3-1. Region-level population-weighted summary -------------------------------

# Region-level summaries remain descriptive and therefore use the pooled
# five-DHS-imputation country-year estimates.
summarise_region <- function(country_year_table) {
  
  group_vars <- c("outcome", "gbd_region", "Year")
  
  if("age_group" %in% names(country_year_table)){
    group_vars <- c("outcome", "age_group", "gbd_region", "Year")
  }
  
  country_year_table %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      pred_proportion_mean = weighted.mean(pred_proportion_mean, Population, na.rm = TRUE),
      observed_proportion_mean = weighted.mean(observed_proportion_mean, Population, na.rm = TRUE),
      GDP_pw_mean = weighted.mean(GDP, Population, na.rm = TRUE),
      n_country_year = n(),
      .groups = "drop"
    )
}

region_year_lrti_marginal_age_gdp <- summarise_region(country_year_lrti_marginal_age_gdp)
region_year_fever_marginal_age_gdp <- summarise_region(country_year_fever_marginal_age_gdp)







## 4. Save prediction and aggregation outputs ---------------------------------

out_dir <- "outputs/pipelines"
save_dir <- file.path(out_dir, "AA_marginal_RE0_prediction_outputs")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)


## 4-1. Save individual RDS files ---------------------------------------------

objects_to_save <- list(
  
  # Primary DHS-imputation-specific country-year marginal RE0
  country_year_lrti_marginal_age_imp_gdp = country_year_lrti_marginal_age_imp_gdp,
  country_year_fever_marginal_age_imp_gdp = country_year_fever_marginal_age_imp_gdp,
  
  # Descriptive pooled country-year marginal RE0
  country_year_lrti_marginal_age = country_year_lrti_marginal_age,
  country_year_fever_marginal_age = country_year_fever_marginal_age,
  
  # Descriptive pooled GDP/population-added marginal RE0
  country_year_lrti_marginal_age_gdp = country_year_lrti_marginal_age_gdp,
  country_year_fever_marginal_age_gdp = country_year_fever_marginal_age_gdp,
  
  # Descriptive region-year population-weighted marginal RE0
  region_year_lrti_marginal_age_gdp = region_year_lrti_marginal_age_gdp,
  region_year_fever_marginal_age_gdp = region_year_fever_marginal_age_gdp
)

purrr::iwalk(
  objects_to_save,
  ~ saveRDS(.x, file.path(save_dir, paste0(.y, ".rds")))
)


# Also save explicit AA-prefixed primary files for Section 7
saveRDS(
  country_year_lrti_marginal_age_imp_gdp,
  file.path(save_dir, "AA_country_year_lrti_marginal_age_imp_gdp.rds")
)

saveRDS(
  country_year_fever_marginal_age_imp_gdp,
  file.path(save_dir, "AA_country_year_fever_marginal_age_imp_gdp.rds")
)



## 4-2. Save combined RDS files -----------------------------------------------

# Primary MI-specific combined table for Section 7
country_year_marginal_imp_gdp_all <- bind_rows(
  country_year_lrti_marginal_age_imp_gdp,
  country_year_fever_marginal_age_imp_gdp
)

# Descriptive pooled tables
country_year_all <- bind_rows(
  country_year_lrti_marginal_age,
  country_year_fever_marginal_age
)

country_year_marginal_gdp_all <- bind_rows(
  country_year_lrti_marginal_age_gdp,
  country_year_fever_marginal_age_gdp
)

region_year_marginal_gdp_all <- bind_rows(
  region_year_lrti_marginal_age_gdp,
  region_year_fever_marginal_age_gdp
)


saveRDS(
  country_year_marginal_imp_gdp_all,
  file.path(save_dir, "AA_country_year_marginal_RE0_imp_gdp_all.rds")
)

saveRDS(
  country_year_all,
  file.path(save_dir, "AA_country_year_all_marginal_conditional.rds")
)

saveRDS(
  country_year_marginal_gdp_all,
  file.path(save_dir, "AA_country_year_marginal_RE0_gdp_all.rds")
)

saveRDS(
  region_year_marginal_gdp_all,
  file.path(save_dir, "AA_region_year_marginal_RE0_gdp_all.rds")
)



## 4-3. Prediction checks ------------------------------------------------------

lrti_pred_range <- range(
  unlist(lapply(completed_data_LRTI_pred, \(dat) dat$pred_proportion)),
  na.rm = TRUE
)

fever_pred_range <- range(
  unlist(lapply(completed_data_fever_pred, \(dat) dat$pred_proportion)),
  na.rm = TRUE
)

lrti_pred_i <- unlist(lapply(completed_data_LRTI_pred, \(dat) dat$pred_proportion))
fever_pred_i <- unlist(lapply(completed_data_fever_pred, \(dat) dat$pred_proportion))

lrti_n_outside_01 <- sum(
  lrti_pred_i < 0 | lrti_pred_i > 1,
  na.rm = TRUE
)

fever_n_outside_01 <- sum(
  fever_pred_i < 0 | fever_pred_i > 1,
  na.rm = TRUE
)

lrti_pred_range
fever_pred_range
lrti_n_outside_01
fever_n_outside_01

stopifnot(
  lrti_n_outside_01 == 0,
  fever_n_outside_01 == 0
)

## 4-4. Save CSV files for easy inspection ------------------------------------

write.csv(
  country_year_marginal_imp_gdp_all,
  file.path(save_dir, "AA_country_year_marginal_RE0_imp_gdp_all.csv"),
  row.names = FALSE
)

write.csv(
  country_year_marginal_gdp_all,
  file.path(save_dir, "AA_country_year_marginal_RE0_gdp_all.csv"),
  row.names = FALSE
)

write.csv(
  region_year_marginal_gdp_all,
  file.path(save_dir, "AA_region_year_marginal_RE0_gdp_all.csv"),
  row.names = FALSE
)


cat("\nSection 6 completed.\n")
cat("Primary Section 7 input: AA_country_year_marginal_RE0_imp_gdp_all.rds\n")
cat("DHS imputations retained:", paste(sort(unique(country_year_marginal_imp_gdp_all$.imp_dhs)), collapse = ", "), "\n")


## 5. Plots ---------------------------------------------------------------------

fig_dir_diag <- "figures/main_analysis/AA_model_diagnostics"
dir.create(fig_dir_diag, recursive = TRUE, showWarnings = FALSE)


## 5-0. Colors and graphical parameters -----------------------------------------

income_cols_fixed <- c(
  "L" = "#CC79A7",
  "LM" = "#009E73",
  "UM" = "#0072B2"
)

income_labels <- c(
  "L" = "Low income",
  "LM" = "Lower-middle income",
  "UM" = "Upper-middle income"
)

region_cols_fixed <- c(
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "Latin America and Caribbean" = "#E69F00",
  "North Africa and Middle East" = "#009E73",
  "South Asia" = "#A7B97F",
  "Sub-Saharan Africa" = "#0072B2"
)

age_levels <- c("0-11 months", "12-23 months", "24-59 months")

# Graphical parameters for a 6 × 3.5 inch PDF
cex_figure_title <- 0.70
cex_panel_title <- 0.54
cex_axis_text <- 0.50
cex_axis_title <- 0.48

cex_income_legend <- 0.60
cex_region_legend <- 0.44
cex_legend_point <- 0.6

cex_country_point <- 0.55
cex_region_point <- 0.55
cex_year_label <- 0.33

lwd_reference <- 1.10
lwd_regression <- 1.20


## 5-1. Prepare combined plotting data ------------------------------------------

standardize_region_name <- function(x) {
  dplyr::recode(
    as.character(x),
    "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
    .default = as.character(x)
  )
}

get_regions_in_fixed_order <- function(dat, region_col = "gbd_region") {
  regions_present <- unique(standardize_region_name(dat[[region_col]]))
  regions_present <- regions_present[!is.na(regions_present)]
  
  c(
    names(region_cols_fixed)[names(region_cols_fixed) %in% regions_present],
    sort(setdiff(regions_present, names(region_cols_fixed)))
  )
}

get_region_cols <- function(regions) {
  regions <- standardize_region_name(regions)
  cols <- region_cols_fixed[regions]
  cols[is.na(cols)] <- "#808080"
  cols
}

country_year_marginal_age <- bind_rows(
  country_year_lrti_marginal_age_gdp %>% mutate(outcome_label = "LRTI"),
  country_year_fever_marginal_age_gdp %>% mutate(outcome_label = "fever")
) %>%
  mutate(
    gbd_region = standardize_region_name(gbd_region),
    Income = as.character(Income),
    log_GDP = log10(GDP),
    age_group = factor(age_group, levels = age_levels)
  )

region_year_marginal_age <- bind_rows(
  region_year_lrti_marginal_age_gdp %>% mutate(outcome_label = "LRTI"),
  region_year_fever_marginal_age_gdp %>% mutate(outcome_label = "fever")
) %>%
  mutate(
    gbd_region = standardize_region_name(gbd_region),
    log_GDP_pw = log10(GDP_pw_mean),
    age_group = factor(age_group, levels = age_levels)
  )


## 5-2. Layout and legend functions ---------------------------------------------

setup_2x3_with_legend <- function() {
  layout(
    matrix(
      c(
        1 ,  2,  3,  13,
        4 ,  5,  6,  13,
        0 ,  0,  0,  13,
        7 ,  8,  9,  13,
        10, 11, 12,  13
      ),
      nrow = 5,
      byrow = TRUE
    ),
    widths = c(1, 1, 1, 1.2),
    heights = c(1, 1, 0.15, 1, 1)
  )
}

draw_income_legend_panel <- function() {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  
  legend(
    "left",
    legend = unname(income_labels[names(income_cols_fixed)]),
    col = unname(income_cols_fixed),
    pch = 16,
    lty = 1,
    lwd = 0,
    pt.cex = cex_legend_point,
    cex = cex_income_legend,
    title = "",
    title.font = 2,
    x.intersp = 0.65,
    y.intersp = 0.85,
    bty = "n"
  )
}


draw_region_legend_panel <- function(regions) {
  region_cols <- get_region_cols(regions)
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  
  legend(
    "center",
    legend = regions,
    col = region_cols[regions],
    pch = 16,
    lty = 1,
    lwd = 0,
    bty = "n",
    cex = 0.56
  )
}



## 5-3. Custom axis function -----------------------------------------------------

draw_custom_panel <- function(
    xlim,
    ylim,
    xlab,
    ylab,
    main = "",
    x_at = NULL,
    y_at = NULL,
    x_digits = 0,
    y_digits = 2,
    y_nsmall = 2,
    show_xlab = TRUE,
    show_ylab = TRUE
) {
  
  xmin <- xlim[1]
  xmax <- xlim[2]
  ymin <- ylim[1]
  ymax <- ylim[2]
  
  if (is.null(x_at)) {
    x_at <- pretty(xlim, n = 5)
    x_at <- x_at[x_at >= xmin & x_at <= xmax]
  }
  
  if (is.null(y_at)) {
    y_at <- seq(ymin, ymax, length.out = 5)
  }
  
  plot(
    NA,
    xlab = "",
    ylab = "",
    pch = 16,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    lwd = 1,
    axes = FALSE,
    ann = FALSE,
    main = "",
    xaxs = "i",
    yaxs = "i"
  )
  
  axis(1, at = x_at, labels = FALSE, las = 2, pos = ymin, tck = -0.025)
  
  text(
    x = x_at,
    y = ymin - (ymax - ymin) * 0.07,
    labels = format(round(x_at, digits = x_digits), nsmall = 0),
    xpd = TRUE,
    cex = 0.6
  )
  
  axis(2, at = y_at, labels = FALSE, las = 2, pos = xmin, tck = -0.025)
  
  text(
    y = y_at,
    x = xmin - 0.05 * (xmax - xmin),
    labels = format(round(y_at, digits = y_digits), nsmall = y_nsmall),
    srt = 0,
    adj = 1,
    xpd = TRUE,
    cex = 0.6
  )
  
  if (show_ylab) {
    mtext(ylab, side = 2, line = 1.5, cex = 0.52)
  }
  
  if (show_xlab) {
    mtext(xlab, side = 1, line = 1.5, cex = 0.52)
  }
  
  mtext(main, side = 3, line = -0.5, adj = 0.5, cex = 0.52, font = 2)
}


## 5-4. Observed-versus-predicted plot using income colors ----------------------

plot_pred_vs_obs_custom <- function(dat, outcome_lab, main = "",
                                    show_xlab = TRUE, show_ylab = TRUE) {
  plot_data <- dat %>%
    filter(
      outcome_label == outcome_lab,
      is.finite(observed_proportion_mean),
      is.finite(pred_proportion_mean),
      Income %in% names(income_cols_fixed)
    )
  
  point_cols <- unname(income_cols_fixed[plot_data$Income])
  point_cols[is.na(point_cols)] <- "grey50"
  
  lim <- range(
    c(plot_data$observed_proportion_mean, plot_data$pred_proportion_mean),
    na.rm = TRUE
  )
  
  lim <- c(0, max(lim) * 1.05)
  
  draw_custom_panel(
    xlim = lim,
    ylim = lim,
    xlab = paste0("Observed proportion"),
    ylab = paste0("Predicted proportion"),
    main = main,
    x_at = seq(lim[1], lim[2], length.out = 5),
    y_at = seq(lim[1], lim[2], length.out = 5),
    x_digits = 1,
    y_digits = 2,
    y_nsmall = 2,
    show_xlab = show_xlab,
    show_ylab = show_ylab
  )

  points(
    plot_data$observed_proportion_mean,
    plot_data$pred_proportion_mean,
    pch = 16,
    cex = cex_country_point,
    col = point_cols
  )
}







## 5-5. Country-year GDP plot using income colors -------------------------------

plot_country_gdp_custom <- function(dat, outcome_lab, x_scale = c("log", "normal"),
                                    main = "", y_lim = NULL,
                                    show_xlab = TRUE, show_ylab = TRUE) {
  x_scale <- match.arg(x_scale)
  
  plot_data <- dat %>%
    filter(
      outcome_label == outcome_lab,
      is.finite(GDP),
      GDP > 0,
      is.finite(pred_proportion_mean),
      is.finite(Population),
      Population > 0,
      Income %in% names(income_cols_fixed)
    ) %>%
    mutate(log_GDP = log10(GDP))
  
  point_cols <- unname(income_cols_fixed[plot_data$Income])
  point_cols[is.na(point_cols)] <- "grey50"
  
  if (x_scale == "log") {
    x <- plot_data$log_GDP
    xlab <- expression(Log[10]~"GDP per capita")
    x_at <- pretty(range(x, na.rm = TRUE), n = 5)
    x_at <- x_at[is.finite(x_at)]
    xlim <- range(x_at, na.rm = TRUE)
    x_digits <- 1
  } else {
    x <- plot_data$GDP
    xlab <- expression(Log[10]~"GDP per capita")
    x_at <- pretty(c(0, range(x, na.rm = TRUE)), n = 5)
    x_at <- x_at[is.finite(x_at)]
    xlim <- range(x_at, na.rm = TRUE)
    x_digits <- 0
  }
  
  y <- plot_data$pred_proportion_mean
  
  if (is.null(y_lim)) {
    y_at <- pretty(c(0, y), n = 5)
    y_at <- y_at[is.finite(y_at)]
    ylim <- range(y_at, na.rm = TRUE)
  } else {
    ylim <- y_lim
    y_at <- seq(ylim[1], ylim[2], length.out = 5)
  }
  
  draw_custom_panel(
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = paste0("Predicted proportion"),
    main = main,
    x_at = x_at,
    y_at = y_at,
    x_digits = x_digits,
    y_digits = 2,
    y_nsmall = 2,
    show_xlab = show_xlab,
    show_ylab = show_ylab
  )
  
  points(
    x,
    y,
    pch = 16,
    cex = cex_country_point,
    col = point_cols
  )
}




## 5-7. Predicted versus observed: income colors --------------------------------

pdf(file.path(fig_dir_diag, "AA_predicted_vs_observed_by_gdp_age_marginal_RE0_custom_axis.pdf"),width = 6,height = 6)

old_par <- par(no.readonly = TRUE)
setup_2x3_with_legend()

par(mar = c(2.5, 2.5, 0.9, 0.7), mgp = c(2, 0.35, 0), lwd = 0.8)

for (jj in seq_along(age_levels)) {
  plot_pred_vs_obs_custom(
    country_year_marginal_age %>% filter(age_group == age_levels[jj]),
    "LRTI",
    main = paste0("LRTI: ", age_levels[jj]),
    show_xlab = FALSE,
    show_ylab = (jj == 1)
  )
}

for (jj in seq_along(age_levels)) {
  plot_pred_vs_obs_custom(
    country_year_marginal_age %>% filter(age_group == age_levels[jj]),
    "fever",
    main = paste0("Fever: ", age_levels[jj]),
    show_xlab = TRUE,
    show_ylab = (jj == 1)
  )
}

par(mar = c(2.5, 2.5, 0.9, 0.7), mgp = c(2, 0.35, 0), lwd = 0.8)

for (jj in seq_along(age_levels)) {
  plot_country_gdp_custom(
    country_year_marginal_age %>% filter(age_group == age_levels[jj]),
    "LRTI",
    x_scale = "log",
    main = paste0("LRTI: ", age_levels[jj]),
    show_xlab = FALSE,
    y_lim = c(0,1),
    show_ylab = (jj == 1)
  )
}

for (jj in seq_along(age_levels)) {
  plot_country_gdp_custom(
    country_year_marginal_age %>% filter(age_group == age_levels[jj]),
    "fever",
    x_scale = "log",
    main = paste0("Fever: ", age_levels[jj]),
    y_lim = c(0,0.6),
    show_xlab = TRUE,
    show_ylab = (jj == 1)
  )
}

draw_income_legend_panel()

par(old_par)
dev.off()



## 5-9. Export model summary tables --------------------------------------------

summary_lrti <- readRDS("outputs/pipelines/(GBD_RE)AA_summary_lrti.rds")
summary_fever <- readRDS("outputs/pipelines/(GBD_RE)AA_summary_fever.rds")

write.csv(
  summary_lrti,
  "outputs/pipelines/(GBD_RE)AA_summary_lrti.csv",
  row.names = FALSE
)

write.csv(
  summary_fever,
  "outputs/pipelines/(GBD_RE)AA_summary_fever.csv",
  row.names = FALSE
)




















# _____________________________________________ ----------
# 7. Full model comparison for marginal RE0: age-specific ---------------------

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(mgcv)
  library(ranger)
  library(glmnet)
  library(xgboost)
})

set.seed(123)
options(scipen = 999)


## 0. Paths and runtime options -----------------------------------------------

out_dir <- "outputs/pipelines"
prediction_output_dir <- file.path(out_dir, "AA_marginal_RE0_prediction_outputs")
model_search_dir <- file.path(out_dir, "AA_marginal_RE0_full_model_search_outputs")
dir.create(model_search_dir, recursive = TRUE, showWarnings = FALSE)

# 40 common country holdout splits x 25 PC-DHS MI states = 1000 validation evaluations
n_splits_per_combo <- 40L
holdout_prop <- 0.10


# DHS-imputation-specific estimates from revised Section 6
country_year_marginal_age_imp_gdp <- readRDS(
  file.path(prediction_output_dir, "AA_country_year_marginal_RE0_imp_gdp_all.rds")
)

# Five PC imputations
UNI <- readRDS(file.path(out_dir, "pc_gdp_2023_incidence.rds"))

stopifnot(".imp_dhs" %in% names(country_year_marginal_age_imp_gdp))
stopifnot(".imp_pc" %in% names(UNI))



## 1. Country-name map to UNI standard ----------------------------------------

country_name_map <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Democratic Republic of Congo" = "Congo, Dem. Rep.",
  "Congo [DRC]" = "Congo, Dem. Rep.",
  "Congo \\[DRC\\]" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "Republic of the Congo" = "Congo, Rep.",
  "Congo [Republic]" = "Congo, Rep.",
  "Congo \\[Republic\\]" = "Congo, Rep.",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "C\u0099te d'Ivoire" = "Cote d'Ivoire",
  "CÂ\u0099te d'Ivoire" = "Cote d'Ivoire",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Swaziland" = "Eswatini",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Iran" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Laos" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "Macedonia [FYROM]" = "North Macedonia",
  "Macedonia \\[FYROM\\]" = "North Macedonia",
  "Russia" = "Russian Federation",
  "United Republic of Tanzania" = "Tanzania",
  "T\x9frkiye" = "Turkiye",
  "T\u009frkiye" = "Turkiye",
  "Türkiye" = "Turkiye",
  "Turkey" = "Turkiye",
  "Vietnam" = "Viet Nam",
  "Yemen" = "Yemen, Rep.",
  "Myanmar [Burma]" = "Myanmar",
  "Myanmar \\[Burma\\]" = "Myanmar"
)


country_year_marginal_age_imp_gdp <- country_year_marginal_age_imp_gdp %>%
  mutate(
    .imp_dhs = as.integer(.imp_dhs),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    outcome = as.character(outcome),
    age_group = as.character(age_group)
  )


## 2. Prepare PC-imputation-specific UNI data ---------------------------------

UNI_model <- UNI %>%
  mutate(
    .imp_pc = as.integer(.imp_pc),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2),
    Endemic = case_when(
      as.character(Endemic) %in% c("1", "TRUE", "True", "true", "endemic", "Endemic") ~ 1,
      as.character(Endemic) %in% c("0", "FALSE", "False", "false", "non-endemic", "Non-endemic") ~ 0,
      TRUE ~ suppressWarnings(as.numeric(as.character(Endemic)))
    ),
    gbd_region = dplyr::recode(
      as.character(gbd_region),
      "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
      .default = as.character(gbd_region)
    )
  ) %>%
  filter(
    is.finite(Year),
    is.finite(PC1),
    is.finite(PC2),
    !is.na(Endemic),
    !is.na(gbd_region)
  )

pc_imp_values <- sort(unique(UNI_model$.imp_pc))
dhs_imp_values <- sort(unique(country_year_marginal_age_imp_gdp$.imp_dhs))

stopifnot(length(pc_imp_values) == 5)
stopifnot(length(dhs_imp_values) == 5)

for(pc_imp_i in pc_imp_values){
  UNI_check_i <- UNI_model %>% filter(.imp_pc == pc_imp_i)
  if(anyDuplicated(UNI_check_i[c("Country", "Year")])) stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
}


combo_grid <- tidyr::crossing(
  .imp_pc = pc_imp_values,
  .imp_dhs = dhs_imp_values
) %>%
  mutate(combo_id = row_number())

n_combinations <- nrow(combo_grid)
n_validation_expected <- n_combinations * n_splits_per_combo

cat(
  "\nPC imputations:", length(pc_imp_values),
  "\nDHS imputations:", length(dhs_imp_values),
  "\nJoint PC x DHS combinations:", n_combinations,
  "\nHoldout splits per combination:", n_splits_per_combo,
  "\nValidation replicates per candidate model:", n_validation_expected,
  "\n"
)

stopifnot(n_combinations == 25)
stopifnot(n_validation_expected == 1000)



## 3. Specification ------------------------------------------------------------

outcome_list <- c("LRTI", "fever")
age_group_list <- c("0-11 months", "12-23 months", "24-59 months")

spec_name_list <- c(
  "LRTI_0_11_months",
  "LRTI_12_23_months",
  "LRTI_24_59_months",
  "fever_0_11_months",
  "fever_12_23_months",
  "fever_24_59_months"
)



## 4. Model specifications -----------------------------------------------------

base_formula_i <- logit_value ~ PC1 + PC2 + Endemic

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

if(!requireNamespace("kernlab", quietly = TRUE)){
  model_labels_i <- model_labels_i[names(model_labels_i) != "svm_radial"]
}

candidate_model_keys <- names(model_labels_i)



## 5. Validation across 5 PC x 5 DHS x 40 holdout splits ----------------------

validation_result_list <- vector("list", length(spec_name_list))
validation_prediction_list <- vector("list", length(spec_name_list))
validation_design_list <- vector("list", length(spec_name_list))

names(validation_result_list) <- spec_name_list
names(validation_prediction_list) <- spec_name_list
names(validation_design_list) <- spec_name_list


for(spec_index_i in seq_along(spec_name_list)){
  
  spec_name_i <- spec_name_list[spec_index_i]
  outcome_i <- outcome_list[ceiling(spec_index_i / 3)]
  age_group_i <- age_group_list[((spec_index_i - 1) %% 3) + 1]
  
  message("\nPreparing validation: ", spec_name_i)
  
  
  # MI-specific first-stage antibiotic-treatment estimates
  outcome_data_i <- country_year_marginal_age_imp_gdp %>%
    filter(outcome == outcome_i, age_group == age_group_i) %>%
    dplyr::select(
      -any_of(c(
        "PC1", "PC2", "Endemic",
        "logit_value", "proportion_bounded",
        "Population_under5"
      ))
    )
  
  if(nrow(outcome_data_i) == 0) stop("No data for ", spec_name_i)
  
  
  # Create 25 PC x DHS training datasets
  combo_train_list_i <- vector("list", n_combinations)
  
  
  for(combo_index_i in seq_len(n_combinations)){
    
    pc_imp_i <- combo_grid$.imp_pc[combo_index_i]
    dhs_imp_i <- combo_grid$.imp_dhs[combo_index_i]
    
    base_dat_i <- outcome_data_i %>%
      filter(.imp_dhs == dhs_imp_i)
    
    pc_dat_i <- UNI_model %>%
      filter(.imp_pc == pc_imp_i) %>%
      dplyr::select(
        Country, Year,
        PC1, PC2, Endemic,
        gbd_region_from_uni = gbd_region
      )
    
    train_dat_i <- base_dat_i %>%
      left_join(pc_dat_i, by = c("Country", "Year")) %>%
      mutate(
        .imp_pc = pc_imp_i,
        .imp_dhs = dhs_imp_i,
        gbd_region = coalesce(as.character(gbd_region), as.character(gbd_region_from_uni)),
        gbd_region = dplyr::recode(
          gbd_region,
          "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
          .default = gbd_region
        )
      ) %>%
      dplyr::select(-gbd_region_from_uni) %>%
      filter(
        is.finite(pred_proportion),
        pred_proportion >= 0,
        pred_proportion <= 1,
        is.finite(PC1),
        is.finite(PC2),
        !is.na(Endemic),
        !is.na(gbd_region)
      ) %>%
      mutate(
        proportion_bounded = pmin(pmax(pred_proportion, 1e-6), 1 - 1e-6),
        logit_value = qlogis(proportion_bounded),
        Endemic = as.numeric(Endemic),
        gbd_region = factor(gbd_region)
      )
    
    
    # Model weights based on number of DHS children in each country-year
    weight_raw_i <- as.numeric(train_dat_i$n_child)
    weight_valid_i <- is.finite(weight_raw_i) & weight_raw_i > 0
    
    if(any(weight_valid_i)){
      weight_raw_i[!weight_valid_i] <- median(weight_raw_i[weight_valid_i], na.rm = TRUE)
      weight_cap_i <- as.numeric(quantile(weight_raw_i, 0.95, na.rm = TRUE))
      train_dat_i$weight_model <- pmin(weight_raw_i, weight_cap_i)
    } else {
      train_dat_i$weight_model <- 1
    }
    
    train_dat_i <- train_dat_i %>%
      filter(
        is.finite(logit_value),
        is.finite(PC1),
        is.finite(PC2),
        is.finite(Endemic),
        is.finite(weight_model),
        weight_model > 0
      )
    
    if(nrow(train_dat_i) < 10){
      stop("Too few training rows for ", spec_name_i, " | PC ", pc_imp_i, " | DHS ", dhs_imp_i)
    }
    
    combo_train_list_i[[combo_index_i]] <- train_dat_i
  }
  
  
  # Use same country universe across all 25 MI combinations
  country_sets_i <- lapply(combo_train_list_i, function(x) unique(x$Country))
  common_countries_i <- Reduce(intersect, country_sets_i)
  
  if(length(common_countries_i) == 0){
    stop("No common countries across all PC x DHS combinations for ", spec_name_i)
  }
  
  combo_train_list_i <- lapply(
    combo_train_list_i,
    function(x) x %>% filter(Country %in% common_countries_i)
  )
  
  
  # Create the same 40 country holdout sets for all 25 MI combinations
  reference_train_i <- combo_train_list_i[[1]]
  countries_i <- sort(unique(reference_train_i$Country))
  endemic_levels_i <- sort(unique(as.character(reference_train_i$Endemic)))
  n_holdout_i <- max(1, ceiling(length(countries_i) * holdout_prop))
  
  holdout_list_i <- vector("list", n_splits_per_combo)
  
  
  for(split_id_i in seq_len(n_splits_per_combo)){
    
    set.seed(spec_index_i * 100000 + split_id_i)
    holdout_countries_i <- character(0)
    
    for(attempt_i in seq_len(1000)){
      
      holdout_try_i <- sample(countries_i, n_holdout_i)
      
      endemic_train_try_i <- reference_train_i %>%
        filter(!Country %in% holdout_try_i) %>%
        pull(Endemic) %>%
        as.character() %>%
        unique() %>%
        sort()
      
      if(setequal(endemic_levels_i, endemic_train_try_i)){
        holdout_countries_i <- holdout_try_i
        break
      }
    }
    
    if(length(holdout_countries_i) == 0){
      stop("Could not create valid holdout for ", spec_name_i, " | split ", split_id_i)
    }
    
    holdout_list_i[[split_id_i]] <- holdout_countries_i
  }
  
  
  validation_design_list[[spec_index_i]] <- bind_rows(
    lapply(seq_len(n_splits_per_combo), function(split_id_i){
      tibble(
        outcome = outcome_i,
        age_group = age_group_i,
        spec_name = spec_name_i,
        split_id = split_id_i,
        n_holdout_country = length(holdout_list_i[[split_id_i]]),
        holdout_countries = paste(sort(holdout_list_i[[split_id_i]]), collapse = "; ")
      )
    })
  )
  
  
  split_metric_list_i <- vector("list", n_validation_expected)
  split_prediction_list_i <- vector("list", n_validation_expected)
  
  
  # 25 joint MI combinations x 40 common holdout sets = 1000 evaluations
  for(combo_index_i in seq_len(n_combinations)){
    
    pc_imp_i <- combo_grid$.imp_pc[combo_index_i]
    dhs_imp_i <- combo_grid$.imp_dhs[combo_index_i]
    train_dat_combo_i <- combo_train_list_i[[combo_index_i]]
    
    
    for(split_id_i in seq_len(n_splits_per_combo)){
      
      validation_id_i <- (combo_index_i - 1) * n_splits_per_combo + split_id_i
      holdout_countries_i <- holdout_list_i[[split_id_i]]
      
      train_split_i <- train_dat_combo_i %>%
        filter(!Country %in% holdout_countries_i)
      
      test_split_i <- train_dat_combo_i %>%
        filter(Country %in% holdout_countries_i)
      
      if(nrow(train_split_i) == 0 || nrow(test_split_i) == 0){
        stop(
          "Empty train/test set for ", spec_name_i,
          " | PC ", pc_imp_i,
          " | DHS ", dhs_imp_i,
          " | split ", split_id_i
        )
      }
      
      
      # Same model matrix for glmnet, SVM, and XGBoost
      combined_matrix_i <- bind_rows(
        train_split_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
        test_split_i %>% transmute(PC1, PC2, Endemic, .set = "test")
      )
      
      x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
      
      x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
      x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
      
      prediction_split_i <- setNames(
        vector("list", length(candidate_model_keys)),
        candidate_model_keys
      )
      
      
      ## Linear model ----------------------------------------------------------
      
      prediction_split_i$lm <- tryCatch(
        {
          fit_i <- lm(base_formula_i, data = train_split_i, weights = weight_model)
          as.numeric(predict(fit_i, newdata = test_split_i))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      ## Spline linear model ---------------------------------------------------
      
      prediction_split_i$spline_lm <- tryCatch(
        {
          fit_i <- lm(
            logit_value ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3) + Endemic,
            data = train_split_i,
            weights = weight_model
          )
          
          as.numeric(predict(fit_i, newdata = test_split_i))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      ## GAM -------------------------------------------------------------------
      
      prediction_split_i$gam <- tryCatch(
        {
          fit_i <- mgcv::gam(
            logit_value ~ s(PC1, k = 4) + s(PC2, k = 4) + Endemic,
            data = train_split_i,
            weights = weight_model,
            method = "REML"
          )
          
          as.numeric(predict(fit_i, newdata = test_split_i, type = "response"))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      ## Random forest ---------------------------------------------------------
      
      prediction_split_i$rf_balanced <- tryCatch(
        {
          fit_i <- ranger::ranger(
            base_formula_i,
            data = train_split_i,
            case.weights = train_split_i$weight_model,
            num.trees = 1000,
            mtry = 2,
            min.node.size = 5,
            splitrule = "variance",
            seed = 123
          )
          
          as.numeric(predict(fit_i, data = test_split_i)$predictions)
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      prediction_split_i$rf_small_nodes <- tryCatch(
        {
          fit_i <- ranger::ranger(
            base_formula_i,
            data = train_split_i,
            case.weights = train_split_i$weight_model,
            num.trees = 1000,
            mtry = 3,
            min.node.size = 2,
            splitrule = "variance",
            seed = 123
          )
          
          as.numeric(predict(fit_i, data = test_split_i)$predictions)
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      prediction_split_i$extra_trees <- tryCatch(
        {
          fit_i <- ranger::ranger(
            base_formula_i,
            data = train_split_i,
            case.weights = train_split_i$weight_model,
            num.trees = 1500,
            mtry = 3,
            min.node.size = 3,
            splitrule = "extratrees",
            seed = 123
          )
          
          as.numeric(predict(fit_i, data = test_split_i)$predictions)
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      ## Ridge / elastic net / lasso ------------------------------------------
      
      glmnet_alpha_map_i <- c(
        ridge = 0,
        enet_025 = 0.25,
        enet_050 = 0.50,
        enet_075 = 0.75,
        lasso = 1
      )
      
      nfolds_i <- min(5, nrow(train_split_i))
      foldid_i <- ((seq_len(nrow(train_split_i)) - 1) %% nfolds_i) + 1
      
      
      for(model_key_i in names(glmnet_alpha_map_i)){
        
        fit_i <- tryCatch(
          glmnet::cv.glmnet(
            x = x_train_i,
            y = train_split_i$logit_value,
            weights = train_split_i$weight_model,
            alpha = glmnet_alpha_map_i[model_key_i],
            family = "gaussian",
            nfolds = nfolds_i,
            foldid = foldid_i
          ),
          error = function(e) NULL
        )
        
        prediction_split_i[[model_key_i]] <- if(is.null(fit_i)){
          rep(NA_real_, nrow(test_split_i))
        } else {
          tryCatch(
            as.numeric(predict(fit_i, newx = x_test_i, s = "lambda.min")),
            error = function(e) rep(NA_real_, nrow(test_split_i))
          )
        }
      }
      
      
      ## Radial SVM ------------------------------------------------------------
      
      if("svm_radial" %in% candidate_model_keys){
        
        prediction_split_i$svm_radial <- tryCatch(
          {
            fit_i <- kernlab::ksvm(
              x = x_train_i,
              y = train_split_i$logit_value,
              type = "eps-svr",
              kernel = "rbfdot",
              kpar = "automatic",
              C = 2,
              scaled = TRUE
            )
            
            as.numeric(kernlab::predict(fit_i, x_test_i))
          },
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      ## XGBoost ---------------------------------------------------------------
      
      dtrain_i <- xgboost::xgb.DMatrix(
        data = x_train_i,
        label = train_split_i$logit_value,
        weight = train_split_i$weight_model
      )
      
      
      prediction_split_i$xgb_smooth <- tryCatch(
        {
          fit_i <- xgboost::xgb.train(
            params = list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              max_depth = 2,
              eta = 0.02,
              subsample = 0.85,
              colsample_bytree = 0.85,
              min_child_weight = 5,
              seed = 123
            ),
            data = dtrain_i,
            nrounds = 500,
            verbose = 0
          )
          
          as.numeric(predict(fit_i, newdata = x_test_i))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      prediction_split_i$xgb_balanced <- tryCatch(
        {
          fit_i <- xgboost::xgb.train(
            params = list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              max_depth = 3,
              eta = 0.03,
              subsample = 0.80,
              colsample_bytree = 0.80,
              min_child_weight = 3,
              seed = 123
            ),
            data = dtrain_i,
            nrounds = 300,
            verbose = 0
          )
          
          as.numeric(predict(fit_i, newdata = x_test_i))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      prediction_split_i$xgb_deeper <- tryCatch(
        {
          fit_i <- xgboost::xgb.train(
            params = list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              max_depth = 4,
              eta = 0.02,
              subsample = 0.75,
              colsample_bytree = 0.75,
              min_child_weight = 2,
              seed = 123
            ),
            data = dtrain_i,
            nrounds = 450,
            verbose = 0
          )
          
          as.numeric(predict(fit_i, newdata = x_test_i))
        },
        error = function(e) rep(NA_real_, nrow(test_split_i))
      )
      
      
      ## Store validation predictions -----------------------------------------
      
      prediction_long_list_i <- vector("list", length(candidate_model_keys))
      metric_list_i <- vector("list", length(candidate_model_keys))
      
      
      for(model_index_i in seq_along(candidate_model_keys)){
        
        model_key_i <- candidate_model_keys[model_index_i]
        predicted_link_i <- prediction_split_i[[model_key_i]]
        
        valid_i <- is.finite(test_split_i$logit_value) & is.finite(predicted_link_i)
        
        
        prediction_long_list_i[[model_index_i]] <- test_split_i %>%
          transmute(
            outcome = outcome_i,
            age_group = age_group_i,
            spec_name = spec_name_i,
            validation_id = validation_id_i,
            combo_id = combo_index_i,
            .imp_pc = pc_imp_i,
            .imp_dhs = dhs_imp_i,
            split_id = split_id_i,
            Country,
            Year,
            gbd_region,
            model_key = model_key_i,
            model = unname(model_labels_i[model_key_i]),
            observed_link = logit_value,
            predicted_link = predicted_link_i,
            observed_proportion = plogis(logit_value),
            predicted_proportion = plogis(predicted_link_i)
          )
        
        
        if(!any(valid_i)){
          
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels_i[model_key_i]),
            rmse_link = NA_real_,
            mae_link = NA_real_,
            bias_link = NA_real_,
            abs_bias_link = NA_real_,
            rmse_proportion = NA_real_,
            mae_proportion = NA_real_,
            cor_proportion = NA_real_,
            n_eval = 0
          )
          
        } else {
          
          observed_link_i <- test_split_i$logit_value[valid_i]
          predicted_link_valid_i <- predicted_link_i[valid_i]
          
          observed_proportion_i <- plogis(observed_link_i)
          predicted_proportion_i <- plogis(predicted_link_valid_i)
          
          cor_proportion_i <- if(
            length(observed_proportion_i) >= 2 &&
            sd(observed_proportion_i) > 0 &&
            sd(predicted_proportion_i) > 0
          ){
            cor(observed_proportion_i, predicted_proportion_i)
          } else {
            NA_real_
          }
          
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels_i[model_key_i]),
            rmse_link = sqrt(mean((predicted_link_valid_i - observed_link_i)^2)),
            mae_link = mean(abs(predicted_link_valid_i - observed_link_i)),
            bias_link = mean(predicted_link_valid_i - observed_link_i),
            abs_bias_link = abs(mean(predicted_link_valid_i - observed_link_i)),
            rmse_proportion = sqrt(mean((predicted_proportion_i - observed_proportion_i)^2)),
            mae_proportion = mean(abs(predicted_proportion_i - observed_proportion_i)),
            cor_proportion = cor_proportion_i,
            n_eval = sum(valid_i)
          )
        }
      }
      
      
      split_metric_list_i[[validation_id_i]] <- bind_rows(metric_list_i) %>%
        mutate(
          outcome = outcome_i,
          age_group = age_group_i,
          spec_name = spec_name_i,
          validation_id = validation_id_i,
          combo_id = combo_index_i,
          .imp_pc = pc_imp_i,
          .imp_dhs = dhs_imp_i,
          split_id = split_id_i,
          n_holdout_country = length(holdout_countries_i),
          holdout_countries = paste(sort(holdout_countries_i), collapse = "; ")
        )
      
      split_prediction_list_i[[validation_id_i]] <- bind_rows(prediction_long_list_i)
      
      
      if(validation_id_i %% 100 == 0){
        message(
          spec_name_i,
          ": completed validation ",
          validation_id_i,
          " / ",
          n_validation_expected
        )
      }
    }
  }
  
  
  validation_result_list[[spec_index_i]] <- bind_rows(split_metric_list_i)
  validation_prediction_list[[spec_index_i]] <- bind_rows(split_prediction_list_i)
  
  
  saveRDS(
    validation_result_list[[spec_index_i]],
    file.path(model_search_dir, paste0(spec_name_i, "_validation_results.rds"))
  )
  
  saveRDS(
    validation_prediction_list[[spec_index_i]],
    file.path(model_search_dir, paste0(spec_name_i, "_validation_predictions.rds"))
  )
}



## 6. Combine validation results ----------------------------------------------

validation_results_marginal_re0 <- bind_rows(validation_result_list)
validation_predictions_marginal_re0 <- bind_rows(validation_prediction_list)
validation_design_marginal_re0 <- bind_rows(validation_design_list)


validation_count_check <- validation_results_marginal_re0 %>%
  count(spec_name, model_key, name = "n_validation")

print(validation_count_check, n = Inf)

stopifnot(all(validation_count_check$n_validation == 1000))



## 7. Overall validation summary ----------------------------------------------

validation_summary_marginal_re0 <- validation_results_marginal_re0 %>%
  group_by(outcome, age_group, spec_name, model_key, model) %>%
  summarise(
    mean_rmse_link = mean(rmse_link, na.rm = TRUE),
    median_rmse_link = median(rmse_link, na.rm = TRUE),
    mean_mae_link = mean(mae_link, na.rm = TRUE),
    mean_bias_link = mean(bias_link, na.rm = TRUE),
    mean_abs_bias_link = mean(abs_bias_link, na.rm = TRUE),
    mean_rmse_proportion = mean(rmse_proportion, na.rm = TRUE),
    mean_mae_proportion = mean(mae_proportion, na.rm = TRUE),
    mean_cor_proportion = mean(cor_proportion, na.rm = TRUE),
    n_valid_replicates = sum(is.finite(rmse_link)),
    total_replicates = n(),
    .groups = "drop"
  ) %>%
  mutate(valid_fraction = n_valid_replicates / total_replicates) %>%
  filter(n_valid_replicates > 0) %>%
  group_by(outcome, age_group, spec_name) %>%
  mutate(
    rmse_rank = rank(mean_rmse_link, ties.method = "min"),
    bias_rank = rank(mean_abs_bias_link, ties.method = "min"),
    combined_rank = rmse_rank + bias_rank
  ) %>%
  ungroup() %>%
  arrange(outcome, age_group, combined_rank, mean_rmse_link)



## 8. Select models ------------------------------------------------------------

model_selection_rules_marginal_re0 <- bind_rows(
  
  validation_summary_marginal_re0 %>%
    group_by(outcome, age_group, spec_name) %>%
    arrange(mean_rmse_link, mean_abs_bias_link, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(selection_rule = "Lowest mean RMSE on logit scale"),
  
  validation_summary_marginal_re0 %>%
    group_by(outcome, age_group, spec_name) %>%
    arrange(mean_abs_bias_link, mean_rmse_link, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(selection_rule = "Lowest absolute mean bias on logit scale"),
  
  validation_summary_marginal_re0 %>%
    group_by(outcome, age_group, spec_name) %>%
    arrange(combined_rank, mean_rmse_link, mean_abs_bias_link, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(selection_rule = "Best combined RMSE and bias rank")
) %>%
  dplyr::select(selection_rule, everything())


# Primary selected model
model_selection_marginal_re0 <- model_selection_rules_marginal_re0 %>%
  filter(selection_rule == "Best combined RMSE and bias rank") %>%
  arrange(outcome, age_group)



## 9. Model-selection stability across 25 joint MI combinations ---------------

validation_combo_summary_marginal_re0 <- validation_results_marginal_re0 %>%
  group_by(
    outcome, age_group, spec_name,
    .imp_pc, .imp_dhs,
    model_key, model
  ) %>%
  summarise(
    mean_rmse_link = mean(rmse_link, na.rm = TRUE),
    mean_abs_bias_link = mean(abs_bias_link, na.rm = TRUE),
    mean_rmse_proportion = mean(rmse_proportion, na.rm = TRUE),
    mean_mae_proportion = mean(mae_proportion, na.rm = TRUE),
    n_valid_splits = sum(is.finite(rmse_link)),
    .groups = "drop"
  ) %>%
  group_by(outcome, age_group, spec_name, .imp_pc, .imp_dhs) %>%
  mutate(
    rmse_rank = rank(mean_rmse_link, ties.method = "min"),
    bias_rank = rank(mean_abs_bias_link, ties.method = "min"),
    combined_rank = rmse_rank + bias_rank
  ) %>%
  ungroup()


model_selection_by_mi_marginal_re0 <- validation_combo_summary_marginal_re0 %>%
  group_by(outcome, age_group, spec_name, .imp_pc, .imp_dhs) %>%
  arrange(combined_rank, mean_rmse_link, mean_abs_bias_link, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()


model_selection_stability_marginal_re0 <- model_selection_by_mi_marginal_re0 %>%
  count(
    outcome, age_group, spec_name,
    model_key, model,
    name = "n_best_joint_MI"
  ) %>%
  group_by(outcome, age_group, spec_name) %>%
  mutate(proportion_best_joint_MI = n_best_joint_MI / 25) %>%
  ungroup() %>%
  arrange(outcome, age_group, spec_name, desc(n_best_joint_MI))



## 10. Save validation outputs -------------------------------------------------

saveRDS(
  validation_results_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_results.rds")
)

saveRDS(
  validation_predictions_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_predictions.rds")
)

saveRDS(
  validation_design_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_design.rds")
)

saveRDS(
  validation_summary_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_summary.rds")
)

saveRDS(
  model_selection_rules_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_model_selection_rules.rds")
)

saveRDS(
  model_selection_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_selected_models.rds")
)

saveRDS(
  validation_combo_summary_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_by_joint_MI.rds")
)

saveRDS(
  model_selection_by_mi_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_selected_models_by_joint_MI.rds")
)

saveRDS(
  model_selection_stability_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_model_selection_stability.rds")
)


write.csv(
  validation_summary_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_summary.csv"),
  row.names = FALSE
)

write.csv(
  model_selection_rules_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_model_selection_rules.csv"),
  row.names = FALSE
)

write.csv(
  model_selection_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_selected_models.csv"),
  row.names = FALSE
)

write.csv(
  validation_design_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_design.csv"),
  row.names = FALSE
)

write.csv(
  model_selection_stability_marginal_re0,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_model_selection_stability.csv"),
  row.names = FALSE
)



## 11. Keep original per-specification objects/files ---------------------------

for(spec_name_i in spec_name_list){
  
  validation_summary_i <- validation_summary_marginal_re0 %>%
    filter(spec_name == spec_name_i)
  
  model_selection_i <- model_selection_rules_marginal_re0 %>%
    filter(spec_name == spec_name_i)
  
  validation_prediction_i <- validation_predictions_marginal_re0 %>%
    filter(spec_name == spec_name_i)
  
  validation_result_i <- validation_results_marginal_re0 %>%
    filter(spec_name == spec_name_i)
  
  best_model_key_i <- model_selection_i %>%
    filter(selection_rule == "Best combined RMSE and bias rank") %>%
    pull(model_key) %>%
    .[1]
  
  assign(paste0(spec_name_i, "_validation_summary"), validation_summary_i)
  assign(paste0(spec_name_i, "_model_selection"), model_selection_i)
  assign(paste0(spec_name_i, "_validation_prediction"), validation_prediction_i)
  assign(paste0(spec_name_i, "_validation_result"), validation_result_i)
  assign(paste0(spec_name_i, "_best_model_key"), best_model_key_i)
  
  saveRDS(
    validation_summary_i,
    file.path(out_dir, paste0(spec_name_i, "_validation_summary.rds"))
  )
}


AA_validation_sumamry <- validation_summary_marginal_re0

dir.create("outputs/supplementary", recursive = TRUE, showWarnings = FALSE)

write.csv(
  AA_validation_sumamry,
  "outputs/supplementary/AA_validation_sumamry.csv",
  row.names = FALSE
)



## 12. Validation diagnostic plots --------------------------------------------

fig_dir_validation_i <- "figures/main_analysis/AA_validation_predictions"
dir.create(fig_dir_validation_i, recursive = TRUE, showWarnings = FALSE)

region_cols_fixed <- c(
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "Latin America and Caribbean" = "#E69F00",
  "North Africa and Middle East" = "#009E73",
  "South Asia" = "#A7B97F",
  "Sub-Saharan Africa" = "#0072B2"
)

validation_plot_object_list_i <- vector("list", length(spec_name_list))
names(validation_plot_object_list_i) <- spec_name_list


for(spec_name_i in spec_name_list){
  
  message("Creating validation plots: ", spec_name_i)
  
  validation_prediction_i <- validation_predictions_marginal_re0 %>%
    filter(
      spec_name == spec_name_i,
      is.finite(observed_link),
      is.finite(predicted_link),
      is.finite(observed_proportion),
      is.finite(predicted_proportion)
    )
  
  
  # Descriptive plot only:
  # average predictions over PC MI, DHS MI, and validation splits
  validation_plot_i <- validation_prediction_i %>%
    group_by(
      outcome, age_group, spec_name,
      Country, Year, gbd_region,
      model_key, model
    ) %>%
    summarise(
      observed_link = mean(observed_link, na.rm = TRUE),
      predicted_link = mean(predicted_link, na.rm = TRUE),
      observed_proportion = mean(observed_proportion, na.rm = TRUE),
      predicted_proportion = mean(predicted_proportion, na.rm = TRUE),
      residual_link = mean(predicted_link - observed_link, na.rm = TRUE),
      n_validation_predictions = n_distinct(validation_id),
      .groups = "drop"
    )
  
  validation_plot_object_list_i[[spec_name_i]] <- validation_plot_i
  
  if(nrow(validation_plot_i) == 0){
    warning("No finite validation predictions for ", spec_name_i)
    next
  }
  
  outcome_i <- unique(validation_plot_i$outcome)[1]
  age_group_i <- unique(validation_plot_i$age_group)[1]
  
  model_keys_i <- candidate_model_keys[
    candidate_model_keys %in% unique(validation_plot_i$model_key)
  ]
  
  region_levels_i <- names(region_cols_fixed)[
    names(region_cols_fixed) %in% unique(validation_plot_i$gbd_region)
  ]
  
  region_colors_i <- region_cols_fixed[region_levels_i]
  
  
  pdf_file_i <- file.path(
    fig_dir_validation_i,
    paste0(spec_name_i, "_all_models_validation_obs_pred_residual.pdf")
  )
  
  pdf(
    file = pdf_file_i,
    width = 6,
    height = 7.2,
    onefile = TRUE,
    useDingbats = FALSE
  )
  
  par(
    mfrow = c(4, 4),
    mar = c(3.5, 3.5, 3.0, 0.8),
    mgp = c(2.2, 0.7, 0),
    tcl = -0.25,
    cex.axis = 0.75,
    cex.lab = 0.80,
    cex.main = 0.75
  )
  
  
  for(model_index_i in seq_along(model_keys_i)){
    
    model_key_i <- model_keys_i[model_index_i]
    panel_column_i <- ((model_index_i - 1) %% 4) + 1
    panel_row_i <- ceiling(model_index_i / 4)
    
    ylab_i <- if(panel_column_i == 1) "Held-out predicted proportion" else ""
    xlab_i <- if(panel_row_i == 4) "Held-out observed proportion" else ""
    
    plot_data_i <- validation_plot_i %>%
      filter(model_key == model_key_i)
    
    model_name_i <- unique(plot_data_i$model)[1]
    valid_i <- is.finite(plot_data_i$observed_proportion) & is.finite(plot_data_i$predicted_proportion)
    
    if(sum(valid_i) < 2){
      plot.new()
      title(main = model_name_i)
      text(0.5, 0.5, labels = "No finite validation predictions", cex = 0.75)
      next
    }
    
    plot_data_i <- plot_data_i[valid_i, , drop = FALSE]
    
    point_colors_i <- region_colors_i[as.character(plot_data_i$gbd_region)]
    point_colors_i[is.na(point_colors_i)] <- "grey60"
    
    
    if(outcome_i == "LRTI"){
      x_axis_at_i <- c(0.2, 0.4, 0.6, 0.8)
      y_axis_at_i <- c(0.2, 0.4, 0.6, 0.8)
      xmin_i <- ymin_i <- 0.2
      xmax_i <- ymax_i <- 0.8
    } else {
      x_axis_at_i <- c(0.0, 0.2, 0.4, 0.6)
      y_axis_at_i <- c(0.0, 0.2, 0.4, 0.6)
      xmin_i <- ymin_i <- 0.0
      xmax_i <- ymax_i <- 0.6
    }
    
    
    plot(
      plot_data_i$observed_proportion,
      plot_data_i$predicted_proportion,
      col = point_colors_i,
      pch = 16,
      cex = 0.65,
      xlim = c(xmin_i, xmax_i),
      ylim = c(ymin_i, ymax_i),
      asp = 1,
      axes = FALSE,
      ann = FALSE,
      xaxs = "i",
      yaxs = "i"
    )
    
    axis(side = 1, at = x_axis_at_i, labels = FALSE, pos = ymin_i, tck = -0.03, lwd = 0.8, lwd.ticks = 0.8)
    axis(side = 2, at = y_axis_at_i, labels = FALSE, pos = xmin_i, tck = -0.03, lwd = 0.8, lwd.ticks = 0.8)
    
    text(x = x_axis_at_i, y = ymin_i - 0.055 * (ymax_i - ymin_i), labels = formatC(x_axis_at_i, format = "f", digits = 1), xpd = TRUE, cex = 0.65)
    text(x = xmin_i - 0.055 * (xmax_i - xmin_i), y = y_axis_at_i, labels = formatC(y_axis_at_i, format = "f", digits = 1), adj = 1, xpd = TRUE, cex = 0.65)
    
    if(xlab_i != "") mtext(xlab_i, side = 1, line = 2.1, cex = 0.5)
    if(ylab_i != "") mtext(ylab_i, side = 2, line = 2.1, cex = 0.5)
    
    title(
      main = paste0(outcome_i, ", ", age_group_i, ",\n", model_name_i),
      cex.main = 0.75,
      line = 1
    )
    
    abline(a = 0, b = 1, lty = 2, col = "red")
  }
  
  
  if(length(model_keys_i) < 15){
    for(blank_i in seq_len(15 - length(model_keys_i))){
      plot.new()
    }
  }
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  
  legend(
    "center",
    legend = region_levels_i,
    col = region_colors_i,
    pch = 16,
    pt.cex = 0.9,
    bty = "n",
    cex = 0.52,
    title = "GBD region"
  )
  
  dev.off()
  
  message("PDF written: ", pdf_file_i)
}


saveRDS(
  validation_plot_object_list_i,
  file.path(model_search_dir, "AA_marginal_RE0_age_specific_validation_plot_objects.rds")
)



## 13. Final results -----------------------------------------------------------

cat("\nSelected models based on combined RMSE + absolute-bias rank:\n")
print(model_selection_marginal_re0, n = Inf)

cat("\nModel-selection stability across 25 joint PC x DHS MI combinations:\n")
print(model_selection_stability_marginal_re0, n = Inf)


for(spec_name_i in spec_name_list){
  
  cat("\n", spec_name_i, "\n", sep = "")
  
  print(
    model_selection_rules_marginal_re0 %>%
      filter(spec_name == spec_name_i)
  )
}


cat(
  "\nFinal validation design:\n",
  "PC imputations = ", length(pc_imp_values),
  "\nDHS imputations = ", length(dhs_imp_values),
  "\nHoldout splits per joint MI combination = ", n_splits_per_combo,
  "\nJoint MI combinations = ", n_combinations,
  "\nValidation replicates per candidate model = ", n_validation_expected,
  "\n",
  sep = ""
)

message(
  "Section 7 complete. Outputs saved to: ",
  normalizePath(model_search_dir, mustWork = FALSE)
)



 












# _____________________________________________ ----------
# 8. Selected-model uncertainty projection -----------------------------------
# One common selected model across all three age groups for each outcome
# 5 PC imputations x 5 DHS imputations x 40 draws = 1000 uncertainty draws
# enet_050 showed conssitently good performance,
# SVM showed slighted better performance than gam but couldn't reflect the survey weight. 


rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(mice)
  library(glmmTMB)
  library(MASS)
  library(glmnet)
  library(mgcv)
  library(ranger)
  library(xgboost)
})

set.seed(123)
options(scipen = 999)

out_dir <- "outputs/pipelines"
selected_model_uncertainty_dir_i <- file.path(out_dir, "AA_age_specific_selected_model_uncertainty")
dir.create(selected_model_uncertainty_dir_i, recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------#
# CHANGE ONLY THESE TWO LINES AFTER REVIEWING SECTION 7 -----------------------

selected_model_key_LRTI_i <- "enet_025"
selected_model_key_fever_i <- "gam"

# -----------------------------------------------------------------------------#


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

glmnet_alpha_i <- c(
  ridge = 0,
  enet_025 = 0.25,
  enet_050 = 0.50,
  enet_075 = 0.75,
  lasso = 1
)

stopifnot(selected_model_key_LRTI_i %in% names(model_labels_i))
stopifnot(selected_model_key_fever_i %in% names(model_labels_i))

if(selected_model_key_LRTI_i == "svm_radial" || selected_model_key_fever_i == "svm_radial"){
  if(!requireNamespace("kernlab", quietly = TRUE)) stop("kernlab is required for svm_radial.")
}



# 8-1. Load MI and first-stage GLMM objects -----------------------------------

LRTI_imp_i <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_LRTI.rds")
LRTI_fit_i <- readRDS("outputs/pipelines/(GBD_RE)AA_fit_lrti_mi.rds")

fever_imp_i <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_tropical_Fever.rds")
fever_fit_i <- readRDS("outputs/pipelines/(GBD_RE)AA_fit_fever_mi.rds")

UNI_raw_i <- readRDS(file.path(out_dir, "pc_gdp_2023_incidence.rds"))

draw_design_i <- readRDS(
  file.path(
    out_dir,
    "age_specific_selected_model_uncertainty",
    "uncertainty_draw_design_5PC_5DHS_40draw.rds"
  )
)

stopifnot(
  length(LRTI_fit_i$analyses) == LRTI_imp_i$m,
  length(fever_fit_i$analyses) == fever_imp_i$m,
  all(vapply(LRTI_fit_i$analyses, inherits, logical(1), "glmmTMB")),
  all(vapply(fever_fit_i$analyses, inherits, logical(1), "glmmTMB")),
  nrow(draw_design_i) == 1000,
  length(unique(draw_design_i$.imp_pc)) == 5,
  length(unique(draw_design_i$.imp_dhs)) == 5
)

draw_balance_i <- draw_design_i %>% count(.imp_pc, .imp_dhs, name = "n_draw")

stopifnot(all(draw_balance_i$n_draw == 40))

n_draw_i <- nrow(draw_design_i)
holdout_prop_i <- 0.10

print(draw_balance_i)



# 8-2. Country-name map -------------------------------------------------------

country_name_map <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Democratic Republic of Congo" = "Congo, Dem. Rep.",
  "Congo [DRC]" = "Congo, Dem. Rep.",
  "Congo \\[DRC\\]" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "Republic of the Congo" = "Congo, Rep.",
  "Congo [Republic]" = "Congo, Rep.",
  "Congo \\[Republic\\]" = "Congo, Rep.",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "C\u0099te d'Ivoire" = "Cote d'Ivoire",
  "CÂ\u0099te d'Ivoire" = "Cote d'Ivoire",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Swaziland" = "Eswatini",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Iran" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Laos" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "Macedonia [FYROM]" = "North Macedonia",
  "Macedonia \\[FYROM\\]" = "North Macedonia",
  "Russia" = "Russian Federation",
  "United Republic of Tanzania" = "Tanzania",
  "T\x9frkiye" = "Turkiye",
  "T\u009frkiye" = "Turkiye",
  "Türkiye" = "Turkiye",
  "Turkey" = "Turkiye",
  "Vietnam" = "Viet Nam",
  "Yemen" = "Yemen, Rep.",
  "Myanmar [Burma]" = "Myanmar",
  "Myanmar \\[Burma\\]" = "Myanmar"
)



# 8-3. Model-based covariance matrices -----------------------------------------

get_vcov_list_i <- function(fit_mi_i){
  
  lapply(seq_along(fit_mi_i$analyses), function(k){
    
    fit_i <- fit_mi_i$analyses[[k]]
    beta_names_i <- names(glmmTMB::fixef(fit_i)$cond)
    
    vcov_i <- as.matrix(stats::vcov(fit_i)$cond)
    
    if(!all(beta_names_i %in% rownames(vcov_i))){
      stop("Coefficient names do not match covariance matrix in DHS imputation ", k)
    }
    
    vcov_i <- vcov_i[beta_names_i, beta_names_i, drop = FALSE]
    vcov_i <- (vcov_i + t(vcov_i)) / 2
    
    if(any(!is.finite(vcov_i))){
      stop("Non-finite covariance values in DHS imputation ", k)
    }
    
    eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
    eigen_scale_i <- max(max(abs(eigen_i)), .Machine$double.eps)
    
    if(min(eigen_i) < -1e-6 * eigen_scale_i){
      stop(
        "Covariance is not positive semidefinite in DHS imputation ",
        k,
        "; minimum eigenvalue = ",
        signif(min(eigen_i), 5)
      )
    }
    
    vcov_i
  })
}

vcov_LRTI_list_i <- get_vcov_list_i(LRTI_fit_i)
vcov_fever_list_i <- get_vcov_list_i(fever_fit_i)

vcov_check_i <- bind_rows(
  lapply(c("LRTI", "fever"), function(outcome_i){
    
    fit_mi_check_i <- if(outcome_i == "LRTI") LRTI_fit_i else fever_fit_i
    vcov_list_check_i <- if(outcome_i == "LRTI") vcov_LRTI_list_i else vcov_fever_list_i
    
    bind_rows(lapply(seq_along(vcov_list_check_i), function(k){
      
      vcov_i <- vcov_list_check_i[[k]]
      eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
      
      tibble(
        outcome = outcome_i,
        .imp_dhs = k,
        n_gbd_regions = nlevels(droplevels(factor(model.frame(fit_mi_check_i$analyses[[k]])$gbd_region))),
        n_coefficients = nrow(vcov_i),
        covariance_rank = qr(vcov_i)$rank,
        minimum_eigenvalue = min(eigen_i),
        maximum_eigenvalue = max(eigen_i)
      )
    }))
  })
)

print(vcov_check_i)

saveRDS(vcov_check_i, file.path(selected_model_uncertainty_dir_i, "AA_vcov_check.rds"))
write.csv(vcov_check_i, file.path(selected_model_uncertainty_dir_i, "AA_vcov_check.csv"), row.names = FALSE)



# 8-4. Prepare PC-imputation-specific UNI -------------------------------------

UNI_model_i <- UNI_raw_i %>%
  mutate(
    .imp_pc = as.integer(.imp_pc),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    GDP = as.numeric(gsub(",", "", as.character(GDP))),
    Population_under5 = as.numeric(gsub(",", "", as.character(Population))),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2),
    Endemic = case_when(
      as.character(Endemic) %in% c("1", "TRUE", "True", "true", "endemic", "Endemic") ~ 1,
      as.character(Endemic) %in% c("0", "FALSE", "False", "false", "non-endemic", "Non-endemic") ~ 0,
      TRUE ~ suppressWarnings(as.numeric(as.character(Endemic)))
    ),
    gbd_region = dplyr::recode(
      as.character(gbd_region),
      "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
      .default = as.character(gbd_region)
    )
  ) %>%
  filter(
    is.finite(Year),
    is.finite(GDP), GDP > 0,
    is.finite(Population_under5), Population_under5 > 0,
    is.finite(PC1), is.finite(PC2),
    !is.na(Endemic),
    !is.na(gbd_region)
  )

pc_imp_values_i <- sort(unique(UNI_model_i$.imp_pc))

stopifnot(length(pc_imp_values_i) == 5)

for(pc_imp_i in pc_imp_values_i){
  pc_check_i <- UNI_model_i %>% filter(.imp_pc == pc_imp_i)
  if(anyDuplicated(pc_check_i[c("Country", "Year")])) stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
}



# 8-5. Age-specific population ------------------------------------------------

df_population_i <- read.csv("data/metadata/Population_by_age.csv", stringsAsFactors = FALSE)

age_population_long_i <- df_population_i %>%
  mutate(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    Age_0 = as.numeric(gsub(",", "", as.character(Age_0))),
    Age_1 = as.numeric(gsub(",", "", as.character(Age_1))),
    Age_2 = as.numeric(gsub(",", "", as.character(Age_2))),
    Age_3 = as.numeric(gsub(",", "", as.character(Age_3))),
    Age_4 = as.numeric(gsub(",", "", as.character(Age_4)))
  ) %>%
  transmute(
    Country,
    Year,
    `0-11 months` = Age_0,
    `12-23 months` = Age_1,
    `24-59 months` = Age_2 + Age_3 + Age_4
  ) %>%
  pivot_longer(
    cols = c(`0-11 months`, `12-23 months`, `24-59 months`),
    names_to = "age_group",
    values_to = "age_population"
  ) %>%
  mutate(age_group = as.character(age_group), age_population = as.numeric(age_population)) %>%
  filter(is.finite(Year), is.finite(age_population), age_population > 0)



# 8-6. Specification information ---------------------------------------------

outcome_list_i <- c("LRTI", "fever")
age_group_list_i <- c("0-11 months", "12-23 months", "24-59 months")

spec_name_matrix_i <- matrix(
  c(
    "LRTI_0_11_months",
    "LRTI_12_23_months",
    "LRTI_24_59_months",
    "fever_0_11_months",
    "fever_12_23_months",
    "fever_24_59_months"
  ),
  nrow = 2,
  byrow = TRUE
)

selected_model_uncertainty_result_list_i <- vector("list", 6)
names(selected_model_uncertainty_result_list_i) <- as.vector(t(spec_name_matrix_i))

selected_model_holdout_result_list_i <- vector("list", 2 * n_draw_i * 3)
holdout_result_index_i <- 0L



# 8-7. Fixed Country-Year prediction-grid keys --------------------------------

prediction_grid_base_list_i <- vector("list", 3)

for(j in 1:3){
  
  age_group_i <- age_group_list_i[j]
  
  prediction_grid_base_list_i[[j]] <- UNI_model_i %>%
    filter(.imp_pc == pc_imp_values_i[1]) %>%
    dplyr::select(Country, Year, GDP, any_of("Income"), Population_under5, Endemic, gbd_region) %>%
    mutate(age_group = age_group_i) %>%
    left_join(
      age_population_long_i %>%
        filter(age_group == age_group_i) %>%
        dplyr::select(Country, Year, age_group, age_population),
      by = c("Country", "Year", "age_group")
    ) %>%
    filter(is.finite(age_population), age_population > 0) %>%
    arrange(Country, Year)
}




# Selected-model prediction helper for holdout validation ----------------------
# Uses the same model specifications and model weights as Sections 7 and 8.

predict_selected_holdout_i <- function(
    train_i,
    test_i,
    model_key_i,
    weight_i,
    seed_i
){
  
  predicted_link_i <- rep(NA_real_, nrow(test_i))
  model_error_i <- NA_character_
  lambda_selected_i <- NA_real_
  
  
  if(model_key_i == "lm"){
    
    fit_i <- tryCatch(
      lm(logit_value ~ PC1 + PC2 + Endemic, data = train_i, weights = weight_i),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, newdata = test_i)),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
    
  } else if(model_key_i == "spline_lm"){
    
    fit_i <- tryCatch(
      lm(
        logit_value ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3) + Endemic,
        data = train_i,
        weights = weight_i
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, newdata = test_i)),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
    
  } else if(model_key_i == "gam"){
    
    fit_i <- tryCatch(
      mgcv::gam(
        logit_value ~ s(PC1, k = 4) + s(PC2, k = 4) + Endemic,
        data = train_i,
        weights = weight_i,
        method = "REML"
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, newdata = test_i, type = "response")),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
    
  } else if(model_key_i %in% c("rf_balanced", "rf_small_nodes", "extra_trees")){
    
    if(model_key_i == "rf_balanced"){
      num_trees_i <- 1000
      mtry_i <- 2
      min_node_i <- 5
      splitrule_i <- "variance"
    }
    
    if(model_key_i == "rf_small_nodes"){
      num_trees_i <- 1000
      mtry_i <- 3
      min_node_i <- 2
      splitrule_i <- "variance"
    }
    
    if(model_key_i == "extra_trees"){
      num_trees_i <- 1500
      mtry_i <- 3
      min_node_i <- 3
      splitrule_i <- "extratrees"
    }
    
    fit_i <- tryCatch(
      ranger::ranger(
        logit_value ~ PC1 + PC2 + Endemic,
        data = train_i,
        case.weights = weight_i,
        num.trees = num_trees_i,
        mtry = mtry_i,
        min.node.size = min_node_i,
        splitrule = splitrule_i,
        seed = seed_i
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, data = test_i)$predictions),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
    
  } else if(model_key_i %in% names(glmnet_alpha_i)){
    
    combined_matrix_i <- bind_rows(
      train_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
      test_i %>% transmute(PC1, PC2, Endemic, .set = "test")
    )
    
    x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
    x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
    x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
    
    nfolds_i <- min(5, nrow(train_i))
    foldid_i <- ((seq_len(nrow(train_i)) - 1) %% nfolds_i) + 1
    
    fit_i <- tryCatch(
      glmnet::cv.glmnet(
        x = x_train_i,
        y = train_i$logit_value,
        weights = weight_i,
        alpha = glmnet_alpha_i[model_key_i],
        family = "gaussian",
        nfolds = nfolds_i,
        foldid = foldid_i
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, newx = x_test_i, s = "lambda.min")),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
      lambda_selected_i <- fit_i$lambda.min
    }
    
    
  } else if(model_key_i == "svm_radial"){
    
    combined_matrix_i <- bind_rows(
      train_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
      test_i %>% transmute(PC1, PC2, Endemic, .set = "test")
    )
    
    x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
    x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
    x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
    
    fit_i <- tryCatch(
      kernlab::ksvm(
        x = x_train_i,
        y = train_i$logit_value,
        type = "eps-svr",
        kernel = "rbfdot",
        kpar = "automatic",
        C = 2,
        scaled = TRUE
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(kernlab::predict(fit_i, x_test_i)),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
    
  } else if(model_key_i %in% c("xgb_smooth", "xgb_balanced", "xgb_deeper")){
    
    combined_matrix_i <- bind_rows(
      train_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
      test_i %>% transmute(PC1, PC2, Endemic, .set = "test")
    )
    
    x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
    x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
    x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
    
    if(model_key_i == "xgb_smooth"){
      xgb_params_i <- list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = 2,
        eta = 0.02,
        subsample = 0.85,
        colsample_bytree = 0.85,
        min_child_weight = 5,
        seed = seed_i
      )
      nrounds_i <- 500
    }
    
    if(model_key_i == "xgb_balanced"){
      xgb_params_i <- list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = 3,
        eta = 0.03,
        subsample = 0.80,
        colsample_bytree = 0.80,
        min_child_weight = 3,
        seed = seed_i
      )
      nrounds_i <- 300
    }
    
    if(model_key_i == "xgb_deeper"){
      xgb_params_i <- list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = 4,
        eta = 0.02,
        subsample = 0.75,
        colsample_bytree = 0.75,
        min_child_weight = 2,
        seed = seed_i
      )
      nrounds_i <- 450
    }
    
    dtrain_i <- xgboost::xgb.DMatrix(
      data = x_train_i,
      label = train_i$logit_value,
      weight = weight_i
    )
    
    fit_i <- tryCatch(
      xgboost::xgb.train(
        params = xgb_params_i,
        data = dtrain_i,
        nrounds = nrounds_i,
        verbose = 0
      ),
      error = function(e){model_error_i <<- conditionMessage(e); NULL}
    )
    
    if(!is.null(fit_i)){
      predicted_link_i <- tryCatch(
        as.numeric(predict(fit_i, newdata = x_test_i)),
        error = function(e){model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(test_i))}
      )
    }
    
  } else {
    model_error_i <- paste0("Unsupported selected model key: ", model_key_i)
  }
  
  
  list(
    predicted_link = predicted_link_i,
    lambda_selected = lambda_selected_i,
    error = model_error_i
  )
}



# 8-8. Run uncertainty projection --------------------------------------------

for(i in 1:2){
  
  outcome_i <- outcome_list_i[i]
  spec_names_i <- spec_name_matrix_i[i, ]
  
  if(outcome_i == "LRTI"){
    imp_i <- LRTI_imp_i
    fit_mi_i <- LRTI_fit_i
    vcov_list_i <- vcov_LRTI_list_i
    selected_model_key_i <- selected_model_key_LRTI_i
    seed_base_i <- 100000
  } else {
    imp_i <- fever_imp_i
    fit_mi_i <- fever_fit_i
    vcov_list_i <- vcov_fever_list_i
    selected_model_key_i <- selected_model_key_fever_i
    seed_base_i <- 200000
  }
  
  selected_model_label_i <- unname(model_labels_i[selected_model_key_i])
  selected_alpha_i <- if(selected_model_key_i %in% names(glmnet_alpha_i)) unname(glmnet_alpha_i[selected_model_key_i]) else NA_real_
  
  cat(
    "\nOutcome: ", outcome_i,
    "\nSelected common model: ", selected_model_label_i,
    "\nModel key: ", selected_model_key_i,
    "\n",
    sep = ""
  )
  
  
  projection_matrix_list_i <- lapply(1:3, function(j){
    matrix(
      NA_real_,
      nrow = nrow(prediction_grid_base_list_i[[j]]),
      ncol = n_draw_i,
      dimnames = list(NULL, paste0("draw_", sprintf("%04d", seq_len(n_draw_i))))
    )
  })
  
  draw_success_i <- matrix(
    FALSE,
    nrow = n_draw_i,
    ncol = 3,
    dimnames = list(paste0("draw_", sprintf("%04d", seq_len(n_draw_i))), age_group_list_i)
  )
  
  draw_message_i <- matrix(
    NA_character_,
    nrow = n_draw_i,
    ncol = 3,
    dimnames = list(paste0("draw_", sprintf("%04d", seq_len(n_draw_i))), age_group_list_i)
  )
  
  
  for(draw_id_i in seq_len(n_draw_i)){
    
    design_i <- draw_design_i %>% filter(draw_id == draw_id_i)
    
    pc_imp_i <- design_i$.imp_pc
    dhs_imp_i <- design_i$.imp_dhs
    draw_within_combo_i <- design_i$.draw_within_combo
    
    
    # First-stage DHS coefficient draw ----------------------------------------
    
    set.seed(seed_base_i + draw_id_i)
    
    dat_i <- mice::complete(imp_i, dhs_imp_i)
    fit_i <- fit_mi_i$analyses[[dhs_imp_i]]
    
    fit_frame_i <- model.frame(fit_i)
    used_rows_i <- match(rownames(fit_frame_i), rownames(dat_i))
    
    if(anyNA(used_rows_i)) used_rows_i <- suppressWarnings(as.integer(rownames(fit_frame_i)))
    
    if(length(used_rows_i) != nrow(fit_frame_i) || anyNA(used_rows_i)){
      draw_message_i[draw_id_i, ] <- "Could not align GLMM model-frame rows"
      next
    }
    
    dat_used_i <- dat_i[used_rows_i, , drop = FALSE]
    
    beta_hat_i <- glmmTMB::fixef(fit_i)$cond
    beta_vcov_i <- vcov_list_i[[dhs_imp_i]]
    beta_vcov_i <- beta_vcov_i[names(beta_hat_i), names(beta_hat_i), drop = FALSE]
    
    beta_draw_i <- tryCatch(
      MASS::mvrnorm(n = 1, mu = beta_hat_i, Sigma = beta_vcov_i, tol = 1e-6),
      error = function(e) rep(NA_real_, length(beta_hat_i))
    )
    
    names(beta_draw_i) <- names(beta_hat_i)
    
    if(any(!is.finite(beta_draw_i))){
      draw_message_i[draw_id_i, ] <- "First-stage coefficient draw failed"
      next
    }
    
    
    # Fixed-effect marginal RE0 prediction ------------------------------------
    
    X_i <- model.matrix(fit_i, component = "cond", part = "fixed")
    X_i <- X_i[, names(beta_hat_i), drop = FALSE]
    
    if(nrow(X_i) != nrow(dat_used_i)){
      draw_message_i[draw_id_i, ] <- "Fixed-effect matrix and model data do not align"
      next
    }
    
    dat_used_i$pred_proportion_draw <- plogis(as.vector(X_i %*% beta_draw_i))
    
    
    # Matching PC imputation --------------------------------------------------
    
    pc_dat_i <- UNI_model_i %>%
      filter(.imp_pc == pc_imp_i) %>%
      dplyr::select(
        .imp_pc,
        Country,
        Year,
        GDP,
        any_of("Income"),
        Population_under5,
        PC1,
        PC2,
        Endemic,
        gbd_region
      )
    
    
    # Aggregate first-stage predictions --------------------------------------
    
    train_draw_all_age_i <- dat_used_i %>%
      mutate(
        Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
        Country = trimws(gsub("\\\\", "", Country)),
        Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
        Year = as.numeric(Year_c + 2010),
        age_group = as.character(ch_age_group),
        gbd_region = dplyr::recode(
          as.character(gbd_region),
          "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
          .default = as.character(gbd_region)
        )
      ) %>%
      filter(
        age_group %in% age_group_list_i,
        is.finite(sample_weight), sample_weight > 0,
        is.finite(pred_proportion_draw)
      ) %>%
      group_by(age_group, Country, Year, CountryYear, gbd_region) %>%
      summarise(
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        pred_proportion = weighted.mean(pred_proportion_draw, sample_weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        pc_dat_i %>%
          dplyr::select(Country, Year, PC1, PC2, Endemic, gbd_region_from_uni = gbd_region),
        by = c("Country", "Year")
      ) %>%
      mutate(
        .imp_pc = pc_imp_i,
        .imp_dhs = dhs_imp_i,
        gbd_region = coalesce(as.character(gbd_region), as.character(gbd_region_from_uni)),
        gbd_region = dplyr::recode(
          gbd_region,
          "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
          .default = gbd_region
        ),
        Endemic = as.numeric(Endemic),
        proportion_bounded = pmin(pmax(pred_proportion, 1e-6), 1 - 1e-6),
        logit_value = qlogis(proportion_bounded)
      ) %>%
      dplyr::select(-gbd_region_from_uni) %>%
      filter(
        is.finite(logit_value),
        is.finite(PC1),
        is.finite(PC2),
        is.finite(Endemic),
        !is.na(gbd_region)
      )
    
    if(nrow(train_draw_all_age_i) < 10){
      draw_message_i[draw_id_i, ] <- "Too few first-stage country-year estimates"
      next
    }
    
    
    # Draw-specific 90/10 country holdout validation --------------------------
    # The seed depends on .draw_within_combo rather than draw_id, so each of the
    # 40 holdout sets is reused across all 25 PC x DHS combinations.
    
    for(j in 1:3){
      
      age_group_i <- age_group_list_i[j]
      
      validation_age_all_i <- train_draw_all_age_i %>%
        filter(age_group == age_group_i)
      
      if(nrow(validation_age_all_i) < 10) next
      
      
      # Same capped country-year weight as Section 7
      
      weight_raw_validation_i <- as.numeric(validation_age_all_i$n_child)
      weight_valid_validation_i <- is.finite(weight_raw_validation_i) & weight_raw_validation_i > 0
      
      if(any(weight_valid_validation_i)){
        weight_raw_validation_i[!weight_valid_validation_i] <- median(
          weight_raw_validation_i[weight_valid_validation_i],
          na.rm = TRUE
        )
        weight_cap_validation_i <- as.numeric(
          quantile(weight_raw_validation_i, 0.95, na.rm = TRUE)
        )
        validation_age_all_i$weight_model <- pmin(
          weight_raw_validation_i,
          weight_cap_validation_i
        )
      } else {
        validation_age_all_i$weight_model <- 1
      }
      
      
      # Country-level 10% holdout
      
      country_meta_validation_i <- validation_age_all_i %>%
        distinct(Country, Endemic) %>%
        arrange(Country)
      
      countries_validation_i <- sort(unique(country_meta_validation_i$Country))
      holdout_countries_validation_i <- character(0)
      
      if(length(countries_validation_i) >= 3){
        
        n_holdout_validation_i <- max(
          1,
          ceiling(length(countries_validation_i) * holdout_prop_i)
        )
        
        set.seed(
          seed_base_i +
            2000000 +
            draw_within_combo_i * 10 +
            j
        )
        
        for(attempt_i in seq_len(1000)){
          
          holdout_try_i <- sample(
            countries_validation_i,
            n_holdout_validation_i,
            replace = FALSE
          )
          
          endemic_all_validation_i <- sort(
            unique(as.character(country_meta_validation_i$Endemic))
          )
          
          endemic_train_validation_i <- country_meta_validation_i %>%
            filter(!Country %in% holdout_try_i) %>%
            pull(Endemic) %>%
            as.character() %>%
            unique() %>%
            sort()
          
          if(
            length(endemic_all_validation_i) == 1 ||
            setequal(endemic_all_validation_i, endemic_train_validation_i)
          ){
            holdout_countries_validation_i <- holdout_try_i
            break
          }
        }
      }
      
      
      if(length(holdout_countries_validation_i) > 0){
        
        validation_train_i <- validation_age_all_i %>%
          filter(!Country %in% holdout_countries_validation_i) %>%
          filter(is.finite(weight_model), weight_model > 0)
        
        validation_test_i <- validation_age_all_i %>%
          filter(Country %in% holdout_countries_validation_i)
        
        if(nrow(validation_train_i) >= 10 && nrow(validation_test_i) > 0){
          
          holdout_fit_i <- predict_selected_holdout_i(
            train_i = validation_train_i,
            test_i = validation_test_i,
            model_key_i = selected_model_key_i,
            weight_i = validation_train_i$weight_model,
            seed_i = seed_base_i + 3000000 + draw_id_i * 10 + j
          )
          
          predicted_holdout_link_i <- holdout_fit_i$predicted_link
          
          if(
            length(predicted_holdout_link_i) == nrow(validation_test_i) &&
            any(is.finite(predicted_holdout_link_i))
          ){
            
            validation_test_i$predicted_logit <- predicted_holdout_link_i
            
            validation_test_i <- validation_test_i %>%
              mutate(
                predicted_proportion = plogis(predicted_logit)
              ) %>%
              filter(
                is.finite(logit_value),
                is.finite(predicted_logit),
                is.finite(pred_proportion),
                is.finite(predicted_proportion)
              )
            
            if(nrow(validation_test_i) > 0){
              
              holdout_result_index_i <- holdout_result_index_i + 1L
              
              selected_model_holdout_result_list_i[[holdout_result_index_i]] <-
                validation_test_i %>%
                transmute(
                  draw_id = draw_id_i,
                  .draw_within_combo = draw_within_combo_i,
                  .imp_pc = pc_imp_i,
                  .imp_dhs = dhs_imp_i,
                  outcome = outcome_i,
                  prediction_type = "country_holdout",
                  model_key = selected_model_key_i,
                  model = selected_model_label_i,
                  alpha = selected_alpha_i,
                  lambda_selected = holdout_fit_i$lambda_selected,
                  Country,
                  Year,
                  CountryYear,
                  gbd_region,
                  age_group,
                  n_child,
                  weight_model,
                  n_train_country = n_distinct(validation_train_i$Country),
                  n_holdout_country = n_distinct(validation_test_i$Country),
                  heldout_estimated_logit = logit_value,
                  predicted_logit,
                  heldout_estimated_proportion = pred_proportion,
                  predicted_proportion,
                  residual_logit = predicted_logit - heldout_estimated_logit,
                  residual_proportion = predicted_proportion - heldout_estimated_proportion
                )
            }
          }
        }
      }
    }
    
    
    # One country bootstrap shared across all three ages ----------------------
    
    countries_i <- unique(train_draw_all_age_i$Country)
    endemic_values_i <- sort(unique(as.character(train_draw_all_age_i$Endemic)))
    boot_countries_i <- character(0)
    
    set.seed(seed_base_i + 1000000 + draw_id_i)
    
    for(attempt_i in seq_len(1000)){
      
      boot_try_i <- sample(countries_i, length(countries_i), replace = TRUE)
      
      boot_endemic_i <- train_draw_all_age_i %>%
        filter(Country %in% unique(boot_try_i)) %>%
        pull(Endemic) %>%
        as.character() %>%
        unique() %>%
        sort()
      
      if(length(endemic_values_i) == 1 || setequal(endemic_values_i, boot_endemic_i)){
        boot_countries_i <- boot_try_i
        break
      }
    }
    
    if(length(boot_countries_i) == 0){
      draw_message_i[draw_id_i, ] <- "Could not generate valid country bootstrap"
      next
    }
    
    boot_country_count_i <- as.data.frame(table(boot_countries_i), stringsAsFactors = FALSE)
    names(boot_country_count_i) <- c("Country", "bootstrap_count")
    boot_country_count_i$Country <- as.character(boot_country_count_i$Country)
    boot_country_count_i$bootstrap_count <- as.numeric(boot_country_count_i$bootstrap_count)
    
    
    # Fit same selected model separately within each age ----------------------
    
    for(j in 1:3){
      
      spec_name_i <- spec_names_i[j]
      age_group_i <- age_group_list_i[j]
      
      train_draw_age_i <- train_draw_all_age_i %>%
        filter(age_group == age_group_i)
      
      if(nrow(train_draw_age_i) < 10){
        draw_message_i[draw_id_i, j] <- "Too few age-specific training rows"
        next
      }
      
      
      # Same capped weight as Section 7
      weight_raw_i <- as.numeric(train_draw_age_i$n_child)
      weight_valid_i <- is.finite(weight_raw_i) & weight_raw_i > 0
      
      if(any(weight_valid_i)){
        weight_raw_i[!weight_valid_i] <- median(weight_raw_i[weight_valid_i], na.rm = TRUE)
        weight_cap_i <- as.numeric(quantile(weight_raw_i, 0.95, na.rm = TRUE))
        train_draw_age_i$weight_model <- pmin(weight_raw_i, weight_cap_i)
      } else {
        train_draw_age_i$weight_model <- 1
      }
      
      boot_train_age_i <- train_draw_age_i %>%
        inner_join(boot_country_count_i, by = "Country") %>%
        mutate(weight_model_boot = weight_model * bootstrap_count) %>%
        filter(is.finite(weight_model_boot), weight_model_boot > 0)
      
      if(nrow(boot_train_age_i) < 10){
        draw_message_i[draw_id_i, j] <- "Too few bootstrapped training rows"
        next
      }
      
      
      # Matching target grid for the assigned PC imputation
      prediction_grid_i <- pc_dat_i %>%
        mutate(age_group = age_group_i, Endemic = as.numeric(Endemic)) %>%
        left_join(
          age_population_long_i %>%
            filter(age_group == age_group_i) %>%
            dplyr::select(Country, Year, age_group, age_population),
          by = c("Country", "Year", "age_group")
        ) %>%
        filter(
          is.finite(age_population), age_population > 0,
          is.finite(PC1), is.finite(PC2),
          is.finite(Endemic)
        ) %>%
        arrange(Country, Year)
      
      reference_key_i <- prediction_grid_base_list_i[[j]] %>% dplyr::select(Country, Year)
      prediction_key_i <- prediction_grid_i %>% dplyr::select(Country, Year)
      
      if(!identical(reference_key_i, prediction_key_i)){
        stop("Prediction-grid Country-Year ordering differs across PC imputations.")
      }
      
      
      # Fit/predict selected model --------------------------------------------
      
      predicted_link_i <- rep(NA_real_, nrow(prediction_grid_i))
      selected_model_error_i <- NA_character_
      
      
      if(selected_model_key_i == "lm"){
        
        fit_selected_i <- tryCatch(
          lm(logit_value ~ PC1 + PC2 + Endemic, data = boot_train_age_i, weights = weight_model_boot),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, newdata = prediction_grid_i)),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i == "spline_lm"){
        
        fit_selected_i <- tryCatch(
          lm(
            logit_value ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3) + Endemic,
            data = boot_train_age_i,
            weights = weight_model_boot
          ),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, newdata = prediction_grid_i)),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i == "gam"){
        
        fit_selected_i <- tryCatch(
          mgcv::gam(
            logit_value ~ s(PC1, k = 4) + s(PC2, k = 4) + Endemic,
            data = boot_train_age_i,
            weights = weight_model_boot,
            method = "REML"
          ),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, newdata = prediction_grid_i, type = "response")),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i %in% c("rf_balanced", "rf_small_nodes", "extra_trees")){
        
        if(selected_model_key_i == "rf_balanced"){
          num_trees_i <- 1000
          mtry_i <- 2
          min_node_i <- 5
          splitrule_i <- "variance"
        }
        
        if(selected_model_key_i == "rf_small_nodes"){
          num_trees_i <- 1000
          mtry_i <- 3
          min_node_i <- 2
          splitrule_i <- "variance"
        }
        
        if(selected_model_key_i == "extra_trees"){
          num_trees_i <- 1500
          mtry_i <- 3
          min_node_i <- 3
          splitrule_i <- "extratrees"
        }
        
        fit_selected_i <- tryCatch(
          ranger::ranger(
            logit_value ~ PC1 + PC2 + Endemic,
            data = boot_train_age_i,
            case.weights = boot_train_age_i$weight_model_boot,
            num.trees = num_trees_i,
            mtry = mtry_i,
            min.node.size = min_node_i,
            splitrule = splitrule_i,
            seed = seed_base_i + draw_id_i * 10 + j
          ),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, data = prediction_grid_i)$predictions),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i %in% names(glmnet_alpha_i)){
        
        combined_matrix_i <- bind_rows(
          boot_train_age_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
          prediction_grid_i %>% transmute(PC1, PC2, Endemic, .set = "prediction")
        )
        
        x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
        x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
        x_prediction_i <- x_all_i[combined_matrix_i$.set == "prediction", , drop = FALSE]
        
        nfolds_i <- min(5, nrow(boot_train_age_i))
        foldid_i <- ((seq_len(nrow(boot_train_age_i)) - 1) %% nfolds_i) + 1
        
        fit_selected_i <- tryCatch(
          glmnet::cv.glmnet(
            x = x_train_i,
            y = boot_train_age_i$logit_value,
            weights = boot_train_age_i$weight_model_boot,
            alpha = glmnet_alpha_i[selected_model_key_i],
            family = "gaussian",
            nfolds = nfolds_i,
            foldid = foldid_i
          ),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, newx = x_prediction_i, s = "lambda.min")),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i == "svm_radial"){
        
        combined_matrix_i <- bind_rows(
          boot_train_age_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
          prediction_grid_i %>% transmute(PC1, PC2, Endemic, .set = "prediction")
        )
        
        x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
        x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
        x_prediction_i <- x_all_i[combined_matrix_i$.set == "prediction", , drop = FALSE]
        
        fit_selected_i <- tryCatch(
          kernlab::ksvm(
            x = x_train_i,
            y = boot_train_age_i$logit_value,
            type = "eps-svr",
            kernel = "rbfdot",
            kpar = "automatic",
            C = 2,
            scaled = TRUE
          ),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(kernlab::predict(fit_selected_i, x_prediction_i)),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
        
        
      } else if(selected_model_key_i %in% c("xgb_smooth", "xgb_balanced", "xgb_deeper")){
        
        combined_matrix_i <- bind_rows(
          boot_train_age_i %>% transmute(PC1, PC2, Endemic, .set = "train"),
          prediction_grid_i %>% transmute(PC1, PC2, Endemic, .set = "prediction")
        )
        
        x_all_i <- model.matrix(~ PC1 + PC2 + Endemic, data = combined_matrix_i)[, -1, drop = FALSE]
        x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
        x_prediction_i <- x_all_i[combined_matrix_i$.set == "prediction", , drop = FALSE]
        
        if(selected_model_key_i == "xgb_smooth"){
          xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 2, eta = 0.02, subsample = 0.85, colsample_bytree = 0.85, min_child_weight = 5, seed = seed_base_i + draw_id_i * 10 + j)
          nrounds_i <- 500
        }
        
        if(selected_model_key_i == "xgb_balanced"){
          xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 3, eta = 0.03, subsample = 0.80, colsample_bytree = 0.80, min_child_weight = 3, seed = seed_base_i + draw_id_i * 10 + j)
          nrounds_i <- 300
        }
        
        if(selected_model_key_i == "xgb_deeper"){
          xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 4, eta = 0.02, subsample = 0.75, colsample_bytree = 0.75, min_child_weight = 2, seed = seed_base_i + draw_id_i * 10 + j)
          nrounds_i <- 450
        }
        
        dtrain_i <- xgboost::xgb.DMatrix(data = x_train_i, label = boot_train_age_i$logit_value, weight = boot_train_age_i$weight_model_boot)
        
        fit_selected_i <- tryCatch(
          xgboost::xgb.train(params = xgb_params_i, data = dtrain_i, nrounds = nrounds_i, verbose = 0),
          error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
        )
        
        if(!is.null(fit_selected_i)){
          predicted_link_i <- tryCatch(
            as.numeric(predict(fit_selected_i, newdata = x_prediction_i)),
            error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
          )
        }
      }
      
      
      if(length(predicted_link_i) != nrow(prediction_grid_i) || any(!is.finite(predicted_link_i))){
        draw_message_i[draw_id_i, j] <- paste0(selected_model_label_i, " fit/prediction failed: ", selected_model_error_i)
        next
      }
      
      projection_matrix_list_i[[j]][, draw_id_i] <- plogis(predicted_link_i)
      draw_success_i[draw_id_i, j] <- TRUE
      draw_message_i[draw_id_i, j] <- "success"
    }
    
    
    if(draw_id_i %% 25 == 0){
      message(
        outcome_i,
        ": completed draw ",
        draw_id_i,
        " / ",
        n_draw_i
      )
    }
  }
  
  
  
  # 8-9. Draw-balance check ---------------------------------------------------
  
  successful_draw_check_i <- bind_rows(
    lapply(1:3, function(j){
      
      tibble(
        draw_id = seq_len(n_draw_i),
        age_group = age_group_list_i[j],
        success = draw_success_i[, j]
      ) %>%
        left_join(
          draw_design_i %>%
            dplyr::select(draw_id, .imp_pc, .imp_dhs),
          by = "draw_id"
        ) %>%
        group_by(.imp_pc, .imp_dhs, age_group) %>%
        summarise(n_successful_draws = sum(success), .groups = "drop")
    })
  )
  
  print(successful_draw_check_i, n = Inf)
  
  write.csv(
    successful_draw_check_i,
    file.path(selected_model_uncertainty_dir_i, paste0(outcome_i, "_", selected_model_key_i, "_draw_balance_check.csv")),
    row.names = FALSE
  )
  
  if(any(successful_draw_check_i$n_successful_draws != 40)){
    stop(
      outcome_i,
      ": not all PC x DHS x age cells contain exactly 40 successful uncertainty draws."
    )
  }
  
  
  
  # 8-10. Summarize each age-specific result ----------------------------------
  
  for(j in 1:3){
    
    spec_name_i <- spec_names_i[j]
    age_group_i <- age_group_list_i[j]
    projection_matrix_i <- projection_matrix_list_i[[j]]
    
    prediction_grid_i <- prediction_grid_base_list_i[[j]] %>%
      mutate(
        outcome = outcome_i,
        age_group = age_group_i
      )
    
    
    n_successful_draws_i <- rowSums(is.finite(projection_matrix_i))
    
    predicted_mean_i <- rowMeans(projection_matrix_i, na.rm = TRUE)
    predicted_mean_i[n_successful_draws_i == 0] <- NA_real_
    
    predicted_median_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) median(x, na.rm = TRUE) else NA_real_)
    predicted_lcl_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.025, na.rm = TRUE)) else NA_real_)
    predicted_ucl_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.975, na.rm = TRUE)) else NA_real_)
    
    
    # Wide Country-Year x 1000 draw object
    projection_draw_df_i <- as.data.frame(projection_matrix_i)
    
    country_year_draws_i <- bind_cols(
      prediction_grid_i %>%
        transmute(
          outcome,
          age_group,
          Country,
          Year,
          gbd_region,
          Population_under5,
          age_population
        ),
      projection_draw_df_i
    )
    
    
    # Long draw object retaining PC/DHS provenance
    country_year_draws_long_i <- country_year_draws_i %>%
      pivot_longer(
        cols = starts_with("draw_"),
        names_to = "draw_name",
        values_to = "pred_proportion"
      ) %>%
      mutate(
        draw_id = as.integer(sub("^draw_", "", draw_name))
      ) %>%
      dplyr::select(-draw_name) %>%
      left_join(
        draw_design_i %>%
          dplyr::select(draw_id, .draw_within_combo, .imp_pc, .imp_dhs),
        by = "draw_id"
      ) %>%
      relocate(
        draw_id,
        .draw_within_combo,
        .imp_pc,
        .imp_dhs,
        outcome,
        age_group,
        Country,
        Year
      )
    
    
    country_year_summary_i <- prediction_grid_i %>%
      transmute(
        outcome,
        age_group,
        Country,
        Year,
        gbd_region,
        Population_under5,
        age_population,
        model_key = selected_model_key_i,
        model = selected_model_label_i,
        alpha = selected_alpha_i,
        predicted_proportion_mean = predicted_mean_i,
        predicted_proportion_median = predicted_median_i,
        predicted_proportion_lcl = predicted_lcl_i,
        predicted_proportion_ucl = predicted_ucl_i,
        n_successful_draws = n_successful_draws_i
      )
    
    
    # Year-level summaries
    year_levels_i <- sort(unique(prediction_grid_i$Year))
    year_draw_matrix_i <- matrix(NA_real_, nrow = length(year_levels_i), ncol = n_draw_i)
    
    for(year_index_i in seq_along(year_levels_i)){
      
      year_i <- year_levels_i[year_index_i]
      row_index_i <- which(prediction_grid_i$Year == year_i)
      
      prediction_submatrix_i <- projection_matrix_i[row_index_i, , drop = FALSE]
      population_weight_i <- prediction_grid_i$age_population[row_index_i]
      
      weighted_numerator_i <- colSums(prediction_submatrix_i * population_weight_i, na.rm = TRUE)
      weighted_denominator_i <- colSums(is.finite(prediction_submatrix_i) * population_weight_i, na.rm = TRUE)
      
      year_draw_matrix_i[year_index_i, ] <- ifelse(
        weighted_denominator_i > 0,
        weighted_numerator_i / weighted_denominator_i,
        NA_real_
      )
    }
    
    year_summary_i <- tibble(
      outcome = outcome_i,
      age_group = age_group_i,
      Year = year_levels_i,
      model_key = selected_model_key_i,
      model = selected_model_label_i,
      alpha = selected_alpha_i,
      mean_proportion = apply(year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_),
      median_proportion = apply(year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) median(x, na.rm = TRUE) else NA_real_),
      proportion_lcl = apply(year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.025, na.rm = TRUE)) else NA_real_),
      proportion_ucl = apply(year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.975, na.rm = TRUE)) else NA_real_),
      total_age_population = vapply(year_levels_i, function(x) sum(prediction_grid_i$age_population[prediction_grid_i$Year == x], na.rm = TRUE), numeric(1)),
      n_country = vapply(year_levels_i, function(x) n_distinct(prediction_grid_i$Country[prediction_grid_i$Year == x]), integer(1)),
      n_successful_draws = rowSums(is.finite(year_draw_matrix_i))
    )
    
    
    # Region-year summaries
    region_year_keys_i <- prediction_grid_i %>% distinct(Year, gbd_region) %>% arrange(Year, gbd_region)
    region_year_draw_matrix_i <- matrix(NA_real_, nrow = nrow(region_year_keys_i), ncol = n_draw_i)
    
    for(region_year_index_i in seq_len(nrow(region_year_keys_i))){
      
      year_i <- region_year_keys_i$Year[region_year_index_i]
      region_i <- region_year_keys_i$gbd_region[region_year_index_i]
      
      row_index_i <- which(
        prediction_grid_i$Year == year_i &
          prediction_grid_i$gbd_region == region_i
      )
      
      prediction_submatrix_i <- projection_matrix_i[row_index_i, , drop = FALSE]
      population_weight_i <- prediction_grid_i$age_population[row_index_i]
      
      weighted_numerator_i <- colSums(prediction_submatrix_i * population_weight_i, na.rm = TRUE)
      weighted_denominator_i <- colSums(is.finite(prediction_submatrix_i) * population_weight_i, na.rm = TRUE)
      
      region_year_draw_matrix_i[region_year_index_i, ] <- ifelse(
        weighted_denominator_i > 0,
        weighted_numerator_i / weighted_denominator_i,
        NA_real_
      )
    }
    
    region_year_summary_i <- region_year_keys_i %>%
      mutate(
        outcome = outcome_i,
        age_group = age_group_i,
        model_key = selected_model_key_i,
        model = selected_model_label_i,
        alpha = selected_alpha_i,
        mean_proportion = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_),
        median_proportion = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) median(x, na.rm = TRUE) else NA_real_),
        proportion_lcl = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.025, na.rm = TRUE)) else NA_real_),
        proportion_ucl = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.975, na.rm = TRUE)) else NA_real_),
        total_age_population = vapply(seq_len(nrow(region_year_keys_i)), function(k){
          sum(
            prediction_grid_i$age_population[
              prediction_grid_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_i$gbd_region == region_year_keys_i$gbd_region[k]
            ],
            na.rm = TRUE
          )
        }, numeric(1)),
        n_country = vapply(seq_len(nrow(region_year_keys_i)), function(k){
          n_distinct(
            prediction_grid_i$Country[
              prediction_grid_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_i$gbd_region == region_year_keys_i$gbd_region[k]
            ]
          )
        }, integer(1)),
        n_successful_draws = rowSums(is.finite(region_year_draw_matrix_i))
      ) %>%
      relocate(outcome, age_group, Year, gbd_region)
    
    
    # Store result
    selected_model_uncertainty_result_i <- list(
      outcome = outcome_i,
      age_group = age_group_i,
      model_key = selected_model_key_i,
      model = selected_model_label_i,
      alpha = selected_alpha_i,
      n_draw_requested = n_draw_i,
      draw_design = draw_design_i,
      prediction_grid = prediction_grid_i,
      projection_draw_matrix = projection_matrix_i,
      country_year_draws = country_year_draws_i,
      country_year_draws_long = country_year_draws_long_i,
      country_year_summary = country_year_summary_i,
      year_draw_matrix = year_draw_matrix_i,
      year_summary = year_summary_i,
      region_year_draw_matrix = region_year_draw_matrix_i,
      region_year_summary = region_year_summary_i,
      draw_success = draw_success_i[, j],
      draw_message = draw_message_i[, j]
    )
    
    selected_model_uncertainty_result_list_i[[spec_name_i]] <- selected_model_uncertainty_result_i
    
    
    # Save individual specification
    saveRDS(
      selected_model_uncertainty_result_i,
      file.path(
        selected_model_uncertainty_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_uncertainty.rds")
      )
    )
    
    saveRDS(
      country_year_draws_long_i,
      file.path(
        selected_model_uncertainty_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_projection_draws_long.rds")
      )
    )
    
    write.csv(
      country_year_summary_i,
      file.path(
        selected_model_uncertainty_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_country_year_summary.csv")
      ),
      row.names = FALSE
    )
    
    write.csv(
      year_summary_i,
      file.path(
        selected_model_uncertainty_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_year_summary.csv")
      ),
      row.names = FALSE
    )
    
    write.csv(
      region_year_summary_i,
      file.path(
        selected_model_uncertainty_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_region_year_summary.csv")
      ),
      row.names = FALSE
    )
    
    message(
      "Saved ", selected_model_label_i,
      " uncertainty result: ", spec_name_i,
      " | successful draws: ", sum(draw_success_i[, j]),
      " / ", n_draw_i
    )
  }
}



# 8-11. Save all six results ---------------------------------------------------

saveRDS(
  selected_model_uncertainty_result_list_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_all_age_specific_selected_model_uncertainty_results.rds"
  )
)


country_year_summary_all_i <- bind_rows(
  lapply(selected_model_uncertainty_result_list_i, `[[`, "country_year_summary")
)

country_year_draws_long_all_i <- bind_rows(
  lapply(selected_model_uncertainty_result_list_i, `[[`, "country_year_draws_long")
)

year_summary_all_i <- bind_rows(
  lapply(selected_model_uncertainty_result_list_i, `[[`, "year_summary")
)

region_year_summary_all_i <- bind_rows(
  lapply(selected_model_uncertainty_result_list_i, `[[`, "region_year_summary")
)


saveRDS(
  country_year_draws_long_all_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_all_age_specific_selected_model_projection_draws_long.rds"
  )
)

saveRDS(
  country_year_summary_all_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_all_age_specific_selected_model_country_year_summary.rds"
  )
)

saveRDS(
  year_summary_all_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_all_age_specific_selected_model_year_summary.rds"
  )
)

saveRDS(
  region_year_summary_all_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_all_age_specific_selected_model_region_year_summary.rds"
  )
)




# 8-12. Selected-model holdout validation summaries --------------------------

if(holdout_result_index_i == 0){
  stop("No successful antibiotic-access holdout-validation draws were generated.")
}

selected_model_holdout_draws_i <- bind_rows(
  selected_model_holdout_result_list_i[seq_len(holdout_result_index_i)]
) %>%
  filter(
    is.finite(heldout_estimated_logit),
    is.finite(predicted_logit),
    is.finite(heldout_estimated_proportion),
    is.finite(predicted_proportion)
  )


selected_model_holdout_country_year_summary_i <- selected_model_holdout_draws_i %>%
  group_by(
    outcome,
    age_group,
    Country,
    Year,
    CountryYear,
    gbd_region,
    model_key,
    model,
    alpha
  ) %>%
  summarise(
    estimated_proportion_mean = mean(heldout_estimated_proportion, na.rm = TRUE),
    estimated_proportion_median = median(heldout_estimated_proportion, na.rm = TRUE),
    estimated_proportion_lcl = as.numeric(
      quantile(heldout_estimated_proportion, 0.025, na.rm = TRUE)
    ),
    estimated_proportion_ucl = as.numeric(
      quantile(heldout_estimated_proportion, 0.975, na.rm = TRUE)
    ),
    predicted_proportion_mean = mean(predicted_proportion, na.rm = TRUE),
    predicted_proportion_median = median(predicted_proportion, na.rm = TRUE),
    predicted_proportion_lcl = as.numeric(
      quantile(predicted_proportion, 0.025, na.rm = TRUE)
    ),
    predicted_proportion_ucl = as.numeric(
      quantile(predicted_proportion, 0.975, na.rm = TRUE)
    ),
    residual_proportion_mean = mean(residual_proportion, na.rm = TRUE),
    residual_logit_mean = mean(residual_logit, na.rm = TRUE),
    n_holdout_draws = n_distinct(draw_id),
    .groups = "drop"
  )


selected_model_holdout_draw_metrics_i <- selected_model_holdout_draws_i %>%
  group_by(
    draw_id,
    .draw_within_combo,
    .imp_pc,
    .imp_dhs,
    outcome,
    age_group,
    model_key,
    model,
    alpha
  ) %>%
  summarise(
    rmse_logit = sqrt(
      mean((predicted_logit - heldout_estimated_logit)^2, na.rm = TRUE)
    ),
    mae_logit = mean(
      abs(predicted_logit - heldout_estimated_logit),
      na.rm = TRUE
    ),
    bias_logit = mean(
      predicted_logit - heldout_estimated_logit,
      na.rm = TRUE
    ),
    rmse_proportion = sqrt(
      mean((predicted_proportion - heldout_estimated_proportion)^2, na.rm = TRUE)
    ),
    mae_proportion = mean(
      abs(predicted_proportion - heldout_estimated_proportion),
      na.rm = TRUE
    ),
    bias_proportion = mean(
      predicted_proportion - heldout_estimated_proportion,
      na.rm = TRUE
    ),
    cor_proportion = if(
      n() >= 2 &&
      sd(heldout_estimated_proportion, na.rm = TRUE) > 0 &&
      sd(predicted_proportion, na.rm = TRUE) > 0
    ){
      cor(
        heldout_estimated_proportion,
        predicted_proportion,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    n_eval = n(),
    n_holdout_country = n_distinct(Country),
    .groups = "drop"
  )


selected_model_holdout_metric_summary_i <- selected_model_holdout_draw_metrics_i %>%
  group_by(
    outcome,
    age_group,
    model_key,
    model,
    alpha
  ) %>%
  summarise(
    mean_rmse_logit = mean(rmse_logit, na.rm = TRUE),
    median_rmse_logit = median(rmse_logit, na.rm = TRUE),
    rmse_logit_lcl = as.numeric(quantile(rmse_logit, 0.025, na.rm = TRUE)),
    rmse_logit_ucl = as.numeric(quantile(rmse_logit, 0.975, na.rm = TRUE)),
    mean_mae_logit = mean(mae_logit, na.rm = TRUE),
    mean_bias_logit = mean(bias_logit, na.rm = TRUE),
    mean_abs_bias_logit = mean(abs(bias_logit), na.rm = TRUE),
    mean_rmse_proportion = mean(rmse_proportion, na.rm = TRUE),
    median_rmse_proportion = median(rmse_proportion, na.rm = TRUE),
    rmse_proportion_lcl = as.numeric(
      quantile(rmse_proportion, 0.025, na.rm = TRUE)
    ),
    rmse_proportion_ucl = as.numeric(
      quantile(rmse_proportion, 0.975, na.rm = TRUE)
    ),
    mean_mae_proportion = mean(mae_proportion, na.rm = TRUE),
    mean_bias_proportion = mean(bias_proportion, na.rm = TRUE),
    mean_cor_proportion = mean(cor_proportion, na.rm = TRUE),
    n_successful_validation_draws = n_distinct(draw_id),
    .groups = "drop"
  )


holdout_draw_balance_i <- selected_model_holdout_draws_i %>%
  distinct(
    draw_id,
    .draw_within_combo,
    .imp_pc,
    .imp_dhs,
    outcome,
    age_group
  ) %>%
  count(
    outcome,
    age_group,
    .imp_pc,
    .imp_dhs,
    name = "n_successful_validation_draws"
  )

print(holdout_draw_balance_i, n = Inf)
print(selected_model_holdout_metric_summary_i, n = Inf)

if(any(holdout_draw_balance_i$n_successful_validation_draws != 40)){
  warning(
    "Some PC x DHS x age holdout-validation cells contain fewer than 40 successful draws. ",
    "Inspect AA_selected_model_holdout_draw_balance.csv before interpreting the validation plot."
  )
}


saveRDS(
  selected_model_holdout_draws_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_validation_draws.rds"
  )
)

saveRDS(
  selected_model_holdout_country_year_summary_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_country_year_summary.rds"
  )
)

saveRDS(
  selected_model_holdout_draw_metrics_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_draw_metrics.rds"
  )
)

saveRDS(
  selected_model_holdout_metric_summary_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_metric_summary.rds"
  )
)

saveRDS(
  holdout_draw_balance_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_draw_balance.rds"
  )
)


write.csv(
  selected_model_holdout_country_year_summary_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_country_year_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  selected_model_holdout_draw_metrics_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_draw_metrics.csv"
  ),
  row.names = FALSE
)

write.csv(
  selected_model_holdout_metric_summary_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_metric_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  holdout_draw_balance_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_draw_balance.csv"
  ),
  row.names = FALSE
)



# 8-13. Holdout validation estimated-versus-predicted plot ---------------------
out_dir <- "outputs/pipelines"
selected_model_uncertainty_dir_i <- file.path(out_dir, "AA_age_specific_selected_model_uncertainty")

selected_model_holdout_draws_i <- readRDS(
  file.path(selected_model_uncertainty_dir_i, "AA_selected_model_holdout_validation_draws.rds")
)

age_group_list_i <- c("0-11 months", "12-23 months", "24-59 months")

validation_fig_dir_i <- "figures/main_analysis/AA_validation_predictions"
dir.create(validation_fig_dir_i, recursive = TRUE, showWarnings = FALSE)

point_col_i <- adjustcolor("#4D4D4D", alpha.f = 0.18)

pdf(file.path(validation_fig_dir_i, "AA_selected_model_holdout_validation_draw_scatter.pdf"),
    width = 4.5, height = 6.0, useDingbats = FALSE)

old_par_i <- par(no.readonly = TRUE)

layout(matrix(c(0, 0, 0, 
                1, 0, 4, 
                2, 0, 5, 
                3, 0, 6), nrow = 4, byrow = TRUE), widths = c(1,0.1,1), heights = c(0.15,1,1,1))

par(mar = c(6.0, 6.5, 1.0, 1.0), mgp = c(1.8, 0.5, 0), tcl = -0.25,
    cex.axis = 1.0, cex.lab = 1.0, cex.main = 1.0)

axis_at_i <- seq(0, 1, by = 0.25)
axis_labels_i <- format(axis_at_i, nsmall = 2)

for(outcome_i in c("LRTI", "fever")){
  
  plot_outcome_i <- selected_model_holdout_draws_i %>%
    filter(outcome == outcome_i, is.finite(heldout_estimated_proportion), is.finite(predicted_proportion))
  
  for(j in 1:3){
    
    age_group_i <- age_group_list_i[j]
    
    plot_data_i <- plot_outcome_i %>%
      filter(age_group == age_group_i)
    
    plot(
      plot_data_i$heldout_estimated_proportion,
      plot_data_i$predicted_proportion,
      xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
      axes = FALSE, xlab = "", ylab = "",
      pch = 16, cex = 0.6, col = point_col_i
    )
    
    panel_letter_i <- if(outcome_i == "LRTI") c("a", "c", "e")[j] else c("b", "d", "f")[j]
    usr <- par("usr")
    
    mtext(panel_letter_i, side = 3, line = 2.0, at = usr[1] - 0.71 * diff(usr[1:2]), adj = 0.1, font = 2, cex = 0.75)
    
    axis(1, at = axis_at_i, labels = axis_labels_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
    axis(2, at = axis_at_i, labels = axis_labels_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
    
    if(outcome_i == "LRTI"){
      mtext("Held-out predicted\nantibiotic treatment\nprobability", side = 2, line = 3.0, cex = 0.8)
    }
    
    if(j == 3){
      mtext("Held-out estimated\nantibiotic treatment\nprobability", side = 1, line = 4.8, cex = 0.8)
    }
    
    abline(a = 0, b = 1, lty = 1, lwd = 0.8, col = "darkred")
  }
}

par(old_par_i)
dev.off()


holdout_validation_results_i <- list(
  LRTI = list(
    model_key = selected_model_key_LRTI_i,
    model = unname(model_labels_i[selected_model_key_LRTI_i]),
    alpha = if(
      selected_model_key_LRTI_i %in% names(glmnet_alpha_i)
    ){
      unname(glmnet_alpha_i[selected_model_key_LRTI_i])
    } else {
      NA_real_
    }
  ),
  fever = list(
    model_key = selected_model_key_fever_i,
    model = unname(model_labels_i[selected_model_key_fever_i]),
    alpha = if(
      selected_model_key_fever_i %in% names(glmnet_alpha_i)
    ){
      unname(glmnet_alpha_i[selected_model_key_fever_i])
    } else {
      NA_real_
    }
  ),
  country_year_summary = selected_model_holdout_country_year_summary_i,
  draw_metrics = selected_model_holdout_draw_metrics_i,
  metric_summary = selected_model_holdout_metric_summary_i,
  draw_balance = holdout_draw_balance_i,
  holdout_draws_file = file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_validation_draws.rds"
  ),
  validation_plot_file = validation_plot_file_i
)

saveRDS(
  holdout_validation_results_i,
  file.path(
    selected_model_uncertainty_dir_i,
    "AA_selected_model_holdout_validation_results.rds"
  )
)

message("Saved antibiotic-access holdout validation plot to: ", validation_plot_file_i)



# 8-14. Final checks -----------------------------------------------------------

successful_draw_check_all_i <- bind_rows(
  lapply(names(selected_model_uncertainty_result_list_i), function(spec_name_i){
    
    result_i <- selected_model_uncertainty_result_list_i[[spec_name_i]]
    
    tibble(
      specification = spec_name_i,
      outcome = result_i$outcome,
      age_group = result_i$age_group,
      model_key = result_i$model_key,
      n_successful_draws = sum(result_i$draw_success),
      n_requested_draws = result_i$n_draw_requested
    )
  })
)

print(successful_draw_check_all_i)

stopifnot(all(successful_draw_check_all_i$n_successful_draws == 1000))

saveRDS(
  successful_draw_check_all_i,
  file.path(selected_model_uncertainty_dir_i, "AA_successful_draw_check.rds")
)

write.csv(
  successful_draw_check_all_i,
  file.path(selected_model_uncertainty_dir_i, "AA_successful_draw_check.csv"),
  row.names = FALSE
)


cat(
  "\nFinal antibiotic-access uncertainty design:\n",
  "LRTI common model = ", selected_model_key_LRTI_i,
  "\nFever common model = ", selected_model_key_fever_i,
  "\nPC imputations = ", length(unique(draw_design_i$.imp_pc)),
  "\nDHS imputations = ", length(unique(draw_design_i$.imp_dhs)),
  "\nConditional draws per PC x DHS combination = 40",
  "\nTotal draws = ", n_draw_i,
  "\n",
  sep = ""
)

message(
  "Section 8 completed. Outputs saved to: ",
  normalizePath(selected_model_uncertainty_dir_i, mustWork = FALSE)
)