# _____________________________________________ ----------
# 6. Counterfactual wealth-quintile prediction and country-year aggregation ----

rm(list = ls()); gc()

library(dplyr)
library(tidyr)
library(mice)
library(glmmTMB)
library(purrr)
library(tibble)
library(openxlsx)


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

wealth_levels <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_tags <- c("q1", "q2", "q3", "q4", "q5")

stopifnot(
  "hh_wealth" %in% names(completed_data_LRTI[[1]]),
  "hh_wealth" %in% names(completed_data_fever[[1]]),
  !"hh_wealth_z" %in% names(completed_data_LRTI[[1]]),
  !"hh_wealth_z" %in% names(completed_data_fever[[1]]),
  is.factor(completed_data_LRTI[[1]]$hh_wealth),
  is.factor(completed_data_fever[[1]]$hh_wealth),
  identical(levels(completed_data_LRTI[[1]]$hh_wealth), wealth_levels),
  identical(levels(completed_data_fever[[1]]$hh_wealth), wealth_levels),
  any(grepl("^hh_wealth", names(glmmTMB::fixef(fit_LRTI[[1]])$cond))),
  any(grepl("^hh_wealth", names(glmmTMB::fixef(fit_fever[[1]])$cond)))
)

wealth_scenarios <- tibble(
  wealth_level = factor(wealth_levels, levels = wealth_levels),
  wealth_tag = wealth_tags
)


## 1. Prepare completed datasets ----------------------------------------------

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


## 2. Counterfactual wealth prediction ----------------------------------------

completed_data_LRTI_wealth_prepared <- purrr::map(completed_data_LRTI_prepared, \(dat) {
  dat_observed <- dat %>% rename(hh_wealth_observed = hh_wealth)
  tidyr::expand_grid(dat_observed, wealth_scenarios) %>%
    mutate(hh_wealth = factor(as.character(wealth_level), levels = wealth_levels))
})

completed_data_fever_wealth_prepared <- purrr::map(completed_data_fever_prepared, \(dat) {
  dat_observed <- dat %>% rename(hh_wealth_observed = hh_wealth)
  tidyr::expand_grid(dat_observed, wealth_scenarios) %>%
    mutate(hh_wealth = factor(as.character(wealth_level), levels = wealth_levels))
})


completed_data_LRTI_wealth_pred <- purrr::map2(completed_data_LRTI_wealth_prepared, fit_LRTI, \(dat, fit) {
  dat$pred_proportion <- predict(fit, newdata = dat, type = "response", re.form = NA, allow.new.levels = TRUE)
  dat
})

completed_data_fever_wealth_pred <- purrr::map2(completed_data_fever_wealth_prepared, fit_fever, \(dat, fit) {
  dat$pred_proportion <- predict(fit, newdata = dat, type = "response", re.form = NA, allow.new.levels = TRUE)
  dat
})


## 3. DHS-imputation-specific country-year-age estimates ----------------------

country_year_LRTI_wealth_imp <- purrr::map_dfr(completed_data_LRTI_wealth_pred, \(dat) {
  dat %>%
    filter(!is.na(Ab_treatment), !is.na(sample_weight), sample_weight > 0, !is.na(pred_proportion)) %>%
    group_by(.imp_dhs, outcome, wealth_level, wealth_tag, age_group, CountryYear, Country, Year, gbd_region) %>%
    summarise(
      n_child = n(),
      weight_sum = sum(sample_weight, na.rm = TRUE),
      observed_proportion = weighted.mean(Ab_treatment, sample_weight, na.rm = TRUE),
      pred_proportion = weighted.mean(pred_proportion, sample_weight, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(wealth_level = factor(as.character(wealth_level), levels = wealth_levels), Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(.imp_dhs, Country, Year, CountryYear, outcome, wealth_level, wealth_tag, age_group, gbd_region)


country_year_fever_wealth_imp <- purrr::map_dfr(completed_data_fever_wealth_pred, \(dat) {
  dat %>%
    filter(!is.na(Ab_treatment), !is.na(sample_weight), sample_weight > 0, !is.na(pred_proportion)) %>%
    group_by(.imp_dhs, outcome, wealth_level, wealth_tag, age_group, CountryYear, Country, Year, gbd_region) %>%
    summarise(
      n_child = n(),
      weight_sum = sum(sample_weight, na.rm = TRUE),
      observed_proportion = weighted.mean(Ab_treatment, sample_weight, na.rm = TRUE),
      pred_proportion = weighted.mean(pred_proportion, sample_weight, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(wealth_level = factor(as.character(wealth_level), levels = wealth_levels), Year = as.integer(sub(".*_(\\d{4})$", "\\1", CountryYear))) %>%
  relocate(.imp_dhs, Country, Year, CountryYear, outcome, wealth_level, wealth_tag, age_group, gbd_region)


stopifnot(
  n_distinct(country_year_LRTI_wealth_imp$.imp_dhs) == 5,
  n_distinct(country_year_fever_wealth_imp$.imp_dhs) == 5
)

print(country_year_LRTI_wealth_imp %>% count(.imp_dhs))
print(country_year_fever_wealth_imp %>% count(.imp_dhs))


## 4. Descriptive pooled estimates across five DHS imputations ----------------

country_year_LRTI_wealth <- country_year_LRTI_wealth_imp %>%
  group_by(outcome, wealth_level, wealth_tag, age_group, CountryYear, Country, Year, gbd_region) %>%
  summarise(
    n_imputations = n_distinct(.imp_dhs),
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_proportion_mean = mean(observed_proportion, na.rm = TRUE),
    pred_proportion_mean = mean(pred_proportion, na.rm = TRUE),
    pred_proportion_between_imp_sd = sd(pred_proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(wealth_level = factor(as.character(wealth_level), levels = wealth_levels)) %>%
  relocate(Country, Year, CountryYear, outcome, wealth_level, wealth_tag, age_group, gbd_region)


country_year_fever_wealth <- country_year_fever_wealth_imp %>%
  group_by(outcome, wealth_level, wealth_tag, age_group, CountryYear, Country, Year, gbd_region) %>%
  summarise(
    n_imputations = n_distinct(.imp_dhs),
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_proportion_mean = mean(observed_proportion, na.rm = TRUE),
    pred_proportion_mean = mean(pred_proportion, na.rm = TRUE),
    pred_proportion_between_imp_sd = sd(pred_proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(wealth_level = factor(as.character(wealth_level), levels = wealth_levels)) %>%
  relocate(Country, Year, CountryYear, outcome, wealth_level, wealth_tag, age_group, gbd_region)



## 5. Add GDP, population, and income -----------------------------------------

GDP.PPP <- read.csv("data/metadata/GDP_PPP.csv")
colnames(GDP.PPP)[-1] <- gsub("^X", "", colnames(GDP.PPP)[-1])

GDP.PPP <- GDP.PPP %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.numeric(Year))

Population <- read.csv("data/metadata/Under_5.csv") %>%
  dplyr::select(Entity, Year, Population) %>%
  rename(Country = Entity)


GDP.PPP <- GDP.PPP %>%
  mutate(Country = dplyr::recode(
    Country,
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Congo, Rep." = "Republic of the Congo",
    "Gambia, The" = "Gambia",
    "Egypt" = "Egypt, Arab Rep.",
    "Yemen, Rep." = "Yemen",
    .default = Country
  ))

Population <- Population %>%
  mutate(Country = dplyr::recode(
    Country,
    "Democratic Republic of Congo" = "Democratic Republic of the Congo",
    "Congo" = "Republic of the Congo",
    "Egypt" = "Egypt, Arab Rep.",
    "Yemen, Rep." = "Yemen",
    .default = Country
  ))


manual_country_year_values <- tibble::tribble(
  ~CountryYear, ~Population_manual, ~GDP_manual,
  "Haiti_2012", 1243438, 3286,
  "Maldives_2009", 30631, 16163,
  "Maldives_2016", 36815, 19264,
  "Maldives_2017", 36199, 19948,
  "Yemen_2013", 4616819, 3665
)


add_gdp_population <- function(dat) {
  dat %>%
    mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
    left_join(Population %>% mutate(Country = as.character(Country), Year = as.numeric(Year)) %>% dplyr::select(Country, Year, Population), by = c("Country", "Year")) %>%
    left_join(GDP.PPP %>% mutate(Country = as.character(Country), Year = as.numeric(Year)) %>% dplyr::select(Country, Year, GDP), by = c("Country", "Year")) %>%
    left_join(manual_country_year_values, by = "CountryYear") %>%
    mutate(Population = if_else(!is.na(Population_manual), Population_manual, Population),
           GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)) %>%
    dplyr::select(-Population_manual, -GDP_manual)
}


country_year_LRTI_wealth_imp_gdp <- add_gdp_population(country_year_LRTI_wealth_imp)
country_year_fever_wealth_imp_gdp <- add_gdp_population(country_year_fever_wealth_imp)

country_year_LRTI_wealth_gdp <- add_gdp_population(country_year_LRTI_wealth)
country_year_fever_wealth_gdp <- add_gdp_population(country_year_fever_wealth)


UNI_income <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>%
  dplyr::select(Country, Income) %>%
  distinct()

UNI_income <- UNI_income %>%
  bind_rows(tibble(Country = c("Azerbaijan", "Gabon", "Guyana", "Haiti", "Maldives"),
                   Income = c("UM", "UM", "UM", "LM", "UM"))) %>%
  distinct(Country, .keep_all = TRUE)

stopifnot(!anyDuplicated(UNI_income$Country))


country_year_LRTI_wealth_imp_gdp <- country_year_LRTI_wealth_imp_gdp %>% inner_join(UNI_income, by = "Country")
country_year_fever_wealth_imp_gdp <- country_year_fever_wealth_imp_gdp %>% inner_join(UNI_income, by = "Country")

country_year_LRTI_wealth_gdp <- country_year_LRTI_wealth_gdp %>% inner_join(UNI_income, by = "Country")
country_year_fever_wealth_gdp <- country_year_fever_wealth_gdp %>% inner_join(UNI_income, by = "Country")



## 6. Region-year descriptive summaries ---------------------------------------

region_year_LRTI_wealth_gdp <- country_year_LRTI_wealth_gdp %>%
  filter(is.finite(Population), Population > 0) %>%
  group_by(outcome, wealth_level, wealth_tag, age_group, gbd_region, Year) %>%
  summarise(
    pred_proportion_mean = weighted.mean(pred_proportion_mean, Population, na.rm = TRUE),
    observed_proportion_mean = weighted.mean(observed_proportion_mean, Population, na.rm = TRUE),
    GDP_pw_mean = weighted.mean(GDP, Population, na.rm = TRUE),
    n_country_year = n(),
    .groups = "drop"
  )

region_year_fever_wealth_gdp <- country_year_fever_wealth_gdp %>%
  filter(is.finite(Population), Population > 0) %>%
  group_by(outcome, wealth_level, wealth_tag, age_group, gbd_region, Year) %>%
  summarise(
    pred_proportion_mean = weighted.mean(pred_proportion_mean, Population, na.rm = TRUE),
    observed_proportion_mean = weighted.mean(observed_proportion_mean, Population, na.rm = TRUE),
    GDP_pw_mean = weighted.mean(GDP, Population, na.rm = TRUE),
    n_country_year = n(),
    .groups = "drop"
  )



## 7. Save wealth subgroup descriptive outputs --------------------------------

out_dir <- "outputs/pipelines"
save_dir_wealth <- file.path(out_dir, "AA_marginal_RE0_wealth_prediction_outputs")
dir.create(save_dir_wealth, recursive = TRUE, showWarnings = FALSE)


# Primary DHS-MI-specific outputs
saveRDS(country_year_LRTI_wealth_imp_gdp, file.path(save_dir_wealth, "AA_country_year_LRTI_wealth_imp_gdp.rds"))
saveRDS(country_year_fever_wealth_imp_gdp, file.path(save_dir_wealth, "AA_country_year_fever_wealth_imp_gdp.rds"))

country_year_wealth_imp_gdp_all <- bind_rows(
  country_year_LRTI_wealth_imp_gdp,
  country_year_fever_wealth_imp_gdp
)

saveRDS(country_year_wealth_imp_gdp_all, file.path(save_dir_wealth, "AA_country_year_wealth_imp_gdp_all.rds"))
write.csv(country_year_wealth_imp_gdp_all, file.path(save_dir_wealth, "AA_country_year_wealth_imp_gdp_all.csv"), row.names = FALSE)


# Descriptive pooled outputs
saveRDS(country_year_LRTI_wealth, file.path(save_dir_wealth, "AA_country_year_LRTI_wealth.rds"))
saveRDS(country_year_fever_wealth, file.path(save_dir_wealth, "AA_country_year_fever_wealth.rds"))
saveRDS(country_year_LRTI_wealth_gdp, file.path(save_dir_wealth, "AA_country_year_LRTI_wealth_gdp.rds"))
saveRDS(country_year_fever_wealth_gdp, file.path(save_dir_wealth, "AA_country_year_fever_wealth_gdp.rds"))
saveRDS(region_year_LRTI_wealth_gdp, file.path(save_dir_wealth, "AA_region_year_LRTI_wealth_gdp.rds"))
saveRDS(region_year_fever_wealth_gdp, file.path(save_dir_wealth, "AA_region_year_fever_wealth_gdp.rds"))

country_year_wealth_all <- bind_rows(country_year_LRTI_wealth, country_year_fever_wealth)
country_year_wealth_gdp_all <- bind_rows(country_year_LRTI_wealth_gdp, country_year_fever_wealth_gdp)
region_year_wealth_gdp_all <- bind_rows(region_year_LRTI_wealth_gdp, region_year_fever_wealth_gdp)

saveRDS(country_year_wealth_all, file.path(save_dir_wealth, "AA_country_year_wealth_all.rds"))
saveRDS(country_year_wealth_gdp_all, file.path(save_dir_wealth, "AA_country_year_wealth_gdp_all.rds"))
saveRDS(region_year_wealth_gdp_all, file.path(save_dir_wealth, "AA_region_year_wealth_gdp_all.rds"))

write.csv(country_year_wealth_gdp_all, file.path(save_dir_wealth, "AA_country_year_wealth_gdp_all.csv"), row.names = FALSE)
write.csv(region_year_wealth_gdp_all, file.path(save_dir_wealth, "AA_region_year_wealth_gdp_all.csv"), row.names = FALSE)

cat("\nSection 6 completed.\n")
cat("DHS imputations retained:", paste(sort(unique(country_year_wealth_imp_gdp_all$.imp_dhs)), collapse = ", "), "\n")



# _____________________________________________ ----------
# 7. Primary-model uncertainty projection by household wealth quintile --------
# One common selected model across all three age groups for each outcome
# 5 PC imputations x 5 DHS imputations x 40 draws = 1000 uncertainty draws

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
selected_model_uncertainty_parent_dir_i <- file.path(out_dir, "AA_age_specific_selected_model_uncertainty_by_wealth")
dir.create(selected_model_uncertainty_parent_dir_i, recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------#
# CHANGE ONLY THESE TWO LINES AFTER REVIEWING MAIN SECTION 7 ------------------

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

glmnet_alpha_i <- c(ridge = 0, enet_025 = 0.25, enet_050 = 0.50, enet_075 = 0.75, lasso = 1)

stopifnot(selected_model_key_LRTI_i %in% names(model_labels_i))
stopifnot(selected_model_key_fever_i %in% names(model_labels_i))

if(selected_model_key_LRTI_i == "svm_radial" || selected_model_key_fever_i == "svm_radial"){
  if(!requireNamespace("kernlab", quietly = TRUE)) stop("kernlab is required for svm_radial.")
}



# 7-1. Load MI, first-stage models, PC MI, and draw design --------------------

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



# 7-2. Country-name map --------------------------------------------------------

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



# 7-3. Model-based covariance matrices -----------------------------------------

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



# 7-4. Confirm categorical wealth variable ------------------------------------

completed_data_LRTI_first_i <- mice::complete(LRTI_imp_i, 1)
completed_data_fever_first_i <- mice::complete(fever_imp_i, 1)

wealth_levels_i <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_tag_map_i <- c("Poorest" = "q1", "Poorer" = "q2", "Middle" = "q3", "Richer" = "q4", "Richest" = "q5")

stopifnot(
  "hh_wealth" %in% names(completed_data_LRTI_first_i),
  "hh_wealth" %in% names(completed_data_fever_first_i),
  is.factor(completed_data_LRTI_first_i$hh_wealth),
  is.factor(completed_data_fever_first_i$hh_wealth),
  identical(levels(completed_data_LRTI_first_i$hh_wealth), wealth_levels_i),
  identical(levels(completed_data_fever_first_i$hh_wealth), wealth_levels_i)
)



# 7-5. Prepare PC-imputation-specific UNI -------------------------------------

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



# 7-6. Age-specific population -------------------------------------------------

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
  transmute(Country, Year,
            `0-11 months` = Age_0,
            `12-23 months` = Age_1,
            `24-59 months` = Age_2 + Age_3 + Age_4) %>%
  pivot_longer(cols = c(`0-11 months`, `12-23 months`, `24-59 months`), names_to = "age_group", values_to = "age_population") %>%
  mutate(age_group = as.character(age_group), age_population = as.numeric(age_population)) %>%
  filter(is.finite(Year), is.finite(age_population), age_population > 0)



# 7-7. Specification -----------------------------------------------------------

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


prediction_grid_base_list_i <- vector("list", 3)

for(j in 1:3){
  
  age_group_i <- age_group_list_i[j]
  
  prediction_grid_base_list_i[[j]] <- UNI_model_i %>%
    filter(.imp_pc == pc_imp_values_i[1]) %>%
    dplyr::select(Country, Year, GDP, any_of("Income"), Population_under5, Endemic, gbd_region) %>%
    mutate(age_group = age_group_i) %>%
    left_join(
      age_population_long_i %>% filter(age_group == age_group_i) %>% dplyr::select(Country, Year, age_group, age_population),
      by = c("Country", "Year", "age_group")
    ) %>%
    filter(is.finite(age_population), age_population > 0) %>%
    arrange(Country, Year)
}



# 7-8. Run all five paired wealth scenarios -----------------------------------

all_wealth_result_list_i <- vector("list", length(wealth_levels_i))
names(all_wealth_result_list_i) <- unname(wealth_tag_map_i[wealth_levels_i])


for(wealth_index_i in seq_along(wealth_levels_i)){
  
  wealth_level_i <- wealth_levels_i[wealth_index_i]
  wealth_tag_i <- unname(wealth_tag_map_i[wealth_level_i])
  
  cat(
    "\n============================================================\n",
    "Running household wealth subgroup: ", wealth_level_i, "\n",
    "Output tag: ", wealth_tag_i, "\n",
    "============================================================\n",
    sep = ""
  )
  
  selected_model_uncertainty_dir_i <- file.path(selected_model_uncertainty_parent_dir_i, wealth_tag_i)
  dir.create(selected_model_uncertainty_dir_i, recursive = TRUE, showWarnings = FALSE)
  
  selected_model_uncertainty_result_list_i <- vector("list", 6)
  names(selected_model_uncertainty_result_list_i) <- as.vector(t(spec_name_matrix_i))
  
  wealth_scenario_summary_i <- tibble(
    outcome = c("LRTI", "fever"),
    wealth_level = factor(rep(wealth_level_i, 2), levels = wealth_levels_i),
    wealth_tag = wealth_tag_i
  )
  
  write.csv(wealth_scenario_summary_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_wealth_scenario_values_", wealth_tag_i, ".csv")), row.names = FALSE)
  
  
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
    
    
    projection_matrix_list_i <- lapply(1:3, function(j){
      matrix(
        NA_real_,
        nrow = nrow(prediction_grid_base_list_i[[j]]),
        ncol = n_draw_i,
        dimnames = list(NULL, paste0("draw_", sprintf("%04d", seq_len(n_draw_i))))
      )
    })
    
    draw_success_i <- matrix(FALSE, nrow = n_draw_i, ncol = 3,
                             dimnames = list(paste0("draw_", sprintf("%04d", seq_len(n_draw_i))), age_group_list_i))
    
    draw_message_i <- matrix(NA_character_, nrow = n_draw_i, ncol = 3,
                             dimnames = list(paste0("draw_", sprintf("%04d", seq_len(n_draw_i))), age_group_list_i))
    
    
    for(draw_id_i in seq_len(n_draw_i)){
      
      design_i <- draw_design_i %>% filter(draw_id == draw_id_i)
      
      pc_imp_i <- design_i$.imp_pc
      dhs_imp_i <- design_i$.imp_dhs
      draw_within_combo_i <- design_i$.draw_within_combo
      
      
      # first-stage DHS coefficient draw
      # same seed for the same draw_id across all five wealth scenarios
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
      
      if(!"hh_wealth" %in% names(dat_used_i) || !is.factor(dat_used_i$hh_wealth)){
        draw_message_i[draw_id_i, ] <- "Categorical hh_wealth was not found"
        next
      }
      
      
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
      
      
      # set all children to the selected counterfactual wealth quintile
      dat_scenario_i <- dat_used_i
      wealth_levels_fit_i <- levels(model.frame(fit_i)$hh_wealth)
      
      if(!wealth_level_i %in% wealth_levels_fit_i){
        draw_message_i[draw_id_i, ] <- paste0("Wealth level not found in fitted model: ", wealth_level_i)
        next
      }
      
      dat_scenario_i$hh_wealth <- factor(wealth_level_i, levels = wealth_levels_fit_i)
      
      fixed_formula_i <- formula(fit_i, fixed.only = TRUE, component = "cond")
      X_reference_i <- model.matrix(fit_i, component = "cond", part = "fixed")
      
      X_scenario_i <- tryCatch(
        model.matrix(fixed_formula_i, data = dat_scenario_i, contrasts.arg = attr(X_reference_i, "contrasts")),
        error = function(e) NULL
      )
      
      if(is.null(X_scenario_i)){
        draw_message_i[draw_id_i, ] <- "Wealth-specific design matrix failed"
        next
      }
      
      missing_beta_columns_i <- setdiff(names(beta_hat_i), colnames(X_scenario_i))
      
      if(length(missing_beta_columns_i) > 0){
        draw_message_i[draw_id_i, ] <- paste0("Missing design-matrix columns: ", paste(missing_beta_columns_i, collapse = ", "))
        next
      }
      
      X_scenario_i <- X_scenario_i[, names(beta_hat_i), drop = FALSE]
      
      if(nrow(X_scenario_i) != nrow(dat_scenario_i)){
        draw_message_i[draw_id_i, ] <- "Wealth-specific design matrix and GLMM data do not align"
        next
      }
      
      dat_scenario_i$pred_proportion_draw <- plogis(as.vector(X_scenario_i %*% beta_draw_i))
      
      
      # matching PC imputation
      pc_dat_i <- UNI_model_i %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(.imp_pc, Country, Year, GDP, any_of("Income"), Population_under5, PC1, PC2, Endemic, gbd_region)
      
      
      # aggregate wealth-specific first-stage predictions
      train_draw_all_age_i <- dat_scenario_i %>%
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
        filter(age_group %in% age_group_list_i, is.finite(sample_weight), sample_weight > 0, is.finite(pred_proportion_draw)) %>%
        group_by(age_group, Country, Year, CountryYear, gbd_region) %>%
        summarise(
          n_child = n(),
          weight_sum = sum(sample_weight, na.rm = TRUE),
          pred_proportion = weighted.mean(pred_proportion_draw, sample_weight, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(
          pc_dat_i %>% dplyr::select(Country, Year, PC1, PC2, Endemic, gbd_region_from_uni = gbd_region),
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
          logit_value = qlogis(proportion_bounded),
          wealth_level = wealth_level_i,
          wealth_tag = wealth_tag_i
        ) %>%
        dplyr::select(-gbd_region_from_uni) %>%
        filter(is.finite(logit_value), is.finite(PC1), is.finite(PC2), is.finite(Endemic), !is.na(gbd_region))
      
      
      if(nrow(train_draw_all_age_i) < 10){
        draw_message_i[draw_id_i, ] <- "Too few first-stage country-year estimates"
        next
      }
      
      
      # one identical country-bootstrap realization for corresponding q1-q5 draws
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
      
      
      # same selected outcome-specific model fitted separately within each age
      for(j in 1:3){
        
        spec_name_i <- spec_names_i[j]
        age_group_i <- age_group_list_i[j]
        
        train_draw_age_i <- train_draw_all_age_i %>%
          filter(age_group == age_group_i)
        
        if(nrow(train_draw_age_i) < 10){
          draw_message_i[draw_id_i, j] <- "Too few age-specific training rows"
          next
        }
        
        
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
        
        
        prediction_grid_i <- pc_dat_i %>%
          mutate(age_group = age_group_i, Endemic = as.numeric(Endemic),
                 wealth_level = wealth_level_i, wealth_tag = wealth_tag_i) %>%
          left_join(
            age_population_long_i %>% filter(age_group == age_group_i) %>% dplyr::select(Country, Year, age_group, age_population),
            by = c("Country", "Year", "age_group")
          ) %>%
          filter(is.finite(age_population), age_population > 0,
                 is.finite(PC1), is.finite(PC2), is.finite(Endemic)) %>%
          arrange(Country, Year)
        
        
        reference_key_i <- prediction_grid_base_list_i[[j]] %>% dplyr::select(Country, Year)
        prediction_key_i <- prediction_grid_i %>% dplyr::select(Country, Year)
        
        if(!identical(reference_key_i, prediction_key_i)){
          stop("Prediction-grid Country-Year ordering differs across PC imputations.")
        }
        
        
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
            lm(logit_value ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3) + Endemic,
               data = boot_train_age_i, weights = weight_model_boot),
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
            mgcv::gam(logit_value ~ s(PC1, k = 4) + s(PC2, k = 4) + Endemic,
                      data = boot_train_age_i, weights = weight_model_boot, method = "REML"),
            error = function(e){selected_model_error_i <<- conditionMessage(e); NULL}
          )
          
          if(!is.null(fit_selected_i)){
            predicted_link_i <- tryCatch(
              as.numeric(predict(fit_selected_i, newdata = prediction_grid_i, type = "response")),
              error = function(e){selected_model_error_i <<- conditionMessage(e); rep(NA_real_, nrow(prediction_grid_i))}
            )
          }
          
          
        } else if(selected_model_key_i %in% c("rf_balanced", "rf_small_nodes", "extra_trees")){
          
          if(selected_model_key_i == "rf_balanced"){num_trees_i <- 1000; mtry_i <- 2; min_node_i <- 5; splitrule_i <- "variance"}
          if(selected_model_key_i == "rf_small_nodes"){num_trees_i <- 1000; mtry_i <- 3; min_node_i <- 2; splitrule_i <- "variance"}
          if(selected_model_key_i == "extra_trees"){num_trees_i <- 1500; mtry_i <- 3; min_node_i <- 3; splitrule_i <- "extratrees"}
          
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
            kernlab::ksvm(x = x_train_i, y = boot_train_age_i$logit_value, type = "eps-svr",
                          kernel = "rbfdot", kpar = "automatic", C = 2, scaled = TRUE),
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
            xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 2, eta = 0.02,
                                 subsample = 0.85, colsample_bytree = 0.85, min_child_weight = 5,
                                 seed = seed_base_i + draw_id_i * 10 + j)
            nrounds_i <- 500
          }
          
          if(selected_model_key_i == "xgb_balanced"){
            xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 3, eta = 0.03,
                                 subsample = 0.80, colsample_bytree = 0.80, min_child_weight = 3,
                                 seed = seed_base_i + draw_id_i * 10 + j)
            nrounds_i <- 300
          }
          
          if(selected_model_key_i == "xgb_deeper"){
            xgb_params_i <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 4, eta = 0.02,
                                 subsample = 0.75, colsample_bytree = 0.75, min_child_weight = 2,
                                 seed = seed_base_i + draw_id_i * 10 + j)
            nrounds_i <- 450
          }
          
          dtrain_i <- xgboost::xgb.DMatrix(data = x_train_i, label = boot_train_age_i$logit_value,
                                           weight = boot_train_age_i$weight_model_boot)
          
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
        message(outcome_i, " | ", wealth_tag_i, ": completed draw ", draw_id_i, " / ", n_draw_i)
      }
    }
    
    
    
    # draw-balance check
    successful_draw_check_i <- bind_rows(
      lapply(1:3, function(j){
        
        tibble(
          draw_id = seq_len(n_draw_i),
          age_group = age_group_list_i[j],
          success = draw_success_i[, j]
        ) %>%
          left_join(draw_design_i %>% dplyr::select(draw_id, .imp_pc, .imp_dhs), by = "draw_id") %>%
          group_by(.imp_pc, .imp_dhs, age_group) %>%
          summarise(n_successful_draws = sum(success), .groups = "drop")
      })
    )
    
    print(successful_draw_check_i, n = Inf)
    
    write.csv(
      successful_draw_check_i,
      file.path(selected_model_uncertainty_dir_i, paste0(outcome_i, "_", selected_model_key_i, "_", wealth_tag_i, "_draw_balance_check.csv")),
      row.names = FALSE
    )
    
    if(any(successful_draw_check_i$n_successful_draws != 40)){
      stop(outcome_i, " | ", wealth_tag_i, ": not all PC x DHS x age cells contain exactly 40 successful draws.")
    }
    
    
    
    # summarize three age groups
    for(j in 1:3){
      
      spec_name_i <- spec_names_i[j]
      age_group_i <- age_group_list_i[j]
      projection_matrix_i <- projection_matrix_list_i[[j]]
      
      prediction_grid_i <- prediction_grid_base_list_i[[j]] %>%
        mutate(
          outcome = outcome_i,
          wealth_level = wealth_level_i,
          wealth_tag = wealth_tag_i,
          age_group = age_group_i
        )
      
      n_successful_draws_i <- rowSums(is.finite(projection_matrix_i))
      
      predicted_mean_i <- rowMeans(projection_matrix_i, na.rm = TRUE)
      predicted_mean_i[n_successful_draws_i == 0] <- NA_real_
      
      predicted_median_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) median(x, na.rm = TRUE) else NA_real_)
      predicted_lcl_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.025, na.rm = TRUE)) else NA_real_)
      predicted_ucl_i <- apply(projection_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.975, na.rm = TRUE)) else NA_real_)
      
      
      country_year_draws_i <- bind_cols(
        prediction_grid_i %>%
          transmute(
            outcome, wealth_level, wealth_tag, age_group,
            Country, Year, gbd_region,
            Population_under5, age_population
          ),
        as.data.frame(projection_matrix_i)
      )
      
      
      country_year_draws_long_i <- country_year_draws_i %>%
        pivot_longer(cols = starts_with("draw_"), names_to = "draw_name", values_to = "pred_proportion") %>%
        mutate(draw_id = as.integer(sub("^draw_", "", draw_name))) %>%
        dplyr::select(-draw_name) %>%
        left_join(
          draw_design_i %>% dplyr::select(draw_id, .draw_within_combo, .imp_pc, .imp_dhs),
          by = "draw_id"
        ) %>%
        relocate(
          draw_id, .draw_within_combo, .imp_pc, .imp_dhs,
          outcome, wealth_level, wealth_tag, age_group,
          Country, Year
        )
      
      
      country_year_summary_i <- prediction_grid_i %>%
        transmute(
          outcome, wealth_level, wealth_tag, age_group,
          Country, Year, gbd_region,
          Population_under5, age_population,
          model_key = selected_model_key_i,
          model = selected_model_label_i,
          alpha = selected_alpha_i,
          predicted_proportion_mean = predicted_mean_i,
          predicted_proportion_median = predicted_median_i,
          predicted_proportion_lcl = predicted_lcl_i,
          predicted_proportion_ucl = predicted_ucl_i,
          n_successful_draws = n_successful_draws_i
        )
      
      
      # year summary
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
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
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
      
      
      # region-year summary
      region_year_keys_i <- prediction_grid_i %>% distinct(Year, gbd_region) %>% arrange(Year, gbd_region)
      region_year_draw_matrix_i <- matrix(NA_real_, nrow = nrow(region_year_keys_i), ncol = n_draw_i)
      
      for(region_year_index_i in seq_len(nrow(region_year_keys_i))){
        
        year_i <- region_year_keys_i$Year[region_year_index_i]
        region_i <- region_year_keys_i$gbd_region[region_year_index_i]
        
        row_index_i <- which(prediction_grid_i$Year == year_i & prediction_grid_i$gbd_region == region_i)
        
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
          wealth_level = wealth_level_i,
          wealth_tag = wealth_tag_i,
          age_group = age_group_i,
          model_key = selected_model_key_i,
          model = selected_model_label_i,
          alpha = selected_alpha_i,
          mean_proportion = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_),
          median_proportion = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) median(x, na.rm = TRUE) else NA_real_),
          proportion_lcl = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.025, na.rm = TRUE)) else NA_real_),
          proportion_ucl = apply(region_year_draw_matrix_i, 1, function(x) if(any(is.finite(x))) as.numeric(quantile(x, 0.975, na.rm = TRUE)) else NA_real_),
          total_age_population = vapply(seq_len(nrow(region_year_keys_i)), function(k){
            sum(prediction_grid_i$age_population[
              prediction_grid_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_i$gbd_region == region_year_keys_i$gbd_region[k]
            ], na.rm = TRUE)
          }, numeric(1)),
          n_country = vapply(seq_len(nrow(region_year_keys_i)), function(k){
            n_distinct(prediction_grid_i$Country[
              prediction_grid_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_i$gbd_region == region_year_keys_i$gbd_region[k]
            ])
          }, integer(1)),
          n_successful_draws = rowSums(is.finite(region_year_draw_matrix_i))
        ) %>%
        relocate(outcome, wealth_level, wealth_tag, age_group, Year, gbd_region)
      
      
      selected_model_uncertainty_result_i <- list(
        outcome = outcome_i,
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
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
      
      
      saveRDS(
        selected_model_uncertainty_result_i,
        file.path(selected_model_uncertainty_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_uncertainty.rds"))
      )
      
      saveRDS(
        country_year_draws_long_i,
        file.path(selected_model_uncertainty_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_projection_draws_long.rds"))
      )
      
      write.csv(country_year_summary_i, file.path(selected_model_uncertainty_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_country_year_summary.csv")), row.names = FALSE)
      write.csv(year_summary_i, file.path(selected_model_uncertainty_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_year_summary.csv")), row.names = FALSE)
      write.csv(region_year_summary_i, file.path(selected_model_uncertainty_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_region_year_summary.csv")), row.names = FALSE)
    }
  }
  
  
  # save all six specifications for this wealth scenario
  saveRDS(
    selected_model_uncertainty_result_list_i,
    file.path(selected_model_uncertainty_dir_i, paste0("AA_all_age_specific_selected_model_uncertainty_results_", wealth_tag_i, ".rds"))
  )
  
  country_year_draws_long_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "country_year_draws_long"))
  country_year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "country_year_summary"))
  year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "year_summary"))
  region_year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "region_year_summary"))
  
  saveRDS(country_year_draws_long_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_all_age_projection_draws_long_", wealth_tag_i, ".rds")))
  saveRDS(country_year_summary_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_all_age_country_year_summary_", wealth_tag_i, ".rds")))
  saveRDS(year_summary_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_all_age_year_summary_", wealth_tag_i, ".rds")))
  saveRDS(region_year_summary_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_all_age_region_year_summary_", wealth_tag_i, ".rds")))
  
  
  successful_draw_check_all_i <- bind_rows(
    lapply(names(selected_model_uncertainty_result_list_i), function(spec_name_i){
      
      result_i <- selected_model_uncertainty_result_list_i[[spec_name_i]]
      
      tibble(
        specification = spec_name_i,
        outcome = result_i$outcome,
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        age_group = result_i$age_group,
        model_key = result_i$model_key,
        n_successful_draws = sum(result_i$draw_success),
        n_requested_draws = result_i$n_draw_requested
      )
    })
  )
  
  print(successful_draw_check_all_i)
  
  stopifnot(all(successful_draw_check_all_i$n_successful_draws == 1000))
  
  saveRDS(successful_draw_check_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_successful_draw_check_", wealth_tag_i, ".rds")))
  write.csv(successful_draw_check_all_i, file.path(selected_model_uncertainty_dir_i, paste0("AA_successful_draw_check_", wealth_tag_i, ".csv")), row.names = FALSE)
  
  all_wealth_result_list_i[[wealth_tag_i]] <- selected_model_uncertainty_result_list_i
  
  message("Completed uncertainty projection for ", wealth_level_i, " (", wealth_tag_i, ").")
}



# 7-9. Combined five-wealth outputs -------------------------------------------

all_wealth_country_year_draws_long_i <- bind_rows(
  lapply(all_wealth_result_list_i, function(wealth_result_i){
    bind_rows(lapply(wealth_result_i, `[[`, "country_year_draws_long"))
  })
)

all_wealth_country_year_summary_i <- bind_rows(
  lapply(all_wealth_result_list_i, function(wealth_result_i){
    bind_rows(lapply(wealth_result_i, `[[`, "country_year_summary"))
  })
)

all_wealth_year_summary_i <- bind_rows(
  lapply(all_wealth_result_list_i, function(wealth_result_i){
    bind_rows(lapply(wealth_result_i, `[[`, "year_summary"))
  })
)

all_wealth_region_year_summary_i <- bind_rows(
  lapply(all_wealth_result_list_i, function(wealth_result_i){
    bind_rows(lapply(wealth_result_i, `[[`, "region_year_summary"))
  })
)


saveRDS(
  all_wealth_result_list_i,
  file.path(selected_model_uncertainty_parent_dir_i, "AA_all_wealth_selected_model_uncertainty_results.rds")
)

saveRDS(
  all_wealth_country_year_draws_long_i,
  file.path(selected_model_uncertainty_parent_dir_i, "AA_all_wealth_selected_model_projection_draws_long.rds")
)

saveRDS(
  all_wealth_country_year_summary_i,
  file.path(selected_model_uncertainty_parent_dir_i, "AA_all_wealth_selected_model_country_year_summary.rds")
)

saveRDS(
  all_wealth_year_summary_i,
  file.path(selected_model_uncertainty_parent_dir_i, "AA_all_wealth_selected_model_year_summary.rds")
)

saveRDS(
  all_wealth_region_year_summary_i,
  file.path(selected_model_uncertainty_parent_dir_i, "AA_all_wealth_selected_model_region_year_summary.rds")
)


cat(
  "\nFinal antibiotic-access wealth-subgroup uncertainty design:\n",
  "LRTI common model = ", selected_model_key_LRTI_i,
  "\nFever common model = ", selected_model_key_fever_i,
  "\nWealth scenarios = 5",
  "\nPC imputations = ", length(unique(draw_design_i$.imp_pc)),
  "\nDHS imputations = ", length(unique(draw_design_i$.imp_dhs)),
  "\nConditional draws per PC x DHS combination = 40",
  "\nTotal draws per wealth scenario = ", n_draw_i,
  "\n",
  sep = ""
)

message(
  "All wealth-subgroup uncertainty projections completed. Outputs saved to: ",
  normalizePath(selected_model_uncertainty_parent_dir_i, mustWork = FALSE)
)