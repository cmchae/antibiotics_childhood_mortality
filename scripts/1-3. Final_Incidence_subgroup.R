# _____________________________________________ ----------
# 6. Wealth-specific marginal RE0 prediction and country-year aggregation ------

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(mice)
  library(glmmTMB)
  library(openxlsx)
})

set.seed(123)
options(scipen = 999)

out_dir <- "outputs/pipelines"
prediction_output_dir <- file.path(out_dir, "incidence_marginal_RE0_wealth_prediction_outputs")
dir.create(prediction_output_dir, recursive = TRUE, showWarnings = FALSE)

imp <- readRDS(file.path(out_dir, "incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds"))
fit_lrti_mi <- readRDS(file.path(out_dir, "(GBD_RE)fit_lrti_mi_gps.rds"))
fit_fever_mi <- readRDS(file.path(out_dir, "(GBD_RE)fit_fever_mi_gps.rds"))

stopifnot(
  length(fit_lrti_mi$analyses) == imp$m,
  length(fit_fever_mi$analyses) == imp$m,
  all(vapply(fit_lrti_mi$analyses, inherits, logical(1), "glmmTMB")),
  all(vapply(fit_fever_mi$analyses, inherits, logical(1), "glmmTMB"))
)

recall_days <- 14
D_lrti_symptom <- 17.5
D_fever_symptom <- 5
exposure_fraction_lrti <- (recall_days + D_lrti_symptom) / 365
exposure_fraction_fever <- (recall_days + D_fever_symptom) / 365

wealth_level_order <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_quintile_vector <- 1:5
wealth_tag_vector <- paste0("q", wealth_quintile_vector)

completed_data_first <- mice::complete(imp, 1)

stopifnot(
  "hh_wealth" %in% names(completed_data_first),
  is.factor(completed_data_first$hh_wealth),
  !anyNA(completed_data_first$hh_wealth),
  identical(levels(completed_data_first$hh_wealth), wealth_level_order),
  !"hh_wealth_z" %in% names(completed_data_first)
)

lrti_wealth_terms <- names(glmmTMB::fixef(fit_lrti_mi$analyses[[1]])$cond)
fever_wealth_terms <- names(glmmTMB::fixef(fit_fever_mi$analyses[[1]])$cond)

stopifnot(
  any(grepl("^hh_wealth", lrti_wealth_terms)),
  any(grepl("^hh_wealth", fever_wealth_terms)),
  !any(grepl("^hh_wealth_z", lrti_wealth_terms)),
  !any(grepl("^hh_wealth_z", fever_wealth_terms))
)

wealth_scenario_summary <- tibble(
  wealth_level = factor(wealth_level_order, levels = wealth_level_order),
  wealth_tag = wealth_tag_vector,
  wealth_quintile = wealth_quintile_vector
)

print(wealth_scenario_summary)
write.csv(wealth_scenario_summary, file.path(prediction_output_dir, "incidence_wealth_quintile_scenarios.csv"), row.names = FALSE)

country_year_lrti_overall_imp_list <- vector("list", imp$m * length(wealth_level_order))
country_year_fever_overall_imp_list <- vector("list", imp$m * length(wealth_level_order))
country_year_lrti_age_imp_list <- vector("list", imp$m * length(wealth_level_order))
country_year_fever_age_imp_list <- vector("list", imp$m * length(wealth_level_order))

list_index_i <- 0

for(i in seq_len(imp$m)){
  
  dat_i <- mice::complete(imp, i) %>%
    mutate(
      log_recall_offset_lrti = log(exposure_fraction_lrti),
      log_recall_offset_fever = log(exposure_fraction_fever),
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE),
      age_group = as.character(ch_age_group)
    )
  
  maternal_edu_levels_i <- levels(dat_i$maternal_edu)
  age_group_levels_i <- levels(dat_i$ch_age_group)
  
  dat_i <- dat_i %>%
    mutate(
      maternal_edu = as.character(maternal_edu),
      maternal_edu = if_else(maternal_edu == "0", "1", maternal_edu),
      maternal_edu = factor(maternal_edu, levels = maternal_edu_levels_i),
      ch_age_group = factor(ch_age_group, levels = age_group_levels_i)
    )
  
  for(k in seq_along(wealth_level_order)){
    
    list_index_i <- list_index_i + 1
    wealth_level_i <- wealth_level_order[k]
    wealth_tag_i <- wealth_tag_vector[k]
    wealth_quintile_i <- wealth_quintile_vector[k]
    
    message("DHS MI ", i, "/", imp$m, " | ", wealth_level_i)
    
    dat_scenario_i <- dat_i %>%
      mutate(hh_wealth = factor(wealth_level_i, levels = wealth_level_order))
    
    pred_lrti_window_i <- predict(
      fit_lrti_mi$analyses[[i]],
      newdata = dat_scenario_i,
      type = "response",
      re.form = NA,
      allow.new.levels = TRUE
    )
    
    pred_fever_window_i <- predict(
      fit_fever_mi$analyses[[i]],
      newdata = dat_scenario_i,
      type = "response",
      re.form = NA,
      allow.new.levels = TRUE
    )
    
    dat_prediction_i <- dat_scenario_i %>%
      mutate(
        .imp_dhs = i,
        wealth_level = factor(wealth_level_i, levels = wealth_level_order),
        wealth_tag = wealth_tag_i,
        wealth_quintile = wealth_quintile_i,
        pred_lrti_annual_rate = pred_lrti_window_i / exposure_fraction_lrti,
        pred_fever_annual_rate = pred_fever_window_i / exposure_fraction_fever
      )
    
    country_year_lrti_overall_imp_list[[list_index_i]] <- dat_prediction_i %>%
      group_by(.imp_dhs, wealth_level, wealth_tag, wealth_quintile, Country, CountryYear, gbd_region, Year_c) %>%
      summarise(
        outcome = "LRTI",
        age_group = "Overall",
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        observed_14d_risk = weighted.mean(LRTI, sample_weight, na.rm = TRUE),
        observed_annual_rate = observed_14d_risk / exposure_fraction_lrti,
        pred_annual_rate = weighted.mean(pred_lrti_annual_rate, sample_weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    country_year_fever_overall_imp_list[[list_index_i]] <- dat_prediction_i %>%
      group_by(.imp_dhs, wealth_level, wealth_tag, wealth_quintile, Country, CountryYear, gbd_region, Year_c) %>%
      summarise(
        outcome = "fever",
        age_group = "Overall",
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        observed_14d_risk = weighted.mean(fever, sample_weight, na.rm = TRUE),
        observed_annual_rate = observed_14d_risk / exposure_fraction_fever,
        pred_annual_rate = weighted.mean(pred_fever_annual_rate, sample_weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    country_year_lrti_age_imp_list[[list_index_i]] <- dat_prediction_i %>%
      group_by(.imp_dhs, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year_c) %>%
      summarise(
        outcome = "LRTI",
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        observed_14d_risk = weighted.mean(LRTI, sample_weight, na.rm = TRUE),
        observed_annual_rate = observed_14d_risk / exposure_fraction_lrti,
        pred_annual_rate = weighted.mean(pred_lrti_annual_rate, sample_weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    country_year_fever_age_imp_list[[list_index_i]] <- dat_prediction_i %>%
      group_by(.imp_dhs, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year_c) %>%
      summarise(
        outcome = "fever",
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        observed_14d_risk = weighted.mean(fever, sample_weight, na.rm = TRUE),
        observed_annual_rate = observed_14d_risk / exposure_fraction_fever,
        pred_annual_rate = weighted.mean(pred_fever_annual_rate, sample_weight, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  gc()
}

country_year_lrti_overall_imp <- bind_rows(country_year_lrti_overall_imp_list) %>% mutate(Country = as.character(Country), Year = Year_c + 2010)
country_year_fever_overall_imp <- bind_rows(country_year_fever_overall_imp_list) %>% mutate(Country = as.character(Country), Year = Year_c + 2010)
country_year_lrti_age_imp <- bind_rows(country_year_lrti_age_imp_list) %>% mutate(Country = as.character(Country), Year = Year_c + 2010)
country_year_fever_age_imp <- bind_rows(country_year_fever_age_imp_list) %>% mutate(Country = as.character(Country), Year = Year_c + 2010)


manual_country_year_values <- tribble(
  ~CountryYear, ~Population_manual, ~GDP_manual,
  "Haiti_2012", 1243438, 3286,
  "Maldives_2009", 30631, 16163,
  "Maldives_2016", 36815, 19264,
  "Maldives_2017", 36199, 19948,
  "Yemen_2013", 4616819, 3665
)

GDP_PPP <- read.csv("data/metadata/GDP_PPP.csv")
colnames(GDP_PPP)[-1] <- gsub("^X", "", colnames(GDP_PPP)[-1])

GDP_PPP <- GDP_PPP %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP") %>%
  mutate(
    Year = as.numeric(Year),
    Country = dplyr::recode(
      Country,
      "Congo, Dem. Rep." = "Democratic Republic of the Congo",
      "Congo, Rep." = "Republic of the Congo",
      "Gambia, The" = "Gambia",
      "Egypt, Arab Rep." = "Egypt",
      "Yemen, Rep." = "Yemen",
      .default = Country
    )
  )

Population_under5 <- read.csv("data/metadata/Under_5.csv") %>%
  dplyr::select(Entity, Year, Population) %>%
  rename(Country = Entity) %>%
  mutate(
    Country = dplyr::recode(
      Country,
      "Democratic Republic of Congo" = "Democratic Republic of the Congo",
      "Congo" = "Republic of the Congo",
      "Egypt, Arab Rep." = "Egypt",
      "Yemen, Rep." = "Yemen",
      .default = Country
    ),
    Year = as.numeric(Year),
    Population = as.numeric(Population)
  )


country_year_lrti_overall_imp_gdp <- country_year_lrti_overall_imp %>%
  left_join(Population_under5, by = c("Country", "Year")) %>%
  left_join(GDP_PPP %>% dplyr::select(Country, Year, GDP), by = c("Country", "Year")) %>%
  left_join(manual_country_year_values, by = "CountryYear") %>%
  mutate(Population = if_else(!is.na(Population_manual), Population_manual, Population), GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)) %>%
  dplyr::select(-Population_manual, -GDP_manual)

country_year_fever_overall_imp_gdp <- country_year_fever_overall_imp %>%
  left_join(Population_under5, by = c("Country", "Year")) %>%
  left_join(GDP_PPP %>% dplyr::select(Country, Year, GDP), by = c("Country", "Year")) %>%
  left_join(manual_country_year_values, by = "CountryYear") %>%
  mutate(Population = if_else(!is.na(Population_manual), Population_manual, Population), GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)) %>%
  dplyr::select(-Population_manual, -GDP_manual)

country_year_lrti_age_imp_gdp <- country_year_lrti_age_imp %>%
  left_join(Population_under5, by = c("Country", "Year")) %>%
  left_join(GDP_PPP %>% dplyr::select(Country, Year, GDP), by = c("Country", "Year")) %>%
  left_join(manual_country_year_values, by = "CountryYear") %>%
  mutate(Population = if_else(!is.na(Population_manual), Population_manual, Population), GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)) %>%
  dplyr::select(-Population_manual, -GDP_manual)

country_year_fever_age_imp_gdp <- country_year_fever_age_imp %>%
  left_join(Population_under5, by = c("Country", "Year")) %>%
  left_join(GDP_PPP %>% dplyr::select(Country, Year, GDP), by = c("Country", "Year")) %>%
  left_join(manual_country_year_values, by = "CountryYear") %>%
  mutate(Population = if_else(!is.na(Population_manual), Population_manual, Population), GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)) %>%
  dplyr::select(-Population_manual, -GDP_manual)


# Descriptive means across the five DHS imputations only.
# These are not used for the final uncertainty propagation in Section 7.

country_year_lrti_overall <- country_year_lrti_overall_imp_gdp %>%
  group_by(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year, Year_c, Population, GDP) %>%
  summarise(
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_annual_rate_mean = mean(observed_annual_rate, na.rm = TRUE),
    pred_annual_rate_mean = mean(pred_annual_rate, na.rm = TRUE),
    pred_annual_rate_sd = sd(pred_annual_rate, na.rm = TRUE),
    .groups = "drop"
  )

country_year_fever_overall <- country_year_fever_overall_imp_gdp %>%
  group_by(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year, Year_c, Population, GDP) %>%
  summarise(
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_annual_rate_mean = mean(observed_annual_rate, na.rm = TRUE),
    pred_annual_rate_mean = mean(pred_annual_rate, na.rm = TRUE),
    pred_annual_rate_sd = sd(pred_annual_rate, na.rm = TRUE),
    .groups = "drop"
  )

country_year_lrti_age <- country_year_lrti_age_imp_gdp %>%
  group_by(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year, Year_c, Population, GDP) %>%
  summarise(
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_annual_rate_mean = mean(observed_annual_rate, na.rm = TRUE),
    pred_annual_rate_mean = mean(pred_annual_rate, na.rm = TRUE),
    pred_annual_rate_sd = sd(pred_annual_rate, na.rm = TRUE),
    .groups = "drop"
  )

country_year_fever_age <- country_year_fever_age_imp_gdp %>%
  group_by(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, Country, CountryYear, gbd_region, Year, Year_c, Population, GDP) %>%
  summarise(
    n_child_mean = mean(n_child, na.rm = TRUE),
    weight_sum_mean = mean(weight_sum, na.rm = TRUE),
    observed_annual_rate_mean = mean(observed_annual_rate, na.rm = TRUE),
    pred_annual_rate_mean = mean(pred_annual_rate, na.rm = TRUE),
    pred_annual_rate_sd = sd(pred_annual_rate, na.rm = TRUE),
    .groups = "drop"
  )


country_year_wealth_gdp_all <- bind_rows(
  country_year_lrti_overall,
  country_year_fever_overall,
  country_year_lrti_age,
  country_year_fever_age
) %>%
  mutate(wealth_level = factor(as.character(wealth_level), levels = wealth_level_order)) %>%
  arrange(outcome, wealth_level, age_group, Country, Year)

country_year_wealth_age_gdp_all <- country_year_wealth_gdp_all %>%
  filter(age_group != "Overall")

region_year_wealth_gdp_all <- country_year_wealth_gdp_all %>%
  filter(is.finite(Population), Population > 0) %>%
  group_by(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, gbd_region, Year) %>%
  summarise(
    pred_annual_rate_mean = weighted.mean(pred_annual_rate_mean, Population, na.rm = TRUE),
    observed_annual_rate_mean = weighted.mean(observed_annual_rate_mean, Population, na.rm = TRUE),
    GDP_pw_mean = weighted.mean(GDP, Population, na.rm = TRUE),
    n_country_year = n(),
    .groups = "drop"
  )


# Primary DHS-imputation-specific outputs
saveRDS(country_year_lrti_overall_imp_gdp, file.path(prediction_output_dir, "country_year_lrti_wealth_overall_by_imp_gdp.rds"))
saveRDS(country_year_fever_overall_imp_gdp, file.path(prediction_output_dir, "country_year_fever_wealth_overall_by_imp_gdp.rds"))
saveRDS(country_year_lrti_age_imp_gdp, file.path(prediction_output_dir, "country_year_lrti_wealth_age_by_imp_gdp.rds"))
saveRDS(country_year_fever_age_imp_gdp, file.path(prediction_output_dir, "country_year_fever_wealth_age_by_imp_gdp.rds"))

# Descriptive pooled outputs
saveRDS(country_year_wealth_gdp_all, file.path(prediction_output_dir, "incidence_country_year_wealth_gdp_all.rds"))
saveRDS(country_year_wealth_age_gdp_all, file.path(prediction_output_dir, "incidence_country_year_wealth_age_gdp_all.rds"))
saveRDS(region_year_wealth_gdp_all, file.path(prediction_output_dir, "incidence_region_year_wealth_gdp_all.rds"))

write.csv(country_year_wealth_gdp_all, file.path(prediction_output_dir, "incidence_country_year_wealth_gdp_all.csv"), row.names = FALSE)
write.csv(country_year_wealth_age_gdp_all, file.path(prediction_output_dir, "incidence_country_year_wealth_age_gdp_all.csv"), row.names = FALSE)
write.csv(region_year_wealth_gdp_all, file.path(prediction_output_dir, "incidence_region_year_wealth_gdp_all.csv"), row.names = FALSE)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Country-year all")
openxlsx::writeData(wb, "Country-year all", country_year_wealth_gdp_all)
openxlsx::addWorksheet(wb, "Country-year age")
openxlsx::writeData(wb, "Country-year age", country_year_wealth_age_gdp_all)
openxlsx::addWorksheet(wb, "Region-year")
openxlsx::writeData(wb, "Region-year", region_year_wealth_gdp_all)
openxlsx::addWorksheet(wb, "Wealth scenarios")
openxlsx::writeData(wb, "Wealth scenarios", wealth_scenario_summary)
openxlsx::saveWorkbook(wb, file.path(prediction_output_dir, "incidence_marginal_RE0_wealth_prediction_outputs.xlsx"), overwrite = TRUE)

cat("\nSection 6 completed. Outputs saved to:\n", normalizePath(prediction_output_dir), "\n", sep = "")



`



# _____________________________________________ ----------
# 7. Primary-model uncertainty projection by household-wealth scenario ---------
# Uses the same 5 PC x 5 DHS x 40 draw design as the primary analysis.
# LRTI: elastic net alpha 0.25; fever: weighted linear model.
# Run this section once for each wealth_level_i.

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(mice)
  library(glmmTMB)
  library(sandwich)
  library(MASS)
  library(glmnet)
})

set.seed(123)
options(scipen = 999)

wealth_level_order_i <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_tag_map_i <- c("Poorest" = "q1", "Poorer" = "q2", "Middle" = "q3", "Richer" = "q4", "Richest" = "q5")
wealth_quintile_map_i <- c("Poorest" = 1L, "Poorer" = 2L, "Middle" = 3L, "Richer" = 4L, "Richest" = 5L)

# Change this only, and run q1-q5 separately
wealth_level_i <- "Richest"

wealth_tag_i <- unname(wealth_tag_map_i[wealth_level_i])
wealth_quintile_i <- unname(wealth_quintile_map_i[wealth_level_i])

if(is.na(wealth_tag_i) || is.na(wealth_quintile_i)){
  stop("Invalid wealth_level_i: ", wealth_level_i)
}

cat(
  "\n============================================================\n",
  "Running household wealth subgroup: ", wealth_level_i, "\n",
  "Output tag: ", wealth_tag_i, "\n",
  "============================================================\n",
  sep = ""
)

out_dir <- "outputs/pipelines"
checkpoint_every_i <- 50L
uncertainty_output_dir_i <- file.path(out_dir, "incidence_age_specific_selected_model_uncertainty_by_wealth", wealth_tag_i)
dir.create(uncertainty_output_dir_i, recursive = TRUE, showWarnings = FALSE)

imp_i <- readRDS(file.path(out_dir, "incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds"))
fit_lrti_mi_i <- readRDS(file.path(out_dir, "(GBD_RE)fit_lrti_mi_gps.rds"))
fit_fever_mi_i <- readRDS(file.path(out_dir, "(GBD_RE)fit_fever_mi_gps.rds"))
UNI_raw_i <- readRDS(file.path(out_dir, "pc_gdp_2023_incidence.rds"))

draw_design_i <- readRDS(
  file.path(
    out_dir,
    "age_specific_selected_model_uncertainty",
    "uncertainty_draw_design_5PC_5DHS_40draw.rds"
  )
)

stopifnot(
  length(fit_lrti_mi_i$analyses) == imp_i$m,
  length(fit_fever_mi_i$analyses) == imp_i$m,
  all(vapply(fit_lrti_mi_i$analyses, inherits, logical(1), "glmmTMB")),
  all(vapply(fit_fever_mi_i$analyses, inherits, logical(1), "glmmTMB")),
  nrow(draw_design_i) == 1000,
  length(unique(draw_design_i$.imp_pc)) == 5,
  length(unique(draw_design_i$.imp_dhs)) == 5
)

draw_balance_i <- draw_design_i %>%
  count(.imp_pc, .imp_dhs, name = "n_draw")

stopifnot(all(draw_balance_i$n_draw == 40))

n_draw_i <- nrow(draw_design_i)

wealth_scenario_summary_i <- tibble(
  wealth_level = factor(wealth_level_i, levels = wealth_level_order_i),
  wealth_tag = wealth_tag_i,
  wealth_quintile = wealth_quintile_i
)

print(wealth_scenario_summary_i)
write.csv(wealth_scenario_summary_i, file.path(uncertainty_output_dir_i, paste0("incidence_wealth_quintile_scenario_", wealth_tag_i, ".csv")), row.names = FALSE)


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
  "Gambia" = "Gambia, The",
  "Iran" = "Iran, Islamic Rep.",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Laos" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "United Republic of Tanzania" = "Tanzania",
  "T\x9frkiye" = "Turkiye",
  "Türkiye" = "Turkiye",
  "Turkey" = "Turkiye",
  "Vietnam" = "Viet Nam",
  "Yemen" = "Yemen, Rep.",
  "Myanmar [Burma]" = "Myanmar",
  "Myanmar \\[Burma\\]" = "Myanmar",
  "Macedonia [FYROM]" = "North Macedonia",
  "Macedonia \\[FYROM\\]" = "North Macedonia",
  "Russia" = "Russian Federation",
  "Swaziland" = "Eswatini"
)


# robust covariance matrices
get_robust_vcov_list_i <- function(fit_mi_i){
  
  lapply(seq_along(fit_mi_i$analyses), function(k){
    
    fit_i <- fit_mi_i$analyses[[k]]
    beta_names_i <- names(glmmTMB::fixef(fit_i)$cond)
    cluster_i <- droplevels(factor(model.frame(fit_i)$gbd_region))
    
    if(nlevels(cluster_i) < 2) stop("Fewer than two clusters in DHS imputation ", k)
    
    vcov_i <- sandwich::vcovHC(fit_i, type = "HC0", cluster = cluster_i, full = FALSE)
    vcov_i <- as.matrix(vcov_i)
    
    if(!all(beta_names_i %in% rownames(vcov_i))){
      stop("Coefficient names do not match robust covariance matrix in DHS imputation ", k)
    }
    
    vcov_i <- vcov_i[beta_names_i, beta_names_i, drop = FALSE]
    vcov_i <- (vcov_i + t(vcov_i)) / 2
    
    if(any(!is.finite(vcov_i))){
      stop("Non-finite robust covariance values in DHS imputation ", k)
    }
    
    eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
    eigen_scale_i <- max(max(abs(eigen_i)), .Machine$double.eps)
    
    if(min(eigen_i) < -1e-6 * eigen_scale_i){
      stop(
        "Robust covariance is not positive semidefinite in DHS imputation ",
        k,
        "; minimum eigenvalue = ",
        signif(min(eigen_i), 5)
      )
    }
    
    vcov_i
  })
}

robust_vcov_lrti_list_i <- get_robust_vcov_list_i(fit_lrti_mi_i)
robust_vcov_fever_list_i <- get_robust_vcov_list_i(fit_fever_mi_i)


robust_vcov_check_i <- bind_rows(
  lapply(c("LRTI", "fever"), function(outcome_i){
    
    fit_mi_check_i <- if(outcome_i == "LRTI") fit_lrti_mi_i else fit_fever_mi_i
    vcov_list_check_i <- if(outcome_i == "LRTI") robust_vcov_lrti_list_i else robust_vcov_fever_list_i
    
    bind_rows(lapply(seq_along(vcov_list_check_i), function(k){
      
      vcov_i <- vcov_list_check_i[[k]]
      eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
      
      tibble(
        outcome = outcome_i,
        .imp_dhs = k,
        n_gbd_clusters = nlevels(droplevels(factor(model.frame(fit_mi_check_i$analyses[[k]])$gbd_region))),
        n_coefficients = nrow(vcov_i),
        covariance_rank = qr(vcov_i)$rank,
        minimum_eigenvalue = min(eigen_i),
        maximum_eigenvalue = max(eigen_i)
      )
    }))
  })
)

print(robust_vcov_check_i)
saveRDS(robust_vcov_check_i, file.path(uncertainty_output_dir_i, "incidence_robust_vcov_check.rds"))
write.csv(robust_vcov_check_i, file.path(uncertainty_output_dir_i, "incidence_robust_vcov_check.csv"), row.names = FALSE)


# wealth variable checks
completed_data_first_i <- mice::complete(imp_i, 1)

stopifnot(
  "hh_wealth" %in% names(completed_data_first_i),
  is.factor(completed_data_first_i$hh_wealth),
  !anyNA(completed_data_first_i$hh_wealth),
  identical(levels(completed_data_first_i$hh_wealth), wealth_level_order_i),
  !"hh_wealth_z" %in% names(completed_data_first_i)
)

lrti_wealth_terms_i <- names(glmmTMB::fixef(fit_lrti_mi_i$analyses[[1]])$cond)
fever_wealth_terms_i <- names(glmmTMB::fixef(fit_fever_mi_i$analyses[[1]])$cond)

stopifnot(
  any(grepl("^hh_wealth", lrti_wealth_terms_i)),
  any(grepl("^hh_wealth", fever_wealth_terms_i)),
  !any(grepl("^hh_wealth_z", lrti_wealth_terms_i)),
  !any(grepl("^hh_wealth_z", fever_wealth_terms_i))
)


# PC-imputation-specific UNI
latitude_i <- read.csv("data/metadata/latitude.csv", fileEncoding = "latin1", stringsAsFactors = FALSE) %>%
  mutate(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    absolute_latitude = as.numeric(absolute_latitude)
  ) %>%
  filter(!is.na(Country), is.finite(absolute_latitude)) %>%
  distinct(Country, .keep_all = TRUE) %>%
  dplyr::select(Country, absolute_latitude)

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
    ),
    log_GDP = log(GDP),
    Year_c = Year - 2010
  ) %>%
  dplyr::select(-any_of("absolute_latitude")) %>%
  left_join(latitude_i, by = "Country") %>%
  filter(
    is.finite(Year),
    is.finite(GDP), GDP > 0,
    is.finite(log_GDP),
    is.finite(Population_under5), Population_under5 > 0,
    is.finite(PC1), is.finite(PC2),
    !is.na(Endemic),
    is.finite(absolute_latitude),
    !is.na(gbd_region)
  )

pc_imp_values_i <- sort(unique(UNI_model_i$.imp_pc))
dhs_imp_values_i <- sort(unique(draw_design_i$.imp_dhs))

stopifnot(length(pc_imp_values_i) == 5)
stopifnot(length(dhs_imp_values_i) == 5)

for(pc_imp_i in pc_imp_values_i){
  pc_check_i <- UNI_model_i %>% filter(.imp_pc == pc_imp_i)
  if(anyDuplicated(pc_check_i[c("Country", "Year")])) stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
}


# age-specific target population
population_by_age_i <- read.csv("data/metadata/Population_by_age.csv", stringsAsFactors = FALSE) %>%
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
  )

age_population_long_i <- population_by_age_i %>%
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


outcome_list_i <- c("LRTI", "fever")
age_group_list_i <- c("0-11 months", "12-23 months", "24-59 months")

spec_name_list_i <- c(
  "LRTI_0_11_months",
  "LRTI_12_23_months",
  "LRTI_24_59_months",
  "fever_0_11_months",
  "fever_12_23_months",
  "fever_24_59_months"
)


# Fixed Country-Year row keys used only to store the 1000 predictions.
# PC1/PC2 themselves are always taken from the draw-specific .imp_pc.
prediction_grid_base_list_i <- vector("list", 3)

for(j in 1:3){
  
  age_group_i <- age_group_list_i[j]
  
  prediction_grid_base_list_i[[j]] <- UNI_model_i %>%
    filter(.imp_pc == pc_imp_values_i[1]) %>%
    dplyr::select(Country, Year, GDP, any_of("Income"), Population_under5, Endemic, gbd_region) %>%
    mutate(Population = Population_under5, age_group = age_group_i) %>%
    left_join(
      age_population_long_i %>%
        filter(age_group == age_group_i) %>%
        dplyr::select(Country, Year, age_group, age_population),
      by = c("Country", "Year", "age_group")
    ) %>%
    filter(is.finite(age_population), age_population > 0) %>%
    arrange(Country, Year)
}


selected_model_uncertainty_result_list_i <- vector("list", 6)
names(selected_model_uncertainty_result_list_i) <- spec_name_list_i


for(i in 1:2){
  
  outcome_i <- outcome_list_i[i]
  
  if(outcome_i == "LRTI"){
    fit_mi_i <- fit_lrti_mi_i
    robust_vcov_list_i <- robust_vcov_lrti_list_i
    selected_model_key_vector_i <- rep("enet_025", 3)
    selected_model_label_vector_i <- rep("Elastic net alpha 0.25", 3)
    selected_alpha_vector_i <- rep(0.25, 3)
    seed_base_i <- 100000
  } else {
    fit_mi_i <- fit_fever_mi_i
    robust_vcov_list_i <- robust_vcov_fever_list_i
    selected_model_key_vector_i <- rep("lm", 3)
    selected_model_label_vector_i <- rep("Weighted linear model", 3)
    selected_alpha_vector_i <- rep(NA_real_, 3)
    seed_base_i <- 200000
  }
  
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
    
    dhs_imp_i <- design_i$.imp_dhs
    pc_imp_i <- design_i$.imp_pc
    draw_within_combo_i <- design_i$.draw_within_combo
    
    message(
      "Uncertainty ", outcome_i,
      " | ", wealth_tag_i,
      " | PC ", pc_imp_i,
      " | DHS ", dhs_imp_i,
      " | draw ", draw_id_i, "/", n_draw_i
    )
    
    
    # DHS-imputation-specific coefficient draw
    set.seed(seed_base_i + draw_id_i)
    
    dat_i <- mice::complete(imp_i, dhs_imp_i) %>%
      mutate(
        log_recall_offset_lrti = log((14 + 17.5) / 365),
        log_recall_offset_fever = log((14 + 5) / 365)
      )
    
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
      draw_message_i[draw_id_i, ] <- "Factor hh_wealth was not found"
      next
    }
    
    if(!identical(levels(dat_used_i$hh_wealth), wealth_level_order_i)){
      draw_message_i[draw_id_i, ] <- "hh_wealth factor levels do not match expected order"
      next
    }
    
    beta_hat_i <- glmmTMB::fixef(fit_i)$cond
    beta_vcov_i <- robust_vcov_list_i[[dhs_imp_i]]
    beta_vcov_i <- beta_vcov_i[names(beta_hat_i), names(beta_hat_i), drop = FALSE]
    
    beta_draw_i <- tryCatch(
      MASS::mvrnorm(n = 1, mu = beta_hat_i, Sigma = beta_vcov_i, tol = 1e-6),
      error = function(e) rep(NA_real_, length(beta_hat_i))
    )
    
    names(beta_draw_i) <- names(beta_hat_i)
    
    if(any(!is.finite(beta_draw_i))){
      draw_message_i[draw_id_i, ] <- "First-stage robust coefficient draw failed"
      next
    }
    
    
    # Set household wealth to selected counterfactual
    dat_scenario_i <- dat_used_i
    dat_scenario_i$hh_wealth <- factor(wealth_level_i, levels = wealth_level_order_i)
    
    maternal_edu_levels_i <- levels(dat_scenario_i$maternal_edu)
    age_group_levels_i <- levels(dat_scenario_i$ch_age_group)
    
    dat_scenario_i <- dat_scenario_i %>%
      mutate(
        maternal_edu = as.character(maternal_edu),
        maternal_edu = if_else(maternal_edu == "0", "1", maternal_edu),
        maternal_edu = factor(maternal_edu, levels = maternal_edu_levels_i),
        ch_age_group = factor(ch_age_group, levels = age_group_levels_i)
      )
    
    fixed_formula_i <- formula(fit_i, fixed.only = TRUE, component = "cond")
    X_reference_i <- model.matrix(fit_i, component = "cond", part = "fixed")
    
    X_scenario_i <- tryCatch(
      model.matrix(
        fixed_formula_i,
        data = dat_scenario_i,
        contrasts.arg = attr(X_reference_i, "contrasts")
      ),
      error = function(e) NULL
    )
    
    if(is.null(X_scenario_i)){
      draw_message_i[draw_id_i, ] <- "Wealth-specific GLMM design matrix failed"
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
    
    dat_scenario_i$pred_annual_rate_draw <- exp(as.vector(X_scenario_i %*% beta_draw_i))
    
    
    # Matching PC imputation for this draw
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
        absolute_latitude,
        gbd_region
      )
    
    if(anyDuplicated(pc_dat_i[c("Country", "Year")])){
      stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
    }
    
    
    # DHS country-year-age estimates for this wealth scenario
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
      filter(
        age_group %in% age_group_list_i,
        is.finite(sample_weight), sample_weight > 0,
        is.finite(pred_annual_rate_draw)
      ) %>%
      group_by(age_group, Country, Year, CountryYear, gbd_region) %>%
      summarise(
        n_child = n(),
        weight_sum = sum(sample_weight, na.rm = TRUE),
        pred_annual_rate = weighted.mean(pred_annual_rate_draw, sample_weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        pc_dat_i %>%
          dplyr::select(Country, Year, GDP, PC1, PC2, Endemic, absolute_latitude),
        by = c("Country", "Year")
      ) %>%
      mutate(
        .imp_pc = pc_imp_i,
        .imp_dhs = dhs_imp_i,
        log_GDP = log(GDP),
        log_rate = log(pmax(pred_annual_rate, 1e-6)),
        Year_c = Year - 2010,
        Endemic = as.numeric(Endemic),
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        wealth_quintile = wealth_quintile_i
      ) %>%
      filter(
        is.finite(log_rate),
        is.finite(log_GDP),
        is.finite(PC1),
        is.finite(PC2),
        !is.na(Endemic),
        is.finite(absolute_latitude)
      )
    
    if(nrow(train_draw_all_age_i) < 10){
      draw_message_i[draw_id_i, ] <- "Too few first-stage country-year estimates"
      next
    }
    
    
    # Extrapolation-model weights
    weight_raw_i <- as.numeric(train_draw_all_age_i$n_child)
    weight_valid_i <- is.finite(weight_raw_i) & weight_raw_i > 0
    
    if(any(weight_valid_i)){
      weight_raw_i[!weight_valid_i] <- median(weight_raw_i[weight_valid_i], na.rm = TRUE)
      weight_cap_i <- as.numeric(quantile(weight_raw_i, 0.95, na.rm = TRUE))
      train_draw_all_age_i$weight_model <- pmin(weight_raw_i, weight_cap_i)
    } else {
      train_draw_all_age_i$weight_model <- 1
    }
    
    
    # Same country-bootstrap seed for the corresponding draw across wealth scenarios
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
      
      if(setequal(endemic_values_i, boot_endemic_i)){
        boot_countries_i <- boot_try_i
        break
      }
    }
    
    if(length(boot_countries_i) == 0){
      draw_message_i[draw_id_i, ] <- "Could not create valid country bootstrap"
      next
    }
    
    boot_country_count_i <- as.data.frame(table(boot_countries_i), stringsAsFactors = FALSE)
    names(boot_country_count_i) <- c("Country", "bootstrap_count")
    boot_country_count_i$Country <- as.character(boot_country_count_i$Country)
    boot_country_count_i$bootstrap_count <- as.numeric(boot_country_count_i$bootstrap_count)
    
    
    # Fit selected model separately for each age group
    for(j in 1:3){
      
      age_group_i <- age_group_list_i[j]
      
      boot_train_age_i <- train_draw_all_age_i %>%
        filter(age_group == age_group_i) %>%
        inner_join(boot_country_count_i, by = "Country") %>%
        mutate(weight_model_boot = weight_model * bootstrap_count) %>%
        filter(is.finite(weight_model_boot), weight_model_boot > 0)
      
      if(nrow(boot_train_age_i) < 10){
        draw_message_i[draw_id_i, j] <- "Too few bootstrap rows for age group"
        next
      }
      
      # GDP scaling is draw-specific, as in the primary analysis
      log_gdp_mean_i <- mean(boot_train_age_i$log_GDP, na.rm = TRUE)
      log_gdp_sd_i <- sd(boot_train_age_i$log_GDP, na.rm = TRUE)
      
      if(!is.finite(log_gdp_sd_i) || log_gdp_sd_i == 0) log_gdp_sd_i <- 1
      
      boot_train_age_i <- boot_train_age_i %>%
        mutate(log_GDP_z = (log_GDP - log_gdp_mean_i) / log_gdp_sd_i)
      
      prediction_grid_i <- pc_dat_i %>%
        mutate(
          age_group = age_group_i,
          log_GDP_z = (log(GDP) - log_gdp_mean_i) / log_gdp_sd_i,
          Year_c = Year - 2010,
          Endemic = as.numeric(Endemic),
          wealth_level = wealth_level_i,
          wealth_tag = wealth_tag_i,
          wealth_quintile = wealth_quintile_i
        ) %>%
        left_join(
          age_population_long_i %>%
            filter(age_group == age_group_i) %>%
            dplyr::select(Country, Year, age_group, age_population),
          by = c("Country", "Year", "age_group")
        ) %>%
        filter(
          is.finite(age_population), age_population > 0,
          is.finite(PC1), is.finite(PC2),
          is.finite(log_GDP_z),
          !is.na(Endemic),
          is.finite(absolute_latitude)
        ) %>%
        arrange(Country, Year)
      
      reference_key_i <- prediction_grid_base_list_i[[j]] %>%
        dplyr::select(Country, Year)
      
      prediction_key_i <- prediction_grid_i %>%
        dplyr::select(Country, Year)
      
      if(!identical(reference_key_i, prediction_key_i)){
        stop("Prediction-grid Country-Year ordering differs across PC imputations.")
      }
      
      predicted_log_rate_i <- rep(NA_real_, nrow(prediction_grid_i))
      
      
      # LRTI: elastic net alpha 0.25
      if(outcome_i == "LRTI"){
        
        combined_matrix_i <- bind_rows(
          boot_train_age_i %>%
            transmute(PC1, PC2, log_GDP_z, Year_c, Endemic, absolute_latitude, .set = "train"),
          prediction_grid_i %>%
            transmute(PC1, PC2, log_GDP_z, Year_c, Endemic, absolute_latitude, .set = "prediction")
        )
        
        x_all_i <- model.matrix(
          ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
          data = combined_matrix_i
        )[, -1, drop = FALSE]
        
        x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
        x_prediction_i <- x_all_i[combined_matrix_i$.set == "prediction", , drop = FALSE]
        
        nfolds_i <- min(5, nrow(boot_train_age_i))
        foldid_i <- ((seq_len(nrow(boot_train_age_i)) - 1) %% nfolds_i) + 1
        
        selected_fit_i <- tryCatch(
          glmnet::cv.glmnet(
            x = x_train_i,
            y = boot_train_age_i$log_rate,
            weights = boot_train_age_i$weight_model_boot,
            alpha = 0.25,
            family = "gaussian",
            nfolds = nfolds_i,
            foldid = foldid_i
          ),
          error = function(e) NULL
        )
        
        if(!is.null(selected_fit_i)){
          predicted_log_rate_i <- tryCatch(
            as.numeric(predict(selected_fit_i, newx = x_prediction_i, s = "lambda.min")),
            error = function(e) rep(NA_real_, nrow(prediction_grid_i))
          )
        }
        
        
        # Fever: weighted linear model
      } else {
        
        selected_fit_i <- tryCatch(
          lm(
            log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
            data = boot_train_age_i,
            weights = weight_model_boot
          ),
          error = function(e) NULL
        )
        
        if(!is.null(selected_fit_i)){
          predicted_log_rate_i <- tryCatch(
            as.numeric(predict(selected_fit_i, newdata = prediction_grid_i)),
            error = function(e) rep(NA_real_, nrow(prediction_grid_i))
          )
        }
      }
      
      predicted_rate_i <- exp(predicted_log_rate_i)
      
      if(
        length(predicted_rate_i) != nrow(prediction_grid_i) ||
        any(!is.finite(predicted_rate_i))
      ){
        draw_message_i[draw_id_i, j] <- "Projection failed or contained non-finite values"
        next
      }
      
      projection_matrix_list_i[[j]][, draw_id_i] <- predicted_rate_i
      draw_success_i[draw_id_i, j] <- TRUE
      draw_message_i[draw_id_i, j] <- "success"
    }
    
    
    if(draw_id_i %% checkpoint_every_i == 0 || draw_id_i == n_draw_i){
      
      saveRDS(
        list(
          outcome = outcome_i,
          wealth_level = wealth_level_i,
          wealth_tag = wealth_tag_i,
          wealth_quintile = wealth_quintile_i,
          draw_design = draw_design_i,
          completed_draw = draw_id_i,
          projection_matrix_list = projection_matrix_list_i,
          draw_success = draw_success_i,
          draw_message = draw_message_i
        ),
        file.path(
          uncertainty_output_dir_i,
          paste0(outcome_i, "_selected_models_", wealth_tag_i, "_checkpoint.rds")
        )
      )
    }
  }
  
  
  # Hard balance check: 40 successful draws in every PC x DHS x age cell
  successful_draw_check_outcome_i <- bind_rows(
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
  
  print(successful_draw_check_outcome_i, n = Inf)
  
  write.csv(
    successful_draw_check_outcome_i,
    file.path(uncertainty_output_dir_i, paste0(outcome_i, "_", wealth_tag_i, "_draw_balance_check.csv")),
    row.names = FALSE
  )
  
  if(any(successful_draw_check_outcome_i$n_successful_draws != 40)){
    stop(
      outcome_i,
      " | ",
      wealth_tag_i,
      ": not all PC x DHS x age cells have exactly 40 successful draws."
    )
  }
  
  
  # Summaries and saved result objects
  for(j in 1:3){
    
    age_group_i <- age_group_list_i[j]
    spec_index_i <- 3 * (i - 1) + j
    spec_name_i <- spec_name_list_i[spec_index_i]
    
    selected_model_key_i <- selected_model_key_vector_i[j]
    selected_model_label_i <- selected_model_label_vector_i[j]
    selected_alpha_i <- selected_alpha_vector_i[j]
    
    projection_matrix_i <- projection_matrix_list_i[[j]]
    
    prediction_grid_base_i <- prediction_grid_base_list_i[[j]] %>%
      mutate(
        outcome = outcome_i,
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        wealth_quintile = wealth_quintile_i
      ) %>%
      relocate(outcome, wealth_level, wealth_tag, wealth_quintile, age_group)
    
    cases_matrix_i <- projection_matrix_i * prediction_grid_base_i$age_population
    
    n_successful_draws_i <- rowSums(is.finite(projection_matrix_i))
    
    predicted_rate_mean_i <- rowMeans(projection_matrix_i, na.rm = TRUE)
    predicted_rate_mean_i[n_successful_draws_i == 0] <- NA_real_
    predicted_rate_median_i <- apply(projection_matrix_i, 1, function(x) median(x, na.rm = TRUE))
    predicted_rate_lcl_i <- apply(projection_matrix_i, 1, function(x) as.numeric(quantile(x, 0.025, na.rm = TRUE)))
    predicted_rate_ucl_i <- apply(projection_matrix_i, 1, function(x) as.numeric(quantile(x, 0.975, na.rm = TRUE)))
    
    predicted_cases_mean_i <- rowMeans(cases_matrix_i, na.rm = TRUE)
    predicted_cases_mean_i[n_successful_draws_i == 0] <- NA_real_
    predicted_cases_median_i <- apply(cases_matrix_i, 1, function(x) median(x, na.rm = TRUE))
    predicted_cases_lcl_i <- apply(cases_matrix_i, 1, function(x) as.numeric(quantile(x, 0.025, na.rm = TRUE)))
    predicted_cases_ucl_i <- apply(cases_matrix_i, 1, function(x) as.numeric(quantile(x, 0.975, na.rm = TRUE)))
    
    country_year_draws_i <- bind_cols(
      prediction_grid_base_i,
      as.data.frame(projection_matrix_i)
    )
    
    country_year_summary_i <- prediction_grid_base_i %>%
      mutate(
        model_key = selected_model_key_i,
        model = selected_model_label_i,
        alpha = selected_alpha_i,
        predicted_annual_rate_mean = predicted_rate_mean_i,
        predicted_annual_rate_median = predicted_rate_median_i,
        predicted_annual_rate_lcl = predicted_rate_lcl_i,
        predicted_annual_rate_ucl = predicted_rate_ucl_i,
        predicted_cases_mean = predicted_cases_mean_i,
        predicted_cases_median = predicted_cases_median_i,
        predicted_cases_lcl = predicted_cases_lcl_i,
        predicted_cases_ucl = predicted_cases_ucl_i,
        n_successful_draws = n_successful_draws_i
      )
    
    
    # Overall year draw matrices
    year_levels_i <- sort(unique(prediction_grid_base_i$Year))
    year_rate_draw_matrix_i <- matrix(NA_real_, nrow = length(year_levels_i), ncol = n_draw_i)
    year_cases_draw_matrix_i <- matrix(NA_real_, nrow = length(year_levels_i), ncol = n_draw_i)
    
    for(year_index_i in seq_along(year_levels_i)){
      
      year_value_i <- year_levels_i[year_index_i]
      row_index_i <- which(prediction_grid_base_i$Year == year_value_i)
      
      rate_submatrix_i <- projection_matrix_i[row_index_i, , drop = FALSE]
      cases_submatrix_i <- cases_matrix_i[row_index_i, , drop = FALSE]
      population_weight_i <- prediction_grid_base_i$age_population[row_index_i]
      
      weighted_numerator_i <- colSums(rate_submatrix_i * population_weight_i, na.rm = TRUE)
      weighted_denominator_i <- colSums(is.finite(rate_submatrix_i) * population_weight_i, na.rm = TRUE)
      
      year_rate_draw_matrix_i[year_index_i, ] <- ifelse(
        weighted_denominator_i > 0,
        weighted_numerator_i / weighted_denominator_i,
        NA_real_
      )
      
      year_cases_draw_matrix_i[year_index_i, ] <- colSums(cases_submatrix_i, na.rm = TRUE)
      year_cases_draw_matrix_i[year_index_i, colSums(is.finite(cases_submatrix_i)) == 0] <- NA_real_
    }
    
    year_summary_i <- tibble(
      outcome = outcome_i,
      wealth_level = wealth_level_i,
      wealth_tag = wealth_tag_i,
      wealth_quintile = wealth_quintile_i,
      age_group = age_group_i,
      Year = year_levels_i,
      model_key = selected_model_key_i,
      model = selected_model_label_i,
      alpha = selected_alpha_i,
      predicted_annual_rate_mean = apply(year_rate_draw_matrix_i, 1, mean, na.rm = TRUE),
      predicted_annual_rate_median = apply(year_rate_draw_matrix_i, 1, median, na.rm = TRUE),
      predicted_annual_rate_lcl = apply(year_rate_draw_matrix_i, 1, quantile, 0.025, na.rm = TRUE),
      predicted_annual_rate_ucl = apply(year_rate_draw_matrix_i, 1, quantile, 0.975, na.rm = TRUE),
      predicted_cases_mean = apply(year_cases_draw_matrix_i, 1, mean, na.rm = TRUE),
      predicted_cases_median = apply(year_cases_draw_matrix_i, 1, median, na.rm = TRUE),
      predicted_cases_lcl = apply(year_cases_draw_matrix_i, 1, quantile, 0.025, na.rm = TRUE),
      predicted_cases_ucl = apply(year_cases_draw_matrix_i, 1, quantile, 0.975, na.rm = TRUE),
      total_age_population = vapply(year_levels_i, function(x) sum(prediction_grid_base_i$age_population[prediction_grid_base_i$Year == x], na.rm = TRUE), numeric(1)),
      n_country = vapply(year_levels_i, function(x) n_distinct(prediction_grid_base_i$Country[prediction_grid_base_i$Year == x]), integer(1)),
      n_successful_draws = rowSums(is.finite(year_rate_draw_matrix_i))
    )
    
    
    # Region-year draw matrices
    region_year_keys_i <- prediction_grid_base_i %>%
      distinct(Year, gbd_region) %>%
      arrange(Year, gbd_region)
    
    region_year_rate_draw_matrix_i <- matrix(NA_real_, nrow = nrow(region_year_keys_i), ncol = n_draw_i)
    region_year_cases_draw_matrix_i <- matrix(NA_real_, nrow = nrow(region_year_keys_i), ncol = n_draw_i)
    
    for(region_year_index_i in seq_len(nrow(region_year_keys_i))){
      
      year_value_i <- region_year_keys_i$Year[region_year_index_i]
      region_value_i <- region_year_keys_i$gbd_region[region_year_index_i]
      
      row_index_i <- which(
        prediction_grid_base_i$Year == year_value_i &
          prediction_grid_base_i$gbd_region == region_value_i
      )
      
      rate_submatrix_i <- projection_matrix_i[row_index_i, , drop = FALSE]
      cases_submatrix_i <- cases_matrix_i[row_index_i, , drop = FALSE]
      population_weight_i <- prediction_grid_base_i$age_population[row_index_i]
      
      weighted_numerator_i <- colSums(rate_submatrix_i * population_weight_i, na.rm = TRUE)
      weighted_denominator_i <- colSums(is.finite(rate_submatrix_i) * population_weight_i, na.rm = TRUE)
      
      region_year_rate_draw_matrix_i[region_year_index_i, ] <- ifelse(
        weighted_denominator_i > 0,
        weighted_numerator_i / weighted_denominator_i,
        NA_real_
      )
      
      region_year_cases_draw_matrix_i[region_year_index_i, ] <- colSums(cases_submatrix_i, na.rm = TRUE)
      region_year_cases_draw_matrix_i[region_year_index_i, colSums(is.finite(cases_submatrix_i)) == 0] <- NA_real_
    }
    
    region_year_summary_i <- region_year_keys_i %>%
      mutate(
        outcome = outcome_i,
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        wealth_quintile = wealth_quintile_i,
        age_group = age_group_i,
        model_key = selected_model_key_i,
        model = selected_model_label_i,
        alpha = selected_alpha_i,
        predicted_annual_rate_mean = apply(region_year_rate_draw_matrix_i, 1, mean, na.rm = TRUE),
        predicted_annual_rate_median = apply(region_year_rate_draw_matrix_i, 1, median, na.rm = TRUE),
        predicted_annual_rate_lcl = apply(region_year_rate_draw_matrix_i, 1, quantile, 0.025, na.rm = TRUE),
        predicted_annual_rate_ucl = apply(region_year_rate_draw_matrix_i, 1, quantile, 0.975, na.rm = TRUE),
        predicted_cases_mean = apply(region_year_cases_draw_matrix_i, 1, mean, na.rm = TRUE),
        predicted_cases_median = apply(region_year_cases_draw_matrix_i, 1, median, na.rm = TRUE),
        predicted_cases_lcl = apply(region_year_cases_draw_matrix_i, 1, quantile, 0.025, na.rm = TRUE),
        predicted_cases_ucl = apply(region_year_cases_draw_matrix_i, 1, quantile, 0.975, na.rm = TRUE),
        total_age_population = vapply(seq_len(nrow(region_year_keys_i)), function(k){
          sum(
            prediction_grid_base_i$age_population[
              prediction_grid_base_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_base_i$gbd_region == region_year_keys_i$gbd_region[k]
            ],
            na.rm = TRUE
          )
        }, numeric(1)),
        n_country = vapply(seq_len(nrow(region_year_keys_i)), function(k){
          n_distinct(
            prediction_grid_base_i$Country[
              prediction_grid_base_i$Year == region_year_keys_i$Year[k] &
                prediction_grid_base_i$gbd_region == region_year_keys_i$gbd_region[k]
            ]
          )
        }, integer(1)),
        n_successful_draws = rowSums(is.finite(region_year_rate_draw_matrix_i))
      ) %>%
      relocate(outcome, wealth_level, wealth_tag, wealth_quintile, age_group, Year, gbd_region)
    
    
    selected_model_uncertainty_result_i <- list(
      outcome = outcome_i,
      wealth_level = wealth_level_i,
      wealth_tag = wealth_tag_i,
      wealth_quintile = wealth_quintile_i,
      age_group = age_group_i,
      model_key = selected_model_key_i,
      model = selected_model_label_i,
      alpha = selected_alpha_i,
      n_draw_requested = n_draw_i,
      draw_design = draw_design_i,
      prediction_grid = prediction_grid_base_i,
      projection_rate_draw_matrix = projection_matrix_i,
      projection_cases_draw_matrix = cases_matrix_i,
      country_year_draws = country_year_draws_i,
      country_year_summary = country_year_summary_i,
      year_rate_draw_matrix = year_rate_draw_matrix_i,
      year_cases_draw_matrix = year_cases_draw_matrix_i,
      year_summary = year_summary_i,
      region_year_rate_draw_matrix = region_year_rate_draw_matrix_i,
      region_year_cases_draw_matrix = region_year_cases_draw_matrix_i,
      region_year_summary = region_year_summary_i,
      draw_success = draw_success_i[, j],
      draw_message = draw_message_i[, j]
    )
    
    selected_model_uncertainty_result_list_i[[spec_name_i]] <- selected_model_uncertainty_result_i
    
    saveRDS(
      selected_model_uncertainty_result_i,
      file.path(
        uncertainty_output_dir_i,
        paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_uncertainty.rds")
      )
    )
    
    write.csv(
      country_year_summary_i,
      file.path(uncertainty_output_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_country_year_summary.csv")),
      row.names = FALSE
    )
    
    write.csv(
      year_summary_i,
      file.path(uncertainty_output_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_year_summary.csv")),
      row.names = FALSE
    )
    
    write.csv(
      region_year_summary_i,
      file.path(uncertainty_output_dir_i, paste0(spec_name_i, "_", selected_model_key_i, "_", wealth_tag_i, "_region_year_summary.csv")),
      row.names = FALSE
    )
  }
}


saveRDS(
  selected_model_uncertainty_result_list_i,
  file.path(
    uncertainty_output_dir_i,
    paste0("incidence_all_age_specific_selected_model_uncertainty_results_", wealth_tag_i, ".rds")
  )
)

country_year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "country_year_summary"))
year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "year_summary"))
region_year_summary_all_i <- bind_rows(lapply(selected_model_uncertainty_result_list_i, `[[`, "region_year_summary"))

saveRDS(country_year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_country_year_summary_all_", wealth_tag_i, ".rds")))
saveRDS(year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_year_summary_all_", wealth_tag_i, ".rds")))
saveRDS(region_year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_region_year_summary_all_", wealth_tag_i, ".rds")))

write.csv(country_year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_country_year_summary_all_", wealth_tag_i, ".csv")), row.names = FALSE)
write.csv(year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_year_summary_all_", wealth_tag_i, ".csv")), row.names = FALSE)
write.csv(region_year_summary_all_i, file.path(uncertainty_output_dir_i, paste0("incidence_region_year_summary_all_", wealth_tag_i, ".csv")), row.names = FALSE)


successful_draw_check_i <- bind_rows(
  lapply(names(selected_model_uncertainty_result_list_i), function(spec_name_check_i){
    
    result_check_i <- selected_model_uncertainty_result_list_i[[spec_name_check_i]]
    
    tibble(
      specification = spec_name_check_i,
      wealth_level = wealth_level_i,
      n_successful_draws = sum(result_check_i$draw_success),
      n_requested_draws = result_check_i$n_draw_requested
    )
  })
)

print(successful_draw_check_i)

stopifnot(all(successful_draw_check_i$n_successful_draws == 1000))

saveRDS(successful_draw_check_i, file.path(uncertainty_output_dir_i, paste0("incidence_successful_draw_check_", wealth_tag_i, ".rds")))
write.csv(successful_draw_check_i, file.path(uncertainty_output_dir_i, paste0("incidence_successful_draw_check_", wealth_tag_i, ".csv")), row.names = FALSE)

cat(
  "\nSection 7 completed for ", wealth_level_i,
  ". 5 PC x 5 DHS x 40 = 1000 balanced draws.\nOutputs saved to:\n",
  normalizePath(uncertainty_output_dir_i),
  "\n",
  sep = ""
)







# _____________________________________________ ----------
# 8. Incidence of bacterial infection by wealth subgroup ----------------------

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

out_dir <- "outputs/pipelines"
bacterial_output_dir <- file.path(out_dir, "bacterial_incidence", "wealth_subgroup")
dir.create(bacterial_output_dir, recursive = TRUE, showWarnings = FALSE)

result_q1 <- readRDS("outputs/pipelines/incidence_age_specific_selected_model_uncertainty_by_wealth/q1/incidence_all_age_specific_selected_model_uncertainty_results_q1.rds")
result_q2 <- readRDS("outputs/pipelines/incidence_age_specific_selected_model_uncertainty_by_wealth/q2/incidence_all_age_specific_selected_model_uncertainty_results_q2.rds")
result_q3 <- readRDS("outputs/pipelines/incidence_age_specific_selected_model_uncertainty_by_wealth/q3/incidence_all_age_specific_selected_model_uncertainty_results_q3.rds")
result_q4 <- readRDS("outputs/pipelines/incidence_age_specific_selected_model_uncertainty_by_wealth/q4/incidence_all_age_specific_selected_model_uncertainty_results_q4.rds")
result_q5 <- readRDS("outputs/pipelines/incidence_age_specific_selected_model_uncertainty_by_wealth/q5/incidence_all_age_specific_selected_model_uncertainty_results_q5.rds")

# Revised Section 9 outputs
prop_bact_fever <- readRDS("outputs/pipelines/bact_fever/bact_fever_draws_1000_long.rds")
prop_bact_LRTI <- readRDS("outputs/pipelines/bact_lrti/bact_lrti_draws_1000.rds")

draw_design <- readRDS(
  "outputs/pipelines/age_specific_selected_model_uncertainty/uncertainty_draw_design_5PC_5DHS_40draw.rds"
)

stopifnot(
  n_distinct(prop_bact_fever$draw_id) == 1000,
  n_distinct(prop_bact_LRTI$draw_id) == 1000,
  nrow(draw_design) == 1000
)

wealth_level_list <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_tag_list <- paste0("q", 1:5)

age_group_list <- c("0-11 months", "12-23 months", "24-59 months")
age_tag_list <- c("0_11", "12_23", "24_59")

fever_spec_list <- c(
  "fever_0_11_months",
  "fever_12_23_months",
  "fever_24_59_months"
)

lrti_spec_list <- c(
  "LRTI_0_11_months",
  "LRTI_12_23_months",
  "LRTI_24_59_months"
)

lrti_prop_list <- c(
  "prop_bact_0_11m",
  "prop_bact_12_23m",
  "prop_bact_24_59m"
)


wide_to_long <- function(dat_i){
  
  dat_i %>%
    dplyr::select(Country, Year, dplyr::starts_with("draw_")) %>%
    pivot_longer(
      cols = starts_with("draw_"),
      names_to = "draw_name",
      values_to = "pred_rate"
    ) %>%
    mutate(
      draw_id = as.integer(sub("^draw_", "", draw_name))
    ) %>%
    dplyr::select(-draw_name) %>%
    left_join(
      draw_design %>%
        dplyr::select(draw_id, .draw_within_combo, .imp_pc, .imp_dhs),
      by = "draw_id"
    )
}


for(i in 1:5){
  
  wealth_level_i <- wealth_level_list[i]
  wealth_tag_i <- wealth_tag_list[i]
  result_i <- get(paste0("result_", wealth_tag_i))
  
  fever_long_all_i <- vector("list", 3)
  lrti_long_all_i <- vector("list", 3)
  
  
  for(j in 1:3){
    
    age_group_i <- age_group_list[j]
    age_tag_i <- age_tag_list[j]
    
    fever_spec_i <- fever_spec_list[j]
    lrti_spec_i <- lrti_spec_list[j]
    lrti_prop_i <- lrti_prop_list[j]
    
    
    # Fever incidence draws
    fever_incidence_long_i <- wide_to_long(
      result_i[[fever_spec_i]]$country_year_draws
    )
    
    fever_missing_i <- fever_incidence_long_i %>%
      distinct(Country, Year, draw_id, .imp_pc, .imp_dhs) %>%
      anti_join(
        prop_bact_fever %>%
          distinct(Country, Year, draw_id, .imp_pc, .imp_dhs),
        by = c("Country", "Year", "draw_id", ".imp_pc", ".imp_dhs")
      )
    
    if(nrow(fever_missing_i) > 0){
      stop("Missing AFI bacterial-fraction draws for ", wealth_tag_i, " | ", age_group_i)
    }
    
    inc_bact_fever_long_i <- fever_incidence_long_i %>%
      inner_join(
        prop_bact_fever %>%
          dplyr::select(Country, Year, draw_id, .imp_pc, .imp_dhs, p_u5_nonL),
        by = c("Country", "Year", "draw_id", ".imp_pc", ".imp_dhs")
      ) %>%
      mutate(
        outcome = "fever",
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        age_group = age_group_i,
        pred_bacterial_rate = pred_rate * p_u5_nonL
      )
    
    
    # LRTI incidence draws
    lrti_incidence_long_i <- wide_to_long(
      result_i[[lrti_spec_i]]$country_year_draws
    )
    
    prop_bact_lrti_i <- prop_bact_LRTI %>%
      transmute(
        draw_id,
        .imp_pc,
        .imp_dhs,
        prop_bact = .data[[lrti_prop_i]]
      )
    
    lrti_missing_i <- lrti_incidence_long_i %>%
      distinct(draw_id, .imp_pc, .imp_dhs) %>%
      anti_join(
        prop_bact_lrti_i %>%
          distinct(draw_id, .imp_pc, .imp_dhs),
        by = c("draw_id", ".imp_pc", ".imp_dhs")
      )
    
    if(nrow(lrti_missing_i) > 0){
      stop("Missing LRTI bacterial-fraction draws for ", wealth_tag_i, " | ", age_group_i)
    }
    
    inc_bact_lrti_long_i <- lrti_incidence_long_i %>%
      left_join(
        prop_bact_lrti_i,
        by = c("draw_id", ".imp_pc", ".imp_dhs")
      ) %>%
      mutate(
        outcome = "LRTI",
        wealth_level = wealth_level_i,
        wealth_tag = wealth_tag_i,
        age_group = age_group_i,
        pred_bacterial_rate = pred_rate * prop_bact
      )
    
    
    stopifnot(
      all(is.finite(inc_bact_fever_long_i$pred_bacterial_rate)),
      all(is.finite(inc_bact_lrti_long_i$pred_bacterial_rate))
    )
    
    
    # Metadata
    meta_incidence_i <- result_i[[fever_spec_i]]$prediction_grid %>%
      dplyr::select(
        Country,
        Year,
        any_of(c(
          "GDP",
          "Income",
          "gbd_region",
          "Endemic",
          "Population",
          "Population_under5",
          "age_population"
        ))
      ) %>%
      distinct(Country, Year, .keep_all = TRUE) %>%
      arrange(Country, Year)
    
    
    # Wide AFI bacterial-incidence draws
    inc_bact_fever_wide_i <- inc_bact_fever_long_i %>%
      mutate(draw_name = paste0("draw_", sprintf("%04d", draw_id))) %>%
      dplyr::select(Country, Year, draw_name, pred_bacterial_rate) %>%
      pivot_wider(names_from = draw_name, values_from = pred_bacterial_rate) %>%
      arrange(Country, Year)
    
    inc_bact_fever_i <- meta_incidence_i %>%
      left_join(inc_bact_fever_wide_i, by = c("Country", "Year")) %>%
      arrange(Country, Year)
    
    inc_bact_fever_matrix_i <- inc_bact_fever_wide_i %>%
      dplyr::select(-Country, -Year)
    
    
    # Wide LRTI bacterial-incidence draws
    inc_bact_lrti_wide_i <- inc_bact_lrti_long_i %>%
      mutate(draw_name = paste0("draw_", sprintf("%04d", draw_id))) %>%
      dplyr::select(Country, Year, draw_name, pred_bacterial_rate) %>%
      pivot_wider(names_from = draw_name, values_from = pred_bacterial_rate) %>%
      arrange(Country, Year)
    
    inc_bact_lrti_i <- meta_incidence_i %>%
      left_join(inc_bact_lrti_wide_i, by = c("Country", "Year")) %>%
      arrange(Country, Year)
    
    inc_bact_lrti_matrix_i <- inc_bact_lrti_wide_i %>%
      dplyr::select(-Country, -Year)
    
    
    stopifnot(
      ncol(inc_bact_fever_matrix_i) == 1000,
      ncol(inc_bact_lrti_matrix_i) == 1000
    )
    
    
    # Save same basic outputs as the previous Section 8
    saveRDS(
      inc_bact_fever_matrix_i,
      file.path(bacterial_output_dir, paste0("inc_bact_fever_", age_tag_i, "_matrix_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      inc_bact_lrti_matrix_i,
      file.path(bacterial_output_dir, paste0("inc_bact_lrti_", age_tag_i, "_matrix_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      inc_bact_fever_i,
      file.path(bacterial_output_dir, paste0("inc_bact_fever_", age_tag_i, "_", wealth_tag_i, ".rds"))
    )
    
    saveRDS(
      inc_bact_lrti_i,
      file.path(bacterial_output_dir, paste0("inc_bact_lrti_", age_tag_i, "_", wealth_tag_i, ".rds"))
    )
    
    
    # Primary long-format outputs retaining uncertainty identifiers
    saveRDS(
      inc_bact_fever_long_i,
      file.path(bacterial_output_dir, paste0("inc_bact_fever_", age_tag_i, "_", wealth_tag_i, "_long.rds"))
    )
    
    saveRDS(
      inc_bact_lrti_long_i,
      file.path(bacterial_output_dir, paste0("inc_bact_lrti_", age_tag_i, "_", wealth_tag_i, "_long.rds"))
    )
    
    
    fever_long_all_i[[j]] <- inc_bact_fever_long_i
    lrti_long_all_i[[j]] <- inc_bact_lrti_long_i
  }
  
  
  inc_bact_fever_all_age_i <- bind_rows(fever_long_all_i)
  inc_bact_lrti_all_age_i <- bind_rows(lrti_long_all_i)
  
  saveRDS(
    inc_bact_fever_all_age_i,
    file.path(bacterial_output_dir, paste0("inc_bact_fever_all_age_", wealth_tag_i, "_long.rds"))
  )
  
  saveRDS(
    inc_bact_lrti_all_age_i,
    file.path(bacterial_output_dir, paste0("inc_bact_lrti_all_age_", wealth_tag_i, "_long.rds"))
  )
  
  cat(
    "\nCompleted bacterial incidence for ",
    wealth_level_i,
    " (",
    wealth_tag_i,
    ")\n",
    sep = ""
  )
}

message("Section 8 completed for all five wealth scenarios.")`