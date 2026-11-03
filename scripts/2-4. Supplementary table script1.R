analysis_data_LRTI <- readRDS("outputs/pipelines/AA_LRTI_analysis_data.rds")
analysis_data_Fever <- readRDS("outputs/pipelines/AA_Fever_analysis_data.rds")

result_LRTI <- analysis_data_LRTI %>% 
  group_by(gbd_region, Country, Year, CountryYear) %>% 
  summarize(
    children = n(),
    n_Ab = sum(Ab_treatment == 1, na.rm = TRUE),
    prop = round(n_Ab / children, digits = 3),
    .groups = "drop_last"
  )

write.csv(result_LRTI, "outputs/supplementary/AA_LRTI_dhs_list.csv")


result_fever <- analysis_data_Fever %>% 
  group_by(gbd_region, Country, Year, CountryYear) %>% 
  summarize(
    children = n(),
    n_Ab = sum(Ab_treatment == 1, na.rm = TRUE),
    prop = round(n_Ab / children, digits = 3),
    .groups = "drop_last"
  )

write.csv(result_fever, "outputs/supplementary/AA_fever_dhs_list.csv")


UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(.imp_pc == 1 & Year == 2023) %>% dplyr::select(gbd_region, Country, Year, GDP, Endemic, Population)

result_UNI <- UNI %>% 
  group_by(gbd_region, Country, Year, GDP, Endemic, Population)

write.csv(result_LRTI, "outputs/supplementary/AA_LRTI_dhs_list.csv")




UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(.imp_pc == 1 & Year == 2023) %>% dplyr::select(gbd_region, Country, Year, GDP, Income, Endemic, Population)
dir.create("outputs/pipelines/suppl_table", recursive = TRUE, showWarnings = FALSE)
write.csv(UNI, "outputs/pipelines/suppl_table/target_country.csv")








prop_bact_fever <- readRDS("outputs/pipelines/bact_fever/bact_fever_draws_1000_long.rds") %>% filter(Year == 2023)
prop_bact_fever_summary <- prop_bact_fever %>%
  group_by(gbd_region, Country) %>%
  summarize(
    median = quantile(p_u5_nonL, 0.5, na.rm = TRUE),
    lower  = quantile(p_u5_nonL, 0.025, na.rm = TRUE),
    upper  = quantile(p_u5_nonL, 0.975, na.rm = TRUE),
    .groups = "drop")

write.csv(prop_bact_fever_summary, "outputs/pipelines/suppl_table/prop_bact_fever.csv")

          




lrti_incidence_age <- readRDS("outputs/pipelines/age_specific_selected_model_uncertainty/lrti_age_marginal_RE0_enet025_uncertainty_projection_draws.rds") %>% filter(Year == 2023)
fever_incidence_age <- readRDS("outputs/pipelines/age_specific_selected_model_uncertainty/fever_age_marginal_RE0_lm_uncertainty_projection_draws.rds") %>% filter(Year == 2023)

lrti_incidence_0 <- lrti_incidence_age %>% filter(age_group == "0-11 months")
lrti_incidence_1 <- lrti_incidence_age %>% filter(age_group == "12-23 months")
lrti_incidence_2 <- lrti_incidence_age %>% filter(age_group == "24-59 months")

fever_incidence_0 <- fever_incidence_age %>% filter(age_group == "0-11 months")
fever_incidence_1 <- fever_incidence_age %>% filter(age_group == "12-23 months")
fever_incidence_2 <- fever_incidence_age %>% filter(age_group == "24-59 months")


UNI                        <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(Year == 2023)
population                 <- read.csv("data/metadata/Population_by_age.csv")  %>% filter(Year == 2023)

setdiff(UNI$Country, population$Country)

country_mapping <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "United Republic of Tanzania" = "Tanzania",
  "T\x9frkiye" = "Turkiye")

population <- population %>% mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))
setdiff(UNI$Country, population$Country)

UNI <- UNI %>% dplyr::select("Country", "Year", "GDP", "Income", "gbd_region", "Endemic", "Population") %>% distinct()
UNI_pop <- UNI %>% inner_join(population, by = c("Country", "Year"))
UNI_pop <- UNI_pop %>% mutate(Age_2to4 = Age_2 + Age_3 + Age_4, Age_all = Age_0 + Age_1 + Age_2 + Age_3 + Age_4) %>% dplyr::select(Country, Year, Age_0, Age_1, Age_2to4, Age_all)

lrti_incidence_0$rate_all_age <- (lrti_incidence_0$pred_rate * UNI_pop$Age_0 +
                     lrti_incidence_1$pred_rate * UNI_pop$Age_1 +
                     lrti_incidence_2$pred_rate * UNI_pop$Age_2to4) / UNI_pop$Age_all

fever_incidence_0$rate_all_age <- (fever_incidence_0$pred_rate * UNI_pop$Age_0 +
                  fever_incidence_1$pred_rate * UNI_pop$Age_1 +
                  fever_incidence_2$pred_rate * UNI_pop$Age_2to4) / UNI_pop$Age_all

lrti_incidence_sumamry <- lrti_incidence_0 %>% group_by(gbd_region, Country) %>%
  summarize(
    median = quantile(rate_all_age, 0.5),
    lower = quantile(rate_all_age, 0.025),
    upper = quantile(rate_all_age, 0.975),
    .groups = "drop"
  )

fever_incidence_sumamry <- fever_incidence_0 %>% group_by(gbd_region, Country) %>%
  summarize(
    median = quantile(rate_all_age, 0.5),
    lower = quantile(rate_all_age, 0.025),
    upper = quantile(rate_all_age, 0.975),
    .groups = "drop"
  )

write.csv(lrti_incidence_sumamry, "outputs/pipelines/suppl_table/all_lrti_incidence.csv")
write.csv(fever_incidence_sumamry, "outputs/pipelines/suppl_table/all_fever_incidence.csv")
