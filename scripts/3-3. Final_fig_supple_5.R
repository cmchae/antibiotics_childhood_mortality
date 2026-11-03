
rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

options(scipen = 999)

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
row <- which(UNI$.imp_pc == 1 & UNI$Year == 2023 )
UNI <- UNI[row,]
population <- read.csv("data/metadata/Population_by_age.csv") 
population <- population %>% filter(Year == 2023)

# _____________________________________________ ----------
# 0. Data preparation -----------------------------

## 1. five wealth-specific matrix (inc, mort, AA)--------------
wealth_level_list <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
wealth_quintile_map <- c("Poorest" = 1L, "Poorer" = 2L, "Middle" = 3L, "Richer" = 4L, "Richest" = 5L)
wealth_list <- c("Poorest" = "q1", "Poorer" = "q2", "Middle" = "q3", "Richer" = "q4", "Richest" = "q5")

for (wealth_i in wealth_list) {
  assign(paste0(wealth_i, "_inc_bact_fever_0_11"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_0_11_matrix_", wealth_i, ".rds"))[row,])
  assign(paste0(wealth_i, "_inc_bact_fever_12_23"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_12_23_matrix_", wealth_i, ".rds"))[row,])
  assign(paste0(wealth_i, "_inc_bact_fever_24_59"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_24_59_matrix_", wealth_i, ".rds"))[row,])
  assign(paste0(wealth_i, "_inc_bact_lrti_0_11"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_0_11_matrix_", wealth_i, ".rds"))[row,])
  assign(paste0(wealth_i, "_inc_bact_lrti_12_23"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_12_23_matrix_", wealth_i, ".rds"))[row,])
  assign(paste0(wealth_i, "_inc_bact_lrti_24_59"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_24_59_matrix_", wealth_i, ".rds"))[row,])
}

spec_map <- c(
  lrti_0_11 = "LRTI_0_11_months",
  lrti_12_23 = "LRTI_12_23_months",
  lrti_24_59 = "LRTI_24_59_months",
  fever_0_11 = "fever_0_11_months",
  fever_12_23 = "fever_12_23_months",
  fever_24_59 = "fever_24_59_months"
)

for (wealth_i in wealth_list) {
  file_i <- file.path("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth", wealth_i, 
                      paste0("AA_all_age_specific_selected_model_uncertainty_results_", wealth_i, ".rds"))
  AA_bact_i <- readRDS(file_i)
  assign(paste0(wealth_i, "_AA_bact"), AA_bact_i)

  for (spec_i in names(spec_map)) {
    draws_i <- AA_bact_i[[spec_map[[spec_i]]]]$country_year_draws %>% filter(Year == 2023)
    draws_i <- draws_i[, 10:1009, drop = FALSE]
    assign(paste0(wealth_i, "_AA_bact_", spec_i), draws_i)
  }
}


lrti_map <- c("lrti_0_11", "lrti_12_23", 'lrti_24_59')

for (i in 1:5){
  for (j in 1:3){
  data_i <- readRDS("outputs/pipelines/mortality_by_wealth_quintile/bacterial_LRTI_death_rate_matrices_by_wealth_quintile.rds")
  draws_i <- as.data.frame(data_i[[3*(i-1)+j]])
  draws_i <- draws_i[row,]
  assign(paste0(wealth_list[i], "_mort_bact_", lrti_map[j]), draws_i)
  }
}

fever_map <- c("fever_0_11", "fever_12_23", 'fever_24_59')

for (i in 1:5){
  for (j in 1:3){
    data_i <- readRDS("outputs/pipelines/mortality_by_wealth_quintile/bacterial_fever_death_rate_matrices_by_wealth_quintile.rds")
    draws_i <- as.data.frame(data_i[[3*(i-1)+j]])
    draws_i <- draws_i[row,]
    assign(paste0(wealth_list[i], "_mort_bact_", fever_map[j]), draws_i)
  }
}

rm(list = c("AA_bact_i", "data_i", "draws_i", "q1_AA_bact", "q2_AA_bact", "q3_AA_bact", "q4_AA_bact", "q5_AA_bact"))
ls()




wealth_tags <- unname(wealth_list)

matrix_names <- c(
  "inc_bact_lrti_0_11", "inc_bact_lrti_12_23", "inc_bact_lrti_24_59",
  "mort_bact_lrti_0_11", "mort_bact_lrti_12_23", "mort_bact_lrti_24_59",
  "AA_bact_lrti_0_11", "AA_bact_lrti_12_23", "AA_bact_lrti_24_59",
  "inc_bact_fever_0_11", "inc_bact_fever_12_23", "inc_bact_fever_24_59",
  "mort_bact_fever_0_11", "mort_bact_fever_12_23", "mort_bact_fever_24_59",
  "AA_bact_fever_0_11", "AA_bact_fever_12_23", "AA_bact_fever_24_59"
)



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
  "T\x9frkiye" = "Turkiye"
)

population <- population %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), unname(country_mapping[Country]), Country))

population_unmatched <- setdiff(UNI$Country, population$Country)

UNI_pop <- UNI %>%
  inner_join(population, by = c("Country", "Year")) %>%
  mutate(Age_2to4 = Age_2 + Age_3 + Age_4, Age_all = Age_0 + Age_1 + Age_2to4) %>%
  dplyr::select(Country, Year, GDP, Age_0, Age_1, Age_2to4, Age_all)


wealth_matrix_list <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  wealth_matrix_list[[wealth_i]] <- setNames(vector("list", length(matrix_names)), matrix_names)
  
  for (matrix_i in matrix_names) {
    object_name_i <- paste0(wealth_i, "_", matrix_i)
    
    wealth_matrix_list[[wealth_i]][[matrix_i]] <- as.matrix(get(object_name_i))
  }
  
  matrix_nrow_i <- vapply(wealth_matrix_list[[wealth_i]], nrow, integer(1))
  matrix_ncol_i <- vapply(wealth_matrix_list[[wealth_i]], ncol, integer(1))
  
}


wealth_under5_results <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  data_i <- wealth_matrix_list[[wealth_i]]
  
  ## LRTI incidence: population-weighted rate
  
  inc_count_lrti_0_11_i <- sweep(data_i$inc_bact_lrti_0_11, 1, UNI_pop$Age_0, "*")
  inc_count_lrti_12_23_i <- sweep(data_i$inc_bact_lrti_12_23, 1, UNI_pop$Age_1, "*")
  inc_count_lrti_24_59_i <- sweep(data_i$inc_bact_lrti_24_59, 1, UNI_pop$Age_2to4, "*")
  
  inc_count_lrti_all_i <- inc_count_lrti_0_11_i + inc_count_lrti_12_23_i + inc_count_lrti_24_59_i
  inc_rate_lrti_i <- sweep(inc_count_lrti_all_i, 1, UNI_pop$Age_all, "/")
  
  
  ## LRTI mortality: population-weighted rate
  
  mort_count_lrti_0_11_i <- sweep(data_i$mort_bact_lrti_0_11, 1, UNI_pop$Age_0, "*")
  mort_count_lrti_12_23_i <- sweep(data_i$mort_bact_lrti_12_23, 1, UNI_pop$Age_1, "*")
  mort_count_lrti_24_59_i <- sweep(data_i$mort_bact_lrti_24_59, 1, UNI_pop$Age_2to4, "*")
  
  mort_count_lrti_all_i <- mort_count_lrti_0_11_i + mort_count_lrti_12_23_i + mort_count_lrti_24_59_i
  mort_rate_lrti_i <- sweep(mort_count_lrti_all_i, 1, UNI_pop$Age_all, "/")
  
  
  ## LRTI antibiotic treatment: case-weighted proportion
  
  treated_count_lrti_i <-
    data_i$AA_bact_lrti_0_11 * inc_count_lrti_0_11_i +
    data_i$AA_bact_lrti_12_23 * inc_count_lrti_12_23_i +
    data_i$AA_bact_lrti_24_59 * inc_count_lrti_24_59_i
  
  AA_rate_lrti_i <- treated_count_lrti_i / inc_count_lrti_all_i
  
  
  ## Fever incidence: population-weighted rate
  
  inc_count_fever_0_11_i <- sweep(data_i$inc_bact_fever_0_11, 1, UNI_pop$Age_0, "*")
  inc_count_fever_12_23_i <- sweep(data_i$inc_bact_fever_12_23, 1, UNI_pop$Age_1, "*")
  inc_count_fever_24_59_i <- sweep(data_i$inc_bact_fever_24_59, 1, UNI_pop$Age_2to4, "*")
  
  inc_count_fever_all_i <- inc_count_fever_0_11_i + inc_count_fever_12_23_i + inc_count_fever_24_59_i
  inc_rate_fever_i <- sweep(inc_count_fever_all_i, 1, UNI_pop$Age_all, "/")
  
  
  ## Fever mortality: population-weighted rate
  
  mort_count_fever_0_11_i <- sweep(data_i$mort_bact_fever_0_11, 1, UNI_pop$Age_0, "*")
  mort_count_fever_12_23_i <- sweep(data_i$mort_bact_fever_12_23, 1, UNI_pop$Age_1, "*")
  mort_count_fever_24_59_i <- sweep(data_i$mort_bact_fever_24_59, 1, UNI_pop$Age_2to4, "*")
  
  mort_count_fever_all_i <- mort_count_fever_0_11_i + mort_count_fever_12_23_i + mort_count_fever_24_59_i
  mort_rate_fever_i <- sweep(mort_count_fever_all_i, 1, UNI_pop$Age_all, "/")
  
  
  ## Fever antibiotic treatment: case-weighted proportion
  
  treated_count_fever_i <-
    data_i$AA_bact_fever_0_11 * inc_count_fever_0_11_i +
    data_i$AA_bact_fever_12_23 * inc_count_fever_12_23_i +
    data_i$AA_bact_fever_24_59 * inc_count_fever_24_59_i
  
  AA_rate_fever_i <- treated_count_fever_i / inc_count_fever_all_i
  
  
  ## Save results
  
  wealth_under5_results[[wealth_i]] <- list(
    inc_rate_lrti = inc_rate_lrti_i,
    mort_rate_lrti = mort_rate_lrti_i,
    AA_rate_lrti = AA_rate_lrti_i,
    inc_rate_fever = inc_rate_fever_i,
    mort_rate_fever = mort_rate_fever_i,
    AA_rate_fever = AA_rate_fever_i
  )
}



q1_inc_rate_lrti <- wealth_under5_results$q1$inc_rate_lrti
q1_mort_rate_lrti <- wealth_under5_results$q1$mort_rate_lrti
q1_AA_rate_lrti <- wealth_under5_results$q1$AA_rate_lrti
q1_inc_rate_fever <- wealth_under5_results$q1$inc_rate_fever
q1_mort_rate_fever <- wealth_under5_results$q1$mort_rate_fever
q1_AA_rate_fever <- wealth_under5_results$q1$AA_rate_fever





## 2. antibiotic effectiveness --------------------
load("data/abx_effect/parsFevSx.Rdata")
load("data/abx_effect/parsRespSx.Rdata" )

parsFevSx_order_i <- order(parsFevSx)
draw_position_i <- round(seq(from = 1, to = length(parsFevSx), length.out = 1000))
draw_index_i <- parsFevSx_order_i[draw_position_i]

parsFevSx_1000 <- parsFevSx[draw_index_i]
parsRespSx_1000 <- parsRespSx[draw_index_i]

par(mfrow = c(2,1))
hist(exp(parsFevSx_1000), breaks = seq(0, 2, by = 0.1), xlim = c(0, 1), freq = FALSE, main = "Fever: 1,000 draws", xlab = "Value")
hist(exp(parsRespSx_1000), breaks = seq(0, 2, by = 0.1), xlim = c(0, 1), freq = FALSE, main = "Respiratory: 1,000 draws", xlab = "Value")
hist(exp(parsFevSx), breaks = seq(0, 2, by = 0.1), xlim = c(0, 1), freq = FALSE, main = "Fever: 100,000 draws", xlab = "Value")
hist(exp(parsRespSx), breaks = seq(0, 2, by = 0.1), xlim = c(0, 1), freq = FALSE, main = "Respiratory: 100,000 draws", xlab = "Value")

Ab_fev_eff  <- 1 - exp(parsFevSx_1000)
Ab_lrti_eff <- 1 - exp(parsRespSx_1000) 

Ab_lrti_eff_matrix <- matrix(Ab_lrti_eff, nrow = nrow(q1_inc_rate_lrti), ncol = ncol(q1_inc_rate_lrti), byrow = TRUE)
Ab_fev_eff_matrix <- matrix(Ab_fev_eff, nrow = nrow(q1_inc_rate_lrti), ncol = ncol(q1_inc_rate_lrti), byrow = TRUE)

dim(Ab_lrti_eff_matrix)



























# _____________________________________________ ----------
# 1. mortality-to-incidence ratio with ineffective treatment -----------------------------

# Estimated mortality-to-incidence ratio attributable to lack of effective treatment
wealth_tags <- names(wealth_under5_results)
CMR_results_by_wealth <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  data_i <- wealth_under5_results[[wealth_i]]
  
  inc_rate_lrti_i <- as.matrix(data_i$inc_rate_lrti)
  mort_rate_lrti_i <- as.matrix(data_i$mort_rate_lrti)
  AA_rate_lrti_i <- as.matrix(data_i$AA_rate_lrti)
  
  inc_rate_fever_i <- as.matrix(data_i$inc_rate_fever)
  mort_rate_fever_i <- as.matrix(data_i$mort_rate_fever)
  AA_rate_fever_i <- as.matrix(data_i$AA_rate_fever)
  
  ## Central mortality rates among children not effectively treated
  CMR_lrti_i <- mort_rate_lrti_i / (inc_rate_lrti_i * (AA_rate_lrti_i * (1 - Ab_lrti_eff_matrix) + (1 - AA_rate_lrti_i)))
  CMR_fever_i <- mort_rate_fever_i / (inc_rate_fever_i * (AA_rate_fever_i * (1 - Ab_fev_eff_matrix) + (1 - AA_rate_fever_i)))
  
  ## Antibiotic-preventable fraction
  lrti_Ab_prop_i <- ((1 - AA_rate_lrti_i) * Ab_lrti_eff_matrix) / (1 - AA_rate_lrti_i * Ab_lrti_eff_matrix)
  fever_Ab_prop_i <- ((1 - AA_rate_fever_i) * Ab_fev_eff_matrix) / (1 - AA_rate_fever_i * Ab_fev_eff_matrix)
  
  ## Posterior summaries
  
  CMR_lrti_summary_i <- t(apply(CMR_lrti_i, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
  colnames(CMR_lrti_summary_i) <- c("median", "lower", "upper")
  
  CMR_fever_summary_i <- t(apply(CMR_fever_i, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
  colnames(CMR_fever_summary_i) <- c("median", "lower", "upper")
  
  lrti_Ab_prop_summary_i <- t(apply(lrti_Ab_prop_i, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
  colnames(lrti_Ab_prop_summary_i) <- c("median", "lower", "upper")
  
  fever_Ab_prop_summary_i <- t(apply(fever_Ab_prop_i, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
  colnames(fever_Ab_prop_summary_i) <- c("median", "lower", "upper")
  
  
  ## Add country-year information
  
  CMR_lrti_summary_i <- cbind(UNI_pop %>% dplyr::select(Country, Year, GDP), 
                              as.data.frame(CMR_lrti_summary_i))
  
  CMR_fever_summary_i <- cbind(UNI_pop %>% dplyr::select(Country, Year, GDP),
                               as.data.frame(CMR_fever_summary_i))
  
  lrti_Ab_prop_summary_i <- cbind(UNI_pop %>% dplyr::select(Country, Year, GDP),
                                  as.data.frame(lrti_Ab_prop_summary_i))
  
  fever_Ab_prop_summary_i <- cbind(UNI_pop %>% dplyr::select(Country, Year, GDP), 
                                   as.data.frame(fever_Ab_prop_summary_i))
  
  ## Save all matrices and summaries for this wealth quintile
  
  CMR_results_by_wealth[[wealth_i]] <- list(
    CMR_lrti = CMR_lrti_i,
    CMR_fever = CMR_fever_i,
    lrti_Ab_prop = lrti_Ab_prop_i,
    fever_Ab_prop = fever_Ab_prop_i,
    CMR_lrti_summary = CMR_lrti_summary_i,
    CMR_fever_summary = CMR_fever_summary_i,
    lrti_Ab_prop_summary = lrti_Ab_prop_summary_i,
    fever_Ab_prop_summary = fever_Ab_prop_summary_i
  )
}













## prop of averted by Ab: LRTI
lrti_Ab_prop_summary_q1 <- CMR_results_by_wealth$q1$lrti_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
lrti_Ab_prop_summary_q2 <- CMR_results_by_wealth$q2$lrti_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
lrti_Ab_prop_summary_q3 <- CMR_results_by_wealth$q3$lrti_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
lrti_Ab_prop_summary_q4 <- CMR_results_by_wealth$q4$lrti_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
lrti_Ab_prop_summary_q5 <- CMR_results_by_wealth$q5$lrti_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))

x_lim <- range(c(lrti_Ab_prop_summary_q1$log_GDP), na.rm = TRUE)

y_lim <- range(c(lrti_Ab_prop_summary_q1$median,
                 lrti_Ab_prop_summary_q2$median,
                 lrti_Ab_prop_summary_q3$median,
                 lrti_Ab_prop_summary_q4$median,
                 lrti_Ab_prop_summary_q5$median), na.rm = TRUE)

par(mfrow = c(1, 1))
plot(lrti_Ab_prop_summary_q1$log_GDP, lrti_Ab_prop_summary_q1$median, pch = "", xlim = x_lim, ylim = c(y_lim[1],y_lim[2]*1.05), xlab = "Log10 GDP per capita (PPP)", ylab = "Median Proportion treated with antibiotics")
points(lrti_Ab_prop_summary_q1$log_GDP, lrti_Ab_prop_summary_q1$median, cex = 1.1, pch = 16, col = adjustcolor("#D73027", alpha.f = 0.25))
points(lrti_Ab_prop_summary_q2$log_GDP, lrti_Ab_prop_summary_q2$median, cex = 1.1, pch = 16, col = adjustcolor("#F46D43", alpha.f = 0.25))
points(lrti_Ab_prop_summary_q3$log_GDP, lrti_Ab_prop_summary_q3$median, cex = 1.1, pch = 16, col = adjustcolor("grey20" , alpha.f = 0.25))
points(lrti_Ab_prop_summary_q4$log_GDP, lrti_Ab_prop_summary_q4$median, cex = 1.1, pch = 16, col = adjustcolor("#00A6D6", alpha.f = 0.25))
points(lrti_Ab_prop_summary_q5$log_GDP, lrti_Ab_prop_summary_q5$median, cex = 1.1, pch = 16, col = adjustcolor("#4575B4", alpha.f = 0.25))

abline(lm(median ~ log_GDP, data = lrti_Ab_prop_summary_q1), lty = 1, lwd = 1.5, col = "#D73027")
abline(lm(median ~ log_GDP, data = lrti_Ab_prop_summary_q2), lty = 1, lwd = 1.5, col = "#F46D43")
abline(lm(median ~ log_GDP, data = lrti_Ab_prop_summary_q3), lty = 1, lwd = 1.5, col = "grey20" )
abline(lm(median ~ log_GDP, data = lrti_Ab_prop_summary_q4), lty = 1, lwd = 1.5, col = "#00A6D6")
abline(lm(median ~ log_GDP, data = lrti_Ab_prop_summary_q5), lty = 1, lwd = 1.5, col = "#4575B4")

legend("bottomleft", legend = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
       lty = rep(1,5),
       lwd = rep(1.5,5), 
       col = c("#D73027","#F46D43","grey20","#00A6D6","#4575B4"),
       bty = "n", cex = 0.85)





## prop of averted by Ab: fever
fever_Ab_prop_summary_q1 <- CMR_results_by_wealth$q1$fever_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
fever_Ab_prop_summary_q2 <- CMR_results_by_wealth$q2$fever_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
fever_Ab_prop_summary_q3 <- CMR_results_by_wealth$q3$fever_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
fever_Ab_prop_summary_q4 <- CMR_results_by_wealth$q4$fever_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))
fever_Ab_prop_summary_q5 <- CMR_results_by_wealth$q5$fever_Ab_prop_summary %>% dplyr::select(Country, GDP, median) %>% mutate(log_GDP = log10(GDP))

x_lim <- range(c(fever_Ab_prop_summary_q1$log_GDP), na.rm = TRUE)

y_lim <- range(c(fever_Ab_prop_summary_q1$median,
                 fever_Ab_prop_summary_q2$median,
                 fever_Ab_prop_summary_q3$median,
                 fever_Ab_prop_summary_q4$median,
                 fever_Ab_prop_summary_q5$median), na.rm = TRUE)

par(mfrow = c(1, 1))
plot(fever_Ab_prop_summary_q1$log_GDP, fever_Ab_prop_summary_q1$median, pch = "", xlim = x_lim, ylim = c(y_lim[1],y_lim[2]*1.05), xlab = "Log10 GDP per capita (PPP)", ylab = "Median Proportion treated with antibiotics")
points(fever_Ab_prop_summary_q1$log_GDP, fever_Ab_prop_summary_q1$median, cex = 1.1, pch = 16, col = adjustcolor("#D73027", alpha.f = 0.25))
points(fever_Ab_prop_summary_q2$log_GDP, fever_Ab_prop_summary_q2$median, cex = 1.1, pch = 16, col = adjustcolor("#F46D43", alpha.f = 0.25))
points(fever_Ab_prop_summary_q3$log_GDP, fever_Ab_prop_summary_q3$median, cex = 1.1, pch = 16, col = adjustcolor("grey20" , alpha.f = 0.25))
points(fever_Ab_prop_summary_q4$log_GDP, fever_Ab_prop_summary_q4$median, cex = 1.1, pch = 16, col = adjustcolor("#00A6D6", alpha.f = 0.25))
points(fever_Ab_prop_summary_q5$log_GDP, fever_Ab_prop_summary_q5$median, cex = 1.1, pch = 16, col = adjustcolor("#4575B4", alpha.f = 0.25))

abline(lm(median ~ log_GDP, data = fever_Ab_prop_summary_q1), lty = 1, lwd = 1.5, col = "#D73027")
abline(lm(median ~ log_GDP, data = fever_Ab_prop_summary_q2), lty = 1, lwd = 1.5, col = "#F46D43")
abline(lm(median ~ log_GDP, data = fever_Ab_prop_summary_q3), lty = 1, lwd = 1.5, col = "grey20" )
abline(lm(median ~ log_GDP, data = fever_Ab_prop_summary_q4), lty = 1, lwd = 1.5, col = "#00A6D6")
abline(lm(median ~ log_GDP, data = fever_Ab_prop_summary_q5), lty = 1, lwd = 1.5, col = "#4575B4")

legend("bottomleft", legend = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
       lty = rep(1,5),
       lwd = rep(1.5,5), 
       col = c("#D73027","#F46D43","grey20","#00A6D6","#4575B4"),
       bty = "n", cex = 0.85)








summarize_CMR_draws_subgroup <- function(draw_matrix, metadata, wealth_tag, wealth_level) {
  draw_matrix <- as.matrix(draw_matrix)
  
  required_cols <- c("Country", "Year", "GDP", "Income", "gbd_region", "Population")
  missing_cols <- setdiff(required_cols, names(metadata))
  
  if (length(missing_cols) > 0) {
    stop("Missing metadata columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(draw_matrix) != nrow(metadata)) {
    stop(
      "Row mismatch: draw matrix has ", nrow(draw_matrix),
      " rows, but metadata has ", nrow(metadata), " rows."
    )
  }
  
  colnames(draw_matrix) <- paste0("draw_", seq_len(ncol(draw_matrix)))
  draw_cols <- colnames(draw_matrix)
  
  data_i <- dplyr::bind_cols(
    metadata,
    as.data.frame(draw_matrix)
  )
  
  
  ## 1. Country-level posterior summary ----------------------------------------
  
  summary_country_year <- metadata %>%
    dplyr::select(dplyr::any_of(c(
      "Country", "Year", "GDP", "Income", "gbd_region",
      "PC1", "PC2", "Endemic", "Population"
    ))) %>%
    dplyr::mutate(
      wealth_tag = wealth_tag,
      wealth_level = wealth_level,
      mean = rowMeans(draw_matrix, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  
  
  ## 2. Overall population-weighted summary -----------------------------------
  
  overall_draws <- data_i %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(draw_cols),
        ~ weighted.mean(.x, w = Population, na.rm = TRUE)
      )
    ) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    wealth_tag = wealth_tag,
    wealth_level = wealth_level,
    group = "Overall",
    mean = mean(overall_draws, na.rm = TRUE),
    lower = quantile(overall_draws, probs = 0.025, na.rm = TRUE),
    upper = quantile(overall_draws, probs = 0.975, na.rm = TRUE)
  )
  
  
  ## 3. Income-level population-weighted summary ------------------------------
  
  income_draws <- data_i %>%
    dplyr::filter(Income %in% c("L", "LM", "UM")) %>%
    dplyr::group_by(Income) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(draw_cols),
        ~ weighted.mean(.x, w = Population, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  income_matrix <- as.matrix(income_draws[, draw_cols, drop = FALSE])
  
  summary_income <- income_draws %>%
    dplyr::select(Income) %>%
    dplyr::mutate(
      wealth_tag = wealth_tag,
      wealth_level = wealth_level,
      mean = rowMeans(income_matrix, na.rm = TRUE),
      lower = apply(income_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(income_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  
  
  ## 4. GBD-region population-weighted summary --------------------------------
  
  region_draws <- data_i %>%
    dplyr::filter(!is.na(gbd_region)) %>%
    dplyr::group_by(gbd_region) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(draw_cols),
        ~ weighted.mean(.x, w = Population, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  region_matrix <- as.matrix(region_draws[, draw_cols, drop = FALSE])
  
  summary_gbd_region <- region_draws %>%
    dplyr::select(gbd_region) %>%
    dplyr::mutate(
      wealth_tag = wealth_tag,
      wealth_level = wealth_level,
      mean = rowMeans(region_matrix, na.rm = TRUE),
      lower = apply(region_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(region_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  
  
  list(
    overall = summary_overall,
    country_year = summary_country_year,
    income = summary_income,
    gbd_region = summary_gbd_region
  )
}




wealth_level_map <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)

wealth_tags <- names(CMR_results_by_wealth)

CMR_summary_by_wealth <- setNames(
  vector("list", length(wealth_tags)),
  wealth_tags
)

for (wealth_i in wealth_tags) {
  wealth_level_i <- unname(wealth_level_map[wealth_i])
  
  CMR_summary_by_wealth[[wealth_i]] <- list(
    CMR_lrti = summarize_CMR_draws_subgroup(
      CMR_results_by_wealth[[wealth_i]]$CMR_lrti,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    CMR_fever = summarize_CMR_draws_subgroup(
      CMR_results_by_wealth[[wealth_i]]$CMR_fever,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    incidence_lrti = summarize_CMR_draws_subgroup(
      wealth_under5_results[[wealth_i]]$inc_rate_lrti,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    incidence_fever = summarize_CMR_draws_subgroup(
      wealth_under5_results[[wealth_i]]$inc_rate_fever,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    mortality_lrti = summarize_CMR_draws_subgroup(
      wealth_under5_results[[wealth_i]]$mort_rate_lrti,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    mortality_fever = summarize_CMR_draws_subgroup(
      wealth_under5_results[[wealth_i]]$mort_rate_fever,
      UNI,
      wealth_i,
      wealth_level_i
    )
  )
}






# _____________________________________________ ----------
# 2. Preventable bacterial mortality by household-wealth scenario -------------

## 1. Data -------------
wealth_tags <- names(wealth_under5_results)
wealth_level_map <- c(q1 = "Poorest", q2 = "Poorer", q3 = "Middle", q4 = "Richer", q5 = "Richest")

preventable_results_by_wealth <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  data_i <- wealth_under5_results[[wealth_i]]
  CMR_data_i <- CMR_results_by_wealth[[wealth_i]]
  
  ## Input matrices
  
  inc_rate_lrti_i <- as.matrix(data_i$inc_rate_lrti)
  mort_rate_lrti_i <- as.matrix(data_i$mort_rate_lrti)
  AA_rate_lrti_i <- as.matrix(data_i$AA_rate_lrti)
  CMR_lrti_i <- as.matrix(CMR_data_i$CMR_lrti)
  
  inc_rate_fever_i <- as.matrix(data_i$inc_rate_fever)
  mort_rate_fever_i <- as.matrix(data_i$mort_rate_fever)
  AA_rate_fever_i <- as.matrix(data_i$AA_rate_fever)
  CMR_fever_i <- as.matrix(CMR_data_i$CMR_fever)
  
  stopifnot(
    identical(dim(inc_rate_lrti_i), dim(mort_rate_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(AA_rate_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(CMR_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(Ab_lrti_eff_matrix)),
    identical(dim(inc_rate_fever_i), dim(mort_rate_fever_i)),
    identical(dim(inc_rate_fever_i), dim(AA_rate_fever_i)),
    identical(dim(inc_rate_fever_i), dim(CMR_fever_i)),
    identical(dim(inc_rate_fever_i), dim(Ab_fev_eff_matrix))
  )
  
  
  ## Minimum CMR across countries for each uncertainty draw
  CMR_lrti_min_i <- apply(CMR_lrti_i, 2, min, na.rm = TRUE)
  CMR_fever_min_i <- apply(CMR_fever_i, 2, min, na.rm = TRUE)
  
  CMR_lrti_min_i[!is.finite(CMR_lrti_min_i)] <- NA_real_
  CMR_fever_min_i[!is.finite(CMR_fever_min_i)] <- NA_real_
  
  CMR_lrti_min_matrix_i <- matrix(CMR_lrti_min_i, nrow = nrow(CMR_lrti_i), ncol = ncol(CMR_lrti_i), byrow = TRUE)
  CMR_fever_min_matrix_i <- matrix(CMR_fever_min_i, nrow = nrow(CMR_fever_i), ncol = ncol(CMR_fever_i), byrow = TRUE)
  
  
  ## Incidence not effectively treated
  ineffective_inc_lrti_i <- inc_rate_lrti_i * (AA_rate_lrti_i * (1 - Ab_lrti_eff_matrix) + (1 - AA_rate_lrti_i))
  ineffective_inc_fever_i <- inc_rate_fever_i * (AA_rate_fever_i * (1 - Ab_fev_eff_matrix) + (1 - AA_rate_fever_i))

  ## Preventable mortality through improved antibiotic access
  mortality_lrti_Ab_preventable_i <- inc_rate_lrti_i * (1 - AA_rate_lrti_i) * Ab_lrti_eff_matrix * CMR_lrti_i
  mortality_fever_Ab_preventable_i <- inc_rate_fever_i * (1 - AA_rate_fever_i) * Ab_fev_eff_matrix * CMR_fever_i

  ## Preventable mortality through improvement in other factors
  mortality_lrti_other_preventable_i <- ineffective_inc_lrti_i * (CMR_lrti_i - CMR_lrti_min_matrix_i)
  mortality_fever_other_preventable_i <- ineffective_inc_fever_i * (CMR_fever_i - CMR_fever_min_matrix_i)
  
  ## Preventable mortality through both interventions
  mortality_lrti_both_preventable_i <- ineffective_inc_lrti_i * CMR_lrti_i - inc_rate_lrti_i * (1 - Ab_lrti_eff_matrix) * CMR_lrti_min_matrix_i
  mortality_fever_both_preventable_i <- ineffective_inc_fever_i * CMR_fever_i - inc_rate_fever_i * (1 - Ab_fev_eff_matrix) * CMR_fever_min_matrix_i
  
  ## Proportion of all preventable mortality attributable to antibiotic access
  proportion_lrti_Ab_preventable_i <- mortality_lrti_Ab_preventable_i / mortality_lrti_both_preventable_i
  proportion_fever_Ab_preventable_i <- mortality_fever_Ab_preventable_i / mortality_fever_both_preventable_i
  
  ## Proportion of observed mortality preventable through antibiotic access
  proportion_lrti_Ab_preventable_all_mortality_i <- mortality_lrti_Ab_preventable_i / mort_rate_lrti_i
  proportion_fever_Ab_preventable_all_mortality_i <- mortality_fever_Ab_preventable_i / mort_rate_fever_i
  
  
  ## Replace undefined ratios
  proportion_lrti_Ab_preventable_i[!is.finite(proportion_lrti_Ab_preventable_i)] <- NA_real_
  proportion_fever_Ab_preventable_i[!is.finite(proportion_fever_Ab_preventable_i)] <- NA_real_
  proportion_lrti_Ab_preventable_all_mortality_i[!is.finite(proportion_lrti_Ab_preventable_all_mortality_i)] <- NA_real_
  proportion_fever_Ab_preventable_all_mortality_i[!is.finite(proportion_fever_Ab_preventable_all_mortality_i)] <- NA_real_
  
  
  ## Store all matrices
  
  preventable_results_by_wealth[[wealth_i]] <- list(
    CMR_lrti_min = CMR_lrti_min_matrix_i,
    CMR_fever_min = CMR_fever_min_matrix_i,
    mortality_lrti_Ab_preventable = mortality_lrti_Ab_preventable_i,
    mortality_lrti_other_preventable = mortality_lrti_other_preventable_i,
    mortality_lrti_both_preventable = mortality_lrti_both_preventable_i,
    mortality_fever_Ab_preventable = mortality_fever_Ab_preventable_i,
    mortality_fever_other_preventable = mortality_fever_other_preventable_i,
    mortality_fever_both_preventable = mortality_fever_both_preventable_i,
    proportion_lrti_Ab_preventable = proportion_lrti_Ab_preventable_i,
    proportion_fever_Ab_preventable = proportion_fever_Ab_preventable_i,
    proportion_lrti_Ab_preventable_all_mortality = proportion_lrti_Ab_preventable_all_mortality_i,
    proportion_fever_Ab_preventable_all_mortality = proportion_fever_Ab_preventable_all_mortality_i
  )
}




preventable_summary_by_wealth <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  result_i <- preventable_results_by_wealth[[wealth_i]]
  wealth_level_i <- unname(wealth_level_map[wealth_i])
  
  preventable_summary_by_wealth[[wealth_i]] <- list(
    mortality_lrti_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_both_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_both_preventable, UNI, wealth_i, wealth_level_i)
  )
}



## 2. Summarize preventable mortality and proportions by wealth quintile --------

wealth_tags <- c("q1", "q2", "q3", "q4", "q5")

wealth_level_map <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)

required_metadata_cols <- c("Country", "Year", "GDP", "Income", "gbd_region", "Population")
missing_metadata_cols <- setdiff(required_metadata_cols, names(UNI))

if (length(missing_metadata_cols) > 0) {
  stop("Missing columns in UNI: ", paste(missing_metadata_cols, collapse = ", "))
}

preventable_summary_by_wealth <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  result_i <- preventable_results_by_wealth[[wealth_i]]
  wealth_level_i <- unname(wealth_level_map[wealth_i])
  
  stopifnot(
    nrow(result_i$mortality_lrti_Ab_preventable) == nrow(UNI),
    nrow(result_i$mortality_lrti_other_preventable) == nrow(UNI),
    nrow(result_i$mortality_lrti_both_preventable) == nrow(UNI),
    nrow(result_i$mortality_fever_Ab_preventable) == nrow(UNI),
    nrow(result_i$mortality_fever_other_preventable) == nrow(UNI),
    nrow(result_i$mortality_fever_both_preventable) == nrow(UNI),
    nrow(result_i$proportion_lrti_Ab_preventable_all_mortality) == nrow(UNI),
    nrow(result_i$proportion_fever_Ab_preventable_all_mortality) == nrow(UNI)
  )
  
  preventable_summary_by_wealth[[wealth_i]] <- list(
    mortality_lrti_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_both_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_both_preventable, UNI, wealth_i, wealth_level_i),
    proportion_lrti_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(result_i$proportion_lrti_Ab_preventable_all_mortality, UNI, wealth_i, wealth_level_i),
    proportion_fever_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(result_i$proportion_fever_Ab_preventable_all_mortality, UNI, wealth_i, wealth_level_i))
}

rm(result_i, wealth_i, wealth_level_i, missing_metadata_cols)



# (Suppl_Plot 4_sub_1) preventable mortality versus GDP, colored by household wealth --------

dir.create("figures/subgroup_analysis", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/subgroup_analysis/(Suppl_plot 4_subgroup) preventable mortality rate ~ GDP (color by wealth)_2023.pdf",
  width = 6,
  height = 3.25
)

layout(
  matrix(c(
    0, 1, 2, 3, 10, 4, 9,
    0, 5, 6, 7, 10, 8, 9
  ), nrow = 2, byrow = TRUE),
  widths = c(0.1, 1, 1, 1, 0.25, 1, 0.9),
  heights = c(1, 1)
)

par(oma = c(0, 0, 0, 0))
par(mar = c(2.0, 2.5, 0.8, 0.1))
par(mgp = c(0.35, 0.35, 0))
par(bty = "l")


## Household wealth colors and labels -----------------------------------------

wealth_cols_fixed <- c(
  q1 = "#D73027",
  q2 = "#F46D43",
  q3 = "grey20",
  q4 = "#00A6D6",
  q5 = "#4575B4"
)

wealth_labels <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)


## GDP axis --------------------------------------------------------------------

xmin <- 500
xmax <- 20000

x_tick_values <- round(10^(seq(log10(xmin), log10(xmax), length.out = 4)), digits = 0)
x_ticks <- log10(x_tick_values)
x_labels <- format(x_tick_values, big.mark = ",", scientific = FALSE)
xlim_log <- log10(c(xmin, xmax))

cex.axis_value <- 0.6
cex.axis_title <- 0.53
cex_title <- 0.55
cex_legend <- 0.6

plot_floor <- 1e-8



## 3. Prepare mortality-rate plotting data -------------------------------------

mortality_lrti_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )

mortality_lrti_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_other_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )

mortality_lrti_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )

mortality_fev_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )

mortality_fev_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_other_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )

mortality_fev_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  ) %>%
  dplyr::mutate(
    mean = pmax(mean, plot_floor),
    lower = pmax(lower, plot_floor),
    upper = pmax(upper, plot_floor)
  )



## 4. Prepare proportion plotting data -----------------------------------------

proportion_lrti_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$proportion_lrti_Ab_preventable_all_mortality$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

proportion_fev_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$proportion_fever_Ab_preventable_all_mortality$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )



## 5. Common logarithmic y-axis for mortality-rate panels -----------------------

all_y_values <- c(
  mortality_lrti_Ab_plot$lower,
  mortality_lrti_Ab_plot$upper,
  mortality_lrti_other_plot$lower,
  mortality_lrti_other_plot$upper,
  mortality_lrti_both_plot$lower,
  mortality_lrti_both_plot$upper,
  mortality_fev_Ab_plot$lower,
  mortality_fev_Ab_plot$upper,
  mortality_fev_other_plot$lower,
  mortality_fev_other_plot$upper,
  mortality_fev_both_plot$lower,
  mortality_fev_both_plot$upper
)

all_y_values <- all_y_values * 1000
all_y_values <- all_y_values[is.finite(all_y_values) & all_y_values > 0]

if (length(all_y_values) == 0) {
  stop("No finite positive mortality-rate values are available for plotting.")
}

common_y_exp_min <- floor(log10(min(all_y_values, na.rm = TRUE)))
common_y_exp_max <- ceiling(log10(max(all_y_values, na.rm = TRUE)))

if (common_y_exp_min == common_y_exp_max) {
  common_y_exp_min <- common_y_exp_min - 1
  common_y_exp_max <- common_y_exp_max + 1
}

common_y_ticks <- seq(-5, 1, by = 1)
common_y_labels <- parse(text = paste0("10^", common_y_ticks))
common_y_lim_log <- c(-5, 1)



### Panel 1. LRTI mortality preventable by antibiotics --------------------------

dat <- mortality_lrti_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("Preventable bacterial LRTI", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 2. LRTI mortality preventable by other factors ------------------------

dat <- mortality_lrti_other_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 3. LRTI mortality preventable by all factors --------------------------

dat <- mortality_lrti_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 4. Proportion of LRTI mortality preventable by antibiotics ------------

dat <- proportion_lrti_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

proportion_lrti_ticks <- seq(0, 0.4, by = 0.1)
proportion_lrti_labels <- format(proportion_lrti_ticks, nsmall = 1)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 0.4),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_lrti_ticks, labels = proportion_lrti_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = dat$lower,
  x1 = log10(dat$GDP), y1 = dat$upper,
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), dat$mean,
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)



### Panel 5. Fever mortality preventable by antibiotics -------------------------

dat <- mortality_fev_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("Preventable bacterial fever", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 6. Fever mortality preventable by other factors -----------------------

dat <- mortality_fev_other_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 7. Fever mortality preventable by all factors -------------------------

dat <- mortality_fev_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(dat$lower * 1000),
  x1 = log10(dat$GDP), y1 = log10(dat$upper * 1000),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(dat$mean * 1000),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)



### Panel 8. Proportion of fever mortality preventable by antibiotics -----------

dat <- proportion_fev_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

proportion_fev_ticks <- c(0, 0.16, 0.32, 0.48, 0.64)
proportion_fev_labels <- format(proportion_fev_ticks, nsmall = 2)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 0.64),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_fev_ticks, labels = proportion_fev_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = dat$lower,
  x1 = log10(dat$GDP), y1 = dat$upper,
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), dat$mean,
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)



### Panel 9. Household wealth legend --------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  x = 0.5,  y = 0.45,  xjust = 0.5,  yjust = 0.5,
  legend = unname(wealth_labels),
  title = "Household wealth quintile",
  col = adjustcolor(unname(wealth_cols_fixed), alpha.f = 0.80),
  pch = 16,
  lty = NA,
  lwd = NA,
  pt.cex = 0.6,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.5,
  bty = "n"
)



### Panel 10. Blank spacer -------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

dev.off()





































# _____________________________________________ ----------
# 3. Preventable bacterial mortality using each country's richest-quintile CMR -------------------------------

wealth_tags <- c("q1", "q2", "q3", "q4", "q5")

wealth_level_map <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)


## Literal minimum across q1-q5 instead of the richest q5 ----------------------
## Use these two expressions instead of the q5 matrices above only when the estimand is the lowest estimated CMR among all quintiles, regardless of which quintile produces it.

CMR_lrti_richest_reference <- pmin(
  as.matrix(CMR_results_by_wealth$q1$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q2$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q3$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q4$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q5$CMR_lrti),
  na.rm = TRUE
)

CMR_fever_richest_reference <- pmin(
  as.matrix(CMR_results_by_wealth$q1$CMR_fever),
  as.matrix(CMR_results_by_wealth$q2$CMR_fever),
  as.matrix(CMR_results_by_wealth$q3$CMR_fever),
  as.matrix(CMR_results_by_wealth$q4$CMR_fever),
  as.matrix(CMR_results_by_wealth$q5$CMR_fever),
  na.rm = TRUE
)

## Calculate preventable mortality --------------------------------------------

preventable_results_by_wealth <- setNames(
  vector("list", length(wealth_tags)),
  wealth_tags
)

for (wealth_i in wealth_tags) {
  data_i <- wealth_under5_results[[wealth_i]]
  CMR_data_i <- CMR_results_by_wealth[[wealth_i]]
  
  ## Input matrices
  
  inc_rate_lrti_i <- as.matrix(data_i$inc_rate_lrti)
  mort_rate_lrti_i <- as.matrix(data_i$mort_rate_lrti)
  AA_rate_lrti_i <- as.matrix(data_i$AA_rate_lrti)
  CMR_lrti_i <- as.matrix(CMR_data_i$CMR_lrti)
  
  inc_rate_fever_i <- as.matrix(data_i$inc_rate_fever)
  mort_rate_fever_i <- as.matrix(data_i$mort_rate_fever)
  AA_rate_fever_i <- as.matrix(data_i$AA_rate_fever)
  CMR_fever_i <- as.matrix(CMR_data_i$CMR_fever)

  ## Incidence not effectively treated
  ineffective_inc_lrti_i <- inc_rate_lrti_i * (AA_rate_lrti_i * (1 - Ab_lrti_eff_matrix) + (1 - AA_rate_lrti_i))
  ineffective_inc_fever_i <- inc_rate_fever_i * (AA_rate_fever_i * (1 - Ab_fev_eff_matrix) + (1 - AA_rate_fever_i))
  
  ## Observed mortality reconstructed from incidence and CMR
  observed_mortality_lrti_i <- ineffective_inc_lrti_i * CMR_lrti_i
  observed_mortality_fever_i <- ineffective_inc_fever_i * CMR_fever_i
  
  ## Counterfactual 1: improved antibiotic access only
  ## CMR remains at the current wealth-specific level.
  counterfactual_mortality_lrti_Ab_i <- inc_rate_lrti_i * (1 - Ab_lrti_eff_matrix) * CMR_lrti_i
  counterfactual_mortality_fever_Ab_i <- inc_rate_fever_i * (1 - Ab_fev_eff_matrix) * CMR_fever_i
  
  ## Mortality preventable through improved antibiotic access
  mortality_lrti_Ab_preventable_i <- observed_mortality_lrti_i - counterfactual_mortality_lrti_Ab_i
  mortality_fever_Ab_preventable_i <- observed_mortality_fever_i - counterfactual_mortality_fever_Ab_i
  
  ## Counterfactual 2: other circumstances changed to the richest-quintile level
  ## Antibiotic access remains at the current wealth-specific level.
  counterfactual_mortality_lrti_other_i <- ineffective_inc_lrti_i * CMR_lrti_richest_reference
  counterfactual_mortality_fever_other_i <- ineffective_inc_fever_i * CMR_fever_richest_reference

  ## Mortality preventable through improvement in other circumstances
  mortality_lrti_other_preventable_i <- observed_mortality_lrti_i - counterfactual_mortality_lrti_other_i
  mortality_fever_other_preventable_i <- observed_mortality_fever_i - counterfactual_mortality_fever_other_i
  
  ## Counterfactual 3: improved antibiotic access and richest circumstances
  counterfactual_mortality_lrti_both_i <- inc_rate_lrti_i * (1 - Ab_lrti_eff_matrix) * CMR_lrti_richest_reference
  counterfactual_mortality_fever_both_i <- inc_rate_fever_i * (1 - Ab_fev_eff_matrix) * CMR_fever_richest_reference

  ## Mortality preventable through both interventions
  mortality_lrti_both_preventable_i <- observed_mortality_lrti_i - counterfactual_mortality_lrti_both_i
  mortality_fever_both_preventable_i <- observed_mortality_fever_i - counterfactual_mortality_fever_both_i
  
  ## Proportion of all preventable mortality attributable to antibiotics
  proportion_lrti_Ab_preventable_i <- mortality_lrti_Ab_preventable_i / mortality_lrti_both_preventable_i
  proportion_fever_Ab_preventable_i <- mortality_fever_Ab_preventable_i / mortality_fever_both_preventable_i

  ## Proportion of observed mortality preventable through antibiotics
  proportion_lrti_Ab_preventable_all_mortality_i <- mortality_lrti_Ab_preventable_i / mort_rate_lrti_i
  proportion_fever_Ab_preventable_all_mortality_i <- mortality_fever_Ab_preventable_i / mort_rate_fever_i
  
  
  ## Replace undefined ratios
  proportion_lrti_Ab_preventable_i[!is.finite(proportion_lrti_Ab_preventable_i)] <- NA_real_
  proportion_fever_Ab_preventable_i[!is.finite(proportion_fever_Ab_preventable_i)] <- NA_real_
  proportion_lrti_Ab_preventable_all_mortality_i[!is.finite(proportion_lrti_Ab_preventable_all_mortality_i)] <- NA_real_
  proportion_fever_Ab_preventable_all_mortality_i[!is.finite(proportion_fever_Ab_preventable_all_mortality_i)] <- NA_real_
  
  
  ## Store matrices
  
  preventable_results_by_wealth[[wealth_i]] <- list(
    CMR_lrti_richest_reference = CMR_lrti_richest_reference,
    CMR_fever_richest_reference = CMR_fever_richest_reference,
    
    observed_mortality_lrti = observed_mortality_lrti_i,
    observed_mortality_fever = observed_mortality_fever_i,
    
    counterfactual_mortality_lrti_Ab = counterfactual_mortality_lrti_Ab_i,
    counterfactual_mortality_fever_Ab = counterfactual_mortality_fever_Ab_i,
    
    counterfactual_mortality_lrti_other = counterfactual_mortality_lrti_other_i,
    counterfactual_mortality_fever_other = counterfactual_mortality_fever_other_i,
    
    counterfactual_mortality_lrti_both = counterfactual_mortality_lrti_both_i,
    counterfactual_mortality_fever_both = counterfactual_mortality_fever_both_i,
    
    mortality_lrti_Ab_preventable = mortality_lrti_Ab_preventable_i,
    mortality_lrti_other_preventable = mortality_lrti_other_preventable_i,
    mortality_lrti_both_preventable = mortality_lrti_both_preventable_i,
    
    mortality_fever_Ab_preventable = mortality_fever_Ab_preventable_i,
    mortality_fever_other_preventable = mortality_fever_other_preventable_i,
    mortality_fever_both_preventable = mortality_fever_both_preventable_i,
    
    proportion_lrti_Ab_preventable = proportion_lrti_Ab_preventable_i,
    proportion_fever_Ab_preventable = proportion_fever_Ab_preventable_i,
    
    proportion_lrti_Ab_preventable_all_mortality = proportion_lrti_Ab_preventable_all_mortality_i,
    proportion_fever_Ab_preventable_all_mortality = proportion_fever_Ab_preventable_all_mortality_i
  )
}


# _____________________________________________ ----------
# 3-1. Summarize preventable mortality and proportions by wealth quintile --------

required_metadata_cols <- c(
  "Country",
  "Year",
  "GDP",
  "Income",
  "gbd_region",
  "Population"
)

missing_metadata_cols <- setdiff(required_metadata_cols, names(UNI))

preventable_summary_by_wealth <- setNames(
  vector("list", length(wealth_tags)),
  wealth_tags)

for (wealth_i in wealth_tags) {
  result_i <- preventable_results_by_wealth[[wealth_i]]
  wealth_level_i <- unname(wealth_level_map[wealth_i])
  
  preventable_summary_by_wealth[[wealth_i]] <- list(
    mortality_lrti_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_lrti_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_lrti_both_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_Ab_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_Ab_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_other_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_other_preventable, UNI, wealth_i, wealth_level_i),
    mortality_fever_both_preventable = summarize_CMR_draws_subgroup(result_i$mortality_fever_both_preventable, UNI, wealth_i, wealth_level_i),
    proportion_lrti_Ab_preventable = summarize_CMR_draws_subgroup(result_i$proportion_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i),
    proportion_fever_Ab_preventable = summarize_CMR_draws_subgroup(result_i$proportion_fever_Ab_preventable, UNI, wealth_i, wealth_level_i),
    proportion_lrti_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(result_i$proportion_lrti_Ab_preventable_all_mortality, UNI, wealth_i, wealth_level_i),
    proportion_fever_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(result_i$proportion_fever_Ab_preventable_all_mortality, UNI, wealth_i, wealth_level_i))
}












# (Suppl_Plot 4_sub_2) each country's richest household quintile as the CMR reference ---------------

dir.create("figures/subgroup_analysis", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/subgroup_analysis/(Suppl_Plot 4_subgroup) preventable mortality with richest household reference_2023.pdf",
  width = 6,
  height = 3.25
)

layout(
  matrix(c(
    0, 1, 2, 3, 10, 4, 9,
    0, 5, 6, 7, 10, 8, 9
  ), nrow = 2, byrow = TRUE),
  widths = c(0.1, 1, 1, 1, 0.25, 1, 0.9),
  heights = c(1, 1)
)

par(oma = c(0, 0, 0, 0))
par(mar = c(2.0, 2.5, 0.8, 0.1))
par(mgp = c(0.35, 0.35, 0))
par(bty = "l")


## Household wealth colors and labels -----------------------------------------

wealth_cols_fixed <- c(
  q1 = "#D73027",
  q2 = "#F46D43",
  q3 = "grey20",
  q4 = "#00A6D6",
  q5 = "#4575B4"
)

wealth_labels <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)

wealth_tags <- names(wealth_cols_fixed)


## GDP axis --------------------------------------------------------------------

xmin <- 500
xmax <- 20000

x_tick_values <- round(10^(seq(log10(xmin), log10(xmax), length.out = 4)), digits = 0)
x_ticks <- log10(x_tick_values)
x_labels <- format(x_tick_values, big.mark = ",", scientific = FALSE)
xlim_log <- log10(c(xmin, xmax))

cex.axis_value <- 0.6
cex.axis_title <- 0.53
cex_title <- 0.55
cex_legend <- 0.6



# 1. Prepare preventable mortality-rate plotting data -------------------------

mortality_lrti_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

mortality_lrti_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_other_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

mortality_lrti_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

mortality_fev_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

mortality_fev_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_other_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

mortality_fev_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )



# 2. Prepare proportion plotting data -----------------------------------------
#
# These proportions use:
#
# mortality preventable through antibiotics /
# mortality preventable through antibiotics and richest-household circumstances

proportion_lrti_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$proportion_lrti_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )

proportion_fev_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$proportion_fever_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper)
  )



# 3. Common logarithmic y-axis for mortality-rate panels -----------------------

all_y_values <- c(
  mortality_lrti_Ab_plot$lower,
  mortality_lrti_Ab_plot$upper,
  mortality_lrti_other_plot$lower,
  mortality_lrti_other_plot$upper,
  mortality_lrti_both_plot$lower,
  mortality_lrti_both_plot$upper,
  mortality_fev_Ab_plot$lower,
  mortality_fev_Ab_plot$upper,
  mortality_fev_other_plot$lower,
  mortality_fev_other_plot$upper,
  mortality_fev_both_plot$lower,
  mortality_fev_both_plot$upper
)

all_y_values <- all_y_values * 1000
all_y_values <- all_y_values[is.finite(all_y_values) & all_y_values > 0]

if (length(all_y_values) == 0) {
  stop("No finite positive mortality-rate values are available for plotting.")
}

common_y_exp_min <- floor(log10(min(all_y_values, na.rm = TRUE)))
common_y_exp_max <- ceiling(log10(max(all_y_values, na.rm = TRUE)))

if (common_y_exp_min == common_y_exp_max) {
  common_y_exp_min <- common_y_exp_min - 1
  common_y_exp_max <- common_y_exp_max + 1
}

common_y_ticks <- seq(-5, 1, by = 1)
common_y_labels <- parse(text = paste0("10^", common_y_ticks))
common_y_lim_log <- c(-5, 1)

mortality_floor_per1000 <- 10^common_y_exp_min



# Panel 1. LRTI mortality preventable by antibiotics --------------------------

dat <- mortality_lrti_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("Preventable bacterial LRTI", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



# Panel 2. LRTI mortality preventable by richest-household circumstances -------

dat <- mortality_lrti_other_plot %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::filter(!dplyr::near(mean, min(mean, na.rm = TRUE))) %>%
  dplyr::ungroup()

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)





# Panel 3. LRTI mortality preventable by both interventions --------------------

dat <- mortality_lrti_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)




# Panel 4. Antibiotic share of total preventable LRTI mortality ----------------

dat <- proportion_lrti_plot %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::filter(!dplyr::near(mean, max(mean, na.rm = TRUE))) %>%
  dplyr::ungroup()

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

proportion_ticks <- seq(0, 1, by = 0.25)
proportion_labels <- format(proportion_ticks, nsmall = 2)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 1),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_ticks, labels = proportion_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = pmax(dat$lower, 0),
  x1 = log10(dat$GDP), y1 = pmin(dat$upper, 1),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), pmin(pmax(dat$mean, 0), 1),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)



# Panel 5. Fever mortality preventable by antibiotics -------------------------

dat <- mortality_fev_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("Preventable bacterial fever", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



# Panel 6. Fever mortality preventable by richest-household circumstances ------

dat <- mortality_fev_other_plot %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::filter(!dplyr::near(mean, min(mean, na.rm = TRUE))) %>%
  dplyr::ungroup()

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)





# Panel 7. Fever mortality preventable by both interventions -------------------

dat <- mortality_fev_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)




# Panel 8. Antibiotic share of total preventable fever mortality ---------------

dat <- proportion_fev_plot %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::filter(!dplyr::near(mean, max(mean, na.rm = TRUE))) %>%
  dplyr::ungroup()

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 1),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_ticks, labels = proportion_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = pmax(dat$lower, 0),
  x1 = log10(dat$GDP), y1 = pmin(dat$upper, 1),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), pmin(pmax(dat$mean, 0), 1),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)


# Panel 9. Household wealth legend --------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  x = 0.5,  y = 0.45,  xjust = 0.5,  yjust = 0.5,
  legend = unname(wealth_labels),
  title = "Household wealth quintile",
  col = adjustcolor(unname(wealth_cols_fixed), alpha.f = 0.80),
  pch = 16,
  lty = NA,
  lwd = NA,
  pt.cex = 0.6,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.5,
  bty = "n"
)




# Panel 10. Blank spacer -------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

dev.off()




















# _____________________________________________ ----------
# 3-2. Absolute death preventable by improvement within country----------------------------------------------------

# Settings ---------------------------------------------------------------------

selected_year <- 2023
rate_scale <- 1

# rate_scale = 1 when mortality is expressed per child
# rate_scale = 1000 when mortality is expressed per 1,000 children

wealth_tags <- c("q1", "q2", "q3", "q4", "q5")
wealth_share_dhs <- c(
  q1 = 0.231,
  q2  = 0.214,
  q3  = 0.200,
  q4  = 0.188,
  q5 = 0.166
)
wealth_share_dhs <- wealth_share_dhs / sum(wealth_share_dhs)
# wealth_population_share <- c(q1 = 0.20, q2 = 0.20, q3 = 0.20, q4 = 0.20, q5 = 0.20)
wealth_population_share <- wealth_share_dhs

# Select 2023 metadata ---------------------------------------------------------

row_2023 <- which(UNI$Year == selected_year)
metadata_2023 <- UNI[row_2023, c("Country", "Year", "GDP", "Income", "gbd_region", "PC1", "PC2", "Endemic", "Population")]

# Convert wealth-specific rates to absolute numbers ---------------------------

number_preventable_results_by_wealth <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  population_i <- metadata_2023$Population * unname(wealth_population_share[wealth_i])
  
  # both_preventable for the best-within-country CMR reference
  mortality_lrti_rate_i <- as.matrix(
    preventable_results_by_wealth[[wealth_i]]$mortality_lrti_both_preventable
  )[row_2023, , drop = FALSE]
  
  mortality_fever_rate_i <- as.matrix(
    preventable_results_by_wealth[[wealth_i]]$mortality_fever_both_preventable
  )[row_2023, , drop = FALSE]

  number_lrti_i <- sweep(
    mortality_lrti_rate_i,
    MARGIN = 1,
    STATS = population_i / rate_scale,
    FUN = "*")
  
  number_fever_i <- sweep(
    mortality_fever_rate_i,
    MARGIN = 1,
    STATS = population_i / rate_scale,
    FUN = "*")
  
  number_preventable_results_by_wealth[[wealth_i]] <- list(
    mortality_lrti_both_preventable = number_lrti_i,
    mortality_fever_both_preventable = number_fever_i)
}


# Sum the five wealth groups within each country -------------------------------

matrix_number_lrti_both_preventable_2023 <-
  number_preventable_results_by_wealth$q1$mortality_lrti_both_preventable +
  number_preventable_results_by_wealth$q2$mortality_lrti_both_preventable +
  number_preventable_results_by_wealth$q3$mortality_lrti_both_preventable +
  number_preventable_results_by_wealth$q4$mortality_lrti_both_preventable +
  number_preventable_results_by_wealth$q5$mortality_lrti_both_preventable

matrix_number_fever_both_preventable_2023 <-
  number_preventable_results_by_wealth$q1$mortality_fever_both_preventable +
  number_preventable_results_by_wealth$q2$mortality_fever_both_preventable +
  number_preventable_results_by_wealth$q3$mortality_fever_both_preventable +
  number_preventable_results_by_wealth$q4$mortality_fever_both_preventable +
  number_preventable_results_by_wealth$q5$mortality_fever_both_preventable


# Country-level summaries ------------------------------------------------------

summary_country_lrti_both_preventable_2023 <- metadata_2023 %>%
  dplyr::mutate(
    median = apply(matrix_number_lrti_both_preventable_2023, 1, median, na.rm = TRUE),
    lower = apply(matrix_number_lrti_both_preventable_2023, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
    upper = apply(matrix_number_lrti_both_preventable_2023, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
  )

summary_country_fever_both_preventable_2023 <- metadata_2023 %>%
  dplyr::mutate(
    median = apply(matrix_number_fever_both_preventable_2023, 1, median, na.rm = TRUE),
    lower = apply(matrix_number_fever_both_preventable_2023, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
    upper = apply(matrix_number_fever_both_preventable_2023, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
  )

# 


# Overall summaries ------------------------------------------------------------

overall_lrti_both_preventable_2023_draw <- colSums(
  matrix_number_lrti_both_preventable_2023,
  na.rm = TRUE
)

overall_fever_both_preventable_2023_draw <- colSums(
  matrix_number_fever_both_preventable_2023,
  na.rm = TRUE
)

# (Manuscript) for manuscript calculation -------
within_country_both_draw <- overall_lrti_both_preventable_2023_draw + overall_fever_both_preventable_2023_draw 
write.csv(within_country_both_draw, "outputs/sub_results/within_country_both_draw.csv")

summary_overall_lrti_both_preventable_2023 <- data.frame(
  Year = selected_year,
  group = "Overall",
  median = median(overall_lrti_both_preventable_2023_draw, na.rm = TRUE),
  lower = quantile(overall_lrti_both_preventable_2023_draw, 0.025, na.rm = TRUE, names = FALSE),
  upper = quantile(overall_lrti_both_preventable_2023_draw, 0.975, na.rm = TRUE, names = FALSE)
)

summary_overall_fever_both_preventable_2023 <- data.frame(
  Year = selected_year,
  group = "Overall",
  median = median(overall_fever_both_preventable_2023_draw, na.rm = TRUE),
  lower = quantile(overall_fever_both_preventable_2023_draw, 0.025, na.rm = TRUE, names = FALSE),
  upper = quantile(overall_fever_both_preventable_2023_draw, 0.975, na.rm = TRUE, names = FALSE)
)


# Income-level draw aggregation ------------------------------------------------

income_levels <- c("L", "LM", "UM")

income_lrti_both_preventable_2023_draws <- matrix(
  NA_real_,
  nrow = length(income_levels),
  ncol = ncol(matrix_number_lrti_both_preventable_2023),
  dimnames = list(income_levels, NULL)
)

income_fever_both_preventable_2023_draws <- matrix(
  NA_real_,
  nrow = length(income_levels),
  ncol = ncol(matrix_number_fever_both_preventable_2023),
  dimnames = list(income_levels, NULL)
)

for (income_i in income_levels) {
  row_income_i <- which(metadata_2023$Income == income_i)
  
  income_lrti_both_preventable_2023_draws[income_i, ] <- colSums(
    matrix_number_lrti_both_preventable_2023[row_income_i, , drop = FALSE],
    na.rm = TRUE
  )
  
  income_fever_both_preventable_2023_draws[income_i, ] <- colSums(
    matrix_number_fever_both_preventable_2023[row_income_i, , drop = FALSE],
    na.rm = TRUE
  )
}

summary_income_lrti_both_preventable_2023 <- data.frame(
  Income = income_levels,
  Year = selected_year,
  median = apply(income_lrti_both_preventable_2023_draws, 1, median, na.rm = TRUE),
  lower = apply(income_lrti_both_preventable_2023_draws, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(income_lrti_both_preventable_2023_draws, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)

summary_income_fever_both_preventable_2023 <- data.frame(
  Income = income_levels,
  Year = selected_year,
  median = apply(income_fever_both_preventable_2023_draws, 1, median, na.rm = TRUE),
  lower = apply(income_fever_both_preventable_2023_draws, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(income_fever_both_preventable_2023_draws, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)


# GBD-region draw aggregation --------------------------------------------------

region_levels <- c(
  "Sub-Saharan Africa",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "North Africa and Middle East",
  "Latin America and Caribbean",
  "Central Europe, Eastern Europe, and Central Asia"
)

region_lrti_both_preventable_2023_draws <- matrix(
  NA_real_,
  nrow = length(region_levels),
  ncol = ncol(matrix_number_lrti_both_preventable_2023),
  dimnames = list(region_levels, NULL)
)

region_fever_both_preventable_2023_draws <- matrix(
  NA_real_,
  nrow = length(region_levels),
  ncol = ncol(matrix_number_fever_both_preventable_2023),
  dimnames = list(region_levels, NULL)
)

for (region_i in region_levels) {
  row_region_i <- which(metadata_2023$gbd_region == region_i)
  
  region_lrti_both_preventable_2023_draws[region_i, ] <- colSums(
    matrix_number_lrti_both_preventable_2023[row_region_i, , drop = FALSE],
    na.rm = TRUE
  )
  
  region_fever_both_preventable_2023_draws[region_i, ] <- colSums(
    matrix_number_fever_both_preventable_2023[row_region_i, , drop = FALSE],
    na.rm = TRUE
  )
}

summary_region_lrti_both_preventable_2023 <- data.frame(
  gbd_region = region_levels,
  Year = selected_year,
  median = apply(region_lrti_both_preventable_2023_draws, 1, median, na.rm = TRUE),
  lower = apply(region_lrti_both_preventable_2023_draws, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(region_lrti_both_preventable_2023_draws, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)

summary_region_fever_both_preventable_2023 <- data.frame(
  gbd_region = region_levels,
  Year = selected_year,
  median = apply(region_fever_both_preventable_2023_draws, 1, median, na.rm = TRUE),
  lower = apply(region_fever_both_preventable_2023_draws, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(region_fever_both_preventable_2023_draws, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)


# Combine summaries into the same structure used by the existing plot ---------

summary.number_lrti_both_preventable_best_country <- list(
  overall = summary_overall_lrti_both_preventable_2023,
  country = summary_country_lrti_both_preventable_2023,
  income = summary_income_lrti_both_preventable_2023,
  gbd_region = summary_region_lrti_both_preventable_2023
)

summary.number_fever_both_preventable_best_country <- list(
  overall = summary_overall_fever_both_preventable_2023,
  country = summary_country_fever_both_preventable_2023,
  income = summary_income_fever_both_preventable_2023,
  gbd_region = summary_region_fever_both_preventable_2023
)

write.csv(summary.number_lrti_both_preventable_best_country$country, "outputs/pipelines/suppl_table/preventable_deaths_wealth_within_country_lrti.csv")
write.csv(summary.number_fever_both_preventable_best_country$country, "outputs/pipelines/suppl_table/preventable_deaths_wealth_within_country_fever.csv")



# _____________________________________________ ----------
# Prepare data for the Figure 5-style plot -------------------------------------

category_levels <- c(
  "Total",
  "Low-income",
  "Lower-middle-income",
  "Upper-middle-income",
  "Sub-Saharan Africa",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "North Africa and Middle East",
  "Latin America and Caribbean",
  "Central Europe, Eastern Europe, and Central Asia"
)

category_cols <- c(
  "Total" = "#0072B2",
  "Low-income" = "#CC79A7",
  "Lower-middle-income" = "#009E73",
  "Upper-middle-income" = "#0072B2",
  "Sub-Saharan Africa" = "#0072B2",
  "South Asia" = "#A7B97F",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "North Africa and Middle East" = "#009E73",
  "Latin America and Caribbean" = "#E69F00",
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7"
)

plot_floor <- 1e-5

dat_lrti_best_country <- dplyr::bind_rows(
  summary.number_lrti_both_preventable_best_country$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_lrti_both_preventable_best_country$income %>%
    dplyr::transmute(
      group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"),
      median,
      lower,
      upper
    ),
  summary.number_lrti_both_preventable_best_country$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    median_plot = pmax(median, plot_floor),
    lower_plot = pmax(lower, plot_floor),
    upper_plot = pmax(upper, plot_floor),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

dat_fever_best_country <- dplyr::bind_rows(
  summary.number_fever_both_preventable_best_country$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_fever_both_preventable_best_country$income %>%
    dplyr::transmute(
      group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"),
      median,
      lower,
      upper
    ),
  summary.number_fever_both_preventable_best_country$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    median_plot = pmax(median, plot_floor),
    lower_plot = pmax(lower, plot_floor),
    upper_plot = pmax(upper, plot_floor),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )


# Common logarithmic axis ------------------------------------------------------

x_values <- c(
  dat_lrti_best_country$lower_plot,
  dat_lrti_best_country$upper_plot,
  dat_fever_best_country$lower_plot,
  dat_fever_best_country$upper_plot
)

x_values <- x_values[is.finite(x_values) & x_values > 0]

x_exp <- seq(floor(log10(min(x_values))), ceiling(log10(max(x_values))), by = 1)
x_ticks <- x_exp
x_labels <- parse(text = paste0("10^", x_exp))
xlim_log <- range(x_ticks)


# Plot settings ----------------------------------------------------------------

category_y <- rev(seq_along(category_levels))
divider_y <- c(category_y[1] - 0.5, category_y[4] - 0.5)
ylim_panel <- c(0.5, length(category_levels) + 1)
text_title_y <- length(category_levels) + 0.8

cex_axis_value <- 0.70
cex_category <- 0.70
cex_axis_title <- 0.55
cex_title <- 0.60
cex_point <- 0.70
cex_text <- 0.70
cex_text_title <- 0.7

lwd_interval <- 0.8
lwd_divider <- 1.2
alpha_interval <- 0.40
alpha_point <- 0.80
axis_tick_length <- -0.025


# (Supple 5_2) Create plot ------------------------------------------------------------------

dir.create("figures/main_analysis/mortality", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/subgroup_analysis/(plot 5_subgroup) absolute preventable deaths_best within country.pdf",
  width = 6,
  height = 2,
  useDingbats = FALSE
)

layout(
  matrix(c(1, 2, 3, 4, 5), nrow = 1),
  widths = c(1.5, 0.75, 0.85, 0.75, 0.85)
)


# Category labels --------------------------------------------------------------
 
par(mar = c(2.3, 0.2, 0.5, 0.1), mgp = c(0.35, 0.35, 0), bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  rep(0.98, length(category_levels)),
  category_y,
  labels = category_levels,
  adj = c(1, 0.5),
  cex = cex_category,
  xpd = NA
)


# Bacterial LRTI plot ----------------------------------------------------------

dat <- dat_lrti_best_country

par(mar = c(2.3, 0.3, 0.5, 0.2), mgp = c(0.35, 0.35, 0), bty = "l")

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = x_ticks,
  labels = x_labels,
  cex.axis = cex_axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  rep(xlim_log[1], length(divider_y)),
  divider_y,
  rep(xlim_log[2], length(divider_y)),
  divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  log10(dat$lower_plot),
  category_y,
  log10(dat$upper_plot),
  category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median_plot),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex_axis_title)


# Bacterial LRTI result text ---------------------------------------------------

par(mar = c(2.3, 0.1, 0.5, 0.1), mgp = c(0.35, 0.35, 0), bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  0.5,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(0.5, nrow(dat_lrti_best_country)),
  category_y,
  labels = dat_lrti_best_country$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)


# Bacterial fever plot ---------------------------------------------------------

dat <- dat_fever_best_country

par(mar = c(2.3, 0.3, 0.5, 0.2), mgp = c(0.35, 0.35, 0), bty = "l")

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = x_ticks,
  labels = x_labels,
  cex.axis = cex_axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  rep(xlim_log[1], length(divider_y)),
  divider_y,
  rep(xlim_log[2], length(divider_y)),
  divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  log10(dat$lower_plot),
  category_y,
  log10(dat$upper_plot),
  category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median_plot),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex_axis_title)


# Bacterial fever result text --------------------------------------------------

par(mar = c(2.3, 0.1, 0.5, 0.1), mgp = c(0.35, 0.35, 0), bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  0.5,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(0.5, nrow(dat_fever_best_country)),
  category_y,
  labels = dat_fever_best_country$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

dev.off()




















rm(data_i, CMR_data_i, result_i, wealth_i, wealth_level_i, missing_metadata_cols)








# _____________________________________________ ----------
# 4. Preventable bacterial mortality using the best country-wealth CMR ---------
#
# Counterfactual:
# All countries and household wealth quintiles have the same non-antibiotic
# circumstances as the wealth subgroup with the lowest CMR across all
# countries and wealth quintiles, separately for each uncertainty draw.

wealth_tags <- c("q1", "q2", "q3", "q4", "q5")

wealth_level_map <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)


## 1. Combine CMR matrices across all countries and wealth quintiles -----------

CMR_lrti_all_country_wealth <- rbind(
  as.matrix(CMR_results_by_wealth$q1$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q2$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q3$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q4$CMR_lrti),
  as.matrix(CMR_results_by_wealth$q5$CMR_lrti)
)

CMR_fever_all_country_wealth <- rbind(
  as.matrix(CMR_results_by_wealth$q1$CMR_fever),
  as.matrix(CMR_results_by_wealth$q2$CMR_fever),
  as.matrix(CMR_results_by_wealth$q3$CMR_fever),
  as.matrix(CMR_results_by_wealth$q4$CMR_fever),
  as.matrix(CMR_results_by_wealth$q5$CMR_fever)
)


## 2. Lowest CMR across all countries and wealth quintiles for each draw -------

CMR_lrti_global_best_draw <- apply(CMR_lrti_all_country_wealth, 2, min, na.rm = TRUE)
CMR_fever_global_best_draw <- apply(CMR_fever_all_country_wealth, 2, min, na.rm = TRUE)

CMR_lrti_global_best_draw[!is.finite(CMR_lrti_global_best_draw)] <- NA_real_
CMR_fever_global_best_draw[!is.finite(CMR_fever_global_best_draw)] <- NA_real_


## 3. Repeat the global reference across all countries -------------------------

CMR_lrti_global_best_reference <- matrix(
  CMR_lrti_global_best_draw,
  nrow = nrow(CMR_results_by_wealth$q1$CMR_lrti),
  ncol = ncol(CMR_results_by_wealth$q1$CMR_lrti),
  byrow = TRUE
)

CMR_fever_global_best_reference <- matrix(
  CMR_fever_global_best_draw,
  nrow = nrow(CMR_results_by_wealth$q1$CMR_fever),
  ncol = ncol(CMR_results_by_wealth$q1$CMR_fever),
  byrow = TRUE
)

stopifnot(
  identical(dim(CMR_lrti_global_best_reference), dim(Ab_lrti_eff_matrix)),
  identical(dim(CMR_fever_global_best_reference), dim(Ab_fev_eff_matrix))
)


## 4. identify the best country and wealth group for each draw -------

n_country <- nrow(CMR_results_by_wealth$q1$CMR_lrti)

best_lrti_row_by_draw <- apply(CMR_lrti_all_country_wealth, 2, which.min)
best_fever_row_by_draw <- apply(CMR_fever_all_country_wealth, 2, which.min)

best_lrti_country_row <- ((best_lrti_row_by_draw - 1) %% n_country) + 1
best_fever_country_row <- ((best_fever_row_by_draw - 1) %% n_country) + 1

best_lrti_wealth_number <- ((best_lrti_row_by_draw - 1) %/% n_country) + 1
best_fever_wealth_number <- ((best_fever_row_by_draw - 1) %/% n_country) + 1

global_best_lrti_reference_by_draw <- data.frame(
  draw = seq_along(CMR_lrti_global_best_draw),
  Country = UNI$Country[best_lrti_country_row],
  wealth_tag = wealth_tags[best_lrti_wealth_number],
  wealth_level = unname(wealth_level_map[wealth_tags[best_lrti_wealth_number]]),
  CMR = CMR_lrti_global_best_draw
)

global_best_fever_reference_by_draw <- data.frame(
  draw = seq_along(CMR_fever_global_best_draw),
  Country = UNI$Country[best_fever_country_row],
  wealth_tag = wealth_tags[best_fever_wealth_number],
  wealth_level = unname(wealth_level_map[wealth_tags[best_fever_wealth_number]]),
  CMR = CMR_fever_global_best_draw
)


## 5. Calculate preventable mortality -----------------------------------------

preventable_results_global_best <- setNames(vector("list", length(wealth_tags)), wealth_tags)

for (wealth_i in wealth_tags) {
  data_i <- wealth_under5_results[[wealth_i]]
  CMR_data_i <- CMR_results_by_wealth[[wealth_i]]
  
  ## Input matrices
  
  inc_rate_lrti_i <- as.matrix(data_i$inc_rate_lrti)
  mort_rate_lrti_i <- as.matrix(data_i$mort_rate_lrti)
  AA_rate_lrti_i <- as.matrix(data_i$AA_rate_lrti)
  CMR_lrti_i <- as.matrix(CMR_data_i$CMR_lrti)
  
  inc_rate_fever_i <- as.matrix(data_i$inc_rate_fever)
  mort_rate_fever_i <- as.matrix(data_i$mort_rate_fever)
  AA_rate_fever_i <- as.matrix(data_i$AA_rate_fever)
  CMR_fever_i <- as.matrix(CMR_data_i$CMR_fever)
  
  stopifnot(
    identical(dim(inc_rate_lrti_i), dim(mort_rate_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(AA_rate_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(CMR_lrti_i)),
    identical(dim(inc_rate_lrti_i), dim(Ab_lrti_eff_matrix)),
    identical(dim(inc_rate_lrti_i), dim(CMR_lrti_global_best_reference)),
    identical(dim(inc_rate_fever_i), dim(mort_rate_fever_i)),
    identical(dim(inc_rate_fever_i), dim(AA_rate_fever_i)),
    identical(dim(inc_rate_fever_i), dim(CMR_fever_i)),
    identical(dim(inc_rate_fever_i), dim(Ab_fev_eff_matrix)),
    identical(dim(inc_rate_fever_i), dim(CMR_fever_global_best_reference))
  )
  
  
  ## Incidence not effectively treated
  
  ineffective_inc_lrti_i <- inc_rate_lrti_i * (
    AA_rate_lrti_i * (1 - Ab_lrti_eff_matrix) +
      (1 - AA_rate_lrti_i)
  )
  
  ineffective_inc_fever_i <- inc_rate_fever_i * (
    AA_rate_fever_i * (1 - Ab_fev_eff_matrix) +
      (1 - AA_rate_fever_i)
  )
  
  
  ## Observed mortality reconstructed from incidence and CMR
  
  observed_mortality_lrti_i <- ineffective_inc_lrti_i * CMR_lrti_i
  observed_mortality_fever_i <- ineffective_inc_fever_i * CMR_fever_i
  
  
  ## Counterfactual 1: complete antibiotic access only
  ## Current wealth- and country-specific CMR remains unchanged.
  
  counterfactual_mortality_lrti_Ab_i <-
    inc_rate_lrti_i *
    (1 - Ab_lrti_eff_matrix) *
    CMR_lrti_i
  
  counterfactual_mortality_fever_Ab_i <-
    inc_rate_fever_i *
    (1 - Ab_fev_eff_matrix) *
    CMR_fever_i
  
  
  ## Mortality preventable through improved antibiotic access
  
  mortality_lrti_Ab_preventable_i <-
    observed_mortality_lrti_i -
    counterfactual_mortality_lrti_Ab_i
  
  mortality_fever_Ab_preventable_i <-
    observed_mortality_fever_i -
    counterfactual_mortality_fever_Ab_i
  
  
  ## Counterfactual 2: circumstances changed to global best CMR
  ## Antibiotic access remains at its current wealth-specific level.
  
  counterfactual_mortality_lrti_other_i <-
    ineffective_inc_lrti_i *
    CMR_lrti_global_best_reference
  
  counterfactual_mortality_fever_other_i <-
    ineffective_inc_fever_i *
    CMR_fever_global_best_reference
  
  
  ## Mortality preventable through improvement in other circumstances
  
  mortality_lrti_other_preventable_i <-
    observed_mortality_lrti_i -
    counterfactual_mortality_lrti_other_i
  
  mortality_fever_other_preventable_i <-
    observed_mortality_fever_i -
    counterfactual_mortality_fever_other_i
  
  
  ## Counterfactual 3: complete antibiotic access and global best CMR
  
  counterfactual_mortality_lrti_both_i <-
    inc_rate_lrti_i *
    (1 - Ab_lrti_eff_matrix) *
    CMR_lrti_global_best_reference
  
  counterfactual_mortality_fever_both_i <-
    inc_rate_fever_i *
    (1 - Ab_fev_eff_matrix) *
    CMR_fever_global_best_reference
  
  
  ## Mortality preventable through both interventions
  
  mortality_lrti_both_preventable_i <-
    observed_mortality_lrti_i -
    counterfactual_mortality_lrti_both_i
  
  mortality_fever_both_preventable_i <-
    observed_mortality_fever_i -
    counterfactual_mortality_fever_both_i
  
  
  ## Antibiotic share of all preventable mortality
  
  proportion_lrti_Ab_preventable_i <-
    mortality_lrti_Ab_preventable_i /
    mortality_lrti_both_preventable_i
  
  proportion_fever_Ab_preventable_i <-
    mortality_fever_Ab_preventable_i /
    mortality_fever_both_preventable_i
  
  
  ## Proportion of observed mortality preventable through antibiotics
  
  proportion_lrti_Ab_preventable_all_mortality_i <-
    mortality_lrti_Ab_preventable_i /
    mort_rate_lrti_i
  
  proportion_fever_Ab_preventable_all_mortality_i <-
    mortality_fever_Ab_preventable_i /
    mort_rate_fever_i
  
  
  ## Replace undefined ratios
  
  proportion_lrti_Ab_preventable_i[
    !is.finite(proportion_lrti_Ab_preventable_i)
  ] <- NA_real_
  
  proportion_fever_Ab_preventable_i[
    !is.finite(proportion_fever_Ab_preventable_i)
  ] <- NA_real_
  
  proportion_lrti_Ab_preventable_all_mortality_i[
    !is.finite(proportion_lrti_Ab_preventable_all_mortality_i)
  ] <- NA_real_
  
  proportion_fever_Ab_preventable_all_mortality_i[
    !is.finite(proportion_fever_Ab_preventable_all_mortality_i)
  ] <- NA_real_
  
  
  ## Store matrices
  
  preventable_results_global_best[[wealth_i]] <- list(
    CMR_lrti_global_best_reference = CMR_lrti_global_best_reference,
    CMR_fever_global_best_reference = CMR_fever_global_best_reference,
    
    observed_mortality_lrti = observed_mortality_lrti_i,
    observed_mortality_fever = observed_mortality_fever_i,
    
    counterfactual_mortality_lrti_Ab = counterfactual_mortality_lrti_Ab_i,
    counterfactual_mortality_fever_Ab = counterfactual_mortality_fever_Ab_i,
    
    counterfactual_mortality_lrti_other = counterfactual_mortality_lrti_other_i,
    counterfactual_mortality_fever_other = counterfactual_mortality_fever_other_i,
    
    counterfactual_mortality_lrti_both = counterfactual_mortality_lrti_both_i,
    counterfactual_mortality_fever_both = counterfactual_mortality_fever_both_i,
    
    mortality_lrti_Ab_preventable = mortality_lrti_Ab_preventable_i,
    mortality_lrti_other_preventable = mortality_lrti_other_preventable_i,
    mortality_lrti_both_preventable = mortality_lrti_both_preventable_i,
    
    mortality_fever_Ab_preventable = mortality_fever_Ab_preventable_i,
    mortality_fever_other_preventable = mortality_fever_other_preventable_i,
    mortality_fever_both_preventable = mortality_fever_both_preventable_i,
    
    proportion_lrti_Ab_preventable = proportion_lrti_Ab_preventable_i,
    proportion_fever_Ab_preventable = proportion_fever_Ab_preventable_i,
    
    proportion_lrti_Ab_preventable_all_mortality =
      proportion_lrti_Ab_preventable_all_mortality_i,
    
    proportion_fever_Ab_preventable_all_mortality =
      proportion_fever_Ab_preventable_all_mortality_i
  )
}


## 6. Summarize global-best-reference results by wealth quintile ----------------

required_metadata_cols <- c(
  "Country",
  "Year",
  "GDP",
  "Income",
  "gbd_region",
  "Population"
)

missing_metadata_cols <- setdiff(required_metadata_cols, names(UNI))

if (length(missing_metadata_cols) > 0) {
  stop("Missing columns in UNI: ", paste(missing_metadata_cols, collapse = ", "))
}

preventable_summary_global_best <- setNames(
  vector("list", length(wealth_tags)),
  wealth_tags
)

for (wealth_i in wealth_tags) {
  result_i <- preventable_results_global_best[[wealth_i]]
  wealth_level_i <- unname(wealth_level_map[wealth_i])
  
  preventable_summary_global_best[[wealth_i]] <- list(
    mortality_lrti_Ab_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    mortality_lrti_other_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_lrti_other_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    mortality_lrti_both_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_lrti_both_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    mortality_fever_Ab_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_fever_Ab_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    mortality_fever_other_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_fever_other_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    mortality_fever_both_preventable = summarize_CMR_draws_subgroup(
      result_i$mortality_fever_both_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    proportion_lrti_Ab_preventable = summarize_CMR_draws_subgroup(
      result_i$proportion_lrti_Ab_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    proportion_fever_Ab_preventable = summarize_CMR_draws_subgroup(
      result_i$proportion_fever_Ab_preventable, UNI, wealth_i, wealth_level_i
    ),
    
    proportion_lrti_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(
      result_i$proportion_lrti_Ab_preventable_all_mortality,
      UNI,
      wealth_i,
      wealth_level_i
    ),
    
    proportion_fever_Ab_preventable_all_mortality = summarize_CMR_draws_subgroup(
      result_i$proportion_fever_Ab_preventable_all_mortality,
      UNI,
      wealth_i,
      wealth_level_i
    )
  )
}






















# (Suppl_Plot 4_sub_3) each country's richest household quintile as the CMR reference ---------------

dir.create("figures/subgroup_analysis", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/subgroup_analysis/(Supple 5) preventable mortality with best country_household reference for each country and household wealth_2023.pdf",
  width = 6,
  height = 3.25
)

layout(
  matrix(c(
    0, 1, 2, 3, 10, 4, 9,
    0, 5, 6, 7, 10, 8, 9
  ), nrow = 2, byrow = TRUE),
  widths = c(0.1, 1, 1, 1, 0.25, 1, 0.9),
  heights = c(1, 1)
)

par(oma = c(0, 0, 0, 0))
par(mar = c(2.0, 2.5, 0.8, 0.1))
par(mgp = c(0.35, 0.35, 0))
par(bty = "l")


## Household wealth colors and labels -----------------------------------------

wealth_cols_fixed <- c(
  q1 = "#D73027",
  q2 = "#F46D43",
  q3 = "grey20",
  q4 = "#00A6D6",
  q5 = "#4575B4"
)

wealth_labels <- c(
  q1 = "Poorest",
  q2 = "Poorer",
  q3 = "Middle",
  q4 = "Richer",
  q5 = "Richest"
)

wealth_tags <- names(wealth_cols_fixed)


## GDP axis --------------------------------------------------------------------

xmin <- 500
xmax <- 20000

x_tick_values <- round(10^(seq(log10(xmin), log10(xmax), length.out = 4)), digits = 0)
x_ticks <- log10(x_tick_values)
x_labels <- format(x_tick_values, big.mark = ",", scientific = FALSE)
xlim_log <- log10(c(xmin, xmax))

cex.axis_value <- 0.6
cex.axis_title <- 0.53
cex_title <- 0.55
cex_legend <- 0.6



# 1. Prepare preventable mortality-rate plotting data -------------------------


mortality_lrti_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_lrti_Ab_preventable$country_year
  })
)

mortality_lrti_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_lrti_other_preventable$country_year
  })
)

mortality_lrti_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_lrti_both_preventable$country_year
  })
)

mortality_fev_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_fever_Ab_preventable$country_year
  })
)

mortality_fev_other_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_fever_other_preventable$country_year
  })
)

mortality_fev_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$mortality_fever_both_preventable$country_year
  })
)

proportion_lrti_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$proportion_lrti_Ab_preventable$country_year
  })
)

proportion_fev_plot <- dplyr::bind_rows(
  lapply(preventable_summary_global_best, function(x) {
    x$proportion_fever_Ab_preventable$country_year
  })
)

# 2. Common logarithmic y-axis for mortality-rate panels -----------------------

all_y_values <- c(
  mortality_lrti_Ab_plot$lower,
  mortality_lrti_Ab_plot$upper,
  mortality_lrti_other_plot$lower,
  mortality_lrti_other_plot$upper,
  mortality_lrti_both_plot$lower,
  mortality_lrti_both_plot$upper,
  mortality_fev_Ab_plot$lower,
  mortality_fev_Ab_plot$upper,
  mortality_fev_other_plot$lower,
  mortality_fev_other_plot$upper,
  mortality_fev_both_plot$lower,
  mortality_fev_both_plot$upper
)

all_y_values <- all_y_values * 1000
all_y_values <- all_y_values[is.finite(all_y_values) & all_y_values > 0]

if (length(all_y_values) == 0) {
  stop("No finite positive mortality-rate values are available for plotting.")
}

common_y_exp_min <- floor(log10(min(all_y_values, na.rm = TRUE)))
common_y_exp_max <- ceiling(log10(max(all_y_values, na.rm = TRUE)))

if (common_y_exp_min == common_y_exp_max) {
  common_y_exp_min <- common_y_exp_min - 1
  common_y_exp_max <- common_y_exp_max + 1
}

common_y_ticks <- seq(-5, 1, by = 1)
common_y_labels <- parse(text = paste0("10^", common_y_ticks))
common_y_lim_log <- c(-5, 1)

mortality_floor_per1000 <- 10^common_y_exp_min



# Panel 1. LRTI mortality preventable by antibiotics --------------------------

dat <- mortality_lrti_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("Preventable bacterial LRTI", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



# Panel 2. LRTI mortality preventable by richest-household circumstances -------

dat <- mortality_lrti_other_plot

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)





# Panel 3. LRTI mortality preventable by both interventions --------------------

dat <- mortality_lrti_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)




# Panel 4. Antibiotic share of total preventable LRTI mortality ----------------

dat <- proportion_lrti_plot 
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

proportion_ticks <- seq(0, 1, by = 0.25)
proportion_labels <- format(proportion_ticks, nsmall = 2)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 1),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_ticks, labels = proportion_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = pmax(dat$lower, 0),
  x1 = log10(dat$GDP), y1 = pmin(dat$upper, 1),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), pmin(pmax(dat$mean, 0), 1),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)



# Panel 5. Fever mortality preventable by antibiotics -------------------------

dat <- mortality_fev_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("Preventable bacterial fever", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.5, cex = cex_title, font = 2)



# Panel 6. Fever mortality preventable by richest-household circumstances ------

dat <- mortality_fev_other_plot 

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)





# Panel 7. Fever mortality preventable by both interventions -------------------

dat <- mortality_fev_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

mean_plot <- pmax(dat$mean * 1000, mortality_floor_per1000)
lower_plot <- pmax(dat$lower * 1000, mortality_floor_per1000)
upper_plot <- pmax(dat$upper * 1000, mortality_floor_per1000)

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = common_y_ticks, labels = common_y_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), log10(mean_plot),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)




# Panel 8. Antibiotic share of total preventable fever mortality ---------------

dat <- proportion_fev_plot 

wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = c(0, 1),
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value,
     tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))

axis(2, at = proportion_ticks, labels = proportion_labels, las = 1,
     cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP), y0 = pmax(dat$lower, 0),
  x1 = log10(dat$GDP), y1 = pmin(dat$upper, 1),
  col = adjustcolor(wealth_col, alpha.f = 0.30), lwd = 0.6
)

points(
  log10(dat$GDP), pmin(pmax(dat$mean, 0), 1),
  pch = 16, cex = 0.6,
  col = adjustcolor(wealth_col, alpha.f = 0.60)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)

mtext(
  "Antibiotic share of total\npreventable bacterial fever mortality",
  side = 2, line = 1.8, cex = cex.axis_title
)


# Panel 9. Household wealth legend --------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  x = 0.5,  y = 0.45,  xjust = 0.5,  yjust = 0.5,
  legend = unname(wealth_labels),
  title = "Household wealth quintile",
  col = adjustcolor(unname(wealth_cols_fixed), alpha.f = 0.80),
  pch = 16,
  lty = NA,
  lwd = NA,
  pt.cex = 0.6,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.5,
  bty = "n"
)




# Panel 10. Blank spacer -------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

dev.off()






# _____________________________________________ ----------
# (Supple 5_3) 4-2. Absolute deaths preventable using the best country-wealth CMR ------------

# Settings ---------------------------------------------------------------------

selected_year <- 2023
rate_scale <- 1

# rate_scale = 1 when mortality is expressed per child
# rate_scale = 1000 when mortality is expressed per 1,000 children

wealth_tags <- c("q1", "q2", "q3", "q4", "q5")
wealth_share_dhs <- c(
  q1 = 0.231,
  q2  = 0.214,
  q3  = 0.200,
  q4  = 0.188,
  q5 = 0.166
)
wealth_share_dhs <- wealth_share_dhs / sum(wealth_share_dhs)
# wealth_population_share <- c(q1 = 0.20, q2 = 0.20, q3 = 0.20, q4 = 0.20, q5 = 0.20)
wealth_population_share <- wealth_share_dhs


# Select 2023 metadata ---------------------------------------------------------

row_2023 <- which(UNI$Year == selected_year)

metadata_2023 <- UNI[
  row_2023,
  c("Country", "Year", "GDP", "Income", "gbd_region", "PC1", "PC2", "Endemic", "Population")
]


# Convert wealth-specific preventable mortality rates to absolute numbers ------

number_preventable_results_global_best <- setNames(
  vector("list", length(wealth_tags)),
  wealth_tags
)

for (wealth_i in wealth_tags) {
  
  population_i <- metadata_2023$Population *
    unname(wealth_population_share[wealth_i])
  
  # Both interventions:
  # complete antibiotic access + global best country-wealth CMR
  mortality_lrti_rate_i <- as.matrix(
    preventable_results_global_best[[wealth_i]]$mortality_lrti_both_preventable
  )[row_2023, , drop = FALSE]
  
  mortality_fever_rate_i <- as.matrix(
    preventable_results_global_best[[wealth_i]]$mortality_fever_both_preventable
  )[row_2023, , drop = FALSE]
  
  number_lrti_i <- sweep(
    mortality_lrti_rate_i,
    MARGIN = 1,
    STATS = population_i / rate_scale,
    FUN = "*"
  )
  
  number_fever_i <- sweep(
    mortality_fever_rate_i,
    MARGIN = 1,
    STATS = population_i / rate_scale,
    FUN = "*"
  )
  
  number_preventable_results_global_best[[wealth_i]] <- list(
    mortality_lrti_both_preventable = number_lrti_i,
    mortality_fever_both_preventable = number_fever_i
  )
}


# Sum the five wealth groups within each country -------------------------------

matrix_number_lrti_both_preventable_global_best_2023 <-
  number_preventable_results_global_best$q1$mortality_lrti_both_preventable +
  number_preventable_results_global_best$q2$mortality_lrti_both_preventable +
  number_preventable_results_global_best$q3$mortality_lrti_both_preventable +
  number_preventable_results_global_best$q4$mortality_lrti_both_preventable +
  number_preventable_results_global_best$q5$mortality_lrti_both_preventable

matrix_number_fever_both_preventable_global_best_2023 <-
  number_preventable_results_global_best$q1$mortality_fever_both_preventable +
  number_preventable_results_global_best$q2$mortality_fever_both_preventable +
  number_preventable_results_global_best$q3$mortality_fever_both_preventable +
  number_preventable_results_global_best$q4$mortality_fever_both_preventable +
  number_preventable_results_global_best$q5$mortality_fever_both_preventable


# Country-level summaries ------------------------------------------------------

summary_country_lrti_both_preventable_global_best_2023 <- metadata_2023 %>%
  dplyr::mutate(
    median = apply(
      matrix_number_lrti_both_preventable_global_best_2023,
      1,
      median,
      na.rm = TRUE
    ),
    lower = apply(
      matrix_number_lrti_both_preventable_global_best_2023,
      1,
      quantile,
      probs = 0.025,
      na.rm = TRUE,
      names = FALSE
    ),
    upper = apply(
      matrix_number_lrti_both_preventable_global_best_2023,
      1,
      quantile,
      probs = 0.975,
      na.rm = TRUE,
      names = FALSE
    )
  )

summary_country_fever_both_preventable_global_best_2023 <- metadata_2023 %>%
  dplyr::mutate(
    median = apply(
      matrix_number_fever_both_preventable_global_best_2023,
      1,
      median,
      na.rm = TRUE
    ),
    lower = apply(
      matrix_number_fever_both_preventable_global_best_2023,
      1,
      quantile,
      probs = 0.025,
      na.rm = TRUE,
      names = FALSE
    ),
    upper = apply(
      matrix_number_fever_both_preventable_global_best_2023,
      1,
      quantile,
      probs = 0.975,
      na.rm = TRUE,
      names = FALSE
    )
  )


# Overall summaries ------------------------------------------------------------

overall_lrti_both_preventable_global_best_2023_draw <- colSums(
  matrix_number_lrti_both_preventable_global_best_2023,
  na.rm = TRUE
)

overall_fever_both_preventable_global_best_2023_draw <- colSums(
  matrix_number_fever_both_preventable_global_best_2023,
  na.rm = TRUE
)

summary_overall_lrti_both_preventable_global_best_2023 <- data.frame(
  Year = selected_year,
  group = "Overall",
  median = median(
    overall_lrti_both_preventable_global_best_2023_draw,
    na.rm = TRUE
  ),
  lower = quantile(
    overall_lrti_both_preventable_global_best_2023_draw,
    0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = quantile(
    overall_lrti_both_preventable_global_best_2023_draw,
    0.975,
    na.rm = TRUE,
    names = FALSE
  )
)

summary_overall_fever_both_preventable_global_best_2023 <- data.frame(
  Year = selected_year,
  group = "Overall",
  median = median(
    overall_fever_both_preventable_global_best_2023_draw,
    na.rm = TRUE
  ),
  lower = quantile(
    overall_fever_both_preventable_global_best_2023_draw,
    0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = quantile(
    overall_fever_both_preventable_global_best_2023_draw,
    0.975,
    na.rm = TRUE,
    names = FALSE
  )
)

matrix_number_all_both_preventable_global_best_2023 <- 
  matrix_number_lrti_both_preventable_global_best_2023 + 
  matrix_number_fever_both_preventable_global_best_2023

overall_all_both_preventable_global_best_2023_draw <- colSums(
  matrix_number_all_both_preventable_global_best_2023,
  na.rm = TRUE
)

summary_overall_all_both_preventable_global_best_2023 <- data.frame(
  Year = selected_year,
  group = "Overall",
  median = round(median(
    overall_all_both_preventable_global_best_2023_draw,
    na.rm = TRUE
  ), digits = 0),
  lower = round(quantile(
    overall_all_both_preventable_global_best_2023_draw,
    0.025,
    na.rm = TRUE,
    names = FALSE
  ), digits = 0),
  upper = round(quantile(
    overall_all_both_preventable_global_best_2023_draw,
    0.975,
    na.rm = TRUE,
    names = FALSE
  ), digits = 0)
)

summary_overall_all_both_preventable_global_best_2023


# estimateing the proprotion of Ab preventable death over both across best quintile-country 
Ab_preventable_all <- readRDS("outputs/pipelines/Ab_preventable_all.rds")
Ab_preventable_prop <- Ab_preventable_all/overall_all_both_preventable_global_best_2023_draw

summary_matrix <- function(mat) {
  data.frame(
    Income = rownames(mat),
    median = round(apply(mat, 1, quantile, probs = 0.500, na.rm = TRUE), digits = 3),
    lower  = round(apply(mat, 1, quantile, probs = 0.025, na.rm = TRUE), digits = 3),
    upper  = round(apply(mat, 1, quantile, probs = 0.975, na.rm = TRUE), digits = 3),
    row.names = NULL)
}

summary_matrix(Ab_preventable_prop)

# Income-level draw aggregation ------------------------------------------------

income_levels <- c("L", "LM", "UM")

income_lrti_both_preventable_global_best_2023_draws <- matrix(
  NA_real_,
  nrow = length(income_levels),
  ncol = ncol(matrix_number_lrti_both_preventable_global_best_2023),
  dimnames = list(income_levels, NULL)
)

income_fever_both_preventable_global_best_2023_draws <- matrix(
  NA_real_,
  nrow = length(income_levels),
  ncol = ncol(matrix_number_fever_both_preventable_global_best_2023),
  dimnames = list(income_levels, NULL)
)

for (income_i in income_levels) {
  
  row_income_i <- which(metadata_2023$Income == income_i)
  
  income_lrti_both_preventable_global_best_2023_draws[income_i, ] <- colSums(
    matrix_number_lrti_both_preventable_global_best_2023[
      row_income_i,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
  
  income_fever_both_preventable_global_best_2023_draws[income_i, ] <- colSums(
    matrix_number_fever_both_preventable_global_best_2023[
      row_income_i,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
}

summary_income_lrti_both_preventable_global_best_2023 <- data.frame(
  Income = income_levels,
  Year = selected_year,
  median = apply(
    income_lrti_both_preventable_global_best_2023_draws,
    1,
    median,
    na.rm = TRUE
  ),
  lower = apply(
    income_lrti_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = apply(
    income_lrti_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  )
)

summary_income_fever_both_preventable_global_best_2023 <- data.frame(
  Income = income_levels,
  Year = selected_year,
  median = apply(
    income_fever_both_preventable_global_best_2023_draws,
    1,
    median,
    na.rm = TRUE
  ),
  lower = apply(
    income_fever_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = apply(
    income_fever_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  )
)


# GBD-region draw aggregation --------------------------------------------------

region_levels <- c(
  "Sub-Saharan Africa",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "North Africa and Middle East",
  "Latin America and Caribbean",
  "Central Europe, Eastern Europe, and Central Asia"
)

region_lrti_both_preventable_global_best_2023_draws <- matrix(
  NA_real_,
  nrow = length(region_levels),
  ncol = ncol(matrix_number_lrti_both_preventable_global_best_2023),
  dimnames = list(region_levels, NULL)
)

region_fever_both_preventable_global_best_2023_draws <- matrix(
  NA_real_,
  nrow = length(region_levels),
  ncol = ncol(matrix_number_fever_both_preventable_global_best_2023),
  dimnames = list(region_levels, NULL)
)

for (region_i in region_levels) {
  
  row_region_i <- which(metadata_2023$gbd_region == region_i)
  
  region_lrti_both_preventable_global_best_2023_draws[region_i, ] <- colSums(
    matrix_number_lrti_both_preventable_global_best_2023[
      row_region_i,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
  
  region_fever_both_preventable_global_best_2023_draws[region_i, ] <- colSums(
    matrix_number_fever_both_preventable_global_best_2023[
      row_region_i,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
}

summary_region_lrti_both_preventable_global_best_2023 <- data.frame(
  gbd_region = region_levels,
  Year = selected_year,
  median = apply(
    region_lrti_both_preventable_global_best_2023_draws,
    1,
    median,
    na.rm = TRUE
  ),
  lower = apply(
    region_lrti_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = apply(
    region_lrti_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  )
)

summary_region_fever_both_preventable_global_best_2023 <- data.frame(
  gbd_region = region_levels,
  Year = selected_year,
  median = apply(
    region_fever_both_preventable_global_best_2023_draws,
    1,
    median,
    na.rm = TRUE
  ),
  lower = apply(
    region_fever_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ),
  upper = apply(
    region_fever_both_preventable_global_best_2023_draws,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  )
)


# Combine summaries ------------------------------------------------------------

summary.number_lrti_both_preventable_global_best <- list(
  overall = summary_overall_lrti_both_preventable_global_best_2023,
  country = summary_country_lrti_both_preventable_global_best_2023,
  income = summary_income_lrti_both_preventable_global_best_2023,
  gbd_region = summary_region_lrti_both_preventable_global_best_2023
)

summary.number_fever_both_preventable_global_best <- list(
  overall = summary_overall_fever_both_preventable_global_best_2023,
  country = summary_country_fever_both_preventable_global_best_2023,
  income = summary_income_fever_both_preventable_global_best_2023,
  gbd_region = summary_region_fever_both_preventable_global_best_2023
)


write.csv(summary.number_lrti_both_preventable_global_best$country, "outputs/pipelines/suppl_table/preventable_deaths_global_best_wealth_lrti.csv")
write.csv(summary.number_fever_both_preventable_global_best$country, "outputs/pipelines/suppl_table/preventable_deaths_global_best_wealth_fever.csv")




# _____________________________________________ ----------
# Prepare data for the Figure 5-style plot -------------------------------------

category_levels <- c(
  "Total",
  "Low-income",
  "Lower-middle-income",
  "Upper-middle-income",
  "Sub-Saharan Africa",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "North Africa and Middle East",
  "Latin America and Caribbean",
  "Central Europe, Eastern Europe, and Central Asia"
)

category_cols <- c(
  "Total" = "#0072B2",
  "Low-income" = "#CC79A7",
  "Lower-middle-income" = "#009E73",
  "Upper-middle-income" = "#0072B2",
  "Sub-Saharan Africa" = "#0072B2",
  "South Asia" = "#A7B97F",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "North Africa and Middle East" = "#009E73",
  "Latin America and Caribbean" = "#E69F00",
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7"
)

plot_floor <- 1e-5

dat_lrti_global_best <- dplyr::bind_rows(
  summary.number_lrti_both_preventable_global_best$overall %>%
    dplyr::transmute(
      group = "Total",
      median,
      lower,
      upper
    ),
  summary.number_lrti_both_preventable_global_best$income %>%
    dplyr::transmute(
      group = dplyr::recode(
        as.character(Income),
        L = "Low-income",
        LM = "Lower-middle-income", 
        UM = "Upper-middle-income"
      ),
      median,
      lower,
      upper
    ),
  summary.number_lrti_both_preventable_global_best$gbd_region %>%
    dplyr::transmute(
      group = gbd_region,
      median,
      lower,
      upper
    )
) %>%
  dplyr::mutate(
    group = factor(group, levels = category_levels)
  ) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    median_plot = pmax(median, plot_floor),
    lower_plot = pmax(lower, plot_floor),
    upper_plot = pmax(upper, plot_floor),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(
      median_text,
      " (",
      lower_text,
      "–",
      upper_text,
      ")"
    )
  )

dat_fever_global_best <- dplyr::bind_rows(
  summary.number_fever_both_preventable_global_best$overall %>%
    dplyr::transmute(
      group = "Total",
      median,
      lower,
      upper
    ),
  summary.number_fever_both_preventable_global_best$income %>%
    dplyr::transmute(
      group = dplyr::recode(
        as.character(Income),
        L = "Low-income",
        LM = "Lower-middle-income", 
        UM = "Upper-middle-income"
      ),
      median,
      lower,
      upper
    ),
  summary.number_fever_both_preventable_global_best$gbd_region %>%
    dplyr::transmute(
      group = gbd_region,
      median,
      lower,
      upper
    )
) %>%
  dplyr::mutate(
    group = factor(group, levels = category_levels)
  ) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    median_plot = pmax(median, plot_floor),
    lower_plot = pmax(lower, plot_floor),
    upper_plot = pmax(upper, plot_floor),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(
      median_text,
      " (",
      lower_text,
      "–",
      upper_text,
      ")"
    )
  )



# Common logarithmic axis ------------------------------------------------------

x_values <- c(
  dat_lrti_global_best$lower_plot,
  dat_lrti_global_best$upper_plot,
  dat_fever_global_best$lower_plot,
  dat_fever_global_best$upper_plot
)

x_values <- x_values[
  is.finite(x_values) &
    x_values > 0
]

x_exp <- seq(
  floor(log10(min(x_values))),
  ceiling(log10(max(x_values))),
  by = 1
)

x_ticks <- x_exp
x_labels <- parse(text = paste0("10^", x_exp))
xlim_log <- range(x_ticks)


# Plot settings ----------------------------------------------------------------

category_y <- rev(seq_along(category_levels))
divider_y <- c(
  category_y[1] - 0.5,
  category_y[4] - 0.5
)

ylim_panel <- c(
  0.5,
  length(category_levels) + 1
)

text_title_y <- length(category_levels) + 0.8

cex_axis_value <- 0.70
cex_category <- 0.70
cex_axis_title <- 0.55
cex_title <- 0.60
cex_point <- 0.70
cex_text <- 0.70
cex_text_title <- 0.7

lwd_interval <- 0.8
lwd_divider <- 1.2
alpha_interval <- 0.40
alpha_point <- 0.80
axis_tick_length <- -0.025

# Create plot ------------------------------------------------------------------

dir.create(
  "figures/subgroup_analysis",
  recursive = TRUE,
  showWarnings = FALSE
)

pdf(
  "figures/subgroup_analysis/(plot 5_subgroup) absolute preventable deaths_global best country wealth.pdf",
  width = 6,
  height = 2,
  useDingbats = FALSE
)

layout(
  matrix(c(1, 2, 3, 4, 5), nrow = 1),
  widths = c(1.5, 0.75, 0.85, 0.75, 0.85)
)


# Category labels --------------------------------------------------------------

par(
  mar = c(2.3, 0.2, 0.5, 0.1),
  mgp = c(0.35, 0.35, 0),
  bty = "n"
)

plot.new()

plot.window(
  xlim = c(0, 1),
  ylim = ylim_panel,
  xaxs = "i",
  yaxs = "i"
)

text(
  rep(0.98, length(category_levels)),
  category_y,
  labels = category_levels,
  adj = c(1, 0.5),
  cex = cex_category,
  xpd = NA
)


# Bacterial LRTI plot ----------------------------------------------------------

dat <- dat_lrti_global_best

par(
  mar = c(2.3, 0.3, 0.5, 0.2),
  mgp = c(0.35, 0.35, 0),
  bty = "l"
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = x_ticks,
  labels = x_labels,
  cex.axis = cex_axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  rep(xlim_log[1], length(divider_y)),
  divider_y,
  rep(xlim_log[2], length(divider_y)),
  divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  log10(dat$lower_plot),
  category_y,
  log10(dat$upper_plot),
  category_y,
  col = adjustcolor(
    dat$col,
    alpha.f = alpha_interval
  ),
  lwd = lwd_interval
)

points(
  log10(dat$median_plot),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(
    dat$col,
    alpha.f = alpha_point
  )
)

mtext(
  "Number of preventable deaths",
  side = 1,
  line = 1.1,
  cex = cex_axis_title
)




# Bacterial LRTI result text ---------------------------------------------------

par(
  mar = c(2.3, 0.1, 0.5, 0.1),
  mgp = c(0.35, 0.35, 0),
  bty = "n"
)

plot.new()

plot.window(
  xlim = c(0, 1),
  ylim = ylim_panel,
  xaxs = "i",
  yaxs = "i"
)

text(
  0.5,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(0.5, nrow(dat_lrti_global_best)),
  category_y,
  labels = dat_lrti_global_best$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)


# Bacterial AFI plot -----------------------------------------------------------

dat <- dat_fever_global_best

par(
  mar = c(2.3, 0.3, 0.5, 0.2),
  mgp = c(0.35, 0.35, 0),
  bty = "l"
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = x_ticks,
  labels = x_labels,
  cex.axis = cex_axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  rep(xlim_log[1], length(divider_y)),
  divider_y,
  rep(xlim_log[2], length(divider_y)),
  divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  log10(dat$lower_plot),
  category_y,
  log10(dat$upper_plot),
  category_y,
  col = adjustcolor(
    dat$col,
    alpha.f = alpha_interval
  ),
  lwd = lwd_interval
)

points(
  log10(dat$median_plot),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(
    dat$col,
    alpha.f = alpha_point
  )
)

mtext(
  "Number of preventable deaths",
  side = 1,
  line = 1.1,
  cex = cex_axis_title
)


# Bacterial AFI result text ----------------------------------------------------

par(
  mar = c(2.3, 0.1, 0.5, 0.1),
  mgp = c(0.35, 0.35, 0),
  bty = "n"
)

plot.new()

plot.window(
  xlim = c(0, 1),
  ylim = ylim_panel,
  xaxs = "i",
  yaxs = "i"
)

text(
  0.5,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(0.5, nrow(dat_fever_global_best)),
  category_y,
  labels = dat_fever_global_best$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

dev.off()





















