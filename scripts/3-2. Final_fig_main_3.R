

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

options(scipen = 999)

# _____________________________________________ ----------
# 1. Incidence by household-wealth scenario -----------------------------

## 1. Load the five wealth-specific incidence uncertainty results --------------
wealth_level_list <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")

wealth_quintile_map <- c("Poorest" = 1L, "Poorer" = 2L, "Middle" = 3L, "Richer" = 4L, "Richest" = 5L)
wealth_tag_map <- c("Poorest" = "q1", "Poorer" = "q2", "Middle" = "q3", "Richer" = "q4", "Richest" = "q5")

for (i in 1:5) {
  wealth_level_i <- wealth_level_list[i]
  wealth_quintile_i <- unname(wealth_quintile_map[wealth_level_i])
  wealth_tag_i <- unname(wealth_tag_map[wealth_level_i])
  
  assign(paste0(wealth_tag_i, "_inc_bact_fever_0_11"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_0_11_", wealth_tag_i, ".rds")))
  assign(paste0(wealth_tag_i, "_inc_bact_fever_12_23"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_12_23_", wealth_tag_i, ".rds")))
  assign(paste0(wealth_tag_i, "_inc_bact_fever_24_59"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_fever_24_59_", wealth_tag_i, ".rds")))
  
  assign(paste0(wealth_tag_i, "_inc_bact_lrti_0_11"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_0_11_", wealth_tag_i, ".rds")))
  assign(paste0(wealth_tag_i, "_inc_bact_lrti_12_23"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_12_23_", wealth_tag_i, ".rds")))
  assign(paste0(wealth_tag_i, "_inc_bact_lrti_24_59"), readRDS(paste0("outputs/pipelines/bacterial_incidence/wealth_subgroup/inc_bact_lrti_24_59_", wealth_tag_i, ".rds")))
}

wealth_list <- c("q1", "q2", "q3", "q4", "q5")
outcome_list <- c("LRTI", "fever")


## 2. Combine the three age-specific bacterial-incidence distributions ---------
# Each uncertainty draw is weighted by the population in each age group.

for (i in 1:2) {
  outcome_i <- outcome_list[i]
  
  if (outcome_i == "LRTI") {
    outcome_tag_i <- "lrti"
  } else {
    outcome_tag_i <- "fever"
  }
  
  for (j in 1:5) {
    wealth_i <- wealth_list[j]
    
    incidence_0_11_i <- get(paste0(wealth_i, "_inc_bact_", outcome_tag_i, "_0_11")) %>%
      mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
      filter(Year == 2023)
    
    incidence_12_23_i <- get(paste0(wealth_i, "_inc_bact_", outcome_tag_i, "_12_23")) %>%
      mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
      filter(Year == 2023)
    
    incidence_24_59_i <- get(paste0(wealth_i, "_inc_bact_", outcome_tag_i, "_24_59")) %>%
      mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
      filter(Year == 2023)
    
    population_0_11_i <- incidence_0_11_i %>%
      transmute(Country, Year, population_0_11 = as.numeric(age_population))
    
    population_12_23_i <- incidence_12_23_i %>%
      transmute(Country, Year, population_12_23 = as.numeric(age_population))
    
    population_24_59_i <- incidence_24_59_i %>%
      transmute(Country, Year, population_24_59 = as.numeric(age_population))
    
    population_all_age_i <- population_0_11_i %>%
      inner_join(population_12_23_i, by = c("Country", "Year")) %>%
      inner_join(population_24_59_i, by = c("Country", "Year")) %>%
      filter(
        is.finite(population_0_11), is.finite(population_12_23), is.finite(population_24_59),
        population_0_11 > 0, population_12_23 > 0, population_24_59 > 0
      ) %>%
      mutate(age_population = population_0_11 + population_12_23 + population_24_59)
    
    common_key_i <- population_all_age_i %>%
      inner_join(incidence_0_11_i %>% distinct(Country, Year), by = c("Country", "Year")) %>%
      inner_join(incidence_12_23_i %>% distinct(Country, Year), by = c("Country", "Year")) %>%
      inner_join(incidence_24_59_i %>% distinct(Country, Year), by = c("Country", "Year")) %>%
      left_join(incidence_0_11_i %>%
          transmute(Country, Year, GDP = as.numeric(GDP)) %>%
          distinct(Country, Year, .keep_all = TRUE),
        by = c("Country", "Year")
      )
    
    incidence_0_11_aligned_i <- common_key_i %>%
      dplyr::select(Country, Year) %>%
      left_join(incidence_0_11_i, by = c("Country", "Year"))
    
    incidence_12_23_aligned_i <- common_key_i %>%
      dplyr::select(Country, Year) %>%
      left_join(incidence_12_23_i, by = c("Country", "Year"))
    
    incidence_24_59_aligned_i <- common_key_i %>%
      dplyr::select(Country, Year) %>%
      left_join(incidence_24_59_i, by = c("Country", "Year"))
    
    metadata_cols_i <- c(
      "Country", "Year", "GDP", "Income", "gbd_region",
      "Endemic", "Population", "age_population"
    )
    
    draw_cols_0_11_i <- setdiff(names(incidence_0_11_aligned_i), metadata_cols_i)
    draw_cols_12_23_i <- setdiff(names(incidence_12_23_aligned_i), metadata_cols_i)
    draw_cols_24_59_i <- setdiff(names(incidence_24_59_aligned_i), metadata_cols_i)

    draw_cols_i <- draw_cols_0_11_i[2:1001]
    
    incidence_0_11_matrix_i <- as.matrix(incidence_0_11_aligned_i[,draw_cols_i])
    incidence_12_23_matrix_i <- as.matrix(incidence_12_23_aligned_i[,draw_cols_i])
    incidence_24_59_matrix_i <- as.matrix(incidence_24_59_aligned_i[,draw_cols_i])
    
    all_age_draw_matrix_i <- (
      incidence_0_11_matrix_i * common_key_i$population_0_11 +
        incidence_12_23_matrix_i * common_key_i$population_12_23 +
        incidence_24_59_matrix_i * common_key_i$population_24_59
    ) / common_key_i$age_population
    
    assign(paste0("incidence_result_", outcome_i, "_", wealth_i, "_draw"), all_age_draw_matrix_i)
    
    colnames(all_age_draw_matrix_i) <- draw_cols_i
    
    all_age_draws_i <- bind_cols(
      common_key_i %>% dplyr::select(Country, Year, age_population),
      as.data.frame(all_age_draw_matrix_i, check.names = FALSE)
    )
    
    all_age_median_i <- apply(all_age_draw_matrix_i, 1, median, na.rm = TRUE)
    all_age_lower_i <- apply(all_age_draw_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE)
    all_age_upper_i <- apply(all_age_draw_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE)
    
    result_all_age_i <- common_key_i %>%
      transmute(
        outcome = outcome_i,
        wealth_tag = wealth_i,
        Country,
        Year,
        GDP,
        age_population,
        predicted_annual_rate_median = all_age_median_i,
        predicted_annual_rate_lower = all_age_lower_i,
        predicted_annual_rate_upper = all_age_upper_i
      )
    
    assign(paste0("result_", outcome_i, "_", wealth_i), result_all_age_i)
    assign(paste0("result_", outcome_i, "_", wealth_i, "_draws"), all_age_draws_i)
    assign(paste0("incidence_result_", outcome_i, "_", wealth_i, "_draw"), all_age_draw_matrix_i)
  }
}

## 3. Put the five wealth scenarios into one table for each outcome ------------

result_LRTI_all_wealth <- bind_rows(
  result_LRTI_q1,
  result_LRTI_q2,
  result_LRTI_q3,
  result_LRTI_q4,
  result_LRTI_q5
) %>%
  dplyr::select(Country, Year, GDP, wealth_tag, predicted_annual_rate_median, ) %>%
  pivot_wider(names_from = wealth_tag, values_from = predicted_annual_rate_median)

result_fever_all_wealth <- bind_rows(
  result_fever_q1,
  result_fever_q2,
  result_fever_q3,
  result_fever_q4,
  result_fever_q5
) %>%
  dplyr::select(Country, Year, GDP, wealth_tag, predicted_annual_rate_median) %>%
  pivot_wider(names_from = wealth_tag, values_from = predicted_annual_rate_median)


result_inc_lrti_wealth <- bind_rows(
  result_LRTI_q1,
  result_LRTI_q2,
  result_LRTI_q3,
  result_LRTI_q4,
  result_LRTI_q5
) %>% dplyr::select(Country, wealth_tag, predicted_annual_rate_median, predicted_annual_rate_lower, predicted_annual_rate_upper)

result_inc_fever_wealth <- bind_rows(
  result_fever_q1,
  result_fever_q2,
  result_fever_q3,
  result_fever_q4,
  result_fever_q5
) %>% dplyr::select(Country, wealth_tag, predicted_annual_rate_median, predicted_annual_rate_lower, predicted_annual_rate_upper)
  
write.csv(result_inc_lrti_wealth, "outputs/pipelines/suppl_table/quintile_inc_lrti.csv")
write.csv(result_inc_fever_wealth, "outputs/pipelines/suppl_table/quintile_inc_fever.csv")



## 4. GDP and health-expenditure covariates -------------------------------------

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")

UNI_GDP <- UNI %>%
  transmute(Country = as.character(Country), Year = as.numeric(Year)) %>%
  distinct(Country, Year, .keep_all = TRUE)

## 5. Plot settings --------------------------------------------------------------

wealth_names <- c("q5", "q4", "q3", "q2", "q1")
wealth_labels <- c("Richest", "Richer", "Middle", "Poorer", "Poorest")

base_cols <- c("#4575B4", "#00A6D6", "grey20", "#F46D43", "#D73027")
cols <- adjustcolor(base_cols, alpha.f = 0.7)
line_widths <- c(0.5, 0.5, 0.9, 0.5, 0.5)

x_at_GDP <- c(500, 1000, 2000, 5000, 20000)
x_labels_GDP <- c("500", "1000", "2000", "5000", "20000")


## 6. LRTI plotting data ---------------------------------------------------------

plot_dat_LRTI <- result_LRTI_all_wealth %>%
  left_join(UNI_GDP, by = c("Country", "Year")) %>%
  filter(Year == 2023, if_all(all_of(wealth_names), is.finite)) %>%
  mutate(q1 = q1*1000,
         q2 = q2*1000,
         q3 = q3*1000,
         q4 = q4*1000,
         q5 = q5*1000)

plot_dat_LRTI_GDP <- plot_dat_LRTI %>% filter(is.finite(GDP), GDP > 0) %>% arrange(GDP)
result_matrix_LRTI_GDP <- as.matrix(plot_dat_LRTI_GDP[, wealth_names])


## 7. Fever plotting data --------------------------------------------------------

plot_dat_fever <- result_fever_all_wealth %>%
  left_join(UNI_GDP, by = c("Country", "Year")) %>%
  filter(Year == 2023, if_all(all_of(wealth_names), is.finite)) %>%
  mutate(q1 = q1*1000,
         q2 = q2*1000,
         q3 = q3*1000,
         q4 = q4*1000,
         q5 = q5*1000)

plot_dat_fever_GDP <- plot_dat_fever %>% filter(is.finite(GDP), GDP > 0) %>% arrange(GDP)
result_matrix_fever_GDP <- as.matrix(plot_dat_fever_GDP[, wealth_names])


## 8. Automatic incidence-rate y-axis ranges ------------------------------------

LRTI_y_values <- as.numeric(result_matrix_LRTI_GDP)
LRTI_y_values <- LRTI_y_values[is.finite(LRTI_y_values)]
LRTI_y_range <- range(LRTI_y_values)
LRTI_ylim <- c(0, 100)
LRTI_y_at <- seq(LRTI_ylim[1], LRTI_ylim[2], length.out = 5)
LRTI_y_labels <- LRTI_y_at

fever_y_values <- as.numeric(result_matrix_fever_GDP)
fever_y_range <- range(fever_y_values)
fever_ylim <- c(0, 400)
fever_y_at <- seq(fever_ylim[1], fever_ylim[2], length.out = 5)
fever_y_labels <- fever_y_at


## 9. Create the four-panel PDF --------------------------------------------------

dir.create("figures/subgroup_analysis", recursive = TRUE, showWarnings = FALSE)

# pdf("figures/subgroup_analysis/Inc_LRTI_fever_by_wealth_2023.pdf", width = 6, height = 2, useDingbats = FALSE)
# layout(matrix(c(1, 2, 3, 4), nrow = 1), widths = c(0.1, 1, 1, 0.4))


### Shared vertical y-axis title --------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()
mtext("Predicted annual incidence rate \n(episodes per 1000 child-years)", side = 2, line = -1.5, cex = 0.45, las = 0)


### Panel 1: LRTI versus GDP ------------------------------------------------------

par(bty = "l", las = 1, mar = c(3, 2, 1.4, 0.5), mgp = c(1.9, 0.45, 0), tcl = -0.2)
plot(plot_dat_LRTI_GDP$GDP, result_matrix_LRTI_GDP[, 1], type = "n", log = "x", xlim = c(500, 20000), ylim = LRTI_ylim, axes = FALSE, ann = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = x_at_GDP, labels = x_labels_GDP, cex.axis = 0.55, mgp = c(0.35, 0.025, 0))
axis(2, at = LRTI_y_at, labels = LRTI_y_labels, cex.axis = 0.55)
mtext("GDP per capita (PPP)", side = 1, line = 2, cex = 0.5)
mtext("LRTI", side = 3, line = 0.3, cex = 0.5, font = 2)
for (i in seq_along(wealth_names)) lines(plot_dat_LRTI_GDP$GDP, result_matrix_LRTI_GDP[, i], col = cols[i], lwd = line_widths[i], lty = 1)


### Panel 2: Fever versus GDP -----------------------------------------------------

par(bty = "l", las = 1, mar = c(3, 2, 1.4, 0.5), mgp = c(1.9, 0.45, 0), tcl = -0.2)
plot(plot_dat_fever_GDP$GDP, result_matrix_fever_GDP[, 1], type = "n", log = "x", xlim = c(500, 20000), ylim = fever_ylim, axes = FALSE, ann = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = x_at_GDP, labels = x_labels_GDP, cex.axis = 0.55, mgp = c(0.35, 0.025, 0))
axis(2, at = fever_y_at, labels = fever_y_labels, cex.axis = 0.55)
mtext("GDP per capita (PPP)", side = 1, line = 2, cex = 0.5)
mtext("Fever", side = 3, line = 0.3, cex = 0.5, font = 2)
for (i in seq_along(wealth_names)) lines(plot_dat_fever_GDP$GDP, result_matrix_fever_GDP[, i], col = cols[i], lwd = line_widths[i], lty = 1)


### Wealth legend ----------------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = wealth_labels, title = "Wealth quintile", col = cols, lwd = line_widths, lty = 1, bty = "n", cex = 0.6, title.cex = 0.6, x.intersp = 0.5, y.intersp = 0.8)

# dev.off()










# _____________________________________________ ----------
# 2. Antibiotic access by household-wealth scenario -----------------------------

AA_result_q1 <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth/q1/AA_all_age_specific_selected_model_uncertainty_results_q1.rds")
AA_result_q2 <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth/q2/AA_all_age_specific_selected_model_uncertainty_results_q2.rds")
AA_result_q3 <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth/q3/AA_all_age_specific_selected_model_uncertainty_results_q3.rds")
AA_result_q4 <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth/q4/AA_all_age_specific_selected_model_uncertainty_results_q4.rds")
AA_result_q5 <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty_by_wealth/q5/AA_all_age_specific_selected_model_uncertainty_results_q5.rds")

wealth_list <- c("q1", "q2", "q3", "q4", "q5")

spec_map <- c(
  "LRTI_0_11" = "LRTI_0_11_months",
  "LRTI_12_23" = "LRTI_12_23_months",
  "LRTI_24_59" = "LRTI_24_59_months",
  "fever_0_11" = "fever_0_11_months",
  "fever_12_23" = "fever_12_23_months",
  "fever_24_59" = "fever_24_59_months"
)

for (wealth_i in wealth_list) {
  result_wealth_i <- get(paste0("AA_result_", wealth_i))
  
  for (spec_i in names(spec_map)) {
    result_i <- result_wealth_i[[spec_map[spec_i]]]$country_year_summary %>% filter(Year == 2023)
    assign(paste0("AA_result_", spec_i, "_", wealth_i), result_i)
  }
}

for (wealth_i in wealth_list) {
  result_wealth_i <- get(paste0("AA_result_", wealth_i))
  
  for (spec_i in names(spec_map)) {
    result_i <- result_wealth_i[[spec_map[spec_i]]]$country_year_draws %>% filter(Year == 2023)
    assign(paste0("AA_result_", spec_i, "_", wealth_i, "_draw"), result_i)
  }
}

outcome_list <- c("LRTI", "fever")
age_list <- c("0_11", "12_23", "24_59")

for (outcome_i in outcome_list) {
  for (wealth_i in wealth_list) {
    dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
    
    stopifnot(all(vapply(dat_list_i[-1], \(x) identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
    
    numerator_i <- Reduce(`+`, lapply(dat_list_i, \(x) as.matrix(x[,10:1009]) * x$age_population))
    denominator_i <- Reduce(`+`, lapply(dat_list_i, \(x) x$age_population))
    
    result_i <- numerator_i / denominator_i
    
    assign(paste0("AA_result_", outcome_i, "_", wealth_i), apply(result_i, 1, quantile, 0.5))
  }
}


for (outcome_i in outcome_list) {
  for (wealth_i in wealth_list) {
    dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
    
    stopifnot(all(vapply(dat_list_i[-1], \(x) identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
    
    numerator_i <- Reduce(`+`, lapply(dat_list_i, \(x) as.matrix(x[,10:1009]) * x$age_population))
    denominator_i <- Reduce(`+`, lapply(dat_list_i, \(x) x$age_population))
    
    result_i <- numerator_i / denominator_i
    
    assign(paste0("AA_result_", outcome_i, "_", wealth_i, "_summary"), data.frame(
      Country = dat_list_i[[1]]$Country,
      Year = dat_list_i[[1]]$Year,
      wealth_i = wealth_i,
      median = apply(result_i, 1, quantile, 0.5),
      lower = apply(result_i, 1, quantile, 0.025),
      upper = apply(result_i, 1, quantile, 0.975)
    ))
  }
}


AA_result_LRTI_summary <- 
  bind_rows(AA_result_LRTI_q1_summary,
          AA_result_LRTI_q2_summary,
          AA_result_LRTI_q3_summary,
          AA_result_LRTI_q4_summary,
          AA_result_LRTI_q5_summary)

AA_result_fever_summary <- 
  bind_rows(AA_result_fever_q1_summary,
            AA_result_fever_q2_summary,
            AA_result_fever_q3_summary,
            AA_result_fever_q4_summary,
            AA_result_fever_q5_summary)

write.csv(AA_result_LRTI_summary, "outputs/pipelines/suppl_table/quintile_AA_lrti.csv")
write.csv(AA_result_fever_summary, "outputs/pipelines/suppl_table/quintile_AA_fever.csv")

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")

UNI_GDP <- UNI %>%
  dplyr::transmute(Country = as.character(Country), Year = as.numeric(Year), GDP = as.numeric(GDP)) %>%
  dplyr::distinct(Country, Year, .keep_all = TRUE)


wealth_names <- c("q5", "q4", "q3", "q2", "q1")
wealth_labels <- c("Richest", "Richer", "Middle", "Poorer", "Poorest")

base_cols <- c("#4575B4", "#00A6D6", "black", "#F46D43", "#D73027")

cols <- adjustcolor(base_cols, alpha.f = 0.7)

line_widths <- c(0.5, 0.5, 0.9, 0.5, 0.5)

x_at_GDP <- c(500, 1000, 2000, 5000, 20000)
x_labels_GDP <- c("500", "1000", "2000", "5000", "20000")


# LRTI data: GDP ---------------------------------------------------------------

plot_dat_LRTI_GDP <- AA_result_LRTI_0_11_q1 %>%
  dplyr::select(Country, Year) %>%
  dplyr::left_join(UNI_GDP, by = c("Country", "Year")) %>%
  dplyr::mutate(q1 = AA_result_LRTI_q1, q2 = AA_result_LRTI_q2, q3 = AA_result_LRTI_q3, q4 = AA_result_LRTI_q4, q5 = AA_result_LRTI_q5) %>%
  dplyr::filter(is.finite(GDP), GDP > 0, dplyr::if_all(dplyr::all_of(wealth_names), is.finite)) %>%
  dplyr::arrange(GDP)

result_matrix_LRTI_GDP <- as.matrix(plot_dat_LRTI_GDP[, wealth_names])



# Fever data: GDP --------------------------------------------------------------

plot_dat_fever_GDP <- AA_result_fever_0_11_q1 %>%
  dplyr::select(Country, Year) %>%
  dplyr::left_join(UNI_GDP, by = c("Country", "Year")) %>%
  dplyr::mutate(q1 = AA_result_fever_q1, q2 = AA_result_fever_q2, q3 = AA_result_fever_q3, q4 = AA_result_fever_q4, q5 = AA_result_fever_q5)

plot_dat_fever_GDP <- plot_dat_fever_GDP %>% dplyr::arrange(GDP)
result_matrix_fever_GDP <- as.matrix(plot_dat_fever_GDP[, wealth_names])


# PDF --------------------------------------------------------------------------

# pdf("figures/subgroup_analysis/AA_LRTI_fever_by_wealth_2023.pdf", width = 6, height = 2, useDingbats = FALSE)
# layout(matrix(c(1, 2, 3, 4), nrow = 1), widths = c(0.1, 1, 1, 0.4))


# Shared vertical y-axis title --------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()
mtext("Predicted treatment proportion", side = 2, line = -1.5, cex = 0.5, las = 0)


# Panel 1: LRTI versus GDP ------------------------------------------------------

par(bty = "l", las = 1, mar = c(3, 2, 1.4, 0.5), mgp = c(1.9, 0.45, 0), tcl = -0.2)

plot(plot_dat_LRTI_GDP$GDP, result_matrix_LRTI_GDP[, 1], type = "n", log = "x", xlim = c(500, 20000), ylim = c(0.3, 0.7), axes = FALSE, ann = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

axis(1, at = x_at_GDP, labels = x_labels_GDP, cex.axis = 0.55, mgp = c(0.35, 0.025, 0))
axis(2, at = c(0.3, 0.4, 0.5, 0.6, 0.7), labels = sprintf("%.2f", c(0.3, 0.4, 0.5, 0.6, 0.7)), cex.axis = 0.55)

mtext("GDP per capita (PPP)", side = 1, line = 2, cex = 0.5)
mtext("LRTI", side = 3, line = 0.3, cex = 0.5, font = 2)

for (i in seq_along(wealth_names)) {lines(plot_dat_LRTI_GDP$GDP, result_matrix_LRTI_GDP[, i], col = cols[i], lwd = line_widths[i], lty = 1)}

# Panel 3: Fever versus GDP -----------------------------------------------------

par(bty = "l", las = 1, mar = c(3, 2, 1.4, 0.5), mgp = c(1.9, 0.45, 0), tcl = -0.2)

plot(plot_dat_fever_GDP$GDP, plot_dat_fever_GDP$q5, type = "n", log = "x", xlim = c(500, 20000), ylim = c(0.2, 0.6), axes = FALSE, ann = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

axis(1, at = x_at_GDP, labels = x_labels_GDP, cex.axis = 0.55, mgp = c(0.35, 0.025, 0))
axis(2, at = c(0.2, 0.3, 0.4, 0.5, 0.6), labels = sprintf("%.2f", c(0.2, 0.3, 0.4, 0.5, 0.6)), cex.axis = 0.55)

mtext("GDP per capita (PPP)", side = 1, line = 2, cex = 0.5)
mtext("Fever", side = 3, line = 0.3, cex = 0.5, font = 2)

for (i in seq_along(wealth_names)) {lines(plot_dat_fever_GDP$GDP, result_matrix_fever_GDP[, i], col = cols[i], lwd = line_widths[i], lty = 1)}

# Panel 5: Wealth legend --------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", legend = wealth_labels, title = "Wealth", col = cols, lwd = line_widths, lty = 1, bty = "n", cex = 0.6, title.cex = 0.6, x.intersp = 0.5, y.intersp = 0.8)

# dev.off()










bacterial_LRTI_death_rate_matrix_list  <- readRDS(file.path("outputs/pipelines/mortality_by_wealth_quintile/bacterial_LRTI_death_rate_matrices_by_wealth_quintile.rds"))
bacterial_fever_death_rate_matrix_list <- readRDS(file.path("outputs/pipelines/mortality_by_wealth_quintile/bacterial_fever_death_rate_matrices_by_wealth_quintile.rds"))

UNI_check_row <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
row_2023 <- which(UNI_check_row$Year == 2023 & UNI_check_row$.imp_pc == 1)

wealth_list <- c("q1", "q2", "q3", "q4", "q5")
outcome_list <- c("LRTI", "fever")
age_list <- c("0_11", "12_23", "24_59")

mortality_list <- list(
  LRTI = bacterial_LRTI_death_rate_matrix_list,
  fever = bacterial_fever_death_rate_matrix_list
)

age_map <- c(
  "0_11" = "year0",
  "12_23" = "year1",
  "24_59" = "year2to4"
)

for (outcome_i in outcome_list) {
  for (wealth_i in wealth_list) {
    dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
    
    mortality_list_i <- lapply(age_list, \(age_i) mortality_list[[outcome_i]][[paste0("q_", age_map[age_i], "_", wealth_i)]][row_2023,])
    
    stopifnot(all(vapply(dat_list_i[-1], \(x) identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
    stopifnot(all(vapply(mortality_list_i, nrow, integer(1)) == nrow(dat_list_i[[1]])))
    
    numerator_i <- Reduce(`+`, Map(\(x, p) x * p$age_population, mortality_list_i, dat_list_i))
    denominator_i <- Reduce(`+`, lapply(dat_list_i, \(x) x$age_population))
    
    result_i <- numerator_i / denominator_i
    
    assign(paste0("mortality_result_", outcome_i, "_", wealth_i, "_draw"), result_i)
  }
}

for (outcome_i in outcome_list) {
  for (wealth_i in wealth_list) {
    dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
    result_i <- get(paste0("mortality_result_", outcome_i, "_", wealth_i, "_draw"))
    
    assign(paste0("mortality_result_", outcome_i, "_", wealth_i, "_summary"), data.frame(
      Country = dat_list_i[[1]]$Country,
      Year = dat_list_i[[1]]$Year,
      wealth_i = wealth_i,
      median = apply(result_i, 1, quantile, 0.5),
      lower = apply(result_i, 1, quantile, 0.025),
      upper = apply(result_i, 1, quantile, 0.975)
    ))
  }
}

mortality_result_LRTI <- bind_rows(mortality_result_LRTI_q1_summary,
                                   mortality_result_LRTI_q2_summary,
                                   mortality_result_LRTI_q3_summary,
                                   mortality_result_LRTI_q4_summary,
                                   mortality_result_LRTI_q5_summary)

mortality_result_fever <- bind_rows(mortality_result_fever_q1_summary,
                                    mortality_result_fever_q2_summary,
                                    mortality_result_fever_q3_summary,
                                    mortality_result_fever_q4_summary,
                                    mortality_result_fever_q5_summary)

write.csv(mortality_result_LRTI, "outputs/pipelines/suppl_table/quintile_mortality_lrti.csv")
write.csv(mortality_result_fever, "outputs/pipelines/suppl_table/quintile_mortality_fever.csv")







## CFR code ================
for (outcome_i in outcome_list) {
  
  if (outcome_i == "LRTI") {
    outcome_tag_i <- "lrti"
  } else {
    outcome_tag_i <- "fever"
  }
  
  for (wealth_i in wealth_list) {
    
    # Age-specific population data
    dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
    # Age-specific mortality draws
    mortality_list_i <- lapply(age_list, \(age_i) mortality_list[[outcome_i]][[paste0("q_", age_map[age_i], "_", wealth_i)]][row_2023,])
    # Age-specific incidence data
    incidence_list_i <- lapply(age_list, \(age_i) {
      get(paste0(wealth_i, "_inc_bact_", outcome_tag_i, "_", age_i)) %>%
        mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
        filter(Year == 2023) %>%
        arrange(match(Country, dat_list_i[[1]]$Country))
    })
    
    # Check country/year alignment across age groups
    stopifnot(all(vapply(dat_list_i[-1], \(x) identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
    stopifnot(all(vapply(incidence_list_i, \(x) identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
    
    # Check row dimensions
    stopifnot(all(vapply(mortality_list_i, nrow, integer(1)) == nrow(dat_list_i[[1]])))
    stopifnot(all(vapply(incidence_list_i, nrow, integer(1)) == nrow(dat_list_i[[1]])))
    
    # Metadata columns
    metadata_cols_i <- c("Country", "Year", "GDP", "Income", "gbd_region", "Endemic", "Population", "age_population")
    
    # Convert incidence draws to matrices
    # First non-metadata column is population, so exclude it
    incidence_matrix_list_i <- lapply(incidence_list_i, \(x) {
      draw_cols_i <- setdiff(names(x), metadata_cols_i)
      as.matrix(x[,draw_cols_i[-1]])
    })
    
    # Check that mortality and incidence both contain 1000 draws
    stopifnot(all(vapply(incidence_matrix_list_i, ncol, integer(1)) == 1000))
    stopifnot(all(vapply(mortality_list_i, ncol, integer(1)) == 1000))
    
    # Expected deaths by draw:
    # sum(age-specific mortality risk × age-specific population)
    death_draw_i <- Reduce(`+`, Map(\(x, p) x * p$age_population, mortality_list_i, dat_list_i))
    
    # Expected cases by draw:
    # sum(age-specific incidence rate × age-specific population)
    case_draw_i <- Reduce(`+`, Map(\(x, p) x * p$age_population, incidence_matrix_list_i, dat_list_i))
    
    # Case fatality risk by draw
    cfr_draw_i <- death_draw_i / case_draw_i
    cfr_draw_i[!is.finite(cfr_draw_i)] <- NA
    
    colnames(cfr_draw_i) <- colnames(incidence_matrix_list_i[[1]])
    rownames(cfr_draw_i) <- dat_list_i[[1]]$Country
    
    # Save draw-level results
    assign(paste0("CFR_result_", outcome_i, "_", wealth_i, "_draw"), cfr_draw_i)
    
    # Save summary results
    assign(paste0("CFR_result_", outcome_i, "_", wealth_i, "_summary"), data.frame(
      Country = dat_list_i[[1]]$Country,
      Year = dat_list_i[[1]]$Year,
      wealth_i = wealth_i,
      median = apply(cfr_draw_i, 1, quantile, 0.5, na.rm = TRUE),
      lower = apply(cfr_draw_i, 1, quantile, 0.025, na.rm = TRUE),
      upper = apply(cfr_draw_i, 1, quantile, 0.975, na.rm = TRUE)
    ))
  }
}

CFR_result_LRTI <- bind_rows(CFR_result_LRTI_q1_summary,
                                   CFR_result_LRTI_q2_summary,
                                   CFR_result_LRTI_q3_summary,
                                   CFR_result_LRTI_q4_summary,
                                   CFR_result_LRTI_q5_summary)

CFR_result_fever <- bind_rows(CFR_result_fever_q1_summary,
                                    CFR_result_fever_q2_summary,
                                    CFR_result_fever_q3_summary,
                                    CFR_result_fever_q4_summary,
                                    CFR_result_fever_q5_summary)

write.csv(CFR_result_LRTI, "outputs/pipelines/suppl_table/quintile_CFR_lrti.csv")
write.csv(CFR_result_fever, "outputs/pipelines/suppl_table/quintile_CFR_fever.csv")















# _____________________________________________ ----------
# 3. Bacterial mortality by household-wealth scenario --------------------------






# 3-1. GDP ---------------------------------------------------------------------

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")

UNI_GDP <- UNI %>%
  transmute(
    Country = as.character(Country),
    Year = as.numeric(Year),
    GDP = as.numeric(GDP),
  ) %>% 
  filter(Year == 2023) %>%
  distinct(Country, Year, .keep_all = TRUE)



mortality_result_LRTI <- bind_cols(
  q1 = mortality_result_LRTI_q1_summary$median*1000,
  q2 = mortality_result_LRTI_q2_summary$median*1000,
  q3 = mortality_result_LRTI_q3_summary$median*1000,
  q4 = mortality_result_LRTI_q4_summary$median*1000,
  q5 = mortality_result_LRTI_q5_summary$median*1000
)

mortality_result_fever <- bind_cols(
  q1 = mortality_result_fever_q1_summary$median*1000,
  q2 = mortality_result_fever_q2_summary$median*1000,
  q3 = mortality_result_fever_q3_summary$median*1000,
  q4 = mortality_result_fever_q4_summary$median*1000,
  q5 = mortality_result_fever_q5_summary$median*1000
)


# 3-4. Prepare GDP plotting tables ---------------------------------------------

wealth_names <- c("q5", "q4", "q3", "q2", "q1")
wealth_labels <- c("Richest", "Richer", "Middle", "Poorer", "Poorest")

base_cols <- c("#4575B4", "#00A6D6", "grey20", "#F46D43", "#D73027")

cols <- adjustcolor(base_cols, alpha.f = 0.7)
cols[3] <- "grey20"

line_widths <- c(0.5, 0.5, 0.9, 0.5, 0.5)


# 3-5. Define mortality-rate axes ----------------------------------------------

y_at_LRTI <- c(0.0, 0.5, 1.0, 1.5, 2.0)
y_max_LRTI <- max(y_at_LRTI)
y_at_fever <- c(0.0, 0.5, 1.0, 1.5, 2.0)
y_max_fever <- max(y_at_fever)


# 3-6. Plot --------------------------------------------------------------------

dir.create(
  "figures/subgroup_analysis",
  recursive = TRUE,
  showWarnings = FALSE
)

# pdf(
#   "figures/subgroup_analysis/bacterial_mortality_LRTI_fever_by_wealth_2023.pdf",
#   width = 6,
#   height = 2,
#   useDingbats = FALSE
# )
# 
# layout(
#   matrix(c(1, 2, 3, 4), nrow = 1),
#   widths = c(0.1, 1, 1, 0.4)
# )


# Shared vertical y-axis title --------------------------------------------------

# dev.off()



# _____________________________________________ ----------
# All-age antibiotic-treatment probability draws -------------------------------

make_AA_all_age_draw <- function(outcome_i, wealth_i){
  
  dat_list_i <- lapply(age_list, \(age_i) get(paste0("AA_result_", outcome_i, "_", age_i, "_", wealth_i, "_draw")))
  
  stopifnot(all(vapply(dat_list_i[-1], \(x)
                       identical(x$Country, dat_list_i[[1]]$Country) && identical(x$Year, dat_list_i[[1]]$Year), logical(1))))
  
  draw_matrix_list_i <- lapply(dat_list_i, \(x) as.matrix(x[,10:1009]))
  
  numerator_i <- Reduce(`+`, Map(\(x, p) x * p$age_population, draw_matrix_list_i, dat_list_i))
  denominator_i <- Reduce(`+`, lapply(dat_list_i, \(x) x$age_population))
  
  result_i <- numerator_i / denominator_i
  
  rownames(result_i) <- dat_list_i[[1]]$Country
  colnames(result_i) <- colnames(draw_matrix_list_i[[1]])
  
  result_i
}

AA_result_LRTI_q1_draw  <- make_AA_all_age_draw("LRTI", "q1")
AA_result_LRTI_q5_draw  <- make_AA_all_age_draw("LRTI", "q5")
AA_result_fever_q1_draw <- make_AA_all_age_draw("fever", "q1")
AA_result_fever_q5_draw <- make_AA_all_age_draw("fever", "q5")






mortality_result_fever_q1_draw
mortality_result_fever_q5_draw
mortality_result_LRTI_q1_draw
mortality_result_LRTI_q5_draw
incidence_result_fever_q1_draw
incidence_result_fever_q5_draw
incidence_result_LRTI_q1_draw
incidence_result_LRTI_q5_draw
AA_result_LRTI_q1_draw 
AA_result_LRTI_q5_draw 
AA_result_fever_q1_draw
AA_result_fever_q5_draw
CFR_result_fever_q1_draw
CFR_result_fever_q5_draw
CFR_result_LRTI_q1_draw
CFR_result_LRTI_q5_draw

UNI_income <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>%
  filter(.imp_pc == 1, Year == 2023) %>%
  dplyr::transmute(Country = as.character(Country), Income = as.character(Income), Population = as.numeric(Population)) %>%
  distinct(Country, .keep_all = TRUE)


# Overall poorest-versus-richest estimates across 78 countries -----------------

population_i <- result_LRTI_q1$age_population

stopifnot(
  length(population_i) == nrow(incidence_result_LRTI_q1_draw),
  all(is.finite(population_i)),
  all(population_i > 0)
)

population_weight_rate <- function(draw_matrix_i, population_i){
  colSums(sweep(draw_matrix_i, 1, population_i, `*`), na.rm = TRUE) / sum(population_i)
}

overall_CFR <- function(mortality_i, incidence_i, population_i){
  death_i <- colSums(sweep(mortality_i, 1, population_i, `*`), na.rm = TRUE)
  case_i <- colSums(sweep(incidence_i, 1, population_i, `*`), na.rm = TRUE)
  death_i / case_i
}

overall_AA <- function(AA_i, incidence_i, population_i){
  case_i <- sweep(incidence_i, 1, population_i, `*`)
  colSums(AA_i * case_i, na.rm = TRUE) / colSums(case_i, na.rm = TRUE)
}

summarize_ratio <- function(q1_i, q5_i){
  ratio_i <- q1_i / q5_i
  ratio_i[!is.finite(ratio_i)] <- NA
  
  c(
    median = median(ratio_i, na.rm = TRUE),
    lower = quantile(ratio_i, 0.025, na.rm = TRUE),
    upper = quantile(ratio_i, 0.975, na.rm = TRUE)
  )
}

incidence_LRTI_q1_overall <- population_weight_rate(incidence_result_LRTI_q1_draw, population_i)
incidence_LRTI_q5_overall <- population_weight_rate(incidence_result_LRTI_q5_draw, population_i)
incidence_fever_q1_overall <- population_weight_rate(incidence_result_fever_q1_draw, population_i)
incidence_fever_q5_overall <- population_weight_rate(incidence_result_fever_q5_draw, population_i)
summarize_ratio(incidence_LRTI_q1_overall, incidence_LRTI_q5_overall)
summarize_ratio(incidence_fever_q1_overall, incidence_fever_q5_overall)

mortality_LRTI_q1_overall <- population_weight_rate(mortality_result_LRTI_q1_draw, population_i)
mortality_LRTI_q5_overall <- population_weight_rate(mortality_result_LRTI_q5_draw, population_i)
mortality_fever_q1_overall <- population_weight_rate(mortality_result_fever_q1_draw, population_i)
mortality_fever_q5_overall <- population_weight_rate(mortality_result_fever_q5_draw, population_i)
summarize_ratio(mortality_LRTI_q1_overall, mortality_LRTI_q5_overall)
summarize_ratio(mortality_fever_q1_overall, mortality_fever_q5_overall)

CFR_LRTI_q1_overall <- overall_CFR(mortality_result_LRTI_q1_draw, incidence_result_LRTI_q1_draw, population_i)
CFR_LRTI_q5_overall <- overall_CFR(mortality_result_LRTI_q5_draw, incidence_result_LRTI_q5_draw, population_i)
CFR_fever_q1_overall <- overall_CFR(mortality_result_fever_q1_draw, incidence_result_fever_q1_draw, population_i)
CFR_fever_q5_overall <- overall_CFR(mortality_result_fever_q5_draw, incidence_result_fever_q5_draw, population_i)
summarize_ratio(CFR_LRTI_q1_overall, CFR_LRTI_q5_overall)
summarize_ratio(CFR_fever_q1_overall, CFR_fever_q5_overall)

AA_LRTI_q1_overall <- overall_AA(AA_result_LRTI_q1_draw, incidence_result_LRTI_q1_draw, population_i)
AA_LRTI_q5_overall <- overall_AA(AA_result_LRTI_q5_draw, incidence_result_LRTI_q5_draw, population_i)
AA_fever_q1_overall <- overall_AA(AA_result_fever_q1_draw, incidence_result_fever_q1_draw, population_i)
AA_fever_q5_overall <- overall_AA(AA_result_fever_q5_draw, incidence_result_fever_q5_draw, population_i)
summarize_ratio(AA_LRTI_q1_overall, AA_LRTI_q5_overall)
summarize_ratio(AA_fever_q1_overall, AA_fever_q5_overall)

AA_LRTI_reduction_i <- 1 - AA_LRTI_q1_overall / AA_LRTI_q5_overall
AA_fever_reduction_i <- 1 - AA_fever_q1_overall / AA_fever_q5_overall
round(quantile(AA_LRTI_reduction_i * 100, c(0.5, 0.025, 0.975), na.rm = TRUE), digits = 1)
round(quantile(AA_fever_reduction_i * 100, c(0.5, 0.025, 0.975), na.rm = TRUE), digits = 1)


# _____________________________________________ ----------
# 4. Overall plot --------------------------
# prepare dataset for Mortality, Incidence, and AA

mort_result_LRTI  <- read.csv("outputs/pipelines/suppl_table/quintile_mortality_lrti.csv" ) %>% dplyr::select(Country, wealth_i, median, lower, upper)
mort_result_fever <- read.csv("outputs/pipelines/suppl_table/quintile_mortality_fever.csv") %>% dplyr::select(Country, wealth_i, median, lower, upper)
AA_result_LRTI         <- read.csv("outputs/pipelines/suppl_table/quintile_AA_lrti.csv" )        %>% dplyr::select(Country, wealth_i, median, lower, upper)
AA_result_fever        <- read.csv("outputs/pipelines/suppl_table/quintile_AA_fever.csv")        %>% dplyr::select(Country, wealth_i, median, lower, upper)
CFR_result_LRTI        <- read.csv("outputs/pipelines/suppl_table/quintile_CFR_lrti.csv" )       %>% dplyr::select(Country, wealth_i, median, lower, upper)
CFR_result_fever       <- read.csv("outputs/pipelines/suppl_table/quintile_CFR_fever.csv")       %>% dplyr::select(Country, wealth_i, median, lower, upper)
inc_result_LRTI        <- read.csv("outputs/pipelines/suppl_table/quintile_inc_lrti.csv" )       %>% dplyr::select(Country, wealth_tag, predicted_annual_rate_median, predicted_annual_rate_lower, predicted_annual_rate_upper)
inc_result_fever       <- read.csv("outputs/pipelines/suppl_table/quintile_inc_fever.csv")       %>% dplyr::select(Country, wealth_tag, predicted_annual_rate_median, predicted_annual_rate_lower, predicted_annual_rate_upper)


colnames(inc_result_LRTI ) <- c("Country", "wealth_i", "median", "lower", "upper")
colnames(inc_result_fever) <- c("Country", "wealth_i", "median", "lower", "upper")

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")

UNI_GDP <- UNI %>% filter(UNI$Year == 2023 & UNI$.imp_pc == 1) %>%
  transmute(Country = as.character(Country), GDP = as.numeric(GDP))

mort_result_LRTI  <- mort_result_LRTI   %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
mort_result_fever <- mort_result_fever  %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
inc_result_LRTI        <- inc_result_LRTI         %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
inc_result_fever       <- inc_result_fever        %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
AA_result_LRTI         <- AA_result_LRTI          %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
AA_result_fever        <- AA_result_fever         %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
CFR_result_LRTI        <- CFR_result_LRTI         %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")
CFR_result_fever       <- CFR_result_fever        %>% inner_join(UNI_GDP[,c("Country", "GDP")], by = "Country")

lowess_fraction <- 2/3

add_lowess_line <- function(dat, col, log_y = TRUE, lwd = 1){
  valid_i <- is.finite(dat$GDP) & dat$GDP > 0 & is.finite(dat$median)
  if(log_y) valid_i <- valid_i & dat$median > 0
  
  smooth_i <- lowess(x = log10(dat$GDP[valid_i]),
                     y = if(log_y) log10(dat$median[valid_i]) else dat$median[valid_i],
                     f = lowess_fraction, iter = 3)
  
  lines(smooth_i$x, smooth_i$y, col = col, lwd = lwd, lty = 1)
}

pdf("figures/subgroup_analysis/(Main 3) by_wealth_plot.pdf", width = 6, height = 3.5, useDingbats = FALSE)

layout(
  matrix(c(
    0, 0, 0, 0, 9,
    1, 3, 5, 7, 9,
    0, 0, 0, 0, 9,
    2, 4, 6, 8, 9
  ), nrow = 4, byrow = TRUE),
  widths = c(1, 1, 1, 1, 1),
  heights = c(0.1, 1, 0.1, 1)
)


## 0. Plotting specifications --------------------------------------------------
cex.axis_value <- 0.7
cex.axis_title <- 0.55
cex_title <- 0.55
cex_legend <- 0.70
cex_legend_title <- 0.70
cex_point <- 0.60
multiple <- 1000

## GDP axis: log10 positions with labels on the original scale
xmin <- 100
xmax <- 20000

x_tick_values <- c(100, 1000, 10000)
x_minor_tick_values <- c(1:9*100, 2:9*1000, 20000)
x_ticks <- log10(x_tick_values)
x_minor_ticks <- log10(x_minor_tick_values)
x_labels <- parse(text = paste0("10^", x_ticks))
xlim_log <- log10(c(xmin, xmax))

# y axis
LRTI_inc_y_exp <- 0:3
LRTI_inc_y_ticks <- LRTI_inc_y_exp
LRTI_inc_y_labels <- parse(text = paste0("10^", LRTI_inc_y_exp))
LRTI_inc_ylim <- range(LRTI_inc_y_ticks)
LRTI_inc_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_inc_y_exp):max(LRTI_inc_y_exp)-1), \(x) x * (2:9))))

LRTI_mort_y_exp <- -2:2
LRTI_mort_y_ticks <- LRTI_mort_y_exp
LRTI_mort_y_labels <- parse(text = paste0("10^", LRTI_mort_y_exp))
LRTI_mort_ylim <- range(LRTI_mort_y_ticks)
LRTI_mort_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_mort_y_exp):max(LRTI_mort_y_exp)-1), \(x) x * (2:9))))

LRTI_CFR_y_exp <- -1:3
LRTI_CFR_y_ticks <- LRTI_CFR_y_exp
LRTI_CFR_y_labels <- parse(text = paste0("10^", LRTI_CFR_y_exp))
LRTI_CFR_ylim <- range(LRTI_CFR_y_ticks)
LRTI_CFR_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_CFR_y_exp):max(LRTI_CFR_y_exp)-1), \(x) x * (2:9))))

fever_inc_y_exp <- 0:3
fever_inc_y_ticks <- fever_inc_y_exp
fever_inc_y_labels <- parse(text = paste0("10^", fever_inc_y_exp))
fever_inc_ylim <- range(fever_inc_y_ticks)
fever_inc_y_minor_ticks <- log10(unlist(lapply(10^(min(fever_inc_y_exp):max(fever_inc_y_exp)-1), \(x) x * (2:9))))

fever_mort_y_exp <- -2:2
fever_mort_y_ticks <- fever_mort_y_exp
fever_mort_y_labels <- parse(text = paste0("10^", fever_mort_y_exp))
fever_mort_ylim <- range(fever_mort_y_ticks)
fever_mort_y_minor_ticks <- log10(unlist(lapply(10^(min(fever_mort_y_exp):max(fever_mort_y_exp)-1), \(x) x * (2:9))))

fever_CFR_y_exp <- -1:3
fever_CFR_y_ticks <- fever_CFR_y_exp
fever_CFR_y_labels <- parse(text = paste0("10^", fever_CFR_y_exp))
fever_CFR_ylim <- range(fever_CFR_y_ticks)
fever_CFR_y_minor_ticks <- log10(unlist(lapply(10^(min(fever_CFR_y_exp):max(fever_CFR_y_exp)-1), \(x) x * (2:9))))

wealth_cols_fixed <- c("#CC79A7", "#0072B2")
wealth_labels <- c(
  "Poorest quintile" = "q1",
  "Richest quintile" = "q5"
)

line_widths <- c(1, 1, 1, 1, 1)
line_types <- rep(1, 5)


## GDP axis: positions on log10 scale, labels on original GDP scale ------------

# _____________________________________________ ----------
# 1. Incidence ---------------------------------------------------------------

## 1-0. Incidence plotting data -----------------------------------------------

inc_result_LRTI 
inc_result_fever

# LRTI
plot_dat_LRTI <- inc_result_LRTI %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_lrti_inc_GDP_q1 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_lrti_inc_GDP_q5 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

# Fever
plot_dat_fever <- inc_result_fever %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_fever_inc_GDP_q1 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_fever_inc_GDP_q5 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

## 1-1. Panel 1: Bacterial LRTI incidence versus GDP --------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = LRTI_inc_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = LRTI_inc_y_ticks, labels = LRTI_inc_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = LRTI_inc_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(plot_dat_lrti_inc_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_lrti_inc_GDP_q1$lower),
  x1 = log10(plot_dat_lrti_inc_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_lrti_inc_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_inc_GDP_q1$GDP)-0.015,
#   log10(plot_dat_lrti_inc_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_lrti_inc_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_lrti_inc_GDP_q5$lower),
  x1 = log10(plot_dat_lrti_inc_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_lrti_inc_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_inc_GDP_q5$GDP)+0.015,
#   log10(plot_dat_lrti_inc_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_lrti_inc_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_lrti_inc_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("Incidence rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("a", side = 3, adj = -0.53, line = 1.2, cex = 0.75, font = 2)


## 1-2. Panel 2: Bacterial fever incidence versus GDP -------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)


plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fever_inc_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fever_inc_y_ticks, labels = fever_inc_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fever_inc_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(plot_dat_fever_inc_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_fever_inc_GDP_q1$lower),
  x1 = log10(plot_dat_fever_inc_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_fever_inc_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_inc_GDP_q1$GDP)-0.015,
#   log10(plot_dat_fever_inc_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_fever_inc_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_fever_inc_GDP_q5$lower),
  x1 = log10(plot_dat_fever_inc_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_fever_inc_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_inc_GDP_q5$GDP)+0.015,
#   log10(plot_dat_fever_inc_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_fever_inc_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_fever_inc_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("Incidence rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("b", side = 3, adj = -0.55, line = 1.2, cex = 0.75, font = 2)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)

# _____________________________________________ ----------
# 2. Mortality ----------------------------------------------------------------

## 2-0. Mortality plotting data -----------------------------------------------




mort_result_LRTI 
mort_result_fever

# LRTI
plot_dat_LRTI <- mort_result_LRTI %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_lrti_mort_GDP_q1 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_lrti_mort_GDP_q5 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

# Fever
plot_dat_fever <- mort_result_fever %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_fever_mort_GDP_q1 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_fever_mort_GDP_q5 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

## 2-1. Panel 1: Bacterial LRTI mortality versus GDP --------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = LRTI_mort_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = LRTI_mort_y_ticks, labels = LRTI_mort_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = LRTI_mort_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(plot_dat_lrti_mort_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_lrti_mort_GDP_q1$lower),
  x1 = log10(plot_dat_lrti_mort_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_lrti_mort_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_mort_GDP_q1$GDP)-0.015,
#   log10(plot_dat_lrti_mort_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_lrti_mort_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_lrti_mort_GDP_q5$lower),
  x1 = log10(plot_dat_lrti_mort_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_lrti_mort_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_mort_GDP_q5$GDP)+0.015,
#   log10(plot_dat_lrti_mort_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_lrti_mort_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_lrti_mort_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("mortality rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("c", side = 3, adj = -0.54, line = 1.2, cex = 0.75, font = 2)


## 2-2. Panel 2: Bacterial fever mortality versus GDP -------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)


plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fever_mort_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fever_mort_y_ticks, labels = fever_mort_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fever_mort_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(plot_dat_fever_mort_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_fever_mort_GDP_q1$lower),
  x1 = log10(plot_dat_fever_mort_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_fever_mort_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_mort_GDP_q1$GDP)-0.015,
#   log10(plot_dat_fever_mort_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_fever_mort_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_fever_mort_GDP_q5$lower),
  x1 = log10(plot_dat_fever_mort_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_fever_mort_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_mort_GDP_q5$GDP)+0.015,
#   log10(plot_dat_fever_mort_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_fever_mort_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_fever_mort_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("mortality rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("d", side = 3, adj = -0.55, line = 1.2, cex = 0.75, font = 2)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)

# _____________________________________________ ----------
# 3. Mortality-to-incidence ratio ---------------------------------------------

## 3-0. ratio plotting data -----------------------------------------------




CFR_result_LRTI 
CFR_result_fever

# LRTI
plot_dat_LRTI <- CFR_result_LRTI %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_lrti_CFR_GDP_q1 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_lrti_CFR_GDP_q5 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

# Fever
plot_dat_fever <- CFR_result_fever %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 1000,
    lower = lower * 1000,
    upper = upper * 1000
  )

plot_dat_fever_CFR_GDP_q1 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_fever_CFR_GDP_q5 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

## 3-1. Panel 1: Bacterial LRTI ratio versus GDP --------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = LRTI_CFR_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = LRTI_CFR_y_ticks, labels = LRTI_CFR_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = LRTI_CFR_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(plot_dat_lrti_CFR_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_lrti_CFR_GDP_q1$lower),
  x1 = log10(plot_dat_lrti_CFR_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_lrti_CFR_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_CFR_GDP_q1$GDP)-0.015,
#   log10(plot_dat_lrti_CFR_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_lrti_CFR_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_lrti_CFR_GDP_q5$lower),
  x1 = log10(plot_dat_lrti_CFR_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_lrti_CFR_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_CFR_GDP_q5$GDP)+0.015,
#   log10(plot_dat_lrti_CFR_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )


add_lowess_line(plot_dat_lrti_CFR_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_lrti_CFR_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("Case-fatality risk per 1000 cases", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("e", side = 3, adj = -0.55, line = 1.2, cex = 0.75, font = 2)


## 3-2. Panel 2: Bacterial fever ratio versus GDP -------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)


plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fever_CFR_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fever_CFR_y_ticks, labels = fever_CFR_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fever_CFR_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(plot_dat_fever_CFR_GDP_q1$GDP)-0.015,
  y0 = log10(plot_dat_fever_CFR_GDP_q1$lower),
  x1 = log10(plot_dat_fever_CFR_GDP_q1$GDP)-0.015,
  y1 = log10(plot_dat_fever_CFR_GDP_q1$upper),
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_CFR_GDP_q1$GDP)-0.015,
#   log10(plot_dat_fever_CFR_GDP_q1$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_fever_CFR_GDP_q5$GDP)+0.015,
  y0 = log10(plot_dat_fever_CFR_GDP_q5$lower),
  x1 = log10(plot_dat_fever_CFR_GDP_q5$GDP)+0.015,
  y1 = log10(plot_dat_fever_CFR_GDP_q5$upper),
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_CFR_GDP_q5$GDP)+0.015,
#   log10(plot_dat_fever_CFR_GDP_q5$median),
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_fever_CFR_GDP_q1, col = wealth_cols_fixed[1], log_y = TRUE)
add_lowess_line(plot_dat_fever_CFR_GDP_q5, col = wealth_cols_fixed[2], log_y = TRUE)

mtext("Case-fatality risk per 1000 cases", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("f", side = 3, adj = -0.50, line = 1.2, cex = 0.75, font = 2)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)



# _____________________________________________ ----------
# 4. Antibiotic treatment proportion ------------------------------------------

## 4-0. antibiotic treatment plotting data -----------------------------------------------




AA_result_LRTI 
AA_result_fever

# LRTI
plot_dat_LRTI <- AA_result_LRTI %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 100,
    lower = lower * 100,
    upper = upper * 100
  )

plot_dat_lrti_AA_GDP_q1 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_lrti_AA_GDP_q5 <- plot_dat_LRTI %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

# Fever
plot_dat_fever <- AA_result_fever %>%
  filter(wealth_i %in% c("q1", "q5")) %>%
  dplyr::select(Country, wealth_i, median, lower, upper, GDP) %>%
  mutate(
    median = median * 100,
    lower = lower * 100,
    upper = upper * 100
  )

plot_dat_fever_AA_GDP_q1 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q1") %>%
  arrange(GDP)

plot_dat_fever_AA_GDP_q5 <- plot_dat_fever %>%
  filter(is.finite(GDP), GDP > 0) %>% filter(wealth_i == "q5") %>%
  arrange(GDP)

## 4-1. Panel 1: Bacterial LRTI antibiotic treatment versus GDP --------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = c(0,100),
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)

axis(2, at = c(0, 25, 50, 75, 100), labels = c("0", "25", "50", "75", "100"), las = 1, cex.axis = cex.axis_value, tck = -0.025)

mtext("Proportion treated (%)", side = 2, line = 1.8, cex = cex.axis_title, las = 0)

mtext("g", side = 3, adj = -0.55, line = 1.2, cex = 0.75, font = 2)

segments(
  x0 = log10(plot_dat_lrti_AA_GDP_q1$GDP)-0.015,
  y0 = plot_dat_lrti_AA_GDP_q1$lower,
  x1 = log10(plot_dat_lrti_AA_GDP_q1$GDP)-0.015,
  y1 = plot_dat_lrti_AA_GDP_q1$upper,
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_AA_GDP_q1$GDP)-0.015,
#   plot_dat_lrti_AA_GDP_q1$median,
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_lrti_AA_GDP_q5$GDP)+0.015,
  y0 = plot_dat_lrti_AA_GDP_q5$lower,
  x1 = log10(plot_dat_lrti_AA_GDP_q5$GDP)+0.015,
  y1 = plot_dat_lrti_AA_GDP_q5$upper,
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_lrti_AA_GDP_q5$GDP)+0.015,
#   plot_dat_lrti_AA_GDP_q5$median,
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_lrti_AA_GDP_q1, col = wealth_cols_fixed[1], log_y = FALSE)
add_lowess_line(plot_dat_lrti_AA_GDP_q5, col = wealth_cols_fixed[2], log_y = FALSE)

## 4-2. Panel 2: Bacterial fever antibiotic treatment versus GDP -------------------------

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.0, 1, 1),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = c(0,100),
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)

axis(2, at = c(0, 25, 50, 75, 100), labels = c("0", "25", "50", "75", "100"), las = 1, cex.axis = cex.axis_value, tck = -0.025)

mtext("Proportion treated (%)", side = 2, line = 1.8, cex = cex.axis_title, las = 0)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("h", side = 3, adj = -0.55, line = 1.2, cex = 0.75, font = 2)

segments(
  x0 = log10(plot_dat_fever_AA_GDP_q1$GDP)-0.015,
  y0 = plot_dat_fever_AA_GDP_q1$lower,
  x1 = log10(plot_dat_fever_AA_GDP_q1$GDP)-0.015,
  y1 = plot_dat_fever_AA_GDP_q1$upper,
  col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_AA_GDP_q1$GDP)-0.015,
#   plot_dat_fever_AA_GDP_q1$median,
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[1], alpha.f = 0.60)
# )

segments(
  x0 = log10(plot_dat_fever_AA_GDP_q5$GDP)+0.015,
  y0 = plot_dat_fever_AA_GDP_q5$lower,
  x1 = log10(plot_dat_fever_AA_GDP_q5$GDP)+0.015,
  y1 = plot_dat_fever_AA_GDP_q5$upper,
  col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.15)
)

# points(
#   log10(plot_dat_fever_AA_GDP_q5$GDP)+0.015,
#   plot_dat_fever_AA_GDP_q5$median,
#   pch = 16,
#   cex = cex_point,
#   col = adjustcolor(wealth_cols_fixed[2], alpha.f = 0.60)
# )

add_lowess_line(plot_dat_fever_AA_GDP_q1, col = wealth_cols_fixed[1], log_y = FALSE)
add_lowess_line(plot_dat_fever_AA_GDP_q5, col = wealth_cols_fixed[2], log_y = FALSE)


## Household wealth legend ----------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

text(
  x = -0.05, y = 0.55,
  labels = "[Household wealth quintile]",
  font = 2,
  cex = 0.7,
  pos = 4
)

legend(
  x = 0.00, y = 0.54,
  legend = c("Richest", "Poorest"),
  col = wealth_cols_fixed[c(2, 1)],
  lwd = line_widths[c(2, 1)],
  lty = line_types[c(2, 1)],
  bty = "n",
  cex = 0.7,
  x.intersp = 0.5,
  y.intersp = 1.0
)



# Points represent country-level estimates;
# lines represent LOWESS-smoothed trends by household wealth quintile.

dev.off()
