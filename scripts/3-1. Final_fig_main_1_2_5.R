# _____________________________________________ ----------
# 1. Mortality-to-incidence ratio attributable to lack of effective treatment ----------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

rm(list = ls()); gc()

death_rate_lrti_Age_0      <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_0.rds"              )
death_rate_lrti_Age_1      <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_1.rds"              )
death_rate_lrti_Age_2to4   <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_2to4.rds"           )
death_rate_fev_Age_0       <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_0.rds"             )
death_rate_fev_Age_1       <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_1.rds"             )
death_rate_fev_Age_2to4    <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_2to4.rds"          )

inc_rate_lrti_Age_0        <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_0_11_matrix.rds"  )
inc_rate_lrti_Age_1        <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_12_23_matrix.rds" )
inc_rate_lrti_Age_2to4     <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_24_59_matrix.rds" )
inc_rate_fev_Age_0         <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_0_11_matrix.rds" )
inc_rate_fev_Age_1         <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_12_23_matrix.rds")
inc_rate_fev_Age_2to4      <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_24_59_matrix.rds")


#endemic without latitude
AA_LRTI_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_0_11_months_enet_025_uncertainty.rds")$projection_draw_matrix
AA_LRTI_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_12_23_months_enet_025_uncertainty.rds")$projection_draw_matrix
AA_LRTI_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_24_59_months_enet_025_uncertainty.rds")$projection_draw_matrix
AA_fev_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_0_11_months_gam_uncertainty.rds")$projection_draw_matrix
AA_fev_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_12_23_months_gam_uncertainty.rds")$projection_draw_matrix
AA_fev_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_24_59_months_gam_uncertainty.rds")$projection_draw_matrix

# latitude without endemic
# AA_LRTI_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/LRTI_0_11_months_GAM_uncertainty.rds")$projection_draw_matrix
# AA_LRTI_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/LRTI_12_23_months_GAM_uncertainty.rds")$projection_draw_matrix
# AA_LRTI_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/LRTI_24_59_months_GAM_uncertainty.rds")$projection_draw_matrix
# AA_fev_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/fever_0_11_months_GAM_uncertainty.rds")$projection_draw_matrix
# AA_fev_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/fever_12_23_months_GAM_uncertainty.rds")$projection_draw_matrix
# AA_fev_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_GAM_uncertainty/fever_24_59_months_GAM_uncertainty.rds")$projection_draw_matrix


death_rate_list <- list(death_rate_lrti_Age_0, death_rate_lrti_Age_1, death_rate_lrti_Age_2to4, death_rate_fev_Age_0, death_rate_fev_Age_1, death_rate_fev_Age_2to4)
inc_rate_list   <- list(inc_rate_lrti_Age_0, inc_rate_lrti_Age_1, inc_rate_lrti_Age_2to4, inc_rate_fev_Age_0, inc_rate_fev_Age_1, inc_rate_fev_Age_2to4)
AA_list         <- list(AA_LRTI_Age_0, AA_LRTI_Age_1, AA_LRTI_Age_2to4, AA_fev_Age_0, AA_fev_Age_1, AA_fev_Age_2to4)

UNI                        <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
population                 <- read.csv("data/metadata/Population_by_age.csv") 

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


death_rate_lrti <- (sweep(death_rate_list[[1]], 1, UNI_pop$Age_0,"*") +
                      sweep(death_rate_list[[2]], 1, UNI_pop$Age_1,"*") +
                      sweep(death_rate_list[[3]], 1, UNI_pop$Age_2to4,"*")
) / UNI_pop$Age_all

death_rate_fev <- (sweep(death_rate_list[[4]],1,UNI_pop$Age_0,"*") +
                     sweep(death_rate_list[[5]],1,UNI_pop$Age_1,"*") +
                     sweep(death_rate_list[[6]],1,UNI_pop$Age_2to4,"*")
) / UNI_pop$Age_all

inc_rate_lrti <- (sweep(inc_rate_list[[1]],1,UNI_pop$Age_0,"*") +
                    sweep(inc_rate_list[[2]],1,UNI_pop$Age_1,"*") +
                    sweep(inc_rate_list[[3]],1,UNI_pop$Age_2to4,"*")
) / UNI_pop$Age_all

inc_rate_fev <- (sweep(inc_rate_list[[4]],1,UNI_pop$Age_0,"*") +
                   sweep(inc_rate_list[[5]],1,UNI_pop$Age_1,"*") +
                   sweep(inc_rate_list[[6]],1,UNI_pop$Age_2to4,"*")
) / UNI_pop$Age_all


# Age-specific numbers of bacterial episodes
inc_count_lrti_Age_0    <- sweep(inc_rate_list[[1]], 1, UNI_pop$Age_0, "*")
inc_count_lrti_Age_1    <- sweep(inc_rate_list[[2]], 1, UNI_pop$Age_1, "*")
inc_count_lrti_Age_2to4 <- sweep(inc_rate_list[[3]], 1, UNI_pop$Age_2to4, "*")

inc_count_fev_Age_0    <- sweep(inc_rate_list[[4]], 1, UNI_pop$Age_0, "*")
inc_count_fev_Age_1    <- sweep(inc_rate_list[[5]], 1, UNI_pop$Age_1, "*")
inc_count_fev_Age_2to4 <- sweep(inc_rate_list[[6]], 1, UNI_pop$Age_2to4, "*")

inc_count_lrti <- inc_count_lrti_Age_0 + inc_count_lrti_Age_1 + inc_count_lrti_Age_2to4
inc_count_fev  <- inc_count_fev_Age_0 + inc_count_fev_Age_1 + inc_count_fev_Age_2to4

# Bacterial-episode-weighted probability of antibiotic treatment
Ab_lrti_prop <- (
  AA_list[[1]] * inc_count_lrti_Age_0 +
    AA_list[[2]] * inc_count_lrti_Age_1 +
    AA_list[[3]] * inc_count_lrti_Age_2to4
) / inc_count_lrti

Ab_fev_prop <- (
  AA_list[[4]] * inc_count_fev_Age_0 +
    AA_list[[5]] * inc_count_fev_Age_1 +
    AA_list[[6]] * inc_count_fev_Age_2to4
) / inc_count_fev






## AA check
plot_year <- 2023

Ab_lrti_prop_summary <- t(apply(Ab_lrti_prop, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
colnames(Ab_lrti_prop_summary) <- c("median", "lower", "upper")
Ab_lrti_prop_summary <- cbind(UNI, Ab_lrti_prop_summary)
with(Ab_lrti_prop_summary %>% filter(Year == plot_year), plot(log10(GDP), median))
abline(lm(median ~ log10(GDP), data = Ab_lrti_prop_summary %>% filter(Year == plot_year)), lty = 2, lwd = 1.5, col = "blue")

Ab_fev_prop_summary <- t(apply(Ab_fev_prop, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
colnames(Ab_fev_prop_summary) <- c("median", "lower", "upper")
Ab_fev_prop_summary <- cbind(UNI, Ab_fev_prop_summary)
with(Ab_fev_prop_summary %>% filter(Year == plot_year), plot(log10(GDP), median))
abline(lm(median ~ log10(GDP), data = Ab_fev_prop_summary %>% filter(Year == plot_year & Endemic == 1)), lty = 2, lwd = 1.5, col = "blue")
abline(lm(median ~ log10(GDP), data = Ab_fev_prop_summary %>% filter(Year == plot_year & Endemic == 0)), lty = 2, lwd = 1.5, col = "blue")
##






## 1.1.1 plot: antibiotic access by Year (LRTI) ------------------------------

income_cols_fixed <- c(
  "L" = "#CC79A7",
  "LM" = "#009E73",
  "UM" = "#0072B2"
)

years_plot <- 2011:2023

xlim_all <- range(
  log10(Ab_lrti_prop_summary$GDP[
    is.finite(Ab_lrti_prop_summary$GDP) &
      Ab_lrti_prop_summary$GDP > 0
  ]),
  na.rm = TRUE
)

pdf("figures/supplementary/mortality_to_incidence/p_Ab_treatment_lrti.pdf", width = 6, height = 6)

par(
  mfrow = c(4, 4),
  mar = c(2.35, 2.35, 0.95, 0.30),
  mgp = c(1.35, 0.35, 0),
  tcl = -0.15
)

for (plot_i in seq_along(years_plot)) {
  
  year_i <- years_plot[plot_i]
  
  dat_i <- Ab_lrti_prop_summary %>%
    filter(Year == year_i, is.finite(GDP), GDP > 0, is.finite(median))
  
  point_cols_i <- income_cols_fixed[as.character(dat_i$Income)]
  point_cols_i[is.na(point_cols_i)] <- "grey50"
  
  ## These conditions control only the axis titles
  show_y_axis_title_i <- plot_i %in% c(1, 5, 9, 13)
  show_x_axis_title_i <- plot_i %in% c(10, 11, 12, 13)
  
  plot(
    log10(dat_i$GDP),
    dat_i$median,
    type = "n",
    xlim = c(2.8, 4.4),
    ylim = c(0.3, 0.8),
    axes = FALSE,
    ann = FALSE,
    frame.plot = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  ## Show the x-axis on every panel
  axis(side = 1, at = c(2.8, 3.2, 3.6, 4.0, 4.4), labels = format(c(2.8, 3.2, 3.6, 4.0, 4.4), nsmall = 1), cex.axis = 0.60, tck = -0.025)
  
  if (show_x_axis_title_i) {mtext("Log10 GDP per capita (PPP)", side = 1, line = 1.25, cex = 0.4)}
  
  axis(side = 2, at = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8), labels = format(c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8), nsmall = 1), cex.axis = 0.60, tck = -0.025)
  
  if (show_y_axis_title_i) {mtext("Proportion treated with antibiotics", side = 2, line = 1.35, cex = 0.4)}
  points(log10(dat_i$GDP), dat_i$median, pch = 16, cex = 0.7, col = point_cols_i)
  title(main = as.character(year_i), line = -0.5, cex.main = 0.6)}

## Plot 14: legend
plot.new()

legend(
  "center",
  legend = c("Low income", "Lower-middle income", "Upper-middle income"),
  col = income_cols_fixed[c("L", "LM", "UM")],
  pch = 16,
  pt.cex = 0.75,
  cex = 0.75,
  bty = "n",
  title = "Income group"
)

## Plots 15 and 16
plot.new(); plot.new()

dev.off()




## 1.1.2 plot: antibiotic access by Year (Fever) ------------------------------

pdf(
  "figures/supplementary/mortality_to_incidence/p_Ab_treatment_fev.pdf",
  width = 6,
  height = 6
)

par(
  mfrow = c(4, 4),
  mar = c(2.35, 2.35, 0.95, 0.30),
  mgp = c(1.35, 0.35, 0),
  tcl = -0.15
)

for (plot_i in seq_along(years_plot)) {
  
  year_i <- years_plot[plot_i]
  
  dat_i <- Ab_fev_prop_summary %>%
    filter(Year == year_i, is.finite(GDP), GDP > 0, is.finite(median))
  
  point_cols_i <- income_cols_fixed[as.character(dat_i$Income)]
  point_cols_i[is.na(point_cols_i)] <- "grey50"
  
  ## These conditions control only the axis titles
  show_y_axis_title_i <- plot_i %in% c(1, 5, 9, 13)
  show_x_axis_title_i <- plot_i %in% c(10, 11, 12, 13)
  
  plot(
    log10(dat_i$GDP),
    dat_i$median,
    type = "n",
    xlim = c(2.8, 4.4),
    ylim = c(0.2, 0.7),
    axes = FALSE,
    ann = FALSE,
    frame.plot = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  
  ## Show the x-axis on every panel
  axis(side = 1, at = c(2.8, 3.2, 3.6, 4.0, 4.4), labels = format(c(2.8, 3.2, 3.6, 4.0, 4.4), nsmall = 1), cex.axis = 0.60, tck = -0.025)
  
  if (show_x_axis_title_i) {mtext("Log10 GDP per capita (PPP)", side = 1, line = 1.25, cex = 0.4)}
  
  axis(side = 2, at = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7), labels = format(c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7), nsmall = 1), cex.axis = 0.60, tck = -0.025)
  
  if (show_y_axis_title_i) {mtext("Proportion treated with antibiotics", side = 2, line = 1.35, cex = 0.4)}
  points(log10(dat_i$GDP), dat_i$median, pch = 16, cex = 0.7, col = point_cols_i)
  
  title(main = as.character(year_i), line = -0.5, cex.main = 0.6)}

## Plot 14: legend
plot.new()

legend(
  "center",
  legend = c("Low income", "Lower-middle income", "Upper-middle income"),
  col = income_cols_fixed[c("L", "LM", "UM")],
  pch = 16,
  pt.cex = 0.75,
  cex = 0.75,
  bty = "n",
  title = "Income group"
)

## Plots 15 and 16
plot.new(); plot.new()

dev.off()






## data -----------------------------------------

load("outputs/pipelines/parsFevSx.Rdata")
load("outputs/pipelines/parsRespSx.Rdata" )

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

Ab_lrti_eff_matrix <- matrix(Ab_lrti_eff, nrow = nrow(inc_rate_lrti), ncol = ncol(inc_rate_lrti), byrow = TRUE)
Ab_fev_eff_matrix <- matrix(Ab_fev_eff, nrow = nrow(inc_rate_fev), ncol = ncol(inc_rate_fev), byrow = TRUE)

dim(death_rate_lrti)
dim(inc_rate_lrti)
dim(Ab_lrti_prop)
dim(Ab_lrti_eff_matrix)







## IHME AMR attributable death rate per 100,000 draw

AMR_attributable <- read.csv("data/comparison/IHME_2021_AMR_rate_per_100000.csv")























# (Main 1) # Antibiotic effectiveness against death -----------------------------------
## Summarize draws -------------------------------------------------------------

effect_dat <- data.frame(
  group = c("Antibiotic effectiveness against LRTI mortality",
            "Antibiotic effectiveness against fever mortality"),
  median = c(median(Ab_lrti_eff, na.rm = TRUE),
             median(Ab_fev_eff, na.rm = TRUE)),
  lower = c(quantile(Ab_lrti_eff, probs = 0.025, na.rm = TRUE, names = FALSE),
            quantile(Ab_fev_eff, probs = 0.025, na.rm = TRUE, names = FALSE)),
  upper = c(quantile(Ab_lrti_eff, probs = 0.975, na.rm = TRUE, names = FALSE),
            quantile(Ab_fev_eff, probs = 0.975, na.rm = TRUE, names = FALSE)))

effect_dat <- effect_dat %>%
  dplyr::mutate(
    col = c("#0072B2", "#CC79A7"),
    median_text = sprintf("%.3f", median),
    lower_text = sprintf("%.3f", lower),
    upper_text = sprintf("%.3f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

## Plot settings ---------------------------------------------------------------

mar_category <- c(2.1, 0.1, 0.1, 0.1)
mar_plot <- c(2.1, 0.1, 0.1, 0.1)
mar_number <- c(2.1, 0.1, 0.1, 0.1)

panel_mgp <- c(0.35, 0.35, 0)

cex.axis_value <- 0.70
cex.axis_title <- 0.55
cex.category <- 0.70
cex.number <- 0.70
cex.number_title <- 0.7
cex_point <- 0.70

lwd_interval <- 1.0
lwd_zero <- 0.8

alpha_interval <- 0.4
alpha_point <- 0.8
axis_tick_length <- -0.025

category_y <- c(1.8, 1.3, 0.8)
ylim_panel <- c(0.5, 2.65)

category_text_x <- 1.0
number_text_x <- 0.50
title_y <- 2.4


## Common x-axis ---------------------------------------------------------------



effect_x_values <- c(effect_dat$lower, effect_dat$upper, 0)
effect_x_values <- effect_x_values[is.finite(effect_x_values)]

effect_x_ticks <- seq(0,1, length.out = 5)
effect_xlim <- c(0,1)
effect_x_labels <- sprintf("%.2f", effect_x_ticks)

effect_dat_plot <- read.csv("outputs/pipelines/figure_1_data.csv",  fileEncoding = "macintosh",  stringsAsFactors = FALSE)
effect_dat_plot <- effect_dat_plot %>% dplyr::select(-X)
effect_AF_x_ticks <- pretty(c(0,0.5), n = 5)
effect_AF_xlim <- c(0,0.3)
effect_AF_x_labels <- sprintf("%.1f", effect_AF_x_ticks)

## Plot ------------------------------------------------------------------------

pdf(
  "figures/supplementary/mortality_to_incidence/(Main 1) Combined estimates.pdf", width = 6, height = 2.5)

layout(
  matrix(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0,
      1, 0, 2, 2, 3, 4, 4, 5, 5,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 
      6, 6, 6, 7, 8, 8, 9, 10, 10),
    nrow = 4, byrow = TRUE),
  widths = c(1.55, 0.1, 0.1, 0.75, 0.95, 0.1, 0.75, 0.95, 0.1),
  heights = c(0.10, 0.75, 0.10, 1.3))

par(oma = c(0, 0, 0, 0))

### (1) Category text -----------------------------------------------------------

par(mar = mar_category, mgp = panel_mgp, bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  category_text_x,
  title_y,
  labels = "Outcome",
  adj = c(1, 0.5),
  cex = cex.number_title,
  font = 2
)

text(
  rep(category_text_x, nrow(effect_dat_plot)),
  category_y,
  labels = effect_dat_plot$group,
  adj = c(1, 0.5),
  cex = cex.category
)

mtext(
  "a",
  side = 3,
  adj = 0.5,
  line = -0.2,
  cex = 0.75,
  font = 2
)

### (2) effectiveness plot ------------------------------------------------------

par(mar = mar_plot, mgp = panel_mgp, bty = "l")

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = effect_xlim,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = effect_x_ticks,
  labels = effect_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = effect_dat_plot$lower,
  y0 = category_y,
  x1 = effect_dat_plot$upper,
  y1 = category_y,
  col = adjustcolor(effect_dat_plot$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  effect_dat_plot$median,
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(effect_dat_plot$col, alpha.f = alpha_point)
)

mtext(
  "Antibiotic effectiveness\nagainst mortality",
  side = 1,
  line = 1.8,
  cex = cex.axis_title
)


### (3) effectiveness Numeric summaries -------------------------------------------------------

par(mar = mar_number, mgp = panel_mgp, bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  number_text_x,
  title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex.number_title,
  font = 2
)

text(
  rep(number_text_x, nrow(effect_dat_plot)),
  category_y,
  labels = effect_dat_plot$result_text,
  adj = c(0.5, 0.5),
  cex = cex.number
)






### (4) AF plot ------------------------------------------------------

par(mar = mar_plot, mgp = panel_mgp, bty = "l")

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = effect_AF_xlim,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = effect_AF_x_ticks,
  labels = effect_AF_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = effect_dat_plot$AF_lower,
  y0 = category_y,
  x1 = effect_dat_plot$AF_upper,
  y1 = category_y,
  col = adjustcolor(effect_dat_plot$AF_col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  effect_dat_plot$AF_median,
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(effect_dat_plot$AF_col, alpha.f = alpha_point)
)

mtext(
  "Attributable fraction\namong bacterial deaths",
  side = 1,
  line = 1.8,
  cex = cex.axis_title
)


### (5) AF Numeric summaries -------------------------------------------------------

par(mar = mar_number, mgp = panel_mgp, bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

text(
  number_text_x,
  title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex.number_title,
  font = 2
)

text(
  rep(number_text_x, nrow(effect_dat_plot)),
  category_y,
  labels = effect_dat_plot$AF_result_text,
  adj = c(0.5, 0.5),
  cex = cex.number
)








## Plot settings ----------------------------------------------------------------
AMR_result <- readRDS("outputs/pipelines/AMR_result.rds")
AA_result <- read.csv("outputs/pipelines/death_region_AA.csv") %>% dplyr::select(-c(X, gbd_region))
rownames(AA_result) <- c("Central Europe, Eastern Europe, and Central Asia", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa", "Overall")

pdf_width <- 8.6
pdf_height <- 4.2

mar_cat <- c(2.3, 0.2, 1.5, 0.1)
mar_plot <- c(2.3, 0.3, 1.5, 0.2)
mar_text <- c(2.3, 0.1, 1.5, 0.1)

panel_oma <- c(0, 0, 0, 0)
panel_mgp <- c(0.35, 0.35, 0)

cex.axis_value <- 0.70
cex.category <- 0.70
cex.axis_title <- 0.55
cex_title <- 0.60
cex_point <- 0.70
cex_text <- 0.70
cex_text_title <- 0.7

lwd_interval <- 0.8
lwd_divider <- 1.2

alpha_interval <- 0.40
alpha_point <- 0.80
axis_tick_length <- -0.025

plot_floor <- 1e-3

category_levels <- c(
  "Total",
  "Sub-Saharan Africa",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "North Africa and Middle East",
  "Latin America and Caribbean",
  "Central Europe, Eastern Europe, and Central Asia"
)

category_cols <- c(
  "Total" = "#0072B2",
  "Sub-Saharan Africa" = "#0072B2",
  "South Asia" = "#A7B97F",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "North Africa and Middle East" = "#009E73",
  "Latin America and Caribbean" = "#E69F00",
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7"
)

category_y <- rev(seq_along(category_levels)) * 0.95
divider_y <- category_y[1] - 0.5

ylim_panel <- c(0.5, length(category_levels) + 1.0)
text_title_y <- length(category_levels) + 0.6

number_title_x <- 0.5
number_text_x <- 0.5


## Prepare plotting data --------------------------------------------------------

dat_AMR <- as.data.frame(AMR_result) %>%
  tibble::rownames_to_column("group") %>%
  dplyr::transmute(
    group = dplyr::recode(group, "Overall" = "Total"),
    median = Median,
    lower = Lower,
    upper = Upper
  ) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

dat_AA <- as.data.frame(AA_result) %>%
  tibble::rownames_to_column("group") %>%
  dplyr::transmute(
    group = dplyr::recode(group, "Overall" = "Total"),
    median = median,
    lower = lower,
    upper = upper
  ) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )


## Common x-axis ----------------------------------------------------------------

x_values <- c(
  dat_AMR$lower, dat_AMR$upper,
  dat_AA$lower, dat_AA$upper
)

x_values <- x_values[is.finite(x_values) & x_values > 0]

x_exp <- seq(
  floor(log10(min(x_values))),
  ceiling(log10(max(x_values))),
  by = 1
)

x_ticks <- x_exp
x_labels <- parse(text = paste0("10^", x_exp))
xlim_log <- range(x_ticks)


### (1) Category text column ----------------------------------------------------

par(mar = mar_cat, mgp = panel_mgp, bty = "n")

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
  cex = cex.category,
  xpd = NA
)

text(
  category_text_x,
  text_title_y,
  labels = "Regions",
  adj = c(1.1, 0.5),
  cex = cex_text_title,
  font = 2
)

mtext(
  "b",
  side = 3,
  adj = 0,
  line = -0.2,
  cex = 0.75,
  font = 2
)


### (2) AMR plot column ---------------------------------------------------------

dat <- dat_AMR

par(mar = mar_plot, mgp = panel_mgp, bty = "l")

plot(
  NA_real_, NA_real_,
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
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext(
  "Number of deaths",
  side = 1,
  line = 1.1,
  cex = cex.axis_title
)


### (3) AMR number text column --------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")

plot.new()
plot.window(
  xlim = c(0, 1),
  ylim = ylim_panel,
  xaxs = "i",
  yaxs = "i"
)

text(
  number_title_x,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(number_text_x, nrow(dat_AMR)),
  category_y,
  labels = dat_AMR$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)


### (4) AA plot column ----------------------------------------------------------

dat <- dat_AA

par(mar = mar_plot, mgp = panel_mgp, bty = "l")

plot(
  NA_real_, NA_real_,
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
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext(
  "Number of deaths",
  side = 1,
  line = 1.1,
  cex = cex.axis_title
)


### (5) AA number text column ---------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")

plot.new()
plot.window(
  xlim = c(0, 1),
  ylim = ylim_panel,
  xaxs = "i",
  yaxs = "i"
)

text(
  number_title_x,
  text_title_y,
  labels = "Median (95% UI)",
  adj = c(0.5, 0.5),
  cex = cex_text_title,
  font = 2
)

text(
  rep(number_text_x, nrow(dat_AA)),
  category_y,
  labels = dat_AA$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)



dev.off()











# Estimated mortality-to-incidence ratio attributable to lack of effective treatment

CMR_fev <- CMR_lrti <- matrix(NA, nrow = 1343, ncol = 1000)

CMR_lrti    <- death_rate_lrti / ( (inc_rate_lrti * Ab_lrti_prop * (1-Ab_lrti_eff_matrix)) +  (inc_rate_lrti * (1-Ab_lrti_prop)) )
CMR_fev    <- death_rate_fev / ( (inc_rate_fev * Ab_fev_prop * (1-Ab_fev_eff_matrix)) +  (inc_rate_fev * (1-Ab_fev_prop)) )

lrti_Ab_prop    <- ((1 - Ab_lrti_prop) * Ab_lrti_eff_matrix) / (1 - Ab_lrti_prop * Ab_lrti_eff_matrix)
lrti_Ab_prop_summary <- t(apply(lrti_Ab_prop, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
colnames(lrti_Ab_prop_summary) <- c("median", "lower", "upper")
lrti_Ab_prop_summary <- cbind(UNI, lrti_Ab_prop_summary)

fev_Ab_prop    <- ((1 - Ab_fev_prop) * Ab_fev_eff_matrix) / (1 - Ab_fev_prop * Ab_fev_eff_matrix)
fev_Ab_prop_summary <- t(apply(fev_Ab_prop, 1, quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE))
colnames(fev_Ab_prop_summary) <- c("median", "lower", "upper")
fev_Ab_prop_summary <- cbind(UNI, fev_Ab_prop_summary)

library(dplyr)

## prop of averted by Ab: LRTI
lrti_Ab_prop_summary_2011 <- lrti_Ab_prop_summary %>% filter(Year == 2011) %>% dplyr::select(Country, GDP_2011 = GDP, median_2011 = median)
lrti_Ab_prop_summary_2023 <- lrti_Ab_prop_summary %>% filter(Year == 2023) %>% dplyr::select(Country, GDP_2023 = GDP, median_2023 = median)
lrti_Ab_prop_movement <- lrti_Ab_prop_summary_2011 %>% 
  inner_join(lrti_Ab_prop_summary_2023, by = "Country") %>% 
  filter(GDP_2011 > 0, GDP_2023 > 0, is.finite(median_2011), is.finite(median_2023)) %>%
  mutate(log_GDP_2011 = log10(GDP_2011), log_GDP_2023 = log10(GDP_2023))

x_lim <- range(c(lrti_Ab_prop_movement$log_GDP_2011,
                 lrti_Ab_prop_movement$log_GDP_2023), na.rm = TRUE)

y_lim <- range(c(lrti_Ab_prop_movement$median_2011,
                 lrti_Ab_prop_movement$median_2023), na.rm = TRUE)

par(mfrow = c(1, 1))
plot(lrti_Ab_prop_movement$log_GDP_2011, lrti_Ab_prop_movement$median_2011, pch = 1, xlim = x_lim, ylim = y_lim, xlab = "Log10 GDP per capita (PPP)", ylab = "Median Proportion treated with antibiotics")
arrows(x0 = lrti_Ab_prop_movement$log_GDP_2011, y0 = lrti_Ab_prop_movement$median_2011, x1 = lrti_Ab_prop_movement$log_GDP_2023, y1 = lrti_Ab_prop_movement$median_2023, length = 0.06, col = adjustcolor("darkblue", alpha.f = 0.1))
points(lrti_Ab_prop_movement$log_GDP_2011, lrti_Ab_prop_movement$median_2011, cex = 1.1, pch = 16, col = adjustcolor("darkblue", alpha.f = 0.25))
points(lrti_Ab_prop_movement$log_GDP_2023, lrti_Ab_prop_movement$median_2023, cex = 1.1, pch = 16, col = adjustcolor("darkblue", alpha.f = 0.90))
abline(lm(median_2011 ~ log_GDP_2011, data = lrti_Ab_prop_movement), lty = 2, lwd = 1.5, col = "blue")
abline(lm(median_2023 ~ log_GDP_2023, data = lrti_Ab_prop_movement), lty = 1, lwd = 1.5, col = "blue")

legend("bottomleft", legend = c("2011", "2023", "2011 regression", "2023 regression"),
       pch = c(1, 16, NA, NA),
       lty = c(NA, NA, 2, 1),
       lwd = c(NA, NA, 2, 2), bty = "n", cex = 0.85)



## prop of averted by Ab: fever

fev_Ab_prop_summary_2011 <- fev_Ab_prop_summary %>% filter(Year == 2011) %>% dplyr::select(Country, GDP_2011 = GDP, median_2011 = median)
fev_Ab_prop_summary_2023 <- fev_Ab_prop_summary %>% filter(Year == 2023) %>% dplyr::select(Country, GDP_2023 = GDP, median_2023 = median, Endemic)
fev_Ab_prop_movement <- fev_Ab_prop_summary_2011 %>% 
  inner_join(fev_Ab_prop_summary_2023, by = "Country") %>% 
  filter(GDP_2011 > 0, GDP_2023 > 0, is.finite(median_2011), is.finite(median_2023)) %>%
  mutate(log_GDP_2011 = log10(GDP_2011), log_GDP_2023 = log10(GDP_2023))

x_lim <- range(c(fev_Ab_prop_movement$log_GDP_2011,
                 fev_Ab_prop_movement$log_GDP_2023), na.rm = TRUE)

y_lim <- range(c(fev_Ab_prop_movement$median_2011,
                 fev_Ab_prop_movement$median_2023), na.rm = TRUE)

par(mfrow = c(1, 1))
plot(fev_Ab_prop_movement$log_GDP_2011, fev_Ab_prop_movement$median_2011, pch = 1, xlim = x_lim, ylim = y_lim, xlab = "Log10 GDP per capita (PPP)", ylab = "Median Proportion treated with antibiotics")
arrows(x0 = fev_Ab_prop_movement$log_GDP_2011, y0 = fev_Ab_prop_movement$median_2011, x1 = fev_Ab_prop_movement$log_GDP_2023, y1 = fev_Ab_prop_movement$median_2023, length = 0.06, col = adjustcolor("darkblue", alpha.f = 0.1))
points(fev_Ab_prop_movement$log_GDP_2011, fev_Ab_prop_movement$median_2011, cex = 1.1, pch = 16, col = adjustcolor("darkblue", alpha.f = 0.25))
points(fev_Ab_prop_movement$log_GDP_2023, fev_Ab_prop_movement$median_2023, cex = 1.1, pch = 16, col = adjustcolor("darkblue", alpha.f = 0.90))
abline(lm(median_2011 ~ log_GDP_2011, data = fev_Ab_prop_movement %>% filter(Endemic == 0)), lty = 2, lwd = 1.5, col = "blue")
abline(lm(median_2011 ~ log_GDP_2011, data = fev_Ab_prop_movement %>% filter(Endemic == 1)), lty = 2, lwd = 1.5, col = "blue")
abline(lm(median_2023 ~ log_GDP_2023, data = fev_Ab_prop_movement %>% filter(Endemic == 0)), lty = 1, lwd = 1.5, col = "blue")
abline(lm(median_2023 ~ log_GDP_2023, data = fev_Ab_prop_movement %>% filter(Endemic == 1)), lty = 1, lwd = 1.5, col = "blue")

legend("bottomleft", legend = c("2011", "2023", "2011 regression", "2023 regression"),
       pch = c(1, 16, NA, NA),
       lty = c(NA, NA, 2, 1),
       lwd = c(NA, NA, 2, 2), bty = "n", cex = 0.85)


UNI <- UNI %>% mutate(Population = UNI_pop$Age_all)



death_inc_ratio_lrti <- death_rate_lrti/inc_rate_lrti
death_inc_ratio_fev <- death_rate_fev/inc_rate_fev    

df.death_rate_lrti       <- cbind(UNI, as.data.frame(death_rate_lrti )) %>% filter(GDP < 20000)
df.death_rate_fev        <- cbind(UNI, as.data.frame(death_rate_fev  )) %>% filter(GDP < 20000)
df.inc_rate_lrti         <- cbind(UNI, as.data.frame(inc_rate_lrti   )) %>% filter(GDP < 20000)
df.inc_rate_fev          <- cbind(UNI, as.data.frame(inc_rate_fev    )) %>% filter(GDP < 20000)
df.death_inc_ratio_lrti  <- cbind(UNI, as.data.frame(death_inc_ratio_lrti )) %>% filter(GDP < 20000)
df.death_inc_ratio_fev   <- cbind(UNI, as.data.frame(death_inc_ratio_fev  )) %>% filter(GDP < 20000)
df.CMR_lrti              <- cbind(UNI, as.data.frame(CMR_lrti)) %>% filter(GDP < 20000)
df.CMR_fev               <- cbind(UNI, as.data.frame(CMR_fev )) %>% filter(GDP < 20000)
df.ab.lrti <- cbind(UNI, as.data.frame(Ab_lrti_prop)) %>% filter(GDP < 20000)
df.ab.fev <- cbind(UNI, as.data.frame(Ab_fev_prop)) %>% filter(GDP < 20000)

df.ab.lrti_L <- df.ab.lrti %>% filter(Income == "L") %>% filter(GDP < 20000)
df.ab.lrti_LM<-  df.ab.lrti %>% filter(Income == "LM") %>% filter(GDP < 20000)
df.ab.lrti_UM<-  df.ab.lrti %>% filter(Income == "UM") %>% filter(GDP < 20000)
df.ab.fev_L <- df.ab.fev %>% filter(Income == "L") %>% filter(GDP < 20000)
df.ab.fev_LM<-  df.ab.fev %>% filter(Income == "LM") %>% filter(GDP < 20000)
df.ab.fev_UM<-  df.ab.fev %>% filter(Income == "UM") %>% filter(GDP < 20000)

par(mfrow = c(3,1))
hist(rowMeans(df.ab.lrti_L[,8:1007]), xlim = c(0,1))
hist(rowMeans(df.ab.lrti_LM[,8:1007]), xlim = c(0,1))
hist(rowMeans(df.ab.lrti_UM[,8:1007]), xlim = c(0,1))

hist(rowMeans(df.ab.fev_L[,8:1007]), xlim = c(0,1))
hist(rowMeans(df.ab.fev_LM[,8:1007]), xlim = c(0,1))
hist(rowMeans(df.ab.fev_UM[,8:1007]), xlim = c(0,1))

par(mfrow = c(1,2))
plot(df.ab.lrti$GDP, rowMeans(df.ab.lrti[,8:1007]))
plot(df.ab.fev$GDP, rowMeans(df.ab.fev[,8:1007]))


df.death_rate_lrti      <- df.death_rate_lrti       %>% filter(Year == 2023)
df.death_rate_fev       <- df.death_rate_fev        %>% filter(Year == 2023)
df.inc_rate_lrti        <- df.inc_rate_lrti         %>% filter(Year == 2023)
df.inc_rate_fev         <- df.inc_rate_fev          %>% filter(Year == 2023)
df.death_inc_ratio_lrti <- df.death_inc_ratio_lrti  %>% filter(Year == 2023)
df.death_inc_ratio_fev  <- df.death_inc_ratio_fev   %>% filter(Year == 2023)
df.CMR_lrti             <- df.CMR_lrti              %>% filter(Year == 2023)
df.CMR_fev              <- df.CMR_fev               %>% filter(Year == 2023)
df.ab.lrti              <- df.ab.lrti               %>% filter(Year == 2023)
df.ab.fev               <- df.ab.fev                %>% filter(Year == 2023)



summarize_CMR_draws <- function(data) {
  # Metadata columns
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  # 1. Country-year summary
  draw_matrix <- as.matrix(data[, draw_cols])
  
  summary_country_year <- data %>%
    dplyr::select(Country, Year, Income, gbd_region, GDP) %>%
    mutate(
      median = apply(draw_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 2. Overall summary
  overall_draws <- data %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    group = "Overall",
    median = quantile(overall_draws, probs = 0.500, na.rm = TRUE),
    lower = quantile(overall_draws, probs = 0.025, na.rm = TRUE),
    upper = quantile(overall_draws, probs = 0.975, na.rm = TRUE))
  
  
  
  # 3. Income-level summary
  income_draws <- data %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE)),
              .groups = "drop")
  
  income_matrix <- as.matrix(income_draws[, draw_cols])
  
  summary_income <- income_draws %>%
    dplyr::select(Income) %>%
    mutate(
      median = apply(income_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(income_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(income_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 4. GBD-region summary
  
  region_draws <- data %>%
    filter(
      !is.na(gbd_region)
    ) %>%
    group_by(gbd_region) %>%
    summarise(
      across(
        all_of(draw_cols),
        ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE)),
      .groups = "drop")
  
  region_matrix <- as.matrix(region_draws[, draw_cols])
  
  summary_gbd_region <- region_draws %>%
    dplyr::select(gbd_region) %>%
    mutate(
      median = apply(region_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(region_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(region_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  # Return all four summaries
  list(
    overall = summary_overall,
    country_year = summary_country_year,
    income = summary_income,
    gbd_region = summary_gbd_region
  )
}

# Income level matarix generating function
generate_income_draw_matrices <- function(data) {
  
  # Metadata columns
  meta_cols <- c(
    "Country", "Year", "GDP", "Income", "gbd_region",
    "Endemic", "Population"
  )
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  # Population-weighted draw estimates by income level
  income_draws <- data %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(
      across(
        all_of(draw_cols),
        ~ sum(.x * Population, na.rm = TRUE) /
          sum(Population[!is.na(.x)], na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  # Separate into three matrices
  L_draws <- income_draws %>%
    filter(Income == "L") %>%
    dplyr::select(all_of(draw_cols)) %>%
    as.matrix()
  
  LM_draws <- income_draws %>%
    filter(Income == "LM") %>%
    dplyr::select(all_of(draw_cols)) %>%
    as.matrix()
  
  UM_draws <- income_draws %>%
    filter(Income == "UM") %>%
    dplyr::select(all_of(draw_cols)) %>%
    as.matrix()
  
  list(
    L = L_draws,
    LM = LM_draws,
    UM = UM_draws
  )
}

income_inc_lrti <- generate_income_draw_matrices(df.inc_rate_lrti)
income_inc_fev <- generate_income_draw_matrices(df.inc_rate_fev)
income_mort_lrti <- generate_income_draw_matrices(df.death_rate_lrti)
income_mort_fev <- generate_income_draw_matrices(df.death_rate_fev)
income_ab_lrti <- generate_income_draw_matrices(df.ab.lrti)
income_ab_fev  <- generate_income_draw_matrices(df.ab.fev )

### main manuscript inc lower by income ###
round((quantile(income_inc_lrti$LM/income_inc_lrti$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_inc_lrti$L/income_inc_lrti$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_inc_fev$LM/income_inc_fev$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_inc_fev$L/income_inc_fev$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)

###
round((quantile(income_mort_lrti$LM/income_mort_lrti$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_mort_lrti$L/income_mort_lrti$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_mort_fev$LM/income_mort_fev$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)
round((quantile(income_mort_fev$L/income_mort_fev$UM, c(0.5,0.025,0.975))-1)*100, digits = 1)

###
round((1-quantile(income_ab_lrti$LM/income_ab_lrti$UM, c(0.5,0.975,0.025)))*100, digits = 1)
round((1-quantile(income_ab_lrti$L /income_ab_lrti$UM, c(0.5,0.975,0.025)))*100, digits = 1)
round((1-quantile(income_ab_fev $LM/income_ab_fev $UM, c(0.5,0.975,0.025)))*100, digits = 1)
round((1-quantile(income_ab_fev $L /income_ab_fev $UM, c(0.5,0.975,0.025)))*100, digits = 1)


summary.CMR_lrti               <- summarize_CMR_draws(df.CMR_lrti)$country_year %>% filter(GDP < 20000)
summary.CMR_fev                <- summarize_CMR_draws(df.CMR_fev)$country_year %>% filter(GDP < 20000)
summary.death_rate_lrti        <- summarize_CMR_draws(df.death_rate_lrti )$country_year %>% filter(GDP < 20000)
summary.death_rate_fev         <- summarize_CMR_draws(df.death_rate_fev  )$country_year %>% filter(GDP < 20000)
summary.inc_rate_lrti          <- summarize_CMR_draws(df.inc_rate_lrti   )$country_year %>% filter(GDP < 20000)
summary.inc_rate_fev           <- summarize_CMR_draws(df.inc_rate_fev    )$country_year %>% filter(GDP < 20000)
summary.death_inc_ratio_lrti   <- summarize_CMR_draws(df.death_inc_ratio_lrti )$country_year %>% filter(GDP < 20000)
summary.death_inc_ratio_fev    <- summarize_CMR_draws(df.death_inc_ratio_fev  )$country_year %>% filter(GDP < 20000)

summary.death_ab_lrti   <- summarize_CMR_draws(df.ab.lrti )$country_year %>% filter(GDP < 20000)
summary.death_ab_fev    <- summarize_CMR_draws(df.ab.fev  )$country_year %>% filter(GDP < 20000)



CMR_summary_LRTI_inc   <- summarize_CMR_draws(df.inc_rate_lrti)
CMR_summary_fever_inc  <- summarize_CMR_draws(df.inc_rate_fev)
CMR_summary_LRTI_mort  <- summarize_CMR_draws(df.death_rate_lrti)
CMR_summary_fever_mort <- summarize_CMR_draws(df.death_rate_fev)
CMR_summary_LRTI_ratio  <- summarize_CMR_draws(df.death_inc_ratio_lrti)
CMR_summary_fever_ratio <- summarize_CMR_draws(df.death_inc_ratio_fev)
CMR_summary_LRTI_ab  <- summarize_CMR_draws(df.ab.lrti )
CMR_summary_fever_ab <- summarize_CMR_draws(df.ab.fev  )

CMR_overall_LRTI_inc    <- CMR_summary_LRTI_inc$overall    %>% tibble::as_tibble() %>% mutate(class = "LRTI_incidence_rate")          %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_LRTI_inc     <- CMR_summary_LRTI_inc$income     %>% mutate(class = "LRTI_incidence_rate")                                  %>% dplyr::transmute(class, income = Income, median, lower, upper) 
CMR_overall_LRTI_mort   <- CMR_summary_LRTI_mort$overall   %>% tibble::as_tibble() %>% mutate(class = "LRTI_mortality_rate")          %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_LRTI_mort    <- CMR_summary_LRTI_mort$income    %>% mutate(class = "LRTI_mortality_rate")                                  %>% dplyr::transmute(class, income = Income, median, lower, upper) 
CMR_overall_LRTI_ratio  <- CMR_summary_LRTI_ratio$overall  %>% tibble::as_tibble() %>% mutate(class = "LRTI_mortality_to_incidence")  %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_LRTI_ratio   <- CMR_summary_LRTI_ratio$income   %>% mutate(class = "LRTI_mortality_to_incidence")                          %>% dplyr::transmute(class, income = Income, median, lower, upper) 
CMR_overall_fever_inc   <- CMR_summary_fever_inc$overall   %>% tibble::as_tibble() %>% mutate(class = "fever_incidence_rate")         %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_fever_inc    <- CMR_summary_fever_inc$income    %>% mutate(class = "fever_incidence_rate")                                 %>% dplyr::transmute(class, income = Income, median, lower, upper) 
CMR_overall_fever_mort  <- CMR_summary_fever_mort$overall  %>% tibble::as_tibble() %>% mutate(class = "fever_mortality_rate")         %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_fever_mort   <- CMR_summary_fever_mort$income   %>% mutate(class = "fever_mortality_rate")                                 %>% dplyr::transmute(class, income = Income, median, lower, upper) 
CMR_overall_fever_ratio <- CMR_summary_fever_ratio$overall %>% tibble::as_tibble() %>% mutate(class = "fever_mortality_to_incidence") %>% dplyr::transmute(class, income = group, median, lower, upper) 
CMR_income_fever_ratio  <- CMR_summary_fever_ratio$income  %>% mutate(class = "fever_mortality_to_incidence")                         %>% dplyr::transmute(class, income = Income, median, lower, upper) 

write.csv(CMR_summary_LRTI_inc$country_year  , "outputs/pipelines/suppl_table/bact_LRTI_incidence_rate.csv")
write.csv(CMR_summary_fever_inc$country_year , "outputs/pipelines/suppl_table/bact_AFI_incidence_rate.csv")
write.csv(CMR_summary_LRTI_mort$country_year , "outputs/pipelines/suppl_table/bact_LRTI_mortality_rate.csv")
write.csv(CMR_summary_fever_mort$country_year, "outputs/pipelines/suppl_table/bact_AFI_mortality_rate.csv")
write.csv(CMR_summary_LRTI_ab  $country_year , "outputs/pipelines/suppl_table/bact_LRTI_ab_treatment.csv")
write.csv(CMR_summary_fever_ab $country_year, "outputs/pipelines/suppl_table/bact_AFI_ab_treatment.csv")
write.csv(CMR_summary_LRTI_ratio  $country_year , "outputs/pipelines/suppl_table/bact_LRTI_CMR.csv")
write.csv(CMR_summary_fever_ratio $country_year, "outputs/pipelines/suppl_table/bact_AFI_CMR.csv")


CMR_combined <- dplyr::bind_rows(
  CMR_overall_LRTI_inc   ,
  CMR_income_LRTI_inc    ,
  CMR_overall_fever_inc  ,
  CMR_income_fever_inc   ,
  CMR_overall_LRTI_mort  ,
  CMR_income_LRTI_mort   ,
  CMR_overall_LRTI_ratio ,
  CMR_income_LRTI_ratio  ,
  CMR_overall_fever_mort ,
  CMR_income_fever_mort  ,
  CMR_overall_fever_ratio,
  CMR_income_fever_ratio )

CMR_combined[,3:5] <- CMR_combined[,3:5]*1000

CMR_combined[1:8,3:5] <- round(CMR_combined[1:8,3:5], digits = 1)
CMR_combined[9:24,3:5] <- round(CMR_combined[9:24,3:5], digits = 3)


print(CMR_combined, n = Inf)











 
# (Main 2) Plot: incidence, mortality, and mortality-to-incidence ratio -----------
pdf("figures/supplementary/mortality_to_incidence/(Main 2) mortality_to_incidence ~ GDP (color by income)_2023_layout_modified.pdf", width = 6, height = 3.5)

layout(matrix(c(0, 0, 0, 0 , 0, 
                1, 2, 3, 0 , 4, 
                1, 2, 3, 9 , 4, 
                1, 2, 3, 10, 4, 
                1, 2, 3, 11, 4, 
                1, 2, 3, 0 , 4, 
                0, 0, 0, 0 , 0, 
                5, 6, 7, 0 , 8,
                5, 6, 7, 12, 8, 
                5, 6, 7, 13, 8, 
                5, 6, 7, 14, 8,
                5, 6, 7, 0 , 8), 
              nrow = 12, byrow = TRUE), widths = c(1, 1, 1, 1.2, 1), heights = c(0.25, 0.15, 1, 1, 1, 0.15, 0.25, 0.15, 1, 1, 1, 0.15))


par(oma = c(0, 0, 0, 0))
par(mar = c(2.2, 2.7, 1, 1.3))
par(mgp = c(0.35, 0.35, 0))
par(bty = "l")


income_cols_fixed <- c(
  "L" = "#CC79A7",
  "LM" = "#009E73",
  "UM" = "#0072B2"
)

income_labels <- c(
  "L" = "L",
  "LM" = "LM",
  "UM" = "UM"
)

## GDP axis: log10 positions with labels on the original scale
xmin <- 100
xmax <- 20000

x_tick_values <- c(100, 1000, 10000)
x_minor_tick_values <- c(1:9*100, 2:9*1000, 20000)
x_ticks <- log10(x_tick_values)
x_minor_ticks <- log10(x_minor_tick_values)
x_labels <- parse(text = paste0("10^", x_ticks))
xlim_log <- log10(c(xmin, xmax))

cex.axis_value <- 0.7
cex.axis_title <- 0.55
cex_title <- 0.55
cex_legend <- 0.70
cex_legend_title <- 0.70 
cex_point <- 0.60
multiple <- 1000


LRTI_inc_y_exp <- -2:3
LRTI_inc_y_ticks <- LRTI_inc_y_exp
LRTI_inc_y_labels <- parse(text = paste0("10^", LRTI_inc_y_exp))
LRTI_inc_ylim <- range(LRTI_inc_y_ticks)
LRTI_inc_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_inc_y_exp):max(LRTI_inc_y_exp)-1), \(x) x * (2:9))))

LRTI_mort_y_exp <- -2:3
LRTI_mort_y_ticks <- LRTI_mort_y_exp
LRTI_mort_y_labels <- parse(text = paste0("10^", LRTI_mort_y_exp))
LRTI_mort_ylim <- range(LRTI_mort_y_ticks)
LRTI_mort_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_mort_y_exp):max(LRTI_mort_y_exp)-1), \(x) x * (2:9))))

LRTI_ratio_y_exp <- -1:3
LRTI_ratio_y_ticks <- LRTI_ratio_y_exp
LRTI_ratio_y_labels <- parse(text = paste0("10^", LRTI_ratio_y_exp))
LRTI_ratio_ylim <- range(LRTI_ratio_y_ticks)
LRTI_ratio_y_minor_ticks <- log10(unlist(lapply(10^(min(LRTI_ratio_y_exp):max(LRTI_ratio_y_exp)-1), \(x) x * (2:9))))

fev_inc_y_exp <- -2:3
fev_inc_y_ticks <- fev_inc_y_exp
fev_inc_y_labels <- parse(text = paste0("10^", fev_inc_y_exp))
fev_inc_ylim <- range(fev_inc_y_ticks)
fev_inc_y_minor_ticks <- log10(unlist(lapply(10^(min(fev_inc_y_exp):max(fev_inc_y_exp)-1), \(x) x * (2:9))))

fev_mort_y_exp <- -2:3
fev_mort_y_ticks <- fev_mort_y_exp
fev_mort_y_labels <- parse(text = paste0("10^", fev_mort_y_exp))
fev_mort_ylim <- range(fev_mort_y_ticks)
fev_mort_y_minor_ticks <- log10(unlist(lapply(10^(min(fev_mort_y_exp):max(fev_mort_y_exp)-1), \(x) x * (2:9))))

fev_ratio_y_exp <- -1:3
fev_ratio_y_ticks <- fev_ratio_y_exp
fev_ratio_y_labels <- parse(text = paste0("10^", fev_ratio_y_exp))
fev_ratio_ylim <- range(fev_ratio_y_ticks)
fev_ratio_y_minor_ticks <- log10(unlist(lapply(10^(min(fev_ratio_y_exp):max(fev_ratio_y_exp)-1), \(x) x * (2:9))))


### (1) LRTI: incidence -------------------------------------------------------------

dat <- summary.inc_rate_lrti %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

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
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Incidence rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("a", side = 3, adj = -0.58, line = 1.0, cex = 0.75, font = 2)



### (2) LRTI: mortality -------------------------------------------------------------

dat <- summary.death_rate_lrti %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

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
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Mortality rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("c", side = 3, adj = -0.59, line = 1.0, cex = 0.75, font = 2)



### (3) LRTI: mortality-to-incidence ratio -----------------------------------------

dat <- summary.death_inc_ratio_lrti %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = LRTI_ratio_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = LRTI_ratio_y_ticks, labels = LRTI_ratio_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = LRTI_ratio_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Case-fatality risk per 1000 cases", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("e", side = 3, adj = -0.59, line = 1.0, cex = 0.75, font = 2)





### (4) LRTI: antibiotic access -----------------------------------------



dat <- CMR_summary_LRTI_ab$country_year %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*100

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

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

segments(
  x0 = log10(dat$GDP),
  y0 = dat$lower,
  x1 = log10(dat$GDP),
  y1 = dat$upper,
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  dat$median,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Proportion treated (%)", side = 2, line = 1.8, cex = cex.axis_title)
mtext("g", side = 3, adj = -0.60, line = 1.0, cex = 0.75, font = 2)





### (5) Fever: incidence -------------------------------------------------------------

dat <- summary.inc_rate_fev %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fev_inc_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fev_inc_y_ticks, labels = fev_inc_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fev_inc_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Incidence rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("b", side = 3, adj = -0.59, line = 1.0, cex = 0.75, font = 2)





### (6) Fever: mortality -------------------------------------------------------------

dat <- summary.death_rate_fev %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fev_mort_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fev_mort_y_ticks, labels = fev_mort_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fev_mort_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Mortality rate per 1000 child-years", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("d", side = 3, adj = -0.59, line = 1.0, cex = 0.75, font = 2)



### (7) Fever: mortality-to-incidence ratio -----------------------------------------

dat <- summary.death_inc_ratio_fev %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*multiple

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = fev_ratio_ylim,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = fev_ratio_y_ticks, labels = fev_ratio_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = fev_ratio_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper),
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  log10(dat$median),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Case-fatality risk per 1000 cases", side = 2, line = 1.8, adj = 0.68, cex = cex.axis_title)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("f", side = 3, adj = -0.55, line = 1.0, cex = 0.75, font = 2)





### (8) Fever: Antibiotic access -----------------------------------------

dat <- CMR_summary_fever_ab $country_year %>%
  filter(
    is.finite(GDP),
    GDP >= xmin,
    GDP <= xmax,
    is.finite(median),
    is.finite(lower),
    is.finite(upper),
    median > 0,
    lower > 0,
    upper > 0
  )

dat[6:8] <- dat[6:8]*100

region_col <- income_cols_fixed[as.character(dat$Income)]
region_col[is.na(region_col)] <- "grey50"

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

segments(
  x0 = log10(dat$GDP),
  y0 = dat$lower,
  x1 = log10(dat$GDP),
  y1 = dat$upper,
  col = adjustcolor(region_col, alpha.f = 0.40)
)

points(
  log10(dat$GDP),
  dat$median,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(region_col, alpha.f = 0.80)
)

mtext("Proportion treated (%)", side = 2, line = 1.8, cex = cex.axis_title)
mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("h", side = 3, adj = -0.60, line = 1.0, cex = 0.75, font = 2)






### Individual text legends for all eight plots --------------------------------------

income_display <- c("Overall" = "Overall", income_labels)
legend_point_cols <- c("Overall" = NA, income_cols_fixed)

legend_main_title_x <- 0.7
legend_title_x <- 0
legend_title_y <- 0.74
legend_point_x <- 0.035
legend_income_x <- 0.07
legend_result_x <- 0.60
legend_y_top <- 0.54
legend_y_bottom <- 0.04
legend_overall_title_y <- 0.97


### (9) LRTI: incidence legend -----------------------------------------------------

legend_dat_LRTI_incidence <- CMR_combined %>%
  dplyr::filter(class == "LRTI_incidence_rate") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.1f", median),
    lower_text = sprintf("%.1f", lower),
    upper_text = sprintf("%.1f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_LRTI_incidence <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_LRTI_incidence))

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y, labels = "[Incidence rate per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_LRTI_incidence)),
  legend_y_LRTI_incidence,
  pch = ifelse(as.character(legend_dat_LRTI_incidence$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_LRTI_incidence$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_LRTI_incidence)),
  legend_y_LRTI_incidence,
  labels = legend_dat_LRTI_incidence$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_LRTI_incidence)),
  legend_y_LRTI_incidence,
  labels = legend_dat_LRTI_incidence$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)

text(
  legend_main_title_x,
  legend_overall_title_y,
  labels = "Bacterial LRTI: Median (95% UI)",
  adj = c(0.75, 0.5),
  cex = cex_legend_title + 0.05,
  font = 2
)

### (10) LRTI: mortality legend -----------------------------------------------------

legend_dat_LRTI_mortality <- CMR_combined %>%
  dplyr::filter(class == "LRTI_mortality_rate") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.3f", median),
    lower_text = sprintf("%.3f", lower),
    upper_text = sprintf("%.3f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_LRTI_mortality <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_LRTI_mortality))+0.06

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y+0.06, labels = "[Mortality rate per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_LRTI_mortality)),
  legend_y_LRTI_mortality,
  pch = ifelse(as.character(legend_dat_LRTI_mortality$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_LRTI_mortality$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_LRTI_mortality)),
  legend_y_LRTI_mortality,
  labels = legend_dat_LRTI_mortality$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_LRTI_mortality)),
  legend_y_LRTI_mortality,
  labels = legend_dat_LRTI_mortality$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)


### (11) LRTI: mortality-to-incidence legend ----------------------------------------

legend_dat_LRTI_ratio <- CMR_combined %>%
  dplyr::filter(class == "LRTI_mortality_to_incidence") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.2f", median),
    lower_text = sprintf("%.2f", lower),
    upper_text = sprintf("%.2f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_LRTI_ratio <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_LRTI_ratio))+0.12

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y+0.12, labels = "[Case-fatality risk per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_LRTI_ratio)),
  legend_y_LRTI_ratio,
  pch = ifelse(as.character(legend_dat_LRTI_ratio$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_LRTI_ratio$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_LRTI_ratio)),
  legend_y_LRTI_ratio,
  labels = legend_dat_LRTI_ratio$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_LRTI_ratio)),
  legend_y_LRTI_ratio,
  labels = legend_dat_LRTI_ratio$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)




### (12) Fever: incidence legend ---------------------------------------------------

legend_dat_fever_incidence <- CMR_combined %>%
  dplyr::filter(class == "fever_incidence_rate") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.1f", median),
    lower_text = sprintf("%.1f", lower),
    upper_text = sprintf("%.1f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_fever_incidence <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_fever_incidence))

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y, labels = "[Incidence rate per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_fever_incidence)),
  legend_y_fever_incidence,
  pch = ifelse(as.character(legend_dat_fever_incidence$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_fever_incidence$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_fever_incidence)),
  legend_y_fever_incidence,
  labels = legend_dat_fever_incidence$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_fever_incidence)),
  legend_y_fever_incidence,
  labels = legend_dat_fever_incidence$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)

text(
  legend_main_title_x-0.025,
  legend_overall_title_y,
  labels = "Bacterial AFI: Median (95% UI)",
  adj = c(0.75, 0.5),
  cex = cex_legend_title + 0.05,
  font = 2
)



### (13) Fever: mortality legend ---------------------------------------------------

legend_dat_fever_mortality <- CMR_combined %>%
  dplyr::filter(class == "fever_mortality_rate") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.3f", median),
    lower_text = sprintf("%.3f", lower),
    upper_text = sprintf("%.3f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_fever_mortality <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_fever_mortality))+0.06

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y+0.06, labels = "[Mortality rate per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_fever_mortality)),
  legend_y_fever_mortality,
  pch = ifelse(as.character(legend_dat_fever_mortality$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_fever_mortality$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_fever_mortality)),
  legend_y_fever_mortality,
  labels = legend_dat_fever_mortality$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_fever_mortality)),
  legend_y_fever_mortality,
  labels = legend_dat_fever_mortality$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)


### (14) Fever: mortality-to-incidence legend --------------------------------------

legend_dat_fever_ratio <- CMR_combined %>%
  dplyr::filter(class == "fever_mortality_to_incidence") %>%
  dplyr::mutate(income = factor(income, levels = c("Overall", "L", "LM", "UM"))) %>%
  dplyr::arrange(income) %>%
  dplyr::mutate(
    income_name = unname(income_display[as.character(income)]),
    income_text = paste0(income_name, ":"),
    median_text = sprintf("%.2f", median),
    lower_text = sprintf("%.2f", lower),
    upper_text = sprintf("%.2f", upper),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

legend_y_fever_ratio <- seq(legend_y_top, legend_y_bottom, length.out = nrow(legend_dat_fever_ratio))+0.12

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(legend_title_x, legend_title_y+0.12, labels = "[Case-fatality risk per 1000]", adj = c(0, 0.5), cex = cex_legend_title, font = 2)

points(
  rep(legend_point_x, nrow(legend_dat_fever_ratio)),
  legend_y_fever_ratio,
  pch = ifelse(as.character(legend_dat_fever_ratio$income) == "Overall", NA, 16),
  col = legend_point_cols[as.character(legend_dat_fever_ratio$income)],
  cex = cex_point
)

text(
  rep(legend_income_x, nrow(legend_dat_fever_ratio)),
  legend_y_fever_ratio,
  labels = legend_dat_fever_ratio$income_text,
  adj = c(0, 0.5),
  cex = cex_legend
)

text(
  rep(legend_result_x, nrow(legend_dat_fever_ratio)),
  legend_y_fever_ratio,
  labels = legend_dat_fever_ratio$result_text,
  adj = c(0.5, 0.5),
  cex = cex_legend
)


dev.off()



# _____________________________________________ ----------
# 2. Additional preventable mortality rate ----------------------------------------------------
# Lowest country-level CMR among the 78 countries in 2023, separately for each draw
row_2023 <- which(UNI$Year == 2023)

CMR_lrti_2023 <- CMR_lrti[row_2023, , drop = FALSE]
CMR_fev_2023  <- CMR_fev[row_2023, , drop = FALSE]

CMR_lrti_min <- apply(CMR_lrti_2023, 2, min, na.rm = TRUE)
CMR_fev_min  <- apply(CMR_fev_2023, 2, min, na.rm = TRUE)

CMR_lrti_min[!is.finite(CMR_lrti_min)] <- NA_real_
CMR_fev_min[!is.finite(CMR_fev_min)] <- NA_real_


CMR_meta <- UNI %>% filter(Year == 2023) %>% dplyr::select(gbd_region, Country)

CMR_meta$lrti_median <- apply(CMR_lrti_2023, 1, quantile, probs = 0.50, na.rm = TRUE)
CMR_meta$lrti_lower <- apply(CMR_lrti_2023, 1, quantile, probs = 0.025, na.rm = TRUE)
CMR_meta$lrti_upper <- apply(CMR_lrti_2023, 1, quantile, probs = 0.975, na.rm = TRUE)
CMR_meta$fever_median <- apply(CMR_fev_2023, 1, quantile, probs = 0.50, na.rm = TRUE)
CMR_meta$fever_lower <- apply(CMR_fev_2023, 1, quantile, probs = 0.025, na.rm = TRUE)
CMR_meta$fever_upper <- apply(CMR_fev_2023, 1, quantile, probs = 0.975, na.rm = TRUE)

write.csv(CMR_meta, "outputs/pipelines/suppl_table/CMR_ineffective_treatment.csv")

# Repeat the 2023 draw-specific minimum across rows for the existing downstream calculations
CMR_lrti_min_matrix <- matrix(
  CMR_lrti_min,
  nrow = nrow(CMR_lrti),
  ncol = ncol(CMR_lrti),
  byrow = TRUE
)

CMR_fev_min_matrix <- matrix(
  CMR_fev_min,
  nrow = nrow(CMR_fev),
  ncol = ncol(CMR_fev),
  byrow = TRUE
)


mortality_lrti_Ab_preventable <- inc_rate_lrti * (1 - Ab_lrti_prop) * Ab_lrti_eff_matrix * CMR_lrti
mortality_fev_Ab_preventable  <- inc_rate_fev * (1 - Ab_fev_prop) * Ab_fev_eff_matrix * CMR_fev

mortality_lrti_other_preventable <- (
  (inc_rate_lrti * Ab_lrti_prop * (1 - Ab_lrti_eff_matrix)) +
    (inc_rate_lrti * (1 - Ab_lrti_prop))
) * (CMR_lrti - CMR_lrti_min_matrix)

mortality_fev_other_preventable <- (
  (inc_rate_fev * Ab_fev_prop * (1 - Ab_fev_eff_matrix)) +
    (inc_rate_fev * (1 - Ab_fev_prop))
) * (CMR_fev - CMR_fev_min_matrix)

mortality_lrti_both_preventable <- (
  (
    (inc_rate_lrti * Ab_lrti_prop * (1 - Ab_lrti_eff_matrix)) +
      (inc_rate_lrti * (1 - Ab_lrti_prop))
  ) * CMR_lrti
) - (
  inc_rate_lrti *
    (1 - Ab_lrti_eff_matrix) *
    CMR_lrti_min_matrix
)

mortality_fev_both_preventable <- (
  (
    (inc_rate_fev * Ab_fev_prop * (1 - Ab_fev_eff_matrix)) +
      (inc_rate_fev * (1 - Ab_fev_prop))
  ) * CMR_fev
) - (
  inc_rate_fev *
    (1 - Ab_fev_eff_matrix) *
    CMR_fev_min_matrix
)

proportion_lrti_Ab_preventable <- mortality_lrti_Ab_preventable / mortality_lrti_both_preventable
proportion_fev_Ab_preventable  <- mortality_fev_Ab_preventable / mortality_fev_both_preventable

df.mortality_lrti_Ab_preventable       <- cbind(UNI, as.data.frame(mortality_lrti_Ab_preventable    ))
df.mortality_fev_Ab_preventable        <- cbind(UNI, as.data.frame(mortality_fev_Ab_preventable     ))
df.mortality_lrti_other_preventable    <- cbind(UNI, as.data.frame(mortality_lrti_other_preventable ))
df.mortality_fev_other_preventable     <- cbind(UNI, as.data.frame(mortality_fev_other_preventable  ))
df.mortality_lrti_both_preventable     <- cbind(UNI, as.data.frame(mortality_lrti_both_preventable  ))
df.mortality_fev_both_preventable      <- cbind(UNI, as.data.frame(mortality_fev_both_preventable   ))
df.proportion_lrti_Ab_preventable     <- cbind(UNI, as.data.frame(proportion_lrti_Ab_preventable    ))
df.proportion_fev_Ab_preventable      <- cbind(UNI, as.data.frame(proportion_fev_Ab_preventable     ))
df.death_rate_lrti_2023 <- cbind(UNI, as.data.frame(death_rate_lrti)) %>% filter(Year == 2023)
df.death_rate_fev_2023 <- cbind(UNI, as.data.frame(death_rate_fev)) %>% filter(Year == 2023)
UNI_2023 <- UNI %>% filter(Year == 2023)

df.mortality_lrti_Ab_preventable    <- df.mortality_lrti_Ab_preventable    %>% filter(Year == 2023)
df.mortality_fev_Ab_preventable     <- df.mortality_fev_Ab_preventable     %>% filter(Year == 2023)
df.mortality_lrti_other_preventable <- df.mortality_lrti_other_preventable %>% filter(Year == 2023)
df.mortality_fev_other_preventable  <- df.mortality_fev_other_preventable  %>% filter(Year == 2023)
df.mortality_lrti_both_preventable  <- df.mortality_lrti_both_preventable  %>% filter(Year == 2023)
df.mortality_fev_both_preventable   <- df.mortality_fev_both_preventable   %>% filter(Year == 2023)
df.proportion_lrti_Ab_preventable   <- df.proportion_lrti_Ab_preventable   %>% filter(Year == 2023)
df.proportion_fev_Ab_preventable    <- df.proportion_fev_Ab_preventable    %>% filter(Year == 2023)

df.proportion_lrti_Ab_preventable_all_mortality <- cbind(UNI_2023 ,df.mortality_lrti_Ab_preventable[,8:1007]/df.death_rate_lrti_2023[,8:1007])
df.proportion_fev_Ab_preventable_all_mortality <- cbind(UNI_2023 ,df.mortality_fev_Ab_preventable[,8:1007]/df.death_rate_fev_2023[,8:1007])

summarize_CMR_draws <- function(data) {
  # Metadata columns
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                  "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  # 1. Country-year summary
  draw_matrix <- as.matrix(data[, draw_cols])
  
  summary_country_year <- data %>%
    dplyr::select(Country, Year, Income, gbd_region, Endemic, GDP) %>%
    mutate(
      median = apply(draw_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 2. Overall summary
  overall_draws <- data %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    group = "Overall",
    median = quantile(overall_draws, probs = 0.50, na.rm = TRUE),
    lower = quantile(overall_draws, probs = 0.025, na.rm = TRUE),
    upper = quantile(overall_draws, probs = 0.975, na.rm = TRUE))
  
  
  
  # 3. Income-level summary
  income_draws <- data %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE)),
              .groups = "drop")
  
  income_matrix <- as.matrix(income_draws[, draw_cols])
  
  summary_income <- income_draws %>%
    dplyr::select(Income) %>%
    mutate(
      median = apply(income_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
      lower = apply(income_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(income_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 4. GBD-region summary
  
  region_draws <- data %>%
    filter(
      !is.na(gbd_region)
    ) %>%
    group_by(gbd_region) %>%
    summarise(
      across(
        all_of(draw_cols),
        ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE)),
      .groups = "drop")
  
  region_matrix <- as.matrix(region_draws[, draw_cols])
  
  summary_gbd_region <- region_draws %>%
    dplyr::select(gbd_region) %>%
    mutate(
      median = apply(region_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
      lower = apply(region_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(region_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  # Return all four summaries
  list(
    overall = summary_overall,
    country_year = summary_country_year,
    income = summary_income,
    gbd_region = summary_gbd_region
  )
}

summary.mortality_lrti_Ab_preventable    <- summarize_CMR_draws(df.mortality_lrti_Ab_preventable)
summary.mortality_lrti_other_preventable <- summarize_CMR_draws(df.mortality_lrti_other_preventable)
summary.mortality_lrti_both_preventable  <- summarize_CMR_draws(df.mortality_lrti_both_preventable )
summary.proportion_lrti_Ab_preventable   <- summarize_CMR_draws(df.proportion_lrti_Ab_preventable  )
summary.proportion_lrti_Ab_preventable_all_mrotality   <- summarize_CMR_draws(df.proportion_lrti_Ab_preventable_all_mortality )

summary.mortality_fev_Ab_preventable     <- summarize_CMR_draws(df.mortality_fev_Ab_preventable)
summary.mortality_fev_other_preventable  <- summarize_CMR_draws(df.mortality_fev_other_preventable )
summary.mortality_fev_both_preventable   <- summarize_CMR_draws(df.mortality_fev_both_preventable  )
summary.proportion_fev_Ab_preventable    <- summarize_CMR_draws(df.proportion_fev_Ab_preventable   )
summary.proportion_fev_Ab_preventable_all_mrotality   <- summarize_CMR_draws(df.proportion_fev_Ab_preventable_all_mortality )






# (Main 4)  Plot: preventable mortality versus GDP, colored by income -------------

# pdf("figures/supplementary/mortality_to_incidence/(Main 4) preventable mortality rate ~ GDP (color by income)_2023.pdf", width = 6, height = 3.25)
# 
# layout(matrix(c(0,1,2,3,10,4,9,
#                 0,5,6,7,10,8,9), nrow = 2, byrow = TRUE),
#        widths = c(0.1, 1, 1, 1, 0.25, 1, 0.9), heights = c(1, 1))

par(oma = c(0, 0, 0, 0))
par(mar = c(2.0, 2.5, 0.8, 0.1))
par(mgp = c(0.35, 0.35, 0))
par(bty = "l")

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

## GDP axis: log10 positions with labels on the original scale
xmin <- 500
xmax <- 20000

x_tick_values <- round(10^(seq(log10(500), log10(20000), length.out = 4)), digits = 0)
x_ticks <- log10(x_tick_values)
x_labels <- format(x_tick_values, big.mark = ",", scientific = FALSE)
xlim_log <- log10(c(xmin, xmax))

cex.axis_value <- 0.7
cex.axis_title <- 0.55
cex_title <- 0.55
cex_legend <- 0.7

## Plotting floor for mortality rates
plot_floor <- 1e-7


## Prepare mortality-rate data -----------------------------------------------

mortality_lrti_Ab_plot <- summary.mortality_lrti_Ab_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))

mortality_lrti_other_plot <- summary.mortality_lrti_other_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))

mortality_lrti_both_plot <- summary.mortality_lrti_both_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))

mortality_fev_Ab_plot <- summary.mortality_fev_Ab_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))

mortality_fev_other_plot <- summary.mortality_fev_other_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))

mortality_fev_both_plot <- summary.mortality_fev_both_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper)) %>%
  mutate(median = pmax(median, plot_floor), lower = pmax(lower, plot_floor), upper = pmax(upper, plot_floor))


## Common logarithmic y-axis for six mortality-rate panels --------------------
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

common_y_exp_min <- floor(log10(min(all_y_values, na.rm = TRUE)))
common_y_exp_max <- ceiling(log10(max(all_y_values, na.rm = TRUE)))

common_y_ticks <- seq(common_y_exp_min, common_y_exp_max, by = 1)
common_y_labels <- parse(text = paste0("10^", common_y_ticks))
common_y_lim_log <- range(common_y_ticks)


## Prepare proportion data ----------------------------------------------------

proportion_lrti_plot <- summary.proportion_lrti_Ab_preventable_all_mrotality$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper))

proportion_fev_plot <- summary.proportion_fev_Ab_preventable_all_mrotality$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(median), is.finite(lower), is.finite(upper))


### LRTI: preventable by antibiotics ------------------------------------------

dat <- mortality_lrti_Ab_plot
income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("Preventable bacterial LRTI", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)

mtext("by antibiotics", side = 3, line = 0, cex = cex_title, font = 2)


### LRTI: preventable by other factors ----------------------------------------

dat <- mortality_lrti_other_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("by other factors", side = 3, line = 0, cex = cex_title, font = 2)

### LRTI: preventable by antibiotics and other factors ------------------------

dat <- mortality_lrti_both_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)


### LRTI: proportion prevented by antibiotics ---------------------------------

dat <- proportion_lrti_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

proportion_lrti_ticks <- seq(0, 0.4, by = 0.1)
proportion_lrti_labels <- format(proportion_lrti_ticks, nsmall = 1)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = c(0, 0.4),
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = proportion_lrti_ticks, labels = proportion_lrti_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = dat$lower,
  x1 = log10(dat$GDP),
  y1 = dat$upper,
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  dat$median,
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("Antibiotic share of total\npreventable bacterial fever mortality", side = 2, line = 1.8, cex = cex.axis_title)

fit_lrti <- lm(median ~ log(GDP), data = dat)
gdp_seq_lrti <- exp(seq(log(xmin), log(xmax), length.out = 200))
pred_lrti <- predict(fit_lrti, newdata = data.frame(GDP = gdp_seq_lrti))


### Fever: preventable by antibiotics -----------------------------------------

dat <- mortality_fev_Ab_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("Preventable bacterial fever", side = 2, line = 2.5, cex = cex.axis_title)
mtext(expression("mortality rate"^a), side = 2, line = 1.5, cex = cex.axis_title)
mtext("by antibiotics", side = 3, line = -0.50, cex = cex_title, font = 2)


### Fever: preventable by other factors ---------------------------------------

dat <- mortality_fev_other_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("by other factors", side = 3, line = -0.5, cex = cex_title, font = 2)
mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)

### Fever: preventable by antibiotics and other factors -----------------------

dat <- mortality_fev_both_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = common_y_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = log10(dat$lower*1000),
  x1 = log10(dat$GDP),
  y1 = log10(dat$upper*1000),
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  log10(dat$median*1000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("by all factors", side = 3, line = -0.5, cex = cex_title, font = 2)
mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)

### Fever: proportion prevented by antibiotics --------------------------------

dat <- proportion_fev_plot

income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

proportion_fev_ticks <- c(0, 0.16, 0.32, 0.48, 0.64)
proportion_fev_labels <- format(proportion_fev_ticks, nsmall = 1)

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = xlim_log,
  ylim = c(0, 0.64),
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.025, padj = 0.3, mgp = c(0.35, -0.20, 0))
axis(2, at = proportion_fev_ticks, labels = proportion_fev_labels, las = 1, cex.axis = cex.axis_value, tck = -0.025)

segments(
  x0 = log10(dat$GDP),
  y0 = dat$lower,
  x1 = log10(dat$GDP),
  y1 = dat$upper,
  col = adjustcolor(income_col, alpha.f = 0.30),
  lwd = 0.6
)

points(
  log10(dat$GDP),
  dat$median,
  pch = 16,
  cex = 0.7,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("GDP per capita (PPP)", side = 1, line = 1, cex = cex.axis_title)
mtext("Antibiotic share of total\npreventable bacterial fever mortality", side = 2, line = 1.8, cex = cex.axis_title)

fit_fev_endemic <- lm(median ~ log(GDP), data = dat %>% filter(Endemic == 1))
fit_fev_non_endemic <- lm(median ~ log(GDP), data = dat %>% filter(Endemic == 0))

gdp_seq_fev <- exp(seq(log(xmin), log(xmax), length.out = 200))
pred_fev_endemic <- predict(fit_fev_endemic, newdata = data.frame(GDP = gdp_seq_fev))
pred_fev_non_endemic <- predict(fit_fev_non_endemic, newdata = data.frame(GDP = gdp_seq_fev))


### Fever legend ---------------------------------------------------------------

par(mar = c(0, 0, 0, 0))
plot.new()

legend(
  "center",
  legend = c(income_labels[names(income_cols_fixed)]),
  col = c(adjustcolor(income_cols_fixed, alpha.f = 0.80)),
  pch = c(rep(16, length(income_cols_fixed))),
  lty = c(rep(NA, length(income_cols_fixed))),
  lwd = c(rep(NA, length(income_cols_fixed))),
  pt.cex = c(rep(0.6, length(income_cols_fixed))),
  cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.40,
  seg.len = 1.5,
  bty = "n"
)

# dev.off()




















# _____________________________________________ ----------
# 3. Absolute death preventable ----------------------------------------------------

# 3-1. by Antibiotics ----------------------------------------------------

summarize_absolute_draws <- function(data, selected_year, rate_scale = 1) {
  
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                "Endemic", "Population")
  draw_cols <- setdiff(names(data), meta_cols)
  
  data_count <- data %>%
    filter(Year == selected_year) %>%
    mutate(across(all_of(draw_cols), ~ .x * Population / rate_scale))
  
  # 1. Country
  draw_matrix <- as.matrix(data_count[, draw_cols])
  
  summary_country <- data_count %>%
    dplyr::select(Country, Year, Income, gbd_region, GDP, Population) %>%
    mutate(
      median = apply(draw_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  # 2. Overall
  overall_draws <- data_count %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    Year = selected_year,
    group = "Overall",
    median = quantile(overall_draws, 0.500, na.rm = TRUE),
    lower = quantile(overall_draws, 0.025, na.rm = TRUE),
    upper = quantile(overall_draws, 0.975, na.rm = TRUE))
  
  # 3. Income
  income_draws <- data_count %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  income_matrix <- as.matrix(income_draws[, draw_cols])
  
  summary_income <- income_draws %>%
    dplyr::select(Income) %>%
    mutate(
      Year = selected_year,
      median = apply(income_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(income_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(income_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  # 4. GBD region
  region_draws <- data_count %>%
    filter(!is.na(gbd_region)) %>%
    group_by(gbd_region) %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  region_matrix <- as.matrix(region_draws[, draw_cols])
  
  summary_gbd_region <- region_draws %>%
    dplyr::select(gbd_region) %>%
    mutate(
      Year = selected_year,
      median = apply(region_matrix, 1, quantile, probs = 0.500, na.rm = TRUE),
      lower = apply(region_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(region_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  list(overall = summary_overall,
       country = summary_country,
       income = summary_income,
       gbd_region = summary_gbd_region)
}

summary.number_lrti_Ab_preventable    <- summarize_absolute_draws(df.mortality_lrti_Ab_preventable, 2023)
summary.number_fev_Ab_preventable     <- summarize_absolute_draws(df.mortality_fev_Ab_preventable, 2023)
df.mortality_lrti_and_fever_Ab_preventable_matrix <- df.mortality_lrti_Ab_preventable[,8:1007] + df.mortality_fev_Ab_preventable[,8:1007]
df.mortality_lrti_and_fever_Ab_preventable <- cbind(df.mortality_lrti_Ab_preventable[,1:7], df.mortality_lrti_and_fever_Ab_preventable_matrix)
summarize_absolute_draws(df.mortality_lrti_and_fever_Ab_preventable, 2023)

death_rate_lrti_summary <- cbind(UNI, death_rate_lrti)
death_rate_fev_summary <- cbind(UNI, death_rate_fev)
death_rate_all <- death_rate_lrti + death_rate_fev
death_rate_all_summary <- cbind(UNI, death_rate_all)

summary.number_lrti_death    <- summarize_absolute_draws(death_rate_lrti_summary, 2023)
summary.number_fev_death     <- summarize_absolute_draws(death_rate_fev_summary, 2023)
summary.number_all_death     <- summarize_absolute_draws(death_rate_all_summary, 2023)

death_rate_lrti_summary[1:12,1:12]
row_2023 <- which(UNI$Year == 2023)

population_2023 <- UNI$Population[row_2023]
death_rate_lrti_2023 <- death_rate_lrti[row_2023, , drop = FALSE]
death_rate_fev_2023 <- death_rate_fev[row_2023, , drop = FALSE]

par(mfrow = c(2,1))
death_lrti_2023 <- sweep(death_rate_lrti_2023, 1, population_2023, "*")
death_fev_2023 <- sweep(death_rate_fev_2023, 1, population_2023, "*")
hist(colSums(death_lrti_2023))
hist(colSums(death_fev_2023))

death_all_2023 <- death_lrti_2023 + death_fev_2023


save(death_lrti_2023, file = "outputs/lrti_death_draw_by_country.RData")
save(death_fev_2023, file = "outputs/fever_death_draw_by_country.RData")
save(death_all_2023, file = "outputs/all_death_draw_by_country.RData")

save(summary.number_lrti_death, file = "outputs/lrti_death_summary.RData")
save(summary.number_fev_death, file = "outputs/fever_death_summary.RData") 
save(summary.number_all_death, file = "outputs/all_death_summary.RData") 

death_lrti_2023_all_LMIC <- colSums(death_lrti_2023)
death_fev_2023_all_LMIC <- colSums(death_fev_2023)
death_all_2023_all_LMIC <- colSums(death_all_2023)

save(death_lrti_2023_all_LMIC,  file = "outputs/lrti_death_draw.RData")
save(death_fev_2023_all_LMIC,  file = "outputs/fever_death_draw.RData")
save(death_all_2023_all_LMIC,  file = "outputs/all_death_draw.RData")

write.csv(death_lrti_2023_all_LMIC, "outputs/lrti_death_draw.csv")
write.csv(death_fev_2023_all_LMIC,  "outputs/fever_death_draw.csv")
write.csv(death_all_2023_all_LMIC,  "outputs/all_death_draw.csv")



summary.number_lrti_both_preventable <- summarize_absolute_draws(df.mortality_lrti_both_preventable, 2023)
summary.number_fev_both_preventable <- summarize_absolute_draws(df.mortality_fev_both_preventable, 2023)


## Create summary objects -------------------------------------------------------

summary.number_lrti_Ab_preventable <- summarize_absolute_draws(df.mortality_lrti_Ab_preventable, 2023)
summary.number_fev_Ab_preventable <- summarize_absolute_draws(df.mortality_fev_Ab_preventable, 2023)

write.csv(summary.number_lrti_Ab_preventable$country, "outputs/pipelines/suppl_table/preventable_deaths_Ab_lrti.csv")
write.csv(summary.number_fev_Ab_preventable$country, "outputs/pipelines/suppl_table/preventable_deaths_Ab_fever.csv")


## Plot settings ----------------------------------------------------------------

pdf_width <- 8.6
pdf_height <- 4.2



mar_cat <- c(2.3, 0.2, 0.5, 0.1)
mar_plot <- c(2.3, 0.3, 0.5, 0.2)
mar_text <- c(2.3, 0.1, 0.5, 0.1)

panel_oma <- c(0, 0, 0, 0)
panel_mgp <- c(0.35, 0.35, 0)

cex.axis_value <- 0.70
cex.category <- 0.70
cex.axis_title <- 0.55
cex_title <- 0.60
cex_point <- 0.70
cex_text <- 0.70
cex_text_title <- 0.7

lwd_interval <- 0.8
lwd_divider <- 1.2

alpha_interval <- 0.40
alpha_point <- 0.80
axis_tick_length <- -0.025

plot_floor <- 1e-3

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

category_y <- rev(seq_along(category_levels))
divider_y <- c(category_y[1] - 0.5, category_y[4] - 0.5)

ylim_panel <- c(0.5, length(category_levels) + 1.0)
text_title_y <- length(category_levels) + 0.8

number_title_x <- 0.5
number_text_x <- 0.5

## Prepare plotting data --------------------------------------------------------

dat_lrti_Ab <- dplyr::bind_rows(
  summary.number_lrti_Ab_preventable$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_lrti_Ab_preventable$income %>%
    dplyr::transmute(group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"), median, lower, upper),
  summary.number_lrti_Ab_preventable$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

dat_fev_Ab <- dplyr::bind_rows(
  summary.number_fev_Ab_preventable$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_fev_Ab_preventable$income %>%
    dplyr::transmute(group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"), median, lower, upper),
  summary.number_fev_Ab_preventable$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

## Common x-axis ----------------------------------------------------------------

Ab_x_values <- c(dat_lrti_Ab$lower, dat_lrti_Ab$upper, dat_fev_Ab$lower, dat_fev_Ab$upper)
Ab_x_values <- Ab_x_values[is.finite(Ab_x_values) & Ab_x_values > 0]

Ab_x_exp <- seq(floor(log10(min(Ab_x_values))), ceiling(log10(max(Ab_x_values))), by = 1)
Ab_x_ticks <- Ab_x_exp
Ab_x_labels <- parse(text = paste0("10^", Ab_x_exp))
Ab_xlim_log <- range(Ab_x_ticks)

## (Main 5) Plot -------------------------------------------------------------------------

pdf("figures/supplementary/mortality_to_incidence/(Main 5) Absolute preventable deaths_Ab.pdf", width = 6, height = 2)

layout(matrix(c(1, 2, 3, 4, 5), nrow = 1, byrow = TRUE), widths = c(1.5, 0.75, 0.85, 0.75, 0.85))
par(oma = panel_oma)

### (1) Category text column ----------------------------------------------------

par(mar = mar_cat, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

# segments(
#   x0 = rep(0, length(divider_y)),
#   y0 = divider_y,
#   x1 = rep(1, length(divider_y)),
#   y1 = divider_y,
#   col = "grey90",
#   lwd = lwd_divider
# )

text(
  rep(0.98, length(category_levels)),
  category_y,
  labels = category_levels,
  adj = c(1, 0.5),
  cex = cex.category,
  xpd = NA
)

### (2) LRTI plot column --------------------------------------------------------

dat <- dat_lrti_Ab

par(mar = mar_plot, mgp = panel_mgp, bty = "l")
plot(
  NA_real_, NA_real_,
  type = "n",
  xlim = Ab_xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = Ab_x_ticks,
  labels = Ab_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = rep(Ab_xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(Ab_xlim_log[2], length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex.axis_title)

### (3) LRTI number text column -------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

text(number_title_x, text_title_y, labels = "Median (95% UI)", adj = c(0.5, 0.5), cex = cex_text_title, font = 2)

text(
  rep(number_text_x, nrow(dat_lrti_Ab)),
  category_y,
  labels = dat_lrti_Ab$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

### (4) Fever plot column -------------------------------------------------------

dat <- dat_fev_Ab

par(mar = mar_plot, mgp = panel_mgp, bty = "l")
plot(
  NA_real_, NA_real_,
  type = "n",
  xlim = Ab_xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = Ab_x_ticks,
  labels = Ab_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = rep(Ab_xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(Ab_xlim_log[2], length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex.axis_title)

### (5) Fever number text column ------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

text(number_title_x, text_title_y, labels = "Median (95% UI)", adj = c(0.5, 0.5), cex = cex_text_title, font = 2)

text(
  rep(number_text_x, nrow(dat_fev_Ab)),
  category_y,
  labels = dat_fev_Ab$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

dev.off()



## (for main text) generate the proportion of Ab preventable death over all-cause death by income -----------------------
absolute_matrix <- function(data, selected_year, rate_scale = 1) {
  
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "Endemic", "Population")
  draw_cols <- setdiff(names(data), meta_cols)
  
  data_count <- data %>%
    filter(Year == selected_year) %>%
    mutate(across(all_of(draw_cols), ~ .x * Population / rate_scale))
  
  # Income
  income_draws <- data_count %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  income_matrix <- as.matrix(income_draws[, draw_cols])
  
  # Changed: assign income groups as row names
  rownames(income_matrix) <- income_draws$Income
  
  # Changed: return the absolute number matrix
  return(income_matrix)
}

Ab_preventable_lrti <- absolute_matrix(df.mortality_lrti_Ab_preventable, 2023)
Ab_preventable_fev <- absolute_matrix(df.mortality_fev_Ab_preventable, 2023)

death_all_2023 <- load("outputs/death_all_2023_income_matrix.RData")
prop_lrti_Ab_all_death <- Ab_preventable_lrti/death_all_2023_income_matrix
prop_fev_Ab_all_death <- Ab_preventable_fev/death_all_2023_income_matrix

summary_matrix <- function(mat) {
  data.frame(
    Income = rownames(mat),
    median = round(apply(mat, 1, quantile, probs = 0.500, na.rm = TRUE), digits = 3),
    lower  = round(apply(mat, 1, quantile, probs = 0.025, na.rm = TRUE), digits = 3),
    upper  = round(apply(mat, 1, quantile, probs = 0.975, na.rm = TRUE), digits = 3),
    row.names = NULL)
}

summary_prop_lrti_Ab_all_death <-
  summary_matrix(prop_lrti_Ab_all_death)

summary_prop_fev_Ab_all_death <-
  summary_matrix(prop_fev_Ab_all_death)



















# 3-2. by all factors ----------------------------------------------------

summary.number_lrti_both_preventable    <- summarize_absolute_draws(df.mortality_lrti_both_preventable, 2023)
summary.number_fev_both_preventable     <- summarize_absolute_draws(df.mortality_fev_both_preventable, 2023)

death_rate_lrti_summary <- cbind(UNI, death_rate_lrti)
death_rate_fev_summary <- cbind(UNI, death_rate_fev)

summary.number_lrti_death    <- summarize_absolute_draws(death_rate_lrti_summary, 2023)
summary.number_fev_death     <- summarize_absolute_draws(death_rate_fev_summary, 2023)

summary.number_lrti_both_preventable <- summarize_absolute_draws(df.mortality_lrti_both_preventable, 2023)
summary.number_fev_both_preventable <- summarize_absolute_draws(df.mortality_fev_both_preventable, 2023)


# (for main text) overall sum of LRTI and AFI ----------------------
overall_matrix <- function(data, selected_year, rate_scale = 1) {
  
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "Endemic", "Population")
  draw_cols <- setdiff(names(data), meta_cols)
  
  data_count <- data %>%
    filter(Year == selected_year) %>%
    mutate(across(all_of(draw_cols), ~ .x * Population / rate_scale))
  
  overall_draws <- data_count %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE)))
  
  overall_matrix <- as.matrix(overall_draws[, draw_cols, drop = FALSE])
  rownames(overall_matrix) <- "Total"
  
  return(overall_matrix)
}

both_preventable_lrti <- overall_matrix(df.mortality_lrti_both_preventable, 2023)
both_preventable_fev <- overall_matrix(df.mortality_fev_both_preventable, 2023)
both_preventable_all <- both_preventable_lrti + both_preventable_fev
summary_both_preventable_lrti <- summary_matrix(both_preventable_all)
summary_both_preventable_lrti


Ab_preventable_lrti <- overall_matrix(df.mortality_lrti_Ab_preventable, 2023)
Ab_preventable_fev <- overall_matrix(df.mortality_fev_Ab_preventable, 2023)
Ab_preventable_all <- Ab_preventable_lrti + Ab_preventable_fev
summary_Ab_preventable_all <- summary_matrix(Ab_preventable_all)
summary_Ab_preventable_all

Ab_preventable_prop <- Ab_preventable_all/both_preventable_all
summary_Ab_preventable_prop <- summary_matrix(Ab_preventable_prop)
summary_Ab_preventable_prop

saveRDS(Ab_preventable_all, "outputs/pipelines/Ab_preventable_all.rds")



## Create summary objects -------------------------------------------------------

summary.number_lrti_both_preventable <- summarize_absolute_draws(df.mortality_lrti_both_preventable, 2023)
summary.number_fev_both_preventable <- summarize_absolute_draws(df.mortality_fev_both_preventable, 2023)

write.csv(summary.number_lrti_both_preventable$country, "outputs/pipelines/suppl_table/preventable_deaths_country_level_lrti.csv")
write.csv(summary.number_fev_both_preventable$country, "outputs/pipelines/suppl_table/preventable_deaths_country_level_fever.csv")












## Plot settings ----------------------------------------------------------------

pdf_width <- 8.6
pdf_height <- 4.2



mar_cat <- c(2.3, 0.2, 0.5, 0.1)
mar_plot <- c(2.3, 0.3, 0.5, 0.2)
mar_text <- c(2.3, 0.1, 0.5, 0.1)

panel_oma <- c(0, 0, 0, 0)
panel_mgp <- c(0.35, 0.35, 0)

cex.axis_value <- 0.70
cex.category <- 0.70
cex.axis_title <- 0.55
cex_title <- 0.60
cex_point <- 0.70
cex_text <- 0.70
cex_text_title <- 0.7

lwd_interval <- 0.8
lwd_divider <- 1.2

alpha_interval <- 0.40
alpha_point <- 0.80
axis_tick_length <- -0.025

plot_floor <- 1e-3

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

category_y <- rev(seq_along(category_levels))
divider_y <- c(category_y[1] - 0.5, category_y[4] - 0.5)

ylim_panel <- c(0.5, length(category_levels) + 1.0)
text_title_y <- length(category_levels) + 0.8

number_title_x <- 0.5
number_text_x <- 0.5

## Prepare plotting data --------------------------------------------------------

dat_lrti_both <- dplyr::bind_rows(
  summary.number_lrti_both_preventable$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_lrti_both_preventable$income %>%
    dplyr::transmute(group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"), median, lower, upper),
  summary.number_lrti_both_preventable$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

dat_fev_both <- dplyr::bind_rows(
  summary.number_fev_both_preventable$overall %>%
    dplyr::transmute(group = "Total", median, lower, upper),
  summary.number_fev_both_preventable$income %>%
    dplyr::transmute(group = dplyr::recode(as.character(Income), L = "Low-income", LM = "Lower-middle-income", UM = "Upper-middle-income"), median, lower, upper),
  summary.number_fev_both_preventable$gbd_region %>%
    dplyr::transmute(group = gbd_region, median, lower, upper)
) %>%
  dplyr::mutate(group = factor(group, levels = category_levels)) %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    col = unname(category_cols[as.character(group)]),
    dplyr::across(c(median, lower, upper), ~ pmax(.x, plot_floor)),
    median_text = formatC(median, format = "f", digits = 0, big.mark = ","),
    lower_text = formatC(lower, format = "f", digits = 0, big.mark = ","),
    upper_text = formatC(upper, format = "f", digits = 0, big.mark = ","),
    result_text = paste0(median_text, " (", lower_text, "–", upper_text, ")")
  )

## Common x-axis ----------------------------------------------------------------

both_x_values <- c(dat_lrti_both$lower, dat_lrti_both$upper, dat_fev_both$lower, dat_fev_both$upper)
both_x_values <- both_x_values[is.finite(both_x_values) & both_x_values > 0]

both_x_exp <- seq(floor(log10(min(both_x_values))), ceiling(log10(max(both_x_values))), by = 1)
both_x_ticks <- both_x_exp
both_x_labels <- parse(text = paste0("10^", both_x_exp))
both_xlim_log <- range(both_x_ticks)

# (Supple 5_1) plot -------------------------------------------------------------------------

pdf("figures/supplementary/mortality_to_incidence/(Supple 5) Absolute preventable deaths_both.pdf", width = 6, height = 2)

layout(matrix(c(1, 2, 3, 4, 5), nrow = 1, byrow = TRUE), widths = c(1.5, 0.75, 0.85, 0.75, 0.85))
par(oma = panel_oma)

### (1) Category text column ----------------------------------------------------

par(mar = mar_cat, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

# segments(
#   x0 = rep(0, length(divider_y)),
#   y0 = divider_y,
#   x1 = rep(1, length(divider_y)),
#   y1 = divider_y,
#   col = "grey90",
#   lwd = lwd_divider
# )

text(
  rep(0.98, length(category_levels)),
  category_y,
  labels = category_levels,
  adj = c(1, 0.5),
  cex = cex.category,
  xpd = NA
)

### (2) LRTI plot column --------------------------------------------------------

dat <- dat_lrti_both

par(mar = mar_plot, mgp = panel_mgp, bty = "l")
plot(
  NA_real_, NA_real_,
  type = "n",
  xlim = both_xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = both_x_ticks,
  labels = both_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = rep(both_xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(both_xlim_log[2], length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex.axis_title)

### (3) LRTI number text column -------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

text(number_title_x, text_title_y, labels = "Median (95% UI)", adj = c(0.5, 0.5), cex = cex_text_title, font = 2)

text(
  rep(number_text_x, nrow(dat_lrti_both)),
  category_y,
  labels = dat_lrti_both$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

### (4) Fever plot column -------------------------------------------------------

dat <- dat_fev_both

par(mar = mar_plot, mgp = panel_mgp, bty = "l")
plot(
  NA_real_, NA_real_,
  type = "n",
  xlim = both_xlim_log,
  ylim = ylim_panel,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = both_x_ticks,
  labels = both_x_labels,
  cex.axis = cex.axis_value,
  tck = axis_tick_length,
  padj = 0.3,
  mgp = c(0.35, -0.20, 0)
)

segments(
  x0 = rep(both_xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(both_xlim_log[2], length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

segments(
  x0 = log10(dat$lower),
  y0 = category_y,
  x1 = log10(dat$upper),
  y1 = category_y,
  col = adjustcolor(dat$col, alpha.f = alpha_interval),
  lwd = lwd_interval
)

points(
  log10(dat$median),
  category_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(dat$col, alpha.f = alpha_point)
)

mtext("Number of preventable deaths", side = 1, line = 1.1, cex = cex.axis_title)

### (5) Fever number text column ------------------------------------------------

par(mar = mar_text, mgp = panel_mgp, bty = "n")
plot.new()
plot.window(xlim = c(0, 1), ylim = ylim_panel, xaxs = "i", yaxs = "i")

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
)

text(number_title_x, text_title_y, labels = "Median (95% UI)", adj = c(0.5, 0.5), cex = cex_text_title, font = 2)

text(
  rep(number_text_x, nrow(dat_fev_both)),
  category_y,
  labels = dat_fev_both$result_text,
  adj = c(0.5, 0.5),
  cex = cex_text
)

dev.off()