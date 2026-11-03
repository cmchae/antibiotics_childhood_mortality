

ceeca <- read.csv("data/AMR/GBD_2021/ceeeca_attr.csv")
latin <- read.csv("data/AMR/GBD_2021/latin_attr.csv")
namea <- read.csv("data/AMR/GBD_2021/namea_attr.csv")
sa    <- read.csv("data/AMR/GBD_2021/sa_attr.csv")
sea   <- read.csv("data/AMR/GBD_2021/sea_attr.csv")
ssa   <- read.csv("data/AMR/GBD_2021/ssa_attr.csv")



estimate <- function(val, lower, upper) { # fit optimizing function (with least error of each estimate - upper/lower/point)
  obj_fun <- function(par) {
    shape <- par[1]
    scale <- par[2]
    q_lower <- qgamma(0.025, shape = shape, scale = scale)
    q_upper <- qgamma(0.975, shape = shape, scale = scale)
    mean_val <- shape * scale
    (q_lower - lower)^2 + (q_upper - upper)^2 + (mean_val - val)^2
  }
  
  sd_est <- (upper - lower) / 4
  shape_init <- (val / sd_est)^2
  scale_init <- val / shape_init
  
  fit <- optim(
    par = c(shape_init, scale_init),
    fn = obj_fun,
    method = "L-BFGS-B",
    lower = c(1e-3, 1e-3)
  )
  
  shape <- fit$par[1]
  scale <- fit$par[2]
  
  fitted_mean <- shape * scale   # Fitted values
  fitted_lower <- qgamma(0.025, shape = shape, scale = scale)
  fitted_upper <- qgamma(0.975, shape = shape, scale = scale)
  
  error_val <- abs(fitted_mean - val)/val  # errors
  error_lower <- abs(fitted_lower - lower)/lower
  error_upper <- abs(fitted_upper - upper)/upper
  
  return(c(shape = shape, scale = scale,
           error_val = error_val,
           error_lower = error_lower,
           error_upper = error_upper))
}


ceeca <- ceeca %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()


latin <- latin %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()


namea <- namea %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()


sa <- sa %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()


sea <- sea %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()


ssa <- ssa %>%
  filter(year_id == 2023 & sex_id == 3 & age_group_id == 1) %>%
  rowwise() %>%
  mutate(
    gamma_est = list(estimate(deathcounts_mean, deathcounts_lower, deathcounts_upper)),
    shape = gamma_est[[1]],
    scale = gamma_est[[2]],
    error_val = gamma_est[[3]],
    error_lower = gamma_est[[4]],
    error_upper = gamma_est[[5]]
  ) %>%
  dplyr::select(-gamma_est) %>%
  ungroup()

meta_dat <- rbind(ceeca, latin, namea, sa, sea, ssa)

n_sim <- 1000
sim_dat <- rbind(
  rgamma(n_sim, shape = ceeca$shape, scale = ceeca$scale),
  rgamma(n_sim, shape = latin$shape, scale = latin$scale),
  rgamma(n_sim, shape = namea$shape, scale = namea$scale),
  rgamma(n_sim, shape = sa$shape,    scale = sa$scale),
  rgamma(n_sim, shape = sea$shape,   scale = sea$scale),
  rgamma(n_sim, shape = ssa$shape,   scale = ssa$scale)
)

  
AMR_dat <- cbind(meta_dat, sim_dat)
dim(AMR_dat)

AMR_dat[1:6,1:20]

gbd_region <- c(
  "Central Europe, Eastern Europe, and Central Asia",
  "Latin America and Caribbean",
  "North Africa and Middle East",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "Sub-Saharan Africa")

AMR_dat$location_id <- gbd_region

AMR_dat <- AMR_dat %>% dplyr::select(-c(age_group_id, sex_id, shape, scale, error_val, error_lower, error_upper))

(quantile(AMR_dat[1, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[1,3:5])/AMR_dat[1,3:5]*100
(quantile(AMR_dat[2, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[2,3:5])/AMR_dat[2,3:5]*100
(quantile(AMR_dat[3, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[3,3:5])/AMR_dat[3,3:5]*100
(quantile(AMR_dat[4, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[4,3:5])/AMR_dat[4,3:5]*100
(quantile(AMR_dat[5, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[5,3:5])/AMR_dat[5,3:5]*100
(quantile(AMR_dat[6, 6:1005], c(0.5, 0.975, 0.025), na.rm = TRUE) - AMR_dat[6,3:5])/AMR_dat[6,3:5]*100

AMR_total = AMR_dat[1, 6:1005] + AMR_dat[2, 6:1005] + AMR_dat[3, 6:1005] + AMR_dat[4, 6:1005] + AMR_dat[5, 6:1005] + AMR_dat[6, 6:1005]
AMR_total

quantile(AMR_total, c(0.5, 0.025, 0.975), na.rm = TRUE)




IHME_country <- read.csv("data/comparison/IHME_country.csv")
population <- read.csv("data/metadata/Population_by_age.csv") %>% filter(Year == 2023) %>% mutate(population = Age_0  +  Age_1  +  Age_2  +  Age_3  +  Age_4) %>% dplyr::select(Country, Year, population)

IHME_country
setdiff(unique(population$Country), unique(IHME_country$Country))
setdiff(unique(IHME_country$Country), unique(population$Country))

unique(population$Country)
unique(IHME_country$Country)

IHME_country$Country[
  grepl("^S.*o Tom.*and Pr.*ncipe$", IHME_country$Country, useBytes = TRUE)
] <- "Sao Tome and Principe"

population <- population %>%
  dplyr::mutate(
    Country = dplyr::recode(
      Country,
      "Republic of Moldova" = "Moldova",
      "Russian Federation" = "Russia",
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Bahamas" = "The Bahamas",
      "United States Virgin Islands" = "Virgin Islands",
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Iran (Islamic Republic of)" = "Iran",
      "State of Palestine" = "Palestine",
      "Syrian Arab Republic" = "Syria",
      "Dem. People's Republic of Korea" = "North Korea",
      "China, Taiwan Province of China" = "Taiwan (province of China)",
      "Micronesia (Fed. States of)" = "Federated States of Micronesia",
      "Lao People's Democratic Republic" = "Laos",
      "Congo" = "Congo (Brazzaville)",
      "Democratic Republic of the Congo" = "DR Congo",
      "United Republic of Tanzania" = "Tanzania",
      "Gambia" = "The Gambia",
      "Sao Tome and Principe" = "Sao Tome and Principe"
    )
  )

IHME_population <- IHME_country %>% inner_join(population, by = "Country")
UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(Year == 2023 & .imp_pc == 1) %>% select(Country, Population)

colnames(IHME_population) <- c("Country", "gbd_region", "Year", "Population_IHME")
colnames(UNI) <- c("Country", "Population_UNI")


UNI_country <- unique(UNI$Country)
IHME_country <- unique(IHME_population$Country)

setdiff(UNI_country, IHME_country)
setdiff(IHME_country, UNI_country)

IHME_population[IHME_population$Country == "DR Congo"           , "Country"] <- "Congo, Dem. Rep."   
IHME_population[IHME_population$Country == "C\x99te d'Ivoire"   , "Country"] <- "Cote d'Ivoire"      
IHME_population[IHME_population$Country == "Egypt"              , "Country"] <- "Egypt, Arab Rep."   
IHME_population[IHME_population$Country == "The Gambia"         , "Country"] <- "Gambia, The"        
IHME_population[IHME_population$Country == "Iran"               , "Country"] <- "Iran, Islamic Rep." 
IHME_population[IHME_population$Country == "Kyrgyzstan"         , "Country"] <- "Kyrgyz Republic"    
IHME_population[IHME_population$Country == "Laos"               , "Country"] <- "Lao PDR"     

population_dat <- IHME_population %>% left_join(UNI, by = "Country")

population_summary <- population_dat %>% arrange(gbd_region) %>%
  group_by(gbd_region) %>%
  summarize(
    IHME_pop_gbd_region = sum(Population_IHME, na.rm = TRUE),
    UNI_pop_gbd_region  = sum(Population_UNI, na.rm = TRUE),
    UNI_IHME_prop       = UNI_pop_gbd_region / IHME_pop_gbd_region 
  )

population_prop <- population_summary$UNI_IHME_prop

AMR_dat_result_1 <- AMR_dat[1, 6:1005] * population_prop[1]
AMR_dat_result_2 <- AMR_dat[2, 6:1005] * population_prop[2]
AMR_dat_result_3 <- AMR_dat[3, 6:1005] * population_prop[3]
AMR_dat_result_4 <- AMR_dat[4, 6:1005] * population_prop[4]
AMR_dat_result_5 <- AMR_dat[5, 6:1005] * population_prop[5]
AMR_dat_result_6 <- AMR_dat[6, 6:1005] * population_prop[6]

AMR_dat_result_total <- AMR_dat_result_1 + AMR_dat_result_2 + AMR_dat_result_3 + AMR_dat_result_4 + AMR_dat_result_5 + AMR_dat_result_6 

AMR_result <- rbind(
"Central Europe, Eastern Europe, and Central Asia" = quantile(AMR_dat_result_1, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"Latin America and Caribbean" = quantile(AMR_dat_result_2, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"North Africa and Middle East" = quantile(AMR_dat_result_3, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"South Asia" = quantile(AMR_dat_result_4, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"Southeast Asia, East Asia, and Oceania" = quantile(AMR_dat_result_5, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"Sub-Saharan Africa" = quantile(AMR_dat_result_6, c(0.5, 0.025, 0.975), na.rm = TRUE), 
"Overall" = quantile(AMR_dat_result_total, c(0.5, 0.025, 0.975), na.rm = TRUE) 
)

colnames(AMR_result) <- c("Median", "Lower", "Upper")
AMR_result


















# 6-1. death number calculation ---------------------------------------------

matrix_death_rate_q_year0    <- readRDS("outputs/pipelines/matrix_death_rate_year0.rds")
matrix_death_rate_q_year1    <- readRDS("outputs/pipelines/matrix_death_rate_year1.rds")
matrix_death_rate_q_year2to4 <- readRDS("outputs/pipelines/matrix_death_rate_year2to4.rds")

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(.imp_pc == 1) 
row_2023 <- which(UNI$Year == 2023)
UNI <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(Year == 2023 & .imp_pc == 1) %>% select(Country, gbd_region, Population)

population <- read.csv("data/metadata/Population_by_age.csv") %>% filter(Year == 2023)

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

setdiff(UNI$Country, population$Country)

UNI_pop <- UNI %>%
  dplyr::select(Country, gbd_region) %>%
  left_join(population, by = "Country") %>%
  mutate(
    Age_2to4 = Age_2 + Age_3 + Age_4,
    Age_all = Age_0 + Age_1 + Age_2 + Age_3 + Age_4
  ) %>%
  dplyr::select(Country, Year, gbd_region, Age_0, Age_1, Age_2to4, Age_all)

if (any(!is.finite(UNI_pop$Age_0)) ||
    any(!is.finite(UNI_pop$Age_1)) ||
    any(!is.finite(UNI_pop$Age_2to4))) {
  stop("Missing age-specific population for one or more mortality target rows.")
}

UNI_pop

matrix_death_rate_q_year0_2023 <- matrix_death_rate_q_year0[row_2023, , drop = FALSE]
matrix_death_rate_q_year1_2023 <- matrix_death_rate_q_year1[row_2023, , drop = FALSE]
matrix_death_rate_q_year2to4_2023 <- matrix_death_rate_q_year2to4[row_2023, , drop = FALSE]

death_year0_2023 <- sweep(matrix_death_rate_q_year0_2023, 1, UNI_pop$Age_0, "*")
death_year1_2023 <- sweep(matrix_death_rate_q_year1_2023, 1, UNI_pop$Age_1, "*")
death_year2to4_2023 <- sweep(matrix_death_rate_q_year2to4_2023, 1, UNI_pop$Age_2to4, "*")

death_all_2023_by_country <- death_year0_2023 + death_year1_2023 + death_year2to4_2023

death_all_2023_by_gbd_region <- rowsum(
  death_all_2023_by_country,
  group = UNI_pop$gbd_region
)

value <- 0.051; lower <- 0.024; upper <- 0.072
sd_lower <- (qlogis(value) - qlogis(lower)) / qnorm(0.975)
sd_upper <- (qlogis(upper) - qlogis(value)) / qnorm(0.975)
set.seed(123)
z <- rnorm(1000)
draw_af <- plogis(qlogis(value) + ifelse(z < 0, z * sd_lower, z * sd_upper))
length(draw_af)
quantile(draw_af, c(0.025, 0.5, 0.975))

death_AA_by_gbd_region <- sweep(death_all_2023_by_gbd_region, 2, draw_af, "*")
death_AA_by_gbd_region[,1:10]
AA_overall <- colSums(death_AA_by_gbd_region)
result_death_AA_by_gbd_region <- rbind(death_AA_by_gbd_region, Overall = AA_overall)
result_death_AA_by_gbd_region[,1:10]


quantile(result_death_AA_by_gbd_region[1,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[2,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[3,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[4,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[5,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[6,], c(0.5, 0.025, 0.975), na.rm = TRUE) 
quantile(result_death_AA_by_gbd_region[7,], c(0.5, 0.025, 0.975), na.rm = TRUE) 


AA_result <- rbind(
  "Central Europe, Eastern Europe, and Central Asia" = quantile(result_death_AA_by_gbd_region[1,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "Latin America and Caribbean" = quantile(result_death_AA_by_gbd_region[2,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "North Africa and Middle East" = quantile(result_death_AA_by_gbd_region[3,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "South Asia" = quantile(result_death_AA_by_gbd_region[4,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "Southeast Asia, East Asia, and Oceania" = quantile(result_death_AA_by_gbd_region[5,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "Sub-Saharan Africa" = quantile(result_death_AA_by_gbd_region[6,], c(0.5, 0.025, 0.975), na.rm = TRUE),
  "Overall" = quantile(result_death_AA_by_gbd_region[7,], c(0.5, 0.025, 0.975), na.rm = TRUE)
)

colnames(AA_result) <- c("Median", "Lower", "Upper")
AA_result












saveRDS(AMR_result, "outputs/pipelines/AMR_result.rds")
saveRDS(AA_result, "outputs/pipelines/AA_result.rds")

AA_result
AMR_result/AA_result






## Plot settings ----------------------------------------------------------------

AMR_result <- readRDS("outputs/pipelines/AMR_result.rds")
AA_result <- readRDS("outputs/pipelines/AA_result.rds")

pdf_width <- 8.6
pdf_height <- 4.2

mar_cat <- c(2.3, 0.2, 1.2, 0.1)
mar_plot <- c(2.3, 0.3, 1.2, 0.2)
mar_text <- c(2.3, 0.1, 1.2, 0.1)

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

category_y <- rev(seq_along(category_levels))
divider_y <- category_y[1] - 0.5

ylim_panel <- c(0.5, length(category_levels) + 1.0)
text_title_y <- length(category_levels) + 0.8

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


## Plot -------------------------------------------------------------------------

# pdf(
#   "figures/supplementary/mortality_to_incidence/(Main 1B) AMR and AA deaths.pdf",
#   width = 6,
#   height = 2
# )

layout(
  matrix(
    c(0, 0, 0, 0, 0,
      1, 2, 3, 4, 5), nrow = 2, byrow = TRUE),
  widths = c(1.5, 0.75, 0.85, 0.75, 0.85), heights = c(0.1,1)
)

par(oma = panel_oma)


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
  x0 = rep(xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(xlim_log[2], length(divider_y)),
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

mtext(
  "Number of deaths",
  side = 1,
  line = 1.1,
  cex = cex.axis_title
)

mtext(
  "Deaths attributable to\nantimicrobial resistance",
  side = 3,
  line = 0.1,
  cex = cex_title,
  font = 2
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

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
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
  x0 = rep(xlim_log[1], length(divider_y)),
  y0 = divider_y,
  x1 = rep(xlim_log[2], length(divider_y)),
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

mtext(
  "Number of deaths",
  side = 1,
  line = 1.1,
  cex = cex.axis_title
)

mtext(
  "Deaths attributable to\nlack of antibiotic access",
  side = 3,
  line = 0.1,
  cex = cex_title,
  font = 2
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

segments(
  x0 = rep(0, length(divider_y)),
  y0 = divider_y,
  x1 = rep(1, length(divider_y)),
  y1 = divider_y,
  col = "grey90",
  lwd = lwd_divider
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









