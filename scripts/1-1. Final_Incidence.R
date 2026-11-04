# _____________________________________________ ----------
# 1. Generating Principal components using UN HNP index -------------------

# 78 countries, GDP per capita <20000

rm(list = ls())
gc()

# generating the folders for reproducibility.
output_dirs <- c(
  "outputs/pipelines",
  "outputs/pipelines/suppl_table",
  "outputs/supplementary",
  "outputs/sub_results",
  "outputs/tables"
)

invisible(lapply(
  output_dirs,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

requiredPackages <- c("data.table", "dplyr", "tidyr", "tibble", "missMDA", "FactoMineR", "naniar")
lib.path <- .libPaths()

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p, lib = lib.path)
  library(p, character.only = TRUE)
}

options(scipen = 999)
set.seed(123)

# Prepare UNIndex dataset
# Merging datasets
df.1 <- fread("data/UNIndex/1to2.csv", select = c("Series Name", "Country Name", "2001 [YR2001]", "2002 [YR2002]"), col.names = c("Series", "Country", "2001", "2002"))
df.2 <- fread("data/UNIndex/3to5.csv", select = c("Series Name", "Country Name", "2003 [YR2003]", "2004 [YR2004]", "2005 [YR2005]"), col.names = c("Series", "Country", "2003", "2004", "2005"))
df.3 <- fread("data/UNIndex/6to8.csv", select = c("Series Name", "Country Name", "2006 [YR2006]", "2007 [YR2007]", "2008 [YR2008]"), col.names = c("Series", "Country", "2006", "2007", "2008"))
df.4 <- fread("data/UNIndex/9to11.csv", select = c("Series Name", "Country Name", "2009 [YR2009]", "2010 [YR2010]", "2011 [YR2011]"), col.names = c("Series", "Country", "2009", "2010", "2011"))
df.5 <- fread("data/UNIndex/12to13.csv", select = c("Series Name", "Country Name", "2012 [YR2012]", "2013 [YR2013]"), col.names = c("Series", "Country", "2012", "2013"))
df.6 <- fread("data/UNIndex/14to16.csv", select = c("Series Name", "Country Name", "2014 [YR2014]", "2015 [YR2015]", "2016 [YR2016]"), col.names = c("Series", "Country", "2014", "2015", "2016"))
df.7 <- fread("data/UNIndex/17to19.csv", select = c("Series Name", "Country Name", "2017 [YR2017]", "2018 [YR2018]", "2019 [YR2019]"), col.names = c("Series", "Country", "2017", "2018", "2019"))
df.8 <- fread("data/UNIndex/20to22.csv", select = c("Series Name", "Country Name", "2020 [YR2020]", "2021 [YR2021]", "2022 [YR2022]"), col.names = c("Series", "Country", "2020", "2021", "2022"))
df.9 <- fread("data/UNIndex/23.csv", select = c("Series Name", "Country Name", "2023 [YR2023]"), col.names = c("Series", "Country", "2023"))

df_list <- list(df.1, df.2, df.3, df.4, df.5, df.6, df.7, df.8, df.9)
df_list <- lapply(df_list, function(df) df[1:(.N - 5)])

first_two_cols <- lapply(df_list, function(df) df[, .(Series, Country)])
sapply(2:9, function(i) all.equal(first_two_cols[[1]], first_two_cols[[i]])) # Diagnostic only

df.combined <- as_tibble(df_list[[1]])

for (i in 2:length(df_list)) {
  df.combined <- full_join(df.combined, as_tibble(df_list[[i]]), by = c("Series", "Country"))
}

dim(df.combined)
colnames(df.combined)

df.long <- df.combined %>%
  pivot_longer(cols = -c(Series, Country), names_to = "Year", values_to = "Value") %>%
  mutate(Series = trimws(Series)) %>%
  filter(!is.na(Series), Series != "")

df.ready <- df.long %>% pivot_wider(names_from = Series, values_from = Value)
dim(df.ready)
unique(df.ready$Country)

non_countries <- c(
  "Africa Eastern and Southern", "Africa Western and Central", "Arab World",
  "Caribbean small states", "Central Europe and the Baltics", "East Asia & Pacific",
  "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD countries)",
  "Early-demographic dividend", "Euro area", "Europe & Central Asia",
  "Europe & Central Asia (excluding high income)", "Europe & Central Asia (IDA & IBRD countries)",
  "European Union", "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)", "High income", "IBRD only",
  "IDA & IBRD total", "IDA blend", "IDA only", "IDA total",
  "Late-demographic dividend", "Latin America & Caribbean",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Least developed countries: UN classification", "Low & middle income", "Low income",
  "Lower middle income", "Middle East & North Africa",
  "Middle East & North Africa (excluding high income)",
  "Middle East & North Africa (IDA & IBRD countries)", "Middle income",
  "North America", "Not classified", "OECD members", "Other small states",
  "Pacific island small states", "Post-demographic dividend", "Pre-demographic dividend",
  "Small states", "South Asia", "South Asia (IDA & IBRD)", "Sub-Saharan Africa",
  "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)",
  "Upper middle income", "World"
)

df.ready <- df.ready %>% filter(!Country %in% non_countries)
df.ready$Year <- as.numeric(df.ready$Year)

df.ready <- df.ready %>%
  filter(Year > 2010) # PERCH study started 2010, and CHAMPS started 2016


## Income classification ---------------------------------------------------

Income <- read.csv("data/metadata/Income_level.csv")
colnames(Income)[-1] <- gsub("^X", "", colnames(Income)[-1])

Income <- Income %>%
  pivot_longer(cols = 2:24, names_to = "Year", values_to = "level")

country_mapping <- c(
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "T\x9frkiye" = "Turkiye",
  "Korea, Dem. Rep." = "Korea, Dem. People's Rep."
)

Income <- Income %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))

colnames(Income)[3] <- "Income"
Income$Year <- as.numeric(Income$Year)

a <- unique(df.ready$Country)
b <- unique(Income$Country)
setdiff(a, b)

df.ready2 <- df.ready %>%
  inner_join(Income, by = c("Country", "Year")) %>%
  filter(Income %in% c("L", "LM", "UM", "R"))

df.ready2[df.ready2$Income == "R", ]$Income <- "H"

a1 <- as.data.frame(with(df.ready2, table(Country, Year)))

b1 <- a1 %>%
  group_by(Country) %>%
  summarise(Country_sum = mean(Freq), .groups = "drop")

c1 <- b1[b1$Country_sum > 0.3, ]$Country

df.ready2 <- df.ready2 %>%
  filter(Country %in% c1)

with(df.ready2, table(Country, Year))
df.ready2

Income_dist <- df.ready2 %>%
  dplyr::select(Country, Year, Income)

income_dist <- pivot_wider(Income_dist, names_from = Year, values_from = Income)

first5_years <- as.character(2011:2015)
last5_years <- as.character(2019:2023)

income_dist_summary <- income_dist %>%
  rowwise() %>%
  mutate(
    First5 = {
      first_vals <- c_across(all_of(first5_years))
      tab <- table(first_vals)
      if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
    },
    Last5 = {
      last_vals <- c_across(all_of(last5_years))
      tab <- table(last_vals)
      if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
    }
  ) %>%
  ungroup()

income_dist_summary[which_na(income_dist_summary$Last5), ]

income_dist_summary[income_dist_summary$Country == "St. Kitts and Nevis", ]$Last5 <- "H"

df.ready <- df.ready %>%
  inner_join(income_dist_summary[, c("Country", "Last5")], by = "Country")

rm(income_dist, income_dist_summary, first5_years, last5_years)

df.ready <- df.ready %>%
  mutate(Income = Last5) %>%
  dplyr::select(-Last5)

with(df.ready, table(Country, Income))


## Country exclusions ------------------------------------------------------

exclude_countries_small_island <- c(
  "Cabo Verde", "Dominica", "Grenada", "Kiribati", "Maldives",
  "Marshall Islands", "Micronesia, Fed. Sts.", "Palau", "Samoa",
  "Seychelles", "St. Kitts and Nevis", "St. Lucia",
  "St. Vincent and the Grenadines", "Tonga", "Tuvalu", "Vanuatu"
)

exclude_countries_war_catastrophic_structural_shock <- c(
  "Kosovo", "Libya", "Somalia", "Syrian Arab Republic",
  "West Bank and Gaza", "Lebanon", "Haiti", "Sudan",
  "Yemen, Rep.", "Ukraine"
)

exclude_countries_last_3yr_high_income <- c(
  "Chile", "Croatia", "Hungary", "Latvia", "Lithuania",
  "Panama", "Poland", "Romania", "Seychelles", "Uruguay"
)

exclude_countries <- c(
  exclude_countries_small_island,
  exclude_countries_war_catastrophic_structural_shock,
  exclude_countries_last_3yr_high_income
)

df.ready <- df.ready %>%
  filter(!Country %in% exclude_countries)


## GDP ---------------------------------------------------------------------

GDP.PPP <- read.csv("data/metadata/GDP_PPP.csv")
colnames(GDP.PPP)[-1] <- gsub("^X", "", colnames(GDP.PPP)[-1])

GDP.PPP <- GDP.PPP %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP")

GDP.PPP$Year <- as.numeric(GDP.PPP$Year)

a <- unique(GDP.PPP$Country)
b <- unique(df.ready$Country)
setdiff(b, a) # American Samoa, Cuba

df.ready.gdp <- df.ready %>%
  inner_join(GDP.PPP, by = c("Country", "Year"))

unique(df.ready.gdp[!complete.cases(df.ready.gdp), ]$Country)

df.ready.gdp <- df.ready.gdp[!is.na(df.ready.gdp$GDP), ]

# Exclude all years for countries that exceeded GDP 20,000 in any included year
countries_over_GDP <- df.ready.gdp %>%
  filter(GDP > 20000) %>%
  pull(Country) %>%
  unique()

df.ready.gdp <- df.ready.gdp %>%
  filter(!Country %in% countries_over_GDP)


## GBD region ---------------------------------------------------------------

gbd_region <- read.csv("data/metadata/gbd_region.csv")

a <- unique(gbd_region$Country)
b <- unique(df.ready.gdp$Country)

setdiff(a, b)
setdiff(b, a)

country_mapping <- c(
  "Arab Republic of Egypt" = "Egypt, Arab Rep.",
  "Islamic Republic of Iran" = "Iran, Islamic Rep.",
  "Turkey" = "Turkiye",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Vietnam" = "Viet Nam",
  "Cape Verde" = "Cabo Verde",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "The Gambia" = "Gambia, The",
  "Congo" = "Congo, Rep.",
  "Democratic Republic of Congo" = "Congo, Dem. Rep.",
  "Federated States of Micronesia" = "Micronesia, Fed. Sts.",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Lucia" = "St. Lucia",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Republic of Yemen" = "Yemen, Rep."
)

gbd_region <- gbd_region %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))

gbd_region

df.ready.gdp <- df.ready.gdp %>%
  inner_join(gbd_region, by = "Country") %>%
  filter(gbd_region %in% c(
    "Sub-Saharan Africa",
    "South Asia",
    "North Africa and Middle East",
    "Southeast Asia, East Asia, and Oceania",
    "Latin America and Caribbean",
    "Central Europe, Eastern Europe, and Central Asia"
  ))


## PCA data preparation -----------------------------------------------------

meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region")

df.pca <- df.ready.gdp %>%
  mutate(across(-all_of(meta_cols), ~ na_if(., ".."))) %>%
  mutate(across(-all_of(meta_cols), ~ as.numeric(.)))

par(mfrow = c(1, 1))
hist(round(colSums(is.na(df.pca)) / nrow(df.pca), digits = 3))

na_prop <- colSums(is.na(df.pca)) / nrow(df.pca)
df.pca.ready <- df.pca[, na_prop <= 0.35]

dim(df.pca.ready)
colnames(df.pca.ready)

df.numeric <- df.pca.ready %>%
  dplyr::select(-Country, -Year, -GDP, -Income, -gbd_region)

df.numeric <- df.numeric %>%
  dplyr::select(c(
    "Adolescent fertility rate (births per 1,000 women ages 15-19)",
    "Antiretroviral therapy coverage (% of people living with HIV)",
    "Birth rate, crude (per 1,000 people)",
    "Death rate, crude (per 1,000 people)",
    "Fertility rate, total (births per woman)",
    "GNI per capita, Atlas method (current US$)",
    "Immunization, BCG (% of one-year-old children)",
    "Immunization, DPT (% of children ages 12-23 months)",
    "Immunization, HepB3 (% of one-year-old children)",
    "Immunization, Hib3 (% of children ages 12-23 months)",
    "Immunization, measles (% of children ages 12-23 months)",
    "Immunization, Pol3 (% of one-year-old children)",
    "Incidence of HIV, ages 15-24 (per 1,000 uninfected population ages 15-24)",
    "Incidence of HIV, ages 15-49 (per 1,000 uninfected population ages 15-49)",
    "Incidence of HIV, ages 50+ (per 1,000 uninfected population ages 50+)",
    "Incidence of HIV, all (per 1,000 uninfected population)",
    "Incidence of malaria (per 1,000 population at risk)",
    "Incidence of tuberculosis (per 100,000 people)",
    "Labor force, female (% of total labor force)",
    "Labor force, total",
    "Life expectancy at birth, female (years)",
    "Life expectancy at birth, male (years)",
    "Life expectancy at birth, total (years)",
    "Mortality rate, adult, female (per 1,000 female adults)",
    "Mortality rate, adult, male (per 1,000 male adults)",
    "Mortality rate, infant (per 1,000 live births)",
    "Mortality rate, infant, female (per 1,000 live births)",
    "Mortality rate, infant, male (per 1,000 live births)",
    "Mortality rate, neonatal (per 1,000 live births)",
    "Mortality rate, under-5 (per 1,000)",
    "Mortality rate, under-5, female (per 1,000)",
    "Mortality rate, under-5, male (per 1,000)",
    "Net migration",
    "People practicing open defecation (% of population)",
    "People practicing open defecation, rural (% of rural population)",
    "People practicing open defecation, urban (% of urban population)",
    "People using at least basic drinking water services (% of population)",
    "People using at least basic drinking water services, rural (% of rural population)",
    "People using at least basic drinking water services, urban (% of urban population)",
    "People using at least basic sanitation services (% of population)",
    "People using at least basic sanitation services, rural (% of rural population)",
    "People using at least basic sanitation services, urban  (% of urban population)",
    "Population ages 00-04, female (% of female population)",
    "Population ages 00-04, male (% of male population)",
    "Population ages 0-14 (% of total population)",
    "Population ages 0-14, female (% of female population)",
    "Population ages 0-14, male (% of male population)",
    "Population ages 05-09, female (% of female population)",
    "Population ages 05-09, male (% of male population)",
    "Population ages 10-14, female (% of female population)",
    "Population ages 10-14, male (% of male population)",
    "Population ages 15-19, female (% of female population)",
    "Population ages 15-19, male (% of male population)",
    "Population ages 20-24, female (% of female population)",
    "Population ages 20-24, male (% of male population)",
    "Population ages 25-29, female (% of female population)",
    "Population ages 25-29, male (% of male population)",
    "Population ages 30-34, female (% of female population)",
    "Population ages 30-34, male (% of male population)",
    "Population ages 35-39, female (% of female population)",
    "Population ages 35-39, male (% of male population)",
    "Population ages 40-44, female (% of female population)",
    "Population ages 40-44, male (% of male population)",
    "Population ages 45-49, female (% of female population)",
    "Population ages 45-49, male (% of male population)",
    "Population ages 50-54, female (% of female population)",
    "Population ages 50-54, male (% of male population)",
    "Population ages 55-59, female (% of female population)",
    "Population ages 55-59, male (% of male population)",
    "Population ages 60-64, female (% of female population)",
    "Population ages 60-64, male (% of male population)",
    "Population ages 65 and above (% of total population)",
    "Population ages 65 and above, female (% of female population)",
    "Probability of dying among adolescents ages 10-14 years, female (per 1,000)",
    "Probability of dying among adolescents ages 10-14 years, male (per 1,000)",
    "Probability of dying among adolescents ages 10-19 years (per 1,000)",
    "Probability of dying among adolescents ages 10-19 years, female (per 1,000)",
    "Probability of dying among adolescents ages 10-19 years, male (per 1,000)",
    "Probability of dying among adolescents ages 15-19 years (per 1,000)",
    "Probability of dying among adolescents ages 15-19 years, female (per 1,000)",
    "Probability of dying among adolescents ages 15-19 years, male (per 1,000)",
    "Probability of dying among children ages 5-9 years (per 1,000)"
  ))

colnames(df.numeric) # 82 variables


## Multiple imputation and imputation-specific PCA ---------------------------

set.seed(123)

# Select the PCA reconstruction rank used for MIPCA independently from the
# two principal components retained in the downstream extrapolation models.
ncp.est <- estim_ncpPCA(
  df.numeric,
  ncp.min = 1,
  ncp.max = 10,
  method = "Regularized",
  scale = TRUE,
  method.cv = "gcv",
  verbose = TRUE
)

ncp_imp <- ncp.est$ncp
print(ncp.est)
cat("MIPCA reconstruction rank:", ncp_imp, "\n")

# Five Bayesian PCA imputations. Each completed dataset is analyzed separately.
mi.pca <- MIPCA(
  df.numeric,
  ncp = ncp_imp,
  scale = TRUE,
  method = "Regularized",
  nboot = 5,
  method.mi = "Bayes",
  Lstart = 1000,
  L = 100,
  verbose = TRUE
)

imputed.list <- mi.pca$res.MI

length(imputed.list)
sapply(imputed.list, nrow)
sapply(imputed.list, ncol)

stopifnot(length(imputed.list) == 5)
stopifnot(all(sapply(imputed.list, nrow) == nrow(df.numeric)))
stopifnot(all(sapply(imputed.list, ncol) == ncol(df.numeric)))

# Run PCA separately within each imputed dataset.
# These original imputation-specific PCA solutions are used downstream.
pca.results <- lapply(imputed.list, function(dat) {
  prcomp(dat, center = TRUE, scale. = TRUE)
})

n_pc_analysis <- 2

meta_pca <- df.pca.ready[, c("Country", "Year", "GDP", "Income", "gbd_region")] %>%
  mutate(.row = row_number())

df.pc.mi.list <- vector("list", length(pca.results))

for (m in seq_along(pca.results)) {
  pc_scores_m <- pca.results[[m]]$x[, 1:n_pc_analysis, drop = FALSE]
  colnames(pc_scores_m) <- c("PC1", "PC2")
  
  df.pc.mi.list[[m]] <- bind_cols(
    tibble(.imp_pc = rep(m, nrow(meta_pca))),
    meta_pca,
    as_tibble(pc_scores_m)
  )
}

df.pc.mi <- bind_rows(df.pc.mi.list)

stopifnot(nrow(df.pc.mi) == nrow(meta_pca) * length(pca.results))
stopifnot(all(table(df.pc.mi$.imp_pc) == nrow(meta_pca)))

# Save the imputation and PCA objects for reproducibility.
saveRDS(mi.pca, "outputs/pipelines/mipca_incidence_m5_bayes.rds")
saveRDS(pca.results, "outputs/pipelines/pca_models_incidence_mi.rds")


## Data exploration ---------------------------------------------------------

# The first imputation is used only for descriptive plots.
# No mean PC score is created or used in the analysis.
df.pc.plot <- df.pc.mi %>%
  filter(.imp_pc == 1) %>%
  dplyr::select(-.imp_pc, -.row)

df.exploring <- df.pca.ready %>%
  left_join(
    df.pc.plot %>% dplyr::select(Country, Year, PC1, PC2),
    by = c("Country", "Year")
  )

dir.create("figures/supplementary", recursive = TRUE, showWarnings = FALSE)

pdf("figures/supplementary/Distribution of UNHNP by year.pdf", width = 12, height = 8)

par(mfrow = c(6, 4))
par(mar = c(2.5, 2.5, 1.5, 0.2))

df.exploring$Country <- as.factor(df.exploring$Country)
cols_to_plot <- colnames(df.numeric)

for (col in cols_to_plot) {
  plot(
    x = df.exploring$Year,
    y = df.exploring[[col]],
    col = df.exploring$Country,
    pch = 19,
    xlab = "",
    ylab = "",
    main = col,
    cex.main = 0.65
  )
}

dev.off()

unique(df.pc.plot$Country)
head(df.pc.plot)
str(df.pc.plot)

par(mfrow = c(1, 1))
hist(df.pc.plot$PC1)
hist(df.pc.plot$PC2)

years <- sort(unique(df.pc.plot$Year))
colors <- rainbow(length(years))
year_colors <- colors[as.numeric(factor(df.pc.plot$Year, levels = years))]

pdf("figures/supplementary/Distribution of PC by year_before exclusion.pdf", width = 12, height = 12)

par(mfrow = c(2, 2))

plot(df.pc.plot$PC1, df.pc.plot$PC2, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

plot(df.pc.plot$PC1, df.pc.plot$Year, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

plot(df.pc.plot$PC2, df.pc.plot$Year, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

dev.off()


## Imputation-specific loadings and GPA summary -----------------------------

# Raw loadings from the five PCA analyses.
loading_mi <- bind_rows(lapply(seq_along(pca.results), function(m) {
  loading_m <- pca.results[[m]]$rotation[, 1:2, drop = FALSE]
  
  tibble(
    .imp_pc = m,
    Variable = rownames(loading_m),
    Loading_PC1 = loading_m[, 1],
    Loading_PC2 = loading_m[, 2]
  )
}))

write.csv(
  loading_mi,
  "outputs/supplementary/result_incidence_2023_PC_loadings_by_imputation.csv",
  row.names = FALSE
)

# Generalized Procrustes analysis is used only for descriptive combination
# of the loading configurations. GPA-transformed scores are NOT used downstream.
loading_gpa_input <- as.data.frame(
  do.call(cbind, lapply(pca.results, function(x) x$rotation[, 1:2, drop = FALSE]))
)

colnames(loading_gpa_input) <- unlist(
  lapply(seq_along(pca.results), function(m) paste0("Imp", m, "_PC", 1:2)))

gpa.loading <- FactoMineR::GPA(
  loading_gpa_input,
  group = rep(2, length(pca.results)),
  name.group = paste0("Imp", seq_along(pca.results)),
  scale = FALSE,
  graph = FALSE
)

# Orient the GPA consensus toward the first PCA solution only for presentation.
gpa_consensus <- as.matrix(gpa.loading$consensus)
reference_loading <- pca.results[[1]]$rotation[, 1:2, drop = FALSE]
reference_loading_centered <- scale(reference_loading, center = TRUE, scale = FALSE)

sv_gpa <- svd(crossprod(gpa_consensus, reference_loading_centered))
R_gpa <- sv_gpa$u %*% t(sv_gpa$v)

gpa_consensus <- gpa_consensus %*% R_gpa
gpa_Xfin <- array(NA_real_, dim = dim(gpa.loading$Xfin))

for (m in seq_len(dim(gpa.loading$Xfin)[3])) {
  gpa_Xfin[, , m] <- gpa.loading$Xfin[, , m] %*% R_gpa
}

gpa_loading_summary <- tibble(
  Variable = rownames(reference_loading),
  GPA_Loading_PC1 = gpa_consensus[, 1],
  GPA_Loading_PC2 = gpa_consensus[, 2],
  Loading_PC1_SD = apply(gpa_Xfin[, 1, , drop = FALSE], 1, sd),
  Loading_PC1_min = apply(gpa_Xfin[, 1, , drop = FALSE], 1, min),
  Loading_PC1_max = apply(gpa_Xfin[, 1, , drop = FALSE], 1, max),
  Loading_PC2_SD = apply(gpa_Xfin[, 2, , drop = FALSE], 1, sd),
  Loading_PC2_min = apply(gpa_Xfin[, 2, , drop = FALSE], 1, min),
  Loading_PC2_max = apply(gpa_Xfin[, 2, , drop = FALSE], 1, max)
)

result_PC1 <- gpa_loading_summary %>%
  transmute(
    Variable,
    Loading_PC1 = round(GPA_Loading_PC1, 3),
    SD_PC1 = round(Loading_PC1_SD, 3),
    Min_PC1 = round(Loading_PC1_min, 3),
    Max_PC1 = round(Loading_PC1_max, 3)
  ) %>%
  arrange(desc(abs(Loading_PC1)))

result_PC2 <- gpa_loading_summary %>%
  transmute(
    Variable,
    Loading_PC2 = round(GPA_Loading_PC2, 3),
    SD_PC2 = round(Loading_PC2_SD, 3),
    Min_PC2 = round(Loading_PC2_min, 3),
    Max_PC2 = round(Loading_PC2_max, 3)
  ) %>%
  arrange(desc(abs(Loading_PC2)))

write.csv(result_PC1, "outputs/supplementary/result_incidence_2023_PC1.csv", row.names = FALSE)
write.csv(result_PC2, "outputs/supplementary/result_incidence_2023_PC2.csv", row.names = FALSE)

# PCA subspace stability diagnostic relative to imputation 1.
# Principal angles are invariant to sign changes and rotations within the retained 2-D subspace.
pca_subspace_stability <- bind_rows(lapply(seq_along(pca.results), function(m) {
  loading_m <- pca.results[[m]]$rotation[, 1:2, drop = FALSE]
  singular_values_m <- svd(crossprod(reference_loading, loading_m), nu = 0, nv = 0)$d
  singular_values_m <- pmin(1, pmax(0, singular_values_m))
  
  tibble(
    .imp_pc = m,
    principal_angle_1_deg = acos(singular_values_m[1]) * 180 / pi,
    principal_angle_2_deg = acos(singular_values_m[2]) * 180 / pi
  )
}))

print(pca_subspace_stability)

write.csv(
  pca_subspace_stability,
  "outputs/supplementary/result_incidence_2023_PC_subspace_stability.csv",
  row.names = FALSE
)


## Exclusion of PC outliers -------------------------------------------------

exclude_countries_outboundary <- c(
  "Angola", "Bosnia and Herzegovina", "Comoros",
  "Congo, Rep.", "Nicaragua", "Solomon Islands"
)

df.pc.mi <- df.pc.mi %>%
  filter(!Country %in% exclude_countries_outboundary)

# First imputation is retained only as a plotting reference.
df.pc.plot <- df.pc.mi %>%
  filter(.imp_pc == 1) %>%
  dplyr::select(-.imp_pc, -.row)

# Recalculate plotting colors after filtering
years <- sort(unique(df.pc.plot$Year))
colors <- rainbow(length(years))
year_colors <- colors[as.numeric(factor(df.pc.plot$Year, levels = years))]

length(unique(df.pc.mi$Country))

# Excluding outlying country-years
# Country                  2018 2019 2020 2021 2022 2023
# Angola                    0    0    0    0    0    1
# Bosnia and Herzegovina    0    1    1    1    1    1
# Comoros                   0    0    1    1    1    0
# Congo, Rep.               0    0    0    0    1    1
# Nicaragua                 0    0    0    1    1    1
# Solomon Islands           0    0    0    0    1    0

pdf("figures/supplementary/Distribution of PC by year_after exclusion.pdf", width = 12, height = 12)

par(mfrow = c(2, 2))

plot(df.pc.plot$PC1, df.pc.plot$PC2, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

plot(df.pc.plot$PC1, df.pc.plot$Year, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

plot(df.pc.plot$PC2, df.pc.plot$Year, col = year_colors)
legend("topright", legend = years, col = colors, pch = 19, cex = 0.6, title = "Year")

dev.off()

unique(df.pc.plot$Country)
table(df.pc.plot$Country)

plot(df.pc.plot$PC1, df.pc.plot$PC2, col = year_colors)

plot(df.pc.plot$Year, df.pc.plot$PC1, col = "#0072B2")
df.pc.plot[which.min(df.pc.plot$PC1), ]

plot(df.pc.plot$Year, df.pc.plot$PC2, col = "#0072B2")
df.pc.plot[which.min(df.pc.plot$PC2), ]


## Malaria endemicity -------------------------------------------------------

mal.endemic.df <- read.csv("data/metadata/WHO2025_malarial_endemic.csv")

country_mapping <- c(
  "Comoros " = "Comoros",
  "Congo" = "Congo, Rep.",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Gambia" = "Gambia, The",
  "Niger " = "Niger",
  "United Republic of Tanzania" = "Tanzania",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Venezuela (Bolivarian Republic of)" = "Venezuela, RB",
  "Democratic People\xd5s Republic of Korea" = "Korea, Dem. People's Rep.",
  "Lao People\xd5s Democratic Republic " = "Lao PDR",
  "Republic of Korea" = "Korea, Rep.",
  "Yemen" = "Yemen, Rep."
)

mal.endemic.df <- mal.endemic.df %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))

a <- unique(mal.endemic.df$Country)
b <- unique(df.pc.mi$Country)

setdiff(a, b)
setdiff(b, a)

df.pc.mi <- df.pc.mi %>%
  left_join(mal.endemic.df, by = "Country") %>%
  mutate(Endemic = ifelse(is.na(Endemic), 0, Endemic))


## Prepare proportion of malaria infection --------------------------------

all.inf <- read.csv("data/metadata/All_inf3.csv")

head(all.inf)
unique(all.inf$cause)

all.inf[all.inf$cause == "HIV/AIDS and sexually transmitted infections", ]$cause <- "HIV_STI"
all.inf[all.inf$cause == "Enteric infections", ]$cause <- "Enteric"
all.inf[all.inf$cause == "Respiratory infections and tuberculosis", ]$cause <- "RI_TB"
all.inf[all.inf$cause == "Other infectious diseases", ]$cause <- "Others"
all.inf[all.inf$cause == "Neglected tropical diseases and malaria", ]$cause <- "Neg_malaria"
all.inf[all.inf$cause == "Malaria", ]$cause <- "Malaria"

unique(all.inf$cause)

all.inf <- all.inf %>%
  pivot_wider(names_from = cause, values_from = c(val, upper, lower))

all.inf$overall.inc <- all.inf$val_HIV_STI +
  all.inf$val_Enteric +
  all.inf$val_RI_TB +
  all.inf$val_Others +
  all.inf$val_Neg_malaria

all.inf$mal.prop <- all.inf$val_Malaria / all.inf$overall.inc

hist(all.inf$mal.prop)
hist(all.inf$mal.prop, xlim = c(0, 0.2), breaks = seq(0, max(all.inf$mal.prop, 0.2), length.out = 21))

all.inf <- all.inf %>%
  dplyr::select(location, year, age, mal.prop)

write.csv(all.inf, "outputs/pipelines/malaria_proportion.csv", row.names = FALSE)


## Supplemental variance-explained plot ------------------------------------

# Variance explained is summarized descriptively across the five separate PCA solutions.
pve_matrix <- do.call(cbind, lapply(pca.results, function(x) {
  eigvals_m <- x$sdev^2
  eigvals_m / sum(eigvals_m)
}))

n_pve_plot <- min(10, nrow(pve_matrix))
pve_plot <- pve_matrix[1:n_pve_plot, , drop = FALSE]

pve_summary <- tibble(
  PC = seq_len(nrow(pve_matrix)),
  Mean = rowMeans(pve_matrix),
  SD = apply(pve_matrix, 1, sd),
  Min = apply(pve_matrix, 1, min),
  Max = apply(pve_matrix, 1, max)
)

write.csv(
  pve_summary,
  "outputs/supplementary/result_incidence_2023_PC_variance_explained.csv",
  row.names = FALSE
)

cat(
  "PC1 variance explained: mean =", mean(pve_matrix[1, ]),
  "; range =", range(pve_matrix[1, ]), "\n"
)

cat(
  "PC2 variance explained: mean =", mean(pve_matrix[2, ]),
  "; range =", range(pve_matrix[2, ]), "\n"
)

cat(
  "Cumulative PC1-PC2 variance explained: mean =",
  mean(colSums(pve_matrix[1:2, , drop = FALSE])),
  "; range =", range(colSums(pve_matrix[1:2, , drop = FALSE])), "\n"
)

pdf("figures/supplementary/proportion of variance explained by principal components.pdf", width = 7.5, height = 4)

par(mar = c(2.5, 2.5, 1, 1))
par(mgp = c(3, 0.3, 0))
par(tck = -0.015)
par(lwd = 1)

pcs <- 1:n_pve_plot
propvar <- rowMeans(pve_plot)
propvar_min <- apply(pve_plot, 1, min)
propvar_max <- apply(pve_plot, 1, max)

xmin <- 1
xmax <- n_pve_plot
ymin <- 0
ymax <- max(0.5, ceiling(max(propvar_max) * 20) / 20)

plot(
  pcs,
  propvar,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(xmin, xmax),
  ylim = c(ymin, ymax),
  axes = FALSE,
  ann = FALSE,
  main = ""
)

axis(1, at = pcs, labels = FALSE, pos = ymin)

text(
  x = pcs,
  y = ymin - 0.05 * (ymax - ymin),
  labels = pcs,
  xpd = TRUE,
  cex = 0.65
)

axis(2, at = seq(0, ymax, length.out = 6), labels = FALSE, pos = xmin)

text(
  y = seq(0, ymax, length.out = 6),
  x = xmin - 0.02 * (xmax - xmin),
  labels = round(seq(0, ymax, length.out = 6), 2),
  srt = 0,
  adj = 1,
  xpd = TRUE,
  cex = 0.65
)

segments(pcs, propvar_min, pcs, propvar_max, col = "grey60", lwd = 1)
lines(pcs, propvar, col = "#0072B2", lwd = 1.5)
points(pcs, propvar, pch = 16, cex = 0.7, col = "#0072B2")

mtext("Principal components", side = 1, line = 1.5, cex = 0.8)
mtext("Proportion of variance explained", side = 2, line = 1.5, cex = 0.8)

dev.off()


## Population and final output ----------------------------------------------

# Keep all five PC imputations. The .imp_pc identifier will be crossed later
# with the five DHS imputations in Sections 7-10.
UNI <- df.pc.mi %>%
  dplyr::select(-.row)

Population <- read.csv("data/metadata/Under_5.csv") %>%
  dplyr::select(Entity, Year, Population)

colnames(Population) <- c("Country", "Year", "Population")

head(Population)
head(UNI)

a <- unique(UNI$Country)
b <- unique(Population$Country)

setdiff(a, b)
setdiff(b, a)

country_mapping <- c(
  "Democratic Republic of Congo" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "Egypt" = "Egypt, Arab Rep.",
  "Gambia" = "Gambia, The",
  "Iran" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Laos" = "Lao PDR",
  "Russia" = "Russian Federation",
  "East Timor" = "Timor-Leste",
  "Turkey" = "Turkiye",
  "Vietnam" = "Viet Nam",
  "Yemen" = "Yemen, Rep."
)

Population <- Population %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))

UNI <- UNI %>%
  inner_join(Population, by = c("Country", "Year")) %>%
  arrange(.imp_pc, Country, Year)

head(UNI)
sum(is.na(UNI))
table(UNI$.imp_pc)

stopifnot(length(unique(UNI$.imp_pc)) == 5)
stopifnot(all(table(UNI$.imp_pc) == table(UNI$.imp_pc)[1]))

write.csv(UNI, "outputs/sub_results/UNI.data_2023_incidence.csv", row.names = FALSE)
saveRDS(UNI, "outputs/pipelines/pc_gdp_2023_incidence.rds")











# _____________________________________________ ----------
# 2. DHS data cleaning ----------------------------------------------------

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
risk_variables_explanation <- c(
  ## Identifiers / survey structure
  "caseid",  # Child unique identifier within DHS survey
  "v000",    # DHS survey ID (Country + survey phase)
  "v001",    # Cluster / PSU identifier
  "v005",    # Sampling weight
  "v006",
  "v007",    # Survey Year
  "v006",    # month of interview
  "v008",    # Century month code (CMC) of interview
  ## Geographic variables
  "v021",    # Primary sampling unit (alternative PSU variable)
  "v022",    # Sample strata for variance estimation
  "v023",    # Stratification variable (available in some surveys)
  "v024",    # Subnational administrative region
  "v025",    # Urban/rural residence
  ## Child demographics
  "b3",      # Child date of birth (CMC)
  "b4",      # Child sex
  "b5",      # Child survival status
  "b8",      # Child age in completed Years
  "b19",     # Child age in months (preferred if available) -> excluded as there are many missing variables
  "bidx",    # Birth index
  ## Main outcomes
  "h22",     # Fever in previous 2 weeks
  "h31",     # Cough in previous 2 weeks
  "h31b",    # Rapid/short breathing
  "h31c",    # Chest-related breathing problem
  ## Maternal education / health literacy
  "v106",    # Highest education level attained
  ## Household socioeconomic status
  "v191",    # Household wealth index (continuous)
  ## Water, sanitation and hygiene (WASH)
  "v113",    # Source of drinking water
  "v115",    # Time to obtain drinking water
  "v116",    # Toilet facility type
  ## Household infrastructure
  "v119",    # Household has electricity
  ## Indoor air pollution
  "v161",    # Type of cooking fuel
  ## Child nutritional status
  "hw70",    # Height-for-age Z-score (stunting) -> excluded as it represents more chornic nutrion status 
  "hw71",    # Weight-for-age Z-score (underweight) -> excluded as it represents chornic + acute nutrion status 
  "hw72",    # Weight-for-height Z-score (wasting) -> only included to avoid redundacy with hw70 and hw71
  ## Malaria / anemia variables
  "hml1",    # Slept under mosquito net last night -> excluded as there are many missing variables
  "v456",    # Maternal anemia level
  ## Health seeking behavior
  "h32y",    # Whether no treatment or advice was sought -> excluded as there are many missing values
  "h32z",    # Whether the child was taken to a medical facility > excluded as there are many missing values
  "h37"      # regarding antibiotics
)


keep_variables <- c(
  "caseid", "v000", "v001", "v005", "v006", "v007", "v008",
  "v024", "v025",
  "b3", "b4", "b5", "b8", "bidx",
  "h22", "h31", "h31b", "h31c",
  "v106", "v190",
  "v113", "v116", "v119", "v161",
  "hw72",
  "h37i", "h37j", "h37n", "h37o", "h37p", "h37a", "h37b", "h37c", "h37d", "h37da", "h37e", "h37aa", "h37ab", "h37f", "h37g", "h37h",
  "ml0", "v460"
)

setdiff(keep_variables,risk_variables_explanation)



# Function to load and filter a dataset
load_and_filter <- function(file) {
  dat <- as.data.table(readRDS(file))
  dat <- dat[, intersect(keep_variables, names(dat)), with = FALSE]
  dat$filename <- tools::file_path_sans_ext(basename(file))
  dat
}


# Load all datasets into a named list: 165 
extract <- lapply(local_files, load_and_filter)
names(extract) <- sapply(local_files, function(x) tools::file_path_sans_ext(basename(x)))

for (i in 1:length(extract)) {
  Year_val <- min(unique(extract[[i]]$v007), na.rm = TRUE)
  file_label <- names(extract)[i]
  cat("i =", i, "filename =", file_label, "Year =", Year_val, "\n")
}

length(extract) #164 (dataset including 2005~2023 years, individual year under 2005 will be removed in country-year stage)
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
sum(lapply(column_names, length)<40) # Check the variables in datasets.
n_vars <- sapply(column_names, length)
sum(n_vars < length(keep_variables))
for (i in which(n_vars < length(keep_variables))) {
  cat("\nDataset index:", i, "\n")
  cat("Number of variables:", n_vars[i], "\n")
  cat("Missing variables:\n")
  print(setdiff(keep_variables, column_names[[i]]))
}

for (i in which(n_vars < length(keep_variables) - 3)) {
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


# Combining the datasets into one dataframe and align the variables with order by A-Z
combined_data <- rbindlist(extract, fill = TRUE, ignore.attr = TRUE)
combined_data <- combined_data %>% dplyr::select(order(names(.))) 

# DHS survey version from filename
combined_data <- combined_data %>% mutate(survey_v = substr(filename, 5,5))

# Country name
Country_name <- c( AF7 = "Afghanistan", AL5 = "Albania", AL7 = "Albania", AM6 = "Armenia", AM7 = "Armenia", AO7 = "Angola", AZ5 = "Azerbaijan", BD5 = "Bangladesh", BD6 = "Bangladesh", BD7 = "Bangladesh", BD8 = "Bangladesh", BF6 = "Burkina Faso", BF8 = "Burkina Faso", BJ5 = "Benin", BJ6 = "Benin", BJ7 = "Benin", BO5 = "Bolivia", CD5 = "Democratic Republic of the Congo", CD6 = "Democratic Republic of the Congo", CG5 = "Republic of the Congo", CG6 = "Republic of the Congo", CI6 = "Cote d'Ivoire", CI8 = "Cote d'Ivoire", CM6 = "Cameroon", CM7 = "Cameroon", CO6 = "Colombia", CO7 = "Colombia", EG5 = "Egypt", EG6 = "Egypt", ET7 = "Ethiopia", ET8 = "Ethiopia", GA6 = "Gabon", GA7 = "Gabon", GH5 = "Ghana", GH7 = "Ghana", GH8 = "Ghana", GM6 = "Gambia", GM8 = "Gambia", GN6 = "Guinea", GN7 = "Guinea", GY5 = "Guyana", HN5 = "Honduras", HN6 = "Honduras", HT5 = "Haiti", HT6 = "Haiti", HT7 = "Haiti", ID5 = "Indonesia", ID6 = "Indonesia", ID7 = "Indonesia", JO5 = "Jordan", JO6 = "Jordan", JO7 = "Jordan", JO8 = "Jordan", KE5 = "Kenya", KE7 = "Kenya", KE8 = "Kenya", KH5 = "Cambodia", KH6 = "Cambodia", KH7 = "Cambodia", KH8 = "Cambodia", KM6 = "Comoros", LS6 = "Lesotho", LS7 = "Lesotho", LS8 = "Lesotho", MD5 = "Madagascar", MD8 = "Madagascar", ML5 = "Mali", ML6 = "Mali", ML7 = "Mali", MM7 = "Myanmar", MR7 = "Mauritania", MV5 = "Maldives", MV7 = "Maldives", MW6 = "Malawi", MW7 = "Malawi", MZ6 = "Mozambique", MZ8 = "Mozambique", NG5 = "Nigeria", NG6 = "Nigeria", NG7 = "Nigeria", NI5 = "Niger", NI6 = "Niger", NP6 = "Nepal", NP7 = "Nepal", NP8 = "Nepal", PE5 = "Peru", PE6 = "Peru", PG7 = "Papua New Guinea", PH5 = "Philippines", PH6 = "Philippines", PH7 = "Philippines", PH8 = "Philippines", PK5 = "Pakistan", PK6 = "Pakistan", PK7 = "Pakistan", RW5 = "Rwanda", RW6 = "Rwanda", RW7 = "Rwanda", RW8 = "Rwanda", SL5 = "Sierra Leone", SL6 = "Sierra Leone", SL7 = "Sierra Leone", SN6 = "Senegal", SN7 = "Senegal", SN8 = "Senegal", ST5 = "Sao Tome and Principe", SZ5 = "Eswatini", TD7 = "Chad", TG6 = "Togo", TJ6 = "Tajikistan", TJ7 = "Tajikistan", TR5 = "Turkey", TR6 = "Turkey", TZ6 = "Tanzania", TZ7 = "Tanzania", TZ8 = "Tanzania", UA5 = "Ukraine", UG5 = "Uganda", UG6 = "Uganda", UG7 = "Uganda", YE6 = "Yemen", ZA7 = "South Africa", ZM5 = "Zambia", ZM6 = "Zambia", ZM7 = "Zambia", ZW5 = "Zimbabwe", ZW6 = "Zimbabwe", ZW7 = "Zimbabwe"
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
length(unique(combined_data$Country)); length(unique(combined_data$v000)) #59 countries, 125 surveys
length(unique(combined_data$Country)); length(unique(combined_data$filename)) #59 countries, 139 dataset
sum(with(combined_data, table(Country, v007))>0) #201 datapoints (Country Year)

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
  rename(
    ch_bth_index = bidx, 
    ch_bth_month = b3, 
    ch_sex = b4, 
    ch_alive = b5, 
    ch_age_yr = b8)
colnames(combined_data)

## v section
combined_data %>% dplyr::select(starts_with("v")) %>% colnames()
v_columns <- combined_data %>% dplyr::select(starts_with("v"))
v_column_tables <- # check the values in variables in v section
  lapply(v_columns, function(column) {table(column, useNA = "always")})
v_column_tables

# mutate the names of variables in v section
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

# reflecting seasonality using calendar month
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



hh_columns <- combined_data %>% dplyr::select(starts_with("hh"))
combined_data[, c("maternal_edu")] <- ## Mutate "I don't know (8, 9)" to NA
  lapply(combined_data[, c("maternal_edu")], function(x) ifelse(x  %in% c(6, 8, 9), NA, x))

# check the duplication
combined_data <- combined_data %>% mutate(ch_id = paste(filename, caseid, ch_bth_index, sep = ""))
any(duplicated(combined_data$ch_id)) # check duplication of child_id: none
unique(any(duplicated(combined_data$ch_id)))
combined_data <- combined_data %>% dplyr::select(-c("caseid", "ch_bth_index"))

# sub-national region
combined_data <- combined_data %>% mutate(sub_region_id = paste(Country, Year, sub_region, sep = "_"))

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

combined_data <- combined_data %>%
  mutate(
    bednet_any_ml0 = case_when(
      ml0 == 0 ~ 0,
      ml0 %in% c(1, 2, 3) ~ 1,
      ml0 == 9 ~ NA_real_,
      TRUE ~ NA_real_),
    
    bednet_treated = case_when(
      ml0 == 0 ~ 0,
      ml0 %in% c(1, 2) ~ 1,
      ml0 == 3 ~ 0,
      ml0 == 9 ~ NA_real_,
      TRUE ~ NA_real_),
    
    bednet_any_v460 = case_when(
      v460 %in% c(1, 2) ~ 1,
      v460 %in% c(0, 3) ~ 0,
      v460 == 9 ~ NA_real_,
      TRUE ~ NA_real_),
    
    bednet_any_combined = coalesce(bednet_any_ml0, bednet_any_v460),
    bednet_treated_combined = coalesce(bednet_treated, bednet_any_v460)
  )

with(combined_data, table(bednet_any_combined, useNA = "ifany"))
with(combined_data, table(bednet_any_combined, Endemic, useNA = "ifany"))
sum(is.na(combined_data$bednet_any_combined))/nrow(combined_data) #0.25
sum(is.na(combined_data$bednet_treated))/nrow(combined_data) #0.31

combined_data <- combined_data %>% filter(!(CountryYear %in% c("Peru_2003", "Peru_2004"))) # excluding country-year before 2005 (these are also systematic 0 for LRTI)

##selecting dataset
analysis_data <- combined_data %>%
  dplyr::select(c("Country", "Year", "gbd_region", "sub_region_id", "cluster", "ch_id", "ch_age_group", "ch_sex", "survey_month", "month_sin", "month_cos", 
                  "sample_weight", "wh_z_score", "rural_urban", "bednet_treated", "Endemic", "maternal_edu", "water", "sanitation", "electricity", 
                  "cooking_fuel", "hh_wealth", "fever", "cough", "rapid_breath", "cpain_rnose", "filename", "survey_v"))

analysis_data$CountryYear <- paste(analysis_data$Country, analysis_data$Year, sep = "_")

length(unique(combined_data$CountryYears)) # 59
length(unique(combined_data$CountryYear)) # 199

# Remove rows where cough or fever is NA
with(analysis_data, table(fever, useNA = 'always'))/nrow(analysis_data) # fever missing 4.7% 
with(analysis_data, table(cough, useNA = 'always'))/nrow(analysis_data) # cough missing 4.1% 
with(analysis_data, table(cough,fever, useNA = 'always'))/nrow(analysis_data) # both missing 3.9%
with(analysis_data, table(CountryYear, fever, useNA = 'always'))
with(analysis_data, table(CountryYear, cough, useNA = 'always'))

length(unique(analysis_data$Country)); with(analysis_data,length(unique(CountryYear))) # 59 countries, 199 country-year
analysis_data <- analysis_data %>% filter(!(CountryYear %in% c("Bolivia_2008", "Colombia_2015", "Colombia_2016", "Ethiopia_2011", "Jordan_2009", "Turkey_2008", "Turkey_2013", "Ukraine_2007"))) # excluding country-year with systemic 0 cases
length(unique(analysis_data$Country)); with(analysis_data,length(unique(CountryYear))) # 56 countries, 191 country-year
with(analysis_data, table(cough,fever, useNA = 'always'))/nrow(analysis_data) # both missing 1.2% 
analysis_data <- analysis_data[!is.na(analysis_data$cough) & !is.na(analysis_data$fever), ]


# Check the distribution of symptoms
with(analysis_data, table(cough, useNA='always')); with(analysis_data, table(fever, useNA='always'))
with(analysis_data, table(rapid_breath, useNA='always')); with(analysis_data, table(cpain_rnose, useNA='always'))
with(analysis_data, table(cough, rapid_breath, useNA='always'))
with(analysis_data, table(cough, cpain_rnose, useNA='always'))
with(analysis_data, table(rapid_breath, cpain_rnose, useNA='always'))

# LRTI variable
analysis_data <- analysis_data %>%
  mutate(LRTI = case_when(
      cough == 1 & rapid_breath == 1 & cpain_rnose %in% c(1, 3) ~ 1L,
      cough == 0 ~ 0L,
      cough == 1 & rapid_breath == 0 ~ 0L,
      cough == 1 & rapid_breath == 1 & !(cpain_rnose %in% c(1, 3)) ~ 0L,
      TRUE ~ 0)) # cough = 1, rapid_breat = NA --> lrti = 0

# LRTI ~ Fever table by survey version
with(analysis_data, table(fever, LRTI))/nrow(analysis_data) # LRTI ~ 4.9%, Fever ~ 21.9% 

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

with(analysis_data, table(fever, LRTI))/nrow(analysis_data) # LRTI ~ 4.9%, nonLRTI-Fever ~ 18.6% 


# excluding country years not contributing to the analysis
with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 56 countries, 191 country-year
with(analysis_data, table(CountryYear, LRTI)) # Peru 2003:2005 did not collect chest involvement information
analysis_data <- analysis_data %>% filter(!(CountryYear %in% c("Benin_2006", "Democratic Republic of the Congo_2007", "Indonesia_2007", "Mali_2006", "Niger_2006", "Peru_2005", "Republic of the Congo_2005"))) # excluding country-year with systemic 0 cases
with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 56 countries, 184 country-year

r <- with(analysis_data,unique(CountryYear))

# add GDP variable
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
  "Yemen, Rep." = "Yemen"
)

GDP.PPP <- GDP.PPP %>% mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[Country], Country))
GDP.PPP$CountryYear <- paste(GDP.PPP$Country, GDP.PPP$Year, sep = "_")

a <- unique(analysis_data$CountryYear)
b <- unique(GDP.PPP$CountryYear)

setdiff(a,b)


analysis_data <- analysis_data %>% inner_join(GDP.PPP, by = c("Country" = "Country", "Year" = "Year", "CountryYear" = "CountryYear"))
unique(analysis_data$GDP)
analysis_data %>% dplyr::select(c(Country, Year, GDP)) %>% distinct()
analysis_data <- analysis_data %>%
  mutate(GDP = ifelse(Country == "Yemen" & Year == 2013 & is.na(GDP), 110.19/95.14*3164, GDP)) # Yemen does not have the value for GDP per capita in 2021 international dollar so I used proxy using current international dollar and US GDP deflation index

with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 56 countries, 184 country-year
t <- with(analysis_data,unique(CountryYear))

setdiff(r,t)

analysis_data <- analysis_data %>%
  dplyr::select(c("Country", "Year", "gbd_region", "sub_region_id", "cluster", "ch_id", "ch_age_group", "ch_sex", "survey_month", "month_sin", "month_cos", "sample_weight", "wh_z_score", "rural_urban", "bednet_treated", "Endemic", "maternal_edu", "water", "sanitation", "electricity", "cooking_fuel", "hh_wealth", "fever", "LRTI", "CountryYear", "GDP", "filename"))



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

colnames(analysis_data)
saveRDS(analysis_data, file = "outputs/pipelines/incidence_analysis_data.rds")





# _____________________________________________ ----------
# 3. Merging GPS data -------------------

rm(list = ls())
gc()

library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(rdhs)


# 1. Load cleaned analysis data
analysis_data <- readRDS("outputs/pipelines/incidence_analysis_data.rds")


# 2. GE folder
ge_folder <- "/Users/chungmanchae/Documents/GitHub/antibiotic_impact/data/DHS_GE/datasets"

local_ge_files <- list.files(
  ge_folder,
  pattern = "\\.zip$|\\.ZIP$",
  recursive = TRUE,
  full.names = TRUE
)

local_ge_lookup <- tibble(
  ge_path = local_ge_files,
  ge_filename = basename(local_ge_files),
  ge_stem = toupper(tools::file_path_sans_ext(basename(local_ge_files)))
)


# 3. Manual KR-GE mapping
# Use NA if no GE data are available.
kr_ge_map <- tribble(
  ~filename,    ~ge_stem,
  "AFKR71FL",  NA_character_,
  "ALKR51FL",  "ALGE51FL",
  "ALKR71FL",  "ALGE71FL",
  "AMKR61FL",  "AMGE61FL",
  "AMKR72FL",  "AMGE71FL",
  "AZKR52FL",  NA_character_,
  "BDKR61FL",  "BDGE61FL",
  "BFKR81FL",  "BFGE81FL",
  "BJKR61FL",  "BJGE61FL",
  "BJKR71FL",  "BJGE71FL",
  "CIKR81FL",  "CIGE81FL",
  "CMKR71FL",  "CMGE71FL",
  "EGKR5AFL",  "EGGE5DFL",
  "EGKR61FL",  "EGGE61FL",
  "ETKR61FL",  "ETGE61FL",
  "ETKR71FL",  "ETGE71FL",
  "GAKR71FL",  "GAGE71FL",
  "GHKR8BFL",  "GHGE8AFL",
  "GNKR62FL",  "GNGE61FL",
  "GNKR71FL",  "GNGE71FL",
  "GYKR5IFL",  "GYGE5JFL",
  "HNKR52FL",  NA_character_,
  "HNKR62FL",  "HNGE61FL",
  "HTKR61FL",  "HTGE61FL",
  "IDKR63FL",  NA_character_,
  "IDKR71FL",  NA_character_,
  "JOKR51FL",  "JOGE52FL",
  "JOKR6CFL",  "JOGE6AFL",
  "JOKR74FL",  "JOGE71FL",
  "JOKR81FL",  "JOGE81FL",
  "KEKR52FL",  "KEGE52FL",
  "KEKR72FL",  "KEGE71FL",
  "KEKR8CFL",  "KEGE8AFL",
  "KHKR61FL",  "KHGE62FL",
  "KHKR73FL",  "KHGE71FL",
  "KHKR82FL",  "KHGE81FL",
  "LSKR71FL",  "LSGE71FL",
  "LSKR81FL",  "LSGE81FL",
  "MLKR6AFL",  "MLGE6BFL",
  "MVKR52FL",  NA_character_,
  "MVKR71FL",  NA_character_,
  "MWKR7AFL",  "MWGE7AFL",
  "MZKR62FL",  "MZGE61FL",
  "MZKR81FL",  "MZGE81FL",
  "NPKR61FL",  "NPGE61FL",
  "NPKR7HFL",  "NPGE7AFL",
  "NPKR82FL",  "NPGE82FL",
  "PEKR5AFL",  "PEGE5BFL",
  "PEKR5IFL",  "PEGE5IFL",
  "PEKR61FL",  NA_character_,
  "PEKR6AFL",  NA_character_,
  "PEKR6IFL",  NA_character_,
  "PHKR52FL",  "PHGE52FL",
  "PHKR61FL",  NA_character_,
  "PHKR71FL",  "PHGE71FL",
  "PHKR82FL",  "PHGE81FL",
  "PKKR53FL",  "PKGE51FL",
  "PKKR61FL",  NA_character_,
  "PKKR71FL",  "PKGE71FL",
  "RWKR81FL",  "RWGE81FL",
  "SNKR7ZFL",  "SNGE7RFL",
  "SNKR8RFL",  "SNGE8RFL",
  "SZKR51FL",  "SZGE53FL",
  "TJKR61FL",  "TJGE61FL",
  "TJKR72FL",  "TJGE71FL",
  "TZKR82FL",  "TZGE81FL",
  "UGKR52FL",  "UGGE53FL",
  "UGKR61FL",  "UGGE61FL",
  "YEKR61FL",  NA_character_,
  "ZAKR71FL",  "ZAGE71FL",
  "ZMKR51FL",  "ZMGE52FL",
  "ZMKR71FL",  "ZMGE71FL",
  "AOKR71FL",  "AOGE71FL",              
  "BDKR51FL",  "BDGE52FL",              
  "BDKR72FL",  "BDGE71FL",              
  "BDKR7RFL",  "BDGE7SFL",              
  "BDKR81FL",  "BDGE81FL",              
  "BFKR62FL",  "BFGE61FL",              
  "CDKR61FL",  "CDGE61FL",              
  "CGKR61FL",  NA_character_,              
  "CIKR62FL",  "CIGE61FL",              
  "CMKR61FL",  "CMGE61FL",              
  "GAKR61FL",  "GAGE61FL",              
  "GHKR5AFL",  "GHGE5AFL",              
  "GMKR61FL",  NA_character_,              
  "HTKR52FL",  "HTGE52FL",             
  "HTKR71FL",  "HTGE71FL",             
  "KHKR51FL",  "KHGE51FL",             
  "KMKR61FL",  "KMGE61FL",             
  "MDKR51FL",  "MDGE53FL",             
  "MLKR7AFL",  "MLGE7AFL",             
  "MMKR71FL",  "MMGE71FL",             
  "MRKR71FL",  "MRGE71FL",             
  "NGKR53FL",  "NGGE52FL",             
  "NGKR6AFL",  "NGGE6AFL",             
  "NGKR7BFL",  "NGGE7BFL",             
  "NIKR61FL",  "NIGE61FL",             
  "PGKR71FL",  NA_character_,             
  "RWKR5AFL",  "RWGE54FL",              
  "RWKR61FL",  "RWGE61FL",              
  "RWKR70FL",  "RWGE72FL",              
  "SLKR51FL",  "SLGE53FL",              
  "SLKR61FL",  "SLGE61FL",              
  "SLKR7AFL",  "SLGE7AFL",              
  "SNKR61FL",  "SNGE61FL",              
  "SNKR6DFL",  "SNGE6AFL",              
  "SNKR71FL",  "SNGE71FL",              
  "SNKR7AFL",  "SNGE7AFL",              
  "SNKR7IFL",  "SNGE7IFL",              
  "SNKR81FL",  "SNGE81FL",
  "SNKR8BFL",  "SNGE8BFL",              
  "STKR51FL",  NA_character_,     
  "TGKR61FL",  "TGGE62FL",             
  "TZKR7BFL",  "TZGE7AFL",             
  "UGKR7BFL",  "UGGE7AFL",             
  "ZMKR61FL",  "ZMGE61FL",             
  "ZWKR52FL",  "ZWGE52FL",             
  "ZWKR62FL",  "ZWGE61FL",             
  "ZWKR72FL",  "ZWGE72FL",
  "COKR61FL",  "COGE61FL",
  "GHKR72FL",  "GHGE71FL",
  "LSKR61FL",  "LSGE62FL",
  "MDKR81FL",  "MDGE81FL",
  "MWKR61FL",  "MWGE62FL",
  "TDKR71FL",  "TDGE71FL",
  "TZKR63FL",  "TZGE61FL",
)

# Keep only KR files actually present in analysis_data
kr_ge_map <- kr_ge_map %>%
  filter(filename %in% unique(analysis_data$filename))


# 4. Check which mapped GE files are missing locally
mapped_ge_check <- kr_ge_map %>%
  left_join(local_ge_lookup, by = "ge_stem")

cat("\nMapped GE files missing locally:\n")
first_ge_path <- mapped_ge_check %>%
  filter(!is.na(ge_path)) %>%
  slice(1) %>%
  pull(ge_path)

unzip(first_ge_path, list = TRUE)


# 5. Function to read one GE zip file
# rdhs can read DHS GPS flat files using its internal GPS format dictionary.

read_one_ge <- function(ge_path, kr_filename, ge_stem) {
  message("Reading: ", basename(ge_path), " for ", kr_filename)
  
  tmp_dir <- tempfile(pattern = "dhs_ge_")
  dir.create(tmp_dir)
  unzip(ge_path, exdir = tmp_dir)
  files <- list.files(tmp_dir, recursive = TRUE, full.names = TRUE)
  
  shp_file <- files[str_detect(tolower(files), "\\.shp$")]
  dta_file <- files[str_detect(tolower(files), "\\.dta$")]
  
  if (length(shp_file) > 0) {
    ge_dat <- sf::st_read(shp_file[1], quiet = TRUE)
    ge_dat <- sf::st_drop_geometry(ge_dat)
  } else if (length(dta_file) > 0) {
    ge_dat <- haven::read_dta(dta_file[1])
  } else {
    stop(
      "No .shp or .dta file found inside ",
      basename(ge_path),
      ". Files inside are:\n",
      paste(basename(files), collapse = ", ")
    )
  }
  
  ge_dat <- as.data.frame(ge_dat)
  names(ge_dat) <- toupper(names(ge_dat))
  
  required_vars <- c("DHSCLUST", "LATNUM", "LONGNUM")
  missing_vars <- setdiff(required_vars, names(ge_dat))
  
  if (length(missing_vars) > 0) {
    stop(
      "Missing required GPS variables in ",
      basename(ge_path),
      ": ",
      paste(missing_vars, collapse = ", "),
      "\nAvailable variables are:\n",
      paste(names(ge_dat), collapse = ", ")
    )
  }
  
  ge_dat %>%
    mutate(
      filename = kr_filename,
      ge_stem = ge_stem,
      DHSCLUST = as.integer(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM),
      tropical = case_when(
        is.na(latitude) ~ NA,
        abs(latitude) <= 23.5 ~ TRUE,
        TRUE ~ FALSE
      ),
      near_tropic_boundary = case_when(
        is.na(latitude) ~ NA,
        abs(abs(latitude) - 23.5) <= 0.5 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select(
      filename, ge_stem,
      DHSCLUST,
      latitude, longitude,
      tropical, near_tropic_boundary,
      everything()
    )
}


# 6. Read all available mapped GE files
ge_lookup <- mapped_ge_check %>%
  filter(!is.na(ge_stem), !is.na(ge_path)) %>%
  mutate(
    gps_data = pmap(
      list(ge_path, filename, ge_stem),
      read_one_ge
    )
  ) %>%
  dplyr::select(gps_data) %>%
  tidyr::unnest(gps_data)

ge_lookup %>%
  dplyr::select(filename, ge_stem, DHSCLUST, latitude, longitude, tropical) %>%
  head()


# 7. Check cluster overlap before merging
cluster_match_check <- analysis_data %>%
  distinct(filename, cluster) %>%
  mutate(cluster = as.integer(cluster)) %>%
  left_join(
    ge_lookup %>%
      distinct(filename, DHSCLUST) %>%
      mutate(
        DHSCLUST = as.integer(DHSCLUST),
        gps_cluster_found = TRUE
      ),
    by = c("filename", "cluster" = "DHSCLUST")
  ) %>%
  group_by(filename) %>%
  summarise(
    n_kr_clusters = n_distinct(cluster),
    n_matched_clusters = sum(gps_cluster_found %in% TRUE, na.rm = TRUE),
    prop_clusters_matched = n_matched_clusters / n_kr_clusters,
    .groups = "drop"
  ) %>%
  left_join(kr_ge_map, by = "filename") %>%
  arrange(prop_clusters_matched)

cluster_match_check %>%
  as_tibble() %>%
  print(n = Inf)


# 8. Merge GPS variables to analysis_data
analysis_data_gps <- analysis_data %>%
  mutate(cluster = as.integer(cluster)) %>%
  left_join(
    ge_lookup %>%
      distinct(
        filename,
        DHSCLUST,
        latitude,
        longitude,
        tropical,
        near_tropic_boundary
      ) %>%
      mutate(DHSCLUST = as.integer(DHSCLUST)),
    by = c("filename", "cluster" = "DHSCLUST")
  )

analysis_data_gps <- analysis_data_gps %>%
  mutate(
    country_code = substr(filename, 1, 2),
    
    tropical_final = case_when(
      !is.na(tropical) ~ tropical,
      # Countries clearly outside tropical latitude band
      country_code %in% c("AF", "AZ", "PK") ~ FALSE,
      # Countries clearly inside tropical latitude band
      country_code %in% c("ID", "MV", "YE", "HN", "PE", "PH", "CG", "ST", "PG", "GM") ~ TRUE,
      TRUE ~ NA
    ),
    gps_tropical_source = case_when(
      !is.na(tropical) ~ "cluster_gps",
      country_code %in% c("AF", "AZ", "PK") ~ "country_assigned_non_tropical",
      country_code %in% c("ID", "MV", "YE", "HN", "PE", "PH", "CG", "ST", "PG", "GM") ~ "country_assigned_tropical",
      TRUE ~ "missing"
    ), 
    latitude = case_when(
      !is.na(latitude) ~ latitude,
      country_code == "AF" ~  33.9, # Afghanistan
      country_code == "AZ" ~  40.1, # Azerbaijan
      country_code == "PK" ~  30.4, # Pakistan
      country_code == "ID" ~  2.5, # Indonesia
      country_code == "MV" ~  3.2, # Maldives
      country_code == "YE" ~  15.6, # Yemen
      country_code == "HN" ~  15.2, # Honduras
      country_code == "PE" ~  9.2, # Peru
      country_code == "PH" ~  12.9, # Philippines
      country_code == "CG" ~  4.0, # Republic of the Congo
      country_code == "ST" ~  0.2, # São Tomé and Príncipe
      country_code == "PG" ~  6.0, # Papua New Guinea
      country_code == "GM" ~  13.4, # Gambia
      TRUE ~ NA_real_
    ), 
    near_tropic_boundary = case_when(
      !is.na(near_tropic_boundary) ~ near_tropic_boundary,
      is.na(near_tropic_boundary) ~ FALSE,
      TRUE ~ NA
    )
  )



analysis_data_gps %>%
  summarise(
    n = n(),
    prop_missing_latitude = mean(is.na(latitude)),
    prop_tropical = mean(tropical, na.rm = TRUE),
    prop_near_tropic_boundary = mean(near_tropic_boundary, na.rm = TRUE)
  )


# 9. Summary after merge
gps_summary <- analysis_data_gps %>%
  group_by(filename) %>%
  summarise(
    n = n(),
    prop_missing_latitude = mean(is.na(latitude)),
    prop_tropical = mean(tropical_final, na.rm = TRUE),
    prop_near_boundary = mean(near_tropic_boundary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_missing_latitude))

cat("\nGPS merge summary:\n")
gps_summary %>%
  print(n = Inf)

analysis_data_gps <- analysis_data_gps %>% mutate(latitude = abs(latitude))

analysis_data_gps %>%
  summarise(
    n = n(),
    prop_missing_latitude = mean(is.na(latitude)),
    prop_missing_tropical = mean(is.na(tropical_final), na.rm = TRUE),
    prop_tropical = mean(tropical_final, na.rm = TRUE),
    prop_near_boundary = mean(near_tropic_boundary, na.rm = TRUE)
  )

# 10. Save GPS-enhanced analysis data
saveRDS(analysis_data_gps, "outputs/pipelines/incidence_analysis_data_with_gps.rds")









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

analysis_data_gps <- readRDS("outputs/pipelines/incidence_analysis_data_with_gps.rds")

# Variables to report in the missingness table
missing_vars <- c(
  "fever", "LRTI",
  "ch_age_group", "ch_sex",
  "wh_z_score",
  "water_collapsed", "sanitation_collapsed", "electricity", "fuel_collapsed",
  "rural_urban", "hh_wealth",
  "maternal_edu",
  "month_sin", "month_cos",
  "gbd_region", "GDP", "Endemic",
  "tropical_final", "latitude",
  "bednet_treated",
  "sample_weight"
)

# Optional readable labels
var_labels <- c(
  fever = "Fever in previous 2 weeks",
  LRTI = "Lower respiratory tract infection",
  ch_age_group = "Child age group",
  ch_sex = "Child sex",
  wh_z_score = "Weight-for-height z-score",
  water_collapsed = "Drinking water category",
  sanitation_collapsed = "Sanitation category",
  electricity = "Household electricity",
  fuel_collapsed = "Cooking fuel category",
  rural_urban = "Rural or urban residence",
  hh_wealth = "Household wealth quintile",
  maternal_edu = "Maternal education",
  month_sin = "Seasonality sine term",
  month_cos = "Seasonality cosine term",
  gbd_region = "GBD region",
  GDP = "GDP per capita",
  Endemic = "Malaria endemicity",
  tropical_final = "Tropical latitude category",
  latitude = "Absolute latitude",
  bednet_treated = "Treated bednet use",
  sample_weight = "DHS sampling weight"
)

missing_table <- analysis_data_gps %>%
  summarise(
    across(
      all_of(missing_vars),
      list(
        n_missing = ~sum(is.na(.x)),
        pct_missing = ~mean(is.na(.x)) * 100,
        n_nonmissing = ~sum(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) %>%
  mutate(
    variable_label = unname(var_labels[variable]),
    n_total = nrow(analysis_data_gps),
    pct_missing = round(pct_missing, 2)
  ) %>%
  dplyr::select(
    variable,
    variable_label,
    n_total,
    n_nonmissing,
    n_missing,
    pct_missing
  ) %>%
  arrange(desc(pct_missing), variable)

missing_table

write.xlsx(
  missing_table,
  file = "outputs/supplementary/incidence_missingness_table.xlsx",
  overwrite = TRUE
)

missing_table_pdf <- missing_table %>%
  mutate(
    pct_missing = sprintf("%.2f", pct_missing)
  )

dir.create("figures/main_analysis/inc_model_diagnostics", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/main_analysis/inc_model_diagnostics/missingness_table.pdf",
  width = 12,
  height = 8.5
)

grid.newpage()

grid.text(
  "Missingness of variables used in incidence models",
  x = 0.5,
  y = 0.96,
  gp = gpar(fontsize = 14, fontface = "bold")
)

grid.table(
  missing_table_pdf,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    core = list(fg_params = list(cex = 0.75)),
    colhead = list(fg_params = list(fontface = "bold", cex = 0.8))
  ),
  vp = viewport(y = 0.48, height = 0.86)
)

dev.off()

model_data_mi <- analysis_data_gps %>%
  dplyr::select(
    ch_id,
    filename, cluster,
    Country, Year, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, GDP, Endemic,
    tropical_final, latitude,
    bednet_treated,
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
    
    filename = factor(filename),
    cluster = as.integer(cluster),
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
    
    tropical_final = factor(
      tropical_final,
      levels = c(FALSE, TRUE),
      labels = c("non_tropical", "tropical")
    ),
    latitude_z = as.numeric(scale(latitude)),
    bednet_treated = factor(
      bednet_treated,
      levels = c(0, 1)
    ),
    
    sample_weight = sample_weight / 1000000,
    
    fever = as.integer(fever),
    LRTI = as.integer(LRTI)
  ) %>%
  dplyr::select(
    ch_id,
    filename, cluster,
    Country, CountryYear,
    ch_age_group, ch_sex, wh_z_score,
    water_collapsed, sanitation_collapsed, electricity, fuel_collapsed, rural_urban,
    hh_wealth, maternal_edu,
    month_sin, month_cos,
    gbd_region, Endemic, tropical_final, latitude_z, log_GDP_z, Year_c,
    bednet_treated,
    fever, LRTI,
    sub_region_id,
    sample_weight
  ) %>%
  as.data.frame()

sort(colMeans(is.na(model_data_mi)), decreasing = TRUE)

ini <- mice(model_data_mi, maxit = 0)

meth <- ini$method
pred <- ini$predictorMatrix

meth[] <- ""

meth["water_collapsed"] <- "logreg"
meth["fuel_collapsed"] <- "logreg"
meth["sanitation_collapsed"] <- "polyreg"
meth["electricity"] <- "logreg"
meth["maternal_edu"] <- "polyreg"
meth["wh_z_score"] <- "pmm"
# Because bednet missingness is high and survey/module-dependent, cart is usually more stable than logreg here.
meth["bednet_treated"] <- "cart"

pred[,] <- 0

imputed_vars <- c(
  "wh_z_score",
  "electricity",
  "water_collapsed",
  "sanitation_collapsed",
  "fuel_collapsed",
  "maternal_edu",
  "bednet_treated"
)

predictor_vars <- c(
  "Year_c",
  "ch_age_group", "ch_sex",
  "rural_urban",
  "hh_wealth",
  "month_sin", "month_cos",
  "gbd_region", "Endemic", "latitude_z", "log_GDP_z",
  "fever", "LRTI",
  "sample_weight"
)

pred[imputed_vars, predictor_vars] <- 1

# Allow imputed variables to predict each other
pred[imputed_vars, imputed_vars] <- 1
diag(pred) <- 0

# Do not use identifiers as predictors
pred[, c("ch_id", "filename", "cluster", "CountryYear", "sub_region_id","tropical_final")] <- 0
pred[c("ch_id", "filename", "cluster", "CountryYear", "sub_region_id","tropical_final"), ] <- 0

Sys.time()

## Parallel multiple imputation ---------------------------------------------
# Use three workers to limit memory use.
# Each worker holds its own copy of the data and imputation models.
n_core_mi <- min(3L, future::availableCores())

Sys.time()

imp <- mice::futuremice(
  data = model_data_mi,
  m = 5,
  maxit = 10,
  method = meth,
  predictorMatrix = pred,
  ridge = 1e-04,
  n.core = n_core_mi,
  parallelseed = 123,
  future.plan = "multisession"
)

Sys.time()

future::plan(future::sequential)

imp
summary(imp)
plot(imp)

complete_data_check <- mice::complete(imp, 1)

complete_data_check %>%
  summarise(
    hh_wealth_missing = mean(is.na(hh_wealth)),
    wh_z_missing = mean(is.na(wh_z_score)),
    water_missing = mean(is.na(water_collapsed)),
    sanitation_missing = mean(is.na(sanitation_collapsed)),
    electricity_missing = mean(is.na(electricity)),
    fuel_missing = mean(is.na(fuel_collapsed)),
    maternal_edu_missing = mean(is.na(maternal_edu)),
    bednet_missing = mean(is.na(bednet_treated))
  )

if (!is.null(imp$loggedEvents)) {
  imp$loggedEvents %>%
    as_tibble() %>%
    count(dep, meth, out, sort = TRUE) %>%
    print(n = 500)
}

saveRDS(imp, "outputs/pipelines/incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds")





# _____________________________________________ ----------
# 5. Estimation of Fever and LRTI incidence rate -------------------
# Poisson GLMMs with robust sandwich SEs and MI pooling

rm(list = ls()); gc()

library(dplyr)
library(mice)
library(glmmTMB)
library(sandwich)
library(future)
library(future.apply)
library(stringr)
library(grid)
library(gridExtra)


imp <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds")

plan(multisession, workers = 3)

completed_data <- mice::complete(imp, "all")

recall_days <- 14
D_lrti_symptom <- 17.5
D_fever_symptom <- 5

completed_data <- lapply(completed_data, function(dat) {
  dat %>%
    mutate(
      log_recall_offset_lrti = log((recall_days + D_lrti_symptom) / 365),
      log_recall_offset_fever = log((recall_days + D_fever_symptom) / 365),
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE)
    )
})

# LRTI models ---------------------------------------------------------------

fit_lrti_list <- future_lapply(completed_data, function(dat) {
  glmmTMB::glmmTMB(
    LRTI ~ ch_age_group + ch_sex + wh_z_score +
      water_collapsed + sanitation_collapsed + electricity + fuel_collapsed +
      rural_urban + hh_wealth + maternal_edu + latitude_z +
      Endemic + bednet_treated +
      month_sin + month_cos + Year_c +
      log_GDP_z +
      offset(log_recall_offset_lrti) +
      (1 | gbd_region) +
      (1 | sub_region_nested_id),
    data = dat,
    family = poisson(link = "log"),
    weights = sample_weight,
    REML = FALSE,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2e5, eval.max = 2e5)
    )
  )
}, future.seed = TRUE)

fit_lrti_mi <- mice::as.mira(fit_lrti_list)
saveRDS(fit_lrti_mi, "outputs/pipelines/(GBD_RE)fit_lrti_mi_gps.rds")

coef_names_lrti <- names(glmmTMB::fixef(fit_lrti_list[[1]])$cond)

beta_lrti <- do.call(cbind, lapply(fit_lrti_list, function(fit_i) {
  glmmTMB::fixef(fit_i)$cond[coef_names_lrti]
}))

robust_var_lrti <- do.call(cbind, lapply(seq_along(fit_lrti_list), function(i) {
  fit_i <- fit_lrti_list[[i]]
  cluster_i <- model.frame(fit_i)$gbd_region
  
  vcov_i <- sandwich::vcovHC(
    fit_i,
    type = "HC0",
    cluster = cluster_i,
    full = FALSE
  )
  
  diag(vcov_i[coef_names_lrti, coef_names_lrti, drop = FALSE])
}))

m_lrti <- ncol(beta_lrti)
qbar_lrti <- rowMeans(beta_lrti)
ubar_lrti <- rowMeans(robust_var_lrti)
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
  estimate_log = qbar_lrti,
  robust_std_error = se_lrti,
  statistic = statistic_lrti,
  df = df_lrti,
  p.value = 2 * pt(abs(statistic_lrti), df = df_lrti, lower.tail = FALSE),
  estimate = exp(qbar_lrti),
  conf.low = exp(qbar_lrti - critical_lrti * se_lrti),
  conf.high = exp(qbar_lrti + critical_lrti * se_lrti)
)

# Fever models --------------------------------------------------------------

fit_fever_list <- future_lapply(completed_data, function(dat) {
  glmmTMB::glmmTMB(
    fever ~ ch_age_group + ch_sex + wh_z_score +
      water_collapsed + sanitation_collapsed + electricity + fuel_collapsed +
      rural_urban + hh_wealth + maternal_edu + latitude_z +
      Endemic + bednet_treated +
      month_sin + month_cos + Year_c +
      log_GDP_z +
      offset(log_recall_offset_fever) +
      (1 | gbd_region) +
      (1 | sub_region_nested_id),
    data = dat,
    family = poisson(link = "log"),
    weights = sample_weight,
    REML = FALSE,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2e5, eval.max = 2e5)
    )
  )
}, future.seed = TRUE)

fit_fever_mi <- mice::as.mira(fit_fever_list)
saveRDS(fit_fever_mi, "outputs/pipelines/(GBD_RE)fit_fever_mi_gps.rds")

coef_names_fever <- names(glmmTMB::fixef(fit_fever_list[[1]])$cond)

beta_fever <- do.call(cbind, lapply(fit_fever_list, function(fit_i) {
  glmmTMB::fixef(fit_i)$cond[coef_names_fever]
}))

robust_var_fever <- do.call(cbind, lapply(seq_along(fit_fever_list), function(i) {
  fit_i <- fit_fever_list[[i]]
  cluster_i <- model.frame(fit_i)$gbd_region
  
  vcov_i <- sandwich::vcovHC(
    fit_i,
    type = "HC0",
    cluster = cluster_i,
    full = FALSE
  )
  
  diag(vcov_i[coef_names_fever, coef_names_fever, drop = FALSE])
}))

m_fever <- ncol(beta_fever)
qbar_fever <- rowMeans(beta_fever)
ubar_fever <- rowMeans(robust_var_fever)
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
  estimate_log = qbar_fever,
  robust_std_error = se_fever,
  statistic = statistic_fever,
  df = df_fever,
  p.value = 2 * pt(abs(statistic_fever), df = df_fever, lower.tail = FALSE),
  estimate = exp(qbar_fever),
  conf.low = exp(qbar_fever - critical_fever * se_fever),
  conf.high = exp(qbar_fever + critical_fever * se_fever)
)

summary_lrti
summary_fever



saveRDS(summary_lrti, "outputs/pipelines/(GBD_RE)summary_lrti_gps.rds")
saveRDS(summary_fever, "outputs/pipelines/(GBD_RE)summary_fever_gps.rds")

plan(sequential)
gc()

# Regression tables and model diagnostics -----------------------------------
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
      term = str_replace_all(term, "latitude z", "Absolute latitude, z-score"),
      term = str_replace_all(term, "log GDP z", "Log GDP per capita, z-score"),
      term = str_replace_all(term, "Year c", "Survey year, centered"),
      term = str_replace_all(term, "Endemic1", "Malaria endemic"),
      term = str_replace_all(term, "bednet treated1", "Treated bednet use"),
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
      IRR = sprintf("%.3f", estimate),
      `95% CI` = paste0(sprintf("%.3f", conf.low), "–", sprintf("%.3f", conf.high)),
      `Robust SE, log scale` = sprintf("%.3f", robust_std_error),
      `P value` = format_p(p.value)
    )
}

summary_lrti_pdf <- format_regression_table(summary_lrti, "LRTI")
summary_fever_pdf <- format_regression_table(summary_fever, "Fever")

dir.create("figures/main_analysis/inc_model_diagnostics", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/main_analysis/inc_model_diagnostics/(GBD_RE)regression_summary_lrti_fever.pdf",
  width = 13,
  height = 10
)

grid.newpage()
grid.text(
  "LRTI incidence model",
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
  "Fever incidence model",
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






# _____________________________________________ ----------
# 6. Prediction and Aggregation to country-year specific incidence -----------------------

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

imp <- readRDS("outputs/pipelines/incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds")

fit_lrti_mi  <- readRDS("outputs/pipelines/(GBD_RE)fit_lrti_mi_gps.rds")
fit_fever_mi <- readRDS("outputs/pipelines/(GBD_RE)fit_fever_mi_gps.rds")

# PC data are not used in Section 6.
# The five PC imputations will be crossed with the five DHS imputations in Section 7.

recall_days <- 14
D_lrti_symptom <- 17.5
D_fever_symptom <- 5

get_exposure_fraction <- function(outcome) {
  if (outcome == "LRTI") {
    (recall_days + D_lrti_symptom) / 365
  } else if (outcome == "fever") {
    (recall_days + D_fever_symptom) / 365
  } else {
    stop("outcome must be either 'LRTI' or 'fever'")
  }
}

get_log_offset <- function(outcome) {
  log(get_exposure_fraction(outcome))
}

add_incidence_offsets <- function(dat) {
  dat %>%
    mutate(
      log_recall_offset_lrti  = log((recall_days + D_lrti_symptom) / 365),
      log_recall_offset_fever = log((recall_days + D_fever_symptom) / 365)
    )
}

add_grouping_vars <- function(dat) {
  dat %>%
    mutate(
      gbd_region = factor(gbd_region),
      sub_region_id = factor(sub_region_id),
      sub_region_nested_id = interaction(gbd_region, sub_region_id, drop = TRUE)
    )
}

add_age_group_for_output <- function(dat, stratify_age = FALSE) {
  if (stratify_age) {
    dat %>% mutate(age_group = as.character(ch_age_group))
  } else {
    dat %>% mutate(age_group = "Overall")
  }
}

manual_country_year_values <- tibble::tribble(
  ~CountryYear,      ~Population_manual, ~GDP_manual,
  "Haiti_2012",       1243438,             3286,
  "Maldives_2009",      30631,            16163,
  "Maldives_2016",      36815,            19264,
  "Maldives_2017",      36199,            19948,
  "Yemen_2013",       4616819,             3665
)

get_fit <- function(i, outcome) {
  if (outcome == "LRTI") {
    fit_lrti_mi$analyses[[i]]
  } else if (outcome == "fever") {
    fit_fever_mi$analyses[[i]]
  } else {
    stop("outcome must be either 'LRTI' or 'fever'")
  }
}


## 1. Individual prediction -> country-year mean ------------------------------
### 1-1. Aggregation function --------------------------------------------------

aggregate_country_year <- function(
    i,
    outcome = c("LRTI", "fever"),
    prediction_type = c("marginal_RE0", "conditional_RE"),
    stratify_age = FALSE
) {
  
  outcome <- match.arg(outcome)
  prediction_type <- match.arg(prediction_type)
  
  exposure_fraction_i <- get_exposure_fraction(outcome)
  
  dat_i <- mice::complete(imp, i) %>%
    add_incidence_offsets() %>%
    add_grouping_vars() %>%
    add_age_group_for_output(stratify_age = stratify_age)
  
  maternal_edu_levels <- levels(dat_i$maternal_edu)
  age_group_levels <- levels(dat_i$ch_age_group)
  
  dat_pred <- dat_i %>%
    mutate(
      maternal_edu = as.character(maternal_edu),
      maternal_edu = case_when(
        maternal_edu == "0" ~ "1",
        TRUE ~ maternal_edu
      ),
      maternal_edu = factor(maternal_edu, levels = maternal_edu_levels),
      ch_age_group = factor(ch_age_group, levels = age_group_levels)
    )
  
  fit_i <- get_fit(i, outcome)
  
  re_form_i <- if (prediction_type == "marginal_RE0") {
    NA
  } else {
    NULL
  }
  
  pred_window <- predict(
    fit_i,
    newdata = dat_pred,
    type = "response",
    re.form = re_form_i,
    allow.new.levels = TRUE
  )
  
  dat_i %>%
    mutate(
      .imp_dhs = i,
      outcome = outcome,
      prediction_type = prediction_type,
      pred_annual_rate = pred_window / exposure_fraction_i
    ) %>%
    group_by(
      .imp_dhs, outcome, age_group,
      Country, CountryYear, gbd_region, Year_c, prediction_type
    ) %>%
    summarise(
      n_child = n(),
      weight_sum = sum(sample_weight, na.rm = TRUE),
      observed_14d_risk = weighted.mean(.data[[outcome]], sample_weight, na.rm = TRUE),
      observed_annual_rate = observed_14d_risk / exposure_fraction_i,
      pred_annual_rate = weighted.mean(pred_annual_rate, sample_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Country = as.character(Country),
      Year = Year_c + 2010
    )
}


### 1-2. Run and retain every DHS imputation ----------------------------------

run_country_year_imp <- function(outcome, prediction_type, stratify_age = FALSE) {
  
  map_dfr(
    seq_len(imp$m),
    aggregate_country_year,
    outcome = outcome,
    prediction_type = prediction_type,
    stratify_age = stratify_age
  )
}

country_year_lrti_marginal_imp      <- run_country_year_imp(outcome = "LRTI",  prediction_type = "marginal_RE0", stratify_age = FALSE)
country_year_fever_marginal_imp     <- run_country_year_imp(outcome = "fever", prediction_type = "marginal_RE0", stratify_age = FALSE)
country_year_lrti_marginal_age_imp  <- run_country_year_imp(outcome = "LRTI",  prediction_type = "marginal_RE0", stratify_age = TRUE)
country_year_fever_marginal_age_imp <- run_country_year_imp(outcome = "fever", prediction_type = "marginal_RE0", stratify_age = TRUE)

table(country_year_lrti_marginal_imp$.imp_dhs)
table(country_year_fever_marginal_imp$.imp_dhs)
table(country_year_lrti_marginal_age_imp$.imp_dhs)
table(country_year_fever_marginal_age_imp$.imp_dhs)

stopifnot(
  setequal(unique(country_year_lrti_marginal_imp$.imp_dhs), seq_len(imp$m)),
  setequal(unique(country_year_fever_marginal_imp$.imp_dhs), seq_len(imp$m)),
  setequal(unique(country_year_lrti_marginal_age_imp$.imp_dhs), seq_len(imp$m)),
  setequal(unique(country_year_fever_marginal_age_imp$.imp_dhs), seq_len(imp$m))
)


### 1-3. MI-averaged summaries retained only for descriptive plots ------------

summarise_mi_country_year <- function(country_year_imp) {
  
  country_year_imp %>%
    group_by(
      outcome, age_group,
      Country, CountryYear, gbd_region, Year_c, Year, prediction_type
    ) %>%
    summarise(
      n_child_mean = mean(n_child, na.rm = TRUE),
      weight_sum_mean = mean(weight_sum, na.rm = TRUE),
      observed_annual_rate_mean = mean(observed_annual_rate, na.rm = TRUE),
      pred_annual_rate_mean = mean(pred_annual_rate, na.rm = TRUE),
      pred_annual_rate_sd = sd(pred_annual_rate, na.rm = TRUE),
      pred_annual_rate_lcl = quantile(pred_annual_rate, 0.025, na.rm = TRUE),
      pred_annual_rate_ucl = quantile(pred_annual_rate, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
}

country_year_lrti_marginal      <- summarise_mi_country_year(country_year_lrti_marginal_imp)
country_year_fever_marginal     <- summarise_mi_country_year(country_year_fever_marginal_imp)
country_year_lrti_marginal_age  <- summarise_mi_country_year(country_year_lrti_marginal_age_imp)
country_year_fever_marginal_age <- summarise_mi_country_year(country_year_fever_marginal_age_imp)


## 2. Add GDP and population -------------------------------------------------

GDP.PPP <- read.csv("data/metadata/GDP_PPP.csv") # Yemen and Venezuela from IMF data
colnames(GDP.PPP)[-1] <- gsub("^X", "", colnames(GDP.PPP)[-1])

GDP.PPP <- GDP.PPP %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP")

GDP.PPP$Year <- as.numeric(GDP.PPP$Year)

Population <- read.csv("data/metadata/Under_5.csv") %>%
  dplyr::select(Entity, Year, Population)

colnames(Population) <- c("Country", "Year", "Population")

head(GDP.PPP)
head(Population)

a <- unique(GDP.PPP$Country)
b <- unique(Population$Country)
c <- unique(country_year_lrti_marginal$Country)

setdiff(c, a)
setdiff(c, b)

GDP.PPP <- GDP.PPP %>%
  mutate(Country = dplyr::recode(
    Country,
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Congo, Rep."      = "Republic of the Congo",
    "Gambia, The"      = "Gambia",
    "Egypt, Arab Rep." = "Egypt",
    "Yemen, Rep."      = "Yemen"
  ))

Population <- Population %>%
  mutate(Country = dplyr::recode(
    Country,
    "Democratic Republic of Congo" = "Democratic Republic of the Congo",
    "Congo"                        = "Republic of the Congo",
    "Egypt, Arab Rep."             = "Egypt",
    "Yemen, Rep."                  = "Yemen"
  ))

add_gdp_population <- function(country_year_table) {
  
  country_year_table %>%
    mutate(
      Country = as.character(Country),
      Year = as.numeric(Year)
    ) %>%
    
    left_join(
      Population %>%
        mutate(
          Country = as.character(Country),
          Year = as.numeric(Year)
        ) %>%
        dplyr::select(Country, Year, Population),
      by = c("Country", "Year")
    ) %>%
    
    left_join(
      GDP.PPP %>%
        mutate(
          Country = as.character(Country),
          Year = as.numeric(Year)
        ) %>%
        dplyr::select(Country, Year, GDP),
      by = c("Country", "Year")
    ) %>%
    
    left_join(
      manual_country_year_values,
      by = "CountryYear"
    ) %>%
    mutate(
      Population = if_else(!is.na(Population_manual), Population_manual, Population),
      GDP = if_else(!is.na(GDP_manual), GDP_manual, GDP)
    ) %>%
    dplyr::select(-Population_manual, -GDP_manual)
}


### 2-1. DHS-imputation-specific objects used in Sections 7-10 ----------------

country_year_lrti_marginal_imp_gdp      <- add_gdp_population(country_year_lrti_marginal_imp)
country_year_fever_marginal_imp_gdp     <- add_gdp_population(country_year_fever_marginal_imp)
country_year_lrti_marginal_age_imp_gdp  <- add_gdp_population(country_year_lrti_marginal_age_imp)
country_year_fever_marginal_age_imp_gdp <- add_gdp_population(country_year_fever_marginal_age_imp)


### 2-2. MI-averaged objects retained for descriptive summaries/plots ----------

country_year_lrti_marginal_gdp      <- add_gdp_population(country_year_lrti_marginal)
country_year_fever_marginal_gdp     <- add_gdp_population(country_year_fever_marginal)
country_year_lrti_marginal_age_gdp  <- add_gdp_population(country_year_lrti_marginal_age)
country_year_fever_marginal_age_gdp <- add_gdp_population(country_year_fever_marginal_age)

a <- country_year_lrti_marginal_gdp[
  is.na(country_year_lrti_marginal_gdp$GDP), ]

b <- country_year_lrti_marginal_gdp[
  is.na(country_year_lrti_marginal_gdp$Population), ]

a[, c("CountryYear")]
b[, c("CountryYear")]


## 3. Region-level population-weighted summary -------------------------------

# Descriptive summaries only.
# Primary downstream analyses use the imputation-specific country-year objects.

summarise_region <- function(country_year_table) {
  
  group_vars <- c("outcome", "gbd_region", "Year")
  
  if ("age_group" %in% names(country_year_table)) {
    group_vars <- c("outcome", "age_group", "gbd_region", "Year")
  }
  
  country_year_table %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      pred_annual_rate_mean = weighted.mean(
        pred_annual_rate_mean,
        Population,
        na.rm = TRUE
      ),
      observed_annual_rate_mean = weighted.mean(
        observed_annual_rate_mean,
        Population,
        na.rm = TRUE
      ),
      GDP_pw_mean = weighted.mean(GDP, Population, na.rm = TRUE),
      n_country_year = n(),
      .groups = "drop"
    )
}

region_year_lrti_marginal_gdp      <- summarise_region(country_year_lrti_marginal_gdp)
region_year_fever_marginal_gdp     <- summarise_region(country_year_fever_marginal_gdp)
region_year_lrti_marginal_age_gdp  <- summarise_region(country_year_lrti_marginal_age_gdp)
region_year_fever_marginal_age_gdp <- summarise_region(country_year_fever_marginal_age_gdp)


## 4. Save prediction and aggregation outputs --------------------------------

out_dir <- "outputs/pipelines"
save_dir <- file.path(out_dir, "marginal_RE0_prediction_outputs")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)


### 4-1. Save individual RDS files --------------------------------------------

objects_to_save <- list(
  
  # DHS-imputation-specific country-year estimates
  country_year_lrti_marginal_imp      = country_year_lrti_marginal_imp,
  country_year_fever_marginal_imp     = country_year_fever_marginal_imp,
  country_year_lrti_marginal_age_imp  = country_year_lrti_marginal_age_imp,
  country_year_fever_marginal_age_imp = country_year_fever_marginal_age_imp,
  
  # DHS-imputation-specific GDP/population-added estimates
  # These will be used in Sections 7-10
  country_year_lrti_marginal_imp_gdp      = country_year_lrti_marginal_imp_gdp,
  country_year_fever_marginal_imp_gdp     = country_year_fever_marginal_imp_gdp,
  country_year_lrti_marginal_age_imp_gdp  = country_year_lrti_marginal_age_imp_gdp,
  country_year_fever_marginal_age_imp_gdp = country_year_fever_marginal_age_imp_gdp,
  
  # MI-averaged country-year estimates retained for descriptive use
  country_year_lrti_marginal      = country_year_lrti_marginal,
  country_year_fever_marginal     = country_year_fever_marginal,
  country_year_lrti_marginal_age  = country_year_lrti_marginal_age,
  country_year_fever_marginal_age = country_year_fever_marginal_age,
  
  # MI-averaged GDP/population-added estimates
  country_year_lrti_marginal_gdp      = country_year_lrti_marginal_gdp,
  country_year_fever_marginal_gdp     = country_year_fever_marginal_gdp,
  country_year_lrti_marginal_age_gdp  = country_year_lrti_marginal_age_gdp,
  country_year_fever_marginal_age_gdp = country_year_fever_marginal_age_gdp,
  
  # Region-year population-weighted estimates
  region_year_lrti_marginal_gdp      = region_year_lrti_marginal_gdp,
  region_year_fever_marginal_gdp     = region_year_fever_marginal_gdp,
  region_year_lrti_marginal_age_gdp  = region_year_lrti_marginal_age_gdp,
  region_year_fever_marginal_age_gdp = region_year_fever_marginal_age_gdp
)

purrr::iwalk(
  objects_to_save,
  ~ saveRDS(.x, file.path(save_dir, paste0(.y, ".rds")))
)


### 4-2. Save combined RDS files ---------------------------------------------

country_year_dhs_imp_all <- bind_rows(
  country_year_lrti_marginal_imp,
  country_year_fever_marginal_imp,
  country_year_lrti_marginal_age_imp,
  country_year_fever_marginal_age_imp
)

country_year_dhs_imp_gdp_all <- bind_rows(
  country_year_lrti_marginal_imp_gdp,
  country_year_fever_marginal_imp_gdp,
  country_year_lrti_marginal_age_imp_gdp,
  country_year_fever_marginal_age_imp_gdp
)

country_year_all <- bind_rows(
  country_year_lrti_marginal,
  country_year_fever_marginal,
  country_year_lrti_marginal_age,
  country_year_fever_marginal_age
)

country_year_marginal_gdp_all <- bind_rows(
  country_year_lrti_marginal_gdp,
  country_year_fever_marginal_gdp,
  country_year_lrti_marginal_age_gdp,
  country_year_fever_marginal_age_gdp
)

region_year_marginal_gdp_all <- bind_rows(
  region_year_lrti_marginal_gdp,
  region_year_fever_marginal_gdp,
  region_year_lrti_marginal_age_gdp,
  region_year_fever_marginal_age_gdp
)

saveRDS(
  country_year_dhs_imp_all,
  file.path(save_dir, "country_year_dhs_imputation_specific.rds")
)

saveRDS(
  country_year_dhs_imp_gdp_all,
  file.path(save_dir, "country_year_dhs_imputation_specific_gdp.rds")
)

saveRDS(
  country_year_all,
  file.path(save_dir, "country_year_all_marginal_conditional.rds")
)

saveRDS(
  country_year_marginal_gdp_all,
  file.path(save_dir, "country_year_marginal_RE0_gdp_all.rds")
)

saveRDS(
  region_year_marginal_gdp_all,
  file.path(save_dir, "region_year_marginal_RE0_gdp_all.rds")
)


### 4-3. Save CSV files for easy inspection ----------------------------------

write.csv(
  country_year_dhs_imp_gdp_all,
  file.path(save_dir, "country_year_dhs_imputation_specific_gdp.csv"),
  row.names = FALSE
)

write.csv(
  country_year_marginal_gdp_all,
  file.path(save_dir, "country_year_marginal_RE0_gdp_all.csv"),
  row.names = FALSE
)

write.csv(
  region_year_marginal_gdp_all,
  file.path(save_dir, "region_year_marginal_RE0_gdp_all.csv"),
  row.names = FALSE
)


### 4-4. Save one Excel workbook ---------------------------------------------

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "CY DHS MI GDP")
openxlsx::writeData(
  wb,
  "CY DHS MI GDP",
  country_year_dhs_imp_gdp_all
)

openxlsx::addWorksheet(wb, "CY marginal RE0 GDP")
openxlsx::writeData(
  wb,
  "CY marginal RE0 GDP",
  country_year_marginal_gdp_all
)

openxlsx::addWorksheet(wb, "Region marginal RE0 GDP")
openxlsx::writeData(
  wb,
  "Region marginal RE0 GDP",
  region_year_marginal_gdp_all
)

openxlsx::saveWorkbook(
  wb,
  file.path(save_dir, "marginal_RE0_prediction_outputs.xlsx"),
  overwrite = TRUE
)


### 4-5. Console checks -------------------------------------------------------

cat(
  "\nDHS imputation-specific age-specific LRTI rows by imputation:\n"
)

print(
  table(country_year_lrti_marginal_age_imp_gdp$.imp_dhs)
)

cat(
  "\nDHS imputation-specific age-specific fever rows by imputation:\n"
)

print(
  table(country_year_fever_marginal_age_imp_gdp$.imp_dhs)
)

cat(
  "\nSaved marginal RE0 prediction outputs to:\n",
  normalizePath(save_dir),
  "\n\nFiles saved:\n",
  paste(list.files(save_dir), collapse = "\n"),
  "\n",
  sep = ""
)


## 5. Plots ---------------------------------------------------

country_year_lrti_marginal_gdp <-
  readRDS(file.path(save_dir, "country_year_lrti_marginal_gdp.rds"))

country_year_fever_marginal_gdp <-
  readRDS(file.path(save_dir, "country_year_fever_marginal_gdp.rds"))

region_year_lrti_marginal_gdp <-
  readRDS(file.path(save_dir, "region_year_lrti_marginal_gdp.rds"))

region_year_fever_marginal_gdp <-
  readRDS(file.path(save_dir, "region_year_fever_marginal_gdp.rds"))

country_year_lrti_marginal_age_gdp <-
  readRDS(file.path(save_dir, "country_year_lrti_marginal_age_gdp.rds"))

country_year_fever_marginal_age_gdp <-
  readRDS(file.path(save_dir, "country_year_fever_marginal_age_gdp.rds"))

region_year_lrti_marginal_age_gdp <-
  readRDS(file.path(save_dir, "region_year_lrti_marginal_age_gdp.rds"))

region_year_fever_marginal_age_gdp <-
  readRDS(file.path(save_dir, "region_year_fever_marginal_age_gdp.rds"))









# _____________________________________________ ----------
# 7. Age-specific marginal RE0 model comparison -------------------------------

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(mgcv)
  library(ranger)
  library(glmnet)
  library(xgboost)
})

set.seed(123)
options(scipen = 999)

out_dir <- "outputs/pipelines"
prediction_output_dir <- file.path(out_dir, "marginal_RE0_prediction_outputs")
model_search_dir <- file.path(out_dir, "marginal_RE0_full_model_search_outputs")
dir.create(model_search_dir, recursive = TRUE, showWarnings = FALSE)

# 5 PC imputations x 5 DHS imputations x 40 holdout splits = 1000 validation replicates
n_splits_per_combo <- 40L
holdout_prop <- 0.10


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


# 7-1. Read DHS-imputation-specific incidence estimates and PC imputations -----

country_year_lrti_marginal_age_imp_gdp <- readRDS(
  file.path(prediction_output_dir, "country_year_lrti_marginal_age_imp_gdp.rds")
)

country_year_fever_marginal_age_imp_gdp <- readRDS(
  file.path(prediction_output_dir, "country_year_fever_marginal_age_imp_gdp.rds")
)

UNI <- readRDS(file.path(out_dir, "pc_gdp_2023_incidence.rds"))

latitude <- read.csv(
  "data/metadata/latitude.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

stopifnot(".imp_dhs" %in% names(country_year_lrti_marginal_age_imp_gdp))
stopifnot(".imp_dhs" %in% names(country_year_fever_marginal_age_imp_gdp))
stopifnot(".imp_pc" %in% names(UNI))


# 7-2. Prepare latitude and PC-imputation-specific UNI data -------------------

latitude <- latitude %>%
  mutate(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    absolute_latitude = as.numeric(absolute_latitude)
  ) %>%
  filter(!is.na(Country), is.finite(absolute_latitude)) %>%
  distinct(Country, .keep_all = TRUE) %>%
  dplyr::select(Country, absolute_latitude)

UNI_model <- UNI %>%
  mutate(
    .imp_pc = as.integer(.imp_pc),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    GDP = as.numeric(GDP),
    Population_under5 = as.numeric(Population),
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
    log_GDP = log(GDP)
  ) %>%
  dplyr::select(-any_of("absolute_latitude")) %>%
  left_join(latitude, by = "Country") %>%
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

pc_imp_values <- sort(unique(UNI_model$.imp_pc))

dhs_imp_values <- sort(unique(c(
  country_year_lrti_marginal_age_imp_gdp$.imp_dhs,
  country_year_fever_marginal_age_imp_gdp$.imp_dhs
)))

stopifnot(length(pc_imp_values) == 5)
stopifnot(length(dhs_imp_values) == 5)

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
  "\nJoint imputation combinations:", n_combinations,
  "\nHoldout splits per combination:", n_splits_per_combo,
  "\nValidation replicates per candidate model:", n_validation_expected,
  "\n"
)

stopifnot(n_validation_expected == 1000)


# 7-3. Outcome and age-group definitions -------------------------------------

outcome_list <- c("LRTI", "fever")

age_group_list <- c(
  "0-11 months",
  "12-23 months",
  "24-59 months"
)

spec_name_list <- c(
  "LRTI_0_11_months",
  "LRTI_12_23_months",
  "LRTI_24_59_months",
  "fever_0_11_months",
  "fever_12_23_months",
  "fever_24_59_months"
)


# 7-4. Candidate models -------------------------------------------------------

candidate_model_keys <- c(
  "lm",
  "spline_lm",
  "gam",
  "rf_balanced",
  "rf_small_nodes",
  "extra_trees",
  "ridge",
  "enet_025",
  "enet_050",
  "enet_075",
  "lasso",
  "xgb_smooth",
  "xgb_balanced",
  "xgb_deeper"
)

if(requireNamespace("kernlab", quietly = TRUE)){
  candidate_model_keys <- c(candidate_model_keys, "svm_radial")
}

model_labels <- c(
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
  xgb_smooth = "XGBoost, smooth",
  xgb_balanced = "XGBoost, balanced",
  xgb_deeper = "XGBoost, deeper",
  svm_radial = "Radial SVM"
)


# 7-5. Validation across 5 PC x 5 DHS x 40 holdout splits --------------------

validation_result_list <- vector("list", length(spec_name_list))
validation_prediction_list <- vector("list", length(spec_name_list))
validation_design_list <- vector("list", length(spec_name_list))


for(spec_index_i in seq_along(spec_name_list)){
  
  spec_name_i <- spec_name_list[spec_index_i]
  outcome_i <- outcome_list[ceiling(spec_index_i / 3)]
  age_group_i <- age_group_list[((spec_index_i - 1) %% 3) + 1]
  
  message("\nPreparing validation: ", spec_name_i)
  
  if(outcome_i == "LRTI"){
    outcome_data_i <- country_year_lrti_marginal_age_imp_gdp
  } else {
    outcome_data_i <- country_year_fever_marginal_age_imp_gdp
  }
  
  outcome_data_i <- outcome_data_i %>%
    filter(age_group == age_group_i) %>%
    mutate(
      .imp_dhs = as.integer(.imp_dhs),
      Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
      Country = trimws(gsub("\\\\", "", Country)),
      Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
      Year = as.numeric(Year),
      age_group = as.character(age_group)
    ) %>%
    dplyr::select(
      -any_of(c(
        "GDP", "PC1", "PC2", "Endemic",
        "absolute_latitude", "Population",
        "Population_under5", "log_GDP", "log_GDP_z"
      ))
    )
  
  
  # Create the 25 PC x DHS training datasets.
  combo_train_list_i <- vector("list", n_combinations)
  
  for(combo_index_i in seq_len(n_combinations)){
    
    pc_imp_i <- combo_grid$.imp_pc[combo_index_i]
    dhs_imp_i <- combo_grid$.imp_dhs[combo_index_i]
    
    base_dat_i <- outcome_data_i %>%
      filter(.imp_dhs == dhs_imp_i)
    
    pc_dat_i <- UNI_model %>%
      filter(.imp_pc == pc_imp_i) %>%
      dplyr::select(
        Country, Year, GDP, Population_under5,
        PC1, PC2, Endemic, absolute_latitude,
        gbd_region_from_uni = gbd_region
      )
    
    if(anyDuplicated(pc_dat_i[c("Country", "Year")])){
      stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
    }
    
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
        ),
        log_rate = log(pmax(pred_annual_rate, 1e-6)),
        log_GDP = log(GDP),
        Year_c = Year - 2010,
        Endemic = as.numeric(Endemic)
      ) %>%
      dplyr::select(-gbd_region_from_uni) %>%
      filter(
        is.finite(log_rate),
        is.finite(log_GDP),
        is.finite(GDP), GDP > 0,
        is.finite(PC1), is.finite(PC2),
        !is.na(Endemic),
        is.finite(absolute_latitude),
        !is.na(gbd_region)
      )
    
    if("n_child" %in% names(train_dat_i)){
      
      weight_raw_i <- as.numeric(train_dat_i$n_child)
      weight_valid_i <- is.finite(weight_raw_i) & weight_raw_i > 0
      
      if(any(weight_valid_i)){
        weight_raw_i[!weight_valid_i] <- median(weight_raw_i[weight_valid_i], na.rm = TRUE)
        weight_cap_i <- as.numeric(quantile(weight_raw_i, 0.95, na.rm = TRUE))
        train_dat_i$weight_model <- pmin(weight_raw_i, weight_cap_i)
      } else {
        train_dat_i$weight_model <- 1
      }
      
    } else {
      train_dat_i$weight_model <- 1
    }
    
    train_dat_i <- train_dat_i %>%
      filter(is.finite(weight_model), weight_model > 0)
    
    combo_train_list_i[[combo_index_i]] <- train_dat_i
  }
  
  
  # Use the same country universe across all 25 imputation combinations.
  country_sets_i <- lapply(combo_train_list_i, function(x) unique(x$Country))
  common_countries_i <- Reduce(intersect, country_sets_i)
  
  if(length(common_countries_i) == 0){
    stop("No common countries across all MI combinations for ", spec_name_i)
  }
  
  combo_train_list_i <- lapply(
    combo_train_list_i,
    function(x) x %>% filter(Country %in% common_countries_i)
  )
  
  country_meta_i <- combo_train_list_i[[1]] %>%
    distinct(Country, Endemic) %>%
    filter(Country %in% common_countries_i)
  
  if(anyDuplicated(country_meta_i$Country)){
    stop("Non-unique endemicity classification for ", spec_name_i)
  }
  
  countries_i <- sort(unique(country_meta_i$Country))
  n_holdout_i <- max(1, ceiling(length(countries_i) * holdout_prop))
  
  
  # Create 40 holdout-country sets ONCE.
  # These exact same 40 sets are reused for all 25 PC x DHS combinations.
  holdout_list_i <- vector("list", n_splits_per_combo)
  
  for(split_id_i in seq_len(n_splits_per_combo)){
    
    set.seed(spec_index_i * 100000 + split_id_i)
    holdout_countries_i <- character(0)
    
    for(attempt_i in seq_len(1000)){
      
      holdout_try_i <- sample(countries_i, n_holdout_i)
      
      endemic_train_try_i <- country_meta_i %>%
        filter(!Country %in% holdout_try_i) %>%
        pull(Endemic) %>%
        unique()
      
      if(setequal(unique(country_meta_i$Endemic), endemic_train_try_i)){
        holdout_countries_i <- holdout_try_i
        break
      }
    }
    
    if(length(holdout_countries_i) == 0){
      stop("Could not create valid holdout for ", spec_name_i, ", split ", split_id_i)
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
  
  
  # 25 joint MI states x 40 common country splits = 1000 validation replicates.
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
          ", PC MI ", pc_imp_i,
          ", DHS MI ", dhs_imp_i,
          ", split ", split_id_i
        )
      }
      
      log_gdp_mean_i <- mean(train_split_i$log_GDP, na.rm = TRUE)
      log_gdp_sd_i <- sd(train_split_i$log_GDP, na.rm = TRUE)
      
      if(!is.finite(log_gdp_sd_i) || log_gdp_sd_i == 0){
        log_gdp_sd_i <- 1
      }
      
      train_split_i <- train_split_i %>%
        mutate(log_GDP_z = (log_GDP - log_gdp_mean_i) / log_gdp_sd_i)
      
      test_split_i <- test_split_i %>%
        mutate(log_GDP_z = (log_GDP - log_gdp_mean_i) / log_gdp_sd_i)
      
      
      prediction_list_i <- setNames(
        vector("list", length(candidate_model_keys)),
        candidate_model_keys
      )
      
      
      # Linear model ----------------------------------------------------------
      
      fit_i <- tryCatch(
        lm(
          log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
          data = train_split_i,
          weights = weight_model
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$lm <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = test_split_i)),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      # Spline linear model ---------------------------------------------------
      
      fit_i <- tryCatch(
        lm(
          log_rate ~
            splines::ns(PC1, df = 3) +
            splines::ns(PC2, df = 3) +
            splines::ns(log_GDP_z, df = 3) +
            splines::ns(Year_c, df = 4) +
            Endemic +
            splines::ns(absolute_latitude, df = 3),
          data = train_split_i,
          weights = weight_model
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$spline_lm <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = test_split_i)),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      # GAM -------------------------------------------------------------------
      
      fit_i <- tryCatch(
        mgcv::gam(
          log_rate ~
            s(PC1, k = 4) +
            s(PC2, k = 4) +
            s(log_GDP_z, k = 4) +
            s(Year_c, k = 5) +
            Endemic +
            s(absolute_latitude, k = 4),
          data = train_split_i,
          weights = weight_model,
          method = "REML"
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$gam <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = test_split_i, type = "response")),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      # Random forest ---------------------------------------------------------
      
      fit_i <- tryCatch(
        ranger::ranger(
          log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
          data = train_split_i,
          case.weights = train_split_i$weight_model,
          num.trees = 1000,
          mtry = 3,
          min.node.size = 5,
          seed = 123
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$rf_balanced <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, data = test_split_i)$predictions),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      fit_i <- tryCatch(
        ranger::ranger(
          log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
          data = train_split_i,
          case.weights = train_split_i$weight_model,
          num.trees = 1000,
          mtry = 4,
          min.node.size = 2,
          seed = 123
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$rf_small_nodes <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, data = test_split_i)$predictions),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      fit_i <- tryCatch(
        ranger::ranger(
          log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
          data = train_split_i,
          case.weights = train_split_i$weight_model,
          num.trees = 1500,
          mtry = 4,
          min.node.size = 3,
          splitrule = "extratrees",
          seed = 123
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$extra_trees <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, data = test_split_i)$predictions),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      # Model matrix for glmnet and XGBoost ----------------------------------
      
      combined_matrix_i <- bind_rows(
        train_split_i %>%
          transmute(
            PC1, PC2, log_GDP_z, Year_c,
            Endemic, absolute_latitude, .set = "train"
          ),
        test_split_i %>%
          transmute(
            PC1, PC2, log_GDP_z, Year_c,
            Endemic, absolute_latitude, .set = "test"
          )
      )
      
      x_all_i <- model.matrix(
        ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
        data = combined_matrix_i
      )[, -1, drop = FALSE]
      
      x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
      x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
      
      nfolds_i <- min(5, nrow(train_split_i))
      foldid_i <- ((seq_len(nrow(train_split_i)) - 1) %% nfolds_i) + 1
      
      
      # Ridge / elastic net / lasso ------------------------------------------
      
      glmnet_alpha_map_i <- c(
        ridge = 0,
        enet_025 = 0.25,
        enet_050 = 0.50,
        enet_075 = 0.75,
        lasso = 1
      )
      
      for(model_key_i in names(glmnet_alpha_map_i)){
        
        fit_i <- tryCatch(
          glmnet::cv.glmnet(
            x = x_train_i,
            y = train_split_i$log_rate,
            weights = train_split_i$weight_model,
            alpha = glmnet_alpha_map_i[model_key_i],
            family = "gaussian",
            nfolds = nfolds_i,
            foldid = foldid_i
          ),
          error = function(e) NULL
        )
        
        prediction_list_i[[model_key_i]] <- if(is.null(fit_i)){
          rep(NA_real_, nrow(test_split_i))
        } else {
          tryCatch(
            as.numeric(predict(fit_i, newx = x_test_i, s = "lambda.min")),
            error = function(e) rep(NA_real_, nrow(test_split_i))
          )
        }
      }
      
      
      # XGBoost ---------------------------------------------------------------
      
      dtrain_i <- xgboost::xgb.DMatrix(
        data = x_train_i,
        label = train_split_i$log_rate,
        weight = train_split_i$weight_model
      )
      
      fit_i <- tryCatch(
        xgboost::xgb.train(
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
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$xgb_smooth <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = x_test_i)),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      fit_i <- tryCatch(
        xgboost::xgb.train(
          params = list(
            objective = "reg:squarederror",
            eval_metric = "rmse",
            max_depth = 3,
            eta = 0.03,
            subsample = 0.8,
            colsample_bytree = 0.8,
            min_child_weight = 3,
            seed = 123
          ),
          data = dtrain_i,
          nrounds = 300,
          verbose = 0
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$xgb_balanced <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = x_test_i)),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      fit_i <- tryCatch(
        xgboost::xgb.train(
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
        ),
        error = function(e) NULL
      )
      
      prediction_list_i$xgb_deeper <- if(is.null(fit_i)){
        rep(NA_real_, nrow(test_split_i))
      } else {
        tryCatch(
          as.numeric(predict(fit_i, newdata = x_test_i)),
          error = function(e) rep(NA_real_, nrow(test_split_i))
        )
      }
      
      
      # Radial SVM ------------------------------------------------------------
      
      if("svm_radial" %in% candidate_model_keys){
        
        fit_i <- tryCatch(
          kernlab::ksvm(
            x = x_train_i,
            y = train_split_i$log_rate,
            type = "eps-svr",
            kernel = "rbfdot",
            kpar = "automatic",
            C = 2,
            scaled = TRUE
          ),
          error = function(e) NULL
        )
        
        prediction_list_i$svm_radial <- if(is.null(fit_i)){
          rep(NA_real_, nrow(test_split_i))
        } else {
          tryCatch(
            as.numeric(kernlab::predict(fit_i, x_test_i)),
            error = function(e) rep(NA_real_, nrow(test_split_i))
          )
        }
      }
      
      
      # Validation metrics ----------------------------------------------------
      
      metric_list_i <- vector("list", length(candidate_model_keys))
      prediction_long_list_i <- vector("list", length(candidate_model_keys))
      
      for(model_index_i in seq_along(candidate_model_keys)){
        
        model_key_i <- candidate_model_keys[model_index_i]
        pred_log_i <- prediction_list_i[[model_key_i]]
        
        valid_i <- is.finite(test_split_i$log_rate) & is.finite(pred_log_i)
        
        if(!any(valid_i)){
          
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels[model_key_i]),
            rmse_log = NA_real_,
            mae_log = NA_real_,
            bias_log = NA_real_,
            abs_bias_log = NA_real_,
            rmse_rate = NA_real_,
            mae_rate = NA_real_,
            cor_rate = NA_real_,
            n_eval = 0
          )
          
        } else {
          
          observed_log_i <- test_split_i$log_rate[valid_i]
          predicted_log_i <- pred_log_i[valid_i]
          observed_rate_i <- exp(observed_log_i)
          predicted_rate_i <- exp(predicted_log_i)
          
          cor_rate_i <- if(
            length(observed_rate_i) >= 2 &&
            sd(observed_rate_i) > 0 &&
            sd(predicted_rate_i) > 0
          ){
            cor(observed_rate_i, predicted_rate_i)
          } else {
            NA_real_
          }
          
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels[model_key_i]),
            rmse_log = sqrt(mean((predicted_log_i - observed_log_i)^2)),
            mae_log = mean(abs(predicted_log_i - observed_log_i)),
            bias_log = mean(predicted_log_i - observed_log_i),
            abs_bias_log = abs(mean(predicted_log_i - observed_log_i)),
            rmse_rate = sqrt(mean((predicted_rate_i - observed_rate_i)^2)),
            mae_rate = mean(abs(predicted_rate_i - observed_rate_i)),
            cor_rate = cor_rate_i,
            n_eval = sum(valid_i)
          )
        }
        
        
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
            model = unname(model_labels[model_key_i]),
            observed_log_rate = log_rate,
            predicted_log_rate = pred_log_i,
            observed_rate = exp(log_rate),
            predicted_rate = exp(pred_log_i)
          )
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


# 7-6. Overall validation summary and model selection -------------------------

validation_results_marginal_re0 <- bind_rows(validation_result_list)
validation_predictions_marginal_re0 <- bind_rows(validation_prediction_list)
validation_design_marginal_re0 <- bind_rows(validation_design_list)


# Average performance across all:
# 5 PC MI x 5 DHS MI x 40 holdout splits = 1000 replicates/model.
validation_summary_marginal_re0 <- validation_results_marginal_re0 %>%
  group_by(outcome, age_group, spec_name, model_key, model) %>%
  summarise(
    mean_rmse_log = mean(rmse_log, na.rm = TRUE),
    median_rmse_log = median(rmse_log, na.rm = TRUE),
    mean_mae_log = mean(mae_log, na.rm = TRUE),
    mean_bias_log = mean(bias_log, na.rm = TRUE),
    mean_abs_bias_log = mean(abs_bias_log, na.rm = TRUE),
    mean_rmse_rate = mean(rmse_rate, na.rm = TRUE),
    mean_mae_rate = mean(mae_rate, na.rm = TRUE),
    mean_cor_rate = mean(cor_rate, na.rm = TRUE),
    n_valid_replicates = sum(is.finite(rmse_log)),
    total_replicates = n(),
    .groups = "drop"
  ) %>%
  mutate(valid_fraction = n_valid_replicates / total_replicates) %>%
  filter(n_valid_replicates > 0) %>%
  group_by(outcome, age_group, spec_name) %>%
  mutate(
    rmse_rank = rank(mean_rmse_log, ties.method = "min"),
    bias_rank = rank(mean_abs_bias_log, ties.method = "min"),
    combined_rank = rmse_rank + bias_rank
  ) %>%
  ungroup() %>%
  arrange(outcome, age_group, combined_rank, mean_rmse_log)


model_selection_marginal_re0 <- validation_summary_marginal_re0 %>%
  group_by(outcome, age_group, spec_name) %>%
  arrange(
    combined_rank,
    mean_rmse_log,
    mean_abs_bias_log,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    selection_rule = paste0(
      "Best combined RMSE and absolute-bias rank across ",
      n_validation_expected,
      " validation replicates (5 PC MI x 5 DHS MI x 40 holdout splits)"
    )
  )


# 7-7. Model-selection stability across the 25 joint MI combinations ----------

validation_combo_summary_marginal_re0 <- validation_results_marginal_re0 %>%
  group_by(
    outcome, age_group, spec_name,
    .imp_pc, .imp_dhs,
    model_key, model
  ) %>%
  summarise(
    mean_rmse_log = mean(rmse_log, na.rm = TRUE),
    mean_abs_bias_log = mean(abs_bias_log, na.rm = TRUE),
    mean_rmse_rate = mean(rmse_rate, na.rm = TRUE),
    mean_mae_rate = mean(mae_rate, na.rm = TRUE),
    n_valid_splits = sum(is.finite(rmse_log)),
    .groups = "drop"
  ) %>%
  group_by(outcome, age_group, spec_name, .imp_pc, .imp_dhs) %>%
  mutate(
    rmse_rank = rank(mean_rmse_log, ties.method = "min"),
    bias_rank = rank(mean_abs_bias_log, ties.method = "min"),
    combined_rank = rmse_rank + bias_rank
  ) %>%
  ungroup()


model_selection_by_mi_marginal_re0 <- validation_combo_summary_marginal_re0 %>%
  group_by(outcome, age_group, spec_name, .imp_pc, .imp_dhs) %>%
  arrange(
    combined_rank,
    mean_rmse_log,
    mean_abs_bias_log,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup()


model_selection_stability_marginal_re0 <- model_selection_by_mi_marginal_re0 %>%
  count(
    outcome, age_group, spec_name,
    model_key, model,
    name = "n_best_joint_MI"
  ) %>%
  group_by(outcome, age_group, spec_name) %>%
  mutate(proportion_best_joint_MI = n_best_joint_MI / n_combinations) %>%
  ungroup() %>%
  arrange(outcome, age_group, spec_name, desc(n_best_joint_MI))


# 7-8. Save validation/model-selection outputs --------------------------------

saveRDS(
  validation_results_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_results.rds")
)

saveRDS(
  validation_predictions_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_predictions.rds")
)

saveRDS(
  validation_design_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_design.rds")
)

saveRDS(
  validation_summary_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_summary.rds")
)

saveRDS(
  model_selection_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_selected_models.rds")
)

saveRDS(
  validation_combo_summary_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_by_joint_MI.rds")
)

saveRDS(
  model_selection_by_mi_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_selected_models_by_joint_MI.rds")
)

saveRDS(
  model_selection_stability_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_model_selection_stability.rds")
)


write.csv(
  validation_summary_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_summary.csv"),
  row.names = FALSE
)

write.csv(
  model_selection_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_selected_models.csv"),
  row.names = FALSE
)

write.csv(
  validation_design_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_design.csv"),
  row.names = FALSE
)

write.csv(
  model_selection_stability_marginal_re0,
  file.path(model_search_dir, "marginal_RE0_age_specific_model_selection_stability.csv"),
  row.names = FALSE
)


cat("\nSelected models:\n")
print(model_selection_marginal_re0, n = Inf)

cat("\nModel-selection stability across 25 joint MI combinations:\n")
print(model_selection_stability_marginal_re0, n = Inf)


# 7-9. Validation diagnostic plots --------------------------------------------

model_search_dir <- file.path(
  "outputs/pipelines",
  "marginal_RE0_full_model_search_outputs"
)

validation_results_marginal_re0 <- readRDS(
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_results.rds")
)

validation_predictions_marginal_re0 <- readRDS(
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_predictions.rds")
)

validation_summary_marginal_re0 <- readRDS(
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_summary.rds")
)

model_selection_marginal_re0 <- readRDS(
  file.path(model_search_dir, "marginal_RE0_age_specific_selected_models.rds")
)


outcome_list <- c("LRTI", "fever")

age_group_list <- c(
  "0-11 months",
  "12-23 months",
  "24-59 months"
)

spec_name_list <- c(
  "LRTI_0_11_months",
  "LRTI_12_23_months",
  "LRTI_24_59_months",
  "fever_0_11_months",
  "fever_12_23_months",
  "fever_24_59_months"
)

candidate_model_keys <- c(
  "lm",
  "spline_lm",
  "gam",
  "rf_balanced",
  "rf_small_nodes",
  "extra_trees",
  "ridge",
  "enet_025",
  "enet_050",
  "enet_075",
  "lasso",
  "xgb_smooth",
  "xgb_balanced",
  "xgb_deeper"
)

if("svm_radial" %in% unique(validation_predictions_marginal_re0$model_key)){
  candidate_model_keys <- c(candidate_model_keys, "svm_radial")
}

fig_dir_validation_i <- "figures/main_analysis/inc_validation_predictions"

dir.create(
  fig_dir_validation_i,
  recursive = TRUE,
  showWarnings = FALSE
)

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


for(spec_index_i in seq_along(spec_name_list)){
  
  spec_name_i <- spec_name_list[spec_index_i]
  
  message("Creating validation plots: ", spec_name_i)
  
  
  # Average held-out predictions across all joint MI states and splits
  # for descriptive validation plots only.
  validation_plot_i <- validation_predictions_marginal_re0 %>%
    filter(
      spec_name == spec_name_i,
      is.finite(observed_log_rate),
      is.finite(predicted_log_rate),
      is.finite(observed_rate),
      is.finite(predicted_rate)
    ) %>%
    group_by(
      outcome, age_group, spec_name,
      Country, Year, gbd_region,
      model_key, model
    ) %>%
    summarise(
      observed_log_rate = mean(observed_log_rate, na.rm = TRUE),
      predicted_log_rate = mean(predicted_log_rate, na.rm = TRUE),
      observed_rate = mean(observed_rate, na.rm = TRUE),
      predicted_rate = mean(predicted_rate, na.rm = TRUE),
      residual_log_rate = mean(predicted_log_rate - observed_log_rate, na.rm = TRUE),
      residual_rate = mean(predicted_rate - observed_rate, na.rm = TRUE),
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
  
  rate_min_i <- 0
  rate_max_i_LRTI <- 1
  rate_max_i_fever <- 6
  
  rate_axis_at_i_LRTI <- seq(rate_min_i, rate_max_i_LRTI, length.out = 5)
  rate_axis_at_i_fever <- seq(rate_min_i, rate_max_i_fever, length.out = 5)
  
  
  pdf_file_i <- file.path(
    fig_dir_validation_i,
    paste0(spec_name_i, "_all_models_validation_diagnostics.pdf")
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
    cex.axis = 0.55,
    cex.lab = 0.55,
    cex.main = 0.55
  )
  
  
  for(model_index_i in seq_along(model_keys_i)){
    
    model_key_i <- model_keys_i[model_index_i]
    panel_column_i <- ((model_index_i - 1) %% 4) + 1
    panel_row_i <- ceiling(model_index_i / 4)
    
    ylab_i <- if(panel_column_i == 1){
      "Held-out predicted annual rate"
    } else {
      ""
    }
    
    xlab_i <- if(panel_row_i == 4){
      "Held-out observed annual rate"
    } else {
      ""
    }
    
    plot_data_i <- validation_plot_i %>%
      filter(model_key == model_key_i)
    
    model_name_i <- unique(plot_data_i$model)[1]
    
    valid_i <- is.finite(plot_data_i$observed_rate) &
      is.finite(plot_data_i$predicted_rate)
    
    if(sum(valid_i) < 2){
      plot.new()
      title(main = model_name_i)
      text(
        0.5,
        0.5,
        labels = "No finite validation predictions",
        cex = 0.55
      )
      next
    }
    
    plot_data_i <- plot_data_i[valid_i, , drop = FALSE]
    
    point_colors_i <- region_colors_i[
      as.character(plot_data_i$gbd_region)
    ]
    
    point_colors_i[is.na(point_colors_i)] <- "grey60"
    
    
    if(spec_index_i < 4){
      
      plot(
        plot_data_i$observed_rate,
        plot_data_i$predicted_rate,
        col = point_colors_i,
        pch = 16,
        cex = 0.55,
        xlim = c(rate_min_i, rate_max_i_LRTI),
        ylim = c(rate_min_i, rate_max_i_LRTI),
        asp = 1,
        axes = FALSE,
        ann = FALSE,
        xaxs = "i",
        yaxs = "i"
      )
      
      axis(
        side = 1,
        at = rate_axis_at_i_LRTI,
        labels = FALSE,
        pos = rate_min_i,
        tck = -0.03,
        lwd = 0.8,
        lwd.ticks = 0.8
      )
      
      axis(
        side = 2,
        at = rate_axis_at_i_LRTI,
        labels = FALSE,
        pos = rate_min_i,
        tck = -0.03,
        lwd = 0.8,
        lwd.ticks = 0.8
      )
      
      text(
        x = rate_axis_at_i_LRTI,
        y = rate_min_i - 0.055 * (rate_max_i_LRTI - rate_min_i),
        labels = format(
          rate_axis_at_i_LRTI,
          trim = TRUE,
          scientific = FALSE,
          digits = 3
        ),
        xpd = TRUE,
        cex = 0.55
      )
      
      text(
        x = rate_min_i - 0.055 * (rate_max_i_LRTI - rate_min_i),
        y = rate_axis_at_i_LRTI,
        labels = format(
          rate_axis_at_i_LRTI,
          trim = TRUE,
          scientific = FALSE,
          digits = 3
        ),
        adj = 1,
        xpd = TRUE,
        cex = 0.55
      )
      
    } else {
      
      plot(
        plot_data_i$observed_rate,
        plot_data_i$predicted_rate,
        col = point_colors_i,
        pch = 16,
        cex = 0.55,
        xlim = c(rate_min_i, rate_max_i_fever),
        ylim = c(rate_min_i, rate_max_i_fever),
        asp = 1,
        axes = FALSE,
        ann = FALSE,
        xaxs = "i",
        yaxs = "i"
      )
      
      axis(
        side = 1,
        at = rate_axis_at_i_fever,
        labels = FALSE,
        pos = rate_min_i,
        tck = -0.03,
        lwd = 0.8,
        lwd.ticks = 0.8
      )
      
      axis(
        side = 2,
        at = rate_axis_at_i_fever,
        labels = FALSE,
        pos = rate_min_i,
        tck = -0.03,
        lwd = 0.8,
        lwd.ticks = 0.8
      )
      
      text(
        x = rate_axis_at_i_fever,
        y = rate_min_i - 0.055 * (rate_max_i_fever - rate_min_i),
        labels = format(
          rate_axis_at_i_fever,
          trim = TRUE,
          scientific = FALSE,
          digits = 3
        ),
        xpd = TRUE,
        cex = 0.55
      )
      
      text(
        x = rate_min_i - 0.055 * (rate_max_i_fever - rate_min_i),
        y = rate_axis_at_i_fever,
        labels = format(
          rate_axis_at_i_fever,
          trim = TRUE,
          scientific = FALSE,
          digits = 3
        ),
        adj = 1,
        xpd = TRUE,
        cex = 0.55
      )
    }
    
    
    if(xlab_i != ""){
      mtext(xlab_i, side = 1, line = 2.1, cex = 0.55)
    }
    
    if(ylab_i != ""){
      mtext(ylab_i, side = 2, line = 2.1, cex = 0.55)
    }
    
    title(
      main = paste0(
        outcome_i,
        ", ",
        age_group_i,
        ",\n",
        model_name_i
      ),
      cex.main = 0.6,
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
    pt.cex = 0.55,
    bty = "n",
    cex = 0.55,
    title = "GBD region"
  )
  
  dev.off()
  
  message("PDF written: ", pdf_file_i)
}


saveRDS(
  validation_plot_object_list_i,
  file.path(model_search_dir, "marginal_RE0_age_specific_validation_plot_objects.rds")
)












# _____________________________________________ ----------
# 8. Age-specific marginal RE0 uncertainty propagation ------------------------
###### (LRTI: elastic net alpha 0.25, fever: weighted lm) -----------

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

out_dir <- "outputs/pipelines"
uncertainty_out_dir <- file.path(out_dir, "age_specific_selected_model_uncertainty")
dir.create(uncertainty_out_dir, recursive = TRUE, showWarnings = FALSE)

draw_chunk_size <- 50L
n_draw_per_combo <- 40L
holdout_prop <- 0.10


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

age_group_list <- c(
  "0-11 months",
  "12-23 months",
  "24-59 months"
)

age_key_list <- c(
  "age_0_11_mth",
  "age_12_23_mth",
  "age_24_59_mth"
)


# 8-1. Read DHS MI, PC MI, population, and latitude ---------------------------

imp <- readRDS(file.path(out_dir, "incidence_mice_imp_m5_maxit10_with_bednet_tropical.rds"))
fit_lrti_mi <- readRDS(file.path(out_dir, "(GBD_RE)fit_lrti_mi_gps.rds"))
fit_fever_mi <- readRDS(file.path(out_dir, "(GBD_RE)fit_fever_mi_gps.rds"))
UNI_raw <- readRDS(file.path(out_dir, "pc_gdp_2023_incidence.rds"))

df_population <- read.csv("data/metadata/Population_by_age.csv", stringsAsFactors = FALSE)
latitude <- read.csv("data/metadata/latitude.csv", fileEncoding = "latin1", stringsAsFactors = FALSE)

stopifnot(".imp_pc" %in% names(UNI_raw))
stopifnot(length(fit_lrti_mi$analyses) == imp$m)
stopifnot(length(fit_fever_mi$analyses) == imp$m)


# 8-2. Age-specific population ------------------------------------------------

df_population <- df_population %>%
  mutate(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    age_0_11_mth = as.numeric(gsub(",", "", as.character(Age_0))),
    age_12_23_mth = as.numeric(gsub(",", "", as.character(Age_1))),
    age_24_59_mth =
      as.numeric(gsub(",", "", as.character(Age_2))) +
      as.numeric(gsub(",", "", as.character(Age_3))) +
      as.numeric(gsub(",", "", as.character(Age_4)))
  )

age_population_long <- df_population %>%
  dplyr::select(Country, Year, age_0_11_mth, age_12_23_mth, age_24_59_mth) %>%
  pivot_longer(cols = starts_with("age_"), names_to = "age_key", values_to = "age_population") %>%
  mutate(
    age_group = dplyr::recode(
      age_key,
      "age_0_11_mth" = "0-11 months",
      "age_12_23_mth" = "12-23 months",
      "age_24_59_mth" = "24-59 months"
    )
  ) %>%
  filter(is.finite(age_population), age_population > 0)


# 8-3. Latitude and imputation-specific UNI -----------------------------------

latitude <- latitude %>%
  mutate(
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    absolute_latitude = as.numeric(absolute_latitude)
  ) %>%
  filter(!is.na(Country), is.finite(absolute_latitude)) %>%
  distinct(Country, .keep_all = TRUE) %>%
  dplyr::select(Country, absolute_latitude)

UNI_model <- UNI_raw %>%
  mutate(
    .imp_pc = as.integer(.imp_pc),
    Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
    Country = trimws(gsub("\\\\", "", Country)),
    Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
    Year = as.numeric(Year),
    GDP = as.numeric(GDP),
    Population_under5 = as.numeric(Population),
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
    log_GDP = log(GDP)
  ) %>%
  dplyr::select(-any_of("absolute_latitude")) %>%
  left_join(latitude, by = "Country") %>%
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

pc_imp_values <- sort(unique(UNI_model$.imp_pc))
dhs_imp_values <- seq_len(imp$m)

stopifnot(length(pc_imp_values) == 5)
stopifnot(length(dhs_imp_values) == 5)


# 8-4. Balanced 5 PC x 5 DHS x 40 draw design --------------------------------

draw_design <- tidyr::expand_grid(
  .draw_within_combo = seq_len(n_draw_per_combo),
  .imp_pc = pc_imp_values,
  .imp_dhs = dhs_imp_values
) %>%
  mutate(draw_id = row_number()) %>%
  dplyr::select(draw_id, .draw_within_combo, .imp_pc, .imp_dhs)

n_draw <- nrow(draw_design)
stopifnot(n_draw == 1000)

draw_balance_check <- draw_design %>%
  count(.imp_pc, .imp_dhs, name = "n_draw")

stopifnot(all(draw_balance_check$n_draw == 40))
print(draw_balance_check)

saveRDS(
  draw_design,
  file.path(uncertainty_out_dir, "uncertainty_draw_design_5PC_5DHS_40draw.rds")
)

write.csv(
  draw_design,
  file.path(uncertainty_out_dir, "uncertainty_draw_design_5PC_5DHS_40draw.csv"),
  row.names = FALSE
)


# 8-5. Robust covariance matrices for DHS incidence models --------------------

get_robust_vcov_list <- function(fit_mi) {
  
  lapply(seq_along(fit_mi$analyses), function(i) {
    
    fit_i <- fit_mi$analyses[[i]]
    beta_names_i <- names(glmmTMB::fixef(fit_i)$cond)
    cluster_i <- droplevels(factor(model.frame(fit_i)$gbd_region))
    
    vcov_i <- sandwich::vcovHC(
      fit_i,
      type = "HC0",
      cluster = cluster_i,
      full = FALSE
    )
    
    vcov_i <- as.matrix(vcov_i)
    vcov_i <- vcov_i[beta_names_i, beta_names_i, drop = FALSE]
    vcov_i <- (vcov_i + t(vcov_i)) / 2
    
    if(any(!is.finite(vcov_i))){
      stop("Non-finite robust covariance values for DHS imputation ", i)
    }
    
    eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
    eigen_scale_i <- max(max(abs(eigen_i)), .Machine$double.eps)
    
    if(min(eigen_i) < -1e-6 * eigen_scale_i){
      stop(
        "Robust covariance is not positive semidefinite for DHS imputation ",
        i,
        "; minimum eigenvalue = ",
        signif(min(eigen_i), 5)
      )
    }
    
    vcov_i
  })
}

robust_vcov_lrti_list <- get_robust_vcov_list(fit_lrti_mi)
robust_vcov_fever_list <- get_robust_vcov_list(fit_fever_mi)


robust_vcov_check <- bind_rows(
  lapply(c("LRTI", "fever"), function(outcome_i) {
    
    fit_mi_i <- if(outcome_i == "LRTI") fit_lrti_mi else fit_fever_mi
    vcov_list_i <- if(outcome_i == "LRTI") robust_vcov_lrti_list else robust_vcov_fever_list
    
    bind_rows(
      lapply(seq_along(vcov_list_i), function(i) {
        
        vcov_i <- vcov_list_i[[i]]
        eigen_i <- eigen(vcov_i, symmetric = TRUE, only.values = TRUE)$values
        
        tibble(
          outcome = outcome_i,
          .imp_dhs = i,
          n_gbd_clusters = nlevels(droplevels(factor(model.frame(fit_mi_i$analyses[[i]])$gbd_region))),
          n_coefficients = nrow(vcov_i),
          covariance_rank = qr(vcov_i)$rank,
          minimum_eigenvalue = min(eigen_i),
          maximum_eigenvalue = max(eigen_i)
        )
      })
    )
  })
)

print(robust_vcov_check)

write.csv(
  robust_vcov_check,
  file.path(uncertainty_out_dir, "DHS_robust_vcov_check.csv"),
  row.names = FALSE
)


# 8-6. Function for 1000 uncertainty draws ------------------------------------

run_uncertainty_draws <- function(
    outcome_i,
    fit_mi_i,
    robust_vcov_list_i,
    model_key_i,
    model_label_i,
    alpha_i,
    file_tag_i,
    seed_base_i
){
  
  chunk_dir_i <- file.path(uncertainty_out_dir, paste0(file_tag_i, "_draw_chunks"))
  holdout_chunk_dir_i <- file.path(uncertainty_out_dir, paste0(file_tag_i, "_holdout_chunks"))
  dir.create(chunk_dir_i, recursive = TRUE, showWarnings = FALSE)
  dir.create(holdout_chunk_dir_i, recursive = TRUE, showWarnings = FALSE)
  
  draw_chunks_i <- split(
    draw_design$draw_id,
    ceiling(draw_design$draw_id / draw_chunk_size)
  )
  
  chunk_files_i <- character(length(draw_chunks_i))
  holdout_chunk_files_i <- character(length(draw_chunks_i))
  
  
  for(chunk_index_i in seq_along(draw_chunks_i)){
    
    draw_ids_i <- draw_chunks_i[[chunk_index_i]]
    chunk_result_list_i <- vector("list", length(draw_ids_i) * 3)
    holdout_result_list_i <- vector("list", length(draw_ids_i) * 3)
    result_index_i <- 0
    holdout_result_index_i <- 0
    
    
    for(draw_id_i in draw_ids_i){
      
      design_i <- draw_design %>% filter(draw_id == draw_id_i)
      
      dhs_imp_i <- design_i$.imp_dhs
      pc_imp_i <- design_i$.imp_pc
      draw_within_combo_i <- design_i$.draw_within_combo
      
      
      # DHS imputation-specific incidence-model coefficient draw ---------------
      
      set.seed(seed_base_i + draw_id_i)
      
      dat_i <- mice::complete(imp, dhs_imp_i)
      fit_i <- fit_mi_i$analyses[[dhs_imp_i]]
      fit_frame_i <- model.frame(fit_i)
      
      used_rows_i <- match(rownames(fit_frame_i), rownames(dat_i))
      
      if(anyNA(used_rows_i)){
        used_rows_i <- suppressWarnings(as.integer(rownames(fit_frame_i)))
      }
      
      if(length(used_rows_i) != nrow(fit_frame_i) || anyNA(used_rows_i)){
        next
      }
      
      dat_used_i <- dat_i[used_rows_i, , drop = FALSE]
      
      beta_hat_i <- glmmTMB::fixef(fit_i)$cond
      vcov_draw_i <- robust_vcov_list_i[[dhs_imp_i]]
      vcov_draw_i <- vcov_draw_i[names(beta_hat_i), names(beta_hat_i), drop = FALSE]
      
      beta_draw_i <- tryCatch(
        MASS::mvrnorm(
          n = 1,
          mu = beta_hat_i,
          Sigma = vcov_draw_i,
          tol = 1e-6
        ),
        error = function(e){
          warning(
            outcome_i,
            " coefficient draw failed for draw ",
            draw_id_i,
            ": ",
            conditionMessage(e)
          )
          rep(NA_real_, length(beta_hat_i))
        }
      )
      
      names(beta_draw_i) <- names(beta_hat_i)
      
      if(any(!is.finite(beta_draw_i))){
        next
      }
      
      X_i <- model.matrix(fit_i, component = "cond", part = "fixed")
      X_i <- X_i[, names(beta_hat_i), drop = FALSE]
      
      if(nrow(X_i) != nrow(dat_used_i)){
        next
      }
      
      dat_used_i$pred_annual_rate_draw <- exp(as.vector(X_i %*% beta_draw_i))
      
      
      # Use only the PC imputation assigned to this draw ------------------------
      
      pc_dat_i <- UNI_model %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(
          .imp_pc, Country, Year, GDP, Population_under5,
          PC1, PC2, Endemic, absolute_latitude, gbd_region
        )
      
      if(anyDuplicated(pc_dat_i[c("Country", "Year")])){
        stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
      }
      
      
      # Aggregate DHS individual predictions to country-year-age ----------------
      
      train_draw_all_age_i <- dat_used_i %>%
        mutate(
          Country = iconv(as.character(Country), from = "latin1", to = "UTF-8", sub = ""),
          Country = trimws(gsub("\\\\", "", Country)),
          Country = dplyr::recode(Country, !!!country_name_map, .default = Country),
          Year = as.numeric(Year_c + 2010),
          age_group = as.character(ch_age_group),
          age_key = dplyr::recode(
            age_group,
            "0-11 months" = "age_0_11_mth",
            "12-23 months" = "age_12_23_mth",
            "24-59 months" = "age_24_59_mth"
          ),
          gbd_region = dplyr::recode(
            as.character(gbd_region),
            "East Asia, Southeast Asia, and Oceania" = "Southeast Asia, East Asia, and Oceania",
            .default = as.character(gbd_region)
          )
        ) %>%
        filter(
          age_group %in% age_group_list,
          is.finite(sample_weight), sample_weight > 0,
          is.finite(pred_annual_rate_draw)
        ) %>%
        group_by(age_group, age_key, Country, Year, CountryYear, gbd_region) %>%
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
          Year_c = Year - 2010,
          log_rate = log(pmax(pred_annual_rate, 1e-6)),
          Endemic = as.numeric(Endemic)
        ) %>%
        filter(
          is.finite(log_rate),
          is.finite(log_GDP),
          is.finite(PC1),
          is.finite(PC2),
          !is.na(Endemic),
          is.finite(absolute_latitude)
        )
      
      
      # Model weights ----------------------------------------------------------
      
      weight_raw_i <- as.numeric(train_draw_all_age_i$n_child)
      weight_valid_i <- is.finite(weight_raw_i) & weight_raw_i > 0
      
      if(any(weight_valid_i)){
        
        weight_raw_i[!weight_valid_i] <- median(
          weight_raw_i[weight_valid_i],
          na.rm = TRUE
        )
        
        weight_cap_i <- as.numeric(
          quantile(weight_raw_i, 0.95, na.rm = TRUE)
        )
        
        train_draw_all_age_i$weight_model <- pmin(weight_raw_i, weight_cap_i)
        
      } else {
        
        train_draw_all_age_i$weight_model <- 1
      }
      
      
      # Draw-specific 90/10 country holdout validation for the selected model ---
      # For this diagnostic only, .draw_within_combo indexes 40 common holdout
      # sets, so each split is reused across the 25 PC x DHS states.
      
      common_validation_countries_i <- train_draw_all_age_i %>%
        distinct(age_group, Country) %>%
        count(Country, name = "n_age_group") %>%
        filter(n_age_group == length(age_group_list)) %>%
        pull(Country) %>%
        sort()
      
      holdout_countries_validation_i <- character(0)
      
      if(length(common_validation_countries_i) >= 3){
        country_meta_validation_i <- train_draw_all_age_i %>%
          filter(Country %in% common_validation_countries_i) %>%
          group_by(Country) %>%
          summarise(Endemic = dplyr::first(Endemic), .groups = "drop") %>%
          arrange(Country)
        
        n_holdout_validation_i <- max(
          1L,
          ceiling(nrow(country_meta_validation_i) * holdout_prop)
        )
        
        set.seed(seed_base_i + 2000000 + draw_within_combo_i)
        
        for(attempt_i in seq_len(1000)){
          holdout_try_i <- sample(
            country_meta_validation_i$Country,
            n_holdout_validation_i,
            replace = FALSE
          )
          
          endemic_train_try_i <- country_meta_validation_i %>%
            filter(!Country %in% holdout_try_i) %>%
            pull(Endemic) %>%
            as.character() %>%
            unique() %>%
            sort()
          
          endemic_all_validation_i <- country_meta_validation_i$Endemic %>%
            as.character() %>%
            unique() %>%
            sort()
          
          if(setequal(endemic_all_validation_i, endemic_train_try_i)){
            holdout_countries_validation_i <- holdout_try_i
            break
          }
        }
      }
      
      if(length(holdout_countries_validation_i) > 0){
        for(j in 1:3){
          age_group_i <- age_group_list[j]
          age_key_i <- age_key_list[j]
          
          validation_age_all_i <- train_draw_all_age_i %>%
            filter(
              age_group == age_group_i,
              Country %in% common_validation_countries_i
            )
          
          validation_train_age_i <- validation_age_all_i %>%
            filter(!Country %in% holdout_countries_validation_i)
          
          validation_test_age_i <- validation_age_all_i %>%
            filter(Country %in% holdout_countries_validation_i)
          
          if(nrow(validation_train_age_i) < 10 || nrow(validation_test_age_i) == 0){
            next
          }
          
          # Scale GDP from the validation-training subset only.
          log_gdp_mean_validation_i <- mean(
            validation_train_age_i$log_GDP,
            na.rm = TRUE
          )
          log_gdp_sd_validation_i <- sd(
            validation_train_age_i$log_GDP,
            na.rm = TRUE
          )
          if(!is.finite(log_gdp_sd_validation_i) || log_gdp_sd_validation_i == 0){
            log_gdp_sd_validation_i <- 1
          }
          
          validation_train_age_i <- validation_train_age_i %>%
            mutate(
              log_GDP_z =
                (log_GDP - log_gdp_mean_validation_i) /
                log_gdp_sd_validation_i
            )
          validation_test_age_i <- validation_test_age_i %>%
            mutate(
              log_GDP_z =
                (log_GDP - log_gdp_mean_validation_i) /
                log_gdp_sd_validation_i
            )
          
          if(outcome_i == "LRTI"){
            validation_matrix_i <- bind_rows(
              validation_train_age_i %>%
                transmute(
                  PC1, PC2, log_GDP_z, Year_c, Endemic,
                  absolute_latitude, .set = "train"
                ),
              validation_test_age_i %>%
                transmute(
                  PC1, PC2, log_GDP_z, Year_c, Endemic,
                  absolute_latitude, .set = "test"
                )
            )
            
            x_validation_all_i <- model.matrix(
              ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
              data = validation_matrix_i
            )[, -1, drop = FALSE]
            
            x_validation_train_i <-
              x_validation_all_i[validation_matrix_i$.set == "train", , drop = FALSE]
            x_validation_test_i <-
              x_validation_all_i[validation_matrix_i$.set == "test", , drop = FALSE]
            
            nfolds_validation_i <- min(5, nrow(validation_train_age_i))
            foldid_validation_i <-
              ((seq_len(nrow(validation_train_age_i)) - 1) %% nfolds_validation_i) + 1
            
            fit_validation_i <- tryCatch(
              glmnet::cv.glmnet(
                x = x_validation_train_i,
                y = validation_train_age_i$log_rate,
                weights = validation_train_age_i$weight_model,
                alpha = alpha_i,
                family = "gaussian",
                nfolds = nfolds_validation_i,
                foldid = foldid_validation_i
              ),
              error = function(e) NULL
            )
            if(is.null(fit_validation_i)) next
            
            predicted_holdout_log_i <- tryCatch(
              as.numeric(
                predict(
                  fit_validation_i,
                  newx = x_validation_test_i,
                  s = "lambda.min"
                )
              ),
              error = function(e) rep(NA_real_, nrow(validation_test_age_i))
            )
            lambda_validation_i <- fit_validation_i$lambda.min
            
          } else {
            fit_validation_i <- tryCatch(
              lm(
                log_rate ~
                  PC1 + PC2 + log_GDP_z + Year_c +
                  Endemic + absolute_latitude,
                data = validation_train_age_i,
                weights = weight_model
              ),
              error = function(e) NULL
            )
            if(is.null(fit_validation_i)) next
            
            predicted_holdout_log_i <- tryCatch(
              as.numeric(
                predict(
                  fit_validation_i,
                  newdata = validation_test_age_i
                )
              ),
              error = function(e) rep(NA_real_, nrow(validation_test_age_i))
            )
            lambda_validation_i <- NA_real_
          }
          
          validation_test_age_i$predicted_log_rate <- predicted_holdout_log_i
          validation_test_age_i <- validation_test_age_i %>%
            filter(is.finite(predicted_log_rate))
          if(nrow(validation_test_age_i) == 0) next
          
          holdout_result_index_i <- holdout_result_index_i + 1
          holdout_result_list_i[[holdout_result_index_i]] <-
            validation_test_age_i %>%
            transmute(
              draw_id = draw_id_i,
              .draw_within_combo = draw_within_combo_i,
              .imp_pc = pc_imp_i,
              .imp_dhs = dhs_imp_i,
              outcome = outcome_i,
              prediction_type = "country_holdout",
              model_key = model_key_i,
              model = model_label_i,
              alpha = alpha_i,
              lambda_selected = lambda_validation_i,
              Country, Year, CountryYear, gbd_region, age_group, age_key,
              n_child, weight_model,
              n_train_country = n_distinct(validation_train_age_i$Country),
              n_holdout_country = n_distinct(validation_test_age_i$Country),
              heldout_estimated_log_rate = log_rate,
              predicted_log_rate,
              heldout_estimated_rate = pred_annual_rate,
              predicted_rate = exp(predicted_log_rate),
              residual_log_rate =
                predicted_log_rate - heldout_estimated_log_rate,
              residual_rate =
                predicted_rate - heldout_estimated_rate
            )
        }
      }
      # Country bootstrap for extrapolation-model uncertainty ------------------
      
      countries_i <- unique(train_draw_all_age_i$Country)
      endemic_levels_i <- sort(unique(as.character(train_draw_all_age_i$Endemic)))
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
        
        if(setequal(endemic_levels_i, boot_endemic_i)){
          boot_countries_i <- boot_try_i
          break
        }
      }
      
      if(length(boot_countries_i) == 0){
        next
      }
      
      boot_country_count_i <- as.data.frame(
        table(boot_countries_i),
        stringsAsFactors = FALSE
      )
      
      names(boot_country_count_i) <- c("Country", "bootstrap_count")
      boot_country_count_i$Country <- as.character(boot_country_count_i$Country)
      boot_country_count_i$bootstrap_count <- as.numeric(boot_country_count_i$bootstrap_count)
      
      
      # Fit selected extrapolation model separately for three age groups --------
      
      for(j in 1:3){
        
        age_group_i <- age_group_list[j]
        age_key_i <- age_key_list[j]
        
        train_draw_age_i <- train_draw_all_age_i %>%
          filter(age_group == age_group_i) %>%
          inner_join(boot_country_count_i, by = "Country") %>%
          mutate(weight_model_boot = weight_model * bootstrap_count) %>%
          filter(is.finite(weight_model_boot), weight_model_boot > 0)
        
        if(nrow(train_draw_age_i) < 10){
          next
        }
        
        
        # GDP scaling is estimated from this draw's training data and
        # then applied to the matching PC-imputation prediction grid.
        
        log_gdp_mean_i <- mean(train_draw_age_i$log_GDP, na.rm = TRUE)
        log_gdp_sd_i <- sd(train_draw_age_i$log_GDP, na.rm = TRUE)
        
        if(!is.finite(log_gdp_sd_i) || log_gdp_sd_i == 0){
          log_gdp_sd_i <- 1
        }
        
        train_draw_age_i <- train_draw_age_i %>%
          mutate(log_GDP_z = (log_GDP - log_gdp_mean_i) / log_gdp_sd_i)
        
        prediction_grid_i <- pc_dat_i %>%
          mutate(
            age_group = age_group_i,
            age_key = age_key_i,
            log_GDP_z = (log(GDP) - log_gdp_mean_i) / log_gdp_sd_i,
            Year_c = Year - 2010,
            Endemic = as.numeric(Endemic)
          ) %>%
          left_join(
            age_population_long %>%
              filter(age_group == age_group_i) %>%
              dplyr::select(Country, Year, age_group, age_key, age_population),
            by = c("Country", "Year", "age_group", "age_key")
          ) %>%
          filter(
            is.finite(age_population), age_population > 0,
            is.finite(PC1),
            is.finite(PC2),
            is.finite(log_GDP_z),
            !is.na(Endemic),
            is.finite(absolute_latitude)
          )
        
        
        # LRTI: elastic net alpha = 0.25 ---------------------------------------
        
        if(outcome_i == "LRTI"){
          
          combined_matrix_i <- bind_rows(
            train_draw_age_i %>%
              transmute(
                PC1, PC2, log_GDP_z, Year_c, Endemic,
                absolute_latitude, .set = "train"
              ),
            prediction_grid_i %>%
              transmute(
                PC1, PC2, log_GDP_z, Year_c, Endemic,
                absolute_latitude, .set = "prediction"
              )
          )
          
          x_all_i <- model.matrix(
            ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
            data = combined_matrix_i
          )[, -1, drop = FALSE]
          
          x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
          x_prediction_i <- x_all_i[combined_matrix_i$.set == "prediction", , drop = FALSE]
          
          nfolds_i <- min(5, nrow(train_draw_age_i))
          foldid_i <- ((seq_len(nrow(train_draw_age_i)) - 1) %% nfolds_i) + 1
          
          fit_selected_i <- tryCatch(
            glmnet::cv.glmnet(
              x = x_train_i,
              y = train_draw_age_i$log_rate,
              weights = train_draw_age_i$weight_model_boot,
              alpha = 0.25,
              family = "gaussian",
              nfolds = nfolds_i,
              foldid = foldid_i
            ),
            error = function(e) NULL
          )
          
          if(is.null(fit_selected_i)){
            next
          }
          
          pred_log_i <- tryCatch(
            as.numeric(
              predict(
                fit_selected_i,
                newx = x_prediction_i,
                s = "lambda.min"
              )
            ),
            error = function(e) rep(NA_real_, nrow(prediction_grid_i))
          )
          
          lambda_selected_i <- fit_selected_i$lambda.min
          
          
          # Fever: weighted linear model ---------------------------------------
          
        } else {
          
          fit_selected_i <- tryCatch(
            lm(
              log_rate ~ PC1 + PC2 + log_GDP_z + Year_c + Endemic + absolute_latitude,
              data = train_draw_age_i,
              weights = weight_model_boot
            ),
            error = function(e) NULL
          )
          
          if(is.null(fit_selected_i)){
            next
          }
          
          pred_log_i <- tryCatch(
            as.numeric(
              predict(
                fit_selected_i,
                newdata = prediction_grid_i
              )
            ),
            error = function(e) rep(NA_real_, nrow(prediction_grid_i))
          )
          
          lambda_selected_i <- NA_real_
        }
        
        
        if(!any(is.finite(pred_log_i))){
          next
        }
        
        result_index_i <- result_index_i + 1
        
        chunk_result_list_i[[result_index_i]] <- prediction_grid_i %>%
          transmute(
            draw_id = draw_id_i,
            .draw_within_combo = draw_within_combo_i,
            .imp_pc = pc_imp_i,
            .imp_dhs = dhs_imp_i,
            outcome = outcome_i,
            prediction_type = "marginal_RE0",
            model_key = model_key_i,
            model = model_label_i,
            alpha = alpha_i,
            lambda_selected = lambda_selected_i,
            Country,
            Year,
            gbd_region,
            age_group,
            age_key,
            age_population,
            pred_log = pred_log_i,
            pred_rate = exp(pred_log_i),
            pred_cases = exp(pred_log_i) * age_population
          )
      }
      
      
      if(draw_id_i %% 25 == 0){
        message(
          outcome_i,
          ": completed draw ",
          draw_id_i,
          " / ",
          n_draw
        )
      }
    }
    
    
    projection_chunk_i <- bind_rows(
      chunk_result_list_i[seq_len(result_index_i)]
    )
    
    holdout_chunk_i <- bind_rows(
      holdout_result_list_i[seq_len(holdout_result_index_i)]
    )
    
    chunk_files_i[chunk_index_i] <- file.path(
      chunk_dir_i,
      paste0(
        file_tag_i,
        "_chunk_",
        sprintf("%03d", chunk_index_i),
        ".rds"
      )
    )
    
    holdout_chunk_files_i[chunk_index_i] <- file.path(
      holdout_chunk_dir_i,
      paste0(
        file_tag_i,
        "_holdout_chunk_",
        sprintf("%03d", chunk_index_i),
        ".rds"
      )
    )
    
    saveRDS(projection_chunk_i, chunk_files_i[chunk_index_i])
    saveRDS(holdout_chunk_i, holdout_chunk_files_i[chunk_index_i])
    
    rm(
      projection_chunk_i,
      holdout_chunk_i,
      chunk_result_list_i,
      holdout_result_list_i
    )
    gc()
  }
  
  
  projection_draws_i <- bind_rows(lapply(chunk_files_i, readRDS))
  holdout_draws_i <- bind_rows(lapply(holdout_chunk_files_i, readRDS))
  
  if(nrow(projection_draws_i) == 0){
    stop("No successful ", outcome_i, " uncertainty draws were generated.")
  }
  
  if(nrow(holdout_draws_i) == 0){
    stop("No successful ", outcome_i, " holdout-validation draws were generated.")
  }
  
  saveRDS(
    projection_draws_i,
    file.path(uncertainty_out_dir, paste0(file_tag_i, "_projection_draws.rds"))
  )
  
  saveRDS(
    holdout_draws_i,
    file.path(uncertainty_out_dir, paste0(file_tag_i, "_holdout_draws.rds"))
  )
  
  unlink(chunk_dir_i, recursive = TRUE)
  unlink(holdout_chunk_dir_i, recursive = TRUE)
  # Check that every PC x DHS cell contributes exactly 40 successful
  # draws for each age group.
  
  successful_draw_check_i <- projection_draws_i %>%
    distinct(draw_id, .imp_pc, .imp_dhs, age_group) %>%
    count(.imp_pc, .imp_dhs, age_group, name = "n_successful_draws")
  
  expected_success_i <- tidyr::crossing(
    .imp_pc = pc_imp_values,
    .imp_dhs = dhs_imp_values,
    age_group = age_group_list
  ) %>%
    left_join(
      successful_draw_check_i,
      by = c(".imp_pc", ".imp_dhs", "age_group")
    ) %>%
    mutate(n_successful_draws = replace_na(n_successful_draws, 0L))
  
  print(expected_success_i, n = Inf)
  
  saveRDS(
    expected_success_i,
    file.path(uncertainty_out_dir, paste0(file_tag_i, "_draw_balance_check.rds"))
  )
  
  write.csv(
    expected_success_i,
    file.path(uncertainty_out_dir, paste0(file_tag_i, "_draw_balance_check.csv")),
    row.names = FALSE
  )
  
  if(any(expected_success_i$n_successful_draws != n_draw_per_combo)){
    stop(
      outcome_i,
      ": not all PC x DHS x age cells have exactly ",
      n_draw_per_combo,
      " successful draws. Check the saved draw-balance file."
    )
  }
  
  
  list(
    projection_draws = projection_draws_i,
    holdout_draws = holdout_draws_i
  )
}


# 8-7. LRTI: elastic net alpha 0.25 ------------------------------------------

LRTI_file_tag <- "lrti_age_marginal_RE0_enet025_uncertainty"

LRTI_uncertainty <- run_uncertainty_draws(
  outcome_i = "LRTI",
  fit_mi_i = fit_lrti_mi,
  robust_vcov_list_i = robust_vcov_lrti_list,
  model_key_i = "enet_025",
  model_label_i = "Elastic net alpha 0.25",
  alpha_i = 0.25,
  file_tag_i = LRTI_file_tag,
  seed_base_i = 100000
)

LRTI_projection_draws <- LRTI_uncertainty$projection_draws
LRTI_holdout_draws <- LRTI_uncertainty$holdout_draws
rm(LRTI_uncertainty)


# 8-8. Fever: weighted linear model -------------------------------------------

fever_file_tag <- "fever_age_marginal_RE0_lm_uncertainty"

fever_uncertainty <- run_uncertainty_draws(
  outcome_i = "fever",
  fit_mi_i = fit_fever_mi,
  robust_vcov_list_i = robust_vcov_fever_list,
  model_key_i = "lm",
  model_label_i = "Weighted linear model",
  alpha_i = NA_real_,
  file_tag_i = fever_file_tag,
  seed_base_i = 200000
)

fever_projection_draws <- fever_uncertainty$projection_draws
fever_holdout_draws <- fever_uncertainty$holdout_draws
rm(fever_uncertainty)


# 8-9. Summary function --------------------------------------------------------

summarise_projection_draws <- function(projection_draws_i){
  
  country_year_summary_i <- projection_draws_i %>%
    group_by(
      outcome, age_group, age_key,
      Country, Year, gbd_region,
      model_key, model, alpha
    ) %>%
    summarise(
      age_population = mean(age_population, na.rm = TRUE),
      predicted_annual_rate_mean = mean(pred_rate, na.rm = TRUE),
      predicted_annual_rate_median = median(pred_rate, na.rm = TRUE),
      predicted_annual_rate_lcl = as.numeric(quantile(pred_rate, 0.025, na.rm = TRUE)),
      predicted_annual_rate_ucl = as.numeric(quantile(pred_rate, 0.975, na.rm = TRUE)),
      predicted_cases_mean = mean(pred_cases, na.rm = TRUE),
      predicted_cases_median = median(pred_cases, na.rm = TRUE),
      predicted_cases_lcl = as.numeric(quantile(pred_cases, 0.025, na.rm = TRUE)),
      predicted_cases_ucl = as.numeric(quantile(pred_cases, 0.975, na.rm = TRUE)),
      n_successful_draws = n_distinct(draw_id),
      .groups = "drop"
    )
  
  
  year_draws_i <- projection_draws_i %>%
    group_by(
      draw_id, .imp_pc, .imp_dhs,
      outcome, age_group, age_key, Year,
      model_key, model, alpha
    ) %>%
    summarise(
      predicted_annual_rate = weighted.mean(pred_rate, age_population, na.rm = TRUE),
      predicted_cases = sum(pred_cases, na.rm = TRUE),
      age_population = sum(age_population, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  year_summary_i <- year_draws_i %>%
    group_by(
      outcome, age_group, age_key, Year,
      model_key, model, alpha
    ) %>%
    summarise(
      age_population = mean(age_population, na.rm = TRUE),
      predicted_annual_rate_mean = mean(predicted_annual_rate, na.rm = TRUE),
      predicted_annual_rate_median = median(predicted_annual_rate, na.rm = TRUE),
      predicted_annual_rate_lcl = as.numeric(quantile(predicted_annual_rate, 0.025, na.rm = TRUE)),
      predicted_annual_rate_ucl = as.numeric(quantile(predicted_annual_rate, 0.975, na.rm = TRUE)),
      predicted_cases_mean = mean(predicted_cases, na.rm = TRUE),
      predicted_cases_median = median(predicted_cases, na.rm = TRUE),
      predicted_cases_lcl = as.numeric(quantile(predicted_cases, 0.025, na.rm = TRUE)),
      predicted_cases_ucl = as.numeric(quantile(predicted_cases, 0.975, na.rm = TRUE)),
      n_successful_draws = n_distinct(draw_id),
      .groups = "drop"
    )
  
  
  region_year_draws_i <- projection_draws_i %>%
    group_by(
      draw_id, .imp_pc, .imp_dhs,
      outcome, age_group, age_key,
      Year, gbd_region,
      model_key, model, alpha
    ) %>%
    summarise(
      predicted_annual_rate = weighted.mean(pred_rate, age_population, na.rm = TRUE),
      predicted_cases = sum(pred_cases, na.rm = TRUE),
      age_population = sum(age_population, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  region_year_summary_i <- region_year_draws_i %>%
    group_by(
      outcome, age_group, age_key,
      Year, gbd_region,
      model_key, model, alpha
    ) %>%
    summarise(
      age_population = mean(age_population, na.rm = TRUE),
      predicted_annual_rate_mean = mean(predicted_annual_rate, na.rm = TRUE),
      predicted_annual_rate_median = median(predicted_annual_rate, na.rm = TRUE),
      predicted_annual_rate_lcl = as.numeric(quantile(predicted_annual_rate, 0.025, na.rm = TRUE)),
      predicted_annual_rate_ucl = as.numeric(quantile(predicted_annual_rate, 0.975, na.rm = TRUE)),
      predicted_cases_mean = mean(predicted_cases, na.rm = TRUE),
      predicted_cases_median = median(predicted_cases, na.rm = TRUE),
      predicted_cases_lcl = as.numeric(quantile(predicted_cases, 0.025, na.rm = TRUE)),
      predicted_cases_ucl = as.numeric(quantile(predicted_cases, 0.975, na.rm = TRUE)),
      n_successful_draws = n_distinct(draw_id),
      .groups = "drop"
    )
  
  
  country_year_matrix_wide_i <- country_year_summary_i %>%
    dplyr::select(
      Country, Year, gbd_region, age_key, age_population,
      predicted_annual_rate_mean,
      predicted_annual_rate_median,
      predicted_annual_rate_lcl,
      predicted_annual_rate_ucl,
      predicted_cases_mean,
      predicted_cases_median,
      predicted_cases_lcl,
      predicted_cases_ucl,
      model_key, model, alpha, n_successful_draws
    ) %>%
    pivot_wider(
      id_cols = c(
        Country, Year, gbd_region,
        model_key, model, alpha, n_successful_draws
      ),
      names_from = age_key,
      values_from = c(
        age_population,
        predicted_annual_rate_mean,
        predicted_annual_rate_median,
        predicted_annual_rate_lcl,
        predicted_annual_rate_ucl,
        predicted_cases_mean,
        predicted_cases_median,
        predicted_cases_lcl,
        predicted_cases_ucl
      )
    )
  
  
  list(
    country_year_summary = country_year_summary_i,
    year_draws = year_draws_i,
    year_summary = year_summary_i,
    region_year_draws = region_year_draws_i,
    region_year_summary = region_year_summary_i,
    country_year_matrix_wide = country_year_matrix_wide_i
  )
}


# 8-10. LRTI summaries --------------------------------------------------------

LRTI_summary <- summarise_projection_draws(LRTI_projection_draws)

LRTI_country_year_summary <- LRTI_summary$country_year_summary
LRTI_year_draws <- LRTI_summary$year_draws
LRTI_year_summary <- LRTI_summary$year_summary
LRTI_region_year_draws <- LRTI_summary$region_year_draws
LRTI_region_year_summary <- LRTI_summary$region_year_summary
LRTI_country_year_matrix_wide <- LRTI_summary$country_year_matrix_wide

saveRDS(
  LRTI_country_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_country_year_summary.rds"))
)

saveRDS(
  LRTI_year_draws,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_year_draws.rds"))
)

saveRDS(
  LRTI_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_year_summary.rds"))
)

saveRDS(
  LRTI_region_year_draws,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_region_year_draws.rds"))
)

saveRDS(
  LRTI_region_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_region_year_summary.rds"))
)

saveRDS(
  LRTI_country_year_matrix_wide,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_country_year_matrix_wide.rds"))
)

write.csv(
  LRTI_country_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_country_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  LRTI_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  LRTI_region_year_summary,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_region_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  LRTI_country_year_matrix_wide,
  file.path(uncertainty_out_dir, paste0(LRTI_file_tag, "_country_year_matrix_wide.csv")),
  row.names = FALSE
)


# 8-11. Fever summaries -------------------------------------------------------

fever_summary <- summarise_projection_draws(fever_projection_draws)

fever_country_year_summary <- fever_summary$country_year_summary
fever_year_draws <- fever_summary$year_draws
fever_year_summary <- fever_summary$year_summary
fever_region_year_draws <- fever_summary$region_year_draws
fever_region_year_summary <- fever_summary$region_year_summary
fever_country_year_matrix_wide <- fever_summary$country_year_matrix_wide

saveRDS(
  fever_country_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_country_year_summary.rds"))
)

saveRDS(
  fever_year_draws,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_year_draws.rds"))
)

saveRDS(
  fever_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_year_summary.rds"))
)

saveRDS(
  fever_region_year_draws,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_region_year_draws.rds"))
)

saveRDS(
  fever_region_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_region_year_summary.rds"))
)

saveRDS(
  fever_country_year_matrix_wide,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_country_year_matrix_wide.rds"))
)

write.csv(
  fever_country_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_country_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  fever_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  fever_region_year_summary,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_region_year_summary.csv")),
  row.names = FALSE
)

write.csv(
  fever_country_year_matrix_wide,
  file.path(uncertainty_out_dir, paste0(fever_file_tag, "_country_year_matrix_wide.csv")),
  row.names = FALSE
)


# 8-12. Selected-model holdout validation summaries and draw scatter -----------

selected_model_holdout_draws <- bind_rows(
  LRTI_holdout_draws,
  fever_holdout_draws
) %>%
  filter(
    is.finite(heldout_estimated_log_rate),
    is.finite(predicted_log_rate),
    is.finite(heldout_estimated_rate),
    is.finite(predicted_rate)
  )


selected_model_holdout_country_year_summary <-
  selected_model_holdout_draws %>%
  group_by(
    outcome, age_group, age_key,
    Country, Year, CountryYear, gbd_region,
    model_key, model, alpha
  ) %>%
  summarise(
    observed_annual_rate_mean =
      mean(heldout_estimated_rate, na.rm = TRUE),
    observed_annual_rate_median =
      median(heldout_estimated_rate, na.rm = TRUE),
    observed_annual_rate_lcl =
      as.numeric(quantile(heldout_estimated_rate, 0.025, na.rm = TRUE)),
    observed_annual_rate_ucl =
      as.numeric(quantile(heldout_estimated_rate, 0.975, na.rm = TRUE)),
    pred_annual_rate_mean =
      mean(predicted_rate, na.rm = TRUE),
    pred_annual_rate_median =
      median(predicted_rate, na.rm = TRUE),
    pred_annual_rate_lcl =
      as.numeric(quantile(predicted_rate, 0.025, na.rm = TRUE)),
    pred_annual_rate_ucl =
      as.numeric(quantile(predicted_rate, 0.975, na.rm = TRUE)),
    residual_rate_mean =
      mean(residual_rate, na.rm = TRUE),
    residual_log_rate_mean =
      mean(residual_log_rate, na.rm = TRUE),
    n_holdout_draws = n_distinct(draw_id),
    .groups = "drop"
  )


selected_model_holdout_draw_metrics <-
  selected_model_holdout_draws %>%
  group_by(
    draw_id, .draw_within_combo, .imp_pc, .imp_dhs,
    outcome, age_group, age_key,
    model_key, model, alpha
  ) %>%
  summarise(
    rmse_log =
      sqrt(mean(
        (predicted_log_rate - heldout_estimated_log_rate)^2,
        na.rm = TRUE
      )),
    mae_log =
      mean(
        abs(predicted_log_rate - heldout_estimated_log_rate),
        na.rm = TRUE
      ),
    bias_log =
      mean(
        predicted_log_rate - heldout_estimated_log_rate,
        na.rm = TRUE
      ),
    rmse_rate =
      sqrt(mean(
        (predicted_rate - heldout_estimated_rate)^2,
        na.rm = TRUE
      )),
    mae_rate =
      mean(
        abs(predicted_rate - heldout_estimated_rate),
        na.rm = TRUE
      ),
    bias_rate =
      mean(
        predicted_rate - heldout_estimated_rate,
        na.rm = TRUE
      ),
    cor_rate = if(
      n() >= 2 &&
      sd(heldout_estimated_rate, na.rm = TRUE) > 0 &&
      sd(predicted_rate, na.rm = TRUE) > 0
    ){
      cor(
        heldout_estimated_rate,
        predicted_rate,
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    n_eval = n(),
    n_holdout_country = n_distinct(Country),
    .groups = "drop"
  )


selected_model_holdout_metric_summary <-
  selected_model_holdout_draw_metrics %>%
  group_by(
    outcome, age_group, age_key,
    model_key, model, alpha
  ) %>%
  summarise(
    mean_rmse_log = mean(rmse_log, na.rm = TRUE),
    median_rmse_log = median(rmse_log, na.rm = TRUE),
    rmse_log_lcl =
      as.numeric(quantile(rmse_log, 0.025, na.rm = TRUE)),
    rmse_log_ucl =
      as.numeric(quantile(rmse_log, 0.975, na.rm = TRUE)),
    mean_mae_log = mean(mae_log, na.rm = TRUE),
    mean_bias_log = mean(bias_log, na.rm = TRUE),
    mean_rmse_rate = mean(rmse_rate, na.rm = TRUE),
    median_rmse_rate = median(rmse_rate, na.rm = TRUE),
    rmse_rate_lcl =
      as.numeric(quantile(rmse_rate, 0.025, na.rm = TRUE)),
    rmse_rate_ucl =
      as.numeric(quantile(rmse_rate, 0.975, na.rm = TRUE)),
    mean_mae_rate = mean(mae_rate, na.rm = TRUE),
    mean_bias_rate = mean(bias_rate, na.rm = TRUE),
    mean_cor_rate = mean(cor_rate, na.rm = TRUE),
    n_successful_validation_draws = n_distinct(draw_id),
    .groups = "drop"
  )


saveRDS(
  selected_model_holdout_draws,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_validation_draws.rds"
  )
)

saveRDS(
  selected_model_holdout_country_year_summary,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_country_year_summary.rds"
  )
)

saveRDS(
  selected_model_holdout_draw_metrics,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_draw_metrics.rds"
  )
)

saveRDS(
  selected_model_holdout_metric_summary,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_metric_summary.rds"
  )
)

write.csv(
  selected_model_holdout_country_year_summary,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_country_year_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  selected_model_holdout_draw_metrics,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_draw_metrics.csv"
  ),
  row.names = FALSE
)

write.csv(
  selected_model_holdout_metric_summary,
  file.path(
    uncertainty_out_dir,
    "selected_model_holdout_metric_summary.csv"
  ),
  row.names = FALSE
)



# 8-12-2. Selected-model holdout validation plot only -------------------------------
out_dir <- "outputs/pipelines"
uncertainty_out_dir <- file.path(out_dir, "age_specific_selected_model_uncertainty")
validation_fig_dir <- "figures/main_analysis/inc_validation_predictions"
dir.create(validation_fig_dir, recursive = TRUE, showWarnings = FALSE)

selected_model_holdout_draws <- readRDS(file.path(uncertainty_out_dir, "selected_model_holdout_validation_draws.rds"))

# Use the same age-group labels used in the saved validation data
age_group_list <- c("0-11 months", "12-23 months", "24-59 months")

point_col <- adjustcolor("#4D4D4D", alpha.f = 0.18)

pdf(file.path(validation_fig_dir, "selected_model_holdout_validation_draw_scatter.pdf"), width = 4.5, height = 6.0, useDingbats = FALSE)

old_par <- par(no.readonly = TRUE)

layout(matrix(c(0, 0, 0, 
                1, 0, 4, 
                2, 0, 5, 
                3, 0, 6), nrow = 4, byrow = TRUE), widths = c(1,0.1,1), heights = c(0.15,1,1,1))

par(mar = c(5.0, 5.0, 1.0, 1.0), mgp = c(1.8, 0.5, 0), tcl = -0.25, cex.axis = 1.0, cex.lab = 1.0, cex.main = 1.0)

for(outcome_i in c("LRTI", "fever")){
  
  plot_outcome_i <- selected_model_holdout_draws %>%
    filter(outcome == outcome_i, is.finite(heldout_estimated_rate), is.finite(predicted_rate))
  
  lim_max_i <- max(plot_outcome_i$heldout_estimated_rate, plot_outcome_i$predicted_rate, na.rm = TRUE)
  lim_i <- c(0, max(pretty(c(0, lim_max_i), n = 5)))
  axis_at_i <- pretty(lim_i, n = 5)
  axis_at_i <- axis_at_i[axis_at_i >= lim_i[1] & axis_at_i <= lim_i[2]]
  
  for(j in 1:3){
    
    age_group_i <- age_group_list[j]
    
    plot_data_i <- plot_outcome_i %>%
      filter(age_group == age_group_i)
    
    plot(
      plot_data_i$heldout_estimated_rate,
      plot_data_i$predicted_rate,
      xlim = lim_i, ylim = lim_i, xaxs = "i", yaxs = "i",
      axes = FALSE, xlab = "", ylab = "",
      main = "",
      pch = 16, cex = 0.6, col = point_col
    )
    
    panel_letter_i <- if(outcome_i == "LRTI") c("a", "c", "e")[j] else c("b", "d", "f")[j]
    usr <- par("usr")
    
    mtext(panel_letter_i, side = 3, line = 2.0, at = usr[1] - 0.45 * diff(usr[1:2]), adj = 0.1, font = 2, cex = 0.75)
    
    axis(1, at = axis_at_i, labels = axis_at_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
    axis(2, at = axis_at_i, labels = axis_at_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
    
    if(outcome_i == "LRTI"){
      mtext("Held-out predicted\nannualized incidence rate", side = 2, line = 2.5, cex = 0.8)
    }
    
    if(j == 3){
      mtext("Held-out estimated\nannualized incidence rate", side = 1, line = 3.5, cex = 0.8)
    }
    
    abline(a = 0, b = 1, lty = 1, lwd = 0.8, col = "darkred")
  }
}

par(old_par)
dev.off()


# 8-13. Combined selected-model uncertainty object ----------------------------

selected_model_uncertainty_results <- list(
  draw_design = draw_design,
  
  LRTI = list(
    model_key = "enet_025",
    model = "Elastic net alpha 0.25",
    alpha = 0.25,
    country_year_summary = LRTI_country_year_summary,
    year_summary = LRTI_year_summary,
    region_year_summary = LRTI_region_year_summary,
    projection_draws_file = file.path(
      uncertainty_out_dir,
      paste0(LRTI_file_tag, "_projection_draws.rds")
    ),
    holdout_draws_file = file.path(
      uncertainty_out_dir,
      paste0(LRTI_file_tag, "_holdout_draws.rds")
    )
  ),
  
  fever = list(
    model_key = "lm",
    model = "Weighted linear model",
    alpha = NA_real_,
    country_year_summary = fever_country_year_summary,
    year_summary = fever_year_summary,
    region_year_summary = fever_region_year_summary,
    projection_draws_file = file.path(
      uncertainty_out_dir,
      paste0(fever_file_tag, "_projection_draws.rds")
    ),
    holdout_draws_file = file.path(
      uncertainty_out_dir,
      paste0(fever_file_tag, "_holdout_draws.rds")
    )
  ),
  
  holdout_validation = list(
    country_year_summary =
      selected_model_holdout_country_year_summary,
    draw_metrics =
      selected_model_holdout_draw_metrics,
    metric_summary =
      selected_model_holdout_metric_summary,
    holdout_draws_file = file.path(
      uncertainty_out_dir,
      "selected_model_holdout_validation_draws.rds"
    ),
    validation_plot_file = file.path(
      validation_fig_dir,
      "selected_model_holdout_validation_draw_scatter.pdf"
    )
  )
)

saveRDS(
  selected_model_uncertainty_results,
  file.path(
    uncertainty_out_dir,
    "all_age_specific_selected_model_uncertainty_results_enet025_lm.rds"
  )
)


# 8-14. Final checks -----------------------------------------------------------

cat(
  "\nFinal uncertainty design:\n",
  "PC imputations = ", length(pc_imp_values),
  "\nDHS imputations = ", length(dhs_imp_values),
  "\nConditional draws per joint MI combination = ", n_draw_per_combo,
  "\nTotal draws = ", n_draw,
  "\n",
  sep = ""
)

cat("\nLRTI successful draws by age group:\n")
print(
  LRTI_projection_draws %>%
    distinct(draw_id, age_group) %>%
    count(age_group)
)

cat("\nFever successful draws by age group:\n")
print(
  fever_projection_draws %>%
    distinct(draw_id, age_group) %>%
    count(age_group)
)

print(
  LRTI_country_year_summary %>%
    count(age_group, n_successful_draws)
)

print(
  fever_country_year_summary %>%
    count(age_group, n_successful_draws)
)

cat("\nSelected-model holdout validation summary:\n")
print(selected_model_holdout_metric_summary, n = Inf)

message(
  "Saved uncertainty outputs to: ",
  normalizePath(uncertainty_out_dir, mustWork = FALSE)
)



  # _____________________________________________ ----------
  # 9. Proportion of bacterial infection ---------------------------------------
  
  rm(list = ls()); gc()
  options(scipen = 999)
  
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
  library(cmdstanr)
  library(posterior)
  library(tibble)
  
  set.seed(123)
  
  out_dir <- "outputs/pipelines"
  bact_fever_dir <- file.path(out_dir, "bact_fever")
  bact_lrti_dir <- file.path(out_dir, "bact_lrti")
  
  dir.create(bact_fever_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(bact_lrti_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  # Same 1000-draw structure as Section 8:
  # 5 PC imputations x 5 DHS imputations x 40 draws
  
  draw_design <- readRDS(
    "outputs/pipelines/age_specific_selected_model_uncertainty/uncertainty_draw_design_5PC_5DHS_40draw.rds"
  )
  
  stopifnot(
    nrow(draw_design) == 1000,
    length(unique(draw_design$.imp_pc)) == 5,
    length(unique(draw_design$.imp_dhs)) == 5
  )
  
  draw_design_check <- draw_design %>% count(.imp_pc, .imp_dhs, name = "n_draw")
  draw_per_pc_check <- draw_design %>% count(.imp_pc, name = "n_draw")
  
  stopifnot(all(draw_design_check$n_draw == 40))
  stopifnot(all(draw_per_pc_check$n_draw == 200))
  
  print(draw_per_pc_check)
  
  
  
  ## 1. AFI --------------------------------
  
  ## 1-1. Data load and cleaning --------------------------------
  
  # Use tested denominator for bacterial bounds
  bacterial_denominator <- "pop"   # "tested" or "pop"
  
  # Stan sampling controls
  n_chains <- 4
  iter_warmup <- 2500
  iter_sampling_per_chain <- 2500
  total_post_warmup_draws <- n_chains * iter_sampling_per_chain # 10,000 per PC imputation
  
  cat("Total posterior draws per PC-specific fit =", total_post_warmup_draws, "\n")
  
  
  # lower text
  clean_text <- function(x) tolower(trimws(as.character(x)))
  
  
  # country-name key for joining literature data to UNI
  country_key <- function(x) {
    x <- tolower(trimws(as.character(x)))
    
    dplyr::recode(
      x,
      "iran" = "iran, islamic rep.",
      "vietnam" = "viet nam",
      "egypt" = "egypt, arab rep.",
      "democratic republic of the congo" = "congo, dem. rep.",
      "democratic republic of congo" = "congo, dem. rep.",
      "republic of the congo" = "congo, rep.",
      "gambia" = "gambia, the",
      "laos" = "lao pdr",
      "turkey" = "turkiye",
      .default = x
    )
  }
  
  
  # extract idx
  extract_idx_in_brackets <- function(x) {
    as.integer(str_match(x, "\\[(\\d+)\\]")[, 2])
  }
  
  
  # pick the row
  pick_best_row <- function(pop_rows, type) {
    x <- pop_rows %>% filter(measure_type == type)
    if (nrow(x) == 0) return(NULL)
    
    x <- x %>%
      mutate(
        score =
          coalesce(n_tested, 0) +
          ifelse(type == "malaria" & test == "mrdt_or_tbs", 1e6, 0) +
          ifelse(specimen_type == "overall", 1e3, 0) +
          ifelse(!is.na(n_positive), 1e2, 0)
      ) %>%
      arrange(desc(score))
    
    x[1, ]
  }
  
  
  # extract objectives
  extract_obs <- function(row) {
    if (is.null(row)) {
      return(list(exact = 0L, int = 0L, N = 0L, y = 0L, lb = 0L, ub = 0L))
    }
    
    N <- row$n_tested
    if (is.na(N) && "n_tested_ub" %in% names(row)) N <- row$n_tested_ub
    N <- as.integer(round(ifelse(is.na(N), 0, N)))
    
    y <- row$n_positive
    lb <- row$n_positive_lb
    ub <- row$n_positive_ub
    
    exact <- as.integer(N > 0 && !is.na(y))
    int <- as.integer(N > 0 && is.na(y) && !is.na(lb) && !is.na(ub))
    
    list(
      exact = exact,
      int = int,
      N = N,
      y = as.integer(round(ifelse(is.na(y), 0, y))),
      lb = as.integer(round(ifelse(is.na(lb), 0, lb))),
      ub = as.integer(round(ifelse(is.na(ub), 0, ub)))
    )
  }
  
  
  # collapsing the extracted data
  collapse_bacterial_bounds <- function(pop_rows, denom_mode = c("tested", "pop")) {
    denom_mode <- match.arg(denom_mode)
    
    b <- pop_rows %>% filter(measure_type == "bacterial")
    if (nrow(b) == 0) return(NULL)
    
    allowed_tests <- c("culture", "pcr", "culture_pcr")
    b <- b %>% filter(is.na(test) | test %in% allowed_tests)
    if (nrow(b) == 0) return(NULL)
    
    allowed_specimens <- c("blood", "csf", "urine")
    b <- b %>% filter(is.na(specimen_type) | specimen_type %in% allowed_specimens)
    if (nrow(b) == 0) return(NULL)
    
    n_pop <- suppressWarnings(max(b$n_pop, na.rm = TRUE))
    n_tested_max <- suppressWarnings(max(b$n_tested, na.rm = TRUE))
    
    N_ref <- if (denom_mode == "pop") {
      if (is.finite(n_pop) && !is.na(n_pop) && n_pop > 0) n_pop else n_tested_max
    } else {
      n_tested_max
    }
    
    if (!is.finite(N_ref) || is.na(N_ref) || N_ref <= 0) return(NULL)
    N_ref <- as.integer(round(N_ref))
    
    b <- b %>%
      mutate(
        pos_lb = case_when(
          !is.na(n_positive_lb) ~ n_positive_lb,
          !is.na(n_positive) ~ n_positive,
          TRUE ~ NA_real_
        ),
        pos_ub = case_when(
          !is.na(n_positive_ub) ~ n_positive_ub,
          !is.na(n_positive) ~ n_positive,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(pos_lb) & !is.na(pos_ub))
    
    if (nrow(b) == 0) return(NULL)
    
    y_lb <- as.integer(round(max(b$pos_lb)))
    y_ub <- as.integer(round(min(sum(b$pos_ub), N_ref)))
    
    y_lb <- max(0L, y_lb)
    y_ub <- max(y_lb, min(y_ub, N_ref))
    
    list(N_B = N_ref, yB_lb = y_lb, yB_ub = y_ub)
  }
  
  
  
  ## 1-2. loading data --------------------------------
  
  env <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
  
  stopifnot(".imp_pc" %in% names(env))
  
  region_levels <- c(
    "Sub-Saharan Africa",
    "Southeast Asia, East Asia, and Oceania",
    "South Asia",
    "North Africa and Middle East",
    "Latin America and Caribbean",
    "Central Europe, Eastern Europe, and Central Asia"
  )
  
  env_dat <- env %>%
    transmute(
      .imp_pc = as.integer(.imp_pc),
      Country = as.character(Country),
      Country_key = country_key(Country),
      Year = as.integer(Year),
      gbd_region = as.character(gbd_region),
      PC1 = as.numeric(PC1),
      PC2 = as.numeric(PC2),
      Endemic = as.integer(Endemic)
    ) %>%
    filter(
      !is.na(.imp_pc),
      !is.na(Country),
      !is.na(Year),
      !is.na(gbd_region),
      is.finite(PC1),
      is.finite(PC2),
      !is.na(Endemic),
      gbd_region %in% region_levels
    ) %>%
    mutate(gbd_region = factor(gbd_region, levels = region_levels))
  
  stopifnot(length(unique(env_dat$.imp_pc)) == 5)
  
  for (pc_imp_i in sort(unique(env_dat$.imp_pc))) {
    env_pc_check_i <- env_dat %>% filter(.imp_pc == pc_imp_i)
    
    if (anyDuplicated(env_pc_check_i[c("Country_key", "Year")])) {
      stop("Duplicate Country-Year rows in PC imputation ", pc_imp_i)
    }
  }
  
  
  # Load study table
  study <- read_csv(
    "data/bacterial_fever_ratio/df.analysis.csv",
    locale = locale(encoding = "latin1"),
    show_col_types = FALSE
  )
  
  study <- study %>% filter(!country %in% c("Brazil", "Colombia"))
  names(study) <- trimws(names(study))
  
  study <- study %>%
    mutate(
      Country = as.character(country),
      Country_key = country_key(Country),
      measure_type = clean_text(measure_type),
      specimen_type = clean_text(specimen_type),
      test = clean_text(test),
      panel_scope = clean_text(panel_scope)
    )
  
  # Optional checks
  unique(study[study$measure_type == "bacterial", ]$specimen_type)
  unique(study[study$measure_type == "bacterial", ]$test)
  
  
  # Restrict to strata including any under-5
  study_child <- study %>%
    filter(is.na(age_month_min) | age_month_min < 60)
  
  
  # Pop-level metadata
  pop_meta <- study_child %>%
    group_by(study_id, pop_id) %>%
    summarise(
      Country = first(Country),
      Country_key = first(Country_key),
      Year_mid = first(study_year_mid),
      age_max = first(age_month_max),
      x_mixed = as.integer(is.na(age_max) | age_max > 60),
      excludes_malaria = as.integer(any(excludes_malaria == 1, na.rm = TRUE)),
      excludes_lrti = as.integer(any(excludes_lrti == 1, na.rm = TRUE)),
      .groups = "drop"
    )
  
  
  # Build one observation per pop_id
  pop_groups <- study_child %>%
    group_by(study_id, pop_id) %>%
    group_split()
  
  pop_obs <- bind_rows(lapply(pop_groups, function(pop_rows) {
    sid <- pop_rows$study_id[1]
    pid <- pop_rows$pop_id[1]
    
    B_int <- collapse_bacterial_bounds(pop_rows, denom_mode = bacterial_denominator)
    if (is.null(B_int)) return(NULL)
    
    M_row <- pick_best_row(pop_rows, "malaria")
    L_row <- pick_best_row(pop_rows, "lrti")
    BM_row <- pick_best_row(pop_rows, "bacterial_malaria")
    
    M <- extract_obs(M_row)
    L <- extract_obs(L_row)
    BM <- extract_obs(BM_row)
    
    tibble(
      study_id = sid,
      pop_id = pid,
      
      # bacterial interval
      has_B_exact = 0L,
      has_B_int = 1L,
      N_B = B_int$N_B,
      y_B = 0L,
      yB_lb = B_int$yB_lb,
      yB_ub = B_int$yB_ub,
      
      # malaria
      has_M_exact = M$exact,
      has_M_int = M$int,
      N_M = M$N,
      y_M = M$y,
      yM_lb = M$lb,
      yM_ub = M$ub,
      
      # lrti
      has_L_exact = L$exact,
      has_L_int = L$int,
      N_L = L$N,
      y_L = L$y,
      yL_lb = L$lb,
      yL_ub = L$ub,
      
      # B ∩ M
      has_BM_exact = BM$exact,
      has_BM_int = BM$int,
      N_BM = BM$N,
      y_BM = BM$y,
      yBM_lb = BM$lb,
      yBM_ub = BM$ub
    )
  }))
  
  
  # Combine metadata + observations
  pop_base <- pop_meta %>%
    left_join(pop_obs, by = c("study_id", "pop_id")) %>%
    mutate(
      B_def_excl_M = excludes_malaria,
      B_def_excl_L = excludes_lrti,
      Year = as.integer(round(Year_mid))
    ) %>%
    filter(!is.na(Country_key), !is.na(Year))
  
  
  # Replace NA with 0 for Stan safety
  flag_cols <- c(
    "has_B_exact", "has_B_int",
    "has_M_exact", "has_M_int",
    "has_L_exact", "has_L_int",
    "has_BM_exact", "has_BM_int",
    "B_def_excl_M", "B_def_excl_L"
  )
  
  count_cols <- c(
    "N_B", "y_B", "yB_lb", "yB_ub",
    "N_M", "y_M", "yM_lb", "yM_ub",
    "N_L", "y_L", "yL_lb", "yL_ub",
    "N_BM", "y_BM", "yBM_lb", "yBM_ub"
  )
  
  pop_base <- pop_base %>%
    mutate(across(all_of(flag_cols), ~ as.integer(ifelse(is.na(.), 0, .)))) %>%
    mutate(across(all_of(count_cols), ~ as.integer(ifelse(is.na(.), 0, round(.))))) %>%
    filter(
      N_B > 0,
      has_B_exact == 1 | has_B_int == 1
    )
  
  
  # Check unmatched countries
  setdiff(unique(pop_base$Country_key), unique(env_dat$Country_key))
  
  
  
  ## 1-3. Fit model separately for each PC imputation --------------------------
  
  # Compile once
  mod <- cmdstan_model("scripts/stan/prop_bact_fever_hierarch.stan")
  
  pc_imp_values <- sort(unique(env_dat$.imp_pc))
  
  pred_long_pc_list <- vector("list", length(pc_imp_values))
  fit_diag_pc_list <- vector("list", length(pc_imp_values))
  study_count_pc_list <- vector("list", length(pc_imp_values))
  
  
  for (pc_index_i in seq_along(pc_imp_values)) {
    
    pc_imp_i <- pc_imp_values[pc_index_i]
    
    message("\nAFI bacterial fraction: PC imputation ", pc_imp_i, " / 5")
    
    
    # PC-imputation-specific environmental data
    env_pc_i <- env_dat %>%
      filter(.imp_pc == pc_imp_i)
    
    
    # Join literature data to ONE PC imputation only
    pop_dat_i <- pop_base %>%
      inner_join(
        env_pc_i %>%
          dplyr::select(
            Country_key, Year,
            Country_env = Country,
            gbd_region, PC1, PC2, Endemic
          ),
        by = c("Country_key", "Year")
      ) %>%
      mutate(Country = Country_env) %>%
      dplyr::select(-Country_env) %>%
      filter(
        is.finite(PC1),
        is.finite(PC2),
        !is.na(Endemic),
        !is.na(gbd_region)
      )
    
    if (nrow(pop_dat_i) == 0) {
      stop("No literature observations matched PC imputation ", pc_imp_i)
    }
    
    
    # Build study index AFTER final filtering
    study_lookup_i <- pop_dat_i %>%
      distinct(study_id) %>%
      arrange(study_id) %>%
      mutate(study_idx = row_number())
    
    pop_dat_i <- pop_dat_i %>%
      left_join(study_lookup_i, by = "study_id")
    
    
    # Standardization parameters estimated separately within each PC imputation
    Year_mean_i <- mean(pop_dat_i$Year, na.rm = TRUE)
    Year_sd_i <- sd(pop_dat_i$Year, na.rm = TRUE)
    
    pc1_mean_i <- mean(pop_dat_i$PC1, na.rm = TRUE)
    pc1_sd_i <- sd(pop_dat_i$PC1, na.rm = TRUE)
    
    pc2_mean_i <- mean(pop_dat_i$PC2, na.rm = TRUE)
    pc2_sd_i <- sd(pop_dat_i$PC2, na.rm = TRUE)
    
    if (!is.finite(Year_sd_i) || Year_sd_i == 0 ||
        !is.finite(pc1_sd_i) || pc1_sd_i == 0 ||
        !is.finite(pc2_sd_i) || pc2_sd_i == 0) {
      stop("Invalid standardization SD in PC imputation ", pc_imp_i)
    }
    
    
    pop_dat_i <- pop_dat_i %>%
      mutate(
        Year_std = (Year - Year_mean_i) / Year_sd_i,
        PC1_std = (PC1 - pc1_mean_i) / pc1_sd_i,
        PC2_std = (PC2 - pc2_mean_i) / pc2_sd_i
      )
    
    
    # Prediction grid uses the SAME PC imputation and scaling
    pred_grid_i <- env_pc_i %>%
      mutate(
        Year_std = (Year - Year_mean_i) / Year_sd_i,
        PC1_std = (PC1 - pc1_mean_i) / pc1_sd_i,
        PC2_std = (PC2 - pc2_mean_i) / pc2_sd_i
      ) %>%
      filter(
        is.finite(Year_std),
        is.finite(PC1_std),
        is.finite(PC2_std),
        !is.na(Endemic)
      ) %>%
      dplyr::select(
        .imp_pc, Country, Country_key, gbd_region,
        Year, Year_std, Endemic, PC1_std, PC2_std
      ) %>%
      arrange(Country, Year)
    
    
    # Predictor matrices
    X_obs_i <- model.matrix(
      ~ Year_std + Endemic + PC1_std + PC2_std,
      data = pop_dat_i
    )[, -1, drop = FALSE]
    
    X_new_i <- model.matrix(
      ~ Year_std + Endemic + PC1_std + PC2_std,
      data = pred_grid_i
    )[, -1, drop = FALSE]
    
    
    pred_key_i <- pred_grid_i %>%
      transmute(.imp_pc, Country, gbd_region, Year, Endemic)
    
    
    # Stan data
    stan_data_i <- list(
      I = nrow(pop_dat_i),
      K = ncol(X_obs_i),
      X = X_obs_i,
      x_mixed = as.vector(pop_dat_i$x_mixed),
      
      S = nrow(study_lookup_i),
      study_idx = pop_dat_i$study_idx,
      
      B_def_excl_M = pop_dat_i$B_def_excl_M,
      B_def_excl_L = pop_dat_i$B_def_excl_L,
      
      has_B_exact = pop_dat_i$has_B_exact,
      has_B_int = pop_dat_i$has_B_int,
      N_B = pop_dat_i$N_B,
      y_B = pop_dat_i$y_B,
      yB_lb = pop_dat_i$yB_lb,
      yB_ub = pop_dat_i$yB_ub,
      
      has_M_exact = pop_dat_i$has_M_exact,
      has_M_int = pop_dat_i$has_M_int,
      N_M = pop_dat_i$N_M,
      y_M = pop_dat_i$y_M,
      yM_lb = pop_dat_i$yM_lb,
      yM_ub = pop_dat_i$yM_ub,
      
      has_L_exact = pop_dat_i$has_L_exact,
      has_L_int = pop_dat_i$has_L_int,
      N_L = pop_dat_i$N_L,
      y_L = pop_dat_i$y_L,
      yL_lb = pop_dat_i$yL_lb,
      yL_ub = pop_dat_i$yL_ub,
      
      has_BM_exact = pop_dat_i$has_BM_exact,
      has_BM_int = pop_dat_i$has_BM_int,
      N_BM = pop_dat_i$N_BM,
      y_BM = pop_dat_i$y_BM,
      yBM_lb = pop_dat_i$yBM_lb,
      yBM_ub = pop_dat_i$yBM_ub,
      
      N_new = nrow(X_new_i),
      X_new = X_new_i
    )
    
    
    # Fit separately within each PC imputation
    fit_i <- mod$sample(
      data = stan_data_i,
      seed = 123 + pc_imp_i,
      chains = n_chains,
      parallel_chains = n_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling_per_chain,
      adapt_delta = 0.95,
      max_treedepth = 12,
      refresh = 200
    )
    
    
    # Diagnostics
    diag_pars <- c("sigma_u_B", "sigma_u_M", "sigma_u_L", "mu_B", "mu_M", "mu_L")
    print(fit_i$summary(diag_pars))
    
    fit_summary_i <- fit_i$summary() %>%
      mutate(.imp_pc = pc_imp_i)
    
    fit_diag_pc_list[[pc_index_i]] <- fit_summary_i
    
    write_csv(
      fit_summary_i,
      file.path(bact_fever_dir, paste0("stan_fit_summary_PCimp", pc_imp_i, ".csv"))
    )
    
    
    study_count_pc_list[[pc_index_i]] <- tibble(
      .imp_pc = pc_imp_i,
      n_population_observations = nrow(pop_dat_i),
      n_studies = nrow(study_lookup_i),
      n_prediction_country_years = nrow(pred_grid_i)
    )
    
    
    # Posterior prediction draws
    draws_new_i <- posterior::as_draws_df(
      fit_i$draws("p_target_u5_new")
    )
    
    
    # Each PC imputation corresponds to 200 final Section-8 draw IDs
    # = 5 DHS imputations x 40 draws
    draw_map_i <- draw_design %>%
      filter(.imp_pc == pc_imp_i) %>%
      arrange(draw_id)
    
    n_needed_i <- nrow(draw_map_i)
    stopifnot(n_needed_i == 200)
    
    if (nrow(draws_new_i) < n_needed_i) {
      stop("Not enough posterior draws for PC imputation ", pc_imp_i)
    }
    
    
    # Randomly select 200 posterior draws and map them to the matching final draw IDs
    set.seed(9000 + pc_imp_i)
    
    posterior_row_i <- sample(
      seq_len(nrow(draws_new_i)),
      size = n_needed_i,
      replace = FALSE
    )
    
    draws_selected_i <- draws_new_i[
      posterior_row_i,
      ,
      drop = FALSE
    ]
    
    prediction_cols_i <- grep(
      "^p_target_u5_new\\[",
      names(draws_selected_i),
      value = TRUE
    )
    
    stopifnot(length(prediction_cols_i) == nrow(pred_key_i))
    
    draws_selected_i$draw_id <- draw_map_i$draw_id
    
    
    pred_long_i <- draws_selected_i %>%
      dplyr::select(draw_id, all_of(prediction_cols_i)) %>%
      pivot_longer(
        cols = all_of(prediction_cols_i),
        names_to = "param",
        values_to = "p_u5_nonL"
      ) %>%
      mutate(idx = extract_idx_in_brackets(param)) %>%
      left_join(
        pred_key_i %>% mutate(idx = row_number()),
        by = "idx"
      ) %>%
      left_join(
        draw_map_i %>%
          dplyr::select(draw_id, .draw_within_combo, .imp_pc, .imp_dhs),
        by = c("draw_id", ".imp_pc")
      ) %>%
      dplyr::select(
        draw_id, .draw_within_combo, .imp_pc, .imp_dhs,
        Country, gbd_region, Year, Endemic, p_u5_nonL
      )
    
    
    pred_long_pc_list[[pc_index_i]] <- pred_long_i
    
    rm(fit_i, draws_new_i, draws_selected_i, pred_long_i)
    gc()
  }
  
  
  
  ## 1-4. Combine PC-specific results --------------------------------
  
  pred_long <- bind_rows(pred_long_pc_list) %>%
    arrange(draw_id, Country, Year)
  
  
  # Check balanced 1000-draw structure
  afi_draw_check <- pred_long %>%
    distinct(draw_id, .draw_within_combo, .imp_pc, .imp_dhs) %>%
    arrange(draw_id)
  
  stopifnot(
    nrow(afi_draw_check) == 1000,
    setequal(afi_draw_check$draw_id, draw_design$draw_id)
  )
  
  afi_pc_balance_check <- afi_draw_check %>%
    count(.imp_pc, name = "n_draw")
  
  stopifnot(all(afi_pc_balance_check$n_draw == 200))
  
  print(afi_pc_balance_check)
  
  
  study_count_pc <- bind_rows(study_count_pc_list)
  fit_summary_all <- bind_rows(fit_diag_pc_list)
  
  print(study_count_pc)
  
  write_csv(
    study_count_pc,
    file.path(bact_fever_dir, "PC_imputation_analysis_counts.csv")
  )
  
  write_csv(
    fit_summary_all,
    file.path(bact_fever_dir, "stan_fit_summary_all_PC_imputations.csv")
  )
  
  
  # This is the PRIMARY output for Section 10
  saveRDS(
    pred_long,
    file.path(bact_fever_dir, "bact_fever_draws_1000_long.rds")
  )
  
  write_csv(
    pred_long,
    file.path(bact_fever_dir, "bact_fever_draws_1000_long.csv")
  )
  
  
  
  ## 1-5. Summary --------------------------------
  
  pred_summary_Country <- pred_long %>%
    group_by(Country, Year) %>%
    summarise(
      median = median(p_u5_nonL, na.rm = TRUE),
      low = quantile(p_u5_nonL, 0.025, na.rm = TRUE),
      high = quantile(p_u5_nonL, 0.975, na.rm = TRUE),
      mean = mean(p_u5_nonL, na.rm = TRUE),
      sd = sd(p_u5_nonL, na.rm = TRUE),
      n_draw = n_distinct(draw_id),
      .groups = "drop"
    ) %>%
    arrange(Country, Year)
  
  
  pred_summary_gbd_region <- pred_long %>%
    group_by(gbd_region, Year) %>%
    summarise(
      median = median(p_u5_nonL, na.rm = TRUE),
      low = quantile(p_u5_nonL, 0.025, na.rm = TRUE),
      high = quantile(p_u5_nonL, 0.975, na.rm = TRUE),
      mean = mean(p_u5_nonL, na.rm = TRUE),
      sd = sd(p_u5_nonL, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(gbd_region, Year)
  
  
  pred_summary_Year <- pred_long %>%
    group_by(Year) %>%
    summarise(
      median = median(p_u5_nonL, na.rm = TRUE),
      low = quantile(p_u5_nonL, 0.025, na.rm = TRUE),
      high = quantile(p_u5_nonL, 0.975, na.rm = TRUE),
      mean = mean(p_u5_nonL, na.rm = TRUE),
      sd = sd(p_u5_nonL, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Year)
  
  
  write_csv(
    pred_summary_Country,
    file.path(bact_fever_dir, "hierarch_Country_PCs.csv")
  )
  
  write_csv(
    pred_summary_gbd_region,
    file.path(bact_fever_dir, "hierarch_region_PCs.csv")
  )
  
  write_csv(
    pred_summary_Year,
    file.path(bact_fever_dir, "hierarch_Year_PCs.csv")
  )
  
  
  
  ## 1-6. Wide matrix for compatibility --------------------------------
  
  duplicate_draw_check <- pred_long %>%
    count(Country, Year, draw_id) %>%
    filter(n != 1)
  
  if (nrow(duplicate_draw_check) > 0) {
    stop("Duplicate Country-Year-draw combinations in AFI bacterial fraction output.")
  }
  
  pred_Country_Year_matrix_1000 <- pred_long %>%
    dplyr::select(Country, Year, draw_id, p_u5_nonL) %>%
    pivot_wider(
      names_from = draw_id,
      names_prefix = "draw_",
      values_from = p_u5_nonL
    ) %>%
    arrange(Country, Year)
  
  saveRDS(
    pred_Country_Year_matrix_1000,
    file.path(bact_fever_dir, "matrix_hierarch_Country_Year_PCs_1000.RDS")
  )
  
  
  pop_table <- pred_long %>%
    dplyr::select(Country, gbd_region) %>%
    distinct() %>%
    arrange(gbd_region, Country)
  
  Country_region_table <- with(pop_table, table(Country, gbd_region))
  
  write.csv(
    Country_region_table,
    file.path(bact_fever_dir, "hierarch_Country_region_table_PCs.csv")
  )
  
  
  
  ## 1-7. draw plots --------------------------------
  
  pdf(
    "figures/supplementary/bacterial_proportion/S0-1.Proportion_of_bacterial_infection_without_LRTI.pdf",
    width = 12,
    height = 6
  )
  
  layout_matrix <- matrix(c(1, 2), nrow = 1, byrow = TRUE)
  layout(layout_matrix, widths = c(1, 1), heights = c(1))
  
  
  # Data Preparation & Parameters
  df_year <- pred_summary_Year[order(pred_summary_Year$Year), ]
  df_reg <- pred_summary_gbd_region[
    order(pred_summary_gbd_region$gbd_region, pred_summary_gbd_region$Year),
  ]
  
  xmin <- min(df_year$Year, na.rm = TRUE)
  xmax <- max(df_year$Year, na.rm = TRUE)
  ymin <- 0
  ymax <- 0.50
  
  par(oma = c(0, 0, 3, 0))
  par(mar = c(3, 4, 2.5, 1))
  par(mgp = c(2, 0.5, 0))
  par(tck = -0.025)
  par(lwd = 1)
  
  
  draw_custom_axes <- function(ymin, ymax) {
    x_at <- seq(xmin, xmax, by = 3)
    y_at <- seq(ymin, ymax, by = 0.1)
    
    axis(1, at = x_at, labels = FALSE, las = 2, pos = ymin)
    text(
      x = x_at,
      y = ymin - (ymax - ymin) * 0.08,
      labels = x_at,
      xpd = TRUE,
      cex = 1
    )
    
    axis(2, at = y_at, labels = FALSE, las = 2, pos = xmin)
    text(
      y = y_at,
      x = xmin - 0.04 * (xmax - xmin),
      labels = sprintf("%.2f", y_at),
      srt = 0,
      adj = 1,
      xpd = TRUE,
      cex = 1
    )
  }
  
  
  # PANEL 1: Overall By Year
  plot(
    median ~ Year,
    data = df_year,
    xlab = "",
    ylab = "",
    pch = 16,
    col = rgb(0.8, 0.8, 0.8, 0),
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    axes = FALSE,
    ann = FALSE
  )
  
  draw_custom_axes(ymin, ymax)
  
  polygon(
    c(df_year$Year, rev(df_year$Year)),
    c(df_year$low, rev(df_year$high)),
    col = adjustcolor("#0072B2", alpha.f = 0.25),
    border = NA
  )
  
  lines(median ~ Year, data = df_year, col = "#0072B2", lwd = 1.5)
  
  mtext("Year", side = 1, line = 1.5, cex = 0.8)
  mtext("Predicted proportion", side = 2, line = 3, cex = 0.8)
  mtext("A. Overall", at = xmin, side = 3, line = 0.5, adj = 0, cex = 1, font = 2)
  
  
  # PANEL 2: By GBD Region
  region_cols <- c(
    "North Africa and Middle East" = "#009E73",
    "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7",
    "Latin America and Caribbean" = "#E69F00",
    "Southeast Asia, East Asia, and Oceania" = "#D55E00",
    "South Asia" = "#A7B97F",
    "Sub-Saharan Africa" = "#0072B2"
  )
  
  regions <- names(region_cols)
  
  plot(
    median ~ Year,
    data = df_reg[df_reg$gbd_region == regions[1], ],
    xlab = "",
    ylab = "",
    pch = 16,
    col = rgb(0.8, 0.8, 0.8, 0),
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    axes = FALSE,
    ann = FALSE
  )
  
  draw_custom_axes(ymin, ymax)
  
  avail_regions <- unique(df_reg$gbd_region)
  
  for (r in regions) {
    if (r %in% avail_regions) {
      sub_df <- df_reg[df_reg$gbd_region == r, ]
      
      polygon(
        c(sub_df$Year, rev(sub_df$Year)),
        c(sub_df$low, rev(sub_df$high)),
        col = adjustcolor(region_cols[r], alpha.f = 0.25),
        border = NA
      )
      
      lines(median ~ Year, data = sub_df, col = region_cols[r], lwd = 1.5)
    }
  }
  
  mtext("Year", side = 1, line = 1.5, cex = 0.8)
  mtext("B. By GBD Region", at = xmin, side = 3, line = 0.5, adj = 0, cex = 1, font = 2)
  
  legend(
    "topright",
    legend = regions,
    col = region_cols,
    lwd = 2,
    bty = "n",
    cex = 1,
    ncol = 1
  )
  
  mtext(
    "Proportion of Bacterial Infections Among Acute Febrile Illnesses",
    side = 3,
    outer = TRUE,
    line = 0.5,
    cex = 1,
    font = 2,
    adj = 0
  )
  
  dev.off()
  
  
  cat("\nAFI bacterial-fraction draws by PC x DHS imputation:\n")
  
  print(
    pred_long %>%
      distinct(draw_id, .imp_pc, .imp_dhs) %>%
      count(.imp_pc, .imp_dhs)
  )
  
  
  
  ## 2. LRTI --------------------------------
  
  # PERCH bacterial fraction does not depend on PC or DHS imputations.
  # Generate one bacterial-fraction draw for each of the same 1000 draw IDs.
  
  set.seed(123)
  
  ### Perch data correlation with environmental factors ###
  
  perch_env <- read.csv("data/metadata/perch_site_env.csv")
  
  env_vars <- c(
    "Absolute_latitude",
    "Elevation",
    "Mean.temperature",
    "Mean_relative_humidity",
    "precipitation",
    "GDP"
  )
  
  cor_results <- data.frame(
    variable = env_vars,
    n = NA_integer_,
    spearman_rho = NA_real_,
    p_value = NA_real_
  )
  
  for (j in seq_along(env_vars)) {
    keep_j <- complete.cases(perch_env[, c("Bacterial_etiology", env_vars[j])])
    
    test_j <- cor.test(
      perch_env$Bacterial_etiology[keep_j],
      perch_env[[env_vars[j]]][keep_j],
      method = "spearman",
      exact = FALSE
    )
    
    cor_results$n[j] <- sum(keep_j)
    cor_results$spearman_rho[j] <- unname(test_j$estimate)
    cor_results$p_value[j] <- test_j$p.value
  }
  
  cor_results$p_value_FDR <- p.adjust(cor_results$p_value, method = "BH")
  cor_results$spearman_rho <- round(cor_results$spearman_rho, digits = 4)
  cor_results$p_value <- round(cor_results$p_value, digits = 4)
  cor_results$p_value_FDR <- round(cor_results$p_value_FDR, digits = 4)
  
  cor_results
  
  write.csv(
    cor_results,
    file.path(bact_lrti_dir, "PERCH_environmental_correlations.csv"),
    row.names = FALSE
  )
  
  
  
  ### Perch data ###
  
  # BACTERIAL ETIOLOGY PROPORTIONS
  
  n_sim <- nrow(draw_design)
  stopifnot(n_sim == 1000)
  
  # 1. Define the PERCH estimates
  prop_bact <- data.frame(
    agegroup = c("1-5m", "6-11m", "12-23m", "24-59m"),
    PE = c(0.238, 0.251, 0.335, 0.306),
    LL = c(0.191, 0.196, 0.266, 0.226),
    UL = c(0.286, 0.307, 0.415, 0.392)
  )
  
  
  # 2. Optimization function to find Beta Shape 1 & Shape 2
  get_beta_parms <- function(PE, LL, UL) {
    loss_function <- function(NU) {
      a <- PE * NU
      b <- (1 - PE) * NU
      est_ll <- qbeta(0.025, a, b)
      est_ul <- qbeta(0.975, a, b)
      
      (est_ll - LL)^2 + (est_ul - UL)^2
    }
    
    opt <- optimize(loss_function, interval = c(1, 10000))
    best_NU <- opt$minimum
    
    c(
      shape1 = PE * best_NU,
      shape2 = (1 - PE) * best_NU
    )
  }
  
  
  # 3. Extract parameters
  parms_list <- lapply(seq_len(nrow(prop_bact)), function(i) {
    get_beta_parms(prop_bact$PE[i], prop_bact$LL[i], prop_bact$UL[i])
  })
  
  prop_bact$shape1 <- sapply(parms_list, `[[`, "shape1")
  prop_bact$shape2 <- sapply(parms_list, `[[`, "shape2")
  
  
  # 4. Generate 1000 draws
  posterior_matrix <- sapply(seq_len(nrow(prop_bact)), function(i) {
    rbeta(n_sim, prop_bact$shape1[i], prop_bact$shape2[i])
  })
  
  
  # 5. Blend 1-5m and 6-11m using PERCH sample weights
  # 3217 cases for 1-5m and 2118 cases for 6-11m
  prop.x <- 3217 / (3217 + 2118)
  
  prop_bact_LRTI_0_11m <- posterior_matrix[,1] * prop.x + posterior_matrix[,2] * (1 - prop.x)
  prop_bact_LRTI_12_23m <- posterior_matrix[,3]
  prop_bact_LRTI_24_59m <- posterior_matrix[,4]
  
  
  # Primary long-format object for Section 10
  prop_bact_LRTI_draws <- draw_design %>%
    arrange(draw_id) %>%
    mutate(
      prop_bact_0_11m = prop_bact_LRTI_0_11m,
      prop_bact_12_23m = prop_bact_LRTI_12_23m,
      prop_bact_24_59m = prop_bact_LRTI_24_59m
    )
  
  saveRDS(
    prop_bact_LRTI_draws,
    file.path(bact_lrti_dir, "bact_lrti_draws_1000.rds")
  )
  
  write.csv(
    prop_bact_LRTI_draws,
    file.path(bact_lrti_dir, "bact_lrti_draws_1000.csv"),
    row.names = FALSE
  )
  
  
  # Retain old matrix format for compatibility
  prop_bact_LRTI <- cbind(
    prop_bact_LRTI_0_11m,
    prop_bact_LRTI_12_23m,
    prop_bact_LRTI_24_59m
  )
  
  colnames(prop_bact_LRTI) <- c("0_11m", "12_23m", "24_59m")
  
  saveRDS(
    prop_bact_LRTI,
    file.path(bact_lrti_dir, "bact_lrti_matrix_1000.RDS")
  )
  
  
  
  # Final checks -----------------------------------------------------------
  
  cat("\nAFI final draw count =", n_distinct(pred_long$draw_id), "\n")
  cat("LRTI final draw count =", nrow(prop_bact_LRTI_draws), "\n")
  
  stopifnot(
    n_distinct(pred_long$draw_id) == 1000,
    nrow(prop_bact_LRTI_draws) == 1000
  )
  
  message(
    "Section 9 complete: AFI bacterial fractions fitted separately across five PC imputations."
  )



  # _____________________________________________ ----------
  # 10. Incidence of bacterial infection ---------------------------------------
  
  rm(list =ls()) ; gc()
  library(dplyr)
  library(tidyr)
  
  save_dir <- "outputs/pipelines/bacterial_incidence"
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  fever_incidence_age <- readRDS("outputs/pipelines/age_specific_selected_model_uncertainty/fever_age_marginal_RE0_lm_uncertainty_projection_draws.rds")
  lrti_incidence_age <- readRDS("outputs/pipelines/age_specific_selected_model_uncertainty/lrti_age_marginal_RE0_enet025_uncertainty_projection_draws.rds")
  prop_bact_fever <- readRDS("outputs/pipelines/bact_fever/bact_fever_draws_1000_long.rds")
  prop_bact_LRTI <- readRDS("outputs/pipelines/bact_lrti/bact_lrti_draws_1000.rds")
  
  dim(fever_incidence_age)
  dim(lrti_incidence_age)
  dim(prop_bact_fever)
  dim(prop_bact_LRTI)
  
  stopifnot(
    n_distinct(fever_incidence_age$draw_id) == 1000,
    n_distinct(lrti_incidence_age$draw_id) == 1000,
    n_distinct(prop_bact_fever$draw_id) == 1000,
    n_distinct(prop_bact_LRTI$draw_id) == 1000
  )
  
  
  ## 1. Fever / AFI --------------------------------
  
  # check matching Country-Year-draw-PC-DHS combinations
  fever_missing <- fever_incidence_age %>%
    distinct(Country, Year, draw_id, .imp_pc, .imp_dhs) %>%
    anti_join(
      prop_bact_fever %>% distinct(Country, Year, draw_id, .imp_pc, .imp_dhs),
      by = c("Country", "Year", "draw_id", ".imp_pc", ".imp_dhs")
    )
  
  fever_missing
  
  if(nrow(fever_missing) > 0){
    stop("Some fever incidence draws do not have matching bacterial-fraction draws.")
  }
  
  
  # bacterial incidence = AFI incidence x bacterial fraction
  inc_bact_fever_long <- fever_incidence_age %>%
    inner_join(
      prop_bact_fever %>% dplyr::select(Country, Year, draw_id, .imp_pc, .imp_dhs, p_u5_nonL),
      by = c("Country", "Year", "draw_id", ".imp_pc", ".imp_dhs")
    ) %>%
    mutate(pred_bacterial_rate = pred_rate * p_u5_nonL)
  
  stopifnot(nrow(inc_bact_fever_long) == nrow(fever_incidence_age))
  
  
  fever_incidence_0_11 <- inc_bact_fever_long %>% filter(age_group == "0-11 months")
  fever_incidence_12_23 <- inc_bact_fever_long %>% filter(age_group == "12-23 months")
  fever_incidence_24_59 <- inc_bact_fever_long %>% filter(age_group == "24-59 months")
  
  
  # Country-Year x 1000 draws
  inc_bact_fever_0_11 <- fever_incidence_0_11 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  inc_bact_fever_12_23 <- fever_incidence_12_23 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  inc_bact_fever_24_59 <- fever_incidence_24_59 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  
  inc_bact_fever_0_11_matrix <- inc_bact_fever_0_11 %>% dplyr::select(-Country, -Year)
  inc_bact_fever_12_23_matrix <- inc_bact_fever_12_23 %>% dplyr::select(-Country, -Year)
  inc_bact_fever_24_59_matrix <- inc_bact_fever_24_59 %>% dplyr::select(-Country, -Year)
  
  
  
  ## 2. LRTI --------------------------------
  
  # PERCH bacterial fraction is not country-specific,
  # but each bacterial fraction draw is matched to the same final draw_id
  lrti_missing <- lrti_incidence_age %>%
    distinct(draw_id, .imp_pc, .imp_dhs) %>%
    anti_join(
      prop_bact_LRTI %>% distinct(draw_id, .imp_pc, .imp_dhs),
      by = c("draw_id", ".imp_pc", ".imp_dhs")
    )
  
  lrti_missing
  
  if(nrow(lrti_missing) > 0){
    stop("Some LRTI incidence draws do not have matching PERCH bacterial-fraction draws.")
  }
  
  
  inc_bact_lrti_long <- lrti_incidence_age %>%
    left_join(
      prop_bact_LRTI %>% dplyr::select(draw_id, .imp_pc, .imp_dhs, prop_bact_0_11m, prop_bact_12_23m, prop_bact_24_59m),
      by = c("draw_id", ".imp_pc", ".imp_dhs")
    ) %>%
    mutate(
      prop_bact = case_when(
        age_group == "0-11 months" ~ prop_bact_0_11m,
        age_group == "12-23 months" ~ prop_bact_12_23m,
        age_group == "24-59 months" ~ prop_bact_24_59m,
        TRUE ~ NA_real_
      ),
      pred_bacterial_rate = pred_rate * prop_bact
    )
  
  stopifnot(
    nrow(inc_bact_lrti_long) == nrow(lrti_incidence_age),
    all(is.finite(inc_bact_lrti_long$prop_bact))
  )
  
  
  lrti_incidence_0_11 <- inc_bact_lrti_long %>% filter(age_group == "0-11 months")
  lrti_incidence_12_23 <- inc_bact_lrti_long %>% filter(age_group == "12-23 months")
  lrti_incidence_24_59 <- inc_bact_lrti_long %>% filter(age_group == "24-59 months")
  
  
  # Country-Year x 1000 draws
  inc_bact_lrti_0_11 <- lrti_incidence_0_11 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  inc_bact_lrti_12_23 <- lrti_incidence_12_23 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  inc_bact_lrti_24_59 <- lrti_incidence_24_59 %>% dplyr::select(Country, Year, draw_id, pred_bacterial_rate) %>% pivot_wider(names_from = draw_id, values_from = pred_bacterial_rate) %>% arrange(Country, Year)
  
  inc_bact_lrti_0_11_matrix <- inc_bact_lrti_0_11 %>% dplyr::select(-Country, -Year)
  inc_bact_lrti_12_23_matrix <- inc_bact_lrti_12_23 %>% dplyr::select(-Country, -Year)
  inc_bact_lrti_24_59_matrix <- inc_bact_lrti_24_59 %>% dplyr::select(-Country, -Year)
  
  
  
  ## 3. Check --------------------------------
  
  dim(inc_bact_fever_0_11)
  dim(inc_bact_fever_12_23)
  dim(inc_bact_fever_24_59)
  
  dim(inc_bact_lrti_0_11)
  dim(inc_bact_lrti_12_23)
  dim(inc_bact_lrti_24_59)
  
  # Country + Year + 1000 draws
  stopifnot(
    ncol(inc_bact_fever_0_11) == 1002,
    ncol(inc_bact_fever_12_23) == 1002,
    ncol(inc_bact_fever_24_59) == 1002,
    ncol(inc_bact_lrti_0_11) == 1002,
    ncol(inc_bact_lrti_12_23) == 1002,
    ncol(inc_bact_lrti_24_59) == 1002
  )
  
  # Country-Year ordering should be the same across age groups
  stopifnot(
    identical(inc_bact_fever_0_11[,1:2], inc_bact_fever_12_23[,1:2]),
    identical(inc_bact_fever_0_11[,1:2], inc_bact_fever_24_59[,1:2]),
    identical(inc_bact_lrti_0_11[,1:2], inc_bact_lrti_12_23[,1:2]),
    identical(inc_bact_lrti_0_11[,1:2], inc_bact_lrti_24_59[,1:2])
  )
  
  
  
  ## 4. Save --------------------------------
  
  saveRDS(inc_bact_fever_0_11_matrix  , "outputs/pipelines/bacterial_incidence/inc_bact_fever_0_11_matrix.rds")
  saveRDS(inc_bact_fever_12_23_matrix , "outputs/pipelines/bacterial_incidence/inc_bact_fever_12_23_matrix.rds")
  saveRDS(inc_bact_fever_24_59_matrix , "outputs/pipelines/bacterial_incidence/inc_bact_fever_24_59_matrix.rds")
  saveRDS(inc_bact_lrti_0_11_matrix   , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_0_11_matrix.rds")
  saveRDS(inc_bact_lrti_12_23_matrix  , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_12_23_matrix.rds")
  saveRDS(inc_bact_lrti_24_59_matrix  , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_24_59_matrix.rds")
  
  saveRDS(inc_bact_fever_0_11  , "outputs/pipelines/bacterial_incidence/inc_bact_fever_0_11.rds")
  saveRDS(inc_bact_fever_12_23 , "outputs/pipelines/bacterial_incidence/inc_bact_fever_12_23.rds")
  saveRDS(inc_bact_fever_24_59 , "outputs/pipelines/bacterial_incidence/inc_bact_fever_24_59.rds")
  saveRDS(inc_bact_lrti_0_11   , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_0_11.rds")
  saveRDS(inc_bact_lrti_12_23  , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_12_23.rds")
  saveRDS(inc_bact_lrti_24_59  , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_24_59.rds")
  
  # keep long-format draws with MI identifiers
  saveRDS(inc_bact_fever_long, "outputs/pipelines/bacterial_incidence/inc_bact_fever_all_age_draws_long.rds")
  saveRDS(inc_bact_lrti_long , "outputs/pipelines/bacterial_incidence/inc_bact_lrti_all_age_draws_long.rds")




