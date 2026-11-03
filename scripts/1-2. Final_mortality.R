# _____________________________________________ ----------
# 0. Generating Principal components for mortality using UN HNP index -------------------

# 78 countries, GDP per capita <20000

rm(list = ls())
gc()

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
    # "Mortality rate, infant (per 1,000 live births)",
    # "Mortality rate, infant, female (per 1,000 live births)",
    # "Mortality rate, infant, male (per 1,000 live births)",
    # "Mortality rate, neonatal (per 1,000 live births)",
    # "Mortality rate, under-5 (per 1,000)",
    # "Mortality rate, under-5, female (per 1,000)",
    # "Mortality rate, under-5, male (per 1,000)",
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

colnames(df.numeric) # 75 variables



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
saveRDS(mi.pca, "outputs/pipelines/mipca_mortality_m5_bayes.rds")
saveRDS(pca.results, "outputs/pipelines/pca_models_mortality_mi.rds")


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

pdf("figures/supplementary/Distribution of UNHNP by year_mortality.pdf", width = 12, height = 8)

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

pdf("figures/supplementary/Distribution of PC by year_before exclusion_mortality.pdf", width = 12, height = 12)

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
  "outputs/supplementary/result_mortality_2023_PC_loadings_by_imputation.csv",
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

write.csv(result_PC1, "outputs/supplementary/result_mortality_2023_PC1.csv", row.names = FALSE)
write.csv(result_PC2, "outputs/supplementary/result_mortality_2023_PC2.csv", row.names = FALSE)

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
  "outputs/supplementary/result_mortality_2023_PC_subspace_stability.csv",
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

pdf("figures/supplementary/Distribution of PC by year_after exclusion_mortality.pdf", width = 12, height = 12)

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
  "outputs/supplementary/result_mortality_2023_PC_variance_explained.csv",
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

pdf("figures/supplementary/proportion of variance explained by principal components_mortality.pdf", width = 7.5, height = 4)

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

# Keep all five PC imputations for the primary mortality analyses.
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

# One row per Country-Year containing only PC-invariant metadata.
UNI_base <- UNI %>%
  dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
  distinct() %>%
  arrange(Country, Year)

# First imputation is retained only for the existing one-row-per-Country-Year
# descriptive CSV. Primary downstream analyses use UNI with all five imputations.
UNI_plot <- UNI %>%
  filter(.imp_pc == 1) %>%
  dplyr::select(-.imp_pc) %>%
  arrange(Country, Year)

head(UNI)
sum(is.na(UNI))
table(UNI$.imp_pc)

stopifnot(
  n_distinct(UNI$.imp_pc) == 5,
  nrow(UNI) == 5 * nrow(UNI_base),
  all((UNI %>% count(Country, Year) %>% pull(n)) == 5),
  nrow(UNI_plot) == nrow(UNI_base)
)

write.csv(UNI_plot, "outputs/sub_results/UNI.data_2023_mortality.csv", row.names = FALSE)

# Primary downstream PCA file: five imputation-specific PC datasets.
saveRDS(UNI, "outputs/pipelines/pc_gdp_2023_mortality.rds")

# PC-invariant metadata file: one row per Country-Year.
saveRDS(UNI_base, "outputs/pipelines/pc_gdp_2023_mortality_base.rds")

# Retain the existing mortality PCA-results filename for downstream compatibility.
saveRDS(pca.results, "outputs/pipelines/mortality_pca_results_5imp.rds")

































# _____________________________________________ ----------
# 1. DHS data cleaning ----------------------------------------------------


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
datasets_to_remove <- c("ETKR61FL") # remove 2003 year only data
extract <- extract[!names(extract) %in% datasets_to_remove]
length(extract) #164 (2003~2023)


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

## datasets without variables: -6 (remaining: 140)
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
    int_month = v008)

# check the duplication
combined_data <- combined_data %>% mutate(ch_id = paste(filename, caseid, ch_bth_index, sep = ""))
any(duplicated(combined_data$ch_id)) # check duplication of child_id: none
unique(any(duplicated(combined_data$ch_id)))
combined_data <- combined_data %>% dplyr::select(-c("caseid", "ch_bth_index"))
#age in month
combined_data$ch_age_month <- combined_data$int_month - combined_data$ch_bth_month


##selecting dataset
analysis_data <- combined_data %>%
  dplyr::select(c("Country", "Year", "ch_id", "ch_sex", "ch_age_month","ch_alive","ch_dth_age","ch_dth_month", "sample_weight",
                  "survey_code", "survey_v"))

analysis_data$sample_weight <- analysis_data$sample_weight / 1000000

# make Country_Year variable
with(analysis_data, table(Year, Country, useNA ='always'))

datapoint_table <- with(analysis_data, table(Year, Country, useNA ='always'))
as.data.frame(datapoint_table) %>% filter(Freq < 1000, Freq > 0) 

analysis_data$CountryYear <- paste(analysis_data$Country, analysis_data$Year, sep = "_")
unique(analysis_data$CountryYear) #201
with(analysis_data, table(CountryYear, ch_alive, useNA = "always"))
analysis_data <- analysis_data %>% filter(Year > 2004)
length(unique(analysis_data$CountryYear)) #199

a <- analysis_data %>% filter(ch_alive == 0) %>% group_by(CountryYear) %>% count()
a[a$n <20,] 

print(analysis_data %>% filter(Country %in% c("Albania", "Armenia", "Cambodia", "Cameroon", 
                                              "Colombia", "Gabon", "Ghana", "Jordan", "Lesotho", 
                                              "Maldives", "Sao Tome and Principe", "Ukraine", "Zambia")) %>% group_by(CountryYear) %>% count(), n = Inf)

analysis_data$CountryYear[analysis_data$CountryYear == "Albania_2008"] <- "Albania_2009" 
analysis_data$CountryYear[analysis_data$CountryYear == "Albania_2018"] <- "Albania_2017" 
analysis_data$CountryYear[analysis_data$CountryYear == "Armenia_2015"] <- "Armenia_2016" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cambodia_2011"] <- "Cambodia_2010" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cambodia_2022"] <- "Cambodia_2021" 
analysis_data$CountryYear[analysis_data$CountryYear == "Cameroon_2019"] <- "Cameroon_2018"
analysis_data$CountryYear[analysis_data$CountryYear == "Gabon_2019"] <- "Gabon_2020"
analysis_data$CountryYear[analysis_data$CountryYear == "Jordan_2018"] <- "Jordan_2017"
analysis_data$CountryYear[analysis_data$CountryYear == "Maldives_2016"] <- "Maldives_2017"
analysis_data$CountryYear[analysis_data$CountryYear == "Sao Tome and Principe_2009"] <- "Sao Tome and Principe_2008"
analysis_data$CountryYear[analysis_data$CountryYear == "Zambia_2019"] <- "Zambia_2018"

analysis_data$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", analysis_data$CountryYear)) # applying same change to Year variable.

analysis_data[analysis_data$Country == "Egypt",]$Country <- "Egypt, Arab Rep."
analysis_data[analysis_data$Country == "Yemen",]$Country <- "Yemen, Rep."
analysis_data[analysis_data$Country == "Democratic Republic of the Congo",]$Country <- "Congo, Dem. Rep."
analysis_data[analysis_data$Country == "Republic of the Congo",]$Country <- "Congo, Rep." 
analysis_data[analysis_data$Country == "Gambia",]$Country <- "Gambia, The"
analysis_data[analysis_data$Country == "Turkey",]$Country <- "Turkiye"


## adding GBD regions without duplicating DHS records -----------------------

UNI_model <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")
if (!".imp_pc" %in% names(UNI_model)) stop("Mortality PCA file must contain .imp_pc.")

UNI_base <- if (file.exists("outputs/pipelines/pc_gdp_2023_mortality_base.rds")) {
  readRDS("outputs/pipelines/pc_gdp_2023_mortality_base.rds")
} else {
  UNI_model %>%
    dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
    distinct()
}

# The first PC imputation is used only for descriptive plots.
# Primary analyses use all five imputation-specific PC datasets.
UNI_plot <- UNI_model %>%
  filter(.imp_pc == 1) %>%
  dplyr::select(Country, Year, PC1, PC2, gbd_region)

a <- unique(UNI_base$Country)
b <- unique(analysis_data$Country)
setdiff(a,b)
setdiff(b,a)

with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear))) # 59 countries, 189 country-year (Maldives were excluded)
analysis_data <- analysis_data %>% filter(!Country %in% c("Haiti", "Maldives", "Sao Tome and Principe", "Yemen, Rep."))

# Join only PC-invariant metadata here. Joining UNI_model directly would create
# five copies of every DHS child because UNI_model has five PC imputations.
analysis_data <- analysis_data %>%
  left_join(UNI_base %>% dplyr::select(Country, Year, gbd_region), by = c("Country", "Year"))

with(analysis_data,length(unique(Country))); with(analysis_data,length(unique(CountryYear)))
nrow(distinct(analysis_data[,c("Country","Year")]))
with((distinct(analysis_data[,c("Country","Year","gbd_region")])), table(gbd_region, useNA = "always"))
analysis_data %>% distinct(Country, Year, gbd_region) %>% count(gbd_region)
analysis_data %>% distinct(Country, gbd_region) %>% count(gbd_region)
with(analysis_data, table(Country, Year), useNA = "always")




# _____________________________________________ ----------
# 2. Estimate age-specific risks of all-cause death -----------------------------

unique(analysis_data$ch_dth_age)
analysis_data$ch_dth_age <- as.character(analysis_data$ch_dth_age)

analysis_data <- analysis_data %>%
  mutate(
    ch_dth_age_raw =
      suppressWarnings(as.integer(as.character(ch_dth_age))),
    
    death_event = as.integer(ch_alive == 0),
    
    death_unit  = ch_dth_age_raw %/% 100,
    death_value = ch_dth_age_raw %% 100,
    
    ch_dth_age = case_when(
      death_event == 0 ~ NA_real_,
      
      ch_dth_age_raw %in%
        c(99, 297, 298, 299, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 318, 323, 324, 326, 327, 336, 340, 397, 399, 997, 998, 999) ~ NA_real_,
      
      # Completed days
      death_unit == 1 &
        death_value <= 98 ~
        as.numeric(death_value),
      
      # Completed months: use interval midpoint
      death_unit == 2 &
        death_value <= 59 ~
        (death_value + 0.5) * 365.25 / 12,
      
      # Completed years: use interval midpoint
      death_unit == 3 &
        death_value <= 4 ~
        (death_value + 0.5) * 365.25,
      
      TRUE ~ NA_real_
    )
  )


analysis_data <- analysis_data %>%
  filter(
    !(death_event == 1 & is.na(ch_dth_age))
  ) %>%
  mutate(
    age_at_interview = ch_age_month * 365.25 / 12,
    
    time = if_else(
      death_event == 1,
      ch_dth_age,
      age_at_interview
    ),
    
    # Only replace zero rather than adding 0.1 to every record
    time_fixed = pmax(time, 0.1)
  )

par(mfrow = c(1,1))

with(analysis_data, table(CountryYear, ch_dth_age, useNA = "always"))
hist(analysis_data$ch_dth_age, main = "Histogram of children's age at death", xlab = "Age at death")

cy <- unique(analysis_data$CountryYear)

pdf("figures/main_analysis/mortality/S1.crude_distribution of age at death by country year.pdf", width = 6, height = 4)
par(mfrow = c(7,6), mar = c(2,2,1,1))
for(i in 1:length(cy)){
  country_year <- cy[i]
  plot_data <- analysis_data %>% filter(CountryYear == country_year, !is.na(ch_dth_age)) %>% pull(ch_dth_age)
  if(length(plot_data) > 0){
    hist(plot_data, main = country_year, xlab = "data", ylab = "number of death", cex.main = 0.8)
  }
}
dev.off()

# Get a list of all unique country-years
cy_list <- unique(analysis_data$CountryYear)
country_list <- unique(analysis_data$Country)
year_list <- unique(analysis_data$Year)

# Create a list to store the results
all_fits <- list()

# Loop through and fit each one individually
for (cy in cy_list) {
  message("Processing: ", cy)
  sub_data <- subset(analysis_data, CountryYear == cy)
  sub_data <- subset(sub_data, !is.na(sample_weight) & sample_weight > 0)
  sub_data$norm_weight <- sub_data$sample_weight / mean(sub_data$sample_weight)
  fit <- try(flexsurvreg(Surv(time_fixed, death_event) ~ 1, 
                         data = sub_data, dist = "weibull", weights = norm_weight), silent = TRUE)
  all_fits[[cy]] <- fit
}



pdf(
  "figures/main_analysis/mortality/S2.Weibull fitting by country year.pdf",
  width = 6,
  height = 8
)

par(
  mfrow = c(8, 6),
  mar = c(2, 2, 1.5, 1),
  mgp = c(1.1, 0.25, 0),
  bty = "l"
)

for (cy in cy_list) {
  
  fit <- all_fits[[cy]]
  
  if (inherits(fit, "try-error") || is.null(fit)) {
    message("Skipping plot for ", cy, " - Model failed to fit.")
    next
  }
  
  try({
    
    Y <- fit$data$Y
    
    if ("stop" %in% colnames(Y)) {
      time <- Y[, "stop"]
    } else {
      time <- Y[, "time"]
    }
    
    status <- Y[, "status"]
    event_times <- time[status == 1 & is.finite(time)]
    
    if (length(event_times) == 0) {
      message("Skipping plot for ", cy, " - No observed deaths.")
      next
    }
    
    max_plot_time <- min(365, max(event_times))
    
    observed_hazard <- muhaz::muhaz(
      times = time,
      delta = status,
      min.time = 0,
      max.time = max_plot_time
    )
    
    day0_index <- which.min(abs(observed_hazard$est.grid))
    day0_hazard <- observed_hazard$haz.est[day0_index]
    
    if (!is.finite(day0_hazard) || day0_hazard <= 0) {
      day0_hazard <- max(
        observed_hazard$haz.est[is.finite(observed_hazard$haz.est)],
        na.rm = TRUE
      )
    }
    
    y_max <- day0_hazard * 1.05
    
    plot_title <- if (grepl("^Democratic Republic of the Congo_", cy)) {
      sub(
        "^Democratic Republic of the Congo_(\\d{4})$",
        "DR Congo_\\1",
        cy
      )
    } else {
      cy
    }
    
    plot(
      fit,
      type = "hazard",
      ylab = expression(h(t)),
      xlim = c(0, 365),
      ylim = c(0, y_max),
      main = plot_title,
      cex.main = 0.55,
      ci = FALSE,
      cex.lab = 0.45,
      min.time = 0,
      max.time = max_plot_time,
      xaxt = "n",
      yaxt = "n",
      bty = "l",
      lwd = 2 / 3
    )
    
    axis(
      side = 1,
      at = c(0, 365),
      labels = c(0, 365),
      cex.axis = 0.45,
      tck = -0.05,
      mgp = c(1.1, 0.25, 0)
    )
    
    axis(
      side = 2,
      at = c(0, day0_hazard),
      labels = round(c(0, day0_hazard), digits = 3),
      las = 2,
      cex.axis = 0.45,
      tck = -0.05,
      mgp = c(1.1, 0.25, 0)
    )
    
  }, silent = TRUE)
}

dev.off()


set.seed(123)

n_sim <- 1000

simulated_mortality_list <- vector("list", length(all_fits))
names(simulated_mortality_list) <- names(all_fits)

for (fit_index_i in seq_along(all_fits)) {
  country_year_i <- names(all_fits)[fit_index_i]
  fit_i <- all_fits[[fit_index_i]]
  
  if (inherits(fit_i, "try-error") || is.null(fit_i)) {
    message("Skipping simulation for ", country_year_i, " - Model failed to fit.")
    next
  }
  
  sims_log_i <- MASS::mvrnorm(n = n_sim, mu = fit_i$res.t[, "est"], Sigma = vcov(fit_i))
  shape_sims_i <- exp(sims_log_i[, "shape"])
  scale_sims_i <- exp(sims_log_i[, "scale"])
  
  p0_i <- pweibull(0, shape = shape_sims_i, scale = scale_sims_i)
  p365_i <- pweibull(365, shape = shape_sims_i, scale = scale_sims_i)
  p730_i <- pweibull(730, shape = shape_sims_i, scale = scale_sims_i)
  p1825_i <- pweibull(1825, shape = shape_sims_i, scale = scale_sims_i)
  
  simulated_mortality_list[[fit_index_i]] <- data.frame(
    CountryYear_original = country_year_i,
    sim_id = seq_len(n_sim),
    q_year0 = (p365_i - p0_i) / (1 - p0_i),
    q_year1 = (p730_i - p365_i) / (1 - p365_i),
    q_year2to4 = (p1825_i - p730_i) / (1 - p730_i)
  )
}

simulated_mortality_data <- dplyr::bind_rows(simulated_mortality_list)

# Use the already cleaned Country and Year variables rather than reparsing country names.
mortality_cy_lookup <- analysis_data %>%
  dplyr::select(CountryYear_original = CountryYear, Country, Year) %>%
  distinct()

stopifnot(nrow(mortality_cy_lookup) == n_distinct(mortality_cy_lookup$CountryYear_original))

simulated_mortality_data <- simulated_mortality_data %>%
  left_join(mortality_cy_lookup, by = "CountryYear_original") %>%
  mutate(
    Country = as.character(Country),
    Year = as.numeric(Year),
    CountryYear = paste(Country, Year, sep = "_")
  )

summary_table <- simulated_mortality_data %>%
  group_by(CountryYear) %>%
  summarise(across(starts_with("q_"), list(mean = mean, sd = sd)), .groups = "drop")

dim(simulated_mortality_data)


# Descriptive PC plots only -------------------------------------------------

simulated_mortality_data_plot <- simulated_mortality_data %>%
  inner_join(UNI_plot %>% dplyr::select(Country, Year, PC1, PC2), by = c("Country", "Year"))

pdf("figures/main_analysis/mortality/S3.distribution of mortality data.pdf", width = 6, height = 3.3)

years <- sort(unique(UNI_plot$Year))
num_years <- length(years)
my_palette <- colorRampPalette(c("lightgreen", "darkblue"))(num_years)
mapped_colors <- my_palette[match(UNI_plot$Year, years)]

par(mfrow = c(1,1))
plot(
  UNI_plot$PC1, UNI_plot$PC2,
  col = mapped_colors,
  xlim = c(min(UNI_plot$PC1), max(UNI_plot$PC1) * 1.1),
  ylim = c(min(UNI_plot$PC2), max(UNI_plot$PC2) * 1.1),
  xlab = "PC1", ylab = "PC2"
)

simulated_mortality_data_unique <- simulated_mortality_data_plot %>%
  dplyr::select(PC1, PC2) %>% distinct()

points(simulated_mortality_data_unique$PC1, simulated_mortality_data_unique$PC2, col = "red", pch = 19)
legend("topright", legend = years, col = my_palette, pch = 19, cex = 0.6, title = "Year")
legend("bottomright", legend = "DHS survey country-years", col = "red", pch = 19, lty = NA, lwd = NA, cex = 0.6)

dev.off()


# S4. Plot estimated risk of death against descriptive PC1 and PC2 from imputation 1

pdf("figures/main_analysis/mortality/S4.logit(probability of death) ~ PC1 and PC2.pdf", width = 6, height = 3.3)
par(mfrow = c(2,3), mar = c(4, 4, 3, 1))

age_interval_q <- c("q_year0", "q_year1", "q_year2to4")
clean_titles <- c("Age 0", "Age 1", "Age 2 to 4")

for (i in seq_along(age_interval_q)) {
  age_subset <- age_interval_q[i]
  x_var <- simulated_mortality_data_plot$PC1
  y_var <- qlogis(simulated_mortality_data_plot[[age_subset]])
  
  plot(x_var, y_var, xlab = "PC1 (Health & Nutrition)", ylab = "Logit(Probability of Death)",
       main = clean_titles[i], pch = 16, col = rgb(0.2, 0.3, 0.4, alpha = 0.02), cex = 0.5)
  
  for (j in seq_len(n_sim)) {
    sim_data_i <- simulated_mortality_data_plot %>% filter(sim_id == j)
    abline(lm(qlogis(sim_data_i[[age_subset]]) ~ sim_data_i$PC1), col = rgb(1, 0, 0, alpha = 0.01), lwd = 1)
  }
  
  abline(lm(y_var ~ x_var), col = "darkred", lwd = 2, lty = 2)
}

for (i in seq_along(age_interval_q)) {
  age_subset <- age_interval_q[i]
  x_var <- simulated_mortality_data_plot$PC2
  y_var <- qlogis(simulated_mortality_data_plot[[age_subset]])
  
  plot(x_var, y_var, xlab = "PC2 (Health & Nutrition)", ylab = "Logit(Probability of Death)",
       main = clean_titles[i], pch = 16, col = rgb(0.2, 0.3, 0.4, alpha = 0.02), cex = 0.5)
  
  for (j in seq_len(n_sim)) {
    sim_data_i <- simulated_mortality_data_plot %>% filter(sim_id == j)
    abline(lm(qlogis(sim_data_i[[age_subset]]) ~ sim_data_i$PC2), col = rgb(1, 0, 0, alpha = 0.01), lwd = 1)
  }
  
  abline(lm(y_var ~ x_var), col = "darkred", lwd = 2, lty = 2)
}

dev.off()


# _____________________________________________ ----------
# 3. Convert estimated death risks to central mortality rates -------------------

pc_imp_values <- sort(unique(UNI_model$.imp_pc))
n_pc_imp <- length(pc_imp_values)

shared_draw_design_file <- "outputs/pipelines/age_specific_selected_model_uncertainty/uncertainty_draw_design_5PC_5DHS_40draw.rds"

if(!file.exists(shared_draw_design_file)){
  stop("Shared incidence/antibiotic-access uncertainty draw design was not found.")
}

shared_draw_design <- readRDS(shared_draw_design_file)

stopifnot(
  all(c("draw_id", ".imp_pc") %in% names(shared_draw_design)),
  nrow(shared_draw_design) == n_sim,
  n_distinct(shared_draw_design$.imp_pc) == 5
)

mortality_draw_design <- shared_draw_design %>%
  dplyr::select(draw_id, .imp_pc) %>%
  arrange(draw_id) %>%
  group_by(.imp_pc) %>%
  mutate(.draw_within_pc = row_number()) %>%
  ungroup() %>%
  mutate(sim_id = draw_id)

n_draw_within_pc <- max(mortality_draw_design$.draw_within_pc)

stopifnot(
  n_pc_imp == 5,
  nrow(mortality_draw_design) == n_sim,
  n_draw_within_pc == 200,
  all((mortality_draw_design %>% count(.imp_pc) %>% pull(n)) == n_draw_within_pc)
)

dir.create("outputs/pipelines/mortality_rate_uncertainty", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  mortality_draw_design,
  "outputs/pipelines/mortality_rate_uncertainty/mortality_draw_design_5PC_200draw.rds"
)


# Estimate a0 separately within each PC imputation --------------------------

empirical_a0_base <- analysis_data %>%
  filter(death_event == 1, ch_dth_age <= 365.25) %>%
  group_by(Country, Year) %>%
  summarise(
    empirical_a0 = weighted.mean(ch_dth_age / 365.25, w = sample_weight, na.rm = TRUE),
    infant_deaths = n(),
    .groups = "drop"
  ) %>%
  mutate(Country = as.character(Country), Year = as.numeric(Year))

a0_model_list <- setNames(vector("list", n_pc_imp), as.character(pc_imp_values))
a0_model_data_list <- setNames(vector("list", n_pc_imp), as.character(pc_imp_values))
a0_coef_draw_list <- setNames(vector("list", n_pc_imp), as.character(pc_imp_values))
a0_coef_hat_list <- vector("list", n_pc_imp)

for (pc_index_i in seq_along(pc_imp_values)) {
  pc_imp_i <- pc_imp_values[pc_index_i]
  
  a0_data_i <- empirical_a0_base %>%
    inner_join(
      UNI_model %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(Country, Year, PC1, PC2),
      by = c("Country", "Year")
    ) %>%
    filter(is.finite(empirical_a0), empirical_a0 > 0, empirical_a0 < 1,
           is.finite(PC1), is.finite(PC2), infant_deaths > 0) %>%
    mutate(empirical_a0_logit = qlogis(pmin(pmax(empirical_a0, 1e-6), 1 - 1e-6)))
  
  a0_model_i <- lm(empirical_a0_logit ~ PC1 + PC2, weights = infant_deaths, data = a0_data_i)
  
  set.seed(300000 + pc_imp_i)
  a0_draw_i <- MASS::mvrnorm(
    n = n_draw_within_pc,
    mu = coef(a0_model_i),
    Sigma = vcov(a0_model_i)
  )
  colnames(a0_draw_i) <- names(coef(a0_model_i))
  
  a0_model_list[[as.character(pc_imp_i)]] <- a0_model_i
  a0_model_data_list[[as.character(pc_imp_i)]] <- a0_data_i
  a0_coef_draw_list[[as.character(pc_imp_i)]] <- a0_draw_i
  
  a0_coef_hat_list[[pc_index_i]] <- tibble(
    .imp_pc = pc_imp_i,
    a0_intercept = unname(coef(a0_model_i)["(Intercept)"]),
    a0_PC1 = unname(coef(a0_model_i)["PC1"]),
    a0_PC2 = unname(coef(a0_model_i)["PC2"])
  )
}

a0_coef_hat <- bind_rows(a0_coef_hat_list)

a0_coef_draw_data <- bind_rows(lapply(pc_imp_values, function(pc_imp_i) {
  draw_i <- as.data.frame(a0_coef_draw_list[[as.character(pc_imp_i)]])
  tibble(
    .imp_pc = pc_imp_i,
    .draw_within_pc = seq_len(nrow(draw_i)),
    a0_intercept = draw_i[["(Intercept)"]],
    a0_PC1 = draw_i[["PC1"]],
    a0_PC2 = draw_i[["PC2"]]
  )
}))


# Central mortality-rate estimates for model selection ---------------------

# Use the same central Weibull estimate in all five PC-imputation states.
mortality_q_central <- simulated_mortality_data %>%
  group_by(Country, Year) %>%
  summarise(
    q_year0 = median(q_year0, na.rm = TRUE),
    q_year1 = median(q_year1, na.rm = TRUE),
    q_year2to4 = median(q_year2to4, na.rm = TRUE),
    .groups = "drop"
  )

mortality_model_selection_wide <- mortality_q_central %>%
  inner_join(
    UNI_model %>% dplyr::select(.imp_pc, Country, Year, PC1, PC2),
    by = c("Country", "Year")
  ) %>%
  left_join(a0_coef_hat, by = ".imp_pc") %>%
  mutate(
    CountryYear = paste(Country, Year, sep = "_"),
    a0 = plogis(a0_intercept + a0_PC1 * PC1 + a0_PC2 * PC2),
    m_year0 = q_year0 / (1 - (1 - a0) * q_year0),
    m_year1 = q_year1 / (1 - 0.5 * q_year1),
    m_year2to4 = q_year2to4 / (3 - 1.5 * q_year2to4)
  ) %>%
  filter(
    is.finite(m_year0), m_year0 > 0,
    is.finite(m_year1), m_year1 > 0,
    is.finite(m_year2to4), m_year2to4 > 0
  )


# Final 1000 joint mortality-rate draws ------------------------------------

simulated_mortality_rate_data <- simulated_mortality_data %>%
  inner_join(
    mortality_draw_design %>%
      dplyr::select(sim_id, draw_id, .imp_pc, .draw_within_pc),
    by = "sim_id"
  ) %>%
  inner_join(
    UNI_model %>% dplyr::select(.imp_pc, Country, Year, PC1, PC2),
    by = c(".imp_pc", "Country", "Year")
  ) %>%
  left_join(a0_coef_draw_data, by = c(".imp_pc", ".draw_within_pc")) %>%
  mutate(
    a0_logit = a0_intercept + a0_PC1 * PC1 + a0_PC2 * PC2,
    a0 = plogis(a0_logit),
    m_year0 = q_year0 / (1 - (1 - a0) * q_year0),
    m_year1 = q_year1 / (1 - 0.5 * q_year1),
    m_year2to4 = q_year2to4 / (3 - 1.5 * q_year2to4)
  ) %>%
  filter(
    is.finite(m_year0), m_year0 > 0,
    is.finite(m_year1), m_year1 > 0,
    is.finite(m_year2to4), m_year2to4 > 0
  )

stopifnot(
  n_distinct(simulated_mortality_rate_data$draw_id) == 1000,
  n_distinct(simulated_mortality_rate_data$.imp_pc) == 5
)

summary_mortality_rate_country_year <- simulated_mortality_rate_data %>%
  group_by(CountryYear, Country, Year) %>%
  summarise(
    m_year0_median = median(m_year0, na.rm = TRUE),
    m_year0_lower = quantile(m_year0, probs = 0.025, na.rm = TRUE),
    m_year0_upper = quantile(m_year0, probs = 0.975, na.rm = TRUE),
    m_year1_median = median(m_year1, na.rm = TRUE),
    m_year1_lower = quantile(m_year1, probs = 0.025, na.rm = TRUE),
    m_year1_upper = quantile(m_year1, probs = 0.975, na.rm = TRUE),
    m_year2to4_median = median(m_year2to4, na.rm = TRUE),
    m_year2to4_lower = quantile(m_year2to4, probs = 0.025, na.rm = TRUE),
    m_year2to4_upper = quantile(m_year2to4, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )


# Descriptive a0 diagnostic plot using PC imputation 1 only -----------------------

empirical_a0_data_plot <- empirical_a0_base %>%
  inner_join(UNI_plot %>% dplyr::select(Country, Year, PC1, PC2), by = c("Country", "Year")) %>%
  filter(is.finite(empirical_a0), empirical_a0 > 0, empirical_a0 < 1,
         is.finite(PC1), is.finite(PC2), infant_deaths > 0) %>%
  mutate(empirical_a0_logit = qlogis(pmin(pmax(empirical_a0, 1e-6), 1 - 1e-6)))

a0_model_plot <- lm(empirical_a0_logit ~ PC1 + PC2, weights = infant_deaths, data = empirical_a0_data_plot)
coef_a0_plot <- coef(a0_model_plot)

set.seed(123)
simulated_a0_coefs_plot <- MASS::mvrnorm(n = n_sim, mu = coef_a0_plot, Sigma = vcov(a0_model_plot))
colnames(simulated_a0_coefs_plot) <- names(coef_a0_plot)

pc1_grid_a0 <- seq(min(empirical_a0_data_plot$PC1, na.rm = TRUE), max(empirical_a0_data_plot$PC1, na.rm = TRUE), length.out = 200)
pc2_grid_a0 <- seq(min(empirical_a0_data_plot$PC2, na.rm = TRUE), max(empirical_a0_data_plot$PC2, na.rm = TRUE), length.out = 200)
pc1_reference_a0 <- weighted.mean(empirical_a0_data_plot$PC1, w = empirical_a0_data_plot$infant_deaths, na.rm = TRUE)
pc2_reference_a0 <- weighted.mean(empirical_a0_data_plot$PC2, w = empirical_a0_data_plot$infant_deaths, na.rm = TRUE)

a0_pc1_curve_matrix <- sapply(seq_len(n_sim), function(j) {
  plogis(simulated_a0_coefs_plot[j, "(Intercept)"] +
           simulated_a0_coefs_plot[j, "PC1"] * pc1_grid_a0 +
           simulated_a0_coefs_plot[j, "PC2"] * pc2_reference_a0)
})

a0_pc2_curve_matrix <- sapply(seq_len(n_sim), function(j) {
  plogis(simulated_a0_coefs_plot[j, "(Intercept)"] +
           simulated_a0_coefs_plot[j, "PC1"] * pc1_reference_a0 +
           simulated_a0_coefs_plot[j, "PC2"] * pc2_grid_a0)
})

a0_pc1_lower <- apply(a0_pc1_curve_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
a0_pc1_upper <- apply(a0_pc1_curve_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
a0_pc2_lower <- apply(a0_pc2_curve_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
a0_pc2_upper <- apply(a0_pc2_curve_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)

a0_pc1_fit <- plogis(coef_a0_plot["(Intercept)"] + coef_a0_plot["PC1"] * pc1_grid_a0 + coef_a0_plot["PC2"] * pc2_reference_a0)
a0_pc2_fit <- plogis(coef_a0_plot["(Intercept)"] + coef_a0_plot["PC1"] * pc1_reference_a0 + coef_a0_plot["PC2"] * pc2_grid_a0)

cex_axis_a0 <- 0.70
cex_lab_a0 <- 0.75
cex_main_a0 <- 0.75
cex_point_a0 <- 0.60
lwd_axis_a0 <- 0.8
lwd_curve_a0 <- 1.5
point_col_a0 <- adjustcolor("grey25", alpha.f = 0.35)
curve_col_a0 <- "#0072B2"
band_col_a0 <- adjustcolor("#0072B2", alpha.f = 0.18)
ylim_a0 <- c(0, 1)
y_ticks_a0 <- seq(0, 1, by = 0.2)

pdf("figures/main_analysis/mortality/S5.Fraction of alive days in infant by PC.pdf", width = 6, height = 3.3, useDingbats = FALSE)
par(mfrow = c(1, 2), mgp = c(1.7, 0.45, 0), tcl = -0.20, xaxs = "i", yaxs = "i", family = "sans")

par(mar = c(3.1, 3.4, 1.7, 0.5))
plot(NA, xlim = range(c(empirical_a0_data_plot$PC1, pc1_grid_a0), finite = TRUE), ylim = ylim_a0, axes = FALSE, ann = FALSE)
polygon(c(pc1_grid_a0, rev(pc1_grid_a0)), c(a0_pc1_lower, rev(a0_pc1_upper)), col = band_col_a0, border = NA)
points(empirical_a0_data_plot$PC1, empirical_a0_data_plot$empirical_a0, pch = 16, cex = cex_point_a0, col = point_col_a0)
lines(pc1_grid_a0, a0_pc1_fit, col = curve_col_a0, lwd = lwd_curve_a0)
axis(1, cex.axis = cex_axis_a0, lwd = lwd_axis_a0, lwd.ticks = lwd_axis_a0)
axis(2, at = y_ticks_a0, labels = formatC(y_ticks_a0, format = "f", digits = 1), las = 1, cex.axis = cex_axis_a0, lwd = lwd_axis_a0, lwd.ticks = lwd_axis_a0)
box(bty = "l", lwd = lwd_axis_a0)
mtext("PC1 value", side = 1, line = 1.9, cex = cex_lab_a0)
mtext(expression("Fraction of interval lived by deceased infant (" *a[0]* ")"), side = 2, line = 2.35, cex = cex_lab_a0)
mtext(expression("(A) PC1 vs " * a[0]), side = 3, line = 0.45, adj = 0, cex = cex_main_a0, font = 2)

par(mar = c(3.1, 1.2, 1.7, 0.5))
plot(NA, xlim = range(c(empirical_a0_data_plot$PC2, pc2_grid_a0), finite = TRUE), ylim = ylim_a0, axes = FALSE, ann = FALSE)
polygon(c(pc2_grid_a0, rev(pc2_grid_a0)), c(a0_pc2_lower, rev(a0_pc2_upper)), col = band_col_a0, border = NA)
points(empirical_a0_data_plot$PC2, empirical_a0_data_plot$empirical_a0, pch = 16, cex = cex_point_a0, col = point_col_a0)
lines(pc2_grid_a0, a0_pc2_fit, col = curve_col_a0, lwd = lwd_curve_a0)
axis(1, cex.axis = cex_axis_a0, lwd = lwd_axis_a0, lwd.ticks = lwd_axis_a0)
axis(2, at = y_ticks_a0, labels = formatC(y_ticks_a0, format = "f", digits = 1), las = 1, cex.axis = cex_axis_a0, lwd = lwd_axis_a0, lwd.ticks = lwd_axis_a0)
box(bty = "l", lwd = lwd_axis_a0)
mtext("PC2 value", side = 1, line = 1.9, cex = cex_lab_a0)
mtext(expression("(B) PC2 vs " * a[0]), side = 3, line = 0.45, adj = 0, cex = cex_main_a0, font = 2)

dev.off()


mortality_rate_variable <- c("m_year0", "m_year1", "m_year2to4")
mortality_rate_title <- c("Age 0 (total infant)", "Age 1", "Age 2 to 4")

pdf("figures/main_analysis/mortality/S6.Estimated mortality rate by age interval.pdf", width = 6, height = 3.3)
par(mfrow = c(1,3))

for (i in seq_along(mortality_rate_variable)) {
  hist(simulated_mortality_rate_data[[mortality_rate_variable[i]]],
       main = mortality_rate_title[i], xlab = "Mortality rate per person-year")
}

dev.off()















#__________________________________________________-----------------------------
# 4. Age-specific mortality-rate model comparison ------------------------------

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

mortality_model_search_dir <- "outputs/pipelines/mortality_rate_full_model_search_outputs"
dir.create(mortality_model_search_dir, recursive = TRUE, showWarnings = FALSE)

# Five PC imputations x 200 common country holdouts = 1000 evaluations/model/spec.
# The same 200 country-holdout sets are reused across all five PC imputations.
n_splits <- as.integer(Sys.getenv("MORTALITY_RATE_MODEL_SEARCH_SPLITS", "200"))
holdout_prop <- as.numeric(Sys.getenv("MORTALITY_RATE_MODEL_SEARCH_HOLDOUT_PROP", "0.10"))

n_validation_expected <- length(pc_imp_values) * n_splits
stopifnot(length(pc_imp_values) == 5, n_validation_expected == 1000)

cat(
  "\nMortality validation design:\n",
  "PC imputations = ", length(pc_imp_values),
  "\nCommon country holdout sets = ", n_splits,
  "\nValidation evaluations per model and age group = ", n_validation_expected,
  "\n",
  sep = ""
)

age_interval <- c("m_year0", "m_year1", "m_year2to4")

age_group_labels <- c(
  m_year0 = "0-11 months",
  m_year1 = "12-23 months",
  m_year2to4 = "24-59 months"
)

mortality_spec_name_list <- c(
  "mortality_0_11_months",
  "mortality_12_23_months",
  "mortality_24_59_months"
)


# 4-1. Create one central estimate per country-year and PC imputation --------

mortality_country_year_weight <- analysis_data %>%
  mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
  group_by(Country, Year) %>%
  summarise(
    n_child = n(),
    n_death = sum(death_event == 1, na.rm = TRUE),
    .groups = "drop"
  )

mortality_model_selection_data <- mortality_model_selection_wide %>%
  mutate(
    Country = as.character(Country),
    Year = as.numeric(Year),
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2)
  ) %>%
  left_join(mortality_country_year_weight, by = c("Country", "Year")) %>%
  pivot_longer(
    cols = all_of(age_interval),
    names_to = "age_interval",
    values_to = "mortality_rate"
  ) %>%
  mutate(
    age_group = unname(age_group_labels[age_interval]),
    mortality_rate = pmax(mortality_rate, 1e-12),
    y_log_rate = log(mortality_rate)
  ) %>%
  filter(
    is.finite(PC1), is.finite(PC2), is.finite(y_log_rate),
    !is.na(Country), !is.na(.imp_pc)
  )

mortality_weight_cap <- quantile(
  mortality_model_selection_data$n_child[
    is.finite(mortality_model_selection_data$n_child) &
      mortality_model_selection_data$n_child > 0
  ],
  probs = 0.95,
  na.rm = TRUE
)

if (!is.finite(mortality_weight_cap)) mortality_weight_cap <- 1

mortality_model_selection_data <- mortality_model_selection_data %>%
  mutate(
    weight_model = if_else(
      is.finite(n_child) & n_child > 0,
      pmin(n_child, mortality_weight_cap),
      1
    )
  ) %>%
  filter(is.finite(weight_model), weight_model > 0)

stopifnot(n_distinct(mortality_model_selection_data$.imp_pc) == 5)


# 4-2. Candidate models ------------------------------------------------------

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

if (requireNamespace("kernlab", quietly = TRUE)) {
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

mortality_candidate_predictions <- function(train_split_i, test_split_i, seed_i) {
  prediction_list_i <- setNames(vector("list", length(candidate_model_keys)), candidate_model_keys)
  
  fit_i <- tryCatch(
    lm(y_log_rate ~ PC1 + PC2, data = train_split_i, weights = weight_model),
    error = function(e) NULL
  )
  prediction_list_i$lm <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = test_split_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  fit_i <- tryCatch(
    lm(y_log_rate ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3),
       data = train_split_i, weights = weight_model),
    error = function(e) NULL
  )
  prediction_list_i$spline_lm <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = test_split_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  fit_i <- tryCatch(
    mgcv::gam(y_log_rate ~ s(PC1, k = 4) + s(PC2, k = 4),
              data = train_split_i, weights = weight_model, method = "REML"),
    error = function(e) NULL
  )
  prediction_list_i$gam <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = test_split_i, type = "response")), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  set.seed(seed_i)
  fit_i <- tryCatch(
    ranger::ranger(y_log_rate ~ PC1 + PC2, data = train_split_i,
                   case.weights = train_split_i$weight_model,
                   num.trees = 1000, mtry = 1, min.node.size = 5, seed = seed_i),
    error = function(e) NULL
  )
  prediction_list_i$rf_balanced <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, data = test_split_i)$predictions), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  set.seed(seed_i)
  fit_i <- tryCatch(
    ranger::ranger(y_log_rate ~ PC1 + PC2, data = train_split_i,
                   case.weights = train_split_i$weight_model,
                   num.trees = 1000, mtry = 2, min.node.size = 2, seed = seed_i),
    error = function(e) NULL
  )
  prediction_list_i$rf_small_nodes <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, data = test_split_i)$predictions), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  set.seed(seed_i)
  fit_i <- tryCatch(
    ranger::ranger(y_log_rate ~ PC1 + PC2, data = train_split_i,
                   case.weights = train_split_i$weight_model,
                   num.trees = 1500, mtry = 2, min.node.size = 3,
                   splitrule = "extratrees", seed = seed_i),
    error = function(e) NULL
  )
  prediction_list_i$extra_trees <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, data = test_split_i)$predictions), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  combined_matrix_i <- bind_rows(
    train_split_i %>% transmute(PC1, PC2, .set = "train"),
    test_split_i %>% transmute(PC1, PC2, .set = "test")
  )
  
  x_all_i <- model.matrix(~ PC1 + PC2, data = combined_matrix_i)[, -1, drop = FALSE]
  x_train_i <- x_all_i[combined_matrix_i$.set == "train", , drop = FALSE]
  x_test_i <- x_all_i[combined_matrix_i$.set == "test", , drop = FALSE]
  
  nfolds_i <- min(5, nrow(train_split_i))
  foldid_i <- ((seq_len(nrow(train_split_i)) - 1) %% nfolds_i) + 1
  
  glmnet_alpha_map_i <- c(ridge = 0, enet_025 = 0.25, enet_050 = 0.50, enet_075 = 0.75, lasso = 1)
  
  for (model_key_i in names(glmnet_alpha_map_i)) {
    fit_i <- tryCatch(
      glmnet::cv.glmnet(
        x = x_train_i,
        y = train_split_i$y_log_rate,
        weights = train_split_i$weight_model,
        alpha = glmnet_alpha_map_i[model_key_i],
        family = "gaussian",
        nfolds = nfolds_i,
        foldid = foldid_i
      ),
      error = function(e) NULL
    )
    
    prediction_list_i[[model_key_i]] <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
      tryCatch(as.numeric(predict(fit_i, newx = x_test_i, s = "lambda.min")), error = function(e) rep(NA_real_, nrow(test_split_i)))
  }
  
  dtrain_i <- xgboost::xgb.DMatrix(data = x_train_i, label = train_split_i$y_log_rate, weight = train_split_i$weight_model)
  
  fit_i <- tryCatch(
    xgboost::xgb.train(
      params = list(objective = "reg:squarederror", eval_metric = "rmse",
                    max_depth = 2, eta = 0.02, subsample = 0.85,
                    colsample_bytree = 0.85, min_child_weight = 5, seed = seed_i),
      data = dtrain_i, nrounds = 500, verbose = 0
    ),
    error = function(e) NULL
  )
  prediction_list_i$xgb_smooth <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = x_test_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  fit_i <- tryCatch(
    xgboost::xgb.train(
      params = list(objective = "reg:squarederror", eval_metric = "rmse",
                    max_depth = 3, eta = 0.03, subsample = 0.80,
                    colsample_bytree = 0.80, min_child_weight = 3, seed = seed_i),
      data = dtrain_i, nrounds = 300, verbose = 0
    ),
    error = function(e) NULL
  )
  prediction_list_i$xgb_balanced <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = x_test_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  fit_i <- tryCatch(
    xgboost::xgb.train(
      params = list(objective = "reg:squarederror", eval_metric = "rmse",
                    max_depth = 4, eta = 0.02, subsample = 0.75,
                    colsample_bytree = 0.75, min_child_weight = 2, seed = seed_i),
      data = dtrain_i, nrounds = 450, verbose = 0
    ),
    error = function(e) NULL
  )
  prediction_list_i$xgb_deeper <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
    tryCatch(as.numeric(predict(fit_i, newdata = x_test_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  
  if ("svm_radial" %in% candidate_model_keys) {
    fit_i <- tryCatch(
      kernlab::ksvm(
        x = x_train_i,
        y = train_split_i$y_log_rate,
        type = "eps-svr",
        kernel = "rbfdot",
        kpar = "automatic",
        C = 2,
        scaled = TRUE
      ),
      error = function(e) NULL
    )
    
    prediction_list_i$svm_radial <- if (is.null(fit_i)) rep(NA_real_, nrow(test_split_i)) else
      tryCatch(as.numeric(kernlab::predict(fit_i, x_test_i)), error = function(e) rep(NA_real_, nrow(test_split_i)))
  }
  
  prediction_list_i
}


# 4-3. Repeated 10% country-level validation across five PC imputations ------

mortality_validation_result_list <- vector("list", length(age_interval))
mortality_validation_prediction_list <- vector("list", length(age_interval))
mortality_holdout_country_list <- setNames(vector("list", length(age_interval)), mortality_spec_name_list)

for (spec_index_i in seq_along(age_interval)) {
  age_interval_i <- age_interval[spec_index_i]
  age_group_i <- unname(age_group_labels[age_interval_i])
  spec_name_i <- mortality_spec_name_list[spec_index_i]
  
  spec_data_i <- mortality_model_selection_data %>% filter(age_interval == age_interval_i)
  
  # Use only countries represented in all five PC-imputation states.
  common_countries_i <- spec_data_i %>%
    distinct(.imp_pc, Country) %>%
    count(Country, name = "n_present") %>%
    filter(n_present == length(pc_imp_values)) %>%
    pull(Country) %>%
    sort()
  
  if (length(common_countries_i) < 3) stop("Insufficient common countries for ", spec_name_i)
  
  holdout_country_list_i <- vector("list", n_splits)
  
  for (split_id_i in seq_len(n_splits)) {
    set.seed(spec_index_i * 100000 + split_id_i)
    n_holdout_i <- max(1, ceiling(length(common_countries_i) * holdout_prop))
    holdout_country_list_i[[split_id_i]] <- sample(common_countries_i, n_holdout_i, replace = FALSE)
  }
  
  mortality_holdout_country_list[[spec_name_i]] <- holdout_country_list_i
  
  metric_state_list_i <- list()
  prediction_state_list_i <- list()
  state_index_i <- 1
  
  for (pc_imp_i in pc_imp_values) {
    train_dat_state_i <- spec_data_i %>%
      filter(.imp_pc == pc_imp_i, Country %in% common_countries_i)
    
    for (split_id_i in seq_len(n_splits)) {
      holdout_countries_i <- holdout_country_list_i[[split_id_i]]
      
      train_split_i <- train_dat_state_i %>% filter(!Country %in% holdout_countries_i)
      test_split_i <- train_dat_state_i %>% filter(Country %in% holdout_countries_i)
      
      prediction_list_i <- mortality_candidate_predictions(
        train_split_i = train_split_i,
        test_split_i = test_split_i,
        seed_i = spec_index_i * 100000 + split_id_i
      )
      
      metric_list_i <- vector("list", length(candidate_model_keys))
      prediction_long_list_i <- vector("list", length(candidate_model_keys))
      
      for (model_index_i in seq_along(candidate_model_keys)) {
        model_key_i <- candidate_model_keys[model_index_i]
        pred_log_rate_i <- prediction_list_i[[model_key_i]]
        valid_i <- is.finite(test_split_i$y_log_rate) & is.finite(pred_log_rate_i)
        
        if (!any(valid_i)) {
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels[model_key_i]),
            rmse_log_rate = NA_real_,
            mae_log_rate = NA_real_,
            bias_log_rate = NA_real_,
            abs_bias_log_rate = NA_real_,
            rmse_rate = NA_real_,
            mae_rate = NA_real_,
            cor_rate = NA_real_,
            n_eval = 0
          )
        } else {
          observed_log_rate_i <- test_split_i$y_log_rate[valid_i]
          predicted_log_rate_i <- pred_log_rate_i[valid_i]
          observed_rate_i <- exp(observed_log_rate_i)
          predicted_rate_i <- exp(predicted_log_rate_i)
          
          cor_rate_i <- if (length(observed_rate_i) >= 2 &&
                            sd(observed_rate_i) > 0 &&
                            sd(predicted_rate_i) > 0) {
            cor(observed_rate_i, predicted_rate_i)
          } else {
            NA_real_
          }
          
          metric_list_i[[model_index_i]] <- tibble(
            model_key = model_key_i,
            model = unname(model_labels[model_key_i]),
            rmse_log_rate = sqrt(mean((predicted_log_rate_i - observed_log_rate_i)^2)),
            mae_log_rate = mean(abs(predicted_log_rate_i - observed_log_rate_i)),
            bias_log_rate = mean(predicted_log_rate_i - observed_log_rate_i),
            abs_bias_log_rate = abs(mean(predicted_log_rate_i - observed_log_rate_i)),
            rmse_rate = sqrt(mean((predicted_rate_i - observed_rate_i)^2)),
            mae_rate = mean(abs(predicted_rate_i - observed_rate_i)),
            cor_rate = cor_rate_i,
            n_eval = sum(valid_i)
          )
        }
        
        prediction_long_list_i[[model_index_i]] <- test_split_i %>%
          transmute(
            .imp_pc,
            age_interval,
            age_group,
            spec_name = spec_name_i,
            split_id = split_id_i,
            CountryYear,
            Country,
            Year,
            PC1,
            PC2,
            model_key = model_key_i,
            model = unname(model_labels[model_key_i]),
            observed_log_rate = y_log_rate,
            predicted_log_rate = pred_log_rate_i,
            observed_rate = mortality_rate,
            predicted_rate = exp(pred_log_rate_i)
          )
      }
      
      metric_state_list_i[[state_index_i]] <- bind_rows(metric_list_i) %>%
        mutate(
          .imp_pc = pc_imp_i,
          age_interval = age_interval_i,
          age_group = age_group_i,
          spec_name = spec_name_i,
          split_id = split_id_i,
          n_holdout_country = length(holdout_countries_i),
          holdout_countries = paste(holdout_countries_i, collapse = "; ")
        )
      
      prediction_state_list_i[[state_index_i]] <- bind_rows(prediction_long_list_i)
      state_index_i <- state_index_i + 1
      
      if (split_id_i %% 10 == 0) {
        message(spec_name_i, ", PC imputation ", pc_imp_i, ": completed split ", split_id_i, " / ", n_splits)
      }
    }
  }
  
  mortality_validation_result_list[[spec_index_i]] <- bind_rows(metric_state_list_i)
  mortality_validation_prediction_list[[spec_index_i]] <- bind_rows(prediction_state_list_i)
  
  saveRDS(
    mortality_validation_result_list[[spec_index_i]],
    file.path(mortality_model_search_dir, paste0(spec_name_i, "_validation_results.rds"))
  )
  
  saveRDS(
    mortality_validation_prediction_list[[spec_index_i]],
    file.path(mortality_model_search_dir, paste0(spec_name_i, "_validation_predictions.rds"))
  )
}


# 4-4. Combine and summarize model-validation results -----------------------

mortality_validation_results <- bind_rows(mortality_validation_result_list)
mortality_validation_predictions <- bind_rows(mortality_validation_prediction_list)

mortality_validation_summary <- mortality_validation_results %>%
  group_by(age_interval, age_group, spec_name, model_key, model) %>%
  summarise(
    mean_rmse_log_rate = mean(rmse_log_rate, na.rm = TRUE),
    median_rmse_log_rate = median(rmse_log_rate, na.rm = TRUE),
    mean_mae_log_rate = mean(mae_log_rate, na.rm = TRUE),
    mean_bias_log_rate = mean(bias_log_rate, na.rm = TRUE),
    mean_abs_bias_log_rate = mean(abs_bias_log_rate, na.rm = TRUE),
    mean_rmse_rate = mean(rmse_rate, na.rm = TRUE),
    mean_mae_rate = mean(mae_rate, na.rm = TRUE),
    mean_cor_rate = mean(cor_rate, na.rm = TRUE),
    n_valid_evaluations = sum(is.finite(rmse_log_rate)),
    .groups = "drop"
  ) %>%
  filter(n_valid_evaluations > 0) %>%
  group_by(age_interval, age_group, spec_name) %>%
  mutate(
    rmse_rank = rank(mean_rmse_log_rate, ties.method = "min"),
    bias_rank = rank(mean_abs_bias_log_rate, ties.method = "min"),
    combined_rank = rmse_rank + bias_rank
  ) %>%
  ungroup() %>%
  arrange(age_interval, combined_rank, mean_rmse_log_rate, mean_abs_bias_log_rate)


# 4-5. Select age-specific winners and one common mortality model -----------

mortality_model_selection <- mortality_validation_summary %>%
  group_by(age_interval, age_group, spec_name) %>%
  arrange(combined_rank, mean_rmse_log_rate, mean_abs_bias_log_rate, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(selection_rule = "Best combined RMSE and absolute-bias rank across 5 PC imputations x 200 common country holdouts")

mortality_common_model_selection <- mortality_validation_summary %>%
  group_by(model_key, model) %>%
  summarise(
    n_age = n_distinct(age_interval),
    rmse_rank_sum = sum(rmse_rank),
    bias_rank_sum = sum(bias_rank),
    combined_rank_sum = sum(combined_rank),
    mean_rmse_log_rate = mean(mean_rmse_log_rate),
    mean_abs_bias_log_rate = mean(mean_abs_bias_log_rate),
    .groups = "drop"
  ) %>%
  filter(n_age == length(age_interval)) %>%
  arrange(combined_rank_sum, rmse_rank_sum, bias_rank_sum, mean_rmse_log_rate, mean_abs_bias_log_rate) %>%
  mutate(selection_rule = "Lowest summed age-specific RMSE plus absolute-bias rank")

if (nrow(mortality_common_model_selection) == 0) stop("No common mortality model had valid results across all three age groups.")

selected_mortality_model_key <- mortality_common_model_selection$model_key[1]
selected_mortality_model_label <- mortality_common_model_selection$model[1]

saveRDS(mortality_validation_results,
        file.path(mortality_model_search_dir, "mortality_age_specific_validation_results.rds"))
saveRDS(mortality_validation_predictions,
        file.path(mortality_model_search_dir, "mortality_age_specific_validation_predictions.rds"))
saveRDS(mortality_validation_summary,
        file.path(mortality_model_search_dir, "mortality_age_specific_validation_summary.rds"))
saveRDS(mortality_model_selection,
        file.path(mortality_model_search_dir, "mortality_age_specific_selected_models.rds"))
saveRDS(mortality_common_model_selection,
        file.path(mortality_model_search_dir, "mortality_common_model_selection.rds"))
saveRDS(mortality_holdout_country_list,
        file.path(mortality_model_search_dir, "mortality_common_200_country_holdouts.rds"))

write.csv(mortality_validation_summary,
          file.path(mortality_model_search_dir, "mortality_age_specific_validation_summary.csv"),
          row.names = FALSE)
write.csv(mortality_model_selection,
          file.path(mortality_model_search_dir, "mortality_age_specific_selected_models.csv"),
          row.names = FALSE)
write.csv(mortality_common_model_selection,
          file.path(mortality_model_search_dir, "mortality_common_model_selection.csv"),
          row.names = FALSE)

print(mortality_model_selection, n = Inf)
print(mortality_common_model_selection, n = Inf)








# 4-6. Mortality validation diagnostic plots -----------------------------------

mortality_model_search_dir <- "outputs/pipelines/mortality_rate_full_model_search_outputs"

if (!exists("mortality_validation_predictions")) {
  mortality_validation_predictions <- readRDS(
    file.path(
      mortality_model_search_dir,
      "mortality_age_specific_validation_predictions.rds"
    )
  )
}

fig_dir_validation_i <- "figures/main_analysis/mortality/mortality_rate_model_validation_predictions"
dir.create(fig_dir_validation_i, recursive = TRUE, showWarnings = FALSE)


## Candidate-model order ---------------------------------------------------------

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
  "xgb_deeper",
  "svm_radial"
)


## GBD-region colors -------------------------------------------------------------

region_cols_fixed <- c(
  "Central Europe, Eastern Europe, and Central Asia" = "#CC79A7",
  "Southeast Asia, East Asia, and Oceania" = "#D55E00",
  "Latin America and Caribbean" = "#E69F00",
  "North Africa and Middle East" = "#009E73",
  "South Asia" = "#A7B97F",
  "Sub-Saharan Africa" = "#0072B2"
)


## Add GBD region if it was not retained in validation predictions ---------------

mortality_region_lookup_i <- UNI_base %>%
  transmute(
    Country = as.character(Country),
    Year = as.numeric(Year),
    gbd_region = dplyr::recode(
      as.character(gbd_region),
      "East Asia, Southeast Asia, and Oceania" =
        "Southeast Asia, East Asia, and Oceania",
      .default = as.character(gbd_region)
    )
  ) %>%
  distinct(Country, Year, .keep_all = TRUE)

if (!"gbd_region" %in% names(mortality_validation_predictions)) {
  mortality_validation_predictions <- mortality_validation_predictions %>%
    mutate(
      Country = as.character(Country),
      Year = as.numeric(Year)
    ) %>%
    left_join(
      mortality_region_lookup_i,
      by = c("Country", "Year")
    )
} else {
  mortality_validation_predictions <- mortality_validation_predictions %>%
    mutate(
      gbd_region = dplyr::recode(
        as.character(gbd_region),
        "East Asia, Southeast Asia, and Oceania" =
          "Southeast Asia, East Asia, and Oceania",
        .default = as.character(gbd_region)
      )
    )
}


## Create explicit age-specific validation objects -------------------------------

validation_prediction_list_i <- list(
  mortality_0_11_months = mortality_validation_predictions %>%
    filter(spec_name == "mortality_0_11_months"),
  
  mortality_12_23_months = mortality_validation_predictions %>%
    filter(spec_name == "mortality_12_23_months"),
  
  mortality_24_59_months = mortality_validation_predictions %>%
    filter(spec_name == "mortality_24_59_months")
)

validation_plot_object_list_i <- vector(
  "list",
  length(validation_prediction_list_i)
)

names(validation_plot_object_list_i) <- names(validation_prediction_list_i)


## Create one PDF for each age group --------------------------------------------

validation_plot_object_list_i <- vector("list", length(validation_prediction_list_i))
names(validation_plot_object_list_i) <- names(validation_prediction_list_i)

for (spec_name_i in names(validation_prediction_list_i)) {
  message("Creating mortality validation plots: ", spec_name_i)
  
  validation_prediction_i <- validation_prediction_list_i[[spec_name_i]]
  
  validation_plot_i <- validation_prediction_i %>%
    filter(
      is.finite(observed_log_rate),
      is.finite(predicted_log_rate),
      is.finite(observed_rate),
      is.finite(predicted_rate)
    ) %>%
    group_by(
      age_interval,
      age_group,
      spec_name,
      CountryYear,
      Country,
      Year,
      gbd_region,
      model_key,
      model,
      observed_log_rate,
      observed_rate
    ) %>%
    summarise(
      predicted_log_rate = mean(predicted_log_rate, na.rm = TRUE),
      predicted_rate = mean(predicted_rate, na.rm = TRUE),
      n_validation_predictions = n_distinct(split_id),
      .groups = "drop"
    ) %>%
    mutate(
      residual_log_rate = predicted_log_rate - observed_log_rate,
      residual_rate = predicted_rate - observed_rate
    )
  
  validation_plot_object_list_i[[spec_name_i]] <- validation_plot_i
  
  if (nrow(validation_plot_i) == 0) {
    warning("No finite validation predictions were available for ", spec_name_i)
    next
  }
  
  age_group_i <- unique(validation_plot_i$age_group)[1]
  
  model_keys_i <- candidate_model_keys[
    candidate_model_keys %in% unique(validation_plot_i$model_key)
  ]
  
  region_levels_i <- names(region_cols_fixed)[
    names(region_cols_fixed) %in% unique(validation_plot_i$gbd_region)
  ]
  
  region_colors_i <- region_cols_fixed[region_levels_i]
  
  
  ## Age-specific rate axes ---------------------------------------------
  
  rate_min_i <- 0
  rate_axis_digits_i <- 2
  
  if (spec_name_i == "mortality_0_11_months") {
    rate_axis_at_i <- c(0, 0.02, 0.04, 0.06, 0.08)
  } else if (spec_name_i == "mortality_12_23_months") {
    rate_axis_at_i <- c(0, 0.01, 0.02, 0.03, 0.04)
  } else if (spec_name_i == "mortality_24_59_months") {
    rate_axis_at_i <- c(0, 0.02, 0.04, 0.06)
  } else {
    stop("Unknown mortality specification: ", spec_name_i)
  }
  
  rate_max_i <- max(rate_axis_at_i)
  
  rate_axis_labels_i <- formatC(
    rate_axis_at_i,
    format = "f",
    digits = rate_axis_digits_i
  )
  
  
  ## Open age-specific PDF ------------------------------------------------------
  
  pdf_file_i <- file.path(
    fig_dir_validation_i,
    paste0(spec_name_i, "_all_models_validation_observed_predicted.pdf")
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
  
  
  ## Observed versus predicted mortality rate ---------------------------
  
  for (model_index_i in seq_along(model_keys_i)) {
    model_key_i <- model_keys_i[model_index_i]
    
    panel_column_i <- ((model_index_i - 1) %% 4) + 1
    panel_row_i <- ceiling(model_index_i / 4)
    
    ylab_i <- if (panel_column_i == 1) {
      "Held-out predicted rate"
    } else {
      ""
    }
    
    xlab_i <- if (panel_row_i == 4) {
      "Held-out observed rate"
    } else {
      ""
    }
    
    plot_data_i <- validation_plot_i %>%
      filter(model_key == model_key_i)
    
    model_name_i <- unique(plot_data_i$model)[1]
    
    valid_i <- is.finite(plot_data_i$observed_rate) &
      is.finite(plot_data_i$predicted_rate)
    
    if (sum(valid_i) < 2) {
      plot.new()
      
      title(
        main = paste0(age_group_i, ",\n", model_name_i),
        cex.main = 0.75
      )
      
      text(
        0.5,
        0.5,
        labels = "No finite validation predictions",
        cex = 0.70
      )
      
      next
    }
    
    plot_data_i <- plot_data_i[valid_i, , drop = FALSE]
    
    point_colors_i <- unname(
      region_colors_i[as.character(plot_data_i$gbd_region)]
    )
    
    point_colors_i[is.na(point_colors_i)] <- "grey60"
    
    plot(
      plot_data_i$observed_rate,
      plot_data_i$predicted_rate,
      col = point_colors_i,
      pch = 16,
      cex = 0.65,
      xlim = c(rate_min_i, rate_max_i),
      ylim = c(rate_min_i, rate_max_i),
      asp = 1,
      axes = FALSE,
      ann = FALSE,
      xaxs = "i",
      yaxs = "i"
    )
    
    axis(
      side = 1,
      at = rate_axis_at_i,
      labels = FALSE,
      pos = rate_min_i,
      tck = -0.03,
      lwd = 0.8,
      lwd.ticks = 0.8
    )
    
    axis(
      side = 2,
      at = rate_axis_at_i,
      labels = FALSE,
      pos = rate_min_i,
      tck = -0.03,
      lwd = 0.8,
      lwd.ticks = 0.8
    )
    
    text(
      x = rate_axis_at_i,
      y = rate_min_i -
        0.10 * (rate_max_i - rate_min_i),
      labels = rate_axis_labels_i,
      xpd = TRUE,
      cex = 0.65
    )
    
    text(
      x = rate_min_i -
        0.055 * (rate_max_i - rate_min_i),
      y = rate_axis_at_i,
      labels = rate_axis_labels_i,
      adj = 1,
      xpd = TRUE,
      cex = 0.65
    )
    
    if (xlab_i != "") {
      mtext(
        xlab_i,
        side = 1,
        line = 2.1,
        cex = 0.50
      )
    }
    
    if (ylab_i != "") {
      mtext(
        ylab_i,
        side = 2,
        line = 2.1,
        cex = 0.50
      )
    }
    
    title(
      main = paste0(age_group_i, ",\n", model_name_i),
      cex.main = 0.75,
      line = 1
    )
    
    abline(
      a = 0,
      b = 1,
      lty = 2,
      lwd = 1,
      col = "red"
    )
  }
  
  
  ## Leave panel 16 for the GBD-region legend ----------------------------------
  
  n_blank_panels_i <- 15 - length(model_keys_i)
  
  if (n_blank_panels_i > 0) {
    for (blank_i in seq_len(n_blank_panels_i)) {
      plot.new()
    }
  }
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  
  if (length(region_levels_i) > 0) {
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
  } else {
    text(
      0.5,
      0.5,
      labels = "GBD-region information unavailable",
      cex = 0.60
    )
  }
  
  dev.off()
  
  message("PDF written: ", pdf_file_i)
}


## Save the aggregated validation plotting data --------------------------------

saveRDS(
  validation_plot_object_list_i,
  file.path(
    mortality_model_search_dir,
    "mortality_age_specific_validation_plot_objects.rds"
  )
)








# _____________________________________________ ----------
# 5. Selected-model mortality extrapolation with 5-PC uncertainty -------------

selected_mortality_model_key <- "lasso"

selected_mortality_model_label <- mortality_common_model_selection %>%
  filter(model_key == selected_mortality_model_key) %>%
  pull(model) %>%
  .[1]

cat("\nSelected common mortality model:", selected_mortality_model_key,
    "-", selected_mortality_model_label, "\n")

mortality_weight_lookup <- mortality_model_selection_data %>%
  dplyr::select(Country, Year, weight_model) %>%
  distinct()

stopifnot(
  nrow(mortality_weight_lookup) ==
    dplyr::n_distinct(paste(mortality_weight_lookup$Country, mortality_weight_lookup$Year))
)

simulated_mortality_rate_model_data <- simulated_mortality_rate_data %>%
  mutate(Country = as.character(Country), Year = as.numeric(Year)) %>%
  left_join(mortality_weight_lookup, by = c("Country", "Year")) %>%
  mutate(weight_model = if_else(is.finite(weight_model) & weight_model > 0, weight_model, 1))

draw_id_values <- sort(unique(simulated_mortality_rate_model_data$draw_id))
n_sim <- length(draw_id_values)

stopifnot(
  n_sim == 1000,
  identical(draw_id_values, seq_len(1000)),
  n_distinct(simulated_mortality_rate_model_data$.imp_pc) == 5
)

# One country bootstrap per final draw; exactly the same bootstrap is used for
# all three age groups within that draw.
mortality_bootstrap_country_levels <- sort(unique(simulated_mortality_rate_model_data$Country))
n_mortality_bootstrap_countries <- length(mortality_bootstrap_country_levels)

if (n_mortality_bootstrap_countries < 3L) {
  stop("At least three countries are required for mortality extrapolation.")
}

mortality_country_bootstrap_list <- vector("list", n_sim)
names(mortality_country_bootstrap_list) <- as.character(draw_id_values)

for (draw_index_i in seq_along(draw_id_values)) {
  draw_id_i <- draw_id_values[draw_index_i]
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


# Target-country prediction grids, one for each PC imputation ----------------

UNI_base <- UNI_model %>%
  dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
  distinct()

UNI_base <- UNI_base %>% mutate(.row_pred = row_number())
UNI_base_key <- paste(UNI_base$Country, UNI_base$Year, sep = "___")

UNI_pred_pc_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))

for (pc_imp_i in pc_imp_values) {
  UNI_pred_i <- UNI_model %>%
    filter(.imp_pc == pc_imp_i) %>%
    mutate(.row_pred = match(paste(Country, Year, sep = "___"), UNI_base_key)) %>%
    arrange(.row_pred)
  
  if (nrow(UNI_pred_i) != nrow(UNI_base) || anyNA(UNI_pred_i$.row_pred)) {
    stop("Target prediction grid is not aligned for PC imputation ", pc_imp_i, ".")
  }
  
  UNI_pred_pc_list[[as.character(pc_imp_i)]] <- UNI_pred_i
}

if (any(vapply(UNI_pred_pc_list, function(x) any(!is.finite(x$PC1) | !is.finite(x$PC2)), logical(1)))) {
  stop("UNI contains missing or non-finite PC1 or PC2 values.")
}


# Fit the selected model within each uncertainty draw -----------------------

mortality_rate_matrix_names <- c(
  m_year0 = "matrix_death_rate_q_year0",
  m_year1 = "matrix_death_rate_q_year1",
  m_year2to4 = "matrix_death_rate_q_year2to4"
)

mortality_rate_selected_model_diagnostic_list <- vector("list", length(age_interval) * n_sim)
diagnostic_index_i <- 1

for (age_index_i in seq_along(age_interval)) {
  age_interval_i <- age_interval[age_index_i]
  
  mortality_rate_matrix_i <- matrix(
    NA_real_,
    nrow = nrow(UNI_base),
    ncol = n_sim,
    dimnames = list(
      paste(UNI_base$Country, UNI_base$Year, sep = "_"),
      paste0("draw_", draw_id_values)
    )
  )
  
  for (draw_index_i in seq_along(draw_id_values)) {
    draw_id_i <- draw_id_values[draw_index_i]
    
    sim_data_i <- simulated_mortality_rate_model_data %>%
      filter(draw_id == draw_id_i) %>%
      mutate(y_log_rate = log(pmax(.data[[age_interval_i]], 1e-12))) %>%
      filter(
        is.finite(y_log_rate), is.finite(PC1), is.finite(PC2),
        is.finite(weight_model), weight_model > 0, !is.na(Country)
      )
    
    if (nrow(sim_data_i) == 0) {
      stop("No valid training observations for ", age_interval_i, ", draw ", draw_id_i, ".")
    }
    
    pc_imp_i <- unique(sim_data_i$.imp_pc)
    if (length(pc_imp_i) != 1) stop("Each mortality draw must correspond to exactly one PC imputation.")
    
    pred_data_i <- UNI_pred_pc_list[[as.character(pc_imp_i)]]
    
    # Country bootstrap: bootstrap multiplicity enters the fitting weight.
    boot_country_count_i <- mortality_country_bootstrap_list[[as.character(draw_id_i)]]
    
    sim_data_boot_i <- sim_data_i %>%
      inner_join(boot_country_count_i, by = "Country") %>%
      mutate(weight_model_boot = weight_model * bootstrap_count) %>%
      filter(is.finite(weight_model_boot), weight_model_boot > 0)
    
    if (nrow(sim_data_boot_i) < 5 || dplyr::n_distinct(sim_data_boot_i$Country) < 3) {
      stop("Insufficient bootstrap training data for ", age_interval_i, ", draw ", draw_id_i, ".")
    }
    
    seed_model_i <- 1500000 + draw_id_i
    lambda_min_i <- NA_real_
    lambda_1se_i <- NA_real_
    intercept_i <- NA_real_
    PC1_coef_i <- NA_real_
    PC2_coef_i <- NA_real_
    
    if (selected_mortality_model_key == "lm") {
      fit_i <- lm(y_log_rate ~ PC1 + PC2, data = sim_data_boot_i, weights = weight_model_boot)
      predicted_log_mortality_rate_i <- as.numeric(predict(fit_i, newdata = pred_data_i))
      coef_i <- coef(fit_i)
      intercept_i <- unname(coef_i["(Intercept)"])
      PC1_coef_i <- unname(coef_i["PC1"])
      PC2_coef_i <- unname(coef_i["PC2"])
      
    } else if (selected_mortality_model_key == "spline_lm") {
      fit_i <- lm(
        y_log_rate ~ splines::ns(PC1, df = 3) + splines::ns(PC2, df = 3),
        data = sim_data_boot_i,
        weights = weight_model_boot
      )
      predicted_log_mortality_rate_i <- as.numeric(predict(fit_i, newdata = pred_data_i))
      
    } else if (selected_mortality_model_key == "gam") {
      fit_i <- mgcv::gam(
        y_log_rate ~ s(PC1, k = 4) + s(PC2, k = 4),
        data = sim_data_boot_i,
        weights = weight_model_boot,
        method = "REML"
      )
      predicted_log_mortality_rate_i <- as.numeric(predict(fit_i, newdata = pred_data_i, type = "response"))
      
    } else if (selected_mortality_model_key %in% c("rf_balanced", "rf_small_nodes", "extra_trees")) {
      if (selected_mortality_model_key == "rf_balanced") {
        num_trees_i <- 1000; mtry_i <- 1; min_node_i <- 5; splitrule_i <- "variance"
      } else if (selected_mortality_model_key == "rf_small_nodes") {
        num_trees_i <- 1000; mtry_i <- 2; min_node_i <- 2; splitrule_i <- "variance"
      } else {
        num_trees_i <- 1500; mtry_i <- 2; min_node_i <- 3; splitrule_i <- "extratrees"
      }
      
      set.seed(seed_model_i)
      fit_i <- ranger::ranger(
        y_log_rate ~ PC1 + PC2,
        data = sim_data_boot_i,
        case.weights = sim_data_boot_i$weight_model_boot,
        num.trees = num_trees_i,
        mtry = mtry_i,
        min.node.size = min_node_i,
        splitrule = splitrule_i,
        seed = seed_model_i
      )
      predicted_log_mortality_rate_i <- as.numeric(predict(fit_i, data = pred_data_i)$predictions)
      
    } else if (selected_mortality_model_key %in% c("ridge", "enet_025", "enet_050", "enet_075", "lasso")) {
      alpha_map_i <- c(ridge = 0, enet_025 = 0.25, enet_050 = 0.50, enet_075 = 0.75, lasso = 1)
      
      X_train_i <- model.matrix(~ PC1 + PC2, data = sim_data_boot_i)[, -1, drop = FALSE]
      X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_data_i)[, -1, drop = FALSE]
      
      country_levels_i <- sort(unique(sim_data_boot_i$Country))
      nfolds_i <- min(5L, length(country_levels_i))
      if (nfolds_i < 3L) stop("At least three country-level folds are required.")
      
      set.seed(1400000 + draw_id_i)
      country_fold_i <- sample(rep(seq_len(nfolds_i), length.out = length(country_levels_i)), replace = FALSE)
      names(country_fold_i) <- country_levels_i
      foldid_i <- unname(country_fold_i[sim_data_boot_i$Country])
      
      fit_i <- glmnet::cv.glmnet(
        x = X_train_i,
        y = sim_data_boot_i$y_log_rate,
        weights = sim_data_boot_i$weight_model_boot,
        family = "gaussian",
        alpha = alpha_map_i[selected_mortality_model_key],
        foldid = foldid_i,
        type.measure = "mse",
        standardize = TRUE,
        intercept = TRUE
      )
      
      predicted_log_mortality_rate_i <- as.numeric(
        predict(fit_i, newx = X_pred_i, s = "lambda.min")
      )
      
      lambda_min_i <- fit_i$lambda.min
      lambda_1se_i <- fit_i$lambda.1se
      coefficient_i <- as.matrix(coef(fit_i, s = "lambda.min"))
      intercept_i <- coefficient_i["(Intercept)", 1]
      PC1_coef_i <- coefficient_i["PC1", 1]
      PC2_coef_i <- coefficient_i["PC2", 1]
      
    } else if (selected_mortality_model_key %in% c("xgb_smooth", "xgb_balanced", "xgb_deeper")) {
      X_train_i <- model.matrix(~ PC1 + PC2, data = sim_data_boot_i)[, -1, drop = FALSE]
      X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_data_i)[, -1, drop = FALSE]
      dtrain_i <- xgboost::xgb.DMatrix(
        data = X_train_i,
        label = sim_data_boot_i$y_log_rate,
        weight = sim_data_boot_i$weight_model_boot
      )
      
      if (selected_mortality_model_key == "xgb_smooth") {
        max_depth_i <- 2; eta_i <- 0.02; subsample_i <- 0.85; colsample_i <- 0.85
        min_child_i <- 5; nrounds_i <- 500
      } else if (selected_mortality_model_key == "xgb_balanced") {
        max_depth_i <- 3; eta_i <- 0.03; subsample_i <- 0.80; colsample_i <- 0.80
        min_child_i <- 3; nrounds_i <- 300
      } else {
        max_depth_i <- 4; eta_i <- 0.02; subsample_i <- 0.75; colsample_i <- 0.75
        min_child_i <- 2; nrounds_i <- 450
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
      
      predicted_log_mortality_rate_i <- as.numeric(predict(fit_i, newdata = X_pred_i))
      
    } else if (selected_mortality_model_key == "svm_radial") {
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        stop("Selected mortality model is svm_radial but kernlab is unavailable.")
      }
      
      X_train_i <- model.matrix(~ PC1 + PC2, data = sim_data_boot_i)[, -1, drop = FALSE]
      X_pred_i <- model.matrix(~ PC1 + PC2, data = pred_data_i)[, -1, drop = FALSE]
      
      fit_i <- kernlab::ksvm(
        x = X_train_i,
        y = sim_data_boot_i$y_log_rate,
        type = "eps-svr",
        kernel = "rbfdot",
        kpar = "automatic",
        C = 2,
        scaled = TRUE
      )
      
      predicted_log_mortality_rate_i <- as.numeric(kernlab::predict(fit_i, X_pred_i))
      
    } else {
      stop("Unsupported selected mortality model: ", selected_mortality_model_key)
    }
    
    if (length(predicted_log_mortality_rate_i) != nrow(UNI_base) ||
        any(!is.finite(predicted_log_mortality_rate_i))) {
      stop("Invalid target predictions for ", age_interval_i, ", draw ", draw_id_i, ".")
    }
    
    mortality_rate_matrix_i[, draw_index_i] <- exp(predicted_log_mortality_rate_i)
    
    mortality_rate_selected_model_diagnostic_list[[diagnostic_index_i]] <- data.frame(
      age_interval = age_interval_i,
      draw_id = draw_id_i,
      .imp_pc = pc_imp_i,
      .draw_within_pc = unique(sim_data_i$.draw_within_pc),
      selected_model_key = selected_mortality_model_key,
      n_training_country = dplyr::n_distinct(sim_data_i$Country),
      n_bootstrap_country = dplyr::n_distinct(sim_data_boot_i$Country),
      bootstrap_country_draws = sum(boot_country_count_i$bootstrap_count),
      lambda_min = lambda_min_i,
      lambda_1se = lambda_1se_i,
      intercept = intercept_i,
      PC1 = PC1_coef_i,
      PC2 = PC2_coef_i
    )
    
    diagnostic_index_i <- diagnostic_index_i + 1
  }
  
  assign(unname(mortality_rate_matrix_names[age_interval_i]), mortality_rate_matrix_i)
  message("Finished selected-model mortality extrapolation for: ", age_interval_i)
}

mortality_rate_selected_model_diagnostics <- bind_rows(mortality_rate_selected_model_diagnostic_list)

stopifnot(
  nrow(mortality_rate_selected_model_diagnostics) == length(age_interval) * n_sim,
  all(mortality_rate_selected_model_diagnostics$bootstrap_country_draws == n_mortality_bootstrap_countries),
  !anyNA(matrix_death_rate_q_year0),
  !anyNA(matrix_death_rate_q_year1),
  !anyNA(matrix_death_rate_q_year2to4),
  identical(colnames(matrix_death_rate_q_year0), paste0("draw_", seq_len(1000))),
  identical(colnames(matrix_death_rate_q_year1), paste0("draw_", seq_len(1000))),
  identical(colnames(matrix_death_rate_q_year2to4), paste0("draw_", seq_len(1000)))
)

print(
  mortality_rate_selected_model_diagnostics %>%
    group_by(age_interval) %>%
    summarise(
      mean_n_bootstrap_country = mean(n_bootstrap_country),
      min_n_bootstrap_country = min(n_bootstrap_country),
      max_n_bootstrap_country = max(n_bootstrap_country),
      median_lambda_min = median(lambda_min, na.rm = TRUE),
      .groups = "drop"
    )
)


# Long-format primary mortality draws with explicit PC provenance -----------

mortality_rate_draws_long <- bind_rows(lapply(age_interval, function(age_interval_i) {
  mortality_rate_matrix_i <- get(unname(mortality_rate_matrix_names[age_interval_i]))
  
  as.data.frame(mortality_rate_matrix_i) %>%
    mutate(.row_pred = row_number()) %>%
    pivot_longer(
      cols = starts_with("draw_"),
      names_to = "draw_name",
      values_to = "mortality_rate"
    ) %>%
    mutate(
      draw_id = as.integer(sub("^draw_", "", draw_name)),
      age_interval = age_interval_i,
      age_group = unname(age_group_labels[age_interval_i])
    ) %>%
    dplyr::select(-draw_name) %>%
    left_join(UNI_base, by = ".row_pred") %>%
    left_join(
      mortality_draw_design %>% dplyr::select(draw_id, .imp_pc, .draw_within_pc),
      by = "draw_id"
    )
}))

saveRDS(
  mortality_rate_draws_long,
  "outputs/pipelines/mortality_rate_uncertainty/mortality_rate_draws_1000_long.rds"
)


# Summarize extrapolated mortality rates -----------------------------------

matrix_death_rate_list <- unname(mortality_rate_matrix_names[age_interval])
mortality_rate_title <- c("Age 0 (total infant)", "Age 1", "Age 2 to 4")

pdf("figures/main_analysis/mortality/S7.Extrapolated mortality rate by age interval.pdf", width = 6, height = 3.3)
par(mfrow = c(1, 3))

for (age_index_i in seq_along(matrix_death_rate_list)) {
  mortality_rate_matrix_i <- get(matrix_death_rate_list[age_index_i])
  hist(mortality_rate_matrix_i, main = mortality_rate_title[age_index_i], xlab = "Mortality rate per person-year")
}

dev.off()

mortality_rate_summary_list <- vector("list", length(matrix_death_rate_list))
names(mortality_rate_summary_list) <- age_interval

for (age_index_i in seq_along(matrix_death_rate_list)) {
  age_interval_i <- age_interval[age_index_i]
  mortality_rate_matrix_i <- get(matrix_death_rate_list[age_index_i])
  
  mortality_rate_summary_list[[age_interval_i]] <- data.frame(
    Country = UNI_base$Country,
    Year = UNI_base$Year,
    gbd_region = UNI_base$gbd_region,
    median = apply(mortality_rate_matrix_i, 1, median, na.rm = TRUE),
    lower = apply(mortality_rate_matrix_i, 1, quantile, probs = 0.025, na.rm = TRUE),
    upper = apply(mortality_rate_matrix_i, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
}

summary_mortality_rate_q_year0 <- mortality_rate_summary_list$m_year0
summary_mortality_rate_q_year1 <- mortality_rate_summary_list$m_year1
summary_mortality_rate_q_year2to4 <- mortality_rate_summary_list$m_year2to4

saveRDS(matrix_death_rate_q_year0, "outputs/pipelines/matrix_death_rate_year0.rds")
saveRDS(matrix_death_rate_q_year1, "outputs/pipelines/matrix_death_rate_year1.rds")
saveRDS(matrix_death_rate_q_year2to4, "outputs/pipelines/matrix_death_rate_year2to4.rds")
saveRDS(summary_mortality_rate_q_year0, "outputs/pipelines/summary_mortality_rate_year0.rds")
saveRDS(summary_mortality_rate_q_year1, "outputs/pipelines/summary_mortality_rate_year1.rds")
saveRDS(summary_mortality_rate_q_year2to4, "outputs/pipelines/summary_mortality_rate_year2to4.rds")

saveRDS(
  mortality_rate_selected_model_diagnostics,
  "outputs/pipelines/mortality_rate_selected_model_diagnostics.rds"
)

# Compatibility file name retained; contents now correspond to the selected common model.
saveRDS(
  mortality_rate_selected_model_diagnostics,
  "outputs/pipelines/mortality_rate_enet_025_diagnostics.rds"
)

saveRDS(
  mortality_country_bootstrap_list,
  "outputs/pipelines/mortality_rate_country_bootstrap_counts.rds"
)






# _____________________________________________ ----------
# 5-1. Selected-model holdout validation plot only -----------------------------

mortality_model_search_dir <- "outputs/pipelines/mortality_rate_full_model_search_outputs"
validation_fig_dir <- "figures/main_analysis/mortality_validation_predictions"
dir.create(validation_fig_dir, recursive = TRUE, showWarnings = FALSE)

# Read saved validation results
mortality_validation_predictions <- readRDS(file.path(mortality_model_search_dir, "mortality_age_specific_validation_predictions.rds"))
mortality_common_model_selection <- readRDS(file.path(mortality_model_search_dir, "mortality_common_model_selection.rds"))

selected_mortality_model_key <- mortality_common_model_selection$model_key[1]

mortality_selected_model_holdout <- mortality_validation_predictions %>%
  dplyr::filter(model_key == selected_mortality_model_key, is.finite(observed_rate), is.finite(predicted_rate))

mortality_age_interval_list <- c("m_year0", "m_year1", "m_year2to4")

point_col <- adjustcolor("#4D4D4D", alpha.f = 0.18)

pdf(file.path(validation_fig_dir, "mortality_selected_model_holdout_validation_scatter.pdf"),
    width = 2.2, height = 6.0, useDingbats = FALSE)

old_par <- par(no.readonly = TRUE)

layout(matrix(c(0, 1, 0, 2, 0, 3, 0), nrow = 7, byrow = TRUE),
       heights = c(0.2, 1, 0.25, 1, 0.25, 1, 0.4))

par(mar = c(2.0, 6.0, 1.0, 1.0), mgp = c(1.8, 0.5, 0), tcl = -0.25,
    cex.axis = 1.0, cex.lab = 1.0, cex.main = 1.0)

for(j in seq_along(mortality_age_interval_list)){
  
  age_interval_i <- mortality_age_interval_list[j]
  
  plot_data_i <- mortality_selected_model_holdout %>%
    dplyr::filter(age_interval == age_interval_i)
  
  lim_max_i <- max(plot_data_i$observed_rate*1000, plot_data_i$predicted_rate*1000, na.rm = TRUE)
  lim_i <- c(0, lim_max_i)
  axis_at_i <- seq(0, lim_max_i, length.out = 4)
  axis_labels_i <- formatC(axis_at_i, format = "f", digits = 0)
  
  plot(
    plot_data_i$observed_rate*1000,
    plot_data_i$predicted_rate*1000,
    xlim = lim_i, ylim = lim_i, xaxs = "i", yaxs = "i",
    axes = FALSE, xlab = "", ylab = "", main = "",
    pch = 16, cex = 0.6, col = point_col
  )
  
  usr <- par("usr")
  mtext(letters[j], side = 3, line = 1.8, at = usr[1] - 0.60 * diff(usr[1:2]),
        adj = 0, font = 2, cex = 0.75)
  
  axis(1, at = axis_at_i, labels = axis_labels_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
  axis(2, at = axis_at_i, labels = axis_labels_i, las = 1, lwd = 0.8, lwd.ticks = 0.8)
  
  mtext("Held-out predicted\nmortality rate",
        side = 2, line = 2.5, cex = 0.8)
  
  if(j == length(mortality_age_interval_list)){
    mtext("Held-out estimated\nmortality rate",
          side = 1, line = 5.0, cex = 0.8)
  }
  
  abline(a = 0, b = 1, lty = 1, lwd = 0.8, col = "darkred")
}

par(old_par)
dev.off()














# _____________________________________________ ----------
# 6. Prepare age-specific population data for subsequent death calculations -----

# Mortality matrices contain one row per unique target Country-Year. The five
# PC imputations are represented across columns/draws, not by duplicating rows.
UNI_model <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")
UNI_base <- if (file.exists("outputs/pipelines/pc_gdp_2023_mortality_base.rds")) {
  readRDS("outputs/pipelines/pc_gdp_2023_mortality_base.rds")
} else {
  UNI_model %>%
    dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
    distinct()
}

df.population <- read.csv("data/metadata/Population_by_age.csv")

population_country_mapping <- c(
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

df.population <- df.population %>%
  mutate(
    Country = ifelse(
      Country %in% names(population_country_mapping),
      unname(population_country_mapping[Country]),
      Country
    )
  )

population_unmatched <- setdiff(unique(UNI_base$Country), unique(df.population$Country))
print(population_unmatched)

df.population <- UNI_base %>%
  dplyr::select(Country, Year) %>%
  left_join(df.population, by = c("Country", "Year"))

dim(df.population)


# 6-1. death number calculation ---------------------------------------------

matrix_death_rate_q_year0    <- readRDS("outputs/pipelines/matrix_death_rate_year0.rds")
matrix_death_rate_q_year1    <- readRDS("outputs/pipelines/matrix_death_rate_year1.rds")
matrix_death_rate_q_year2to4 <- readRDS("outputs/pipelines/matrix_death_rate_year2to4.rds")

stopifnot(
  nrow(matrix_death_rate_q_year0) == nrow(UNI_base),
  nrow(matrix_death_rate_q_year1) == nrow(UNI_base),
  nrow(matrix_death_rate_q_year2to4) == nrow(UNI_base)
)

row_2023 <- which(UNI_base$Year == 2023)

population <- read.csv("data/metadata/Population_by_age.csv")

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

setdiff(UNI_base$Country, population$Country)

UNI_pop <- UNI_base %>%
  dplyr::select(Country, Year, Income) %>%
  left_join(population, by = c("Country", "Year")) %>%
  mutate(
    Age_2to4 = Age_2 + Age_3 + Age_4,
    Age_all = Age_0 + Age_1 + Age_2 + Age_3 + Age_4
  ) %>%
  dplyr::select(Country, Year, Income, Age_0, Age_1, Age_2to4, Age_all)

if (any(!is.finite(UNI_pop$Age_0)) ||
    any(!is.finite(UNI_pop$Age_1)) ||
    any(!is.finite(UNI_pop$Age_2to4))) {
  stop("Missing age-specific population for one or more mortality target rows.")
}

UNI_pop_2023 <- UNI_pop[row_2023, , drop = FALSE]

matrix_death_rate_q_year0_2023 <- matrix_death_rate_q_year0[row_2023, , drop = FALSE]
matrix_death_rate_q_year1_2023 <- matrix_death_rate_q_year1[row_2023, , drop = FALSE]
matrix_death_rate_q_year2to4_2023 <- matrix_death_rate_q_year2to4[row_2023, , drop = FALSE]

death_year0_2023_LMIC <- sweep(matrix_death_rate_q_year0_2023, 1, UNI_pop_2023$Age_0, "*")
death_year1_2023_LMIC <- sweep(matrix_death_rate_q_year1_2023, 1, UNI_pop_2023$Age_1, "*")
death_year2to4_2023_LMIC <- sweep(matrix_death_rate_q_year2to4_2023, 1, UNI_pop_2023$Age_2to4, "*")

death_all_2023_LMIC_by_country <- death_year0_2023_LMIC + death_year1_2023_LMIC + death_year2to4_2023_LMIC
death_all_2023_LMIC <- colSums(death_all_2023_LMIC_by_country)

summary(death_all_2023_LMIC)
length(death_all_2023_LMIC)

save(death_all_2023_LMIC, file = "outputs/all_death_draw.RData")
write.csv(death_all_2023_LMIC, "outputs/all_death_draw.csv")

# 6-2. Aggregate absolute deaths by income -------------------------------
death_all_2023_by_income <- death_all_2023_LMIC_by_country %>%
  as.data.frame() %>%
  mutate(Income = UNI_pop_2023$Income) %>%
  group_by(Income) %>%
  summarise(
    across(where(is.numeric), sum),
    .groups = "drop")

# Convert to matrix
death_all_2023_income_matrix <- death_all_2023_by_income %>%
  tibble::column_to_rownames("Income") %>%
  as.matrix()

save(death_all_2023_income_matrix, file = "outputs/death_all_2023_income_matrix.RData")
write.csv(death_all_2023_income_matrix, "outputs/death_all_2023_income_matrix.csv")

# _____________________________________________ ----------
# 7. Champs_proportion of bacteiral death----------------------------------------------------
rm(list = ls()); gc()
library(mice)
library(glmmTMB)
library(broom.mixed)
library(dplyr)
library(naniar)
library(marginaleffects)
library(sandwich)
library(survival)
library(MASS)
library(car)
library(FNN)
library(rnaturalearth)
library(ggplot2)
library(patchwork)

options(scipen = 999)

df.demo <- read.csv("data/CHAMPS/CHAMPS_deid_basic_demographics.csv")
df.decode <- read.csv("data/CHAMPS/CHAMPS_deid_decode_results.csv")
df.va <- read.csv("data/CHAMPS/CHAMPS_deid_verbal_autopsy.csv")
df.tac <- read.csv("data/CHAMPS/CHAMPS_deid_tac_results.csv")
df.lab <- read.csv("data/CHAMPS/CHAMPS_deid_lab_results_2.csv")

###
df.list <- list(
  df.demo   = df.demo, 
  df.decode = df.decode, 
  df.lab    = df.lab, 
  df.va     = df.va, 
  df.tac    = df.tac
)

df.list <- lapply(df.list, function(df) {
  df %>%
    mutate(across(where(is.character), ~ {
      x <- trimws(.)
      x <- na_if(x, "")
      x <- na_if(x, "Don’t know")
      x <- na_if(x, "Donâ€™t know") # Fixes the encoding artifact
      x <- na_if(x, "dk")
      x <- na_if(x, "DK")
      x <- na_if(x, "Doesn't know")
      x <- na_if(x, "ref")
      x <- na_if(x, "dk")
      x
    }))
})
list2env(df.list, envir = .GlobalEnv)

### 1. Age distribution from 1) demo ###
demo_selected <- df.demo %>% dplyr::select(c(champs_deid, sex, site_iso_code, date_of_birth, date_of_death, age_years, age_months, age_days, age_group))
demo_selected <- demo_selected[demo_selected$age_years != 5, ]
demo_selected <- demo_selected[demo_selected$age_group != "Stillbirth",]
demo_selected <- demo_selected %>% 
  mutate(age_group = ifelse(age_years %in% c(2,3,4), "Age_2to4",
                            ifelse(age_years %in% c(1), "Age_1",
                                   ifelse(age_months %in% c(0:11), "Age_0", "unclassified"))))

demo_selected <- demo_selected %>% mutate(Year = format(as.Date(date_of_death), "%Y"))
demo_selected <- demo_selected %>% dplyr::select("champs_deid", "site_iso_code", "Year","age_group", "sex")
# demo_selected <- demo_selected[demo_selected$site_iso_code != "NG",]
# demo_selected <- demo_selected[demo_selected$site_iso_code != "PK",]

# 2) syndrome
decode_syndrome <- df.decode %>% dplyr::select(c(champs_deid, Underlying_Cause, UC_champs_group_desc))
icd_code <- unique(decode_syndrome$Underlying_Cause)
# write.csv(icd_code, "data/analysis/ICD_code.csv")

### 2. LRTI from Syndrome ###
update_syndromes <- function(data, icd_col, syndrome_col) {
  matches <- match(data[[icd_col]], map$icd)
  valid_rows <- !is.na(matches)
  data[[syndrome_col]][valid_rows] <- map$syndrome[matches[valid_rows]]
  return(data)
}

col_icd <- "Underlying_Cause" ; col_syndrome <- "UC_champs_group_desc"

map <- data.frame(
  icd = c("CA40.01","CA40.02","CA40.03","CA40.06","CA40.07","CA40.0Y","CA40.0Z","CA40.10","CA40.11","CA40.12","CA40.13","CA40.1Y","CA40.20","CA40.Y", "CA40.Z","J12.0","J12.1","J12.2","J12.3","J12.8","J12.9","J13","J14","J15","J15.0","J15.1","J15.3","J15.4","J15.5","J15.6","J15.8","J15.9", "J16.8","J17","J17.3","J18","J18.0","J18.8","J18.9","J21.0","J21.9","J85.1","J86.9","J10.0","P23","P23.0","P23.2","P23.3","P23.4","P23.6","P23.8","P23.9"),
  syndrome = c(rep("Lower respiratory infections", 52)),
  stringsAsFactors = FALSE
)

decode_syndrome_LRTI <- update_syndromes(decode_syndrome, col_icd, col_syndrome)
decode_syndrome_LRTI <- decode_syndrome_LRTI %>% mutate(LRTI = ifelse(UC_champs_group_desc == "Lower respiratory infections", 1, 0))

decode_syndrome_LRTI <- decode_syndrome_LRTI %>% dplyr::select("champs_deid", "LRTI")
decode_syndrome <- decode_syndrome %>% dplyr::select("champs_deid", "UC_champs_group_desc")
decode_syndrome <- decode_syndrome %>% inner_join(decode_syndrome_LRTI, by = c("champs_deid"))

### 3. Fever from VA ###
df.va <- df.va %>% mutate(Fever = ifelse(Id10147 %in% c("YES", "yes", "Yes"), 1,
                                         ifelse(Id10147 %in% c("NO", "no", "No"), 0, NA)))

df.va <- df.va %>% mutate(Ab = ifelse(Id10423 %in% c("YES", "yes", "Yes"), 1,
                                      ifelse(Id10423 %in% c("NO", "no", "No"), 0, NA)))

df.va_selected <- df.va %>% dplyr::select(c(champs_deid, Fever, Ab))

### 4. Bacterial infection 
decode_etiology <- df.decode %>% dplyr::select(c(champs_deid, Underlying_Cause_Factor_etiol1, Underlying_Cause_Factor_etiol2, Underlying_Cause_Factor_etiol3))

decode_etiology <- decode_etiology %>%
  mutate(missing_Underlying_Cause =
           ifelse((is.na(Underlying_Cause_Factor_etiol1) | Underlying_Cause_Factor_etiol1 == "") &
                    (is.na(Underlying_Cause_Factor_etiol2) | Underlying_Cause_Factor_etiol2 == "") &
                    (is.na(Underlying_Cause_Factor_etiol3) | Underlying_Cause_Factor_etiol3 == ""),
                  NA, "with_eti_U_data"))

underlying_cols <- c("Underlying_Cause_Factor_etiol1",
                     "Underlying_Cause_Factor_etiol2",
                     "Underlying_Cause_Factor_etiol3")

bact_pathogen <- list(
  Acinetobacter_baumannii = c("Acinetobacter spp."),
  Acinetobacter_spp = c("Acinetobacter spp.","Acinetobacter baumannii"),
  Aeromonas_spp = c("Aeromonas spp."),
  B_parapertussis = c("B. parapertussis","Bordetella spp. (B. parapertussis/B. bronchiseptica)"),
  B_pertussis = c("B. pertussis"),
  Bacillus_spp = c("Bacillus spp."),
  Bordetella_spp = c("Bordetella spp.","B. pertussis","B. parapertussis","Bordetella spp. (B. parapertussis/B. bronchiseptica)"),
  Brucella_spp = c("Brucella spp."),
  Campylobacter_coli = c("Campylobacter coli"),
  Campylobacter_jejuni = c("Campylobacter jejuni"),
  Chlamydia_trachomatis = c("Chlamydia trachomatis"),
  Coagulase_negative_Staphylococcus = c("Coagulase negative Staphylococcus"),
  Corynebacterium_diptheriae = c("Corynebacterium diptheriae"),
  Enterobacter_cloacae = c("Enterobacter cloacae"),
  Enterococcus_faecalis = c("Enterococcus faecalis"),
  Enterococcus_faecium = c("Enterococcus faecium"),
  Enterococcus_spp = c("Enterococcus spp.","Enterococcus faecalis","Enterococcus faecium"),
  Escherichia_coli = c("Escherichia coli","Escherichia coli/Shigella spp."),
  Haemophilus_influenzae = c("Haemophilus influenzae","Haemophilus influenzae Type A","Haemophilus influenzae Type B","Haemophilus parainfluenzae"),
  Haemophilus_influenzae_Type_A = c("Haemophilus influenzae Type A"),
  Haemophilus_influenzae_Type_B = c("Haemophilus influenzae Type B"),
  Haemophilus_parainfluenzae = c("Haemophilus parainfluenzae"),
  Klebsiella_pneumoniae = c("Klebsiella pneumoniae"),
  Klebsiella_spp = c("Klebsiella spp.","Klebsiella pneumoniae","Klebsiella terragina"),
  Klebsiella_terragina = c("Klebsiella terragina"),
  Listeria_monocytogenes = c("Listeria monocytogenes"),
  Moraxella_catarrhalis = c("Moraxella catarrhalis"),
  Mycobacterium_tuberculosis = c("Mycobacterium tuberculosis"),
  Neisseria_meningitidis = c("Neisseria meningitidis"),
  Orientia_tsutsugamushi = c("Orientia tsutsugamushi"),
  Proteus_mirabilis = c("Proteus mirabilis"),
  Pseudomonas_aeruginosa = c("Pseudomonas aeruginosa"),
  Salmonella_enterica_sub_species_paratyphi_A = c("Salmonella enterica  sub-species paratyphi A"),
  Salmonella_enterica_sub_species_typhi = c("Salmonella enterica  sub-species typhi"),
  Salmonella_spp = c("Salmonella spp.","Salmonella enterica  sub-species paratyphi A","Salmonella enterica  sub-species typhi"),
  Shigella = c("Shigella","Escherichia coli/Shigella spp."),
  Staphylococcus_aureus = c("Staphylococcus aureus"),
  Staphylococcus_epidermidis = c("Staphylococcus epidermidis"),
  Staphylococcus_spp = c("Staphylococcus spp.","Staphylococcus aureus","Staphylococcus epidermidis"),
  Stenotrophomonas_maltophilia = c("Stenotrophomonas maltophilia"),
  Streptococcus_agalactiae = c("Streptococcus agalactiae"),
  Streptococcus_pneumoniae = c("Streptococcus pneumoniae"),
  Streptococcus_pyogenes = c("Streptococcus pyogenes"),
  Streptococcus_spp = c("Streptococcus spp.","Streptococcus agalactiae","Streptococcus pneumoniae","Streptococcus pyogenes"),
  Treponema_pallidum = c("Treponema pallidum"),
  Ureaplasma_spp = c("Ureaplasma spp."),
  Vibrio_cholerae = c("Vibrio cholerae"),
  Viridans_streptococcus = c("Viridans streptococcus"),
  other_underlying_etiology = c("Other Etiology/Agent","Respiratory syncytial virus (RSV)","Pneumocystis jirovecii","Candida parapsilosis","Influenza A","Rhinovirus","Parainfluenza virus type 2","Human Immunodeficiency Virus (HIV)","Parechovirus","Parainfluenza virus type 4","Human coronavirus 3 (OC43)","Norovirus GII","Lassa Fever Virus","Dengue (pan)","Sapovirus I/II/IV","Hepatitis C","Plasmodium malariae","Cytomegalovirus (CMV)","Parainfluenza virus type 1","Parainfluenza virus type 3","Candida spp.","Measles","Human metapneumovirus (HMPV)","Norovirus GI","Herpes simplex virus 1 (HSV1)","Varicella zoster virus","Enterovirus","Sapovirus V","Herpes simplex virus 2 (HSV2)","Ascaris lumbricoides","Adenovirus 40/41","Aspergillus spp.","Astrovirus","Giardia spp.","Plasmodium falciparum","Candida albicans","Adenovirus","Influenza B","Candida glabrata","Rotavirus A","Nipah virus","Candida tropicalis","Parvovirus B19","Rotavirus non-typable","Toxoplasma gondii","Cryptosporidium parvum","Polymicrobial")
)

calc_presence <- function(df, cols, targets) {
  mat <- as.matrix(df[cols])
  matches <- matrix(mat %in% targets, nrow = nrow(mat), ncol = ncol(mat))
  as.integer(rowSums(matches, na.rm = TRUE) > 0)
}

# Loop through pathogens
for (p_name in names(bact_pathogen)) {
  targets <- bact_pathogen[[p_name]]
  und_path_flag  <- calc_presence(decode_etiology, underlying_cols, targets)
  new_col_name <- paste0(p_name)
  decode_etiology[[new_col_name]] <- paste0(und_path_flag)
}

decode_etiology <- decode_etiology %>%
  mutate(bact_underlying_etiology_1 = ifelse(is.na(missing_Underlying_Cause),NA,ifelse(rowSums(across(6:(length(decode_etiology)-1), ~ as.numeric(.)), na.rm = TRUE) > 0, 1, 0)))

decode_etiology <- decode_etiology %>% 
  mutate(bact_underlying_etiology_2 = ifelse(rowSums(across(6:(length(decode_etiology)-2), ~ as.numeric(.)), na.rm = TRUE) > 0, 1, 0))

bact_underlying_etiology <- decode_etiology[, c(1, (length(decode_etiology)-2):length(decode_etiology))]

bact_underlying_etiology <- bact_underlying_etiology %>% mutate(microbial_cause = ifelse(other_underlying_etiology == 1 | bact_underlying_etiology_2 == 1, 1, 0))


## (all cause)
decode_etiology_all <- df.decode[,c(1, 37:length(df.decode))]
decode_etiology_all <- decode_etiology_all %>%
  mutate(missing_Underlying_Cause =
           ifelse((is.na(Immediate_Cause_of_Death_etiol1)  | Immediate_Cause_of_Death_etiol1 == "")  &
                    (is.na(Immediate_Cause_of_Death_etiol2)  | Immediate_Cause_of_Death_etiol2 == "")  &
                    (is.na(Immediate_Cause_of_Death_etiol3)  | Immediate_Cause_of_Death_etiol3 == "")  &
                    (is.na(Underlying_Cause_Factor_etiol1) | Underlying_Cause_Factor_etiol1 == "") &
                    (is.na(Underlying_Cause_Factor_etiol2) | Underlying_Cause_Factor_etiol2 == "") &
                    (is.na(Underlying_Cause_Factor_etiol3) | Underlying_Cause_Factor_etiol3 == "") &
                    (is.na(Morbid_Condition_01_etiol1)     | Morbid_Condition_01_etiol1 == "")     &
                    (is.na(Morbid_Condition_01_etiol2)     | Morbid_Condition_01_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_01_etiol3)     | Morbid_Condition_01_etiol3 == "")     &
                    (is.na(Morbid_Condition_02_etiol1)     | Morbid_Condition_02_etiol1 == "")     &
                    (is.na(Morbid_Condition_02_etiol2)     | Morbid_Condition_02_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_02_etiol3)     | Morbid_Condition_02_etiol3 == "")     &
                    (is.na(Morbid_Condition_03_etiol1)     | Morbid_Condition_03_etiol1 == "")     &
                    (is.na(Morbid_Condition_03_etiol2)     | Morbid_Condition_03_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_03_etiol3)     | Morbid_Condition_03_etiol3 == "")     &
                    (is.na(Morbid_Condition_04_etiol1)     | Morbid_Condition_04_etiol1 == "")     &
                    (is.na(Morbid_Condition_04_etiol2)     | Morbid_Condition_04_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_04_etiol3)     | Morbid_Condition_04_etiol3 == "")     &
                    (is.na(Morbid_Condition_05_etiol1)     | Morbid_Condition_05_etiol1 == "")     &
                    (is.na(Morbid_Condition_05_etiol2)     | Morbid_Condition_05_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_05_etiol3)     | Morbid_Condition_05_etiol3 == "")     &
                    (is.na(Morbid_Condition_06_etiol1)     | Morbid_Condition_06_etiol1 == "")     &
                    (is.na(Morbid_Condition_06_etiol2)     | Morbid_Condition_06_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_06_etiol3)     | Morbid_Condition_06_etiol3 == "")     &
                    (is.na(Morbid_Condition_07_etiol1)     | Morbid_Condition_07_etiol1 == "")     &
                    (is.na(Morbid_Condition_07_etiol2)     | Morbid_Condition_07_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_07_etiol3)     | Morbid_Condition_07_etiol3 == "")     &
                    (is.na(Morbid_Condition_08_etiol1)     | Morbid_Condition_08_etiol1 == "")     &
                    (is.na(Morbid_Condition_08_etiol2)     | Morbid_Condition_08_etiol2 == "")     &                    
                    (is.na(Morbid_Condition_08_etiol3)     | Morbid_Condition_08_etiol3 == "")     ,
                  NA, "with_eti_U_data"))

underlying_cols <- c("Immediate_Cause_of_Death_etiol1" ,"Immediate_Cause_of_Death_etiol2" ,"Immediate_Cause_of_Death_etiol3" ,"Underlying_Cause_Factor_etiol1" ,"Underlying_Cause_Factor_etiol2" ,"Underlying_Cause_Factor_etiol3" ,"Morbid_Condition_01_etiol1" ,"Morbid_Condition_01_etiol2" ,"Morbid_Condition_01_etiol3" ,"Morbid_Condition_02_etiol1" ,"Morbid_Condition_02_etiol2" ,"Morbid_Condition_02_etiol3" ,"Morbid_Condition_03_etiol1" ,"Morbid_Condition_03_etiol2" ,"Morbid_Condition_03_etiol3" ,"Morbid_Condition_04_etiol1" ,"Morbid_Condition_04_etiol2" ,"Morbid_Condition_04_etiol3" ,"Morbid_Condition_05_etiol1" ,"Morbid_Condition_05_etiol2" ,"Morbid_Condition_05_etiol3" ,"Morbid_Condition_06_etiol1" ,"Morbid_Condition_06_etiol2" ,"Morbid_Condition_06_etiol3" ,"Morbid_Condition_07_etiol1" ,"Morbid_Condition_07_etiol2" ,"Morbid_Condition_07_etiol3" ,"Morbid_Condition_08_etiol1" ,"Morbid_Condition_08_etiol2" ,"Morbid_Condition_08_etiol3")

decode_etiology_all[, underlying_cols][decode_etiology_all[, underlying_cols] == "NA"] <- NA

for (p_name in names(bact_pathogen)) {
  targets <- bact_pathogen[[p_name]]
  und_path_flag  <- calc_presence(decode_etiology_all, underlying_cols, targets)
  new_col_name <- paste0(p_name)
  decode_etiology_all[[new_col_name]] <- paste0(und_path_flag)
}

decode_etiology_all <- decode_etiology_all %>%
  mutate(bact_all_etiology_1 = ifelse(is.na(missing_Underlying_Cause),NA,ifelse(rowSums(across(33:(length(decode_etiology_all)-1), ~ as.numeric(.)), na.rm = TRUE) > 0, 1, 0)))

decode_etiology_all <- decode_etiology_all %>% 
  mutate(bact_all_etiology_2 = ifelse(rowSums(across(33:(length(decode_etiology_all)-2), ~ as.numeric(.)), na.rm = TRUE) > 0, 1, 0))

bact_all_etiology <- decode_etiology_all[, c(1, (length(decode_etiology_all)-2):length(decode_etiology_all))]

df.analyze         <- demo_selected            %>% inner_join(decode_syndrome, by = c("champs_deid"))
df.analyze         <- df.analyze               %>% inner_join(df.va_selected, by = c("champs_deid"))
df.analyze         <- df.analyze               %>% inner_join(bact_underlying_etiology, by = c("champs_deid"))
df.analyze         <- df.analyze               %>% inner_join(bact_all_etiology, by = c("champs_deid"))

df.analyze <- df.analyze %>% mutate(
  bact_underlying_fever = ifelse(Fever == 1 & LRTI == 0 & bact_underlying_etiology_1 == 1, 1, 0),
  bact_all_fever = ifelse(Fever == 1 & LRTI == 0 & bact_all_etiology_1 == 1, 1, 0),
  bact_underlying_LRTI = ifelse(LRTI == 1 & bact_underlying_etiology_1 == 1, 1, 0),
  bact_all_LRTI = ifelse(LRTI == 1 & bact_all_etiology_1 == 1, 1, 0))

df.analyze <- df.analyze %>% 
  mutate(
    Country = case_when(
      site_iso_code == "ZA" ~ "South Africa",
      site_iso_code == "ML" ~ "Mali",
      site_iso_code == "MZ" ~ "Mozambique",
      site_iso_code == "KE" ~ "Kenya",
      site_iso_code == "ET" ~ "Ethiopia",
      site_iso_code == "BD" ~ "Bangladesh",
      site_iso_code == "SL" ~ "Sierra Leone",
      site_iso_code == "PK" ~ "Pakistan",
      TRUE ~ NA_character_
    )
  )

df.analyze$Year[df.analyze$Year %in% c(2024, 2025)] <- 2023
df.analyze$CountryYear <- paste(df.analyze$Country, df.analyze$Year, sep = "_")
df.analyze$Year <- as.integer(df.analyze$Year)

UNI_model <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")
if (!".imp_pc" %in% names(UNI_model)) stop("Mortality PCA file must contain .imp_pc.")

UNI_base <- if (file.exists("outputs/pipelines/pc_gdp_2023_mortality_base.rds")) {
  readRDS("outputs/pipelines/pc_gdp_2023_mortality_base.rds")
} else {
  UNI_model %>%
    dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
    distinct()
}

# Restrict CHAMPS records using only one row per Country-Year. Do not join the
# five PC-imputation rows to individual deaths.
df.analyze <- df.analyze %>%
  inner_join(UNI_base %>% dplyr::select(Country, Year), by = c("Country", "Year"))

df.analyze %>%
  group_by(Country) %>%
  summarise(median_year = median(Year, na.rm = TRUE), .groups = "drop")

death_agg <- df.analyze %>%
  group_by(Country, age_group) %>%
  summarise(
    total_deaths = n(),
    bact_fever_deaths = sum(bact_all_fever, na.rm = TRUE),
    bact_LRTI_deaths = sum(bact_all_LRTI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    other_deaths_LRTI = total_deaths - bact_LRTI_deaths,
    other_deaths_fever = total_deaths - bact_fever_deaths,
    observed_prop_bact_LRTI = bact_LRTI_deaths / total_deaths,
    observed_prop_bact_fever = bact_fever_deaths / total_deaths,
    Year = case_when(
      Country == "Bangladesh"   ~ 2022,
      Country == "Ethiopia"     ~ 2023,
      Country == "Kenya"        ~ 2021,
      Country == "Mali"         ~ 2021,
      Country == "Mozambique"   ~ 2021,
      Country == "Sierra Leone" ~ 2022,
      Country == "South Africa" ~ 2020
    )
  )

write.csv(death_agg, "outputs/pipelines/champs_table.csv", row.names = FALSE)


# Fit CHAMPS bacterial-death fraction models separately in each PC imputation ----

mortality_draw_design <- readRDS(
  "outputs/pipelines/mortality_rate_uncertainty/mortality_draw_design_5PC_200draw.rds"
)

pc_imp_values <- sort(unique(mortality_draw_design$.imp_pc))
n_draw_within_pc <- max(mortality_draw_design$.draw_within_pc)
age_levels <- c("Age_0", "Age_1", "Age_2to4")

stopifnot(
  nrow(mortality_draw_design) == 1000,
  length(pc_imp_values) == 5,
  all((mortality_draw_design %>% count(.imp_pc) %>% pull(n)) == n_draw_within_pc)
)

mod_bact_LRTI_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
mod_bact_fever_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
beta_draws_bact_LRTI_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
beta_draws_bact_fever_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))

n_uni <- nrow(UNI_base)
row_key_base <- paste(UNI_base$Country, UNI_base$Year, sep = "___")

matrix_prop_bact_LRTI_deaths_Age_0 <- matrix(
  NA_real_, nrow = n_uni, ncol = 1000,
  dimnames = list(paste(UNI_base$Country, UNI_base$Year, sep = "_"), paste0("draw_", 1:1000))
)
matrix_prop_bact_LRTI_deaths_Age_1 <- matrix_prop_bact_LRTI_deaths_Age_0
matrix_prop_bact_LRTI_deaths_Age_2to4 <- matrix_prop_bact_LRTI_deaths_Age_0
matrix_prop_bact_fever_deaths_Age_0 <- matrix_prop_bact_LRTI_deaths_Age_0
matrix_prop_bact_fever_deaths_Age_1 <- matrix_prop_bact_LRTI_deaths_Age_0
matrix_prop_bact_fever_deaths_Age_2to4 <- matrix_prop_bact_LRTI_deaths_Age_0

for (pc_imp_i in pc_imp_values) {
  death_agg_i <- death_agg %>%
    inner_join(
      UNI_model %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(Country, Year, PC1, PC2),
      by = c("Country", "Year")
    ) %>%
    mutate(age_group = factor(age_group, levels = age_levels))
  
  mod_bact_LRTI_i <- glm(
    cbind(bact_LRTI_deaths, other_deaths_LRTI) ~ PC1 + PC2 + age_group,
    family = binomial(link = "logit"),
    data = death_agg_i
  )
  
  mod_bact_fever_i <- glm(
    cbind(bact_fever_deaths, other_deaths_fever) ~ PC1 + PC2 + age_group,
    family = binomial(link = "logit"),
    data = death_agg_i
  )
  
  set.seed(700000 + pc_imp_i)
  beta_draws_bact_LRTI_i <- MASS::mvrnorm(
    n = n_draw_within_pc,
    mu = coef(mod_bact_LRTI_i),
    Sigma = vcov(mod_bact_LRTI_i)
  )
  
  set.seed(800000 + pc_imp_i)
  beta_draws_bact_fever_i <- MASS::mvrnorm(
    n = n_draw_within_pc,
    mu = coef(mod_bact_fever_i),
    Sigma = vcov(mod_bact_fever_i)
  )
  
  colnames(beta_draws_bact_LRTI_i) <- names(coef(mod_bact_LRTI_i))
  colnames(beta_draws_bact_fever_i) <- names(coef(mod_bact_fever_i))
  
  mod_bact_LRTI_list[[as.character(pc_imp_i)]] <- mod_bact_LRTI_i
  mod_bact_fever_list[[as.character(pc_imp_i)]] <- mod_bact_fever_i
  beta_draws_bact_LRTI_list[[as.character(pc_imp_i)]] <- beta_draws_bact_LRTI_i
  beta_draws_bact_fever_list[[as.character(pc_imp_i)]] <- beta_draws_bact_fever_i
  
  UNI_pred_base_i <- UNI_model %>%
    filter(.imp_pc == pc_imp_i) %>%
    mutate(.row_pred = match(paste(Country, Year, sep = "___"), row_key_base)) %>%
    arrange(.row_pred)
  
  if (nrow(UNI_pred_base_i) != n_uni || anyNA(UNI_pred_base_i$.row_pred)) {
    stop("CHAMPS target grid is not aligned for PC imputation ", pc_imp_i, ".")
  }
  
  UNI_pred_i <- bind_rows(
    UNI_pred_base_i %>% mutate(age_group = "Age_0"),
    UNI_pred_base_i %>% mutate(age_group = "Age_1"),
    UNI_pred_base_i %>% mutate(age_group = "Age_2to4")
  ) %>%
    mutate(age_group = factor(age_group, levels = age_levels))
  
  X_pred_i <- model.matrix(~ PC1 + PC2 + age_group, data = UNI_pred_i)
  
  X_pred_LRTI_i <- X_pred_i[, names(coef(mod_bact_LRTI_i)), drop = FALSE]
  X_pred_fever_i <- X_pred_i[, names(coef(mod_bact_fever_i)), drop = FALSE]
  
  pred_LRTI_i <- plogis(X_pred_LRTI_i %*% t(beta_draws_bact_LRTI_i))
  pred_fever_i <- plogis(X_pred_fever_i %*% t(beta_draws_bact_fever_i))
  
  draw_ids_i <- mortality_draw_design %>%
    filter(.imp_pc == pc_imp_i) %>%
    arrange(.draw_within_pc) %>%
    pull(draw_id)
  
  matrix_prop_bact_LRTI_deaths_Age_0[, draw_ids_i] <- pred_LRTI_i[1:n_uni, , drop = FALSE]
  matrix_prop_bact_LRTI_deaths_Age_1[, draw_ids_i] <- pred_LRTI_i[(n_uni + 1):(2 * n_uni), , drop = FALSE]
  matrix_prop_bact_LRTI_deaths_Age_2to4[, draw_ids_i] <- pred_LRTI_i[(2 * n_uni + 1):(3 * n_uni), , drop = FALSE]
  
  matrix_prop_bact_fever_deaths_Age_0[, draw_ids_i] <- pred_fever_i[1:n_uni, , drop = FALSE]
  matrix_prop_bact_fever_deaths_Age_1[, draw_ids_i] <- pred_fever_i[(n_uni + 1):(2 * n_uni), , drop = FALSE]
  matrix_prop_bact_fever_deaths_Age_2to4[, draw_ids_i] <- pred_fever_i[(2 * n_uni + 1):(3 * n_uni), , drop = FALSE]
}

stopifnot(
  !anyNA(matrix_prop_bact_LRTI_deaths_Age_0),
  !anyNA(matrix_prop_bact_LRTI_deaths_Age_1),
  !anyNA(matrix_prop_bact_LRTI_deaths_Age_2to4),
  !anyNA(matrix_prop_bact_fever_deaths_Age_0),
  !anyNA(matrix_prop_bact_fever_deaths_Age_1),
  !anyNA(matrix_prop_bact_fever_deaths_Age_2to4)
)

saveRDS(matrix_prop_bact_LRTI_deaths_Age_0      ,     "outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_0.rds")
saveRDS(matrix_prop_bact_LRTI_deaths_Age_1      ,     "outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_1.rds")
saveRDS(matrix_prop_bact_LRTI_deaths_Age_2to4   ,     "outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_2to4.rds")
saveRDS(matrix_prop_bact_fever_deaths_Age_0     ,     "outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_0.rds")
saveRDS(matrix_prop_bact_fever_deaths_Age_1     ,     "outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_1.rds")
saveRDS(matrix_prop_bact_fever_deaths_Age_2to4  ,     "outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_2to4.rds")

saveRDS(
  list(LRTI = mod_bact_LRTI_list, fever = mod_bact_fever_list),
  "outputs/pipelines/champs_bacterial_death_fraction_models_5PC.rds"
)

UNI_row_check <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
row_2023 <- which(UNI_row_check$Year == 2023 & UNI_row_check$.imp_pc == 1)

summary_prop_bact_LRTI_deaths_Age_0     <- as.data.frame(t(apply(matrix_prop_bact_LRTI_deaths_Age_0     [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))
summary_prop_bact_LRTI_deaths_Age_1     <- as.data.frame(t(apply(matrix_prop_bact_LRTI_deaths_Age_1     [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))
summary_prop_bact_LRTI_deaths_Age_2to4  <- as.data.frame(t(apply(matrix_prop_bact_LRTI_deaths_Age_2to4  [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))
summary_prop_bact_fever_deaths_Age_0    <- as.data.frame(t(apply(matrix_prop_bact_fever_deaths_Age_0    [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))
summary_prop_bact_fever_deaths_Age_1    <- as.data.frame(t(apply(matrix_prop_bact_fever_deaths_Age_1    [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))
summary_prop_bact_fever_deaths_Age_2to4 <- as.data.frame(t(apply(matrix_prop_bact_fever_deaths_Age_2to4 [row_2023, ], 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)))

names(summary_prop_bact_LRTI_deaths_Age_0)     <- c("lower", "median", "upper")
names(summary_prop_bact_LRTI_deaths_Age_1)     <- c("lower", "median", "upper")
names(summary_prop_bact_LRTI_deaths_Age_2to4)  <- c("lower", "median", "upper")
names(summary_prop_bact_fever_deaths_Age_0)    <- c("lower", "median", "upper")
names(summary_prop_bact_fever_deaths_Age_1)    <- c("lower", "median", "upper")
names(summary_prop_bact_fever_deaths_Age_2to4) <- c("lower", "median", "upper")

summary_prop_bact_LRTI_deaths_Age_0$Age     <- "0-11 months"
summary_prop_bact_LRTI_deaths_Age_1$Age     <- "12-23 months"
summary_prop_bact_LRTI_deaths_Age_2to4$Age  <- "24-59 months"
summary_prop_bact_fever_deaths_Age_0$Age    <- "0-11 months"
summary_prop_bact_fever_deaths_Age_1$Age    <- "12-23 months"
summary_prop_bact_fever_deaths_Age_2to4$Age <- "24-59 months"

summary_prop_bact_LRTI_deaths_Age_0$outcome     <- "LRTI"
summary_prop_bact_LRTI_deaths_Age_1$outcome     <- "LRTI"
summary_prop_bact_LRTI_deaths_Age_2to4$outcome  <- "LRTI"
summary_prop_bact_fever_deaths_Age_0$outcome    <- "Fever"
summary_prop_bact_fever_deaths_Age_1$outcome    <- "Fever"
summary_prop_bact_fever_deaths_Age_2to4$outcome <- "Fever"

summary_prop_bact_deaths <- bind_rows(
  summary_prop_bact_LRTI_deaths_Age_0,
  summary_prop_bact_LRTI_deaths_Age_1,
  summary_prop_bact_LRTI_deaths_Age_2to4,
  summary_prop_bact_fever_deaths_Age_0,
  summary_prop_bact_fever_deaths_Age_1,
  summary_prop_bact_fever_deaths_Age_2to4
)

write.csv(summary_prop_bact_deaths, "outputs/pipelines/suppl_table/prop_bact_deaths.csv")


# Calculate bacterial LRTI and bacterial non-LRTI fever mortality rates ------

matrix_death_rate_q_year0    <- readRDS("outputs/pipelines/matrix_death_rate_year0.rds")
matrix_death_rate_q_year1    <- readRDS("outputs/pipelines/matrix_death_rate_year1.rds")
matrix_death_rate_q_year2to4 <- readRDS("outputs/pipelines/matrix_death_rate_year2to4.rds")

stopifnot(
  identical(dim(matrix_death_rate_q_year0), dim(matrix_prop_bact_LRTI_deaths_Age_0)),
  identical(dim(matrix_death_rate_q_year1), dim(matrix_prop_bact_LRTI_deaths_Age_1)),
  identical(dim(matrix_death_rate_q_year2to4), dim(matrix_prop_bact_LRTI_deaths_Age_2to4)),
  identical(colnames(matrix_death_rate_q_year0), colnames(matrix_prop_bact_LRTI_deaths_Age_0)),
  identical(rownames(matrix_death_rate_q_year0), rownames(matrix_prop_bact_LRTI_deaths_Age_0))
)

matrix_bact_death_rate_LRTI_Age_0     <- matrix_death_rate_q_year0    * matrix_prop_bact_LRTI_deaths_Age_0
matrix_bact_death_rate_fever_Age_0    <- matrix_death_rate_q_year0    * matrix_prop_bact_fever_deaths_Age_0
matrix_bact_death_rate_LRTI_Age_1     <- matrix_death_rate_q_year1    * matrix_prop_bact_LRTI_deaths_Age_1
matrix_bact_death_rate_fever_Age_1    <- matrix_death_rate_q_year1    * matrix_prop_bact_fever_deaths_Age_1
matrix_bact_death_rate_LRTI_Age_2to4  <- matrix_death_rate_q_year2to4 * matrix_prop_bact_LRTI_deaths_Age_2to4
matrix_bact_death_rate_fever_Age_2to4 <- matrix_death_rate_q_year2to4 * matrix_prop_bact_fever_deaths_Age_2to4

saveRDS(matrix_bact_death_rate_LRTI_Age_0,     "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_0.rds")
saveRDS(matrix_bact_death_rate_fever_Age_0,    "outputs/pipelines/matrix_bact_death_rate_fever_Age_0.rds")
saveRDS(matrix_bact_death_rate_LRTI_Age_1,     "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_1.rds")
saveRDS(matrix_bact_death_rate_fever_Age_1,    "outputs/pipelines/matrix_bact_death_rate_fever_Age_1.rds")
saveRDS(matrix_bact_death_rate_LRTI_Age_2to4,  "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_2to4.rds")
saveRDS(matrix_bact_death_rate_fever_Age_2to4, "outputs/pipelines/matrix_bact_death_rate_fever_Age_2to4.rds")


















# _____________________________________________ ----------
# 8. CHAMPS comparison: aggregate LRTI-to-fever death ratio under age 5 -------
rm(list = ls()); gc()

death_agg <- read.csv("outputs/pipelines/champs_table.csv")

death_agg_country_year <- death_agg %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::summarise(
    bact_fever_deaths = sum(bact_fever_deaths, na.rm = TRUE),
    bact_LRTI_deaths = sum(bact_LRTI_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Country, Year)

death_agg_country_year

death_agg_country_year <- death_agg %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::summarise(
    bact_fever_deaths = sum(bact_fever_deaths, na.rm = TRUE),
    bact_LRTI_deaths = sum(bact_LRTI_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    LRTI_to_fever_ratio = dplyr::if_else(
      bact_fever_deaths > 0,
      bact_LRTI_deaths / bact_fever_deaths,
      NA_real_
    )
  ) %>%
  dplyr::arrange(Country, Year)

death_agg_country_year






# Load age-specific death matrices --------------------------------------------
matrix_prop_bact_LRTI_deaths_Age_0     <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_0.rds"    )
matrix_prop_bact_LRTI_deaths_Age_1     <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_1.rds"    )
matrix_prop_bact_LRTI_deaths_Age_2to4  <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_LRTI_deaths_Age_2to4.rds" )
matrix_prop_bact_fever_deaths_Age_0    <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_0.rds"   )
matrix_prop_bact_fever_deaths_Age_1    <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_1.rds"   )
matrix_prop_bact_fever_deaths_Age_2to4 <- readRDS("outputs/pipelines/matrix_matrix_prop_bact_fever_deaths_Age_2to4.rds")


# Identify the 2023 rows -------------------------------------------------------

UNI_model <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")
UNI_all <- UNI_model %>%
  dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
  distinct()
row_2023 <- which(UNI_all$Year == 2023)
country_2023 <- as.character(UNI_all$Country[row_2023])



# Prepare 2023 age-specific population ----------------------------------------

population_by_age <- read.csv("data/metadata/Population_by_age.csv")

country_mapping <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "United Republic of Tanzania" = "Tanzania",
  "Türkiye" = "Turkiye",
  "T\x9frkiye" = "Turkiye"
)
population_by_age <- population_by_age %>%
  dplyr::mutate(
    Country = ifelse(
      Country %in% names(country_mapping),
      unname(country_mapping[Country]),
      Country
    )
  )

population_2023 <- population_by_age %>%
  dplyr::filter(Year == 2023) %>%
  dplyr::mutate(
    Age_2to4 = Age_2 + Age_3 + Age_4,
    Population_under5 = Age_0 + Age_1 + Age_2 + Age_3 + Age_4
  ) %>%
  dplyr::select(Country, Age_0, Age_1, Age_2to4, Population_under5)


# Align population with the matrix row order ----------------------------------

population_2023_aligned <- data.frame(Country = country_2023) %>%
  dplyr::left_join(population_2023, by = "Country")


# Extract 2023 matrices --------------------------------------------------------

matrix_prop_bact_LRTI_deaths_Age_0_2023    <- matrix_prop_bact_LRTI_deaths_Age_0[row_2023, , drop = FALSE]
matrix_prop_bact_LRTI_deaths_Age_1_2023    <- matrix_prop_bact_LRTI_deaths_Age_1[row_2023, , drop = FALSE]
matrix_prop_bact_LRTI_deaths_Age_2to4_2023 <- matrix_prop_bact_LRTI_deaths_Age_2to4[row_2023, , drop = FALSE]

matrix_prop_bact_fever_deaths_Age_0_2023    <- matrix_prop_bact_fever_deaths_Age_0[row_2023, , drop = FALSE]
matrix_prop_bact_fever_deaths_Age_1_2023    <- matrix_prop_bact_fever_deaths_Age_1[row_2023, , drop = FALSE]
matrix_prop_bact_fever_deaths_Age_2to4_2023 <- matrix_prop_bact_fever_deaths_Age_2to4[row_2023, , drop = FALSE]

# Population vectors ----------------------------------------------------------

population_Age_0_2023    <- population_2023_aligned$Age_0
population_Age_1_2023    <- population_2023_aligned$Age_1
population_Age_2to4_2023 <- population_2023_aligned$Age_2to4
population_Age_all_2023  <- population_2023_aligned$Population_under5

pop_Age_0_prop    <- population_Age_0_2023    / population_Age_all_2023 
pop_Age_1_prop    <- population_Age_1_2023    / population_Age_all_2023 
pop_Age_2to4_prop <- population_Age_2to4_2023 / population_Age_all_2023 


# Population-weighted age-specific LRTI proportions ---------------------------

matrix_prop_bact_LRTI_deaths_Age_0_2023_weighted <- sweep(
  matrix_prop_bact_LRTI_deaths_Age_0_2023,
  MARGIN = 1,
  STATS = pop_Age_0_prop,
  FUN = "*"
)

matrix_prop_bact_LRTI_deaths_Age_1_2023_weighted <- sweep(
  matrix_prop_bact_LRTI_deaths_Age_1_2023,
  MARGIN = 1,
  STATS = pop_Age_1_prop,
  FUN = "*"
)

matrix_prop_bact_LRTI_deaths_Age_2to4_2023_weighted <- sweep(
  matrix_prop_bact_LRTI_deaths_Age_2to4_2023,
  MARGIN = 1,
  STATS = pop_Age_2to4_prop,
  FUN = "*"
)


# Population-weighted age-specific fever proportions --------------------------

matrix_prop_bact_fever_deaths_Age_0_2023_weighted <- sweep(
  matrix_prop_bact_fever_deaths_Age_0_2023,
  MARGIN = 1,
  STATS = pop_Age_0_prop,
  FUN = "*"
)

matrix_prop_bact_fever_deaths_Age_1_2023_weighted <- sweep(
  matrix_prop_bact_fever_deaths_Age_1_2023,
  MARGIN = 1,
  STATS = pop_Age_1_prop,
  FUN = "*"
)

matrix_prop_bact_fever_deaths_Age_2to4_2023_weighted <- sweep(
  matrix_prop_bact_fever_deaths_Age_2to4_2023,
  MARGIN = 1,
  STATS = pop_Age_2to4_prop,
  FUN = "*"
)


# Combine age groups to obtain under-5 proportions ----------------------------

matrix_prop_bact_LRTI_deaths_Age_under5_2023 <-
  matrix_prop_bact_LRTI_deaths_Age_0_2023_weighted +
  matrix_prop_bact_LRTI_deaths_Age_1_2023_weighted +
  matrix_prop_bact_LRTI_deaths_Age_2to4_2023_weighted

matrix_prop_bact_fever_deaths_Age_under5_2023 <-
  matrix_prop_bact_fever_deaths_Age_0_2023_weighted +
  matrix_prop_bact_fever_deaths_Age_1_2023_weighted +
  matrix_prop_bact_fever_deaths_Age_2to4_2023_weighted


# Calculate the under-5 LRTI-to-fever ratio for each draw ----------------------

matrix_prop_bact_ratio_deaths_Age_under5_2023 <-
  matrix_prop_bact_LRTI_deaths_Age_under5_2023 /
  matrix_prop_bact_fever_deaths_Age_under5_2023

matrix_prop_bact_ratio_deaths_Age_under5_2023[
  !is.finite(matrix_prop_bact_ratio_deaths_Age_under5_2023)
] <- NA_real_


# Check dimensions -------------------------------------------------------------

dim(matrix_prop_bact_LRTI_deaths_Age_under5_2023)
dim(matrix_prop_bact_fever_deaths_Age_under5_2023)
dim(matrix_prop_bact_ratio_deaths_Age_under5_2023)



# Summarize country-level under-5 LRTI deaths ----------------------------------

summary_bact_LRTI_deaths_Age_under5_2023 <- data.frame(
  Country = country_2023,
  median = apply(matrix_prop_bact_LRTI_deaths_Age_under5_2023, 1, quantile, probs = 0.500, na.rm = TRUE, names = FALSE),
  lower = apply(matrix_prop_bact_LRTI_deaths_Age_under5_2023, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(matrix_prop_bact_LRTI_deaths_Age_under5_2023, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)


# Summarize country-level under-5 fever deaths ---------------------------------

summary_bact_fever_deaths_Age_under5_2023 <- data.frame(
  Country = country_2023,
  median = apply(matrix_prop_bact_fever_deaths_Age_under5_2023, 1, quantile, probs = 0.500, na.rm = TRUE, names = FALSE),
  lower = apply(matrix_prop_bact_fever_deaths_Age_under5_2023, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(matrix_prop_bact_fever_deaths_Age_under5_2023, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)


# Summarize country-level under-5 LRTI-to-fever ratios -------------------------

summary_bact_ratio_deaths_Age_under5_2023 <- data.frame(
  Country = country_2023,
  median = apply(matrix_prop_bact_ratio_deaths_Age_under5_2023, 1, quantile, probs = 0.500, na.rm = TRUE, names = FALSE),
  lower = apply(matrix_prop_bact_ratio_deaths_Age_under5_2023, 1, quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
  upper = apply(matrix_prop_bact_ratio_deaths_Age_under5_2023, 1, quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
)



# Aggregate across all 2023 countries -----------------------------------------

global_bact_LRTI_deaths_2023_draw <- colSums(
  matrix_prop_bact_LRTI_deaths_Age_under5_2023,
  na.rm = TRUE
)

global_bact_fever_deaths_2023_draw <- colSums(
  matrix_prop_bact_fever_deaths_Age_under5_2023,
  na.rm = TRUE
)

global_bact_ratio_deaths_2023_draw <-
  global_bact_LRTI_deaths_2023_draw /
  global_bact_fever_deaths_2023_draw

global_bact_ratio_deaths_2023_draw[
  !is.finite(global_bact_ratio_deaths_2023_draw)
] <- NA_real_


# Summarize global 2023 deaths and ratio ---------------------------------------

summary_global_bact_deaths_2023 <- data.frame(
  outcome = c(
    "Bacterial LRTI deaths",
    "Bacterial fever deaths",
    "LRTI-to-fever death ratio"
  ),
  median = c(
    median(global_bact_LRTI_deaths_2023_draw, na.rm = TRUE),
    median(global_bact_fever_deaths_2023_draw, na.rm = TRUE),
    median(global_bact_ratio_deaths_2023_draw, na.rm = TRUE)
  ),
  lower = c(
    quantile(global_bact_LRTI_deaths_2023_draw, 0.025, na.rm = TRUE, names = FALSE),
    quantile(global_bact_fever_deaths_2023_draw, 0.025, na.rm = TRUE, names = FALSE),
    quantile(global_bact_ratio_deaths_2023_draw, 0.025, na.rm = TRUE, names = FALSE)
  ),
  upper = c(
    quantile(global_bact_LRTI_deaths_2023_draw, 0.975, na.rm = TRUE, names = FALSE),
    quantile(global_bact_fever_deaths_2023_draw, 0.975, na.rm = TRUE, names = FALSE),
    quantile(global_bact_ratio_deaths_2023_draw, 0.975, na.rm = TRUE, names = FALSE)
  )
)

summary_global_bact_deaths_2023


# setdiff(unique(),unique(summary_bact_ratio_deaths_Age_under5_2023$Country))
comparison <- death_agg_country_year %>% inner_join(summary_bact_ratio_deaths_Age_under5_2023, by = "Country")
plot(comparison$LRTI_to_fever_ratio, comparison$median, log = "xy")

colnames(comparison) <- c("Country", "Year", "Bacterial_Fever", "Bacterial_LRTI", "Observed_Ratio", "Estimated_ratio", "Lower", "Upper")
sum(death_agg$bact_LRTI_deaths)/sum(death_agg$bact_fever_deaths)
summary_global_bact_deaths_2023[3,]
comparison







# _____________________________________________ ----------
# 8. Comparison with IHME: children under 5 -----------------------------------


# 1. Import and prepare IHME and UNI data --------------------------------------

IHME.data <- read.csv("data/comparison/IHME_2019_DATA/IHME_AMR_PATHOGEN_2019_DATA_Y2022M11D21.csv")
UNI_model <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")
UNI_all <- UNI_model %>%
  dplyr::select(Country, Year, GDP, Income, gbd_region, Endemic, Population) %>%
  distinct()

row_2019 <- which(UNI_all$Year == 2019)
country_2019 <- as.character(UNI_all$Country[row_2019])

UNI_2019 <- UNI_all %>%
  dplyr::filter(Year == 2019) %>%
  dplyr::select(Country, GDP, Income, gbd_region, Endemic, Population)

country_mapping <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Congo" = "Congo, Rep.",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "C\x99te d'Ivoire" = "Cote d'Ivoire",
  "Egypt" = "Egypt, Arab Rep.",
  "Gambia" = "Gambia, The",
  "Iran (Islamic Republic of)" = "Iran, Islamic Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Republic of Moldova" = "Moldova",
  "United Republic of Tanzania" = "Tanzania",
  "Türkiye" = "Turkiye",
  "T\x9frkiye" = "Turkiye"
)

IHME.data <- IHME.data %>%
  dplyr::filter(
    age_group_name %in% c("Neonatal", "Post Neonatal", "1 to 4"),
    measure_name == "Deaths",
    infectious_syndrome %in% c(
      "All infectious syndromes",
      "Bloodstream infections",
      "Lower respiratory infections and all related infections in the thorax"
    )
  ) %>%
  dplyr::mutate(
    location_name = ifelse(
      location_name %in% names(country_mapping),
      unname(country_mapping[location_name]),
      location_name
    )
  ) %>%
  dplyr::transmute(
    Country = location_name,
    age_group = age_group_name,
    infectious_syndrome,
    pathogen,
    val,
    lower,
    upper
  )

IHME_country_unmatched <- setdiff(UNI_2019$Country, IHME.data$Country)
IHME_country_unmatched

IHME.data <- UNI_2019 %>%
  dplyr::inner_join(IHME.data, by = "Country")


# 2. Fit gamma distributions to IHME estimates --------------------------------

estimate <- function(val, lower, upper) {
  if (any(!is.finite(c(val, lower, upper))) || val <= 0 || lower <= 0 || upper <= 0 || lower >= upper) {
    return(c(
      shape = NA_real_,
      scale = NA_real_,
      error_val = NA_real_,
      error_lower = NA_real_,
      error_upper = NA_real_
    ))
  }
  
  obj_fun <- function(par) {
    shape <- par[1]
    scale <- par[2]
    
    fitted_lower <- qgamma(0.025, shape = shape, scale = scale)
    fitted_upper <- qgamma(0.975, shape = shape, scale = scale)
    fitted_mean <- shape * scale
    
    (fitted_lower - lower)^2 + (fitted_upper - upper)^2 + (fitted_mean - val)^2
  }
  
  sd_est <- (upper - lower) / 4
  shape_init <- (val / sd_est)^2
  scale_init <- val / shape_init
  
  fit <- tryCatch(
    optim(
      par = c(shape_init, scale_init),
      fn = obj_fun,
      method = "L-BFGS-B",
      lower = c(1e-8, 1e-8)
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit) || fit$convergence != 0 || any(!is.finite(fit$par))) {
    return(c(
      shape = NA_real_,
      scale = NA_real_,
      error_val = NA_real_,
      error_lower = NA_real_,
      error_upper = NA_real_
    ))
  }
  
  shape <- fit$par[1]
  scale <- fit$par[2]
  
  fitted_mean <- shape * scale
  fitted_lower <- qgamma(0.025, shape = shape, scale = scale)
  fitted_upper <- qgamma(0.975, shape = shape, scale = scale)
  
  c(
    shape = shape,
    scale = scale,
    error_val = abs(fitted_mean - val) / val,
    error_lower = abs(fitted_lower - lower) / lower,
    error_upper = abs(fitted_upper - upper) / upper
  )
}

IHME.data <- IHME.data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gamma_est = list(estimate(val = val, lower = lower, upper = upper))) %>%
  dplyr::ungroup() %>%
  tidyr::unnest_wider(gamma_est)

IHME_gamma_fit_summary <- IHME.data %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_failed = sum(is.na(shape) | is.na(scale)),
    median_error_val = median(error_val, na.rm = TRUE),
    median_error_lower = median(error_lower, na.rm = TRUE),
    median_error_upper = median(error_upper, na.rm = TRUE),
    max_error_val = max(error_val, na.rm = TRUE),
    max_error_lower = max(error_lower, na.rm = TRUE),
    max_error_upper = max(error_upper, na.rm = TRUE)
  )

IHME_gamma_fit_summary

IHME.data <- IHME.data %>%
  dplyr::select(
    Country,
    GDP,
    Income,
    gbd_region,
    Endemic,
    Population,
    age_group,
    infectious_syndrome,
    pathogen,
    shape,
    scale
  )


# 3. Generate 1,000 IHME uncertainty draws ------------------------------------

n_sim <- 1000
set.seed(12345)

valid_i <- is.finite(IHME.data$shape) &
  is.finite(IHME.data$scale) &
  IHME.data$shape > 0 &
  IHME.data$scale > 0

IHME_sim_matrix <- matrix(NA_real_, nrow = nrow(IHME.data), ncol = n_sim)

IHME_sim_matrix[valid_i, ] <- matrix(
  rgamma(
    n = sum(valid_i) * n_sim,
    shape = rep(IHME.data$shape[valid_i], times = n_sim),
    scale = rep(IHME.data$scale[valid_i], times = n_sim)
  ),
  nrow = sum(valid_i),
  ncol = n_sim,
  byrow = FALSE
)

colnames(IHME_sim_matrix) <- paste0("draw_", seq_len(n_sim))

IHME.data <- dplyr::bind_cols(IHME.data, as.data.frame(IHME_sim_matrix))
draw_cols <- grep("^draw_[0-9]+$", names(IHME.data), value = TRUE)

if (length(draw_cols) != n_sim) {
  stop("The expected number of IHME draw columns was not found.")
}


# 4. Prepare 2019 age-specific population -------------------------------------

population.data <- read.csv("data/metadata/Population_by_age.csv")

population.data <- population.data %>%
  dplyr::mutate(
    Country = ifelse(
      Country %in% names(country_mapping),
      unname(country_mapping[Country]),
      Country
    )
  )

population_2019 <- population.data %>%
  dplyr::filter(Year == 2019) %>%
  dplyr::mutate(
    Age_2to4 = Age_2 + Age_3 + Age_4,
    Population_under5 = Age_0 + Age_1 + Age_2 + Age_3 + Age_4
  ) %>%
  dplyr::select(Country, Age_0, Age_1, Age_2to4, Population_under5)

if (anyDuplicated(population_2019$Country) > 0) {
  stop("population_2019 contains duplicate countries.")
}

population_country_unmatched <- setdiff(country_2019, population_2019$Country)

if (length(population_country_unmatched) > 0) {
  print(population_country_unmatched)
  stop("Some 2019 countries do not have population data.")
}


# 5. IHME: merge all pathogens and ages under 5 -------------------------------

IHME.data_overall_pathogen_under5_count <- IHME.data %>%
  dplyr::group_by(Country, GDP, Income, gbd_region, Endemic, infectious_syndrome) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(draw_cols),
      ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

IHME.data_overall_pathogen_under5_rate <- IHME.data_overall_pathogen_under5_count %>%
  dplyr::left_join(
    population_2019 %>% dplyr::select(Country, Population_under5),
    by = "Country"
  )

IHME_population_unmatched <- IHME.data_overall_pathogen_under5_rate %>%
  dplyr::filter(!is.finite(Population_under5) | Population_under5 <= 0) %>%
  dplyr::distinct(Country, Population_under5)

if (nrow(IHME_population_unmatched) > 0) {
  print(IHME_population_unmatched)
  stop("Some IHME countries do not have a valid under-5 population.")
}

IHME.data_overall_pathogen_under5_rate <- IHME.data_overall_pathogen_under5_rate %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(draw_cols), ~ .x / Population_under5 * 1000),
    Population = Population_under5,
    age_group = "Age_under5"
  ) %>%
  dplyr::select(
    Country,
    GDP,
    Income,
    gbd_region,
    Endemic,
    Population,
    age_group,
    infectious_syndrome,
    dplyr::all_of(draw_cols)
  ) %>%
  dplyr::arrange(Country, infectious_syndrome)


# 6. Summarize IHME under-5 draws ---------------------------------------------

IHME_under5_draw_matrix <- as.matrix(IHME.data_overall_pathogen_under5_rate[, draw_cols])
IHME_under5_valid_rows <- rowSums(is.finite(IHME_under5_draw_matrix)) > 0

IHME_under5_val <- rep(NA_real_, nrow(IHME_under5_draw_matrix))
IHME_under5_lower <- rep(NA_real_, nrow(IHME_under5_draw_matrix))
IHME_under5_upper <- rep(NA_real_, nrow(IHME_under5_draw_matrix))

IHME_under5_val[IHME_under5_valid_rows] <- rowMeans(
  IHME_under5_draw_matrix[IHME_under5_valid_rows, , drop = FALSE],
  na.rm = TRUE
)

IHME_under5_lower[IHME_under5_valid_rows] <- apply(
  IHME_under5_draw_matrix[IHME_under5_valid_rows, , drop = FALSE],
  1,
  quantile,
  probs = 0.025,
  na.rm = TRUE,
  names = FALSE
)

IHME_under5_upper[IHME_under5_valid_rows] <- apply(
  IHME_under5_draw_matrix[IHME_under5_valid_rows, , drop = FALSE],
  1,
  quantile,
  probs = 0.975,
  na.rm = TRUE,
  names = FALSE
)

IHME.data_overall_pathogen_under5_summary <- IHME.data_overall_pathogen_under5_rate %>%
  dplyr::select(-dplyr::all_of(draw_cols)) %>%
  dplyr::mutate(
    val = IHME_under5_val,
    lower = IHME_under5_lower,
    upper = IHME_under5_upper
  ) %>%
  dplyr::select(
    Country,
    GDP,
    Income,
    gbd_region,
    Endemic,
    Population,
    age_group,
    infectious_syndrome,
    val,
    lower,
    upper
  ) %>%
  dplyr::arrange(Country, infectious_syndrome)

IHME.data_rate_overall_Age_under5 <- IHME.data_overall_pathogen_under5_summary %>%
  dplyr::filter(infectious_syndrome == "All infectious syndromes")

IHME.data_rate_LRTI_Age_under5 <- IHME.data_overall_pathogen_under5_summary %>%
  dplyr::filter(
    infectious_syndrome ==
      "Lower respiratory infections and all related infections in the thorax"
  )

IHME.data_rate_BSI_Age_under5 <- IHME.data_overall_pathogen_under5_summary %>%
  dplyr::filter(infectious_syndrome == "Bloodstream infections")


# 7. Load model-based age-specific mortality-rate matrices --------------------

matrix_bact_death_rate_LRTI_Age_0 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_0.rds"
)[row_2019, , drop = FALSE]

matrix_bact_death_rate_fever_Age_0 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_fever_Age_0.rds"
)[row_2019, , drop = FALSE]

matrix_bact_death_rate_LRTI_Age_1 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_1.rds"
)[row_2019, , drop = FALSE]

matrix_bact_death_rate_fever_Age_1 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_fever_Age_1.rds"
)[row_2019, , drop = FALSE]

matrix_bact_death_rate_LRTI_Age_2to4 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_LRTI_Age_2to4.rds"
)[row_2019, , drop = FALSE]

matrix_bact_death_rate_fever_Age_2to4 <- readRDS(
  "outputs/pipelines/matrix_bact_death_rate_fever_Age_2to4.rds"
)[row_2019, , drop = FALSE]


# 8. Align under-5 population with matrix rows ---------------------------------

UNI_pop_under5_aligned <- data.frame(Country = country_2019) %>%
  dplyr::left_join(population_2019, by = "Country")

population_unmatched <- UNI_pop_under5_aligned %>%
  dplyr::filter(
    !is.finite(Age_0) |
      !is.finite(Age_1) |
      !is.finite(Age_2to4) |
      !is.finite(Population_under5) |
      Population_under5 <= 0
  )

if (nrow(population_unmatched) > 0) {
  print(population_unmatched)
  stop("Some model countries do not have valid age-specific population data.")
}

stopifnot(
  identical(as.character(UNI_pop_under5_aligned$Country), country_2019),
  identical(dim(matrix_bact_death_rate_LRTI_Age_0), dim(matrix_bact_death_rate_LRTI_Age_1)),
  identical(dim(matrix_bact_death_rate_LRTI_Age_0), dim(matrix_bact_death_rate_LRTI_Age_2to4)),
  identical(dim(matrix_bact_death_rate_fever_Age_0), dim(matrix_bact_death_rate_fever_Age_1)),
  identical(dim(matrix_bact_death_rate_fever_Age_0), dim(matrix_bact_death_rate_fever_Age_2to4)),
  nrow(matrix_bact_death_rate_LRTI_Age_0) == nrow(UNI_pop_under5_aligned),
  nrow(matrix_bact_death_rate_fever_Age_0) == nrow(UNI_pop_under5_aligned)
)

population_Age_0 <- UNI_pop_under5_aligned$Age_0
population_Age_1 <- UNI_pop_under5_aligned$Age_1
population_Age_2to4 <- UNI_pop_under5_aligned$Age_2to4
population_under5 <- UNI_pop_under5_aligned$Population_under5


# 9. Combine model LRTI mortality rates for all ages under 5 ------------------

matrix_bact_death_rate_LRTI_Age_0_weighted <- sweep(
  matrix_bact_death_rate_LRTI_Age_0,
  MARGIN = 1,
  STATS = population_Age_0,
  FUN = "*"
)

matrix_bact_death_rate_LRTI_Age_1_weighted <- sweep(
  matrix_bact_death_rate_LRTI_Age_1,
  MARGIN = 1,
  STATS = population_Age_1,
  FUN = "*"
)

matrix_bact_death_rate_LRTI_Age_2to4_weighted <- sweep(
  matrix_bact_death_rate_LRTI_Age_2to4,
  MARGIN = 1,
  STATS = population_Age_2to4,
  FUN = "*"
)

matrix_bact_death_rate_LRTI_Age_under5 <- sweep(
  matrix_bact_death_rate_LRTI_Age_0_weighted +
    matrix_bact_death_rate_LRTI_Age_1_weighted +
    matrix_bact_death_rate_LRTI_Age_2to4_weighted,
  MARGIN = 1,
  STATS = population_under5,
  FUN = "/"
)


# 10. Combine model fever mortality rates for all ages under 5 ----------------

matrix_bact_death_rate_fever_Age_0_weighted <- sweep(
  matrix_bact_death_rate_fever_Age_0,
  MARGIN = 1,
  STATS = population_Age_0,
  FUN = "*"
)

matrix_bact_death_rate_fever_Age_1_weighted <- sweep(
  matrix_bact_death_rate_fever_Age_1,
  MARGIN = 1,
  STATS = population_Age_1,
  FUN = "*"
)

matrix_bact_death_rate_fever_Age_2to4_weighted <- sweep(
  matrix_bact_death_rate_fever_Age_2to4,
  MARGIN = 1,
  STATS = population_Age_2to4,
  FUN = "*"
)

matrix_bact_death_rate_fever_Age_under5 <- sweep(
  matrix_bact_death_rate_fever_Age_0_weighted +
    matrix_bact_death_rate_fever_Age_1_weighted +
    matrix_bact_death_rate_fever_Age_2to4_weighted,
  MARGIN = 1,
  STATS = population_under5,
  FUN = "/"
)


# 11. Summarize model under-5 draws -------------------------------------------

summary_bact_death_rate_LRTI_Age_under5 <- data.frame(
  Country = country_2019,
  val = rowMeans(matrix_bact_death_rate_LRTI_Age_under5, na.rm = TRUE) * 1000,
  lower = apply(
    matrix_bact_death_rate_LRTI_Age_under5,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ) * 1000,
  upper = apply(
    matrix_bact_death_rate_LRTI_Age_under5,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  ) * 1000
)

summary_bact_death_rate_fever_Age_under5 <- data.frame(
  Country = country_2019,
  val = rowMeans(matrix_bact_death_rate_fever_Age_under5, na.rm = TRUE) * 1000,
  lower = apply(
    matrix_bact_death_rate_fever_Age_under5,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE
  ) * 1000,
  upper = apply(
    matrix_bact_death_rate_fever_Age_under5,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE
  ) * 1000
)


# 12. Align datasets for comparison plots -------------------------------------

comparison_LRTI_under5 <- IHME.data_rate_LRTI_Age_under5 %>%
  dplyr::select(Country, IHME = val) %>%
  dplyr::inner_join(
    summary_bact_death_rate_LRTI_Age_under5 %>%
      dplyr::select(Country, Estimated = val),
    by = "Country"
  ) %>%
  dplyr::filter(
    is.finite(IHME),
    is.finite(Estimated),
    IHME > 0,
    Estimated > 0
  )

comparison_BSI_under5 <- IHME.data_rate_BSI_Age_under5 %>%
  dplyr::select(Country, IHME = val) %>%
  dplyr::inner_join(
    summary_bact_death_rate_fever_Age_under5 %>%
      dplyr::select(Country, Estimated = val),
    by = "Country"
  ) %>%
  dplyr::filter(
    is.finite(IHME),
    is.finite(Estimated),
    IHME > 0,
    Estimated > 0
  )

comparison_overall_under5 <- IHME.data_rate_overall_Age_under5 %>%
  dplyr::select(Country, IHME = val) %>%
  dplyr::inner_join(
    summary_bact_death_rate_fever_Age_under5 %>%
      dplyr::select(Country, Estimated = val),
    by = "Country"
  ) %>%
  dplyr::filter(
    is.finite(IHME),
    is.finite(Estimated),
    IHME > 0,
    Estimated > 0
  )


# 13. Basic log-scale comparison plots ----------------------------------------

old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 3), mar = c(5, 5, 3, 1))

plot(
  comparison_LRTI_under5$IHME,
  comparison_LRTI_under5$Estimated,
  log = "xy",
  xlab = "IHME bacterial LRTI mortality rate\nper 1,000 children (log scale) in 2019",
  ylab = "Estimated bacterial LRTI mortality rate\nper 1,000 children (log scale) in 2019",
  main = "Bacterial LRTI"
)

plot(
  comparison_BSI_under5$IHME,
  comparison_BSI_under5$Estimated,
  log = "xy",
  xlab = "IHME bacterial BSI mortality rate\nper 1,000 children (log scale) in 2019",
  ylab = "Estimated bacterial fever mortality rate\nper 1,000 children (log scale) in 2019",
  main = "BSI and fever"
)

plot(
  comparison_overall_under5$IHME,
  comparison_overall_under5$Estimated,
  log = "xy",
  xlab = "IHME bacterial all-syndrome mortality rate\nper 1,000 children (log scale) in 2019",
  ylab = "Estimated bacterial fever mortality rate\nper 1,000 children (log scale) in 2019",
  main = "All syndromes and fever"
)

par(old_par)


# 14. Remove temporary objects ------------------------------------------------

rm(
  IHME_sim_matrix,
  IHME_under5_draw_matrix,
  IHME_under5_valid_rows,
  IHME_under5_val,
  IHME_under5_lower,
  IHME_under5_upper,
  matrix_bact_death_rate_LRTI_Age_0_weighted,
  matrix_bact_death_rate_LRTI_Age_1_weighted,
  matrix_bact_death_rate_LRTI_Age_2to4_weighted,
  matrix_bact_death_rate_fever_Age_0_weighted,
  matrix_bact_death_rate_fever_Age_1_weighted,
  matrix_bact_death_rate_fever_Age_2to4_weighted
)




# _____________________________________________ ----------
# 9. IHME comparison plot. IHME versus estimated mortality rates, children under 5 ---------------------


# Add income group to each comparison dataset ---------------------------------

comparison_LRTI_under5 <- comparison_LRTI_under5 %>%
  dplyr::select(-dplyr::any_of("Income")) %>%
  dplyr::left_join(UNI_2019 %>% dplyr::select(Country, Income), by = "Country") %>%
  dplyr::filter(is.finite(IHME), is.finite(Estimated), IHME > 0, Estimated > 0)

comparison_BSI_under5 <- comparison_BSI_under5 %>%
  dplyr::select(-dplyr::any_of("Income")) %>%
  dplyr::left_join(UNI_2019 %>% dplyr::select(Country, Income), by = "Country") %>%
  dplyr::filter(is.finite(IHME), is.finite(Estimated), IHME > 0, Estimated > 0)

comparison_overall_under5 <- comparison_overall_under5 %>%
  dplyr::select(-dplyr::any_of("Income")) %>%
  dplyr::left_join(UNI_2019 %>% dplyr::select(Country, Income), by = "Country") %>%
  dplyr::filter(is.finite(IHME), is.finite(Estimated), IHME > 0, Estimated > 0)


# Fixed income-group colors ----------------------------------------------------

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

col_LRTI <- income_cols_fixed[as.character(comparison_LRTI_under5$Income)]
col_BSI <- income_cols_fixed[as.character(comparison_BSI_under5$Income)]
col_overall <- income_cols_fixed[as.character(comparison_overall_under5$Income)]

col_LRTI[is.na(col_LRTI)] <- "grey50"
col_BSI[is.na(col_BSI)] <- "grey50"
col_overall[is.na(col_overall)] <- "grey50"


# Correlation on log10 scale ---------------------------------------------------

cor_LRTI <- cor(log10(comparison_LRTI_under5$IHME), log10(comparison_LRTI_under5$Estimated), use = "complete.obs", method = "pearson")
cor_BSI <- cor(log10(comparison_BSI_under5$IHME), log10(comparison_BSI_under5$Estimated), use = "complete.obs", method = "pearson")
cor_overall <- cor(log10(comparison_overall_under5$IHME), log10(comparison_overall_under5$Estimated), use = "complete.obs", method = "pearson")


# Common logarithmic axis range ------------------------------------------------

all_values <- c(
  comparison_LRTI_under5$IHME,
  comparison_BSI_under5$IHME,
  comparison_overall_under5$IHME,
  comparison_LRTI_under5$Estimated,
  comparison_BSI_under5$Estimated,
  comparison_overall_under5$Estimated
)

all_values <- all_values[is.finite(all_values) & all_values > 0]
range_all_values <- range(all_values)
range_all_values <- c(0.005, 15)
plot_lim_log <- log10(range_all_values)
axis_ticks <- seq(plot_lim_log[1], plot_lim_log[2], length.out = 5)
axis_labels <- parse(text = paste0(format(10^(axis_ticks), trim = TRUE, scientific = FALSE)))


# Text position for correlation ------------------------------------------------

corr_x <- plot_lim_log[1] + 0.06 * diff(plot_lim_log)
corr_y <- plot_lim_log[2] - 0.16 * diff(plot_lim_log)


# Plot settings ----------------------------------------------------------------

cex_axis_value <- 0.50
cex_axis_title <- 0.50
cex_title <- 0.50
cex_point <- 0.55
cex_legend <- 0.50
cex_legend_title <- 0.52
cex_corr <- 0.55

dir.create("figures/main_analysis/mortality", recursive = TRUE, showWarnings = FALSE)

pdf(
  "figures/main_analysis/mortality/IHME_vs_estimated_bacterial_mortality_under5.pdf",
  width = 6,
  height = 1.75,
  useDingbats = FALSE
)

layout(
  matrix(c(1, 2, 3, 4), nrow = 1),
  widths = c(1, 1, 1, 0.2)
)

par(
  oma = c(1.8, 2.0, 0.1, 0.1),
  mar = c(1.5, 1.5, 1, 1),
  mgp = c(0.35, 0.35, 0),
  bty = "l",
  tcl = -0.20,
  family = "sans"
)


# Panel 1: bacterial LRTI ------------------------------------------------------

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = plot_lim_log,
  ylim = plot_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = axis_ticks,
  labels = axis_labels,
  cex.axis = cex_axis_value,
  tck = -0.025,
  padj = 0.3,
  mgp = c(0.35, -0.15, 0)
)

axis(
  2,
  at = axis_ticks,
  labels = axis_labels,
  las = 1,
  cex.axis = cex_axis_value,
  tck = -0.025
)

points(
  log10(comparison_LRTI_under5$IHME),
  log10(comparison_LRTI_under5$Estimated),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(col_LRTI, alpha.f = 0.80)
)

text(
  x = corr_x,
  y = corr_y,
  labels = paste0("Pearson correlation = ", formatC(cor_LRTI, digits = 2, format = "f")),
  adj = c(0, 1),
  cex = cex_corr
)

mtext("Bacterial LRTI", side = 3, line = -0.5, cex = cex_title, font = 2)
mtext(
  "IHME mortality rate per\n1,000 children in 2019 (log scale)",
  side = 1,
  line = 2.0,
  cex = cex_axis_title
)

mtext(
  "Estimated mortality rate per\n1,000 children in 2019 (log scale)",
  side = 2,
  line = 2.0,
  cex = cex_axis_title
)


# Panel 2: BSI versus bacterial fever -----------------------------------------

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = plot_lim_log,
  ylim = plot_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = axis_ticks,
  labels = axis_labels,
  cex.axis = cex_axis_value,
  tck = -0.025,
  padj = 0.3,
  mgp = c(0.35, -0.15, 0)
)

axis(
  2,
  at = axis_ticks,
  labels = axis_labels,
  las = 1,
  cex.axis = cex_axis_value,
  tck = -0.025
)

points(
  log10(comparison_BSI_under5$IHME),
  log10(comparison_BSI_under5$Estimated),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(col_BSI, alpha.f = 0.80)
)

text(
  x = corr_x,
  y = corr_y,
  labels = paste0("Pearson correlation = ", formatC(cor_BSI, digits = 2, format = "f")),
  adj = c(0, 1),
  cex = cex_corr
)

mtext("BSI vs. bacterial fever", side = 3, line = -0.5, cex = cex_title, font = 2)
mtext(
  "IHME mortality rate per\n1,000 children in 2019 (log scale)",
  side = 1,
  line = 2.0,
  cex = cex_axis_title
)


# Panel 3: all infectious syndromes versus bacterial fever --------------------

plot(
  NA_real_,
  NA_real_,
  type = "n",
  xlim = plot_lim_log,
  ylim = plot_lim_log,
  axes = FALSE,
  ann = FALSE,
  frame.plot = FALSE,
  xaxs = "i",
  yaxs = "i"
)

axis(
  1,
  at = axis_ticks,
  labels = axis_labels,
  cex.axis = cex_axis_value,
  tck = -0.025,
  padj = 0.3,
  mgp = c(0.35, -0.15, 0)
)

axis(
  2,
  at = axis_ticks,
  labels = axis_labels,
  las = 1,
  cex.axis = cex_axis_value,
  tck = -0.025
)

points(
  log10(comparison_overall_under5$IHME),
  log10(comparison_overall_under5$Estimated),
  pch = 16,
  cex = cex_point,
  col = adjustcolor(col_overall, alpha.f = 0.80)
)

text(
  x = corr_x,
  y = corr_y,
  labels = paste0("Pearson correlation = ", formatC(cor_overall, digits = 2, format = "f")),
  adj = c(0, 1),
  cex = cex_corr
)

mtext("All syndromes vs. bacterial fever", side = 3, line = -0.5, cex = cex_title, font = 2)
mtext(
  "IHME mortality rate per\n1,000 children in 2019 (log scale)",
  side = 1,
  line = 2.0,
  cex = cex_axis_title
)


# Income-group legend ----------------------------------------------------------

par(mar = c(0, 0, 0, 0))

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(
  x = 0.02,
  y = 0.60,
  labels = "Income",
  adj = c(0, 0.5),
  cex = cex_legend_title,
  font = 2
)

legend_y <- c(0.55, 0.50, 0.45)

points(
  rep(0.12, 3),
  legend_y,
  pch = 16,
  cex = cex_point,
  col = adjustcolor(unname(income_cols_fixed), alpha.f = 0.80)
)

text(
  rep(0.28, 3),
  legend_y,
  labels = unname(income_labels),
  adj = c(0, 0.5),
  cex = cex_legend
)

dev.off()