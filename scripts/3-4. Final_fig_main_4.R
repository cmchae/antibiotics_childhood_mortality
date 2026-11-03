# _____________________________________________ ----------
# 1. Mortality-to-incidence ratio attributable to lack of effective treatment ----------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

rm(list = ls()); gc()

UNI                        <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
row <- which(UNI$.imp_pc == 1 & UNI$Year == 2023 )
UNI <- UNI[row,]

death_rate_lrti_Age_0      <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_0.rds"              )[row,]
death_rate_lrti_Age_1      <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_1.rds"              )[row,]
death_rate_lrti_Age_2to4   <- readRDS("outputs/pipelines/matrix_bact_death_rate_LRTI_Age_2to4.rds"           )[row,]
death_rate_fev_Age_0       <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_0.rds"             )[row,]
death_rate_fev_Age_1       <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_1.rds"             )[row,]
death_rate_fev_Age_2to4    <- readRDS("outputs/pipelines/matrix_bact_death_rate_fever_Age_2to4.rds"          )[row,]

inc_rate_lrti_Age_0        <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_0_11_matrix.rds"  )[row,]
inc_rate_lrti_Age_1        <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_12_23_matrix.rds" )[row,]
inc_rate_lrti_Age_2to4     <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_lrti_24_59_matrix.rds" )[row,]
inc_rate_fev_Age_0         <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_0_11_matrix.rds" )[row,]
inc_rate_fev_Age_1         <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_12_23_matrix.rds")[row,]
inc_rate_fev_Age_2to4      <- readRDS("outputs/pipelines/bacterial_incidence/inc_bact_fever_24_59_matrix.rds")[row,]


#endemic without latitude
AA_LRTI_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_0_11_months_enet_025_uncertainty.rds")$projection_draw_matrix[row,]
AA_LRTI_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_12_23_months_enet_025_uncertainty.rds")$projection_draw_matrix[row,]
AA_LRTI_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/LRTI_24_59_months_enet_025_uncertainty.rds")$projection_draw_matrix[row,]
AA_fev_Age_0     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_0_11_months_gam_uncertainty.rds")$projection_draw_matrix[row,]
AA_fev_Age_1     <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_12_23_months_gam_uncertainty.rds")$projection_draw_matrix[row,]
AA_fev_Age_2to4  <- readRDS("outputs/pipelines/AA_age_specific_selected_model_uncertainty/fever_24_59_months_gam_uncertainty.rds")$projection_draw_matrix[row,]


death_rate_list <- list(death_rate_lrti_Age_0, death_rate_lrti_Age_1, death_rate_lrti_Age_2to4, death_rate_fev_Age_0, death_rate_fev_Age_1, death_rate_fev_Age_2to4)
inc_rate_list   <- list(inc_rate_lrti_Age_0, inc_rate_lrti_Age_1, inc_rate_lrti_Age_2to4, inc_rate_fev_Age_0, inc_rate_fev_Age_1, inc_rate_fev_Age_2to4)
AA_list         <- list(AA_LRTI_Age_0, AA_LRTI_Age_1, AA_LRTI_Age_2to4, AA_fev_Age_0, AA_fev_Age_1, AA_fev_Age_2to4)


population                 <- read.csv("data/metadata/Population_by_age.csv") 
population <- population %>% filter(Year == 2023)

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







## data -----------------------------------------

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

Ab_lrti_eff_matrix <- matrix(Ab_lrti_eff, nrow = nrow(inc_rate_lrti), ncol = ncol(inc_rate_lrti), byrow = TRUE)
Ab_fev_eff_matrix <- matrix(Ab_fev_eff, nrow = nrow(inc_rate_fev), ncol = ncol(inc_rate_fev), byrow = TRUE)

dim(death_rate_lrti)
dim(inc_rate_lrti)
dim(Ab_lrti_prop)
dim(Ab_lrti_eff_matrix)


UNI <- UNI %>% mutate(Population = UNI_pop$Age_all)




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
hist(rowMeans(df.ab.lrti_L[,10:1009]), xlim = c(0,1))
hist(rowMeans(df.ab.lrti_LM[,10:1009]), xlim = c(0,1))
hist(rowMeans(df.ab.lrti_UM[,10:1009]), xlim = c(0,1))

hist(rowMeans(df.ab.fev_L[,10:1009]), xlim = c(0,1))
hist(rowMeans(df.ab.fev_LM[,10:1009]), xlim = c(0,1))
hist(rowMeans(df.ab.fev_UM[,10:1009]), xlim = c(0,1))

par(mfrow = c(1,2))
plot(df.ab.lrti$GDP, rowMeans(df.ab.lrti[,10:1009]))
plot(df.ab.fev$GDP, rowMeans(df.ab.fev[,10:1009]))


df.death_rate_lrti      <- df.death_rate_lrti       %>% filter(Year == 2023)
df.death_rate_fev       <- df.death_rate_fev        %>% filter(Year == 2023)
df.inc_rate_lrti        <- df.inc_rate_lrti         %>% filter(Year == 2023)
df.inc_rate_fev         <- df.inc_rate_fev          %>% filter(Year == 2023)
df.death_inc_ratio_lrti <- df.death_inc_ratio_lrti  %>% filter(Year == 2023)
df.death_inc_ratio_fev  <- df.death_inc_ratio_fev   %>% filter(Year == 2023)
df.CMR_lrti             <- df.CMR_lrti              %>% filter(Year == 2023)
df.CMR_fev              <- df.CMR_fev               %>% filter(Year == 2023)




summarize_CMR_draws <- function(data) {
  # Metadata columns
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "PC1", "PC2", "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  # 1. Country-year summary
  draw_matrix <- as.matrix(data[, draw_cols])
  
  summary_country_year <- data %>%
    dplyr::select(Country, Year, Income, gbd_region, GDP, PC1, PC2) %>%
    mutate(
      mean = rowMeans(draw_matrix, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 2. Overall summary
  overall_draws <- data %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    group = "Overall",
    mean = mean(overall_draws, na.rm = TRUE),
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
      mean = rowMeans(income_matrix, na.rm = TRUE),
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
      mean = rowMeans(region_matrix, na.rm = TRUE),
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

summary.CMR_lrti               <- summarize_CMR_draws(df.CMR_lrti)$country_year %>% filter(GDP < 20000)
summary.CMR_fev                <- summarize_CMR_draws(df.CMR_fev)$country_year %>% filter(GDP < 20000)
summary.death_rate_lrti        <- summarize_CMR_draws(df.death_rate_lrti )$country_year %>% filter(GDP < 20000)
summary.death_rate_fev         <- summarize_CMR_draws(df.death_rate_fev  )$country_year %>% filter(GDP < 20000)
summary.inc_rate_lrti          <- summarize_CMR_draws(df.inc_rate_lrti   )$country_year %>% filter(GDP < 20000)
summary.inc_rate_fev           <- summarize_CMR_draws(df.inc_rate_fev    )$country_year %>% filter(GDP < 20000)
summary.death_inc_ratio_lrti   <- summarize_CMR_draws(df.death_inc_ratio_lrti )$country_year %>% filter(GDP < 20000)
summary.death_inc_ratio_fev    <- summarize_CMR_draws(df.death_inc_ratio_fev  )$country_year %>% filter(GDP < 20000)

CMR_summary_LRTI_inc   <- summarize_CMR_draws(df.inc_rate_lrti)
CMR_summary_fever_inc  <- summarize_CMR_draws(df.inc_rate_fev)
CMR_summary_LRTI_mort  <- summarize_CMR_draws(df.death_rate_lrti)
CMR_summary_fever_mort <- summarize_CMR_draws(df.death_rate_fev)
CMR_summary_LRTI_ratio  <- summarize_CMR_draws(df.death_inc_ratio_lrti)
CMR_summary_fever_ratio <- summarize_CMR_draws(df.death_inc_ratio_fev)

CMR_overall_LRTI_inc    <- CMR_summary_LRTI_inc$overall    %>% tibble::as_tibble() %>% mutate(class = "LRTI_incidence_rate")          %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_LRTI_inc     <- CMR_summary_LRTI_inc$income     %>% mutate(class = "LRTI_incidence_rate")                                  %>% dplyr::transmute(class, income = Income, mean, lower, upper) 
CMR_overall_LRTI_mort   <- CMR_summary_LRTI_mort$overall   %>% tibble::as_tibble() %>% mutate(class = "LRTI_mortality_rate")          %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_LRTI_mort    <- CMR_summary_LRTI_mort$income    %>% mutate(class = "LRTI_mortality_rate")                                  %>% dplyr::transmute(class, income = Income, mean, lower, upper) 
CMR_overall_LRTI_ratio  <- CMR_summary_LRTI_ratio$overall  %>% tibble::as_tibble() %>% mutate(class = "LRTI_mortality_to_incidence")  %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_LRTI_ratio   <- CMR_summary_LRTI_ratio$income   %>% mutate(class = "LRTI_mortality_to_incidence")                          %>% dplyr::transmute(class, income = Income, mean, lower, upper) 
CMR_overall_fever_inc   <- CMR_summary_fever_inc$overall   %>% tibble::as_tibble() %>% mutate(class = "fever_incidence_rate")         %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_fever_inc    <- CMR_summary_fever_inc$income    %>% mutate(class = "fever_incidence_rate")                                 %>% dplyr::transmute(class, income = Income, mean, lower, upper) 
CMR_overall_fever_mort  <- CMR_summary_fever_mort$overall  %>% tibble::as_tibble() %>% mutate(class = "fever_mortality_rate")         %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_fever_mort   <- CMR_summary_fever_mort$income   %>% mutate(class = "fever_mortality_rate")                                 %>% dplyr::transmute(class, income = Income, mean, lower, upper) 
CMR_overall_fever_ratio <- CMR_summary_fever_ratio$overall %>% tibble::as_tibble() %>% mutate(class = "fever_mortality_to_incidence") %>% dplyr::transmute(class, income = group, mean, lower, upper) 
CMR_income_fever_ratio  <- CMR_summary_fever_ratio$income  %>% mutate(class = "fever_mortality_to_incidence")                         %>% dplyr::transmute(class, income = Income, mean, lower, upper) 

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






  





# _____________________________________________ ----------
# 2. Additional preventable mortality rate ----------------------------------------------------

CMR_lrti_min <- apply(CMR_lrti, 2, min, na.rm = TRUE)
CMR_fev_min <- apply(CMR_fev, 2, min, na.rm = TRUE)

CMR_lrti_min_matrix <- matrix(CMR_lrti_min, nrow = nrow(CMR_lrti), ncol = ncol(CMR_lrti), byrow = TRUE)
CMR_fev_min_matrix <- matrix(CMR_fev_min, nrow = nrow(CMR_fev), ncol = ncol(CMR_fev), byrow = TRUE)

mortality_lrti_Ab_preventable    <- inc_rate_lrti * (1-Ab_lrti_prop) * Ab_lrti_eff_matrix * CMR_lrti
mortality_fev_Ab_preventable     <- inc_rate_fev * (1-Ab_fev_prop) * Ab_fev_eff_matrix * CMR_fev
mortality_lrti_other_preventable    <- ((inc_rate_lrti * Ab_lrti_prop * (1-Ab_lrti_eff_matrix)) +  (inc_rate_lrti * (1-Ab_lrti_prop))) * (CMR_lrti - CMR_lrti_min_matrix) 
mortality_fev_other_preventable     <- ((inc_rate_fev * Ab_fev_prop * (1-Ab_fev_eff_matrix)) +  (inc_rate_fev * (1-Ab_fev_prop))) * (CMR_fev - CMR_fev_min_matrix) 
mortality_lrti_both_preventable     <- (((inc_rate_lrti * Ab_lrti_prop * (1-Ab_lrti_eff_matrix)) +  (inc_rate_lrti * (1-Ab_lrti_prop))) * CMR_lrti) - ((inc_rate_lrti * (1-Ab_lrti_eff_matrix)) * CMR_lrti_min_matrix)
mortality_fev_both_preventable      <- (((inc_rate_fev * Ab_fev_prop * (1-Ab_fev_eff_matrix)) +  (inc_rate_fev * (1-Ab_fev_prop))) * CMR_fev) - ((inc_rate_fev * (1-Ab_fev_eff_matrix)) * CMR_fev_min_matrix)

proportion_lrti_Ab_preventable <- mortality_lrti_Ab_preventable/mortality_lrti_both_preventable
proportion_fev_Ab_preventable <- mortality_fev_Ab_preventable/mortality_fev_both_preventable
# Create data frames -----------------------------------------------------------

df.mortality_lrti_Ab_preventable    <- cbind(UNI, as.data.frame(mortality_lrti_Ab_preventable))
df.mortality_fev_Ab_preventable     <- cbind(UNI, as.data.frame(mortality_fev_Ab_preventable))

df.mortality_lrti_other_preventable <- cbind(UNI, as.data.frame(mortality_lrti_other_preventable))
df.mortality_fev_other_preventable  <- cbind(UNI, as.data.frame(mortality_fev_other_preventable))

df.mortality_lrti_both_preventable  <- cbind(UNI, as.data.frame(mortality_lrti_both_preventable))
df.mortality_fev_both_preventable   <- cbind(UNI, as.data.frame(mortality_fev_both_preventable))

df.proportion_lrti_Ab_preventable   <- cbind(UNI, as.data.frame(proportion_lrti_Ab_preventable))
df.proportion_fev_Ab_preventable    <- cbind(UNI, as.data.frame(proportion_fev_Ab_preventable))


# Restrict downstream analyses to 2023 ----------------------------------------

df.death_rate_lrti_2023 <- cbind(UNI, as.data.frame(death_rate_lrti)) %>%
  dplyr::filter(Year == 2023)

df.death_rate_fev_2023 <- cbind(UNI, as.data.frame(death_rate_fev)) %>%
  dplyr::filter(Year == 2023)

UNI_2023 <- UNI %>%
  dplyr::filter(Year == 2023)

df.mortality_lrti_Ab_preventable <- df.mortality_lrti_Ab_preventable %>%
  dplyr::filter(Year == 2023)

df.mortality_fev_Ab_preventable <- df.mortality_fev_Ab_preventable %>%
  dplyr::filter(Year == 2023)

df.mortality_lrti_other_preventable <- df.mortality_lrti_other_preventable %>%
  dplyr::filter(Year == 2023)

df.mortality_fev_other_preventable <- df.mortality_fev_other_preventable %>%
  dplyr::filter(Year == 2023)

df.mortality_lrti_both_preventable <- df.mortality_lrti_both_preventable %>%
  dplyr::filter(Year == 2023)

df.mortality_fev_both_preventable <- df.mortality_fev_both_preventable %>%
  dplyr::filter(Year == 2023)

df.proportion_lrti_Ab_preventable <- df.proportion_lrti_Ab_preventable %>%
  dplyr::filter(Year == 2023)

df.proportion_fev_Ab_preventable <- df.proportion_fev_Ab_preventable %>%
  dplyr::filter(Year == 2023)


# Proportion of all bacterial mortality preventable through antibiotics --------

df.proportion_lrti_Ab_preventable_all_mortality <- cbind(
  UNI_2023,
  df.mortality_lrti_Ab_preventable[, 11:1010] /
    df.death_rate_lrti_2023[, 11:1010]
)

df.proportion_fev_Ab_preventable_all_mortality <- cbind(
  UNI_2023,
  df.mortality_fev_Ab_preventable[, 11:1010] /
    df.death_rate_fev_2023[, 11:1010]
)

summarize_CMR_draws <- function(data) {
  # Metadata columns
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "PC1", "PC2", "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  # 1. Country-year summary
  draw_matrix <- as.matrix(data[, draw_cols])
  
  summary_country_year <- data %>%
    dplyr::select(Country, Year, Income, gbd_region, Endemic, GDP, PC1, PC2) %>%
    mutate(
      mean = apply(draw_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
      lower = apply(draw_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
      upper = apply(draw_matrix, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  
  
  # 2. Overall summary
  overall_draws <- data %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  summary_overall <- data.frame(
    group = "Overall",
    mean = quantile(overall_draws, probs = 0.50, na.rm = TRUE),
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
      mean = apply(income_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
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
      mean = apply(region_matrix, 1, quantile, probs = 0.50, na.rm = TRUE),
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



write.csv(summary.mortality_lrti_Ab_preventable$country_year, "outputs/pipelines/suppl_table/preventable_mortality_rate_Ab_lrti.csv")
write.csv(summary.mortality_fev_Ab_preventable$country_year, "outputs/pipelines/suppl_table/preventable_mortality_rate_Ab_fev.csv")
write.csv(summary.mortality_lrti_both_preventable$country_year, "outputs/pipelines/suppl_table/preventable_mortality_rate_both_lrti.csv")
write.csv(summary.mortality_fev_both_preventable$country_year, "outputs/pipelines/suppl_table/preventable_mortality_rate_both_fev.csv")



#### (manuscript cacluation) country-level L-UM and LM-UM comparison ####
summarize_income_ratios <- function(data) {
  
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "PC1", "PC2", "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  income_draws <- data %>%
    filter(Income %in% c("L", "LM", "UM")) %>%
    group_by(Income) %>%
    summarise(across(all_of(draw_cols),
                     ~ sum(.x * Population, na.rm = TRUE) / sum(Population[!is.na(.x)], na.rm = TRUE)),
              .groups = "drop")
  
  L_draws  <- unlist(income_draws[income_draws$Income == "L", draw_cols], use.names = FALSE)
  LM_draws <- unlist(income_draws[income_draws$Income == "LM", draw_cols], use.names = FALSE)
  UM_draws <- unlist(income_draws[income_draws$Income == "UM", draw_cols], use.names = FALSE)
  
  ratio_L_UM  <- L_draws / UM_draws
  ratio_LM_UM <- LM_draws / UM_draws
  
  data.frame(
    comparison = c("L / UM", "LM / UM"),
    mean = c(
      round(quantile(ratio_L_UM, 0.50, na.rm = TRUE) , digits = 1), 
      round(quantile(ratio_LM_UM, 0.50, na.rm = TRUE), digits = 1)),
    lower = c(
      round(quantile(ratio_L_UM, 0.025, na.rm = TRUE) , digits = 1), 
      round(quantile(ratio_LM_UM, 0.025, na.rm = TRUE), digits = 1)),
    upper = c(
      round(quantile(ratio_L_UM, 0.975, na.rm = TRUE) , digits = 1),
      round(quantile(ratio_LM_UM, 0.975, na.rm = TRUE), digits = 1)))
}

# Income-level ratios relative to upper-middle-income countries
summarize_income_ratios(df.mortality_lrti_Ab_preventable)
summarize_income_ratios(df.mortality_fev_Ab_preventable)
summarize_income_ratios(df.mortality_lrti_both_preventable)
summarize_income_ratios(df.mortality_fev_both_preventable)




#### (manuscript cacluation) sub-sharan % ####
summarize_preventable_death_shares <- function(data) {
  
  meta_cols <- c("Country", "Year", "GDP", "Income", "gbd_region",
                 "PC1", "PC2", "Endemic", "Population")
  
  draw_cols <- setdiff(names(data), meta_cols)
  
  total_draws <- data %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  ssa_draws <- data %>%
    filter(gbd_region == "Sub-Saharan Africa") %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  low_lm_draws <- data %>%
    filter(Income %in% c("L", "LM")) %>%
    summarise(across(all_of(draw_cols), ~ sum(.x, na.rm = TRUE))) %>%
    unlist(use.names = FALSE)
  
  ssa_prop_draws <- ssa_draws / total_draws * 100
  low_lm_prop_draws <- low_lm_draws / total_draws * 100
  
  data.frame(
    group = c("Sub-Saharan Africa", "Low + lower-middle income"),
    mean = c(
      round(quantile(ssa_prop_draws, 0.50, na.rm = TRUE), digits = 1),
      round(quantile(low_lm_prop_draws, 0.50, na.rm = TRUE), digits = 1)),
    lower = c(
      round(quantile(ssa_prop_draws, 0.025, na.rm = TRUE), digits = 1),
      round(quantile(low_lm_prop_draws, 0.025, na.rm = TRUE), digits = 1)),
    upper = c(
      round(quantile(ssa_prop_draws, 0.975, na.rm = TRUE), digits = 1),
      round(quantile(low_lm_prop_draws, 0.975, na.rm = TRUE), digits = 1)))
}


value_mortality_lrti_fev_Ab_preventable <- df.mortality_fev_Ab_preventable[,11:1010] + df.mortality_lrti_Ab_preventable[,11:1010]
summarize_preventable_death_shares(bind_cols(df.mortality_fev_Ab_preventable[,1:10], value_mortality_lrti_fev_Ab_preventable))

# proportion of antibiotics over all improvement (best country)
value_mortality_lrti_fev_both_preventable <- df.mortality_fev_both_preventable[,11:1010] + df.mortality_lrti_both_preventable[,11:1010]
round(quantile(colSums(value_mortality_lrti_fev_Ab_preventable)/colSums(value_mortality_lrti_fev_both_preventable), c(0.50, 0.025, 0.975)), digits = 3)

# proportion of antibiotics over within country improvement
df.mortality_lrti_Ab_abs_preventable <- sweep(df.mortality_lrti_Ab_preventable[, 11:1010], 1, df.mortality_lrti_Ab_preventable$Population, `*`)
df.mortality_fev_Ab_abs_preventable <- sweep(df.mortality_fev_Ab_preventable[, 11:1010], 1, df.mortality_fev_Ab_preventable$Population, `*`)
df.mortality_lrti_fev_Ab_abs_preventable <- colSums(df.mortality_lrti_Ab_abs_preventable) + colSums(df.mortality_fev_Ab_abs_preventable )

within_country_both_draw <- read.csv("outputs/sub_results/within_country_both_draw.csv")[,2]
round(quantile(df.mortality_lrti_fev_Ab_abs_preventable / within_country_both_draw, c(0.50, 0.025, 0.975)), digits = 3)






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




for (outcome_i in c("lrti", "fever")) {
  for (q_i in paste0("q", 1:5)) {
    
    matrix_i <- CMR_results_by_wealth[[q_i]][[paste0("CMR_", outcome_i)]]
    
    result_i <- data.frame(
      q_i,
      Country = UNI_2023$Country,
      median = apply(matrix_i, 1, quantile, 0.5, na.rm = TRUE),
      lower = apply(matrix_i, 1, quantile, 0.025, na.rm = TRUE),
      upper = apply(matrix_i, 1, quantile, 0.975, na.rm = TRUE)
    )
    
    assign(paste0(outcome_i, "_", q_i), result_i)
  }
}


CMR_result_LRTI <- bind_rows(lrti_q1,
                                   lrti_q2,
                                   lrti_q3,
                                   lrti_q4,
                                   lrti_q5)

CMR_result_fever <- bind_rows(fever_q1,
                             fever_q2,
                             fever_q3,
                             fever_q4,
                             fever_q5)

write.csv(CMR_result_LRTI, "outputs/pipelines/suppl_table/quintile_ineff_CFR_lrti.csv")
write.csv(CMR_result_fever, "outputs/pipelines/suppl_table/quintile_ineff_CFR_fever.csv")










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











##(manuscript calculation) wealth quintile level comparison ######
summarize_q1_q5_difference <- function(q1_matrix, q5_matrix, metadata) {
  
  q1_matrix <- as.matrix(q1_matrix)
  q5_matrix <- as.matrix(q5_matrix)
  
  if (nrow(q1_matrix) != nrow(metadata) | nrow(q5_matrix) != nrow(metadata)) {
    stop("Row mismatch between draw matrices and metadata.")
  }
  
  if (ncol(q1_matrix) != ncol(q5_matrix)) {
    stop("q1 and q5 matrices must have the same number of draws.")
  }
  
  draw_cols <- paste0("draw_", seq_len(ncol(q1_matrix)))
  colnames(q1_matrix) <- draw_cols
  colnames(q5_matrix) <- draw_cols
  
  q1_data <- dplyr::bind_cols(
    metadata %>% dplyr::select(Income, Population),
    as.data.frame(q1_matrix)
  )
  
  q5_data <- dplyr::bind_cols(
    metadata %>% dplyr::select(Income, Population),
    as.data.frame(q5_matrix)
  )
  
  q1_income_draws <- q1_data %>%
    dplyr::filter(Income %in% c("L", "LM", "UM")) %>%
    dplyr::group_by(Income) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(draw_cols),
                    ~ weighted.mean(.x, w = Population, na.rm = TRUE)),
      .groups = "drop"
    )
  
  q5_income_draws <- q5_data %>%
    dplyr::filter(Income %in% c("L", "LM", "UM")) %>%
    dplyr::group_by(Income) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(draw_cols),
                    ~ weighted.mean(.x, w = Population, na.rm = TRUE)),
      .groups = "drop"
    )
  
  q1_income_draws <- q1_income_draws %>% dplyr::arrange(match(Income, c("L", "LM", "UM")))
  q5_income_draws <- q5_income_draws %>% dplyr::arrange(match(Income, c("L", "LM", "UM")))
  
  q1_draws <- as.matrix(q1_income_draws[, draw_cols, drop = FALSE])
  q5_draws <- as.matrix(q5_income_draws[, draw_cols, drop = FALSE])
  
  difference_draws <- (q1_draws / q5_draws - 1) * 100
  
  data.frame(
    Income = q1_income_draws$Income,
    mean =  round(apply(difference_draws, 1, quantile, probs = 0.50, na.rm = TRUE), digits = 1),
    lower = round(apply(difference_draws, 1, quantile, probs = 0.025, na.rm = TRUE), digits = 1),
    upper = round(apply(difference_draws, 1, quantile, probs = 0.975, na.rm = TRUE), digits = 1)
  )
}

summarize_q1_q5_difference(preventable_results_by_wealth$q1$mortality_lrti_Ab_preventable, preventable_results_by_wealth$q5$mortality_lrti_Ab_preventable, UNI)
summarize_q1_q5_difference(preventable_results_by_wealth$q1$mortality_fever_Ab_preventable, preventable_results_by_wealth$q5$mortality_fever_Ab_preventable, UNI)
summarize_q1_q5_difference(preventable_results_by_wealth$q1$mortality_lrti_both_preventable, preventable_results_by_wealth$q5$mortality_lrti_both_preventable, UNI)
summarize_q1_q5_difference(preventable_results_by_wealth$q1$mortality_fever_both_preventable, preventable_results_by_wealth$q5$mortality_fever_both_preventable, UNI)





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








quintile_preventable_mortality_rate_Ab <- bind_rows(
preventable_summary_by_wealth$q1$mortality_lrti_Ab_preventable$country_year,
preventable_summary_by_wealth$q2$mortality_lrti_Ab_preventable$country_year,
preventable_summary_by_wealth$q3$mortality_lrti_Ab_preventable$country_year,
preventable_summary_by_wealth$q4$mortality_lrti_Ab_preventable$country_year,
preventable_summary_by_wealth$q5$mortality_lrti_Ab_preventable$country_year,
preventable_summary_by_wealth$q1$mortality_fever_Ab_preventable$country_year,
preventable_summary_by_wealth$q2$mortality_fever_Ab_preventable$country_year,
preventable_summary_by_wealth$q3$mortality_fever_Ab_preventable$country_year,
preventable_summary_by_wealth$q4$mortality_fever_Ab_preventable$country_year,
preventable_summary_by_wealth$q5$mortality_fever_Ab_preventable$country_year)

quintile_preventable_mortality_rate_Ab <- bind_cols(
  quintile_preventable_mortality_rate_Ab,
  outcome = c(rep("LRTI", 390), rep("AFI", 390)))

quintile_preventable_mortality_rate_both <- bind_rows(
preventable_summary_by_wealth$q1$mortality_lrti_both_preventable$country_year,
preventable_summary_by_wealth$q2$mortality_lrti_both_preventable$country_year,
preventable_summary_by_wealth$q3$mortality_lrti_both_preventable$country_year,
preventable_summary_by_wealth$q4$mortality_lrti_both_preventable$country_year,
preventable_summary_by_wealth$q5$mortality_lrti_both_preventable$country_year,
preventable_summary_by_wealth$q1$mortality_fever_both_preventable$country_year,
preventable_summary_by_wealth$q2$mortality_fever_both_preventable$country_year,
preventable_summary_by_wealth$q3$mortality_fever_both_preventable$country_year,
preventable_summary_by_wealth$q4$mortality_fever_both_preventable$country_year,
preventable_summary_by_wealth$q5$mortality_fever_both_preventable$country_year)

quintile_preventable_mortality_rate_both <- bind_cols(
  quintile_preventable_mortality_rate_both,
  outcome = c(rep("LRTI", 390), rep("AFI", 390)))






write.csv(quintile_preventable_mortality_rate_Ab, "outputs/pipelines/suppl_table/quintile_preventable_mortality_rate_Ab.csv")
write.csv(quintile_preventable_mortality_rate_both, "outputs/pipelines/suppl_table/quintile_preventable_mortality_rate_both.csv")







# (Main 4)  Plot: preventable mortality versus GDP, colored by income -------------

pdf("figures/supplementary/mortality_to_incidence/(Main 4) preventable mortality rate ~ GDP (color by income)_2023.pdf", width = 6, height = 3.25)

layout(
  matrix(c(
    0, 0, 0, 0, 0,
    1, 2, 3, 4, 9,
    0, 0, 0, 0, 10,
    5, 6, 7, 8, 10
  ), nrow = 4, byrow = TRUE),
  widths = c(1, 1, 1, 1, 1),
  heights = c(0.1, 1, 0.1, 1)
)

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.6, 1, 0.4),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

income_cols_fixed <- c(
  "L" = "#CC79A7",
  "LM" = "#009E73",
  "UM" = "#0072B2"
)

income_labels <- c(
  "L" = "Low-income",
  "LM" = "Lower-middle-income",
  "UM" = "Upper-middle-income"
)


## Household wealth colors and labels -----------------------------------------

wealth_cols_fixed <- c(q1 = "#CC79A7", q5 = "#0072B2")
wealth_labels <- c(
  "q1" = "Poorest quintile",
  "q5" = "Richest quintile"
)

wealth_tags <- names(wealth_cols_fixed)

lowess_fraction <- 2/3
wealth_x_offset <- c(q1 = -0.030, q5 = 0.030)

add_wealth_lowess <- function(dat){
  for(wealth_i in wealth_tags){
    dat_i <- dat %>% dplyr::filter(wealth_tag == wealth_i, is.finite(GDP), GDP > 0, is.finite(mean), mean > 0) %>% dplyr::arrange(GDP)
    smooth_i <- lowess(log10(dat_i$GDP), log10(dat_i$mean*multiple), f = lowess_fraction, iter = 3)
    lines(smooth_i$x, smooth_i$y, col = wealth_cols_fixed[wealth_i], lwd = 1, lty = 1)
  }
}

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


## (main) Prepare mortality-rate data -----------------------------------------------

mortality_lrti_Ab_plot_main <- summary.mortality_lrti_Ab_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(mean), is.finite(lower), is.finite(upper)) 

mortality_lrti_both_plot_main <- summary.mortality_lrti_both_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(mean), is.finite(lower), is.finite(upper)) 

mortality_fev_Ab_plot_main <- summary.mortality_fev_Ab_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(mean), is.finite(lower), is.finite(upper)) 

mortality_fev_both_plot_main <- summary.mortality_fev_both_preventable$country_year %>%
  filter(is.finite(GDP), GDP >= xmin, GDP <= xmax, is.finite(mean), is.finite(lower), is.finite(upper)) 

## (main) Common logarithmic y-axis for six mortality-rate panels --------------------
all_y_values <- c(
  mortality_lrti_Ab_plot_main$lower,
  mortality_lrti_Ab_plot_main$upper,
  mortality_lrti_both_plot_main$lower,
  mortality_lrti_both_plot_main$upper,
  mortality_fev_Ab_plot_main$lower,
  mortality_fev_Ab_plot_main$upper,
  mortality_fev_both_plot_main$lower,
  mortality_fev_both_plot_main$upper
)

all_y_values <- all_y_values * 1000
all_y_values <- all_y_values[is.finite(all_y_values) & all_y_values > 0]

common_y_exp_min <- -4
common_y_exp_max <- 2

common_y_ticks <- seq(common_y_exp_min, common_y_exp_max, by = 1)
common_y_labels <- parse(text = paste0("10^", common_y_ticks))
common_y_lim_log <- range(common_y_ticks)
common_y_minor_ticks <- log10(unlist(lapply(10^(min(common_y_ticks):max(common_y_ticks)-1), \(x) x * (2:9))))





## (sub) Prepare preventable mortality-rate plotting data -------------------------

mortality_lrti_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper),
    wealth_level %in% c("Poorest", "Richest")
  )

mortality_lrti_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_lrti_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper),
    wealth_level %in% c("Poorest", "Richest")
  )

mortality_fev_Ab_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_Ab_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper),
    wealth_level %in% c("Poorest", "Richest")
  )

mortality_fev_both_plot <- dplyr::bind_rows(
  lapply(preventable_summary_by_wealth, function(x) {
    x$mortality_fever_both_preventable$country_year
  })
) %>%
  dplyr::filter(
    is.finite(GDP), GDP >= xmin, GDP <= xmax,
    is.finite(mean), is.finite(lower), is.finite(upper),
    wealth_level %in% c("Poorest", "Richest")
  )


## Pnael 1. LRTI (main) preventable by antibiotics ------------------------------------------

dat <- mortality_lrti_Ab_plot_main
income_col <- income_cols_fixed[as.character(dat$Income)]
income_col[is.na(income_col)] <- "grey50"

par(
  bty = "l",
  las = 1,
  mar = c(2.5, 3.6, 0.4, 0.4),
  mgp = c(1.9, 0.45, 0),
  tcl = -0.2
)

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

axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

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
  log10(dat$mean*1000),
  pch = 16,
  cex = 0.6,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("Mortality rate\nper 1000 child-years", side = 2, line = 2.0, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("a", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)

## Pnael 2. LRTI (sub) mortality preventable by antibiotics --------------------------

dat <- mortality_lrti_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"
x_offset <- unname(wealth_x_offset[as.character(dat$wealth_tag)])

mean_plot  <- dat$mean * 1000 
lower_plot <- dat$lower * 1000
upper_plot <- dat$upper * 1000

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)

segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.15), lwd = 0.6
)

# points(
#   log10(dat$GDP), log10(mean_plot),
#   pch = 16, cex = 0.6,
#   col = adjustcolor(wealth_col, alpha.f = 0.60)
# )

add_wealth_lowess(dat)

mtext("c", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)





## Panel 3. LRTI (main) preventable by antibiotics and other factors ------------------------

dat <- mortality_lrti_both_plot_main

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


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


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
  log10(dat$mean*1000),
  pch = 16,
  cex = 0.6,
  col = adjustcolor(income_col, alpha.f = 0.80)
)


mtext("e", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)




## Panel 4. LRTI (sub) mortality preventable by both interventions --------------------

dat <- mortality_lrti_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"
x_offset <- unname(wealth_x_offset[as.character(dat$wealth_tag)])

mean_plot  <- dat$mean * 1000 
lower_plot <- dat$lower * 1000
upper_plot <- dat$upper * 1000

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.15), lwd = 0.6
)

# points(
#   log10(dat$GDP), log10(mean_plot),
#   pch = 16, cex = 0.6,
#   col = adjustcolor(wealth_col, alpha.f = 0.60)
# )

add_wealth_lowess(dat)

mtext("g", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)




## Panel 5. Fever (main) preventable by antibiotics -----------------------------------------

dat <- mortality_fev_Ab_plot_main

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


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


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
  log10(dat$mean*1000),
  pch = 16,
  cex = 0.6,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("Mortality rate\nper 1000 child-years", side = 2, line = 2.0, adj = 0.68, cex = cex.axis_title, las = 0)
mtext("b", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)




## Panel 6. Fever (sub) mortality preventable by antibiotics -------------------------

dat <- mortality_fev_Ab_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"
x_offset <- unname(wealth_x_offset[as.character(dat$wealth_tag)])

mean_plot <-  dat$mean * 1000 
lower_plot <- dat$lower * 1000
upper_plot <- dat$upper * 1000

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.15), lwd = 0.6
)

# points(
#   log10(dat$GDP), log10(mean_plot),
#   pch = 16, cex = 0.6,
#   col = adjustcolor(wealth_col, alpha.f = 0.60)
# )

add_wealth_lowess(dat)

mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("d", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)







## Panel 7. Fever (main) preventable by antibiotics and other factors -----------------------

dat <- mortality_fev_both_plot_main

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


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


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
  log10(dat$mean*1000),
  pch = 16,
  cex = 0.6,
  col = adjustcolor(income_col, alpha.f = 0.80)
)

mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("f", side = 3, adj = -0.73, line = 0.65, cex = 0.75, font = 2)




## Panel 8. Fever (sub) mortality preventable by both interventions -------------------

dat <- mortality_fev_both_plot
wealth_col <- unname(wealth_cols_fixed[as.character(dat$wealth_tag)])
wealth_col[is.na(wealth_col)] <- "grey50"
x_offset <- unname(wealth_x_offset[as.character(dat$wealth_tag)])

mean_plot <-  dat$mean * 1000  
lower_plot <- dat$lower * 1000
upper_plot <- dat$upper * 1000

plot(
  NA_real_, NA_real_, type = "n",
  xlim = xlim_log, ylim = common_y_lim_log,
  axes = FALSE, ann = FALSE, frame.plot = FALSE,
  xaxs = "i", yaxs = "i"
)


axis(1, at = x_ticks, labels = x_labels, cex.axis = cex.axis_value, tck = -0.040, padj = 0.3, mgp = c(0.35, -0.20, 0), lwd = 0, lwd.ticks = 1)
axis(1, at = x_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1)
axis(2, at = common_y_ticks, labels = common_y_labels, las = 1, cex.axis = cex.axis_value, tck = -0.045)
axis(2, at = common_y_minor_ticks, labels = FALSE, cex.axis = cex.axis_value, tck = -0.015, lwd.ticks = 1, lwd = 0)


segments(
  x0 = log10(dat$GDP), y0 = log10(lower_plot),
  x1 = log10(dat$GDP), y1 = log10(upper_plot),
  col = adjustcolor(wealth_col, alpha.f = 0.15), lwd = 0.6
)

# points(
#   log10(dat$GDP), log10(mean_plot),
#   pch = 16, cex = 0.6,
#   col = adjustcolor(wealth_col, alpha.f = 0.60)
# )

add_wealth_lowess(dat)

mtext("GDP per capita (PPP)", side = 1, line = 1.1, cex = cex.axis_title)
mtext("h", side = 3, adj = -0.76, line = 0.65, cex = 0.75, font = 2)

## Panel 9: Income legend ------------------------------------------------------

par(mar = c(0, 0.3, 0, 0))
plot.new()

income_legend_x <- 0.01
income_legend_y <- 0.15
income_legend_text <- unname(income_labels[names(income_cols_fixed)])
income_legend_cols <- adjustcolor(unname(income_cols_fixed), alpha.f = 0.80)

income_legend_info <- legend(
  x = income_legend_x, y = income_legend_y,
  xjust = 0, yjust = 0.48,
  title = " ",
  legend = income_legend_text,
  col = income_legend_cols,
  pch = 16,
  lty = 1,
  lwd = 1,
  pt.cex = 0.6,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.40,
  bty = "n",
  plot = FALSE
)

legend(
  x = income_legend_x, y = income_legend_y,
  xjust = 0, yjust = 0.5,
  title = " ",
  legend = income_legend_text,
  col = income_legend_cols,
  pch = 16,
  lty = 1,
  lwd = 1,
  pt.cex = 0.6,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.95,
  x.intersp = 0.40,
  bty = "n"
)

text(
  x = income_legend_x-0.03,
  y = max(income_legend_info$text$y) + strheight("M", cex = cex_legend) * 1.8,
  labels = "[Country income level]",
  adj = c(0, 0.5),
  cex = cex_legend,
  font = 2,
  xpd = NA
)



## Panel 10: Household wealth legend ------------------------------------------

par(mar = c(0, 0.3, 0, 0))
plot.new()

wealth_legend_x <- -0.05
wealth_legend_y <- 0.95
wealth_legend_text <- unname(wealth_labels[names(wealth_cols_fixed)])
wealth_legend_cols <- adjustcolor(unname(wealth_cols_fixed), alpha.f = 0.80)

wealth_legend_info <- legend(
  x = wealth_legend_x, y = wealth_legend_y,
  xjust = 0, yjust = 0.55,
  title = " ",
  legend = wealth_legend_text,
  col = wealth_legend_cols,
  lty = 1,
  lwd = 1,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.85,
  x.intersp = 0.40,
  bty = "n",
  plot = FALSE
)

legend(
  x = wealth_legend_x, y = wealth_legend_y,
  xjust = 0, yjust = 0.85,
  title = " ",
  legend = wealth_legend_text,
  col = wealth_legend_cols,
  lty = 1,
  lwd = 1,
  cex = cex_legend,
  title.cex = cex_legend,
  y.intersp = 0.95,
  x.intersp = 0.40,
  bty = "n"
)

text(
  x = wealth_legend_x+0.02,
  y = max(wealth_legend_info$text$y),
  labels = "[Household wealth quintile]",
  adj = c(0, 0.5),
  cex = cex_legend,
  font = 2,
  xpd = NA
)


dev.off()

 











