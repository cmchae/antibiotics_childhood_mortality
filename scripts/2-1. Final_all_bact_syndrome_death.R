
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

# df.analyze <- df.analyze %>% mutate(
#   bact_underlying_fever = ifelse(Fever == 1 & LRTI == 0 & bact_underlying_etiology_1 == 1, 1, 0),
#   bact_all_fever = ifelse(Fever == 1 & LRTI == 0 & bact_all_etiology_1 == 1, 1, 0),
#   bact_underlying_LRTI = ifelse(LRTI == 1 & bact_underlying_etiology_1 == 1, 1, 0),
#   bact_all_LRTI = ifelse(LRTI == 1 & bact_all_etiology_1 == 1, 1, 0))

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
    bact_deaths = sum(bact_all_etiology_1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    other_deaths = total_deaths - bact_deaths,
    observed_prop_bact = bact_deaths / total_deaths,
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

write.csv(death_agg, "outputs/pipelines/champs_all_bact_death_table.csv", row.names = FALSE)


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

mod_bact_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))
beta_draws_bact_list <- setNames(vector("list", length(pc_imp_values)), as.character(pc_imp_values))

n_uni <- nrow(UNI_base)
row_key_base <- paste(UNI_base$Country, UNI_base$Year, sep = "___")

matrix_prop_bact_deaths_Age_0 <- matrix(
  NA_real_, nrow = n_uni, ncol = 1000,
  dimnames = list(paste(UNI_base$Country, UNI_base$Year, sep = "_"), paste0("draw_", 1:1000))
)
matrix_prop_bact_deaths_Age_1 <- matrix_prop_bact_deaths_Age_0
matrix_prop_bact_deaths_Age_2to4 <- matrix_prop_bact_deaths_Age_0

for (pc_imp_i in pc_imp_values) {
  death_agg_i <- death_agg %>%
    inner_join(
      UNI_model %>%
        filter(.imp_pc == pc_imp_i) %>%
        dplyr::select(Country, Year, PC1, PC2),
      by = c("Country", "Year")
    ) %>%
    mutate(age_group = factor(age_group, levels = age_levels))
  
  mod_bact_i <- glm(
    cbind(bact_deaths, other_deaths) ~ PC1 + PC2 + age_group,
    family = binomial(link = "logit"),
    data = death_agg_i
  )

  
  set.seed(700000 + pc_imp_i)
  beta_draws_bact_i <- MASS::mvrnorm(
    n = n_draw_within_pc,
    mu = coef(mod_bact_i),
    Sigma = vcov(mod_bact_i)
  )

  colnames(beta_draws_bact_i) <- names(coef(mod_bact_i))

  mod_bact_list[[as.character(pc_imp_i)]] <- mod_bact_i
  beta_draws_bact_list[[as.character(pc_imp_i)]] <- beta_draws_bact_i

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
  
  X_pred_i <- X_pred_i[, names(coef(mod_bact_i)), drop = FALSE]

  pred_i <- plogis(X_pred_i %*% t(beta_draws_bact_i))

  draw_ids_i <- mortality_draw_design %>%
    filter(.imp_pc == pc_imp_i) %>%
    arrange(.draw_within_pc) %>%
    pull(draw_id)
  
  matrix_prop_bact_deaths_Age_0[, draw_ids_i] <- pred_i[1:n_uni, , drop = FALSE]
  matrix_prop_bact_deaths_Age_1[, draw_ids_i] <- pred_i[(n_uni + 1):(2 * n_uni), , drop = FALSE]
  matrix_prop_bact_deaths_Age_2to4[, draw_ids_i] <- pred_i[(2 * n_uni + 1):(3 * n_uni), , drop = FALSE]

}

stopifnot(
  !anyNA(matrix_prop_bact_deaths_Age_0),
  !anyNA(matrix_prop_bact_deaths_Age_1),
  !anyNA(matrix_prop_bact_deaths_Age_2to4)
)


saveRDS(matrix_prop_bact_deaths_Age_0,     "outputs/pipelines/matrix_matrix_prop_bact_allsyndrome_deaths_Age_0.rds")
saveRDS(matrix_prop_bact_deaths_Age_1,     "outputs/pipelines/matrix_matrix_prop_bact_allsyndrome_deaths_Age_1.rds")
saveRDS(matrix_prop_bact_deaths_Age_2to4,  "outputs/pipelines/matrix_matrix_prop_bact_allsyndrome_deaths_Age_2to4.rds")

saveRDS(
  list(LRTI = mod_bact_list),
  "outputs/pipelines/champs_bacterial_allsyndrome_death_fraction_models_5PC.rds"
)


# Calculate bacterial LRTI and bacterial non-LRTI fever mortality rates ------

matrix_death_rate_q_year0    <- readRDS("outputs/pipelines/matrix_death_rate_year0.rds")
matrix_death_rate_q_year1    <- readRDS("outputs/pipelines/matrix_death_rate_year1.rds")
matrix_death_rate_q_year2to4 <- readRDS("outputs/pipelines/matrix_death_rate_year2to4.rds")

stopifnot(
  identical(dim(matrix_death_rate_q_year0), dim(matrix_prop_bact_deaths_Age_0)),
  identical(dim(matrix_death_rate_q_year1), dim(matrix_prop_bact_deaths_Age_1)),
  identical(dim(matrix_death_rate_q_year2to4), dim(matrix_prop_bact_deaths_Age_2to4)),
  identical(colnames(matrix_death_rate_q_year0), colnames(matrix_prop_bact_deaths_Age_0)),
  identical(rownames(matrix_death_rate_q_year0), rownames(matrix_prop_bact_deaths_Age_0))
)

matrix_bact_death_rate_Age_0     <- matrix_death_rate_q_year0    * matrix_prop_bact_deaths_Age_0
matrix_bact_death_rate_Age_1     <- matrix_death_rate_q_year1    * matrix_prop_bact_deaths_Age_1
matrix_bact_death_rate_Age_2to4  <- matrix_death_rate_q_year2to4 * matrix_prop_bact_deaths_Age_2to4

saveRDS(matrix_bact_death_rate_Age_0,     "outputs/pipelines/matrix_bact_allsyndrome_death_rate_Age_0.rds")
saveRDS(matrix_bact_death_rate_Age_1,     "outputs/pipelines/matrix_bact_allsyndrome_death_rate_Age_1.rds")
saveRDS(matrix_bact_death_rate_Age_2to4,  "outputs/pipelines/matrix_bact_allsyndrome_death_rate_Age_2to4.rds")

quantile(matrix_prop_bact_deaths_Age_0, c(0.5, 0.025, 0.975))
quantile(matrix_prop_bact_deaths_Age_1, c(0.5, 0.025, 0.975))
quantile(matrix_prop_bact_deaths_Age_2to4, c(0.5, 0.025, 0.975))



UNI                        <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds")
population                 <- read.csv("data/metadata/Population_by_age.csv") 

setdiff(UNI$Country, population$Country)

row_2023 <- which(UNI$Year == 2023 & UNI$.imp_pc == 1)
death_rate_list <- list(matrix_bact_death_rate_Age_0[row_2023,], matrix_bact_death_rate_Age_1[row_2023,], matrix_bact_death_rate_Age_2to4[row_2023,])

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
UNI_pop <- UNI %>% inner_join(population, by = c("Country", "Year")) %>% filter(Year == 2023)
UNI_pop <- UNI_pop %>% mutate(Age_2to4 = Age_2 + Age_3 + Age_4, Age_all = Age_0 + Age_1 + Age_2 + Age_3 + Age_4) %>% dplyr::select(Country, Year, Age_0, Age_1, Age_2to4, Age_all)


death_number <- sweep(death_rate_list[[1]], 1, UNI_pop$Age_0,"*") +
                sweep(death_rate_list[[2]], 1, UNI_pop$Age_1,"*") +
                sweep(death_rate_list[[3]], 1, UNI_pop$Age_2to4,"*")

death_number_global <- colSums(death_number)

write.csv(death_number, "outputs/pipelines/death_number_draw.csv")

dim(death_number)

death_number_summary <- quantile(death_number_global, probs = c(0.5, 0.025, 0.975), na.rm = TRUE)
death_number_summary


global_death_number <- colSums(death_number)

value <- 0.153; lower <- 0.074; upper <- 0.217
sd_lower <- (qlogis(value) - qlogis(lower)) / qnorm(0.975)
sd_upper <- (qlogis(upper) - qlogis(value)) / qnorm(0.975)
set.seed(123)
z <- rnorm(1000)
draw_af <- plogis(qlogis(value) + ifelse(z < 0, z * sd_lower, z * sd_upper))
length(draw_af)
quantile(draw_af, c(0.025, 0.5, 0.975))

death_AA <- global_death_number*draw_af
quantile(death_AA, c(0.025, 0.5, 0.975))

gbd_region <- readRDS("outputs/pipelines/pc_gdp_2023_incidence.rds") %>% filter(Year == 2023 & .imp_pc == 1) %>% dplyr::select(gbd_region)
dim(death_number)


death_AA_number <- sweep(death_number, 2, draw_af,"*") 
death_number_region <- bind_cols(
  gbd_region, as.data.frame(death_AA_number)) %>% 
  group_by(gbd_region) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

overall_row <- c(list(gbd_region = "Overall"), as.list(death_AA))
names(overall_row) <- names(death_number_region)

death_number_region <- bind_rows(
  death_number_region,
  overall_row
)

death_number_region_summary <- data.frame(
  gbd_region = death_number_region$gbd_region,
  median = apply(death_number_region[, 2:1001], 1, quantile, probs = 0.5),
  lower  = apply(death_number_region[, 2:1001], 1, quantile, probs = 0.025),
  upper  = apply(death_number_region[, 2:1001], 1, quantile, probs = 0.975)
)

write.csv(death_number_region_summary, "outputs/pipelines/death_region_AA.csv")








# comparing other death (after running section 6 in mortality code)
af_all_death_draw <- death_AA / death_all_2023_LMIC

quantile(af_all_death_draw, c(0.025, 0.5, 0.975), na.rm = TRUE)

unicef <- read.csv("data/all_cause_death/Unicef.csv")

unicef_clean <- unicef %>%
  mutate(Country = iconv(Country, from = "", to = "UTF-8", sub = ""), Uncertainty = iconv(Uncertainty, from = "", to = "UTF-8", sub = "")) %>% 
  mutate(Country = trimws(Country), Uncertainty = trimws(Uncertainty)) %>% 
  filter(!is.na(Country), Country != "", Uncertainty %in% c("Lower", "Median", "Upper"))

unicef_clean <- unicef_clean %>%
  mutate(Country = case_when(grepl("Ivoire", Country) ~ "Cote d'Ivoire", TRUE ~ Country))

unicef_clean %>% count(Country, Uncertainty) %>% filter(n > 1)

unicef_wide <- unicef_clean %>% pivot_wider(names_from = Uncertainty, values_from = Number)

Country_name <- unique(UNI$Country)
Unicef_name <- unique(unicef_wide$Country)

setdiff(Country_name, Unicef_name)
setdiff(Unicef_name, Country_name)

unicef_wide <- unicef_wide %>%
  mutate(
    Country = case_when(
      Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      Country == "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
      Country == "Egypt" ~ "Egypt, Arab Rep.",
      Country == "Gambia" ~ "Gambia, The",
      Country == "Iran (Islamic Republic of)" ~ "Iran, Islamic Rep.",
      Country == "Kyrgyzstan" ~ "Kyrgyz Republic",
      Country == "Lao People's Democratic Republic" ~ "Lao PDR",
      Country == "Republic of Moldova" ~ "Moldova",
      Country == "United Republic of Tanzania" ~ "Tanzania",
      TRUE ~ Country))

unicef_wide <- unicef_wide %>% filter(Country %in% Country_name)



fit_gamma_quantiles <- function(lower, median, upper) {
  obj_fun <- function(log_par) {
    shape <- exp(log_par[1]); scale <- exp(log_par[2])
    fitted <- qgamma(c(.025, .5, .975), shape = shape, scale = scale)
    sum((log(fitted) - log(c(lower, median, upper)))^2)
  }
  
  fit <- optim(log(c(10, median / 10)), obj_fun, method = "BFGS")
  c(shape = exp(fit$par[1]), scale = exp(fit$par[2]))
}

unicef_fit <- unicef_wide %>%
  rowwise() %>%
  mutate(
    pars = list(fit_gamma_quantiles(Lower, Median, Upper)),
    shape = pars["shape"],
    scale = pars["scale"]
  ) %>%
  ungroup()

set.seed(123)
n_draw <- 1000

unicef_draws <- sapply(seq_len(nrow(unicef_fit)), function(i)
  rgamma(n_draw, shape = unicef_fit$shape[i], scale = unicef_fit$scale[i]))

unicef_total_draw <- rowSums(unicef_draws)

quantile(unicef_total_draw, c(.025, .5, .975))

quantile(death_all_2023_LMIC, c(.025, .5, .975))


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



# 6. Summarize IHME under-5 draws ---------------------------------------------

IHME_under5_draw_matrix <- as.matrix(IHME.data_overall_pathogen_under5_count[, draw_cols])
dim(IHME_under5_draw_matrix)
IHME_under5_valid_rows <- rowSums(is.finite(IHME_under5_draw_matrix)) > 0

IHME_under5_val <- rep(NA_real_, nrow(IHME_under5_draw_matrix))
IHME_under5_lower <- rep(NA_real_, nrow(IHME_under5_draw_matrix))
IHME_under5_upper <- rep(NA_real_, nrow(IHME_under5_draw_matrix))

global_sum <- colSums(
  IHME_under5_draw_matrix[IHME_under5_valid_rows, , drop = FALSE],
  na.rm = TRUE
)

quantile(global_sum, c(0.5,0.025, 0.975))





