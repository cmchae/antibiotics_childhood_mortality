# _____________________________________________ ----------
# 5. Final CHAMPS outcomes + Table S2 + extrapolation validation ----------------

# 5-1. Country and syndrome definitions ----------------------------------------

df.analyze <- df.analyze %>%
  mutate(
    Country = case_when(site_iso_code == "ZA" ~ "South Africa", site_iso_code == "ML" ~ "Mali", site_iso_code == "MZ" ~ "Mozambique",
                        site_iso_code == "KE" ~ "Kenya", site_iso_code == "ET" ~ "Ethiopia", site_iso_code == "BD" ~ "Bangladesh",
                        site_iso_code == "SL" ~ "Sierra Leone", site_iso_code == "PK" ~ "Pakistan", TRUE ~ NA_character_),
    Year = as.integer(Year),
    malaria_death = !is.na(UC_champs_group_desc) & grepl("malaria", UC_champs_group_desc, ignore.case = TRUE),
    respiratory_death = LRTI == 1 | (!is.na(UC_champs_group_desc) & grepl("respir", UC_champs_group_desc, ignore.case = TRUE)),
    AFI_non_malaria_non_resp = as.integer(dplyr::coalesce(Fever == 1, FALSE) & !malaria_death & !respiratory_death)
  )

df.analyze$Year[df.analyze$Year %in% c(2024, 2025)] <- 2023
df.analyze$CountryYear <- paste(df.analyze$Country, df.analyze$Year, sep = "_")

stopifnot(nrow(df.analyze) == dplyr::n_distinct(df.analyze$champs_deid))


# 5-2. Underlying-cause pathogen flags for Table S2 ----------------------------

etiology_cols_table <- c("Underlying_Cause_Factor_etiol1", "Underlying_Cause_Factor_etiol2", "Underlying_Cause_Factor_etiol3")

etiology_table <- df.decode %>%
  dplyr::select(champs_deid, all_of(etiology_cols_table)) %>%
  mutate(across(all_of(etiology_cols_table), ~ ifelse(is.na(.x), NA_character_, gsub("\\s+", " ", trimws(.x)))))

# Names/order exactly corresponding to Table S2
bacterial_display <- c(
  any_bacterial = "Any bacterial cause", acinetobacter_baumannii = "Acinetobacter baumannii", acinetobacter_spp = "Acinetobacter spp.",
  aeromonas_spp = "Aeromonas spp.", bacillus_spp = "Bacillus spp.", bordetella_pertussis = "Bordetella pertussis",
  bordetella_parapertussis = "Bordetella parapertussis/B. bronchiseptica", bordetella_spp = "Bordetella spp.", brucella_spp = "Brucella spp.",
  campylobacter_coli = "Campylobacter coli", campylobacter_jejuni = "Campylobacter jejuni", chlamydia_trachomatis = "Chlamydia trachomatis",
  enterobacter_cloacae = "Enterobacter cloacae", enterococcus_faecium = "Enterococcus faecium", enterococcus_spp = "Enterococcus spp.",
  escherichia_coli = "Escherichia coli", haemophilus_influenzae = "Haemophilus influenzae", haemophilus_parainfluenzae = "Haemophilus parainfluenzae",
  klebsiella_pneumoniae = "Klebsiella pneumoniae", klebsiella_spp = "Klebsiella spp.", listeria_monocytogenes = "Listeria monocytogenes",
  moraxella_catarrhalis = "Moraxella catarrhalis", neisseria_meningitidis = "Neisseria meningitidis", orientia_tsutsugamushi = "Orientia tsutsugamushi",
  pseudomonas_aeruginosa = "Pseudomonas aeruginosa", proteus_mirabilis = "Proteus mirabilis",
  salmonella_paratyphi = "Salmonella enterica sub-species paratyphi", salmonella_typhi = "Salmonella enterica sub-species typhi",
  salmonella_spp = "Salmonella spp.", shigella_spp = "Shigella spp.", staphylococcus_aureus = "Staphylococcus aureus",
  staphylococcus_epidermidis = "Staphylococcus epidermidis", staphylococcus_spp = "Staphylococcus spp.",
  coagulase_negative_staphylococcus = "Coagulase-negative Staphylococcus", stenotrophomonas_maltophilia = "Stenotrophomonas maltophilia",
  streptococcus_agalactiae = "Streptococcus agalactiae", streptococcus_pneumoniae = "Streptococcus pneumoniae",
  streptococcus_pyogenes = "Streptococcus pyogenes", streptococcus_spp = "Streptococcus spp.", viridans_streptococcus = "Viridans-group Streptococcus"
)

viral_display <- c(
  adenovirus = "Adenovirus", astrovirus = "Astrovirus", cytomegalovirus = "Cytomegalovirus", dengue = "Dengue virus", enterovirus = "Enterovirus",
  hepatitis_c = "Hepatitis C virus", hsv1 = "Herpes simplex virus 1", hsv2 = "Herpes simplex virus 2", coronavirus_oc43 = "Human coronavirus 3 (OC43)",
  hiv = "Human immunodeficiency virus", hmpv = "Human metapneumovirus", influenza_a = "Influenza A virus", influenza_b = "Influenza B virus",
  lassa = "Lassa fever virus", measles = "Measles", nipah = "Nipah virus", norovirus_gi = "Norovirus GI", norovirus_gii = "Norovirus GII",
  parainfluenza_1 = "Parainfluenza virus type 1", parainfluenza_2 = "Parainfluenza virus type 2", parainfluenza_3 = "Parainfluenza virus type 3",
  parainfluenza_4 = "Parainfluenza virus type 4", parechovirus = "Parechovirus", parvovirus_b19 = "Parvovirus B19",
  rsv = "Respiratory syncytial virus", rhinovirus = "Rhinovirus", rotavirus_a = "Rotavirus A", rotavirus_nontypeable = "Non-typeable rotavirus",
  sapovirus_i_ii_iv = "Sapovirus I/II/IV", sapovirus_v = "Sapovirus V", varicella = "Varicella zoster virus"
)

fungal_display <- c(
  aspergillus = "Aspergillus spp.", candida_albicans = "Candida albicans", candida_glabrata = "Candida glabrata",
  candida_parapsilosis = "Candida parapsilosis", candida_tropicalis = "Candida tropicalis", candida_spp = "Candida spp.",
  pneumocystis = "Pneumocystis jirovecii"
)

parasitic_display <- c(ascaris = "Ascaris lumbricoides", giardia = "Giardia spp.")
other_display <- c(other_agent = "Any other agent")

display_map <- c(bacterial_display, viral_display, fungal_display, parasitic_display, other_display)

# Exact CHAMPS labels; most equal the display labels, with these exceptions/combined categories
table_pathogen_map <- as.list(display_map[names(display_map) != "any_bacterial"])
table_pathogen_map$bordetella_pertussis <- "B. pertussis"
table_pathogen_map$bordetella_parapertussis <- c("B. parapertussis", "Bordetella spp. (B. parapertussis/B. bronchiseptica)")
table_pathogen_map$escherichia_coli <- c("Escherichia coli", "Escherichia coli/Shigella spp.")
table_pathogen_map$haemophilus_influenzae <- c("Haemophilus influenzae", "Haemophilus influenzae Type A", "Haemophilus influenzae Type B")
table_pathogen_map$salmonella_paratyphi <- "Salmonella enterica sub-species paratyphi A"
table_pathogen_map$salmonella_typhi <- "Salmonella enterica sub-species typhi"
table_pathogen_map$shigella_spp <- c("Shigella", "Escherichia coli/Shigella spp.")
table_pathogen_map$coagulase_negative_staphylococcus <- "Coagulase negative Staphylococcus"
table_pathogen_map$viridans_streptococcus <- "Viridans streptococcus"
table_pathogen_map$adenovirus <- c("Adenovirus", "Adenovirus 40/41")
table_pathogen_map$cytomegalovirus <- "Cytomegalovirus (CMV)"
table_pathogen_map$dengue <- "Dengue (pan)"
table_pathogen_map$hepatitis_c <- "Hepatitis C"
table_pathogen_map$hsv1 <- "Herpes simplex virus 1 (HSV1)"
table_pathogen_map$hsv2 <- "Herpes simplex virus 2 (HSV2)"
table_pathogen_map$hiv <- "Human Immunodeficiency Virus (HIV)"
table_pathogen_map$hmpv <- "Human metapneumovirus (HMPV)"
table_pathogen_map$influenza_a <- "Influenza A"
table_pathogen_map$influenza_b <- "Influenza B"
table_pathogen_map$lassa <- "Lassa Fever Virus"
table_pathogen_map$rsv <- "Respiratory syncytial virus (RSV)"
table_pathogen_map$rotavirus_nontypeable <- "Rotavirus non-typable"
table_pathogen_map$other_agent <- "Other Etiology/Agent"

# Comprehensive bacterial list for "Any bacterial cause"
bacterial_values_all <- c(
  "Acinetobacter spp.", "Acinetobacter baumannii", "Aeromonas spp.", "B. parapertussis", "B. pertussis",
  "Bordetella spp. (B. parapertussis/B. bronchiseptica)", "Bacillus spp.", "Bordetella spp.", "Brucella spp.",
  "Campylobacter coli", "Campylobacter jejuni", "Chlamydia trachomatis", "Coagulase negative Staphylococcus",
  "Corynebacterium diptheriae", "Enterobacter cloacae", "Enterococcus faecalis", "Enterococcus faecium", "Enterococcus spp.",
  "Escherichia coli", "Escherichia coli/Shigella spp.", "Haemophilus influenzae", "Haemophilus influenzae Type A",
  "Haemophilus influenzae Type B", "Haemophilus parainfluenzae", "Klebsiella pneumoniae", "Klebsiella spp.", "Klebsiella terragina",
  "Listeria monocytogenes", "Moraxella catarrhalis", "Mycobacterium tuberculosis", "Neisseria meningitidis",
  "Orientia tsutsugamushi", "Proteus mirabilis", "Pseudomonas aeruginosa", "Salmonella enterica sub-species paratyphi A",
  "Salmonella enterica sub-species typhi", "Salmonella spp.", "Shigella", "Staphylococcus aureus", "Staphylococcus epidermidis",
  "Staphylococcus spp.", "Stenotrophomonas maltophilia", "Streptococcus agalactiae", "Streptococcus pneumoniae",
  "Streptococcus pyogenes", "Streptococcus spp.", "Treponema pallidum", "Ureaplasma spp.", "Vibrio cholerae", "Viridans streptococcus"
)

pathogen_flags <- etiology_table %>% dplyr::select(champs_deid)
pathogen_flags$any_bacterial <- calc_presence(etiology_table, etiology_cols_table, bacterial_values_all)

for (p_name in names(table_pathogen_map)) pathogen_flags[[p_name]] <- calc_presence(etiology_table, etiology_cols_table, table_pathogen_map[[p_name]])

flag_cols <- c("any_bacterial", names(table_pathogen_map))
df.analyze <- df.analyze %>% left_join(pathogen_flags, by = "champs_deid") %>%
  mutate(across(all_of(flag_cols), ~ dplyr::coalesce(as.integer(.x), 0L)),
         bact_underlying_LRTI = as.integer(LRTI == 1 & any_bacterial == 1),
         bact_underlying_fever = as.integer(AFI_non_malaria_non_resp == 1 & any_bacterial == 1))


# 5-3. Table S2 ----------------------------------------------------------------

table_order <- tibble(
  Category = c(rep("Bacterial causes", length(bacterial_display)), rep("Viral causes", length(viral_display)),
               rep("Fungal infections", length(fungal_display)), rep("Parasitic infections", length(parasitic_display)),
               rep("Other etiology/agent", length(other_display))),
  key = names(display_map), Pathogen = unname(display_map)
)

denom_all <- nrow(df.analyze)
denom_lrti <- sum(df.analyze$LRTI == 1, na.rm = TRUE)
denom_afi <- sum(df.analyze$AFI_non_malaria_non_resp == 1, na.rm = TRUE)

table_S2_list <- vector("list", nrow(table_order))

for (i in seq_len(nrow(table_order))) {
  key_i <- table_order$key[i]
  flag_i <- df.analyze[[key_i]] == 1
  n_all_i <- sum(flag_i, na.rm = TRUE)
  n_lrti_i <- sum(flag_i & df.analyze$LRTI == 1, na.rm = TRUE)
  n_afi_i <- sum(flag_i & df.analyze$AFI_non_malaria_non_resp == 1, na.rm = TRUE)
  
  table_S2_list[[i]] <- tibble(
    Category = table_order$Category[i], Pathogen = table_order$Pathogen[i],
    n_all = n_all_i, pct_all = 100 * n_all_i / denom_all,
    n_LRTI = n_lrti_i, pct_LRTI = 100 * n_lrti_i / denom_lrti,
    n_AFI = n_afi_i, pct_AFI = 100 * n_afi_i / denom_afi,
    `All decedents analyzed` = sprintf("%d (%.1f%%)", n_all_i, 100 * n_all_i / denom_all),
    `Decedents with LRTI` = sprintf("%d (%.1f%%)", n_lrti_i, 100 * n_lrti_i / denom_lrti),
    `Decedents with AFI without malaria or respiratory-attributed death` = sprintf("%d (%.1f%%)", n_afi_i, 100 * n_afi_i / denom_afi)
  )
}

table_S2 <- bind_rows(table_S2_list)
table_S2_denominators <- tibble(group = c("All decedents analyzed", "Decedents with LRTI", "AFI without malaria or respiratory-attributed death"),
                                n = c(denom_all, denom_lrti, denom_afi))

print(table_S2_denominators)
print(table_S2, n = Inf)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(table_S2, "outputs/tables/Table_S2_CHAMPS_pathogens.csv", row.names = FALSE)
write.csv(table_S2_denominators, "outputs/tables/Table_S2_CHAMPS_denominators.csv", row.names = FALSE)


# 5-4. Aggregate bacterial deaths for extrapolation ----------------------------

UNI <- readRDS("outputs/pipelines/pc_gdp_2023_mortality.rds")

death_agg <- df.analyze %>%
  group_by(Country, age_group) %>%
  summarise(total_deaths = n(),
            bact_LRTI_deaths = sum(bact_underlying_LRTI, na.rm = TRUE),
            bact_fever_deaths = sum(bact_underlying_fever, na.rm = TRUE), .groups = "drop") %>%
  mutate(other_deaths_LRTI = total_deaths - bact_LRTI_deaths,
         other_deaths_fever = total_deaths - bact_fever_deaths,
         observed_prop_bact_LRTI = bact_LRTI_deaths / total_deaths,
         observed_prop_bact_fever = bact_fever_deaths / total_deaths)

champs_reference_year <- c("Bangladesh" = 2022, "Ethiopia" = 2023, "Kenya" = 2021, "Mali" = 2021, "Mozambique" = 2021, "Sierra Leone" = 2022, "South Africa" = 2020)

death_agg <- death_agg %>%
  mutate(Year = as.integer(champs_reference_year[Country])) %>%
  inner_join(UNI %>% dplyr::select(Country, Year, PC1, PC2), by = c("Country", "Year"))

age_levels <- c("Age_0", "Age_1", "Age_2to4")
death_agg <- death_agg %>% mutate(age_group = factor(age_group, levels = age_levels))

stopifnot(!anyNA(death_agg$PC1), !anyNA(death_agg$PC2))
write.csv(death_agg, "outputs/pipelines/champs_table.csv", row.names = FALSE)


# 5-5. Small prespecified leave-one-country-out validation ----------------------
# Do NOT use a large model search with only seven CHAMPS countries.

candidate_rhs <- c(age_only = "age_group", PC1_age = "PC1 + age_group", PC1_PC2_age = "PC1 + PC2 + age_group")
champs_validation_list <- list()
validation_index_i <- 1

for (outcome_i in c("LRTI", "AFI")) {
  response_i <- if (outcome_i == "LRTI") "cbind(bact_LRTI_deaths, other_deaths_LRTI)" else "cbind(bact_fever_deaths, other_deaths_fever)"
  success_col_i <- if (outcome_i == "LRTI") "bact_LRTI_deaths" else "bact_fever_deaths"
  
  for (model_i in names(candidate_rhs)) {
    formula_i <- as.formula(paste(response_i, "~", candidate_rhs[model_i]))
    
    for (country_i in sort(unique(death_agg$Country))) {
      train_i <- death_agg %>% filter(Country != country_i)
      test_i <- death_agg %>% filter(Country == country_i)
      fit_i <- try(glm(formula_i, family = binomial(link = "logit"), data = train_i), silent = TRUE)
      if (inherits(fit_i, "try-error")) next
      
      pred_i <- try(as.numeric(predict(fit_i, newdata = test_i, type = "response")), silent = TRUE)
      if (inherits(pred_i, "try-error") || any(!is.finite(pred_i))) next
      pred_i <- pmin(pmax(pred_i, 1e-8), 1 - 1e-8)
      
      observed_i <- test_i[[success_col_i]] / test_i$total_deaths
      nll_i <- -sum(dbinom(test_i[[success_col_i]], size = test_i$total_deaths, prob = pred_i, log = TRUE))
      brier_i <- weighted.mean((observed_i - pred_i)^2, w = test_i$total_deaths)
      
      champs_validation_list[[validation_index_i]] <- tibble(outcome = outcome_i, model_key = model_i, heldout_country = country_i,
                                                              n_deaths = sum(test_i$total_deaths), negative_log_likelihood = nll_i,
                                                              nll_per_death = nll_i / sum(test_i$total_deaths), weighted_brier = brier_i)
      validation_index_i <- validation_index_i + 1
    }
  }
}

champs_validation_results <- bind_rows(champs_validation_list)

champs_validation_summary <- champs_validation_results %>%
  group_by(outcome, model_key) %>%
  summarise(n_countries = n(), total_n_deaths = sum(n_deaths),
            mean_nll_per_death = weighted.mean(nll_per_death, w = n_deaths),
            mean_weighted_brier = weighted.mean(weighted_brier, w = n_deaths), .groups = "drop") %>%
  group_by(outcome) %>%
  mutate(nll_rank = rank(mean_nll_per_death, ties.method = "min"),
         brier_rank = rank(mean_weighted_brier, ties.method = "min"),
         combined_rank = nll_rank + brier_rank) %>%
  ungroup() %>% arrange(outcome, combined_rank, mean_nll_per_death)

print(champs_validation_summary, n = Inf)
write.csv(champs_validation_results, "outputs/tables/CHAMPS_bacterial_fraction_LOCO_validation_results.csv", row.names = FALSE)
write.csv(champs_validation_summary, "outputs/tables/CHAMPS_bacterial_fraction_LOCO_validation_summary.csv", row.names = FALSE)


# 5-6. Current prespecified extrapolation models --------------------------------
# Keep these until the LOCO results are reviewed; do not automatically change
# the primary model based on tiny differences among only seven countries.

mod_bact_LRTI <- glm(cbind(bact_LRTI_deaths, other_deaths_LRTI) ~ PC1 + PC2 + age_group,
                     family = binomial(link = "logit"), data = death_agg)

mod_bact_fever <- glm(cbind(bact_fever_deaths, other_deaths_fever) ~ PC1 + PC2 + age_group,
                      family = binomial(link = "logit"), data = death_agg)

summary(mod_bact_LRTI)
summary(mod_bact_fever)

UNI_pred <- bind_rows(UNI %>% mutate(age_group = "Age_0"),
                      UNI %>% mutate(age_group = "Age_1"),
                      UNI %>% mutate(age_group = "Age_2to4")) %>%
  mutate(age_group = factor(age_group, levels = age_levels))

X_pred <- model.matrix(~ PC1 + PC2 + age_group, data = UNI_pred)










df.decode_check <- df.decode %>% dplyr::select(Immediate_Cause_of_Death_etiol1, Immediate_Cause_of_Death_etiol2, Immediate_Cause_of_Death_etiol3, Underlying_Cause_Factor_etiol1, Underlying_Cause_Factor_etiol2, Underlying_Cause_Factor_etiol3)

table(df.decode_check$Immediate_Cause_of_Death_etiol3, useNA = "always")

df.decode <- df.decode %>%
  mutate(microbial_cause = ifelse(
    !is.na(Immediate_Cause_of_Death_etiol1) |
      !is.na(Immediate_Cause_of_Death_etiol2) |
      !is.na(Immediate_Cause_of_Death_etiol3) |
      !is.na(Underlying_Cause_Factor_etiol1) |
      !is.na(Underlying_Cause_Factor_etiol2) |
      !is.na(Underlying_Cause_Factor_etiol3),
    1, 0
  ))

df.decode <- df.decode %>%
  mutate(microbial_cause = ifelse(
    !is.na(Immediate_Cause_of_Death_etiol1) |
      !is.na(Immediate_Cause_of_Death_etiol2) |
      !is.na(Immediate_Cause_of_Death_etiol3) |
      !is.na(Underlying_Cause_Factor_etiol1) |
      !is.na(Underlying_Cause_Factor_etiol2) |
      !is.na(Underlying_Cause_Factor_etiol3),
    1, 0
  ))














table(df.decode$microbial_cause)
sum(df.decode$microbial_cause)