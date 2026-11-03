#_______________________________________________=========
# 1. (Barnards) data preparation ===================================

library(dplyr)
library(tidyr)

rm(list = ls())
barnards.dir <- "data/BARNARDS"

epi <- read.csv(file.path(barnards.dir,"barnards_epi_06102020.csv")) %>% dplyr::select("ANON_MOTHERID", "ANON_BABYID", "DATEOFADMISSIONINTOHOSPIT", "BABYDOB", "OUTCOME", "DATEATINFANTOUTCOME", "AGEATOUTCOMEDAYS", "CLINICASSUM", "NECULTUCFIRM", "CLINDATE1", "CULTDATE", "TIMETOCLINSEPSIS", "RFINBORNOUTBORN")

abx <- read.csv(file.path(barnards.dir, "abx.csv")); colnames(abx) <- toupper(colnames(abx))
abx <- abx %>% dplyr::select("COUNTRY", "CONTINENT", "ANON_MOTHERID", "ANON_BABYID", "ABX1", "DATE1", "COMBINATION1", "ABX2", "DATE2", "COMBINATION2", "ABX3", "DATE3", "COMBINATION3", "ABX4", "DATE4", "COMBINATION4", "ABX5", "DATE5", "COMBINATION5", "ABX6", "DATE6", "COMBINATION6", "NORFLOXACIN6", "NYSTATINT", "BACITRACINNEOMYCINT", "MUPIROCINT", "CEFPODOXIME6", "CEFRADINET", "CEFALEXINT", "CEFIXIMET", "TRIMETHOPRIMSULFAMETHOXAZOLET", "ERYTHROMYCINT", "CHLORAMPHENICOLT", "LOMEFLOXACINT", "NETILMICINT", "TARGOPLANINT", "OFLOXACINT", "FLUCONAZOLET", "CLARITHROMYCINT", "CEFOPERAZONET", "METRONIDAZOLET", "CEFUROXIMET", "AMPICILLINT", "CLOXACILLINT", "CLINDAMYCINT", "AMOXICILLINCLAVULANATET", "PIPERACILLINTAZOBACTAMT", "CEFTRIAXONE6", "CEFOTAXIME6", "CEFTAZIDIME6", "CEFEPIME6", "IMIPENEM6", "MEROPENEM6", "ERTAPENEM6", "AZTREONAM6", "GENTAMICIN6", "AMIKACIN6", "TOBRAMYCIN6", "TIGECYCLINE6", "MINOCYCLINE6", "FOSFOMYCIN6", "LEVOFLOXACIN6", "CIPROFLOXACIN6", "COLISTIN6", "OXACILLIN6", "FLUCLOXACILLIN6", "RIFAMPICIN6", "VANCOMYCIN6", "AZITHROMYCIN6", "LINEZOLIDE6", "BENZYLPENICILLIN6")

abx2 <- abx

gpb_mic <- read.csv(file.path(barnards.dir,"sepsis_isolate_gpbspecies_mic.csv"))
gnb_mic <- read.csv(file.path(barnards.dir,"sepsis_isolate_gnbspecies_mic.csv"))

dim(epi)
dim(abx)
dim(gpb_mic)
dim(gnb_mic)

colnames(abx)
colnames(epi)
colnames(gpb_mic)
colnames(gnb_mic)

abx_vars <- c(
  "NORFLOXACIN6", "NYSTATINT", "BACITRACINNEOMYCINT", "MUPIROCINT",
  "CEFPODOXIME6", "CEFRADINET", "CEFALEXINT", "CEFIXIMET",
  "TRIMETHOPRIMSULFAMETHOXAZOLET", "ERYTHROMYCINT", "CHLORAMPHENICOLT",
  "LOMEFLOXACINT", "NETILMICINT", "TARGOPLANINT", "OFLOXACINT",
  "FLUCONAZOLET", "CLARITHROMYCINT", "CEFOPERAZONET", "METRONIDAZOLET",
  "CEFUROXIMET", "AMPICILLINT", "CLOXACILLINT", "CLINDAMYCINT",
  "AMOXICILLINCLAVULANATET", "PIPERACILLINTAZOBACTAMT",
  "CEFTRIAXONE6", "CEFOTAXIME6", "CEFTAZIDIME6", "CEFEPIME6",
  "IMIPENEM6", "MEROPENEM6", "ERTAPENEM6", "AZTREONAM6",
  "GENTAMICIN6", "AMIKACIN6", "TOBRAMYCIN6", "TIGECYCLINE6",
  "MINOCYCLINE6", "FOSFOMYCIN6", "LEVOFLOXACIN6", "CIPROFLOXACIN6",
  "COLISTIN6", "OXACILLIN6", "FLUCLOXACILLIN6", "RIFAMPICIN6",
  "VANCOMYCIN6", "AZITHROMYCIN6", "LINEZOLIDE6", "BENZYLPENICILLIN6"
)

antifungal_vars <- c(
  "NYSTATINT",
  "FLUCONAZOLET"
)

topical_vars <- c(
  "BACITRACINNEOMYCINT",
  "MUPIROCINT"
)

antibacterial_vars <- setdiff(
  abx_vars,
  c(antifungal_vars, topical_vars)
)

# Y/N -> 1/0
abx <- abx %>%
  mutate(
    across(
      all_of(abx_vars),
      ~ case_when(
        .x == "Y" ~ 1L,
        .x == "N" ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  )

# Antibiotics actually used at least once
used_antibacterial_vars <- antibacterial_vars[
  colSums(abx[antibacterial_vars] == 1, na.rm = TRUE) > 0
]

# Antibiotics never used
unused_antibacterial_vars <- setdiff(
  antibacterial_vars,
  used_antibacterial_vars
)

# and remove agents not included in the systemic antibiotic analysis
abx <- abx %>%
  select(-all_of(c(antifungal_vars, topical_vars, unused_antibacterial_vars)))


abx <- abx %>%
  rename(
    ABX_CEFPODOXIME       = CEFPODOXIME6,
    ABX_CEFRADINE         = CEFRADINET,
    ABX_CEFALEXIN         = CEFALEXINT,
    ABX_CEFIXIME          = CEFIXIMET,
    ABX_TMP_SMX           = TRIMETHOPRIMSULFAMETHOXAZOLET,
    ABX_ERYTHROMYCIN      = ERYTHROMYCINT,
    ABX_CHLORAMPHENICOL   = CHLORAMPHENICOLT,
    ABX_LOMEFLOXACIN      = LOMEFLOXACINT,
    ABX_NETILMICIN        = NETILMICINT,
    ABX_TARGOPLANIN       = TARGOPLANINT,
    ABX_OFLOXACIN         = OFLOXACINT,
    ABX_CLARITHROMYCIN    = CLARITHROMYCINT,
    ABX_CEFOPERAZONE      = CEFOPERAZONET,
    ABX_METRONIDAZOLE     = METRONIDAZOLET,
    ABX_CEFUROXIME        = CEFUROXIMET,
    
    ABX_AMP               = AMPICILLINT,
    ABX_CLOXACILLIN       = CLOXACILLINT,
    ABX_CLINDAMYCIN       = CLINDAMYCINT,
    ABX_AMC               = AMOXICILLINCLAVULANATET,
    ABX_TZP               = PIPERACILLINTAZOBACTAMT,
    ABX_CRO               = CEFTRIAXONE6,
    ABX_CTX               = CEFOTAXIME6,
    ABX_CAZ               = CEFTAZIDIME6,
    ABX_FEP               = CEFEPIME6,
    ABX_IPM               = IMIPENEM6,
    ABX_MEM               = MEROPENEM6,
    ABX_ETP               = ERTAPENEM6,
    ABX_GEN               = GENTAMICIN6,
    ABX_AMK               = AMIKACIN6,
    ABX_TOB               = TOBRAMYCIN6,
    ABX_TGC               = TIGECYCLINE6,
    ABX_LVX               = LEVOFLOXACIN6,
    ABX_CIP               = CIPROFLOXACIN6,
    ABX_CST               = COLISTIN6,
    
    ABX_FLUCLOX           = FLUCLOXACILLIN6,
    ABX_VAN               = VANCOMYCIN6,
    ABX_AZM               = AZITHROMYCIN6,
    ABX_LZD               = LINEZOLIDE6,
    ABX_BENZYLPENICILLIN  = BENZYLPENICILLIN6
  )

no_ast_abx <- c(
  "ABX_CEFPODOXIME",
  "ABX_CEFRADINE",
  "ABX_CEFALEXIN",
  "ABX_CEFIXIME",
  "ABX_TMP_SMX",
  "ABX_ERYTHROMYCIN",
  "ABX_CHLORAMPHENICOL",
  "ABX_LOMEFLOXACIN",
  "ABX_NETILMICIN",
  "ABX_TARGOPLANIN",
  "ABX_CLARITHROMYCIN",
  "ABX_CEFOPERAZONE",
  "ABX_METRONIDAZOLE",
  "ABX_CEFUROXIME",
  "ABX_CLOXACILLIN",
  "ABX_CLINDAMYCIN",
  "ABX_BENZYLPENICILLIN"
)


abx_ast_map <- tibble::tribble(
  ~drug,          ~abx_var,       ~gnb_ast, ~gpb_ast,
  "Ampicillin",   "ABX_AMP",      "AMP",     "AMP2",
  "Amoxicillin-clavulanate", "ABX_AMC", "AMC", NA_character_,
  "Piperacillin-tazobactam", "ABX_TZP", "TZP", NA_character_,
  "Ceftriaxone",  "ABX_CRO",      "CRO",     NA_character_,
  "Cefotaxime",   "ABX_CTX",      "CTX",     NA_character_,
  "Ceftazidime",  "ABX_CAZ",      "CAZ",     NA_character_,
  "Cefepime",     "ABX_FEP",      "FEP",     NA_character_,
  "Imipenem",     "ABX_IPM",      "IPM",     NA_character_,
  "Meropenem",    "ABX_MEM",      "MEM",     NA_character_,
  "Ertapenem",    "ABX_ETP",      "ETP",     NA_character_,
  "Gentamicin",   "ABX_GEN",      "GEN",     "GEN2",
  "Amikacin",     "ABX_AMK",      "AMK",     "AMK2",
  "Tobramycin",   "ABX_TOB",      "TOB",     "TOB2",
  "Tigecycline",  "ABX_TGC",      "TGC",     "TGC2",
  "Minocycline",  "ABX_MIN",      "MIN",     "MIN2",
  "Levofloxacin", "ABX_LVX",      "LVX",     "LVX2",
  "Ciprofloxacin","ABX_CIP",      "CIP",     "CIP2",
  "Colistin",     "ABX_CST",      "CST",     NA_character_,
  "Flucloxacillin","ABX_FLUCLOX", NA_character_, "FLUCLOX2",
  "Vancomycin",   "ABX_VAN",      NA_character_, "VAN2",
  "Azithromycin", "ABX_AZM",      NA_character_, "AZM2",
  "Linezolid",    "ABX_LZD",      NA_character_, "LZD2"
)

intersect(colnames(abx),colnames(epi))
abx_epi <- abx %>% left_join(epi, by = c("ANON_MOTHERID", "ANON_BABYID"))









#_______________________________________________=========
# 2. (Barnards) proportion of antibiotics use by continent ===================================

abx_epi <- abx_epi %>%
  mutate(
    abx_beta_lactam = if_any(
      c(ABX_AMP, ABX_BENZYLPENICILLIN,
        ABX_CLOXACILLIN, ABX_FLUCLOX,
        ABX_AMC, ABX_TZP,
        ABX_CEFRADINE, ABX_CEFALEXIN, ABX_CEFUROXIME,
        ABX_CEFPODOXIME, ABX_CEFIXIME, ABX_CEFOPERAZONE,
        ABX_CRO, ABX_CTX, ABX_CAZ, ABX_FEP,
        ABX_IPM, ABX_MEM, ABX_ETP),
      ~ .x == 1
    ),
    
    abx_aminoglycoside = if_any(
      c(ABX_GEN, ABX_AMK, ABX_TOB, ABX_NETILMICIN),
      ~ .x == 1
    ),
    
    abx_quinolone = if_any(
      c(ABX_LOMEFLOXACIN, ABX_OFLOXACIN, ABX_LVX, ABX_CIP),
      ~ .x == 1
    ),
    
    abx_cycline = ABX_TGC == 1,
    
    abx_glycopeptide = if_any(
      c(ABX_VAN, ABX_TARGOPLANIN),
      ~ .x == 1
    ),
    
    abx_macrolide = if_any(
      c(ABX_ERYTHROMYCIN, ABX_CLARITHROMYCIN, ABX_AZM),
      ~ .x == 1
    ),
    
    abx_oxazolidinone  = ABX_LZD == 1,
    abx_polymyxin      = ABX_CST == 1,
    abx_lincosamide    = ABX_CLINDAMYCIN == 1,
    abx_tmp_smx        = ABX_TMP_SMX == 1,
    abx_phenicol       = ABX_CHLORAMPHENICOL == 1,
    abx_nitroimidazole = ABX_METRONIDAZOLE == 1
  )


abx_epi <- abx_epi %>%
  mutate(
    abx_penicillin = if_any(
      c(ABX_AMP, ABX_BENZYLPENICILLIN),
      ~ .x == 1
    ),
    
    abx_antistaph_penicillin = if_any(
      c(ABX_CLOXACILLIN, ABX_FLUCLOX),
      ~ .x == 1
    ),
    
    abx_bl_bli = if_any(
      c(ABX_AMC, ABX_TZP),
      ~ .x == 1
    ),
    
    abx_cephalosporin = if_any(
      c(ABX_CEFPODOXIME, ABX_CEFRADINE, ABX_CEFALEXIN,
        ABX_CEFIXIME, ABX_CEFOPERAZONE, ABX_CEFUROXIME,
        ABX_CRO, ABX_CTX, ABX_CAZ, ABX_FEP),
      ~ .x == 1
    ),
    
    abx_carbapenem = if_any(
      c(ABX_IPM, ABX_MEM, ABX_ETP),
      ~ .x == 1
    )
  )

abx_epi





# Add umbrella for the remaining "other" systemic antibacterials
abx_epi <- abx_epi %>%
  mutate(
    abx_other_systemic = if_any(
      c(ABX_CLINDAMYCIN, ABX_TMP_SMX,
        ABX_CHLORAMPHENICOL, ABX_METRONIDAZOLE),
      ~ .x == 1
    )
  )


abx_order <- tibble::tribble(
  ~variable,                  ~antibiotic,                                ~level, ~order,
  
  "abx_beta_lactam",          "β-lactams",                                     1,      1,
  
  "abx_penicillin",           "Penicillins",                                   2,      2,
  "ABX_AMP",                  "Ampicillin",                                     3,      3,
  "ABX_BENZYLPENICILLIN",     "Benzylpenicillin",                               3,      4,
  
  "abx_antistaph_penicillin", "Anti-staphylococcal penicillins",                2,      5,
  "ABX_CLOXACILLIN",          "Cloxacillin",                                    3,      6,
  "ABX_FLUCLOX",              "Flucloxacillin",                                 3,      7,
  
  "abx_bl_bli",               "β-lactam/β-lactamase inhibitors",                2,      8,
  "ABX_AMC",                  "Amoxicillin-clavulanate",                         3,      9,
  "ABX_TZP",                  "Piperacillin-tazobactam",                         3,     10,
  
  "abx_cephalosporin",        "Cephalosporins",                                 2,     11,
  "ABX_CEFRADINE",            "Cefradine",                                      3,     12,
  "ABX_CEFALEXIN",            "Cefalexin",                                      3,     13,
  "ABX_CEFUROXIME",           "Cefuroxime",                                     3,     14,
  "ABX_CEFPODOXIME",          "Cefpodoxime",                                    3,     15,
  "ABX_CEFIXIME",             "Cefixime",                                       3,     16,
  "ABX_CEFOPERAZONE",         "Cefoperazone",                                   3,     17,
  "ABX_CRO",                  "Ceftriaxone",                                     3,     18,
  "ABX_CTX",                  "Cefotaxime",                                      3,     19,
  "ABX_CAZ",                  "Ceftazidime",                                     3,     20,
  "ABX_FEP",                  "Cefepime",                                        3,     21,
  
  "abx_carbapenem",           "Carbapenems",                                    2,     22,
  "ABX_IPM",                  "Imipenem",                                        3,     23,
  "ABX_MEM",                  "Meropenem",                                       3,     24,
  "ABX_ETP",                  "Ertapenem",                                       3,     25,
  
  "abx_aminoglycoside",       "Aminoglycosides",                                1,     26,
  "ABX_GEN",                  "Gentamicin",                                      3,     27,
  "ABX_AMK",                  "Amikacin",                                        3,     28,
  "ABX_TOB",                  "Tobramycin",                                      3,     29,
  "ABX_NETILMICIN",           "Netilmicin",                                      3,     30,
  
  "abx_quinolone",            "Quinolones",                                     1,     31,
  "ABX_LOMEFLOXACIN",         "Lomefloxacin",                                    3,     32,
  "ABX_OFLOXACIN",            "Ofloxacin",                                       3,     33,
  "ABX_LVX",                  "Levofloxacin",                                    3,     34,
  "ABX_CIP",                  "Ciprofloxacin",                                   3,     35,
  
  "abx_glycopeptide",         "Glycopeptides",                                  1,     36,
  "ABX_VAN",                  "Vancomycin",                                      3,     37,
  "ABX_TARGOPLANIN",          "Targoplanin",                                     3,     38,
  
  "abx_macrolide",            "Macrolides",                                     1,     39,
  "ABX_ERYTHROMYCIN",         "Erythromycin",                                    3,     40,
  "ABX_CLARITHROMYCIN",       "Clarithromycin",                                  3,     41,
  "ABX_AZM",                  "Azithromycin",                                     3,     42,
  
  "abx_cycline",              "Cyclines",                                       1,     43,
  "ABX_TGC",                  "Tigecycline",                                     3,     44,
  
  "abx_oxazolidinone",        "Oxazolidinones",                                 1,     45,
  "ABX_LZD",                  "Linezolid",                                       3,     46,
  
  "abx_polymyxin",            "Polymyxins",                                     1,     47,
  "ABX_CST",                  "Colistin",                                        3,     48,
  
  "abx_other_systemic",       "Other systemic antibacterials",                  1,     49,
  "ABX_CLINDAMYCIN",          "Clindamycin",                                     3,     50,
  "ABX_TMP_SMX",              "Trimethoprim-sulfamethoxazole",                   3,     51,
  "ABX_CHLORAMPHENICOL",      "Chloramphenicol",                                 3,     52,
  "ABX_METRONIDAZOLE",        "Metronidazole",                                   3,     53
)





# Number of children by country
n_children_row <- abx_epi %>%
  group_by(COUNTRY, CONTINENT) %>%
  summarize(
    n = n_distinct(ANON_BABYID),
    .groups = "drop"
  ) %>%
  mutate(
    country = paste(CONTINENT, COUNTRY, sep = "_"),
    level = 0,
    antibiotic = "Number of children"
  ) %>%
  select(level, antibiotic, country, n) %>%
  pivot_wider(
    names_from = country,
    values_from = n,
    values_fill = 0
  )


# Summarize every individual drug, subclass, and umbrella class
abx_epi_summary_flip <- abx_epi %>%
  select(
    COUNTRY, CONTINENT,
    all_of(abx_order$variable)
  ) %>%
  pivot_longer(
    cols = -c(COUNTRY, CONTINENT),
    names_to = "variable",
    values_to = "used"
  ) %>%
  group_by(COUNTRY, CONTINENT, variable) %>%
  summarize(
    n = sum(used == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(abx_order, by = "variable") %>%
  mutate(
    country = paste(CONTINENT, COUNTRY, sep = "_")
  ) %>%
  select(order, level, antibiotic, country, n) %>%
  pivot_wider(
    names_from = country,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(order) %>%
  select(-order)

abx_epi_summary_country <- rbind(n_children_row, abx_epi_summary_flip)
print(abx_epi_summary_country, n = Inf)

abx_epi_summary_continent_flip <- abx_epi_summary_flip %>%
  mutate(
    Asia = rowSums(across(starts_with("Asia_")), na.rm = TRUE),
    Africa = rowSums(across(starts_with("Africa_")), na.rm = TRUE)
  ) %>%
  select(level, antibiotic, Asia, Africa)

n_children_row <- abx_epi %>%
  group_by(CONTINENT) %>%
  summarize(
    n = n_distinct(ANON_BABYID),
    .groups = "drop"
  ) %>%
  mutate(
    CONTINENT = paste(CONTINENT),
    level = 0,
    antibiotic = "Number of children"
  ) %>%
  select(level, antibiotic, CONTINENT, n) %>%
  pivot_wider(
    names_from = CONTINENT,
    values_from = n,
    values_fill = 0
  )

abx_epi_summary_continent <- rbind(n_children_row, abx_epi_summary_continent_flip)

abx_summary <- cbind(abx_epi_summary_continent, abx_epi_summary_country[,-c(1,2)])
dir.create("outputs/pipelines/africa_asia/barnards", recursive = TRUE, showWarnings = FALSE)
barnards.output <- "outputs/pipelines/africa_asia/barnards"
write.csv(abx_summary, file.path(barnards.output,"abx_proportion.csv"))



#_______________________________________________
# BARNARDS denominator QC

barnards_abx_qc <- abx2 %>%
  mutate(
    across(all_of(abx_vars), ~ case_when(
      .x == "Y" ~ 1L,
      .x == "N" ~ 0L,
      TRUE ~ NA_integer_
    )),
    any_systemic_abx = if_any(all_of(antibacterial_vars), ~ .x == 1)
  )

tibble(
  n_epi = epi %>% distinct(ANON_MOTHERID, ANON_BABYID) %>% nrow(),
  n_abx_file = barnards_abx_qc %>% distinct(ANON_MOTHERID, ANON_BABYID) %>% nrow(),
  n_any_systemic_abx = barnards_abx_qc %>%
    filter(any_systemic_abx) %>%
    distinct(ANON_MOTHERID, ANON_BABYID) %>%
    nrow(),
  n_no_systemic_abx = barnards_abx_qc %>%
    filter(!any_systemic_abx | is.na(any_systemic_abx)) %>%
    distinct(ANON_MOTHERID, ANON_BABYID) %>%
    nrow()
)

epi %>%
  distinct(ANON_MOTHERID, ANON_BABYID) %>%
  anti_join(
    barnards_abx_qc %>% distinct(ANON_MOTHERID, ANON_BABYID),
    by = c("ANON_MOTHERID", "ANON_BABYID")
  ) %>%
  summarise(n_not_in_abx_file = n())

#_______________________________________________=========
# 3. (Barnards) proportion of effective treatment by continent ===================================
# Neonate
# ↓
# Detected isolate(s)
# ↓
# Antibiotics actually administered
# ↓
# Exact BARNARDS AST for each administered drug
# ↓
# S = active
# I/R = inactive
# missing/ND/no exact AST = unknown
# ↓
# Isolate:
#   any S → covered
# no S + any unknown → indeterminate
# otherwise → not covered
# ↓
# Neonate:
#   all isolates covered → fully covered
# any isolate not covered → not fully covered
# otherwise → indeterminate


head(gpb_mic)
colnames(gpb_mic)
colnames(gnb_mic)

intersect(colnames(abx2),colnames(gpb_mic))
abx_gpb <- abx %>% left_join(epi, by = c("ANON_MOTHERID", "ANON_BABYID"))
abx_gnb <- abx %>% left_join(epi, by = c("ANON_MOTHERID", "ANON_BABYID"))

gnb_ast_map <- c(
  ABX_AMP = "AMP",
  ABX_AMC = "AMC",
  ABX_TZP = "TZP",
  ABX_CRO = "CRO",
  ABX_CTX = "CTX",
  ABX_CAZ = "CAZ",
  ABX_FEP = "FEP",
  ABX_IPM = "IPM",
  ABX_MEM = "MEM",
  ABX_ETP = "ETP",
  ABX_GEN = "GEN",
  ABX_AMK = "AMK",
  ABX_TOB = "TOB",
  ABX_TGC = "TGC",
  ABX_LVX = "LVX",
  ABX_CIP = "CIP",
  ABX_CST = "CST"
)

gpb_ast_map <- c(
  ABX_GEN = "GEN2",
  ABX_AMK = "AMK2",
  ABX_TOB = "TOB2",
  ABX_TGC = "TGC2",
  ABX_LVX = "LVX2",
  ABX_CIP = "CIP2",
  ABX_VAN = "VAN2",
  ABX_AZM = "AZM2",
  ABX_LZD = "LZD2"
)

lapply(gnb_mic[unname(gnb_ast_map)], function(x) table(x, useNA = "ifany"))
lapply(gpb_mic[unname(gpb_ast_map)], function(x) table(x, useNA = "ifany"))

gnb_ast_vars <- unname(gnb_ast_map)
gpb_ast_vars <- unique(c(unname(gpb_ast_map),"AMP2"))

gnb_mic <- gnb_mic %>% mutate(across(all_of(gnb_ast_vars), ~ na_if(trimws(.x), "")))
gpb_mic <- gpb_mic %>% mutate(across(all_of(gpb_ast_vars), ~ na_if(trimws(.x), "")))

ast_status <- function(x) {
  case_when(
    x == "S" ~ "susceptible",
    x %in% c("I", "R") ~ "non_susceptible",
    x %in% c("ND", NA_character_) ~ "indeterminate",
    TRUE ~ "indeterminate"
  )
}

gnb_mic <- gnb_mic %>% mutate(across(all_of(gnb_ast_vars), ast_status, .names = "{.col}_STATUS"))
gpb_mic <- gpb_mic %>% mutate(across(all_of(gpb_ast_vars), ast_status, .names = "{.col}_STATUS"))




# Organism groups: exact 52 nonblank organisms in MIC datasets
enterobacterales <- c(
  "Escherichia coli",
  "Pseudescherichia vulneris",
  "Klebsiella pneumoniae",
  "Klebsiella variicola",
  "Klebsiella quasipneumoniae",
  "Klebsiella michiganensis",
  "Klebsiella oxytoca",
  "Klebsiella aerogenes",
  "Enterobacter cloacae",
  "Enterobacter cloacae complex",
  "Enterobacter xiangfangensis",
  "Enterobacter sp.",
  "Enterobacter hormaechei",
  "Enterobacter kobei",
  "Enterobacter asburiae",
  "Enterobacter ludwigii",
  "Citrobacter freundii",
  "Citrobacter braakii",
  "Citrobacter sedlakii",
  "Serratia marcescens",
  "Serratia nematodiphila",
  "Proteus mirabilis",
  "Morganella morganii",
  "Providencia rettgeri",
  "Salmonella enterica",
  "Raoultella ornithinolytica",
  "Pantoea calida",
  "Franconibacter pulveris"
)

nonfermenters <- c(
  "Burkholderia cenocepacia",
  "Burkholderia sp.",
  "Burkholderia gladioli",
  "Acinetobacter baumannii",
  "Acinetobacter bereziniae",
  "Acinetobacter junii",
  "Acinetobacter sp.",
  "Acinetobacter nosocomialis",
  "Acinetobacter schindleri",
  "Acinetobacter radioresistens",
  "Pseudomonas aeruginosa",
  "Pseudomonas stutzeri",
  "Pseudomonas alcaligenes",
  "Pseudomonas sp.",
  "Achromobacter xylosoxidans",
  "Achromobacter sp.",
  "Ralstonia mannitolilytica",
  "Stenotrophomonas maltophilia"
)

aeromonas <- c(
  "Aeromonas sp.",
  "Aeromonas hydrophila"
)

staphs <- c(
  "Staphylococcus aureus",
  "Staphylococcus sciuri",
  "Staphylococcus epidermidis",
  "Staphylococcus haemolyticus"
)

gnb_orgs <- c(
  enterobacterales,
  nonfermenters,
  aeromonas
)

all_orgs <- sort(unique(c(
  gnb_mic$FINALSPECIESID[gnb_mic$FINALSPECIESID != ""],
  gpb_mic$FINALSPECIESID[gpb_mic$FINALSPECIESID != ""]
)))

# Should be 52
length(all_orgs)

# Check that our groups cover every organism
setdiff(all_orgs,
  c(enterobacterales, nonfermenters, aeromonas, staphs)
)

# Current 39 antibiotics in treatment dataset
abx_vars_current <- grep("^ABX_", names(abx), value = TRUE)
length(abx_vars_current)   # should be 39

# Direct BARNARDS AST crosswalk
gnb_ast_map <- c(
  ABX_AMP = "AMP",
  ABX_AMC = "AMC",
  ABX_TZP = "TZP",
  ABX_CRO = "CRO",
  ABX_CTX = "CTX",
  ABX_CAZ = "CAZ",
  ABX_FEP = "FEP",
  ABX_IPM = "IPM",
  ABX_MEM = "MEM",
  ABX_ETP = "ETP",
  ABX_GEN = "GEN",
  ABX_AMK = "AMK",
  ABX_TOB = "TOB",
  ABX_TGC = "TGC",
  ABX_LVX = "LVX",
  ABX_CIP = "CIP",
  ABX_CST = "CST"
)

gpb_ast_map <- c(
  ABX_GEN = "GEN2",
  ABX_AMK = "AMK2",
  ABX_TOB = "TOB2",
  ABX_TGC = "TGC2",
  ABX_LVX = "LVX2",
  ABX_CIP = "CIP2",
  ABX_VAN = "VAN2",
  ABX_AZM = "AZM2",
  ABX_LZD = "LZD2"
)

# AMP2 and FLUCLOX2 are deliberately excluded because
# they contain no interpretable AST results.

# Build organism × antibiotic applicability matrix
gnb_ast_lookup <- tibble(
  antibiotic = names(gnb_ast_map),
  gnb_ast = unname(gnb_ast_map)
)

gpb_ast_lookup <- tibble(
  antibiotic = names(gpb_ast_map),
  gpb_ast = unname(gpb_ast_map)
)

app_matrix <- tidyr::crossing(
  organism = all_orgs,
  antibiotic = abx_vars_current
) %>%
  mutate(
    organism_group = case_when(
      organism %in% enterobacterales ~ "Enterobacterales",
      organism %in% nonfermenters    ~ "Non-fermenter",
      organism %in% aeromonas        ~ "Aeromonas",
      organism %in% staphs           ~ "Staphylococcus"
    ),
    gram = if_else(
      organism %in% staphs,
      "GPB",
      "GNB"
    )
  ) %>%
  left_join(
    gnb_ast_lookup,
    by = "antibiotic"
  ) %>%
  left_join(
    gpb_ast_lookup,
    by = "antibiotic"
  ) %>%
  mutate(
    ast_var = case_when(
      gram == "GNB" ~ gnb_ast,
      gram == "GPB" ~ gpb_ast
    ),
    
    applicability = case_when(
      
      # None of the organisms in these MIC datasets are anaerobes
      antibiotic == "ABX_METRONIDAZOLE" ~
        "not_applicable",
      
      # Clear Gram-positive-directed drugs with no meaningful GNB activity
      gram == "GNB" &
        antibiotic %in% c(
          "ABX_CLOXACILLIN",
          "ABX_FLUCLOX",
          "ABX_VAN",
          "ABX_TARGOPLANIN",
          "ABX_CLINDAMYCIN",
          "ABX_LZD",
          "ABX_BENZYLPENICILLIN",
          "ABX_ERYTHROMYCIN",
          "ABX_CLARITHROMYCIN"
        ) ~
        "not_applicable",
      
      # Macrolides generally do not provide GNB coverage.
      # Salmonella is left indeterminate because azithromycin
      # can have activity against some Salmonella infections.
      gram == "GNB" & antibiotic == "ABX_AZM" & organism != "Salmonella enterica" ~ 
        "not_applicable",
      
      # Non-fermenters are not covered by first/second-generation
      # cephalosporins
      organism_group == "Non-fermenter" &
        antibiotic %in% c(
          "ABX_CEFRADINE",
          "ABX_CEFALEXIN",
          "ABX_CEFUROXIME"
        ) ~
        "not_applicable",
      
      # Colistin has no Gram-positive activity
      gram == "GPB" &
        antibiotic == "ABX_CST" ~
        "not_applicable",
      
      # Staphylococci are expected resistant to ceftazidime
      gram == "GPB" &
        antibiotic == "ABX_CAZ" ~
        "not_applicable",
      
      # BARNARDS has direct drug-specific AST
      !is.na(ast_var) ~ "evaluable",
      
      # Potentially relevant, but no direct BARNARDS AST
      TRUE ~ "indeterminate_no_AST")
  ) %>%
  select(organism, organism_group, gram, antibiotic, ast_var, applicability)

app_matrix_wide <- app_matrix %>%
  select(organism, antibiotic, applicability
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = applicability)

# View(app_matrix_wide)




# administered antibiotic × isolate → isolate → organism → neonate
# 1. Reshape the administered antibiotics to long format
abx_vars_current <- grep("^ABX_", names(abx), value = TRUE)

abx_long <- abx %>%
  select(
    COUNTRY, CONTINENT,
    ANON_MOTHERID, ANON_BABYID,
    all_of(abx_vars_current)
  ) %>%
  pivot_longer(
    cols = all_of(abx_vars_current),
    names_to = "antibiotic",
    values_to = "used"
  ) %>%
  filter(used == 1) %>%
  select(-used)

# 2. Make the MIC data long
gnb_mic_long <- gnb_mic %>%
  rename(
    ANON_MOTHERID = ANON_MOTHER,
    ANON_BABYID = ANON_BABY,
    organism = FINALSPECIESID
  ) %>%
  filter(!is.na(organism), organism != "") %>%
  select(
    ANON_MOTHERID, ANON_BABYID, ANON_ISOLATE,
    organism,
    all_of(unique(unname(gnb_ast_map)))
  ) %>%
  pivot_longer(
    cols = all_of(unique(unname(gnb_ast_map))),
    names_to = "ast_var",
    values_to = "ast"
  ) %>%
  mutate(
    gram = "GNB",
    ast = trimws(as.character(ast)),
    ast = na_if(ast, "")
  )

gpb_mic_long <- gpb_mic %>%
  rename(
    ANON_MOTHERID = ANON_MOTHER,
    ANON_BABYID = ANON_BABY,
    organism = FINALSPECIESID
  ) %>%
  filter(!is.na(organism), organism != "") %>%
  select(
    ANON_MOTHERID, ANON_BABYID, ANON_ISOLATE,
    organism,
    all_of(unique(unname(gpb_ast_map)))
  ) %>%
  pivot_longer(
    cols = all_of(unique(unname(gpb_ast_map))),
    names_to = "ast_var",
    values_to = "ast"
  ) %>%
  mutate(
    gram = "GPB",
    ast = trimws(as.character(ast)),
    ast = na_if(ast, "")
  )

mic_long <- rbind(
  gnb_mic_long,
  gpb_mic_long
)

isolates <- rbind(
  gnb_mic %>%
    transmute(
      ANON_MOTHERID = ANON_MOTHER,
      ANON_BABYID = ANON_BABY,
      ANON_ISOLATE,
      organism = FINALSPECIESID,
      gram = "GNB"),
  gpb_mic %>%
    transmute(
      ANON_MOTHERID = ANON_MOTHER,
      ANON_BABYID = ANON_BABY,
      ANON_ISOLATE,
      organism = FINALSPECIESID,
      gram = "GPB")
) %>%
  filter(
    !is.na(organism),
    organism != ""
  ) %>%
  distinct()

drug_isolate <- abx_long %>%
  inner_join(
    isolates,
    by = c("ANON_MOTHERID", "ANON_BABYID"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    app_matrix,
    by = c("organism", "gram", "antibiotic" )
  ) %>%
  left_join(
    mic_long,
    by = c("ANON_MOTHERID", "ANON_BABYID", "ANON_ISOLATE", "organism", "gram", "ast_var"),
    na_matches = "never"
  )

drug_isolate <- drug_isolate %>%
  mutate(drug_status = case_when(
      applicability == "not_applicable" ~ "inactive", # Antibiotic does not appropriately cover this organism
      applicability == "indeterminate_no_AST" ~ "indeterminate", # Potentially appropriate antibiotic, but no usable BARNARDS AST
      applicability == "evaluable" & ast == "S" ~ "active", # Appropriate + phenotypically susceptible
      applicability == "evaluable" & ast %in% c("I", "R") ~ "inactive", # Appropriate but non-susceptible
      applicability == "evaluable" & (is.na(ast) | ast == "ND") ~ "indeterminate", # AST unavailable / not determined
      TRUE ~ "indeterminate")
  )

table(drug_isolate$applicability, drug_isolate$ast, useNA = "ifany")
table(drug_isolate$drug_status, useNA = "ifany")

isolate_level <- drug_isolate %>%
  group_by(
    COUNTRY, CONTINENT,
    ANON_MOTHERID, ANON_BABYID,
    ANON_ISOLATE,
    organism,
    gram
  ) %>%
  summarize(
    n_antibiotics = n(),
    n_active = sum(drug_status == "active"),
    n_inactive = sum(drug_status == "inactive"),
    n_indeterminate = sum(drug_status == "indeterminate"),
    
    isolate_status = case_when(
      any(drug_status == "active") ~ "covered",
      any(drug_status == "indeterminate") ~ "indeterminate",
      TRUE ~ "not_covered"),
    
    .groups = "drop"
  )

organism_level <- isolate_level %>%
  group_by(
    COUNTRY, CONTINENT,
    ANON_MOTHERID, ANON_BABYID,
    organism,
    gram
  ) %>%
  summarize(
    n_isolates = n_distinct(ANON_ISOLATE),
    organism_status = case_when(
      any(isolate_status == "not_covered") ~ "not_covered",
      any(isolate_status == "indeterminate") ~ "indeterminate",
      TRUE ~ "covered"),
    .groups = "drop"
  )

neonate_level <- organism_level %>%
  group_by(
    COUNTRY, CONTINENT,
    ANON_MOTHERID, ANON_BABYID
  ) %>%
  summarize(
    n_organisms = n(),
    treatment_status = case_when(
      any(organism_status == "not_covered") ~ "not_fully_covered", # Any definitively uncovered organism means treatment was not fully active
      any(organism_status == "indeterminate") ~ "indeterminate", # None uncovered, but at least one cannot be evaluated
      TRUE ~ "fully_covered"), # Every detected organism is covered
    .groups = "drop"
  )

neonate_continent_summary <- neonate_level %>%
  count(CONTINENT, treatment_status, name = "n") %>%
  group_by(CONTINENT) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

neonate_continent_summary

neonate_country_summary <- neonate_level %>%
  count(COUNTRY, treatment_status, name = "n") %>%
  group_by(COUNTRY) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

neonate_country_summary

continent_coverage <- neonate_level %>%
  group_by(CONTINENT) %>%
  summarize(
    n_total = n(),
    n_indeterminate = sum(treatment_status == "indeterminate"),
    n_fully_covered = sum(treatment_status == "fully_covered"),
    n_not_fully_covered = sum(treatment_status == "not_fully_covered"),
    .groups = "drop"
  )

country_coverage <- neonate_level %>%
  group_by(COUNTRY) %>%
  summarize(
    n_total = n(),
    n_indeterminate = sum(treatment_status == "indeterminate"),
    n_fully_covered = sum(treatment_status == "fully_covered"),
    n_not_fully_covered = sum(treatment_status == "not_fully_covered"),
    .groups = "drop"
  )

coverage <- bind_rows(continent_coverage, country_coverage)
barnards.output <- "outputs/pipelines/africa_asia/barnards"
write.csv(coverage, file.path(barnards.output,"effective_cover.csv"))





#_______________________________________________=========
# 4. (Barnards) Time from clinical sepsis recognition to antibiotic treatment initiation ===================================

# 1. Parse the dates
date_vars <- c(
  "CLINDATE1", "CULTDATE",
  "DATE1", "DATE2", "DATE3",
  "DATE4", "DATE5", "DATE6", "BABYDOB"
)

# month/day/2-digit year
abx_epi <- abx_epi %>%
  mutate(
    across(
      all_of(date_vars),
      ~ as.Date(na_if(.x, ""), format = "%m/%d/%y")
    )
  )

abx_epi <- abx_epi %>%
  rowwise() %>%
  mutate(
    first_abx_date = {
      x <- c_across(all_of(paste0("DATE", 1:6)))
      if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Earliest recorded antibiotic date
table(abx_epi$first_abx_date, useNA = "ifany")
table(abx_epi$BABYDOB, useNA = "ifany")
sum(is.na(abx_epi$first_abx_date))

# Days from birth to clinical sepsis and culture confirmation
abx_epi <- abx_epi %>%
  mutate(
    age_clindate_days = as.numeric(
      difftime(CLINDATE1, BABYDOB, units = "days")
    ),
    age_cultdate_days = as.numeric(
      difftime(CULTDATE, BABYDOB, units = "days")
    ),
    age_first_abx_days = as.numeric(
      difftime(first_abx_date, BABYDOB, units = "days")
    )
  )

summary(abx_epi$age_clindate_days)
summary(abx_epi$age_cultdate_days)
summary(abx_epi$age_first_abx_days)

abx_epi %>%
  summarise(
    n = n(),
    
    clindate_missing =
      sum(is.na(CLINDATE1)),
    clindate_before_dob =
      sum(age_clindate_days < 0, na.rm = TRUE),
    
    cultdate_missing =
      sum(is.na(CULTDATE)),
    cultdate_before_dob =
      sum(age_cultdate_days < 0, na.rm = TRUE),
    
    first_abx_missing =
      sum(is.na(first_abx_date)),
    first_abx_before_dob =
      sum(age_first_abx_days < 0, na.rm = TRUE)
  )



# check abx_date
abx_date_check <- abx_epi %>%
  select(
    CONTINENT, COUNTRY,
    ANON_MOTHERID, ANON_BABYID,
    BABYDOB,
    all_of(paste0("DATE", 1:6))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("DATE", 1:6)),
    names_to = "date_variable",
    values_to = "abx_date"
  ) %>%
  mutate(
    days_from_dob = as.numeric(
      difftime(abx_date, BABYDOB, units = "days")
    )
  )

abx_date_check %>%
  group_by(date_variable) %>%
  summarise(
    n_date = sum(!is.na(abx_date)),
    n_before_dob = sum(days_from_dob < 0, na.rm = TRUE),
    n_day0_60 = sum(
      days_from_dob >= 0 & days_from_dob <= 60,
      na.rm = TRUE
    ),
    n_after_day60 = sum(days_from_dob > 60, na.rm = TRUE),
    .groups = "drop"
  )






date_check <- abx_epi %>%
  select(
    CONTINENT, COUNTRY,
    ANON_MOTHERID, ANON_BABYID,
    BABYDOB,
    CLINDATE1, CULTDATE,
    all_of(paste0("DATE", 1:6))
  ) %>%
  pivot_longer(
    cols = c(
      CLINDATE1, CULTDATE,
      all_of(paste0("DATE", 1:6))
    ),
    names_to = "date_variable",
    values_to = "date"
  ) %>%
  mutate(
    days_from_dob = as.numeric(
      difftime(date, BABYDOB, units = "days")
    )
  )

date_check %>%
  group_by(date_variable) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(date)),
    n_available = sum(!is.na(date)),
    n_before_dob = sum(days_from_dob < 0, na.rm = TRUE),
    n_day0_60 = sum(
      days_from_dob >= 0 & days_from_dob <= 60,
      na.rm = TRUE
    ),
    n_after_day60 = sum(days_from_dob > 60, na.rm = TRUE),
    .groups = "drop"
  )






abx_date_vars <- paste0("DATE", 1:6)

abx_epi <- abx_epi %>%
  mutate(
    across(
      all_of(abx_date_vars),
      ~ case_when(
        is.na(.x) | is.na(BABYDOB) ~ as.Date(NA),
        .x < BABYDOB ~ as.Date(NA),
        .x > BABYDOB + 60 ~ as.Date(NA),
        TRUE ~ .x
      ),
      .names = "{.col}_valid"
    )
  ) %>%
  rowwise() %>%
  mutate(
    first_abx_date_valid = {
      x <- c_across(all_of(paste0(abx_date_vars, "_valid")))
      if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
    }
  ) %>%
  ungroup()



abx_timing <- abx_epi %>%
  filter(
    !is.na(CLINDATE1),
    !is.na(first_abx_date_valid),
    !is.na(BABYDOB)
  ) %>%
  mutate(
    time_to_abx_days = as.numeric(
      difftime(first_abx_date_valid, CLINDATE1, units = "days")
    )
  )




table(abx_timing$first_abx_date_valid - abx_timing$CLINDATE1)



abx_timing$abx_to_clin <-   as.numeric(
    difftime(
      abx_timing$first_abx_date_valid,
      abx_timing$CLINDATE1,
      units = "days"
    )
  )


quantile(abx_timing$abx_to_clin, 0.05)
quantile(abx_timing$abx_to_clin, 0.95)



abx_timing %>%
  group_by(CONTINENT, COUNTRY) %>%
  summarise(
    n = n(),
    median = median(abx_to_clin, na.rm = TRUE),
    p05 = quantile(abx_to_clin, 0.05, na.rm = TRUE),
    p95 = quantile(abx_to_clin, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

abx_timing %>%
  group_by(CONTINENT) %>%
  summarise(
    n = n(),
    median = median(abx_to_clin, na.rm = TRUE),
    p05 = quantile(abx_to_clin, 0.05, na.rm = TRUE),
    p95 = quantile(abx_to_clin, 0.95, na.rm = TRUE),
    .groups = "drop"
  )



abx_epi <- abx_epi %>%
  mutate(
    abx_to_clin = as.numeric(
      difftime(first_abx_date_valid, CLINDATE1, units = "days")
    ),
    
    abx_timing_cat = case_when(
      abx_to_clin < -7  ~ ">7 days before",
      abx_to_clin <= -2 ~ "2–7 days before",
      abx_to_clin == -1 ~ "1 day before",
      abx_to_clin == 0  ~ "Same day",
      abx_to_clin == 1  ~ "1 day after",
      abx_to_clin <= 7  ~ "2–7 days after",
      abx_to_clin > 7   ~ ">7 days after",
      TRUE ~ NA_character_
    )
  )

# show difference
abx_epi %>%
  count(CONTINENT, abx_timing_cat) %>%
  group_by(CONTINENT) %>%
  mutate(
    pct = 100 * n / sum(n)
  ) %>%
  ungroup()

wilcox.test(abx_to_clin ~ CONTINENT, data = abx_timing)

#supplementary table
abx_timing_country_summary <- abx_epi %>%
  filter(!is.na(abx_timing_cat)) %>%
  count(CONTINENT, COUNTRY, abx_timing_cat) %>%
  group_by(CONTINENT, COUNTRY) %>%
  mutate(
    N = sum(n),
    pct = 100 * n / N
  ) %>%
  ungroup()

abx_timing_continent_summary <- abx_epi %>%
  filter(!is.na(abx_timing_cat)) %>%
  count(CONTINENT, abx_timing_cat) %>%
  group_by(CONTINENT) %>%
  mutate(
    N = sum(n),
    pct = 100 * n / N
  ) %>%
  ungroup()

abx_timing <- bind_rows(abx_timing_country_summary, abx_timing_continent_summary)

barnards.output <- "outputs/pipelines/africa_asia/barnards"
write.csv(abx_timing, file.path(barnards.output,"abx_timing.csv"))







#_______________________________________________=========
# 5. (BARNARDS) flow-diagram inspection =======================

n_barnards <- function(x) x %>% distinct(ANON_MOTHERID, ANON_BABYID) %>% nrow()

# Infants with an antimicrobial-treatment record
barnards_abx_record <- abx2 %>%
  distinct(ANON_MOTHERID, ANON_BABYID)

# Infants with ≥1 systemic antibacterial recorded
barnards_systemic <- abx2 %>%
  mutate(any_systemic = if_any(all_of(antibacterial_vars), ~ .x == "Y")) %>%
  filter(any_systemic) %>%
  distinct(ANON_MOTHERID, ANON_BABYID)

# Culture-positive infants represented in bacterial MIC datasets
barnards_culture <- isolates %>%
  distinct(ANON_MOTHERID, ANON_BABYID)

barnards_culture_with_abx <- barnards_culture %>%
  semi_join(barnards_abx_record, by = c("ANON_MOTHERID", "ANON_BABYID"))

barnards_culture_without_abx <- barnards_culture %>%
  anti_join(barnards_abx_record, by = c("ANON_MOTHERID", "ANON_BABYID"))

# Timing populations
barnards_valid_abx_date <- abx_epi %>%
  filter(!is.na(first_abx_date_valid))

barnards_clin_date <- abx_epi %>%
  filter(!is.na(CLINDATE1))

barnards_timing_evaluable <- abx_epi %>%
  filter(!is.na(CLINDATE1), !is.na(first_abx_date_valid), !is.na(BABYDOB))

barnards_flow <- tibble(
  branch = c(
    "Cohort",
    "Treatment patterns", "Treatment patterns",
    "Phenotypic activity", "Phenotypic activity", "Phenotypic activity",
    "Phenotypic activity", "Phenotypic activity",
    "Timing", "Timing", "Timing", "Timing"
  ),
  step = c(
    "Infants in epidemiological dataset",
    "With evaluable antimicrobial-treatment data",
    "With ≥1 systemic antibacterial recorded",
    "Culture-positive bacterial infection",
    "Culture-positive with antimicrobial-treatment record",
    "Culture-positive without antimicrobial-treatment record",
    "Included in phenotypic-activity analysis",
    "Phenotypic activity evaluable",
    "With systemic antibacterial recorded",
    "With valid first antibiotic-treatment date",
    "With clinical-recognition date",
    "Timing evaluable"
  ),
  n = c(
    n_barnards(epi),
    n_barnards(barnards_abx_record),
    n_barnards(barnards_systemic),
    n_barnards(barnards_culture),
    n_barnards(barnards_culture_with_abx),
    n_barnards(barnards_culture_without_abx),
    n_barnards(neonate_level),
    n_barnards(neonate_level %>% filter(treatment_status != "indeterminate")),
    n_barnards(abx_epi),
    n_barnards(barnards_valid_abx_date),
    n_barnards(barnards_clin_date),
    n_barnards(barnards_timing_evaluable)
  )
)

print(barnards_flow, n = Inf)


tibble(
  culture_positive = n_barnards(barnards_culture),
  with_abx_record = n_barnards(barnards_culture_with_abx),
  without_abx_record = n_barnards(barnards_culture_without_abx),
  final_coverage_population = n_barnards(neonate_level)
)





#_______________________________________________=========
# 1. (NeoOBS) data preparation ===================================
rm(list = ls())
library(stringr)

neoobs.dir <- "data/NeoObs/Clinical sepsis cohort"

dat1 <- read.csv(file.path(neoobs.dir, "crf1_clin.csv")) # cohort eligibility, centre, all culture date/time
dat2 <- read.csv(file.path(neoobs.dir, "crf2_clin.csv")) # DOB and patient information
dat3 <- read.csv(file.path(neoobs.dir, "crf3_clin.csv")) # antibiotic start date/time for this sepsis episode
dat5 <- read.csv(file.path(neoobs.dir, "crf5_clin.csv")) # detailed antimicrobial administrations
dat7 <- read.csv(file.path(neoobs.dir, "crf7_clin.csv")) # microbiology, organisms, AST/MIC


dim(dat1)
dim(dat2)
dim(dat3)
dim(dat5)
dim(dat7)

colnames(dat1)
colnames(dat2)
colnames(dat3)
colnames(dat5)
colnames(dat7)

lapply(
  list(dat1 = dat1, dat2 = dat2, dat3 = dat3, dat5 = dat5, dat7 = dat7),
  function(x) {
    c(
      n = nrow(x),
      n_id = dplyr::n_distinct(x$anon_id),
      duplicated_id = sum(duplicated(x$anon_id)),
      n_id_day = dplyr::n_distinct(paste(x$anon_id, x$day)),
      duplicated_id_day = sum(duplicated(x[c("anon_id", "day")]))
    )
  }
)

# Check uniqueness first
nrow(dat1)
n_distinct(dat1$anon_id)
nrow(dat2)
n_distinct(dat2$anon_id)
nrow(dat3)
n_distinct(dat3$anon_id)

neoobs <- dat1 %>%
  select(
    anon_id, cohort, city, country, WHOregion,
    inclusion_diag, everything()
  ) %>%
  left_join(
    dat2 %>%
      select(anon_id, everything(), -day, -cohort, -city, -country, -WHOregion),
    by = "anon_id"
  ) %>%
  left_join(
    dat3 %>%
      select(anon_id, everything(), -day, -cohort, -city, -country, -WHOregion),
    by = "anon_id"
  )

table(dat1$country, useNA = "ifany")
table(dat1$city, useNA = "ifany")
table(dat1$WHOregion, useNA = "ifany")
table(dat1$country, dat1$WHOregion, useNA = "ifany")

africa_countries <- c(
  "Kenya",
  "South Africa",
  "Uganda"
)

asia_countries <- c(
  "Bangladesh",
  "India",
  "Thailand",
  "Vietnam"
)

dat1 <- dat1 %>%
  mutate(
    CONTINENT = case_when(
      country %in% africa_countries ~ "Africa",
      country %in% asia_countries ~ "Asia",
      TRUE ~ NA_character_
    )
  )

table(dat1$inclusion_diag, useNA = "ifany")
table(dat1$cohort, useNA = "ifany")

dat5 <- dat5 %>%
  mutate(
    CONTINENT = case_when(
      country %in% africa_countries ~ "Africa",
      country %in% asia_countries ~ "Asia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(CONTINENT))


#_______________________________________________
# Daily antimicrobial records during observation

abx_long_dat5 <- dat5 %>%
  select(
    anon_id, day, city, country, WHOregion, CONTINENT,
    matches("^abx_[0-9]+_(name|name_o)$")
  ) %>%
  pivot_longer(
    cols = matches("^abx_[0-9]+_(name|name_o)$"),
    names_to = c("dose", ".value"),
    names_pattern = "^abx_([0-9]+)_(name_o|name)$"
  ) %>%
  filter(!is.na(name), name != "") %>%
  mutate(abx_source = "Daily antimicrobial record")


#_______________________________________________
# Antimicrobials in 24 h before enrolment culture
# for THIS sepsis episode

prebc_long <- bind_rows(
  lapply(1:6, function(i) {
    dat3 %>%
      filter(
        country %in% c(africa_countries, asia_countries),
        ab_enrol == "Yes"
      ) %>%
      mutate(
        CONTINENT = case_when(
          country %in% africa_countries ~ "Africa",
          country %in% asia_countries ~ "Asia"
        )
      ) %>%
      transmute(
        anon_id,
        day = NA_integer_,
        city,
        country,
        WHOregion,
        CONTINENT,
        dose = as.character(i),
        name = .data[[paste0("abhist_bc", i)]],
        name_o = .data[[paste0("abhist_bc", i, "_o")]],
        abx_source = "24 h pre-culture history"
      )
  })
) %>%
  filter(!is.na(name), name != "")


# Check conditional CRF consistency
# Should return no records with drug names when ab_enrol != "Yes"
dat3 %>%
  filter(
    country %in% c(africa_countries, asia_countries),
    ab_enrol != "Yes" | is.na(ab_enrol)
  ) %>%
  filter(if_any(matches("^abhist_bc[1-6]$"), ~ !is.na(.x) & .x != "")) %>%
  select(anon_id, country, ab_enrol, abhist_bc_num, matches("^abhist_bc[1-6]$"))


#_______________________________________________
# Combine episode-specific pre-culture history and
# prospective daily antimicrobial records

abx_long <- bind_rows(
  abx_long_dat5,
  prebc_long
)

abx_long


#_______________________________________________
# Standardize antimicrobial names

abx_long <- abx_long %>%
  mutate(
    name_o_clean = str_to_lower(str_trim(name_o)),
    
    antibiotic = case_when(
      name != "Other (please specify)" ~ name,
      str_detect(name_o_clean, "amp.*clox|ampiclox|ampicox|apiclox") ~ "Ampicillin/Cloxacillin",
      str_detect(name_o_clean, "cefoper|zefoper") ~ "Cefoperazone",
      name_o_clean == "cefprozil" ~ "Cefprozil",
      name_o_clean == "kefzol" ~ "Cefazolin",
      name_o_clean == "rocephine" ~ "Ceftriaxone",
      name_o_clean %in% c("piptaz", "tazocin") ~ "Piperacillin/Tazobactam",
      str_detect(name_o_clean, "co.?trimoxazole") ~ "Trimethoprim/Sulfamethoxazole",
      str_detect(name_o_clean, "netilm|netilmy|nitlmy") ~ "Netilmicin",
      str_detect(name_o_clean, "colistimethate") ~ "Colistin",
      name_o_clean == "oxacillin" ~ "Oxacillin",
      
      # Non-antibacterial antimicrobials
      str_detect(name_o_clean, "nyst") ~ "Nystatin",
      str_detect(name_o_clean, "oseltam") | str_detect(name_o_clean, "osseltam") ~ "Oseltamivir",
      str_detect(name_o_clean, "valganc") ~ "Valganciclovir",
      
      # Keep uncertain free-text entries unchanged
      TRUE ~ name_o
    )
  )

abx_long <- abx_long %>%
  mutate(
    antibiotic = recode(
      antibiotic,
      "Piperacillin and enzyme inhibitor" = "Piperacillin/Tazobactam",
      "Imipenem/cilastin" = "Imipenem/Cilastatin",
      "Trimethoprim/ Sulfamethoxazole" = "Trimethoprim/Sulfamethoxazole",
      "Ticarcillin and enzyme inhibitor" = "Ticarcillin/Clavulanic acid",
      "amoxiclav" = "Amoxicillin/Clavulanic acid",
      "Co-Trimexazole" = "Trimethoprim/Sulfamethoxazole",
      "epicephin" = "Ceftriaxone",
      "flagyl" = "Metronidazole",
      "Pen G" = "Benzylpenicillin (Penicillin G)",
      "Selemycin" = "Amikacin",
      "X-pen" = "Benzylpenicillin (Penicillin G)",
      "X-Pen" = "Benzylpenicillin (Penicillin G)"
    )
  )

abx_long %>%
  filter(name == "Other (please specify)") %>%
  count(antibiotic, sort = TRUE)

abx_long <- abx_long %>%
  mutate(
    antibiotic = recode(
      antibiotic,
      "CEFOPRERAZONE" = "Cefoperazone",
      "Flucamox" = "Amoxicillin/Flucloxacillin",
      "PGS" = "Benzylpenicillin (Penicillin G)"
    )
  )

abx_long %>%
  filter(
    name == "Other (please specify)",
    antibiotic %in% c("PGS", "Flucamox")
  ) %>%
  count(country, city, antibiotic, sort = TRUE)


#_______________________________________________
# Retain antibacterial agents

non_antibacterial <- c(
  "Acyclovir",
  "Amphotericin B",
  "Anidulafungin",
  "Caspofungin",
  "Fluconazole",
  "Flucytosin",
  "Ganciclovir",
  "Itraconazole",
  "Micafungin",
  "Nevirapine",
  "Nystatin",
  "Oseltamivir",
  "Posaconazole",
  "Valganciclovir",
  "Voriconazole"
)

abx_long_antibacterial <- abx_long %>%
  filter(
    !is.na(antibiotic),
    antibiotic != "",
    !antibiotic %in% non_antibacterial
  )

# All antibacterial exposure retained for QC only
abx_child_drug_exposure <- abx_long_antibacterial %>%
  distinct(anon_id, country, CONTINENT, antibiotic)

sort(unique(abx_child_drug_exposure$antibiotic))
abx_child_drug_exposure %>% count(antibiotic, sort = TRUE)

# Exposure-source QC
abx_long_antibacterial %>%
  distinct(anon_id, antibiotic, abx_source) %>%
  count(abx_source)

abx_long_antibacterial %>%
  distinct(anon_id, antibiotic, abx_source) %>%
  count(anon_id, antibiotic, name = "n_sources") %>%
  count(n_sources)


#_______________________________________________
# Pre-culture antibacterial treatment

# Route and indication corresponding to abhist_bc1:6
prebc_details <- bind_rows(
  lapply(1:6, function(i) {
    dat3 %>%
      transmute(
        anon_id,
        dose = as.character(i),
        route = .data[[paste0("abhist_bc_r", i)]],
        indication = .data[[paste0("abhist_bc_i", i)]]
      )
  })
)

prebc_antibacterial_details <- abx_long_antibacterial %>%
  filter(abx_source == "24 h pre-culture history") %>%
  select(anon_id, country, CONTINENT, dose, antibiotic) %>%
  distinct() %>%
  left_join(prebc_details, by = c("anon_id", "dose")) %>%
  mutate(
    route_lower = str_to_lower(replace_na(route, "")),
    indication_lower = str_to_lower(replace_na(indication, "")),
    treatment = str_detect(indication_lower, "treatment"),
    prophylaxis = str_detect(indication_lower, "prophylaxis"),
    unknown_indication = indication_lower == "" | str_detect(indication_lower, "unknown"),
    systemic = !str_detect(route_lower, "inhalation")
  )

# Primary definition: explicitly recorded treatment and non-inhaled route.
# Missing route is retained because it is not known to be inhaled.
prebc_abx_treatment <- prebc_antibacterial_details %>%
  filter(treatment, systemic)

prebc_treatment <- prebc_antibacterial_details %>%
  group_by(anon_id) %>%
  summarise(
    prebc_treatment = any(treatment & systemic),
    prebc_prophylaxis = any(prophylaxis),
    prebc_unknown = any(unknown_indication),
    .groups = "drop"
  )


#_______________________________________________
# Prospective systemic antibacterial treatment

abx_dose_timing <- dat5 %>%
  rename(study_day = day) %>%
  select(
    anon_id, study_day, city, country, WHOregion, CONTINENT,
    matches("^abx_[0-9]+_(day|time|route|prim_indic|sec_indic)$")
  ) %>%
  pivot_longer(
    cols = matches("^abx_[0-9]+_(day|time|route|prim_indic|sec_indic)$"),
    names_to = c("dose", ".value"),
    names_pattern = "^abx_([0-9]+)_(day|time|route|prim_indic|sec_indic)$"
  ) %>%
  rename(dose_day = day, dose_time = time) %>%
  left_join(
    abx_long_antibacterial %>%
      filter(abx_source == "Daily antimicrobial record") %>%
      transmute(anon_id, study_day = day, dose, antibiotic) %>%
      distinct(),
    by = c("anon_id", "study_day", "dose")
  ) %>%
  filter(!is.na(antibiotic)) %>%
  mutate(
    prim_indic_lower = str_to_lower(replace_na(prim_indic, "")),
    sec_indic_lower = str_to_lower(replace_na(sec_indic, "")),
    prim_proph = str_detect(prim_indic_lower, "prophylaxis"),
    sec_nonproph = sec_indic_lower != "" &
      !str_detect(sec_indic_lower, "prophylaxis|none|missing|unknown"),
    treatment_dose = !prim_proph | sec_nonproph
  )

# Exclude prophylactic-only and inhaled doses. Missing route is retained.
abx_dose_treatment <- abx_dose_timing %>%
  filter(treatment_dose, is.na(route) | route != "Inhalation")


# Antibiotic classification
abx_classification <- tribble(
  ~antibiotic, ~class, ~subclass,
  
  # Beta-lactams: penicillins
  "Amoxicillin", "Beta-lactams", "Penicillins",
  "Ampicillin", "Beta-lactams", "Penicillins",
  "Benzathine benzylpenicillin (Benzathine Penicillin G)", "Beta-lactams", "Penicillins",
  "Benzylpenicillin (Penicillin G)", "Beta-lactams", "Penicillins",
  "Phenoxymethylpenicillin (Penicillin V)", "Beta-lactams", "Penicillins",
  "Ticarcillin", "Beta-lactams", "Penicillins",
  
  # Anti-staphylococcal penicillins
  "Cloxacillin", "Beta-lactams", "Anti-staphylococcal penicillins",
  "Flucloxacillin", "Beta-lactams", "Anti-staphylococcal penicillins",
  "Oxacillin", "Beta-lactams", "Anti-staphylococcal penicillins",
  
  # Penicillin combinations
  "Amoxicillin/Flucloxacillin", "Beta-lactams", "Penicillin combinations",
  "Ampicillin/Cloxacillin", "Beta-lactams", "Penicillin combinations",
  
  # Beta-lactam/beta-lactamase inhibitor combinations
  "Amoxicillin/Clavulanic acid", "Beta-lactams", "Beta-lactam/beta-lactamase inhibitor combinations",
  "Ampicillin/Sulbactam", "Beta-lactams", "Beta-lactam/beta-lactamase inhibitor combinations",
  "Piperacillin/Tazobactam", "Beta-lactams", "Beta-lactam/beta-lactamase inhibitor combinations",
  "Ticarcillin/Clavulanic acid", "Beta-lactams", "Beta-lactam/beta-lactamase inhibitor combinations",
  
  # Cephalosporins
  "Cefaclor", "Beta-lactams", "Cephalosporins",
  "Cefadroxil", "Beta-lactams", "Cephalosporins",
  "Cefalexin", "Beta-lactams", "Cephalosporins",
  "Cefazolin", "Beta-lactams", "Cephalosporins",
  "Cefepime", "Beta-lactams", "Cephalosporins",
  "Cefixime", "Beta-lactams", "Cephalosporins",
  "Cefoperazone", "Beta-lactams", "Cephalosporins",
  "Cefotaxime", "Beta-lactams", "Cephalosporins",
  "Cefoxitin", "Beta-lactams", "Cephalosporins",
  "Cefpodoxime", "Beta-lactams", "Cephalosporins",
  "Cefprozil", "Beta-lactams", "Cephalosporins",
  "Cefradine", "Beta-lactams", "Cephalosporins",
  "Ceftaroline fosamil", "Beta-lactams", "Cephalosporins",
  "Ceftazidime", "Beta-lactams", "Cephalosporins",
  "Ceftriaxone", "Beta-lactams", "Cephalosporins",
  "Ceftobiprole medocaril", "Beta-lactams", "Cephalosporins",
  "Cefuroxime", "Beta-lactams", "Cephalosporins",
  
  "Cefoperazone/Sulbactam", "Beta-lactams", "Cephalosporin/beta-lactamase inhibitor combinations",
  
  # Monobactams
  "Aztreonam", "Beta-lactams", "Monobactams",
  
  # Carbapenems
  "Doripenem", "Beta-lactams", "Carbapenems",
  "Ertapenem", "Beta-lactams", "Carbapenems",
  "Imipenem/Cilastatin", "Beta-lactams", "Carbapenems",
  "Meropenem", "Beta-lactams", "Carbapenems",
  
  # Aminoglycosides
  "Amikacin", "Aminoglycosides", "Aminoglycosides",
  "Gentamicin", "Aminoglycosides", "Aminoglycosides",
  "Netilmicin", "Aminoglycosides", "Aminoglycosides",
  "Tobramycin", "Aminoglycosides", "Aminoglycosides",
  
  # Glycopeptides
  "Teicoplanin", "Glycopeptides", "Glycopeptides",
  "Vancomycin", "Glycopeptides", "Glycopeptides",
  
  # Polymyxins
  "Colistin", "Polymyxins", "Polymyxins",
  "Polymyxin B", "Polymyxins", "Polymyxins",
  
  # Fluoroquinolones
  "Ciprofloxacin", "Fluoroquinolones", "Fluoroquinolones",
  "Levofloxacin", "Fluoroquinolones", "Fluoroquinolones",
  "Moxifloxacin", "Fluoroquinolones", "Fluoroquinolones",
  "Norfloxacin", "Fluoroquinolones", "Fluoroquinolones",
  "Ofloxacin", "Fluoroquinolones", "Fluoroquinolones",
  
  # Macrolides / ketolides
  "Azithromycin", "Macrolides", "Macrolides",
  "Clarithromycin", "Macrolides", "Macrolides",
  "Erythromycin", "Macrolides", "Macrolides",
  "Telithromycin", "Macrolides", "Macrolides",
  
  # Lincosamides
  "Clindamycin", "Lincosamides", "Lincosamides",
  
  # Oxazolidinones
  "Linezolid", "Oxazolidinones", "Oxazolidinones",
  
  # Lipopeptides
  "Daptomycin", "Lipopeptides", "Lipopeptides",
  
  # Tetracyclines
  "Doxycycline", "Tetracyclines", "Tetracyclines",
  "Minocycline", "Tetracyclines", "Tetracyclines",
  "Tetracycline", "Tetracyclines", "Tetracyclines",
  "Tigecycline", "Tetracyclines", "Tetracyclines",
  
  # Phenicols
  "Chloramphenicol", "Phenicols", "Phenicols",
  
  # Nitroimidazoles
  "Metronidazole", "Nitroimidazoles", "Nitroimidazoles",
  
  # Nitrofurans
  "Nitrofurantoin", "Nitrofurans", "Nitrofurans",
  
  # Folate-pathway inhibitors
  "Trimethoprim", "Folate-pathway inhibitors", "Folate-pathway inhibitors",
  "Trimethoprim/Sulfamethoxazole", "Folate-pathway inhibitors", "Folate-pathway inhibitors",
  
  # Rifamycins
  "Rifabutin", "Rifamycins", "Rifamycins",
  "Rifampicin", "Rifamycins", "Rifamycins",
  
  # Streptogramins
  "Quinupristin/dalfopristin", "Streptogramins", "Streptogramins",
  
  # Other antibacterials
  "Fosfomycin", "Other antibacterials", "Other antibacterials",
  "Fusidic acid", "Other antibacterials", "Other antibacterials",
  
  # Antimycobacterials
  "Ethambutol", "Antimycobacterials", "Antimycobacterials",
  "Isoniazid", "Antimycobacterials", "Antimycobacterials",
  "Pyrazinamide", "Antimycobacterials", "Antimycobacterials"
)

#_______________________________________________
# Infant-level systemic antibacterial treatment

abx_child_drug_treatment <- bind_rows(
  prebc_abx_treatment %>%
    select(anon_id, country, CONTINENT, antibiotic),
  abx_dose_treatment %>%
    select(anon_id, country, CONTINENT, antibiotic)
) %>%
  distinct(anon_id, country, CONTINENT, antibiotic) %>%
  left_join(abx_classification, by = "antibiotic")

# All retained treatment agents should be classified
setdiff(sort(unique(abx_child_drug_treatment$antibiotic)), abx_classification$antibiotic)
setdiff(abx_classification$antibiotic, sort(unique(abx_child_drug_treatment$antibiotic)))

n_distinct(abx_child_drug_treatment$anon_id)

# Compare any exposure with treatment-only definition
tibble(
  definition = c("Any antibacterial exposure", "Systemic antibacterial treatment"),
  n_infants = c(
    n_distinct(abx_child_drug_exposure$anon_id),
    n_distinct(abx_child_drug_treatment$anon_id)
  ),
  n_infant_drug_pairs = c(
    nrow(abx_child_drug_exposure),
    nrow(abx_child_drug_treatment)
  )
)

anti_join(
  abx_child_drug_exposure,
  abx_child_drug_treatment %>% select(anon_id, country, CONTINENT, antibiotic),
  by = c("anon_id", "country", "CONTINENT", "antibiotic")
) %>%
  count(antibiotic, sort = TRUE)

neoobs_denominator <- dat1 %>%
  filter(country %in% c(africa_countries, asia_countries)) %>%
  mutate(CONTINENT = case_when(
    country %in% africa_countries ~ "Africa",
    country %in% asia_countries ~ "Asia"))

# Infants with no qualifying systemic antibacterial treatment
dat1 %>%
  filter(country %in% c(africa_countries, asia_countries)) %>%
  anti_join(abx_child_drug_treatment %>% distinct(anon_id), by = "anon_id") %>%
  count(country)

abx_agent_summary <- abx_child_drug_treatment %>% count(CONTINENT, country, antibiotic, name = "n")
abx_child_class <- abx_child_drug_treatment %>% distinct(anon_id, country, CONTINENT, class)
abx_child_subclass <- abx_child_drug_treatment %>% distinct(anon_id, country, CONTINENT, class, subclass)

abx_continent_agent <- abx_child_drug_treatment %>% distinct(anon_id, CONTINENT, antibiotic) %>% count(CONTINENT, antibiotic, name = "n")
abx_continent_subclass <- abx_child_subclass %>% distinct(anon_id, CONTINENT, class, subclass) %>% count(CONTINENT, class, subclass, name = "n")
abx_continent_class <- abx_child_class %>% distinct(anon_id, CONTINENT, class) %>% count(CONTINENT, class, name = "n")

#_______________________________________________=========
# 2. (NeoOBS) antibacterial treatment by location ==================

# Denominators
denom_continent <- neoobs_denominator %>%
  count(CONTINENT, name = "N") %>%
  transmute(location = CONTINENT, N)

denom_country <- neoobs_denominator %>%
  count(CONTINENT, country, name = "N") %>%
  transmute(location = paste0(CONTINENT, "_", country), N)

denom_all <- bind_rows(denom_continent, denom_country)


# Child-level class, subclass, and agent treatment
abx_rows <- bind_rows(
  
  # Class
  abx_child_drug_treatment %>%
    distinct(anon_id, country, CONTINENT, class) %>%
    transmute(anon_id, country, CONTINENT,class,
              subclass = NA_character_, row_label = class, level = "Class"),
  
  # Subclass
  abx_child_drug_treatment %>%
    distinct(anon_id, country, CONTINENT, class, subclass) %>%
    filter(subclass != class) %>%
    transmute(anon_id, country, CONTINENT, class, subclass,
              row_label = subclass, level = "Subclass"),
  
  # Agent
  abx_child_drug_treatment %>%
    distinct(anon_id, country, CONTINENT, class, subclass, antibiotic) %>%
    transmute(anon_id, country, CONTINENT, class, subclass,
              row_label = antibiotic, level = "Agent"))


# Row order: class -> subclass -> agent
row_layout <- bind_rows(
  lapply(unique(abx_classification$class), function(cl) {
    
    x <- abx_classification %>% filter(class == cl)
    subclasses <- unique(x$subclass)
    class_row <- tibble(
      class = cl,
      subclass = NA_character_,
      row_label = cl,
      level = "Class")
    
    if (length(subclasses) == 1 && subclasses == cl) {
      
      agent_rows <- x %>%
        transmute(
          class,
          subclass,
          row_label = antibiotic,
          level = "Agent")
      
      bind_rows(class_row, agent_rows)
      
    } else {
      
      subclass_rows <- bind_rows(
        lapply(subclasses, function(sc) {
          
          bind_rows(
            tibble(
              class = cl,
              subclass = sc,
              row_label = sc,
              level = "Subclass"),
            
            x %>%
              filter(subclass == sc) %>%
              transmute(
                class,
                subclass,
                row_label = antibiotic,
                level = "Agent")
          )}))
      
      bind_rows(class_row, subclass_rows)
    }})
) %>% mutate(row_order = row_number())


# Continent-level counts
summary_continent <- abx_rows %>%
  distinct(
    anon_id, CONTINENT,
    class, subclass, row_label, level
  ) %>%
  count(
    CONTINENT,
    class, subclass, row_label, level,
    name = "n"
  ) %>%
  left_join(
    denom_continent,
    by = c("CONTINENT" = "location")
  ) %>%
  mutate(
    location = CONTINENT,
    value = sprintf("%d (%.1f)", n, 100 * n / N)
  ) %>%
  select(
    class, subclass, row_label, level,
    location, value
  )


# Country-level counts
summary_country <- abx_rows %>%
  distinct(
    anon_id, CONTINENT, country,
    class, subclass, row_label, level
  ) %>%
  count(
    CONTINENT, country,
    class, subclass, row_label, level,
    name = "n"
  ) %>%
  left_join(
    neoobs_denominator %>%
      count(CONTINENT, country, name = "N"),
    by = c("CONTINENT", "country")
  ) %>%
  mutate(
    location = paste0(CONTINENT, "_", country),
    value = sprintf("%d (%.1f)", n, 100 * n / N)
  ) %>%
  select(
    class, subclass, row_label, level,
    location, value
  )

# Combine continent and country estimates
abx_summary_long <- bind_rows(
  summary_continent,
  summary_country
) %>%
  left_join(
    row_layout,
    by = c(
      "class",
      "subclass",
      "row_label",
      "level"
    )
  )


# Number of children
number_children <- denom_all %>%
  transmute(
    row_order = 0,
    row_label = "Number of children",
    level = "N",
    location,
    value = as.character(N)
  )


# Final supplementary table
neoobs_abx_summary_flip <- bind_rows(
  number_children,
  
  abx_summary_long %>%
    select(
      row_order,
      row_label,
      level,
      location,
      value
    )
) %>%
  arrange(row_order) %>%
  select(-row_order) %>%
  pivot_wider(
    names_from = location,
    values_from = value,
    values_fill = "0 (0.0)"
  ) %>%
  select(
    row_label,
    level,
    Africa,
    `Africa_Kenya`,
    `Africa_South Africa`,
    `Africa_Uganda`,
    Asia,
    `Asia_Bangladesh`,
    `Asia_India`,
    `Asia_Thailand`,
    `Asia_Vietnam`
  )

print(neoobs_abx_summary_flip, n = Inf)

dir.create("outputs/pipelines/africa_asia/neoobs", recursive = TRUE, showWarnings = FALSE)
neoobs.output <- "outputs/pipelines/africa_asia/neoobs"
write.csv(neoobs_abx_summary_flip, file.path(neoobs.output,"abx_proportion.csv"))








#_______________________________________________=========
# 3. (NeoObs) phenotypic antibiotic coverage ================

# Sample source
table(dat7$res1_source, useNA = "ifany")
table(dat7$res2_source, useNA = "ifany")
table(dat7$res3_source, useNA = "ifany")

# Timing of cultures relative to sepsis episode
summary(dat7$days_sep_culture1)
summary(dat7$days_sep_culture2)
summary(dat7$days_sep_culture3)

# Organism classification
table(dat7$res1_path1_class, useNA = "ifany")

# Example organisms
table(dat7$res1_path1, useNA = "ifany")

# Example AST coding
table(dat7$amikacin_res1_path1, useNA = "ifany")
table(dat7$ampicillin_res1_path1, useNA = "ifany")
table(dat7$meropenem_res1_path1, useNA = "ifany")


#_______________________________________________
# 3.1. Bacterial bloodstream isolates

dat7_id <- dat7 %>%
  mutate(row_id = row_number())

# Organisms
micro_path <- dat7_id %>%
  select(
    row_id, anon_id, day, city, country, WHOregion,
    matches("^res[1-3]_path[1-3]$")
  ) %>%
  pivot_longer(
    cols = matches("^res[1-3]_path[1-3]$"),
    names_to = c("sample", "path"),
    names_pattern = "^res([1-3])_path([1-3])$",
    values_to = "organism"
  )

# Organism classification
micro_class <- dat7_id %>%
  select(
    row_id,
    matches("^res[1-3]_path[1-3]_class$")
  ) %>%
  pivot_longer(
    cols = -row_id,
    names_to = c("sample", "path"),
    names_pattern = "^res([1-3])_path([1-3])_class$",
    values_to = "organism_class"
  )

# Sample source
micro_source <- dat7_id %>%
  select(
    row_id,
    matches("^res[1-3]_source$")
  ) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "sample",
    names_pattern = "^res([1-3])_source$",
    values_to = "source"
  )

# Culture timing
micro_culture_day <- dat7_id %>%
  select(
    row_id,
    matches("^days_sep_culture[1-3]$")
  ) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "sample",
    names_pattern = "^days_sep_culture([1-3])$",
    values_to = "culture_day"
  )

# Combine isolate information
micro_isolate <- micro_path %>%
  left_join(
    micro_class,
    by = c("row_id", "sample", "path")
  ) %>%
  left_join(
    micro_source,
    by = c("row_id", "sample")
  ) %>%
  left_join(
    micro_culture_day,
    by = c("row_id", "sample")
  ) %>%
  filter(
    !is.na(organism),
    organism != ""
  )

# Checks
micro_isolate %>%
  count(source, sort = TRUE)

micro_isolate %>%
  count(organism_class, sort = TRUE)

micro_isolate %>%
  filter(source == "Blood") %>%
  count(culture_day, sort = TRUE)

micro_isolate %>%
  filter(
    source == "Blood",
    organism_class == "Pathogen"
  ) %>%
  count(organism, sort = TRUE) %>%
  print(n = Inf)


# Exclude fungal pathogens from antibacterial coverage analysis
fungal_organisms <- c(
  "Candida albicans",
  "Candida non-albicans",
  "Other fungal organism"
)

# Primary microbiological population:
# all blood culture, pathogen, bacterial organism
micro_primary <- micro_isolate %>%
  filter(
    source == "Blood",
    organism_class == "Pathogen",
    !organism %in% fungal_organisms
  ) %>%
  mutate(
    isolate_id = paste(
      row_id,
      sample,
      path,
      sep = "_"
    ),
    CONTINENT = case_when(
      country %in% africa_countries ~ "Africa",
      country %in% asia_countries ~ "Asia",
      TRUE ~ NA_character_
    ), 
    
    organism_group = case_when(
      
      organism %in% c(
        "Klebsiella pneumoniae",
        "Escherichia coli",
        "Enterobacter spp.",
        "Citrobacter spp.",
        "Serratia spp.",
        "Other Enterobacteriaceae"
      ) ~ "Enterobacterales",
      
      organism %in% c(
        "Acinetobacter spp.",
        "Pseudomonas spp."
      ) ~ "Non-fermenters",
      
      organism %in% c(
        "Staphylococcus aureus",
        "Coagulase-negative Staphylococci"
      ) ~ "Staphylococci",
      
      organism %in% c(
        "Enterococcus faecalis",
        "Enterococcus faecium"
      ) ~ "Enterococci",
      
      organism %in% c(
        "Streptococcus agalactiae",
        "Streptococcus pneumoniae",
        "Streptococcus pyogenes",
        "Other Streptococcus spp."
      ) ~ "Streptococci",
      
      organism == "Salmonella spp." ~
        "Salmonella",
      
      organism == "Other Gram negative bacterium" ~
        "Other Gram-negative",
      
      organism == "Other Gram positive bacterium" ~
        "Other Gram-positive",
      
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(CONTINENT)) %>%
  distinct(
    isolate_id,
    .keep_all = TRUE
  )

# Checks
nrow(micro_primary)
n_distinct(micro_primary$isolate_id)
n_distinct(micro_primary$anon_id)

micro_primary %>%
  count(CONTINENT, country)

micro_primary %>%
  count(organism_group, organism, sort = TRUE) %>%
  print(n = Inf)

# Check whether the same organism/culture may occur more than once
micro_primary %>%
  count(
    anon_id,
    culture_day,
    organism,
    sort = TRUE
  ) %>%
  filter(n > 1)


#_______________________________________________
# 3.2. Phenotypic AST data

ast_long <- dat7_id %>%
  select(
    row_id,
    matches("_res[1-3]_path[1-3]$")
  ) %>%
  pivot_longer(
    cols = -row_id,
    names_to = c(
      "ast_drug",
      "sample",
      "path"
    ),
    names_pattern = "^(.*)_res([1-3])_path([1-3])$",
    values_to = "ast_result"
  )

# Retain AST corresponding to primary bacterial isolates
ast_primary <- micro_primary %>%
  select(
    isolate_id,
    row_id,
    anon_id,
    country,
    CONTINENT,
    sample,
    path,
    organism,
    organism_group,
    culture_day
  ) %>%
  left_join(
    ast_long,
    by = c(
      "row_id",
      "sample",
      "path"
    )
  )

# Checks
table(
  ast_primary$ast_result,
  useNA = "ifany"
)

sort(unique(
  ast_primary$ast_drug
))


# Map AST variable names to standardized antibacterial names
ast_drug_map <- c(
  amikacin        = "Amikacin",
  amoxicillin     = "Amoxicillin",
  amox_clav       = "Amoxicillin/Clavulanic acid",
  ampicillin      = "Ampicillin",
  amp_sul         = "Ampicillin/Sulbactam",
  azithromycin    = "Azithromycin",
  aztreonam       = "Aztreonam",
  cefazolin       = "Cefazolin",
  cefepime        = "Cefepime",
  cefop_sulb      = "Cefoperazone/Sulbactam",
  cefotaxime      = "Cefotaxime",
  cefoxitin       = "Cefoxitin",
  ceftazidime     = "Ceftazidime",
  ceftriaxone     = "Ceftriaxone",
  cefuroxime      = "Cefuroxime",
  chloramphenicol = "Chloramphenicol",
  ciprofloxacin   = "Ciprofloxacin",
  clarithromycin  = "Clarithromycin",
  clindamycin     = "Clindamycin",
  colistin        = "Colistin",
  daptomycin      = "Daptomycin",
  doripenem       = "Doripenem",
  doxycycline     = "Doxycycline",
  ertapenem       = "Ertapenem",
  erythromycin    = "Erythromycin",
  gentamicin      = "Gentamicin",
  imipenem        = "Imipenem/Cilastatin",
  levofloxacin    = "Levofloxacin",
  linezolid       = "Linezolid",
  meropenem       = "Meropenem",
  minocycline     = "Minocycline",
  moxifloxacin    = "Moxifloxacin",
  netilmicin      = "Netilmicin",
  norfloxacin     = "Norfloxacin",
  ofloxacin       = "Ofloxacin",
  oxacillin       = "Oxacillin",
  penicillin      = "Benzylpenicillin (Penicillin G)",
  pip_taz         = "Piperacillin/Tazobactam",
  polymyxin_b     = "Polymyxin B",
  rifampicin      = "Rifampicin",
  teicoplanin     = "Teicoplanin",
  telithromycin   = "Telithromycin",
  tetracycline    = "Tetracycline",
  ticar_clav      = "Ticarcillin/Clavulanic acid",
  tigecycline     = "Tigecycline",
  tobramycin      = "Tobramycin",
  trim_sulf       = "Trimethoprim/Sulfamethoxazole",
  vancomycin      = "Vancomycin"
)

# AST variables corresponding to antifungal agents
antifungal_ast <- c(
  "amphob",
  "caspofungin",
  "fluconazole",
  "flucytosin",
  "itraconazole",
  "micafungin",
  "posaconazole",
  "voriconazole"
)

ast_primary <- ast_primary %>%
  mutate(
    ast_antibiotic = unname(
      ast_drug_map[ast_drug]
    )
  )

# Check that every AST variable is accounted for as either
# antibacterial or antifungal
setdiff(
  sort(unique(ast_primary$ast_drug)),
  c(
    names(ast_drug_map),
    antifungal_ast
  )
)

# Should return:
# character(0)

# Check antibacterial AST variables
ast_primary %>%
  filter(!is.na(ast_antibiotic)) %>%
  count(ast_antibiotic, sort = TRUE) %>%
  print(n = Inf)







#_______________________________________________
# 3.3. Antibacterial treatment received by each infant

# Systemic antibacterial treatment recorded for the enrolled sepsis episode
abx_coverage <- abx_child_drug_treatment %>%
  filter(
    anon_id %in% micro_primary$anon_id
  ) %>%
  distinct(
    anon_id,
    country,
    CONTINENT,
    antibiotic
  )

# Checks
n_distinct(abx_coverage$anon_id)

abx_coverage %>%
  count(antibiotic, sort = TRUE) %>%
  print(n = Inf)

# Culture-positive infants with no recorded episode antibacterial
micro_primary %>%
  distinct(
    anon_id,
    country,
    CONTINENT
  ) %>%
  anti_join(
    abx_coverage %>%
      distinct(anon_id),
    by = "anon_id"
  )


# Each bloodstream isolate × every antibacterial received by that infant
isolate_treatment <- micro_primary %>%
  select(
    isolate_id,
    anon_id,
    country,
    CONTINENT,
    culture_day,
    organism,
    organism_group
  ) %>%
  left_join(
    abx_coverage %>%
      select(
        anon_id,
        antibiotic
      ),
    by = "anon_id",
    relationship = "many-to-many"
  )

# Checks
nrow(isolate_treatment)

isolate_treatment %>%
  count(
    organism_group,
    organism,
    antibiotic,
    sort = TRUE
  ) %>%
  print(n = Inf)




#_______________________________________________
# 3.4. Exact antibiotic-AST matching

# Check that each isolate has only one AST result per tested antibiotic
ast_primary %>%
  filter(!is.na(ast_antibiotic)) %>%
  count(
    isolate_id,
    ast_antibiotic
  ) %>%
  filter(n > 1)

# Should return 0 rows


# Exact administered antibiotic × AST antibiotic match
ast_exact <- ast_primary %>%
  filter(!is.na(ast_antibiotic)) %>%
  select(
    isolate_id,
    ast_antibiotic,
    ast_result
  ) %>%
  distinct()


isolate_treatment <- isolate_treatment %>%
  left_join(
    ast_exact,
    by = c(
      "isolate_id",
      "antibiotic" = "ast_antibiotic"
    ),
    relationship = "many-to-one"
  ) %>%
  rename(
    exact_ast_result = ast_result
  )


# Check exact AST availability
table(
  isolate_treatment$exact_ast_result,
  useNA = "ifany"
)


# By administered antibiotic
isolate_treatment %>%
  mutate(
    exact_ast_available =
      exact_ast_result %in%
      c(
        "Susceptible",
        "Intermediate",
        "Resistant"
      )
  ) %>%
  count(
    antibiotic,
    exact_ast_available,
    sort = TRUE
  ) %>%
  print(n = Inf)



#_______________________________________________
# 3.5. Organism-specific susceptibility interpretation

isolate_treatment <- isolate_treatment %>%
  mutate(
    exact_ast_status = case_when(
      exact_ast_result == "Susceptible" ~ "S",
      exact_ast_result == "Intermediate" ~ "I",
      exact_ast_result == "Resistant" ~ "R",
      exact_ast_result == "Not reported" ~ "Not reported",
      is.na(exact_ast_result) | exact_ast_result == "" ~ "Unavailable",
      TRUE ~ "Unavailable"
    )
  )

table(
  isolate_treatment$exact_ast_status,
  useNA = "ifany"
)


organism_abx_check <- isolate_treatment %>%
  group_by(
    organism_group,
    organism,
    antibiotic
  ) %>%
  summarise(
    n = n(),
    n_S = sum(exact_ast_status == "S"),
    n_I = sum(exact_ast_status == "I"),
    n_R = sum(exact_ast_status == "R"),
    n_not_reported = sum(exact_ast_status == "Not reported"),
    n_unavailable = sum(exact_ast_status == "Unavailable"),
    .groups = "drop"
  ) %>%
  arrange(
    organism_group,
    organism,
    desc(n)
  )

organism_abx_check %>%
  print(n = Inf)



# Pairs for which exact AST alone cannot resolve every observation
organism_abx_unresolved <- organism_abx_check %>%
  filter(
    n_not_reported > 0 |
      n_unavailable > 0
  ) %>%
  arrange(
    organism_group,
    organism,
    desc(n)
  )

organism_abx_unresolved %>%
  print(n = Inf)




#_______________________________________________
# 3.5. Identify organism-antibiotic combinations
#      requiring additional interpretation

# Standardize exact AST result
isolate_treatment <- isolate_treatment %>%
  mutate(
    exact_ast_status = case_when(
      exact_ast_result == "Susceptible" ~ "S",
      exact_ast_result == "Intermediate" ~ "I",
      exact_ast_result == "Resistant" ~ "R",
      exact_ast_result == "Not reported" ~ "Not reported",
      is.na(exact_ast_result) | exact_ast_result == "" ~ "Unavailable",
      TRUE ~ "Unavailable"
    )
  )


# Antibiotics for which an exact AST variable exists somewhere
ast_available_drugs <- unique(
  na.omit(ast_primary$ast_antibiotic)
)


# Summarize each organism × administered-antibiotic pair
organism_abx_check <- isolate_treatment %>%
  mutate(
    exact_ast_variable =
      antibiotic %in% ast_available_drugs
  ) %>%
  group_by(
    organism_group,
    organism,
    antibiotic,
    exact_ast_variable
  ) %>%
  summarise(
    n = n(),
    
    n_S = sum(
      exact_ast_status == "S"
    ),
    
    n_I = sum(
      exact_ast_status == "I"
    ),
    
    n_R = sum(
      exact_ast_status == "R"
    ),
    
    n_not_reported = sum(
      exact_ast_status == "Not reported"
    ),
    
    n_unavailable = sum(
      exact_ast_status == "Unavailable"
    ),
    
    n_unresolved =
      n_not_reported +
      n_unavailable,
    
    .groups = "drop"
  ) %>%
  mutate(
    pct_unresolved =
      100 * n_unresolved / n
  ) %>%
  arrange(
    organism_group,
    organism,
    desc(n_unresolved),
    desc(n)
  )


# Only combinations requiring an additional rule
organism_abx_unresolved <- organism_abx_check %>%
  filter(
    n_unresolved > 0
  ) %>%
  mutate(
    unresolved_type = case_when(
      
      !exact_ast_variable ~
        "No exact AST variable",
      
      n_S + n_I + n_R > 0 ~
        "Exact AST available for some isolates",
      
      TRUE ~
        "Exact AST variable exists but no S/I/R observed"
    )
  ) %>%
  arrange(
    organism_group,
    organism,
    desc(n_unresolved),
    desc(n)
  )


organism_abx_unresolved %>%
  print(n = Inf)




#_______________________________________________
# Summary by organism group

organism_abx_unresolved %>%
  group_by(
    organism_group
  ) %>%
  summarise(
    n_pairs = n(),
    n_observations = sum(n),
    n_unresolved = sum(n_unresolved),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_unresolved)
  )


#_______________________________________________
# Drugs with no exact AST variable

organism_abx_unresolved %>%
  filter(
    !exact_ast_variable
  ) %>%
  select(
    organism_group,
    organism,
    antibiotic,
    n,
    n_unresolved
  ) %>%
  arrange(
    organism_group,
    organism,
    desc(n)
  ) %>%
  print(n = Inf)












#_______________________________________________
# 3.6. Expected resistant organism-antibiotic pairs

isolate_treatment <- isolate_treatment %>%
  mutate(
    expected_resistant = case_when(
      
      # Enterobacterales: broadly expected resistant agents
      organism_group == "Enterobacterales" &
        antibiotic %in% c(
          "Benzylpenicillin (Penicillin G)",
          "Vancomycin",
          "Clindamycin",
          "Linezolid"
        ) ~ TRUE,
      
      # Macrolides generally do not provide coverage for
      # Enterobacterales; Salmonella is handled separately
      organism_group == "Enterobacterales" &
        antibiotic %in% c(
          "Azithromycin",
          "Erythromycin"
        ) ~ TRUE,
      
      # Metronidazole does not provide treatment activity
      # against these aerobic/facultative Enterobacterales
      organism_group == "Enterobacterales" &
        antibiotic == "Metronidazole" ~ TRUE,
      
      # Klebsiella pneumoniae
      organism == "Klebsiella pneumoniae" &
        antibiotic %in% c(
          "Ampicillin",
          "Amoxicillin",
          "Ampicillin/Cloxacillin"
        ) ~ TRUE,
      
      # Non-fermenters: broad expected resistance
      organism_group == "Non-fermenters" &
        antibiotic %in% c(
          "Benzylpenicillin (Penicillin G)",
          "Cefazolin",
          "Cefalexin",
          "Cefuroxime",
          "Vancomycin",
          "Azithromycin",
          "Erythromycin",
          "Clindamycin",
          "Linezolid"
        ) ~ TRUE,
      
      organism_group == "Non-fermenters" &
        antibiotic == "Metronidazole" ~ TRUE,
      
      # Acinetobacter spp.
      organism == "Acinetobacter spp." &
        antibiotic %in% c(
          "Ampicillin",
          "Amoxicillin",
          "Amoxicillin/Clavulanic acid",
          "Cefotaxime",
          "Ceftriaxone"
        ) ~ TRUE,
      
      # Pseudomonas spp.
      organism == "Pseudomonas spp." &
        antibiotic %in% c(
          "Ampicillin",
          "Amoxicillin",
          "Amoxicillin/Clavulanic acid",
          "Cefotaxime",
          "Ceftriaxone",
          "Trimethoprim/Sulfamethoxazole"
        ) ~ TRUE,
      
      # Gram-positive bacteria: polymyxins are inactive
      organism_group %in% c(
        "Staphylococci",
        "Enterococci",
        "Streptococci",
        "Other Gram-positive"
      ) &
        antibiotic == "Colistin" ~ TRUE,
      
      # Staphylococci
      organism_group == "Staphylococci" &
        antibiotic == "Ceftazidime" ~ TRUE,
      
      organism_group == "Staphylococci" &
        antibiotic == "Metronidazole" ~ TRUE,
      
      # Enterococci
      organism_group == "Enterococci" &
        antibiotic %in% c(
          "Cefazolin",
          "Cefalexin",
          "Cefuroxime",
          "Cefotaxime",
          "Ceftriaxone",
          "Ceftazidime",
          "Cefepime"
        ) ~ TRUE,
      
      # Aminoglycosides alone do not constitute active
      # monotherapy against enterococci
      organism_group == "Enterococci" &
        antibiotic %in% c(
          "Amikacin",
          "Gentamicin",
          "Netilmicin",
          "Tobramycin"
        ) ~ TRUE,
      
      organism_group == "Enterococci" &
        antibiotic ==
        "Trimethoprim/Sulfamethoxazole" ~ TRUE,
      
      organism_group == "Enterococci" &
        antibiotic == "Metronidazole" ~ TRUE,
      
      # Streptococci
      organism_group == "Streptococci" &
        antibiotic == "Ceftazidime" ~ TRUE,
      
      # Low-level intrinsic aminoglycoside resistance;
      # aminoglycoside alone should not count as coverage
      organism_group == "Streptococci" &
        antibiotic %in% c(
          "Amikacin",
          "Gentamicin",
          "Netilmicin",
          "Tobramycin"
        ) ~ TRUE,
      
      TRUE ~ FALSE
    )
  )

# Preliminary interpreted status:
# expected resistance > exact S/I/R > unresolved
isolate_treatment <- isolate_treatment %>%
  mutate(
    interpreted_ast = case_when(
      expected_resistant ~ "R",
      exact_ast_status %in%
        c("S", "I", "R") ~
        exact_ast_status,
      TRUE ~ NA_character_
    ),
    
    interpretation_source = case_when(
      expected_resistant ~
        "Expected resistant",
      exact_ast_status %in%
        c("S", "I", "R") ~
        "Exact AST",
      TRUE ~
        "Unresolved"
    )
  )

# Important QC:
# reported S but organism-drug combination expected resistant
isolate_treatment %>%
  filter(
    expected_resistant,
    exact_ast_status == "S"
  ) %>%
  count(
    organism,
    antibiotic,
    sort = TRUE
  ) %>%
  print(n = Inf)


#_______________________________________________
# Remaining unresolved organism-antibiotic pairs

remaining_unresolved <- isolate_treatment %>%
  filter(
    is.na(interpreted_ast)
  ) %>%
  count(
    organism_group,
    organism,
    antibiotic,
    sort = TRUE,
    name = "n"
  )

remaining_unresolved %>% print(n = Inf)


remaining_unresolved %>%
  group_by(
    organism_group
  ) %>%
  summarise(
    n_pairs = n(),
    n_observations = sum(n),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_observations)
  )







#_______________________________________________
# 3.7. Additional expected-resistant combinations

isolate_treatment <- isolate_treatment %>%
  mutate(
    additional_expected_resistant = case_when(
      
      # Other Gram-negative bacteria:
      # expected resistant to glycopeptides,
      # lincosamides and oxazolidinones
      organism_group == "Other Gram-negative" &
        antibiotic %in% c(
          "Vancomycin",
          "Clindamycin",
          "Linezolid"
        ) ~ TRUE,
      
      # Enterococci: cephalosporins
      organism_group == "Enterococci" &
        antibiotic %in% c(
          "Cefoperazone",
          "Cefoperazone/Sulbactam"
        ) ~ TRUE,
      
      # Enterococci: macrolides / lincosamides
      organism_group == "Enterococci" &
        antibiotic %in% c(
          "Azithromycin",
          "Erythromycin",
          "Clindamycin"
        ) ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    expected_resistant =
      expected_resistant |
      additional_expected_resistant,
    
    interpretation_source = case_when(
      is.na(interpreted_ast) &
        expected_resistant ~
        "Expected resistant",
      
      TRUE ~ interpretation_source
    ),
    
    interpreted_ast = case_when(
      is.na(interpreted_ast) &
        expected_resistant ~ "R",
      
      TRUE ~ interpreted_ast
    )
  )





#_______________________________________________
# Staphylococcal methicillin phenotype
# from oxacillin / cefoxitin AST

staph_methicillin <- ast_primary %>%
  filter(
    organism_group == "Staphylococci",
    ast_antibiotic %in% c(
      "Oxacillin",
      "Cefoxitin"
    ),
    ast_result %in% c(
      "Susceptible",
      "Intermediate",
      "Resistant"
    )
  ) %>%
  mutate(
    methicillin_result = case_when(
      ast_result == "Susceptible" ~ "S",
      ast_result %in%
        c("Intermediate", "Resistant") ~ "NS"
    )
  ) %>%
  group_by(isolate_id) %>%
  summarise(
    n_meth_S =
      sum(methicillin_result == "S"),
    
    n_meth_NS =
      sum(methicillin_result == "NS"),
    
    methicillin_status = case_when(
      n_meth_S > 0 &
        n_meth_NS == 0 ~ "S",
      
      n_meth_NS > 0 &
        n_meth_S == 0 ~ "NS",
      
      n_meth_S > 0 &
        n_meth_NS > 0 ~ "Conflict",
      
      TRUE ~ NA_character_
    ),
    
    .groups = "drop"
  )

table(
  staph_methicillin$methicillin_status,
  useNA = "ifany"
)



isolate_treatment <- isolate_treatment %>%
  left_join(
    staph_methicillin %>%
      select(
        isolate_id,
        methicillin_status
      ),
    by = "isolate_id"
  )





staph_beta_lactams <- c(
  "Benzylpenicillin (Penicillin G)",
  "Amoxicillin",
  "Ampicillin",
  "Amoxicillin/Clavulanic acid",
  "Ampicillin/Sulbactam",
  "Ampicillin/Cloxacillin",
  "Amoxicillin/Flucloxacillin",
  "Cloxacillin",
  "Flucloxacillin",
  "Piperacillin/Tazobactam",
  "Cefalexin",
  "Cefazolin",
  "Cefepime",
  "Cefoperazone",
  "Cefoperazone/Sulbactam",
  "Cefotaxime",
  "Ceftriaxone",
  "Cefuroxime",
  "Imipenem/Cilastatin",
  "Meropenem",
  "Ertapenem"
)

staph_beta_lactams_if_mss <- c(
  "Amoxicillin/Clavulanic acid",
  "Ampicillin/Sulbactam",
  "Ampicillin/Cloxacillin",
  "Amoxicillin/Flucloxacillin",
  "Cloxacillin",
  "Flucloxacillin",
  "Piperacillin/Tazobactam",
  "Cefalexin",
  "Cefazolin",
  "Cefepime",
  "Cefoperazone",
  "Cefoperazone/Sulbactam",
  "Cefotaxime",
  "Ceftriaxone",
  "Cefuroxime",
  "Imipenem/Cilastatin",
  "Meropenem",
  "Ertapenem"
)



#_______________________________________________
# Aminoglycoside-class susceptibility surrogate

aminoglycosides <- c(
  "Amikacin",
  "Gentamicin",
  "Netilmicin",
  "Tobramycin"
)

aminoglycoside_surrogate <- ast_primary %>%
  filter(
    ast_antibiotic %in% aminoglycosides,
    ast_result %in% c(
      "Susceptible",
      "Intermediate",
      "Resistant"
    )
  ) %>%
  mutate(
    amino_result = case_when(
      ast_result == "Susceptible" ~ "S",
      ast_result %in%
        c("Intermediate", "Resistant") ~ "NS"
    )
  ) %>%
  group_by(isolate_id) %>%
  summarise(
    n_amino_S =
      sum(amino_result == "S"),
    
    n_amino_NS =
      sum(amino_result == "NS"),
    
    amino_surrogate = case_when(
      n_amino_S > 0 &
        n_amino_NS == 0 ~ "S",
      
      n_amino_NS > 0 &
        n_amino_S == 0 ~ "NS",
      
      n_amino_S > 0 &
        n_amino_NS > 0 ~ "Conflict",
      
      TRUE ~ NA_character_
    ),
    
    .groups = "drop"
  )

table(
  aminoglycoside_surrogate$amino_surrogate,
  useNA = "ifany"
)




isolate_treatment <- isolate_treatment %>%
  left_join(
    aminoglycoside_surrogate %>%
      select(
        isolate_id,
        amino_surrogate
      ),
    by = "isolate_id"
  )




isolate_treatment <- isolate_treatment %>%
  mutate(
    phenotypic_call = case_when(
      
      # No qualifying systemic antibacterial treatment
      # means the pathogen phenotype was not covered.
      is.na(antibiotic) ~
        "Inactive",
      
      # Methicillin resistance overrides an apparently
      # susceptible beta-lactam result
      organism_group == "Staphylococci" &
        methicillin_status == "NS" &
        antibiotic %in% staph_beta_lactams ~
        "Inactive",
      
      # Exact AST / expected resistant phenotype
      interpreted_ast == "S" ~
        "Active",
      
      interpreted_ast %in% c(
        "I",
        "R"
      ) ~
        "Inactive",
      
      # Methicillin-susceptible staphylococci:
      # infer appropriate beta-lactam activity
      organism_group == "Staphylococci" &
        methicillin_status == "S" &
        antibiotic %in%
        staph_beta_lactams_if_mss ~
        "Active",
      
      # Aminoglycoside class surrogate
      antibiotic %in% aminoglycosides &
        amino_surrogate == "S" ~
        "Active",
      
      antibiotic %in% aminoglycosides &
        amino_surrogate == "NS" ~
        "Inactive",
      
      TRUE ~
        "Indeterminate"
    ),
    
    phenotypic_source = case_when(
      
      is.na(antibiotic) ~
        "No systemic antibacterial treatment recorded",
      
      organism_group == "Staphylococci" &
        methicillin_status == "NS" &
        antibiotic %in% staph_beta_lactams ~
        "Oxacillin/cefoxitin phenotype",
      
      !is.na(interpreted_ast) ~
        interpretation_source,
      
      organism_group == "Staphylococci" &
        methicillin_status == "S" &
        antibiotic %in%
        staph_beta_lactams_if_mss ~
        "Oxacillin/cefoxitin phenotype",
      
      antibiotic %in% aminoglycosides &
        amino_surrogate %in% c(
          "S",
          "NS"
        ) ~
        "Aminoglycoside-class surrogate",
      
      TRUE ~
        "Unresolved"
    )
  )



table(
  isolate_treatment$phenotypic_call,
  useNA = "ifany"
)

table(
  isolate_treatment$phenotypic_source,
  useNA = "ifany"
)




remaining_unresolved2 <- isolate_treatment %>%
  filter(
    phenotypic_call == "Indeterminate"
  ) %>%
  count(
    organism_group,
    organism,
    antibiotic,
    sort = TRUE,
    name = "n"
  )

remaining_unresolved2 %>%
  print(n = Inf)

remaining_unresolved2 %>%
  group_by(
    organism_group
  ) %>%
  summarise(
    n_pairs = n(),
    n_observations = sum(n),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_observations)
  )










#_______________________________________________
# 3.8. Isolate-level phenotypic antibiotic coverage

isolate_coverage <- isolate_treatment %>%
  group_by(
    isolate_id,
    anon_id,
    country,
    CONTINENT,
    culture_day,
    organism,
    organism_group
  ) %>%
  summarise(
    n_antibiotics =
      n_distinct(antibiotic),
    
    n_active =
      sum(
        phenotypic_call == "Active"
      ),
    
    n_inactive =
      sum(
        phenotypic_call == "Inactive"
      ),
    
    n_indeterminate =
      sum(
        phenotypic_call == "Indeterminate"
      ),
    
    active_antibiotics = paste(
      sort(unique(
        antibiotic[
          phenotypic_call == "Active"
        ]
      )),
      collapse = "; "
    ),
    
    indeterminate_antibiotics = paste(
      sort(unique(
        antibiotic[
          phenotypic_call == "Indeterminate"
        ]
      )),
      collapse = "; "
    ),
    
    isolate_status = case_when(
      
      n_active > 0 ~
        "Covered",
      
      n_active == 0 &
        n_indeterminate > 0 ~
        "Indeterminate",
      
      TRUE ~
        "Not covered"
    ),
    
    .groups = "drop"
  )


# Overall isolate-level coverage
table(
  isolate_coverage$isolate_status,
  useNA = "ifany"
)


# By organism group
isolate_coverage %>%
  count(
    organism_group,
    isolate_status
  ) %>%
  group_by(
    organism_group
  ) %>%
  mutate(
    pct = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    organism_group,
    isolate_status
  )



#_______________________________________________
# Isolates for which coverage is still indeterminate

indeterminate_isolates <- isolate_coverage %>%
  filter(
    isolate_status == "Indeterminate"
  )

nrow(indeterminate_isolates)

indeterminate_isolates %>%
  count(
    organism_group,
    organism,
    sort = TRUE
  ) %>%
  print(n = Inf)


#_______________________________________________
# Unresolved drug pairs among indeterminate isolates only

indeterminate_drivers <- isolate_treatment %>%
  semi_join(
    indeterminate_isolates %>%
      select(isolate_id),
    by = "isolate_id"
  ) %>%
  filter(
    phenotypic_call == "Indeterminate"
  ) %>%
  count(
    organism_group,
    organism,
    antibiotic,
    sort = TRUE,
    name = "n"
  )

indeterminate_drivers %>%
  print(n = Inf)

indeterminate_drivers %>%
  group_by(
    organism_group
  ) %>%
  summarise(
    n_pairs = n(),
    n_observations = sum(n),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_observations)
  )



#_______________________________________________
# 3.9. Final identification of duplicate pathogen phenotypes

# Same infant + same culture day + same organism
dup_groups_final <- micro_primary %>%
  count(
    anon_id,
    culture_day,
    organism,
    name = "n_records"
  ) %>%
  filter(
    n_records > 1
  )


# AST results among duplicate organism-day records
dup_ast_final <- ast_primary %>%
  semi_join(
    micro_primary %>%
      semi_join(
        dup_groups_final,
        by = c(
          "anon_id",
          "culture_day",
          "organism"
        )
      ) %>%
      distinct(isolate_id),
    by = "isolate_id"
  ) %>%
  filter(
    !is.na(ast_antibiotic)
  ) %>%
  mutate(
    ast_evaluable = case_when(
      ast_result %in%
        c(
          "Susceptible",
          "Intermediate",
          "Resistant"
        ) ~ ast_result,
      
      TRUE ~ NA_character_
    )
  )


# Check whether duplicate records contain a genuine
# S/I/R disagreement for the same antibiotic
dup_ast_conflict_final <- dup_ast_final %>%
  group_by(
    anon_id,
    culture_day,
    organism,
    ast_antibiotic
  ) %>%
  summarise(
    n_evaluable_results =
      n_distinct(
        ast_evaluable,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  group_by(
    anon_id,
    culture_day,
    organism
  ) %>%
  summarise(
    phenotypically_discordant =
      any(n_evaluable_results > 1),
    
    .groups = "drop"
  )


# Final duplicate classification
dup_group_final <- dup_groups_final %>%
  left_join(
    dup_ast_conflict_final,
    by = c(
      "anon_id",
      "culture_day",
      "organism"
    )
  ) %>%
  mutate(
    phenotypically_discordant =
      replace_na(
        phenotypically_discordant,
        FALSE
      ),
    
    duplicate_type = case_when(
      phenotypically_discordant ~
        "Phenotypically discordant",
      
      TRUE ~
        "Concordant/complementary AST"
    )
  )

dup_group_final %>%
  arrange(
    duplicate_type,
    anon_id,
    culture_day
  ) %>%
  print(n = Inf)

table(
  dup_group_final$duplicate_type
)




#_______________________________________________
# Create phenotype-unit identifier

isolate_treatment_final <- isolate_treatment %>%
  left_join(
    dup_group_final %>%
      select(
        anon_id,
        culture_day,
        organism,
        n_records,
        phenotypically_discordant
      ),
    by = c(
      "anon_id",
      "culture_day",
      "organism"
    )
  ) %>%
  mutate(
    n_records =
      replace_na(
        n_records,
        1L
      ),
    
    phenotypically_discordant =
      replace_na(
        phenotypically_discordant,
        FALSE
      ),
    
    phenotype_unit_id = case_when(
      
      n_records > 1 &
        !phenotypically_discordant ~
        paste(
          anon_id,
          culture_day,
          organism,
          sep = "_"
        ),
      
      TRUE ~
        isolate_id
    )
  )


# There should be no Active vs Inactive disagreement
# inside a phenotype unit that we intend to collapse

phenotype_call_conflict <- isolate_treatment_final %>%
  filter(
    n_records > 1,
    !phenotypically_discordant
  ) %>%
  group_by(
    phenotype_unit_id,
    antibiotic
  ) %>%
  summarise(
    n_evaluable_calls =
      n_distinct(
        phenotypic_call[
          phenotypic_call != "Indeterminate"
        ]
      ),
    calls = paste(
      sort(unique(
        phenotypic_call[
          phenotypic_call != "Indeterminate"
        ]
      )),
      collapse = " / "
    ),
    .groups = "drop"
  ) %>%
  filter(
    n_evaluable_calls > 1
  )

phenotype_call_conflict %>%
  print(n = Inf)







#_______________________________________________
# 3.10. Phenotype-event level antibiotic coverage

phenotype_coverage <- isolate_treatment_final %>%
  group_by(
    phenotype_unit_id,
    anon_id,
    country,
    CONTINENT,
    culture_day,
    organism,
    organism_group
  ) %>%
  summarise(
    n_raw_isolates =
      n_distinct(isolate_id),
    
    n_antibiotics =
      n_distinct(antibiotic),
    
    n_active =
      n_distinct(
        antibiotic[
          phenotypic_call == "Active"
        ]
      ),
    
    n_inactive =
      n_distinct(
        antibiotic[
          phenotypic_call == "Inactive"
        ]
      ),
    
    n_indeterminate =
      n_distinct(
        antibiotic[
          phenotypic_call == "Indeterminate"
        ]
      ),
    
    active_antibiotics = paste(
      sort(unique(
        antibiotic[
          phenotypic_call == "Active"
        ]
      )),
      collapse = "; "
    ),
    
    indeterminate_antibiotics = paste(
      sort(unique(
        antibiotic[
          phenotypic_call == "Indeterminate"
        ]
      )),
      collapse = "; "
    ),
    
    phenotype_status = case_when(
      
      n_active > 0 ~
        "Covered",
      
      n_active == 0 &
        n_indeterminate > 0 ~
        "Indeterminate",
      
      TRUE ~
        "Not covered"
    ),
    
    .groups = "drop"
  )


# Checks
nrow(phenotype_coverage)

table(
  phenotype_coverage$phenotype_status,
  useNA = "ifany"
)

phenotype_coverage %>%
  count(
    organism_group,
    phenotype_status
  ) %>%
  group_by(
    organism_group
  ) %>%
  mutate(
    pct = 100 * n / sum(n)
  ) %>%
  ungroup()



#_______________________________________________
# 3.11. Infant-level phenotypic antibiotic coverage

infant_coverage <- phenotype_coverage %>%
  group_by(
    anon_id,
    country,
    CONTINENT
  ) %>%
  summarise(
    n_phenotypes =
      n(),
    
    n_covered =
      sum(
        phenotype_status == "Covered"
      ),
    
    n_not_covered =
      sum(
        phenotype_status == "Not covered"
      ),
    
    n_indeterminate =
      sum(
        phenotype_status == "Indeterminate"
      ),
    
    infant_status = case_when(
      
      # Any definitively uncovered pathogen phenotype
      # means the infant was not fully covered
      n_not_covered > 0 ~
        "Not fully covered",
      
      # Every phenotype covered
      n_covered == n_phenotypes ~
        "Fully covered",
      
      # No definitively uncovered phenotype,
      # but at least one remains unresolved
      TRUE ~
        "Indeterminate"
    ),
    
    .groups = "drop"
  )


# Overall
table(
  infant_coverage$infant_status,
  useNA = "ifany"
)



#_______________________________________________
# Infant-level results by continent

infant_coverage_summary <- infant_coverage %>%
  count(
    CONTINENT,
    infant_status,
    name = "n"
  ) %>%
  group_by(
    CONTINENT
  ) %>%
  mutate(
    total = sum(n),
    pct = 100 * n / total
  ) %>%
  ungroup()

infant_coverage_summary


#_______________________________________________
# Evaluable infants only

infant_coverage_evaluable <- infant_coverage %>%
  filter(
    infant_status != "Indeterminate"
  ) %>%
  count(
    CONTINENT,
    infant_status,
    name = "n"
  ) %>%
  group_by(
    CONTINENT
  ) %>%
  mutate(
    evaluable_n = sum(n),
    pct = 100 * n / evaluable_n
  ) %>%
  ungroup()

infant_coverage_evaluable



infant_coverage %>%
  summarise(
    n_total = n(),
    
    n_fully_covered =
      sum(
        infant_status == "Fully covered"
      ),
    
    n_not_fully_covered =
      sum(
        infant_status == "Not fully covered"
      ),
    
    n_indeterminate =
      sum(
        infant_status == "Indeterminate"
      ),
    
    n_evaluable =
      n_fully_covered +
      n_not_fully_covered,
    
    pct_fully_covered_evaluable =
      100 *
      n_fully_covered /
      n_evaluable,
    
    pct_indeterminate =
      100 *
      n_indeterminate /
      n_total
  )






# 3.10. Final phenotype-event and infant-level coverage
# Collapse concordant/complementary duplicate records
# while retaining phenotypically discordant isolates separately
phenotype_coverage <- isolate_treatment_final %>%
  group_by(
    phenotype_unit_id,
    anon_id,
    country,
    CONTINENT,
    culture_day,
    organism,
    organism_group
  ) %>%
  summarise(
    n_raw_isolates = n_distinct(isolate_id),
    
    n_active = n_distinct(
      antibiotic[phenotypic_call == "Active"]
    ),
    
    n_inactive = n_distinct(
      antibiotic[phenotypic_call == "Inactive"]
    ),
    
    n_indeterminate = n_distinct(
      antibiotic[phenotypic_call == "Indeterminate"]
    ),
    
    phenotype_status = case_when(
      n_active > 0 ~ "Covered",
      
      n_active == 0 &
        n_indeterminate > 0 ~ "Indeterminate",
      
      TRUE ~ "Not covered"
    ),
    
    .groups = "drop"
  )


#_______________________________________________
# Infant-level coverage

infant_coverage <- phenotype_coverage %>%
  group_by(
    anon_id,
    country,
    CONTINENT
  ) %>%
  summarise(
    n_phenotypes = n(),
    
    n_covered =
      sum(phenotype_status == "Covered"),
    
    n_not_covered =
      sum(phenotype_status == "Not covered"),
    
    n_indeterminate =
      sum(phenotype_status == "Indeterminate"),
    
    infant_status = case_when(
      
      # Any definitively uncovered phenotype
      # means not fully covered
      n_not_covered > 0 ~
        "Not fully covered",
      
      # All pathogen phenotypes covered
      n_covered == n_phenotypes ~
        "Fully covered",
      
      # No uncovered phenotype, but at least one
      # phenotype remains indeterminate
      TRUE ~
        "Indeterminate"
    ),
    
    .groups = "drop"
  )


#_______________________________________________
# Final Africa / Asia / overall result

final_coverage_continent <- bind_rows(
  infant_coverage %>% mutate(region = CONTINENT),
  infant_coverage %>% mutate(region = "Overall")
) %>%
  group_by(region) %>%
  summarise(
    N = n(),
    Fully_covered = sum(infant_status == "Fully covered"),
    Not_fully_covered = sum(infant_status == "Not fully covered"),
    Indeterminate = sum(infant_status == "Indeterminate"),
    Evaluable = Fully_covered + Not_fully_covered,
    Coverage_evaluable_pct = 100 * Fully_covered / Evaluable,
    Not_covered_evaluable_pct = 100 * Not_fully_covered / Evaluable,
    Indeterminate_pct = 100 * Indeterminate / N,
    .groups = "drop"
  ) %>%
  mutate(across(ends_with("_pct"),~ round(.x, 1)))

final_coverage_country <- bind_rows(
  infant_coverage %>% mutate(region = country),
  infant_coverage %>% mutate(region = "Overall")
) %>%
  group_by(region) %>%
  summarise(
    N = n(),
    Fully_covered = sum(infant_status == "Fully covered"),
    Not_fully_covered = sum(infant_status == "Not fully covered"),
    Indeterminate = sum(infant_status == "Indeterminate"),
    Evaluable = Fully_covered + Not_fully_covered,
    Coverage_evaluable_pct = 100 * Fully_covered / Evaluable,
    Not_covered_evaluable_pct = 100 * Not_fully_covered / Evaluable,
    Indeterminate_pct = 100 * Indeterminate / N,
    .groups = "drop"
  ) %>%
  mutate(across(ends_with("_pct"),~ round(.x, 1)))


final_coverage <- bind_rows(final_coverage_continent, final_coverage_country)

dir.create("outputs/pipelines/africa_asia/neoobs", recursive = TRUE, showWarnings = FALSE)
neoobs.output <- "outputs/pipelines/africa_asia/neoobs"
write.csv(final_coverage, file.path(neoobs.output,"effective_cover.csv"))





#_______________________________________________=========
# 4. Timing of antibiotic treatment =====================

# Primary analysis:
# first documented antibacterial treatment
# relative to clinical recognition of significant sepsis


#_______________________________________________
# Check timing variables

summary(dat5$day)
sort(unique(dat5$day))

summary(dat2$agebc)
summary(dat3$age_sep_diag)
summary(dat3$age_ab_start)

table(dat3$ab_enrol, useNA = "ifany")

grep(
  "^abx_[0-9]+_(day|time|route|prim_indic|sec_indic)$",
  names(dat5),
  value = TRUE
)


#_______________________________________________
# Treatment objects from Section 1

# prebc_treatment: infant-level indicator of explicit, systemic
# antibacterial treatment in the 24 h preceding enrolment culture.
# abx_dose_treatment: prospective systemic antibacterial treatment
# after excluding prophylactic-only and inhaled doses.

table(prebc_treatment$prebc_treatment, useNA = "ifany")
table(abx_dose_treatment$route, useNA = "ifany")
abx_dose_treatment %>% count(prim_indic, sort = TRUE) %>% print(n = Inf)


#_______________________________________________
# Earliest CRF5 antibacterial treatment day

# Study day 1 corresponds to the baseline/enrolment
# observation window, so convert to relative day:
# day 1 = 0, day 2 = 1, ...

first_abx_crf5 <- abx_dose_treatment %>%
  group_by(
    anon_id,
    country,
    CONTINENT
  ) %>%
  summarise(
    first_abx_study_day =
      min(study_day, na.rm = TRUE),
    first_abx_rel_day_crf5 =
      first_abx_study_day - 1,
    .groups = "drop"
  )

nrow(first_abx_crf5)
n_distinct(first_abx_crf5$anon_id)

table(
  first_abx_crf5$first_abx_study_day,
  useNA = "ifany"
)




#_______________________________________________
# First treatment relative to sepsis recognition

timing_first_abx <- dat2 %>%
  select(
    anon_id,
    country,
    agebc
  ) %>%
  left_join(
    dat3 %>%
      select(
        anon_id,
        age_sep_diag,
        age_ab_start,
        ab_enrol,
        abhist_bc_num
      ),
    by = "anon_id"
  ) %>%
  left_join(
    first_abx_crf5 %>%
      select(
        anon_id,
        first_abx_study_day,
        first_abx_rel_day_crf5
      ),
    by = "anon_id"
  ) %>%
  left_join(
    prebc_treatment,
    by = "anon_id"
  ) %>%
  mutate(
    CONTINENT = case_when(
      country %in% africa_countries ~ "Africa",
      country %in% asia_countries ~ "Asia",
      TRUE ~ NA_character_
    ),
    
    prebc_treatment = replace_na(prebc_treatment, FALSE),
    prebc_prophylaxis = replace_na(prebc_prophylaxis, FALSE),
    prebc_unknown = replace_na(prebc_unknown, FALSE),
    
    # Recognition day relative to enrolment/baseline day
    recognition_rel_day = age_sep_diag - agebc,
    
    # Pre-culture treatment is assigned to enrolment day 0.
    # Otherwise use the earliest observed CRF5 treatment day.
    first_abx_rel_day = case_when(
      prebc_treatment ~ 0,
      !is.na(first_abx_rel_day_crf5) ~ first_abx_rel_day_crf5,
      TRUE ~ NA_real_
    ),
    
    timing_source = case_when(
      prebc_treatment ~ "Pre-culture treatment assigned to enrolment day",
      !is.na(first_abx_rel_day_crf5) ~ "CRF5 first treatment dose",
      TRUE ~ "Unavailable"
    ),
    
    delay_recog_first_abx_days =
      first_abx_rel_day - recognition_rel_day,
    
    timing_first_abx_cat = case_when(
      delay_recog_first_abx_days <= -2 ~ "≥2 days before",
      delay_recog_first_abx_days == -1 ~ "1 day before",
      delay_recog_first_abx_days == 0 ~ "Same day",
      delay_recog_first_abx_days == 1 ~ "1 day after",
      delay_recog_first_abx_days >= 2 ~ "≥2 days after",
      TRUE ~ NA_character_
    ),
    
    timing_first_abx_cat = factor(
      timing_first_abx_cat,
      levels = c(
        "≥2 days before",
        "1 day before",
        "Same day",
        "1 day after",
        "≥2 days after"
      )
    )
  ) %>%
  filter(!is.na(CONTINENT))


#_______________________________________________
# Primary timing distribution

table(
  timing_first_abx$timing_first_abx_cat,
  useNA = "ifany"
)

table(
  timing_first_abx$timing_source,
  useNA = "ifany"
)

table(
  timing_first_abx$prebc_treatment,
  timing_first_abx$timing_first_abx_cat,
  useNA = "ifany"
)

timing_first_abx %>%
  summarise(
    n = sum(!is.na(delay_recog_first_abx_days)),
    median = median(delay_recog_first_abx_days, na.rm = TRUE),
    q1 = quantile(delay_recog_first_abx_days, 0.25, na.rm = TRUE),
    q3 = quantile(delay_recog_first_abx_days, 0.75, na.rm = TRUE),
    min = min(delay_recog_first_abx_days, na.rm = TRUE),
    max = max(delay_recog_first_abx_days, na.rm = TRUE)
  )


#_______________________________________________
# Compare CRF5 first dose with recorded age_ab_start

# This comparison is QC for the CRF5 study-day scale.
# Therefore, use first_abx_rel_day_crf5 directly even among
# infants with documented pre-culture treatment.

timing_first_abx %>%
  mutate(
    abstart_rel_day = age_ab_start - agebc,
    difference_vs_abstart =
      first_abx_rel_day_crf5 - abstart_rel_day
  ) %>%
  dplyr::count(
    difference_vs_abstart,
    sort = TRUE
  )


#_______________________________________________
# Compare with pre-culture treatment

table(
  timing_first_abx$prebc_treatment,
  timing_first_abx$timing_first_abx_cat,
  useNA = "ifany"
)

table(
  timing_first_abx$prebc_treatment,
  timing_first_abx$first_abx_rel_day_crf5,
  useNA = "ifany"
)

#_______________________________________________
# QC unusual timing among infants without
# documented pre-culture treatment

timing_first_abx %>%
  filter(
    !prebc_treatment,
    abs(delay_recog_first_abx_days) >= 2
  ) %>%
  select(
    anon_id,
    country,
    agebc,
    age_sep_diag,
    recognition_rel_day,
    age_ab_start,
    first_abx_study_day,
    first_abx_rel_day_crf5,
    first_abx_rel_day,
    ab_enrol,
    prebc_treatment,
    prebc_prophylaxis,
    prebc_unknown,
    delay_recog_first_abx_days
  ) %>%
  arrange(
    delay_recog_first_abx_days
  )


#_______________________________________________
# QC pre-culture treatment group

timing_first_abx %>%
  filter(prebc_treatment) %>%
  select(
    anon_id,
    country,
    agebc,
    age_sep_diag,
    recognition_rel_day,
    age_ab_start,
    first_abx_study_day,
    first_abx_rel_day_crf5,
    ab_enrol,
    prebc_prophylaxis,
    prebc_unknown
  ) %>%
  arrange(
    country,
    anon_id
  )

table(timing_first_abx$timing_first_abx_cat, useNA="ifany")


#_______________________________________________
# Timing by continent and country

geo_order <- c(
  "Africa",
  "Africa_Kenya",
  "Africa_South Africa",
  "Africa_Uganda",
  "Asia",
  "Asia_Bangladesh",
  "Asia_India",
  "Asia_Thailand",
  "Asia_Vietnam"
)

timing_cat_order <- c(
  "≥2 days before",
  "1 day before",
  "Same day",
  "1 day after",
  "≥2 days after",
  "Timing unavailable"
)


#_______________________________________________
# Create continent and country observations

timing_geo <- bind_rows(
  timing_first_abx %>%
    transmute(
      anon_id,
      geo = CONTINENT,
      timing_cat = as.character(timing_first_abx_cat)
    ),
  
  timing_first_abx %>%
    transmute(
      anon_id,
      geo = paste(CONTINENT, country, sep = "_"),
      timing_cat = as.character(timing_first_abx_cat)
    )
) %>%
  mutate(
    timing_cat = replace_na(timing_cat, "Timing unavailable"),
    geo = factor(geo, levels = geo_order)
  ) %>%
  filter(!is.na(geo))


#_______________________________________________
# Geographic denominators

timing_denominator <- timing_geo %>%
  group_by(geo) %>%
  summarise(
    denominator = n_distinct(anon_id),
    .groups = "drop"
  )


#_______________________________________________
# Timing counts and percentages

timing_summary <- timing_geo %>%
  group_by(geo, timing_cat) %>%
  summarise(
    n = n_distinct(anon_id),
    .groups = "drop"
  ) %>%
  complete(
    geo = factor(geo_order, levels = geo_order),
    timing_cat = timing_cat_order,
    fill = list(n = 0)
  ) %>%
  left_join(
    timing_denominator,
    by = "geo"
  ) %>%
  mutate(
    value = paste0(
      n,
      " (",
      sprintf("%.1f", 100 * n / denominator),
      "%)"
    ),
    timing_cat = factor(
      timing_cat,
      levels = timing_cat_order
    )
  )


#_______________________________________________
# Final table

timing_table <- timing_summary %>%
  select(
    geo,
    timing_cat,
    value
  ) %>%
  pivot_wider(
    names_from = timing_cat,
    values_from = value
  ) %>%
  left_join(
    timing_denominator,
    by = "geo"
  ) %>%
  mutate(
    geo = factor(geo, levels = geo_order),
    `Geographic group` = case_when(
      geo == "Africa" ~ "Africa",
      geo == "Africa_Kenya" ~ "Kenya",
      geo == "Africa_South Africa" ~ "South Africa",
      geo == "Africa_Uganda" ~ "Uganda",
      geo == "Asia" ~ "Asia",
      geo == "Asia_Bangladesh" ~ "Bangladesh",
      geo == "Asia_India" ~ "India",
      geo == "Asia_Thailand" ~ "Thailand",
      geo == "Asia_Vietnam" ~ "Vietnam"
    )
  ) %>%
  arrange(geo) %>%
  select(
    `Geographic group`,
    `Number of infants` = denominator,
    all_of(timing_cat_order)
  )

timing_table

dir.create("outputs/pipelines/africa_asia/neoobs", recursive = TRUE, showWarnings = FALSE)
neoobs.output <- "outputs/pipelines/africa_asia/neoobs"
write.csv(timing_table, file.path(neoobs.output,"abx_timing_recog.csv"))





#_______________________________________________=========
# 4.2. Reported illness onset to antibiotic treatment ====

# Secondary analysis:
# approximate timing from reported illness onset
# to first antibacterial treatment among babies admitted
# from the community with sepsis recognized at admission


#_______________________________________________
# Check onset and admission variables

summary(dat2$age_adm_hosp)
summary(dat2$age_adm_unit)
summary(dat2$unwell_prior)

table(dat2$adm_direct, useNA = "ifany")
table(dat2$birth_stat, useNA = "ifany")
table(dat2$unwell_prior, useNA = "ifany")


#_______________________________________________
# Identify community admissions

community_ids <- dat2 %>%
  mutate(
    birth_stat_chr = str_to_lower(str_trim(as.character(birth_stat))),
    community_admission = birth_stat_chr %in% c(
      "1", "5", "6",
      "baby born at home, admitted to this hospital",
      "baby born at another hospital, discharged, admitted to this hospital",
      "baby born at this hospital, discharged, readmitted"
    )
  ) %>%
  filter(community_admission) %>%
  pull(anon_id)

length(unique(community_ids))


#_______________________________________________
# Construct onset-to-antibiotic timing
# among community admissions only

timing_onset_abx <- timing_first_abx %>%
  filter(anon_id %in% community_ids) %>%
  left_join(
    dat2 %>%
      select(
        anon_id,
        age_adm_hosp,
        age_adm_unit,
        unwell_prior,
        birth_hosp,
        adm_direct,
        birth_stat
      ),
    by = "anon_id"
  ) %>%
  mutate(
    adm_direct_yes =
      str_to_lower(str_trim(as.character(adm_direct))) %in%
      c("yes", "1", "true"),
    
    # Prefer hospital-admission age.
    # If unavailable, use unit-admission age only when
    # the baby was admitted directly to that unit.
    age_adm_best = case_when(
      !is.na(age_adm_hosp) ~ as.numeric(age_adm_hosp),
      is.na(age_adm_hosp) &
        adm_direct_yes &
        !is.na(age_adm_unit) ~ as.numeric(age_adm_unit),
      TRUE ~ NA_real_
    ),
    
    admission_day_source = case_when(
      !is.na(age_adm_hosp) ~ "Hospital admission",
      is.na(age_adm_hosp) &
        adm_direct_yes &
        !is.na(age_adm_unit) ~
        "Unit admission, direct-admission fallback",
      TRUE ~ "Unavailable"
    ),
    
    # Community admission excludes continuous hospitalisation
    # and transfer directly from another hospital.
    birth_stat_chr =
      str_to_lower(str_trim(as.character(birth_stat))),
    
    community_admission =
      birth_stat_chr %in% c(
        "1",
        "5",
        "6",
        "baby born at home, admitted to this hospital",
        "baby born at another hospital, discharged, admitted to this hospital",
        "baby born at this hospital, discharged, readmitted"
      ),
    
    recognition_after_admission =
      age_sep_diag - age_adm_best,
    
    # Primary onset analysis:
    # sepsis recognized on the hospital-admission day.
    episode_at_admission =
      community_admission &
      !is.na(recognition_after_admission) &
      recognition_after_admission == 0,
    
    onset_reported =
      episode_at_admission &
      !is.na(unwell_prior),
    
    # Hospital admission relative to enrolment/baseline
    admission_rel_day =
      age_adm_best - agebc,
    
    # unwell_prior is reported in hours.
    # Approximate illness onset relative to baseline.
    onset_rel_day =
      admission_rel_day - unwell_prior / 24,
    
    onset_evaluable =
      onset_reported &
      !is.na(first_abx_rel_day) &
      age_adm_best >= 0 &
      agebc >= 0,
    
    # Same first-treatment definition as the
    # recognition-to-antibiotic analysis.
    delay_onset_first_abx_days = if_else(
      onset_evaluable,
      first_abx_rel_day - onset_rel_day,
      NA_real_
    ),
    
    delay_onset_first_abx_hours =
      delay_onset_first_abx_days * 24
  )


#_______________________________________________
# QC source population

timing_onset_abx %>%
  summarise(
    n_total = n(),
    n_unwell_prior = sum(!is.na(unwell_prior)),
    n_community_admission =
      sum(community_admission, na.rm = TRUE),
    n_episode_at_admission =
      sum(episode_at_admission, na.rm = TRUE),
    n_onset_reported =
      sum(onset_reported, na.rm = TRUE),
    n_evaluable =
      sum(onset_evaluable, na.rm = TRUE)
  )


#_______________________________________________
# QC admission-day recovery

timing_onset_abx %>%
  filter(
    community_admission,
    !is.na(unwell_prior)
  ) %>%
  summarise(
    n = n(),
    n_age_adm_hosp =
      sum(!is.na(age_adm_hosp)),
    n_age_adm_unit =
      sum(!is.na(age_adm_unit)),
    n_direct_unit_fallback =
      sum(
        is.na(age_adm_hosp) &
          adm_direct_yes &
          !is.na(age_adm_unit)
      ),
    n_admission_day_available =
      sum(!is.na(age_adm_best)),
    n_first_abx_available =
      sum(!is.na(first_abx_rel_day))
  )

table(
  timing_onset_abx$admission_day_source,
  useNA = "ifany"
)


#_______________________________________________
# QC hospital vs unit admission age

timing_onset_abx %>%
  filter(
    community_admission,
    !is.na(unwell_prior),
    !is.na(age_adm_hosp),
    !is.na(age_adm_unit)
  ) %>%
  count(
    difference = age_adm_unit - age_adm_hosp,
    sort = TRUE
  )


#_______________________________________________
# QC community-admission definition

timing_onset_abx %>%
  filter(!is.na(unwell_prior)) %>%
  count(
    birth_stat,
    community_admission,
    sort = TRUE
  )


#_______________________________________________
# QC recognition relative to admission

timing_onset_abx %>%
  filter(
    community_admission,
    !is.na(unwell_prior)
  ) %>%
  count(
    recognition_after_admission,
    sort = TRUE
  )


#_______________________________________________
# QC primary onset-analysis population

timing_onset_abx %>%
  filter(onset_evaluable) %>%
  select(
    anon_id,
    country,
    agebc,
    age_adm_hosp,
    age_adm_unit,
    age_adm_best,
    admission_day_source,
    age_sep_diag,
    recognition_after_admission,
    unwell_prior,
    admission_rel_day,
    onset_rel_day,
    first_abx_rel_day,
    prebc_treatment,
    timing_source,
    delay_onset_first_abx_days,
    delay_onset_first_abx_hours
  ) %>%
  arrange(delay_onset_first_abx_days) %>%
  head(30)


#_______________________________________________
# QC negative onset-to-treatment intervals

timing_onset_abx %>%
  filter(
    onset_evaluable,
    delay_onset_first_abx_days < 0
  ) %>%
  select(
    anon_id,
    country,
    agebc,
    age_adm_best,
    age_sep_diag,
    unwell_prior,
    onset_rel_day,
    first_abx_rel_day,
    prebc_treatment,
    timing_source,
    delay_onset_first_abx_days
  ) %>%
  arrange(delay_onset_first_abx_days)


#_______________________________________________
# QC prolonged onset-to-treatment intervals

timing_onset_abx %>%
  filter(
    onset_evaluable,
    delay_onset_first_abx_days > 7
  ) %>%
  select(
    anon_id,
    country,
    agebc,
    age_adm_best,
    age_sep_diag,
    unwell_prior,
    onset_rel_day,
    first_abx_rel_day,
    prebc_treatment,
    timing_source,
    delay_onset_first_abx_days
  ) %>%
  arrange(desc(delay_onset_first_abx_days))


#_______________________________________________
# QC estimated onset before birth

timing_onset_abx %>%
  filter(
    onset_evaluable,
    onset_rel_day < -agebc
  ) %>%
  select(
    anon_id,
    country,
    agebc,
    age_adm_best,
    age_sep_diag,
    unwell_prior,
    onset_rel_day,
    first_abx_rel_day,
    delay_onset_first_abx_days
  ) %>%
  arrange(onset_rel_day)


#_______________________________________________
# Final onset-to-treatment categories

timing_onset_abx <- timing_onset_abx %>%
  mutate(
    timing_onset_abx_cat = case_when(
      !onset_evaluable ~ NA_character_,
      delay_onset_first_abx_days < 0 ~
        "Before reported onset",
      delay_onset_first_abx_days <= 1 ~
        "≤24 h",
      delay_onset_first_abx_days <= 2 ~
        ">24–48 h",
      delay_onset_first_abx_days > 2 ~
        ">48 h"
    ),
    
    timing_onset_abx_cat = factor(
      timing_onset_abx_cat,
      levels = c(
        "Before reported onset",
        "≤24 h",
        ">24–48 h",
        ">48 h"
      )
    )
  )

table(
  timing_onset_abx$timing_onset_abx_cat,
  useNA = "ifany"
)


#_______________________________________________
# Overall onset-to-treatment summary

timing_onset_abx %>%
  filter(onset_evaluable) %>%
  summarise(
    n = n(),
    median_days =
      median(delay_onset_first_abx_days, na.rm = TRUE),
    q1_days =
      quantile(delay_onset_first_abx_days, 0.25, na.rm = TRUE),
    q3_days =
      quantile(delay_onset_first_abx_days, 0.75, na.rm = TRUE),
    min_days =
      min(delay_onset_first_abx_days, na.rm = TRUE),
    max_days =
      max(delay_onset_first_abx_days, na.rm = TRUE)
  )


#_______________________________________________
# Primary category distribution

timing_onset_abx %>%
  filter(onset_evaluable) %>%
  count(timing_onset_abx_cat) %>%
  mutate(
    percent =
      100 * n / sum(n)
  )


#_______________________________________________
# Sensitivity analysis:
# sepsis recognized on admission day or next day

timing_onset_abx_sens <- timing_onset_abx %>%
  mutate(
    onset_evaluable_sens =
      community_admission &
      !is.na(unwell_prior) &
      !is.na(recognition_after_admission) &
      recognition_after_admission %in% 0:1 &
      !is.na(first_abx_rel_day) &
      age_adm_best >= 0 &
      agebc >= 0,
    
    delay_onset_first_abx_days_sens = if_else(
      onset_evaluable_sens,
      first_abx_rel_day -
        ((age_adm_best - agebc) - unwell_prior / 24),
      NA_real_
    ),
    
    timing_onset_abx_cat_sens = case_when(
      !onset_evaluable_sens ~ NA_character_,
      delay_onset_first_abx_days_sens < 0 ~
        "Before reported onset",
      delay_onset_first_abx_days_sens <= 1 ~
        "≤24 h",
      delay_onset_first_abx_days_sens <= 2 ~
        ">24–48 h",
      delay_onset_first_abx_days_sens > 2 ~
        ">48 h"
    )
  )


#_______________________________________________
# Sensitivity summary

timing_onset_abx_sens %>%
  filter(onset_evaluable_sens) %>%
  summarise(
    n = n(),
    median_days =
      median(delay_onset_first_abx_days_sens, na.rm = TRUE),
    q1_days =
      quantile(delay_onset_first_abx_days_sens, 0.25, na.rm = TRUE),
    q3_days =
      quantile(delay_onset_first_abx_days_sens, 0.75, na.rm = TRUE),
    min_days =
      min(delay_onset_first_abx_days_sens, na.rm = TRUE),
    max_days =
      max(delay_onset_first_abx_days_sens, na.rm = TRUE)
  )

table(
  timing_onset_abx_sens$timing_onset_abx_cat_sens,
  useNA = "ifany"
)







#_______________________________________________
# Onset-to-treatment timing by continent and country

# Confirm no negative intervals
stopifnot(
  sum(
    timing_onset_abx$onset_evaluable &
      timing_onset_abx$delay_onset_first_abx_days < 0,
    na.rm = TRUE
  ) == 0
)

geo_order <- c(
  "Africa",
  "Africa_Kenya",
  "Africa_South Africa",
  "Africa_Uganda",
  "Asia",
  "Asia_Bangladesh",
  "Asia_India",
  "Asia_Thailand",
  "Asia_Vietnam"
)

onset_cat_order <- c(
  "≤48 h",
  ">48 h"
)


#_______________________________________________
# Create 48-hour timing categories

timing_onset_abx <- timing_onset_abx %>%
  mutate(
    timing_onset_abx_cat48 = case_when(
      !onset_evaluable ~ NA_character_,
      delay_onset_first_abx_days <= 2 ~ "≤48 h",
      delay_onset_first_abx_days > 2 ~ ">48 h"
    ),
    timing_onset_abx_cat48 = factor(
      timing_onset_abx_cat48,
      levels = onset_cat_order
    )
  )

table(
  timing_onset_abx$timing_onset_abx_cat48,
  useNA = "ifany"
)


#_______________________________________________
# Create continent and country observations

onset_geo <- bind_rows(
  timing_onset_abx %>%
    filter(onset_evaluable) %>%
    transmute(
      anon_id,
      geo = CONTINENT,
      timing_cat = as.character(timing_onset_abx_cat48)
    ),
  
  timing_onset_abx %>%
    filter(onset_evaluable) %>%
    transmute(
      anon_id,
      geo = paste(CONTINENT, country, sep = "_"),
      timing_cat = as.character(timing_onset_abx_cat48)
    )
) %>%
  mutate(
    geo = factor(geo, levels = geo_order),
    timing_cat = factor(timing_cat, levels = onset_cat_order)
  ) %>%
  filter(
    !is.na(geo),
    !is.na(timing_cat)
  )


#_______________________________________________
# Geographic denominators

onset_denominator <- onset_geo %>%
  group_by(geo) %>%
  summarise(
    denominator = n_distinct(anon_id),
    .groups = "drop"
  )


#_______________________________________________
# Timing counts and percentages

onset_summary <- onset_geo %>%
  group_by(
    geo,
    timing_cat
  ) %>%
  summarise(
    n = n_distinct(anon_id),
    .groups = "drop"
  ) %>%
  complete(
    geo = factor(geo_order, levels = geo_order),
    timing_cat = factor(onset_cat_order, levels = onset_cat_order),
    fill = list(n = 0)
  ) %>%
  left_join(
    onset_denominator,
    by = "geo"
  ) %>%
  mutate(
    value = paste0(
      n,
      " (",
      sprintf("%.1f", 100 * n / denominator),
      "%)"
    )
  )


#_______________________________________________
# Final table

onset_timing_table <- onset_summary %>%
  select(
    geo,
    timing_cat,
    value
  ) %>%
  pivot_wider(
    names_from = timing_cat,
    values_from = value
  ) %>%
  left_join(
    onset_denominator,
    by = "geo"
  ) %>%
  mutate(
    geo = factor(geo, levels = geo_order),
    `Geographic group` = case_when(
      geo == "Africa" ~ "Africa",
      geo == "Africa_Kenya" ~ "Kenya",
      geo == "Africa_South Africa" ~ "South Africa",
      geo == "Africa_Uganda" ~ "Uganda",
      geo == "Asia" ~ "Asia",
      geo == "Asia_Bangladesh" ~ "Bangladesh",
      geo == "Asia_India" ~ "India",
      geo == "Asia_Thailand" ~ "Thailand",
      geo == "Asia_Vietnam" ~ "Vietnam"
    )
  ) %>%
  arrange(geo) %>%
  select(
    `Geographic group`,
    `Number of infants` = denominator,
    all_of(onset_cat_order)
  )

onset_timing_table

dir.create("outputs/pipelines/africa_asia/neoobs", recursive = TRUE, showWarnings = FALSE)
neoobs.output <- "outputs/pipelines/africa_asia/neoobs"
write.csv(onset_timing_table, file.path(neoobs.output,"abx_timing_onset.csv"))









#_______________________________________________=========
# NeoOBS flow-diagram inspection =========================

n_neoobs <- function(x) x %>% distinct(anon_id) %>% nrow()

neoobs_included <- neoobs_denominator %>%
  distinct(anon_id, country, CONTINENT)

neoobs_systemic <- abx_child_drug_treatment %>%
  distinct(anon_id)

neoobs_no_systemic <- neoobs_included %>%
  anti_join(neoobs_systemic, by = "anon_id")

# Blood-culture bacterial pathogens in included Africa/Asia countries
neoobs_culture <- micro_primary %>%
  filter(!is.na(CONTINENT)) %>%
  distinct(anon_id, country, CONTINENT)

neoobs_culture_with_abx <- neoobs_culture %>%
  semi_join(neoobs_systemic, by = "anon_id")

neoobs_culture_without_abx <- neoobs_culture %>%
  anti_join(neoobs_systemic, by = "anon_id")

neoobs_coverage <- infant_coverage %>%
  filter(!is.na(CONTINENT))

neoobs_flow <- tibble(
  branch = c(
    "Cohort", "Cohort",
    "Treatment patterns", "Treatment patterns",
    "Phenotypic activity", "Phenotypic activity", "Phenotypic activity",
    "Phenotypic activity", "Phenotypic activity",
    "Timing", "Timing", "Timing", "Timing",
    "Illness onset", "Illness onset", "Illness onset", "Illness onset"
  ),
  step = c(
    "Total NeoOBS infants",
    "Infants in included African and Asian countries",
    "With ≥1 qualifying systemic antibacterial",
    "With no qualifying systemic antibacterial",
    "With bacterial pathogen identified from blood culture",
    "Culture-positive with systemic antibacterial treatment",
    "Culture-positive without systemic antibacterial treatment",
    "Included in phenotypic-activity analysis",
    "Phenotypic activity evaluable",
    "Available for recognition-to-treatment analysis",
    "With clinical-recognition timing",
    "With first-treatment timing",
    "Recognition-to-treatment timing evaluable",
    "Community-admitted",
    "Sepsis recognized on admission",
    "With reported illness onset",
    "Onset-to-treatment timing evaluable"
  ),
  n = c(
    n_neoobs(dat1),
    n_neoobs(neoobs_included),
    n_neoobs(neoobs_systemic),
    n_neoobs(neoobs_no_systemic),
    n_neoobs(neoobs_culture),
    n_neoobs(neoobs_culture_with_abx),
    n_neoobs(neoobs_culture_without_abx),
    n_neoobs(neoobs_coverage),
    n_neoobs(neoobs_coverage %>% filter(infant_status != "Indeterminate")),
    n_neoobs(timing_first_abx),
    n_neoobs(timing_first_abx %>% filter(!is.na(recognition_rel_day))),
    n_neoobs(timing_first_abx %>% filter(!is.na(first_abx_rel_day))),
    n_neoobs(timing_first_abx %>% filter(!is.na(timing_first_abx_cat))),
    n_neoobs(timing_onset_abx),
    n_neoobs(timing_onset_abx %>% filter(episode_at_admission)),
    n_neoobs(timing_onset_abx %>% filter(onset_reported)),
    n_neoobs(timing_onset_abx %>% filter(onset_evaluable))
  )
)

print(neoobs_flow, n = Inf)