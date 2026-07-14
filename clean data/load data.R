

library(dplyr)

#load data from R project folder:
CLSA_baseline <- read.csv("data/24CA022_UBC_SStringhini_Baseline_DRU_Sep2025CoPv7-2_Qx_CANUE_PA_BS_CSD_FSA.csv")
CLSA_epigenetic_clocks <- read.csv("data/Epigenetic_Clocks_CLSA.csv")
cells_for_Amanda <- read.csv("data/cells_for_Amanda.csv")

CLSA_baseline_and_clocks <- left_join(CLSA_baseline, CLSA_epigenetic_clocks, by = "ADM_EPIGEN2_COM") #join baseline data + epi clocks on Epigenetics subsample ID
CLSA_clocks_cells <- left_join(CLSA_baseline_and_clocks, cells_for_Amanda, by = "ADM_EPIGEN2_COM") #add cell type
save(CLSA_clocks_cells, file = "CLSA_clocks_cells.rds")

#create DNAm dataset 
load("data/CLSA_clocks_cells.rds")#load data
CLSA_methylation <- CLSA_clocks_cells %>% #create subset with only DNAm data 
  filter(!is.na(DNAmAge_COM))
save(CLSA_methylation, file = "CLSA_methylation.rds") #4088 variables, 1478 observations


# create condensed cultural background variable  ---------------------------------------
CLSA_methylation <- CLSA_methylation %>%  #make cult backgrd into 7 category variable
  mutate(
    # 12-category race variable
    Race = case_when(
      SDC_CULT_WH_COM == 1 ~ "White",
      SDC_CULT_ZH_COM == 1 ~ "Chinese",
      SDC_CULT_SA_COM == 1 ~ "South Asian",
      SDC_CULT_BL_COM == 1 ~ "Black",
      SDC_CULT_FP_COM == 1 ~ "Filipino",
      SDC_CULT_LA_COM == 1 ~ "Latino",
      SDC_CULT_SE_COM == 1 ~ "Southeast Asian",
      SDC_CULT_AR_COM == 1 ~ "Arab",
      SDC_CULT_WA_COM == 1 ~ "West Asian",
      SDC_CULT_JA_COM == 1 ~ "Japanese",
      SDC_CULT_KO_COM == 1 ~ "Korean",
      SDC_CULT_OT_COM == 1 ~ "Other",
      TRUE ~ NA_character_  # default
    ),
    
    Race7 = case_when(
      SDC_CULT_WH_COM == 1 ~ "White",
      SDC_CULT_SA_COM == 1 ~ "South Asian",
      SDC_CULT_BL_COM == 1 ~ "Black",
      SDC_CULT_FP_COM == 1 | SDC_CULT_SE_COM == 1 | SDC_CULT_WA_COM == 1 |
        SDC_CULT_JA_COM == 1 | SDC_CULT_KO_COM == 1 | SDC_CULT_ZH_COM == 1 ~ "Asian",
      SDC_CULT_LA_COM == 1 ~ "Latin American",
      SDC_CULT_AR_COM == 1 ~ "Arab",
      SDC_CULT_OT_COM == 1 ~ "Other",
    ))


str(CLSA_methylation$Race7) #Race is a factor with 6 levels (White is reference)
table(CLSA_methylation$Race7)

CLSA_methylation$Race7 <- factor(CLSA_methylation$Race7,
                                 levels = c("White", "South Asian", "Black", "Asian",
                                            "Arab", "Latin American", "Other"))
save(CLSA_methylation, file = "CLSA_methylation.rds") 





