

library(dplyr)
library(ggplot2)
library(tidyverse)
library (haven)
library(table1)
library(stargazer)
library(htmltools)


load("data/CLSA_methylation.rds")


subset_data <- CLSA_methylation[, c(
  "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
  
  # Socioeconomic
  "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",
  
  # Outcomes
  "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC",
  
  # Environmental exposures
  "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM", "temp_1yr_com",
  "LGTNLTYY_01_COM",   "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ", "pm25_5yrs_com",   "PM25DALYY_02_COM", "no2_5yrs_com", "so2_1yr_com",
  
  "Locrds_length_200m_com",   "Phwy_length_200m_com", 
  
  "grtcc10_01_COM",   "grtcc10_02_COM",   "grtcc10_03_COM",   "grtcc10_04_COM",  "grtcc10_05_COM",   
  "GRUMPYY_01_COM",  "GRUMPYY_02_COM", "GRUMPYY_03_COM",   "GRUMPYY_04_COM",   "GRUMPYY_05_COM",   "GRUMPYY_06_COM",   "GRUMPYY_07_COM",
  "GRUMPYY_08_COM",   "GRUMPYY_09_COM",
  
  
  # Lifestyle / behavioral
  "PA2_DSCR2_MCQ", "ALC_FREQ_COM", "ICQ_SMOKE_COM", #PASE scale: PASE Score - Final (in sharepoint dictionary)
  
  #cell types
  "CD4nv", "CD8nv",   "Mono", "Treg", "CD4mem", "CD8mem", "NK", "Bmem", "Bnv"
  
)]




# explore variables  ------------------------------------------------------

summary(subset_data$Phwy_length_200m_com) #Length of hwy within 200m of postal code centre
quantile(subset_data$Phwy_length_200m_com,
         probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99),
         na.rm = TRUE)
mean(subset_data$Phwy_length_200m_com == 0, na.rm = TRUE) #96% of the participants (1423 participants) have no hwy nearby. 



summary(subset_data$Locrds_length_200m_com) #Length of local rds within 200m of postal code centre
quantile(subset_data$Locrds_length_200m_com,
         probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99),
         na.rm = TRUE) 

hist(subset_data$Locrds_length_200m_com,
     breaks = 50,
     main = "Length of local rds within 200 m",
     xlab = "Meters",
     col = "grey")




#clean data 
subset_data$pm25_1yr_com[subset_data$pm25_1yr_com == -8888] <- NA  #recode =8888 to missing for these variables 
subset_data$pm25_5yrs_com[subset_data$pm25_5yrs_com == -8888] <- NA  #recode =8888 to missing for these variables 
subset_data$no2_1yr_com[subset_data$no2_1yr_com == -8888] <- NA #NO2
subset_data$so2_1yr_com[subset_data$so2_1yr_com == -8888] <- NA #NO2
subset_data$o3_1yr_com[subset_data$o3_1yr_com == -8888] <- NA #ozone
subset_data$GRLANYY_26_COM[subset_data$GRLANYY_26_COM == -8888] <- NA #greeness
subset_data$LGTNLTYY_01_COM[subset_data$LGTNLTYY_01_COM == -8888] <- NA #light 
subset_data$temp_1yr_com[subset_data$temp_1yr_com == -8888] <- NA#temperature
subset_data$Locrds_length_200m_com[subset_data$Locrds_length_200m_com == -8888] <- NA#length of roads within 200m
subset_data$Phwy_length_200m_com[subset_data$Phwy_length_200m_com == -8888] <- NA#length of hwy within 200m


subset_data$grtcc10_01_COM[subset_data$grtcc10_01_COM %in% c(-8888, -9999)] <- NA
subset_data$grtcc10_02_COM[subset_data$grtcc10_02_COM %in% c(-8888, -9999)] <- NA
subset_data$grtcc10_03_COM[subset_data$grtcc10_03_COM %in% c(-8888, -9999)] <- NA
subset_data$grtcc10_04_COM[subset_data$grtcc10_04_COM %in% c(-8888, -9999)] <- NA
subset_data$grtcc10_05_COM[subset_data$grtcc10_05_COM %in% c(-8888, -9999)] <- NA

subset_data$GRUMPYY_01_COM[subset_data$GRUMPYY_01_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_02_COM[subset_data$GRUMPYY_02_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_03_COM[subset_data$GRUMPYY_03_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_04_COM[subset_data$GRUMPYY_04_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_05_COM[subset_data$GRUMPYY_05_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_06_COM[subset_data$GRUMPYY_06_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_07_COM[subset_data$GRUMPYY_07_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_08_COM[subset_data$GRUMPYY_08_COM %in% c(-8888, -9999)] <- NA
subset_data$GRUMPYY_09_COM[subset_data$GRUMPYY_09_COM %in% c(-8888, -9999)] <- NA



#condensation and noise (binary variables)
str(subset_data$ENV_HMPRB_CON_MCQ) #integer where 1=yes condensation, 0= No. 
table(subset_data$ENV_HMPRB_CON_MCQ)

str(subset_data$ENV_HMPRB_NOI_MCQ) #integer where 1= yes noise, 0= No. 
table(subset_data$ENV_HMPRB_NOI_MCQ)



#environmental variables 

table(subset_data$ENV_HMPRB_CON_MCQ)
subset_data$ENV_HMPRB_CON_MCQ_factored <- factor(subset_data$ENV_HMPRB_CON_MCQ,#condensation
                                                 levels = c(0, 1),
                                                 labels = c("No", "Yes"))

table(subset_data$ENV_HMPRB_NOI_MCQ)
subset_data$ENV_HMPRB_NOI_MCQ_factored <- factor(subset_data$ENV_HMPRB_NOI_MCQ,#noise
                                                 levels = c(0, 1),
                                                 labels = c("No", "Yes"))



label(subset_data$ENV_HMPRB_CON_MCQ_factored)     <- "Home Problems: Condensation"
label(subset_data$ENV_HMPRB_NOI_MCQ_factored)     <- "Home Problems: Noise"
label(subset_data$pm25_1yr_com)          <- "PM2.5 (1-year average)"
label(subset_data$no2_1yr_com)          <- "NO2 (1-year average)"
label(subset_data$so2_1yr_com)          <- "SO2 (1-year average)"
label(subset_data$o3_1yr_com)           <- "Ozone (1-year average)"
label(subset_data$GRLANYY_26_COM)       <- "Greenness (Maximum of annual mean NDVI within 250m)"
label(subset_data$temp_1yr_com)         <- "Temperature (1-year average)"
label(subset_data$LGTNLTYY_01_COM)      <- "Nighttime Light Brightness"
label(subset_data$Locrds_length_200m_com)  <- "Length of local rds within 200m of postal code centre"
label(subset_data$Phwy_length_200m_com) <- "Length of hwy within 200m of postal code centre" 

table1(~ ENV_HMPRB_CON_MCQ_factored + ENV_HMPRB_NOI_MCQ_factored + pm25_1yr_com + no2_1yr_com + so2_1yr_com 
       + o3_1yr_com +  GRLANYY_26_COM + temp_1yr_com + LGTNLTYY_01_COM + 
         Locrds_length_200m_com + Phwy_length_200m_com, data = subset_data)


label(subset_data$grtcc10_01_COM)  <- "Average of estimated percent of pixel covered by vegetation greater than 5m in height (2010)"
label(subset_data$grtcc10_02_COM)  <- "Average of estimated percent of pixel covered by vegetation greater than 5m in height within 100m (2010)"
label(subset_data$grtcc10_03_COM)  <- "Average of estimated percent of pixel covered by vegetation greater than 5m in height within 250m (2010)"
label(subset_data$grtcc10_04_COM)  <- "Average of estimated percent of pixel covered by vegetation greater than 5m in height within 500m (2010)"
label(subset_data$grtcc10_05_COM)  <- "Average of estimated percent of pixel covered by vegetation greater than 5m in height within 1000m (2010)"

label(subset_data$GRUMPYY_01_COM)  <- "Annual percentage of vegetative greenness at postal code"
label(subset_data$GRUMPYY_02_COM)  <- "Mean of annual percentage of vegetative greenness within 100m"
label(subset_data$GRUMPYY_03_COM)  <- "Mean of annual percentage of vegetative greenness within 250m"
label(subset_data$GRUMPYY_04_COM)  <- "Mean of annual percentage of vegetative greenness within 500m"
label(subset_data$GRUMPYY_05_COM)  <- "Mean of annual percentage of vegetative greenness within 1000m"
label(subset_data$GRUMPYY_06_COM)  <- "Max of annual percentage of vegetative greenness within 100m"
label(subset_data$GRUMPYY_07_COM)  <- "Max of annual percentage of vegetative greenness within 100m"
label(subset_data$GRUMPYY_08_COM)  <- "Max of annual percentage of vegetative greenness within 100m"
label(subset_data$GRUMPYY_09_COM)  <- "Max of annual percentage of vegetative greenness within 100m"



t1 <- table1(~ grtcc10_01_COM + grtcc10_02_COM + grtcc10_03_COM + grtcc10_04_COM + grtcc10_05_COM 
       + GRUMPYY_01_COM +  GRUMPYY_02_COM + GRUMPYY_03_COM + GRUMPYY_04_COM + 
         GRUMPYY_05_COM + GRUMPYY_06_COM + GRUMPYY_07_COM + GRUMPYY_08_COM + GRUMPYY_09_COM, data = subset_data)


#to save the table, just go to viewer - export - save as image - to the correct location. then rename after.  
