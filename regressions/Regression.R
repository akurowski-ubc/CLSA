

library(dplyr)
library(ggplot2)
library(tidyverse)
library (haven)
library(table1)
library(stargazer)
library(htmltools)
library(broom)

subset_data <- readRDS("subset_data.rds")

#Interaction Income and Sex ----------------------------------

#include income, age, age2, sex, province, race, incomeXsex
#GrimAgeEAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_c + AGE_c2 + WGHTS_PROV_COM + Race7

#GrimAGe 
#note household income has 53 NAs 
model <- lm(
  GrimAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)



model <- lm(GrimAge_EAA ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



#PhenoAge
model <- lm(PhenoAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(PhenoAge_EAA ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)




#DunedinPACE
model <- lm(DunedinPACE ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(DunedinPACE ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



#DNAmIC
model <- lm(DNAmIC ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(DNAmIC ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)





#Interaction income and age  ----------------------------------

model <- lm(
  GrimAge_EAA ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)



model <- lm(PhenoAge_EAA ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(DunedinPACE ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM  + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



model <- lm(DNAmIC ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)












#Interaction btw education and sex ----------------------------------

table(subset_data$ED_HIGH_COM)

model <- lm(
  GrimAge_EAA ~ ED_HIGH_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)



model <- lm(
  PhenoAge_EAA ~ ED_HIGH_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)


model <- lm(
  DunedinPACE ~ ED_HIGH_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)


model <- lm(DNAmIC ~ ED_HIGH_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data
)
summary(model)

#Interaction btw education and age ----------------------------------
model <- lm(GrimAge_EAA ~ ED_HIGH_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(PhenoAge_EAA ~ ED_HIGH_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ ED_HIGH_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(DNAmIC ~ ED_HIGH_COM * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)







# Interaction btw savings and sex ----------------------------------
model <- lm(GrimAge_EAA ~ WEA_SVNGSVL_MCQ * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data
)
summary(model)

model <- lm(PhenoAge_EAA ~ WEA_SVNGSVL_MCQ * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data
)
summary(model)

model <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data
)
summary(model)

model <- lm(DNAmIC ~ WEA_SVNGSVL_MCQ * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data
)
summary(model)


# Interaction btw savings and age ----------------------------------

model <- lm(GrimAge_EAA ~ WEA_SVNGSVL_MCQ * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(PhenoAge_EAA ~ WEA_SVNGSVL_MCQ * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)

model <- lm(DNAmIC ~ WEA_SVNGSVL_MCQ * AGE_NMBR_COM + SEX_ASK_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)





# AA as outcome, environmental exposure as predictor (bivariate analyses one at at time) ----------------------------------

#GrimAge 

model <- lm(GrimAge_EAA ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(GrimAge_EAA ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(GrimAge_EAA ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


table(subset_data$ENV_HMPRB_CON_MCQ) #0=no problems with condensation, 1=problems. 
model <- lm(GrimAge_EAA ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


table(subset_data$ENV_HMPRB_NOI_MCQ)#0=no problems with noise, 1=problems with noise. 
model <- lm(GrimAge_EAA ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


####
#PhenoAge_EAA 
model <- lm(PhenoAge_EAA ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(PhenoAge_EAA ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)

model <- lm(PhenoAge_EAA ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



####
#DunedinPACE 
model <- lm(DunedinPACE ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)

model <- lm(DunedinPACE ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)

model <- lm(DunedinPACE ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


####
#DNAmIC 
model <- lm(DNAmIC ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DNAmIC ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DNAmIC ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DNAmIC ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DNAmIC ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)


model <- lm(DNAmIC ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)

model <- lm(DNAmIC ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)

model <- lm(DNAmIC ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)
















































































#prep data for regression 
table(subset_data$INC_TOT_COM) #household income
subset_data$INC_TOT_COM[subset_data$INC_TOT_COM == 8] <- NA 
subset_data$INC_TOT_COM[subset_data$INC_TOT_COM == 9] <- NA
subset_data$INC_TOT_COM <- 6 - subset_data$INC_TOT_COM #reverse code so 1 represents higher 
table(subset_data$INC_TOT_COM)

table(subset_data$ED_HIGH_COM) #education 
subset_data$ED_HIGH_COM[subset_data$ED_HIGH_COM == 98] <- NA 
subset_data$ED_HIGH_COM[subset_data$ED_HIGH_COM == 99] <- NA
subset_data$ED_HIGH_COM[subset_data$ED_HIGH_COM == 97] <- 7 #make "other" into 7 for easier...
subset_data$ED_HIGH_COM <- 8 - subset_data$ED_HIGH_COM #reverse code so 1 represents higher 


table(subset_data$WEA_SVNGSVL_MCQ) #savings
subset_data$WEA_SVNGSVL_MCQ[subset_data$WEA_SVNGSVL_MCQ == 8] <- NA 
subset_data$WEA_SVNGSVL_MCQ[subset_data$WEA_SVNGSVL_MCQ == 9] <- NA
subset_data$WEA_SVNGSVL_MCQ <- 5 - subset_data$WEA_SVNGSVL_MCQ #reverse code so 1 represents higher 


#Test for interaction between sex and income: GrimAge_EAA∼INC_TOT_COM+SEX_ASK_COM+(INC_TOT_COM×SEX_ASK_COM)+AGE_NMBR_COM
subset_data$SEX_ASK_COM <- factor(subset_data$SEX_ASK_COM) #make Sex a factor variable 
class(subset_data$SEX_ASK_COM)

m_sex_int <- lm(GrimAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM, data = subset_data)
summary(m_sex_int)

m_sex_int <- lm(PhenoAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM, data = subset_data)
summary(m_sex_int)

m_sex_int <- lm(DunedinPACE ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM, data = subset_data)
summary(m_sex_int)

m_sex_int <- lm(DNAmIC ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM, data = subset_data)
summary(m_sex_int)

#Test for interaction between age and income: GrimAge_EAA∼INC_TOT_COM+AGE_NMBR_COM+(INC_TOT_COM×AGE_NMBR_COM)+SEX_ASK_COM
m_age_int <- lm(GrimAge_EAA ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(m_age_int)

m_age_int <- lm(PhenoAge_EAA ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(m_age_int)

m_age_int <- lm(DunedinPACE ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(m_age_int)

m_age_int <- lm(DNAmIC ~ INC_TOT_COM * AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(m_age_int)




# linear regression AA=environmental exposure  -------------------------------------------

#outcome: "GrimAge_EAA", "PhenoAge_EAA", "DunedinPACE", "DNAmIC"
#enviro variables: "pm25_1yr_com", "no2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM", "temp_1yr_com", "LGTNLTYY_01_COM", ENV_HMPRB_NOI_MCQ, ENV_HMPRB_CON_MCQ_rev
#control for: AGE_NMBR_COM, SEX_ASK_COM, WGHTS_PROV_COM

hist(subset_data$pm25_1yr_com) #histogram of PM2.5 

library(ggplot2) #it doesn't look like a linear relationship btw PM2.5 and GrimAge
ggplot(subset_data, aes(x = pm25_1yr_com, y = GrimAge_EAA)) +
  geom_point() +
  geom_smooth(method = "loess")

sum(is.na(subset_data$pm25_1yr_com)) #there are 215 NAs in PM2.5 data


#model PM2.5 and AA
m_pm25 <- lm(GrimAge_EAA ~ pm25_1yr_com + AGE_NMBR_COM + SEX_ASK_COM + WGHTS_PROV_COM, 
             data = subset_data)
summary(m_pm25)



#GrimAGe 
#note household income has 53 NAs 
model <- lm(
  GrimAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
    WGHTS_PROV_COM + Race7,
  data = subset_data
)
summary(model)



model <- lm(GrimAge_EAA ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



#PhenoAge
model <- lm(PhenoAge_EAA ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(PhenoAge_EAA ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)




#DunedinPACE
model <- lm(DunedinPACE ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(DunedinPACE ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



#DNAmIC
model <- lm(DNAmIC ~ INC_TOT_COM * SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)


model <- lm(DNAmIC ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)








# stratified analyses. Income  -------------------------------------------

#by age 
subset_data$age_group <- ifelse(subset_data$AGE_NMBR_COM < 65, "45-64", "65-85")
subset_data_firstgrp  <- subset_data[subset_data$age_group == "45-64", ]
subset_data_secondgrp <- subset_data[subset_data$age_group == "65-85", ]

table(subset_data$INC_TOT_COM) #make sure it is reverse coded 
str(subset_data$INC_TOT_COM) #make sure it is a factored variable 



#GrimAGe 
library(broom)
model <- lm(GrimAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)




#PhenoAge 
library(broom)
model <- lm(PhenoAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


#DunedinPACE 

model <- lm(DunedinPACE ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#DNAmIC 
model <- lm(DNAmIC ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)









# stratified analyses. Savings  -------------------------------------------
#by age 
subset_data$age_group <- ifelse(subset_data$AGE_NMBR_COM < 65, "45-64", "65-85")
subset_data_firstgrp  <- subset_data[subset_data$age_group == "45-64", ]
subset_data_secondgrp <- subset_data[subset_data$age_group == "65-85", ]



#GrimAGe 
library(broom)
model <- lm(GrimAge_EAA ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)




#PhenoAge 
library(broom)
model <- lm(PhenoAge_EAA ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


#DunedinPACE 
library(broom)
model <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#DNAmIC 
library(broom)
model <- lm(DNAmIC ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

































#PhenoAge
model <- lm(PhenoAge_EAA ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
confint(model, level=0.95)



#DunedinPACE
model <- lm(DunedinPACE ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)



#DNAmIC
model <- lm(DNAmIC ~ INC_TOT_COM +  SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)




# Enviro analyses_PM2.5  -------------------------------

 
names(CLSA_methylation)[grep("pm25", names(CLSA_methylation), ignore.case = TRUE)] #show all the PM2.5 variables available



subset_data$pm25_5yrs_com[subset_data$pm25_5yrs_com == -8888] <- NA 
subset_data$no2_5yrs_com[subset_data$no2_5yrs_com == -8888] <- NA 
subset_data$so2_1yr_com[subset_data$so2_1yr_com == -8888] <- NA 


summary(subset_data$pm25_5yrs_com)
summary(subset_data$no2_5yrs_com)
summary(subset_data$so2_1yr_com)

model <- lm(GrimAge_EAA ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(PhenoAge_EAA ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(DunedinPACE ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)



model <- lm(DNAmIC ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + I(AGE_NMBR_COM^2) +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results.csv", row.names = FALSE)




# check for collinearity with air pollution exposures variables  --------------------
install.packages("corrplot")
library(corrplot)

pollutants <- subset_data[, c("pm25_1yr_com", "no2_1yr_com", "o3_1yr_com", "so2_1yr_com")]
cor_matrix <- cor(pollutants, use = "pairwise.complete.obs")# Correlation matrix with numeric values
round(cor_matrix, 2)# Round for readability (optional)




































#check by province for condensation problems 
tab <- table(subset_data$WGHTS_PROV_COM, subset_data$ENV_HMPRB_CON_MCQ)# Create cross-tabulation
df <- as.data.frame.matrix(tab)# Convert to data frame
province_map <- c(# Add province names
  "1" = "AB",
  "2" = "BC",
  "3" = "MB",
  "4" = "NB",
  "5" = "NL",
  "6" = "NS",
  "7" = "ON",
  "8" = "PEI",
  "9" = "QB",
  "10" = "SK"
)
df$Province <- province_map[rownames(df)]
colnames(df) <- c("No_Condensation (0)", "Condensation (1)", "Province")# Rename columns
df <- df[, c("Province", "No_Condensation (0)", "Condensation (1)")]# Reorder columns and add totals
df$Total <- rowSums(df[, 2:3])
df
write.csv(df, "condensation_by_province.csv", row.names = FALSE)




# Socioeconomic conditions as exposures, environmental as outcomes --------

# exposures 
# "INC_TOT_COM", "WEA_SVNGSVL_MCQ"

# outcomes 
# "pm25_1yr_com", "no2_1yr_com", "so2_1yr_com", "o3_1yr_com", "GRLANYY_26_COM", "temp_1yr_com", 
# "LGTNLTYY_01_COM",   "ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ"

# controls 
# "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",


model <- lm(pm25_1yr_com ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #PM2.5
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results1.csv", row.names = FALSE)

model <- lm(pm25_1yr_com ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results2.csv", row.names = FALSE)



model <- lm(no2_1yr_com ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #NO2
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results3.csv", row.names = FALSE)

model <- lm(no2_1yr_com ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results4.csv", row.names = FALSE)


summary(subset_data$GRLANYY_26_COM)

model <- lm(so2_1yr_com ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #SO2
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results5.csv", row.names = FALSE)

model <- lm(so2_1yr_com ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results6.csv", row.names = FALSE)



model <- lm(o3_1yr_com ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #o3_1yr_com
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results7.csv", row.names = FALSE)

model <- lm(o3_1yr_com ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results8.csv", row.names = FALSE)



model <- lm(GRLANYY_26_COM ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #GRLANYY_26_COM
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results9.csv", row.names = FALSE)

model <- lm(GRLANYY_26_COM ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results10.csv", row.names = FALSE)



model <- lm(temp_1yr_com ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #temp_1yr_com
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results11.csv", row.names = FALSE)

model <- lm(temp_1yr_com ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results12.csv", row.names = FALSE)




model <- lm(LGTNLTYY_01_COM ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #LGTNLTYY_01_COM
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results13.csv", row.names = FALSE)

model <- lm(LGTNLTYY_01_COM ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM +
              WGHTS_PROV_COM + Race7,
            data = subset_data)
summary(model)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)
write.csv(results, "Results14.csv", row.names = FALSE)


#logistic regression 



model <- lm(GRLANYY_26_COM ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #GRLANYY_26_COM
              WGHTS_PROV_COM + Race7,
            data = subset_data)


#"ENV_HMPRB_CON_MCQ", "ENV_HMPRB_NOI_MCQ"

sum(is.na(subset_data$ENV_HMPRB_CON_MCQ))
levels(subset_data$ENV_HMPRB_CON_MCQ)
table(subset_data$ENV_HMPRB_CON_MCQ, useNA = "ifany") #76 NAs 

m1 <- glm(ENV_HMPRB_CON_MCQ ~ AGE_NMBR_COM, #do a univariable regression first with just age 
          data = subset_data,
          family = "binomial"
)
summary(m1)

exp(coef(m1)["AGE_NMBR_COM"])

levels(subset_data$SEX_ASK_COM)




m2 <- glm(ENV_HMPRB_CON_MCQ ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #Income (predictor) and condensation (outcome)
            WGHTS_PROV_COM + Race7,
          data = subset_data,
          family = "binomial"
)
summary(m2)

sum_m2 <- summary(m2)
coef_table <- as.data.frame(sum_m2$coefficients)

coef_table$OddsRatio <- exp(coef_table$Estimate) #add OR

CI <- exp(confint(m2)) #add CI
coef_table$CI_Lower <- CI[,1]
coef_table$CI_Upper <- CI[,2]

write.csv(coef_table, "logistic_summary_table_with_OR.csv", row.names = TRUE)








m2 <- glm(ENV_HMPRB_CON_MCQ ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + #Savings (predictor) and condensation (outcome)
            WGHTS_PROV_COM + Race7,
          data = subset_data,
          family = "binomial"
)
summary(m2)

sum_m2 <- summary(m2)
coef_table <- as.data.frame(sum_m2$coefficients)

coef_table$OddsRatio <- exp(coef_table$Estimate) #add OR

CI <- exp(confint(m2)) #add CI
coef_table$CI_Lower <- CI[,1]
coef_table$CI_Upper <- CI[,2]

write.csv(coef_table, "logistic_summary_table_with_OR.csv", row.names = TRUE)









m2 <- glm(ENV_HMPRB_NOI_MCQ ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + #Income (predictor) and noise (outcome)
            WGHTS_PROV_COM + Race7,
          data = subset_data,
          family = "binomial"
)
summary(m2)

sum_m2 <- summary(m2)
coef_table <- as.data.frame(sum_m2$coefficients)

coef_table$OddsRatio <- exp(coef_table$Estimate) #add OR

CI <- exp(confint(m2)) #add CI
coef_table$CI_Lower <- CI[,1]
coef_table$CI_Upper <- CI[,2]

write.csv(coef_table, "logistic_summary_table_with_OR.csv", row.names = TRUE)







m2 <- glm(ENV_HMPRB_NOI_MCQ ~ WEA_SVNGSVL_MCQ + SEX_ASK_COM + AGE_NMBR_COM + #Income (predictor) and noise (outcome)
            WGHTS_PROV_COM + Race7,
          data = subset_data,
          family = "binomial"
)
summary(m2)

sum_m2 <- summary(m2)
coef_table <- as.data.frame(sum_m2$coefficients)

coef_table$OddsRatio <- exp(coef_table$Estimate) #add OR

CI <- exp(confint(m2)) #add CI
coef_table$CI_Lower <- CI[,1]
coef_table$CI_Upper <- CI[,2]

write.csv(coef_table, "logistic_summary_table_with_OR.csv", row.names = TRUE)




exp(coef(m2)["INC_TOT_COM3"]) #1.69
































#linear regression outcome = AA, exposure = SEC 
#GrimAGe 

m1 <- lm(GrimAge_EAA ~ INC_PTOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m2 <- lm(GrimAge_EAA ~ INC_TOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m3 <- lm(GrimAge_EAA ~ ED_HIGH_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m4 <- lm(GrimAge_EAA ~ WEA_SVNGSVL_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m5 <- lm(GrimAge_EAA ~ WEA_DEBT_CCRD_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)

stargazer(m1, m2, m3, m4, m5,
          type = "text",   
          title = "Linear Regression Models",
          dep.var.labels = "GrimAge_EAA",
          covariate.labels = c("Personal Income", 
                               "Household Income", 
                               "Highest Education", 
                               "Total savings", 
                               "Credit Card Debt (yes or no)",
                               "Age", "Sex"),
          ci = TRUE, ci.level = 0.95, single.row = TRUE)
summary(m2)$coefficients


#phenoAGe 
m1 <- lm(PhenoAge_EAA ~ INC_PTOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m2 <- lm(PhenoAge_EAA ~ INC_TOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m3 <- lm(PhenoAge_EAA ~ ED_HIGH_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m4 <- lm(PhenoAge_EAA ~ WEA_SVNGSVL_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m5 <- lm(PhenoAge_EAA ~ WEA_DEBT_CCRD_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)

stargazer(m1, m2, m3, m4, m5,
          type = "text",   
          title = "Linear Regression Models",
          dep.var.labels = "PhenoAge EAA",
          covariate.labels = c("Personal Income", 
                               "Household Income", 
                               "Highest Education", 
                               "Total savings", 
                               "Credit Card Debt (yes or no)",
                               "Age", "Sex"),
          ci = TRUE, ci.level = 0.95, single.row = TRUE)
summary(m2)$coefficients


#DunedinPACE

m1 <- lm(DunedinPACE ~ INC_PTOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m2 <- lm(DunedinPACE ~ INC_TOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m3 <- lm(DunedinPACE ~ ED_HIGH_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m4 <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m5 <- lm(DunedinPACE ~ WEA_DEBT_CCRD_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)

stargazer(m1, m2, m3, m4, m5,
          type = "text",   
          title = "Linear Regression Models",
          dep.var.labels = "DunedinPACE",
          covariate.labels = c("Personal Income", 
                               "Household Income", 
                               "Highest Education", 
                               "Total savings", 
                               "Credit Card Debt (yes or no)",
                               "Age", "Sex"),
          ci = TRUE, ci.level = 0.95, single.row = TRUE)
summary(m2)$coefficients


#Intrinsic capacity 

m1 <- lm(DNAmIC ~ INC_PTOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m2 <- lm(DNAmIC ~ INC_TOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m3 <- lm(DNAmIC ~ ED_HIGH_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m4 <- lm(DNAmIC ~ WEA_SVNGSVL_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
m5 <- lm(DNAmIC ~ WEA_DEBT_CCRD_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)

stargazer(m1, m2, m3, m4, m5,
          type = "text",   
          title = "Linear Regression Models",
          dep.var.labels = "DNAmIC",
          covariate.labels = c("Personal Income", 
                               "Household Income", 
                               "Highest Education", 
                               "Total savings", 
                               "Credit Card Debt (yes or no)",
                               "Age", "Sex"),
          ci = TRUE, ci.level = 0.95, single.row = TRUE)
summary(m2)$coefficients



#DNAmIC
model <- lm(DunedinPACE ~ INC_PTOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ INC_TOT_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ ED_HIGH_COM + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ WEA_SVNGSVL_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(model)

model <- lm(DunedinPACE ~ WEA_DEBT_CCRD_MCQ + AGE_NMBR_COM + SEX_ASK_COM, data = subset_data)
summary(model)

# Create regression table

stargazer(model1,
          type = "text",   # use "latex" or "html" if you need it for a paper
          title = "Linear Regression Model",
          dep.var.labels = "GrimAge EAA",
          covariate.labels = c("Personal income", 
                               "Age", 
                               "Sex"),
          ci = TRUE,        # show confidence intervals
          single.row = TRUE # put coeff + CI on same row
)