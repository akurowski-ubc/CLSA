

library(dplyr)
library(ggplot2)
library(tidyverse)
library (haven)
library(table1)
library(stargazer)
library(htmltools)
library(broom)

load("subset_data.rds")#load data

#note cell types
# "CD4nv", "CD8nv",   "Mono", "Treg", "CD4mem", "CD8mem", "NK", "Bmem", "Bnv"


# #enviro variables 
# pm25_1yr_com       no2_1yr_com        o3_1yr_com    GRLANYY_26_COM      temp_1yr_com  
# #LGTNLTYY_01_COM ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ  






# Visualizations for descriptive stats (boxplots, pie charts)  ------------

cond_tab <- table(subset_data$ENV_HMPRB_CON_MCQ, useNA = "ifany")
cond_labels <- paste0(names(cond_tab), " (", round(100*cond_tab/sum(cond_tab), 1), "%)")

#pie charts for noise and condensation
pie(cond_tab,
    labels = cond_labels,
    main = "Home Problems: Condensation",
    col = c("lightblue", "orange", "grey80"))

noise_tab <- table(subset_data$ENV_HMPRB_NOI_MCQ, useNA = "ifany")
noise_labels <- paste0(names(noise_tab), " (", round(100*noise_tab/sum(noise_tab), 1), "%)")

pie(noise_tab,
    labels = noise_labels,
    main = "Home Problems: Noise",
    col = c("lightblue", "orange", "grey80"))





install.packages("patchwork")
# Packages
library(ggplot2)
library(patchwork)

# (Optional) slide-friendly base theme
theme_set(theme_minimal(base_size = 14))

# A tiny helper to keep plots consistent
bp <- function(data, var, title, ylab) {
  ggplot(data, aes(y = {{ var }}, x = "")) +
    geom_boxplot(fill = "skyblue", color = "darkblue", width = 0.4, outlier.alpha = 0.5) +
    labs(title = title, y = ylab, x = NULL) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 6)),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# Build each panel
p_pm25 <- bp(subset_data, pm25_1yr_com, "PM2.5 (1-year average)", "PM2.5 (µg/m³)")
p_no2  <- bp(subset_data, no2_1yr_com,  "NO2 (1-year average)",   "NO2 (ppb)")
p_so2  <- bp(subset_data, so2_1yr_com,  "SO2 (1-year average)",   "SO2 (ppb)")
p_o3   <- bp(subset_data, o3_1yr_com,   "Ozone (1-year average)", "Ozone (ppb)")
p_ndvi <- bp(subset_data, GRLANYY_26_COM, "Greenness within 500 m", "Greenness (NDVI)")
p_tmp  <- bp(subset_data, temp_1yr_com, "Temperature (1-year average)", "Temperature (°C)")
p_ntl  <- bp(subset_data, LGTNLTYY_01_COM, "Nighttime light brightness (1-year average)", "Brightness at postal code")

# Arrange in a 3x3 grid (last row uses spacers to balance)
dashboard <- (p_pm25 | p_no2 | p_so2) /
  (p_o3   | p_ndvi | p_tmp) /
  (p_ntl  | plot_spacer() | plot_spacer())

# Save to PDF (wide slide-friendly aspect)
ggsave("environmental_exposures_boxplots_dashboard.pdf",
       dashboard, width = 12, height = 8, units = "in")





# visualizations_further boxplots  ----------------------------------------


library(ggplot2)
library(dplyr)

income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(pm25_1yr_com)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = pm25_1yr_com, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "PM2.5 (1-year average) by Income Group",
    x = "Household Income",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )




income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(no2_1yr_com)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = no2_1yr_com, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "NO2 (1-year average) by Income Group",
    x = "Household Income",
    y = "NO2 (ppb)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )


# #enviro variables 
#     GRLANYY_26_COM      temp_1yr_com  
# #LGTNLTYY_01_COM ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ  



income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(so2_1yr_com)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = so2_1yr_com, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "SO2 (1-year average) by Income Group",
    x = "Household Income",
    y = "SO2 (ppb)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )




income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(o3_1yr_com)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = o3_1yr_com, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Ozone (1-year average) by Income Group",
    x = "Household Income",
    y = "Ozone (ppb)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )





income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(GRLANYY_26_COM)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = GRLANYY_26_COM, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Greenness (Annual Max NDVI within 500m) by Income Group",
    x = "Household Income",
    y = "Greenness (NDVI)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )


# ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ  


income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(LGTNLTYY_01_COM)) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels))

ggplot(df, aes(x = income_cat, y = LGTNLTYY_01_COM, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Nighttime light brightness by Income Group",
    x = "Household Income",
    y = "Nighttime light brightness at postal code"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )




#other visualization of night brightness 

ggplot(df, aes(income_cat, LGTNLTYY_01_COM)) +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3, 
               color = "darkblue") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.15, 
               color = "darkblue") +
  labs(
    title = "Nighttime Light Brightness by Income Group",
    x = "Household Income",
    y = "Nighttime Light Brightness at Postal Code"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 14
    ),
    axis.title = element_text(face = "bold", size = 12),   # bold axis titles
    axis.text  = element_text(face = "bold", size = 12)    # bold tick labels
  )



# ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ  


table(subset_data$ENV_HMPRB_CON_MCQ)

table(subset_data$ENV_HMPRB_NOI_MCQ)




























































# Regressions for AA with income ------------
#stratified by age 
subset_data$age_group <- ifelse(subset_data$AGE_NMBR_COM < 65, "45-64", "65-85")
subset_data_firstgrp  <- subset_data[subset_data$age_group == "45-64", ]
subset_data_secondgrp <- subset_data[subset_data$age_group == "65-85", ]

table(subset_data$INC_TOT_COM) #make sure it is reverse coded 
str(subset_data$INC_TOT_COM) #make sure it is a factored variable 


#GrimAGe 
library(broom)
model <- lm(GrimAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(GrimAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)




#PhenoAge 
library(broom)
model <- lm(PhenoAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


#DunedinPACE 

model <- lm(DunedinPACE ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#DNAmIC 
model <- lm(DNAmIC ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ INC_TOT_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_secondgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)





# Regressions for AA with Environmental exposure ------------

#aging measures GrimAge_EAA PhenoAge_EAA DunedinPACE DNAmIC

# #enviro variables 
# pm25_1yr_com       no2_1yr_com        o3_1yr_com   so2_1yr_com   GRLANYY_26_COM      temp_1yr_com  
# #LGTNLTYY_01_COM ENV_HMPRB_CON_MCQ ENV_HMPRB_NOI_MCQ  


#GrimAGe 
library(broom)
#pm2.5
model <- lm(GrimAge_EAA ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "result1.csv", row.names = FALSE)

model <- lm(PhenoAge_EAA ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "result2.csv", row.names = FALSE)

model <- lm(DunedinPACE ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data_firstgrp)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "result1.csv", row.names = FALSE)

model <- lm(DNAmIC ~ pm25_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "result1.csv", row.names = FALSE)


#no2_1yr_com
model <- lm(GrimAge_EAA ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(PhenoAge_EAA ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(DunedinPACE ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(DNAmIC ~ no2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


#so2_1yr_com
model <- lm(GrimAge_EAA ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ so2_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


#ozone
model <- lm(GrimAge_EAA ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ o3_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)





#Greenness
model <- lm(GrimAge_EAA ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(PhenoAge_EAA ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(DunedinPACE ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)

model <- lm(DNAmIC ~ GRLANYY_26_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#temperature
model <- lm(GrimAge_EAA ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(PhenoAge_EAA ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DunedinPACE ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)


model <- lm(DNAmIC ~ temp_1yr_com + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#Light brightness
model <- lm(GrimAge_EAA ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(PhenoAge_EAA ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DunedinPACE ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DNAmIC ~ LGTNLTYY_01_COM + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)





#Condensation
model <- lm(GrimAge_EAA ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(PhenoAge_EAA ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DunedinPACE ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DNAmIC ~ ENV_HMPRB_CON_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



#Noise
model <- lm(GrimAge_EAA ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(PhenoAge_EAA ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DunedinPACE ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
results <- tidy(model, conf.int = TRUE, conf.level = 0.95)# Tidy table with estimates, SE, t, p, and 95% CI
write.csv(results, "GrimAge_model_results_firstgrp.csv", row.names = FALSE)



model <- lm(DNAmIC ~ ENV_HMPRB_NOI_MCQ + SEX_ASK_COM + AGE_NMBR_COM + 
              I(AGE_NMBR_COM^2) + WGHTS_PROV_COM + Race7
            + CD4nv + CD8nv + Mono + Treg + CD4mem + CD8mem + NK + Bmem + Bnv,
            data = subset_data)
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






# Visualizations for presentation  ----------------------------------------

# "AGE_NMBR_COM", "SEX_ASK_COM", "WGHTS_PROV_COM", "Race7",
# 
# # Socioeconomic
# "INC_TOT_COM", "ED_HIGH_COM", "WEA_SVNGSVL_MCQ",


pie(table(subset_data$SEX_ASK_COM),
    labels = paste(names(table(subset_data$SEX_ASK_COM)), 
                   " (", table(subset_data$SEX_ASK_COM), ")", sep=""),
    main = "Distribution of SEX_ASK_COM",
    col = c("steelblue", "salmon"))




# Recode M/F to Male/Female
subset_data$SEX_ASK_COM <- factor(subset_data$SEX_ASK_COM,
                                  levels = c("M", "F"),
                                  labels = c("Male", "Female"))


#pie chart for sex 
library(ggplot2)
library(dplyr)

subset_data$SEX_ASK_COM <- factor(subset_data$SEX_ASK_COM,
                                  levels = c("M", "F"),
                                  labels = c("Male", "Female"))

df <- as.data.frame(table(subset_data$SEX_ASK_COM))
colnames(df) <- c("Sex", "Count")
df <- df %>%
  mutate(Percent = round(100 * Count / sum(Count), 1),
         Label = paste0(Sex, "\n", Count, " (", Percent, "%)"))

ggplot(df, aes(x = "", y = Count, fill = Sex)) +
  geom_col(width = 1, color = "black") +
  coord_polar(theta = "y") +

  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("steelblue", "salmon")) +
  theme_void() +
  ggtitle("Distribution of SEX_ASK_COM") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


#bar chart for income 

# Packages
library(dplyr)
library(ggplot2)
library(forcats)

# --- Recode to clean labels (works if INC_TOT_COM is numeric codes or strings) ---
income_labs <- c("<20k","20-50k","50-100k","100-150k","150k+","Missing","Don't know","Refused")

income_recode <- function(x){
  # If it's already text, normalize a few variants and return
  if (is.character(x) || is.factor(x)) {
    x <- trimws(as.character(x))
    x <- dplyr::recode(x,
                       "<20k" = "<20k",
                       "<$20k" = "<20k",
                       "20-50k" = "20-50k",
                       "50-100k" = "50-100k",
                       "100-150k" = "100-150k",
                       "150k+" = "150k+",
                       "Missing" = "Missing",
                       "Don't know" = "Don't know",
                       "Dont know" = "Don't know",
                       "Refused" = "Refused",
                       .default = x
    )
  } else {
    # CLSA-style numeric categories (adjust if yours differ)
    x <- dplyr::recode(as.integer(x),
                       `1` = "<20k",
                       `2` = "20-50k",
                       `3` = "50-100k",
                       `4` = "100-150k",
                       `5` = "150k+",
                       `7` = "Don't know",
                       `8` = "Refused",
                       `9` = "Missing",
                       .default = NA_character_
    )
  }
  factor(x, levels = income_labs)
}

subset_data <- subset_data %>%
  mutate(INC_TOT_COM_clean = income_recode(INC_TOT_COM))

# --- Counts table ---
df_income <- subset_data %>%
  filter(!is.na(INC_TOT_COM_clean)) %>%
  count(INC_TOT_COM_clean, name = "n") %>%
  # Show from top to bottom like the example image
  mutate(INC_TOT_COM_clean = fct_rev(INC_TOT_COM_clean))

# --- Plot (horizontal bars, shades of blue, no legend) ---
ggplot(df_income, aes(x = INC_TOT_COM_clean, y = n, fill = INC_TOT_COM_clean)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues", direction = 1, guide = "none") +
  labs(
    title = "Household Income (Past 12 Months)",
    x = NULL,
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- Optional: add counts at the end of each bar ---
# + geom_text(aes(label = n), hjust = -0.1, size = 3.8) +
#   expand_limits(y = max(df_income$n) * 1.08)


#race visualization: 





library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# Counts + percents (keep your factor order, show from top to bottom)
df_race <- subset_data %>%
  filter(!is.na(Race7)) %>%
  count(Race7, name = "n") %>%
  mutate(
    perc = n / sum(n),
    Race7 = fct_rev(Race7)  # reverse so first level appears at top
  )

# Plot (horizontal bars, shades of blue, no legend)
p <- ggplot(df_race, aes(x = Race7, y = n, fill = Race7)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues", direction = 1, guide = "none") +
  labs(
    title = "Self-Reported Race",
    x = NULL,
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- Option A: just the bars ----
p

# ---- Option B (uncomment): add labels n (xx.x%) at bar ends ----
# p +
#   geom_text(aes(label = paste0(n, " (", percent(perc, accuracy = 0.1), ")")),
#             hjust = -0.1, size = 3.8) +
#   expand_limits(y = max(df_race$n) * 1.08)





library(dplyr)
library(ggplot2)
library(forcats)

# Counts (keep your existing factor order; reverse so first level shows at top)
df_race <- subset_data %>%
  filter(!is.na(Race7)) %>%
  count(Race7, name = "n") %>%
  mutate(Race7 = fct_rev(Race7))

ggplot(df_race, aes(x = Race7, y = n, fill = Race7)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues", direction = 1, guide = "none") +
  labs(
    title = "Self-Reported Cultural Background",
    x = NULL,
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Optional: add counts at bar ends, like the other charts
# + geom_text(aes(label = n), hjust = -0.1, size = 3.8) +
#   expand_limits(y = max(df_race$n) * 1.08)

