


# Load and clean data  ----------------------------------------------------

load("data/CLSA_methylation.rds")

library(dplyr)
library(ggplot2)
library(tidyverse)
library (haven)
library(table1)
library(stargazer)
library(htmltools)


#create a subset 
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



#NDVI summary stats 
subset_data$GRLANYY_26_COM[subset_data$GRLANYY_26_COM == -8888] <- NA #greeness
table(subset_data$GRLANYY_26_COM)
summary(subset_data$GRLANYY_26_COM)
IQR(subset_data$GRLANYY_26_COM, na.rm = TRUE)



#condensation and noise (binary variables)
str(subset_data$ENV_HMPRB_CON_MCQ) #integer where 1=yes condensation, 0= No. 
table(subset_data$ENV_HMPRB_CON_MCQ)
subset_data$ENV_HMPRB_CON_MCQ <- factor(subset_data$ENV_HMPRB_CON_MCQ)#make into factor

str(subset_data$ENV_HMPRB_NOI_MCQ) #integer where 1= yes noise, 0= No. 
table(subset_data$ENV_HMPRB_NOI_MCQ)
subset_data$ENV_HMPRB_NOI_MCQ <- factor(subset_data$ENV_HMPRB_NOI_MCQ)#make into factor





#sex and age 
barplot(table(subset_data$SEX_ASK_COM),
        main = "Sex Distribution",
        col = "lightblue",
        ylab = "Count",
        las = 2,
        cex.names = 0.8)





# age and sex  ------------------------------------------------------

#pie chart for sex
df_sex <- subset_data %>%
  filter(!is.na(SEX_ASK_COM)) %>%
  count(SEX_ASK_COM) %>%
  mutate(
    percent = n / sum(n),
    label = paste0(SEX_ASK_COM, " (", round(percent * 100, 1), "%)")
  )

ggplot(df_sex, aes(x = 1, y = n, fill = SEX_ASK_COM)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 10,              # big, slide-friendly
    fontface = "plain"
  ) +
  scale_fill_manual(values = c("F" = "#1f77b4", "M" = "#ff7f0e")) +
  labs(title = "Sex Distribution", x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  theme(
    # remove any remaining axis/radial artifacts
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    legend.position = "right",
    legend.text = element_text(size = 26),
    legend.key.size = grid::unit(1.2, "lines")
  )





#age boxplot 
subset_data %>%
  filter(!is.na(AGE_NMBR_COM)) %>%
  ggplot(aes(x = "", y = AGE_NMBR_COM)) +
  geom_boxplot(
    width = 0.25,
    fill = "white",
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  scale_y_continuous(
    breaks = seq(
      floor(min(subset_data$AGE_NMBR_COM, na.rm = TRUE) / 5) * 5,
      ceiling(max(subset_data$AGE_NMBR_COM, na.rm = TRUE) / 5) * 5,
      by = 5
    )
  ) +
  labs(
    title = "Age Distribution",
    x = NULL,
    y = "Age (years)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust  = 0.5,
      face   = "bold",
      size   = 38,
      margin = margin(b = 18)
    ),
    axis.title.y = element_text(
      face   = "bold",
      size   = 30,
      margin = margin(r = 16)
    ),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 26),
    axis.text.x  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dotted"),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )



# ethnicity, income, wealth ---------------------------------------------------
#cultural background
#horizontal bar plots

subset_data %>%
  filter(!is.na(Race7)) %>%
  count(Race7, name = "n") %>%
  arrange(n) %>%
  ggplot(aes(x = n, y = Race7)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  labs(
    title = "Self-Reported Cultural Background",
    x = "Number of participants",
    y = NULL
  ) +
  theme_classic(base_size = 26) +   # ⬅️ bigger global base
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,          # ⬅️ increased
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIGGER
    axis.title.x = element_text(
      face = "bold",
      size = 30,          # ⬅️ increased
      margin = margin(t = 16)
    ),
    axis.title.y = element_blank(),
    
    # AXIS TICK LABELS — BIGGER
    axis.text.x = element_text(size = 26),   # ⬅️ increased
    axis.text.y = element_text(size = 26)
  )




#horizontal bar chart of household income categories 
library(tidyverse)

subset_data %>%
  mutate(
    income_cat = factor(
      INC_TOT_COM,  # personal/household income
      levels = c(1, 2, 3, 4, 5, 7, 8, 9),
      labels = c(
        "<20K",
        "20K–50K",
        "50K–100K",
        "100K–150K",
        "150K+",
        "Missing",
        "Don't know/No answer",
        "Refused"
      )
    )
  ) %>%
  count(income_cat, name = "n") %>%
  arrange(n) %>%
  ggplot(aes(x = n, y = income_cat)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  labs(
    title = "Household Income (Past 12 Months)",
    x = "Number of participants",
    y = NULL
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_blank(),
    
    # AXIS TICK LABELS
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )




#horizontal bar chart of wealth/savings   
subset_data %>%
  mutate(
    wealth_cat = factor(
      WEA_SVNGSVL_MCQ,  
      levels = c(1, 2, 3, 4, 8, 9),
      labels = c(
        "<50K",
        "50K–100K",
        "100K to <$1 million",
        "$1 million +",
        "Don't know/No answer",
        "Refused"
      )
    )
  ) %>%
  count(wealth_cat, name = "n") %>%
  arrange(n) %>%
  ggplot(aes(x = n, y = wealth_cat)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  labs(
    title = "Total Savings and Investments",
    x = "Number of participants",
    y = NULL
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 38,
      margin = margin(b = 18)
    ),
    axis.title.x = element_text(
      face = "bold", size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 26),
    axis.text.y  = element_text(size = 26)
  )


















# Boxplots of environmental variables -------------------------------------

#PM2.5
income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(pm25_1yr_com)) %>%
  mutate(
    income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)
  )

ggplot(df, aes(x = income_cat, y = pm25_1yr_com)) +
  geom_boxplot(
    fill = "#2C7FB8",      # ⬅️ matched darker blue
    outlier.alpha = 0.5,
    width = 0.6
  ) +
  labs(
    title = "PM2.5 1 y average, by Income Group",
    x = "Household Income",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )



#No2

income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df_no2 <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(no2_1yr_com)) %>%
  mutate(
    income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)
  )

ggplot(df_no2, aes(x = income_cat, y = no2_1yr_com)) +
  geom_boxplot(
    fill = "#2C7FB8",      # ⬅️ matched darker blue
    outlier.alpha = 0.5,
    width = 0.6
  ) +
  labs(
    title = "NO2 1 y average, by Income Group",
    x = "Household Income",
    y = "NO2 (ppb)"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIG
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    # AXIS TICK LABELS — BIG
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )


#So2

income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df_so2 <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(so2_1yr_com)) %>%
  mutate(
    income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)
  )

ggplot(df_so2, aes(x = income_cat, y = so2_1yr_com)) +
  geom_boxplot(
    fill = "#2C7FB8",      # ⬅️ matched darker blue
    outlier.alpha = 0.5,
    width = 0.6
  ) +
  labs(
    title = "SO2 1 y average, by Income Group",
    x = "Household Income",
    y = "SO2 (ppb)"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIG
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    # AXIS TICK LABELS — BIG
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )



#ozone
income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df_o3 <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(o3_1yr_com)) %>%
  mutate(
    income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)
  )

ggplot(df_o3, aes(x = income_cat, y = o3_1yr_com)) +
  geom_boxplot(
    fill = "#2C7FB8",      # ⬅️ matched darker blue
    outlier.alpha = 0.5,
    width = 0.6
  ) +
  labs(
    title = "Ozone 1 y average,by Income Group",
    x = "Household Income",
    y = "Ozone (ppb)"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIG
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    # AXIS TICK LABELS — BIG
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )


#greenness (NDVI)
income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

df_greenness <- subset_data %>%
  filter(INC_TOT_COM %in% 1:5, !is.na(GRLANYY_26_COM)) %>%
  mutate(
    income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)
  )

ggplot(df_greenness, aes(x = income_cat, y = GRLANYY_26_COM)) +
  geom_boxplot(
    fill = "#2C7FB8",      # ⬅️ matched darker blue
    outlier.alpha = 0.5,
    width = 0.6
  ) +
  labs(
    title = "Max NDVI within 500m, by Income Group",
    x = "Household Income",
    y = "Greenness (NDVI)"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIG
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    # AXIS TICK LABELS — BIG
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )




#bar plot of condensation problems by income level (only those that have condensation)
library(tidyverse)
library(scales)

cond_only <- subset_data %>% #create df with condensation cases only 
  filter(ENV_HMPRB_CON_MCQ == 1)  
table(cond_only$ENV_HMPRB_CON_MCQ)

income_labels <- c("<20K","20K–50K","50K–100K","100K–150K","150K+")

cond_counts <- cond_only %>%
  filter(INC_TOT_COM %in% 1:5) %>%
  mutate(income_cat = factor(INC_TOT_COM, levels = 1:5, labels = income_labels)) %>%
  count(income_cat, name = "n") %>%
  mutate(pct = n / sum(n))

ggplot(cond_counts, aes(x = income_cat, y = n)) +
  geom_col(fill = "#2C7FB8", width = 0.6) +
  geom_text(
    aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")),
    vjust = -0.35,
    size = 7.5    # bigger text to match your figure scale
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))   # room for labels
  ) +
  labs(
    title = "Homes with Condensation, by Income Group",
    x = "Household Income",
    y = "Homes with Condensation"
  ) +
  theme_classic(base_size = 26) +
  theme(
    legend.position = "none",
    
    # TITLE — BIG
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 38,
      margin = margin(b = 18)
    ),
    
    # AXIS TITLES — BIG
    axis.title.x = element_text(
      face = "bold",
      size = 30,
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 30,
      margin = margin(r = 16)
    ),
    
    # AXIS TICK LABELS — BIG
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )




