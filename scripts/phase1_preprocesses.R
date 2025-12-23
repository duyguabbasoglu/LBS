setwd("/Users/dyliax/Desktop/365/Project/DATA EU27 sample")
#----
# phase 2.1 - data preparation
# Duygu
library(tidyverse)
df <- read_csv("eu_loneliness_survey_eu27_values.csv")
na_codes <- c(997, 998, 999)

df <- df %>%
  mutate(across(where(is.numeric), ~ replace(., . %in% na_codes, NA))) %>%
  mutate(
    age = 2022 - date_birth_year
  ) %>%
  filter(age >= 16, age <= 100)

loneliness <- c("loneliness_ucla_a", "loneliness_ucla_b",
                "loneliness_ucla_c", "loneliness_direct",
                "loneliness_intensity")

health <- c("health_general", "health_condition",
            "health_condition_intensity", "exercise",
            "fruits", "smoking")

demographic <- c("age", "gender", "education", "country")

selected <- c(loneliness, health, demographic)
data <- df[, selected]

# calculation of loneliness scores + categorization
data$ucla_score <- data$loneliness_ucla_a +
  data$loneliness_ucla_b + data$loneliness_ucla_c

data <- subset(data, !is.na(ucla_score))

data$ucla_level <- ifelse(
  data$ucla_score <= 4, "Low",
  ifelse(data$ucla_score <= 7, "Moderate", "High")
)
data$ucla_level <- factor(data$ucla_level,
                          levels = c("Low", "Moderate", "High"),
                          ordered = TRUE)

#----
# phase 2.3 - descriptive statistics
# Burak

##ILK KISIM   DATA TEMIZLEME recode etme kismi

install.packages("tidyverse")


library(tidyverse)

df <- read_csv("eu_loneliness_survey_eu27_values.csv")

#df <- df %>%
# mutate(across(everything(), ~na_if(., 998))) %>% 
#mutate(across(everything(), ~na_if(., 999)))

df <- df %>%
  mutate(across(where(is.numeric), ~replace(., . %in% c(998,999), NA)))


names(df)      # kolon isimlerine bak
glimpse(df)    # tip / örnek değerleri gör

grep("birth", names(df), value = TRUE)


df <- df %>%
  mutate(age = 2022 - date_birth_year) %>%
  filter(age >= 16 & age <= 100)


summary(df$age)
table(df$age_group, useNA = "ifany")


df <- df %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(16, 30, 45, 60, 75, 100),
      labels = c("16–30", "31–45", "46–60", "61–75", "75+"),
      right = TRUE, include.lowest = TRUE
    )
  )


table(df$age_group, useNA = "ifany")


df <- df %>%
  mutate(
    gender_lbl = recode(gender,
                        `1` = "Male",
                        `2` = "Female",
                        `3` = "In another way"),
    
    education_lbl = recode(education,
                           `1` = "Not completed primary",
                           `2` = "Completed primary",
                           `3` = "Completed secondary",
                           `4` = "Post-secondary/Bachelor",
                           `5` = "Master/Doctoral"),
    
    relationship_lbl = recode(relationship,
                              `1` = "Single",
                              `2` = "In a relationship",
                              `3` = "Married/cohabiting",
                              `4` = "Separated/divorced",
                              `5` = "Widowed"),
    
    loneliness_direct_lbl = recode(loneliness_direct,
                                   `1` = "All of the time",
                                   `2` = "Most of the time",
                                   `3` = "Some of the time",
                                   `4` = "A little of the time",
                                   `5` = "None of the time"),
    
    health_lbl = recode(health_general,
                        `1` = "Very good",
                        `2` = "Fairly good",
                        `3` = "Average",
                        `4` = "Fairly poor",
                        `5` = "Very poor"),
    
    depression_lbl = recode(feelings_depr,
                            `1` = "Always",
                            `2` = "Very frequently",
                            `3` = "Occasionally",
                            `4` = "Rarely",
                            `5` = "Very rarely",
                            `6` = "Never")
  )


table(df$gender_lbl, useNA = "ifany")
table(df$education_lbl, useNA = "ifany")
table(df$relationship_lbl, useNA = "ifany")
table(df$loneliness_direct_lbl, useNA = "ifany")
table(df$health_lbl, useNA = "ifany")
table(df$depression_lbl, useNA = "ifany")


summary(df %>% select(age, loneliness_direct, health_general, feelings_depr))

df %>%
  group_by(age_group) %>%
  summarise(
    mean_loneliness = mean(loneliness_direct, na.rm = TRUE),
    mean_depression = mean(feelings_depr, na.rm = TRUE),
    mean_health     = mean(health_general, na.rm = TRUE),
    n = n()
  )

df %>%
  group_by(gender_lbl) %>%
  summarise(
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE),
    depression_mean = mean(feelings_depr, na.rm = TRUE),
    health_mean     = mean(health_general, na.rm = TRUE),
    n = n()
  )

##TABLE 2 BURDAN YAPILDI

demographic_table <- list(
  Gender = table(df$gender_lbl, useNA = "ifany"),
  Education = table(df$education_lbl, useNA = "ifany"),
  Relationship = table(df$relationship_lbl, useNA = "ifany")
)

demographic_table

install.packages("janitor")


library(janitor)

gender_tab <- tabyl(df$gender_lbl)
education_tab <- tabyl(df$education_lbl)
relationship_tab <- tabyl(df$relationship_lbl)

gender_tab
education_tab
relationship_tab


loneliness_tab <- tabyl(df$loneliness_direct_lbl)
health_tab <- tabyl(df$health_lbl)
depression_tab <- tabyl(df$depression_lbl)

loneliness_tab
health_tab
depression_tab

library(tidyverse)

library(e1071)  # skewness & kurtosis

numeric_vars <- df %>% 
  select(age, loneliness_direct, health_general, feelings_depr)

numeric_summary <- data.frame(
  Variable = names(numeric_vars),
  Minimum = sapply(numeric_vars, min, na.rm = TRUE),
  Q1 = sapply(numeric_vars, function(x) quantile(x, 0.25, na.rm = TRUE)),
  Mean = sapply(numeric_vars, mean, na.rm = TRUE),
  Median = sapply(numeric_vars, median, na.rm = TRUE),
  Q3 = sapply(numeric_vars, function(x) quantile(x, 0.75, na.rm = TRUE)),
  Maximum = sapply(numeric_vars, max, na.rm = TRUE),
  Std_Dev = sapply(numeric_vars, sd, na.rm = TRUE),
  Skewness = sapply(numeric_vars, moments::skewness, na.rm = TRUE),
  Kurtosis = sapply(numeric_vars, moments::kurtosis, na.rm = TRUE)
)

numeric_summary


##BU KISIM TABLE 1 ve TABLE 2 de yok ONLALR ICIN KULLANMADIM HENUZ
#BURDA IKI TABLO OLUSTURDUM IKISIDE CSV HALINDE BIRI DESCRIPTIVIN DAHA AZ KAPSAYACI HALI SKEWNESS VE KURTOSIS YOK
#DIGERI GENDER EDUCATION RELATIONSHIP LONELINESS(DIRECT) GENERAL HEALTH ve DEPRESSION FEELING in KATAGORILERINI TOPLAM SAYILARINI VE HER KATAGORININ TOPLAM SAYIDA YUZDE KACA DENK GELDIGI

library(dplyr)
library(janitor)

# Gender
tab_gender <- df %>%
  tabyl(gender_lbl) %>%
  mutate(Variable = "Gender") %>%
  rename(Category = gender_lbl,
         n = n,
         percent = percent)

# Education
tab_edu <- df %>%
  tabyl(education_lbl) %>%
  mutate(Variable = "Education") %>%
  rename(Category = education_lbl,
         n = n,
         percent = percent)

# Relationship
tab_rel <- df %>%
  tabyl(relationship_lbl) %>%
  mutate(Variable = "Relationship") %>%
  rename(Category = relationship_lbl,
         n = n,
         percent = percent)

# Loneliness direct
tab_lon <- df %>%
  tabyl(loneliness_direct_lbl) %>%
  mutate(Variable = "Loneliness (direct)") %>%
  rename(Category = loneliness_direct_lbl,
         n = n,
         percent = percent)

# General health
tab_health <- df %>%
  tabyl(health_lbl) %>%
  mutate(Variable = "General health") %>%
  rename(Category = health_lbl,
         n = n,
         percent = percent)

# Depression
tab_dep <- df %>%
  tabyl(depression_lbl) %>%
  mutate(Variable = "Depression feeling") %>%
  rename(Category = depression_lbl,
         n = n,
         percent = percent)

# Hepsini tek tabloda birleştir
freq_table <- bind_rows(
  tab_gender,
  tab_edu,
  tab_rel,
  tab_lon,
  tab_health,
  tab_dep
) %>%
  mutate(percent = round(percent * 100, 1)) %>%   # yüzdeye çevir
  select(Variable, Category, n, percent)

freq_table

write.csv(freq_table, "table_descriptive_frequencies.csv", row.names = FALSE)



library(dplyr)

numeric_vars <- df %>%
  select(age, loneliness_direct, health_general, feelings_depr)

# Her değişken için istatistik fonksiyonu
stats_fun <- function(x) {
  c(
    N       = sum(!is.na(x)),
    Minimum = min(x, na.rm = TRUE),
    Q1      = quantile(x, 0.25, na.rm = TRUE),
    Mean    = mean(x, na.rm = TRUE),
    Median  = median(x, na.rm = TRUE),
    Q3      = quantile(x, 0.75, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    SD      = sd(x, na.rm = TRUE)
  )
}

numeric_summary <- sapply(numeric_vars, stats_fun) %>%
  as.data.frame()

numeric_summary$Statistic <- rownames(numeric_summary)

numeric_summary <- numeric_summary %>%
  select(Statistic, everything()) %>%
  mutate(across(-Statistic, ~round(., 3)))   # 3 basamaklı yuvarla

numeric_summary

write.csv(numeric_summary, "table_numeric_summary.csv", row.names = FALSE)



##TABLE 1 KISMINI BURDAN YAPTIM

library(dplyr)
library(janitor)

table_gender_full <- df %>%
  group_by(gender_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE),
    health_mean = mean(health_general, na.rm = TRUE),
    depression_mean = mean(feelings_depr, na.rm = TRUE)
  ) %>%
  ungroup()

table_gender_full


table_edu_loneliness <- df %>%
  group_by(education_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  ) %>%
  arrange(loneliness_mean)

table_edu_loneliness


table_relationship_loneliness <- df %>%
  group_by(relationship_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  ) %>%
  arrange(loneliness_mean)

table_relationship_loneliness


table_age <- df %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE),
    health_mean = mean(health_general, na.rm = TRUE),
    depression_mean = mean(feelings_depr, na.rm = TRUE)
  )


table_age


table_gender_loneliness <- df %>%
  group_by(gender_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  )

table_gender_loneliness


table_health_loneliness <- df %>%
  group_by(health_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  ) %>%
  arrange(loneliness_mean)

table_health_loneliness


table_depression_loneliness <- df %>%
  group_by(depression_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  ) %>%
  arrange(loneliness_mean)

table_depression_loneliness


df <- df %>% mutate(
  migrant_lbl = recode(
    country_birth_yourself,
    `1` = "Native",
    `2` = "EU migrant",
    `3` = "Non-EU migrant",
    .default = NA_character_
  )
)

table_migrant_loneliness <- df %>%
  group_by(migrant_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  )

table_migrant_loneliness



df <- df %>% mutate(
  child_health_lbl = recode(child_health,
                            `1` = "Very good",
                            `2` = "Good",
                            `3` = "Average",
                            `4` = "Poor",
                            `5` = "Very poor")
)

table_child_health_loneliness <- df %>%
  group_by(child_health_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  )

table_child_health_loneliness


grep("child", names(df), value = TRUE)

df <- df %>% mutate(
  rel_mother_lbl = recode(
    child_relationship_a,
    `1` = "Very good",
    `2` = "Good",
    `3` = "Average",
    `4` = "Poor",
    `5` = "Very poor",
    .default = NA_character_
  )
)

table_rel_mother_loneliness <- df %>%
  group_by(rel_mother_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  )

table_rel_mother_loneliness


df <- df %>% mutate(
  rel_father_lbl = recode(
    child_relationship_b,
    `1` = "Very good",
    `2` = "Good",
    `3` = "Average",
    `4` = "Poor",
    `5` = "Very poor",
    .default = NA_character_
  )
)


table_rel_father_loneliness <- df %>%
  group_by(rel_father_lbl) %>%
  summarise(
    n = n(),
    loneliness_mean = mean(loneliness_direct, na.rm = TRUE)
  )

table_rel_father_loneliness

# countries
country_names <- c(
  "Belgium","Bulgaria","Czechia","Denmark","Germany",
  "Estonia","Ireland","Greece","Spain","France","Croatia","Italy","Cyprus",
  "Latvia","Lithuania","Luxembourg","Hungary","Malta","Netherlands","Austria",
  "Poland","Portugal","Romania","Slovenia","Slovakia","Finland","Sweden"
)

data$country <- factor(data$country,
                       levels = 1:27,
                       labels = country_names)

# derived variables for graphs
data$age_group <- cut(
  data$age,
  breaks = c(16, 30, 45, 60, 75, 100),
  labels = c("16-30", "31-45", "46-60", "61-75", "75+"),
  right = TRUE,
  include.lowest = TRUE
)

data$gender_f <- factor(
  data$gender,
  levels = c(1, 2, 3),
  labels = c("Male", "Female", "In another way")
)

data$education_f <- factor(
  data$education,
  levels = 1:5,
  labels = c("Not prim.", "Prim.", "Secondary",
             "Post-sec/Bachelor", "Master/PhD")
)

data$loneliness_direct_f <- factor(
  data$loneliness_direct,
  levels = 1:5,
  labels = c("All of the time",
             "Most of the time",
             "Some of the time",
             "A little of the time",
             "None of the time")
)

data$health_general_f <- factor(
  data$health_general,
  levels = 1:5,
  labels = c("Very good", "Fairly good", "Average",
             "Fairly poor", "Very poor")
)

df <- df %>%
  mutate(
    ucla_score = loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c,
    ucla_level = case_when(
      ucla_score <= 4 ~ "Low",
      ucla_score <= 7 ~ "Moderate",
      TRUE           ~ "High"
    ),
    ucla_level = factor(ucla_level,
                        levels = c("Low", "Moderate", "High"),
                        ordered = TRUE),
    
    country_lbl = factor(country,
                         levels = 1:27,
                         labels = country_names),
    
    age_group = cut(
      age,
      breaks = c(16, 30, 45, 60, 75, 100),
      labels = c("16-30", "31-45", "46-60", "61-75", "75+"),
      right = TRUE,
      include.lowest = TRUE
    ),
    
    gender_lbl = recode(gender,
                        `1` = "Female",   # tabloda bu sıra var
                        `2` = "Male",
                        `3` = "In another way"),
    
    education_lbl = recode(education,
                           `1` = "Not completed primary",
                           `2` = "Completed primary",
                           `3` = "Completed secondary",
                           `4` = "Post-secondary/Bachelor",
                           `5` = "Master/Doctoral"),
    
    relationship_lbl = recode(relationship,
                              `1` = "Single",
                              `2` = "In a relationship",
                              `3` = "Married/cohabiting",
                              `4` = "Separated/divorced",
                              `5` = "Widowed"),
    
    loneliness_direct_lbl = recode(loneliness_direct,
                                   `1` = "All of the time",
                                   `2` = "Most of the time",
                                   `3` = "Some of the time",
                                   `4` = "A little of the time",
                                   `5` = "None of the time"),
    
    health_lbl = recode(health_general,
                        `1` = "Very good",
                        `2` = "Fairly good",
                        `3` = "Average",
                        `4` = "Fairly poor",
                        `5` = "Very poor"),
    
    depression_lbl = recode(feelings_depr,
                            `1` = "Always",
                            `2` = "Very frequently",
                            `3` = "Occasionally",
                            `4` = "Rarely",
                            `5` = "Very rarely",
                            `6` = "Never")
  )

head(data)
summary(data$ucla_score)


#----
# phase 2.5 - graphs
# Mutual
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)
library(tidyverse)

# clean df for graphs
graph_df <- df %>%
  mutate(
    ucla_level = factor(
      ucla_level,
      levels = c("Low", "Moderate", "High"),
      ordered = TRUE
    ),
    age_group = factor(
      age_group,
      levels = c("16-30", "31-45", "46-60", "61-75", "75+"),
      ordered = TRUE
    ),
    education_lbl = factor(
      education_lbl,
      levels = c(
        "Not completed primary",
        "Completed primary",
        "Completed secondary",
        "Post-secondary/Bachelor",
        "Master/Doctoral"
      ),
      ordered = TRUE
    ),
    health_lbl = factor(
      health_lbl,
      levels = c("Very good", "Fairly good", "Average",
                 "Fairly poor", "Very poor"),
      ordered = TRUE
    ),
    depression_lbl = factor(
      depression_lbl,
      levels = c("Always", "Very frequently", "Occasionally",
                 "Rarely", "Very rarely", "Never"),
      ordered = TRUE
    )
  )

#----
# graph 1 : analysis of depression and loneliness

med_ucla <- median(graph_df$ucla_score,    na.rm = TRUE)
med_depr <- median(graph_df$feelings_depr, na.rm = TRUE)

quad_df <- graph_df %>%
  drop_na(ucla_score, feelings_depr, age_group) %>%
  mutate(
    is_lonely    = ucla_score > med_ucla,
    is_depressed = feelings_depr <= med_depr,
    quadrant = case_when(
      !is_lonely & !is_depressed ~ "Low loneliness / low depression",
      is_lonely  & !is_depressed ~ "High loneliness / low depression",
      !is_lonely &  is_depressed ~ "Low loneliness / high depression",
      is_lonely  &  is_depressed ~ "High loneliness / high depression"
    )
  )

ggplot(quad_df,
       aes(x = age_group, fill = quadrant)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) + scale_fill_manual(
    values = c(
      "Low loneliness / low depression"   = "#f3a683", 
      "High loneliness / low depression"  = "#d4475d",
      "Low loneliness / high depression"  = "#932667", 
      "High loneliness / high depression" = "#30123b" 
    )
  )
+
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

#----
# graph 2 : age & loneliness w. health

ggplot(graph_df %>% drop_na(health_lbl),
       aes(x = age,
           y = ucla_score,
           color = health_lbl)) +
  geom_smooth(method = "loess",
              se = FALSE,
              linewidth = 1.1) +
  scale_color_viridis_d(option = "magma", end = 0.9,
                        name = "Health") +
  labs(
    x = "age",
    y = "ucla scores"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )

#----
# graph 3 : education & loneliness

graph_df %>%
  drop_na(education_lbl, ucla_level) %>%
  mutate(
    # Stack sırası: Low bottom -> High top
    ucla_for_plot = factor(
      ucla_level,
      levels = c("Low", "Moderate", "High")
    )
  ) %>%
  ggplot(aes(x = education_lbl, fill = ucla_for_plot)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(
    values = c(
      "Low"      = "#4CC9F0",
      "Moderate" = "#4361EE",
      "High"     = "#3A0CA3"
    ),
    name   = "Loneliness",
    breaks = c("High", "Moderate", "Low"),
    labels = c("High", "Moderate", "Low")
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "education level",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


#----
# graph 4 : loneliness by gender

age_gender_pyr <- graph_df %>%
  drop_na(ucla_level, gender_lbl, age_group) %>%
  filter(gender_lbl %in% c("Female", "Male")) %>%
  group_by(age_group, gender_lbl) %>%
  summarise(
    p_high = mean(ucla_level == "High", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    p_signed = if_else(gender_lbl == "Male", -p_high, p_high)
  )

ggplot(age_gender_pyr,
       aes(x = p_signed,
           y = age_group,
           fill = gender_lbl)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_x_continuous(
    labels = function(x) percent(abs(x))
  ) +
  scale_fill_manual(
    values = c(
      "Female" = "deeppink3",
      "Male"   = "steelblue4"
    )
  ) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
#----
# graph 5 : lifestyle and loneliness *

lifestyle_raw <- graph_df %>%
  drop_na(exercise, fruits, smoking, ucla_level) %>%
  mutate(
    exercise_bad = if_else(exercise <= median(exercise, na.rm = TRUE), 1, 0),
    fruits_bad   = if_else(fruits   <= median(fruits,   na.rm = TRUE), 1, 0),
    smoking_bad  = if_else(smoking  >= median(smoking,  na.rm = TRUE), 1, 0),
    risk_sum = exercise_bad + fruits_bad + smoking_bad,
    lifestyle_cluster = case_when(
      risk_sum == 0 ~ "Healthy",
      risk_sum == 1 ~ "Mild risk",
      risk_sum == 2 ~ "Moderate risk",
      risk_sum >= 3 ~ "High risk"
    )
  )

donut_df <- lifestyle_raw %>%
  filter(ucla_level == "High") %>%
  count(lifestyle_cluster) %>%
  mutate(pct = n / sum(n))

ggplot(donut_df,
       aes(x = 2,
           y = pct,
           fill = lifestyle_cluster)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    values = c(
      "Healthy"       = "darkolivegreen1",  # en koyu, en sağlıklı
      "Mild risk"     = "darkolivegreen2",      # canlı yeşil
      "Moderate risk" = "darkolivegreen3",      # yeşil-sarı arası
      "High risk"     = "darkolivegreen4"   # en açık, sarımsı
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Lifestyle"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right"
  )

#----
# graph 6 : ucla & direct loneliness mismatch *

med_direct <- median(graph_df$loneliness_direct, na.rm = TRUE)

mismatch_df <- graph_df %>%
  drop_na(ucla_score, loneliness_direct, country_lbl) %>%
  mutate(
    high_ucla   = ucla_score > med_ucla,
    high_direct = loneliness_direct <= med_direct,
    pattern = case_when(
      high_ucla  &  high_direct ~ "Both high",
      !high_ucla & !high_direct ~ "Both low",
      high_ucla  & !high_direct ~ "UCLA only high",
      !high_ucla & high_direct  ~ "Direct only high"
    )
  ) %>%
  group_by(country_lbl, pattern) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(country_lbl) %>%
  mutate(
    pct = n / sum(n)
  )

mismatch_country <- mismatch_df %>%
  group_by(country_lbl) %>%
  summarise(
    consistency = sum(pct[pattern %in% c("Both high", "Both low")]),
    mismatch    = 1 - consistency,
    n           = sum(n),
    .groups = "drop"
  )

ggplot(mismatch_country,
       aes(x = consistency,
           y = mismatch,
           size = n)) +
  geom_point(alpha = 0.8, color = "royalblue3") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_size_area(max_size = 12, guide = "none") +
  labs(
    x = "ucla scores",
    y = "their replys"
  ) +
  theme_minimal(base_size = 12)

#----
# graph 7 : mood change freq. and loneliness *

affect_df <- graph_df %>%
  drop_na(feelings_happy, feelings_depr, ucla_level) %>%
  mutate(
    affective_instability = abs(feelings_happy - feelings_depr),
    instab_group = case_when(
      affective_instability <= 1 ~ "Stable",
      affective_instability == 2 ~ "Moderate",
      affective_instability >= 3 ~ "High"
    ),
    instab_group = factor(
      instab_group,
      levels = c("Stable", "Moderate", "High"),
      ordered = TRUE
    )
  ) %>%
  group_by(instab_group) %>%
  summarise(
    p_high_loneliness = mean(ucla_level == "High", na.rm = TRUE),
    mean_ucla         = mean(ucla_score, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(affect_df,
       aes(x = instab_group,
           y = p_high_loneliness)) +
  geom_col(fill = "indianred", alpha = 0.9) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Instability",
    y = "High loneliness"
  ) +
  theme_minimal(base_size = 12)

#----
# graph 8 : analysis of happiness and loneliness

med_happy <- median(graph_df$feelings_happy, na.rm = TRUE)

paradox_df <- graph_df %>%
  drop_na(feelings_happy, ucla_level, age_group) %>%
  mutate(
    high_lonely = ucla_level == "High",
    high_happy  = feelings_happy <= med_happy,
    paradox_group = case_when(
      high_happy  &  high_lonely ~ "Happy & lonely",
      !high_happy &  high_lonely ~ "Unhappy & lonely",
      high_happy  & !high_lonely ~ "Happy & not lonely",
      !high_happy & !high_lonely ~ "Unhappy & not lonely"
    )
  ) %>%
  group_by(age_group, paradox_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age_group) %>%
  mutate(
    pct = n / sum(n)
  )

ggplot(paradox_df,
       aes(x = age_group,
           y = pct,
           fill = paradox_group)) +
  geom_col(position = position_dodge(width = 0.7)) +
  scale_fill_manual(
    values = c(
      "Happy & not lonely"   = "salmon1", 
      "Happy & lonely"       = "tomato2", 
      "Unhappy & not lonely" = "orangered2",
      "Unhappy & lonely"     = "red3" 
    )
  )  +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

#---- 
# HEATMAP

lbs_age_gender <- graph_df %>%
  drop_na(age_group, gender_lbl, loneliness_intensity) %>%
  group_by(age_group, gender_lbl) %>%
  summarise(
    mean_lbs = mean(loneliness_intensity, na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    age_group  = factor(age_group,
                        levels = c("16-30", "31-45", "46-60", "61-75", "75+")),
    gender_lbl = factor(gender_lbl,
                        levels = c("Female", "Male", "In another way"))
  )

ggplot(lbs_age_gender,
       aes(x = gender_lbl,
           y = age_group,
           fill = mean_lbs)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f\nn=%d", mean_lbs, n)),
            colour = "white",
            size = 3.2) +
  scale_fill_viridis_c(
    option    = "viridis",
    direction = -1,
    name      = "Mean LBS"
  ) +
  labs(
    x = "",
    y = "",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )

#----
# TESTS

# H3 TEST

# Spearman Direction check for raw lifestyle items vs UCLA
h3_check <- graph_df %>%
  select(exercise, fruits, smoking, health_general, ucla_score) %>%
  drop_na()

ex_test <- cor.test(h3_check$exercise, h3_check$ucla_score, method = "spearman")
fr_test <- cor.test(h3_check$fruits,   h3_check$ucla_score, method = "spearman")
sm_test <- cor.test(h3_check$smoking,  h3_check$ucla_score, method = "spearman")

cat("exercise vs UCLA: rho =", unname(ex_test$estimate), "p =", ex_test$p.value, "\n")
cat("fruits   vs UCLA: rho =", unname(fr_test$estimate), "p =", fr_test$p.value, "\n")
cat("smoking  vs UCLA: rho =", unname(sm_test$estimate), "p =", sm_test$p.value, "\n")

# Assumption: "bad" = less exercise, less fruits, more smoking
h3_df <- graph_df %>%
  select(ucla_score, ucla_level, health_general, age, gender_lbl,
         exercise, fruits, smoking) %>%
  drop_na(ucla_score, health_general, exercise, fruits, smoking, age, gender_lbl) %>%
  mutate(
    exercise_bad = if_else(exercise <= median(exercise, na.rm = TRUE), 1L, 0L),
    fruits_bad   = if_else(fruits   <= median(fruits,   na.rm = TRUE), 1L, 0L),
    smoking_bad  = if_else(smoking  >= median(smoking,  na.rm = TRUE), 1L, 0L),
    lifestyle_risk = exercise_bad + fruits_bad + smoking_bad,
    lifestyle_cluster = factor(
      case_when(
        lifestyle_risk == 0 ~ "Healthy",
        lifestyle_risk == 1 ~ "Mild risk",
        lifestyle_risk == 2 ~ "Moderate risk",
        lifestyle_risk == 3 ~ "High risk"
      ),
      levels = c("Healthy", "Mild risk", "Moderate risk", "High risk"),
      ordered = TRUE
    )
  )

# Spearman Correlation chain tests (pathway evidence)
c1 <- cor.test(h3_df$lifestyle_risk, h3_df$health_general, method = "spearman")
c2 <- cor.test(h3_df$health_general, h3_df$ucla_score,     method = "spearman")
c3 <- cor.test(h3_df$lifestyle_risk, h3_df$ucla_score,     method = "spearman")

cat("lifestyle_risk vs health_general: rho =", unname(c1$estimate), "p =", c1$p.value, "\n")
cat("health_general  vs ucla_score:    rho =", unname(c2$estimate), "p =", c2$p.value, "\n")
cat("lifestyle_risk vs ucla_score:     rho =", unname(c3$estimate), "p =", c3$p.value, "\n")

# Regression attenuation test
m1 <- lm(ucla_score ~ lifestyle_risk + age + gender_lbl, data = h3_df)
m2 <- lm(ucla_score ~ lifestyle_risk + health_general + age + gender_lbl, data = h3_df)

cat("m1 (no health): lifestyle_risk beta =", coef(summary(m1))["lifestyle_risk", "Estimate"],
    "p =", coef(summary(m1))["lifestyle_risk", "Pr(>|t|)"], "\n")

cat("m2 (+health):  lifestyle_risk beta =", coef(summary(m2))["lifestyle_risk", "Estimate"],
    "p =", coef(summary(m2))["lifestyle_risk", "Pr(>|t|)"], "\n")

cat("m2 (+health):  health_general beta  =", coef(summary(m2))["health_general", "Estimate"],
    "p =", coef(summary(m2))["health_general", "Pr(>|t|)"], "\n")

cat("\nR2 m1:", summary(m1)$r.squared, " | R2 m2:", summary(m2)$r.squared, "\n")
