library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)
library(viridisLite)
library(tidyverse)

df <- read.csv("eu_loneliness_survey_eu27_values.csv")

na_codes <- c(997, 998, 999)

df <- df %>%
  mutate(across(where(is.numeric), ~ replace(., . %in% na_codes, NA))) %>%
  mutate(age = 2022 - date_birth_year) %>%
  filter(age >= 16, age <= 100)

country_names <- c(
  "Belgium","Bulgaria","Czechia","Denmark","Germany",
  "Estonia","Ireland","Greece","Spain","France","Croatia","Italy","Cyprus",
  "Latvia","Lithuania","Luxembourg","Hungary","Malta","Netherlands","Austria",
  "Poland","Portugal","Romania","Slovenia","Slovakia","Finland","Sweden"
)

df <- df %>%
  mutate(
    ucla_score = loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c,
    ucla_level = case_when(
      ucla_score <= 4 ~ "Low",
      ucla_score <= 7 ~ "Moderate",
      TRUE            ~ "High"
    ),
    ucla_level = factor(ucla_level, levels = c("Low","Moderate","High"), ordered = TRUE),
    
    country_lbl = factor(country, levels = 1:27, labels = country_names),
    
    age_group = cut(
      age,
      breaks = c(16, 30, 45, 60, 75, 100),
      labels = c("16-30","31-45","46-60","61-75","75+"),
      right = TRUE, include.lowest = TRUE
    ),
    
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

#----
# DESCRIPTIVE STATISTICS



#----
#GRAPHS

# color palette lib
V_OPT <- "viridis"

pal2 <- viridisLite::viridis(2, option = V_OPT, begin = 0.25, end = 0.85)
pal3 <- viridis(3, option = V_OPT, begin = 0.25, end = 0.85)
pal4 <- viridis(4, option = V_OPT, begin = 0.25, end = 0.85)

theme_lbs <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )

graph_df <- df %>%
  mutate(
    ucla_level = factor(ucla_level, levels = c("Low", "Moderate", "High"), ordered = TRUE),
    age_group  = factor(age_group,  levels = c("16-30","31-45","46-60","61-75","75+"), ordered = TRUE),
    education_lbl = factor(
      education_lbl,
      levels = c("Not completed primary","Completed primary","Completed secondary",
                 "Post-secondary/Bachelor","Master/Doctoral"),
      ordered = TRUE
    ),
    health_lbl = factor(health_lbl,
                        levels = c("Very good","Fairly good","Average","Fairly poor","Very poor"),
                        ordered = TRUE),
    depression_lbl = factor(depression_lbl,
                            levels = c("Always","Very frequently","Occasionally","Rarely","Very rarely","Never"),
                            ordered = TRUE)
  )

names(graph_df)

#----
# H1
# MATERIALS

# Graph 1: Age x Loneliness x Depression (quadrant) (stacked proportions)
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
    ),
    quadrant = factor(
      quadrant,
      levels = c(
        "Low loneliness / low depression",
        "High loneliness / low depression",
        "Low loneliness / high depression",
        "High loneliness / high depression"
      ),
      ordered = TRUE
    )
  )

quad_cols <- setNames(pal4, levels(quad_df$quadrant))

g1 <- ggplot(quad_df, aes(x = age_group, fill = quadrant)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = quad_cols, name = "") +
  labs(
    x = "",
    y = ""
  ) +
  theme_lbs

print(g1)

# HYPOTHESIS TESTING
h1_df <- graph_df %>%
  dplyr::select(ucla_score, ucla_level, feelings_depr, feelings_happy, age, gender_lbl) %>%
  tidyr::drop_na(ucla_score, feelings_depr, feelings_happy, age, gender_lbl) %>%
  mutate(
    affective_instability = abs(feelings_happy - feelings_depr),
    happy_rev = max(feelings_happy, na.rm = TRUE) + min(feelings_happy, na.rm = TRUE) - feelings_happy
  )

# 1. Spearman: depression vs UCLA
h1_dep_spear <- cor.test(h1_df$feelings_depr, h1_df$ucla_score, method = "spearman")
print(h1_dep_spear)

# 2. Spearman: happiness (reversed) vs UCLA
h1_hap_spear <- cor.test(h1_df$happy_rev, h1_df$ucla_score, method = "spearman")
print(h1_hap_spear)

# 3. Spearman: affective instability vs UCLA
h1_instab_spear <- cor.test(h1_df$affective_instability, h1_df$ucla_score, method = "spearman")
print(h1_instab_spear)

# 4. OLS: UCLA score ~ depression + happiness + instability + controls
h1_m1 <- lm(ucla_score ~ feelings_depr + happy_rev + affective_instability + age + gender_lbl,
            data = h1_df)
summary(h1_m1)

# 5. Logistic robustness: High loneliness (UCLA level) ~ predictors
h1_df <- h1_df %>%
  mutate(high_lonely = (ucla_level == "High"))

h1_glm <- glm(high_lonely ~ feelings_depr + happy_rev + affective_instability + age + gender_lbl,
              data = h1_df, family = binomial())
summary(h1_glm)

# 6. odds ratios for key predictors
h1_or <- exp(coef(h1_glm)[c("feelings_depr","happy_rev","affective_instability")])
print(h1_or)

#----
# H2
# MATERIALS

# Graph 3: Education x Loneliness level (stacked proportions)
ucla_cols <- setNames(pal3, c("Low","Moderate","High"))

g3 <- graph_df %>%
  drop_na(education_lbl, ucla_level) %>%
  ggplot(aes(x = education_lbl, fill = ucla_level)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = ucla_cols, name = "Loneliness") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Education level",
    y = ""
  ) +
  theme_lbs +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(g3)

# Graph 4: High loneliness share by age group & gender (pyramid)
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

gender_cols <- setNames(pal2, c("Female","Male"))

g4 <- ggplot(age_gender_pyr,
             aes(x = p_signed, y = age_group, fill = gender_lbl)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_x_continuous(labels = function(x) percent(abs(x))) +
  scale_fill_manual(values = gender_cols, name = "") +
  labs(
    x = "",
    y = ""
  ) +
  theme_lbs

print(g4)

# HYPOTHESIS TESTING
h2_df <- graph_df %>%
  dplyr::select(ucla_score, ucla_level, education_lbl, age, age_group, gender_lbl) %>%
  tidyr::drop_na(ucla_score, education_lbl, age, gender_lbl, age_group)

# 1. Spearman: Education (ordinal) vs UCLA score
h2_edu_spear <- cor.test(as.numeric(h2_df$education_lbl), h2_df$ucla_score, method = "spearman")
print(h2_edu_spear)

# 2. Kruskal-Wallis: UCLA score differs by education level
h2_kw_edu <- kruskal.test(ucla_score ~ education_lbl, data = h2_df)
print(h2_kw_edu)

# 3. OLS with controls: UCLA score ~ education + age + gender
h2_m1 <- lm(ucla_score ~ education_lbl + age + gender_lbl, data = h2_df)
summary(h2_m1)

# 4. Ordered logistic robustness: UCLA level ~ education + age + gender
suppressPackageStartupMessages(library(MASS))

h2_polr <- MASS::polr(ucla_level ~ education_lbl + age + gender_lbl,
                      data = h2_df, Hess = TRUE)
summary(h2_polr)

# 5. p-values for polr (approx.)
h2_polr_p <- 2 * pnorm(abs(coef(summary(h2_polr))[, "t value"]), lower.tail = FALSE)
print(h2_polr_p)

#----
# H3
# MATERIALS

# Graph 2: Age x UCLA with Health
g2 <- ggplot(graph_df %>% drop_na(health_lbl),
             aes(x = age, y = ucla_score, color = health_lbl)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  scale_color_viridis_d(option = V_OPT, end = 0.9, name = "Health") +
  labs(x = "Age", y = "UCLA score") +
  theme_lbs +
  theme(legend.position = "right")

print(g2)

# Graph 5: Lifestyle cluster composition among high loneliness (exercise / fruits / smoking)
lifestyle_raw <- graph_df %>%
  drop_na(exercise, fruits, smoking, ucla_level) %>%
  mutate(
    exercise_bad = if_else(exercise <= median(exercise, na.rm = TRUE), 1L, 0L),
    fruits_bad   = if_else(fruits   <= median(fruits,   na.rm = TRUE), 1L, 0L),
    smoking_bad  = if_else(smoking  >= median(smoking,  na.rm = TRUE), 1L, 0L),
    risk_sum = exercise_bad + fruits_bad + smoking_bad,
    lifestyle_cluster = factor(case_when(
      risk_sum == 0 ~ "Healthy",
      risk_sum == 1 ~ "Mild risk",
      risk_sum == 2 ~ "Moderate risk",
      risk_sum >= 3 ~ "High risk"
    ), levels = c("Healthy","Mild risk","Moderate risk","High risk"), ordered = TRUE)
  )

donut_df <- lifestyle_raw %>%
  filter(ucla_level == "High") %>%
  count(lifestyle_cluster) %>%
  mutate(pct = n / sum(n))

life_cols <- setNames(pal4, c("Healthy","Mild risk","Moderate risk","High risk"))

g5 <- ggplot(donut_df, aes(x = 2, y = pct, fill = lifestyle_cluster)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = life_cols, name = "Lifestyle") +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 12) +
  theme(legend.position = "right")

print(g5)

# HYPOTHESIS TESTING
h3_df <- graph_df %>%
  dplyr::select(ucla_score, health_general, age, gender_lbl, exercise, fruits, smoking) %>%
  tidyr::drop_na(ucla_score, health_general, age, gender_lbl, exercise, fruits, smoking) %>%
  dplyr::mutate(
    exercise_bad = dplyr::if_else(exercise <= median(exercise, na.rm = TRUE), 1L, 0L),
    fruits_bad   = dplyr::if_else(fruits   <= median(fruits,   na.rm = TRUE), 1L, 0L),
    smoking_bad  = dplyr::if_else(smoking  >= median(smoking,  na.rm = TRUE), 1L, 0L),
    lifestyle_risk = exercise_bad + fruits_bad + smoking_bad
  )

# 1. Direction sanity checks (raw items vs UCLA)
h3_ex <- cor.test(h3_df$exercise, h3_df$ucla_score, method = "spearman")
cat("exercise ~ UCLA: rho =", unname(h3_ex$estimate), "p =", h3_ex$p.value, "\n")
h3_fr <- cor.test(h3_df$fruits,   h3_df$ucla_score, method = "spearman")
cat("fruits   ~ UCLA: rho =", unname(h3_fr$estimate), "p =", h3_fr$p.value, "\n")
h3_sm <- cor.test(h3_df$smoking,  h3_df$ucla_score, method = "spearman")
cat("smoking  ~ UCLA: rho =", unname(h3_sm$estimate), "p =", h3_sm$p.value, "\n")

# 2. Regression attenuation (mediation-style evidence)
h3_m1 <- lm(ucla_score ~ lifestyle_risk + age + gender_lbl, data = h3_df)
h3_m2 <- lm(ucla_score ~ lifestyle_risk + health_general + age + gender_lbl, data = h3_df)

b1 <- coef(summary(h3_m1))["lifestyle_risk", c("Estimate","Pr(>|t|)")]
b2 <- coef(summary(h3_m2))["lifestyle_risk", c("Estimate","Pr(>|t|)")]
bh <- coef(summary(h3_m2))["health_general", c("Estimate","Pr(>|t|)")]

cat("m1: beta_risk =", b1["Estimate"], "p =", b1["Pr(>|t|)"], " | R2 =", summary(h3_m1)$r.squared, "\n")
cat("m2: beta_risk =", b2["Estimate"], "p =", b2["Pr(>|t|)"], " | R2 =", summary(h3_m2)$r.squared, "\n")
cat("m2: beta_health =", bh["Estimate"], "p =", bh["Pr(>|t|)"], "\n")

attenuation_pct <- 100 * (1 - abs(b2["Estimate"]) / abs(b1["Estimate"]))
cat("attenuation of risk effect after adding health (%):", attenuation_pct, "\n")

#----
# H4
# MATERIALS

# Graph 6: Country-level consistency vs mismatch (UCLA vs Direct)
med_ucla   <- median(graph_df$ucla_score, na.rm = TRUE)
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
      !high_ucla &  high_direct ~ "Direct only high"
    )
  ) %>%
  count(country_lbl, pattern) %>%
  group_by(country_lbl) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

mismatch_country <- mismatch_df %>%
  group_by(country_lbl) %>%
  summarise(
    consistency = sum(pct[pattern %in% c("Both high","Both low")]),
    mismatch    = 1 - consistency,
    n           = sum(n),
    .groups = "drop"
  )

g6 <- ggplot(mismatch_country,
             aes(x = consistency, y = mismatch, size = n)) +
  geom_point(alpha = 0.85, color = pal3[2]) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_size_area(max_size = 11, guide = "none") +
  labs(
    x = "Consistency between UCLA and Direct (country share)",
    y = "Mismatch between UCLA and Direct (country share)"
  ) +
  theme_lbs

print(g6)

# Graph 7: Affective instability vs high loneliness share
affect_df_h4 <- graph_df %>%
  drop_na(feelings_happy, feelings_depr, ucla_level) %>%
  mutate(
    affective_instability = abs(feelings_happy - feelings_depr),
    instab_group = factor(case_when(
      affective_instability <= 1 ~ "Stable",
      affective_instability == 2 ~ "Moderate",
      affective_instability >= 3 ~ "High"
    ), levels = c("Stable","Moderate","High"), ordered = TRUE)
  ) %>%
  group_by(instab_group) %>%
  summarise(
    p_high_loneliness = mean(ucla_level == "High", na.rm = TRUE),
    mean_ucla         = mean(ucla_score, na.rm = TRUE),
    n                 = n(),
    .groups = "drop"
  )

g7 <- ggplot(affect_df_h4, aes(x = instab_group, y = p_high_loneliness, fill = instab_group)) +
  geom_col(alpha = 0.9) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = V_OPT, end = 0.9, guide = "none") +
  labs(
    x = "Affective instability",
    y = "Share with high UCLA loneliness"
  ) +
  theme_lbs

print(g7)

# HYPOTHESIS TESTING
h4_df <- graph_df %>%
  dplyr::select(ucla_score, loneliness_direct, feelings_depr, feelings_happy,
                age, gender_lbl, country_lbl) %>%
  tidyr::drop_na(ucla_score, loneliness_direct, feelings_depr, feelings_happy, age, gender_lbl) %>%
  dplyr::mutate(
    direct_rev = max(loneliness_direct, na.rm = TRUE) + min(loneliness_direct, na.rm = TRUE) - loneliness_direct,
    affective_instability = abs(feelings_happy - feelings_depr)
  )

# 1. Spearman alignment test (UCLA vs Direct reversed)
h4_spear <- cor.test(h4_df$ucla_score, h4_df$direct_rev, method = "spearman")
print(h4_spear)

# 2. Baseline regression: direct_rev ~ ucla
h4_m1 <- lm(direct_rev ~ ucla_score + age + gender_lbl, data = h4_df)
summary(h4_m1)

# 3. Moderation-style model
h4_m2 <- lm(direct_rev ~ ucla_score*feelings_depr + ucla_score*feelings_happy +
              ucla_score*affective_instability + age + gender_lbl,
            data = h4_df)
summary(h4_m2)

# 4. Spearman by instability group
h4instab <- h4_df %>%
  mutate(instab_group = case_when(
    affective_instability <= 1 ~ "Stable",
    affective_instability == 2 ~ "Moderate",
    affective_instability >= 3 ~ "High"
  )) %>%
  group_by(instab_group) %>%
  summarise(
    n = n(),
    rho = suppressWarnings(cor(ucla_score, direct_rev, method = "spearman")),
    .groups = "drop"
  )

print(h4instab)

#----
# H5
# MATERIALS

# Graph 8: Loneliness & Happiness groups by age
med_happy <- median(graph_df$feelings_happy, na.rm = TRUE)

paradox_df <- graph_df %>%
  drop_na(feelings_happy, ucla_level, age_group) %>%
  mutate(
    high_lonely = (ucla_level == "High"),
    high_happy  = feelings_happy <= med_happy,
    paradox_group = case_when(
      high_happy  &  high_lonely ~ "Happy & lonely",
      !high_happy &  high_lonely ~ "Unhappy & lonely",
      high_happy  & !high_lonely ~ "Happy & not lonely",
      !high_happy & !high_lonely ~ "Unhappy & not lonely"
    ),
    paradox_group = factor(paradox_group, levels = c(
      "Happy & not lonely","Happy & lonely","Unhappy & not lonely","Unhappy & lonely"
    ))
  ) %>%
  group_by(age_group, paradox_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age_group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

par_cols <- setNames(pal4, levels(paradox_df$paradox_group))

g8 <- ggplot(paradox_df, aes(x = age_group, y = pct, fill = paradox_group)) +
  geom_col(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = par_cols, name = "") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "Share") +
  theme_lbs

print(g8)

# HYPOTHESIS TESTING
h5_df <- graph_df %>%
  dplyr::select(ucla_score, ucla_level, feelings_happy, age, gender_lbl) %>%
  tidyr::drop_na(ucla_score, feelings_happy, age, gender_lbl) %>%
  dplyr::mutate(
    happy_rev = max(feelings_happy, na.rm = TRUE) + min(feelings_happy, na.rm = TRUE) - feelings_happy
  )

# 1. Spearman correlation: UCLA vs (reversed) happiness
h5_spear <- cor.test(h5_df$ucla_score, h5_df$happy_rev, method = "spearman")
print(h5_spear)

# 2. Linear regression: happy_rev ~ ucla_score + controls
h5_m1 <- lm(happy_rev ~ ucla_score + age + gender_lbl, data = h5_df)
summary(h5_m1)

# 3. Logistic-style robustness
med_happy_rev <- median(h5_df$happy_rev, na.rm = TRUE)

h5_df <- h5_df %>%
  mutate(high_happy = happy_rev > med_happy_rev)

h5_glm <- glm(high_happy ~ ucla_score + age + gender_lbl,
              data = h5_df, family = binomial())
summary(h5_glm)

# 4. convert UCLA coefficient to odds ratio
h5_or <- exp(coef(h5_glm)["ucla_score"])
h5_ci <- exp(confint(h5_glm, parm = "ucla_score"))
cat("\nOdds Ratio (ucla_score):", h5_or,
    "\n95% CI:", h5_ci[1], "-", h5_ci[2], "\n")

# 5. Simple group check (High vs not-High loneliness)
h5_group <- h5_df %>%
  mutate(lonely_high = (ucla_level == "High")) %>%
  group_by(lonely_high) %>%
  summarise(
    n = n(),
    mean_happy = mean(happy_rev, na.rm = TRUE),
    median_happy = median(happy_rev, na.rm = TRUE),
    .groups = "drop"
  )

print(h5_group)
