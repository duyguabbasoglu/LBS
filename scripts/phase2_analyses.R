setwd("/Users/dyliax/Desktop/365/Project/Data EU27 sample")
install.packages("skimr")
install.packages("janitor")
library(tidyverse)  # readr + dplyr + ggplot2
library(skimr)      # özet tablo
library(janitor)    # temiz frequency tablolar

#### 1) Veriyi Okuma (CSV) ####

# Burada .dta DEĞİL, values CSV kullanıyoruz
raw <- read_csv("eu_loneliness_survey_eu27_values.csv")

# Genel kontrol:
dim(raw)        # satır / sütun sayısı
names(raw)      # değişken isimlerine bak
glimpse(raw)    # hızlı yapı

# İPUCU:
# Eğer aşağıda kullandığım isimlerden biri yoksa (ör: health_general),
# names(raw) çıktısından doğru ismi bulup aşağıdaki select/mutate kısımlarını ona göre değiştir.


#### 2) Analizde Kullanacağımız Değişkenleri Seçelim ####

# Buraya projede kullanacağımız temel değişkenleri aldım
# (gerekiyorsa ekleyebilirsin)

data <- raw %>%
  select(
    country,               # ülke
    date_birth_year,       # doğum yılı
    gender,                # 1=Male,2=Female,3=Other
    education,             # eğitim seviyesi
    relationship,          # ilişki durumu
    health_general,        # genel sağlık
    loneliness_ucla_a,
    loneliness_ucla_b,
    loneliness_ucla_c,     # UCLA 3 item
    loneliness_direct,     # son 4 hafta yalnızlık
    loneliness_intensity,  # yalnızlık şiddeti
    feelings_depr,         # depresif hissetme sıklığı
    feelings_happy,        # mutlu hissetme sıklığı
    trust                  # genel güven
  )

#### 3) Yaş Değişkeni Oluşturma ####

# Anket Kasım–Aralık 2022’de yapıldı, o yüzden 2022 - doğum yılı
data <- data %>%
  mutate(
    age = 2022 - date_birth_year
  )


#### 4) 998 / 999 Kodlarını NA Yapma (Missing Cleaning) ####

# Veri setinde 998 = Don't know, 999 = Prefer not to say
to_na <- function(x) {
  ifelse(x %in% c(998, 999), NA, x)
}

data <- data %>%
  mutate(
    gender               = to_na(gender),
    education            = to_na(education),
    relationship         = to_na(relationship),
    health_general       = to_na(health_general),
    loneliness_ucla_a    = to_na(loneliness_ucla_a),
    loneliness_ucla_b    = to_na(loneliness_ucla_b),
    loneliness_ucla_c    = to_na(loneliness_ucla_c),
    loneliness_direct    = to_na(loneliness_direct),
    loneliness_intensity = to_na(loneliness_intensity),
    feelings_depr        = to_na(feelings_depr),
    feelings_happy       = to_na(feelings_happy),
    trust                = to_na(trust)
  )


#### 5) UCLA Loneliness Skoru Hesaplama ####

# 3 maddelik UCLA: her biri 1–3 arası → toplam 3–9
data <- data %>%
  mutate(
    ucla_score = loneliness_ucla_a +
      loneliness_ucla_b +
      loneliness_ucla_c,
    ucla_cat = case_when(
      ucla_score <= 4 ~ "Low",
      ucla_score <= 6 ~ "Moderate",
      ucla_score >  6 ~ "High",
      TRUE            ~ NA_character_
    )
  )


#### 6) Faktör Label’ları (Rapor İçin Daha Okunur Hali) ####

data <- data %>%
  mutate(
    gender_f = factor(
      gender,
      levels = c(1, 2, 3),
      labels = c("Male", "Female", "Other")
    ),
    health_general_f = factor(
      health_general,
      levels = 1:5,
      labels = c("Very good", "Fairly good", "Average",
                 "Fairly poor", "Very poor")
    ),
    loneliness_direct_f = factor(
      loneliness_direct,
      levels = 1:5,
      labels = c("All of the time", "Most of the time",
                 "Some of the time", "A little of the time",
                 "None of the time")
    )
  )


#### 7) SAYISAL DEĞİŞKENLER İÇİN DESCRIPTIVE STATISTICS ####

# İnceleyeceğimiz sayısal değişkenler:
# age, ucla_score, trust, feelings_depr, feelings_happy

numeric_desc <- data %>%
  select(age, ucla_score, trust,
         feelings_depr, feelings_happy) %>%
  skim()

numeric_desc   # RStudio Viewer’da güzel bir özet verecek

# İstersen base summary de görebilirsin:
summary(
  data %>%
    select(age, ucla_score, trust,
           feelings_depr, feelings_happy)
)


#### 8) KATEGORİKLER İÇİN FREKANS TABLOLARI ####

# 8.1 Cinsiyet dağılımı
gender_tab <- data %>%
  tabyl(gender_f) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

gender_tab

# 8.2 Genel sağlık durumu
health_tab <- data %>%
  tabyl(health_general_f) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

health_tab

# 8.3 UCLA kategorileri (Low / Moderate / High)
ucla_tab <- data %>%
  tabyl(ucla_cat) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

ucla_tab

# 8.4 Direkt yalnızlık sorusu ("past 4 weeks")
lonely_direct_tab <- data %>%
  tabyl(loneliness_direct_f) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

lonely_direct_tab


#### 9) ÖRNEK BİR ÇAPRAZ TABLO (Health x UCLA Category) ####

health_ucla_tab <- data %>%
  tabyl(health_general_f, ucla_cat) %>%   # satır: sağlık, sütun: yalnızlık kategorisi
  adorn_totals("row") %>%
  adorn_percentages("row") %>%            # satır içi yüzde
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()                              # parantez içinde n gösterir

health_ucla_tab


#### 10) RAPOR İÇİN KULLANILABİLECEK ÖRNEK YORUMLAR (kodu değil metni alacaksın) ####

# numeric_desc çıktılarına bakarak Phase II raporunda şöyle cümleler yazabilirsin:
#
# - "The mean age of respondents is XX years (SD = YY)."
# - "The UCLA-3 loneliness score ranges from 3 to 9, with an average of AA."
# - "Approximately BB% of respondents fall into the 'High loneliness' category."
# - "Around CC% of respondents rate their general health as 'Fairly good' or 'Very good'."
# - "Direct loneliness reports indicate that DD% of respondents felt lonely 'Most or all of the time' in the last 4 weeks."
#
# Bu cümlelerin içindeki XX, YY, AA gibi yerleri yukarıdaki tabloların gerçek değerleriyle dolduracaksın.



ggplot(data, aes(age, ucla_score)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "UCLA Loneliness Across Age")


data %>%
  group_by(gender_f) %>%
  summarise(mean_ucla = mean(ucla_score, na.rm = TRUE))


country_lonely <- data %>%
  group_by(country) %>%
  summarise(mean_loneliness = mean(ucla_score, na.rm = TRUE)) %>%
  arrange(desc(mean_loneliness))

head(country_lonely, 5)

ggplot(data, aes(feelings_happy, ucla_score)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()

data %>%
  group_by(health_general_f, ucla_cat) %>%
  summarise(avg_depr = mean(feelings_depr, na.rm = TRUE)) %>%
  ggplot(aes(health_general_f, ucla_cat, fill = avg_depr)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title="Depression Levels Across Health × Loneliness") +
  theme_minimal()

data %>%
  group_by(education) %>%
  summarise(mean_loneliness = mean(ucla_score, na.rm = TRUE))

data %>%
  group_by(relationship) %>%
  summarise(mean_ucla = mean(ucla_score, na.rm = TRUE)) %>%
  arrange(desc(mean_ucla))


ggplot(data, aes(trust, ucla_score)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method="loess", se=FALSE, color="purple") +
  theme_minimal()

data$happiness_binary <- ifelse(data$feelings_happy <= 3, "low", "high")

# happiness variability = depresyon + mutluluk farkı
data$affective_instability <- abs(data$feelings_happy - data$feelings_depr)

data %>% 
  group_by(affective_instability) %>% 
  summarise(mean_ucla = mean(ucla_score, na.rm = TRUE))


ggplot(data, aes(trust, ucla_score)) +
  geom_smooth(method="loess")


library(cluster)

cluster_df <- data %>% 
  select(ucla_score, trust, feelings_depr, feelings_happy, health_general) %>% 
  drop_na()

km <- kmeans(cluster_df, centers=3)
km$centers

data %>%
  filter(trust >= 9, ucla_score >= 7) %>%
  summarise(n())

lm(ucla_score ~ relationship + trust, data=data)

lmer(ucla_score ~ trust + health + (1 | country))
fa <- factanal(data %>% select(ucla_score, trust, feelings_depr, feelings_happy, health_general),
               factors = 3)

library(umap)
um <- umap(cluster_df)
plot(um$layout)
library(isolationForest)
moments::skewness(data$feelings_happy - data$feelings_depr)
lm(ucla_score ~ health_general + I(health_general^2))

rf <- randomForest(ucla_cat ~ trust + health_general + feelings_happy + feelings_depr)

