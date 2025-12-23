# Duygu Abbasoglu 22102978 - Lab Project

# part 1 : Use the Data (I selected built in dataset, airquality)
library(dplyr)
library(ggplot2)

data <- airquality
str(data)

# part 2 : Data Preparation
colSums(is.na(data))

if (any(is.na(data))) {
  data <- data %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    print("na values cleaned")
} else {
  print("there isn't any na value")
}
colSums(is.na(data))


# part 3 : Graphs

# plot1 : Wind vs Ozone
ggplot(data, aes(x = Wind, y = Ozone)) +
  geom_point(aes(color = Temp), alpha = 0.7) +
  geom_smooth(se = FALSE, color = "royalblue3") +
  labs(
    title = "Wind Speed vs Ozone Levels by Temperature",
    x     = "Wind (mph)",
    y     = "Ozone (ppb)",
    color = "Temperature (F)"
  ) +
  theme_minimal()

# plot2 : Mean Ozone by Month
monthly_ozone <- data %>%
  group_by(Month) %>%
  summarise(mean_ozone = mean(Ozone), .groups = "drop") %>%
  mutate(MonthName = factor(Month, levels = 5:9,
  labels = c("May", "June", "July", "August", "September")))

monthly_ozone

ggplot(monthly_ozone, aes(x = MonthName, y = mean_ozone, fill = MonthName)) +
  geom_col() + labs(title = "Average Ozone Levels by Month", x = "Month",
  y = "Average Ozone (ppb)") + scale_fill_brewer(palette = "Blues") +
  theme_minimal() + guides(fill = "none")
