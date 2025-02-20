library(ggplot2)
library(dplyr)

plot_histogram <- function(data, column_name, title, xlab) {
  col <- data[[column_name]]
  data_range <- max(col) - min(col)
  first_val <- data_range / 2
  
  if (first_val < 4){
    binwidth <- 0.5
  } else if (first_val < 12) {
    binwidth <- 2
  } else if (first_val < 12 * 2.5) {
    binwidth <- 5
  } else if (first_val < 12 * 2.5 * 2) {
    binwidth <- 10
  } else {
    binwidth <- 15  
  }
  
  ggplot(data, aes(x = .data[[column_name]])) +
    geom_histogram(binwidth = binwidth, fill = "skyblue", color = "black") +
    labs(title = title, x = xlab, y = "Frequency") +
    xlim(min(col), max(col)) +
    theme_minimal()
}

plot_histogram(uskudar, "PM10 ( µg/m3 )", "PM10 Levels in Uskudar", "PM10 (µg/m3)")
plot_histogram(uskudar, "PM 2.5 ( µg/m3 )", "PM2.5 Levels in Uskudar", "PM2.5 (µg/m3)")
plot_histogram(uskudar, "SO2 ( µg/m3 )", "SO2 Levels in Uskudar", "SO2 (µg/m3)")
plot_histogram(uskudar, "NO2 ( µg/m3 )", "NO2 Levels in Uskudar", "NO2 (µg/m3)")
# so2 graph shows that it must have huge outliers
# and my data seems negatively skewed

# plot histograms without outliers
plot_histogram(uskudar, "PM10 ( µg/m3 )", "PM10 Levels in Uskudar without Outliers", "PM10 (µg/m3)")
plot_histogram(uskudar, "PM 2.5 ( µg/m3 )", "PM2.5 Levels in Uskudar without Outliers", "PM2.5 (µg/m3)")
plot_histogram(uskudar, "SO2 ( µg/m3 )", "SO2 Levels in Uskudar without Outliers", "SO2 (µg/m3)")
plot_histogram(uskudar, "NO2 ( µg/m3 )", "NO2 Levels in Uskudar without Outliers", "NO2 (µg/m3)")

boxplot(anatolian_electricity$KARTAL)
boxplot(anatolian_electricity$ÜSKÜDAR)
boxplot(anatolian_electricity$ÜMRANİYE)
boxplot(european_electricity$SİLİVRİ)
boxplot(european_electricity$ARNAVUTKÖY)
boxplot(european_electricity$SULTANGAZİ)
# observed that there are outliers in the data

boxplot(anatolian_electricity$KARTAL)
boxplot(anatolian_electricity$ÜSKÜDAR)
boxplot(anatolian_electricity$ÜMRANİYE)
boxplot(european_electricity$SİLİVRİ)
boxplot(european_electricity$ARNAVUTKÖY)
boxplot(european_electricity$SULTANGAZİ)
# completed cleaning and removing outliers

all_data <- bind_rows(
  uskudar %>% mutate(District = "ÜSKÜDAR"),
  kartal %>% mutate(District = "KARTAL"),
  umraniye %>% mutate(District = "ÜMRANİYE"),
  silivri %>% mutate(District = "SİLİVRİ"),
  arnavutkoy %>% mutate(District = "ARNAVUTKÖY"),
  sultangazi %>% mutate(District = "SULTANGAZİ")
)

all_data <- all_data %>%
  mutate(
    date = as.Date(format(...1, "%Y-%m-%d")),
    Period = ifelse(
      date <= as.Date("2020-04-12"), "Pre-COVID",
      ifelse(date <= as.Date("2020-12-31"), "During COVID", "Post-COVID")
    )
  )

# boxplot pm10 comparison
ggplot(all_data, aes(x = District, y = `PM10 ( µg/m3 )`, fill = District)) +
  geom_boxplot() +
  labs(x = "District", y = expression("PM"[10]~"("*mu*"g/m"^3*")")) +
  theme_minimal()

# boxplot pm2.5 comparison
ggplot(all_data, aes(x = District, y = `PM 2.5 ( µg/m3 )`, fill = District)) +
  geom_boxplot() +
  labs(x = "District", y = "PM 2.5 (µg/m3)") +
  theme_minimal()

# boxplot so2 comparison
ggplot(all_data, aes(x = District, y = `SO2 ( µg/m3 )`, fill = District)) +
  geom_boxplot() +
  labs(x = "District", y = "PM 2.5 (µg/m3)") +
  theme_minimal()

# boxplot no2 comparison
ggplot(all_data, aes(x = District, y = `NO2 ( µg/m3 )`, fill = District)) +
  geom_boxplot() +
  labs(x = "District", y = "PM 2.5 (µg/m3)") +
  theme_minimal()


# h1
# h1
# h1
aggregate_so2 <- function(data, district_name) {
  data %>%
    mutate(month = as.Date(format(as.Date(`...1`), "%Y-%m-01"))) %>%  
    group_by(month) %>%
    summarise(SO2_mean = mean(`SO2 ( µg/m3 )`, na.rm = TRUE)) %>%
    mutate(District = district_name)
}

so2_data <- bind_rows(
  aggregate_so2(kartal, "KARTAL"),
  aggregate_so2(uskudar, "ÜSKÜDAR"),
  aggregate_so2(umraniye, "ÜMRANİYE"),
  aggregate_so2(arnavutkoy, "ARNAVUTKÖY"),
  aggregate_so2(silivri, "SİLİVRİ"),
  aggregate_so2(sultangazi, "SULTANGAZİ")
)

industrial_vs_so2 <- electricity_data %>%
  left_join(so2_data, by = c("month", "District"))


ggplot(industrial_vs_so2, aes(x = month)) +
  geom_point(aes(y = Electricity, color = "Electricity Consumption"), size = 1) +
  geom_smooth(aes(y = Electricity, color = "Electricity Consumption"), method = "loess", linetype = "dashed") +
  geom_line(aes(y = SO2_mean * 100000, color = "SO2 Levels"), size = 1) +
  geom_smooth(aes(y = SO2_mean * 100000, color = "SO2 Levels"), method = "loess", linetype = "dashed") +
  facet_wrap(~ District, scales = "free_y") +
  labs(x = "Month",
       y = expression("Value (" ~ SO[2] ~ " * 10"^6*")"),
       color = "Metric"
       ) +
  scale_color_manual(
    values = c("Electricity Consumption" = "blue", "SO2 Levels" = "red"),
    labels = c(
      "Electricity Consumption" = "Total Electricity Consumption (kWh)",
      "SO2 Levels" = expression("Value (" ~ SO[2] ~ " * 10"^6*")")
        )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# h2
# h2
# h2
no2_period <- all_data %>%
  group_by(District, Period) %>%
  summarize(Avg_NO2 = mean(`NO2 ( µg/m3 )`, na.rm = TRUE), .groups = "drop")  %>%
  mutate(Location = ifelse(
    District %in% anatolian_districts, "Anatolian", "European")
  )

no2_period$Period <- factor(no2_period$Period, levels = c("Pre-COVID", "During COVID", "Post-COVID"))

no2_period$District <- factor(no2_period$District, 
                              levels = no2_period %>%
                                arrange(Location, District) %>%
                                pull(District) %>%
                                unique())

ggplot(no2_period, aes(x = District, y = Avg_NO2, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "District", 
       y = expression("NO"[2]~"("*mu*"g/m"^3*")"),
       fill = "COVID Period") +
  theme_minimal() +
  theme(legend.position = "top") + 
  scale_fill_manual(values = c("Pre-COVID" = "#1f78b4",   
                               "During COVID" = "#33a02c", 
                               "Post-COVID" = "#e31a1c"))  

# h3
# h3
# h3
reductions <- all_data %>%
  group_by(District, Period) %>%
  summarize(
    SO2_Reduction = max(`SO2 ( µg/m3 )`, na.rm = TRUE) - min(`SO2 ( µg/m3 )`, na.rm = TRUE),
    NO2_Reduction = max(`NO2 ( µg/m3 )`, na.rm = TRUE) - min(`NO2 ( µg/m3 )`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Period = factor(Period, levels = c("Pre-COVID", "During COVID", "Post-COVID"))) %>%
  mutate(Location = ifelse(
    District %in% anatolian_districts, "Anatolian", "European"
  )) %>%
  mutate(SO2_Reduction = SO2_Reduction * 10) %>% # scale up by 10
  pivot_longer(cols = c(SO2_Reduction, NO2_Reduction),
               names_to = "Metric",
               values_to = "Value")

reductions$District <- factor(reductions$District, 
                              levels = reductions %>%
                                arrange(Location, District) %>%
                                pull(District) %>%
                                unique())

ggplot(reductions, aes(x = Period, y = Value, group = interaction(District, Metric), color = Metric)) +
  geom_line(aes(linetype = Metric), size = 1.2) +  # Add linetype for dashed lines
  geom_point(size = 3) +
  scale_color_manual(
    values = c("SO2_Reduction" = "orange", "NO2_Reduction" = "skyblue"),
    labels = c(
      "SO2_Reduction" = expression(SO[2] ~ "* 10"),
      "NO2_Reduction" = expression(NO[2])
    )
  ) +
  scale_linetype_manual(values = c("SO2_Reduction" = "solid", "NO2_Reduction" = "dashed"), 
                        labels = c(
                          "SO2_Reduction" = expression(SO[2] ~ "* 10"),
                          "NO2_Reduction" = expression(NO[2])
                          )
                        ) +
  labs(
    x = "COVID Period",
    y = expression("Reduction (" ~ SO[2] ~ " * 10)"),
    color = "Metric",
    linetype = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  ) +
  facet_wrap(~ District, scales = "free_y")
