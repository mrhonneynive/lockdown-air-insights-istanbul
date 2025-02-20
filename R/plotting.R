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