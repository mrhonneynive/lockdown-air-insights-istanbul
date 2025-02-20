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