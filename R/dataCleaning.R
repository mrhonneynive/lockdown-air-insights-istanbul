library(dplyr)
library(tidyr)
library(readxl)

uskudar <- read_excel("data/airQuality/anatolian/uskudar.xlsx", skip = 1)
kartal <- read_excel("data/airQuality/anatolian/kartal.xlsx", skip = 1)
umraniye <- read_excel("data/airQuality/anatolian/umraniye.xlsx", skip = 1)
silivri <- read_excel("data/airQuality/european/silivrimthm.xlsx", skip = 1)
arnavutkoy <- read_excel("data/airQuality/european/arnavutkoy.xlsx", skip = 1)
sultangazi <- read_excel("data/airQuality/european/sultangazimthm.xlsx", skip = 1)

uskudar <- uskudar %>%
  filter(!apply(uskudar, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

kartal <- kartal %>%
  filter(!apply(kartal, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

umraniye <- umraniye %>%
  filter(!apply(umraniye, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

silivri <- silivri %>%
  filter(!apply(silivri, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

arnavutkoy <- arnavutkoy %>%
  filter(!apply(arnavutkoy, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

sultangazi <- sultangazi %>%
  filter(!apply(sultangazi, 1, function(row) any(row == "-"))) %>%
  mutate(across((ncol(.)-3):ncol(.), ~ as.numeric(gsub(",", ".", gsub("[^0-9.,-]", "", .)))))

# remove outliers using IQR method
remove_outliers_iqr <- function(data) {
  data_clean <- data %>%
    mutate(across(where(is.numeric), ~ {
      q1 <- quantile(.x, 0.25, na.rm = TRUE)
      q3 <- quantile(.x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- max(0, q1 - 1.5 * iqr)
      upper_bound <- q3 + 1.5 * iqr 
      ifelse(.x >= lower_bound & .x <= upper_bound, .x, NA)
    })) %>%
    filter(!if_any(where(is.numeric), is.na))  
  
  return(data_clean)
}

# remove outliers from each dataset
uskudar <- remove_outliers_iqr(uskudar)
kartal <- remove_outliers_iqr(kartal)
umraniye <- remove_outliers_iqr(umraniye)
silivri <- remove_outliers_iqr(silivri)
arnavutkoy <- remove_outliers_iqr(arnavutkoy)
sultangazi <- remove_outliers_iqr(sultangazi)