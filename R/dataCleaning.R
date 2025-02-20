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

anatolian_base_path <- "data/electricityUsage/ayedas"
file_paths <- c(
  "2019-01.xlsx", "2019-02.xlsx", "2019-03.xlsx", "2019-04.xlsx", "2019-05.xlsx",
  "2019-08.xlsx", "2019-09.xlsx", "2019-10.xlsx", "2019-11.xlsx", "2019-12.xlsx",
  
  "2020-01.xlsx", "2020-02.xlsx", "2020-03.xlsx", "2020-04.xlsx", "2020-05.xlsx",
  "2020-06.xlsx", "2020-07.xlsx", "2020-08.xlsx", "2020-09.xlsx", "2020-10.xlsx",
  "2020-11.xlsx", "2020-12.xlsx",
  
  "2021-01.xlsx", "2021-02.xlsx", "2021-03.xlsx", "2021-04.xlsx", "2021-05.xlsx",
  "2021-06.xlsx", "2021-07.xlsx", "2021-08.xlsx", "2021-09.xlsx", "2021-10.xlsx",
  "2021-11.xlsx", "2021-12.xlsx"
)
anatolian_districts <- c("KARTAL", "ÜSKÜDAR", "ÜMRANİYE")
anatolian_electricity <- data.frame()
for (file_name in file_paths) {
  file_path <- file.path(anatolian_base_path, file_name)
  data <- read_excel(file_path, skip = 1)
  
  month <- gsub(".xlsx", "", file_name)
  
  data <- data %>%
    filter(`Belediye/İl Özel İdaresi Adı` %in% anatolian_districts) %>%  
    select(`Belediye/İl Özel İdaresi Adı`, last_col()) %>%  
    mutate(month = month)  
  
  anatolian_electricity <- bind_rows(anatolian_electricity, data)
}
anatolian_electricity <- anatolian_electricity %>%
  pivot_wider(
    names_from = `Belediye/İl Özel İdaresi Adı`,  
    values_from = `Toplam Tüketim Miktarı (kWh)`  
  )

european_base_path <- "/home/hasan/Documents/CTIS365/termProject/data/electricityUsage/bedas"
file_paths <- c(
  "2019-01.xlsx", "2019-02.xlsx", "2019-03.xlsx", "2019-04.xlsx", "2019-05.xlsx",
  "2019-06.xlsx", "2019-07.xlsx", "2019-08.xlsx", "2019-09.xlsx", "2019-10.xlsx",
  "2019-11.xlsx", "2019-12.xlsx",
  
  "2020-01.xlsx", "2020-02.xlsx", "2020-03.xlsx", "2020-04.xlsx", "2020-05.xlsx",
  "2020-06.xlsx", "2020-07.xlsx", "2020-08.xlsx", "2020-09.xlsx", "2020-10.xlsx",
  "2020-11.xlsx", "2020-12.xlsx",
  
  "2021-01.xlsx", "2021-02.xlsx", "2021-03.xlsx", "2021-04.xlsx", "2021-05.xlsx",
  "2021-06.xlsx", "2021-07.xlsx", "2021-08.xlsx", "2021-09.xlsx", "2021-10.xlsx",
  "2021-11.xlsx", "2021-12.xlsx"
)
european_districts <- c("ARNAVUTKÖY", "SİLİVRİ", "SULTANGAZİ")
european_electricity <- data.frame()

for (file_name in file_paths) {
  file_path <- file.path(european_base_path, file_name)
  
  data <- read_excel(file_path, skip = 1)
  
  month <- gsub(".xlsx", "", file_name)
  
  
  data <- data %>%
    filter(`Belediye/İl Özel İdaresi Adı` %in% european_districts) %>%  
    select(`Belediye/İl Özel İdaresi Adı`, last_col()) %>%  
    mutate(month = month)  
  
  european_electricity <- bind_rows(european_electricity, data)
}

european_electricity <- european_electricity %>%
  pivot_wider(
    names_from = `Belediye/İl Özel İdaresi Adı`,  
    values_from = `Toplam Tüketim (kWh)`  
  )


remove_outliers_iqr <- function(df, cols) {
  df_clean <- df %>%
    mutate(across(all_of(cols), ~ {
      q1 <- quantile(.x, 0.25, na.rm = T)
      q3 <- quantile(.x, 0.75, na.rm = T)
      iqr <- q3 - q1
      lower_bound <- max(0, q1 - 1.5 * iqr)
      upper_bound <- min(max(.x, na.rm = T), q3 + 1.5 * iqr)
      
      cat("Column:", deparse(substitute(.x)), "\n")
      cat("Q1:", q1, "Q3:", q3, "\n")
      cat("Lower Bound:", lower_bound, "Upper Bound:", upper_bound, "\n")
      
      # Replace outliers with NA
      ifelse(.x >= lower_bound & .x <= upper_bound, .x, NA)
    })) %>%
    filter(if_all(all_of(cols), ~ !is.na(.))) 
  return(df_clean)
}

# apply outlier removal
anatolian_electricity <- remove_outliers_iqr(anatolian_electricity, anatolian_districts)
european_electricity <- remove_outliers_iqr(european_electricity, european_districts)
