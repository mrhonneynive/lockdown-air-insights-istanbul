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
