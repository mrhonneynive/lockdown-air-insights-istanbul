library(dplyr)
library(tidyr)
library(readxl)

uskudar <- read_excel("data/airQuality/anatolian/uskudar.xlsx", skip = 1)
kartal <- read_excel("data/airQuality/anatolian/kartal.xlsx", skip = 1)
umraniye <- read_excel("data/airQuality/anatolian/umraniye.xlsx", skip = 1)
silivri <- read_excel("data/airQuality/european/silivrimthm.xlsx", skip = 1)
arnavutkoy <- read_excel("data/airQuality/european/arnavutkoy.xlsx", skip = 1)
sultangazi <- read_excel("data/airQuality/european/sultangazimthm.xlsx", skip = 1)

