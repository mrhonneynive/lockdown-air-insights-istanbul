library(dplyr)
library(moments) # for skewness and kurtosis

calculate_stats <- function(data, column_name) {
  col <- data[[column_name]]
  
  n <- length(col)                                   
  mean_value <- mean(col)                            
  sd_value <- sd(col)                                
  median_value <- median(col)                        
  trimmed_mean <- mean(col, trim = 0.1)              
  mad_value <- mad(col)                              
  min_value <- min(col)                              
  max_value <- max(col)                              
  first_quartile <- quantile(col, 0.25)              
  third_quartile <- quantile(col, 0.75)              
  skewness_value <- skewness(col)                    
  kurtosis_value <- kurtosis(col)                    
  se_value <- sd(col) / sqrt(n)                      
  
  results <- data.frame(
    Metric = c("n", "Mean", "SD", "Median", "Trimmed Mean", "MAD", "Min", "Max", "1st Q", "3rd Q", "Skewness", "Kurtosis", "SE"),
    Value = c(round(n, 0),
              round(mean_value, 2),
              round(sd_value, 2),
              round(median_value, 2),
              round(trimmed_mean, 2),
              round(mad_value, 2),
              round(min_value, 2),
              round(max_value, 2),
              round(first_quartile, 2),
              round(third_quartile, 2),
              round(skewness_value, 2),
              round(kurtosis_value, 2),
              round(se_value, 2))
  )
  
  
  return(results)
}

# calculate descriptive statistics for each district
kartal_pm10 <- calculate_stats(kartal, "PM10 ( µg/m3 )")
uskudar_pm10 <- calculate_stats(uskudar, "PM10 ( µg/m3 )")
umraniye_pm10 <- calculate_stats(umraniye, "PM10 ( µg/m3 )")
silivri_pm10 <- calculate_stats(silivri, "PM10 ( µg/m3 )")
arnavutkoy_pm10 <- calculate_stats(arnavutkoy, "PM10 ( µg/m3 )")
sultangazi_pm10 <- calculate_stats(sultangazi, "PM10 ( µg/m3 )")

kartal_pm25 <- calculate_stats(kartal, "PM 2.5 ( µg/m3 )")
uskudar_pm25 <- calculate_stats(uskudar, "PM 2.5 ( µg/m3 )")
umraniye_pm25 <- calculate_stats(umraniye, "PM 2.5 ( µg/m3 )")
silivri_pm25 <- calculate_stats(silivri, "PM 2.5 ( µg/m3 )")
arnavutkoy_pm25 <- calculate_stats(arnavutkoy, "PM 2.5 ( µg/m3 )")
sultangazi_pm25 <- calculate_stats(sultangazi, "PM 2.5 ( µg/m3 )")

kartal_so2 <- calculate_stats(kartal, "SO2 ( µg/m3 )")
uskudar_so2 <- calculate_stats(uskudar, "SO2 ( µg/m3 )")
umraniye_so2 <- calculate_stats(umraniye, "SO2 ( µg/m3 )")
silivri_so2 <- calculate_stats(silivri, "SO2 ( µg/m3 )")
arnavutkoy_so2 <- calculate_stats(arnavutkoy, "SO2 ( µg/m3 )")
sultangazi_so2 <- calculate_stats(sultangazi, "SO2 ( µg/m3 )")

kartal_no2 <- calculate_stats(kartal, "NO2 ( µg/m3 )")
uskudar_no2 <- calculate_stats(uskudar, "NO2 ( µg/m3 )")
umraniye_no2 <- calculate_stats(umraniye, "NO2 ( µg/m3 )")
silivri_no2 <- calculate_stats(silivri, "NO2 ( µg/m3 )")
arnavutkoy_no2 <- calculate_stats(arnavutkoy, "NO2 ( µg/m3 )")
sultangazi_no2 <- calculate_stats(sultangazi, "NO2 ( µg/m3 )")

kartal_elec <- calculate_stats(anatolian_electricity, "KARTAL")
uskudar_elec <- calculate_stats(anatolian_electricity, "ÜSKÜDAR")
umraniye_elec <- calculate_stats(anatolian_electricity, "ÜMRANİYE")
arnavutkoy_elec <- calculate_stats(european_electricity, "ARNAVUTKÖY")
silivri_elec <- calculate_stats(european_electricity, "SİLİVRİ")
sultangazi_elec <- calculate_stats(european_electricity, "SULTANGAZİ")