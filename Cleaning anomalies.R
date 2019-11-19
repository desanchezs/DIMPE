library(tidyverse)  # Core data manipulation and visualization libraries
library(tidyquant)  # Used for business-ready ggplot themes
library(anomalize)  # Identify and clean time series anomalies
library(timetk)     # Time Series Machine Learning Features
library(knitr)      # For kable() function
library(dplyr)

tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.3)


lubridate_tbl <- tidyverse_cran_downloads %>%
  ungroup() %>%
  filter(package == "lubridate")
lubridate_anomalized_tbl <- lubridate_tbl %>%
  # 1. Decompose download counts and anomalize the STL decomposition remainder
time_decompose(count) %>%
  # 2. Fix negative values if any in observed
mutate(observed = ifelse(observed < 0, 0, observed)) %>%
  # 3. Identify anomalies
anomalize(remainder) %>%
  # 4. Clean & repair anomalous data
clean_anomalies()


lubridate_anomalized_tbl %>% 
  filter(anomaly == "Yes") %>%
  select(date, anomaly, observed, observed_cleaned) %>%
  head() %>% 
  kable()

lubridate_forecast_with_anomalies_tbl <- lubridate_anomalized_tbl %>%
forecast_downloads(col_train = observed_cleaned, col_test = observed, sep= "2018-01-01",trans= "sqrt")

