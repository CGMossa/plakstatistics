

library(tidyverse)


#' This does not work, as the delimiter in the csv-file is a semicolon, and the default delimiter is assumed to be colon. 
# voresdata <- 
#   read_csv('voresdata.csv')
#' The following alternative approach seems to work. The time point is not written correctly, thus the readr-package assumes it to be `HOURS:MINUTES:SECONDS`, but it should be interpreted as `MINUTES:SECONDS`. This is ammended in the later code. 
#' 
voresdata <- read_delim("voresdata.csv", 
                        ";", 
                        col_types = cols(Tid = col_character()), 
                        trim_ws = TRUE)

#' This the time sequence is not written correctly, thus it needs to be modified. 
#' 
#' A quick overview:
glimpse(voresdata)

#' Correcting the time paramete, and converting it to seconds
voresdata_corrected <- voresdata %>% 
  mutate(Tid_med_timer = paste0('00:', Tid)) %>% 
  mutate(Tid_correct = Tid %>% luridate::ms()) %>% 
  mutate(Tid_sekunder  = Tid_correct %>% 
           lubridate::period_to_seconds())

voresdata_corrected
