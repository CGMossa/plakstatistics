#' This startup-script is helpful to me. 
#' Only run this once. It should not cause issues, if ran over and over, but atleast to be on the safe side.
#' 

library(tidyverse)
library(magrittr)
library(broom)
library(glue)
# library(lubridate)


message('Setting plot theme to classic no. 2.')
theme_set(ggpubr::theme_classic2())

message('Plotting happens outside of RStudio.')
options(device = 'windows')

message('StringsAsFactors is set to `FALSE`.')
options(stringsAsFactors = FALSE)

table_to_slack <- function(tbl = .Last.value) {
  
  tbl %>%
    pander::pandoc.table.return(style = 'rmarkdown', split.tables = Inf, split.cells = Inf) %>%
    str_trim()          %T>% 
    clipr::write_clip() %>% 
    invisible()
  message('Copied to clipboard.')
}


last_to_screen <- function(tbl = .Last.value) 
  screen_display <<- tbl


# NOT IMPLEMENTED
# to_screen <- function(left = NULL, right = NULL) {
#   
#   
# }
