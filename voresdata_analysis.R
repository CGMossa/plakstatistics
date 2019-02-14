#'---
#'title: "Plakprocent analyse"
#'subtitle: Statistical analysis
#'urlcolor: blue
#'author: Mossa Merhi Reimert
#'output: html_document
#'---

#+ message=FALSE, warning = FALSE, include = FALSE
library(tidyverse)
library(lme4)

library(magrittr)

knitr::opts_chunk$set(tidy = 'styler')

knitr::opts_chunk$set(
  fig.path = 'figures/',
  fig.align = 'center', 
  fig.cap = 'Please caption every figure'
)

#separate cache into individual folders
cache_name <- tools::file_path_sans_ext(knitr::current_input())
knitr::opts_chunk$set(cache = TRUE, 
                      cache.path = paste0("cache/", cache_name, '/'),
                      fig.path = paste0('fig/', cache_name, '/'))

theme_set(ggpubr::theme_classic2())
#'
#'
#'
#'
#' This does not work, as the delimiter in the csv-file is a semicolon, and the default delimiter is assumed to be colon. 
# voresdata <- 
#   read_csv('voresdata.csv')
#' The following alternative approach seems to work. The time point is not written correctly, thus the readr-package assumes it to be `HOURS:MINUTES:SECONDS`, but it should be interpreted as `MINUTES:SECONDS`. This is ammended in the later code. 
#+ cache = TRUE
voresdata <- read_delim("voresdata.csv", 
                        delim = ";", 
                        locale = locale(decimal_mark=','),
                        col_types = cols(Tid = col_character()), 
                        trim_ws = TRUE)

#' This the time sequence is not written correctly, thus it needs to be modified. 
#' 
#' A quick overview:
glimpse(voresdata)

#' Correcting the time paramete, and converting it to seconds
voresdata_corrected <- voresdata %>% 
  mutate(Tid_med_timer = paste0('00:', Tid)) %>% 
  mutate(Tid_correct = Tid %>% lubridate::ms()) %>% 
  mutate(Tid_sekunder  = Tid_correct %>% 
           lubridate::period_to_seconds()) %>% 
  mutate(plak = plak / 100) %>% 
  mutate(ID = as.factor(ID)) 

#' Exporting imported and adjusted data. Run only once.
# voresdata_corrected %>% 
#   write_excel_csv2('voresdata_konferteret')
#'


#+ warning = FALSE
plot_of_plak_data <- 
  voresdata_corrected %>% {ggplot(.)  +
      
      aes(Tid_sekunder, plak, color = TP) + 
      
      geom_point() + 
      
      stat_smooth(aes(fill = as.factor(TP)),
                  # aes(Tid_sekunder, plak, fill = as.factor(TP)), 
                  # data = .,
                  se = FALSE,
                  show.legend = FALSE,
                  method = 'glm',
                  alpha = .2,
                  size = .5,
                  # formula = "y ~ fill + (x|fill)",
                  # inherit.aes = FALSE,
                  method.args = list(family = 'binomial'),
                  fullrange = FALSE) + 
      labs(y = 'Plak i procent. ', 
           x = 'Time [sec]') + 
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
      theme(legend.position = 'right', aspect.ratio = 1)
  }
plot_of_plak_data


plot_of_plak_data + 
  aes(color = ID) + 
  labs(color = 'Participant ID') + 
  theme(legend.position = 'none')

plak_behandling_plot <- voresdata_corrected %>% 
  select(ID, plak, Tid_sekunder, TP) %>% { ggplot(.) +
      
      aes(y = plak, Tid_sekunder, color = TP) + 
      
      ggrepel::geom_text_repel(
        aes(label = TP,
            color = NULL),
        alpha = .2,
        show.legend = FALSE,

      ) +
      
      geom_point(aes(color = ID),
                 show.legend = FALSE) + 
      geom_path(aes(group = ID, color = ID), show.legend = FALSE) + 
      
      # scale_x_log10() +
      # scale_y_continuous(labels = scales::percent,
      #                    trans = scales::logit_trans()) +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
      
      labs(y = 'Plak i procent. ', 
           x = 'Time [sec]',
           label = 'Treatment') + 
      
      theme(legend.position = 'right', 
            # aspect.ratio = 1
            ) +
      NULL
  }
plak_behandling_plot 

#+ fig.cap = 'The y-axis is transformed according to the logit-map, to clearly annotate the differences. '
plak_behandling_plot +
  scale_y_continuous(labels = scales::percent, limits = c(NA, .75),
                     trans = scales::logit_trans())

#'
#'
#'
#'

glmer_plak <- glmer(
  plak ~ TP + Tid_sekunder + (Tid_sekunder|ID), 
  data = voresdata_corrected,
  family = binomial
)

