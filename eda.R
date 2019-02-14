#' ---
#' title: "Analyse af data fra Mehdi, Thang, og andre. "
#' author: "Mossa M. Reimert "
#' date: \today
#' ---

library(tidyverse)

theme_set( ggpubr::theme_classic2() ) 

library(readxl)
library(janitor)

# library(tsibble)

library(lubridate)
# library(anytime)

# readxl::read_xlsx('clean_Book1.xlsx', range = cell_rows(3:32))
plak_data_long <- 
  readxl::read_xlsx('clean_Book1.xlsx',
                    col_types = c(
                      'text', 'numeric', 'numeric', 'skip', 'numeric', 'date', 'skip', 'skip',
                      'text', 'numeric', 'numeric', 'skip', 'numeric', 'date'
                    ),
                    range = cellranger::as.cell_limits('A3:N32')) %>% 
  janitor::clean_names() %>%
  
  mutate(brugte_min_dag_1 = ms(paste0(hour(brugte_min_dag_1),':',minute(brugte_min_dag_1)))) %>%
  mutate(brugte_min_dag_2 = ms(paste0(hour(brugte_min_dag_2),':',minute(brugte_min_dag_2)))) %>%

  mutate(deltager = if_else(deltager_1 == deltager_6, deltager_1, NA_character_)) %>% 
  select(-deltager_1, -deltager_6) %>% 
  
  rename(percent_plak_dag2 = percent_plak_dag_2) %>% 
  
  mutate_at(vars(contains('percent')), ~ ./100) %>% 
  
  rename(
    antal_flader_med_plak_dag1 = antal_flader_med_plak_3,
    antal_flader_med_plak_dag2 = antal_flader_med_plak_8,
    antal_taender_dag1 = antal_taender_2,
    antal_taender_dag2 = antal_taender_7,
    brugte_min_dag1 = brugte_min_dag_1,
    brugte_min_dag2 = brugte_min_dag_2
  ) %>% 
  
  mutate(brugte_min_dag1 = brugte_min_dag1 %>% period_to_seconds(),
         brugte_min_dag2 = brugte_min_dag2 %>% period_to_seconds()) %>% 
  
  
  gather(key, value, -deltager) %>% 
  
  separate(key, c('key', 'dag'), convert = TRUE, sep = '_dag') %>%
  
  select(names(.) %>% sort(.)) %>% 
  force()

plak_data_wide <- 
  plak_data_long %>%
  spread(key, value) %>%
  
  mutate(antal_flader = antal_taender * 6) %>% 
  mutate(plak_by_r = antal_flader_med_plak / (antal_taender * 6)) %>% 
  
  arrange(deltager) %>% 
  force()
plak_data_wide

#' Der er et `subjekt`-kolonne. Hvorfor?
#' Alle tider er registreret forkert. Minutter er registreret som timer, og timer er registreret som minutter. 


library(finalfit)

library(GGally)


plak_data_long %>% 
  ggplot(aes(factor(dag), group = key, value)) +
  
  geom_point() +
  
  facet_wrap(~key, scales = 'free')

plak_data_long %>% 
  ggplot(aes(value)) +
  
  aes(group = dag %>% as.factor, fill = dag %>% as.factor) +
  
  ggridges::geom_density_line(alpha = .2, show.legend = FALSE) +
  
  facet_wrap(~key, scales = 'free') +
  
  NULL


plak_data_long %>% 
  ggplot(aes(dag, value)) +
  
  aes(group = dag %>% as.factor, fill = dag %>% as.factor) +
  
  geom_violin(alpha = .2, show.legend = FALSE) +
  
  facet_wrap(~key, scales = 'free') +
  
  NULL


plak_data_long %>% 
  ggplot(aes(value)) +
  
  aes(group = dag %>% as.factor, fill = dag %>% as.factor) +
  
  geom_histogram(alpha = .4, show.legend = FALSE) +
  
  facet_wrap(~key, scales = 'free') +
  
  NULL


plak_data_long %>% 
  ggplot(aes(dag, value)) +
  
  aes(group = dag %>% as.factor, fill = dag %>% as.factor) +
  
  geom_point(alpha = .2, show.legend = FALSE) +
  
  facet_wrap(~key, scales = 'free') + 
  
  geom_smooth(method ='lm', show.legend = FALSE) +
  
  NULL
#' Hvorfor brugte deltagerne længere tid på børstning anden dag?

plak_data_wide_diff <- plak_data_wide %>% 
  group_by(deltager) %>% 
  arrange(-dag) %>% 
  summarise_all(list(diff = diff)) %>%
  
  force()

plak_data_wide_diff %>% 
  ggplot(aes(brugte_min_diff, percent_plak_diff)) +
  
  geom_point(show.legend = FALSE)+
  
  geom_smooth(method = 'glm', 
              method.args = list(family = binomial),  
              show.legend = FALSE)

library(infer)

lme4::glmer(percent_plak ~ . - deltager - antal_taender + (1|deltager), family = binomial(link = 'logit'), 
            data = plak_data_wide) %>% 
  # broom::tidy()
  summary()

# sanity check: 
# plak_data_wide_diff$antal_taender_diff %>% sum


GGally::ggcorr(plak_data_wide)

# 
# glm(cbind(antal_flader_med_plak, antal_taender) ~ 1 + brugte_min, 
#     family = binomial,
#     data = plak_data_wide)
#     
#     
library(nlme)
glmer_plak_data <- plak_data_wide %>% 
  mutate(deltager = as.factor(deltager), 
         dag = as.factor(dag))


lme4::glmer(antal_flader_med_plak/antal_flader ~ 0 + brugte_min + dag + (1|deltager),
# lme4::glmer(antal_flader_med_plak/antal_flader ~ 1 +  (1|deltager),
# lme4::glmer(antal_flader_med_plak/antal_flader ~ 1 + dag * brugte_min + (brugte_min|deltager),
            # lme4::glmer(antal_flader_med_plak/antal_flader ~  dag * brugte_min + (1|deltager),
            # lme4::glmer(antal_flader_med_plak/antal_flader ~ brugte_min*dag + (brugte_min||deltager),
            family = binomial,
            # control = glmerControl(optimizer="bobyqa"),
            data = glmer_plak_data) %T>% 
  broom::tidy() %>%
  # {plot(.) %>% print(.)} %>%
  # {plot(., fitted(.) ~ antal_flader_med_plak/antal_flader) %>% print} %>%
  print() %>% 
  force

