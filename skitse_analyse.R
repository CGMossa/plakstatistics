


library(nlme)


source('voresdata_analysis.R', echo = FALSE)

model_plak <- lme(plak ~ factor(TP) + Tid_sekunder,
                  random = ~ 1 | ID,
                  data = voresdata_corrected)

model_log_plak <-
  lme(log(plak) ~ factor(TP) + Tid_sekunder,
    random = ~ 1 | ID,
    data = voresdata_corrected
  )

model_logit_plak <-
  lme(gtools::logit(plak) ~ factor(TP) + Tid_sekunder,
    random = ~ 1 | ID,
    data = voresdata_corrected
  )



# gtools::inv.logit()

model_logit_plak %>%
  summary

plot(model_plak)
plot(model_log_plak)
plot(model_logit_plak)

plot.lme(model_logit_plak, Tid_sekunder ~ fitted(.), abline = c(0,1))
         