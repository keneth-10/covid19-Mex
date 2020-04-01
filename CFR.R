###########################################################################
# This script is an effort to reproduce the approach showed on:
# https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html
###########################################################################
casosCovidMundo <- covidWWSituation()

names(casosCovidMundo)

covidDataMex <- casosCovidMundo %>%
  arrange(pais_territorio, fecha_corte) %>%
  filter(pais_territorio == 'Mexico') %>%
  select(fecha_corte, casos_nuevos, decesos) %>%
  as.data.frame()

cases <- sum(covidDataMex$casos_nuevos)
deaths <- sum(covidDataMex$decesos)
  
CFR <- round((deaths/cases)*100, 2)

round((1.38/CFR)*100,1)