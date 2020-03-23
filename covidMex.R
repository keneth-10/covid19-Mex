#devtools::install_github("pablorm296/covidMex")
library(covidMex)
library(tidyverse)

#confirmados <- getData(type = 'confirmed', source = 'Serendipia')
#confirmados <- getData(type = 'confirmed', date = "20/03/2020")
confirmados <- getData(type = 'confirmed')

confirmados %>%
  mutate(GrupoEdad = cut(edad, breaks = c(seq(0, 80, by = 10), Inf))) %>%
  ggplot() +
  geom_bar(aes(x = GrupoEdad, y = ..count..), colour = "#CC7390", 
           fill = "#CC7390", alpha = 0.5, na.rm = T) +
  labs(x = "Grupo de Edad", y = "Casos",
       title = "¿Qué edad tienen los infectados de SARS-CoV-2 en México?") +
  theme_light() + 
  theme(text = element_text(family = "Calibri"),
        title = element_text(family = "Arial")) +
  ggsave('visualizations/edadConfirmados.png')

####################################################
# Casos por entidad federativa
####################################################
entidad <- confirmados %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

#install.packages('png')

library(grid)
library(gridExtra)
library(png)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

img <- readPNG('img/CARhE.png')

img <- get_png("img/CARhE.png")

ggplot(entidad, aes(x = ent, y = count)) +
  geom_bar(stat = 'identity', fill = '#73C6B6', alpha = 0.8, color = 'grey40') +
  geom_text(aes(label = count), vjust = 0.5, hjust = -0.1) +
  labs(x = 'Entidad Federativa', y = 'Casos',
       title = '¿Cuáles son los estados con mayor número de casos confirmados?',
       subtitle = 'Al 22 de Marzo de 2020',
       caption = 'Computational Analytic Resources for Health (CARhE)') +
  theme(title = element_text(family = "Arial")) +
  theme_light() +
  coord_flip() +
  ggsave('visualizations/edosCasos.png', width = 750, height = 518, units = 'mm')
  
  
  #annotation_custom(rasterGrob(img), 
  #                  xmin=-3, xmax=0, 
  #                  ymin=0.62*min(entidad$count)-0.5, ymax=0.62*min(entidad$count)+0.5)
  

  #gt <- ggplot_gtable(ggplot_build(ent))
  #gt$layout$clip[gt$layout$name=="panel"] <- "off"
  #grid.draw(gt)


###### Procedencia de casos confirmados!
procedencia <- confirmados %>%
  group_by(procedencia) %>%
  summarise(count = n()) %>%
  as.data.frame()

ggplot(procedencia, aes(x = procedencia, y = count)) +
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.6, color = 'gray30') +
  geom_text(aes(label = count), vjust = -0.3) +
  labs(title = 'Procedencia de Casos Confirmados de COVID-19',
       x= '', y = 'Conteo', subtitle = 'Información al 21 de Marzo de 2020',
       caption = 'Computational Analytic Resources for Health (CARhE)') +
  ggsave('visualizations/procedenciaCasos.png')


######################################################
# Time Series of Cases for Mexico
######################################################
library(readr)
library(tidyr)
library(lubridate)

dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
dat <- dat[c(-1,-3,-4)]


mexico <- dat %>%
  filter(dat$`Country/Region` == 'Mexico') %>%
  gather() %>%
  as.data.frame()

mexico <- mexico[-1,]

str(mexico)

mexico$key <-  mdy(mexico$key)
mexico$value <- as.integer(mexico$value)

#str(mexico$key)
#str(as.Date('2020-03-21'))

mexico$value[60]

ggplot(mexico, aes(x = key, y = value)) +
  geom_line(color = 'steelblue') +
  geom_text(label = mexico$value, size = 3, position = position_nudge( x = 2, y = 1)) + # position_stack(vjust = 1.05)) +
  scale_x_date(date_minor_breaks = "5 day") +
  scale_x_date(date_breaks = "2 day", date_labels = "%D") +
  theme(axis.text.x=element_text(angle=80, hjust=1)) +
  labs(title = 'Casos de COVID-19 Positivos en México por Día', x = '', y = 'Conteo', subtitle = 'Al Viernes 20 de Marzo de 2020') +
  #stat_smooth(
  #  color = "#FC4E07", fill = "#FC4E07",
  #  method = "loess"
  #) +
  ggsave('visualizations/tsMexico.png')


##################################################################################
# Time Series of Cases for Canada, USA, Mexico, and selected Sudamerican Countries
####################################################################################

americas <- dat %>%
  filter(dat$`Country/Region` %in% c('Chile', 'Mexico', 'Uruguay', 'Brazil','Costa Rica', 'Jamaica','Argentina','Peru')) %>%
  gather(Country, Cases, `1/22/20`:`3/21/20`) %>%
  as.data.frame()

names(americas) <- c('Country', 'Date', 'Cases')
americas$Date <- mdy(americas$Date)

head(americas)


library(ggrepel)
ggplot(americas, aes(x = Date, y = Cases)) +
  geom_line(aes(color = Country), size = 1) +
  geom_label_repel(aes(Date, label = Cases),
                   data = tail(americas, 8),                 
                   box.padding = unit(0.45, "lines"),
                   point.padding = unit(0.4, "lines"),
                   segment.color = 'grey50',
                   show.legend = FALSE) +
  #scale_x_date(date_minor_breaks = "5 day") +
  #scale_x_date(date_breaks = "2 day", date_labels = "%D") +
  theme(axis.text.x=element_text(angle=80, hjust=1)) +
  #geom_text(label = americas$Cases, size = 2, position = position_nudge( x = 2, y = 1)) +
  theme_minimal() +
  labs(title = 'Confirmed Cases of SARS-COV-2 (COVID-19) in Latin-America',
       subtitle = 'Up to March 20th 2020',
       caption = 'Using data from the John Hopkins Mapping nCov repository. \n By Computational Analytic Resources for Health') +
  ggsave('visualizations/tsAmericas.png')


###################################################################
###################################################################

ibero <- dat %>%
  filter(dat$`Country/Region` %in% c('Chile', 'Mexico','Brazil', 'Spain', 'Italy', 'Korea, South')) %>%
  gather(Country, Cases, `1/22/20`:`3/21/20`) %>%
  as.data.frame()

names(ibero) <- c('Country', 'Date', 'Cases')
ibero$Date <- mdy(ibero$Date)

head(ibero)

table(ibero$Country)
library(ggrepel)
ggplot(ibero, aes(x = Date, y = Cases)) +
  geom_line(aes(color = Country), size = 1) +
  geom_label_repel(aes(Date, label = Cases),
                   data = tail(ibero, 6),                 
                   box.padding = unit(0.45, "lines"),
                   point.padding = unit(0.4, "lines"),
                   segment.color = 'grey50',
                   show.legend = FALSE) +
  #scale_x_date(date_minor_breaks = "5 day") +
  #scale_x_date(date_breaks = "2 day", date_labels = "%D") +
  theme(axis.text.x=element_text(angle=80, hjust=1)) +
  #geom_text(label = americas$Cases, size = 2, position = position_nudge( x = 2, y = 1)) +
  theme_minimal() +
  labs(title = 'Confirmed Cases of SARS-COV-2 (COVID-19)',
       subtitle = 'Up to March 20th 2020',
       caption = 'Using data from the John Hopkins Mapping nCov repository. \n By Computational Analytic Resources for Health (CARhE).') +
  ggsave('visualizations/tsWorld.png')


