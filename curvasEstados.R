#####################################################################
# This script generates a time series for each state of Mexico
#####################################################################

library(covidMex)
library(tidyverse)

#https://github.com/pablorm296/covidMex/
sospechosos <- getData(type = "suspect", source = "Serendipia")

confirmados16 <- getData(type = 'confirmed',  date = "16/03/2020")
confirmados17 <- getData(type = 'confirmed',  date = "17/03/2020")
confirmados18 <- getData(type = 'confirmed',  date = "18/03/2020")
confirmados19 <- getData(type = 'confirmed',  date = "19/03/2020")
confirmados20 <- getData(type = 'confirmed',  date = "20/03/2020")
confirmados21 <- getData(type = 'confirmed',  date = "21/03/2020")
confirmados22 <- getData(type = 'confirmed',  date = "22/03/2020")
confirmados23 <- getData(type = 'confirmed',  date = "23/03/2020")
confirmados24 <- getData(type = 'confirmed',  date = "24/03/2020")
confirmados25 <- getData(type = 'confirmed',  date = "25/03/2020")
confirmados26 <- getData(type = 'confirmed',  date = "26/03/2020")


entidad16 <- confirmados16 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad17 <- confirmados17 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad18 <- confirmados18 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad19 <- confirmados19 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad20 <- confirmados20 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad21 <- confirmados21 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad22 <- confirmados22 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad23 <- confirmados23 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

entidad24 <- confirmados24 %>%
  group_by(ent) %>%
  summarize(count = n()) %>%
  as.data.frame()

r = 28
newrow = c('TLAXCALA',0)
entidad23 <-rbind(entidad23[1:r,], newrow, entidad23[-(1:r),])
entidad23$count <- as.numeric(entidad23$count)

############################################################################
#Updating values in datasets from previous days adding names of ALL states
############################################################################

guia <- as.data.frame(entidad23$ent)
guia$conteo <- rep(0,32)
guia$`entidad23$ent`<- as.character(guia$`entidad23$ent`)
guia$conteo <- as.integer(guia$conteo)
names(guia) <- c('ent', 'count')

#guia[!guia$ent %in% entidad16$ent,]
#entidad16
entidad16 <- rbind(entidad16,guia[!guia$ent %in% entidad16$ent,])
entidad16 <- entidad16[order(entidad16$ent),]

#entidad17
entidad17 <- rbind(entidad17,guia[!guia$ent %in% entidad17$ent,])
entidad17 <- entidad17[order(entidad17$ent),]

#entidad18
entidad18 <- rbind(entidad18,guia[!guia$ent %in% entidad18$ent,])
entidad18 <- entidad18[order(entidad18$ent),]

#entidad19
entidad19 <- rbind(entidad19,guia[!guia$ent %in% entidad19$ent,])
entidad19 <- entidad19[order(entidad19$ent),]

#entidad20
entidad20 <- rbind(entidad20,guia[!guia$ent %in% entidad20$ent,])
entidad20 <- entidad20[order(entidad20$ent),]

#entidad21
entidad21 <- rbind(entidad21,guia[!guia$ent %in% entidad21$ent,])
entidad21 <- entidad21[order(entidad21$ent),]

#entidad22
entidad22 <- rbind(entidad22,guia[!guia$ent %in% entidad22$ent,])
entidad22 <- entidad22[order(entidad22$ent),]

#entidad23
entidad23 <- rbind(entidad23,guia[!guia$ent %in% entidad23$ent,])
entidad23 <- entidad23[order(entidad23$ent),]

#entidad24
entidad24 <- rbind(entidad24,guia[!guia$ent %in% entidad24$ent,])
entidad24 <- entidad24[order(entidad24$ent),]

entidad16$diezSiete <- entidad17$count
entidad16$diezOcho <- entidad18$count
entidad16$diezNueve <- entidad19$count
entidad16$veinte <- entidad20$count
entidad16$veinteUno <- entidad21$count
entidad16$veinteDos <- entidad22$count
entidad16$veinteTres <- entidad23$count
entidad16$veinteCuatro <- entidad24$count

entidades <- entidad16


entidades <- reshape(entidades, direction = 'long',
        varying = list(names(entidades)[2:9]),
        v.names = 'conteo',
        idvar = 'id',
        timevar = 'day')

#names(entidades)
entidades$day[entidades$day==1] <- as.character(dmy('16/03/20'))
entidades$day[entidades$day=='2'] <- as.character(dmy('17/03/20'))
entidades$day[entidades$day=='3'] <- as.character(dmy('18/03/20'))
entidades$day[entidades$day=='4'] <- as.character(dmy('19/03/20'))
entidades$day[entidades$day=='5'] <- as.character(dmy('20/03/20'))
entidades$day[entidades$day=='6'] <- as.character(dmy('21/03/20'))
entidades$day[entidades$day=='7'] <- as.character(dmy('22/03/20'))
entidades$day[entidades$day=='8'] <- as.character(dmy('23/03/20'))
entidades$day[entidades$day=='9'] <- as.character(dmy('24/03/20'))

entidades <- entidades[c(4,1,2,3)]
entidades$day <- as.Date(ymd(entidades$day))

## Visualizing
library(ggrepel)
ggplot(entidades,aes(x=day,y=conteo,colour=ent,group=ent)) +
  geom_line() +
  labs(title = 'Serie de Tiempo de Casos Positivos de COVID-19 por Estado en México',
       x = 'Día', y = 'Conteo',caption = 'Datos obtenidos de los comunicados técnicos diarios por Secretaría de Salud-México') +
  theme_minimal() +
  geom_label_repel(aes(day, label = conteo),
                   data = tail(entidades, 32),                 
                   box.padding = unit(0.25, "lines"),
                   point.padding = unit(0.4, "lines"),
                   segment.color = 'grey50',
                   show.legend = FALSE)