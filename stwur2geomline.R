
load("~/R/Diagnoza/data/osoby.rda")
load("~/R/Diagnoza/data/gospodarstwa.rda")
load("~/R/Diagnoza/data/osobyDict.rda")
load("~/R/Diagnoza/data/gospodarstwaDict.rda")

library(ggplot2)
library(dplyr)
library(haven)


### parts of data year by year
temp = osoby %>%
  select(ap83_1, wojewodztwo, podregion58, eduk4_2000, waga_2000_ind) %>%
  mutate_each(funs(as_factor), -waga_2000_ind) %>% 
  group_by(ap83_1, eduk4_2000, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2000_ind, na.rm = TRUE), rok = 2000) %>% 
  filter(!is.na(ap83_1)) %>%
  filter(!is.na(eduk4_2000)) %>%
  rename(czy_pali = ap83_1, wyksztalcenie = eduk4_2000) 
temp$rok = 2000

temp2 = osoby %>%
  select(bp64, wojewodztwo, podregion58, eduk4_2003, waga_2003_ind) %>%
  mutate_each(funs(as_factor), -waga_2003_ind) %>% 
  group_by(bp64, eduk4_2003, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2003_ind, na.rm = TRUE), rok = 2003) %>% 
  filter(!is.na(bp64)) %>%
  filter(!is.na(eduk4_2003)) %>%
  rename(czy_pali = bp64, wyksztalcenie = eduk4_2003) 
temp2$rok = 2003

temp3 = osoby %>%
  select(cp60, wojewodztwo, podregion58, eduk4_2005, waga_2005_ind) %>%
  mutate_each(funs(as_factor), -waga_2005_ind) %>% 
  group_by(cp60, eduk4_2005, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2005_ind, na.rm = TRUE), rok = 2005) %>% 
  filter(!is.na(cp60)) %>%
  filter(!is.na(eduk4_2005)) %>%
  rename(czy_pali = cp60, wyksztalcenie = eduk4_2005) 
temp3$rok = 2005

temp4 = osoby %>%
  select(dp56, wojewodztwo, podregion58, eduk4_2007, waga_2007_ind) %>%
  mutate_each(funs(as_factor), -waga_2007_ind) %>% 
  group_by(dp56, eduk4_2007, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2007_ind, na.rm = TRUE), rok = 2007) %>% 
  filter(!is.na(dp56)) %>%
  filter(!is.na(eduk4_2007)) %>%
  rename(czy_pali = dp56, wyksztalcenie = eduk4_2007) 
temp4$rok = 2007

temp5 = osoby %>%
  select(ep46, wojewodztwo, podregion58, eduk4_2009, waga_2009_ind) %>%
  mutate_each(funs(as_factor), -waga_2009_ind) %>% 
  group_by(ep46, eduk4_2009, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2009_ind, na.rm = TRUE), rok = 2009) %>% 
  filter(!is.na(ep46)) %>%
  filter(!is.na(eduk4_2009)) %>%
  rename(czy_pali = ep46, wyksztalcenie = eduk4_2009) 
temp5$rok = 2009

temp6 = osoby %>%
  select(fp44, wojewodztwo, podregion58, eduk4_2011, waga_2011_ind) %>%
  mutate_each(funs(as_factor), -waga_2011_ind) %>% 
  group_by(fp44, eduk4_2011, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2011_ind, na.rm = TRUE), rok = 2011) %>% 
  filter(!is.na(fp44)) %>%
  filter(!is.na(eduk4_2011)) %>%
  rename(czy_pali = fp44, wyksztalcenie = eduk4_2011) 
temp6$rok = 2011

temp7 = osoby %>%
  select(gp43, wojewodztwo, podregion58, eduk4_2013, waga_2013_ind) %>%
  mutate_each(funs(as_factor), -waga_2013_ind) %>% 
  group_by(gp43, eduk4_2013, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2013_ind, na.rm = TRUE), rok = 2013) %>% 
  filter(!is.na(gp43)) %>%
  filter(!is.na(eduk4_2013)) %>%
  rename(czy_pali = gp43, wyksztalcenie = eduk4_2013) 
temp7$rok = 2013

temp8 = osoby %>%
  select(hp43, wojewodztwo, podregion58, eduk4_2015, waga_2015_ind) %>%
  mutate_each(funs(as_factor), -waga_2015_ind) %>% 
  group_by(hp43, eduk4_2015, podregion58, wojewodztwo) %>% 
  summarise(waga = sum(waga_2015_ind, na.rm = TRUE), rok = 2015) %>% 
  filter(!is.na(hp43)) %>%
  filter(!is.na(eduk4_2015)) %>%
  rename(czy_pali = hp43, wyksztalcenie = eduk4_2015) 
temp8$rok = 2015

#
### join'em
palenie = rbind(temp, temp2, temp3, temp4, temp5, temp6, temp7, temp8) 

####
palenie_a_podregion_i_rok = palenie %>%
  select(czy_pali, wyksztalcenie, podregion58, wojewodztwo, rok, waga) %>%
  mutate_each(funs(as.factor), -waga) 

# change values
palenie_a_podregion_i_rok$czy_pali = gsub('TAK',1,palenie_a_podregion_i_rok$czy_pali)
palenie_a_podregion_i_rok$czy_pali = gsub('NIE',0,palenie_a_podregion_i_rok$czy_pali)
palenie_a_podregion_i_rok$czy_pali = as.numeric(palenie_a_podregion_i_rok$czy_pali)
####
##
#palenie_a_podregion_i_rok = palenie_a_podregion_i_rok %>% 
#  filter(!is.na(podregion58)) %>%
#  group_by(podregion58, rok) %>% 
#  summarise(percent = sum(czy_pali*waga)/sum(waga)) 
#
#ggplot(data=palenie_a_podregion_i_rok, aes(x=rok, y=percent, group=podregion58))+geom_line()
###
#palenie_a_region_i_rok = palenie_a_podregion_i_rok %>% 
#  filter(!is.na(wojewodztwo)) %>%
#  group_by(wojewodztwo, rok) %>% 
#  summarise(percent = sum(czy_pali*waga)/sum(waga)) 
#
#ggplot(data=palenie_a_region_i_rok, aes(x=rok, y=percent, group=wojewodztwo))+geom_line()
###
palenie_a_wyksztalcenie_i_rok = palenie_a_podregion_i_rok %>% 
  filter(!is.na(wyksztalcenie)) %>%
  group_by(wyksztalcenie, rok) %>% 
  summarise(percent = sum(czy_pali*waga)/sum(waga)) 

palenie_a_wyksztalcenie_i_rok$rok <- strptime(x = as.character(palenie_a_wyksztalcenie_i_rok$rok), format="%Y")

# Pierwszy wykres
ggplot(data=palenie_a_wyksztalcenie_i_rok, aes(x=rok, y=percent, group=wyksztalcenie, color=wyksztalcenie))+
  geom_line(stat = "identity")+
  scale_colour_brewer(type='seq', palette = 'OrRd')+
  scale_x_datetime(date_labels = "%Y", date_breaks = '2 years')+
  ggtitle('Procent palących na przestrzeni lat')+
  ylab('Procent palących')+
  xlab('Rok badania')

# 2gi wykres
library(ggthemes)
ggplot(data=palenie_a_wyksztalcenie_i_rok, aes(x=rok, y=percent, group=wyksztalcenie, color=wyksztalcenie))+
  geom_line(stat = "identity")+
  scale_colour_brewer(type='seq', palette = 'OrRd')+
  scale_x_datetime(date_labels = "%Y", date_breaks = '2 years')+
  ylab('Procent palących')+
  xlab('Rok badania')+
  geom_smooth(method='loess')+
  scale_y_continuous(labels = scales::percent)+
  theme_dark()+
  ggtitle(expression(atop("Procent palących na przestrzeni lat", atop(italic("Dane z Diagnozy Społecznej"), "")))) +
  theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
        plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1))


### 3ci wykres
ggplot(data=palenie_a_wyksztalcenie_i_rok, aes(x=rok, y=percent, group=wyksztalcenie, color=wyksztalcenie))+
  geom_line(stat = "identity")+
  scale_colour_brewer(type='seq', palette = 'OrRd')+
  scale_x_datetime(date_labels = "%Y", date_breaks = '2 years')+
  geom_smooth(method='loess')+
  scale_y_continuous(labels = scales::percent)+
  theme_dark()+
  ggtitle(expression(atop("Procent palących na przestrzeni lat", atop(italic("Dane z Diagnozy Społecznej"), "")))) +
  theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
        plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1, hjust=0.5))+
  theme(legend.direction = 'vertical',
        legend.key.size = unit(3, 'lines'), legend.justification = c(1, -0.015), 
        legend.title = element_text(size = 18, colour = "black", vjust = 0),
        legend.text = element_text(size = 11),
        axis.title.x = element_text(size = 15, colour = 'grey35'),
        axis.title.y = element_text(size = 15, colour = 'grey35'),
        plot.background = element_rect(fill = 'grey90'),
        legend.background = element_rect(fill = 'grey90')) +
        labs(x = 'Rok badania', y = 'Procent palących', color='Wykształcenie')
