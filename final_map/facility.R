library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)
library(knitr)
library(stringr)
library(rsconnect)
library(maps)
library(dplyr)
mental_health <- read.csv("nmhss-puf-2018.csv")
state_populations <- read.csv("state.csv")
mental_health_facilities <- full_join(mental_health, state_populations, by = "LST")
state_shapes <- map_data("state") %>%
  mutate(name = toupper(region))
abbreviations <- data.frame(name = state.name, abb= state.abb) %>%
  mutate(name = toupper(name))
state_map <- left_join(state_shapes, abbreviations, by = "name") %>%
  mutate(LST = abb)
psych <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 1)
agg_psych <- aggregate(psych['FACILITYTYPE'], by=psych['LST'], sum) %>%
mutate(number = FACILITYTYPE)
#state_psych <- left_join(agg_psych, by = "LST") %>%
#  mutate(number = FACILITYTYPE) %>%
#  left_join(psych, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_psych <- left_join(psych, agg_psych, by = "LST")
map_psych <- final_psych %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
#pop_psych <- state_psych %>%
#  left_join(state_populations, by = "LST") %>%
#  mutate(pop = (FACILITYTYPE / POPESTIMATE2018)*100000)
total <- mental_health_facilities %>%
  select(CASEID, LST, MHINTAKE) %>%
  filter(MHINTAKE == 1)
agg_total <- aggregate(total['MHINTAKE'], by=total['LST'], sum) %>%
  mutate(number = MHINTAKE)
#state_total <- left_join(state_map, agg_total, by = "LST") %>%
#  mutate(number = MHINTAKE) %>%
#  left_join(total, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_total <- left_join(total, agg_total, by = "LST")
map_total <- final_total %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, MHINTAKE.x, number, long, lat, group, order)
separate <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 2)
agg_separate <- aggregate(separate['FACILITYTYPE'], by=separate['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 2)
final_separate <- left_join(separate, agg_separate, by = "LST")
map_separate <- final_separate %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
#state_separate <- left_join(state_map, agg_separate, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 2) %>%
#  left_join(separate, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
res_child <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 3)
agg_res_child <- aggregate(res_child['FACILITYTYPE'], by=res_child['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 3)
#state_res_child <- left_join(state_map, agg_res_child, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 3) %>%
#  left_join(res_child, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_res_child <- left_join(res_child, agg_res_child, by = "LST")
map_res_child <- final_res_child %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
res_adult <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 4)
agg_res_adult <- aggregate(res_adult['FACILITYTYPE'], by=res_adult['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 4)
#state_res_adult <- left_join(state_map, agg_res_adult, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 4) %>%
#  left_join(res_adult, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_res_adult <- left_join(res_adult, agg_res_adult, by = "LST")
map_res_adult <- final_res_adult %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
other_res <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 5)
agg_other_res <- aggregate(other_res['FACILITYTYPE'], by=other_res['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 5)
#state_other_res <- left_join(state_map, agg_other_res, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 5) %>%
#  left_join(other_res, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_other_res <- left_join(other_res, agg_other_res, by = "LST")
map_other_res <- final_other_res %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
vet <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 6)
agg_vet <- aggregate(vet['FACILITYTYPE'], by=vet['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 6)
#state_vet <- left_join(state_map, agg_vet, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 6) %>%
#  left_join(vet, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_vet <- left_join(vet, agg_vet, by = "LST")
map_vet <- final_vet %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
com <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 7)
agg_com <- aggregate(com['FACILITYTYPE'], by=com['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 7)
#state_com <- left_join(state_map, agg_com, by = "LST")  %>%
#  mutate(number = FACILITYTYPE / 7) %>%
#  left_join(com, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_com <- left_join(com, agg_com, by = "LST")
map_com <- final_com %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
partial <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 8)
agg_partial <- aggregate(partial['FACILITYTYPE'], by=partial['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 8)
#state_partial <- left_join(state_map, agg_partial, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 8) %>%
#  left_join(partial, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_partial <- left_join(partial, agg_partial, by = "LST")
map_partial <- final_partial %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
outpatient <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 9)
agg_outpatient <- aggregate(outpatient['FACILITYTYPE'], by=outpatient['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 9)
#state_outpatient <- left_join(state_map, agg_outpatient, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 9) %>%
#  left_join(outpatient, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_outpatient <- left_join(outpatient, agg_outpatient, by = "LST")
map_outpatient <- final_outpatient %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
multi <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 10)
agg_multi <- aggregate(multi['FACILITYTYPE'], by=multi['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 10)
#state_multi <- left_join(state_map, agg_multi, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 10) %>%
#  left_join(multi, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_multi <- left_join(multi, agg_multi, by = "LST")
map_multi <- final_multi %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
other <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 11)
agg_other <- aggregate(other['FACILITYTYPE'], by=other['LST'], sum) %>%
  mutate(number = FACILITYTYPE / 11)
#state_other <- left_join(state_map, agg_other, by = "LST") %>%
#  mutate(number = FACILITYTYPE / 11) %>%
#  left_join(other, by = "LST") %>%
#  select(long, lat, group, order, LST, number, CASEID)
final_other <- left_join(other, agg_other, by = "LST")
map_other <- final_other %>%
  left_join(state_map, by = "LST") %>%
  select(CASEID, LST, FACILITYTYPE.x, number, long, lat, group, order)
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )
age_data<- mental_health %>%
  select(CASEID, LST, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS)
test <- age_data %>%
  semi_join(final_com, by = "LST") %>%
  filter(LST == "CO") %>% 
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS)  %>%
  summarise_each(funs = sum) %>%
  gather()
test_psych <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 1)
test_state_psych <- left_join(state_map, agg_psych, by = "LST") %>%
  mutate(number = FACILITYTYPE) %>%
  left_join(test_psych, by = "LST")
test_test<- age_data %>%
  full_join(test_state_psych, by = "CASEID") %>%
  mutate(LST = LST.x) %>%
  filter(LST == "CO") %>%
  select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, long, lat, group, order, number, LST) %>%
  na.omit() %>%
  distinct(CASEID, .keep_all = TRUE) %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  summarise_each(funs = sum) %>%
  gather()
#  mutate(FACILITYTYPE.y = "psych")
#  filter(FACILITYTYPE.y == 1) %>%
#  distinct(CASEID, .keep_all = TRUE)
tester <- final_com %>%
  full_join(state_map, by = "LST") %>%
  select(long, lat, group, order, LST, number) %>%
  ggplot() +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, 
                  LST = LST, fill = number))
test_chart<- final_total %>%
  full_join(age_data, by = "CASEID") %>%
  mutate(LST = LST.x)  %>%
  filter(LST == "CO") %>%
  select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, LST) %>%
  na.omit() %>%
  distinct(CASEID, .keep_all = TRUE) %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  summarise_each(funs = sum) %>%
  gather() %>%
  ggplot() +
  geom_col(aes(x = key, y = value, fill = value)) 
test_map <- map_total %>%
  ggplot() +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, 
                  LST = LST, fill = number)) 
test_map_two <- final_com %>%
  full_join(state_map, by = "LST") %>%
  select(CASEID, long, lat, group, order, LST, number) %>%
  distinct(order, .keep_all = TRUE) %>%
  ggplot() +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, 
                  LST = LST, fill = number))
new_chart <- final_com %>%
  left_join(age_data, by = "LST") %>%
  filter(LST == "CO") %>%
  select(CASEID.x, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, LST) %>%
  na.omit() %>%
  distinct(CASEID.x, .keep_all = TRUE) %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  summarise_each(funs = sum) %>%
  gather() %>%
  ggplot() +
  geom_col(aes(x = key, y = value, fill = value,
               text = paste("Number of Facilities:", value)))
test_chart_two<- final_com %>%
  full_join(age_data, by = "CASEID") %>%
  mutate(LST = LST.x)  %>%
  filter(LST == "CO") %>%
  select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, LST) %>%
  na.omit() %>%
  distinct(CASEID, .keep_all = TRUE) %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  summarise_each(funs = sum) %>%
  gather() %>%
  ggplot() +
  geom_col(aes(x = key, y = value, fill = value)) 
test_chart_three <- final_com %>%
  left_join(age_data, by = "CASEID") %>%
  filter(LST.x == "CO") %>%
  select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, LST.x) %>%
  na.omit() %>%
  distinct(CASEID, .keep_all = TRUE) %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  summarise_each(funs = sum) %>%
  gather() %>%
  ggplot() +
  geom_col(aes(x = key, y = value, fill = value,
               text = paste("Number of Facilities:", value)))
