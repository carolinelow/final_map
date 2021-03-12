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
mental_health_facilities <- full_join(mental_health, state_populations,
  by = "LST"
)
state_shapes <- map_data("state") %>%
  mutate(name = toupper(region))
abbreviations <- data.frame(name = state.name, abb = state.abb) %>%
  mutate(name = toupper(name))
state_map <- left_join(state_shapes, abbreviations, by = "name") %>%
  mutate(LST = abb)
psych <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 1)
agg_psych <- aggregate(psych["FACILITYTYPE"], by = psych["LST"], sum) %>%
  mutate(number = FACILITYTYPE)
final_psych <- left_join(psych, agg_psych, by = "LST")
total <- mental_health_facilities %>%
  select(CASEID, LST, MHINTAKE) %>%
  filter(MHINTAKE == 1)
agg_total <- aggregate(total["MHINTAKE"], by = total["LST"], sum) %>%
  mutate(number = MHINTAKE)
final_total <- left_join(total, agg_total, by = "LST")
separate <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 2)
agg_separate <- aggregate(separate["FACILITYTYPE"], by = separate["LST"],
                          sum) %>%
  mutate(number = FACILITYTYPE / 2)
final_separate <- left_join(separate, agg_separate, by = "LST")
res_child <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 3)
agg_res_child <- aggregate(res_child["FACILITYTYPE"], by = res_child["LST"],
                           sum) %>%
  mutate(number = FACILITYTYPE / 3)
final_res_child <- left_join(res_child, agg_res_child, by = "LST")
res_adult <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 4)
agg_res_adult <- aggregate(res_adult["FACILITYTYPE"], by = res_adult["LST"],
                           sum) %>%
  mutate(number = FACILITYTYPE / 4)
final_res_adult <- left_join(res_adult, agg_res_adult, by = "LST")
other_res <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 5)
agg_other_res <- aggregate(other_res["FACILITYTYPE"],
  by = other_res["LST"],
  sum
) %>%
  mutate(number = FACILITYTYPE / 5)
final_other_res <- left_join(other_res, agg_other_res, by = "LST")
vet <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 6)
agg_vet <- aggregate(vet["FACILITYTYPE"], by = vet["LST"], sum) %>%
  mutate(number = FACILITYTYPE / 6)
final_vet <- left_join(vet, agg_vet, by = "LST")
com <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 7)
agg_com <- aggregate(com["FACILITYTYPE"], by = com["LST"], sum) %>%
  mutate(number = FACILITYTYPE / 7)
final_com <- left_join(com, agg_com, by = "LST")
partial <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 8)
agg_partial <- aggregate(partial["FACILITYTYPE"], by = partial["LST"], sum) %>%
  mutate(number = FACILITYTYPE / 8)
final_partial <- left_join(partial, agg_partial, by = "LST")
outpatient <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 9)
agg_outpatient <- aggregate(outpatient["FACILITYTYPE"],
  by = outpatient["LST"],
  sum
) %>%
  mutate(number = FACILITYTYPE / 9)
final_outpatient <- left_join(outpatient, agg_outpatient, by = "LST")
multi <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 10)
agg_multi <- aggregate(multi["FACILITYTYPE"], by = multi["LST"], sum) %>%
  mutate(number = FACILITYTYPE / 10)
final_multi <- left_join(multi, agg_multi, by = "LST")
other <- mental_health_facilities %>%
  select(CASEID, LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 11)
agg_other <- aggregate(other["FACILITYTYPE"], by = other["LST"], sum) %>%
  mutate(number = FACILITYTYPE / 11)
final_other <- left_join(other, agg_other, by = "LST")
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
age_data <- mental_health %>%
  select(CASEID, LST, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS)
