


 ## @knitr Packages 
library(tidyverse)
library(knitr)
library(RColorBrewer) # display.brewer.all()...to see colors
library(plyr)
library(captioner)
library(flextable)
library(officer)
library(skimr)
library(vegan)



## @knitr Functions

fig <- local({
     i <- 0
     ref <- list()
     list(
          cap=function(refName, text) {
               i <<- i + 1
               ref[[refName]] <<- i
               paste("Figure ", i, ": ", text, sep="")
          },
          ref=function(refName) {
               ref[[refName]]
          })
})




## @knitr Tables

table_nums <- captioner::captioner(prefix = "Tab.")

tab.1_cap <- table_nums(name = "tab_1", 
                        caption = "Fish taxa identified from 48 bottom-trawl hauls sampled off Guyana. Count = species presence within the samples. With those identified in > 30  hauls colored blue")
tab.2_cap <- table_nums(name = "tab_2", 
                        caption = "Count of species sampled by trip and time-period")

tab.3_cap <- table_nums(name = "tab_3", 
                        caption = "Count of species sampled by trip and fishing depth")

tab.4_cap <- table_nums(name = "tab_4", 
                        caption = "Sample size measured by fishing trip")

tab.6_cap <- table_nums(name = "tab_6", 
                        caption = "Catch and effort for the top 15 species, ordered from highest to lowest catches. ")

tab.7_cap <- table_nums(name = "tab_7", 
                        caption = "Catch rates for the top 15 species in pounds per hour, ordered from highest to lowest catch rates. ")

tab.8_cap <- table_nums(name = "tab_8", 
                        caption = "Count of species measured across all hauls")

tab.9_cap <- table_nums(name = "tab_9", 
                        caption = "Lengths (cm) of all species measured, ordered from highest to lowest mean lengths. ")

## @knitr Data

obs_wt <- read_csv("observer_weights.csv") 
obs_tp <- read_csv("observer_trips.csv")
obs_fy <- read_csv("observer_fishery.csv")
obs_lt <- read_csv("observer_lengths.csv")
obs_wt_db <- obs_wt %>% 
     filter(groups == "Discarded bycatch") %>% filter(!class == "Unknown") 

obs_wt_rb <- obs_wt %>% 
     filter(groups == "Retained bycatch") 
obs_wt_tc <- obs_wt %>% 
     filter(groups == "Target catch") 

lengths <- dplyr::inner_join(obs_wt, obs_lt, by=c("drag_all", "sci_name", "trip"))

species_curve_t <- read_csv("species_accum_trip.csv")
species_curve_h <- read_csv("species_accum_haul.csv")

time_catch <- read_csv("time_period_catch.csv")
time_cpue <- read_csv("time_period_cpue.csv")
depth_catch <- read_csv("depth_catch.csv")
depth_cpue <- read_csv("depth_cpue.csv")

time_catch_2 <- time_catch %>% gather(day, night, key = "time_period", value = "catch") %>% select(-"difference")

time_cpue_2 <- time_cpue %>% gather(day, night, key = "time_period", value = "cpue") %>% select(-"difference")

depth_catch_2 <-depth_catch %>% gather(shallow, deep, key = "fishing_depth", value = "catch") %>% select(-"difference")

depth_cpue_2 <- depth_cpue %>% gather(shallow, deep, key = "fishing_depth", value = "cpue") %>% select(-"difference")



`r table_nums('tab_1')`

## @knitr Table 1

table_1 <- obs_wt_db %>%
     filter(groups == "Discarded bycatch") %>% 
     group_by(order, family, sci_name) %>%
     dplyr::summarise(Count = round(n()/48,1), .groups = 'drop') %>% 
     arrange(order, family)

table_1$order = ifelse(duplicated(table_1$order),"",table_1$order)
table_1$family = ifelse(duplicated(table_1$family),"",table_1$family)

colnames(table_1) <-c("Orders", "Families", "Scientific names", "Counts")

table_1 %>% 
     flextable() %>% 
     autofit() %>% 
     align_text_col(align = "left") %>% 
     align_nottext_col(align = "left") %>% 
     bg(bg = "#C90000", part = "header") %>% 
     color(color = "white", part = "header") %>% 
     color(~ Counts >= 0.5, ~ Counts, color = "blue") %>% 
     bold(~ Counts >= 0.5, ~ Counts, bold = TRUE)
