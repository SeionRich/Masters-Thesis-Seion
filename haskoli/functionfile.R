
#####==============================================#####
###==================FUNCTION ONE====================###
###==================DENSITY PLOTS====================###
#####==============================================#####

TimePeriodWeight <- function(Data, Group, Quan_var, Wvar1, Wvar2, Pos_x = 140, Pos_y1 = 0.025, Pos_y2 = 0.02, hj = 0.3, vj = 1, colr = "Blues", plot_type = geom_density(), species){

#==================TABLE=====================#  
  Wdata <- Data %>% 
  dplyr::group_by({{ Group }}) %>% 
  dplyr::summarise(Sum_wt = sum({{ Quan_var }}), Median_wt = median({{ Quan_var }}), Mean_wt = round(mean({{ Quan_var }})), SD = round(sd({{ Quan_var }})), Min. = min({{ Quan_var }}), Max. = max({{ Quan_var }}), Trips = n(), .groups = 'drop') %>% 
  mutate(Perc_wt = round(Sum_wt/sum(Sum_wt)*100)) 
  
#==================PLOT=====================# 
  Plot <- Data %>% 
    ggplot(aes(x = {{ Quan_var }}, fill = {{ Group }})) +
    plot_type +
    # xlab(substitute(paste(italic(species), " Weights (kgs)"))) +
    xlab(substitute(paste(italic(species), " Weights (kgs)"))) +
  ylab("Density") +
  scale_fill_brewer(palette = colr) +
  facet_wrap(vars({{ Group }}), ncol=1) +
  theme_bw() +
  guides(fill=FALSE) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10)) + geom_vline(data = Wdata, aes(xintercept={{ Wvar1 }}), linetype="dotted", colour="#BB0000") + geom_label(data = Wdata, mapping = aes(label = paste0("Median weight = ", round({{ Wvar1 }})),  x = Pos_x, y = Pos_y1), size = 3, hjust = hj, vjust = vj) + geom_label(data = Wdata, mapping = aes(label = paste0("Total weight = ", round({{ Wvar2 }})),  x = Pos_x, y = Pos_y2), size = 3, hjust = hj, vjust = vj)
  
print(Plot)

print(Wdata %>% 
  kbl() %>%
  kable_classic() %>% 
  row_spec(0, bold = TRUE))

}


#####==============================================#####
###==================FUNCTION TWO====================###
###===================LINE PLOTS====================###
#####==============================================#####

TimePeriodWeight2 <- function(Data, Group, Quan_var, Quan_var2, Wvar1, Wvar2, colr = "Blues", plot_type = geom_line(), plot_type2 = geom_point(), species, Pos_x = 48, Pos_y1 = 40, Pos_y2 = 32, hj = 0.85, vj = 1){

#==================TABLE=====================#  
  
  Wdata <- Data %>% 
    dplyr::group_by({{ Group }}) %>% 
    dplyr::summarise(Median_wt = median({{ Quan_var }}), Mean_wt = round(mean({{ Quan_var }})), SD = round(sd({{ Quan_var }})), Min. = min({{ Quan_var }}), Max. = max({{ Quan_var }}), Trips = n(), .groups = 'drop') 
  
  
#==================PLOT=====================# 
  Plot <- Data %>% 
    ggplot(aes(x = {{ Quan_var2 }}, y = {{ Quan_var}}, color = {{ Group }})) +
    plot_type +
    plot_type2 +
    xlab(substitute(paste(italic(species), " Trips"))) +
    ylab("Catch-per-unit-effort (kgs/hr)") +
    scale_fill_brewer(palette = colr) +
    facet_wrap(vars({{ Group }}), ncol=1) +
    theme_bw() +
    theme(legend.position="none") +
    guides(fill=FALSE) +
    theme(axis.text=element_text(size=10),axis.title=element_text(size=10)) +
 geom_hline(data = Wdata, aes(yintercept={{ Wvar1 }}), linetype= "dotted", colour = "#BB0000") + geom_label(data = Wdata, mapping = aes(label = paste0("Median cpue = ", round({{ Wvar1 }})),  x = Pos_x, y = Pos_y1), size = 3, hjust = hj, vjust = vj) + geom_label(data = Wdata, mapping = aes(label = paste0("Abundance = ", round({{ Wvar2 }})),  x = Pos_x, y = Pos_y2), size = 3, hjust = hj, vjust = vj)
  
  
  print(Plot)
  
  docx_value(Wdata %>% 
               flextable() %>% 
               autofit() %>% 
               align_text_col(align = "left") %>% 
               align_nottext_col(align = "left"))
  
}



#####==================================================#####
###================== FUNCTION THREE ====================###
###=============== SPECIES_COMPARISON ===================###
#####==================================================#####

SpeciesComparison <- function(Data, Group, Group2, Quan_var, Condition1, Condition2, names, Join, variable, by, env = parent.frame(),...){

# Data - (Lengths or weights), Group (Scientific name), Group2(Time period or fishing depth - binomial variables), Quan_var(variable being measured e.g. length/weight), Condition 1 and 2 (time period or fishing depth e.g. day/night or deep/shallow - "String objects"), names (vector of scientific names), Test (the object name for your T-test output), Group 3 (Trip.x), variable(t-test - means for the quantitative variables to be tested), by(t-test- Condition to tested against)
  
Table1 <- Data %>% 
  group_by({{ Group }}, {{ Group2 }}) %>%
  dplyr::summarise(m_len = round(mean({{ Quan_var }})), s_e = round(std.error({{ Quan_var }}),2), Count = n(), .groups = 'drop') %>% 
  filter(duplicated({{ Group }}) | duplicated({{ Group }}, fromLast=TRUE)) 

Table2 <- Table1 %>% 
  filter({{ Group2 }} == Condition1)
Table3 <- Table1 %>% 
  filter({{ Group2 }} == Condition2)

Table4  <- dplyr::left_join(Table2, Table3, by= Join)

colnames(Table4) <- c("Sceintific_names", "Time_period", "ml_cm", "SE", "n", "Time_period_b", "ml_cm_b", "SE_b", "n_b")

Table4 <- Table4 %>% select(-c(2,6)) %>% 
  filter(Sceintific_names %in% names)

Table4 <- Table4 %>% 
  mutate(Difference = round(((ml_cm - ml_cm_b)/ml_cm)*100,2))

Table4 <- Table4 %>%
  slice(match(names, Sceintific_names))

#####=====================T-TEST STARTS=======================######

#####=================== SPECIES ONE =========================#####

# Species1 <- Data %>%
#   filter({{ Group }} == names[1]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) ----OLD CODE TEMPLATE

# Loop1 <- c(1,2,3,4,5,6)
# 
# for (i in Loop1){
# 
#   species_name = paste0("Species",i)
# 
#   assign(species_name, eval({{ Data }} %>% filter({{ Group }} == names[i]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Quan_var }}))) # %>% print()
# 
# } #For loop attempt


# Help source: https://stackoverflow.com/questions/52009932/r-loop-change-variable-names

Species1 <- Data %>%
  filter({{ Group }} == names[1]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})


#####=================== SPECIES TWO =========================#####

Species2 <- Data %>%
  filter({{ Group }} == names[2]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})

#####=================== SPECIES THREE =========================#####

Species3 <- Data %>%
  filter({{ Group }} == names[3]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})

#####=================== SPECIES FOUR =========================#####

Species4 <- Data %>%
  filter({{ Group }} == names[4]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})

#####=================== SPECIES FIVE =========================#####

Species5 <- Data %>%
  filter({{ Group }} == names[5]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})

#####=================== SPECIES SIX =========================#####

Species6 <- Data %>%
  filter({{ Group }} == names[6]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Quan_var }})

exp1 <- expr(!!ensym(variable) ~ !!ensym(by))

T_test1 <- t.test(formula = eval(exp1), data = Species1,...)
T_test2 <- t.test(formula = eval(exp1), data = Species2,...)
T_test3 <- t.test(formula = eval(exp1), data = Species3,...)
T_test4 <- t.test(formula = eval(exp1), data = Species4,...)
T_test5 <- t.test(formula = eval(exp1), data = Species5,...)
T_test6 <- t.test(formula = eval(exp1), data = Species6,...)


#####================ SPECIES LENGTH TABLE ====================#####

Results <- map_df(list(T_test1, T_test2, T_test3, T_test4, T_test5, T_test6), tidy)

#####=======================T-TEST ENDS========================######


p_value <- round(Results$p.value,5)

Table4 <- cbind(Table4, p_value)

Table4 <- flextable(Table4, col_keys = c("Sceintific_names", "ml_cm", "SE", "n", "ml_cm_b", "SE_b", "n_b", "Difference", "p_value"))

# start with no header
Table4 <- delete_part(Table4, part = "header")

# add a line of row
Table4 <- add_header(x = Table4, Sceintific_names = "Species", ml_cm = "Mean_length_cm", SE = "SE", n = "n", ml_cm_b = "Mean_length_cm_n", SE_b = "SE_n", n_b = "n_n", Difference = "Difference", p_value = "p_value", top = FALSE)

# add another line of row at the top position
Table4 <- add_header(x = Table4, ml_cm = Condition1, SE = Condition1, n = Condition1, ml_cm_b = Condition2, SE_b = Condition2, n_b = Condition2, Difference = "Comparison", p_value = "Comparison", top = TRUE )

# merge horizontally when there are identical values
Table4 <- merge_h(Table4, part = "header")

Table4 <- Table4 %>%  
  autofit() %>% 
  theme_booktabs() %>% 
  bold(bold = TRUE, part = "header") %>% 
  align(align = "left", part = "all") %>% 
  align(i = 1, j = c(1:9), align = "center", part = "header") %>% 
  compose(i = 2, j = c(2:9), part = "header", value = as_paragraph(c("Mean)", "SE","n","Mean","SE","n", "Difference.", "P value"))) 

return(Table4)

}


# 
# #####==================================================#####
# ###================== FUNCTION FOUR ====================###
# ###=============== SPECIES_COMPARISON ===================###
# #####================ FIVE SPECIES=======================#####
# 
# SpeciesComparison2 <- function(Data, Group, Group2, Quan_var, Condition1, Condition2, names, Join, variable, by, env = parent.frame(),...){
# 
# # Data - (Lengths or weights), Group (Scientific name), Group2(Time period or fishing depth - binomial variables), Quan_var(variable being measured e.g. length/weight), Condition 1 and 2 (time period or fishing depth e.g. day/night or deep/shallow - "String objects"), names (vector of scientific names), Test (the object name for your T-test output), Group 3 (Trip.x), variable(t-test - means for the quantitative variables to be tested), by(t-test- Condition to tested against)
#   
# Table1 <- Data %>% 
#   group_by({{ Group }}, {{ Group2 }}) %>%
#   dplyr::summarise(m_len = round(mean({{ Quan_var }})), s_e = round(std.error({{ Quan_var }}),2), Count = n(), .groups = 'drop') %>% 
#   filter(duplicated({{ Group }}) | duplicated({{ Group }}, fromLast=TRUE)) 
# 
# Table2 <- Table1 %>% 
#   filter({{ Group2 }} == Condition1)
# Table3 <- Table1 %>% 
#   filter({{ Group2 }} == Condition2)
# 
# Table4  <- dplyr::left_join(Table2, Table3, by= Join)
# 
# colnames(Table4) <- c("Sceintific_names", "Time_period", "ml_cm", "SE", "n", "Time_period_b", "ml_cm_b", "SE_b", "n_b")
# 
# Table4 <- Table4 %>% select(-c(2,6)) %>% 
#   filter(Sceintific_names %in% names)
# 
# Table4 <- Table4 %>% 
#   mutate(Difference = round(((ml_cm - ml_cm_b)/ml_cm)*100,2))
# 
# Table4 <- Table4 %>%
#   slice(match(names, Sceintific_names))
# 
# #####=====================T-TEST STARTS=======================######
# 
# #####=================== SPECIES ONE =========================#####
# 
# Loop1 <- c(1,2,3,4,5)
# 
# for (i in Loop1){
# 
#   species_name = paste0("Species",i)
# 
#   assign(species_name, eval(lengths %>% filter(sci_name == names[i]) %>%
#   select(sci_name, drag_period, length)))
# 
# # Species1 <- Data %>%
# #   filter({{ Group }} == names[1]) %>%
# #   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
# #   dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
# #   dplyr::summarise(mn_ln = mean({{ Quan_var }}))
# # 
# # #####=================== SPECIES TWO =========================#####
# # 
# # Species2 <- Data %>%
# #   filter({{ Group }} == names[2]) %>%
# #   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
# #   group_by({{ Group3 }}, {{ Group2 }}) %>%
# #   dplyr::summarise(mn_ln = mean({{ Quan_var }}))
# # 
# # #####=================== SPECIES THREE =========================#####
# # 
# # Species3 <- Data %>%
# #   filter({{ Group }} == names[3]) %>%
# #   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
# #   group_by({{ Group3 }}, {{ Group2 }}) %>%
# #   dplyr::summarise(mn_ln = mean({{ Quan_var }}))
# # 
# # #####=================== SPECIES FOUR =========================#####
# # 
# # Species4 <-  Data %>%
# #   filter({{ Group }} == names[4]) %>%
# #   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
# #   group_by({{ Group3 }}, {{ Group2 }}) %>%
# #   dplyr::summarise(mn_ln = mean({{ Quan_var }}))
# # 
# # #####=================== SPECIES FIVE =========================#####
# # 
# # Species5 <- Data %>%
# #   filter({{ Group }} == names[5]) %>%
# #   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
# #   group_by({{ Group3 }}, {{ Group2 }}) %>%
# #   dplyr::summarise(mn_ln = mean({{ Quan_var }}))
# 
# exp1 <- expr(!!ensym(variable) ~ !!ensym(by))
# 
# T_test1 <- t.test(formula = eval(exp1), data = Species1,...)
# T_test2 <- t.test(formula = eval(exp1), data = Species2,...)
# T_test3 <- t.test(formula = eval(exp1), data = Species3,...)
# T_test4 <- t.test(formula = eval(exp1), data = Species4,...)
# T_test5 <- t.test(formula = eval(exp1), data = Species5,...)
# 
# 
# 
# #####================ SPECIES LENGTH TABLE ====================#####
# 
# Results <- map_df(list(T_test1, T_test2, T_test3, T_test4, T_test5), tidy)
# 
# #####=======================T-TEST ENDS========================######
# 
# 
# p_value <- round(Results$p.value,5)
# 
# Table4 <- cbind(Table4, p_value)
# 
# Table4 <- flextable(Table4, col_keys = c("Sceintific_names", "ml_cm", "SE", "n", "ml_cm_b", "SE_b", "n_b", "Difference", "p_value"))
# 
# # start with no header
# Table4 <- delete_part(Table4, part = "header")
# 
# # add a line of row
# Table4 <- add_header(x = Table4, Sceintific_names = "Species", ml_cm = "Mean_length_cm", SE = "SE", n = "n", ml_cm_b = "Mean_length_cm_n", SE_b = "SE_n", n_b = "n_n", Difference = "Difference", p_value = "p_value", top = FALSE)
# 
# # add another line of row at the top position
# Table4 <- add_header(x = Table4, ml_cm = Condition1, SE = Condition1, n = Condition1, ml_cm_b = Condition2, SE_b = Condition2, n_b = Condition2, Difference = "Comparison", p_value = "Comparison", top = TRUE )
# 
# # merge horizontally when there are identical values
# Table4 <- merge_h(Table4, part = "header")
# 
# Table4 <- Table4 %>%  
#   autofit() %>% 
#   theme_booktabs() %>% 
#   bold(bold = TRUE, part = "header") %>% 
#   align(align = "left", part = "all") %>% 
#   align(i = 1, j = c(1:9), align = "center", part = "header") %>% 
#   compose(i = 2, j = c(2:9), part = "header", value = as_paragraph(c("Mean)", "SE","n","Mean","SE","n", "Difference.", "P value"))) 
# 
# return(Table4)
# 
# }
# 
# 
# 
# 

#####==================================================#####
###================== FUNCTION FIVE ====================###
###=============== SPECIES_COMPARISON ===================###
#####==================================================#####

SpeciesComparison3 <- function(Data, Group, Group2, Group3, Quan_var, Condition1, Condition2, names, Join, variable, by, env = parent.frame(),...){

# Data - (Lengths or weights), Group (Scientific name), Group2(Time period or fishing depth - binomial variables), Quan_var(variable being measured e.g. length/weight), Condition 1 and 2 (time period or fishing depth e.g. day/night or deep/shallow - "String objects"), names (vector of scientific names), Test (the object name for your T-test output), Group 3 (Trip.x), variable(t-test - means for the quantitative variables to be tested), by(t-test- Condition to tested against)

Table1 <- Data %>%
  group_by({{ Group }}, {{ Group2 }}) %>%
  dplyr::summarise(m_len = round(mean({{ Quan_var }})), s_e = round(std.error({{ Quan_var }}),2), Count = n(), .groups = 'drop') %>%
  filter(duplicated({{ Group }}) | duplicated({{ Group }}, fromLast=TRUE))

Table2 <- Table1 %>%
  filter({{ Group2 }} == Condition1)
Table3 <- Table1 %>%
  filter({{ Group2 }} == Condition2)

Table4  <- dplyr::left_join(Table2, Table3, by= Join)

colnames(Table4) <- c("Sceintific_names", "Time_period", "ml_cm", "SE", "n", "Time_period_b", "ml_cm_b", "SE_b", "n_b")

Table4 <- Table4 %>% select(-c(2,6)) %>%
  filter(Sceintific_names %in% names)

Table4 <- Table4 %>%
  mutate(Difference = round(((ml_cm - ml_cm_b)/ml_cm)*100,2))

Table4 <- Table4 %>%
  slice(match(names, Sceintific_names))

#####=====================T-TEST STARTS=======================######

#####=================== SPECIES ONE =========================#####

Species1 <- Data %>%
  filter({{ Group }} == names[1]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# #####=================== SPECIES TWO =========================#####
# 
# Species2 <- Data %>%
#   filter({{ Group }} == names[2]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
#   ungroup() %>%
#   subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# #####=================== SPECIES THREE =========================#####
# 
# Species3 <- Data %>%
#   filter({{ Group }} == names[3]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
#   ungroup() %>%
#   subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# #####=================== SPECIES FOUR =========================#####
# 
# Species4 <-  Data %>%
#   filter({{ Group }} == names[4]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
#   ungroup() %>%
#   subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# #####=================== SPECIES FIVE =========================#####
# 
# Species5 <- Data %>%
#   filter({{ Group }} == names[5]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
#   ungroup() %>%
#   subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# #####=================== SPECIES SIX =========================#####
# 
# Species6 <- Data %>%
#   filter({{ Group }} == names[6]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
#   group_by({{ Group3 }}, {{ Group2 }}) %>%
#   dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
#   ungroup() %>%
#   subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))
# 
# exp1 <- expr(!!ensym(variable) ~ !!ensym(by))
# 
# T_test1 <- t.test(formula = eval(exp1), data = Species1,...)
# T_test2 <- t.test(formula = eval(exp1), data = Species2,...)
# T_test3 <- t.test(formula = eval(exp1), data = Species3,...)
# T_test4 <- t.test(formula = eval(exp1), data = Species4,...)
# T_test5 <- t.test(formula = eval(exp1), data = Species5,...)
# T_test6 <- t.test(formula = eval(exp1), data = Species6,...)
# 
# 
# #####================ SPECIES LENGTH TABLE ====================#####
# 
# Results <- map_df(list(T_test1, T_test2, T_test3, T_test4, T_test5, T_test6), tidy)
# 
# #####=======================T-TEST ENDS========================######
# 
# 
# p_value <- round(Results$p.value,5)
# 
# Table4 <- cbind(Table4, p_value)
# 
# Table4 <- flextable(Table4, col_keys = c("Sceintific_names", "ml_cm", "SE", "n", "ml_cm_b", "SE_b", "n_b", "Difference", "p_value"))
# 
# # start with no header
# Table4 <- delete_part(Table4, part = "header")
# 
# # add a line of row
# Table4 <- add_header(x = Table4, Sceintific_names = "Species", ml_cm = "Mean_length_cm", SE = "SE", n = "n", ml_cm_b = "Mean_length_cm_n", SE_b = "SE_n", n_b = "n_n", Difference = "Difference", p_value = "p_value", top = FALSE)
# 
# # add another line of row at the top position
# Table4 <- add_header(x = Table4, ml_cm = Condition1, SE = Condition1, n = Condition1, ml_cm_b = Condition2, SE_b = Condition2, n_b = Condition2, Difference = "Comparison", p_value = "Comparison", top = TRUE )
# 
# # merge horizontally when there are identical values
# Table4 <- merge_h(Table4, part = "header")
# 
# Table4 <- Table4 %>%
#   autofit() %>%
#   theme_booktabs() %>%
#   bold(bold = TRUE, part = "header") %>%
#   align(align = "left", part = "all") %>%
#   align(i = 1, j = c(1:9), align = "center", part = "header") %>%
#   compose(i = 2, j = c(2:9), part = "header", value = as_paragraph(c("Mean)", "SE","n","Mean","SE","n", "Difference.", "P value")))
# 
# return(Table4)
# 

}




#####==================================================#####
###================== FUNCTION FIVE ====================###
###=============== SPECIES_COMPARISON ===================###
#####==================================================#####


SpeciesComparison4 <- function(Data, Group, Group2, Group3, Quan_var, Condition1, Condition2, names, Join, variable, by, ...){

# Data - (Lengths or weights), Group (Scientific name), Group2(Time period or fishing depth - binomial variables), Quan_var(variable being measured e.g. length/weight), Condition 1 and 2 (time period or fishing depth e.g. day/night or deep/shallow - "String objects"), names (vector of scientific names), Test (the object name for your T-test output), Group 3 (Trip.x), variable(t-test - means for the quantitative variables to be tested), by(t-test- Condition to tested against)

Table1 <- Data %>%
  group_by({{ Group }}, {{ Group2 }}) %>%
  dplyr::summarise(m_len = round(mean({{ Quan_var }})), s_e = round(std.error({{ Quan_var }}),2), Count = n(), .groups = 'drop')

Table2 <- Table1 %>%
  dplyr::filter({{ Group2 }} == Condition1)
Table3 <- Table1 %>%
  dplyr::filter({{ Group2 }} == Condition2)

Table4  <- dplyr::left_join(Table2, Table3, by= Join)

colnames(Table4) <- c("Sceintific_names", "Time_period", "ml_cm", "SE", "n", "Time_period_b", "ml_cm_b", "SE_b", "n_b")

Table4 <- Table4 %>% select(-c(2,6)) %>%
  dplyr::filter(Sceintific_names %in% names)

Table4 <- Table4 %>%
  mutate(Difference = round(((ml_cm - ml_cm_b)/ml_cm)*100,2))

Table4 <- Table4 %>%
  slice(match(names, Sceintific_names))

#####=====================T-TEST STARTS=======================######

#####=================== SPECIES ONE =========================#####

Species1 <- Data %>%
  dplyr::filter({{ Group }} == names[1]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }}), .groups = 'drop') %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES TWO =========================#####

Species2 <- Data %>%
  filter({{ Group }} == names[2]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }}), .groups = 'drop') %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES THREE =========================#####

Species3 <- Data %>%
  filter({{ Group }} == names[3]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }}), .groups = 'drop') %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES FOUR =========================#####

Species4 <-  Data %>%
  filter({{ Group }} == names[4]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }}), .groups = 'drop') %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))


#####================ SPECIES PAIRED T-TESTS ====================#####

exp1 <- expr(!!ensym(variable) ~ !!ensym(by))

T_test1 <- t.test(formula = eval(exp1), data = Species1,...)
T_test2 <- t.test(formula = eval(exp1), data = Species2,...)
T_test3 <- t.test(formula = eval(exp1), data = Species3,...)
T_test4 <- t.test(formula = eval(exp1), data = Species4,...)



#####================ SPECIES LENGTH TABLE ====================#####

Results <- map_df(list(T_test1, T_test2, T_test3, T_test4), tidy)

#####=======================T-TEST ENDS========================######


p_value <- round(Results$p.value,5)

Table4 <- cbind(Table4, p_value)

Table4 <- flextable(Table4, col_keys = c("Sceintific_names", "ml_cm", "SE", "n", "ml_cm_b", "SE_b", "n_b", "Difference", "p_value"))

# start with no header
Table4 <- delete_part(Table4, part = "header")

# add a line of row
Table4 <- add_header(x = Table4, Sceintific_names = "Species", ml_cm = "Mean_length_cm", SE = "SE", n = "n", ml_cm_b = "Mean_length_cm_n", SE_b = "SE_n", n_b = "n_n", Difference = "Difference", p_value = "p_value", top = FALSE)

# add another line of row at the top position
Table4 <- add_header(x = Table4, ml_cm = Condition1, SE = Condition1, n = Condition1, ml_cm_b = Condition2, SE_b = Condition2, n_b = Condition2, Difference = "Comparison", p_value = "Comparison", top = TRUE )

# merge horizontally when there are identical values
Table4 <- merge_h(Table4, part = "header")

Table4 <- Table4 %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(bold = TRUE, part = "header") %>%
  align(align = "left", part = "all") %>%
  align(i = 1, j = c(1:9), align = "center", part = "header") %>%
  compose(i = 2, j = c(2:9), part = "header", value = as_paragraph(c("Mean)", "SE","n","Mean","SE","n", "Difference.", "P value")))

return(Table4)
}


#####==================================================#####
###================== FUNCTION FIVE ====================###
###=============== SPECIES_COMPARISON ===================###
#####==================================================#####

SpeciesComparison3 <- function(Data, Group, Group2, Quan_var, Condition1, Condition2, Join, Group3, names, variable, by, env = parent.frame(),...){

# Data - (Lengths or weights), Group (Scientific name), Group2(Time period or fishing depth - binomial variables), Quan_var(variable being measured e.g. length/weight), Condition 1 and 2 (time period or fishing depth e.g. day/night or deep/shallow - "String objects"), names (vector of scientific names), Test (the object name for your T-test output), Group 3 (Trip.x), variable(t-test - means for the quantitative variables to be tested), by(t-test- Condition to tested against)

Table1 <- Data %>%
  group_by({{ Group }}, {{ Group2 }}) %>%
  dplyr::summarise(m_len = round(mean({{ Quan_var }})), s_e = round(std.error({{ Quan_var }}),2), Count = n(), .groups = 'drop') %>%
  filter(duplicated({{ Group }}) | duplicated({{ Group }}, fromLast=TRUE))

Table2 <- Table1 %>%
  filter({{ Group2 }} == Condition1)
Table3 <- Table1 %>%
  filter({{ Group2 }} == Condition2)

Table4  <- dplyr::left_join(Table2, Table3, by= Join)

colnames(Table4) <- c("Sceintific_names", "Time_period", "ml_cm", "SE", "n", "Time_period_b", "ml_cm_b", "SE_b", "n_b")

Table4 <- Table4 %>% select(-c(2,6)) %>%
  filter(Sceintific_names %in% names)

Table4 <- Table4 %>%
  mutate(Difference = round(((ml_cm - ml_cm_b)/ml_cm)*100,2))

Table4 <- Table4 %>%
  slice(match(names, Sceintific_names))

#####=====================T-TEST STARTS=======================######

#####=================== SPECIES ONE =========================#####

Species1 <- Data %>%
  dplyr::filter({{ Group }} == names[1]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>% 
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES TWO =========================#####

Species2 <- Data %>%
  filter({{ Group }} == names[2]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>%
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES THREE =========================#####

Species3 <- Data %>%
  filter({{ Group }} == names[3]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>%
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES FOUR =========================#####

Species4 <-  Data %>%
  filter({{ Group }} == names[4]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>%
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES FIVE =========================#####

Species5 <- Data %>%
  filter({{ Group }} == names[5]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>%
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES SIX =========================#####

Species6 <- Data %>%
  filter({{ Group }} == names[6]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  # ungroup() %>%
  subset(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

exp1 <- expr(!!ensym(variable) ~ !!ensym(by))

T_test1 <- t.test(formula = eval(exp1), data = Species1,...)
T_test2 <- t.test(formula = eval(exp1), data = Species2,...)
T_test3 <- t.test(formula = eval(exp1), data = Species3,...)
T_test4 <- t.test(formula = eval(exp1), data = Species4,...)
T_test5 <- t.test(formula = eval(exp1), data = Species5,...)
T_test6 <- t.test(formula = eval(exp1), data = Species6,...)


#####================ SPECIES LENGTH TABLE ====================#####

Results <- map_df(list(T_test1, T_test2, T_test3, T_test4, T_test5, T_test6), tidy)

#####=======================T-TEST ENDS========================######


p_value <- round(Results$p.value,5)

Table4 <- cbind(Table4, p_value)

Table4 <- flextable(Table4, col_keys = c("Sceintific_names", "ml_cm", "SE", "n", "ml_cm_b", "SE_b", "n_b", "Difference", "p_value"))

# start with no header
Table4 <- delete_part(Table4, part = "header")

# add a line of row
Table4 <- add_header(x = Table4, Sceintific_names = "Species", ml_cm = "Mean_length_cm", SE = "SE", n = "n", ml_cm_b = "Mean_length_cm_n", SE_b = "SE_n", n_b = "n_n", Difference = "Difference", p_value = "p_value", top = FALSE)

# add another line of row at the top position
Table4 <- add_header(x = Table4, ml_cm = Condition1, SE = Condition1, n = Condition1, ml_cm_b = Condition2, SE_b = Condition2, n_b = Condition2, Difference = "Comparison", p_value = "Comparison", top = TRUE )

# merge horizontally when there are identical values
Table4 <- merge_h(Table4, part = "header")

Table4 <- Table4 %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(bold = TRUE, part = "header") %>%
  align(align = "left", part = "all") %>%
  align(i = 1, j = c(1:9), align = "center", part = "header") %>%
  compose(i = 2, j = c(2:9), part = "header", value = as_paragraph(c("Mean)", "SE","n","Mean","SE","n", "Difference.", "P value")))

return(Table4)

}
