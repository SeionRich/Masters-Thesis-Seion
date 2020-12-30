
#####==================================================#####
###================== FUNCTION FIVE ====================###
###=============== SPECIES_COMPARISON ===================###
#####==================================================#####

SpeciesComparison3 <- function(Data, Group, Group2, Quan_var, Condition1, Condition2, Join, Group3, names, variable, by, env = parent.frame(),...){
     
     # 
     
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

# Loop1 <- c(1,2,3,4,5,6)
# 
# for (i in Loop1){
# 
#   species_name = paste0("Species",i)
# 
#   assign(species_name, eval({{ Data }} %>% filter({{ Group }} == names[i]) %>%
#   select({{ Group }}, {{ Group2 }}, {{ Quan_var }}))) # %>% print()
# 
# }

Species1 <- Data %>%
  dplyr::filter({{ Group }} == names[1]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  dplyr::group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
ungroup() %>%
  filter(duplicated({{ Group3 }})|duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES TWO =========================#####

Species2 <- Data %>%
  filter({{ Group }} == names[2]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES THREE =========================#####

Species3 <- Data %>%
  filter({{ Group }} == names[3]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES FOUR =========================#####

Species4 <-  Data %>%
  filter({{ Group }} == names[4]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES FIVE =========================#####

Species5 <- Data %>%
  filter({{ Group }} == names[5]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

#####=================== SPECIES SIX =========================#####

Species6 <- Data %>%
  filter({{ Group }} == names[6]) %>%
  select({{ Group }}, {{ Group2 }}, {{ Group3 }}, {{ Quan_var }}) %>%
  group_by({{ Group3 }}, {{ Group2 }}) %>%
  dplyr::summarise(mn_ln = mean({{ Quan_var }})) %>%
  ungroup() %>%
  filter(duplicated({{ Group3 }}) | duplicated({{ Group3 }}, fromLast=TRUE))

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