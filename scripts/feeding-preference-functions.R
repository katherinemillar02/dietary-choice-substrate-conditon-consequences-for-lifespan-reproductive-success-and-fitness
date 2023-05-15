
# playing around 
function_name <- function(arg_1, arg_2, ...) {
}
print(mean(25:82))
# summarising each group down to one row 
# looking at dplyr website "summarise group down to one row" 

# function  for the overall diet calculations
feeding_overalldiet_calculations <- function(data, group_col) {
  summary <- data %>%
    group_by({{ group_col }}) %>%
    summarise(mean = mean(fly_numbers),
              sd = sd(fly_numbers),
              n = n(),
              min = min(fly_numbers),
              max = max(fly_numbers),
              iqr = IQR(fly_numbers),
              se = sd/sqrt(n))
  return(summary)
}


# using function with one line of code of the overall diet calculations
exp1bd1feeding <- feeding_overalldiet_calculations(long_feedinge1bd1, diet)
exp1bd2feeding <- feeding_overalldiet_calculations(long_feedinge1bd2, diet)
exp2d1feeding <- feeding_overalldiet_calculations(long_feedinge2d1, diet)
exp2d2feeding <- feeding_overalldiet_calculations(long_feedinge2d2, diet)
exp3d1feeding <- feeding_overalldiet_calculations(long_feedinge3d1, diet)
exp3d2feeding <- feeding_overalldiet_calculations(long_feedinge3d2, diet)

# function for the food hardness calculations 
feeding_hardness_calculations <- function(data, group_col) {
  summary <- data %>%
    group_by({{ group_col }}) %>%
    summarise(mean = mean(fly_numbers),
              sd = sd(fly_numbers),
              n = n(),
              min = min(fly_numbers),
              max = max(fly_numbers),
              iqr = IQR(fly_numbers),
              se = sd/sqrt(n))
  return(summary)
}

# making the summaries into one line of code 
# maybe go back and do individual days as well 
exp1bfeedinghardness <- feeding_hardness_calculations(exp1ball, food_type)
exp2feedinghardness <- feeding_hardness_calculations(exp2_combined, food_type)
exp3d1feedinghardness <- feeding_hardness_calculations(long_feedinge3d1, food_type)
exp3d2feedinghardness <- feeding_hardness_calculations(long_feedinge3d2, food_type)

# function for the nutrient composition calculations 
feeding_nutrient_calculations <- function(data, group_col) {
  summary <- data %>%
    group_by({{ group_col }}) %>%
    summarise(mean = mean(fly_numbers),
              sd = sd(fly_numbers),
              n = n(),
              min = min(fly_numbers),
              max = max(fly_numbers),
              iqr = IQR(fly_numbers),
              se = sd/sqrt(n))
  return(summary)
}


# maybe go back and do individual days as well 
exp1bfeedinghardness <- feeding_hardness_calculations(exp1ball, food_type)
exp2feedinghardness <- feeding_hardness_calculations(exp2_combined, food_type)
exp3d1feedinghardness <- feeding_hardness_calculations(long_feedinge3d1, food_type)
exp3d2feedinghardness <- feeding_hardness_calculations(long_feedinge3d2, food_type)



