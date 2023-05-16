# creating a summary function for the overall diet calculations of the ovipositon counts 

# this function is for the overall diet calculations 
oviposition_overalldiet_calculations <- function(data, group_col) {
  summary <- data %>% 
    group_by({{group_col}}) %>% 
    summarise(mean = mean(egg_numbers),
              sd = sd (egg_numbers),
              n = n (),
              min = min (egg_numbers),
              max = max (egg_numbers),
              iqr = IQR (egg_numbers),
              se = sd/sqrt(n))
  return(summary)
}

# using the function of the overall diet calculations to minimise lines of code 
exp1boviposition <- oviposition_overalldiet_calculations(long_egg_counting1b, diet)
exp2oviposition <- oviposition_overalldiet_calculations(long_egg_counting2, diet)
exp3oviposition <- oviposition_overalldiet_calculations(long_egg_counting3, diet)


# this function is for the specific food hardness calculations
oviposition_foodtype_calculations <- function(data, group_col) {
  summary <- data %>% 
    group_by({{group_col}}) %>% 
    summarise(mean = mean(egg_numbers),
              sd = sd (egg_numbers),
              n = n (),
              min = min (egg_numbers),
              max = max (egg_numbers),
              iqr = IQR (egg_numbers),
              se = sd/sqrt(n))
  return(summary)
}

# using this function of the food type calculations to minimise lines of code 
exp1bovipositionhardness <- oviposition_foodtype_calculations(long_egg_counting1b, food_type)
exp2ovipositionhardness <- oviposition_foodtype_calculations(long_egg_counting2, food_type)
exp3ovipositionhardness <- oviposition_foodtype_calculations(long_egg_counting3, food_type)

# this function if for the specific nutrient composition calculations
oviposition_nutrientcomposition_calculations <- function(data, group_col) {
  summary <- data %>% 
    group_by({{group_col}}) %>% 
    summarise(mean = mean(egg_numbers),
              sd = sd (egg_numbers),
              n = n (),
              min = min (egg_numbers),
              max = max (egg_numbers),
              iqr = IQR (egg_numbers),
              se = sd/sqrt(n))
  return(summary)
}

# using this function of the nutrient compositions to minimise lines of code 
exp1bovipositionnutrient <- oviposition_foodtype_calculations(long_egg_counting1b, food_nutrition)
exp2ovipositionnutrient <- oviposition_foodtype_calculations(long_egg_counting2, food_nutrition)
exp3ovipositionnutrient <- oviposition_foodtype_calculations(long_egg_counting3, food_nutrition)

