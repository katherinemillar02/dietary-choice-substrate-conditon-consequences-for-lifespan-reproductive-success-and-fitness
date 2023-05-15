# creating a summary function for the overall diet calculations of the ovipositon counts 
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

# using the function to minimise lines of code 
exp1boviposition <- oviposition_overalldiet_calculations(long_egg_counting1b, diet)
exp2oviposition <- oviposition_overalldiet_calculations(long_egg_counting2, diet)
exp3oviposition <- oviposition_overalldiet_calculations(long_egg_counting3, diet)