# summarising each group down to one row 

# function title 
feeding_overalldiet_calculations <- function(data, group_col) {
  summary <- data %>%
    group_by({{ group_col }}) %>%
    summarise(mean = mean(fly_numbers),
              sd = sd(fly_numbers),
              n = n(),
              se = sd/sqrt(n))
  return(summary)
}

# using function with one line of code 
exp1bd1feeding <- feeding_overalldiet_calculations(long_feedinge1bd1, diet)
exp1bd2feeding <- feeding_overalldiet_calculations(long_feedinge1bd2, diet)
exp2d1feeding <- feeding_overalldiet_calculations(long_feedinge2d1, diet)
exp2d2feeding <- feeding_overalldiet_calculations(long_feedinge2d2, diet)

