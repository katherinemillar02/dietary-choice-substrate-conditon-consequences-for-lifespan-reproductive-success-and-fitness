

long_feedinge1d2$food_type <- ifelse(long_feedinge1d2$diet %in% c("1:8H", "1:2H"), "hard", "soft")
long_feedinge1d2$food_nutrition <- ifelse(long_feedinge1d2$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")
