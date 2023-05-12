library(ggplot2)
library(hexSticker)



foodhardness_boxplot_exp2_feeding_s <- foodhardness_boxplot_exp2_feeding + theme_void() + theme_transparent()

sticker <- sticker(foodhardness_boxplot_exp2_feeding_s, package="FoodConditionPlot", p_size=9, s_x=1.1, s_y=.75, s_width=1.4, s_height=1,
        filename="images/ggplot2fhhex.png")

print(sticker)

