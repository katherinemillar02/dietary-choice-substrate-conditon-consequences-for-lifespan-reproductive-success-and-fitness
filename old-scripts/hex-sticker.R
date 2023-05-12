library(ggplot2)
library(hexSticker)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()

foodhardness_boxplot_exp2_feeding_s <- foodhardness_boxplot_exp2_feeding + theme_void() + theme_transparent()

sticker <- sticker(foodhardness_boxplot_exp2_feeding_s, package="FoodConditionPlot", p_size=10, s_x=1.1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/ggplot2.png")

print(sticker)
