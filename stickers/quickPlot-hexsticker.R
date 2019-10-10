#install.packages("hexSticker")
#if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")

library(hexSticker)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto Slab", "roboto")
## Automatically use showtext to render text for future devices
showtext_auto()

#imgurl <- "https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png"

dir.create("inst/figures")

sticker(expression(plot(cars, cex = 0.5, cex.axis = 0.5, mgp = c(0, 0.3, 0), xlab = "", ylab = "")),
        package = "quickPlot",
        h_color = "darkred", h_fill = "#cccccc",
        p_color = "darkred", p_family = "bree", p_size = 22, p_x = 1, p_y = 1.55,
        s_x = 0.8, s_y = 0.75, s_width = 1.2, s_height = 1.2,
        url = "http://quickplot.predictiveecology.org", u_color = "#000000", u_size = 4,
        filename = "~/GitHub/PredictiveEcology/quickPlot/inst/figures/hexsticker.png", spotlight = FALSE)
