# Biblioteca
library(patchwork)
library(ggplot2)
library(ggtext)
library(here)

#Establish file location
i_am("Analyses/7_Fig.2_Patchwork.plots.R")

setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
Fig.2A <-readRDS("Figure.2A_All.years.spiders.rds")
Fig.2B <-readRDS("Figure.2B_Multinom.all.years.rds")

combined.plot <- (Fig.2A|Fig.2B) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 18))

combined.plot


# Save combined figure
setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.2_Full.plot.png", combined.plot,
       width = 8.8, height = 5.5, dpi = 600)