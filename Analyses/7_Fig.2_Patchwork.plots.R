# Biblioteca
library(patchwork)
library(ggplot2)
library(ggtext)
library(here)

#Establish file location
i_am("Analyses/7_Fig.2_Patchwork.plots.R")

Fig.2A <- readRDS(here("Graphics", "RDS.plots", "Figure.2A_All.years.spiders.rds"))
Fig.2B <- readRDS(here("Graphics", "RDS.plots", "Figure.2B_Multinom.all.years.rds"))

combined.plot <- (Fig.2A|Fig.2B) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 18))

combined.plot


# Save combined figure
ggsave(filename = here("Graphics", "Fig.2_Full.plot.1.png"),
       plot = combined.plot,
       width = 8.8,
       height = 5.5,
       dpi = 600)
