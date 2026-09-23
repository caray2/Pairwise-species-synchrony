
# Biblioteca

library(patchwork)
library(ggplot2)
library(here)

setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
vr.plot <- readRDS("Figure.5A.rds")
q1.2006.plot <- readRDS("Figure.5B.rds")
q1.temporal.plot <- readRDS("Figure.5C.rds")
q2.2006.plot <- readRDS("Figure.5D.rds")
q2.temporal.plot <- readRDS("Figure.5E.rds")
legend <- readRDS("Figure.5.shared.legend.rds")

# Reduce top and bottom margins for each row

#Row 1
vr.plot <- vr.plot + 
  theme(plot.margin = margin(t = 3, b = 3, r = 5, l = 5))

#Row 2
q1.2006.plot <- q1.2006.plot + 
  theme(plot.margin = margin(t = 3, b = 3, r = 7, l = 5)) #increase right

q1.temporal.plot <- q1.temporal.plot + 
  theme(plot.margin = margin(t = 3, b = 3, r = 5, l = 7)) #increase left

#Row 3
q2.2006.plot <- q2.2006.plot +
  theme(plot.margin = margin(t = 3, b = 3, r = 7, l = 5)) #increase right

q2.temporal.plot <- q2.temporal.plot + 
  theme(plot.margin = margin(t = 3, b = 3, r = 5, l = 7)) #increase left


# Combine plots using patchwork
row2 <- (q1.2006.plot | q1.temporal.plot) + plot_layout(widths = c(1, 3))
row3 <- (q2.2006.plot | q2.temporal.plot) + plot_layout(widths = c(1, 3))

combined.plot <- vr.plot / row2 / row3 / legend +
  plot_layout(heights = c(1, 1, 1, 0.3)) 

combined.plot

# Save combined figure
setwd("H:/My Drive/Synchrony/Graphics")

ggsave("Fig.5_VR_q1_q2.png", combined.plot,
       width = 14, height = 15, dpi = 600)
