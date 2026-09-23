# Biblioteca
library(patchwork)
library(jpeg)
library(png)
library(ggplot2)
library(grid)
library(magick)
library(here)

#JPGs - Read and upscale field photo
setwd("H:/My Drive/Synchrony/Graphics")
Fig.1B_img <- image_read("Fig.1B_Field.photo.cropped.jpg")
Fig.1B_img <- image_scale(Fig.1B_img, "3000")  # Upscale to 3000 pixels width
Fig.1B <- as.raster(Fig.1B_img)

#PNGs
setwd("H:/My Drive/Synchrony/Graphics")
Fig.1A <-readPNG("Fig.1A_Syn.vs.Comp.png")
Fig.1C <-readPNG("Fig.1C_With.plants.avg.cover.png")
Fig.1D <-readPNG("Fig.1D_With.plants.demo.plot.png")

# Convert photos to ggplot objects
gg.Fig.1A <- ggplot() + 
  annotation_custom(rasterGrob(Fig.1A, interpolate=TRUE)) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5)) +
  coord_fixed(ratio = 1)

gg.Fig.1B <- ggplot() + 
  annotation_custom(rasterGrob(Fig.1B, interpolate=TRUE), 
                    ymin = 0.05, ymax = 1.05) +  # Shift image up
  annotate("rect", 
           xmin = -0.048, xmax = 1.0472, 
           ymin = 0.166, ymax = 0.93,
           color = "black", fill = NA, linewidth = 1) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

gg.Fig.1C <- ggplot() + 
  annotation_custom(rasterGrob(Fig.1C, interpolate=TRUE)) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5)) +
  coord_fixed(ratio = 1)

gg.Fig.1D <- ggplot() + 
  annotation_custom(rasterGrob(Fig.1D, interpolate=TRUE)) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5)) +
  coord_fixed(ratio = 1)

# Combine plots using patchwork
row1 <- (gg.Fig.1B | gg.Fig.1C) + plot_layout(heights = c(2, 1))
row2 <- (gg.Fig.1A | gg.Fig.1D) + plot_layout(heights = c(1.4, 1))

combined.plot <- row1 / row2 +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12),
    plot.tag.position = c(0.02, 0.96))

combined.plot

# Save combined figure
setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.1_Full.plot.png", combined.plot,
       width = 6.5, height = 6.5, dpi = 600)