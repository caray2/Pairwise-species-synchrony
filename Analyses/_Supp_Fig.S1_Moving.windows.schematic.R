
#Biblioteca

library(ggplot2)
library(dplyr)
library(here)

# Create data for the windows
windows_df <- data.frame(
  window = c("Full experiment time period", 
             paste("Window", 1:12)),
  start = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  end = c(18, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  y_pos = c(13, 12:1),
  fill_color = c("darkseagreen3", rep("lightblue3", 12)))  # Different color for first row

# Create year labels data
years_df <- data.frame(
  year = 2006:2024,
  x_pos = 0:18)

# Create Y labels data
y_labels_df <- data.frame(
  y_label = paste0("Y", 0:18),
  x_pos = 0:18)

# Pre-treatment column shading
pretreat_df <- data.frame(xmin = -0.5, xmax = 0.5,
                          ymin = 0.5, ymax = 13.5)

# Create the plot
window_plot <- ggplot() +
  #Pre-treatment column
  geom_rect(data = pretreat_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "lightpink", color = NA, alpha = 0.8) +
  # Add vertical grid lines
  geom_vline(xintercept = seq(1.5, 18.5, by = 1), 
             color = "gray25", linewidth = 0.3, linetype = "solid") +
  # Add the full and moving window bars
  geom_rect(data = windows_df,
            aes(xmin = start - 0.5, xmax = end + 0.5, 
                ymin = y_pos - 0.45, ymax = y_pos + 0.45,
                fill = fill_color),  # Use fill_color column
            alpha = 1, color = "black", linewidth = 0.5) +
  #Dividing line separating pre-treatment from treatment period
  geom_vline(xintercept = 0.5,
             color = "black", linewidth = 0.5, linetype = "solid") +
  # Add top horizontal line
  geom_hline(yintercept = 16, 
             color = "black", linewidth = 2.5, linetype = "solid") +
  # Add horizontal line under the Y lable info 
  geom_hline(yintercept = 15, 
             color = "black", linewidth = 0.5, linetype = "solid") +
  # Add horizontal line under the year info 
  geom_hline(yintercept = 13.45, 
             color = "black", linewidth = 1.0, linetype = "solid") +
  # Add bottom horizontal line
  geom_hline(yintercept = 0.5, 
             color = "black", linewidth = 2.5, linetype = "solid") +
  # Add vertical left side line 
  geom_vline(xintercept = -0.5, 
             color = "black", linewidth = 2.5, linetype = "solid") +
  # Add vertical right side line 
  geom_vline(xintercept = 18.5, 
             color = "black", linewidth = 2.5, linetype = "solid") +
  scale_fill_identity() +  # Use the actual color values
  # Add window labels
  geom_text(data = windows_df,
            aes(x = start - 0.3, y = y_pos, label = window),
            hjust = 0, size = 5, fontface = "bold", color = "black") +  
  # Add year labels at top
  geom_text(data = years_df,
            aes(x = x_pos, y = 14.3, label = year),
            size = 4.5, fontface = "bold", angle =90) +
  # Add Y labels (e.g. Y0, Y2)
  geom_text(data = y_labels_df,
            aes(x = x_pos, y = 15.4, label = y_label),
            size = 4.5, 
            fontface = "bold") +
  #Pre-treatment label
  annotate("text", x = 0, y = 7, label = "Pre-treatment",
           size = 5, fontface = "bold.italic", color = "black",
           hjust = 0.5, vjust = 0.5, lineheight = 0.9, angle = 90) +
  # Styling
  scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 18.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 16)) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA))

window_plot

# Save the plot
setwd("H:/My Drive/Synchrony/Graphics")
#ggsave("Fig.S1_moving.window.diagram.png", window_plot, 
 #      width = 8, height = 6, dpi = 600, bg = "white")


# Streamlined for poster

# Create data for the windows
windows_df <- data.frame(
  window = c("Full experiment time period", 
             paste("Window", 1:12)),
  start = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  end = c(18, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  y_pos = seq(13, 3, length.out = 13),
  fill_color = c("darkseagreen3", rep("lightblue3", 12)))

# Create year labels data
years_df <- data.frame(
  year = 2007:2024,
  x_pos = 1:18)

window_plot <- ggplot() +
  # Add vertical grid lines (clipped to plot area only)
  geom_segment(data = data.frame(x = seq(1.5, 18.5, by = 1)),
               aes(x = x, xend = x, y = 2.65, yend = 13.36),
               color = "gray25", linewidth = 0.3) +
  # Add the full and moving window bars
  geom_rect(data = windows_df,
            aes(xmin = start - 0.5, xmax = end + 0.5, 
                ymin = y_pos - 0.35, ymax = y_pos + 0.35,
                fill = fill_color),
            alpha = 1, color = "black", linewidth = 0.5) +
  # Add top horizontal line
  geom_segment(aes(x = 0.5, xend = 18.5, y = 13.36, yend = 13.36),
               color = "black", linewidth = 1) +
  # Add bottom horizontal line
  geom_segment(aes(x = 0.5, xend = 18.5, y = 2.65, yend = 2.65),
               color = "black", linewidth = 2) +
  # Add vertical left side line 
  geom_segment(aes(x = 0.5, xend = 0.5, y = 2.65, yend = 13.36),
               color = "black", linewidth = 2) +
  # Add vertical right side line 
  geom_segment(aes(x = 18.5, xend = 18.5, y = 2.65, yend = 13.36),
               color = "black", linewidth = 2) +
  scale_fill_identity() +
  # Add window labels
  geom_text(data = windows_df,
            aes(x = start - 0.3, y = y_pos, label = window),
            hjust = 0, size = 5, fontface = "bold", color = "black") +
  # Add year labels at top
  geom_text(data = years_df,
            aes(x = x_pos, y = 14.1, label = year),
            size = 4.5, fontface = "bold", angle = 90) +
  # Styling
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, 18.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(2.65, 14.8)) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA))

window_plot

setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Schem.pdf", plot = window_plot, width = 8, height = 6)

####
#Only full time period

# # Filter for just the full time period
# full_period_df <- windows_df %>% filter(window == "Full time period")
# 
# full_period_df$y_pos <-0.6 #Move it to the bottom
# 
# # Create the plot
# window_plot.full <- ggplot() +
#   # Add vertical grid lines
#   geom_vline(xintercept = seq(0.5, 18.5, by = 1), 
#              color = "gray25", linewidth = 0.3, linetype = "solid") +
#   # Add the window bars
#   geom_rect(data = full_period_df,
#             aes(xmin = start - 0.5, xmax = end + 0.5, 
#                 ymin = y_pos - 0.1, ymax = y_pos  + 0.1,
#                 fill = fill_color),  # Use fill_color column
#             alpha = 1, color = "black", linewidth = 0.5) +
#   # Add top horizontal line
#   geom_hline(yintercept = 1, 
#              color = "black", linewidth = 2.5, linetype = "solid") +
#   # Add bottom horizontal line
#   geom_hline(yintercept = 0.5, 
#              color = "black", linewidth = 2.5, linetype = "solid") +
#   # Add vertical left side line 
#   geom_vline(xintercept = 0.5, 
#              color = "black", linewidth = 2.5, linetype = "solid") +
#   # Add vertical right side line 
#   geom_vline(xintercept = 18.5, 
#              color = "black", linewidth = 2.5, linetype = "solid") +
#   scale_fill_identity() +  # Use the actual color values
#   # Add window labels
#   geom_text(data = full_period_df,
#             aes(x = start - 0.3, y = y_pos + 0.02, label = window),
#             hjust = 0, size = 5.5, fontface = "bold", color = "black") +  
#   # Add year labels at top
#   geom_text(data = years_df,
#             aes(x = x_pos, y = 0.85, label = year),
#             size = 5, fontface = "bold") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0.5, 18.5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1)) +
#   theme_void() +
#   theme(
#     plot.margin = margin(10, 10, 10, 10),
#     plot.background = element_rect(fill = "white", color = NA))
# 
# 
# window_plot.full
# 
# # Save the full period plot
# 
# setwd("H:/My Drive/Synchrony/Graphics/For.presentations")
# ggsave("Fig.S3a_full.period.only.png", window_plot.full, 
#        width = 12, height = 0.9, dpi = 600, bg = "white")
