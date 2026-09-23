######## #
#Analyses
######## #


#
# Biblioteca
#

library(igraph)
library(ggplot2)
library(ggraph)
library(dplyr)
library(tidyr)
library(ggnewscale)
library(ggtext)
library(here)

#Set seed
set.seed(7220)

#Set working directory
setwd("H:/My Drive/Synchrony/Data/Tidy")
inx <- read.csv("pairwise.synch.all.years.csv", as.is = TRUE)

#structure for clusters
hierarchy <- data.frame(from=c( "Node",
                                "Node",
                                "Node",
                                "Node",
                                "Node",
                                "Node",
                                "Node",
                                "Node"),
                        to=c("ARTSCO",
                             "BISBIS",
                             "CALLEP",
                             "CARSCO",
                             "DESCES", 
                             "GENALG",
                             "GEUROS",
                             "TRIPAR"))

#Here we define the order of the plants around the spider plot by rank cover: 
# DESCES, GEUROS, ARTSCO, CARSCO, BISBIS, TRIPAR, GENALG, CALLEP

vertices <- data.frame(name=c("Node",
                              "DESCES",
                              "GEUROS",
                              "ARTSCO",
                              "CARSCO",
                              "BISBIS",
                              "TRIPAR",
                              "GENALG",
                              "CALLEP"))

mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)




#Set up circle plot. Filter = leaf keeps only the outer dots ('leaves') and removes the central dot.
c <- 
  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf)) +
  theme_void()

c

#Extract the coordinates for where each node is located around the circle
dat <- c$data[-1,] #The [-1} removes the first row which is the center point of the circle plot]

#Confirm that from the extracted coordinates that we still have a circle
plot(dat$x, dat$y)

#Remove unneeded columns
dat <- dat %>%
  select(-c(leaf, .ggraph.orig_index, circular, .ggraph.index))

#Define treatment levels
levels_trt <- c("Control", "+ Snow", "+ Nitrogen", "+ Warming")

effects_df <- inx %>%
  select(!c(lowerCI, upperCI, window, end.year, pair, bn)) %>%
  left_join(dat, by = c("pair1" = "name")) %>%
  rename(xstart = x, ystart = y) %>%
  left_join(dat, by = c("pair2" = "name")) %>%
  rename(xend = x, yend = y) %>%
  mutate(treatment2 = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow",
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"
  )) %>%
  select(-treatment) %>%
  rename(treatment = treatment2) %>%
  mutate(treatment = factor(treatment, levels=levels_trt)) %>%  # Factor with correct order
  mutate(node_color = case_when(
    treatment == "Control" ~ "grey40",
    treatment == "+ Warming" ~ "#ab3329",
    treatment == "+ Snow" ~ "royalblue", 
    treatment == "+ Nitrogen" ~ "#381a61"
    ), 
    node_shape = ifelse(
      pair1 == "DESCES", 15, 16
    )) %>%
  mutate(node_color = ifelse(pair1 == "DESCES", "black", node_color)) %>%
  mutate(VR.sig = ifelse(
    (sig.synch == TRUE | sig.comp == TRUE), VR, NA
  )) %>%
  mutate(node_size = ifelse(node_shape == 16, 3.5, 3)) %>%
  # Create display labels AFTER all logic is done
  mutate(treatment_label = case_when(
    treatment == "Control" ~ "<span style='color:grey40;'>Control</span>",
    treatment == "+ Warming" ~ "<span style='color:#ab3329;'>+ Warming</span>",
    treatment == "+ Snow" ~ "<span style='color:royalblue;'>+ Snow</span>",
    treatment == "+ Nitrogen" ~ "<span style='color:#381a61;'>+ Nitrogen</span>"
  )) %>%
  mutate(treatment_label = factor(treatment_label, levels=c(
    "<span style='color:grey40;'>Control</span>",
    "<span style='color:#ab3329;'>+ Warming</span>",
    "<span style='color:royalblue;'>+ Snow</span>",
    "<span style='color:#381a61;'>+ Nitrogen</span>"
    
  )))

#Use the max and min variance ratio to set edge color scale. Using a standardized value across plots/scripts
min(inx$VR) 
max(inx$VR)

minVR <- 0.525
maxVR <- 1.46


# HTML formatting to give custom color headings to the plot
effects_df <- effects_df %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "<span style='color:grey40;'>Control</span>",
    treatment == "+ Snow" ~ "<span style='color:royalblue;'>+ Snow</span>",
    treatment == "+ Nitrogen" ~ "<span style='color:goldenrod;'>+ Nitrogen</span>",
    treatment == "+ Warming" ~ "<span style='color:coral;'>+ Warming</span>",
    TRUE ~ as.character(treatment)  # Keep any other values as-is
  ))

#Plot! The additional geom_point just to plot a node for tripar is due to a convention where pair 1 is earlier in the alphabet than pair 2. Thus, Tripar is never in the 'pair 1' position and is not plotted by xstart/ystart 

graph <- ggplot(effects_df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) + #sets axes for the circle
  geom_segment(data = effects_df %>% drop_na(VR.sig), aes(color = VR.sig), linewidth =1.5) +
  scale_colour_gradientn(
    colours = c("#8c510a", "#f5ddb4", "black","#c7eae5", "#2d8d85"),
    values = c(0, 0.5, 0.51, 0.52, 1),
    limits = c(minVR, maxVR),
    breaks = c(0.6, 0.8, 1, 1.2, 1.4),
    labels = c(0.6, "", 1, "", 1.4),
    guide = guide_colorbar(title.position = "top",
                           title.hjust = 0.5,
                           frame.colour = "black",
                           ticks.colour = "black")) +
  labs(color = "Variance ratio") + #legend title for edges
  new_scale_color() + #allows for a new scale color to be added after this line
  geom_point(aes(shape=as.factor(node_shape), color=node_color, size=node_size)) +
  scale_shape_manual(values = c("15"=15, "16"=16)) + #helps ggplot interpret our node_shape column
  geom_point(data = effects_df %>% filter(pair2 == "TRIPAR"), 
             aes(x=xend, y=yend, shape=as.factor(16), color=node_color), size=3.5)  + #plot Tripar node
  scale_size_identity() + #make ggplot use the provided sizes
  scale_color_identity() + #make ggplot use the provided colors
  coord_equal(clip = "off") + #square treatment plots
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "grey98", color = NA),
    strip.text.y = element_blank(),
    strip.text.x = element_markdown(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    legend.frame = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(1.2, "lines")
  ) +
  guides(shape = "none", color = "none", size = "none") +
  facet_wrap(~treatment_label)

graph

setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.2A_All.years.spiders.png", graph, width = 2.9, height = 4.5, dpi = 600)

#Save as an RDS
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
saveRDS(graph, "Figure.2A_All.years.spiders.rds")
