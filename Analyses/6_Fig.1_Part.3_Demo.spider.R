
######## #
#Analyses
######## #

#
# Biblioteca
#

library(igraph)
library(ggraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggnewscale)
library(ggtext)


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

#Only considering Nitrogen since it has lots of edges to make mock data with
effects_df <- inx %>%
  select(!c(lowerCI, upperCI, window, end.year, pair, bn)) %>% #remove unneeded columns
  left_join(dat, by = c("pair1" = "name")) %>% #add circle coordinates for 1st pair
  rename(xstart = x, ystart = y) %>% #rename x and y to show that they are for the 1st pair
  left_join(dat, by = c("pair2" = "name")) %>% #add circle coordinates for 2nd pair
  rename(xend = x, yend = y) %>% #rename x and y to show that they are for the 2nd pair
  filter(treatment=='XXW') %>%
  mutate(node_color = case_when(
    treatment == "XXW" ~ "grey40"), 
    node_shape = ifelse(
      pair1 == "DESCES", 15, 16 #For Desces we what the shape to be a square, everything else a circle
    )) %>%
  mutate(node_color = ifelse(pair1 == "DESCES", "black", node_color)) %>% #Desces should be black, all else the treatment color
  mutate(VR.sig = ifelse(
    (sig.synch == TRUE | sig.comp == TRUE), VR, NA # Set the variance ratio to NA for non significant affiliations so they are not plotted
  )) %>%
  mutate(node_size = ifelse(node_shape == 16, 5, 4.5))  # Larger circles, normal squares


#Use the max and min variance ratio to set edge color scale. Using a standardized value across plots/scripts
min(inx$VR) 
max(inx$VR)

minVR <- 0.525
maxVR <- 1.385


# HTML formatting to give custom color headings to the plot
effects_df <- effects_df %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "<span style='color:grey40;'>Control</span>",
    treatment == "+ Snow" ~ "<span style='color:royalblue3;'>+ Snow</span>",
    treatment == "+ Nitrogen" ~ "<span style='color:goldenrod;'>+ Nitrogen</span>",
    treatment == "+ Warming" ~ "<span style='color:coral;'>+ Warming</span>",
    TRUE ~ as.character(treatment)  # Keep any other values as-is
  ))


# Mock.data <- effects_df %>%
#   mutate(VR.sig = if_else(pair1 == "CALLEP" & pair2 == "DESCES",  0.53, VR.sig)) #manually change some values for aesthestics


# Scramble sig variance ratios for mock data
Mock.data <- effects_df %>%
  mutate(VR.sig = sample(VR.sig),
         VR.sig = VR.sig + rnorm(n(), mean = 0, sd = 0.2)) %>%
  mutate(VR.sig = if_else(pair1 == "DESCES" & pair2 == "GENALG",
                          0.53, VR.sig)) %>% #manually change some values for aesthetics
  mutate(VR.sig = if_else(pair1 == "ARTSCO" & pair2 == "TRIPAR",
                          0.53, VR.sig)) %>% #manually change some values for aesthetics
  mutate(VR.sig = if_else(pair1 == "CALLEP" & pair2 == "DESCES",
                          NA, VR.sig)) #manually change some values for aesthetics

#Plot! The additional geom_point just to plot a node for tripar is due to a convention where pair 1 is earlier in the alphabet than pair 2. Thus, Tripar is never in the 'pair 1' position and is not plotted by xstart/ystart 

graph <- ggplot(Mock.data, aes(x=xstart, y=ystart, xend=xend, yend=yend)) + #sets axes for the circle
  geom_segment(data = Mock.data %>% drop_na(VR.sig), aes(color = VR.sig), linewidth =2.5) + #plot edges with non-NA sig-VR
  scale_colour_distiller(palette = "BrBG", direction = 1, limits = c(minVR, maxVR)) + #edge color
  new_scale_color() + #allows for a new scale color to be added after this line
  geom_point(aes(shape=as.factor(node_shape), color=node_color, size=node_size)) +
  scale_shape_manual(values = c("15"=15, "16"=16)) + #helps ggplot interpret our node_shape column
  geom_point(data = effects_df %>% filter(pair2 == "TRIPAR"), 
             aes(x=xend, y=yend, shape=as.factor(16), color=node_color), size=5)  + #plot Tripar node
  scale_size_identity() + #make ggplot use the provided sizes
  scale_color_identity() + #make ggplot use the provided colors
  coord_equal(clip = "off") + #Plot should be a square
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.text.y = element_blank(),
    strip.text.x = element_markdown(size = 14, face = "bold"),
    legend.position = "none",
  ) +
  guides(shape = "none", color = "none", size = "none")

graph


#save graphics
setwd("H:/My Drive/Synchrony/Graphics")
ggsave(plot=graph, "Fig.1D_demospider.png", height=3, width=3, units="in", dpi=800)

