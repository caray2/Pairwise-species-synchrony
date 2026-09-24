
######## #
#Analyses
######## #

#
# Biblioteca
#
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggraph)
library(ggnewscale)
library(ggtext)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/8_Fig.3_Moving.windows.spiders.select.years.R")

inx <- read.csv(here("Data", "Tidy", "pairwise.synch.window.7.years.csv"), as.is = T)

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

#set treatment levels
levels_trt <- c("Control", "+ Warming", 
                "+ Snow", "+ Nitrogen" )


effects_df <- inx %>%
  select(!c(lowerCI, upperCI, window, end.year, pair, bn)) %>% #remove unneeded columns
  left_join(dat, by = c("pair1" = "name")) %>% #add circle coordinates for 1st pair
  rename(xstart = x, ystart = y) %>% #rename x and y to show that they are for the 1st pair
  left_join(dat, by = c("pair2" = "name")) %>% #add circle coordinates for 2nd pair
  rename(xend = x, yend = y) %>% #rename x and y to show that they are for the 2nd pair
  mutate(treatment2 = case_when( #make a column with full treatment names
    treatment == "XXX" ~ "Control",
    treatment == "XXW" ~ "+ Warming",
    treatment == "PXX" ~ "+ Snow",
    treatment == "XNX" ~ "+ Nitrogen"
  )) %>%
  select(-treatment) %>% #remove coded treatment names
  rename(treatment = treatment2) %>%
  mutate(treatment = factor(treatment, levels=levels_trt)) %>%
  mutate(node_color = case_when(
    treatment == "Control" ~ "grey40",
    treatment == "+ Warming" ~ "#ab3329",
    treatment == "+ Snow" ~ "royalblue", 
    treatment == "+ Nitrogen" ~ "#381a61"), 
    node_shape = ifelse(
      pair1 == "DESCES", 15, 16 #For Desces we what the shape to be a square, everything else a circle
    )) %>%
  mutate(node_color = ifelse(pair1 == "DESCES", "black", node_color)) %>% #Desces should be black, all else the treatment color
  mutate(VR.sig = ifelse(
    (sig.synch == TRUE | sig.comp == TRUE), VR, NA # Set the variance ratio to NA for non significant affilations so they are not plotted
  )) %>%
  mutate(node_size = ifelse(node_shape == 16, 3.5, 3))  # Larger circles, normal squares


#Use the max and min variance ratio to set edge color scale. Using a standardized value across plots/scripts
min(inx$VR) 
max(inx$VR)

minVR <- 0.525
maxVR <- 1.46

df_select <- effects_df %>%
  filter(start.year %in% c(2007, 2010, 2013, 2016)) %>%
  mutate(treatment_html = case_when(
    treatment == "Control" ~ "<span style='color:grey40;'>Control</span>",
    treatment == "+ Warming" ~ "<span style='color:#ab3329;'>+ Warming</span>",
    treatment == "+ Snow" ~ "<span style='color:royalblue;'>+ Snow</span>",
    treatment == "+ Nitrogen" ~ "<span style='color:#381a61;'>+ Nitrogen</span>",
    TRUE ~ as.character(treatment)
  ))  %>%
  mutate(treatment_html = factor(treatment_html, levels = c(
    "<span style='color:grey40;'>Control</span>",
    "<span style='color:#ab3329;'>+ Warming</span>",
    "<span style='color:royalblue;'>+ Snow</span>",
    "<span style='color:#381a61;'>+ Nitrogen</span>"
  )))


graph <- ggplot(df_select, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
  geom_segment(data = df_select %>% drop_na(VR.sig), aes(color = VR.sig), size=1.5) +
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
  labs(color = "Variance ratio") +
  new_scale_color() +
  geom_point(aes(shape=as.factor(node_shape), color=node_color, size=node_size)) +
  scale_shape_manual(values = c("15"=15, "16"=16)) +
  geom_point(data = df_select %>% filter(pair2 == "TRIPAR"), 
             aes(x=xend, y=yend, shape=as.factor(16), color=node_color), size=3.5) +
  scale_size_identity() +
  scale_color_identity() +
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
    strip.text.y = element_text(size = 14, face = "bold"),
    strip.text.x = element_markdown(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.frame = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(2, "lines")
  ) +
  guides(shape = "none", color = "none", size = "none") +
  facet_grid(start.year ~ treatment_html, switch = "y")

graph

#Save plot
ggsave(here("Graphics", "Fig.3_moving.window.spiders.png"), graph, width = 7.5, height = 8.5, dpi = 600)