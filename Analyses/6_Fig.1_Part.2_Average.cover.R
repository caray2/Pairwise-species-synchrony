#Plotting average cover figure for panel 1

#
# Biblioteca
#

library(ggplot2)
library(dplyr)
library(here)

set.seed(7220)

#Open post-cleaning tidy hit data that is only the focal species
setwd("H:/My Drive/Synchrony/Data/Tidy")
cover <- read.csv("focal.cover.csv")


# Lets remove the pre-treatment data 2006, before any treatments were implemented
cover.2007_2025 <- cover %>% filter(year!=2006)


head(cover.2007_2025)

# Calculate average cover by species
avg.cover <-
  cover.2007_2025 %>%
  group_by(NWT_code)  %>%
  summarize(avg_cover = round(mean(hits), 2))

avg.cover

sprintf("%.2f", avg.cover$avg_cover)

#Add the genus names to avg.cover
genus.ref <- data.frame(NWT_code = c('DESCES', 'GEUROS', 'ARTSCO', 'CARSCO', 
                                     'BISBIS', 'TRIPAR', 'GENALG', 'CALLEP'),
                        genus = c('Deschampsia', 'Geum', 'Artemisia', 'Carex', 
                                  'Bistorta', 'Trifolium', 'Gentiana', 'Caltha'))


avg.cover <- avg.cover %>%
  left_join(genus.ref, by = "NWT_code")

#Make genus a factor, with the level ordered by decreasing cover
avg.cover$genus <- factor(avg.cover$genus, 
                          levels = avg.cover$genus[order(avg.cover$avg_cover, decreasing = TRUE)])

# Barplot
Cover.bar <-
  avg.cover %>%
  ggplot(aes(x=genus, y=avg_cover, fill=genus)) +
  geom_bar(stat='identity', width=0.8) +
  labs(y = "Average plot cover\n across treatments", 
       x = NULL) +
  scale_fill_manual(values = c("Deschampsia" = "chartreuse4",
                               "Carex"       = "chartreuse4",
                               "Geum"        = "plum3",
                               "Artemisia"   = "plum3",
                               "Bistorta"    = "plum3",
                               "Trifolium"   = "plum3",
                               "Gentiana"    = "plum3",
                               "Caltha"      = "plum3")) +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(face = "italic",
                                   vjust = 0,
                                   hjust=0,
                                   size=22), 
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20)) +
  theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(limits = c(0, 70), 
                     breaks = seq(0, 70, 20))

Cover.bar

#Save graphic
setwd("H:/My Drive/Synchrony/Graphics")
ggsave(plot=Cover.bar, "Fig.1C_Average.cover.png", height=5, width=6, units="in", dpi=600)

