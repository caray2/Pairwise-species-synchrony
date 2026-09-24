#
# Biblioteca
#

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(performance)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/_10_Fig.5_Part.2_Q1.R")

hill.repl <- read.csv(here("Data", "Tidy", "hill.numbers.with.replicates.csv"), as.is = T)

str(hill.repl)

#Set order of treatments for all files so that it's control, warming, snow, N
hill.repl$treatment <- factor(hill.repl$treatment, 
                              levels = c('XXX', 'XXW', 'PXX', 'XNX'))

# Convert block and plot to a factor
hill.repl$block <- as.factor(hill.repl$block)

hill.repl$plot <- as.factor(hill.repl$plot)

#
# Hill numbers in 2006 (Pre treatment)
#

# Compare pre-treatment hill numbers. This uses non-average hill.number data just from 2006
hill.2006 <-hill.repl %>% filter(year == 2006)

# Q1 ANOVA
q1.2006.aov <- aov(q1 ~ treatment + Error(block), data = hill.2006)

# Is it singular?
check_singularity(q1.2006.aov)

print(summary(q1.2006.aov)) 
#In the pre-treatment plots, q1 does not differ between treatments


#
# Hill numbers through time!
#

#Now look at the temporal trends for q1 excluding the pre-treatment year of 2006
hill.temporal <- hill.repl %>% filter(year != 2006)
rm(hill.repl) #To avoid confusion

str(hill.temporal)


xtabs(~treatment + year, data = hill.temporal)

#Linear model for q1
lmm.q1.nested <- glmmTMB(q1 ~ treatment * n.year + (1|block/plot), data = hill.temporal,
        family = gaussian())

# Is it singular?
check_singularity(lmm.q1.nested)

# Check terms
check_singularity(lmm.q1.nested, check = "terms")

# Impose Gamma prior on random effects parameters
prior <- data.frame(
  prior = "gamma(1, 2.5)", # mean can be 1, but even 1e8
  class = "ranef") # for random effects


lmm.q1.priors <- update(lmm.q1.nested, priors = prior)

# Recheck for singularity
check_singularity(lmm.q1.priors)
# No singular fit

# remove singular model to avoid coding typos
rm(lmm.q1.nested)

# Summary
summary(lmm.q1.priors)

#this is the n.year est
round(-0.04559, 3)
round(0.01265,3)

# Look at fit
sim.q1.residuals <- simulateResiduals(fittedModel = lmm.q1.priors)
plot(sim.q1.residuals)

testDispersion(sim.q1.residuals)

# Looks really good!!

# #Assess significance of model parameters at year avg
# joint_tests(lmm.q1.priors) <-going to just use the model summary


#Compare treatments using emmeans in every year of the treatment
emm.q1 <- emmeans(lmm.q1.priors, ~ treatment | n.year,
                  at = list(n.year = c(1:18)))

emm.q1.contrasts <- pairs(emm.q1, adjust = "tukey")

emm.q1.contrasts

#Make a data frame for the q1 contrasts
emm.q1.contrasts.df <- as.data.frame(emm.q1.contrasts) %>%
  mutate(
    across(c(estimate, SE, z.ratio), ~round(., 3)),
    is.significant = ifelse(p.value < 0.05, 1, 0), 
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p.value, 3))),
    #Intuitive names for treatment contrasts
    contrast.name = case_match(as.character(contrast),
                               "XXX - XXW" ~ "Control - Warming",
                               "XXX - PXX" ~ "Control - Snow",
                               "XXW - PXX" ~ "Warming - Snow",
                               "XXX - XNX" ~ "Control - Nitrogen",
                               "XXW - XNX" ~ "Warming - Nitrogen",
                               "PXX - XNX" ~ "Snow - Nitrogen"))


#Add column of what kind of contrast was done
emm.q1.contrasts.df$contrast.type <-"Q1"

#Add column with more intuitive year values
emm.q1.contrasts.df$year <- emm.q1.contrasts.df$n.year + 2006

colnames(emm.q1.contrasts.df)

#Add column that combines what I'll report. %d for year / %f for stats values/ %s for text
emm.q1.contrasts.df$reporting <- ifelse(emm.q1.contrasts.df$is.significant == 1,
                sprintf("In %d, for the contrast %s (est = %.3f, SE = %.3f, p = %s)", 
                                                emm.q1.contrasts.df$year,
                                                emm.q1.contrasts.df$contrast.name,
                                                emm.q1.contrasts.df$estimate, 
                                                emm.q1.contrasts.df$SE, 
                                                emm.q1.contrasts.df$p.value),
                                        "") #otherwise blank

#Arrange columns
emm.q1.contrasts.df <- emm.q1.contrasts.df %>% 
  select("n.year", "year", "contrast.type", "contrast", "estimate", 
         "SE", "df", "z.ratio", "p.value", "is.significant", "contrast.name", "reporting")  

#Save dataframe
setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(emm.q1.contrasts.df, "Fig.5C_q1.treatment.effect.temporal.csv")

#visualize the model
#Treatment effect
emm.q1.df <- as.data.frame(emm.q1)

ggplot(emm.q1.df, aes(x = treatment, y = emmean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, linewidth = 1) +
  theme_minimal() +
  labs(title = "Estimated Marginal Means by Treatment in years 1 and 18",
       y = "Estimated Marginal Mean Q1",
       x = "Treatment") +
  scale_color_manual(values = c("XXX" = "grey40",
                                "XXW" = "#ab3329",
                                "PXX" = "royalblue",
                                "XNX" = "#381a61")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  coord_flip() +
  facet_wrap(~n.year)  +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "black", fill = "white", linewidth = 0.5))


#Compare slopes
# Get slopes of n.year for each treatment
q1.slopes <- emtrends(lmm.q1.priors, 
                      ~ treatment, 
                      var = "n.year")

q1.slopes  # View the slopes

# Compare slopes between treatments within each affiliation
q1.slope.pairs <- pairs(q1.slopes, reverse = T)
q1.slope.pairs

#Make a dataframe and save it
q1.slope.pairs.df <- as.data.frame(q1.slope.pairs) %>%
  mutate(
    across(c(estimate, SE, z.ratio), ~round(., 3)),
    is.significant = ifelse(p.value < 0.05, 1, 0), 
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p.value, 3))),
    #Intuitive names for treatment contrasts
    contrast.name = case_match(as.character(contrast),
                               "XXW - XXX" ~ "Warming - Control",
                               "PXX - XXX" ~ "Snow - Control",
                               "PXX - XXW" ~ "Snow - Warming",
                               "XNX - XXX" ~ "Nitrogen - Control",
                               "XNX - XXW" ~ "Nitrogen - Warming",
                               "XNX - PXX" ~ "Nitrogen - Snow"))

#Add column of what kind of contrast was done
q1.slope.pairs.df$contrast.type <-"q1.slope"

#Add column that combines what I'll report
q1.slope.pairs.df$reporting <- ifelse(q1.slope.pairs.df$is.significant == 1,
                                      sprintf("For the slope contrast for %s (est = %.3f, SE = %.3f, p = %s)", 
                                              q1.slope.pairs.df$contrast.name,
                                              q1.slope.pairs.df$estimate, 
                                              q1.slope.pairs.df$SE, 
                                              q1.slope.pairs.df$p.value),
                                      "")

#Arrange columns
q1.slope.pairs.df <- q1.slope.pairs.df %>% 
  select("contrast.type", "contrast", "contrast.name", "estimate", "SE", "df", "z.ratio", "p.value", 
         "is.significant", "reporting")  

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(q1.slope.pairs.df, "Fig.5C_q1.emmeans.slope.contrasts.csv")

#
# Manuscript figures
#

#Panel B: Box plot of 2006 q1 data (pretreatment)
#Uses data with replicates
q1.2006.plot <-
  hill.2006 %>%
  ggplot(aes(x = treatment, 
             y = q1, 
             color = treatment, 
             fill = treatment)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(size = 3, position = position_jitter(width = 0.1)) +
  scale_y_continuous(limits = c(1.1, 7.1), breaks = c(2, 4, 6)) +
  scale_color_manual(values = c("XXX" = "grey40",
                                "PXX" = "royalblue",
                                "XNX" = "#381a61",
                                "XXW" = "#ab3329"),
                     labels = c("XXX" = "Control",
                                "PXX" = "+ Snow",
                                "XNX" = "+ Nitrogen",
                                "XXW" = "+ Warming")) +
  scale_fill_manual(values = c("XXX" = "grey40",
                               "PXX" = "royalblue",
                               "XNX" = "#381a61",
                               "XXW" = "#ab3329"),
                    labels = c("XXX" = "Control",
                               "PXX" = "+ Snow",
                               "XNX" = "+ Nitrogen",
                               "XXW" = "+ Warming")) +
  scale_x_discrete(labels = c("XXX" = "Control",
                              "PXX" = "+ Snow",
                              "XNX" = "+ Nitrogen",
                              "XXW" = "+ Warming")) +
  theme_bw(base_size = 24) +
  theme(axis.text.x = element_text(size=22, angle = 45, hjust = 1),
        axis.text.y = element_text(size=22),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(y = "q=1 (Shannon)", 
       x = NULL,
       tag = "B")


q1.2006.plot

# Save the plot as an RDS so I can combine it with other plots later
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
saveRDS(q1.2006.plot, "Figure.5B.rds")

#Panel C: Create q1 plot over time without 2006

#Get predicted values using emmeans
emmean_q1.preds <- emmeans(
  lmm.q1.priors, 
  specs = ~ treatment | n.year,
  at = list(n.year = seq(1, 18, length.out = 100)))

q1.preds.df <- as.data.frame(emmean_q1.preds) 

# Add columns needed for plotting
q1.preds.df$start.year <- q1.preds.df$n.year + 2006
q1.preds.df$treatment.name <- factor(q1.preds.df$treatment,
                                     levels = c('XXX', 'XXW', 'PXX', 'XNX'),
                                     labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen"))

head(q1.preds.df)

#Set treatment colors and names
hill.temporal$treatment.name <- factor(hill.temporal$treatment,
                                       levels = c('XXX', 'XXW', 'PXX', 'XNX'),
                                       labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen"))

#Plot
q1.temporal.plot <- 
  ggplot(
    q1.preds.df, 
    aes(x = n.year, y = emmean, color = treatment.name, fill = treatment.name)) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = 0.15,
    color = NA) +
  # Add smooth prediction lines
  geom_line(linewidth = 1.2) +
  # Add observed data points with jitter
  geom_point(
    data = hill.temporal, 
    aes(x = n.year, y = q1, color = treatment.name), 
    alpha = 0.8, 
    size = 2.5,
    position = position_jitter(width = 0.15, height = 0)) +
  labs(
    x = NULL,
    y = "q=1 (Shannon)",
    color = NULL,
    fill = NULL) +
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        legend.text = element_text(size = 22),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 22,
                                   vjust = 1,
                                   hjust = 1,
                                   angle = 45,
                                   margin = margin(b = 0, 
                                                   t=5)),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10))) +
  scale_color_manual(
    values = c("Control" = "grey40",
               "+ Snow" = "royalblue",
               "+ Nitrogen" = "#381a61",
               "+ Warming" = "#ab3329")) +
  scale_fill_manual(
    values = c("Control" = "grey40",
               "+ Snow" = "royalblue",
               "+ Nitrogen" = "#381a61",
               "+ Warming" = "#ab3329")) +
  scale_x_continuous(limits = c(0.7, 18.3),
                     breaks = 1:18,
                     labels = c("2007", "", "2009", "", 
                                "2011", "", "2013", "", 
                                "2015", "", "2017", "",
                                "2019", "", "2021", "", 
                                "2023", ""),
                     minor_breaks = NULL) +
  scale_y_continuous(limits = c(1.1, 7.1), breaks = c(2, 4, 6)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         fill = guide_legend(nrow = 1, byrow = TRUE))


q1.temporal.plot

# Save the plot as an RDS so I can combine it with other plots later
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
#saveRDS(q1.temporal.plot, "Figure.6C.rds")
ggsave("Figure.5C.pdf", plot = q1.temporal.plot, width = 8, height = 6)


