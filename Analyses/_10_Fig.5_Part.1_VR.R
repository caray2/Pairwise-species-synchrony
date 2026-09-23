#
# Biblioteca
#

library(dplyr)
library(ggplot2)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(readr)
library(performance)

set.seed(7220)

#Set working directory
setwd("H:/My Drive/Synchrony/Data/Tidy")
vr.repl <- read.csv("community.synch.window.7.years.replicates.csv")

#Set order of treatments for all files so that it's control, warming, snow, N
vr.repl$treatment <- factor(vr.repl$treatment, 
                       levels = c('XXX', 'XXW', 'PXX', 'XNX'))

#Start with VR
str(vr.repl) 

#6 replicates per treatment per year
xtabs(~treatment + start.year, data = vr.repl)

#Make block and plot factors
vr.repl$block <- as.factor(vr.repl$block)

vr.repl$plot <- as.factor(vr.repl$plot)

# Look at the data
ggplot(vr.repl, aes(x = start.year, y = VR, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("XXX" = "grey40",
                                "XXW" = "#ab3329",
                                "PXX" = "royalblue",
                                "XNX" = "#381a61"),
                     labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen")) +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  labs(x = "Start year",
       y = "Variance Ratio (VR)",
       color = "Treatment") +
  theme_classic() +
  theme(legend.position = "bottom")

#Looks like
#VRs change over time (a year effect)
#At least one treatment differs from the others
#Year effect differs by treatment (an interaction)

# There is block-level variation and maybe treatment by block effects
ggplot(vr.repl, 
       aes(x = block, y = VR, color = treatment, group = treatment)) +
  geom_point(size = 3) +
  labs(x = "block",
       y = "VR",
       color = "Treatment") +
  theme_bw() +
  facet_wrap(~treatment)

# Data structure
str(vr.repl)

#Linear model for community variance ratios
lmm.vr.nested <- glmmTMB(VR ~ 
                           treatment * n.year.start + (1|block/plot), 
                         data = vr.repl)
                      
# Is it singular?
performance::check_singularity(lmm.vr.nested)
# yes

# Check terms
check_singularity(lmm.vr.nested, check = "terms")
# Block is singular

# Impose weakly informative Gamma prior on random effects parameters
prior <- data.frame(
  prior = "gamma(1, 2.5)", # mean and shape. mean can be 1, but even 1e8
  class = "ranef") # for random effects

lmm.vr.priors <- update(lmm.vr.nested, priors = prior)

# no singular fit with gamma priors
check_singularity(lmm.vr.priors)

#Remove the singular model to avoid coding typos
rm(lmm.vr.nested)

# Look at fit
sim.vr.residuals <- simulateResiduals(fittedModel = lmm.vr.priors)
plot(sim.vr.residuals)

testDispersion(sim.vr.residuals)

summary(lmm.vr.priors)

#Compare treatments using emmeans at the start and end of the experiment.
#This is how we contrasted variance ratios for in figures 2 and 4
emm.vr <- emmeans(lmm.vr.priors, ~ treatment | n.year.start,
                  at = list(n.year.start = c(1, 12)))

#Data frame for extracting raw estimated marginal means
emm.vr.df <- as.data.frame(emm.vr) %>%
  select(n.year.start, treatment, emmean) %>%
  rename(raw.emmean = emmean)

#Pairwise contrasts
emm.vr.contrasts <- pairs(emm.vr, adjust = "tukey")

emm.vr.contrasts

#Make a dataframe for the vr contrasts
emm.vr.contrasts.df <- as.data.frame(emm.vr.contrasts) %>%
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
                               "PXX - XNX" ~ "Snow - Nitrogen"),
    #Intuitive names for window
    window = recode_values(as.character(n.year.start),
                               "1" ~ "2007-2013",
                               "12" ~ "2018-2024"),
    # Split contrast column to make treat1 and treat2
    treat1 = trimws(sub(" - .*", "", contrast)),
    treat2 = trimws(sub(".* - ", "", contrast))) %>%
    # Join estimates for treat1
    left_join(emm.vr.df, by = c("treat1" = "treatment", "n.year.start")) %>%
  rename(emmean.trt.1 = raw.emmean) %>%
  # Join estimates for treat2
  left_join(emm.vr.df, by = c("treat2" = "treatment", "n.year.start")) %>%
  rename(emmean.trt.2 = raw.emmean) %>%
  mutate(across(c(emmean.trt.1, emmean.trt.2), ~round(., 3)))
    
#Add column of what kind of contrast was done
emm.vr.contrasts.df$contrast.type <-"VR"

colnames(emm.vr.contrasts.df)

#Add column that combines what I'll report. %d for year numbers/ %f for stats values/ %s for text
emm.vr.contrasts.df$reporting <- ifelse(emm.vr.contrasts.df$is.significant == 1,
                      sprintf("In the window %s, for the contrast %s (est = %.3f, SE = %.3f, p = %s)", 
                                                emm.vr.contrasts.df$window,
                                                emm.vr.contrasts.df$contrast.name,
                                                emm.vr.contrasts.df$estimate, 
                                                emm.vr.contrasts.df$SE, 
                                                emm.vr.contrasts.df$p.value),
                                        "") #otherwise blank

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(emm.vr.contrasts.df, "Fig.5A_community.VR.treatment.effect.temporal.csv")

#visualize the model
#Treatment effect
emm.vr.df <- as.data.frame(emm.vr)

ggplot(emm.vr.df, aes(x = treatment, y = emmean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, linewidth = 1) +
  theme_minimal() +
  labs(title = "Estimated Marginal Means by Treatment in years 1 and 12",
       y = "Estimated Marginal Mean VR",
       x = "Treatment") +
  scale_color_manual(values = c("XXX" = "grey40",
                                "XXW" = "#ab3329",
                                "PXX" = "royalblue",
                                "XNX" = "#381a61")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  coord_flip() +
  facet_wrap(~n.year.start)  +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "black", fill = "white", linewidth = 0.5))


#Compare slopes
# Get slopes of n.year for each treatment
vr.slopes <- emtrends(lmm.vr.priors, 
                   ~ treatment, 
                   var = "n.year.start")

vr.slopes  # View the slopes

# Make df to extract raw slope values from the emtrends object
vr.slopes.df <- as.data.frame(vr.slopes) %>%
  select(treatment, n.year.start.trend) %>%
  rename(slope = n.year.start.trend)

# Compare slopes between treatments within each affiliation
vr.slope.pairs <- pairs(vr.slopes, reverse = T)
vr.slope.pairs

#Make a dataframe and save it
vr.slope.pairs.df <- as.data.frame(vr.slope.pairs) %>%
  mutate(
    across(c(estimate, SE, z.ratio), ~round(., 3)),
    is.significant = ifelse(p.value < 0.05, 1, 0), 
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p.value, 3))),
    contrast.name = case_match(as.character(contrast),
                               "XXW - XXX" ~ "Warming - Control",
                               "PXX - XXX" ~ "Snow - Control",
                               "PXX - XXW" ~ "Snow - Warming",
                               "XNX - XXX" ~ "Nitrogen - Control",
                               "XNX - XXW" ~ "Nitrogen - Warming",
                               "XNX - PXX" ~ "Nitrogen - Snow"),
    # Split contrast column to make treat1 and treat2
    treat1 = trimws(sub(" - .*", "", contrast)),
    treat2 = trimws(sub(".* - ", "", contrast))) %>%
    # Join slope for treat1
    left_join(vr.slopes.df, by = c("treat1" = "treatment")) %>%
  rename(slope.treat1 = slope) %>%
  # Join slope for treat2
  left_join(vr.slopes.df, by = c("treat2" = "treatment")) %>%
  rename(slope.treat2 = slope) %>%
  mutate(across(c(slope.treat1, slope.treat2), ~round(., 3)))

#Add column of what kind of contrast was done
vr.slope.pairs.df$contrast.type <-"vr.slope"

#Add column that combines what I'll report
vr.slope.pairs.df$reporting <- ifelse(vr.slope.pairs.df$is.significant == 1,
                                   sprintf("For the slope contrast for, %s, (est = %.3f, SE = %.3f, p = %s)", 
                                           vr.slope.pairs.df$contrast.name,
                                           vr.slope.pairs.df$estimate, 
                                           vr.slope.pairs.df$SE, 
                                           vr.slope.pairs.df$p.value),
                                   "")

vr.slope.pairs.df

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(vr.slope.pairs.df, "Fig.5A_community.VR.emmeans.slope.contrasts.csv")


# Panel A: Plot vr with points for replicates

#Get predicted values using emmeans
emmean_VR.preds <- emmeans(
  lmm.vr.priors, 
  specs = ~ treatment | n.year.start,
  at = list(n.year.start = seq(1, 12, length.out = 100)))

vr.preds.df <- as.data.frame(emmean_VR.preds) 

# Add columns needed for plotting
vr.preds.df$treatment.name <- factor(vr.preds.df$treatment,
                                     levels = c('XXX', 'XXW', 'PXX', 'XNX'),
                                     labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen"))

head(vr.preds.df)

#Plot
vr.plot <- 
  ggplot(
    vr.preds.df, 
    aes(x = n.year.start, y = emmean, color = treatment.name, fill = treatment.name)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = 0.15,
    color = NA) +
  # Add smooth prediction lines
  geom_line(linewidth = 1.2) +
  # Add observed data points with jitter
  geom_point(
    data = vr.repl, 
    aes(x = n.year.start, y = VR, color = treatment.name), 
    alpha = 0.8, 
    size = 2.5,
    position = position_jitter(width = 0.15, height = 0)) +
  labs(
    x = "Window start year",
    y = "Variance ratio",
    color = NULL,
    fill = NULL,
    tag = "A") +
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
    values = c(
      "Control" = "grey40",
      "+ Snow" = "royalblue",
      "+ Nitrogen" = "#381a61",
      "+ Warming" = "#ab3329")) +
  scale_fill_manual(
    values = c(
      "Control" = "grey40",
      "+ Snow" = "royalblue",
      "+ Nitrogen" = "#381a61",
      "+ Warming" = "#ab3329")) +
  scale_x_continuous(
    limits = c(0.7, 12.3),
    breaks = 1:12,
    labels = c("2007", "", "2009", "", "2011", "",
               "2013", "", "2015", "", "2017", ""),
    minor_breaks = NULL) +
  scale_y_continuous(
    limits = c(0, 1.2), 
    breaks = seq(0, 1.2, by = 0.4)) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    fill = guide_legend(nrow = 1, byrow = TRUE))


vr.plot

# Save the plot as an RDS so I can combine it with other plots later
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
saveRDS(vr.plot, "Figure.5A.rds")
