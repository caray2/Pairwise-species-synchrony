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
library(cowplot)
library(performance)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/_10_Fig.5_Part.3_Q2.R")

hill.repl <- read.csv(here("Data", "Tidy", "hill.numbers.with.replicates.csv"), as.is = T)

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

# Q2 ANOVA
q2.2006.aov <- aov(q2 ~ treatment + Error(block), data = hill.2006)

# Is it singular?
check_singularity(q2.2006.aov)

print(summary(q2.2006.aov)) 
#In the pre-treatment plots, q2 does not differ between treatments


#
# Hill numbers through time!
#

#Now look at the temporal trends for q2 excluding the pre-treatment year of 2006
hill.temporal <- hill.repl %>% filter(year != 2006)
rm(hill.repl) #To avoid confusion

str(hill.temporal)

xtabs(~treatment + year, data = hill.temporal)

#Linear model for q2
lmm.q2.nested <- glmmTMB(q2 ~ treatment * n.year + 
                           (1|block/plot), 
                         data = hill.temporal,
                         family = gaussian())


# Is it singular?
check_singularity(lmm.q2.nested) #yes

# Check terms
check_singularity(lmm.q2.nested, check = "terms")
#block is singular.


# Impose Gamma prior on random effects parameters.
# Gamma distributions range from 0 to infinity. When the our mixed model only has a random effect of block the variance is 0.015 and std dev is 0.122, so block captures spatial variation in our diversity metric (q=2). Because the experimental design is nested with plot in block we want to account for this structure in our random effects. The challenge is that the variance of block drops with a nested structure to near 0, leading to singularity. This may be due in large part because block only has 3 levels. 

#To avoid the the variance estimate going to 0 for a random effect, while not heavily influencing model estimates, Chung 2013 proposed: "a maximum penalized likelihood approach"..."which is equivalent to assigning a gamma (not inverse-gamma) prior". Chung recommends a shape value of 2, but in the performance package they say that the shape value should be 2.5.

#When we impose a gamma distribution in glmmTMB the format is gamma(mean, shape). A shape of 2.5 is skewed towards 0. 

#Set random effect priors. # These are the mean and the shape. A mean of 1 is weakly is weakly informative.

prior <- data.frame(
  prior = "gamma(1, 2.5)", 
  class = "ranef")

#update priors
q2.priors <- update(lmm.q2.nested, priors = prior)

# Recheck terms for singularity
check_singularity(q2.priors, check = "terms")
# no singular fit

#remove singular model to avoid typos
rm(lmm.q2.nested)

#Summary
summary(q2.priors)

# Look at fit
sim.q2.residuals <- simulateResiduals(fittedModel = q2.priors)
plot(sim.q2.residuals)

testDispersion(sim.q2.residuals)
# Some quantile deviations

#Compare treatments using emmeans in every year of the treatment
emm.q2 <- emmeans(q2.priors, ~ treatment | n.year,
                  at = list(n.year = c(1: 18)))

emm.q2.contrasts <- pairs(emm.q2, adjust = "tukey")

emm.q2.contrasts

#Make a dataframe for the q2 contrasts
emm.q2.contrasts.df <- as.data.frame(emm.q2.contrasts) %>%
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
emm.q2.contrasts.df$contrast.type <-"Q2"

#Add column with more intuitive year values
emm.q2.contrasts.df$year <- emm.q2.contrasts.df$n.year + 2006

colnames(emm.q2.contrasts.df)

#Add column that combines what I'll report. %d for year / %f for stats values/ %s for text
emm.q2.contrasts.df$reporting <- ifelse(emm.q2.contrasts.df$is.significant == 1,
                    sprintf("In %d, for the contrast %s (est = %.3f, SE = %.3f, p = %s)", 
                                                emm.q2.contrasts.df$year,
                                                emm.q2.contrasts.df$contrast.name,
                                                emm.q2.contrasts.df$estimate, 
                                                emm.q2.contrasts.df$SE, 
                                                emm.q2.contrasts.df$p.value),
                                        "") #otherwise blank

#Arrange columns
emm.q2.contrasts.df <- emm.q2.contrasts.df %>% 
  select("n.year", "year", "contrast.type", "contrast", "estimate", 
         "SE", "df", "z.ratio", "p.value", "is.significant", "contrast.name", "reporting")  

#Save dataframe
write_csv(emm.q2.contrasts.df, here("Posthoc.tests", "Fig.5E_q2.treatment.effect.temporal.csv"))

#visualize the model
#Treatment effect
emm.q2.df <- as.data.frame(emm.q2)

ggplot(emm.q2.df, aes(x = treatment, y = emmean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, linewidth = 1) +
  theme_minimal() +
  labs(title = "Estimated Marginal Means by Treatment in years 1 and 18",
       y = "Estimated Marginal Mean Q2",
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
q2.slopes <- emtrends(q2.priors, 
                      ~ treatment, 
                      var = "n.year")

q2.slopes  # View the slopes

# Compare slopes between treatments within each affiliation
q2.slope.pairs <- pairs(q2.slopes, reverse = T)
q2.slope.pairs

#Make a dataframe and save it
q2.slope.pairs.df <- as.data.frame(q2.slope.pairs) %>%
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
q2.slope.pairs.df$contrast.type <-"q2.slope"

#Add column that combines what I'll report
q2.slope.pairs.df$reporting <- ifelse(q2.slope.pairs.df$is.significant == 1,
                                      sprintf("For the slope contrast for %s (est = %.3f, SE = %.3f, p = %s)", 
                                              q2.slope.pairs.df$contrast.name,
                                              q2.slope.pairs.df$estimate, 
                                              q2.slope.pairs.df$SE, 
                                              q2.slope.pairs.df$p.value),
                                      "")

colnames(q2.slope.pairs.df)

#Arrange columns
q2.slope.pairs.df <- q2.slope.pairs.df %>% 
  select("contrast.type", "contrast", "contrast.name", "estimate", "SE", "df", "z.ratio", "p.value", 
         "is.significant", "reporting")  

#Save df
write_csv(q2.slope.pairs.df, here("Posthoc.tests", "Fig.5E_q2.emmeans.slope.contrasts.csv"))

#
# Manuscript figures
#

#Panel B: Box plot of 2006 q2 data (pretreatment)
#Uses data with replicates
q2.2006.plot <-
  hill.2006 %>%
  ggplot(aes(x = treatment, 
             y = q2, 
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
  labs(y = "q=2 (Simpson)", 
       x = NULL,
       tag = "D")


q2.2006.plot

# Save the plot as an RDS so I can combine it with other plots later
saveRDS(q2.2006.plot, here("Graphics", "RDS.plots", "Figure.5D.rds"))

#Panel C: Create q2 plot over time without 2006

#Get predicted values using emmeans
emmean_q2.preds <- emmeans(
  q2.priors, 
  specs = ~ treatment | n.year,
  at = list(n.year = seq(1, 18, length.out = 100)))

q2.preds.df <- as.data.frame(emmean_q2.preds) 

# Add columns needed for plotting
q2.preds.df$start.year <- q2.preds.df$n.year + 2006
q2.preds.df$treatment.name <- factor(q2.preds.df$treatment,
                                     levels = c('XXX', 'XXW', 'PXX', 'XNX'),
                                     labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen"))

head(q2.preds.df)

#Set treatment colors and names
hill.temporal$treatment.name <- factor(hill.temporal$treatment,
                                       levels = c('XXX', 'XXW', 'PXX', 'XNX'),
                                       labels = c("Control", "+ Warming", "+ Snow", "+ Nitrogen"))

#Plot
q2.temporal.plot <- 
  ggplot(
    q2.preds.df, 
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
    aes(x = n.year, y = q2, color = treatment.name), 
    alpha = 0.8, 
    size = 2.5,
    position = position_jitter(width = 0.15, height = 0)) +
  labs(
    x = NULL,
    y = "q=2 (Simpson)",
    color = NULL,
    fill = NULL,
    tag = "E") +
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


q2.temporal.plot

# Save the plot as an RDS so I can combine it with other plots later
saveRDS(q2.temporal.plot, here("Graphics", "RDS.plots", "Figure.5E.rds"))

# Create a dummy plot just for the legend
legend.plot <- ggplot(
  q2.preds.df, 
  aes(x = n.year, y = emmean, color = treatment.name, fill = treatment.name)) +
  geom_line(linewidth = 3) +
  geom_point(size=7) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 26),
    legend.title = element_blank(),
    legend.key.size = unit(2, "cm"),
    legend.key = element_rect(color = "white", fill = "white", linewidth = 1)) +
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
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         fill = guide_legend(nrow = 1, byrow = TRUE))


legend.plot

# Extract just the legend using cowplot

shared.legend <- get_legend(legend.plot)

# Save it as RDS
saveRDS(shared.legend, here("Graphics", "RDS.plots", "Figure.5.shared.legend.rds"))
