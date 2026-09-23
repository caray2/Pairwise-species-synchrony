
# Biblioteca 

#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mclogit)
library(ggtext)
library(emmeans)
library(readr)
library(here)

# Set seed
set.seed(7220)

# Load Data 

# Set working directory
setwd("H:/My Drive/Synchrony/data/Tidy")

# Load moving windows synchrony data
df.wind <- read.csv("pairwise.synch.window.7.years.csv", as.is = T)

# Make a window number column so that 2007, which is the start year of the first window, is window 1
df.wind$window.n <- df.wind$start.year - 2006

# Select focal columns for analyses
df <- data.frame(
  aff.name = df.wind$aff.name, #None, comp, synch
  treatment = df.wind$treatment, #XXX, PXX, XNX, XXW
  window.n = df.wind$window.n, #Window number: 1-12
  pair = df.wind$pair,  # Species pair
  stringsAsFactors = FALSE)


# Check row numbers makes sense
# 12 years, 4 treatments, 28 species pairs
12 * 4 * 28 == nrow(df)

# Set Factor Levels

# Set levels so that the comparison is with the 'None' affiliation and XXX treatment in the intercept
# Note that these have level structure, but are not rank ordered
df$treatment <- factor(df$treatment, 
                       levels = c('XXX', 'XXW', 'PXX', 'XNX'))

df$aff.name <- factor(df$aff.name, 
                      levels = c('None', 'Compensatory', 'Synchrony'))

# Make sure pair is a factor for random effects
df$pair <- factor(df$pair)

# Check data structure
str(df)

# Exploratory Data Visualization 

# Calculate proportions by year and treatment
plot_data <- df %>%
  group_by(window.n, treatment, aff.name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(window.n, treatment, aff.name, fill = list(count = 0)) %>%
  group_by(window.n, treatment) %>%
  mutate(proportion = count / sum(count))

# Create the plot
ggplot(plot_data, aes(x = window.n, y = proportion, 
                           color = treatment, fill = treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ aff.name, nrow = 3, scales = "free_y") +  # Added scales = "free_y"
  scale_color_manual(values = c("XXX" = "grey40", 
                                 "PXX" = "blue", 
                                 "XNX" = "purple",
                                "XXW"="red")) +
  scale_fill_manual(values = c("XXX" = "grey40", 
                                "PXX" = "blue", 
                                "XNX" = "purple",
                               "XXW"= "red")) +
  labs(x = "Window number",
       y = "Proportion",
       color = "Treatment", 
       fill = "Treatment") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())


# Fit multinomial baseline logistic regression with random effects for species pair
model.random <- mblogit(aff.name ~ window.n * treatment, 
                 random = ~ 1|pair,  # Random intercept for each species pair
                 data = df,
                 control = mmclogit.control(inner.optimizer = 'BFGS'))

# Model summary
summary(model.random)

# Is it singular?
performance::check_singularity(model.random)

# Check deviance
deviance(model.random)
nobs(model.random)

# Post-hoc Tests: Treatment Effects

# Treatment effects for each affiliation type
# emmeans(model.random, ~ treatment | aff.name, mode = "prob")
pairs(emmeans(model.random, ~ treatment | aff.name, mode = "prob"))

# Post-hoc Tests: Treatment at Specific Years

emmeans(model.random, ~ treatment | aff.name + window.n, 
        at = list(window.n = c(1, 12)), mode = "prob")

point.pairs <- pairs(emmeans(model.random, ~ treatment | aff.name + window.n, 
        at = list(window.n = c(1, 12)), mode = "prob"), reverse = TRUE)

point.pairs

# Make a dataframe and save it
year.pairs.df <- as.data.frame(point.pairs) %>%
  mutate(
    across(c(estimate, SE, z.ratio), ~round(., 3)),
    is.significant = ifelse(p.value < 0.05, 1, 0), 
    p.value = case_when(p.value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p.value, 3))),
    contrast.name = case_match(as.character(contrast),
                               "XXW - XXX" ~ "Warming - Control",
                               "PXX - XXX" ~ "Snow - Control",
                               "PXX - XXW" ~ "Snow - Warming",
                               "XNX - XXX" ~ "Nitrogen - Control",
                               "XNX - XXW" ~ "Nitrogen - Warming",
                               "XNX - PXX" ~ "Nitrogen - Snow"),
    window = case_match(as.character(window.n),
                        "1" ~ "2007-2013",
                        "12" ~ "2018-2024"))

# Add column of what kind of contrast was done
year.pairs.df$contrast.type <-"year.point"

# Add column that combines what I'll report
year.pairs.df$reporting <- ifelse(year.pairs.df$is.significant == 1,
          sprintf("For the contrast, %s, and the affiliation, %s, in the year, %s, (est = %.3f, SE = %.3f, p = %s)", 
                                           year.pairs.df$contrast.name,
                                           year.pairs.df$aff.name,
                                           year.pairs.df$window.n,
                                           year.pairs.df$estimate, 
                                           year.pairs.df$SE, 
                                           year.pairs.df$p.value),
          "")

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(year.pairs.df, "Fig.4_emmeans.prob.contrasts.multinom.moving.windows.csv")

# Post-hoc Tests: Compare Slopes

# Get slopes of window.n for each treatment and affiliation
slopes <- emtrends(model.random, 
                   ~ treatment | aff.name, 
                   var = "window.n",
                   mode = "prob")

slopes  # View the slopes

# Make df to extract raw slope values from the emtrends object
slopes.df <- as.data.frame(slopes) %>%
  select(treatment, aff.name, window.n.trend) %>%
  rename(slope = window.n.trend)

# Compare slopes between treatments within each affiliation
slope.pairs <- pairs(slopes, reverse = T)
slope.pairs

# Make a dataframe and save it
slope.pairs.df <- as.data.frame(slope.pairs) %>%
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
    # Split contrast column
    treat1 = trimws(sub(" - .*", "", contrast)),
    treat2 = trimws(sub(".* - ", "", contrast))) %>%
  # Join slope for treat1
  left_join(slopes.df, by = c("treat1" = "treatment", "aff.name")) %>%
  rename(slope.treat1 = slope) %>%
  # Join slope for treat2
  left_join(slopes.df, by = c("treat2" = "treatment", "aff.name")) %>%
  rename(slope.treat2 = slope) %>%
  mutate(across(c(slope.treat1, slope.treat2), ~round(., 3)))

# Add column of what kind of contrast was done
slope.pairs.df$contrast.type <- "slope"
##

# Add column that combines what I'll report
slope.pairs.df$reporting <- ifelse(slope.pairs.df$is.significant == 1,
                       sprintf("For the contrast, %s, and the affiliation, %s, (est = %.3f, SE = %.3f, p = %s)", 
                                           slope.pairs.df$contrast.name,
                                           slope.pairs.df$aff.name,
                                           slope.pairs.df$estimate, 
                                           slope.pairs.df$SE, 
                                           slope.pairs.df$p.value),
                                   "")

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(slope.pairs.df, "Fig.4_emmeans.slope.contrasts.multinom.moving.windows.csv")

# Get Predicted Probabilities

emmean_preds.random <- emmeans(
  model.random, 
  specs = ~ treatment | aff.name * window.n,  # Note: | instead of *
  at = list(window.n = seq(1, 12, length.out = 100)),  
  mode = "prob")

pred_long.random <-as.data.frame(emmean_preds.random)

head(pred_long.random)

# Calculate Observed Proportions

obs_props <- df %>%
  # Count observations for each combination
  group_by(treatment, window.n, aff.name) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Fill in any missing combinations with count = 0
  complete(treatment, window.n, aff.name, fill = list(count = 0)) %>%
  # Calculate proportions within each treatment/year
  group_by(treatment, window.n) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Look at ranges of upper and lower confidence intervals to set the y-axis
pred_long.random %>%
  group_by(aff.name) %>%
  summarise(
    min_LCL = min(asymp.LCL, na.rm = TRUE),
    max_UCL = max(asymp.UCL, na.rm = TRUE))

# Prepare Plot Elements

# Define treatment colors
treatment_colors <- c("XXX" = "grey40",      # Control
                      "PXX" = "royalblue",   # Snow
                      "XNX" = "#381a61",     # Nitrogen (dark purple)
                      "XXW" = "#ab3329")      # Warming (red)

# Set factor levels to control panel order (top to bottom)
pred_long.random$aff.name <- factor(
  pred_long.random$aff.name, 
  levels = c("Synchrony", "Compensatory", "None"))

obs_props$aff.name <- factor(
  obs_props$aff.name, 
  levels = c("Synchrony", "Compensatory", "None"))

# Create facet labels with colored text using ggtext
affiliation_labels <- c("Synchrony" = "<span style='color:#018571;'>**Synchronous**</span>",
                        "Compensatory" = "<span style='color:#a6611a;'>**Compensatory**</span>",
                        "None" = "<span style='color:grey40;'>**No affiliation**</span>")
# Create Final Plot 

multinom.moving.win <- ggplot(
  pred_long.random, 
  aes(x = window.n, y = prob, color = treatment, fill = treatment)) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = 0.15,
    color = NA) +
  #Model predictions
  geom_line(linewidth = 1.2) +
  # Add observed data points with jitter
  geom_point(
    data = obs_props, 
    aes(x = window.n, y = proportion, color = treatment), 
    alpha = 0.8, 
    size = 2.5,
    position = position_jitter(width = 0.15, height = 0)) +
  facet_wrap(~ aff.name, 
             ncol = 1, 
             scales = "free_y",
             labeller = labeller(aff.name = affiliation_labels)) +
  labs(x = "Window start year",
       y = "Probability of affiliation",
       color = NULL,
       fill = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_markdown(size = 14),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12,
                                   vjust = 0,
                                   hjust = 0,
                                   angle = 45,
                                   margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 5))) +
  scale_color_manual(
    values = treatment_colors,
    labels = c(
      "XXX" = "Control",
      "XXW" = "+ Warming",
      "PXX" = "+ Snow",
      "XNX" = "+ Nitrogen")) +
  scale_fill_manual(
    values = treatment_colors,
    labels = c(
      "XXX" = "Control",
      "XXW" = "+ Warming",
      "PXX" = "+ Snow",
      "XNX" = "+ Nitrogen")) +
  scale_x_continuous(limits = c(0.7, 12.3),
                     breaks = 1:12,
                     labels = c("2007", "", "2009", "", "2011", "",
                                "2013", "", "2015", "", "2017", "")) +
scale_y_continuous(
  limits = function(x) {
    if(max(x) > 0.4) {  # This will be "None" panel
      c(0.5, 1.0)
    } else {  # This will be "Synchrony" and "Compensatory" panels
      c(-0.01, 0.32) 
    }
  },
  breaks = function(x) {
    if(max(x) > 0.4) {  # This will be "None" panel
      c(0.6, 0.8, 1.0)
    } else {  # This will be "Synchrony" and "Compensatory" panels
      c(0, 0.1, 0.2, 0.3)
    }
  },
  minor_breaks = NULL) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         fill = guide_legend(nrow = 1, byrow = TRUE))

multinom.moving.win

setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.4_moving.window.multinomial.png", multinom.moving.win, width = 7.5, height = 8.5, dpi = 600)

# Diagnostic Checks

# Check that probabilities sum to 1 for each treatment/year combination
prob_check <- pred_long.random %>%
  group_by(treatment, window.n) %>%
  summarise(sum_prob = sum(prob), .groups = "drop")

# Should all be very close to 1.0
summary(prob_check$sum_prob)

# View the range of predictions for each affiliation
pred_long.random %>%
  group_by(aff.name) %>%
  summarise(
    min_prob = min(prob),
    max_prob = max(prob),
    mean_prob = mean(prob))
