
# CR notes: For later analysis review. Ask Megan if she agrees that emmeans is performing well with the gamma priors.
# I think it is, but it would be good to get a second opinion

# Biblioteca
library(tidyverse)
library(emmeans)
library(ggplot2)
library(glmmTMB)
library(patchwork)
library(DHARMa)
library(ggsignif)
library(ggtext)
library(performance)
library(here)

#Establish file location
i_am("Analyses/_Supp_Fig.S3_Distribution.analyses.R")

# Load & wrangle data
setwd("H:/My Drive/Synchrony/Data/Tidy")
dens <- read.csv("pairwise.synch.window.7.years.replicates.csv",
                 as.is = TRUE)

# Set up plotting aesthetics
treatment_colors <- c("XXX" = "grey40",
                      "PXX" = "royalblue",
                      "XNX" = "#381a61",  
                      "XXW" = "#ab3329")

treatment_labels <- c("XXX" = "Control",
                      "PXX" = "+ Snow",
                      "XNX" = "+ Nitrogen", 
                      "XXW" = "+ Warming")

treatment_order <- c("XXX", "XXW", "PXX", "XNX")

# Create plot and block columns from 'repl' and set the column types
dat <- dens %>%
  separate(Repl,
           into = c("plot", "block", NA),
           sep = "_", remove = FALSE) %>% # plot and block cols
  mutate(treatment = factor(treatment, levels = treatment_order), # treatment = factor
         pair = factor(pair), # spp pair = factor
         block = factor(block), # block = factor
         plot = factor(plot), # plot = factor
         window.n = start.year - 2006) #Window number

# Remove dens to avoid confusion
rm(dens)

# Get a sense of data structure. Lots of data. There are 8064 rows in a balanced, fully factorial design.
# Plot and block are factors
str(dat)

xtabs(~Repl, data = dat) #Each plot (replicate) has 336 observations
xtabs(~window.n + treatment, data = dat) #168 rows per treatment and year

n_distinct(dat$window.n) #12 windows
n_distinct((dat$pair)) #28 pairs
n_distinct(dat$plot) # 24 plots
n_distinct(dat$block) # 3 blocks

# 28 pairs * 24 plots * 12 window is equal to the number of rows in the dataset
28*12*24 == nrow(dat)

# Look at raw data
#violin- Control and Warming have wider middles than Snow and Nitrogen, which makes sense because warming and control tended to have more non-significant affiliations
ggplot(dat, aes(x = treatment, y = VR, fill = treatment)) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = treatment_colors) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "orange")

#density plots
ggplot(dat, aes(x = VR)) +
  geom_density() +
  facet_wrap(~ treatment)


# Compare variance ratio by treatment, experimental year
# Each data set row is a variance ratio for one species pair in one window and one treatment
head(dat)

# Model fitting
model <- glmmTMB(VR ~ treatment * window.n + 
      (1 | block/plot) + (1|pair),
    data = dat)

# Is it singular?
check_singularity(model)
#Yes

#Check terms
check_singularity(model, check = "terms")
#Block is singular

# Impose Gamma prior on random effects parameters
prior <- data.frame(
  prior = "gamma(1, 2.5)", # mean and shape
  class = "ranef") # for random effects

model_with_priors <- update(model, priors = prior)

# Re-check singularity
check_singularity(model_with_priors)
# no singular fit

#Remove singular model to avoid confusion
rm(model)

# Look at summary
summary(model_with_priors)

# Block doesn't still doesn't explain much variance, but at least it is not singular!

# Residual diagnostics
sim_residuals <- simulateResiduals(fittedModel = model_with_priors)
plot(sim_residuals) # Deviation sig, but there are lots of data (>8k). Overall looks good.
testDispersion(sim_residuals) # No dispersion.

# Estimated marginal means. Here we will first estimate the marginal means by treatment across the whole time series.
# In a later step we will use the pairs() function to assess which treatments differ from each other 
emm <- emmeans(model_with_priors, ~ treatment,
  at = list(window.n = c(1:12)))

summary(emm)

# Note: emmeans at the average year value gives the same result because our model is linear
emmeans(model_with_priors, ~ treatment, at = list(window.n = 6.5)) 

# LCI and UCI are the emmean ± 1.96 × SE
# This is a 95% confidence interval
# SEs are the same across treatments

# Make the emmeans summery a df for plotting
emm_df <- as.data.frame(summary(emm))

# Now we compare treatments!
# Pairwise contrast of estimated marginal means by treatment.
pairs(emm)
#We see significant differences between:
  #Snow and warming
  #Nitrogen and warming


# Here we're making a tibble of the pairwise treatment contrasts for plotting

#Treatment contrasts
emm_pairs <- pairs(emm) %>% # default mode: Tukey adjustment

#Convert to a tibble to allow for dplyr/tidyr operations
  as_tibble() %>% 
#Cleaning up the tibble by rounding estimates, SE, p-values
#If P values are less than 0.05 than we are flagging them as significant
#We are creating a second column called "label" that is text-based reporting of the p value
#Then we are splitting the contrast column into two so that we can give the treatments full names
#The column bracket_order is hard coded since we saw earlier which treatment contrasts were significant
  mutate(estimate = round(estimate, 3),
         SE = round(SE, 3),
         p.value = round(p.value, 4),
         treatment1 = str_split_i(contrast, " - ", 1),
         treatment2 = str_split_i(contrast, " - ", 2),
         is.significant = ifelse(p.value < 0.05, 1, 0), 
         label = if_else(p.value < 0.001,
                         "p < 0.001",
                         sprintf("p = %.3f", p.value)),
         trt.name.1 = recode_values(treatment1,
                                 "XXX" ~ "Control",
                                 "PXX" ~ "Snow",
                                 "XNX" ~ "Nitrogen",
                                 "XXW" ~ "Warming"),
         trt.name.2 = recode_values(treatment2,
                                 "XXX" ~ "Control",
                                 "PXX" ~ "Snow",
                                 "XNX" ~ "Nitrogen",
                                 "XXW" ~ "Warming"),
         contrast.name = paste(trt.name.1, trt.name.2, sep = " - "),
    bracket_order = case_when(          
      str_detect(contrast, "PXX") & str_detect(contrast, "XXW") ~ 1L, #Snow vs Warming
      str_detect(contrast, "XXW") & str_detect(contrast, "XNX") ~ 2L)) %>% #Warming vs Nitrogen
  arrange(bracket_order)

emm_pairs

#Add column to make clear this is using pairwise variance ratios from all windows
emm_pairs$data.used <- "pairwise.VR.all.windows"

#Add column that combines what I'll report
emm_pairs <- emm_pairs %>%
  mutate(reporting = ifelse(is.significant == 1,
                            sprintf("For the contrast, %s: est = %.3f, SE = %.3f, %s.",
                                    contrast.name, estimate, SE, label),
                            ""))

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(emm_pairs, "Fig.S3_emmeans.contrasts.all.pairwise.VR.csv")

##
## Plotting panel A!
##

# Filter for significant pairs
sig_pairs <-emm_pairs %>%
  filter(is.significant == 1)

#This fomats the information for geom_signif
sig_comparisons <- map2(sig_pairs$treatment1, sig_pairs$treatment2, c) # The c is concatenate

# Check that treatments are still leveled in XXX, XXW, PXX, XNX
levels(emm_df$treatment)

#Format colored axis labels
treatment_labels_html <- c(
  "XXX" = "<span style='color:grey40;'>Control</span>",
  "XXW" = "<span style='color:#ab3329;'>+ Warming</span>",
  "PXX" = "<span style='color:royalblue;'>+ Snow</span>",
  "XNX" = "<span style='color:#381a61;'>+ Nitrogen</span>")

# Plot
p1 <- ggplot(emm_df, aes(x = treatment, y = emmean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_signif(comparisons = sig_comparisons,
              annotations = sig_pairs$label,
              color = "black",
              step_increase = 0.12, # bracket spacing
              tip_length = 0.01) +
  scale_color_manual(values = treatment_colors, 
                     labels = treatment_labels) +
  scale_x_discrete(
    limits = rev(treatment_order),
    labels = treatment_labels_html,
    expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)),
                     breaks = seq(0.9, 1.2, by = 0.05),
                     limits = c(0.9, 1.13),
                    labels = scales::label_number(accuracy = 0.01)) +
  labs(x = NULL, 
       y = "Variance ratio (estimated marginal mean)",
       tag = "A") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_markdown(size = 16),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p1

# ############################## # 
#                                #     
# Part 2. Look at data variation #
#                                #     
# ############################## # 

# First we'll summarize the mean and sd of variance ratios for species pairs within each replicate by treatment and year
dat_summary_stats <- dat %>%
  group_by(treatment, window.n, Repl, plot, block) %>% 
  summarise(mean_VR = mean(VR),
            sd_VR = sd(VR),
            .groups = "drop")

#Look at structure of dat_summary_stats. There are 288 rows.
#Plot and block are factors
str(dat_summary_stats)

xtabs(~Repl, data = dat_summary_stats) #Each plot (replicate) has 12 observations
xtabs(~Repl + window.n, data = dat_summary_stats) #There is one observation for each replicate per year
xtabs(~block, data = dat_summary_stats) #There are 96 observations per block
xtabs(~window.n + treatment, data = dat_summary_stats) #6 rows per treatment and year

n_distinct(dat_summary_stats$window.n) #12 windows
n_distinct(dat$block) # 3 blocks

# Each row of the data is the summarized stats for one plot in one window and treatment
head(dat_summary_stats)

# Can see a decrease in the standard deviation of snow over time. Variation in sd may increase over time under warming.
ggplot(dat_summary_stats, 
       aes(x = factor(window.n), y = sd_VR, color = treatment, group = treatment)) +
  geom_point(size = 3) +
  labs(x = "Window number",
       y = "sd",
       color = "Treatment") +
  scale_color_manual(values = treatment_colors,
                     labels = treatment_labels) +
  theme_bw() +
  facet_wrap(~treatment)

# There is block-level variation
ggplot(dat_summary_stats, 
       aes(x = factor(block), y = sd_VR, color = treatment, group = treatment)) +
  geom_point(size = 3) +
  labs(x = "block",
       y = "sd",
       color = "Treatment") +
  scale_color_manual(values = treatment_colors,
                     labels = treatment_labels) +
  theme_bw() +
  facet_wrap(~treatment)

#Model with SD of VR as the response
model_sd <- glmmTMB(sd_VR ~ treatment * window.n + 
         (1 | block/plot),
       data = dat_summary_stats)

# Is it singular?
check_singularity(model_sd)

# Check terms
check_singularity(model_sd, check = "terms")

# Residual diagnostics
sim_residuals.sd <- simulateResiduals(fittedModel = model_sd)
plot(sim_residuals.sd) #Looks great
testDispersion(sim_residuals.sd) #Looks great

# Summary
summary(model_sd)

# Estimated marginal means for SD model. We are averaging across all 12 windows, so we have one mean per treatment.
# Since the response variable in the GLMM was the plot-level standard deviation of VR in each window, these estimated marginal means are also at the plot level
emm_sd <- emmeans(model_sd, ~ treatment,
     at = list(window.n = c(1:12)))

#Note that that above emmeans calculation is the default by treatment comparison
as.data.frame(emm_sd) == as.data.frame(emmeans(model_sd, ~ treatment))

####### #
# Plot 

# Make emmeans summary a df for plotting
emm_sd_df <- as.data.frame(summary(emm_sd))

#Look at which pairs are significant
pairs(emm_sd) %>%
  as_tibble() %>%
  filter(p.value < 0.05)

# Get significant pairs for plotting
pairs_sd <- pairs(emm_sd) %>%
  as_tibble() %>%
  mutate(contrast.name = case_match(as.character(contrast),
                                    "XXX - XXW" ~ "Control - Warming",
                                    "XXX - PXX" ~ "Control - Snow",
                                    "XXW - PXX" ~ "Warming - Snow",
                                    "XXX - XNX" ~ "Control - Nitrogen",
                                    "XXW - XNX" ~ "Warming - Nitrogen",
                                    "PXX - XNX" ~ "Snow - Nitrogen"),
         across(c(estimate, SE, z.ratio), ~round(., 3)),
         is.significant = ifelse(p.value < 0.05, 1, 0),
         p.value = case_when(
           p.value < 0.001 ~ "< 0.001",
           TRUE ~ as.character(round(p.value, 3))),
         treatment1 = str_split_i(contrast, " - ", 1),
         treatment2 = str_split_i(contrast, " - ", 2),
         label = ifelse(p.value == "< 0.001", "p < 0.001", paste("p =", p.value)),
         bracket_order = case_when(
           str_detect(contrast, "XXW") & str_detect(contrast, "PXX") ~ 1L,
           str_detect(contrast, "XXW") & str_detect(contrast, "XNX") ~ 2L)) %>%
  arrange(bracket_order)

# Add column that combines what I'll report
pairs_sd$reporting <- ifelse(pairs_sd$is.significant == 1,
                             sprintf("For the contrast, %s, (est = %.3f, SE = %.3f, p = %s)", 
                                     pairs_sd$contrast.name,
                                     pairs_sd$estimate, 
                                     pairs_sd$SE, 
                                     pairs_sd$p.value),
                                   "")

pairs_sd

setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(pairs_sd, "Fig.S3_emmeans.contrasts.all.pairwise.VR.std.dev.csv")

sig_pairs_sd <- pairs_sd %>%
  filter(is.significant == 1)

sig_comparisons_sd <- map2(sig_pairs_sd$treatment1, 
                           sig_pairs_sd$treatment2, 
                           c)

# Plot sd
p2 <- ggplot(emm_sd_df, aes(x = treatment, y = emmean, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL,
                    ymax = asymp.UCL), 
                width = 0.2) +
  geom_signif(comparisons = sig_comparisons_sd,
    annotations = sig_pairs_sd$label,
    color = "black",
    step_increase = 0.12,
    tip_length = 0.01) +
  scale_color_manual(values = treatment_colors, labels = treatment_labels) +
  scale_x_discrete(
    limits = rev(treatment_order),
    labels = treatment_labels_html,
    expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)),
                     breaks = seq(0.2, 0.40, by = 0.05),
                     labels = scales::label_number(accuracy = 0.01)) +
  labs(x = NULL, 
       y = "SD of variance ratio (estimated marginal mean)",
       tag = "B") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_markdown(size = 16),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p2


##### #

combined.plot <- p1 / p2  +
  plot_layout(heights = c(1, 1)) 

combined.plot


setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.S3_VR.Mean.Variance.png", combined.plot, width = 8, height = 6, dpi = 600)





