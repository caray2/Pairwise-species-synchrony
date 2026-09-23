# Biblioteca
#
library(ggplot2)
library(dplyr)
library(Matrix)
library(mclogit)
library(ggtext)
library(emmeans)
library(readr)
library(here)

#Set seed
set.seed(7220)

#Set working directory to pairwise synchrony data from the whole research period
setwd("H:/My Drive/Synchrony/data/Tidy")
df.all.years <- read.csv("pairwise.synch.all.years.csv", as.is = T)
colnames(df.all.years)

# Select focal columns for analyses
df <- data.frame(
  aff.name = df.all.years$aff.name, #None, comp, synch
  treatment = df.all.years$treatment, #XXX, PXX, XNX, XXW
  pair = df.all.years$pair,  # Species pair
  stringsAsFactors = FALSE
)
rm(df.all.years) #no longer needed

# XXXXXXXXXXXXXXXXXXXX
# MODELING TIME
# XXXXXXXXXXXXXXXXXXXX

#Set levels so that the comparison is with the 'None affiliation' and XXX (the control) in the intercept
df$aff.name <- factor(df$aff.name, 
                      levels=c('None',
                               'Compensatory',
                               'Synchrony'))
df$treatment <- factor(df$treatment, 
                       levels=c('XXX',
                                'XXW',
                                'PXX',
                                'XNX'))
str(df)

# Make pair a factor
df$pair <- as.factor(df$pair)

#Fit a multinomial model. Here we are mirroring the model structure used in the moving window multinomial

multinom <- mblogit(aff.name ~ treatment, 
                        random = ~ 1|pair,  # Random intercept for each species pair
                        data = df,
                        control = mmclogit.control(inner.optimizer = 'BFGS'))

summary(multinom)

# Is it singular?
performance::check_singularity(multinom)

#Post hoc comparison for each affiliation
pair.comparison <-
  pairs(emmeans(multinom, ~ treatment | aff.name, mode = "prob"))

pair.comparison

#Make a dataframe and save it
pairs.df <- as.data.frame(pair.comparison) %>%
  mutate(
    across(c(estimate, SE, z.ratio), ~round(., 3)),
    is.significant = ifelse(p.value < 0.05, 1, 0), 
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p.value, 3))),
    contrast.name = case_match(as.character(contrast),
                               "XXX - XXW" ~ "Warming - Control",
                               "XXX - PXX" ~ "Snow - Control",
                               "XXW - PXX" ~ "Snow - Warming",
                               "XXX - XNX" ~ "Nitrogen - Control",
                               "XXW - XNX" ~ "Nitrogen - Warming",
                               "PXX - XNX" ~ "Nitrogen - Snow"))

#Add column to make clear this is using synchrony data from all years
pairs.df$data.used <- "all.years"

#Add column that combines what I'll report
pairs.df$reporting <- ifelse(pairs.df$is.significant == 1,
              sprintf("For the contrast, %s, and the affiliation, %s, using all treatment years (est = %.3f, SE = %.3f, p = %s)",
                      pairs.df$contrast.name,
                      pairs.df$aff.name,
                      pairs.df$estimate,
                      pairs.df$SE, 
                      pairs.df$p.value),
              "")


setwd("H:/My Drive/Synchrony/Posthoc.tests")
write_csv(pairs.df, "Fig.2B_emmeans.prob.contrasts.multinom.all.years.csv")


#Get predicted probabilities from the multinomial model using emmeans. 

#We want to compare treatment effects within an affiliation type. 
#This is what we'll plot.

emmean.prob <- emmeans(
  multinom, 
  specs = ~ treatment | aff.name,  # Note: | instead of *
  mode = "prob")

#Based on these sources. Alot of this #text is copy and pasted:
  #https://github.com/rvlenth/emmeans/issues/272
  #https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression
  #https://cran.r-project.org/web/packages/emmeans/vignettes/models.html#N
 
#The above emmeans calculation is computing the emmeans and confidence intervals on the probability scale (in my case) after re-gridding. The reference grid includes a pseudo-factor with the same name and levels as the multinomial response.

ref_grid(multinom)

#'emmGrid' object with variables:
#  treatment = XXX, XXW, PXX, XNX
#aff.name = multivariate response levels: None, Compensatory, Synchrony

#There is an optional mode argument which should match "prob" or "latent". With mode = "prob", the reference-grid predictions consist of the estimated multinomial probabilities – and this implies a re-gridding so no link functions are passed on. 

#The "latent" mode returns the linear predictor, re-centered so that it averages to zero over the levels of the response variable (similar to sum-to-zero contrasts). Thus each latent variable can be regarded as the log probability at that level minus the average log probability over all levels.

#There are two optional arguments: mode and rescale (which defaults to c(0, 1)).
#Pred latent is on the centered latent response scale so hard to backtransform


#Confirm prob and latent models are behaving as expected.
pred.prob <-as.data.frame(emmean.prob)

pred.prob %>%
  group_by(treatment) %>%
  summarise(total_prob = sum(prob))


#Look at data by low to high asymp.LCL
head(pred.prob %>% arrange(asymp.LCL))

#There are three cases where asymp.LCL is slightly below 0. Investigate why.

#Check assumption that CIs are calculated using a Wald approximation
pred.prob$lower.wald <- pred.prob$prob - (1.96*(pred.prob$SE))

ggplot(pred.prob, aes(x = asymp.LCL, y = lower.wald)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "asymp.LCL (from emmeans)",
       y = "lower.wald (manually calculated)",
       title = "Comparison of Lower Confidence Limits") +
  theme_bw()


#CI's below 0 are likely due to them being calculated on the response scale after regridding rather than the link scale. Since the link scale grid is hard to access in emmeans for multinomial models by design and the asymp lower confidence intervals are only slightly below 0, I think it's reasonable for CIs below 0 to re-assign them to 0

#Look at emmeans before changing confidence intervals
#Reversing treatment order so that it's correctly oriented with the coord flip
pred.prob$treatment <- factor(pred.prob$treatment, 
                       levels=c('XNX',
                                'PXX',
                                'XXW',
                                'XXX'))

ggplot(pred.prob, aes(x = treatment, y = prob, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, linewidth = 1) +
  theme_minimal() +
  labs(title = "Estimated Marginal Means by Treatment",
       y = "Estimated Marginal Mean VR",
       x = "Treatment") +
  scale_color_manual(values = c("XXX" = "grey40",
                                "XXW" = "#ab3329",
                                "PXX" = "royalblue",
                                "XNX" = "#381a61")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  facet_wrap(~aff.name) +
  coord_flip() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "black", fill = "white", linewidth = 0.5))

#Compensatory and synchronous affiliations are rare under warming. Synchrony is rare in the control.

#For pred.prob, if asymp.LCL is below 0, then set it to 0
pred.prob$asymp.LCL <- ifelse(pred.prob$asymp.LCL < 0, 0, pred.prob$asymp.LCL)

pred.prob

#
# Manuscript figure
#

# Define treatment colors
treatment_colors <- c("XXX" = "grey40",      # Control
                      "PXX" = "royalblue",   # Snow
                      "XNX" = "#381a61",     # Nitrogen 
                      "XXW" = "#ab3329")      # Warming

# Set factor levels to control panel and treatment order
pred.prob$aff.name <- factor(
  pred.prob$aff.name, 
  levels = c("Synchrony", "Compensatory", "None"))

pred.prob$treatment <- factor(pred.prob$treatment, 
                              levels=c('XXX',
                                       'XXW',
                                       'PXX',
                                       'XNX'))

# Create facet labels with colored text using ggtext
affiliation_labels <- c(
  "Synchrony" = "<span style='color:#018571;'>**Synchronous**</span>",
  "Compensatory" = "<span style='color:#a6611a;'>**Compensatory**</span>",
  "None" = "<span style='color:grey40;'>**No affiliation**</span>")


# Create the plot 
multinom.plot <- ggplot(pred.prob, 
                        aes(x = treatment, y = prob, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.3, alpha = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.9),
                width = 0.25, linewidth = 0.8) +
  facet_wrap(~ aff.name, ncol = 1, scales = "free_y",
             labeller = labeller(aff.name = affiliation_labels)) +
  scale_x_discrete(labels = NULL) + 
  scale_y_continuous(breaks = function(x) {
    if(max(x) > 0.6) {
      seq(0, 1.0, 0.5)
    } else {
      seq(0, 0.4, 0.2)
    }
  },
  limits = function(x) {
    if(max(x) > 0.6) {
      c(0, 1.0)
    } else {
      c(0, 0.46)  # Same limits for both Synchrony and Compensatory
    }
  }) +  
  labs(x = NULL,
       y = "Probability of affiliation",
       fill = NULL) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(0.8, "lines"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_markdown(size = 16),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  ) +
  scale_fill_manual(values = treatment_colors,
                    labels = c("XXX" = "Control",
                               "XXW" = "+ Warming",
                               "PXX" = "+ Snow",
                               "XNX" = "+ Nitrogen")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
multinom.plot

setwd("H:/My Drive/Synchrony/Graphics")
ggsave("Fig.2B_Multinom.all.years.png", multinom.plot, 
       width = 3.5, height = 4.5, dpi = 600)


#Save as an RDS
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
saveRDS(multinom.plot, "Figure.2B_Multinom.all.years.rds")



