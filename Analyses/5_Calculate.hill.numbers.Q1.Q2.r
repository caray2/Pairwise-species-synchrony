# Calculate Hill numbers q1 and q2
#
#
# Biblioteca
#

library(dplyr)
library(tidyr)
library(hillR)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/5_Calculate.hill.numbers.Q1.Q2.R")

# Open post-cleaning tidy data that is only the focal species
s <- read.csv(here("Data", "Tidy", "focal.cover.csv"), as.is = T) # This is the hit count data for focal species

# Calculate Hill numbers
# q1 is Shannon diversity (exponential of Shannon entropy)
# q2 is Simpson diversity (inverse Simpson index)

# Confirm that this data is only for the focal species

s %>%
  select(NWT_code) %>%
  distinct()

# Get unique combinations of year, treatment, and replicate
year_treatment_combos <- s %>%
  select(year, treatment, repl) %>%
  distinct()

# 6 replicates * 19 years data (2006 is included) * 4 treatments
6 * 19 * 4 == nrow(year_treatment_combos) #456!

# Calculate Hill numbers for each replicate by treatment and year
hill_list <- list()

for (i in 1:nrow(year_treatment_combos)) { #For each replicate
  yr <- year_treatment_combos$year[i] #Track the year
  trt <- year_treatment_combos$treatment[i] #Track the treatment
  plot <- year_treatment_combos$repl[i] #Track the replicate
  
  # Subset data for this year, treatment, and replicate
  subset_data <- s %>%
    filter(year == yr, treatment == trt, repl == plot) %>%
    select(NWT_code, hits)  # Track species with hits
  
  #Make a named vector
  hits_vector <- setNames(subset_data$hits, subset_data$NWT_code)
  
  # Calculate Hill numbers
  # q=1 gives exponential of Shannon entropy (effective number of common species)
  # q=2 gives inverse Simpson index (effective number of dominant species)
  q1 <- hill_taxa(hits_vector, q = 1)
  q2 <- hill_taxa(hits_vector, q = 2)
  
  # Store results
  hill_list[[i]] <- data.frame(
    year = yr,
    treatment = trt,
    repl = plot,
    q1 = q1,
    q2 = q2
  )
}

#Look at one of the data frames in hill_list
hill_list[[1]]

# Combine all results into a single data frame
hill.total <- bind_rows(hill_list)

#Give names to treatments
hill.total <- hill.total %>%
  mutate(treatment.name = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow", 
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"))

#Make columns for plot and block that can be used as random effects
hill.total <- hill.total %>%
  separate(repl, 
           into = c("plot", "block", "treatment_code"), 
           sep = "_", 
           remove = FALSE) %>%
  select(-treatment_code) #treatment is already in the data

head(hill.total)

#Add a year.number column
hill.total$n.year <- hill.total$year - 2006

#Rearrange columns
hill.total <- hill.total %>%
  dplyr::select(year, n.year, treatment, treatment.name, repl, block, plot, q1, q2)

# Look at structure of hill.total
print(xtabs(~year + treatment, hill.total))

# Save the annual Hill number data
setwd("H:/My Drive/Synchrony/Data/Tidy")
write.csv(hill.total, "hill.numbers.with.replicates.csv", row.names = F)
