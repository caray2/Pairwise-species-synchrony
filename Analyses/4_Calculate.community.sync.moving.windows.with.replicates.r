#
# Biblioteca
#

library(codyn)
library(dplyr)
library(tidyr)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/4_Calculate.community.sync.moving.windows.with.replicates.R")

#Open post-cleaning tidy data that is only the focal species
data.full <- read.csv(here("Data", "Tidy", "focal.cover.csv"), as.is = T)

#We want 7 year windows
#2007-2013 #Window 1
#2008-2014 #Window 2
#2009-2015 #Window 3
#2010-2016 #Window 4
#2011-2017 #Window 5
#2012-2018 #Window 6
#2013-2019 #Window 7
#2014-2020 #Window 8
#2015-2021 #Window 9
#2016-2022 #Window 10
#2017-2023 #Window 11
#2018-2024 #Window 12

#Set up for loop parameters

trt <-c("XXX", "PXX", "XNX", "XXW")
start_wind <- c(2007:2018)

#The 24 plots are spread across 3 blocks. Within blocks each treatment is replicated twice. Thus, there are 6 replicates of each of 4 treatments (6*4=24 plots).

#nrows needed in matrix
# 4 treatments * 12 moving windows * 6 replicates
4*12*6

res.df <- data.frame(matrix(NA, nrow=288, ncol = 5))

results <- setNames(res.df, c("replicate",
                              "VR",
                              "start.year",
                              "end.year",
                              "treatment"))

counter <- 1

bn <-1 #set boot number. Since we just want the VR value and not the confidence intervals to assess significance we don't need to generate a null matrix

for(t in trt){
  for(w in start_wind){
    
    #subset to trt
    dat_ss1 <- dplyr::filter(data.full, treatment==t)
    #subset to window
    dat_ss2 <- dplyr::filter(dat_ss1, year >=w &
                               year < w+7)
    
    # Calculate community-wide synchrony (all species together)
    temp_results <- variance_ratio(dat_ss2,
                                   time.var = "year",
                                   species.var = "NWT_code",
                                   abundance.var = "hits",
                                   replicate.var="repl", 
                                   bootnumber=bn,
                                   average.replicates=F)
    
    #"VR", "lowerCI", "upperCI", "start.year", "end.year", "treatment"
    # Assign all 6 replicates at once
    results[counter:(counter + 5), 1] <- temp_results$repl
    results[counter:(counter + 5), 2] <- temp_results$VR
    results[counter:(counter + 5), 3] <- w
    results[counter:(counter + 5), 4] <- w+6
    results[counter:(counter + 5), 5] <- t
    
    counter <- counter + 6 
  }
}


colnames(results)

results$bn <-bn

#Add column that indicates the moving window
results.w <- 
  results %>%
  unite(window, start.year, end.year, sep = "-", remove = FALSE)

#Give names to treatments
df.tidy <- results.w %>%
  mutate(treatment.name = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow", 
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"))

#Make columns for plot and block that can be used as random effects
df.tidy <- df.tidy %>%
  separate(replicate, 
           into = c("plot", "block", "treatment_code"), 
           sep = "_", 
           remove = FALSE) %>%
  select(-treatment_code) #treatment is already in the data

#Add an n year column that show what experimental year the start year was
df.tidy$n.year.start <- df.tidy$start.year - 2006

#Rearrange columns
df.tidy <- df.tidy %>%
  dplyr::select(start.year, end.year, window, n.year.start, treatment, treatment.name, replicate, block, plot, VR)

#Save output
#Set working directory
setwd("H:/My Drive/Synchrony/Data/Tidy")
write.csv(df.tidy, "community.synch.window.7.years.replicates.csv", row.names = F)
