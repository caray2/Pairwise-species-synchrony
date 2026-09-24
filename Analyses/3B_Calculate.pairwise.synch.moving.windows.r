#
# Biblioteca
#

library(codyn)
library(dplyr)
library(tidyr)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/3B_Calculate.pairwise.synch.moving.windows.R")

#Open post-cleaning tidy cover data that is only the focal species
data.full  <- read.csv(here("Data", "Tidy", "focal.cover.csv"), as.is = TRUE)

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
# 4 treatments * 28 species pairs * 12 moving windows * 6 replicates
4*28*12*6

res.df <- data.frame(matrix(NA, nrow=8064, ncol = 7))

results <- setNames(res.df, c("Repl",
                              "VR",
                              "start.year",
                              "end.year", 
                              "treatment",
                              "pair1", 
                              "pair2"
                              ))

#1_ ARTSCO_BISBIS###
#2_ ARTSCO_CALLEP###
#3_ ARTSCO_CARSCO###
#4_ ARTSCO_DESCES###
#5_ ARTSCO_GENALG###
#6_ ARTSCO_GEUROS###
#7_ ARTSCO_TRIPAR###

#8_ BISBIS_CALLEP###
#9_BISBIS_CARSCO###
#10_BISBIS_DESCES###
#11_BISBIS_GENALG###
#12_BISBIS_GEUROS###
#13_BISBIS_TRIPAR###

#14_CALLEP_CARSCO###
#15_CALLEP_DESCES###
#16_CALLEP_GENALG###
#17_CALLEP_GEUROS###
#18_CALLEP_TRIPAR###

#19_CARSCO_DESCES###
#20_CARSCO_GENALG###
#21_CARSCO_GEUROS###
#22_CARSCO_TRIPAR###

#23_DESCES_GENALG###
#24_DESCES_GEUROS###
#25_DESCES_TRIPAR###

#26_GENALG_GEUROS###
#27_GENALG_TRIPAR###

#28_GEUROS_TRIPAR###

pair.list <- list(c('ARTSCO',
                    'ARTSCO',
                    'ARTSCO',
                    'ARTSCO',
                    'ARTSCO',
                    'ARTSCO',
                    'ARTSCO',
                    
                    'BISBIS',
                    'BISBIS',
                    'BISBIS',
                    'BISBIS',
                    'BISBIS',
                    'BISBIS',
                    
                    'CALLEP',
                    'CALLEP',
                    'CALLEP',
                    'CALLEP',
                    'CALLEP',
                    
                    'CARSCO',
                    'CARSCO',
                    'CARSCO',
                    'CARSCO',
                    
                    'DESCES',
                    'DESCES',
                    'DESCES',
                    
                    'GENALG',
                    'GENALG',
                    
                    'GEUROS'
),

c('BISBIS',
  'CALLEP',
  'CARSCO',
  'DESCES',
  'GENALG',
  'GEUROS',
  'TRIPAR',
  
  'CALLEP',
  'CARSCO',
  'DESCES',
  'GENALG',
  'GEUROS',
  'TRIPAR',
  
  'CARSCO',
  'DESCES',
  'GENALG',
  'GEUROS',
  'TRIPAR',
  
  'DESCES',
  'GENALG',
  'GEUROS',
  'TRIPAR',
  
  'GENALG',
  'GEUROS',
  'TRIPAR',
  
  'GEUROS',
  'TRIPAR',
  
  'TRIPAR'
))

pair <- data.frame(setNames(pair.list, c("Pair1", "Pair2")))

counter <- 1

bn <-1

for(t in trt){
  for(w in start_wind){
    for(r in 1:nrow(pair)){
      
      #subset to trt
      dat_ss1 <- dplyr::filter(data.full, treatment==t)
      #subset to window
      dat_ss2 <- dplyr::filter(dat_ss1, year >=w &
                                 year < w+7)
      #subset to pair
      dat_ss3_p1 <- dplyr::filter(dat_ss2,
                                  NWT_code %in% pair[r,1])
      
      dat_ss3_p2 <- dplyr::filter(dat_ss2,
                                  NWT_code %in% pair[r,2])
      
      dat_ss3_full <- bind_rows(dat_ss3_p1, dat_ss3_p2)
      
      
      temp_results <- variance_ratio(dat_ss3_full,
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
      results[counter:(counter + 5), 6] <- pair[r,1]
      results[counter:(counter + 5), 7] <- pair[r,2]
      
      counter <- counter + 6 
    }
  }
}


colnames(results)

results$bn <-bn


#Add columns that indicate the moving window and pair
results.w <-
  results %>%
  unite(window, start.year, end.year, sep = "-", remove = FALSE)

results.wp <-
  results.w %>%
  unite(pair, pair1, pair2, sep = "-", remove = FALSE)

#Add columns that summarize what affiliation a pair is and gives names to the treatments
df.tidy <- results.wp %>%
  mutate(treatment.name = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow",
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"))

#Save output
write.csv(df.tidy, here("pairwise.synch.window.7.years.replicates.csv"), row.names = F)
