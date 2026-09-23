#
# Biblioteca
#

library(codyn)
library(dplyr)
library(tidyr)
library(here)

#Open post-cleaning raw data (step 0) from Collins Ecology Letters paper
setwd("H:/My Drive/Synchrony/Data/Tidy")
focal.cover <- read.csv("focal.cover.csv")

#Remove 2006, which was a pre-treatment year
data <- focal.cover %>% filter(year != 2006)

set.seed(7220)

#Set up for loop parameters

trt <-c("XXX", "PXX", "XNX", "XXW")

#nrows needed in matrix
# 4 treatments * 28 species pairs
4*28

res.df <- data.frame(matrix(NA, nrow=112, ncol = 8))

results <- setNames(res.df, c("VR",
                              "lowerCI",
                              "upperCI",
                              "start.year",
                              "end.year", 
                              "pair1", 
                              "pair2",
                              "treatment"))

#1_ ARTSCO_BISBIS###
#2_ ARTSCO_CALLEP###
#3_ ARTSCO_CARSCO###
#4_ ARTSCO_DESCES###
#5_ ARTSCO_GENALG###
#6_ ARTSCO_GEUROS###
#7_ ARTSCO_TRIPAR###

#8_ BISBIS_CALLEP###
#9_ BISBIS_CARSCO###
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

bn <-10000 #set boot number

for(t in trt){
    for(r in 1:nrow(pair)){
      
      #subset to trt
      dat_ss1 <- dplyr::filter(data, treatment==t)

      #subset to pair
      dat_ss2_p1 <- dplyr::filter(dat_ss1,
                      NWT_code %in% pair[r,1])
      
      dat_ss2_p2 <- dplyr::filter(dat_ss1,
                      NWT_code %in% pair[r,2])

      
      dat_ss2_full <- bind_rows(dat_ss2_p1, dat_ss2_p2)

      temp_results <- variance_ratio(dat_ss2_full,
                                     time.var = "year",
                                species.var = "NWT_code",
                                abundance.var = "hits",
                                replicate.var="repl", 
                                bootnumber=bn,
                                average.replicates=T)

#"VR", "lowerCI", "upperCI", "start.year", "end.year", "pair1", "pair2", "trt"
      results[counter, 1] <-temp_results$VR
      results[counter, 2] <-temp_results$lowerCI
      results[counter, 3] <-temp_results$upperCI
      results[counter, 4] <-min(data$year)
      results[counter, 5] <-max(data$year)
      results[counter, 6] <-pair[r,1]
      results[counter, 7] <-pair[r,2]
      results[counter, 8] <-t
      
      
      counter <- counter + 1
      }
  }

colnames(results)

results$bn <-bn

# Assess significance
results %>% filter(VR==1) #Any weird ones with VR=1?

#If the variance ratio is larger than the upper confidence interval it's significantly synchronous. 
results$sig.synch <-(results$VR-results$upperCI) > 0
#If the variance ratio is smaller than the lower confidence interval it's significantly compensatory.
results$sig.comp  <-(results$VR-results$lowerCI) < 0

#Add columns that indicate the moving window and pair
results.w <- 
  results %>%
  unite(window, start.year, end.year, sep = "-", remove = FALSE)

results.wp <- 
  results.w %>%
  unite(pair, pair1, pair2, sep = "-", remove = FALSE)


#Add columns that summarize what affiliation a pair is and gives names to the treatments
df.tidy <- results.wp %>%
  mutate(affiliation = case_when(
    sig.synch == TRUE ~ 1,
    sig.comp == TRUE ~ -1,
    .default = 0
  )) %>%
  mutate(aff.name = case_when(
    affiliation == 1 ~ "Synchrony",
    affiliation == -1 ~ "Compensatory",
    affiliation == 0 ~ "None"
  )) %>%
  mutate(treatment.name = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow", 
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"))


#Save output
#Set working directory
setwd("H:/My Drive/Synchrony/Data/Tidy")
write.csv(df.tidy, "pairwise.synch.all.years.csv", row.names = F)
