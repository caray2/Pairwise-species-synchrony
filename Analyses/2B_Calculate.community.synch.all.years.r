#
# Biblioteca
#

library(codyn)
library(dplyr)
library(tidyr)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/2B_Calculate.community.synch.all.years.R")

#Open post-cleaning data
focal.cover <- read.csv(here("Data", "Tidy", "focal.cover.csv"), as.is = T)

#Remove 2006, which was a pre-treatment year
cover.experimental.years <- focal.cover %>% filter(year != 2006)

#Check that the species are only the focal species
unique(focal.cover$NWT_code)

bn <-10000

#Separate the data by treatment
control.cover <- cover.experimental.years %>% filter(treatment=="XXX")
snow.cover <- cover.experimental.years %>% filter(treatment=="PXX")
nitrogen.cover <- cover.experimental.years %>% filter(treatment=="XNX")
warming.cover <- cover.experimental.years %>% filter(treatment=="XXW")



Control.VR <- variance_ratio(control.cover,
                             time.var = "year",
                             species.var = "NWT_code",
                             abundance.var = "hits",
                             replicate.var = "repl", 
                             bootnumber = bn,
                             average.replicates = T)

Snow.VR <- variance_ratio(snow.cover,
                             time.var = "year",
                             species.var = "NWT_code",
                             abundance.var = "hits",
                             replicate.var = "repl", 
                             bootnumber = bn,
                             average.replicates = T)

Nitrogen.VR <- variance_ratio(nitrogen.cover,
                             time.var = "year",
                             species.var = "NWT_code",
                             abundance.var = "hits",
                             replicate.var = "repl", 
                             bootnumber = bn,
                             average.replicates = T)

Warming.VR <- variance_ratio(warming.cover,
                             time.var = "year",
                             species.var = "NWT_code",
                             abundance.var = "hits",
                             replicate.var = "repl", 
                             bootnumber = bn,
                             average.replicates = T)

#Clean up dfs
Control.VR$treatment <- "XXX"
Snow.VR$treatment <- "PXX"
Nitrogen.VR$treatment <- "XNX"
Warming.VR$treatment <- "XXW"

#Bind rows
all.VR <- bind_rows(Control.VR, Snow.VR, Nitrogen.VR, Warming.VR)

#Remove unnecessary rows
VR.tidy <- all.VR %>%
  dplyr::select(-nullmean)

#Give names to treatments
VR.tidy <- VR.tidy %>%
  mutate(treatment.name = case_when(
    treatment == "XXX" ~ "Control",
    treatment == "PXX" ~ "+ Snow", 
    treatment == "XNX" ~ "+ Nitrogen",
    treatment == "XXW" ~ "+ Warming"))

VR.tidy$window <- paste(min(cover.experimental.years$year), 
                        max(cover.experimental.years$year), sep = "-")
VR.tidy$bn <-bn

#Save output
write.csv(VR.tidy, here("community.synch.all.years.csv"), row.names = F)