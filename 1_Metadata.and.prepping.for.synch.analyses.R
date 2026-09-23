
#
# Biblioteca
#

library(dplyr)
library(tidyr)

set.seed(7220)

#Open raw data from the EDI data portal. These data are 2006 through 2024.
setwd("H:/My Drive/Synchrony/Data/Raw")
s.raw <- read.csv("itex_sppcomp.ks.data.downloaded.8.12.2025.csv")

#Concatenate-block, plot, and code into a replicate column
s.raw = s.raw %>% unite(repl, c(plot, block, code), sep = "_", remove = FALSE)

# Before any filtering how many block are there
length(unique(s.raw$block)) # 3 blocks

# Before any filtering how many treatments are there
length(unique(s.raw$code)) # 8 treatments

# Before any filtering how many plots are there
length(unique(s.raw$plot)) # 48 plots 

#There are 2 plots of each treatment type in each of the 3 blocks
s.raw %>%
  group_by(block, code) %>%
  summarise(n_plots = n_distinct(plot), .groups = "drop")

#Just look at the treatments we're interested in and remove things that aren't vascular plants from NWT_code and only keep the columns we care about.
s.all <-s.raw %>% filter(code %in% c("XXX", "PXX", "XNX", "XXW")) %>% 
  filter(!NWT_code %in% c("bare", "lichen", "litter", "moss", "rock")) %>%
  dplyr::select(year, repl, code, NWT_code, USDA_name, hits)

#Remove s.raw to avoid confusion
rm(s.raw)

#Change the column name 'code' to treatment
colnames(s.all)[which(colnames(s.all) == 'code')] <- 'treatment'

#Check filter worked well. 24 plots. We kept four treatments and there are 2 plots for each treatment in each of the 3 blocks
2 * 3 * 4 == 24
length(unique(s.all$repl)) == 24

#Look at the unique species across all years (2006-2024)
n_distinct(s.all$NWT_code) #47

n_distinct(s.all$USDA_name) #50

#There are 47 species at Niwot in our focal plots, including those only identified to genus based on the NWT code. The USDA name has 3 more unique names due to inconsistent inclusion of the subspecies name

#This data is only vegetative hits during point counts. Some plots have veg cover >100%. This is from the addition of cover=0.5 for plants that weren't counted at the points but were present in the plots. Look at the data for a replicate with high cover to get a sense of the data

s.all %>%
  filter(repl == '24_2_XXX') %>%
  filter(year == 2023)

#See what the average vegetation cover was per plot (=repl) by treatment once treatments were implemented (no 2006)
veg.cover.per.plot <- s.all %>%
  filter(year != 2006) %>%
  group_by(year, treatment, repl) %>%
  summarise(
    total.veg.cover = sum(hits),
    .groups = "drop")

#There are 6 plots per treatment (n=4) across 18 years. So for each treatment there should be 108 rows
avg.veg.cover_treatment <-
  veg.cover.per.plot %>%
  group_by(treatment) %>%
  summarise(mean.cover_plot = round(mean(total.veg.cover), 2),
            se.cover_plot = round(sd(total.veg.cover) / sqrt(n()), 2),
            .groups = "drop")


avg.veg.cover_treatment


# Verify summary stats
veg.cover.per.plot %>%
  filter(treatment == "XXX") %>%  # check one treatment at a time
  summarise(
    mean = mean(total.veg.cover),
    sd = sd(total.veg.cover),
    n = n(),
    se = sd(total.veg.cover) / sqrt(n()))


#
# Select the focal species
#

#Sum the number of hits per treatment and year by species (NWT_code) across all plots
#Note this is now 2006 - 2024
total.cover <- s.all %>%
  group_by(year, treatment, NWT_code) %>%
  summarise(
    total.cover = sum(hits),
    .groups = "drop")

total.cover

#For each year rank cover of each species by treatment
rank.sp <- total.cover %>% 
  group_by(year, treatment) %>% 
  mutate(rank=dense_rank(desc(total.cover)))

#Look at the species with the most cover by year in each plot
dominant.sp <- as.data.frame(rank.sp %>% filter(rank==1))

#Deschampsia is the dominant species in every year and treatment
xtabs(~NWT_code, data = dominant.sp)

#rank sums by year and code for species that were at least within the 4 most common species in one year and for one treatment (hits are summed across plots)
top.4.species <- rank.sp %>%
  filter(rank <=4)

unique(top.4.species$NWT_code)

#"ARTSCO" "CARSCO" "DESCES" "GEUROS" "GENALG" "CALLEP" "BISBIS" "TRIPAR"


#Filter s.all data set so that it's just the eight species with the highest cover and remove the USDA_names. We can add the sci names after synchrony calculations.
s.focal <- s.all %>% 
  filter(NWT_code %in% c("ARTSCO", "BISBIS", "CALLEP", "CARSCO", "DESCES", "GENALG", "GEUROS", "TRIPAR")) %>%
  dplyr::select(-USDA_name)


#Make sure I still have 24 plots
length(unique(s.focal$repl))==24 #I do!


nrow(s.focal)
# 's.focal' has 3505 rows. This data set doesn't include 0's for species that aren't present. If there was a row for every species there would be
# 24 plots * 8 species * 19 years of data
24 * 8 * 19 == 3648

#To prevent data gaps add in 0s for plots and years when the species is not present. Create an empty data frame with all blocks, plots, years, treatments, and years.

#Data frame all 24 replicates (4 treatments * 6 plots)
repl <- s.focal %>% distinct(repl, treatment)

#Vector of focal species
NWT_code <- c("ARTSCO", "BISBIS", "CALLEP", "CARSCO", "DESCES", "GENALG", "GEUROS", "TRIPAR")

unique(s.focal$USDA_name) 

#All possible species x treatment x year combos
all.pos <- crossing(repl, NWT_code = NWT_code, year = 2006:2024)

#Check that the number of rows makes sense
nrow(all.pos) == 24 * 8 * 19 

#Check that structure is comparable between s.focal and all.pos and join them
str(s.focal)
str(all.pos)

s.tidy <- left_join(all.pos, s.focal, by = c("repl", "treatment", "NWT_code", "year"))

#If number of hits is NA, make it 0
s.tidy$hits[is.na(s.tidy$hits)] <- 0

#Check for replicates that are 0 in all years
hit.count <- s.tidy %>%
  group_by(repl, NWT_code) %>%
  summarise(
    hc = sum(hits))

hit.count %>% filter(hc==0)

#CALLEP is absent in some plots in all years. One warming and one control plot from different blocks.
#21_2_XXW_CALLEP     
#4_1_XXX_CALLEP  

#Check number of rows is correct
# 24 plots * 8 species * 19 years of data
24 * 8 * 19 == 3648
24 * 8 * 19 == nrow(s.tidy)

#Save focal cover data
setwd("H:/My Drive/Synchrony/Data/Tidy")
write.csv(s.tidy, "focal.cover.csv", row.names = F)
