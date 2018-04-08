# File started on 9 March 2018

# This file will look at successional affinity and ecological attributes of the seed

# load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library(scales)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

# Bring in data
sa_seed <-read_excel("Seedsize_sa.xlsx", sheet=2, col_names=TRUE, na= "NA")

# subset data to include only columns for successional affinity analysis

sa_seed2 <- subset(sa_seed, select = c(3, 4, 5, 6, 13))

sa_seed2$species <- as.factor(sa_seed2$species)
sa_seed2$lifeform <- as.factor(sa_seed2$lifeform)
sa_seed2$dispersal <- as.factor(sa_seed2$dispersal)
sa_seed2$size_cat <- as.factor(sa_seed2$size_cat)
sa_seed2$suc_affinity <- as.factor(sa_seed2$suc_affinity)
str(sa_seed2)


# remove rows where no successional affinity is available

#sa_seed3 <- filter(sa_seed2, suc_affinity != "NA")
# 59 species classified to suc_affinity

# Need to add abundance data from each treatment to determine richness.

# bring in abundance file.
abund <- read.csv("abund_summary_year_notrtsp_nw.csv")
abund2 <- subset(abund, select = c(1:5))

sa_abund <- left_join(sa_seed2, abund2, by = "species")
summary(sa_abund)

sa_abund2 <- melt(sa_abund, id.vars = c("species", "dispersal", "lifeform", "size_cat", "suc_affinity"), value=seednum)
sa_abund3 <- filter(sa_abund2, value >= 1)

# % of seeds within each successional category
sa_abund4 <- ddply(sa_abund3, .(variable, suc_affinity), summarise, seednum=sum(value))
str(sa_abund4)

sa_abund5 <- sa_abund4 %>% group_by(variable) %>% summarise(trt_tot = sum(seednum)) %>% right_join(sa_abund4) %>% mutate(percent = percent(seednum/trt_tot))

# number of species within each successional affinity
sa_abund6 <- ddply(sa_abund3, .(variable, suc_affinity), summarise, richness = length(species))


# Consider looking at successional affinity of species with adults absent from plots

# bring in data
adults <- read_excel("ECOS_SeedRain_9Sept17_ar.xlsx", sheet = 2, col_names=TRUE, na= "NA")

#remove column of raw, uncorrected data
adults2 <- subset(adults, select = c(2, 4))
colnames(adults2) <- c("species", "adult")
str(adults2)
adults2$species <- as.factor(adults2$species)
adults2$species <- tolower(adults2$species)

adult_sa <- merge(sa_abund3, adults2, by = "species")

# 1) Does the richness or abundance of seed vary by treatment?

## a) abundance
abund_source <- ddply(adult_sa, .(variable, adult), summarise, seednum=sum(value))

ggplot(abund_source, aes(variable, seednum))+
  geom_bar(aes(fill= adult),stat= "identity", position = "dodge")

## b) richness
rich_source <- ddply(adult_sa, .(variable, adult), summarise, richness = length(species))

ggplot(rich_source, aes(variable, richness))+
  geom_bar(aes(fill= adult),stat= "identity", position = "dodge")

# 2) Does the SA vary across treatments for incoming species seeds?
sa_yes <- filter(adult_sa, adult =="y")
sa_yes2 <- ddply(sa_yes, .(variable))

sa_no <- filter(adult_sa, adult =="n")
summary(sa_no)


## a) What is difference between species with adults present and adults absent?



# 3) Does the seed size vary across treatments for incoming seed species?

# answered above in summary