# File created on 14 March 18 to use a more updated and complete ecology file in this Succession project.

# This wranging file is used to create data in Table 1

# load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library(scales)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

# Bring in data
dm_seed <-read_excel("Seedsize_sa.xlsx", sheet=2, col_names=TRUE, na= "NA")

# subset data to include only columns for successional affinity analysis

dm_seed2 <- subset(dm_seed, select = c(3, 5))

dm_seed2$species <- as.factor(dm_seed2$species)
dm_seed2$dispersal <- as.factor(dm_seed2$dispersal)

summary(dm_seed2)

# Need to add abundance data from each treatment to determine richness.

# bring in abundance file.
abund <- read.csv("abund_summary_year_notrtsp_nw.csv")
abund2 <- subset(abund, select = c(1:5))

dm_abund <- left_join(dm_seed2, abund2, by = "species")
summary(dm_abund)

dm_abund2 <- melt(dm_abund, id.vars = c("species", "dispersal"), value=seednum)
dm_abund3 <- filter(dm_abund2, value >= 1)

# % of seeds within each successional category
dm_abund4 <- ddply(dm_abund3, .(variable, dispersal), summarise, seednum=sum(value))
str(dm_abund4)

dm_abund5 <- dm_abund4 %>% group_by(variable) %>% summarise(trt_tot = sum(seednum)) %>% right_join(dm_abund4) %>% mutate(percent = percent(seednum/trt_tot))

# number of species within each successional affinity
dm_abund6 <- ddply(dm_abund3, .(variable, dispersal), summarise, richness = length(species))

