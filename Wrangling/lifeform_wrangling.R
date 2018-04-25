# File created on 14 March 18 to use a more updated and complete ecology file in this Succession project.

# This wranging file is used to create data in Table 1

# load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library(scales)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

# Bring in data
lf_seed <-read_excel("Seedsize_sa.xlsx", sheet=2, col_names=TRUE, na= "NA")

# subset data to include only columns for successional affinity analysis

lf_seed2 <- subset(lf_seed, select = c(3, 4))

lf_seed2$species <- as.factor(lf_seed2$species)
lf_seed2$lifeform <- as.factor(lf_seed2$lifeform)

summary(lf_seed2)

# Need to add abundance data from each treatment to determine richness.

# bring in abundance file.
abund <- read.csv("abund_summary_year_notrtsp_nw.csv")
abund2 <- subset(abund, select = c(1:5))

lf_abund <- left_join(lf_seed2, abund2, by = "species")
summary(lf_abund)

lf_abund2 <- melt(lf_abund, id.vars = c("species", "lifeform"), value=seednum)
lf_abund3 <- filter(lf_abund2, value >= 1)

# % of seeds within each successional category
lf_abund4 <- ddply(lf_abund3, .(variable, lifeform), summarise, seednum=sum(value))
str(lf_abund4)

lf_abund5 <- lf_abund4 %>% group_by(variable) %>% summarise(trt_tot = sum(seednum)) %>% right_join(lf_abund4) %>% mutate(percent = percent(seednum/trt_tot))

# number of species within each successional affinity
lf_abund6 <- ddply(lf_abund3, .(variable, lifeform), summarise, richness = length(species))

