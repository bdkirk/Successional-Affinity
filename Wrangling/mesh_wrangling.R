# File created on 18 April 2018

# Will look to answer whether size is being biased in capture of seed rain.

# load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library (scales); library(tidyr)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

# bring in data
size <-read_excel("Seedsize_sa2.xlsx", sheet=2, col_names=TRUE, na= "NA")

spp_abund <- read.csv("mesh_seedsize.csv", header = TRUE)

# Questions: 
  #1. Does mesh size bias seed arrival? Are small seeds equally likely to be captured. Do mesh sizes greater than 1 mm capture seeds smaller than 1 mm?
  #2. If it does not, what sort of bias is introduced?
  #3. What are the consequences for interpreting data?


#1. clean up files for comparison
size2 <- subset(size, select = c(3, 4, 5, 6, 13))

# turn from long to wide
spp_abund2 <- spread(spp_abund, meshtype, seednum)

# make NAs 0s
spp_abund2[is.na(spp_abund2)] <- 0

size_mesh <- left_join(size2, spp_abund2, by = "species")

size_mesh2 <- left_join(size2, spp_abund, by = "species")

# plotting
ggplot(size_mesh2, aes(size_cat, seednum))+
  geom_bar(aes(fill = meshtype), stat = "identity", position = "dodge")


# 2. evaluate species richness
rich_mesh <- ddply(size_mesh2, .(meshtype, size_cat), summarise, richness= length(species))

ggplot(rich_mesh, aes(size_cat, richness))+
  geom_bar(aes(fill=meshtype), stat = "identity", position = "dodge")

# 3. evaluate what species are being missed. 

dispersal_mesh <- ddply(size_mesh2, .(meshtype, dispersal), summarise, seednum=sum(seednum))

lifeform_mesh <- ddply(size_mesh2, .(meshtype, lifeform), summarise, seednum=sum(seednum))


## None of the above incorporates treatment.