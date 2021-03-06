# This file was created on 9 March 18

# This file will look at seed size and different trait attributes

# load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library (scales)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

# Bring in data
size <-read_excel("Seedsize_sa.xlsx", sheet=2, col_names=TRUE, na= "NA")

# subset data to include only columns for seed size analysis

size2 <- subset(size, select = c(3, 4, 5, 6))

size2$species <- as.factor(size2$species)
size2$lifeform <- as.factor(size2$lifeform)
size2$dispersal <- as.factor(size2$dispersal)
size2$size_cat <- as.factor(size2$size_cat)
str(size2)


# remove rows where no size category is available

#size3 <- filter(size2, size_cat != "NA")
# 91 species classified to suc_affinity

# Need to add abundance data from each treatment to determine richness.

# bring in abundance file.
abund <- read.csv("abund_summary_year_notrtsp_nw.csv")
abund2 <- subset(abund, select = c(1:5))

size_abund <- left_join(size2, abund2, by = "species")
summary(size_abund)

#To create table for treatment effects and most abundant species by size with other attributes
size_abund_table <- size_abund
size_abund_table$total <- size_abund$Hial+size_abund$Pema+size_abund$Viko+size_abund$Vogu

size_abund2 <- melt(size_abund, id.vars = c("species", "dispersal", "lifeform", "size_cat"), value=seednum)
size_abund3 <- filter(size_abund2, value >= 1)

# % of seeds within each successional category
size_abund4 <- ddply(size_abund3, .(variable, size_cat), summarise, seednum=sum(value))
str(size_abund4)

size_abund5 <- size_abund4 %>% group_by(variable) %>% summarise(trt_tot = sum(seednum)) %>% right_join(size_abund4) %>% mutate(percent = ((seednum/trt_tot)*100))

# number of species within each successional affinity
size_abund6 <- ddply(size_abund3, .(variable, size_cat), summarise, richness = length(species))



# Making a stacked bar chart of species richness by seed size category

ggplot(size_abund6, aes(variable, richness))+
  geom_col(aes(fill = size_cat), position = "dodge")+
  labs(x="Treatment", y= "Species Richness") + 
  theme_bw()+
  scale_x_discrete(limits = c("Hial", "Viko", "Pema", "Vogu"))+
  scale_fill_discrete(name= "Seed Size", labels= c("Large", "Medium", "Small", "Unknown"))


# Making a stacked bar chart of percent by seed size category
library(ggthemes)

ggplot(size_abund5, aes(variable, percent))+ 
  geom_bar(aes(fill = size_cat), position = "dodge", stat = "identity")+
  labs(x="Treatment", y= "Total Seeds") +
  theme_bw()+
  scale_x_discrete(limits = c("Hial", "Viko", "Pema", "Vogu"))+
  scale_fill_discrete(name= "Seed Size", labels= c("Large", "Medium", "Small", "Unknown"))


