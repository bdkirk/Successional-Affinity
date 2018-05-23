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
str(size2)
size2$species <- as.factor(size2$species)
size2$lifeform <- as.factor(size2$lifeform)
size2$dispersal <- as.factor(size2$dispersal)
size2$size_cat <- as.factor(size2$size_cat)
size2$suc_affinity <- as.factor(size2$suc_affinity)


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

rich_mesh2 <- ddply(size_mesh2, .(size_cat, meshtype), summarise, richness= length(species))

## None of the above incorporates treatment.

# The below text does not correctly get the ratio because it would be by species and treatment.

# I want to get the ratio of seeds of a particular species and the abundance.

 # size mesh file has categories of species broken into regular and fine

reg_ratio <- ddply(size_mesh, .(size_cat), summarise, reg=(sum(meshreg)/2))

sm_ratio <- ddply(size_mesh, .(size_cat), summarise, small=(sum(meshsmall)/3))

ratio <- left_join(sm_ratio, reg_ratio, by = "size_cat")
ratio$ratio <- ((ratio$reg)/(ratio$small))

# think about plotting the boxplot, 
# boxplot requires multiple variables. May need to break this down by plot? or treatment?

# by species?

size_mesh$ratio <- (size_mesh$meshreg/2)/(size_mesh$meshsmall/3)

size_mesh$ratio2 <- ((size_mesh$meshreg/2)+0.1)/((size_mesh$meshsmall/3)+0.1)
# Plot ratios by size category!!

ggplot(size_mesh, aes(size_cat, ratio))+
  geom_boxplot()+
  ylim(0, 6)


# Talked to Katie Rey. Try changing to percentages.
size_mesh$total <- (size_mesh$meshreg/2) + (size_mesh$meshsmall/3)
size_mesh$reg_percent <- ((size_mesh$meshreg/2)/size_mesh$total)*100
size_mesh$fine_percent <- ((size_mesh$meshsmall/3)/size_mesh$total)*100


# get median values to put on boxplot
size_med <- ddply(size_mesh, .(size_cat), summarise, med= median(reg_percent))
str(size_med)
size_med$size_cat <- as.factor(size_med$size_cat)

ggplot(size_mesh, aes(size_cat, reg_percent))+
  geom_boxplot()+
  theme_classic()+
  ylab("Capture efficency of regular mesh")+
  xlab("Seed Sizes")+
  geom_text(size_med, aes(x= size_cat, y= med, label = med), size = 3, vjust =0.5)
  #scale_fill_manual(labels= c("Large", "Medium", "Small", "Very Small"))# last line isn't working.

ggplot(size_mesh, aes(size_cat, fine_percent))+
  geom_boxplot()+
  theme_classic()+
  ylab("Capture efficency of fine mesh")+
  xlab("Seed Sizes")

# look at species capture efficiency: 


# Functional groups: 



#1) Dispersal mode

# % of seeds within each dispersal mode category
dispersal <- ddply(size_mesh2, .(meshtype, dispersal), summarise, seednum=sum(seednum))
str(dispersal)

dispersal2 <- dispersal %>% group_by(meshtype) %>% summarise(mesh_tot = sum(seednum)) %>% right_join(dispersal) %>% mutate(percent = percent(seednum/mesh_tot))

# number of species within each successional affinity
dispersal3 <- ddply(size_mesh2, .(meshtype, dispersal), summarise, richness = length(species))

# 2) Life form
# % of seeds within each dispersal mode category
lifeform <- ddply(size_mesh2, .(meshtype, lifeform), summarise, seednum=sum(seednum))
str(lifeform)

lifeform2 <- lifeform %>% group_by(meshtype) %>% summarise(mesh_tot = sum(seednum)) %>% right_join(lifeform) %>% mutate(percent = percent(seednum/mesh_tot))

# number of species within each successional affinity
lifeform3 <- ddply(size_mesh2, .(meshtype, lifeform), summarise, richness = length(species))

# 3) Successional affinity
# % of seeds within each dispersal mode category
sucaffinity <- ddply(size_mesh2, .(meshtype, suc_affinity), summarise, seednum=sum(seednum))
str(sucaffinity)

sucaffinity2 <- sucaffinity %>% group_by(meshtype) %>% summarise(mesh_tot = sum(seednum)) %>% right_join(sucaffinity) %>% mutate(percent = percent(seednum/mesh_tot))

# number of species within each successional affinity
sucaffinity3 <- ddply(size_mesh2, .(meshtype, suc_affinity), summarise, richness = length(species))

# 4) What about the seed source?

adults <- read_excel("ECOS_SeedRain_9Sept17_ar.xlsx", sheet = 2, col_names=TRUE, na= "NA")

#remove column of raw, uncorrected data
adults2 <- subset(adults, select = c(2, 4))
colnames(adults2) <- c("species", "adult")
str(adults2)
adults2$species <- as.factor(adults2$species)
adults2$species <- tolower(adults2$species)

adult_mesh <- merge(size_mesh, adults2, by = "species")
adult_mesh$species <- as.factor(adult_mesh$species)
adult_mesh$lifeform <- as.factor(adult_mesh$lifeform)
adult_mesh$size_cat <- as.factor(adult_mesh$size_cat)
adult_mesh$adult <- as.factor(adult_mesh$adult)
adult_mesh$suc_affinity <- as.factor(adult_mesh$suc_affinity)
adult_mesh$dispersal <- as.factor(adult_mesh$dispersal)
str(adult_mesh)

#Consider breaking into four different groups: 
# question: are there differences in what the mesh traps are catching based on seed source? first filter by which trap is catching it and then by seed source.
# filter to create file of what's captured in regular mesh only
mesh_reg <- filter(adult_mesh, meshreg >"0")

#file that filters out whether adults are present in the species caught in the regular mesh
mesh_reg_yes <- filter(mesh_reg, adult == "y")
mesh_reg_no <- filter(mesh_reg, adult =="n")

# Create a new file with just what's captured in the fine mesh
mesh_fine <- filter(adult_mesh, meshsmall > "0")

# Create files that filter potential seed source by adult presence in the fine mesh
mesh_fine_yes <- filter(mesh_fine, adult =="y")
mesh_fine_no <- filter(mesh_fine, adult =="n")

# compare summaries
# adults present
summary(mesh_reg_yes)
summary(mesh_fine_yes)

# no adult species present
summary(mesh_reg_no)
summary(mesh_fine_no)

##### Make tables for paper of species that were caught in small sized mesh but not found there. ####

# 1) Table of vs seeds not capture in regular mesh



# 2) Table of other sized seeds not captured in regular mesh





# 3) Table of seeds captured in regular mesh that were not captured in the small mesh?



# writing tidy files
# change file directory

write.csv(size_mesh, "mesh_size.csv", row.names= FALSE)

