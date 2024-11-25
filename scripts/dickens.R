## dickens.R
##
## Dylan Schwilk
##
## Data from BIOL-3405 field trip to Dickens springs on 9/14/2024. Work
## included predawn and midday water potentials

library(readr)
library(dplyr)
library(ggplot2)

# read data from csv filess
wp <- read_csv("data/dickens/wp.csv")
species <- read_csv("data/dickens/species.csv")
#species <- read.csv("data/dickens/species.csv", stringsAsFactors=FALSE)

leaves <- read_csv("data/dickens/leaf_area.csv")
pv_leaves <- read_csv("data/dickens/pv_leaf_dickens.csv")

## Calculate LMA and leaf size trait:
leaves <- mutate(leaves, LMA=dry_mass/leaf_area, leaf_size=leaf_area/n_leaves)

# Merge water potential data with leaf trait data and then with species info

dickens <- left_join(wp, leaves)
dickens <- left_join(dickens, species)

## Look at all data:
dickens

nrow(dickens)
# 28

## print column names:
names(dickens)

########################################################
# Selecting the traits from pv_leaves that doesn't overlap with
# leaves and merge with dickens by spcode
#################################################################

pv_leaves <- pv_leaves %>%
  dplyr::select(- c("lma", "leaf_area"))

dickens <- left_join(dickens, pv_leaves, by = "spcode")

## Summarize water potential and trait values by genus. This is the same as
## sumamrizing by species because we have no genera with more than one species
## here.
by_genus <- group_by(dickens, genus)
genus_means <- summarize(by_genus, mean_pd=mean(predawn_wp), mean_md=mean(midday_wp),
          mean_lsize = mean(leaf_size, na.rm=TRUE), mean_LMA = mean(LMA, na.rm=TRUE),
          mean_ldmc = mean(ldmc, na.rm = TRUE),
          leaf_length = mean(leaf_length, na.rm = TRUE),
          tlp = mean(tlp, na.rm = TRUE),
          rwc_tlp = mean(rwc_tlp, na.rm = TRUE),
          capacitance_above_tlp = mean(capacitance_above_tlp, na.rm = TRUE),
          capacitance_below_tlp = mean(capacitance_below_tlp, na.rm = TRUE),
          modulus_elasticity = mean(modulus_elasticity, na.rm = TRUE),
          swc = mean(swc, na.rm = TRUE),
          osmotic_potential = mean(osmotic_potential, na.rm = TRUE))
          

# Water potential figures
ggplot(dickens, aes(predawn_wp, midday_wp, color=genus)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE)

ggsave("results/dickens_pd_vs_md.pdf")

ggplot(dickens, aes(genus, predawn_wp-midday_wp)) +
  geom_boxplot()

ggplot(dickens, aes(predawn_wp, predawn_wp-midday_wp, color=genus)) +
  geom_point(size=3)



## Species comparisons

ggplot(genus_means, aes(mean_LMA, mean_pd, color=genus)) +
  geom_point(size=3)
