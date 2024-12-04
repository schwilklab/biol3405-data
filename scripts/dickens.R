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
## convert leaf area from cm^2 to m^2
leaves <- mutate(leaves, leaf_area = leaf_area/(10000), LMA=dry_mass/leaf_area, leaf_size=leaf_area/n_leaves)

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


## Figure request by Esther, Lauren, Zak group:
## For one of the graphs, the x axis would be average LMA for each genus
## and the Y would be average turgor loss point for the four genus’ that
## we have.

simple_theme <-  theme(panel.border = element_rect(size = 1.6, fill=NA),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_rect(size = 1.6, fill = NA),
                       legend.background = element_rect(fill = "transparent"),
                       legend.key = element_rect(fill="transparent"))
 


lma_tlp_plot <- ggplot(genus_means, aes(mean_LMA, tlp, color=genus)) +
  geom_point(size=3) +
  labs(x =expression(paste("Leal mass per area (", g~m^-2, ")")),
       y = "Turgor loss point (MPa)") +
  simple_theme +
  theme(legend.title = element_blank(),
        legend.text = element_text(face="italic"))
       # legend.position = c(.7, 0.72))
lma_tlp_plot
ggsave("./results/lma_tlp.pdf", plot=lma_tlp_plot, height=8, width=8, units="cm")

## Request from Zander 2024-12-04:

## correlation test:
wp_diff <- genus_means$mean_pd - genus_means$mean_md
cor.test(wp_diff, genus_means$mean_LMA)

## More requests: There was also one more figure we were hoping to get. We were
## hoping to show relative rooting depth versus average predawn water potential
## for species. Maybe we could have two graphs right next to each other, one
## for shallow rooters and one for deep rooters? Celtis, Prunus, and Juniper
## are relatively shallow. Prosopis, Rhus, and Sarcomphalus are relatively
## deep.
 
root_predawn_plot <- ggplot(dickens, aes(rooting, predawn_wp, color=genus)) +
  geom_point(size=3, alpha=0.9,
             position=position_jitter(width=0.05, height=0)) +
  simple_theme

root_predawn_plot

## I was also wondering if there might be a way to show if there’s a
## statistical difference between the predawn water potentials for shallow
## versus deep rooters.

## DWS: Well, we can see from figure that there is no effect. We can run a two
## way anova with genus as fixed effect:
pd_mod <- lm(data=dickens, predawn_wp ~ rooting + genus)
summary(pd_mod)
anova(pd_mod)

## Species effect but no rooting effect. So, species differ but not explained
## by rooting depth.

## Conceptually, genus is really a random eeffect (mixed model) but with that
## won't fit with so few genera. We could do the (wrong) naive simple linear
## model ignoring that observations are nested within species:
pd_mod2 <- lm(data=dickens, predawn_wp ~ rooting)
summary(pd_mod2)
# No effect

