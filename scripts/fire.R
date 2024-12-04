## fire.R
## Anna DesHotels
## Dylan Schwilk

## Code based on email from Anna 2024-12-04.

library(dplyr)
library(ggplot2)
library(stringr)

## Read transects
transects <- read.csv("./data/sacramentos/transects.csv", stringsAsFactors= FALSE)

## Turn transect column into globally unique ids.
transects <- transects %>% 
  mutate(group=substr(transect,1,1),
         transect = paste(site, transect, sep="."),
         dist = stop-start)

## Now just get the cover by species. We don't need the raw start/stop points.
## Note that each transect was 50 m long, so the proportional cover of any
## group (species, family, growth form, etc) is just the sum of the distances
## in that transect divided by 50. We can summarize by species now and then sum
## across other categories later.
cover <- group_by(transects, transect, spcode) %>% summarize(cover = sum(dist)/50)

## We must add in zeroes for missing species. 

# Use expand.grid to get all possible combinations of transect and species ids
all.transects.species <- expand.grid(transect = unique(cover$transect),
                                     spcode=unique(cover$spcode))

cover <- full_join(cover, all.transects.species)

## The above join/merge inserted NA for missing combinations of species and
## transects. NA should really be zero (missing species means zero cover)
cover$cover[is.na(cover$cover)] <- 0

## This data is not tidy: "site" is not its own column but is embedded in the
## transect ids. We must grab `site` as its own column because we will need it
## for analyses:
cover <- mutate(cover, site = str_split_fixed(transect, "\\.", n=2)[,1])

# also need species info 
species <- read.csv("./data/sacramentos/species.csv", stringsAsFactors=FALSE,
                    na.strings = "")

# also need fire traits info by species (AFD)
species <- read.csv("./data/sacramentos/Species_fire_traits.csv", stringsAsFactors=FALSE,
                    na.strings = "")

# This includes "fire" as a trait with values F, F+ or F-. Let's get a binary
# version of the trait as well: AFD

species <- mutate(species, fire.adapted = Fire %in% c("F+"))


## DWS: let's make sure all of our species codes are in the species.csv data:

tspecies <- unique(transects$spcode)
length(tspecies)
# 41

sspecies <- species$spcode
length(sspecies)
#46

unmatched <- tspecies[! tspecies %in% sspecies]
unmatched
# none!

# Merge species info with transect data. Use merge() function and remember that
# we want all rows from the transects data frame in the result. We don't need
# unmatched rows in species as the file contains the full species list over
# several years and some species were not found in 2011.
cover <- merge(cover, species, all.x=TRUE)

# Read in elevations of each site and merge with cover data
sites <- read.csv("./data/sacramentos/sites.csv", stringsAsFactors=FALSE, na.strings = "")

## Although elevation is numeric, it is really a categorical variable with six
## values. Let's ensure that ggplot will treat it as a categorical variable.
sites$elev <- as.factor(as.character(sites$elev))

# might be useful in transects data too:
transects <- transects %>% left_join(sites)

cover <- left_join(cover, sites)

# 'cover' is the tidy dataframe (tibble) for all further use. We can delete the
# intermediate objects to keep our environment cleaner (but I wont for now).
# rm(all.transects.species, sites, species, transects)

###############################################################################
## For some plots, we will want total woody cover (for example, to graph
## proportional cover of fire-adapted plants relative to total cover)
###############################################################################

total_cover <- group_by(cover, elev, transect) %>%
  summarize(total_cover = sum(cover))

total_cover_site <- group_by(total_cover, elev) %>%
  summarize(total_cover_site = sum(total_cover))

###############################################################################
## Proportional cover figures
###############################################################################

## Summarize proportional cover by transect and by fire trait (ft):
## F+ = Fire adapted, FA; F- = Not fire adapted, NFA

FA_cover <- filter(cover, fire.adapted ==TRUE) %>% group_by(elev, transect) %>%
  summarize(FA_cover=sum(cover))
FA_cover <- left_join(FA_cover, total_cover)

ggplot(FA_cover, aes(elev, FA_cover / total_cover)) +
  geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of fire adpated species")
ggsave("./results/FA_cover_by_elev.pdf")

NFA_cover <- filter(cover, fire.adapted == FALSE) %>% group_by(elev, transect) %>%
  summarize(NFA_cover=sum(cover))
NFA_cover <- left_join(NFA_cover, total_cover)

ggplot(NFA_cover, aes(elev, NFA_cover / total_cover)) +
  geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of non-fire adpated species")
ggsave("./results/NFA_cover_by_elev.pdf")

###############################################################################
## Let's try summarizing by a plant family
##############################################################################
by_elev_family <- group_by(cover, elev, transect, family)
family.cover <- summarize(by_elev_family, pcover = sum(cover))

## Conifers
conifer_families <- c("Pinaceae", "Cupressaceae")

conifers <-  subset(cover, family %in% conifer_families)
conifer.cover <- group_by(conifers, elev, transect) %>% summarize(pcover=sum(cover))

con.plot <- ggplot(conifer.cover, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of conifers")

con.plot
ggsave("./results/con_cover_by_elev.pdf")

## Anacardaciaceae
anicard <- subset(family.cover, family=="Anacardiaceae")

anicard.plot <- ggplot(anicard, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Anacardiaceae")

anicard.plot
ggsave("./results/anicard_cover_by_elev.pdf")

## Asparagaceae
aspar <- subset(family.cover, family=="Asparagaceae")

aspar.plot <- ggplot(aspar, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Asparagaceae")

aspar.plot
ggsave("./results/aspar_cover_by_elev.pdf")

## Asteraceae
aster <- subset(family.cover, family=="Asteraceae")

aster.plot <- ggplot(aster, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of asters")

aster.plot
ggsave("./results/aster_cover_by_elev.pdf")

## Berberidaceae
berber <- subset(family.cover, family=="Berberidaceae")

berber.plot <- ggplot(berber, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Berberidaceae")

berber.plot
ggsave("./results/berber_cover_by_elev.pdf")

## Bignoniaceae
bignon <- subset(family.cover, family=="Bignoniaceae")

bignon.plot <- ggplot(bignon, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Bignoniaceae")

bignon.plot
ggsave("./results/bignon_cover_by_elev.pdf")

## Cactaceae
cacta <- subset(family.cover, family=="Cactaceae")

cacta.plot <- ggplot(cacta, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Cactaceae")

cacta.plot
ggsave("./results/cacta_cover_by_elev.pdf")

## Chenopodiaceae
cheno <- subset(family.cover, family=="Chenopodiaceae")

cheno.plot <- ggplot(cheno, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Chenopodiaceae")

cheno.plot
ggsave("./results/cheno_cover_by_elev.pdf")

## Cupressaceae
cuper <- subset(family.cover, family=="Cupressaceae")

cuper.plot <- ggplot(cuper, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Cupressaceae")

cuper.plot
ggsave("./results/cuper_cover_by_elev.pdf")

## Fabaceae
fabaceae <- subset(family.cover, family=="Fabaceae")

fab.plot <- ggplot(fabaceae, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover Fabaceae")

fab.plot
ggsave("./results/fab_cover_by_elev.pdf")

## Fagaceae
faga <- subset(family.cover, family=="Fagaceae")

faga.plot <- ggplot(faga, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Fagaceae")

faga.plot
ggsave("./results/faga_cover_by_elev.pdf")

## Fouqueriaceae
fouq <- subset(family.cover, family=="Fouqueriaceae")

fouq.plot <- ggplot(fouq, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Fouqueriaceae")

fouq.plot
ggsave("./results/fouq_cover_by_elev.pdf")

## Grossulariaceae
grossul <- subset(family.cover, family=="Grossulariaceae")

grossul.plot <- ggplot(grossul, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Grossulariaceae")

grossul.plot
ggsave("./results/grossul_cover_by_elev.pdf")

## Koeberlinieacea
koeber <- subset(family.cover, family=="Koeberlinieacea")

koeber.plot <- ggplot(koeber, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Koeberlinieaceae")

koeber.plot
ggsave("./results/koeber_cover_by_elev.pdf")

## Pinaceae
pine <- subset(family.cover, family=="Pinaceae")

pine.plot <- ggplot(pine, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Pinaceae")

pine.plot
ggsave("./results/pine_cover_by_elev.pdf")

## Rhamnaceae
rham <- subset(family.cover, family=="Rhamnaceae")

rham.plot <- ggplot(rham, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Rhamnaceae")

rham.plot
ggsave("./results/rham_cover_by_elev.pdf")

## Roseaceae
rose <- subset(family.cover, family=="Roseaceae")

rose.plot <- ggplot(rose, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Roseaceae")

rose.plot
ggsave("./results/rose_cover_by_elev.pdf")

## Salinceae
sali <- subset(family.cover, family=="Salinceae")

sali.plot <- ggplot(sali, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Salinaceae")

sali.plot
ggsave("./results/sali_cover_by_elev.pdf")

## Sapindaceae
sapi <- subset(family.cover, family=="Sapindaceae")

sapi.plot <- ggplot(sapi, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Sapindaceae")

sapi.plot
ggsave("./results/sapi_cover_by_elev.pdf")

## Zygophyllaceae
zygo <- subset(family.cover, family=="Zygophyllaceae")

zygo.plot <- ggplot(zygo, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of Zygophyllaceae")

zygo.plot
ggsave("./results/zygo_cover_by_elev.pdf")

##############################################################################
## want to plot all family in line graph by elevation, FA in red, NFA in blue
#############################################################################

all.fam.plot <- ggplot(family.cover, aes(elev, pcover, color=family)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of all families")
all.fam.plot
ggsave("./results/all.fam_cover_by_elev.pdf")

family.cover <- left_join(family.cover, species)

all.fam.plot <- ggplot(family.cover, aes(elev, pcover, color=fire.adapted)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of all families")
all.fam.plot
ggsave("./results/all.fam.ft_cover_by_elev.pdf")

###want to plot FRI by elevation on top of that ##this isnt woring

SM_FRI <- read_csv("./data/sacramentos/SM_FRI.csv")
## DWS: Avoid spaces in filenames Organize code so that all data is read in and
## cleaned BEFORE any analysis. This is unwieldy.

FRI <- c(27, 6, 10)
  
plot(FRI, type = "o", color="black", 
  xlab = "elev", ylab = "FRI",
  main = "FRI by elevation")

### check if cleaning still works
unique(transects$start > transects$stop)
# none are false :)

##############################################################################
## Determine overlapping family occurrences
## + are the functional fire traits different when fam.overlap = TRUE?
##############################################################################

## Merge transect data with trait data
transects <- left_join(transects, species)

# create boolean "overlap" that is true if previous entry overlaps with current
# and is different species
neighbors.fam <- transects %>% arrange(site, transect, start) %>%
  group_by(transect) %>%
  mutate(overlap_dist = start - lag(stop),
         overlap = start < lag(stop) & (spcode != lag(spcode)),
         prev_family = lag(family))

nrow(filter(neighbors.fam, overlap))
# 213 overlaps

# of overlaps of :
nrow(filter(neighbors.fam, overlap & family != prev_family))
# 182 different overlap

nrow(filter(neighbors.fam, overlap & family == prev_family))
# 29 same overlap (2 N/A?)

# Testing this requires some null model expectation probably based on
# probability of any neighbor being same family

# when family = prev_family, does fire = prev_fire? ## this isn't working

neighbors.famfire <- mutate(neighbors.fam, 
          overlap_dist = start - lag(stop),
          overlap = start < lag(stop) & 
          (spcode != lag(spcode)),prev_fire = lag(fire.adapted))

nrow(filter(neighbors.famfire, overlap))

nrow(filter(neighbors.famfire, overlap & (family != prev_family) &
            (fire.adapted != prev_fire)))

## want to do this by elevation too.

## email from Anna 2024-12-04:

## I was able to plot proportion of cover of fire-adapted and non-fire adapted
## species by elevation (check out my figures! do they look all right?), but I
## also want to

# 1. Determine the distances between fire-adapted species (and separately,
# non-fire adapted species) by elevation

neighbors_by_elev <- neighbors.famfire %>% filter(!is.na(overlap)) %>%
  mutate(pair_type = case_when(
         fire.adapted & prev_fire ~ "both_FA",
         !fire.adapted & !prev_fire ~ "neither_FA",
         xor(fire.adapted, prev_fire) ~ "one_FA",
         .default = "other") # should not happen
         ) %>%
  group_by(elev, transect, pair_type)

overlaps_by_transect <- neighbors_by_elev %>%
  summarize(overlap_dist = mean(overlap_dist))

ggplot(overlaps_by_transect, aes(elev, overlap_dist, color=pair_type)) +
  geom_boxplot()


# 2. Determine if co-occurring same-family species are fire adapted or not,
# also by elevation.


## DWS; I don't understand this exactly.
