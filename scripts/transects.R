# 2024-11-01 Code for summarizing and analyzing woody plant community data
# collected by BIOL 3404 class in the Sacramento Mountains of New Mexico on
# September 28 and 29, 2024.

library(dplyr)
library(ggplot2)
library(stringr)

## Read transects
transects <- read.csv("./data/sacramentos/transects.csv", stringsAsFactors= FALSE)

## Turn transect column into globally unique ids. This is necessary for later
## expand.grid and gives us a single column that uniquely identifies a
## transect. If we did not do this, we would need two columns, "site" and
## "transect" to identify a given transect. Also calculate the distance each
## plant covers for every entry

## DWS: Some folks messed up start and stop I assume by switching so we will
## take absolute value for now:

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


## This data is not complete because there are missing implied zeros!
## Correctly, every species in the whole data set should have a value for every
## transect. If a species did not occur there, it should be zero cover but
## inthe current data, a missing species is not included. We must add in zeroes
## for missing species. This is a common question that comes up in data
## cleaning: are missing values truly missing or do they imply a zero? In this
## case, they logically imply zero.

# Use expand.grid to get all possible combinations of transect and species ids
all.transects.species <- expand.grid(transect = unique(cover$transect),
                                     spcode=unique(cover$spcode))

cover <- full_join(cover, all.transects.species)
## same as:
# merge(cover, all.transects.species, all=TRUE)

## ("joins" are the dplyr way, merge is base R)

## The above join/merge inserted NA for missing combinations of species and
## transects. NA should really be zero (missing species means zero cover)
cover$cover[is.na(cover$cover)] <- 0
## again, one could use mutate and case_when() for the above line.

## This data is not tidy: "site" is not its own column but is embedded in the
## transect ids. We must grab `site` as its own column because we will need it
## for analyses:
cover <- mutate(cover, site = str_split_fixed(transect, "\\.", n=2)[,1])

## Read the species list. This gives us more info on each species. We can merge
## this with the transect data by 'spcode'. Here we give read.csv an explicit
## NA string for character vectors --- otherwise empty fields would be
## considered the "" string.
species <- read.csv("./data/sacramentos/species.csv", stringsAsFactors=FALSE,
                    na.strings = "")
# This includes "defense" as a trait with values U, L or A. Let's get a binary
# version of the trait as well:

species <- mutate(species, armed = defense %in% c("L", "A"))


## DWS: let's make sure all of our species codes are in the species.csv data:

## DWS: as of 2024-11-01 I fixed all missing. Still some unknowns
tspecies <- unique(transects$spcode)
length(tspecies)
# was 51, now 41
sspecies <- species$spcode
length(sspecies)

unmatched <- tspecies[! tspecies %in% sspecies]
unmatched

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


## alternatively, we could use readr::read_csv with a column type
## specification.
# sites <- read_csv(
#    "./data/sacramentos/sites.csv",
#    col_types = list(col_character(), col_factor())
# )

## Since we used the first method, we need to get the order of the factor
## right. We lucked out here given the order encountered is correct. But if the
## data changes this could be messed up. For now it is ok. Let's proive that
## the factor levels are ordered by elevation:

## > sites$elev
## [1] 1454 1802 2042 2500 2744
## Levels: 1454 1802 2042 2500 2744

## Good. Or were would need to use as.factor()

cover <- left_join(cover, sites)

# 'cover' is the tidy dataframe (tibble) for all further use. We can delete the
# intermediate objects to keep our environment cleaner (but I wont for now).
# rm(all.transects.species, sites, species, transects)

###############################################################################
## For soem plots, we will want total woody cover (for example, to graph
## proportional cover of armed plants relative to total cover)
total_cover <- group_by(cover, elev, transect) %>%
  summarize(total_cover = sum(cover))

###############################################################################
## Some quick and dirty example analyses and figures
###############################################################################

## Summarize proportional cover by transect and by growth form
by_elev_gf <- group_by(cover, elev, transect, growth_form)
gf.cover <- summarize(by_elev_gf, pcover = sum(cover))

## Just pull out trees
trees <- subset(gf.cover, growth_form=="tree")
## Note that testing equality is "==". "=" is used only in setting argument
## values in function calls

# and plot proportional tree cover by site
ggplot(trees, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")

ggsave("results/tree_cover_by_elev.pdf")

## DWS: We could also pull out both "tree" and "shrub/tree" together, but we
## need to do this BEFORE summarizing or we will end up with two values for
## each transect (one for the tree gf and one for "shrub_tree". We want a single
## combined value.
trees <- subset(cover, grepl("tree", growth_form, fixed = TRUE))
trees.cover <- group_by(trees, transect, elev) %>% summarize(pcover = sum(cover))
ggplot(trees.cover, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")


# succulents?

suc <- subset(gf.cover, grepl("succulent", growth_form, fixed = TRUE))
ggplot(suc, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Succulent cover")

# what about proportionally to all woody plant cover?
suc <- left_join(suc, total_cover)
ggplot(suc, aes(elev, pcover / total_cover)) + geom_boxplot() +
  ylab("Proportional cover of succulents relative to all woody plants")


## Let's try summarizing by a plant family
by_elev_family <- group_by(cover, elev, transect, family)
family.cover <- summarize(by_elev_family, pcover = sum(cover))

## Look at Fabaceae (the bean/pea family):
fabaceae <- subset(family.cover, family=="Fabaceae")

fab.plot <- ggplot(fabaceae, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")

fab.plot


## What about conifers?
conifer_families <- c("Pinaceae", "Cupressaceae")
## We need to start at with species cover data not family here. Think about why!

conifers <-  subset(cover, family %in% conifer_families)
conifer.cover <- group_by(conifers, elev, transect) %>% summarize(pcover=sum(cover))

con.plot <- ggplot(conifer.cover, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")

con.plot

## Defenses
armed_cover <- filter(cover, armed) %>% group_by(elev, transect) %>%
  summarize(armed_cover=sum(cover))
armed_cover <- left_join(armed_cover, total_cover)

ggplot(armed_cover, aes(elev, armed_cover / total_cover)) +
  geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover of armed species")
ggsave("./results/armed_cover_by_elev.pdf")


## by_defense <- cover %>% group_by(elev, transect, armed)
## armed_cover <- summarize(by_defense, pcover = sum(cover))
## ggplot(armed_cover, aes(elev, pcover, color=armed)) +
##   geom_boxplot()


###############################################################################
## Some ideas for investigating placement along a transect (overlap, neighbors)
###############################################################################

# create boolean "overlap" that is true if previous entry overlaps with current
# and is different species
neighbors <- transects %>% arrange(site, transect, start) %>%
  left_join(species) %>%
  group_by(transect) %>%
  mutate(overlap = start < lag(stop) & (spcode != lag(spcode)),
         prev_gf = lag(growth_form))


nrow(filter(neighbors, overlap))
# 213 overlaps

# of overlaps of same growth form:
nrow(filter(neighbors, overlap & growth_form != prev_gf))
# 117

# So 96 are same gf and 117 are different. Testing this requires some null
# model expectation probably based on probability of any neighbor being same
# growth form.

