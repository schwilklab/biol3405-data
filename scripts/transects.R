# DWS 2024 
library(ggplot2)
library(dplyr)
library(stringr)
library(readr) # To show alternative to read.csv()

## Read transects
transects <- read.csv("./data/sacramentos/transects.csv", stringsAsFactors= FALSE)


## Turn transect column into globally unique ids. This is necessary for later
## expand.grid and gives us a single column that uniquely identifies a
## transect. If we did not do this, we would need two columns, "site" and
## "transect" to identify a given transect. Also calculate the distance each
## plant covers for every entry
transects <- mutate(transects,  group=str_sub(transect,1,1),
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

# Merge species info with transect data. Use merge() function and remember that
# we want all rows from the transects data frame in the result. We don't need
# unmatched rows in species as the file contains the full species list over
# several years and some species were not found in 2011.
cover <- merge(cover, species, all.x=TRUE)

# Read in elevations of each site and merge with cover data
## sites <- read.csv(
##            stringsAsFactors=FALSE, na.strings = "")
## # Although elevation is numeric, it is really a categorical variable with six
## # values. Let's ensure that ggplot will treat it as a categorical variable.
## sites$elev <- as.factor(as.character(sites$elev))

## alternatively, we could use readr::read_csv with a column type
## specification:
# sites <- read_csv(
#    "http://r-research-tool.schwilk.org/data/BBNP_sites.csv",
#    col_types = list(col_character(), col_factor())
# )

## DWS: note, that this method avoids something that is imperfect above. You
## might want to come back to this later after looking at your figures.



cover <- left_join(cover, sites)

# 'cover' is the tidy dataframe (tibble) for all further use. We can delete
# the intermediate objects to keep our environment cleaner:
rm(all.transects.species, sites, species, transects)

###############################################################################
## Some quick and dirty example analyses and figures
###############################################################################

## Summarize proportional cover by transect and by growth form
by_elev_gf <- group_by(cover, elev, transect, gf)
gf.cover <- summarize(by_elev_gf, pcover = sum(cover))

## Just pull out trees
trees <- subset(gf.cover, gf=="tree") # gf is growth form

## and plot proportional tree cover by site
ggplot(trees, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")
