## dickens.R
##
## Dylan Schwilk
##
## Data from BIOL-3405 field trip to Dickens springs on 9/14/2024. Work
## included predawn and midday water potentials

library(readr)
library(dplyr)
library(ggplot2)

# read data from csv files

dickens <- read_csv("data/dickens/wp.csv")
dickens <- read.csv("data/dickens/wp.csv", stringsAsFactors=FALSE)


# or
dickens <- read_csv("https://raw.githubusercontent.com/schwilklab/biol3405-data/refs/heads/main/data/dickens/wp.csv")

dickens$leaf_area <- NULL
dickens$dry_mass<- NULL

species <- read_csv("data/dickens/species.csv")
#species <- read.csv("data/dickens/species.csv", stringsAsFactors=FALSE)

leaves <- read_csv("data/dickens/leaf_area.csv")

leaves <- mutate(leaves, LMA=dry_mass/leaf_area, leaf_size=leaf_area/n_leaves)

wp <- left_join(dickens, species)
#wp <- merge(dickens, species)
wp <- left_join(wp, leaves)

wp

nrow(wp)
names(wp)


wp_genus <- group_by(wp, genus)
sp_sum <- summarize(wp_genus,
                    pd=mean(predawn_wp),
                    md=mean(midday_wp),
                    LMA=mean(LMA, na.rm=TRUE))

sp_sum


# Water potential figures


plot1 <- ggplot(wp, aes(x=predawn_wp, y=midday_wp, color=genus)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE)

plot1
ggsave("plot1.pdf", plot1)

ggplot(wp, aes(genus, predawn_wp-midday_wp)) +
  geom_boxplot()



ggplot(wp, aes(predawn_wp, predawn_wp-midday_wp, color=genus)) +
  geom_point(size=3)



## Species comparisons

#wp_leaf_traits <- summarize(
