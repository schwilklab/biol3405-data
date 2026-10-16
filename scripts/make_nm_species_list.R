# Some starter code to produce tables of species by site. This is useful for
# trip preparation and making handouts for students.

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(xtable)

## Read transects
transects_2024 <- read_csv("./data/sacramentos/transects_2024.csv")
transects_2026 <- read_csv("./data/sacramentos/transects_2026.csv")
transects_2024$year <- 2024
transects_2026$year <- 2026
transects_2024$transect <- as.character(transects_2024$transect)
transects_2026$transect <- as.character(transects_2026$transect)

transects <- bind_rows(transects_2024, transects_2026)
species <- read.csv("./data/sacramentos/species.csv", na.strings = "")

for(s in unique(transects$site)) {
  spcodes <- unique(filter(transects, s==site)$spcode)
  species_at_site <- filter(species, spcode %in% spcodes) %>%
    select(spcode, genus, specific_epithet, family, growth_form)
  print(s)
  print(species_at_site)
}


