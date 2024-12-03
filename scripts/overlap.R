## overlap.R
## D. Schwilk
## 2024-11-22

## Run transects.R first:
source("./scripts/transects.R")



###############################################################################
## Some ideas for investigating placement along a transect (overlap, neighbors)
###############################################################################
###############################################################################
## Merge transect data with trait data
transects <- left_join(transects, species)



# create boolean "overlap" that is true if previous entry overlaps with current
# and is different species
neighbors <- transects %>% arrange(site, transect, start) %>%
  group_by(transect) %>%
  mutate(overlap_dist = start - lag(stop),
         overlap = start < lag(stop) & (spcode != lag(spcode)),
         prev_gf = lag(growth_form))


nrow(filter(neighbors, overlap))
# 213 overlaps

# of overlaps of same growth form:
nrow(filter(neighbors, overlap & growth_form != prev_gf))
# 117

# So 96 are same gf and 117 are different. Testing this requires some null
# model expectation probably based on probability of any neighbor being same
# growth form.

## One could do this by elevation...


mean(filter(neighbors, growth_form != prev_gf)$overlap_dist, na.rm=TRUE)
mean(filter(neighbors, growth_form == prev_gf)$overlap_dist, na.rm=TRUE)




###############################################################################
## Null model for overlap question:

## Function to randomize position on single transect
randomize_transect <- function(x) {
  x$start <- sample(x$start) # shuffles species start points
  x$stop <- x$start + x$dist # make new stop
  x <- arrange(x, start) # order the result by start
  return(x)
}

## Calculate our test statistic for proportion of overlaps that involve
## different growth forms
calculate_overlap_stat <- function(x) {
  neighbors <- x %>% arrange(transect, start) %>%
  group_by(transect) %>%
  mutate(overlap = start < lag(stop) & (spcode != lag(spcode)),
         prev_gf = lag(growth_form))
  t_stat <- nrow(filter(neighbors, overlap & growth_form != prev_gf)) /
    nrow(filter(neighbors, overlap))
  print(nrow(x))
  return(t_stat)
}

randomize_and_calculate <- function(x) {
  r_transects <- group_by(transects, transect) %>%
    do(randomize_transect(.))
  return(calculate_overlap_stat(r_transects))
}



null_dist <- replicate(10000, randomize_and_calculate(transects))
hist(null_dist)
median(null_dist)
# empirical cumulative prob function:
null_ecdf <- ecdf(null_dist)


null_ecdf(calculate_overlap_stat(transects))
# 0.3569
                                                                                           
# So, not in tails at all. Our stat is below the mean, but nearly 36% of null
# values are lower. No significance.
  

## overlap by elevation

by_elev <- transects %>% group_by(elev)

dplyr::summarize(by_elev, o_stat = calculate_overlap_stat(pick(transect,start,stop,growth_form,spcode)))

