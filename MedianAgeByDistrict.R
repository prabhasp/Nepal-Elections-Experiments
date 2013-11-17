setwd("~/Code/Nepal-Elections-Experiments/")
election <- readRDS("data/Election2070CandidateDataSet_Copy.rds")

require(plyr); require(ggplot2)
district_stats <- ddply(idata.frame(election), .(District), function(df) {
  data.frame(
                        AverageAge=mean(df$उम.र),
                        MedianAge=median(df$उम.र),
                        MaxAge=max(df$उम.र),
                        MinAge=min(df$उम.र),
                        CandidatesInDistrict=nrow(df)
  )})

# ../NepalMaps refers to this repository: https://github.com/prabhasp/NepalMaps
source("../NepalMaps/NepalMapUtils.R")

# upper case the districts
district_stats$District <- toupper(district_stats$District)
np_distf$id <- toupper(np_distf$id)

# see whats a mismatch
district_stats[!district_stats$District %in% np_distf$id,'District']
unique(np_distf[!np_distf$id %in% district_stats$District,]$id)

# they match in order, so i'll just re-assign one to the other
district_stats[!district_stats$District %in% np_distf$id,'District'] <- 
  unique(np_distf[!np_distf$id %in% district_stats$District,]$id)

# finally, make a plot
npchoropleth(district_stats, 'District', 'MedianAge')

