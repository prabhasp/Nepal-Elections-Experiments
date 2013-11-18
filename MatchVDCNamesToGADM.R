## Match VDC codes from OpenDataNepal csv and GADM shapefile+csv pair

## http://opendatanepal.org/content/district-and-vdc-codes-nepal
vdcs <- read.csv('~/Downloads/VDC codes of Nepal.csv', stringsAsFactors=F)
## https://github.com/prabhasp/NepalMaps
gadm <- read.csv("../NepalMaps/baselayers/NPL_adm/NPL_adm4.csv", stringsAsFactors=F)

## FIRST -- match up the districts
require(doBy)
unique(vdcs[!vdcs$District %in% gadm$NAME_3,'District'])
unique(gadm[!gadm$NAME_3 %in% vdcs$District,'NAME_3'])
gadm$NAME_3 <- recodeVar(gadm$NAME_3, 
                              c("Chitawan", "Sindhupalchok"), c("Chitwan", "Sindhupalchowk"))
vdcs$District <- recodeVar(vdcs$District, c("Kavre"), c("Kavrepalanchok"))
stopifnot(length(gadm[!gadm$NAME_3 %in% vdcs$District,'NAME_3']) == 0)
stopifnot(length(vdcs[!vdcs$District %in% gadm$NAME_3,'District']) == 0)

## NEXT -- vdcs
# how many are different?
length(vdcs[!vdcs$VDC_name %in% gadm$NAME_4,'VDC_name'])
length(gadm[!gadm$NAME_4 %in% vdcs$VDC_name,'NAME_4'])

# too many -- lets look at a few after sorting the datasets a bit
require(plyr)
vdcs <- arrange(vdcs, District, VDC_name)
gadm <- arrange(gadm, NAME_3, NAME_4)

vdcs[!vdcs$VDC_name %in% gadm$NAME_4,'VDC_name'][1:20]
gadm[!gadm$NAME_4 %in% vdcs$VDC_name,'NAME_4'][1:20]

# One difference in the dataset is that the VDC dataset says "municipality", "metropolitan city"
# or "sub-metropolitan city", while the gadm dataset says N.P. (for NagarPalika). Lets normalize this.
vdcs$VDC_name <- str_replace_all(vdcs$VDC_name, 
      "Municipality|[Mm]etropolitan City|[Ss]ub.[Mm]etropolitan City|Sub Metropolitan",
      "N.P.")

# now we break out stringdist
require(stringdist)
gadmmissing <- gadm[!gadm$NAME_4 %in% vdcs$VDC_name,c('NAME_3', 'NAME_4')]
vdcmissing <- vdcs[!vdcs$VDC_name %in% gadm$NAME_4,c('District','VDC_name')]

bestmatches <- function(col1, col2, MD=3) {
  data.frame(
    old=col1,
    bestmatch=col2[amatch(col1, col2, maxDist=MD)],
    stringsAsFactors=F
  )
}
suggested_corrections <- 
  ddply(gadmmissing, .(NAME_3), function(df) {
    vdcsfordistrict <- subset(vdcs, District == df[1,'NAME_3'])
    bestmatches(df$NAME_4, vdcsfordistrict$VDC_name)
  })
# manual inspection proves that these are okay; might be worth doing another look-through
gadm$NAME_4_CORRECTED <- recodeVar(gadm$NAME_4,
                         na.omit(suggested_corrections)$old,
                         na.omit(suggested_corrections)$bestmatch)
## ROUND 2
vdcmissing <- vdcs[!vdcs$VDC_name %in% gadm$NAME_4,c('District','VDC_name')]
gadmmissing <- gadm[!gadm$NAME_4_CORRECTED %in% vdcs$VDC_name & 
                      !str_detect(gadm$NAME_4_CORRECTED, 'Royal|Natio|Wildlife|Development'),
                c('NAME_3','NAME_4_CORRECTED')]
suggested_corrections2 <- ldply(gadmmissing$NAME_4_CORRECTED, function(name) {
  data.frame(
    old=name,
    bestmatch=vdcs$VDC_name[pmatch(name, vdcs$VDC_name)],
    stringsAsFactors=F
  )})
# some manual additions
suggested_corrections2 <- rbind(suggested_corrections2, data.frame(
  old=c("Piparpati Jabadi", "NaikapPuranoBhanjya","PokharibhindaSamgra", # just missed above
        "n.a. (1)", "n.a. (2)", "KalikaN.P."), # from manual map inspections
  bestmatch=c("Paparpatijabdi", "Naikappuranobhanjyang", "Pokharibhindasamgrampur",
              "Raghunathpur", "Rampurwa", "Baglung N.P.")))

gadm$NAME_4_CORRECTED <- recodeVar(gadm$NAME_4_CORRECTED,
                         na.omit(suggested_corrections2)$old,
                         na.omit(suggested_corrections2)$bestmatch)
## FINAL MISSING
vdcmissing <- vdcs[!vdcs$VDC_name %in% gadm$NAME_4_CORRECTED,c('District','VDC_name')]
gadmmissing <- gadm[!gadm$NAME_4_CORRECTED %in% vdcs$VDC_name & 
                      !str_detect(gadm$NAME_4_CORRECTED, 'Royal|Natio|Wildlife|Development'),
                    c('NAME_3','NAME_4_CORRECTED')]
corrections <- rbind(na.omit(suggested_corrections, suggested_corrections2))

### OUTPUTS
write.csv(gadmmissing, "data/MissingFromGADM.csv")
write.csv(vdcmissing, "data/MissingFromVDCCodes.csv")
write.csv(corrections, "data/Corrections.csv")
write.csv(gadm, "data/NPL_adm4.csv")

## FINAL task: polygon output
gadm.shp <- readShapeSpatial('../NepalMaps/baselayers/NPL_adm/NPL_adm4.shp')
gadm.shp@data <- merge(gadm.shp@data, subset(gadm, select=c("ID_4", "NAME_4_CORRECTED")), by="ID_4")