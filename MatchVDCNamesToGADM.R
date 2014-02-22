## Match VDC codes from OpenDataNepal csv and GADM shapefile+csv pair

## http://opendatanepal.org/content/district-and-vdc-codes-nepal
vdcs <- read.csv('~/Downloads/VDC codes of Nepal_0.csv', stringsAsFactors=F)
## https://github.com/prabhasp/NepalMaps
gadm <- read.csv("../NepalMaps/baselayers/NPL_adm/NPL_adm4.csv", stringsAsFactors=F)

# Given data frame 1, data frame 2, and district and vdc columns in each df,
# outputs a subset where matches aren't found from 1 to 2, and 2 to 1
non_matching <- function(df1, df2, distCol1, distCol2, vdcCol1, vdcCol2) {
    # make sure that distCol1 and distCol2 don't have any non-matching elements
    stopifnot(!any(is.na(match(df1[,distCol1], df2[,distCol2]))))
    s1 <- paste(df1[,distCol1], df1[,vdcCol1])
    s2 <- paste(df2[,distCol2], df2[,vdcCol2])
    list(missing_in_1 = df1[!s1 %in% s2, c(distCol1, vdcCol1)],
         missing_in_2 = df2[!s2 %in% s1, c(distCol2, vdcCol2)])
}

## FIRST -- match up the districts
require(plyr)
unique(vdcs[!vdcs$District %in% gadm$NAME_3,'District'])
unique(gadm[!gadm$NAME_3 %in% vdcs$District,'NAME_3'])
gadm$NAME_3 <- revalue(gadm$NAME_3, c("Chitawan" = "Chitwan", "Sindhupalchok" = "Sindhupalchowk"))
stopifnot(length(gadm[!gadm$NAME_3 %in% vdcs$District,'NAME_3']) == 0)
stopifnot(length(vdcs[!vdcs$District %in% gadm$NAME_3,'District']) == 0)

## NEXT -- vdcs
# how many are different?
x <- non_matching(vdcs, gadm, 'District', 'NAME_3', 'VDC_name', 'NAME_4')
# so many -- lets look at a few after sorting the datasets a bit
head(x[[1]] <- arrange(x[[1]], District, VDC_name), 12)$VDC_name
head(x[[2]] <- arrange(x[[2]], NAME_3, NAME_4), 12)$NAME_4

# One difference in the dataset is that the VDC dataset says "municipality", "metropolitan city"
# or "sub-metropolitan city", while the gadm dataset says N.P. (for NagarPalika). Lets normalize this.
require(stringr)
vdcs$VDC_name <- str_replace_all(vdcs$VDC_name, 
                                 "Municipality|[Mm]etropolitan City|[Ss]ub.[Mm]etropolitan City|Sub Metropolitan",
                                 "N.P.")

# now we break out stringdist
require(stringdist)
gadmmissing <- x[[2]]
vdcmissing <- x[[1]]

bestmatches <- function(col1, col2, MD=3) {
    bestmatch <- col2[amatch(col1, col2, maxDist=MD)] # do string distance first
    bestmatch <- ifelse(is.na(bestmatch),
                        col2[pmatch(col1, col2)], # pmatch does "first of the string" matching, to recover
                        # shapefile truncation
                        bestmatch)
    data.frame(
        old=col1,
        bestmatch=bestmatch,
        stringsAsFactors=F
    )
}
suggested_corrections <- 
    ddply(gadmmissing, .(NAME_3), function(df) {
        vdcsfordistrict <- subset(vdcs, District == df[1,'NAME_3'])
        bestmatches(df$NAME_4, vdcsfordistrict$VDC_name)
    })
suggested_corrections <- subset(suggested_corrections, !str_detect(old, 'Royal|Natio|Wildlife|Development'))
subset(suggested_corrections, is.na(bestmatch))

gadm2 <- ddply(gadm, .(NAME_3), function(df) {
    d = df[1,'NAME_3']
    corr <- na.omit(subset(suggested_corrections, NAME_3 == d))
    cbind(df, VDC_Name = revalue(df$NAME_4, setNames(corr$bestmatch, corr$old)))
})

## ROUND 2
x <- non_matching(vdcs, gadm2, 'District', 'NAME_3', 'VDC_name', 'VDC_Name')
vdcmissing <- x[[1]]
gadmmissing <- subset(x[[2]], )

suggested_corrections2 <- na.omit(ldply(gadmmissing$NAME_4_CORRECTED, function(name) {
    data.frame(
        old=name,
        bestmatch=vdcs$VDC_name[pmatch(name, vdcs$VDC_name)],
        stringsAsFactors=F
    )}))
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
## subtask 1: pull in VDC Codes
gadm <- rename(gadm, c('NAME_4_CORRECTED' = 'VDC_Name'))
gadm$DistrictVDC <- str_c(gadm$NAME_3, gadm$VDC_Name)
vdcs$DistrictVDC <- str_c(vdcs$District, vdcs$VDC_name)
f <- merge(gadm, vdcs, by="DistrictVDC")

## subtask 2: output
gadm.shp <- readShapeSpatial('../NepalMaps/baselayers/NPL_adm/NPL_adm4.shp')
gadm.shp@data <- merge(gadm.shp@data, subset(gadm, select=c("ID_4", "NAME_4_CORRECTED")), by="ID_4")