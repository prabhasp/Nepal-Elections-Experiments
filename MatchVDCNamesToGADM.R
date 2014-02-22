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
    col1l <- tolower(col1)
    col2l <- tolower(col2)
    bestmatch <- col2[amatch(col1l, col2l, maxDist=MD)] # do string distance first
    bestmatch <- ifelse(is.na(bestmatch),
                        col2[pmatch(col1l, col2l)], # pmatch does "first of the string" matching, to recover
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
suggested_corrections <- subset(suggested_corrections, 
                                !str_detect(old, 'Royal|Natio|Wildlife|Development'))
gadm2 <- ddply(gadm, .(NAME_3), function(df) {
    d = df[1,'NAME_3']
    corr <- na.omit(subset(suggested_corrections, NAME_3 == d))
    cbind(df, VDC_name = revalue(df$NAME_4, setNames(corr$bestmatch, corr$old)))
})

## ROUND 2 -- manual matches
x <- non_matching(vdcs, gadm2, 'District', 'NAME_3', 'VDC_name', 'VDC_name')
vdcmissing <- x[[1]]
gadmmissing <- subset(x[[2]], 
                    !str_detect(VDC_name, 'Royal|Natio|Wildlife|Development'))
suggested_correction_manual <-
    # CLOSE MATCHES
    c("Piparpati Jabadi" = "Paparpatijabdi",
      "Malikathota" = "Malika",
      "Andheri (Narayanstha" = "Narayansthan",
      "JaisithokMandan" = "Jaisithok",
      "MusikotKhalanga" = "Musikot",
      "Sinhadevisombare" = "Singhadevi",
      "SisneriMahadevsthan" = "Sisneri",
      # determined from map inspection
      "KalikaN.P." = "Baglung N.P.",
      "n.a. (1)" = "Raghunathpur",
      "n.a. (2)" = "Rampurwa"
    )
gadm2$VDC_name <- revalue(gadm2$VDC_name, suggested_correction_manual)
vdcs$VDC_name <- revalue(vdcs$VDC_name, suggested_correction_manual)

### Final missing
x <- non_matching(vdcs, gadm2, 'District', 'NAME_3', 'VDC_name', 'VDC_name')
vdcmissing <- x[[1]]
gadmmissing <- subset(x[[2]], 
                      !str_detect(VDC_name, 'Royal|Natio|Wildlife|Development'))

corrections <- ddply(gadm2, .(NAME_3), function(df) {
    subset(df, df$VDC_name != df$NAME_4, select=c("NAME_3", "NAME_4", "VDC_name"))
})

### OUTPUTS
write.csv(arrange(gadmmissing, NAME_3, VDC_name), "data/MissingFromGADM.csv", row.names=F)
write.csv(arrange(vdcmissing, District,VDC_name), "data/MissingFromVDCCodes.csv", row.names=F)
write.csv(arrange(corrections, NAME_3), "data/Corrections.csv", row.names=F)
write.csv(gadm2, "data/NPL_adm4.csv", row.names=F)

## FINAL task: polygon output
## subtask 1: pull in VDC Codes
gadm2$DistrictVDC <- str_c(gadm$NAME_3, gadm$VDC_Name)
vdcs$DistrictVDC <- str_c(vdcs$District, vdcs$VDC_name)
f <- merge(gadm2, vdcs, by="DistrictVDC")

## subtask 2: output
gadm.shp <- readShapeSpatial('../NepalMaps/baselayers/NPL_adm/NPL_adm4.shp')