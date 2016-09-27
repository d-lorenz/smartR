
###   smart_import_dbeffort

# library(smartR)

###   Initialize smart project
my_sampling <- SmartProject$new()

### File input
# GRID
# pathGridShp <- "/Users/Lomo/Documents/Uni/R/smart/data/Grid/grid_sos_3NM/seabed_SoS_grid3NM.shp"
pathGridShp <- "/Users/Lomo/Documents/Uni/PhD/TESI/Grid/GFCM_Grid_6min_GSA16.shp"

# SEABED
# pathSeabed <- "/Users/Lomo/Documents/Uni/R/smart/data/SeaBed/SoSBiocMat.rData"
pathSeabed <- "/Users/Lomo/Documents/Uni/PhD/TESI/BioM.rData"

# BATHYMETRY
# pathBathymetry <- "/Users/Lomo/Documents/Uni/R/smart/data/seabedSos_Bathy.rData"
pathBathymetry <- "/Users/Lomo/Documents/Uni/R/smart/data/Grid/bathy_test.rData"

# RAW VMS
# pathRawVMS <- "/Users/Lomo/Documents/Uni/R/smart/data/RawEffort/rawEffort_seabedGrid_afterAll.rData"
pathRawVMS <- "/Users/Lomo/Documents/Uni/R/smart/data/RawEffort/smart_rawEffort_new.rData"

# GRID EFFORT AA
pathEffortAA <- "/Users/Lomo/Documents/Uni/R/smart/data/RawEffort/rawEffort_seabedGrid_afterAll.rData"
# pathEffortAA <- "/Users/Lomo/Documents/Uni/PhD/TESI/SoS_vms/smart_rawEffort_new.rData"

# FISHERY DATA
pathFishery <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_merge_CampBiol.csv"


pathClusMat <- "/Users/Lomo/Documents/Uni/R/smart/data/out/FG_cut/fg_sos/clusMat_BedEffDep.rData"
rawDataPath <- "/Users/Lomo/Documents/Uni/R/mixture/fisherySampling.rData"


##################################
#######   Map and Grid   #########
##################################
# tmp_Grid <- gridFromShp(shapeFilePath = "/Users/Lomo/Documents/Uni/R/smart/data/SeaBed/SoSBioc.shp", cellSize = 5, clip = TRUE)
# writeSpatialShape(tmp_Grid, "/Users/Lomo/Documents/Uni/Lab/Maps/GFCM\ -\ GSAs/r_grids/seabed_SoS_grid5NM")

###   Load grid shapefile
my_sampling$loadMap(pathGridShp)

###   Get google hybrid map
my_sampling$sampMap$getGooMap()       ### FIX add a check if sampMap is loaded

###   Set google map + grid plot
my_sampling$sampMap$setGooGrid()

###   Plot google map + grid
my_sampling$sampMap$plotGooGrid()

###   Set google map + grid + bbox
my_sampling$sampMap$setGooBbox()

###   Plot google map + grid + bbox
my_sampling$sampMap$plotGooBbox()


##################################
##########   Seabed   ############
##################################

my_sampling$sampMap$loadBioDF(pathSeabed)

if(!is.null(my_sampling$sampMap$gooMap)){
  my_sampling$sampMap$ggplotBioDF()
}else{
  my_sampling$sampMap$plotBioDF()
}

# ### load seabed shp
# tmp_bioc <- readShapePoly("/Users/Lomo/Documents/Uni/R/smart/data/SeaBed/SoSBioc.shp")
#
# ### aggregate by biocenosis field
# res_agg <- aggregate(x = tmp_bioc["Biocenosis"], by = my_sampling$sampMap$gridShp, areaWeighted = TRUE)
# ### split by substrate
# res_cast <- dcast(cbind(ID = 1:nrow(res_agg@data),res_agg@data), ID ~ Biocenosis, value.var = "Biocenosis")
# res_cast <- res_cast[,-1]
# # res_cast[!is.na(res_cast)] <- 1
# # res_cast[is.na(res_cast)] <- 0
# res_cast <- ifelse(is.na(res_cast), 0, 1)


##################################
########   Bathymetry   ##########
##################################

###   Load saved depth
my_sampling$sampMap$loadGridBath(pathBathymetry)

my_sampling$sampMap$ggplotGridBathy()               ###  Increase line width + set legend title



##################################
###########   Fleet   ############
##################################

###   Create fleet object
my_sampling$createFleet()



##################################
##########   Effort   ############
##################################

###   Load raw effort from vmsbase
# tmp_files <- gfile(text = "Select Effort DBs", type = "open",
#                    initial.filename = NULL, initial.dir = getwd(), filter = list(),
#                    multi = TRUE)

# my_sampling$loadFleeEffoDbs(tmp_files, met_nam = "OTB", onBox = TRUE, perOnBox = 0.9)

###   Load raw effort from rData
my_sampling$fleet$rawEffort <- readRDS(pathRawVMS)

# ###   Plot raw points
# my_sampling$ggplotRawPoints("2009")       ####  FIX add a check if exists gooMapPlot

# for(i in names(my_sampling$fleet$rawEffort)){
#   my_sampling$ggplotRawPoints(i)
#   ggsave(filename = paste("/media/fish_team/SANDISK/smart/data/out/ggmap_raweffort_", i, ".jpeg", sep = ""),
#          units = "cm", width = 20, height = 15, scale = 1.5)
# }

###   Set Effort Ids
my_sampling$fleet$setEffortIds()

# ###   Plot count of effort ids
# my_sampling$fleet$plotCountIDsEffo()
# ggsave(filename = "/media/fish_team/SANDISK/smart/data/out/raweffort_vesselID_count.jpeg",
#        units = "cm", width = 20, height = 15, scale = 1.5)


##################################
######   Fishing Points   ########
##################################

###   Plot speed/depth profiles
# jpeg(filename = "/Users/Lomo/Dropbox/SMART2/Paper/Res/fishPoin_filter.jpeg",
#      units = "cm", res = 150, width = 10, height = 7)
my_sampling$fleet$plotSpeedDepth("2012", c(3,7), c(-20,-800))   ###  TO DO convert to ggplot
# dev.off()

###   Set fishing points filter            ###   FIX no output string
my_sampling$fleet$setFishPoinPara(speed_range = c(3,7), depth_range = c(-20,-800))

###   Set fishing points                  ###   FIX add check if parameters set
my_sampling$fleet$setFishPoin()           # head(my_sampling$fleet$rawEffort[["2009"]])

# ###   Plot fishing points
# my_sampling$ggplotFishingPoints("2009")

# ###   Plot fishing points stats
# my_sampling$fleet$plotFishPoinStat()



##################################
######   Gridded Effort   ########
##################################

###   Set cell effort
my_sampling$setCellPoin()                 # head(my_sampling$fleet$rawEffort[["2009"]])

# ###   Plot gridded effort
# my_project$ggplotGridEffort("2009")

###   Set weeknum x track
my_sampling$fleet$setWeekMonthNum()            # head(my_sampling$fleet$rawEffort[["2009"]])

# saveRDS(my_sampling$fleet$rawEffort, pathEffortAA)



### Load Effort After all
my_sampling$fleet$rawEffort <- readRDS(pathEffortAA)
my_sampling$fleet$setEffortIds()
my_sampling$ggplotRawPoints(names(my_sampling$fleet$rawEffort)[1])
my_sampling$ggplotFishingPoints(names(my_sampling$fleet$rawEffort)[1])
my_sampling$ggplotGridEffort(names(my_sampling$fleet$rawEffort)[1])
###



##################################
######   Fishing Grounds   #######
##################################

my_sampling$setAvailData()
my_sampling$sampMap$setClusInpu()
# my_sampling$sampMap$setClusInpu(howData = c("0.5X", "2X", "2X"))


skater_methods <- c("euclidean", "manhattan", "maximum", "binary", "canberra", "minkowski")

my_sampling$sampMap$calcFishGrou(numCuts = 50, minsize = 10, modeska = "S",
                                 skater_method = skater_methods[1])


num_FG <- 22
# my_sampling$sampMap$plotFishGrou(num_FG)

# # head(my_sampling$sampMap$clusMat)

my_sampling$setFishGround(numCut = num_FG)            # head(my_sampling$fleet$rawEffort[["2009"]])

# library(gridExtra)
suppressWarnings(grid.arrange(my_sampling$sampMap$ggIchFGlin,
                              my_sampling$sampMap$ggSilFGlin,
                              my_sampling$sampMap$ggCutFGmap,
                              my_sampling$sampMap$ggDepthFGbox,
                              my_sampling$sampMap$ggEffoFGbox,
                              my_sampling$sampMap$ggEffoFGmap,
                              my_sampling$sampMap$ggBioFGmat,
                              layout_matrix = rbind(c(1,3,3,4,5,7),c(2,6,6,4,5,7))))

# ## Export cutted shp file
# tmp_shp <- SpatialPolygonsDataFrame(my_sampling$sampMap$cutResShp,
#                                     data = aggregate(formula = . ~ FG, data = my_sampling$sampMap$cutResult, FUN = mean))
# writeSpatialShape(tmp_shp, "/Users/Lomo/Documents/Uni/R/smart/data/out/FG_cut/fg_19cuts")


### setDayEffoMatrGround                          ####    FIX output string
my_sampling$fleet$setDayEffoMatrGround()          ### head(my_sampling$fleet$dayEffoMatr[["2009"]])


# ####  Set weekly effort matrix for fishing grounds
# my_sampling$setWeekEffoMatrGround()          # head(my_sampling$fleet$weekEffoMatr[["2009"]])
#
# ###   Set weekly effort matrix for cells
# my_sampling$setWeekEffoMatrCell()            # head(my_sampling$fleet$weekEffoMatr[["2009"]])



##################################
#########   Harbours   ###########
##################################

# ###   Add harbour shapePoints
# my_sampling$sampMap$loadHarbDbf("/Users/Lomo/Documents/Uni/Lab/Maps/PORTS_ITA/harb_it.dbf")
#
# ###   Add nearest harbour
# my_sampling$setTrackHarb()                      ###  head(my_sampling$fleet$trackHarbs[["2009"]])



##################################
########   Production   ##########
##################################

###   Select csv files
# tmp_files <- gfile(text = "Select Landings Data", type = "open",
#                    filter = list("csv files" = list(patterns = c("*.csv")),
#                                  "All files" = list(patterns = c("*"))),
#                    multi = TRUE)
#
# ###   Import raw production files
# my_sampling$fleet$loadProduction(tmp_files)
my_sampling$fleet$rawProduction <- readRDS("/Users/Lomo/Documents/Uni/R/smart/data/Landings/LandAll.rData")

###   Set production vessels ids
my_sampling$fleet$setProdIds()                 ####   FIX output string

# ###   Plot ids count
# my_sampling$fleet$plotCountIDsProd()



##################################
###   Match effort/production   ##
##################################

###   Set IDs cross match effort/production
my_sampling$fleet$setIdsEffoProd()                ####    FIX no output string

# ###   Plot IDs cross match effort/production stats
# my_sampling$fleet$plotCountIDsEffoProd()


###   dcast production                            ####    FIX no output string
my_project$fleet$setProdMatr()                   ### head(my_sampling$fleet$prodMatr[["2009"]])


### setDayEffoMatrGround                          ####    FIX output string
my_project$fleet$setDayEffoMatrGround()          ### head(my_sampling$fleet$dayEffoMatr[["2009"]])


###   Match effort/production                     ####    FIX no output string
my_sampling$fleet$setEffoProdMatr()               ### head(my_sampling$fleet$effoProd[["2009"]])


###   cumulate month effort
my_sampling$fleet$setEffoProdMont()               ### head(my_sampling$fleet$effoProdMont[["2009"]])


###   set species in all years                    ### FIX no output string
my_sampling$fleet$setProdSpec()                   ### my_sampling$fleet$prodSpec

###   create single dataset from all years with species in common only   ### FIX no output string
my_sampling$fleet$setEffoProdAll()                ### head(my_sampling$fleet$effoProdAll)




##################################
######   Fleet Register   ########
##################################

my_sampling$fleet$loadFleetRegis("/Users/Lomo/Documents/Uni/R/smart/data/Fleet/ITA_export_smart-ed.csv")
my_sampling$fleet$cleanRegister()
# my_sampling$fleet$splitFleet()
my_sampling$fleet$plotRegSum()

my_sampling$fleet$setEffoProdAllLoa()
my_project$fleet$setEffoProdAllLoa()


### summary register
head(my_sampling$fleet$rawRegister)

barplot(table(my_sampling$fleet$rawRegister$Event.Code))

table(my_sampling$fleet$rawRegister$Event.End.Date)

activ <- which(my_sampling$fleet$rawRegister$Event.End.Date == "21001231")
length(activ)
length(which(my_sampling$fleet$rawRegister$Event.Code != "RET"))

barplot(table(round(my_sampling$fleet$rawRegister$Loa[activ])))
vmsequi <- which(my_sampling$fleet$rawRegister$Vms.Code == "Y")

active_vms <- vmsequi[vmsequi %in% activ]
barplot(table(my_sampling$fleet$rawRegister$Vms.Code[activ]))
barplot(table(my_sampling$fleet$rawRegister$Event.Code[active_vms]))
barplot(table(round(my_sampling$fleet$rawRegister$Loa[active_vms])))

vms_reg <- which(substr(my_sampling$fleet$rawRegister$CFR[active_vms], 8, 12) %in% as.character(my_sampling$fleet$effortIds[["All"]]))
length(which(as.character(my_sampling$fleet$effortIds[["All"]]) %in% substr(my_sampling$fleet$rawRegister$CFR[active_vms], 8, 12)))
my_sampling$fleet$rawRegister$Gear.Main.Code[active_vms][which(substr(my_sampling$fleet$rawRegister$CFR[active_vms], 8, 12) %in% as.character(my_sampling$fleet$effortIds[["All"]]))]


###   Create empty list for threshold settings
# my_sampling$fleet$setSpecSett()                            ###   my_sampling$fleet$specSett

###   Set list items
my_project$fleet$setSpecSettItm(specie = "Merluccius merluccius",
                                 thresh = 20,
                                 brea = 100,
                                 max_xlim = 200)

###   Get logit
my_project$fleet$setSpecLogit("Merluccius merluccius")
my_project$fleet$plotLogitROC("Merluccius merluccius")

specie <- "Mullus barbatus" #da far selezionare all'utente tramite interfaccia
specie <- "Merluccius merluccius"


# my_sampling$fleet$setNNLS()

minobs <- 10 #Numero di osservazioni minimo per ogni scenario dei beta (da fissare dall'utente)
thresR2 = 0 #Soglia minima di accettabilità delle regressioni nnls (da fissare dall'utente)


### !! load fleet register first and add loa to effoProdMat

my_project$getNnlsModel(specie, minobs, thresR2)
my_project$fleet$plotNNLS(specie = specie, thresR2 = thresR2)

###   Plot betas
### setBetaMeltYear
# my_sampling$fleet$setBetaMeltYear(specie)

### setPlotBetaMeltYear
my_sampling$setPlotBetaMeltYear(specie, year = "2014")

suppressWarnings(grid.arrange(my_sampling$sampMap$ggBetaFGmap,
                              my_sampling$sampMap$ggBetaFGbox,
                              layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))



##################################
#### Logit and NNLS expansion ####
##################################

### Set effoMont matrix
###   cumulate month effort
###   as my_sampling$fleet$setEffoProdMont()
my_sampling$fleet$setEffoMont()               ### head(my_sampling$fleet$effoMont[["2009"]])

###   create single dataset from all years
###   as my_sampling$fleet$setEffoProdAll()
my_sampling$fleet$setEffoAll()                ### head(my_sampling$fleet$effoAll)

###   add Loa to effoAll
###   as my_sampling$fleet$setEffoProdAllLoa()
my_sampling$fleet$setEffoAllLoa()             ### head(my_sampling$fleet$effoAllLoa)

my_sampling$predictProduction(specie)         ### head(my_sampling$fleet$predProd[[specie]])
# head(cbind(my_sampling$fleet$effoAllLoa[,1:3], my_sampling$fleet$predProd[[specie]]))

### Plot Production

my_sampling$fleet$setProdMeltYear(specie)

my_sampling$setPlotProdMeltYear(specie, year = "2014")

suppressWarnings(grid.arrange(my_sampling$sampMap$ggProdFGmap,
                              my_sampling$sampMap$ggProdFGbox,
                              layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))




##################################
####    Resource - Survey     ####
##################################

my_sampling$loadSurveyLFD(csv_path = "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Survey/survey_data_MEDITS.csv")
stargazer(my_sampling$rawDataSurvey)

if(!is.null(my_sampling$sampMap)){
  my_sampling$setLFDPopSurvey()
}
my_sampling$speDisPlot("MUT")

specie_name <- "MUT"
spe_ind <- which(my_sampling$specieInSurvey == specie_name)

year_drop <- "2009"  ### or "All"
ifelse(year_drop == "All",
       my_cel_dat <- my_sampling$surveyBySpecie[[spe_ind]]$rawDataSurvey[,c("LCLASS","FEMALE","MALE")],
       my_cel_dat <- my_sampling$surveyBySpecie[[spe_ind]]$rawDataSurvey[which(my_sampling$surveyBySpecie[[spe_ind]]$rawDataSurvey[,"Year"] == year_drop),c("LCLASS","FEMALE","MALE")])

the_reclfd <- smartR:::RecLFD(my_cel_dat, my_sampling$surveyBySpecie[[spe_ind]]$lengClas, 1)
smartR:::plotRecLFD(the_reclfd)

tmp_g <- read.table("/media/fish_team/SANDISK/smart/data/dps_g-param.csv", sep = ";", dec = ".", header = TRUE,
                    colClasses = c("factor", rep("numeric",6)))

effe_linf <- tmp_g[1,2:3]
effe_k <- tmp_g[1,4:5]
effe_t0 <- tmp_g[1,6:7]
emme_linf <- tmp_g[2,2:3]
emme_k <- tmp_g[2,4:5]
emme_t0 <- tmp_g[2,6:7]

my_sampling$surveyBySpecie[[spe_ind]]$setPrior(f_linf = effe_linf, f_k = effe_k, f_t0 = effe_t0,
                                         m_linf = emme_linf, m_k = emme_k, m_t0 = emme_t0)

##   Num of Cohorts
ncih_sb <- 3
my_sampling$surveyBySpecie[[spe_ind]]$setNCoho(ncih_sb)

##  Iteration and samples for mcmc
mc_niter <- 100
mc_nsamp <- 200
my_sampling$surveyBySpecie[[spe_ind]]$calcMix(nAdap = mc_niter, nSamp = mc_nsamp)

my_sampling$calcCoh_A_Survey(spe_ind)
my_sampling$intrpCoh_A_Survey(spe_ind)

my_sampling$cohoDisPlot(3, "All", "All", FALSE)



##################################
####    Resource - Fishery    ####
##################################

### "Load Sample"
my_project$loadFisheryLFD(csv_path = "/Users/Lomo/Documents/Uni/R/smart/data/Resource - Fishery/fishery_data_CampBiol.csv")
# head(my_project$rawDataFishery)
# head(my_project$fisheryBySpecie[[1]]$rawLFD)
# "Splitting Fishery Population"
if(!is.null(my_project$sampMap)){
  # svalue(stat_bar) <- "Splitting Fishery Population... "
  my_project$setLFDPopFishery()     # -> calcLFDPopFishery
}

# Set raw_data
# my_project$fisheryBySpecie[[1]]$setPreMix

specie <- "MUL"
for(sex in c("Female", "Male")){
  tmp_spre = my_project$fisheryBySpecie[[1]]$rawLFD[my_project$fisheryBySpecie[[1]]$rawLFD$Specie == specie & !is.na(my_project$fisheryBySpecie[[1]]$rawLFD$numFG), c("Date","Class", "numFG", sex)]

  num_sex <- sum(tmp_spre[,4])
  cat("\nFound", num_sex, sex, as.character(specie), "samples", sep = " ")

  spreDist <- data.frame(UTC = rep(tmp_spre$Date, tmp_spre[,4]),
                Length = rep(tmp_spre$Class, tmp_spre[,4]) + runif(num_sex, -0.5, 0.5),
                NumFG = rep(tmp_spre$numFG, tmp_spre[,4]))

  spreDist$Year <- years(spreDist$UTC)
  spreDist$Month <- months(spreDist$UTC)

  ifelse(sex == "Female",
         my_project$fisheryBySpecie[[1]]$spreFemale <<- spreDist,
         my_project$fisheryBySpecie[[1]]$spreMale <<- spreDist)
}

my_project$fisheryBySpecie[[1]]$spreFemale
raw_data <- my_project$fisheryBySpecie[[1]]$spreFemale

suppressMessages(
  ggplot(raw_data, aes(x = Length, y = ..count..)) +
    geom_hline(yintercept = pretty(range(hist(raw_data$Length,50, plot = FALSE)$counts),
                                   n = 5), col="grey75", lwd = 0.5, lty = 2) +
    geom_histogram(bins = 50, fill = "grey0", alpha = 0.7, col = "grey10") +
    annotate("text", x = min(raw_data$Length)+3, y = Inf, hjust = 0.5, vjust = 1.5,
             family="serif", label = c("Absolute frequency of \nlength values.")) +
    geom_vline(xintercept = mean(raw_data$Length), col = "white", lwd = 0.6, lty = 2) +
    theme_tufte(base_size=14, ticks=F) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 7),
          axis.text.y = element_text(size = 5),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
)

###

### "Set length weight\nrelationship"
# TO DO
###

### "Open\nLFD\nViewer"
# my_cel_dat <- my_project$fisheryBySpecie[[1]]$rawLFD[,c("Class","Female","Male")]
# the_reclfd <- RecLFD(my_cel_dat, my_project$fisheryBySpecie[[1]]$lengClas, 1)
# plotRecLFD(the_reclfd)
###

### "View\nSpatial\nDistribution"
my_sampling$plotGooSpe(whiSpe = "All", whiSou = "Fishery")
###

### "Assign FG"
my_sampling$addFg2Fishery()
# head(my_project$rawDataFishery)
###



##################################
####    Mixture - Fishery     ####
##################################


### Set number of cohorts
my_sampling$fisheryBySpecie[[1]]$setNCoho(7)
###

### "Set Priors"
# my_project$surveyBySpecie[[spe_ind]]$setPrior(...)
###

### Compute mixture
my_project$fisheryBySpecie[[1]]$calcMixDate(nAdap = as.numeric(svalue(mc_niter)),
                                             nSamp = as.numeric(svalue(mc_nsamp)))
###

### Transform length to cohorts
my_project$calcCoh_A_Survey(ind_spe)
###

### Interpolate cohorts
my_project$intrpCoh_A_Survey(ind_spe)
###










# Set priors
tmp_g <- read.table("/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/mut_g-param.csv", sep = ";", dec = ".", header = TRUE,
                    colClasses = c("factor", rep("numeric",6)))

effe_linf <- tmp_g[1,2:3]
effe_k <- tmp_g[1,4:5]
effe_t0 <- tmp_g[1,6:7]
emme_linf <- tmp_g[2,2:3]
emme_k <- tmp_g[2,4:5]
emme_t0 <- tmp_g[2,6:7]

my_sampling$fisheryBySpecie[[1]]$setPrior(f_linf = effe_linf, f_k = effe_k, f_t0 = effe_t0,
                                               m_linf = emme_linf, m_k = emme_k, m_t0 = emme_t0)

my_sampling$fisheryBySpecie[[1]]$setPreMix()

# new mixture model
my_sampling$fisheryBySpecie[[1]]$calcMixDate(nAdap = 100, nSamp = 200)

# Compute mixture
my_sampling$fisheryBySpecie[[1]]$calcMix(nAdap = 100, nSamp = 200)
# Transform length to cohorts
my_sampling$calcCoh_A_Fishery(which(my_sampling$specieInFishery == svalue(spec_drop_mix)))
# Interpolate cohorts
my_sampling$intrpCoh_A_Fishery(which(my_sampling$specieInFishery == svalue(spec_drop_mix)))
cohCoh_drop[] <- c("All", seq(1, my_sampling$fisheryBySpecie[[which(my_sampling$specieInFishery == svalue(spec_drop_mix))]]$nCoho, by = 1))





my_sampling <- SmartProject$new()
my_sampling$loadMap("/Users/Lomo/Documents/Uni/R/smart/data/Grid/GFCM_Grid_6min_GSA16.shp")
my_sampling$sampMap$getGooMap()
my_sampling$sampMap$setGooGrid()
my_sampling$sampMap$setGooBbox()
my_sampling$loadFisheryLFD(csv_path = "/Users/Lomo/Documents/Uni/R/smart/data/Resource - Fishery/fishery_data_CampBiol.csv")
my_sampling$setLFDPopFishery()
my_sampling$plotGooSpe(whiSpe = "All", whiSou = "Fishery")
# Set number of cohorts
my_sampling$fisheryBySpecie[[1]]$setNCoho(7)
# Set priors
tmp_g <- read.table("/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/mut_g-param.csv", sep = ";", dec = ".", header = TRUE,
                    colClasses = c("factor", rep("numeric",6)))
effe_linf <- tmp_g[1,2:3]
effe_k <- tmp_g[1,4:5]
effe_t0 <- tmp_g[1,6:7]
emme_linf <- tmp_g[2,2:3]
emme_k <- tmp_g[2,4:5]
emme_t0 <- tmp_g[2,6:7]

my_sampling$fisheryBySpecie[[1]]$setPrior(f_linf = 29, f_k = 0.5, f_t0 = 0.3,
                                          m_linf = 27, m_k = 0.5, m_t0 = 0.3)
my_sampling$fisheryBySpecie[[1]]$calcMixDate(nAdap = 100, nSamp = 200)





### Plot single vessel gridded effort
for(i in names(effoprod_merge)){
  for(j in unique(effoprod_merge[[i]]$I_NCEE)){
    tmp_ves <- effoprod_merge[[i]][effoprod_merge[[i]]$I_NCEE == j,]
    ves_poi <- my_sampling$fleet$rawEffort[[i]][my_sampling$fleet$rawEffort[[i]]$I_NCEE == j, c("LON","LAT")]
    tmp_dat <- apply(tmp_ves[,5:11],2,sum)

    all_cell <- merge(x = my_sampling$sampMap$gridPolySet$PID,
                      data.frame(x = as.numeric(names(tmp_dat)), y = tmp_dat), all = TRUE)
    grid_data <- cbind(my_sampling$sampMap$gridPolySet, Count = all_cell[,2], Alpha = ifelse(all_cell[,2] == 0, 0.2, 0.8))
    tmp_plot <- my_sampling$sampMap$gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID, fill = Count, alpha = Alpha), size = 0.2,
                                                              data = grid_data) +
      scale_fill_gradient(low = "Yellow", high = "coral") +
      ggtitle(paste(j, " - ", i, sep = "")) + scale_alpha(range=c(0,1), limits=c(0,1)) +
      guides(color = "legend", alpha = "none") +
      lims(x = extendrange(my_sampling$sampMap$plotRange[1:2]),
           y = extendrange(my_sampling$sampMap$plotRange[3:4])) +
      geom_point(data = ves_poi, aes(x = LON, y = LAT),
                 color = "firebrick", size = 0.4, alpha = 0.3)

  }
}


library(plyr)
library(intervals)
for(i in tmp_name){
  tmp_effo <-  ddply(subset(my_sampling$fleet$rawEffort[[i]],
                            I_NCEE %in% my_sampling$fleet$idsEffoProd[[i]]),
                     .(I_NCEE, T_NUM, WeekNum), summarize, min=min(DATE), max=max(DATE))
  tmp_prod <- subset(my_sampling$fleet$rawProduction[[i]],
                     NUMUE %in% my_sampling$fleet$idsEffoProd[[i]])

  res_cov <- data.frame("Vessel" = numeric(),
                        "Intersection" = numeric())

  for(j in my_sampling$fleet$idsEffoProd[[i]]){
    ves_eff <- tmp_effo[tmp_effo$I_NCEE == j,]
    ves_eff$min <- floor(ves_eff$min)
    ves_eff$max <- ceiling(ves_eff$max)
    ves_eff <- unique(ves_eff[,c("I_NCEE", "min", "max", "T_NUM", "WeekNum")])
    ves_pro <- tmp_prod[tmp_prod$NUMUE == j,]
    ves_pro <- unique(ves_pro[,c("NUMUE", "UTC_S", "UTC_E")])

    cov_eff <- sum(ves_eff$max - ves_eff$min)
    cov_pro <- sum(ves_pro$UTC_E - ves_pro$UTC_S)

    int_eff <- Intervals(ves_eff[,c("min", "max")])
    int_pro <- Intervals(ves_pro[,c("UTC_S","UTC_E")])

    ov_lst <- interval_overlap(int_eff, int_pro)

    tmp_ov <- cbind(rep(names(ov_lst), unlist(lapply(ov_lst, length))), unlist(ov_lst))
    cbind(ves_eff[tmp_ov[,1],], ves_pro[tmp_ov[,2],])

    tmp_cov <- round((cov_eff/cov_pro)*100, 2)
    tmp_int <- sum(apply(interval_intersection(int_eff, int_pro)@.Data,1,diff))/sum(apply(ves_pro[,c("UTC_S","UTC_E")] ,1,diff))
    res_cov <- rbind(res_cov, cbind("Vessel" = j,
                                    "Intersection" = tmp_int))
    plot(NULL,
         xlim = c(min(ves_pro$UTC_S, ves_eff$min), max(ves_pro$UTC_E, ves_eff$max)),
         ylim = c(1.5,2.2), mar = c(0,0,0,0),
         type='n', axes=FALSE, ann=FALSE)
    title(paste(j, "\nRaw coverage: ", tmp_cov,
                "%\nIntersection: ", tmp_int, " days",
                sep = ""))
    segments(x0 = ves_eff$min, y0 = 1.5, x1 = ves_eff$max, y1 = 1.5, lwd = 6)
    segments(x0 = ves_pro$UTC_S, y0 = 2, x1 = ves_pro$UTC_E, y1 = 2, lwd = 3, col = "red")
    points(x = ves_pro$UTC_S, y = rep(1.99, length(ves_pro$UTC_S)), pch = 2, col = 2, cex = 0.5)
    points(x = ves_pro$UTC_E, y = rep(2.01, length(ves_pro$UTC_E)), pch = 6, col = 2, cex = 0.5)
    points(x = ves_eff$min, y = rep(1.49, length(ves_eff$min)), pch = 2, cex = 0.5)
    points(x = ves_eff$max, y = rep(1.51, length(ves_eff$min)), pch = 6, cex = 0.5)
  }

  res_cov <- res_cov[order(res_cov$Intersection),]
  plot(1:nrow(res_cov), res_cov$Intersection, main = paste("Ordered Intersection ", i),
       type = "l", col = "navy", lwd = 2, ylab = "", xlab = "Obs")
  abline(h=0.3)
  #   lines(res_cov$Coverage, col = "firebrick", lwd = 2)
  #   legend("topleft", c("Intersection", "Coverage"), col = c("navy","firebrick"),
  #          lty = 1, lwd = 2)
  cat("\n",sum(res_cov$Intersection >= 0.3), " vessels over threshold for year ", i, "\n", sep = "")
  print(res_cov[res_cov$Intersection >= 0.3,"Vessel"])
}




### Depth plotting
tmp_dat <- my_sampling$fleet$rawEffort[[j]][sample(1:nrow(my_sampling$fleet$rawEffort[[j]]), 100000),c("LON","LAT","DEPTH")]
my_sampling$sampMap$gooMapPlot +
  geom_point(data = tmp_dat,
             aes(x = LON, y = LAT, color = DEPTH), size = 2, alpha = 0.5)+
  coord_fixed(xlim = extendrange(my_sampling$sampMap$plotRange[1:2]),
              ylim = extendrange(my_sampling$sampMap$plotRange[3:4]), expand = TRUE)


