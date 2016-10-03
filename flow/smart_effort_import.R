
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
pathFishery <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_data_CampBiol_noExpLand.csv"
# pathFishery <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_merge_CampBiol.csv"

cambBiolCSV <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_merge_CampBiol.csv"


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
my_sampling$loadFisheryLFD(csv_path = pathFishery)

# head(my_sampling$rawDataFishery)
# head(my_sampling$fisheryBySpecie[[1]]$rawLFD)

# # "Splitting Fishery Population"
# if(!is.null(my_sampling$sampMap)){
#   # svalue(stat_bar) <- "Splitting Fishery Population... "
#   my_sampling$setLFDPopFishery()     # -> calcLFDPopFishery
# }

if(!is.null(my_sampling$sampMap$cutResShp)){
  my_sampling$addFg2Fishery()
  my_sampling$setSpreaDistAll()
  my_sampling$setSpatDistAll()

  my_sampling$sampMap$set_ggMapFgSamp()
}

# Set fisheryRawIn

suppressWarnings(grid.arrange(my_sampling$fisheryBySpecie[[1]]$plotFemale[["histLfdTot"]],
                              my_sampling$fisheryBySpecie[[1]]$plotFemale[["histUtcLfd"]],
                              my_sampling$fisheryBySpecie[[1]]$plotFemale[["histUtcTot"]],
                              my_sampling$fisheryBySpecie[[1]]$plotFemale[["dotUtcSplit"]],
                              layout_matrix = rbind(c(1,1,1,1),
                                                    c(2,2,2,2),
                                                    c(2,2,2,2),
                                                    c(3,3,4,4))
))

###

### "Set length weight\nrelationship"
# TO DO
###


### "View\nSpatial\nDistribution"
# my_sampling$plotGooSpe(whiSpe = "All", whiSou = "Fishery")
suppressWarnings(my_sampling$sampMap$ggMapFgSamp)

my_sampling$setSpatDistAll()


suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgSamp,
                              my_project$fisheryBySpecie[[1]]$plotFemale[["spatAbbFreq"]],
                              my_project$fisheryBySpecie[[1]]$plotFemale[["spatRelFreq"]],
                              my_project$fisheryBySpecie[[1]]$plotFemale[["spatAbbTbl"]],
                              layout_matrix = rbind(c(4,1,1,2),
                                                    c(4,1,1,3))
))
###





######
### FishBase data
mut_popgrowth <- popgrowth("Mullus barbatus barbatus")
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

# ### Compute mixture
# my_project$fisheryBySpecie[[1]]$calcMixDate(nAdap = as.numeric(svalue(mc_niter)),
#                                             nSamp = as.numeric(svalue(mc_nsamp)))
# ###



######
### MCMC model definition

modelGomGro

modelGomGro <- system.file("model/gompGrow.jags", package = "smartR")
###

######
### MCMC input setup

# Nclust <- params$Nclus
Nclust <- nCoho
# Nsamp <- params$Nsamp
Nsamp <- 500

outPalette <- rainbow(Nclust)


###
sub_idx <- sample(1:nrow(spreFemale), size = Nsamp)
sub_data <- spreFemale[sub_idx,]

N <- length(sub_data$Length)
alpha = rep(1, Nclust)
Z = rep(NA, N)
Z[which.min(sub_data$Length)] = 1
Z[which.max(sub_data$Length)] = Nclust

dataList <- list(y = sub_data$Length,
                 maxLeng = max(sub_data$Length),         ## !!!
                 alpha = alpha,
                 Z = Z,
                 N = N,
                 Nclust = Nclust)

inits = list(list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0),
             list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0),
             list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0))

tt = as.POSIXlt(chron(spreFemale$UTC))$yday / 366
###

######
### MCMC model setup
# n.adapt <- params$Nadap
n.adapt <- 500

jags.m <- jags.model(textConnection(modelGomGro),
                     data = dataList,
                     inits = inits,
                     n.chains = 3,
                     n.adapt = n.adapt)
###

######
### MCMC chain sampling
# n.iter <- params$Niter
n.iter <- 500

obsNode <- c('Linf', 'k', 't0', 'tau', 'p')
samps <- coda.samples(jags.m, obsNode, n.iter = n.iter)
###

######
### MCMC estimates
vecLinf <- as.matrix(samps[,"Linf"])
vecKapp <- as.matrix(samps[,"k"])
vecTzer <- as.matrix(samps[,"t0"])
LHat = mean(vecLinf)
kHat = mean(vecKapp)
t0Hat = mean(vecTzer)
taus <- as.matrix(samps[,grep("tau" ,varnames(samps))])
sigma2s = 1/taus
sigma2Hat = apply(sigma2s, 2, mean)
pArray <- do.call(rbind, samps[,grep("p" ,varnames(samps))])
pHat <- matrix(apply(pArray, 2, mean), byrow = FALSE, ncol = Nclust)
###

######
### MCMC chain trace
dfLinf <- data.frame(Parameter = "Linf",
                     Iter = 1:n.iter,
                     Chain = as.matrix(samps[,"Linf"], chains = TRUE)[,1],
                     Value = as.matrix(samps[,"Linf"], chains = TRUE)[,2])
dfKapp <- data.frame(Parameter = "Kappa",
                     Iter = 1:n.iter,
                     Chain = as.matrix(samps[,"k"], chains = TRUE)[,1],
                     Value = as.matrix(samps[,"k"], chains = TRUE)[,2])
dfTzer <- data.frame(Parameter = "t0",
                     Iter = 1:n.iter,
                     Chain = as.matrix(samps[,"t0"], chains = TRUE)[,1],
                     Value = as.matrix(samps[,"t0"], chains = TRUE)[,2])
###

######
### MCMC chain Traceplot
ggdataSamps <- rbind(dfLinf, dfKapp)

traceChain <- suppressMessages(
  ggplot(data = ggdataSamps,
         mapping = aes(x = Iter, y = Value, color = factor(Chain)))+
    geom_line(alpha = 0.7) +
    facet_wrap(~ Parameter, nrow = 3, ncol = 1, scales = "free", switch = "y") +
    scale_color_brewer(palette = "Dark2", "Chain") +
    theme_tufte(base_size = 14, ticks = F) +
    theme(title = element_text(size = 10),
          legend.position = "none",
          legend.title = element_text(size = 7),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 6),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 6),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
)
###

######
### MCMC chain scatterplot
ggdataSampScat <- cbind(dfLinf[,2:3], Linf = dfLinf[,4], Kappa = dfKapp[,4])


# IF mut_popgrowth
scatLK <- suppressMessages(
  ggplot()+
    geom_point(data = ggdataSampScat,
               mapping = aes(x = Linf, y = Kappa, color = factor(Chain)),
               size = 0.25, alpha = 0.25) +
    annotate("point", x = mut_popgrowth$Loo, y = mut_popgrowth$K, color = "grey25", size = 0.7) +
    annotate("point", x = LHat, y = kHat, color = "goldenrod1",
             shape = 42, size = 12, alpha = 0.9) +
    annotate("point", x = mean(mut_popgrowth$Loo), y = mean(mut_popgrowth$K), color = "firebrick",
             shape = 20, size = 5, alpha = 0.9) +
    annotate("text", x = Inf, y = Inf, label = paste("LHat = ", round(LHat, 2), "\nKHat = ", round(kHat, 3), sep = ""), hjust = 1, vjust = 1, color = "goldenrod1", fontface = "bold") +
    scale_color_brewer(palette = "Dark2", "Chain") +
    theme_tufte(base_size = 14, ticks = F) +
    theme(legend.position = "top",
          legend.title = element_text(size = 9),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 6),
          axis.title.x = element_text(size = 8),
          axis.text.y = element_text(size = 6),
          axis.title.y = element_text(size = 8),
          axis.ticks.y = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size = 3,
                                                     alpha = 0.9,
                                                     fill = NA)))
)
###



######
### age estimation
means.f = matrix(0, nrow(spreFemale), Nclust)
zHat = numeric(nrow(spreFemale))
for(iObs in 1:nrow(spreFemale)){

  ## GGF
  ##  temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:Nclust) - 1 - t0Hat))))
  temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:Nclust)-1+tt[iObs]))))  ##BEST
  ##  temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:Nclust)-1+tt[iObs] - t0Hat))))

  # VBGF
  # temp = LHat * (1 - exp(-kHat*(((1:Nclust)-1+tt[iObs]) - t0Hat)))

  means.f[iObs,] = temp
  postProbs = dnorm(spreFemale$Length[iObs], temp, sqrt(sigma2Hat))
  zHat[iObs] = as.numeric(names(which.max(table(sample(1:Nclust, size = 150, prob = postProbs, replace = TRUE)))))
}

ages.f = zHat -1 + tt #- t0Hat

AA = floor(ages.f)
###

######
### MCMC chain Boxplot Tau
cohoPreci <- melt(taus[,1:(max(AA)+1)])
names(cohoPreci) <- c("Iter", "Cohort", "Value")
cohoPreci$Cohort <- factor(as.numeric(cohoPreci$Cohort), levels = 1:(Nclust))
stsPreci <- boxplot.stats(cohoPreci$Value)$stats ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display

cohoPreciGG <- suppressMessages(
  ggplot(cohoPreci, aes(x = Cohort, y = Value, fill = Cohort)) +
    geom_boxplot(alpha = 0.6, outlier.color = "grey30", outlier.size = 0.35, notch = TRUE) +
    ggtitle("Precision") +
    scale_x_discrete(labels = 0:(Nclust-1)) +
    scale_fill_manual(values = outPalette) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    theme(legend.position = "none",
          title = element_text(size = 9),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(stsPreci[2]/2,max(stsPreci)*1.25)) ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display

)
###

######
### MCMC Boxplot Sigma

cohoVari <- melt(sqrt(sigma2s[,1:(max(AA)+1)]))
names(cohoVari) <- c("Iter", "Cohort", "Value")
cohoVari$Cohort <- factor(as.numeric(cohoVari$Cohort), levels = 1:(Nclust))
stsVari <- boxplot.stats(cohoVari$Value)$stats ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display

cohoVariGG <- suppressMessages(
  ggplot(cohoVari, aes(x = Cohort, y = Value, fill = Cohort)) +
    geom_boxplot(alpha = 0.6, outlier.color = "grey30", outlier.size = 0.35, notch = TRUE) +
    ggtitle("SD") +
    scale_x_discrete(labels = 0:(Nclust-1)) +
    scale_fill_manual(values = outPalette) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    theme(legend.position = "none",
          title = element_text(size = 9),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(stsVari[2]/2,max(stsVari)*1.25)) ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display

)
###


suppressWarnings(grid.arrange(traceChain,
                              scatLK,
                              layout_matrix = rbind(c(1,1,2),
                                                    c(1,1,2))
))

# suppressWarnings(grid.arrange(traceChain,
#                               scatLK,
#                               # groCurv,
#                               cohoPreciGG,
#                               cohoVariGG,
#                               layout_matrix = rbind(c(1,1),
#                                                     c(2,NA),
#                                                     c(3,4))
# ))



######
### MCMC output
FGlabels = as.numeric(as.character(spreFemale$NumFG))
FGnames = unique(FGlabels)
FG = numeric(length(FGlabels))
for(FGname in 1:length(FGnames)){
  idx_FG = which(FGlabels == FGnames[FGname])
  FG[idx_FG] = rep(FGname, length(idx_FG))
}
nFG = length(unique(FG))

mix_out <- data.frame(Length = spreFemale$Length,
                      Date = spreFemale$UTC,
                      Day = tt,
                      Age = AA,
                      AgeNF = ages.f,
                      FG = FGlabels)

mix_out$Year <- years(mix_out$Date)
mix_out$Month <- as.numeric(months(mix_out$Date))
mix_out$MonthChar <- spreFemale$Month
mix_out$Quarter <- as.numeric(quarters(mix_out$Date))
mix_out$Birth <- as.numeric(as.character(mix_out$Year)) - mix_out$Age

zeroedMonth <- ifelse(nchar(mix_out$Month) == 2, mix_out$Month, paste("0", mix_out$Month, sep = ""))
mix_out$CatcDate <- factor(paste(mix_out$Year,
                                 zeroedMonth, sep = "-"),
                           levels = paste(rep(sort(unique(mix_out$Year)), each = 12),
                                          sort(unique(zeroedMonth)), sep = "-"))

# mix_out$CorrBirth <- mix_out$Birth
# mix_out$CorrBirth[which(mix_out$AgeNF %% 1 + 0.25 > 1)] <- mix_out$CorrBirth[which(mix_out$AgeNF %% 1 + 0.25 > 1)] + 1
# mix_out$CorrBirth[which(mix_out$AgeNF - mix_out$Age > 0.81)] <- mix_out$CorrBirth[which(mix_out$AgeNF - mix_out$Age > 0.81)] - 1

growPath <- data.frame(Birth = rep(min(mix_out$Birth):(min(mix_out$Birth)+11), each = length(levels(mix_out$CatcDate))),
                       Date = rep(levels(mix_out$CatcDate), times = length(min(mix_out$Birth):(min(mix_out$Birth)+11))),
                       Length = NA)
growPath$Age <- as.numeric(strtrim(growPath$Date, 4)) - growPath$Birth + as.numeric(substr(growPath$Date, 6,7))/12
growPath$Length <- calcGomp(LHat, kHat, growPath$Age)
growPath$Date <- factor(growPath$Date, levels = levels(mix_out$CatcDate))
# growPath <- growPath[growPath$Age > 0,]
growPath <- growPath[growPath$Length > floor(min(mix_out$Length)),]

coho_AL <- ddply(mix_out, .(Age), summarise,
                 coh.mean = mean(Length), coh.var = var(Length), coh.num = length(Length))
###

######
### MCMC calc birth
out_birth <- table(paste(mix_out$Year, mix_out$Quarter, sep = "_"),  mix_out$Birth)
birth_melt <- melt(out_birth)
names(birth_melt) <- c("Catch", "Birth", "Qty")
birth_melt$Catch <- factor(birth_melt$Catch, levels = paste(rep(levels(mix_out$Year), each = 4),
                                                            rep(1:4, times = length(levels(mix_out$Year))), sep = "_"))
birth_melt$Birth <- as.factor(birth_melt$Birth)
birth_melt <- birth_melt[birth_melt$Qty != 0,]
###


######
### MCMC Calc Survivors
tot_count <- apply(out_birth,2, sum)
surv_tbl <- out_birth
for(i in 1:nrow(out_birth)){
  surv_tbl[i,] <- tot_count
  tot_count <- tot_count - out_birth[i,]
}

surv_melt <- melt(surv_tbl)
names(surv_melt) <- c("Catch", "Birth", "Qty")
surv_melt$Catch <- factor(surv_melt$Catch, levels = paste(rep(levels(mix_out$Year), each = 4),
                                                          rep(1:4, times = length(levels(mix_out$Year))), sep = "_"))
surv_melt <- surv_melt[!duplicated(surv_melt[,2:3], fromLast = TRUE),]
surv_melt <- surv_melt[surv_melt$Qty != 0,]
surv_melt$Age <- as.numeric(strtrim(surv_melt$Catch, 4)) - surv_melt$Birth + as.numeric(substr(surv_melt$Catch, 6, 7))/4
surv_melt$Birth <- as.factor(surv_melt$Birth)
surv_melt$QtyNorm <- 100*round(as.numeric(surv_melt$Qty/apply(surv_tbl,2,max)[surv_melt$Birth]), 2)
# surv_melt$QtyNorm <- 100*round(as.numeric(surv_melt$Qty/max(surv_tbl)), 1)

surv_melt$Zeta <- 0
for(i in unique(surv_melt$Birth)){
  tmp_surv_i <- surv_melt[surv_melt$Birth == i,]
  surv_melt$Zeta[surv_melt$Birth == i] <- c(0,-diff(tmp_surv_i$Qty)/diff(tmp_surv_i$Age)/tmp_surv_i$Qty[1])
  # surv_melt$Zeta[surv_melt$Birth == i] <- c(0,1/diff(tmp_surv_i$Age)*log(tmp_surv_i$Qty[-nrow(tmp_surv_i)]/tmp_surv_i$Qty[-1]))
  # surv_melt$Zeta[surv_melt$Birth == i] <- c(-diff(tmp_surv_i$Qty)/tmp_surv_i$Qty[-nrow(tmp_surv_i)], 0)
}
# surv_melt$Zeta <- 0.2*(surv_melt$Zeta)/(1/surv_melt$Zeta)
###

######
### MCMC Plot Age-Length
xAgeBreak <- 0:max(ceiling(surv_melt$Age))
yAgeBreak <- pretty(mix_out$Length, 10)

ageLenPlo <- suppressMessages(
  ggplot() +
    scale_x_continuous("Age", breaks = xAgeBreak) +
    scale_y_continuous("Length", breaks = yAgeBreak) +
    geom_point(data = mix_out, aes(x = AgeNF, y = Length, color = factor(Age)), size = 0.3) +
    geom_point(data = mix_out, aes(x = Age, y = Length, fill = factor(Age)), shape = 21, color = "grey20", size = 1.2) +
    scale_color_manual(values = outPalette) +
    scale_fill_manual(values = outPalette) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    theme(legend.position = "none",
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 8))
)
###

######
### MCMC Age-Length Key
ageLenTheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.4)),
  colhead = list(fg_params=list(cex = 0.5)),
  rowhead = list(fg_params=list(cex = 0.3)))

ageLenKey <- tableGrob(table(round(spreFemale$Length), AA), theme = ageLenTheme)
###

######
### MCMC output cohort stats
cohSliTheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.4)),
  colhead = list(fg_params=list(cex = 0.5)),
  rowhead = list(fg_params=list(cex = 0.4)))

cohSliTab <- tableGrob(round(coho_AL, 2), theme = cohSliTheme, rows=NULL)
###

suppressWarnings(grid.arrange(ageLenPlo,
                              ageLenKey,
                              cohSliTab,
                              layout_matrix = rbind(c(1,1,2),
                                                    c(1,1,2),
                                                    c(1,1,3))
))


######
### MCMC quarter vertical hist
growthLine <- suppressMessages(
  ggplot(data = mix_out,
         mapping = aes(x = CatcDate, y = Length,
                       color = factor(Birth))) +
    scale_color_brewer(name = "Year of Birth", palette = "Paired") +
    geom_jitter(size = 0.05, height = 0, width = 0.9, alpha = 0.4) +
    scale_x_discrete(drop = FALSE) +
    geom_line(data = growPath,
              mapping = aes(x = Date, y = Length, group = Birth),
              linetype = 2) +
    guides(colour = guide_legend(override.aes = list(size = 2.5,
                                                     alpha = 0.9,
                                                     fill = NA))) +
    theme_tufte(base_size = 14, ticks = FALSE) +
    theme(legend.position = "bottom",
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 8, angle = 90),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.key = element_blank())
)
###

######
### MCMC Catch * Quarters
catchLine <- suppressMessages(
  ggplot() +
    geom_line(data = birth_melt, aes(x = Catch, y = Qty, group = Birth, color = Birth)) +
    scale_color_brewer(palette = "Paired") +
    theme_tufte(base_size = 14, ticks = F) +
    scale_x_discrete(drop = FALSE) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, family="serif", label = "Catches") +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.text.x = element_text(size = 5, angle = 45),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
)
###

######
### MCMC Survivors * quarter
survLine <- suppressMessages(
  ggplot(data = surv_melt, aes(x = Catch, y = Qty, group = Birth, color = Birth)) +
    geom_line() +
    scale_x_discrete(drop = FALSE) +
    theme_tufte(base_size = 14, ticks=F) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,  family="serif", label = "Survivors") +
    scale_color_brewer(palette = "Paired") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 5, angle = 90),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
)
###

######
### MCMC mortality
ageMaturity <- 2
Z_aftSpaw <- mean(surv_melt[surv_melt$Birth %in% names(which(table(surv_melt$Birth)>4*2)) & surv_melt$Age > ageMaturity, "Zeta"])
Z_befSpaw <- mean(surv_melt[surv_melt$Birth %in% names(which(table(surv_melt$Birth)>4*2)) & surv_melt$Age <= ageMaturity, "Zeta"])


mortLine <- suppressMessages(
  ggplot(data = surv_melt[surv_melt$Birth %in% names(which(table(surv_melt$Birth)>4*2)),],
         aes(x = round(Age), y = Zeta, group = round(Age), fill = factor(round(Age)))) +
    geom_boxplot() +
    geom_vline(aes(xintercept = ageMaturity), linetype = 2, color = "grey20") +
    annotate("text", x = ageMaturity - 0.1, y = Inf, angle = 90, fontface = "bold",
             size = 3, family="serif", color = "grey20", label = "Age at Maturity", hjust = 1, vjust = 0) +
    theme_tufte(base_size = 14, ticks=F) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, family="serif", label = "Mortality") +
    scale_fill_manual(values = outPalette, limits = 0:max(ceiling(surv_melt$Age))) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 5),
          panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 5),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_x_continuous(breaks = 0:max(ceiling(surv_melt$Age))) +
    scale_y_continuous(breaks = pretty(0:2, 10))
)
###

suppressWarnings(grid.arrange(growthLine,
                              catchLine,
                              survLine,
                              mortLine,
                              layout_matrix = rbind(c(1,1,1),
                                                    c(1,1,1),
                                                    c(2,3,4))
))







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


