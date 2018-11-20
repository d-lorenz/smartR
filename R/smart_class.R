#### SmartProject#############################################
#' SmartProject Class
#'
#' The \code{SmartProject} class implements the main class of
#' \pkg{smartR} package.
#'
#' @docType class
#'
#' @usage NULL
#'
#' @keywords data
#' @return Object of \code{\link{R6Class}} with attributes and methods to fullfill
#' a complete analisys with the SMART approach.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field rawDataSurvey Stores the raw survey data after being populated by \code{loadSurveyLFD()} method.
#' @field yearInSurvey Stores the distinct years in the \code{rawDataSurvey} time-serie.
#' @field specieInSurvey Stores the distinct species in the \code{rawDataSurvey} time-serie.
#' @field surveyBySpecie Stores a list of \code{\link{SurveyBySpecie}} objects, one for each species in the time-series.

#' @field rawDataFishery Stores the raw fishery data as is in the provided csv file. The attribute is populated by \code{loadFisheryLFD()} method.
#' @field yearInFishery Stores the distinct years in the \code{rawDataFishery} time-serie.
#' @field specieInFishery Stores the distinct species in the \code{rawDataFishery} time-serie.
#' @field fisheryBySpecie Stores a list of \code{\link{FisheryBySpecie}} objects, one for each species in the time-series.
#'
#' @field gooLstCoho Stores a list of plots of species cohorts spatial distribution .
#'
#' @field sampMap Stores the \code{\link[=SampleMap]{environment}} object.
#' @field fleet Stores the \code{\link{FishFleet}} object.
#'
#' @field simProd Stores the simulated pattern of production.
#' @field simEffo Stores the simulated pattern of effort.
#' @field simBanFG Stores a vector of fishable/banned fishing grounds.
#' @field simSpatialCost Stores the simulated pattern of spatial costs.
#' @field simEffortCost Stores the simulated pattern of effort costs.
#' @field simProdCost Stores the simulated pattern of production costs.
#' @field simTotalCost Stores the simulated pattern of total costs.
#' @field simRevenue Stores the simulated pattern of revenue by species and fishing ground.
#' @field simTotalRevenue Stores the simulated pattern of total revenues.
#' @field simCostRevenue Stores the simulated pattern of costs and revenues.
#' @field simResPlot Stores the plots with the simulation' results.

#' @field outGmat Stores the evolution of gains during the simulation.
#' @field outOptimEffo Stores the resulting pattern of effort.
#' @field outWeiProp Stores the annual proportion of fish by cohort and fishing ground.
#' @field outWeiPropQ Stores the seasonal proportion of fish by cohort and fishing ground.
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{setCostInput()}}{This method is used to setup the required data for costs computation}
#'   \item{\code{setInProduction()}}{This method is used to setup the required data for production costs computation}
#'   \item{\code{setDaysAtSea()}}{This method is used to compute the number of Days at Sea of each vessel}
#'   \item{\code{setEffortIndex()}}{This method is used to compute the value of the Effort Index}
#'   \item{\code{setProductionIndex()}}{This method is used to compute the value of the Production Index}
#'   \item{\code{getHarbFgDist()}}{This method is used to compute the weighted average distance of every fishing ground to each harbour}
#'   \item{\code{setFgWeigDist()}}{This method is used as helper function to get the weighted average distance between fishing ground and harbours}
#'   \item{\code{setRegHarbBox()}}{This method is used to compute the distance of each harbour to every fishing ground centroid}
#'   \item{\code{loadSurveyLFD(csv_path)}}{This method is used to load the raw survey LFD data from a  csv file}
#'   \item{\code{loadFisheryLFD(csv_path)}}{This method is used to load the raw fishery LFD data from a  csv file}
#'   \item{\code{setYearSurvey()}}{This method is used to store the distinct year in the survey time-series}
#'   \item{\code{setYearFishery()}}{TThis method is used to store the distinct year in the fishery time-series}
#'   \item{\code{loadMap(map_path)}}{This method is used to load the Environmental Grid and initialize the Environment object}
#'   \item{\code{createFleet()}}{This method is used to initialize the Fleet object}
#'   \item{\code{setSpecieSurvey()}}{This method is used to store the distinct species in the survey dataset}
#'   \item{\code{setSpecieFishery()}}{This method is used to store the distinct species in the fishery dataset}
#'   \item{\code{splitSpecieSurvey()}}{This method is used to split the survey dataset by species}
#'   \item{\code{splitSpecieFishery()}}{This method is used to split the fishery dataset by species}
#'   \item{\code{addSpecieSurvey(sing_spe)}}{This method is used to initialize a new \code{surveyBySpecie} object}
#'   \item{\code{addSpecieFishery(sing_spe)}}{This method is used to initialize a new \code{fisheryBySpecie} object}
#'   \item{\code{setSpreaFishery()}}{This method is used to prepare the fishery LFD data for MCMC analysis}
#'   \item{\code{setSpatFishery()}}{This method is used to setup the plot with the spatial distribution of the fishery dataset}
#'   \item{\code{setSpreaSurvey()}}{This method is used to prepare the survey LFD data for MCMC analysis}
#'   \item{\code{setSpatSurvey()}}{This method is used to setup the plot with the spatial distribution of the survey dataset}
#'   \item{\code{setDepthSurvey()}}{This method is used to assign the depth of each survey tow}
#'   \item{\code{setStratumSurvey()}}{This method is used to assign a depth stratum to each survey tow}
#'   \item{\code{setAbuAvgAll()}}{This method is used to compute the spiecies abundances at each survey stratum}
#'   \item{\code{setMeditsIndex()}}{This method is used to compute the MEDITS index}
#'   \item{\code{setStrataAbu()}}{This method is used to compute the weighted number of individuals of each size in every stratum}
#'   \item{\code{loadFleeEffoDbs(effort_path, met_nam, onBox = TRUE, perOnBox = 1)}}{This method is used to extract the vms data from one or more vmsbase DB}
#'   \item{\code{ggplotRawPoints(year)}}{This method is used to plot the raw vms points}
#'   \item{\code{ggplotFgWeigDists()}}{This method is used to plot the weighted average distance between harbours and fishing grounds}
#'   \item{\code{setAvailData()}}{This method is used to gather the required data for the spatial clustering}
#'   \item{\code{predictProduction(specie)}}{This method is used to compute the estimated production}
#'   \item{\code{simProdAll(selRow = numeric(0))}}{This method is used to compute the simulated production}
#'   \item{\code{genSimEffo(method = "flat", selRow = numeric(0), areaBan = numeric(0))}}{This method is used to create a simulated pattern of effort}
#'   \item{\code{getSimSpatialCost()}}{This method is used to compute the simulated spatial costs}
#'   \item{\code{getSimEffortCost()}}{This method is used to compute the simulated effort costs}
#'   \item{\code{getSimProdCost()}}{This method is used to compute the simulated production costs}
#'   \item{\code{getSimTotalCost()}}{This method is used to collect all the simulated costs}
#'   \item{\code{getSimRevenue(selRow = numeric(0), timeScale = "Year")}}{This method is used to compute the simulated revenues}
#'   \item{\code{getLWstat()}}{This method is used to compute the length/weight statistics for each fishing ground}
#'   \item{\code{simulateFishery(thr0 = 100, effoBan = numeric(0), timeStep = "Year")}}{This method is used to simulate one year of fishing}
#'   \item{\code{setSimResults()}}{This method is used to store the results of a simulation run}
#'   \item{\code{ggplotFishingPoints(year)}}{This method is used to plot the fishing points}
#'   \item{\code{setCellPoin()}}{This method is used assign a cell to each vms point}
#'   \item{\code{setTrackHarb()}}{This method is used to assign the harbour to each fishing trip}
#'   \item{\code{setFishGround(numCut)}}{This method is used to setup the fishing ground configuration}
#'   \item{\code{addFg2Fishery()}}{This method is used to add the fishing ground information to each fishery data point}
#'   \item{\code{addFg2Survey()}}{This method is used to add the fishing ground information to each survey data point}
#'   \item{\code{setWeekEffoMatrCell()}}{This method is used to combine the raw effort points in weekly aggregated effort by cell}
#'   \item{\code{setWeekEffoMatrGround()}}{This method is used to combine the raw effort points in weekly aggregated effort by fishing ground}
#'   \item{\code{ggplotGridEffort(year)}}{This method is used to plot the gridded fishing effort}
#'   \item{\code{getNnlsModel(specie, minobs, thr_r2)}}{This method is used to compute the coefficients of the NNLS model}
#'   \item{\code{cohoDisPlot(whoSpe, whoCoh, whiYea, interp)}}{This method is used to store the spatial distribution of the species by cohort}
#'   }
#' @examples
#' # Initialize SmartProject
#' yourSmartRstudy <- SmartProject$new()
#' 
#' # Initialize fleet object
#' yourSmartRstudy$createFleet()
#' 
#' 
#' ######################
#' ## Environment Data ##
#' ######################
#' 
#' # Locate the example environment asset' file
#' envAssetPath <- system.file("extdata/mapAsset.RDS", package = "smartR")
#' 
#' # Load environment asset' data
#' yourSmartRstudy$importEnv(readRDS(envAssetPath))
#' 
#' # Setup case study' map
#' yourSmartRstudy$sampMap$getGooMap()
#' yourSmartRstudy$sampMap$setGooGrid()
#' yourSmartRstudy$sampMap$setGooBbox()
#' yourSmartRstudy$sampMap$setGgDepth()
#' yourSmartRstudy$sampMap$setGgBioDF()
#' # View case study' grid
#' print(yourSmartRstudy$sampMap$gooGrid)
#' 
#' 
#' ################
#' ## Fleet Data ##
#' ################
#' 
#' # Locate the example fleet asset' file
#' effAssetPath <- system.file("extdata/effAsset.RDS", package = "smartR")
#' 
#' # Load fleet asset' data
#' yourSmartRstudy$fleet$rawEffort <- readRDS(effAssetPath)
#' 
#' # Setup fishing vessel ids
#' yourSmartRstudy$fleet$setEffortIds()
#' 
#' # View speed distribution to setup fishing point filter
#' yourSmartRstudy$fleet$plotSpeedDepth(
#' which_year = "2012",
#' speed_range = c(2, 8),
#' depth_range = c(-20, -600)
#' )
#' 
#' # Setup fishing points' filter
#' yourSmartRstudy$fleet$setFishPoinPara(
#' speed_range = c(2, 8),
#' depth_range = c(-20, -600)
#' )
#' 
#' # Compute fishing points
#' yourSmartRstudy$fleet$setFishPoin()
#' 
#' # Assign cell id to each fishing point
#' yourSmartRstudy$setCellPoin()
#' 
#' # Add week and month number to each point
#' yourSmartRstudy$fleet$setWeekMonthNum()
#' 
#' 
#' #####################
#' ## Fishing Grounds ##
#' #####################
#' 
#' # Setup available data to identify fishing areas
#' yourSmartRstudy$setAvailData()
#' 
#' # Setup cluster analysis input
#' yourSmartRstudy$sampMap$setClusInpu()
#' 
#' # Run cluster analysis with the SKATER method
#' yourSmartRstudy$sampMap$calcFishGrou(numCuts = 3, minsize = 10,
#'  modeska = "S", skater_method = "manhattan", nei_queen = FALSE)
#' 
#' # Setup cluster plot with 3 clusters
#' yourSmartRstudy$sampMap$setCutResult(ind_clu = 3)
#' 
#' # Map of the clusters' configuration
#' print(yourSmartRstudy$sampMap$ggCutFGmap)
#' 



SmartProject <- R6Class("smartProject",
  portable = FALSE,
  class = TRUE,
  public = list(
    rawDataSurvey = NULL,
    yearInSurvey = NULL,
    specieInSurvey = NULL,
    surveyBySpecie = NULL,
    rawDataFishery = NULL,
    yearInFishery = NULL,
    specieInFishery = NULL,
    fisheryBySpecie = NULL,
    ggEffRaw = NULL,
    ggEffFish = NULL,
    ggEffGrid = NULL,
    ggEff = NULL,
    gooLstCoho = list(),
    sampMap = NULL,
    fleet = NULL,
    simProd = list(),
    simEffo = NULL,
    simBanFG = NULL,
    simSpatialCost = NULL,
    simEffortCost = NULL,
    simProdCost = NULL,
    simTotalCost = NULL,
    simRevenue = list(),
    simTotalRevenue = NULL,
    simCostRevenue = NULL,
    simResPlot = NULL,
    outGmat = NULL,
    outOptimEffo = NULL,
    outWeiProp = NULL,
    outWeiPropQ = NULL,
    assessData = list(),
    assSingleRes = list(),
    assSinglePlot = list(),
    assMultiRes = list(),
    assMultiPlot = list(),
    assessInteract = list(),
    setAssessInteract = function(intName, intType, intWho, intQty, intChi, intOm) {
      assessInteract <<- list()
      assessInteract$name <<- intName
      assessInteract$type <<- intType
      assessInteract$who <<- intWho
      assessInteract$qty <<- intQty
      assessInteract$chi <<- intChi
      assessInteract$om <<- intOm
      # assessInteract$per <<- intPer
    },
    setAssessData = function(specie, forecast = FALSE) {
      cat("\nSetup Assessment Data for", specie, "...\n")
      if (is.null(assessData[[specie]])) {
        assessData[[specie]] <<- list()
      }
      indSpeFis <- which(specieInFishery == specie)
      indSpeSur <- which(specieInSurvey == specie)

      cat("\n\tLoading time-scales... ")
      assessData[[specie]]$Amax <<- fisheryBySpecie[[indSpeFis]]$nCoho
      assessData[[specie]]$Yr1 <<- as.numeric(as.character(min(years(fisheryBySpecie[[indSpeFis]]$rawLFD$Date))))

      assessData[[specie]]$Yr2 <<- as.numeric(as.character(max(years(fisheryBySpecie[[indSpeFis]]$rawLFD$Date))))
      if (forecast) assessData[[specie]]$Yr2 <<- assessData[[specie]]$Yr2 + 1

      assessData[[specie]]$Nyear <<- assessData[[specie]]$Yr2 - assessData[[specie]]$Yr1 + 1

      assessData[[specie]]$Nlen <<- 2
      assessData[[specie]]$NCAL <<- 0
      assessData[[specie]]$Nsurvey <<- 1
      assessData[[specie]]$NSAA <<- assessData[[specie]]$Nyear
      assessData[[specie]]$NSAL <<- 0
      cat("Done!")

      ## Catch
      cat("\n\tLoading Catch Data... ")
      tmpDF <- aggregate(
        Production ~ Year, data.frame(
          Year = as.character(fleet$effoAllLoa$Year),
          Production = apply(fleet$predProd[[specie]], 1, sum),
          stringsAsFactors = FALSE
        ),
        sum
      )
      assessData[[specie]]$Catch <<- tmpDF$Production
      if (forecast) {
        if (is.null(simProd[[specie]])) stop("\nMissing Simulated Production!\n")
        assessData[[specie]]$Catch <<- c(assessData[[specie]]$Catch, sum(simProd[[specie]]))
      }

      ## Catch at Age
      for (sex in 1:length(names(fisheryBySpecie[[indSpeFis]]$groMixout))) {
        if (sex == 1) {
          tmpGro <- fisheryBySpecie[[indSpeFis]]$groMixout[[sex]][, c("FG", "Age", "Date")]
        } else {
          tmpGro <- rbind(tmpGro, fisheryBySpecie[[indSpeFis]]$groMixout[[sex]][, c("FG", "Age", "Date")])
        }
      }
      tmpGro$Season <- factor(quarters(tmpGro$Date + 30), levels = c("1Q", "2Q", "3Q", "4Q"), labels = c("winter", "spring", "summer", "fall"))
      tmpAstat <- ddply(tmpGro, .(FG, Age, Season), summarise, Freq = length(Age))
      tmpAstat <- tmpAstat[tmpAstat$Freq > 0, ]
      outWeiQ <- list()
      for (season in c("winter", "spring", "summer", "fall")) {
        preCAA <- data.frame(FG = 1:(sampMap$cutFG + 1))
        preCAA <- cbind(preCAA, setNames(lapply(0:(fisheryBySpecie[[indSpeFis]]$nCoho - 1), function(x) x <- NA), 0:(fisheryBySpecie[[indSpeFis]]$nCoho - 1)))
        for (i in 1:nrow(preCAA)) {
          tempRev <- tmpAstat[tmpAstat$FG == i & tmpAstat$Season == season, ]
          if (nrow(tempRev) > 0) {
            tempRev$propAge <- tempRev$Freq / sum(tempRev$Freq)
            outClass <- merge(data.frame(Age = 0:(fisheryBySpecie[[indSpeFis]]$nCoho - 1)), aggregate(formula = propAge ~ Age, data = tempRev, FUN = sum), all.x = TRUE)
            preCAA[i, 2:(fisheryBySpecie[[indSpeFis]]$nCoho + 1)] <- outClass$propAge
          }
        }
        outWeiQ[[season]] <- preCAA
      }
      outProp <- matrix(
        data = 0,
        nrow = nrow(fleet$predProd[[specie]]),
        ncol = fisheryBySpecie[[indSpeFis]]$nCoho
      )
      tmpSeason <- data.frame(
        Month = 1:12,
        Season = c(
          "winter", "winter", "spring",
          "spring", "spring", "summer",
          "summer", "summer", "fall",
          "fall", "fall", "winter"
        )
      )
      for (season in c("winter", "spring", "summer", "fall")) {
        tmpOutProp <- apply(fleet$predProd[[specie]][fleet$effoAllLoa$MonthNum %in% tmpSeason$Month[tmpSeason$Season == season], ], 1, function(x) apply(outWeiQ[[season]][, -1] * t(x), 2, sum, na.rm = TRUE))
        outProp[fleet$effoAllLoa$MonthNum %in% tmpSeason$Month[tmpSeason$Season == season], ] <- t(tmpOutProp)
      }
      outCAA <- aggregate(. ~ Year, data.frame(Year = fleet$effoAllLoa$Year, outProp), sum)

      assessData[[specie]]$YCAA <<- as.numeric(as.character(outCAA[, 1]))
      if (forecast) assessData[[specie]]$YCAA <<- c(assessData[[specie]]$YCAA, assessData[[specie]]$Yr2)

      assessData[[specie]]$CAA <<- outCAA[, -1] / apply(outCAA[, -1], 1, sum)
      if (forecast) {
        if (is.null(simProd[[specie]])) stop("\nMissing Simulated Production!\n")
        if (is.null(simEffo)) stop("\nMissing Simulated Effort!\n")
        simProp <- matrix(
          data = 0,
          nrow = nrow(simProd[[specie]]),
          ncol = fisheryBySpecie[[indSpeFis]]$nCoho
        )
        for (season in c("winter", "spring", "summer", "fall")) {
          tmpOutProp <- apply(simProd[[specie]][simEffo$MonthNum %in% tmpSeason$Month[tmpSeason$Season == season], ], 1, function(x) apply(outWeiQ[[season]][, -1] * t(x), 2, sum, na.rm = TRUE))
          simProp[simEffo$MonthNum %in% tmpSeason$Month[tmpSeason$Season == season], ] <- t(tmpOutProp)
        }
        simCAA <- aggregate(. ~ Year, data.frame(Year = simEffo$Year, simProp), sum)
        assessData[[specie]]$CAA <<- rbind(assessData[[specie]]$CAA, simCAA[, -1] / apply(simCAA[, -1], 1, sum))
      }

      assessData[[specie]]$NCAA <<- nrow(assessData[[specie]]$CAA)
      assessData[[specie]]$YCAL <<- 0
      assessData[[specie]]$CAL <<- matrix(0, ncol = assessData[[specie]]$Nlen, nrow = assessData[[specie]]$NCAL)
      cat("Done!")

      ## Survey at Age
      cat("\n\tLoading Survey Data... ")
      for (sex in 1:length(names(surveyBySpecie[[indSpeSur]]$groMixout))) {
        if (sex == 1) {
          tmpAL <- table(
            round(surveyBySpecie[[indSpeSur]]$groMixout[[sex]]$Length),
            factor(surveyBySpecie[[indSpeSur]]$groMixout[[sex]]$Age, levels = 0:surveyBySpecie[[indSpeSur]]$nCoho)
          )
          tmpAL <- round(tmpAL / apply(tmpAL, 1, sum), 2)
          tmpAlDf <- as.data.frame.matrix(tmpAL)
          tmpAlDf$Class <- rownames(tmpAL)
          tmpAbu <- aggregate(. ~ Class + Year, surveyBySpecie[[indSpeSur]]$abuAvg[, c("Class", "Year", paste0("wei", substr(names(surveyBySpecie[[indSpeSur]]$groMixout)[sex], 1, 3)))], sum)
          # tmpAbu <- aggregate(. ~ Class + Year, surveyBySpecie[[indSpeSur]]$abuAvg[,c("Class", "Year", names(surveyBySpecie[[indSpeSur]]$groMixout)[sex])], sum)
          tmpMem <- merge(tmpAbu, tmpAlDf)
          for (numCoh in 1:ncol(tmpAL)) {
            tmpMem[, 3 + numCoh] <- tmpMem[, 3 + numCoh] * tmpMem[, 3]
          }
          colnames(tmpMem)[3] <- "Num"
          outMem <- tmpMem
        } else {
          tmpAL <- table(
            round(surveyBySpecie[[indSpeSur]]$groMixout[[sex]]$Length),
            factor(surveyBySpecie[[indSpeSur]]$groMixout[[sex]]$Age, levels = 0:surveyBySpecie[[indSpeSur]]$nCoho)
          )
          tmpAL <- round(tmpAL / apply(tmpAL, 1, sum), 2)
          tmpAlDf <- as.data.frame.matrix(tmpAL)
          tmpAlDf$Class <- rownames(tmpAL)
          tmpAbu <- aggregate(. ~ Class + Year, surveyBySpecie[[indSpeSur]]$abuAvg[, c("Class", "Year", names(surveyBySpecie[[indSpeSur]]$groMixout)[sex])], sum)
          tmpMem <- merge(tmpAbu, tmpAlDf)
          for (numCoh in 1:ncol(tmpAL)) {
            tmpMem[, 3 + numCoh] <- tmpMem[, 3 + numCoh] * tmpMem[, 3]
          }
          colnames(tmpMem)[3] <- "Num"
          outMem <- rbind(outMem, tmpMem)
        }
      }
      outSAA <- aggregate(. ~ Year, outMem[, c(2, 4:(4 + (surveyBySpecie[[indSpeSur]]$nCoho - 1)))], sum)

      assessData[[specie]]$SAA <<- outSAA[, -1]
      if (forecast) assessData[[specie]]$SAA <<- rbind(assessData[[specie]]$SAA, assessData[[specie]]$SAA[nrow(assessData[[specie]]$SAA), ])

      assessData[[specie]]$YSAA <<- as.numeric(as.character(outSAA[, 1]))
      if (forecast) assessData[[specie]]$YSAA <<- c(assessData[[specie]]$YSAA, assessData[[specie]]$Yr2)

      assessData[[specie]]$SSAA <<- rep(1, nrow(assessData[[specie]]$SAA))
      assessData[[specie]]$SAL <<- matrix(0, ncol = assessData[[specie]]$Nlen, nrow = assessData[[specie]]$NSAL)
      assessData[[specie]]$YSAL <<- 0
      assessData[[specie]]$SSAL <<- 0
      assessData[[specie]]$SelsurvType <<- 1
      assessData[[specie]]$SelexType <<- 1
      cat("Done!")

      ### Mean Weight
      cat("\n\tLoading Weight and Growth Data... ")
      for (sex in 1:length(names(fisheryBySpecie[[indSpeFis]]$groMixout))) {
        if (sex == 1) {
          tmpWei <- fisheryBySpecie[[indSpeFis]]$groMixout[[sex]][, c("Age", "Weight")]
        } else {
          tmpWei <- rbind(tmpWei, fisheryBySpecie[[indSpeFis]]$groMixout[[sex]][, c("Age", "Weight")])
        }
      }
      tmpWei$Age <- factor(tmpWei$Age, levels = 0:(assessData[[specie]]$Amax - 1))

      assessData[[specie]]$WeightS <<- (ddply(tmpWei, .(Age), summarise, coh.mean = mean(Weight) / 1000, .drop = FALSE))[, 2]
      weightNaN <- which(is.nan(assessData[[specie]]$WeightS))
      if (length(weightNaN) > 0) {
        for (i in 1:length(weightNaN)) {
          if (weightNaN[i] == 1) {
            if (!is.na(assessData[[specie]]$WeightS[2])) {
              assessData[[specie]]$WeightS[1] <<- mean(c(0, assessData[[specie]]$WeightS[2]))
            } else {
              assessData[[specie]]$WeightS[1] <<- mean(assessData[[specie]]$WeightS[-1], na.rm = TRUE)
            }
            next
          }
          if (weightNaN[i] == length(assessData[[specie]]$WeightS)) {
            if (!is.na(assessData[[specie]]$WeightS[weightNaN[i] - 1])) {
              assessData[[specie]]$WeightS[weightNaN[i]] <<- assessData[[specie]]$WeightS[weightNaN[i] - 1]
            } else {
              assessData[[specie]]$WeightS[weightNaN[i]] <<- mean(assessData[[specie]]$WeightS[-(weightNaN[i])], na.rm = TRUE)
            }
            next
          }
          if (!is.na(assessData[[specie]]$WeightS[i - 1] & assessData[[specie]]$WeightS[i + 1])) {
            assessData[[specie]]$WeightS[i] <<- mean(c(assessData[[specie]]$WeightS[i - 1], assessData[[specie]]$WeightS[i + 1]))
          } else {
            assessData[[specie]]$WeightS[i] <<- mean(assessData[[specie]]$WeightS[-(weightNaN[i])], na.rm = TRUE)
          }
        }
      }

      assessData[[specie]]$WeightH <<- assessData[[specie]]$WeightS

      assessData[[specie]]$Qinit <<- 0
      assessData[[specie]]$InitN <<- rep(0, fisheryBySpecie[[indSpeFis]]$nCoho)
      assessData[[specie]]$InitR0 <<- 20

      ### Growth
      for (sex in 1:length(names(fisheryBySpecie[[indSpeFis]]$groPars))) {
        if (sex == 1) {
          tmpLHat <- fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$LHat
          tmpkHat <- fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$kHat
          tmpt0Hat <- fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$t0Hat
        } else {
          tmpLHat <- cbind(tmpLHat, fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$LHat)
          tmpkHat <- cbind(tmpkHat, fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$kHat)
          tmpt0Hat <- cbind(tmpt0Hat, fisheryBySpecie[[indSpeFis]]$groPars[[sex]]$t0Hat)
        }
      }
      GrowthAL <- c(
        mean(tmpLHat),
        mean(tmpkHat),
        -mean(tmpt0Hat),
        0.15,
        0.1
      )

      for (sex in 1:length(names(fisheryBySpecie[[indSpeFis]]$LWpar))) {
        if (sex == 1) {
          tmpAlpha <- fisheryBySpecie[[indSpeFis]]$LWpar[[sex]]$alpha
          tmpBeta <- fisheryBySpecie[[indSpeFis]]$LWpar[[sex]]$beta
        } else {
          tmpAlpha <- cbind(tmpAlpha, fisheryBySpecie[[indSpeFis]]$LWpar[[sex]]$alpha)
          tmpBeta <- cbind(tmpBeta, fisheryBySpecie[[indSpeFis]]$LWpar[[sex]]$beta)
        }
      }
      GrowthLW <- c(
        mean(tmpAlpha),
        mean(tmpBeta)
      )
      LenClassMax <- seq(from = assessData[[specie]]$Nlen, by = assessData[[specie]]$Nlen, length = assessData[[specie]]$Nlen)
      GetALK <- GetALKMW(GrowthAL[1], GrowthAL[2], GrowthAL[3], GrowthAL[4], GrowthAL[5], GrowthLW[1], GrowthLW[2], assessData[[specie]]$Amax, LenClassMax, 0.0)
      ALK1 <- GetALK$ALK
      GetALK <- GetALKMW(GrowthAL[1], GrowthAL[2], GrowthAL[3], GrowthAL[4], GrowthAL[5], GrowthLW[1], GrowthLW[2], assessData[[specie]]$Amax, LenClassMax, 0.5)
      ALK2 <- GetALK$ALK
      WeightLen <- GrowthLW[1] * GetALK$Lengths^GrowthLW[2] / 1000
      assessData[[specie]]$ALK1 <<- ALK1
      assessData[[specie]]$ALK2 <<- ALK2
      assessData[[specie]]$WeightLen <<- WeightLen
      SurvBio <- matrix(0, nrow = 10, ncol = assessData[[specie]]$Nyear)
      SurvBio[1, ] <- apply(t(assessData[[specie]]$SAA) * assessData[[specie]]$WeightH, 2, sum)
      assessData[[specie]]$SurvBio <<- SurvBio
      cat("Done!")

      # from GUI
      cat("\n\tLoading User Parameters... ")
      # assessData[[specie]]$M <<- c(2.3, 1.1, 0.8, 0.7)
      if (is.null(assessData[[specie]]$M)) {
        assessData[[specie]]$M <<- round(seq(from = 0.7, to = 0.3, length.out = assessData[[specie]]$Amax), 2)
      }
      # assessData[[specie]]$Mat <<- c(0.5, 1, 1, 1)
      if (is.null(assessData[[specie]]$Mat)) {
        assessData[[specie]]$Mat <<- round(seq(from = 0.2, to = 0.9, length.out = assessData[[specie]]$Amax), 2)
      }
      # assessData[[specie]]$Selex <<- c(0.1, 0.2, 0.6, 1)
      if (is.null(assessData[[specie]][["Selex"]])) {
        assessData[[specie]]$Selex <<- round(seq(from = 0.2, to = 0.9, length.out = assessData[[specie]]$Amax), 2)
      }
      if (is.null(assessData[[specie]]$SelexSurv)) {
        assessData[[specie]]$SelexSurv <<- matrix(0, nrow = 10, ncol = assessData[[specie]]$Amax)
      }
      assessData[[specie]]$SelexSurv[1, ] <<- round(seq(from = 0.5, to = 0.9, length.out = assessData[[specie]]$Amax), 2)
      # assessData[[specie]]$SelexSurv[1,] <<- c(0.2, 0.5, 1, 1)
      if (length(assessData[[specie]]$PropZBeforeMat) == 0) {
        assessData[[specie]]$PropZBeforeMat <<- 0.3
      }
      cat("Done!\n\nSetup Assessment Data for", specie, "Completed!\n")
    },
    assSingle = function(specie = "") {
      if (is.null(assessData[[specie]])) {
        stop("Missing Data! Run setAssesData() first.")
      }
      cat("\n\nAssessing ", specie, "\n", sep = "")
      assSingleRes[[specie]] <<- list()
      Nyear <- assessData[[specie]]$Nyear
      Amax <- assessData[[specie]]$Amax
      Nsurvey <- assessData[[specie]]$Nsurvey
      LogR0 <- assessData[[specie]]$InitR0
      RecDev <- rep(0, assessData[[specie]]$Nyear)
      Qinit <- rep(assessData[[specie]]$Qinit, Nsurvey)
      VecS <- NULL
      VecC <- NULL
      if (assessData[[specie]]$SelsurvType == 1) {
        VecS <- rep(0, Nsurvey * (Amax - 2))
        for (Isurv in 1:Nsurvey) {
          Offset <- (Amax - 2) * (Isurv - 1)
          for (Iage in 1:(Amax - 2)) {
            VecS[Offset + Iage] <- log((1 - assessData[[specie]]$SelexSurv[Isurv, Iage]) / assessData[[specie]]$SelexSurv[Isurv, Iage])
          }
        }
      }
      if (assessData[[specie]]$SelexType == 1) {
        VecC <- rep(0, Amax - 2)
        for (Iage in 1:(Amax - 2)) {
          VecC[Iage] <- log((1 - assessData[[specie]]$Selex[Iage]) / assessData[[specie]]$Selex[Iage])
        }
      }
      Fvals <- rep(0, Nyear)
      InitF <- log(1)
      Pars <- c(assessData[[specie]]$InitN, RecDev, LogR0, VecS, VecC, Fvals, InitF)
      Npar <- length(Pars)
      Res <- fit1Pars(Pars,
        fun1opt,
        FullMin = TRUE,
        DoVarCo = TRUE,
        SpeciesData = assessData[[specie]]
      )
      assSingleRes[[specie]] <<- fun1opt(Res$par,
        DoEst = FALSE,
        SpeciesData = assessData[[specie]]
      )
      assSingleRes[[specie]]$par <<- Res$par
      assSingleRes[[specie]]$VarCo <<- Res$VarCo
      assSingleRes[[specie]]$SSBSD <<- Res$SSBSD
      cat("\n\n", specie, " Assessment Complete!\n", sep = "")
    },
    assMulti = function() {
      if (is.null(assessData)) {
        stop("Missing Data! Run setAssesData() first.")
      }
      cat("\n\nMultiple Species Assessment\n", sep = "")
      assMultiRes <<- list()
      Nspecies <- length(assessData)
      Pars <- NULL
      for (Ispec in 1:Nspecies) {
        Qinit <- rep(assessData[[Ispec]]$Qinit, assessData[[Ispec]]$Nsurvey)
        Amax <- assessData[[Ispec]]$Amax
        Nsurvey <- assessData[[Ispec]]$Nsurvey
        Nyear <- assessData[[Ispec]]$Nyear
        InitN <- assessData[[Ispec]]$InitN
        RecDev <- rep(0, assessData[[Ispec]]$Nyear)
        LogR0 <- assessData[[Ispec]]$InitR0

        VecS <- NULL
        VecC <- NULL
        if (assessData[[Ispec]]$SelsurvType == 1) {
          VecS <- rep(0, Nsurvey * (Amax - 2))
          for (Isurv in 1:Nsurvey) {
            Offset <- (Amax - 2) * (Isurv - 1)
            for (Iage in 1:(Amax - 2)) VecS[Offset + Iage] <- log((1 - assessData[[Ispec]]$SelexSurv[Isurv, Iage]) / assessData[[Ispec]]$SelexSurv[Isurv, Iage])
          }
        }
        if (assessData[[Ispec]]$SelexType == 1) {
          VecC <- rep(0, Amax - 2)
          for (Iage in 1:(Amax - 2)) {
            VecC[Iage] <- log((1 - assessData[[Ispec]]$Selex[Iage]) / assessData[[Ispec]]$Selex[Iage])
          }
        }
        Fvals <- rep(0, Nyear)
        InitF <- log(1)
        Pars <- c(Pars, InitN, RecDev, LogR0, VecS, VecC, Fvals, InitF)
      }
      Npar <- length(Pars)
      Res <- fitNPars(Pars, funNopt,
        FullMin = TRUE, DoVarCo = TRUE,
        SpeciesData = assessData, Nspecies = Nspecies,
        PredationPars = assessInteract
      )

      assMultiRes <<- funNopt(Res$par,
        DoEst = FALSE, SpeciesData = assessData,
        Nspecies = Nspecies, PredationPars = assessInteract
      )
      assMultiRes$par <<- Res$par
      assMultiRes$VarCo <<- Res$VarCo
      assMultiRes$SSBSD <<- Res$SSBSD
      cat("\n\nAssessment Complete!\n", sep = "")
    },
    setPlotSingle = function(specie = "") {
      if (is.null(assSingleRes[[specie]])) {
        stop("\n\nMissing Results! Run Assessment First.")
      }
      assSinglePlot[[specie]] <<- list()

      ssbData <- data.frame(
        Year = 1:assessData[[specie]]$Nyear + assessData[[specie]]$Yr1 - 1,
        SSB = assSingleRes[[specie]]$SSB,
        Lower = assSingleRes[[specie]]$SSB - 1.96 * assSingleRes[[specie]]$SSBSD,
        Upper = assSingleRes[[specie]]$SSB + 1.96 * assSingleRes[[specie]]$SSBSD
      )

      assSinglePlot[[specie]]$SSB <<- ggplot_SSBsingle(choSpecie = specie, assData = ssbData)


      survData <- list()
      tmpObs <- assSingleRes[[specie]]$ObsSAA
      tmpObs$Year <- assSingleRes[[specie]]$YSAA
      survData$obsSAA <- melt(tmpObs,
        id.vars = "Year",
        variable.name = "Age", value.name = "Index"
      )
      survData$obsSAA$Lower <- survData$obsSAA$Index * exp(-1.96 * assSingleRes[[specie]]$CvIndex)
      survData$obsSAA$Upper <- survData$obsSAA$Index * exp(+1.96 * assSingleRes[[specie]]$CvIndex)
      levels(survData$obsSAA$Age) <- paste("Age", levels(survData$obsSAA$Age))

      tmpPred <- as.data.frame(assSingleRes[[specie]]$PredSAA)
      colnames(tmpPred) <- colnames(assSingleRes[[specie]]$ObsSAA)
      tmpPred$Year <- assSingleRes[[specie]]$YSAA
      survData$predSAA <- melt(tmpPred,
        id.vars = "Year",
        variable.name = "Age", value.name = "Index"
      )
      levels(survData$predSAA$Age) <- paste("Age", levels(survData$predSAA$Age))

      assSinglePlot[[specie]]$ObsPredSurv <<- ggplot_OPSsingle(choSpecie = specie, assData = survData)


      caaData <- data.frame(
        Age = 0:(assSingleRes[[specie]]$Amax - 1),
        obsCAA = apply(assSingleRes[[specie]]$ObsCAA, 2, sum),
        predCAA = apply(assSingleRes[[specie]]$PredCAA, 2, sum)
      )

      assSinglePlot[[specie]]$ObsPredCAA <<- ggplot_OPCsingle(choSpecie = specie, assData = caaData)


      catcData <- data.frame(
        Year = 1:assessData[[specie]]$Nyear + assessData[[specie]]$Yr1 - 1,
        Catch = assSingleRes[[specie]]$ObsCatch,
        Lower = assSingleRes[[specie]]$ObsCatch * exp(-1.96 * assSingleRes[[specie]]$CvCatch),
        Upper = assSingleRes[[specie]]$ObsCatch * exp(+1.96 * assSingleRes[[specie]]$CvCatch)
      )

      assSinglePlot[[specie]]$totCatc <<- ggplot_TCsingle(choSpecie = specie, assData = catcData)
    },
    setPlotMulti = function() {
      if (is.null(assMultiRes)) {
        stop("\n\nMissing Results! Run Assessment First.")
      }

      for (specie in 1:length(assessData)) {
        assMultiPlot[[names(assessData)[specie]]] <<- list()

        ssbData <- data.frame(
          Year = 1:assessData[[specie]]$Nyear + assessData[[specie]]$Yr1 - 1,
          SSB = assMultiRes$SSB[specie, ],
          Lower = assMultiRes$SSB[specie, ] - 1.96 * assMultiRes$SSBSD[seq(from = 1, to = length(assMultiRes$SSBSD), by = length(assessData)) + specie - 1],
          Upper = assMultiRes$SSB[specie, ] + 1.96 * assMultiRes$SSBSD[seq(from = 1, to = length(assMultiRes$SSBSD), by = length(assessData)) + specie - 1]
        )

        assMultiPlot[[names(assessData)[specie]]]$SSB <<- ggplot_SSBsingle(choSpecie = names(assessData)[specie], assData = ssbData)


        survData <- list()
        tmpObs <- as.data.frame(assMultiRes$ObsSAA[specie, 1:assessData[[specie]]$Nyear, 1:assessData[[specie]]$Amax])
        tmpObs$Year <- assMultiRes$YSAA[specie, 1:assessData[[specie]]$Nyear]
        survData$obsSAA <- melt(tmpObs,
          id.vars = "Year",
          variable.name = "Age", value.name = "Index"
        )
        survData$obsSAA$Lower <- survData$obsSAA$Index * exp(-1.96 * assMultiRes$CvIndex)
        survData$obsSAA$Upper <- survData$obsSAA$Index * exp(+1.96 * assMultiRes$CvIndex)
        levels(survData$obsSAA$Age) <- paste("Age", substr(levels(survData$obsSAA$Age), start = 2, stop = 2))

        tmpPred <- as.data.frame(assMultiRes$PredSAA[specie, 1:assessData[[specie]]$Nyear, 1:assessData[[specie]]$Amax])
        # colnames(tmpPred) <- colnames(assSingleRes[[specie]]$ObsSAA)
        tmpPred$Year <- assMultiRes$YSAA[specie, 1:assessData[[specie]]$Nyear]
        survData$predSAA <- melt(tmpPred,
          id.vars = "Year",
          variable.name = "Age", value.name = "Index"
        )
        levels(survData$predSAA$Age) <- paste("Age", substr(levels(survData$predSAA$Age), start = 2, stop = 2))

        assMultiPlot[[names(assessData)[specie]]]$ObsPredSurv <<- ggplot_OPSsingle(choSpecie = names(assessData)[specie], assData = survData)


        caaData <- data.frame(
          Age = 0:(assMultiRes$Amax[specie] - 1),
          obsCAA = apply(assMultiRes$ObsCAA[specie, 1:assessData[[specie]]$Nyear, 1:assessData[[specie]]$Amax], 2, sum),
          predCAA = apply(assMultiRes$PredCAA[specie, 1:assessData[[specie]]$Nyear, 1:assessData[[specie]]$Amax], 2, sum)
        )

        assMultiPlot[[names(assessData)[specie]]]$ObsPredCAA <<- ggplot_OPCsingle(choSpecie = names(assessData)[specie], assData = caaData)

        catcData <- data.frame(
          Year = 1:assessData[[specie]]$Nyear + assessData[[specie]]$Yr1 - 1,
          Catch = assMultiRes$ObsCatch[specie, ],
          Lower = assMultiRes$ObsCatch[specie, ] * exp(-1.96 * assMultiRes$CvCatch),
          Upper = assMultiRes$ObsCatch[specie, ] * exp(+1.96 * assMultiRes$CvCatch)
        )

        assMultiPlot[[names(assessData)[specie]]]$totCatc <<- ggplot_TCsingle(choSpecie = names(assessData)[specie], assData = catcData)
      }
    },
    setCostInput = function() {
      if (is.null(fleet$effortIndex)) stop("Missing Effort Index")
      if (is.null(fleet$daysAtSea)) stop("Missing Days at Sea Index")
      if (is.null(fleet$effoAllLoa)) stop("Missing Production Index")
      fleet$setInSpatial()
      fleet$setInEffort()
      setInProduction()
    },
    setInProduction = function() {
      tmp_Prod <- data.frame(
        Year = fleet$effoProdAllLoa$Year,
        I_NCEE = fleet$effoProdAllLoa$I_NCEE,
        MonthNum = fleet$effoProdAllLoa$MonthNum,
        Production = apply(fleet$effoProdAllLoa[, (4 + sampMap$cutFG + 1):ncol(fleet$effoProdAllLoa)], 1, sum)
      )
      agg_Prod <- aggregate(Production ~ I_NCEE + Year, tmp_Prod, sum)
      tmp_effoCost <- fleet$rawEconomy[, c("VessID", "Year", "ProductionCost")]
      fleet$inProductionReg <<- merge(
        x = tmp_effoCost, y = agg_Prod,
        by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year")
      )
    },
    setDaysAtSea = function() {
      cat("\nProcessing year: ", sep = "")
      for (year in names(fleet$rawEffort)) {
        cat(year, "... ", sep = "")
        tmp_vmsY <- fleet$rawEffort[[year]][, c("I_NCEE", "DATE", "MonthNum")]
        tmp_vmsY$DATE <- floor(tmp_vmsY$DATE)
        tmp_vmsY <- tmp_vmsY[!duplicated(tmp_vmsY), ]
        out_tbl <- table(I_NCEE = tmp_vmsY$I_NCEE, Month = tmp_vmsY$MonthNum)
        if (year == names(fleet$rawEffort)[1]) {
          tmp_days <- data.frame(Year = year, out_tbl)
        } else {
          tmp_days <- rbind(
            tmp_days,
            data.frame(Year = year, out_tbl)
          )
        }
      }
      fleet$daysAtSea <<- suppressWarnings(merge(tmp_days, data.frame(
        I_NCEE = as.numeric(substr(fleet$rawRegister$CFR, 4, nchar(fleet$rawRegister$CFR))),
        Loa = fleet$rawRegister$Loa,
        Kw = fleet$rawRegister$Power.Main
      ), all.x = TRUE))
      cat(" Completed!", sep = "")
    },
    setEffortIndex = function() {
      cat("\n\nComputing Effort Index... ", sep = "")
      if (is.null(sampMap$fgWeigDist)) stop("Missing Harbours Weighted Distance")
      if (is.null(sampMap$cutFG)) stop("Missing Fishing Grounds")
      if (is.null(fleet$effoAllLoa)) stop("Missing Effort-LOA data")
      tmp_ei <- apply(data.frame(mapply(
        `*`,
        fleet$effoAllLoa[, 4:(sampMap$cutFG + 4)],
        sampMap$fgWeigDist
      )), 1, sum)
      fleet$effortIndex <<- data.frame(fleet$effoAllLoa[, c(1:3, ncol(fleet$effoAllLoa))],
        EffInd = tmp_ei
      )
      cat("Completed!", sep = "")
    },
    setProductionIndex = function() {
      fleet$prodIndex <<- data.frame(
        Year = fleet$effoProdAllLoa$Year,
        I_NCEE = fleet$effoProdAllLoa$I_NCEE,
        MonthNum = fleet$effoProdAllLoa$MonthNum,
        Production = apply(fleet$effoProdAllLoa[, (4 + sampMap$cutFG + 1):ncol(fleet$effoProdAllLoa)], 1, sum)
      )
    },
    getHarbFgDist = function() {
      if (is.null(fleet$regHarbsUni)) stop("Missing Harbours Coordinates")
      cat("\nRunnnig Fishing ground average distances routine... ", cat = "")
      setRegHarbBox()
      setFgWeigDist()
      cat("\nFishing ground average distances routine completed!", cat = "")
    },
    setFgWeigDist = function() {
      cat("\n\tSetting Fishing ground weighted distances... ", cat = "")
      harb_fg_dist <- spDists(
        y = as.matrix(sampMap$cutResShpCent[, 1:2]),
        x = as.matrix(fleet$regHarbsBox[, 2:3]), longlat = TRUE
      )
      dimnames(harb_fg_dist) <- list(
        as.character(fleet$regHarbsBox$Name),
        paste("FG", sampMap$cutResShpCent$FG, sep = "")
      )
      harb_fg_dist <- data.frame(harb_fg_dist)
      harb_wei_dist <- numeric(length = ncol(harb_fg_dist))
      names(harb_wei_dist) <- names(harb_fg_dist)
      for (i in 1:ncol(harb_fg_dist)) {
        harb_wei_dist[i] <- weighted.mean(harb_fg_dist[, i], fleet$regHarbsBox$relFreq)
      }
      sampMap$fgWeigDist <<- harb_wei_dist[order(as.numeric(substr(names(harb_wei_dist), 3, nchar(names(harb_wei_dist)))))]
      cat(" OK!", cat = "")
    },
    setRegHarbBox = function() {
      cat("\n\tComputing Harbours-FishingGround distances...", cat = "")
      tmp_dist <- gDistance(sampMap$gridShp,
        SpatialPoints(fleet$regHarbsUni[, 2:3]),
        byid = TRUE
      )
      fleet$regHarbsUni$shpDist <<- apply(tmp_dist, 1, min)
      fleet$regHarbsBox <<- fleet$regHarbsUni[fleet$regHarbsUni$shpDist < 0.5, ]
      harb_cur_box <- as.data.frame(table(fleet$vmsRegister[fleet$vmsRegister$Port.Name %in% fleet$regHarbsBox$Name, ]$Port.Name))
      colnames(harb_cur_box) <- c("Name", "absFreq")
      harb_cur_box$relFreq <- harb_cur_box$absFreq / sum(harb_cur_box$absFreq)
      fleet$regHarbsBox <<- merge(fleet$regHarbsBox, harb_cur_box)
      cat(" OK!", cat = "")
    },
    loadSurveyLFD = function(csv_path) {
      cat("\nLoading survey data...", sep = "")
      rawDataSurvey <<- read.csv(file = csv_path, stringsAsFactors = FALSE, header = TRUE)
      surveyBySpecie <<- list()

      cat("\nSetting Years... ", sep = "")
      setYearSurvey()
      cat(" from ", min(yearInSurvey), " to ", max(yearInSurvey), "\nSetting Species... ", sep = "")
      setSpecieSurvey()
      cat(" found: ", paste(specieInSurvey, collapse = " - "), "\nSplitting Species...", sep = "")
      splitSpecieSurvey()

      if (!is.null(sampMap$cutResShp)) {
        addFg2Survey()
        setSpreaSurvey()
        setSpatSurvey()
        sampMap$set_ggMapFgSurvey(rawDataSurvey)
      }

      cat(" completed!", sep = "")
    },
    loadFisheryLFD = function(csv_path) {
      cat("\nLoading fishery data...", sep = "")
      rawDataFishery <<- read.csv(file = csv_path, stringsAsFactors = FALSE, header = TRUE)
      if (is.null(rawDataFishery$Unsex)) rawDataFishery$Unsex <<- rawDataFishery$Female + rawDataFishery$Male
      fisheryBySpecie <<- list()
      cat("\nSetting Years... ", sep = "")
      setYearFishery()
      cat(" from ", min(levels(yearInFishery)[as.numeric(yearInFishery)]), " to ", max(levels(yearInFishery)[as.numeric(yearInFishery)]), "\nSetting Species... ", sep = "")
      setSpecieFishery()
      cat(" found: ", paste(specieInFishery, collapse = " - "), "\nSplitting Species...", sep = "")
      splitSpecieFishery()

      if (!is.null(sampMap$cutResShp)) {
        addFg2Fishery()
        setSpreaFishery()
        setSpatFishery()
        sampMap$set_ggMapFgFishery(rawDataFishery)
      }

      cat(" completed!", sep = "")
    },
    setYearSurvey = function() {
      yearInSurvey <<- sort(unique(years(rawDataSurvey[, "Date"])), decreasing = FALSE)
    },
    setYearFishery = function() {
      yearInFishery <<- sort(unique(years(rawDataFishery[, "Date"])), decreasing = FALSE)
    },
    loadMap = function(map_path) {
      sampMap <<- SampleMap$new(map_path)
    },
    createFleet = function() {
      fleet <<- FishFleet$new()
    },
    importEnv = function(envLst) {
      sampMap <<- SampleMap$new()
      sampMap$gridName <<- envLst$gridName
      sampMap$gridShp <<- envLst$gridShp
      sampMap$nCells <<- envLst$nCells
      sampMap$gridPolySet <<- envLst$gridPolySet
      sampMap$gridFortify <<- envLst$gridFortify
      sampMap$griCent <<- envLst$griCent
      sampMap$gridBbox <<- envLst$gridBbox
      sampMap$gridBboxExt <<- envLst$gridBboxExt
      sampMap$gridBboxSP <<- envLst$gridBboxSP
      sampMap$gooMap <<- envLst$gooMap
      sampMap$gooMapPlot <<- envLst$gooMapPlot
      sampMap$plotRange <<- envLst$plotRange
      sampMap$gooGrid <<- envLst$gooGrid
      sampMap$gooBbox <<- envLst$gooBbox
      sampMap$gridBathy <<- envLst$gridBathy
      sampMap$centDept <<- envLst$centDept
      sampMap$ggDepth <<- envLst$ggDepth
      sampMap$bioDF <<- envLst$bioDF
      sampMap$ggBioDF <<- envLst$ggBioDF
    },
    setSpecieSurvey = function() {
      specieInSurvey <<- sort(unique(rawDataSurvey[, "Specie"]))
    },
    setSpecieFishery = function() {
      specieInFishery <<- sort(unique(rawDataFishery[, "Specie"]))
    },
    splitSpecieSurvey = function() {
      if (length(specieInSurvey) == 1) {
        addSpecieSurvey(rawDataSurvey)
      } else {
        for (i in 1:length(specieInSurvey)) {
          addSpecieSurvey(rawDataSurvey[rawDataSurvey[, "Specie"] == specieInSurvey[i], ])
        }
      }
    },
    splitSpecieFishery = function() {
      if (length(specieInFishery) == 1) {
        addSpecieFishery(rawDataFishery)
      } else {
        for (i in 1:length(specieInFishery)) {
          addSpecieFishery(rawDataFishery[rawDataFishery[, "Specie"] == specieInFishery[i], ])
        }
      }
    },
    addSpecieSurvey = function(sing_spe) {
      surveyBySpecie <<- c(surveyBySpecie, SurveyBySpecie$new(sing_spe))
    },
    addSpecieFishery = function(sing_spe) {
      fisheryBySpecie <<- c(fisheryBySpecie, FisheryBySpecie$new(sing_spe))
    },
    setSpreaFishery = function() {
      for (i in 1:length(fisheryBySpecie)) {
        fisheryBySpecie[[i]]$setSpreDistSing()
        fisheryBySpecie[[i]]$setAvailSex()
      }
    },
    setSpatFishery = function() {
      for (i in 1:length(fisheryBySpecie)) {
        fisheryBySpecie[[i]]$setSpatDistSing()
      }
    },
    setSpreaSurvey = function() {
      for (i in 1:length(surveyBySpecie)) {
        surveyBySpecie[[i]]$setSpreDistSing()
        surveyBySpecie[[i]]$setAvailSex()
      }
    },
    setSpatSurvey = function() {
      for (i in 1:length(surveyBySpecie)) {
        surveyBySpecie[[i]]$setSpatDistSing()
      }
    },
    setDepthSurvey = function() {
      cat("\n\nSetting depth of survey data:", sep = "")
      for (i in 1:length(surveyBySpecie)) {
        cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
        surveyBySpecie[[i]]$setDepth(bathyMatrix = sampMap$gridBathy)
        cat("Done!", sep = "")
      }
      cat("\nCompleted!\n", sep = "")
    },
    setStratumSurvey = function(vectorStrata = c(0, 10, 100, 1000, Inf)) {
      cat("\nSetting stratum of survey data:", sep = "")
      for (i in 1:length(surveyBySpecie)) {
        cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
        surveyBySpecie[[i]]$setStratum(vecStrata = vectorStrata)
        cat("Done!", sep = "")
      }
      cat("\nCompleted!\n", sep = "")
    },
    setAbuAvgAll = function() {
      cat("\nComputing average Number of individuals x Size x Stratum: ", sep = "")
      for (i in 1:length(surveyBySpecie)) {
        cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
        surveyBySpecie[[i]]$setAbuAvg()
        cat("Done!", sep = "")
      }
      cat("\nCompleted!\n", sep = "")
    },
    setMeditsIndex = function() {
      cat("\nComputing MEDITS index: ", sep = "")
      for (i in 1:length(surveyBySpecie)) {
        cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
        surveyBySpecie[[i]]$setIndSpe()
        cat("Done!", sep = "")
      }
      cat("\nCompleted!\n", sep = "")
    },
    setStrataAbu = function() {
      cat("\nComputing weighted Number of individuals x Size x Stratum: ", sep = "")
      for (i in 1:length(surveyBySpecie)) {
        cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
        surveyBySpecie[[i]]$abuAvg$weiFem <<- surveyBySpecie[[i]]$abuAvg$Female * sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
        surveyBySpecie[[i]]$abuAvg$weiMal <<- surveyBySpecie[[i]]$abuAvg$Male * sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
        surveyBySpecie[[i]]$abuAvg$weiUns <<- surveyBySpecie[[i]]$abuAvg$Unsex * sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
        cat("Done!", sep = "")
      }
      cat("\nCompleted!\n", sep = "")
    },
    # setLFDPopSurvey = function(){
    #   if(length(specieInSurvey) == 1){
    #     calcLFDPopSurvey(1)
    #   }else{
    #     for(i in 1:length(specieInSurvey)){
    #       calcLFDPopSurvey(i)
    #     }}
    #   # speDisPlot("All")
    # },
    # setLFDPopFishery = function(){
    #   if(length(specieInFishery) == 1){
    #     calcLFDPopFishery(1)
    #   }else{
    #     for(i in 1:length(specieInFishery)){
    #       calcLFDPopFishery(i)
    #     }}
    #   # speDisPlot("All")
    # },
    loadFleeEffoDbs = function(effort_path, met_nam, onBox = TRUE, perOnBox = 1) {
      cat("\n   ---   Extracting Effort data   ---", sep = "")
      sort_files <- sort(effort_path)
      fleet$rawEffort <<- list()
      for (i in sort_files) {
        cat("\n\nLoading db: ", i, sep = "")
        # cat("\nSelecting tracks in box...", sep = "")
        tmp_eff <- fn$sqldf("select * from (select * from (select *, rowid as i_id from intrp) join (select distinct I_NCEE, T_NUM from intrp where I_NCEE in (select distinct I_NCEE from nn_clas where met_des = '`met_nam`') and LON > `sampMap$gridBboxSP@bbox[1,1]` and LON < `sampMap$gridBboxSP@bbox[1,2]` and LAT > `sampMap$gridBboxSP@bbox[2,1]` and LAT < `sampMap$gridBboxSP@bbox[2,2]`) using (I_NCEE, T_NUM)) join (select * from p_depth) using (i_id)", dbname = i) ### Over in B-Box
        if (onBox) {
          in_box <- over(SpatialPoints(tmp_eff[, c("LON", "LAT")]), sampMap$gridBboxSP)
        } else {
          in_box <- over(
            SpatialPoints(tmp_eff[, c("LON", "LAT")]),
            unionSpatialPolygons(sampMap$gridShp,
              IDs = rep(
                1,
                length(sampMap$gridShp@polygons)
              )
            )
          )
        }
        cat("   -   Completed!", sep = "")

        in_box[is.na(in_box)] <- 0
        tmp_eff$in_box <- in_box
        in_box_ping <- sqldf("select I_NCEE, T_NUM, sum(in_box) from tmp_eff group by I_NCEE, T_NUM")
        all_ping <- sqldf("select I_NCEE, T_NUM, count(*) from tmp_eff group by I_NCEE, T_NUM")

        ### Selecting tracks with at least X points in the bounding box
        if (perOnBox > 100) perOnBox <- 1
        if (perOnBox > 1) perOnBox <- perOnBox / 100
        cat("\nLoading tracks with at least ", perOnBox * 100, "% of points in the bounding box...", sep = "")
        perOnInd <- in_box_ping[, 3] / all_ping[, 3] >= perOnBox
        if (sum(perOnInd) == 0) {
          cat("\nNo tracks with ", perOnBox * 100, "% of points in the bounding box.\nNo tracking data loaded!", sep = "")
        } else {
          all_in_box <- in_box_ping[perOnInd, 1:2]
          all_sos <- sqldf("select * from tmp_eff join (select * from all_in_box) using (I_NCEE, T_NUM)")
          cat("\nSaving Data", sep = "")
          numYea <- as.character(sort(unique(years(all_sos$DATE))))
          for (yea in 1:length(numYea)) {
            if (is.null(fleet$rawEffort[[numYea[yea]]])) {
              fleet$rawEffort[[numYea[yea]]] <<- all_sos[years(all_sos$DATE) == numYea[yea], ]
            } else {
              fleet$rawEffort[[numYea[yea]]] <<- rbind(fleet$rawEffort[[numYea[yea]]], all_sos[years(all_sos$DATE) == numYea[yea], ])
            }
          }
        }
      }
    },
    # effPlot = function(whichYear){
    #   if(whichYear == "All"){
    #     all_sum <- apply(fleet$rawEffort, 1, sum)
    #     round_perc <- 1+round(100*all_sum/max(all_sum))
    #     # col_palette <- rev(heat.colors(100))
    #     # cell_colors <- col_palette[round_perc]
    #     distrPlotCols(cols = rev(heat.colors(101)), vals = round_perc,
    #                   maxVal = ceiling(max(all_sum)),
    #                   plotTitle = "Effort all years",
    #                   legendUnits = "Hours")
    #
    #   }else{
    #     num_col <- which(colnames(fleet$rawEffort) == whichYear)
    #
    #     yea_eff <- round(fleet$rawEffort[,num_col])
    #     round_yea <- 1+100*yea_eff/max(yea_eff)
    #
    #     distrPlotCols(cols = rev(heat.colors(101)), vals = round_yea,
    #                   maxVal = ceiling(max(yea_eff)),
    #                   plotTitle = paste("Effort ", whichYear, sep = ""),
    #                   legendUnits = "Hours")
    #   }
    # },
    speDisPlot = function(whoPlo) {
      if (whoPlo == "All") {
        sampMap$plotSamMap("All species")
        for (i in 1:length(specieInSurvey)) {
          points(surveyBySpecie[[i]]$rawLFD[, c("Lon", "Lat")], pch = 20, col = 1 + i, cex = 0.4)
        }
      } else {
        sampMap$plotSamMap(whoPlo)
        points(surveyBySpecie[[which(specieInSurvey == whoPlo)]]$rawLFD[, c("Lon", "Lat")], pch = 20, col = 1 + which(specieInSurvey == whoPlo), cex = 0.4)
      }
    },
    plotGooSpe = function(whiSpe, whiSou) {
      if (whiSou == "Survey") {
        if (whiSpe == "All") {
          tmp_data <- unique(rawDataSurvey[, c("Specie", "Lat", "Lon")])
        } else {
          tmp_data <- unique(rawDataSurvey[which(rawDataSurvey$Specie == whiSpe), c("Specie", "Lat", "Lon")])
        }
        # levels(tmp_data[,1]) <- unique(rawDataSurvey[,"Specie"])

        sampMap$plotGooSpeSur(tmp_data)
      } else {
        if (whiSpe == "All") {
          tmp_data <- unique(rawDataFishery[, c("Specie", "Lat", "Lon")])
        } else {
          tmp_data <- unique(rawDataFishery[which(rawDataFishery$Specie == whiSpe), c("Specie", "Lat", "Lon")])
        }
        # levels(tmp_data[,1]) <- unique(rawDataFishery[,"Specie"])

        sampMap$plotGooSpeFis(tmp_data)
      }
    },
    setGooPlotCohoFish = function(specie = "", sex = "Female", speCol = "", smooPoi = 500, smooBin = 0.5) {
      cat("\n\nProcessing ", specie, " - ", sex, "... Cohort ", sep = "")
      gooLstCoho[[specie]] <<- list()
      gooLstCoho[[specie]][[sex]] <<- list()

      tmpMix <- fisheryBySpecie[[which(specieInFishery == specie)]]$groMixout[[sex]]
      ageFGtbl <- table(tmpMix$FG, tmpMix$Age)
      cohAbuFG <- as.data.frame(cbind(FG = as.numeric(rownames(ageFGtbl)), ageFGtbl))
      outPalette <- rainbow(ncol(cohAbuFG) - 1)

      for (coh_i in 2:ncol(cohAbuFG)) {
        cat(colnames(ageFGtbl)[coh_i - 1], "... ", sep = "")

        if (speCol == "") {
          cohFillPal <- outPalette[coh_i - 1]
        } else {
          cohFillPal <- speCol
        }
        all_cell <- merge(x = sampMap$cutResShpFort$id, data.frame(x = cohAbuFG$FG, y = round(100 * cohAbuFG[, coh_i] / max(cohAbuFG[, coh_i]))), all = TRUE)
        grid_data <- cbind(sampMap$cutResShpFort, NumInd = all_cell[, 2])


        if (length(sampMap$cutFG) > 0) {
          if (length(sampMap$gridShp@polygons) == (sampMap$cutFG + 1)) {
            tmp_coo <- data.frame(coordinates(sampMap$gridShp), cell_id = 1:length(sampMap$gridShp))
            colnames(tmp_coo) <- c("Lon", "Lat", "FG")
          } else {
            tmp_coo <- sampMap$cutResShpCent
          }
        } else {
          tmp_coo <- sampMap$cutResShpCent
        }

        tmp_dens <- data.frame(
          lon = rep(grid_data[!is.na(grid_data$NumInd), ]$long, grid_data[!is.na(grid_data$NumInd), ]$NumInd),
          lat = rep(grid_data[!is.na(grid_data$NumInd), ]$lat, grid_data[!is.na(grid_data$NumInd), ]$NumInd)
        )

        mapCoho <- suppressMessages(sampMap$gooMapPlot +
          geom_polygon(aes(x = long, y = lat, group = group, fill = NumInd),
            colour = "black", size = 0.1, data = grid_data, alpha = 0.8
          ) +
          scale_fill_gradient(paste0("Max abundance\n", max(cohAbuFG[, coh_i]), " specimens"),
            low = "Grey85", high = cohFillPal,
            # trans = "log10",
            breaks = pretty(1:100, 5), limits = c(1, 100)
          ) +
          ggtitle(paste0("Spatial Distribution of ", specie, " - Cohort ", coh_i - 2)) +
          geom_text(aes(label = FG, x = Lon, y = Lat),
            data = tmp_coo, size = 2
          ) +
          theme(
            legend.position = c(0.1, 0.22),
            legend.text = element_text(size = 10, colour = "grey19"),
            # legend.title = element_blank(),
            legend.background = element_rect(fill = rgb(1, 1, 1, 0.5)),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_text(size = 12),
            axis.text.y = element_text(size = 10),
            legend.key.size = unit(0.75, "cm"),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 20)
          ) +
          stat_density_2d(
            data = tmp_dens,
            mapping = aes(x = lon, y = lat, alpha = ..level..),
            fill = cohFillPal,
            geom = "polygon",
            n = smooPoi,
            h = smooBin,
            show.legend = FALSE
          ) +
          scale_alpha_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.025)) +
          geom_polygon(aes(x = long, y = lat, group = group, alpha = 0.1),
            colour = "black", size = 0.1, data = grid_data, alpha = 0.8, fill = NA
          ) +
          geom_text(aes(label = FG, x = Lon, y = Lat),
            data = tmp_coo, size = 2
          ))

        gooLstCoho[[specie]][[sex]][[colnames(ageFGtbl)[coh_i - 1]]] <<- mapCoho
      }
      cat(" Completed!", sep = "")
    },
    distrPlotCols = function(cols = NULL, vals = NULL, maxVal = 100,
                                 plotTitle = "NoTitle", legendUnits = "NoUnits") {
      def.par <- par(no.readonly = TRUE)
      par(mar = c(2.5, 2.5, 3, 1))
      layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(6, 1))
      sampMap$plotSamMap(title = plotTitle, celCol = cols[vals])
      par(mar = c(5, 3, 5, 1))
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), bty = "n", axes = FALSE, ann = FALSE, main = "Hours")
      rect(0.25, seq(0.2, 0.79, length.out = 100),
        0.55, seq(0.21, 0.80, length.out = 100),
        col = cols, border = rainbow(1, alpha = 0.01)
      )
      mtext(ceiling(seq(from = 0, to = maxVal, length.out = 10)),
        side = 2,
        at = seq(0.21, 0.80, length.out = 10), las = 2, cex = 1
      )
      text(legendUnits, x = 0.25, y = 0.15)
      par(def.par)
    },
    ggplotRawPoints = function(year) {
      tmp_dat <- fleet$rawEffort[[year]][
        sample(1:nrow(fleet$rawEffort[[year]]), min(c(50000, nrow(fleet$rawEffort[[year]])))),
        c("LON", "LAT", "W_HARB")
      ]
      tmp_dat$Status <- factor(tmp_dat$W_HARB,
        levels = c("0", "1"),
        labels = c("At sea", "In harbour")
      )
      ggEffRaw <<- suppressMessages(sampMap$gooMapPlot +
        geom_point(
          data = tmp_dat,
          aes(x = LON, y = LAT, shape = Status, color = Status),
          size = 0.6, alpha = 0.3
        ) +
        geom_point(
          data = subset(tmp_dat, Status == "In harbour"),
          aes(x = LON, y = LAT, shape = Status, color = Status),
          size = 0.6, alpha = 0.3
        ) +
        scale_colour_manual(values = c("coral", "darkseagreen1")) +
        guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
        ggtitle(paste("Sample raw points - ", year, sep = "")) +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    setGgEff = function() {
      if (is.null(ggEffRaw)) {
        tmp_raw <- ggplot() + geom_blank() + ggtitle("Raw Effort Not Loaded Yet")
      } else {
        tmp_raw <- ggEffRaw
      }
      if (is.null(ggEffFish)) {
        tmp_fish <- ggplot() + geom_blank() + ggtitle("Fishing Point Not Loaded Yet")
      } else {
        tmp_fish <- ggEffFish
      }
      if (is.null(ggEffGrid)) {
        tmp_grid <- ggplot() + geom_blank() + ggtitle("Gridded Effort Not Loaded Yet")
      } else {
        tmp_grid <- ggEffGrid
      }
      ggEff <<- suppressWarnings(grid.arrange(tmp_raw,
        tmp_fish,
        tmp_grid,
        layout_matrix = matrix(1:3, 1, 3)
      ))
    },
    plotGgEff = function() {
      suppressWarnings(grid.draw(ggEff))
    },
    ggplotFgWeigDists = function() {
      all_cell <- merge(
        x = sampMap$cutResShpFort$id,
        data.frame(
          x = as.numeric(substr(
            names(sampMap$fgWeigDist), 3,
            nchar(names(sampMap$fgWeigDist))
          )),
          y = sampMap$fgWeigDist
        ), all = TRUE
      )
      all_cell[is.na(all_cell)] <- 0
      grid_data <- cbind(sampMap$cutResShpFort, DistAvg = all_cell[, 2])

      if (length(sampMap$cutFG) > 0) {
        if (length(sampMap$gridShp@polygons) == (sampMap$cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(sampMap$gridShp), cell_id = 1:length(sampMap$gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- sampMap$cutResShpCent
        }
      } else {
        tmp_coo <- sampMap$cutResShpCent
      }

      suppressWarnings(
        print(
          suppressMessages(
            sampMap$gooMapPlot +
              geom_polygon(aes(
                x = long, y = lat,
                group = group, fill = DistAvg
              ),
              colour = "black", size = 0.1,
              data = grid_data, alpha = 0.8
              ) +
              scale_fill_gradient("Weighted\nDistance", low = "snow1", high = "orange1") +
              geom_text(aes(label = FG, x = Lon, y = Lat),
                data = tmp_coo, size = 2
              ) +
              ggtitle("Average Distance x Fishing Ground") +
              xlab("Longitude") + ylab("Latitude") +
              geom_point(
                data = fleet$regHarbsBox,
                mapping = aes(x = Lon, y = Lat, size = absFreq),
                fill = NA, color = "tomato3", shape = 21
              ) +
              scale_size_continuous("Number of\nVessels",
                breaks = pretty(fleet$regHarbsBox$absFreq, 5),
                range = c(1, 15)
              ) +
              geom_label_repel(
                data = fleet$regHarbsBox, mapping = aes(x = Lon, y = Lat, label = Name),
                size = 3, nudge_x = 0.1, nudge_y = 0.1, color = "grey3", fill = "grey89"
              ) +
              theme_tufte(base_size = 14, ticks = T) +
              theme(
                legend.position = "bottom",
                axis.text.x = element_text(size = 8),
                axis.title.x = element_text(size = 10),
                panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                axis.text.y = element_text(size = 8),
                axis.title.y = element_text(size = 10)
              )
          )
        )
      )
    },
    setAvailData = function() {
      sampMap$availData <<- character(0)
      sampMap$rawInpu <<- list()

      cat("\n\nLoading available data:\n")
      if (is.null(sampMap$bioDF)) {
        stop("\nMissing Seabed Data!\n")
      } else {
        sampMap$availData <<- c(sampMap$availData, "Seabed")
        cat("\n   -   Seabed Category")
        sampMap$rawInpu <<- c(sampMap$rawInpu, Seabed = list(data.frame(sampMap$bioDF)))
        cat("\t\t-   Loaded!")
      }

      if (is.null(fleet$rawEffort)) {
        stop("\nMissing Effort Data!\n")
      } else {
        sampMap$availData <<- c(sampMap$availData, "Effort")
        cat("\n   -   Effort Distribution")
        raw_effort <- numeric(length = sampMap$nCells)
        for (i in names(fleet$rawEffort)) {
          tmp_effo <- as.data.frame(table(fleet$rawEffort[[i]]$Cell[which(fleet$rawEffort[[i]]$FishPoint)]))
          names(tmp_effo) <- c("Cell", "Freq")
          tmp_effo$Cell <- as.numeric(as.character(tmp_effo$Cell))
          miss_rows <- as.numeric(setdiff(as.character(sampMap$gridShp@plotOrder), as.character(tmp_effo$Cell)))
          if (length(miss_rows) > 0) {
            tmp_effo <- rbind(tmp_effo, data.frame(Cell = miss_rows, Freq = 0))
            tmp_effo <- tmp_effo[order(tmp_effo[, 1]), ]
          }
          raw_effort <- cbind(raw_effort, tmp_effo[, 2])
          colnames(raw_effort)[ncol(raw_effort)] <- paste("Year_", i, sep = "")
        }
        sampMap$rawInpu <<- c(sampMap$rawInpu, Effort = list(raw_effort[, -1]))
        cat("\t-   Loaded!")
      }

      if (is.null(sampMap$griCent)) {
        stop("\nMissing Depth Data!\n")
      } else {
        sampMap$availData <<- c(sampMap$availData, "Depth")
        cat("\n   -   Cell Depth")
        sampMap$rawInpu <<- c(sampMap$rawInpu, Depth = list(sampMap$centDept[, 3]))
        cat("\t\t-   Loaded!\n")
      }
    },
    predictProduction = function(specie) {
      Prod <- matrix(data = NA, nrow(fleet$effoAllLoa), ncol = sampMap$cutFG + 1)
      lyears <- sort(as.numeric(as.character(unique(fleet$effoAllLoa$Year))))
      thrZero <- mean(fleet$effoProdAllLoa[, specie][fleet$effoProdAllLoa[, specie] < fleet$specSett[[specie]]$threshold & fleet$effoProdAllLoa[, specie] > 0])
      fgClms <- which(colnames(fleet$effoAllLoa) %in% as.character(seq(1, sampMap$cutFG + 1)))
      datalog <- fleet$effoAllLoa
      datalog$MonthNum <- as.factor(datalog$MonthNum)
      datalog$Year <- as.factor(datalog$Year)
      if (fleet$specLogit[[specie]]$logit$Name == "GLM") {
        infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "response") > fleet$specLogit[[specie]]$logit$Cut)
      } else {
        infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "prob")[, 2] > fleet$specLogit[[specie]]$logit$Cut)
      }
      # infish <- which(predict(fleet$specLogit[[specie]]$logit$logit_f, datalog, type="response") > fleet$specLogit[[specie]]$optCut)
      for (i in 1:length(infish)) {
        idata <- as.numeric(fleet$effoAllLoa[infish[i], fgClms])
        iloa <- as.numeric(fleet$effoAllLoa[infish[i], "Loa"])
        iy <- which(lyears == fleet$effoAllLoa[infish[i], "Year"])
        im <- as.numeric(as.character(fleet$effoAllLoa[infish[i], "MonthNum"]))
        ib <- fleet$resNNLS[[specie]]$bmat[which((fleet$resNNLS[[specie]]$SceMat$YEAR == iy) & (fleet$resNNLS[[specie]]$SceMat$MONTH == im)), ]
        # Prod[infish[i]] <- sum(ib * idata * iloa) + mean(fleet$effoProdAllLoa[,specie][fleet$effoProdAllLoa[,specie] < fleet$specSett[[specie]]$threshold & fleet$effoProdAllLoa[,specie] > 0])
        if (sum(ib * idata) > 0) {
          Prod[infish[i], ] <- (ib * idata * iloa) + ((ib * idata) / sum(ib * idata)) * thrZero
        }
      }
      Prod[is.na(Prod)] <- 0
      colnames(Prod) <- paste("PR_", as.character(seq(1, ncol(Prod))), sep = "")
      fleet$predProd[[specie]] <<- Prod
    },
    simProdAll = function(selRow = numeric(0)) {
      if (length(selRow) == 0) {
        selRow <- 1:nrow(simEffo)
      }

      lyears <- sort(as.numeric(as.character(unique(fleet$effoAllLoa$Year))))
      datalog <- simEffo[selRow, ]
      datalog$MonthNum <- as.factor(datalog$MonthNum)
      datalog$Year <- as.factor(datalog$Year)
      fgClms <- which(colnames(simEffo) %in% as.character(seq(1, sampMap$cutFG + 1)))

      for (specie in names(fleet$specLogit)) {
        Prod <- matrix(data = NA, length(selRow), ncol = sampMap$cutFG + 1)
        thrZero <- mean(fleet$effoProdAllLoa[, specie][fleet$effoProdAllLoa[, specie] < fleet$specSett[[specie]]$threshold & fleet$effoProdAllLoa[, specie] > 0])
        if (fleet$specLogit[[specie]]$logit$Name == "GLM") {
          infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "response") > fleet$specLogit[[specie]]$logit$Cut)
        } else {
          infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "prob")[, 2] > fleet$specLogit[[specie]]$logit$Cut)
        }
        for (i in 1:length(infish)) {
          idata <- as.numeric(simEffo[selRow, ][infish[i], fgClms])
          iloa <- as.numeric(simEffo[selRow, ][infish[i], "Loa"])
          iy <- which(lyears == simEffo[selRow, ][infish[i], "Year"])
          im <- as.numeric(as.character(simEffo[selRow, ][infish[i], "MonthNum"]))
          ib <- fleet$resNNLS[[specie]]$bmat[which((fleet$resNNLS[[specie]]$SceMat$YEAR == iy) & (fleet$resNNLS[[specie]]$SceMat$MONTH == im)), ]
          if (sum(ib * idata) > 0) {
            Prod[infish[i], ] <- (ib * idata * iloa) + ((ib * idata) / sum(ib * idata)) * thrZero
          }
        }
        Prod[is.na(Prod)] <- 0
        colnames(Prod) <- paste("PR_", as.character(seq(1, ncol(Prod))), sep = "")
        if (length(selRow) == nrow(simEffo)) {
          simProd[[specie]] <<- Prod
        } else {
          simProd[[specie]][selRow, ] <<- Prod
        }
      }
    },
    genSimEffo = function(selRow = numeric(0), areaBan = numeric(0)) {
      if (is.null(simEffo)) {
        simEffo <<- fleet$effoAllLoa[fleet$effoAllLoa$Year == max(as.numeric(as.character(unique(fleet$effoAllLoa$Year)))), ]
      } else {

        if (length(selRow) == 0) {
          selRow <- 1:nrow(simEffo)
        }

        obsZero <- apply(simEffo[selRow, 4:(ncol(simEffo) - 1)], 2, sum)
        posZero <- which(obsZero == 0)
        lenPosZero <- length(posZero)
        lenAreaBan <- length(areaBan)
        
        if (lenPosZero == 0 & lenAreaBan == 0) {
          selMode <- "flat"
        } else if (lenPosZero == 0 & lenAreaBan > 0) {
          selMode <- "ban"
        } else if (lenPosZero > 0 & lenAreaBan == 0) {
          selMode <- "zero"
          areaBan <- posZero
        } else if (lenPosZero > 0 & lenAreaBan > 0) {
          selMode <- "zeroBan"
          areaBan <- intersect(areaBan, posZero)
        }
        
        simEffo[selRow, 4:(ncol(simEffo) - 1)] <<- switch(selMode,
          flat = {
            t(apply(simEffo[selRow, 4:(ncol(simEffo) - 1)], 1, function(x) genFlatEffo(effoPatt = x)))
          },
          zero = {
            t(apply(simEffo[selRow, 4:(ncol(simEffo) - 1)], 1, function(x) genBanEffo(effoPatt = x, set0 = areaBan)))
          },
          zeroBan = {
            t(apply(simEffo[selRow, 4:(ncol(simEffo) - 1)], 1, function(x) genBanEffo(effoPatt = x, set0 = areaBan)))
          },
          ban = {
            t(apply(simEffo[selRow, 4:(ncol(simEffo) - 1)], 1, function(x) genBanEffo(effoPatt = x, set0 = areaBan)))
          }
        )
      }
    },
    getSimSpatialCost = function() {
      tmp_ei <- apply(data.frame(mapply(`*`, simEffo[, 4:(ncol(simEffo) - 1)], sampMap$fgWeigDist)), 1, sum)
      tmpIndex <- data.frame(simEffo[, c(1:3, ncol(simEffo))], EffInd = tmp_ei)
      simSpatialIndex <- aggregate(EffInd ~ I_NCEE + Year + Loa, tmpIndex, sum)
      predSpatCost <- predict(fleet$outSpatialReg, simSpatialIndex)
      simSpatialCost <<- cbind(simSpatialIndex, predSpatCost)
    },
    getSimEffortCost = function() {
      effortIndex <- aggregate(Freq ~ I_NCEE + Year + Loa + Kw, fleet$daysAtSea[fleet$daysAtSea$Year == max(as.numeric(as.character(fleet$daysAtSea$Year))), ], sum)
      predEffoCost <- predict(fleet$outEffortReg, effortIndex)
      simEffortCost <<- cbind(effortIndex, predEffoCost)
    },
    getSimProdCost = function() {
      outProd <- numeric(nrow(simProd[[1]]))
      for (specie in names(simProd)) {
        outProd <- outProd + apply(simProd[[specie]], 1, sum)
      }
      tmp_Prod <- data.frame(
        Year = simEffo$Year,
        I_NCEE = simEffo$I_NCEE,
        MonthNum = simEffo$MonthNum,
        Production = outProd
      )
      agg_ProdInd <- aggregate(Production ~ I_NCEE + Year, tmp_Prod, sum)
      predProdCost <- predict(fleet$outProductionReg, agg_ProdInd)
      simProdCost <<- cbind(agg_ProdInd, predProdCost = predProdCost)
    },
    getSimTotalCost = function() {
      getSimSpatialCost()
      getSimEffortCost()
      getSimProdCost()

      tmp_out_costs <- merge(merge(simEffortCost, simSpatialCost), simProdCost)
      out_costs <- tmp_out_costs[, c("I_NCEE", "Year", "Loa", "predEffoCost", "predSpatCost", "predProdCost")]
      out_costs$totCost <- out_costs$predEffoCost + out_costs$predSpatCost + out_costs$predProdCost

      simTotalCost <<- out_costs
    },
    getSimRevenue = function(selRow = numeric(0), timeScale = "Year") {
      if (length(selRow) == 0) {
        selRow <- 1:nrow(simEffo)
      }

      # simTotalRevenue <- data.frame(matrix(NA, nrow = nrow(simEffo), ncol = length(names(simProd))))
      speNam <- names(simProd)
      tmp_Revenue <- cbind(simEffo[, 1:3], (matrix(NA, nrow = nrow(simEffo[, ]), ncol = length(speNam))))
      names(tmp_Revenue)[4:(4 + length(speNam) - 1)] <- speNam
      for (specie in speNam) {
        if (timeScale == "Year") {
          tmpRev <- getFleetRevenue(
            predProd = simProd[[specie]][selRow, ],
            lwStat = outWeiProp[[specie]][, -1],
            priceVec = fleet$ecoPrice[[specie]]$Price
          )
        } else {
          tmpRev <- getFleetRevSeason(
            predProd = simProd[[specie]][selRow, ],
            monthVec = tmp_Revenue$MonthNum[selRow],
            lwStat = outWeiPropQ[[specie]],
            priceVec = fleet$ecoPrice[[specie]]$Price
          )
        }
        if (length(selRow) == nrow(simEffo)) {
          simRevenue[[specie]] <<- tmpRev
        } else {
          simRevenue[[specie]][selRow, ] <<- tmpRev
        }
        tmp_Revenue[, specie] <- apply(simRevenue[[specie]], 1, sum, na.rm = TRUE)
      }
      if (ncol(tmp_Revenue) == 4) {
        tmp_Revenue$totRevenue <- tmp_Revenue[, 4]
      } else {
        tmp_Revenue$totRevenue <- apply(tmp_Revenue[, 4:(4 + length(speNam) - 1)], 1, sum)
      }
      simTotalRevenue <<- aggregate(totRevenue ~ I_NCEE + Year, tmp_Revenue, sum)
    },
    getLWstat = function() {
      if (is.null(fisheryBySpecie)) {
        stop("No mcmc output found")
      }
      specList <- intersect(
        specieInFishery,
        names(fleet$ecoPrice)
      )

      for (specie in specList) {
        priIdx <- which(names(fleet$ecoPrice) == specie)
        fisIdx <- which(specieInFishery == specie)
        if (is.null(fleet$ecoPrice[[priIdx]])) {
          stop(paste0("No size/price data found for specie ", names(fleet$ecoPrice)[priIdx]))
        }

        vecSize <- sort(unique(c(fleet$ecoPrice[[priIdx]]$LowerBound, fleet$ecoPrice[[priIdx]]$UpperBound)))
        curUnit <- unique(fleet$ecoPrice[[priIdx]]$Units)[1]

        fisheryBySpecie[[fisIdx]]$setLWstat(lwUnit = curUnit)
        fgNames <- paste0("LW_", 1:(sampMap$cutFG + 1))

        ## yearly
        preRevenue <- data.frame(FG = 1:length(fgNames))
        preRevenue <- cbind(preRevenue, setNames(lapply(1:(length(vecSize) - 1), function(x) x <- NA), 1:(length(vecSize) - 1)))
        for (i in 1:nrow(preRevenue)) {
          tempRev <- fisheryBySpecie[[fisIdx]]$LWstat[fisheryBySpecie[[fisIdx]]$LWstat$FG == i, ]
          if (nrow(tempRev) > 0) {
            if (curUnit == "Length") {
              tempRev$propWei <- tempRev$relAbb / sum(tempRev$relAbb)
              tempRev$SizeClass <- factor(findInterval(x = tempRev$avgLen, vec = vecSize, all.inside = TRUE), levels = 1:(length(vecSize) - 1))
              outClass <- merge(data.frame(SizeClass = levels(tempRev$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tempRev, FUN = sum), all.x = TRUE)
              preRevenue[i, 2:length(vecSize)] <- outClass$propWei
            } else {
              tempRev$propWei <- tempRev$Freq / sum(tempRev$Freq)
              tempRev$SizeClass <- factor(findInterval(x = tempRev$Weight, vec = vecSize), levels = 1:length(vecSize))
              outClass <- merge(data.frame(SizeClass = levels(tempRev$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tempRev, FUN = sum), all.x = TRUE)
              preRevenue[i, 2:length(vecSize)] <- outClass$propWei
            }
          }
        }
        outWeiProp[[fisheryBySpecie[[fisIdx]]$specie]] <<- preRevenue


        ## seasonal
        outWeiPropQ[[fisheryBySpecie[[fisIdx]]$specie]] <<- list()
        for (season in c("winter", "spring", "summer", "fall")) {
          preReveSea <- data.frame(FG = 1:length(fgNames))
          preReveSea <- cbind(preReveSea, setNames(lapply(1:(length(vecSize) - 1), function(x) x <- NA), 1:(length(vecSize) - 1)))
          for (i in 1:nrow(preReveSea)) {
            tempRev <- fisheryBySpecie[[fisIdx]]$LWstatQ[fisheryBySpecie[[fisIdx]]$LWstatQ$FG == i & fisheryBySpecie[[fisIdx]]$LWstatQ$Season == season, ]
            if (nrow(tempRev) > 0) {
              if (curUnit == "Length") {
                tempRev$propWei <- tempRev$relAbb / sum(tempRev$relAbb)
                tempRev$SizeClass <- factor(findInterval(x = tempRev$avgLen, vec = vecSize, all.inside = TRUE), levels = 1:(length(vecSize) - 1))
                outClass <- merge(data.frame(SizeClass = levels(tempRev$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tempRev, FUN = sum), all.x = TRUE)
                preReveSea[i, 2:length(vecSize)] <- outClass$propWei
              } else {
                tempRev$propWei <- tempRev$Freq / sum(tempRev$Freq)
                tempRev$SizeClass <- factor(findInterval(x = tempRev$Weight, vec = vecSize), levels = 1:length(vecSize))
                outClass <- merge(data.frame(SizeClass = levels(tempRev$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tempRev, FUN = sum), all.x = TRUE)
                preReveSea[i, 2:length(vecSize)] <- outClass$propWei
              }
            }
          }
          outWeiPropQ[[fisheryBySpecie[[fisIdx]]$specie]][[season]] <<- preReveSea
        }
      }
    },
    getCostRevenue = function() {
      simCostRevenue <<- merge(
        simTotalCost[, c("I_NCEE", "Year", "totCost")],
        simTotalRevenue[, c("I_NCEE", "Year", "totRevenue")]
      )
    },
    simulateFishery = function(thr0 = 100, effoBan = numeric(0), timeStep = "Year", maxEffo = 0) {
      cat("\nGetting length-weight statistics...", sep = "")
      getLWstat()
      cat("Done!", sep = "")

      cat("\nSetup initial parameters...", sep = "")
      genSimEffo()
      simProdAll()
      getSimTotalCost()
      getSimRevenue(timeScale = timeStep)
      getCostRevenue()
      cat("Done!\n", sep = "")

      nFG <- sampMap$cutFG + 1
      Esim <- Etemp <- simEffo
      nVessels <- length(unique(simEffo$I_NCEE))
      Gmat <- Pmat <- matrix(0, nVessels, 1)
      Gmat[, 1] <- simCostRevenue$totRevenue - simCostRevenue$totCost
      noV <- numeric(0)
      nRec <- nrow(Esim)
      nVproc <- nVessels
      nIter <- 0
      toOpt <- numeric(0)

      while (length(noV) < nVessels) {
        nIter <- nIter + 1
        cat("\nIteration", nIter)

        cat("\n\tOptimising effort... ", sep = "")
        genSimEffo(selRow = toOpt, areaBan = effoBan)
        cat("Done!", sep = "")

        if (maxEffo > 0) {
          cat("\n\tScaling max effort... ", sep = "")
          simEffo[, -c(1:3, ncol(simEffo))] <<- lvlSimEffo(simuEffo = simEffo[, -c(1:3, ncol(simEffo))], maxEff = maxEffo)
          cat("Done!", sep = "")
        }
        cat("\n\tComputing production...", sep = "")
        simProdAll(selRow = toOpt)
        cat("Done!", sep = "")

        cat("\n\tComputing cost-revenues...", sep = "")
        getSimTotalCost()
        getSimRevenue(selRow = toOpt, timeScale = timeStep)
        getCostRevenue()
        cat("Done!", sep = "")

        EsimG <- simCostRevenue$totRevenue - simCostRevenue$totCost
        Gmat <- cbind(Gmat, EsimG)
        set_plus <- which(Gmat[, ncol(Gmat)] > Gmat[, ncol(Gmat) - 1])
        set_minus <- setdiff(1:nrow(Gmat), set_plus)
        pvec <- rep(1, nrow(Gmat))
        if (length(set_plus) > 0) pvec[set_plus] <- 0
        Pmat <- cbind(Pmat, pvec)
        if (ncol(Pmat) >= thr0) {
          noV <- unique(c(noV, which(apply(Pmat[, (ncol(Pmat) - thr0 + 1):ncol(Pmat)], 1, sum, na.rm = T) >= thr0)))
          toOpt <- which(!(simEffo$I_NCEE %in% simCostRevenue$I_NCEE[noV]))
        }
        nVproc <- c(nVproc, nVessels - length(noV))
        rec_minus <- which(simEffo$I_NCEE %in% simCostRevenue$I_NCEE[set_minus])
        simEffo[rec_minus, ] <<- Etemp[rec_minus, ]
        Etemp <- simEffo
        Gmat[set_minus, ncol(Gmat)] <- Gmat[set_minus, ncol(Gmat) - 1]

        par(mfrow = c(1, 2), las = 2)
        plot(1:ncol(Gmat), apply(Gmat, 2, sum, na.rm = TRUE) / 1000000,
          type = "l",
          xlab = "Iteration", ylab = "10^6 Euros", lwd = 3, col = 2
        )
        title(main = "Gains")
        plot(1:ncol(Gmat), nVproc,
          type = "l",
          xlab = "Iteration", ylab = "", lwd = 3, col = 4
        )
        title(main = "Vessels to optimize")
      }

      cat("\nSaving results...", sep = "")
      outGmat <<- Gmat
      # outNVlst <<- nVproc
      outOptimEffo <<- Etemp
      cat("Done!\n", sep = "")
    },
    setSimResults = function() {
      simResPlot <<- list()

      outObsEffo <- fleet$effoAllLoa[fleet$effoAllLoa$Year == max(as.numeric(as.character(unique(fleet$effoAllLoa$Year)))), ]
      cumObsEffo <- data.frame(
        FG = colnames(outObsEffo[, 4:(ncol(outObsEffo) - 1)]),
        Hours = apply(outObsEffo[, 4:(ncol(outObsEffo) - 1)], 2, sum)
      )
      rm(outObsEffo)
      cumOptEffo <- data.frame(
        FG = colnames(simEffo[, 4:(ncol(simEffo) - 1)]),
        Hours = apply(simEffo[, 4:(ncol(simEffo) - 1)], 2, sum)
      )

      deltaObsOpt <- data.frame(
        x = as.numeric(colnames(simEffo[, 4:(ncol(simEffo) - 1)])),
        obs = cumObsEffo$Hours,
        opt = cumOptEffo$Hours,
        delta = cumOptEffo$Hours - cumObsEffo$Hours,
        deltaPerc = 100 * (cumOptEffo$Hours - cumObsEffo$Hours) / cumObsEffo$Hours
      )

      all_cell <- merge(x = sampMap$cutResShpFort$id, deltaObsOpt, all = TRUE)
      all_cell[is.na(all_cell)] <- 0

      grid_data <- cbind(sampMap$cutResShpFort, all_cell[, 2:5])

      if (length(sampMap$cutFG) > 0) {
        if (length(sampMap$gridShp@polygons) == (sampMap$cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(sampMap$gridShp), cell_id = 1:length(sampMap$gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- sampMap$cutResShpCent
        }
      } else {
        tmp_coo <- sampMap$cutResShpCent
      }

      grid_data$deltaPerc[grid_data$deltaPerc > 100] <- 101
      grid_data$deltaPerc[grid_data$deltaPerc < -100] <- -101
      grid_data$deltaPerc[grid_data$deltaPerc == 0] <- NA
      grid_data$delta[grid_data$delta == 0] <- NA

      simResPlot[["obsEffort"]] <<- suppressMessages(sampMap$gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = obs),
          colour = "grey20", size = 0.1, data = grid_data, alpha = 0.8
        ) +
        scale_fill_gradient("Observed\nlog10(Hours)",
          low = "snow1",
          high = "#fc8d59", trans = "log10"
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle("Map of observed effort pattern") +
        xlab("Longitude") + ylab("Latitude") +
        theme(legend.position = "left"))

      simResPlot[["optEffort"]] <<- suppressMessages(sampMap$gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = opt),
          colour = "grey20", size = 0.1, data = grid_data, alpha = 0.8
        ) +
        scale_fill_gradient("Optimized\nlog10(Hours)",
          low = "snow1",
          high = "#fc8d59", trans = "log10"
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle("Map of optimized effort pattern") +
        xlab("Longitude") + ylab("Latitude"))

      simResPlot[["absChange"]] <<- suppressMessages(sampMap$gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = delta),
          colour = "black", size = 0.1, data = grid_data, alpha = 1
        ) +
        scale_fill_gradient2("Effort Delta\n(Hours)",
          low = "#91bfdb",
          high = "#fc8d59", mid = "#ffffbf", na.value = "grey20"
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle("Map of Absolute Change") +
        xlab("Longitude") + ylab("Latitude") +
        theme(legend.position = "left"))

      simResPlot[["relChange"]] <<- suppressMessages(sampMap$gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = deltaPerc),
          colour = "black", size = 0.1, data = grid_data, alpha = 1
        ) +
        scale_fill_gradient2("Effort Delta\n(%)",
          low = "#91bfdb",
          high = "#fc8d59", mid = "#ffffbf", na.value = "grey20"
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle("Map of Relative Change") +
        xlab("Longitude") + ylab("Latitude"))
    },
    ggplotFishingPoints = function(year) {
      tmp_dat <- fleet$rawEffort[[year]][sample(1:nrow(fleet$rawEffort[[year]]), min(c(50000, nrow(fleet$rawEffort[[year]])))), c("LON", "LAT", "FishPoint")]
      tmp_dat$Status <- factor(tmp_dat$FishPoint, levels = c("FALSE", "TRUE"), labels = c("Not fishing", "Fishing"))
      ggEffFish <<- suppressMessages(sampMap$gooMapPlot +
        geom_point(
          data = tmp_dat,
          aes(x = LON, y = LAT, color = Status), size = 0.25, alpha = 0.2
        ) +
        scale_colour_manual(values = c("coral", "darkseagreen1")) +
        guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
        ggtitle(paste("Sample fishing points - ", year, sep = "")) +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    setPlotBetaMeltYear = function(specie, year) {
      tmp_melt_sub <- subset(fleet$betaMeltYear[[specie]], Year == year)
      all_cell <- merge(
        x = sampMap$cutResShpFort$id,
        data.frame(
          x = as.numeric(substr(
            as.character(tmp_melt_sub$FishGround), 4,
            nchar(as.character(tmp_melt_sub$FishGround))
          )),
          y = tmp_melt_sub$Productivity
        ), all = TRUE
      )
      all_cell[is.na(all_cell)] <- 0
      grid_data <- cbind(sampMap$cutResShpFort, Beta = all_cell[, 2])

      if (length(sampMap$cutFG) > 0) {
        if (length(sampMap$gridShp@polygons) == (sampMap$cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(sampMap$gridShp), cell_id = 1:length(sampMap$gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- sampMap$cutResShpCent
        }
      } else {
        tmp_coo <- sampMap$cutResShpCent
      }

      sampMap$ggBetaFGmap <<- suppressMessages(sampMap$gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = Beta),
        colour = "black", size = 0.1,
        data = grid_data, alpha = 0.8
      ) +
        scale_fill_gradient("Beta\nValues", low = "lightyellow", high = "mediumseagreen") +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle(paste("Betas x Fishing Ground - ", year, sep = "")) +
        xlab("Longitude") + ylab("Latitude") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(
          legend.position = "right",
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid = element_line(size = 0.1, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 10),
          axis.ticks.y = element_blank()
        ))
      sampMap$ggBetaFGbox <<- suppressMessages(ggplot(
        fleet$betaMeltYear[[specie]],
        aes(
          x = FishGround, y = Productivity,
          group = FishGround
        )
      ) +
        geom_boxplot() +
        coord_flip() +
        geom_point(
          data = tmp_melt_sub,
          aes(
            x = FishGround, y = Productivity,
            fill = Productivity, group = FishGround
          ),
          size = 2, shape = 21, color = "grey40"
        ) +
        geom_line(
          data = tmp_melt_sub,
          aes(x = FishGround, y = Productivity, group = Year),
          color = "grey40"
        ) +
        scale_fill_gradient(low = "lightyellow", high = "mediumseagreen") +
        xlab("Fishing Ground") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 10),
          axis.ticks.y = element_blank()
        ))
    },
    setPlotProdMeltYear = function(specie, year) {
      tmp_melt_sub <- subset(fleet$prodMeltYear[[specie]], Year == year)
      all_cell <- merge(
        x = sampMap$cutResShpFort$id,
        data.frame(
          x = substr(as.character(tmp_melt_sub$FishGround), 4, nchar(as.character(tmp_melt_sub$FishGround))),
          y = tmp_melt_sub$Production
        ), all = TRUE
      )
      all_cell[is.na(all_cell)] <- 0
      grid_data <- cbind(sampMap$cutResShpFort, Hours = all_cell[, 2])

      if (length(sampMap$cutFG) > 0) {
        if (length(sampMap$gridShp@polygons) == (sampMap$cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(sampMap$gridShp), cell_id = 1:length(sampMap$gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- sampMap$cutResShpCent
        }
      } else {
        tmp_coo <- sampMap$cutResShpCent
      }

      sampMap$ggProdFGmap <- suppressMessages(sampMap$gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = Hours),
          colour = "black", size = 0.1,
          data = grid_data, alpha = 0.8
        ) +
        scale_fill_gradient("Production\nValues",
          low = "lightyellow", high = "slateblue1"
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle(paste("Production x Fishing Ground - ", year, sep = "")) +
        xlab("Longitude") + ylab("Latitude") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(
          legend.position = "right",
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 10),
          axis.ticks.y = element_blank()
        ))
      sampMap$ggProdFGbox <<- suppressMessages(ggplot(fleet$prodMeltYear[[specie]], aes(x = FishGround, y = Production, group = FishGround)) +
        geom_boxplot() +
        coord_flip() +
        geom_point(
          data = tmp_melt_sub, aes(
            x = FishGround, y = Production,
            fill = Production, group = FishGround
          ),
          size = 2, shape = 21, color = "grey40"
        ) +
        geom_line(
          data = tmp_melt_sub, aes(x = FishGround, y = Production, group = Year),
          color = "grey40"
        ) +
        scale_fill_gradient(low = "lightyellow", high = "slateblue1") +
        xlab("Fishing Ground") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 10),
          axis.ticks.y = element_blank()
        ))
    },
    setCellPoin = function() {
      sampMap$gridShp@plotOrder <- 1:length(sampMap$gridShp@plotOrder)
      tmp_polygons <- SpatialPolygons(sampMap$gridShp@polygons)
      cat("\nGridding year ", sep = "")
      for (j in names(fleet$rawEffort)) {
        cat(j, "... ", sep = "")
        fleet$rawEffort[[j]]$Cell <<- over(SpatialPoints(fleet$rawEffort[[j]][, c("LON", "LAT")]), tmp_polygons)
      }
      cat("Done!", sep = "")
    },
    setTrackHarb = function() {
      fleet$trackHarbs <<- list()
      for (i in names(fleet$rawEffort)) {
        cat("\nLoading effort year: ", i, "... ", sep = "")
        tmp_eff <- fleet$rawEffort[[i]]
        tmp_harbs <- sqldf("select xCFR I_NCEE, xTnum T_NUM, LON_S, LAT_S, DATE_S, LON_E, LAT_E, DATE_E from
                                                 (select x.I_NCEE xCFR, LAT LAT_S, LON LON_S, DATE DATE_S, T_NUM xTnum from tmp_eff x where W_HARB = 1)
                                                 join
                                                 (select y.I_NCEE yCFR, LAT LAT_E, LON LON_E, DATE DATE_E, T_NUM yTnum from tmp_eff y where W_HARB = 1)
                                                 on xCFR = yCFR and xTnum = yTnum and DATE_S != DATE_E and DATE_S < DATE_E order by xCFR, xTnum, DATE_S")
        cat("Done!", sep = "")
        uni_harbs <- as.data.frame(unique(rbind(as.matrix(tmp_harbs[, c("LON_S", "LAT_S")]), as.matrix(tmp_harbs[, c("LON_E", "LAT_E")]))))
        uni_harbs$Name <- ""
        cat("\nSetting nearest harbor name... ", sep = "")
        tmp_dist <- apply(spDists(
          x = as.matrix(uni_harbs[, 1:2]),
          y = as.matrix(sampMap$harbDbf[, 1:2]), longlat = TRUE
        ), 1, which.min)
        uni_harbs$Name <- as.character(sampMap$harbDbf[tmp_dist, 3])
        cat("Done!", sep = "")
        cat("\nSaving... ", sep = "")
        fleet$trackHarbs[[i]] <<- sqldf("select I_NCEE, T_NUM, k.LON_S LON_S, k.LAT_S LAT_S, DATE_S, HARB_S, LON_E, LAT_E, DATE_E, Name HARB_E from (select I_NCEE, T_NUM, LON_S, LAT_S, DATE_S, Name HARB_S, LON_E, LAT_E, DATE_E from tmp_harbs join uni_harbs using (LON_S, LAT_S)) k join uni_harbs x on x.LON_S = LON_E and x.LAT_S = LAT_E")
        cat("Done!", sep = "")
      }
    },
    setFishGround = function(numCut) {
      sampMap$cutFG <<- numCut
      sampMap$setCutResult(ind_clu = numCut)
      simBanFG <<- data.frame(FG = sampMap$cutResShpCent$FG, Banned = "0", stringsAsFactors = FALSE)
      tmp_clust <- cbind(
        Cell = 1:sampMap$nCells,
        FishGround = sampMap$clusMat[, numCut]
      )
      cat("\n\nSetting Fishing Ground year\n", sep = "")
      for (j in names(fleet$rawEffort)) {
        cat(j, "... ", sep = "")
        fleet$rawEffort[[j]]$FishGround <<- tmp_clust[fleet$rawEffort[[j]]$Cell, 2]
      }
      cat("Done!\n", sep = "")
    },
    addFg2Fishery = function() {
      cat("\n\nConnecting coordinates to fishing ground...", sep = "")

      rawDataFishery$numFG <<- names(sampMap$cutResShp)[over(
        SpatialPoints(data.frame(
          Lon = rawDataFishery$Lon,
          Lat = rawDataFishery$Lat
        )),
        sampMap$cutResShp
      )]

      for (i in 1:length(fisheryBySpecie)) {
        fisheryBySpecie[[i]]$rawLFD$numFG <<- names(sampMap$cutResShp)[over(
          SpatialPoints(data.frame(
            Lon = fisheryBySpecie[[i]]$rawLFD$Lon,
            Lat = fisheryBySpecie[[i]]$rawLFD$Lat
          )),
          sampMap$cutResShp
        )]
      }

      cat(" Done!\n", sep = "")
    },
    addFg2Survey = function() {
      cat("\n\nConnecting coordinates to fishing ground...", sep = "")
      rawDataSurvey$numFG <<- names(sampMap$cutResShp)[over(
        SpatialPoints(data.frame(
          Lon = rawDataSurvey$Lon,
          Lat = rawDataSurvey$Lat
        )),
        sampMap$cutResShp
      )]

      for (i in 1:length(surveyBySpecie)) {
        surveyBySpecie[[i]]$rawLFD$numFG <<- names(sampMap$cutResShp)[over(
          SpatialPoints(data.frame(
            Lon = surveyBySpecie[[i]]$rawLFD$Lon,
            Lat = surveyBySpecie[[i]]$rawLFD$Lat
          )),
          sampMap$cutResShp
        )]
      }
      cat(" Done!\n", sep = "")
    },
    setWeekEffoMatrCell = function() {
      fleet$weekEffoMatr <<- list()
      for (j in names(fleet$rawEffort)) {
        cat("\n\nLoading year ", j, " ... ", sep = "")
        tmp_dat <- fleet$rawEffort[[j]][fleet$rawEffort[[j]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[j]]$Cell), c("I_NCEE", "T_NUM", "WeekNum", "Cell", "FishPoint")]
        cat("Done!", sep = "")
        tmp_dat$Cell <- as.factor(tmp_dat$Cell)
        cat("\nCreating weekly fishing effort matrix... ", sep = "")
        tmp_matrix <- dcast(tmp_dat,
          I_NCEE + T_NUM + WeekNum ~ Cell,
          fun.aggregate = sum,
          na.rm = TRUE, value.var = "FishPoint"
        )
        cat("Done!", sep = "")
        cat("\nChecking... ", sep = "")
        miss_cols <- setdiff(as.character(sampMap$gridShp@plotOrder), names(tmp_matrix)[4:ncol(tmp_matrix)])
        if (length(miss_cols) > 0) {
          cat(length(miss_cols), " cells with no points... ", sep = "")
          tmp_matrix[, miss_cols] <- 0
          tmp_matrix <- tmp_matrix[, c(1:3, 3 + order(as.numeric(names(tmp_matrix)[4:ncol(tmp_matrix)])))]
        }
        cat(" Done!", sep = "")
        fleet$weekEffoMatr[[j]] <<- tmp_matrix
      }
    },
    setWeekEffoMatrGround = function() {
      fleet$weekEffoMatr <<- list()
      for (j in names(fleet$rawEffort)) {
        cat("\n\nLoading year ", j, " ... ", sep = "")
        tmp_dat <- fleet$rawEffort[[j]][fleet$rawEffort[[j]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[j]]$Cell), c("I_NCEE", "T_NUM", "WeekNum", "MonthNum", "FishGround", "FishPoint")]
        cat("Done!", sep = "")
        tmp_dat$FishGround <- as.factor(tmp_dat$FishGround)
        cat("\nCreating weekly fishing effort matrix... ", sep = "")
        tmp_matrix <- dcast(tmp_dat,
          I_NCEE + T_NUM + WeekNum + MonthNum ~ FishGround,
          fun.aggregate = sum,
          na.rm = TRUE, value.var = "FishPoint"
        )
        cat("Done!", sep = "")
        cat("\nChecking... ", sep = "")
        miss_cols <- setdiff(
          as.character(unique(fleet$rawEffort[[j]]$FishGround[!is.na(fleet$rawEffort[[j]]$FishGround)])),
          names(tmp_matrix)[5:ncol(tmp_matrix)]
        )
        if (length(miss_cols) > 0) {
          cat(length(miss_cols), " cells with no points... ", sep = "")
          tmp_matrix[, miss_cols] <- 0
          tmp_matrix <- tmp_matrix[, c(1:4, 4 + order(as.numeric(names(tmp_matrix)[5:ncol(tmp_matrix)])))]
        }
        tmp_matrix <- sqldf("select * from tmp_matrix join (select I_NCEE, T_NUM, min(DATE) DATE_S, max(DATE) DATE_E from tmp_dat group by I_NCEE, T_NUM) using (I_NCEE, T_NUM)")
        cat(" Done!", sep = "")
        fleet$weekEffoMatr[[j]] <<- tmp_matrix
      }
    },
    ggplotGridEffort = function(year) {
      tmp_dat <- table(fleet$rawEffort[[year]][fleet$rawEffort[[year]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[year]]$Cell), c("Cell")])
      all_cell <- merge(
        x = sampMap$gridPolySet$PID,
        data.frame(x = as.numeric(names(tmp_dat)), y = tmp_dat), all = TRUE
      )[, c(1, 3)]
      all_cell[is.na(all_cell)] <- 0
      # grid_data <- cbind(sampMap$gridPolySet, LogCount = log10(all_cell[,2] + 1))
      grid_data <- cbind(sampMap$gridPolySet, Count = all_cell[, 2])
      ggEffGrid <<- suppressMessages(sampMap$gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID, fill = Count),
        size = 0.2,
        data = grid_data, alpha = 0.8
      ) +
        scale_fill_gradient(
          low = "Yellow", high = "coral",
          trans = "log10",
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        ) +
        ggtitle(paste("Fishing Effort - ", year, sep = "")) +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    getNnlsModel = function(specie, minobs, thr_r2) {
      data <- fleet$effoProdAllLoa
      nFG <- sampMap$cutFG + 1
      thrB <- fleet$specSett[[specie]]$threshold
      thr0 <- mean(data[, specie][data[, specie] < thrB & data[, specie] > 0])
      naFind <- which(is.na(data) == TRUE, arr.ind = TRUE)
      if (length(naFind) > 0) data <- data[-naFind[, 1], ]
      norow <- which(data[, which(colnames(data) == specie)] <= thrB)
      X0 <- data[-norow, which(colnames(data) %in% c("Year", "MonthNum", "Loa", as.character(seq(1:nFG))))]
      Y0 <- data[-norow, which(colnames(data) == specie)]

      # Gen Matrix of scenaria and fill by membership
      nY <- length(unique(X0$Year))
      nM <- 12
      membY <- as.numeric(as.factor(X0$Year)) # Membership delle osservazioni x anno
      membM <- as.numeric(X0$MonthNum) # Membership delle osservazioni x mese
      SceMat <- expand.grid(unique(membY), unique(membM))
      colnames(SceMat) <- c("YEAR", "MONTH")
      SceList <- vector(mode = "list", length = nrow(SceMat))
      il <- 0
      for (i in 1:nrow(SceMat)) {
        il <- il + 1
        SceList[[il]] <- which((membY == SceMat[il, 1]) & (membM == SceMat[il, 2]))
      }
      lsce <- as.numeric(lapply(SceList, length))
      wsce <- which(lsce >= minobs)
      nSce <- length(wsce)

      # Gen and fill the matrix of betas
      bmat <- matrix(NA, length(unique(membY)) * length(unique(membM)), nFG)
      obsY <- fittedY <- vector(mode = "list", length = nSce)
      nnls_r2 <- rep(NA, nSce)
      nfitted <- 0
      nno <- 0
      wcol <- which(colnames(X0) %in% as.character(seq(1, nFG)))

      for (iS in wsce) {
        subX <- X0[SceList[[iS]], wcol] * (X0$Loa[SceList[[iS]]])
        subY <- Y0[SceList[[iS]]]
        zeroFG <- which(apply(subX, 2, sum) == 0)
        eFG <- setdiff(1:nFG, zeroFG)
        if (length(zeroFG) > 0) subX <- subX[, -zeroFG]
        if (min(dim(subX)) > 0) {
          nnls_m <- getNNLS(subX, subY, zeroFG)
          if (abs(nnls_m$r2) > thr_r2) {
            nnls_r2[iS] <- nnls_m$r2
            obsY[[iS]] <- nnls_m$obs
            fittedY[[iS]] <- nnls_m$fitted
            tcoo <- 12 * (SceMat[iS, "YEAR"] - 1) + SceMat[iS, "MONTH"]
            bmat[tcoo, eFG] <- nnls_m$betas
            if (length(zeroFG) > 0) bmat[tcoo, zeroFG] <- 0
            nfitted <- nfitted + 1
          } else {
            nno <- nno + 1
          }
        } else {
          nno <- nno + 1
        }
      }
      cat("\nNNLS: ", nSce, " actual scenarios - ", nfitted, " fitted", "(", floor(100 * (nSce - nno) / nSce), "%)", sep = "")
      blist <- vector(mode = "list", length = 4)
      colnames(bmat) <- paste("BE_", formatC(1:ncol(bmat), width = nchar(ncol(bmat)), format = "d", flag = "0"), sep = "")

      if (anyNA(bmat)) {
        zero_chk <- which(apply(bmat, 2, sum, na.rm = TRUE) == 0)
        if (length(zero_chk) > 0) {
          par_betas <- fillbetas(bmat[, -zero_chk])
          zero_beta <- matrix(0,
            nrow = nrow(data.frame(bmat[, zero_chk])),
            ncol = ncol(data.frame(bmat[, zero_chk]))
          )
          colnames(zero_beta) <- colnames(bmat)[zero_chk]
          colnames(par_betas) <- colnames(bmat)[-zero_chk]
          full_betas <- cbind(par_betas, zero_beta)
          bmat <- full_betas[, order(colnames(full_betas))]
        } else {
          bmat <- fillbetas(bmat)
        }
      }

      if (any(bmat < 0)) bmat[which(bmat < 0)] <- 0

      blist[[1]] <- bmat
      blist[[2]] <- unlist(obsY)
      blist[[3]] <- unlist(fittedY)
      blist[[4]] <- unlist(nnls_r2)
      blist[[5]] <- SceMat
      blist[[6]] <- nfitted
      blist[[7]] <- nSce
      names(blist) <- c("bmat", "obsY", "fittedY", "nnls_r2", "SceMat", "nfitted", "nSce")
      fleet$resNNLS[[specie]] <<- blist
      fleet$setBetaMeltYear(specie)
    },
    cohoDisPlot = function(whoSpe, whoCoh, whiYea, interp) {
      if (interp == FALSE) {
        if (whoCoh == "All") {
          if (whiYea == "All") {
            # 1+round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,,],1,sum)/max(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,,],1,sum)), 2)*100
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[, , , ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - All years", sep = ""), legendUnits = "N."
            )
          } else {
            # 1+round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)/max(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)), 2)*100
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[, , whiYea, ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
              legendUnits = "N."
            )
          }
        } else {
          if (whiYea == "All") {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[, whoCoh, , ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
              legendUnits = "N."
            )
          } else {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[, whoCoh, whiYea, ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
              legendUnits = "N."
            )
          }
        }
      } else {
        if (whoCoh == "All") {
          if (whiYea == "All") {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[, , , ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - All years", sep = ""),
              legendUnits = "N."
            )
          } else {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[, , whiYea, ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
              legendUnits = "N."
            )
          }
        } else {
          if (whiYea == "All") {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[, whoCoh, , ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
              legendUnits = "N."
            )
          } else {
            yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[, whoCoh, whiYea, ], 1, sum))
            round_yea <- 1 + 100 * yea_abb / max(yea_abb)

            distrPlotCols(
              cols = rev(topo.colors(101)), vals = round_yea,
              maxVal = ceiling(max(yea_abb)),
              plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
              legendUnits = "N."
            )
          }
        }
      }
    },
    # setCoh_A_Survey = function(){
    #   if(length(specieInSurvey) == 1){
    #     calcCoh_A_Survey(1)
    #   }else{
    #     for(i in 1:length(specieInSurvey)){
    #       calcCoh_A_Survey(i)
    #     }}
    # },
    # setCoh_A_Fishery = function(){
    #   if(length(specieInFishery) == 1){
    #     calcCoh_A_Fishery(1)
    #   }else{
    #     for(i in 1:length(specieInFishery)){
    #       calcCoh_A_Fishery(i)
    #     }}
    # },
    calcCoh_A_Survey = function(ind_num) {
      Pop <- surveyBySpecie[[ind_num]]$LFDPop
      LC <- surveyBySpecie[[ind_num]]$lengClas[-length(surveyBySpecie[[ind_num]]$lengClas)]
      sp <- surveyBySpecie[[ind_num]]$specie
      nc <- surveyBySpecie[[ind_num]]$nCoho
      surveyBySpecie[[ind_num]]$Coh_A <<- array(dim = c(sampMap$nCells, nc, length(surveyBySpecie[[ind_num]]$year), 2))
      for (y in 1:length(surveyBySpecie[[ind_num]]$year)) {
        for (sex in c(1:2)) {
          mms <- surveyBySpecie[[ind_num]]$mixPar[[sex]][[1]][y, ]
          sds <- surveyBySpecie[[ind_num]]$mixPar[[sex]][[2]][y, ]
          opt <- matrix(0, length(LC), nc)
          for (ij in 1:sampMap$nCells) {
            vv <- Pop[ij, , y, sex]
            coh.abb <- numeric(nc)
            if (sum(vv) > 0) {
              for (coh in c(1:nc)) opt[, coh] <- dnorm(LC, mms[coh], sds[coh])
              opt.ass <- apply(opt, 1, which.max)
              for (coh in c(1:nc)) coh.abb[coh] <- sum(vv[which(opt.ass == coh)])
            }
            surveyBySpecie[[ind_num]]$Coh_A[ij, 1:nc, y, sex] <- as.numeric(coh.abb)
          }
        }
      }
    },
    calcCoh_A_Fishery = function(ind_num) {
      Pop <- fisheryBySpecie[[ind_num]]$LFDPop
      LC <- fisheryBySpecie[[ind_num]]$lengClas[-length(fisheryBySpecie[[ind_num]]$lengClas)]
      sp <- fisheryBySpecie[[ind_num]]$specie
      nc <- fisheryBySpecie[[ind_num]]$nCoho
      fisheryBySpecie[[ind_num]]$Coh_A <<- array(dim = c(sampMap$nCells, nc, length(fisheryBySpecie[[ind_num]]$year), 2))
      for (y in 1:length(fisheryBySpecie[[ind_num]]$year)) {
        for (sex in c(1:2)) {
          mms <- fisheryBySpecie[[ind_num]]$mixPar[[sex]][[1]][y, ]
          sds <- fisheryBySpecie[[ind_num]]$mixPar[[sex]][[2]][y, ]
          opt <- matrix(0, length(LC), nc)
          for (ij in 1:sampMap$nCells) {
            vv <- Pop[ij, , y, sex]
            coh.abb <- numeric(nc)
            if (sum(vv) > 0) {
              for (coh in c(1:nc)) opt[, coh] <- dnorm(LC, mms[coh], sds[coh])
              opt.ass <- apply(opt, 1, which.max)
              for (coh in c(1:nc)) coh.abb[coh] <- sum(vv[which(opt.ass == coh)])
            }
            fisheryBySpecie[[ind_num]]$Coh_A[ij, 1:nc, y, sex] <- as.numeric(coh.abb)
          }
        }
      }
    },
    intrpCoh_A_Survey = function(ind_num) {
      surveyBySpecie[[ind_num]]$Coh_A_Int <<- array(dim = c(sampMap$nCells, surveyBySpecie[[ind_num]]$nCoho, length(surveyBySpecie[[ind_num]]$year), 2))
      for (y in 1:length(surveyBySpecie[[ind_num]]$year)) {
        for (sex in 1:2) {
          for (coh in 1:surveyBySpecie[[ind_num]]$nCoho) {
            xdata <- cbind(sampMap$griCent, surveyBySpecie[[ind_num]]$Coh_A[, coh, y, sex])
            colnames(xdata) <- c("Lon", "Lat", "Coh")
            xdata <- as.data.frame(xdata)
            yea_poi <- surveyBySpecie[[ind_num]]$rawLFD[which(surveyBySpecie[[ind_num]]$rawLFD$Year == surveyBySpecie[[ind_num]]$year[y]), c("Lon", "Lat")]
            cMEDITS <- which(!is.na(over(sampMap$gridShp, SpatialPoints(unique(yea_poi)))))
            noMEDITS <- setdiff(c(1:sampMap$nCells), cMEDITS)
            Areacell <- 9.091279 * 11.112
            RateArea <- Areacell / 100
            surveyBySpecie[[ind_num]]$Coh_A_Int[, coh, y, sex] <- IntInvDis(RateArea * xdata, cMEDITS, noMEDITS,
              Refmax = 5, Refmin = 3,
              sampMap$nCells,
              sampMap$gridShp, graph = T, logplot = F
            )[, 3]
          }
        }
      }
    },
    intrpCoh_A_Fishery = function(ind_num) {
      fisheryBySpecie[[ind_num]]$Coh_A_Int <<- array(dim = c(sampMap$nCells, fisheryBySpecie[[ind_num]]$nCoho, length(fisheryBySpecie[[ind_num]]$year), 2))
      for (y in 1:length(fisheryBySpecie[[ind_num]]$year)) {
        for (sex in 1:2) {
          for (coh in 1:fisheryBySpecie[[ind_num]]$nCoho) {
            xdata <- cbind(sampMap$griCent, fisheryBySpecie[[ind_num]]$Coh_A[, coh, y, sex])
            colnames(xdata) <- c("Lon", "Lat", "Coh")
            xdata <- as.data.frame(xdata)
            yea_poi <- fisheryBySpecie[[ind_num]]$rawLFD[which(fisheryBySpecie[[ind_num]]$rawLFD$Year == fisheryBySpecie[[ind_num]]$year[y]), c("Lon", "Lat")]
            cMEDITS <- which(!is.na(over(sampMap$gridShp, SpatialPoints(unique(yea_poi)))))
            noMEDITS <- setdiff(c(1:sampMap$nCells), cMEDITS)
            Areacell <- 9.091279 * 11.112
            RateArea <- Areacell / 100
            fisheryBySpecie[[ind_num]]$Coh_A_Int[, coh, y, sex] <- IntInvDis(RateArea * xdata, cMEDITS, noMEDITS,
              Refmax = 5, Refmin = 3,
              sampMap$nCells,
              sampMap$gridShp, graph = T, logplot = F
            )[, 3]
          }
        }
      }
    }
  )
)



#### SurveyBySpecie#################################################
#' SurveyBySpecie
#'
#' The \code{SurveyBySpecie} class implements the class of SMART to
#'  handle species samplings.
#'
#' @docType class
#' @usage NULL
#' @keywords data
#' @return Object of \code{\link{R6Class}} with attributes and methods for the survey data.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field specie Name of the specie.
#' @field year Years in the time-serie.
#' @field rawLFD data.frame, raw length frequency distribution.
#' @field abuAvg data.frame, average abundances by depth' stratum.
#' @field meditsIndex data.frame, medits index by depth' stratum.
#' @field lengClas numeric, length classes.
#' @field nCoho numeric, number of cohorts.
#' @field spreDist list of DF, lfd by sex.
#' @field sprePlot plots of LFD statistics.
#' @field spreSpat list of DF, spatial distribution by sex.
#' @field sampMcmc list, mcmc output chains.
#' @field groMixout list of DF, aged individuals by sex.
#' @field groPars list of DF, growth parameters by sex.
#' @field LWpar list of DF, length/weight parameters by sex.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(sing_spe)}}{Automatic initialization made by the
#'   SmartProject class}
#'   \item{\code{setRawData(raw_data)}}{This method is used load the initial
#'   raw dataset}
#'   \item{\code{setYears()}}{This method is used to store the years in the
#'   provided time-serie}
#'   \item{\code{setSpecie()}}{This method is used to store the name of
#'   the specie of the initial raw data}
#'   \item{\code{setLClass()}}{This method is used to store the unique
#'   length values of the sampled specie}
#'   \item{\code{setDepth(bathyMatrix)}}{This method is used to assign the
#'   depth value corresponding to each sampling location}
#'   \item{\code{setStratum(vecStrata)}}{This method is used to set the
#'   depth strata of each sampling location}
#'   \item{\code{setIndSpe()}}{This method is used to aggregate the abundance
#'   data into the medits index}
#'   \item{\code{setAbuAvg()}}{This method is used to standardize the
#'   spatial abundances by depth strata}
#'   \item{\code{setNCoho(num_coh)}}{This method is used to setup the number
#'   of cohorts for the ageing module}
#'   \item{\code{setLWpar(alphaVal, betaVal, sex)}}{This method is used to
#'   store the alpha and beta values for the length/weight relationship}
#'   \item{\code{setWeight(sexVal = "Female")}}{This method is used to
#'   compute the fish weight given their length and the LWrelationship}
#'   \item{\code{setSpreDistSing()}}{This method is used to spread the
#'   aggregated LFD abundances into single individuals}
#'   \item{\code{setSprePlot(sampSex)}}{This method is used to setup the
#'   plots of the LFD statistics}
#'   \item{\code{setSpatDistSing()}}{This method is used to setup the spatial
#'   distribution of the single specimens}
#'   \item{\code{setSpatPlot(sampSex)}}{This method is used to store the
#'   spatial plots of the population}
#'   \item{\code{getMCsamps(numSamp, numAdap, numIter, sexDrop, curveSel)}}{
#'   This method is used to get a sample of the population to feed the mcmc
#'   module}
#'   \item{\code{getGrowPar(sexDrop)}}{This method is used to extract the
#'   growth parameters from the mcmc results}
#'   \item{\code{getMCage(sexDrop)}}{This method is used to assign an age to
#'   each fish}
#'   \item{\code{setMCplot(sexDrop, selCurve)}}{This method is used to setup
#'   the plot of the mcmc results}
#'   \item{\code{calcMixDate(nAdap, nSamp, nIter, sexDrop, curveSel)}}{
#'   This method is used to estimate the growth parameters of a population}
#'   \item{\code{ggplotMcmcOut(selCompo, selSex)}}{This method is used to
#'   output the stored plots of mcmc results}
#'   }


SurveyBySpecie <- R6Class("SurveyBySpecie",
  portable = FALSE,
  class = TRUE,
  public = list(
    specie = NULL,
    year = NULL,
    rawLFD = NULL,
    abuAvg = NULL,
    meditsIndex = NULL,
    lengClas = NULL,
    LFDPop = NULL,
    mixPar = NULL,
    nCoho = NULL,
    speSex = NULL,
    spreDist = list(),
    sprePlot = list(),
    spreSpat = list(),
    sampMcmc = list(),
    groMixout = list(),
    groPars = list(),
    Coh_A = NULL,
    Coh_A_Int = NULL,
    LWpar = NULL,
    initialize = function(sing_spe) {
      setRawData(sing_spe)
      setYears()
      setSpecie()
      setLClass()
    },
    setRawData = function(raw_data) {
      rawLFD <<- raw_data
    },
    plotLFD = function() {
      plotSpeAllYea(rawLFD)
    },
    setYears = function() {
      year <<- sort(unique(rawLFD[, "Date"]), decreasing = FALSE)
    },
    setSpecie = function() {
      specie <<- unique(rawLFD[, "Specie"])
    },
    setLClass = function() {
      lengClas <<- seq(from = min(rawLFD[, "Class"]), to = max(rawLFD[, "Class"]), by = 1)
    },
    setDepth = function(bathyMatrix) {
      rawLFD$Depth <<- get.depth(bathyMatrix, x = rawLFD$Lon, y = rawLFD$Lat, locator = FALSE)[, 3]
    },
    setStratum = function(vecStrata = c(0, 10, 100, 1000, Inf)) {
      tmp_mem <- findInterval(x = -rawLFD$Depth, vec = vecStrata)
      rawLFD$Stratum <<- factor(tmp_mem, levels = 1:(length(vecStrata) - 1), labels = paste(vecStrata[-length(vecStrata)], vecStrata[-1], sep = " - "))
    },
    setIndSpe = function() {
      meditsIndex <<- aggregate(weiFem ~ Class + Year, data = abuAvg, sum)
      meditsIndex <<- merge(x = meditsIndex, y = aggregate(weiMal ~ Class + Year, data = abuAvg, sum), all = TRUE)
      meditsIndex <<- merge(x = meditsIndex, y = aggregate(weiUns ~ Class + Year, data = abuAvg, sum), all = TRUE)
    },
    setAbuAvg = function() {
      tmp_lfd <- rawLFD
      tmp_lfd$Year <- years(tmp_lfd$Date)
      if ("Area" %in% colnames(tmp_lfd)) {
        tmp_lfd$Female <- tmp_lfd$Female / tmp_lfd$Area
        tmp_lfd$Male <- tmp_lfd$Male / tmp_lfd$Area
        tmp_lfd$Unsex <- tmp_lfd$Unsex / tmp_lfd$Area
      }

      tmp_aggFem <- aggregate(Female ~ Class + Stratum + Year, data = tmp_lfd, mean)
      tmp_aggMal <- aggregate(Male ~ Class + Stratum + Year, data = tmp_lfd, mean)
      tmp_aggUns <- aggregate(Unsex ~ Class + Stratum + Year, data = tmp_lfd, mean)
      tmp_all <- merge(x = tmp_aggFem, y = tmp_aggMal, all = TRUE)
      tmp_all <- merge(x = tmp_all, y = tmp_aggUns, all = TRUE)
      abuAvg <<- tmp_all
    },
    setNCoho = function(num_coh) {
      nCoho <<- num_coh
    },
    setLWpar = function(alphaVal, betaVal, sex) {
      LWpar[[sex]] <<- list(alpha = as.numeric(alphaVal), beta = as.numeric(betaVal))
    },
    setWeight = function(sexVal = "Female") {
      groMixout[[sexVal]]$Weight <<- LWpar[[sexVal]][["alpha"]] * groMixout[[sexVal]]$Length^LWpar[[sexVal]][["beta"]]
    },
    setSpreDistSing = function() {
      for (sex in c("Female", "Male", "Unsex")) {
        tmp_spre <- rawLFD[!is.na(rawLFD$numFG), c("Date", "Class", "numFG", sex)]

        num_sex <- sum(tmp_spre[, 4])
        cat("Found", num_sex, sex, as.character(specie), "samples\n", sep = " ")

        spreDist <- data.frame(
          UTC = rep(tmp_spre$Date, tmp_spre[, 4]),
          Length = rep(tmp_spre$Class, tmp_spre[, 4]) + runif(num_sex, -0.5, 0.5),
          NumFG = rep(tmp_spre$numFG, tmp_spre[, 4])
        )

        spreDist$Year <- years(spreDist$UTC)
        spreDist$Month <- months(as.chron(spreDist$UTC))

        spreDist[[sex]] <<- spreDist

        setSprePlot(sampSex = sex)
      }
    },
    setAvailSex = function() {
      speSex <<- sort(names(which(lapply(spreDist, nrow) > 0)))
    },
    setSprePlot = function(sampSex) {
      sprePlot[[sampSex]] <<- list(
        histLfdTot = set_ggHistLfdTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        histUtcTot = set_ggHistUtcTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        dotUtcSplit = set_ggDotUtcSplit(spreDist[[sampSex]]) + scale_color_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        histUtcLfd = set_ggHistUtcLfd(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE")))
      )
    },
    setSpatDistSing = function() {
      for (sex in c("Female", "Male", "Unsex")) {
        tmp_fishSpat <- rawLFD[!is.na(rawLFD$numFG) & rawLFD[, sex] > 0, c("Lon", "Lat", "numFG", sex)]
        if (nrow(tmp_fishSpat) > 0) {
          barploFgAll <- data.frame(table(tmp_fishSpat$numFG))
          barploFgAll <- barploFgAll[order(as.numeric(as.character(barploFgAll[, 1]))), ]
          barploFgAll$FG <- factor(barploFgAll$Var1, levels = barploFgAll$Var1)
          barploFgAll$relFreq <- round(100 * barploFgAll$Freq / sum(barploFgAll$Freq), 1)
          spreSpat[[sex]] <<- barploFgAll
          setSpatPlot(sampSex = sex)
        }
      }
    },
    setSpatPlot = function(sampSex) {
      sprePlot[[sampSex]][["spatAbbTbl"]] <<- set_spatAbbTbl(spreSpat[[sampSex]])
      sprePlot[[sampSex]][["spatAbsFreq"]] <<- set_spatAbsFreq(spreSpat[[sampSex]])
      sprePlot[[sampSex]][["spatRelFreq"]] <<- set_spatRelFreq(spreSpat[[sampSex]])
    },
    getMCsamps = function(numSamp = 2000, numAdap = 100, numIter = 500, sexDrop = "Female", curveSel = "von Bertalanffy") {
      sub_idx <- sample(1:nrow(spreDist[[sexDrop]]), size = numSamp, replace = ifelse(numSamp < nrow(spreDist[[sexDrop]]), FALSE, TRUE))
      sub_data <- spreDist[[sexDrop]][sub_idx, ]

      N <- length(sub_data$Length)
      alpha <- rep(1, nCoho)
      Z <- rep(NA, N)
      Z[which.min(sub_data$Length)] <- 1
      Z[which.max(sub_data$Length)] <- nCoho

      dataList <- list(
        y = sub_data$Length,
        maxLeng = max(sub_data$Length), ## !!!
        alpha = alpha,
        Z = Z,
        N = N,
        Nclust = nCoho
      )

      inits <- list(
        list(Linf = min(sub_data$Length), k = 0.5, t0 = 0.0),
        list(Linf = mean(sub_data$Length), k = 0.5, t0 = 0.0),
        list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0)
      )

      modelGrow <- ifelse(curveSel == "von Bertalanffy",
        system.file("model/bertGrow.jags", package = "smartR"),
        system.file("model/gompGrow.jags", package = "smartR")
      )

      jags.m <- jags.model(modelGrow,
        data = dataList,
        inits = inits,
        n.chains = 3,
        n.adapt = numAdap
      )

      ### MCMC chain sampling
      # n.iter <- 500
      obsNode <- c("Linf", "k", "t0", "tau", "p")
      samps <- coda.samples(jags.m, obsNode, n.iter = numIter)

      sampMcmc[[sexDrop]] <<- samps
    },
    getGrowPar = function(sexDrop = "Female") {
      groPars[[sexDrop]]$LHat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "Linf"]))
      groPars[[sexDrop]]$kHat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "k"]))
      groPars[[sexDrop]]$t0Hat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "t0"]))
      groPars[[sexDrop]]$taus <<- as.matrix(sampMcmc[[sexDrop]][, grep("tau", varnames(sampMcmc[[sexDrop]]))])
      groPars[[sexDrop]]$sigma2s <<- 1 / groPars[[sexDrop]]$taus
      groPars[[sexDrop]]$sigma2Hat <<- apply(groPars[[sexDrop]]$sigma2s, 2, mean)
    },
    getMCage = function(sexDrop = "Female") {
      tt <- as.POSIXlt(chron(spreDist[[sexDrop]]$UTC))$yday / 366

      zHat <- apply(cbind(spreDist[[sexDrop]]$Length, tt), 1, FUN = function(x) length2age(
          numCoh = nCoho,
          Linf = groPars[[sexDrop]]$LHat,
          kappa = groPars[[sexDrop]]$kHat,
          tZero = groPars[[sexDrop]]$t0Hat,
          lengthIn = x[1],
          timeIn = x[2],
          sqrtSigma = sqrt(groPars[[sexDrop]]$sigma2Hat)
        ))

      ages.f <- zHat - 1 + tt
      AA <- floor(ages.f)

      FGlabels <- as.numeric(as.character(spreDist[[sexDrop]]$NumFG))
      FGnames <- unique(FGlabels)
      FG <- numeric(length(FGlabels))
      for (FGname in 1:length(FGnames)) {
        idx_FG <- which(FGlabels == FGnames[FGname])
        FG[idx_FG] <- rep(FGname, length(idx_FG))
      }

      mix_out <- data.frame(
        Length = spreDist[[sexDrop]]$Length,
        Date = spreDist[[sexDrop]]$UTC,
        Day = tt,
        Age = AA,
        AgeNF = ages.f,
        FG = FGlabels
      )

      mix_out$Year <- years(mix_out$Date)
      mix_out$Month <- as.numeric(months(as.chron(mix_out$Date)))
      mix_out$MonthChar <- spreDist[[sexDrop]]$Month
      mix_out$Quarter <- as.numeric(quarters(mix_out$Date))
      mix_out$Birth <- as.numeric(as.character(mix_out$Year)) - mix_out$Age

      zeroedMonth <- ifelse(nchar(mix_out$Month) == 2, mix_out$Month, paste("0", mix_out$Month, sep = ""))
      mix_out$CatcDate <- factor(paste(mix_out$Year,
        zeroedMonth,
        sep = "-"
      ),
      levels = paste(rep(sort(unique(mix_out$Year)), each = 12),
        ifelse(nchar(1:12) == 2, 1:12, paste("0", 1:12, sep = "")),
        sep = "-"
      )
      )

      groMixout[[sexDrop]] <<- mix_out
    },
    setMCplot = function(sexDrop = "Female", selCurve = "von Bertalanffy") {
      nIter <- nrow(as.matrix(sampMcmc[[sexDrop]][[1]]))
      dfLinf <- data.frame(
        Parameter = "Linf",
        Iter = 1:nIter,
        Chain = as.matrix(sampMcmc[[sexDrop]][, "Linf"], chains = TRUE)[, 1],
        Value = as.matrix(sampMcmc[[sexDrop]][, "Linf"], chains = TRUE)[, 2]
      )
      dfKapp <- data.frame(
        Parameter = "Kappa",
        Iter = 1:nIter,
        Chain = as.matrix(sampMcmc[[sexDrop]][, "k"], chains = TRUE)[, 1],
        Value = as.matrix(sampMcmc[[sexDrop]][, "k"], chains = TRUE)[, 2]
      )

      ggdataSamps <- rbind(dfLinf, dfKapp)
      ggdataSampScat <- cbind(dfLinf[, 2:3],
        Linf = dfLinf[, 4],
        Kappa = dfKapp[, 4]
      )

      outPalette <- rainbow(nCoho)

      cat("\n\tSetting mcmc diagnostic plots... ", sep = "")
      ### MCMC chain Traceplot
      sprePlot[[sexDrop]][["traceChain"]] <<- set_ggChainTrace(ggdataSamps)
      ### MCMC chain scatterplot
      sprePlot[[sexDrop]][["scatLK"]] <<- set_ggChainScatter(gg_DFscat = ggdataSampScat, meanL = groPars[[sexDrop]]$LHat, meanK = groPars[[sexDrop]]$kHat)
      ### MCMC chain Boxplot Tau
      sprePlot[[sexDrop]][["cohoPreciGG"]] <<- set_ggTausBox(df_taus = groPars[[sexDrop]]$taus[, 1:(max(groMixout[[sexDrop]]$Age) + 1)], tauPalette = outPalette, numCoho = nCoho)
      ### MCMC Boxplot Sigma
      sprePlot[[sexDrop]][["cohoVariGG"]] <<- set_ggSigmaBox(df_sigma = groPars[[sexDrop]]$sigma2s[, 1:(max(groMixout[[sexDrop]]$Age) + 1)], sigPalette = outPalette, numCoho = nCoho)
      cat("Done!", sep = "")

      coho_AL <- ddply(groMixout[[sexDrop]], .(Age), summarise,
        coh.mean = mean(Length), coh.var = var(Length), coh.num = length(Length)
      )

      cat("\n\tSetting Age-Length plots... ", sep = "")
      ### MCMC Plot Age-Length
      sprePlot[[sexDrop]][["ageLength"]] <<- set_ggAgeLength(df_mix = groMixout[[sexDrop]], mixPalette = outPalette)
      ### MCMC Age-Length Key
      sprePlot[[sexDrop]][["ageLengthTbl"]] <<- set_tblAgeLength(df_mix = groMixout[[sexDrop]])
      ### MCMC output cohort stats
      sprePlot[[sexDrop]][["cohoStatTbl"]] <<- set_tblCohoStat(df_coho = coho_AL)
      cat("Done!", sep = "")

      growPath <- data.frame(
        Birth = rep(min(groMixout[[sexDrop]]$Birth):(min(groMixout[[sexDrop]]$Birth) + 11), each = length(levels(groMixout[[sexDrop]]$CatcDate))),
        Date = rep(levels(groMixout[[sexDrop]]$CatcDate), times = length(min(groMixout[[sexDrop]]$Birth):(min(groMixout[[sexDrop]]$Birth) + 11))),
        Length = NA
      )
      growPath$Age <- as.numeric(strtrim(growPath$Date, 4)) - growPath$Birth + as.numeric(substr(growPath$Date, 6, 7)) / 12

      if (selCurve == "von Bertalanffy") {
        growPath$Length <- calcVonBert(groPars[[sexDrop]]$LHat, groPars[[sexDrop]]$kHat, growPath$Age)
      } else {
        growPath$Length <- calcGomp(groPars[[sexDrop]]$LHat, groPars[[sexDrop]]$kHat, growPath$Age)
      }

      growPath$Date <- factor(growPath$Date, levels = levels(groMixout[[sexDrop]]$CatcDate))
      growPath <- growPath[growPath$Length > floor(min(groMixout[[sexDrop]]$Length)), ]

      cat("\n\tSetting Population plots... ", sep = "")
      ### MCMC quarter vertical hist
      sprePlot[[sexDrop]][["histBirth"]] <<- set_ggHistBirth(df_mix = groMixout[[sexDrop]], df_grow = growPath)

      ### MCMC calc birth
      out_birth <- table(paste(groMixout[[sexDrop]]$Year, groMixout[[sexDrop]]$Quarter, sep = "_"), groMixout[[sexDrop]]$Birth)
      birth_melt <- melt(out_birth)
      names(birth_melt) <- c("Catch", "Birth", "Qty")
      birth_melt$Catch <- factor(birth_melt$Catch, levels = paste(rep(levels(groMixout[[sexDrop]]$Year), each = 4),
        rep(1:4, times = length(levels(groMixout[[sexDrop]]$Year))),
        sep = "_"
      ))
      birth_melt$Birth <- as.factor(birth_melt$Birth)
      birth_melt <- birth_melt[birth_melt$Qty != 0, ]

      ### MCMC Catch * Quarters
      sprePlot[[sexDrop]][["lineCatch"]] <<- set_ggCatchLine(df_birth = birth_melt)

      ### MCMC Calc Survivors
      tot_count <- apply(out_birth, 2, sum)
      surv_tbl <- out_birth
      for (i in 1:nrow(out_birth)) {
        surv_tbl[i, ] <- tot_count
        tot_count <- tot_count - out_birth[i, ]
      }

      surv_melt <- melt(surv_tbl)
      names(surv_melt) <- c("Catch", "Birth", "Qty")
      surv_melt$Catch <- factor(surv_melt$Catch, levels = paste(rep(levels(groMixout[[sexDrop]]$Year), each = 4),
        rep(1:4, times = length(levels(groMixout[[sexDrop]]$Year))),
        sep = "_"
      ))
      surv_melt <- surv_melt[!duplicated(surv_melt[, 2:3], fromLast = TRUE), ]
      surv_melt <- surv_melt[surv_melt$Qty != 0, ]
      surv_melt$Age <- as.numeric(strtrim(surv_melt$Catch, 4)) - surv_melt$Birth + as.numeric(substr(surv_melt$Catch, 6, 7)) / 4
      surv_melt$Birth <- as.factor(surv_melt$Birth)
      surv_melt$QtyNorm <- 100 * round(as.numeric(surv_melt$Qty / apply(surv_tbl, 2, max)[surv_melt$Birth]), 2)
      # surv_melt$QtyNorm <- 100*round(as.numeric(surv_melt$Qty/max(surv_tbl)), 1)

      surv_melt$Zeta <- 0
      for (i in unique(surv_melt$Birth)) {
        tmp_surv_i <- surv_melt[surv_melt$Birth == i, ]
        surv_melt$Zeta[surv_melt$Birth == i] <- c(0, -diff(tmp_surv_i$Qty) / diff(tmp_surv_i$Age) / tmp_surv_i$Qty[1])
        # surv_melt$Zeta[surv_melt$Birth == i] <- c(0,1/diff(tmp_surv_i$Age)*log(tmp_surv_i$Qty[-nrow(tmp_surv_i)]/tmp_surv_i$Qty[-1]))
        # surv_melt$Zeta[surv_melt$Birth == i] <- c(-diff(tmp_surv_i$Qty)/tmp_surv_i$Qty[-nrow(tmp_surv_i)], 0)
      }
      # surv_melt$Zeta <- 0.2*(surv_melt$Zeta)/(1/surv_melt$Zeta)

      ### MCMC Survivors * quarter
      sprePlot[[sexDrop]][["lineSurv"]] <<- set_ggSurvLine(df_surv = surv_melt)
      cat("Done!\n", sep = "")
    },
    calcMixDate = function(nAdap = 100, nSamp = 2000, nIter = 500, sexDrop = "Female", curveSel = "von Bertalanffy") {
      cat("\n\tGetting mcmc samples... ", sep = "")
      getMCsamps(numAdap = nAdap, numSamp = nSamp, numIter = nIter, sexDrop = sexDrop, curveSel = curveSel)
      cat("Done!", sep = "")
      cat("\n\tGetting growth parameters... ", sep = "")
      getGrowPar(sexDrop = sexDrop)
      cat("Done!", sep = "")
      cat("\n\tGetting age estimates... ", sep = "")
      getMCage(sexDrop = sexDrop)
      cat("Done!", sep = "")
      setMCplot(sexDrop = sexDrop, selCurve = curveSel)
    },
    ggplotMcmcOut = function(selCompo = "MCMC", selSex = "Female") {
      switch(selCompo,
        MCMC = suppressWarnings(grid.arrange(sprePlot[[selSex]][["traceChain"]],
          sprePlot[[selSex]][["scatLK"]],
          sprePlot[[selSex]][["cohoPreciGG"]],
          sprePlot[[selSex]][["cohoVariGG"]],
          layout_matrix = rbind(
            c(1, 1, 1, 2),
            c(1, 1, 1, 2),
            c(4, 4, 5, 5)
          )
        )),
        Key = suppressWarnings(grid.arrange(sprePlot[[selSex]][["ageLength"]],
          sprePlot[[selSex]][["ageLengthTbl"]],
          sprePlot[[selSex]][["cohoStatTbl"]],
          layout_matrix = rbind(
            c(1, 1, 2),
            c(1, 1, 2),
            c(1, 1, 3)
          )
        )),
        Birth = suppressWarnings(grid.arrange(sprePlot[[selSex]][["histBirth"]],
          sprePlot[[selSex]][["lineCatch"]],
          sprePlot[[selSex]][["lineSurv"]],
          layout_matrix = rbind(
            c(1, 1),
            c(1, 1),
            c(2, 3)
          )
        ))
      )
    }
  )
)



#### FisheryBySpecie#################################################
#' FisheryBySpecie
#'
#' The \code{FisheryBySpecie} class implements the class of SMART to
#'  handle species samplings.
#'
#' @docType class
#' @usage NULL
#' @keywords data
#' @return Object of \code{\link{R6Class}} with attributes and methods for the fishery data.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field specie Name of the specie.
#' @field year Years of the time serie.
#' @field rawLFD data.frame, raw length frequency distribution.
#' @field abuAvg data.frame, average abundances by depth' stratum.
#' @field meditsIndex data.frame, medits index by depth' stratum.
#' @field lengClas numeric, length classes.
#' @field nCoho numeric, number of cohorts.
#' @field spreDist list of DF, lfd by sex.
#' @field sprePlot plots of LFD statistics.
#' @field spreSpat list of DF, spatial distribution by sex.
#' @field sampMcmc list, mcmc output chains.
#' @field groMixout list of DF, aged individuals by sex.
#' @field groPars list of DF, growth parameters by sex.
#' @field LWpar list of DF, length/weight parameters by sex.
#'
#'   #' @section Methods:
#' \describe{
#'   \item{\code{initialize(sing_spe)}}{Automatic initialization made by the
#'   SmartProject class}
#'   \item{\code{setRawData(raw_data)}}{This method is used load the initial
#'   raw dataset}
#'   \item{\code{setYears()}}{This method is used to store the years in the
#'   provided time-serie}
#'   \item{\code{setSpecie()}}{This method is used to store the name of
#'   the specie of the initial raw data}
#'   \item{\code{setLClass()}}{This method is used to store the unique
#'   length values of the sampled specie}
#'   \item{\code{setDepth(bathyMatrix)}}{This method is used to assign the
#'   depth value corresponding to each sampling location}
#'   \item{\code{setStratum(vecStrata)}}{This method is used to set the
#'   depth strata of each sampling location}
#'   \item{\code{setIndSpe()}}{This method is used to aggregate the abundance
#'   data into the medits index}
#'   \item{\code{setAbuAvg()}}{This method is used to standardize the
#'   spatial abundances by depth strata}
#'   \item{\code{setNCoho(num_coh)}}{This method is used to setup the number
#'   of cohorts for the ageing module}
#'   \item{\code{setLWpar(alphaVal, betaVal, sex)}}{This method is used to
#'   store the alpha and beta values for the length/weight relationship}
#'   \item{\code{setWeight(sexVal = "Female")}}{This method is used to
#'   compute the fish weight given their length and the LWrelationship}
#'   \item{\code{setSpreDistSing()}}{This method is used to spread the
#'   aggregated LFD abundances into single individuals}
#'   \item{\code{setSprePlot(sampSex)}}{This method is used to setup the
#'   plots of the LFD statistics}
#'   \item{\code{setSpatDistSing()}}{This method is used to setup the spatial
#'   distribution of the single specimens}
#'   \item{\code{setSpatPlot(sampSex)}}{This method is used to store the
#'   spatial plots of the population}
#'   \item{\code{getMCsamps(numSamp, numAdap, numIter, sexDrop, curveSel)}}{
#'   This method is used to get a sample of the population to feed the mcmc
#'   module}
#'   \item{\code{getGrowPar(sexDrop)}}{This method is used to extract the
#'   growth parameters from the mcmc results}
#'   \item{\code{getMCage(sexDrop)}}{This method is used to assign an age to
#'   each fish}
#'   \item{\code{setMCplot(sexDrop, selCurve)}}{This method is used to setup
#'   the plot of the mcmc results}
#'   \item{\code{calcMixDate(nAdap, nSamp, nIter, sexDrop, curveSel)}}{
#'   This method is used to estimate the growth parameters of a population}
#'   \item{\code{ggplotMcmcOut(selCompo, selSex)}}{This method is used to
#'   output the stored plots of mcmc results}
#'   }

FisheryBySpecie <- R6Class("FisheryBySpecie",
  portable = FALSE,
  class = TRUE,
  public = list(
    specie = NULL,
    year = NULL,
    rawLFD = NULL,
    lengClas = NULL,
    LFDPop = NULL,
    mixPar = NULL,
    nCoho = NULL,
    speSex = NULL,
    spreDist = list(),
    spreSpat = list(),
    sprePlot = list(),
    sampMcmc = list(),
    groMixout = list(),
    groPars = list(),
    Coh_A = NULL,
    Coh_A_Int = NULL,
    LWpar = list(),
    LWstat = NULL,
    LWstatQ = NULL,
    selPar = NULL,
    initialize = function(sing_spe) {
      setRawData(sing_spe)
      setYears()
      setSpecie()
      setLClass()
    },
    setRawData = function(raw_data) {
      rawLFD <<- raw_data
    },
    plotLFD = function() {
      plotSpeAllYea(rawLFD)
    },
    setYears = function() {
      year <<- sort(unique(years(rawLFD[, "Date"])), decreasing = FALSE)
    },
    setSpecie = function() {
      specie <<- unique(rawLFD[, "Specie"])
    },
    setLClass = function() {
      lengClas <<- seq(from = min(rawLFD[, "Class"]), to = max(rawLFD[, "Class"]), by = 1)
    },
    setNCoho = function(num_coh) {
      nCoho <<- num_coh
    },
    setLWpar = function(alphaVal, betaVal, sex) {
      LWpar[[sex]] <<- list(alpha = as.numeric(alphaVal), beta = as.numeric(betaVal))
    },
    setAvailSex = function() {
      speSex <<- sort(names(which(lapply(spreDist, nrow) > 0)))
    },
    setSprePlot = function(sampSex) {
      sprePlot[[sampSex]] <<- list(
        histLfdTot = set_ggHistLfdTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        histUtcTot = set_ggHistUtcTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        dotUtcSplit = set_ggDotUtcSplit(spreDist[[sampSex]]) + scale_color_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE"))),
        histUtcLfd = set_ggHistUtcLfd(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", ifelse(sampSex == "Male", "#63B8FF", "#63FFAE")))
      )
    },
    setSpreDistSing = function() {
      for (sex in c("Female", "Male", "Unsex")) {
        tmp_spre <- rawLFD[!is.na(rawLFD$numFG), c("Date", "Class", "numFG", sex)]

        num_sex <- sum(tmp_spre[, 4])
        cat("Found", num_sex, sex, as.character(specie), "samples\n", sep = " ")

        spreDist <- data.frame(
          UTC = rep(tmp_spre$Date, tmp_spre[, 4]),
          Length = rep(tmp_spre$Class, tmp_spre[, 4]) + runif(num_sex, -0.5, 0.5),
          NumFG = rep(tmp_spre$numFG, tmp_spre[, 4])
        )

        spreDist$Year <- years(spreDist$UTC)
        spreDist$Month <- months(as.chron(spreDist$UTC))

        spreDist[[sex]] <<- spreDist
        setSprePlot(sampSex = sex)
      }
    },
    setSpatDistSing = function() {
      for (sex in c("Female", "Male", "Unsex")) {
        tmp_fishSpat <- rawLFD[!is.na(rawLFD$numFG) & rawLFD[, sex] > 0, c("Lon", "Lat", "numFG", sex)]
        if (nrow(tmp_fishSpat) > 0) {
          barploFgAll <- data.frame(table(tmp_fishSpat$numFG))
          barploFgAll <- barploFgAll[order(as.numeric(as.character(barploFgAll[, 1]))), ]
          barploFgAll$FG <- factor(barploFgAll$Var1, levels = barploFgAll$Var1)
          barploFgAll$relFreq <- round(100 * barploFgAll$Freq / sum(barploFgAll$Freq), 1)
          spreSpat[[sex]] <<- barploFgAll
          setSpatPlot(sampSex = sex)
        }
      }
    },
    setSpatPlot = function(sampSex) {
      sprePlot[[sampSex]][["spatAbbTbl"]] <<- set_spatAbbTbl(spreSpat[[sampSex]])
      sprePlot[[sampSex]][["spatAbsFreq"]] <<- set_spatAbsFreq(spreSpat[[sampSex]])
      sprePlot[[sampSex]][["spatRelFreq"]] <<- set_spatRelFreq(spreSpat[[sampSex]])
    },
    # setPreMix = function(){
    #   # preMix <<- weight2number(rawLFD[,c("DATE","Class","Unsex")])
    # },
    setWeight = function(sexVal = "Female") {
      groMixout[[sexVal]]$Weight <<- LWpar[[sexVal]][["alpha"]] * groMixout[[sexVal]]$Length^LWpar[[sexVal]][["beta"]]
    },
    setLWstat = function(lwUnit = "Length") {
      if (length(groMixout) > 1) {
        tmp_out <- do.call(rbind, groMixout)
      } else {
        tmp_out <- groMixout[[1]]
      }
      tmp_out$Weight <- ceiling(tmp_out$Weight)
      tmp_out$Season <- factor(quarters(tmp_out$Date + 30), levels = c("1Q", "2Q", "3Q", "4Q"), labels = c("winter", "spring", "summer", "fall"))

      if (lwUnit == "Length") {
        tmp_lw <- ddply(tmp_out, .(Weight, FG), summarise,
          avgLen = mean(Length), sdLen = sd(Length), relAbb = length(Length)
        )
        tmp_lw <- tmp_lw[-which(is.na(tmp_lw), arr.ind = TRUE)[, 1], ]
      } else {
        tmp_lw <- as.data.frame(table(Weight = tmp_out$Weight, FG = tmp_out$FG), stringsAsFactors = FALSE)
        tmp_lw$Weight <- as.numeric(tmp_lw$Weight)
        tmp_lw <- tmp_lw[tmp_lw$Freq > 0, ]
      }
      LWstat <<- tmp_lw
      if (lwUnit == "Length") {
        tmp_lwq <- ddply(tmp_out, .(Weight, FG, Season), summarise,
          avgLen = mean(Length), sdLen = sd(Length), relAbb = length(Length)
        )
        tmp_lwq <- tmp_lwq[-which(is.na(tmp_lwq), arr.ind = TRUE)[, 1], ]
      } else {
        tmp_lwq <- ddply(tmp_out, .(Weight, FG, Season), summarise, Freq = length(Weight))
        tmp_lwq <- tmp_lwq[tmp_lwq$Freq > 0, ]
      }
      LWstatQ <<- tmp_lwq
    },
    getMCsamps = function(numSamp = 2000, numAdap = 100, numIter = 500, sexDrop = "Female", curveSel = "von Bertalanffy") {
      sub_idx <- sample(1:nrow(spreDist[[sexDrop]]), size = numSamp, replace = ifelse(numSamp < nrow(spreDist[[sexDrop]]), FALSE, TRUE))
      sub_data <- spreDist[[sexDrop]][sub_idx, ]

      N <- length(sub_data$Length)
      alpha <- rep(1, nCoho)
      Z <- rep(NA, N)
      Z[which.min(sub_data$Length)] <- 1
      Z[which.max(sub_data$Length)] <- nCoho

      dataList <- list(
        y = sub_data$Length,
        maxLeng = max(sub_data$Length), ## !!!
        alpha = alpha,
        Z = Z,
        N = N,
        Nclust = nCoho
      )

      inits <- list(
        list(Linf = min(sub_data$Length), k = 0.5, t0 = 0.0),
        list(Linf = mean(sub_data$Length), k = 0.5, t0 = 0.0),
        list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0)
      )

      modelGrow <- ifelse(curveSel == "von Bertalanffy",
        system.file("model/bertGrow.jags", package = "smartR"),
        system.file("model/gompGrow.jags", package = "smartR")
      )

      jags.m <- jags.model(modelGrow,
        data = dataList,
        inits = inits,
        n.chains = 3,
        n.adapt = numAdap
      )

      ### MCMC chain sampling
      # n.iter <- 500
      obsNode <- c("Linf", "k", "t0", "tau", "p")
      samps <- coda.samples(jags.m, obsNode, n.iter = numIter)

      sampMcmc[[sexDrop]] <<- samps
    },
    getGrowPar = function(sexDrop = "Female") {
      groPars[[sexDrop]]$LHat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "Linf"]))
      groPars[[sexDrop]]$kHat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "k"]))
      groPars[[sexDrop]]$t0Hat <<- mean(as.matrix(sampMcmc[[sexDrop]][, "t0"]))
      groPars[[sexDrop]]$taus <<- as.matrix(sampMcmc[[sexDrop]][, grep("tau", varnames(sampMcmc[[sexDrop]]))])
      groPars[[sexDrop]]$sigma2s <<- 1 / groPars[[sexDrop]]$taus
      groPars[[sexDrop]]$sigma2Hat <<- apply(groPars[[sexDrop]]$sigma2s, 2, mean)
    },
    getMCage = function(sexDrop = "Female") {
      tt <- as.POSIXlt(chron(spreDist[[sexDrop]]$UTC))$yday / 366

      zHat <- apply(cbind(spreDist[[sexDrop]]$Length, tt), 1, FUN = function(x) length2age(
          numCoh = nCoho,
          Linf = groPars[[sexDrop]]$LHat,
          kappa = groPars[[sexDrop]]$kHat,
          tZero = groPars[[sexDrop]]$t0Hat,
          lengthIn = x[1],
          timeIn = x[2],
          sqrtSigma = sqrt(groPars[[sexDrop]]$sigma2Hat)
        ))

      ages.f <- zHat - 1 + tt
      AA <- floor(ages.f)

      FGlabels <- as.numeric(as.character(spreDist[[sexDrop]]$NumFG))
      FGnames <- unique(FGlabels)
      FG <- numeric(length(FGlabels))
      for (FGname in 1:length(FGnames)) {
        idx_FG <- which(FGlabels == FGnames[FGname])
        FG[idx_FG] <- rep(FGname, length(idx_FG))
      }

      mix_out <- data.frame(
        Length = spreDist[[sexDrop]]$Length,
        Date = spreDist[[sexDrop]]$UTC,
        Day = tt,
        Age = AA,
        AgeNF = ages.f,
        FG = FGlabels
      )

      mix_out$Year <- years(mix_out$Date)
      mix_out$Month <- as.numeric(months(as.chron(mix_out$Date)))
      mix_out$MonthChar <- spreDist[[sexDrop]]$Month
      mix_out$Quarter <- as.numeric(quarters(mix_out$Date))
      mix_out$Birth <- as.numeric(as.character(mix_out$Year)) - mix_out$Age

      zeroedMonth <- ifelse(nchar(mix_out$Month) == 2, mix_out$Month, paste("0", mix_out$Month, sep = ""))
      mix_out$CatcDate <- factor(paste(mix_out$Year,
        zeroedMonth,
        sep = "-"
      ),
      levels = paste(rep(sort(unique(mix_out$Year)), each = 12),
        ifelse(nchar(1:12) == 2, 1:12, paste("0", 1:12, sep = "")),
        sep = "-"
      )
      )

      groMixout[[sexDrop]] <<- mix_out
    },
    setMCplot = function(sexDrop = "Female", selCurve = "von Bertalanffy") {
      nIter <- nrow(as.matrix(sampMcmc[[sexDrop]][[1]]))
      dfLinf <- data.frame(
        Parameter = "Linf",
        Iter = 1:nIter,
        Chain = as.matrix(sampMcmc[[sexDrop]][, "Linf"], chains = TRUE)[, 1],
        Value = as.matrix(sampMcmc[[sexDrop]][, "Linf"], chains = TRUE)[, 2]
      )
      dfKapp <- data.frame(
        Parameter = "Kappa",
        Iter = 1:nIter,
        Chain = as.matrix(sampMcmc[[sexDrop]][, "k"], chains = TRUE)[, 1],
        Value = as.matrix(sampMcmc[[sexDrop]][, "k"], chains = TRUE)[, 2]
      )

      ggdataSamps <- rbind(dfLinf, dfKapp)
      ggdataSampScat <- cbind(dfLinf[, 2:3],
        Linf = dfLinf[, 4],
        Kappa = dfKapp[, 4]
      )

      outPalette <- rainbow(nCoho)

      cat("\n\tSetting mcmc diagnostic plots... ", sep = "")
      ### MCMC chain Traceplot
      sprePlot[[sexDrop]][["traceChain"]] <<- set_ggChainTrace(ggdataSamps)
      ### MCMC chain scatterplot
      sprePlot[[sexDrop]][["scatLK"]] <<- set_ggChainScatter(gg_DFscat = ggdataSampScat, meanL = groPars[[sexDrop]]$LHat, meanK = groPars[[sexDrop]]$kHat)
      ### MCMC chain Boxplot Tau
      sprePlot[[sexDrop]][["cohoPreciGG"]] <<- set_ggTausBox(df_taus = groPars[[sexDrop]]$taus[, 1:(max(groMixout[[sexDrop]]$Age) + 1)], tauPalette = outPalette, numCoho = nCoho)
      ### MCMC Boxplot Sigma
      sprePlot[[sexDrop]][["cohoVariGG"]] <<- set_ggSigmaBox(df_sigma = groPars[[sexDrop]]$sigma2s[, 1:(max(groMixout[[sexDrop]]$Age) + 1)], sigPalette = outPalette, numCoho = nCoho)
      cat("Done!", sep = "")

      coho_AL <- ddply(groMixout[[sexDrop]], .(Age), summarise,
        coh.mean = mean(Length), coh.var = var(Length), coh.num = length(Length)
      )

      cat("\n\tSetting Age-Length plots... ", sep = "")
      ### MCMC Plot Age-Length
      sprePlot[[sexDrop]][["ageLength"]] <<- set_ggAgeLength(df_mix = groMixout[[sexDrop]], mixPalette = outPalette)
      ### MCMC Age-Length Key
      sprePlot[[sexDrop]][["ageLengthTbl"]] <<- set_tblAgeLength(df_mix = groMixout[[sexDrop]])
      ### MCMC output cohort stats
      sprePlot[[sexDrop]][["cohoStatTbl"]] <<- set_tblCohoStat(df_coho = coho_AL)
      cat("Done!", sep = "")

      growPath <- data.frame(
        Birth = rep(min(groMixout[[sexDrop]]$Birth):(min(groMixout[[sexDrop]]$Birth) + 11), each = length(levels(groMixout[[sexDrop]]$CatcDate))),
        Date = rep(levels(groMixout[[sexDrop]]$CatcDate), times = length(min(groMixout[[sexDrop]]$Birth):(min(groMixout[[sexDrop]]$Birth) + 11))),
        Length = NA
      )
      growPath$Age <- as.numeric(strtrim(growPath$Date, 4)) - growPath$Birth + as.numeric(substr(growPath$Date, 6, 7)) / 12

      if (selCurve == "von Bertalanffy") {
        growPath$Length <- calcVonBert(groPars[[sexDrop]]$LHat, groPars[[sexDrop]]$kHat, growPath$Age)
      } else {
        growPath$Length <- calcGomp(groPars[[sexDrop]]$LHat, groPars[[sexDrop]]$kHat, growPath$Age)
      }

      growPath$Date <- factor(growPath$Date, levels = levels(groMixout[[sexDrop]]$CatcDate))
      growPath <- growPath[growPath$Length > floor(min(groMixout[[sexDrop]]$Length)), ]

      cat("\n\tSetting Population plots... ", sep = "")
      ### MCMC quarter vertical hist
      sprePlot[[sexDrop]][["histBirth"]] <<- set_ggHistBirth(df_mix = groMixout[[sexDrop]], df_grow = growPath)

      ### MCMC calc birth
      out_birth <- table(paste(groMixout[[sexDrop]]$Year, groMixout[[sexDrop]]$Quarter, sep = "_"), groMixout[[sexDrop]]$Birth)
      birth_melt <- melt(out_birth)
      names(birth_melt) <- c("Catch", "Birth", "Qty")
      birth_melt$Catch <- factor(birth_melt$Catch, levels = paste(rep(levels(groMixout[[sexDrop]]$Year), each = 4),
        rep(1:4, times = length(levels(groMixout[[sexDrop]]$Year))),
        sep = "_"
      ))
      birth_melt$Birth <- as.factor(birth_melt$Birth)
      birth_melt <- birth_melt[birth_melt$Qty != 0, ]

      ### MCMC Catch * Quarters
      sprePlot[[sexDrop]][["lineCatch"]] <<- set_ggCatchLine(df_birth = birth_melt)

      ### MCMC Calc Survivors
      tot_count <- apply(out_birth, 2, sum)
      surv_tbl <- out_birth
      for (i in 1:nrow(out_birth)) {
        surv_tbl[i, ] <- tot_count
        tot_count <- tot_count - out_birth[i, ]
      }

      surv_melt <- melt(surv_tbl)
      names(surv_melt) <- c("Catch", "Birth", "Qty")
      surv_melt$Catch <- factor(surv_melt$Catch, levels = paste(rep(levels(groMixout[[sexDrop]]$Year), each = 4),
        rep(1:4, times = length(levels(groMixout[[sexDrop]]$Year))),
        sep = "_"
      ))
      surv_melt <- surv_melt[!duplicated(surv_melt[, 2:3], fromLast = TRUE), ]
      surv_melt <- surv_melt[surv_melt$Qty != 0, ]
      surv_melt$Age <- as.numeric(strtrim(surv_melt$Catch, 4)) - surv_melt$Birth + as.numeric(substr(surv_melt$Catch, 6, 7)) / 4
      surv_melt$Birth <- as.factor(surv_melt$Birth)
      surv_melt$QtyNorm <- 100 * round(as.numeric(surv_melt$Qty / apply(surv_tbl, 2, max)[surv_melt$Birth]), 2)
      # surv_melt$QtyNorm <- 100*round(as.numeric(surv_melt$Qty/max(surv_tbl)), 1)

      surv_melt$Zeta <- 0
      for (i in unique(surv_melt$Birth)) {
        tmp_surv_i <- surv_melt[surv_melt$Birth == i, ]
        surv_melt$Zeta[surv_melt$Birth == i] <- c(0, -diff(tmp_surv_i$Qty) / diff(tmp_surv_i$Age) / tmp_surv_i$Qty[1])
        # surv_melt$Zeta[surv_melt$Birth == i] <- c(0,1/diff(tmp_surv_i$Age)*log(tmp_surv_i$Qty[-nrow(tmp_surv_i)]/tmp_surv_i$Qty[-1]))
        # surv_melt$Zeta[surv_melt$Birth == i] <- c(-diff(tmp_surv_i$Qty)/tmp_surv_i$Qty[-nrow(tmp_surv_i)], 0)
      }
      # surv_melt$Zeta <- 0.2*(surv_melt$Zeta)/(1/surv_melt$Zeta)

      ### MCMC Survivors * quarter
      sprePlot[[sexDrop]][["lineSurv"]] <<- set_ggSurvLine(df_surv = surv_melt)
      cat("Done!\n", sep = "")
    },
    calcMixDate = function(nAdap = 100, nSamp = 2000, nIter = 500, sexDrop = "Female", curveSel = "von Bertalanffy") {
      cat("\n\tGetting mcmc samples... ", sep = "")
      getMCsamps(numAdap = nAdap, numSamp = nSamp, numIter = nIter, sexDrop = sexDrop, curveSel = curveSel)
      cat("Done!", sep = "")
      cat("\n\tGetting growth parameters... ", sep = "")
      getGrowPar(sexDrop = sexDrop)
      cat("Done!", sep = "")
      cat("\n\tGetting age estimates... ", sep = "")
      getMCage(sexDrop = sexDrop)
      cat("Done!", sep = "")
      setMCplot(sexDrop = sexDrop, selCurve = curveSel)
    },
    ggplotMcmcOut = function(selCompo = "MCMC", selSex = "Female") {
      switch(selCompo,
        MCMC = suppressWarnings(grid.arrange(sprePlot[[selSex]][["traceChain"]],
          sprePlot[[selSex]][["scatLK"]],
          sprePlot[[selSex]][["cohoPreciGG"]],
          sprePlot[[selSex]][["cohoVariGG"]],
          layout_matrix = rbind(
            c(1, 1, 1, 2),
            c(1, 1, 1, 2),
            c(4, 4, 5, 5)
          )
        )),
        Key = suppressWarnings(grid.arrange(sprePlot[[selSex]][["ageLength"]],
          sprePlot[[selSex]][["ageLengthTbl"]],
          sprePlot[[selSex]][["cohoStatTbl"]],
          layout_matrix = rbind(
            c(1, 1, 2),
            c(1, 1, 2),
            c(1, 1, 3)
          )
        )),
        Birth = suppressWarnings(grid.arrange(sprePlot[[selSex]][["histBirth"]],
          sprePlot[[selSex]][["lineCatch"]],
          sprePlot[[selSex]][["lineSurv"]],
          layout_matrix = rbind(
            c(1, 1),
            c(1, 1),
            c(2, 3)
          )
        ))
      )
    }
  )
)



#### FishFleet################################################
#' FishFleet
#'
#' The \code{FishFleet} class implements the class of SMART
#' to manage fleet data.
#'
#' @docType class
#' @usage NULL
#' @keywords data
#' @return Object of \code{\link{R6Class}} with attributes and methods for the fishery data.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field rawRegister data.frame, raw fleet register data.
#' @field vmsRegister data.frame, raw fleet register data for vms vessels only.
#' @field rawEffort list of DF, raw effort data.
#' @field dayEffoMatr list of DF, daily aggregated effort data.
#' @field prodMatr list of DF, production data.
#' @field effoProd list of DF, merged effort and production data.
#' @field effoProdMont list of DF, monthly aggregated effort and production data.
#' @field effoMont list of DF, monthly aggregated effort data.
#' @field effoProdAll data.frame, monthly aggregated effort and production data.
#' @field effoAll data.frame, monthly aggregated effort data.
#' @field regHarbsUni data.frame, Harbours name, longitude, latitude and
#' distance from the environment grid.
#' @field regHarbsBox data.frame, Harbours name, longitude, latitude, number of
#' registered vessels and distance from the environment grid within the grid
#' box.
#' @field rawProduction list of DF, raw production data.
#' @field rawEconomy data.frame, raw economic data.
#' @field registerIds character, vessel identification from fleet register.
#' @field predProd list of matrix, simulated production.
#' @field productionIds list of int, vessel ids with production data available.
#' @field prodSpec list of character, specie with production data.
#' @field specSett list of DF, logit parameter settings by specie.
#' @field specLogit list, logit results by specie.
#' @field effortIds list of int, vessel ids with effort data available.
#' @field idsEffoProd list of int, merged vessel ids with both effort and
#' production data available.
#' @field effoProdAllLoa data.frame, monthly aggregated effort, production and
#' loa data.
#' @field effoAllLoa  data.frame, monthly aggregated effort and loa data.
#' @field effortIndex data.frame, effort index by vessel, year and month with
#' loa data.
#' @field daysAtSea data.frame, days at sea index by vessel, year, month with
#' loa and Kw data.
#' @field prodIndex data.frame, production index by vessel, year and month.
#' @field resNNLS list, lander results by specie.
#' @field betaMeltYear list of DF, melted yearly productivity by specie.
#' @field prodMeltYear list of DF, melted yearly production by specie.
#' @field fishPoinPara data.frame, fishing point parameters.
#' @field ecoPrice list of DF, price/size by specie.
#' @field inSpatialReg data.frame, input for spatial index regression.
#' @field inEffortReg data.frame, input for effort index regression.
#' @field inProductionReg data.frame, input for production index regression.
#' @field outSpatialReg list, output for spatial index regression.
#' @field outEffortReg list, output for effort index regression.
#' @field outProductionReg list, output for production index regression.
#' @field plotSpatialReg ggplot, spatial index regression results.
#' @field plotEffortReg ggplot, effort index regression results.
#' @field plotProductionReg ggplot, production index regression results.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{setVmsRegister()}}{This method is used to exclude the fleet
#'   register records of vessels without vms}
#'   \item{\code{setRegHarbs()}}{This method is used to fetch the harbours
#'   coordinates}
#'   \item{\code{setEcoPrice(sel_specie, price_df)}}{This method is used to set
#'   the price/size attribute by species}
#'   \item{\code{saveFleetHarb(harb_path)}}{This method is used to export the
#'   rds with the harbours' coordinates}
#'   \item{\code{loadFleetHarb(harb_path)}}{This method is used to import the
#'   rds with the harbours' coordinates}
#'   \item{\code{loadFleetRegis(register_path)}}{This method is used to load the
#'   raw fleet register}
#'   \item{\code{loadMatEffort(effort_path)}}{This method is used to import the
#'   raw effort matrix}
#'   \item{\code{loadRawEconomy(economic_path)}}{This method is used to load the
#'   raw csv file with economic data}
#'   \item{\code{setInSpatial()}}{This method is used to setup the input for the
#'   spatial regression}
#'   \item{\code{setInEffort()}}{This method is used to setup the input for the
#'   effprt regression}
#'   \item{\code{getRegSpatial()}}{This method is used to compute the spatial
#'   regression}
#'   \item{\code{getRegEffort()}}{This method is used to compute the effort
#'   regression}
#'   \item{\code{getRegProduction()}}{This method is used to compute the
#'   production regression}
#'   \item{\code{getCostOutput()}}{This method is a wrapper function to get the
#'   economic regressions}
#'   \item{\code{setCostPlot()}}{This method is used to setup the plot of the
#'   economic regression}
#'   \item{\code{loadProduction(production_path)}}{This method is used to load
#'   the raw csv of the production data}
#'   \item{\code{setFishPoinPara(speed_range, depth_range)}}{This method is
#'   used to setup the fishign points parameters}
#'   \item{\code{setWeekMonthNum()}}{This method is used to assign the week and
#'   month num to the raw effort data}
#'   \item{\code{setFishPoin()}}{This method is used to filter the
#'   fishing points}
#'   \item{\code{plotFishPoinStat()}}{This method is used to show the basic
#'   statistics for the fishing points}
#'   \item{\code{plotSpeedDepth(which_year, speed_range, depth_range)}}{
#'   This method is used to show the speed/depth profile}
#'   \item{\code{setEffortIds()}}{This method is used to set the distinct
#'   vessel' ids in the effort dataset}
#'   \item{\code{setProdSpec()}}{This method is used to set the distinct specie
#'   in the production dataset}
#'   \item{\code{setBetaMeltYear(specie)}}{This method is used to set the melted
#'   yearly productivity by specie}
#'   \item{\code{setProdMeltYear(specie)}}{This method is used to set the melted
#'   yearly production by specie}
#'   \item{\code{plotTotProd(specie)}}{This method is used to plot the total
#'   production by specie}
#'   \item{\code{plotNNLS(specie, thresR2)}}{This method is used to show the
#'   NNLS results}
#'   \item{\code{setSpecSettItm(specie, thresh, brea, max_xlim)}}{
#'   This method is used to set the logit parameters by specie}
#'   \item{\code{plotLogitROC(selSpecie)}}{This method is used to show the
#'   ROC of the logit results}
#'   \item{\code{setSpecLogitConf(selSpecie, cutoff)}}{This method is used to
#'   set the confusion matrix of the logit results by specie}
#'   \item{\code{setLogitTrain(selSpecie, train, cp_val, cv_val)}}{
#'   This method is used to setup the train dataset for the logit model}
#'   \item{\code{setLogitTest(selSpecie, test)}}{This method is used to setup
#'   the test dataset for the logit model}
#'   \item{\code{setLogitPred(selSpecie, test)}}{This method is used to compute
#'   the prediction for the logit model}
#'   \item{\code{setLogitCut(selSpecie)}}{This method is used to tune the
#'   cutoff of the logit model}
#'   \item{\code{setLogitRoc(selSpecie)}}{This method is used to set the ROC of
#'   the logit model}
#'   \item{\code{setLogitConf(selSpecie, test)}}{This method is used to
#'   set the confusion matrix of the logit results}
#'   \item{\code{setSpecLogit(selSpecie, selModel, cp, cv)}}{This method is
#'    a wrapper function to get the logit model}
#'   \item{\code{getMatSpeLand(specie)}}{This method is used to get the input
#'   data for the logit model}
#'   \item{\code{setEffoProdAll()}}{This method is used to combine the
#'   effort/production data from the yearly list into a single data.frame}
#'   \item{\code{setEffoAll()}}{This method is used to combine the
#'   effort data from the yearly list into a single data.frame}
#'   \item{\code{setEffoProdAllLoa()}}{This method is used to add the LOA data
#'   to the effort/production data}
#'   \item{\code{setEffoAllLoa()}}{This method is used to add the LOA data
#'   to the effort data}
#'   \item{\code{setProdIds()}}{This method is used to get the vessel ids with
#'   production data available}
#'   \item{\code{setIdsEffoProd()}}{This method is used to get the vessel ids
#'    with both effort and production data available}
#'   \item{\code{plotCountIDsEffoProd()}}{This method is used to set the plot of
#'   the basic statistics of the effort/production data}
#'   \item{\code{plotCountIDsEffo()}}{This method is used to set the plot of
#'   the basic statistics of the effort data}
#'   \item{\code{plotCountIDsProd()}}{This method is used to set the plot of
#'   the basic statistics of the production data}
#'   \item{\code{setEffoProdMatr()}}{This method is used to merge the effort
#'   and production data}
#'   \item{\code{setEffoProdMont()}}{This method is used to aggregate the
#'   effort/production data by month}
#'   \item{\code{setEffoMont()}}{This method is used to aggregate the
#'   effort data by month}
#'   \item{\code{setProdMatr()}}{This method is used to create the production
#'   matrix from the raw production data}
#'   \item{\code{setDayEffoMatrGround(maxFG)}}{This method is used to assign
#'   the fishing grounds to the raw effort data}
#'   \item{\code{readRegisterEU(reg_path)}}{This method is used to load the
#'   raw european fleet register}
#'   \item{\code{cleanRegister()}}{This method is used to clean the raw data in
#'   the fleet register}
#'   \item{\code{plotRegSum()}}{This method is used to plot the basic statistics
#'   for the fleet register data}
#'   \item{\code{setRegIds()}}{This method is used to get the distinct vessels
#'   ids in the fleet register}
#'   }

FishFleet <- R6Class("fishFleet",
  portable = FALSE,
  class = TRUE,
  public = list(
    rawRegister = NULL,
    vmsRegister = NULL,
    rawEffort = NULL,
    weekEffoMatr = NULL,
    dayEffoMatr = NULL,
    prodMatr = NULL,
    effoProd = NULL,
    effoProdMont = NULL,
    effoMont = NULL,
    effoProdAll = NULL,
    effoAll = NULL,
    trackHarbs = NULL,
    regHarbsUni = NULL,
    regHarbsBox = NULL,
    rawSelectivity = NULL,
    rawProduction = NULL,
    rawEconomy = NULL,
    registerIds = NULL,
    predProd = NULL,
    productionIds = NULL,
    prodIdsLoa = NULL,
    prodSpec = NULL,
    specSett = NULL,
    specLogit = NULL,
    effortIds = NULL,
    idsEffoProd = NULL,
    effoProdAllLoa = NULL,
    effoAllLoa = NULL,
    effortIndex = NULL,
    daysAtSea = NULL,
    prodIndex = NULL,
    resNNLS = NULL,
    betaAvg = NULL,
    effortAvg = NULL,
    betaMeltYear = NULL,
    prodMeltYear = NULL,
    fishPoinPara = NULL,
    ecoPrice = NULL,
    inSpatialReg = NULL,
    inEffortReg = NULL,
    inProductionReg = NULL,
    outSpatialReg = NULL,
    outEffortReg = NULL,
    outProductionReg = NULL,
    plotSpatialReg = NULL,
    plotEffortReg = NULL,
    plotProductionReg = NULL,
    setVmsRegister = function() {
      vmsRegister <<- suppressWarnings(rawRegister[as.numeric(substr(rawRegister$CFR, 4, nchar(rawRegister$CFR[1]))) %in% effortIds$All, ])
    },
    setRegHarbs = function() {
      cat("\n\tGetting Harbours coordinates...\n", cat = "")
      regHarbsUni <<- nominatim_osm(address = sort(unique(vmsRegister$Port.Name)))
      cat("\n\t\tHarbours geocoding completed!", cat = "")
    },
    setEcoPrice = function(sel_specie, price_df) {
      if (is.null(ecoPrice)) ecoPrice <<- list()
      ecoPrice[[sel_specie]] <<- price_df
    },
    saveFleetHarb = function(harb_path) {
      saveRDS(object = regHarbsUni, file = harb_path)
    },
    loadFleetHarb = function(harb_path) {
      regHarbsUni <<- readRDS(file = harb_path)
    },
    setBetaAvg = function(sel_specie) {
      tmp_df <- data.frame(
        Month = resNNLS[[sel_specie]]$SceMat$MONTH,
        resNNLS[[sel_specie]]$bmat
      )
      betaAvg <<- aggregate(. ~ Month, tmp_df, mean)
    },
    setEffortAvg = function() {
      tmp_effo <- aggregate(. ~ Year + MonthNum, effoAllLoa[, -c(2, ncol(effoAllLoa))], sum)
      effortAvg <<- aggregate(. ~ MonthNum, tmp_effo[, -1], mean)
    },
    loadFleetRegis = function(register_path) {
      cat("\nLoading raw Fleet Register data... ", sep = "")
      rawRegister <<- readRegisterEU(register_path)
      cat("\nFleet Register loading completed!", sep = "")
    },
    loadMatEffort = function(effort_path) {
      cat("\nLoading Effort data... ", sep = "")
      rawEffort <<- readRDS(effort_path)
      cat("Done!", sep = "")
    },
    loadRawEconomy = function(economic_path) {
      cat("\nLoading Economic data... ", sep = "")
      rawEconomy <<- read.csv(file = economic_path, stringsAsFactors = FALSE)
      cat("Done!", sep = "")
    },
    setYearEconomy = function() {
      rawEconomy$Year <<- as.numeric(substr(rawEconomy$DateStart, 7, nchar(rawEconomy$DateStart)))
    },
    setInSpatial = function() {
      agg_EffInd <- aggregate(EffInd ~ I_NCEE + Year + Loa, effortIndex, sum)
      tmp_spatCost <- rawEconomy[, c("VessID", "Year", "SpatialCost")]
      inSpatialReg <<- merge(
        x = tmp_spatCost, y = agg_EffInd,
        by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year")
      )
    },
    setInEffort = function() {
      agg_DaysAtSea <- aggregate(Freq ~ I_NCEE + Year + Loa + Kw, daysAtSea, sum)
      tmp_effoCost <- rawEconomy[, c("VessID", "Year", "EffortCost")]
      inEffortReg <<- merge(
        x = tmp_effoCost, y = agg_DaysAtSea,
        by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year")
      )
    },
    getRegSpatial = function() {
      outSpatialReg <<- lm(formula = SpatialCost ~ EffInd + Loa - 1, data = inSpatialReg)
    },
    getRegEffort = function() {
      outEffortReg <<- lm(formula = EffortCost ~ Freq + Loa + Kw - 1, data = inEffortReg)
    },
    getRegProduction = function() {
      outProductionReg <<- lm(formula = ProductionCost ~ Production - 1, data = inProductionReg)
    },
    getCostOutput = function() {
      getRegSpatial()
      getRegEffort()
      getRegProduction()
    },
    setCostPlot = function() {
      plotSpatialReg <<- ggplot_spatialRegression(df_spatialIn = inSpatialReg, reg_spatialOut = outSpatialReg)
      plotEffortReg <<- ggplot_effortRegression(df_effortIn = inEffortReg, reg_effortOut = outEffortReg)
      plotProductionReg <<- ggplot_productionRegression(df_productionIn = inProductionReg, reg_productionOut = outProductionReg)
    },
    loadProduction = function(production_path) {
      cat("\nLoading Production data... ", sep = "")
      sort_files <- sort(production_path)
      rawProduction <<- list()
      for (i in 1:length(sort_files)) {
        tmp_mat <- read.csv(sort_files[i], stringsAsFactors = FALSE)
        numYea <- as.character(sort(unique(years(tmp_mat$UTC_S))))
        for (yea in 1:length(numYea)) {
          if (is.null(rawProduction[[numYea[yea]]])) {
            rawProduction[[numYea[yea]]] <<- tmp_mat[years(tmp_mat$UTC_S) == numYea[yea], ]
          } else {
            rawProduction[[numYea[yea]]] <<- rbind(rawProduction[[numYea[yea]]], tmp_mat[years(tmp_mat$UTC_S) == numYea[yea], ])
          }
        }
      }
      cat("Done!", sep = "")
    },
    setFishPoinPara = function(speed_range, depth_range) {
      fishPoinPara <<- data.frame(
        min_spe = speed_range[1],
        max_spe = speed_range[2],
        min_dep = depth_range[1],
        max_dep = depth_range[2]
      )
    },
    setWeekMonthNum = function() {
      cat("\nAdding week and month number to year ", sep = "")
      for (j in names(rawEffort)) {
        cat(j, "... ", sep = "")
        tmp_dat <- rawEffort[[j]][, c("DATE")]
        tmp_date <- as.Date(chron(tmp_dat))
        rawEffort[[j]]$WeekNum <<- as.numeric(format(tmp_date, "%V"))
        rawEffort[[j]]$MonthNum <<- as.numeric(format(tmp_date, "%m"))
      }
      cat("Done!", sep = "")
    },
    setFishPoin = function() {
      cat("\nComputing fishing points year ", sep = "")
      for (j in names(rawEffort)) {
        cat(j, "... ", sep = "")
        tmp_dat <- rawEffort[[j]][, c("SPE", "DEPTH")]
        tmp_dat$FishSpeed <- tmp_dat$SPE >= as.numeric(fishPoinPara[1]) & tmp_dat$SPE <= as.numeric(fishPoinPara[2])
        tmp_dat$FishDepth <- tmp_dat$DEPTH <= as.numeric(fishPoinPara[3]) & tmp_dat$DEPTH >= as.numeric(fishPoinPara[4])
        rawEffort[[j]]$FishPoint <<- tmp_dat$FishSpeed & tmp_dat$FishDepth
      }
      cat("Done!", sep = "")
    },
    plotFishPoinStat = function() {
      tmp_stat <- data.frame()
      for (j in names(rawEffort)) {
        tmp_sum <- sum(rawEffort[[j]]$FishPoint)
        tmp_stat <- rbind(tmp_stat, cbind(j, c("Fishing", "Not Fishing"), rbind(tmp_sum, length(rawEffort[[j]]$FishPoint) - tmp_sum)))
      }
      colnames(tmp_stat) <- c("Year", "Status", "Value")
      rownames(tmp_stat) <- NULL
      tmp_stat$Value <- as.numeric(as.character(tmp_stat$Value))
      print(tmp_stat)
      fishStatPlot <- ggplot(data = tmp_stat, aes(x = Year, y = Value, fill = Status)) +
        geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
        ggtitle("Number of Fishing Points each Year") +
        scale_fill_manual(values = c("gainsboro", "grey50")) +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "right",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )

      print(fishStatPlot)
    },
    plotSpeedDepth = function(which_year, speed_range, depth_range) {
      tmp_dat <- rawEffort[[which_year]][, c("SPE", "DEPTH")]
      op <- par(no.readonly = TRUE)
      par(mfrow = c(2, 1), mar = c(3, 0, 2, 0))
      speed_hist <- hist(tmp_dat$SPE[which(tmp_dat$SPE <= quantile(tmp_dat$SPE, 0.99) & tmp_dat$SPE > 0)], 100, plot = FALSE)
      plot(speed_hist, xlab = "Speed", main = "")
      abline(v = speed_range[1], col = "red", lty = 2)
      abline(v = speed_range[2], col = "red", lty = 2)
      text(x = speed_range[1] + ((speed_range[2] - speed_range[1]) / 2), y = max(speed_hist$counts) / 2, labels = "FISHING", col = 2)
      title(main = paste("Speed/Depth profile of ", which_year, sep = ""))
      depth_hist <- hist(tmp_dat$DEPTH[which(tmp_dat$DEPTH >= quantile(tmp_dat$DEPTH, 0.01) & tmp_dat$DEPTH <= 0)], 100, plot = FALSE)
      plot(depth_hist, xlab = "Depth", main = "")
      abline(v = depth_range[1], col = "red", lty = 2)
      abline(v = depth_range[2], col = "red", lty = 2)
      text(x = depth_range[1] + ((depth_range[2] - depth_range[1]) / 2), y = max(depth_hist$counts) / 2, labels = "FISHING", col = 2)
      par(op)
    },
    setEffortIds = function() {
      cat("\nSetting Effort IDs x year\n", sep = "")
      effortIds <<- list()
      for (i in names(rawEffort)) {
        cat(i, "... ", sep = "")
        tmp_ids <- unique(rawEffort[[i]][, 1])
        tmp_key <- i
        effortIds[[tmp_key]] <<- tmp_ids
      }
      effortIds[["All"]] <<- unique(unlist(effortIds))
      cat("Done!\n", sep = "")
    },
    setProdSpec = function() {
      prodSpec <<- list()
      cat("\nSetting species list x year\n", sep = "")
      for (i in names(effoProdMont)) {
        cat(i, "... ", sep = "")
        prodSpec[[i]] <<- colnames(effoProdMont[[i]])[ncol(dayEffoMatr[[i]]):ncol(effoProdMont[[i]])]
        # if(i == names(prodSpec)[1]){
        #   prodSpec[["Cross"]] <<- prodSpec[[i]]
        # }else{
        #   prodSpec[["Cross"]] <<- intersect(prodSpec[["Cross"]], prodSpec[[i]])
        # }
      }
      prodSpec[["Cross"]] <<- sort(unique(unlist(prodSpec)))
      setSpecSett()
      setNNLS()
      cat("Done!\n", sep = "")
    },
    setSpecSett = function() {
      specSett <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
      names(specSett) <<- sort(prodSpec[["Cross"]])
    },
    setNNLS = function() {
      resNNLS <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
      names(resNNLS) <<- sort(prodSpec[["Cross"]])
    },
    setBetaMeltYear = function(specie) {
      tmp_df <- data.frame(
        Year = names(effoProd)[resNNLS[[specie]]$SceMat$YEAR],
        resNNLS[[specie]]$bmat
      )
      tmp_df_agg <- aggregate(. ~ Year, tmp_df, sum)
      betaMeltYear[[specie]] <<- melt(
        data = tmp_df_agg, id.vars = "Year",
        measure.vars = c(2:ncol(tmp_df_agg)),
        variable.name = "FishGround", value.name = "Productivity"
      )
    },
    setProdMeltYear = function(specie) {
      tmp_df <- data.frame(
        Year = as.character(effoAllLoa[, 1]),
        predProd[[specie]]
      )
      tmp_df_agg <- aggregate(. ~ Year, tmp_df, sum)
      prodMeltYear[[specie]] <<- melt(
        data = tmp_df_agg, id.vars = "Year",
        measure.vars = c(2:ncol(tmp_df_agg)),
        variable.name = "FishGround", value.name = "Production"
      )
    },
    plotTotProd = function(specie) {
      year_Prod <- aggregate(. ~ Year, prodMeltYear[[specie]][, c(1, 3)], sum)
      year_Prod[, 1] <- as.numeric(as.character(year_Prod[, 1]))
      tot_prod <- ggplot_TotalProduction(year_Prod)
      fg_prod <- ggplot_FGProduction(prodMeltYear[[specie]])
      grid.arrange(tot_prod,
        fg_prod,
        layout_matrix = rbind(c(1, 1, 2), c(1, 1, 2)),
        top = "Production"
      )
    },
    plotNNLS = function(specie, thresR2) {
      tmp_df <- data.frame(
        R2 = "R2",
        Values = as.numeric(resNNLS[[specie]][["nnls_r2"]])
      )
      bp <- suppressMessages(
        ggplot(tmp_df, aes(x = R2, y = Values)) +
          geom_violin(fill = "grey30", colour = "grey90", alpha = 0.05) +
          geom_boxplot(fill = "grey90", width = 0.5) +
          stat_boxplot(geom = "errorbar", width = 0.25) +
          theme(axis.text.x = element_blank()) +
          ylim(0, 1) +
          labs(title = "R^2 values") +
          geom_hline(aes(yintercept = thresR2),
            linetype = "dashed", size = 0.5, colour = "red"
          ) +
          theme_tufte(base_size = 14, ticks = F) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 14),
            axis.text.x = element_text(size = 8),
            axis.title = element_blank(),
            panel.grid = element_line(size = 0.1, linetype = 2, colour = "grey20"),
            axis.text.y = element_text(size = 10),
            axis.ticks.y = element_blank()
          )
      )
      tmp_reg <- data.frame(
        Observed = resNNLS[[specie]]$obsY,
        Fitted = resNNLS[[specie]]$fittedY
      )

      reg_p <- suppressMessages(
        ggplot(tmp_reg, aes(y = Fitted, x = Observed)) +
          geom_point(alpha = 0.25, size = 0.3) + stat_smooth(method = "lm") +
          labs(title = "Observed VS Fitted") +
          scale_x_log10() +
          scale_y_log10() +
          annotation_logticks() +
          theme_tufte(base_size = 14) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 14),
            axis.text.x = element_text(size = 8),
            axis.title = element_blank(),
            panel.grid = element_line(size = 0.1, linetype = 2, colour = "grey20"),
            axis.text.y = element_text(size = 10),
            axis.ticks.y = element_blank()
          )
      )
      suppressWarnings(
        grid.arrange(reg_p, bp, layout_matrix = rbind(c(1, 1, 2), c(1, 1, 2)))
      )
    },
    setSpecSettItm = function(specie, thresh, brea, max_xlim) {
      specSett[[specie]] <<- data.frame(
        threshold = thresh,
        breaks = brea,
        max_x = max_xlim
      )
    },
    plotLogitROC = function(selSpecie) {
      plot(specLogit[[selSpecie]]$logit$Roc,
        print.cutoffs.at = seq(0, 1, 0.1),
        text.adj = c(-0.2, 1.7), main = "Logit ROC results"
      )
    },
    setSpecLogitConf = function(selSpecie, cutoff = specLogit[[selSpecie]]$logit$Cut) {
      predict <- factor(as.numeric(specLogit[[selSpecie]]$logit$Predict > cutoff))
      truth <- factor(1 * (specLogit[[selSpecie]]$Landings[-specLogit[[selSpecie]]$logit$Split] > specSett[[selSpecie]]$threshold))
      tmp_Tbl <- table(predict, truth)
      specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(tmp_Tbl)
    },
    setLogitTrain = function(selSpecie, train, cp_val = 0.01, cv_val = 2) {
      specLogit[[selSpecie]]$logit$Model <<- switch(specLogit[[selSpecie]]$logit$Name,
        GLM = {
          glm(Target ~ ., family = binomial(logit), data = train)
        },
        CART = {
          rpart::rpart(Target ~ .,
            data = train, method = "class",
            control = rpart.control(cp = cp_val)
          )
        },
        RF = {
          caret::train(Target ~ .,
            data = train, method = "rf",
            trControl = trainControl(method = "cv", number = cv_val),
            prox = TRUE, allowParallel = TRUE, metric = "Kappa",
            maximize = TRUE
          )
        },
        NN = {   }
      )
    },
    setLogitTest = function(selSpecie, test) {
      specLogit[[selSpecie]]$logit$Predict <<- switch(specLogit[[selSpecie]]$logit$Name,
        GLM = {
          predict(specLogit[[selSpecie]]$logit$Model,
            newdata = test, type = "response"
          )
        },
        CART = {
          predict(specLogit[[selSpecie]]$logit$Model,
            newdata = test, type = "prob"
          )[, 2]
        },
        RF = {
          predict(specLogit[[selSpecie]]$logit$Model,
            newdata = test, type = "prob"
          )[, 2]
        },
        NN = {   }
      )
    },
    setLogitPred = function(selSpecie, test) {
      specLogit[[selSpecie]]$logit$Prediction <<- ROCR::prediction(specLogit[[selSpecie]]$logit$Predict, test$Target)
    },
    setLogitCut = function(selSpecie) {
      perf <- ROCR::performance(specLogit[[selSpecie]]$logit$Prediction, "acc")
      specLogit[[selSpecie]]$logit$Cut <<- perf@x.values[[1]][which.max(perf@y.values[[1]])]
    },
    setLogitRoc = function(selSpecie) {
      specLogit[[selSpecie]]$logit$Roc <<- ROCR::performance(specLogit[[selSpecie]]$logit$Prediction, "tpr", "fpr")
    },
    setLogitConf = function(selSpecie, test) {
      tryCatch(
        expr = {
          specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(
            factor(specLogit[[selSpecie]]$logit$Predict > specLogit[[selSpecie]]$logit$Cut, levels = c(FALSE, TRUE)),
            test$Target
          )
        },
        error = function(error_message) {
          message("An error has occurred, different categories to compute the confusion matrix")
          message(error_message)
        }
      )
    },
    setSpecLogit = function(selSpecie, selModel = c("GLM", "CART", "RF")[1],
                                cp = 0.01, cv = 2) {
      if (is.null(specLogit)) specLogit <<- list()
      if (is.null(specLogit[[selSpecie]])) specLogit[[selSpecie]] <<- list()

      tmp_mat <- getMatSpeLand(selSpecie)

      colnames(tmp_mat)[ncol(tmp_mat)] <- "Target"
      specLogit[[selSpecie]]$Landings <<- tmp_mat$Target
      tmp_mat$Target <- factor(tmp_mat$Target > specSett[[selSpecie]]$threshold, levels = c(FALSE, TRUE))

      split <- caret::createDataPartition(y = tmp_mat$Target, p = 0.7, list = FALSE)[, 1]
      train <- tmp_mat[split, ]
      test <- tmp_mat[-split, ]
      specLogit[[selSpecie]]$logit$Split <<- split
      specLogit[[selSpecie]]$logit$Name <<- selModel
      # Train
      setLogitTrain(selSpecie, train, cp_val = cp, cv_val = cv)
      # Test
      setLogitTest(selSpecie, test)
      # Prediction
      setLogitPred(selSpecie, test)
      # Cutoff
      setLogitCut(selSpecie)
      # ROC
      setLogitRoc(selSpecie)
      # Confusion
      setLogitConf(selSpecie, test)
    },
    getMatSpeLand = function(specie) {
      tmp_mat <- effoProdAllLoa[, c(
        3:(ncol(dayEffoMatr[[1]])),
        which(colnames(effoProdAllLoa) == "Loa"),
        which(colnames(effoProdAllLoa) == specie)
      )]
      tmp_mat$MonthNum <- as.factor(tmp_mat$MonthNum)
      return(tmp_mat)
    },
    setEffoProdAll = function() {
      cat("\nSetting Effort x Production year\n", sep = "")
      tmp_spe <- sort(prodSpec[["Cross"]])
      for (i in names(effoProdMont)) {
        cat(i, "... ", sep = "")
        tmp_nam <- colnames(effoProdMont[[i]])
        tmp_cols <- which(tmp_nam %in% tmp_spe)
        if (i == names(effoProdMont)[1]) {
          effoProdAll <<- cbind(Year = i, effoProdMont[[i]][, c(1:(ncol(dayEffoMatr[[i]]) - 1), tmp_cols[order(tmp_nam[tmp_cols])])])
        } else {
          effoProdAll <<- rbind(effoProdAll, cbind(Year = i, effoProdMont[[i]][, c(1:(ncol(dayEffoMatr[[i]]) - 1), tmp_cols[order(tmp_nam[tmp_cols])])]))
        }
      }
      cat("Done!\n", sep = "")
    },
    setEffoAll = function() {
      cat("\nSetting Effort x Year\n", sep = "")
      for (i in names(effoMont)) {
        cat(i, "... ", sep = "")
        if (i == names(effoMont)[1]) {
          effoAll <<- cbind(Year = i, effoMont[[i]])
        } else {
          effoAll <<- rbind(effoAll, cbind(Year = i, effoMont[[i]]))
        }
      }
      cat("Done!", sep = "")
    },
    setEffoProdAllLoa = function() {
      tmp_effoProd <- effoProdAll
      tmp_loa <- rawRegister[, c("CFR", "Loa")]
      tmp_loa$CFR <- substr(tmp_loa$CFR, 4, nchar(tmp_loa$CFR[1]))
      names(tmp_loa) <- c("I_NCEE", "Loa")
      tmp_allLoa <- sqldf("select * from tmp_effoProd left join (select * from tmp_loa) using (I_NCEE)")
      naFind <- which(is.na(tmp_allLoa), arr.ind = TRUE)
      if (length(naFind) > 0) {
        effoProdAllLoa <<- tmp_allLoa[-naFind[, 1], ]
      } else {
        effoProdAllLoa <<- tmp_allLoa
      }
    },
    setEffoAllLoa = function() {
      cat("\nSetting Effort LOA... ", sep = "")
      tmp_effo <- effoAll
      tmp_loa <- rawRegister[, c("CFR", "Loa")]
      tmp_loa$CFR <- substr(tmp_loa$CFR, 4, nchar(tmp_loa$CFR[1]))
      names(tmp_loa) <- c("I_NCEE", "Loa")
      tmp_Loa <- sqldf("select * from tmp_effo left join (select * from tmp_loa) using (I_NCEE)")
      naFind <- which(is.na(tmp_Loa), arr.ind = TRUE)
      if (length(naFind) > 0) {
        effoAllLoa <<- tmp_Loa[-naFind[, 1], ]
      } else {
        effoAllLoa <<- tmp_Loa
      }
      cat("Done!\n", sep = "")
    },
    setProdIds = function() {
      cat("\nSetting Production IDs x year\n", sep = "")
      productionIds <<- list()
      for (i in names(rawProduction)) {
        cat(i, "... ", sep = "")
        tmp_ids <- unique(rawProduction[[i]][, 1])
        tmp_key <- i
        productionIds[[tmp_key]] <<- tmp_ids
      }
      productionIds[["All"]] <<- unique(unlist(productionIds))
      cat("Done!\n", sep = "")
    },
    setIdsEffoProd = function() {
      ###   Set IDs cross match effort/production
      to_match <- names(effortIds)[names(effortIds) %in% names(productionIds)]
      idsEffoProd <<- list()
      for (i in to_match) {
        idsEffoProd[[i]] <<- effortIds[[i]][effortIds[[i]] %in% productionIds[[i]]]
      }
    },
    plotCountIDsEffoProd = function() {
      tmp_effo <- data.frame(
        "Year" = names(effortIds),
        "Ids" = unlist(lapply(sapply(effortIds, unique, simplify = FALSE), length)),
        "Dataset" = "Effort"
      )
      tmp_prod <- data.frame(
        "Year" = names(productionIds),
        "Ids" = unlist(lapply(sapply(productionIds, unique, simplify = FALSE), length)),
        "Dataset" = "Production"
      )
      tmp_comb <- data.frame(
        "Year" = names(idsEffoProd),
        "Ids" = unlist(lapply(sapply(idsEffoProd, unique, simplify = FALSE), length)),
        "Dataset" = "Overlap"
      )
      tmp_df <- rbind(tmp_effo, tmp_prod, tmp_comb)
      rownames(tmp_df) <- NULL
      tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids, fill = Dataset)) +
        geom_bar(position = position_dodge(), stat = "identity", alpha = 0.75) +
        geom_text(aes(y = Ids, label = Ids),
          position = position_dodge(width = 1),
          vjust = 2.5, color = "grey20"
        ) +
        scale_fill_brewer(palette = "Set2") +
        ggtitle("Count of Distinct Vessels") +
        ylab("N. of IDs") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "right",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.1, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      print(tmp_plot)
    },
    plotCountIDsEffo = function() {
      tmp_df <- data.frame(
        "Year" = names(effortIds),
        "Ids" = unlist(lapply(sapply(effortIds, unique), length))
      )
      tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
        geom_text(aes(y = Ids, label = Ids),
          position = position_dodge(width = 1),
          vjust = 2.5, color = "white"
        ) +
        ggtitle("Count of Distinct Vessels - Effort Dataset") +
        ylab("N. of IDs") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      print(tmp_plot)
    },
    plotCountIDsProd = function() {
      tmp_df <- data.frame(
        "Year" = names(productionIds),
        "Ids" = unlist(lapply(unique(productionIds), length))
      )
      tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
        geom_text(aes(y = Ids, label = Ids),
          position = position_dodge(width = 1),
          vjust = 2.5, color = "white"
        ) +
        ggtitle("Count of Distinct Vessels - Production Dataset") +
        ylab("N. of IDs") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      print(tmp_plot)
    },
    setEffoProdMatr = function() {
      effoProd <<- list()
      cat("\nCreating Effort x Production matrix\n", sep = "")
      for (i in intersect(names(dayEffoMatr), names(prodMatr))) {
        cat(i, "... ", sep = "")
        tmp_effo <- dayEffoMatr[[i]]
        tmp_prod <- prodMatr[[i]]
        effoProd[[i]] <<- sqldf("select * from tmp_effo, tmp_prod where I_NCEE = NUMUE and DATE >= UTC_S and DATE <= UTC_E")
      }
      cat("Done!\n")
    },
    setEffoProdMont = function() {
      effoProdMont <<- list()
      cat("\nMatching Effort x FG with Production\n", sep = "")
      for (i in names(effoProd)) {
        cat(i, "... ", sep = "")
        dis_vesmon <- unique(effoProd[[i]][, c("I_NCEE", "MonthNum")])
        effoProdMont[[i]] <<- data.frame(matrix(data = 0, nrow = nrow(dis_vesmon), ncol = ncol(effoProd[[i]]) - 5))
        colnames(effoProdMont[[i]]) <<- c(colnames(dayEffoMatr[[i]])[-2], colnames(prodMatr[[i]])[-c(1:4)])
        effoProdMont[[i]][, 1:2] <<- dis_vesmon
        for (j in 1:nrow(dis_vesmon)) {
          tmp_itm <- effoProd[[i]][which(effoProd[[i]]$I_NCEE == dis_vesmon[j, 1] & effoProd[[i]]$MonthNum == dis_vesmon[j, 2]), ]
          effoProdMont[[i]][j, 3:(ncol(dayEffoMatr[[i]]) - 1)] <<- apply(unique(tmp_itm[, 4:ncol(dayEffoMatr[[i]])]), 2, sum)
          tmp_prod_itm <- unique(tmp_itm[, c(ncol(dayEffoMatr[[i]]) + 1, (ncol(dayEffoMatr[[i]]) + 5):ncol(tmp_itm))])
          effoProdMont[[i]][j, (ncol(dayEffoMatr[[i]])):ncol(effoProdMont[[i]])] <<- apply(tmp_prod_itm[, 2:ncol(tmp_prod_itm)], 2, sum)
        }
      }
      cat("Done!\n")
    },
    setEffoMont = function() {
      effoMont <<- list()
      cat("\nAggregating year by month\n", sep = "")
      for (i in names(dayEffoMatr)) {
        cat(i, "... ", sep = "")
        dis_vesmon <- unique(dayEffoMatr[[i]][, c("I_NCEE", "MonthNum")])
        effoMont[[i]] <<- data.frame(matrix(data = 0, nrow = nrow(dis_vesmon), ncol = ncol(dayEffoMatr[[i]]) - 1))
        colnames(effoMont[[i]]) <<- c(colnames(dayEffoMatr[[i]])[-2])
        effoMont[[i]][, 1:2] <<- dis_vesmon
        for (j in 1:nrow(dis_vesmon)) {
          tmp_itm <- dayEffoMatr[[i]][which(dayEffoMatr[[i]]$I_NCEE == dis_vesmon[j, 1] & dayEffoMatr[[i]]$MonthNum == dis_vesmon[j, 2]), ]
          effoMont[[i]][j, 3:(ncol(dayEffoMatr[[i]]) - 1)] <<- apply(unique(tmp_itm[, 4:ncol(dayEffoMatr[[i]])]), 2, sum)
        }
      }
      cat("Done!")
    },
    setProdMatr = function() {
      prodMatr <<- list()
      for (i in names(rawProduction)) {
        tmp_prod <- rawProduction[[i]]
        tmp_prod <- tmp_prod[tmp_prod$NUMUE %in% idsEffoProd[[i]], ]
        tmp_matrix <- dcast(tmp_prod,
          NUMUE + UTC_S + UTC_E ~ SPECIES,
          fun.aggregate = sum,
          na.rm = TRUE, value.var = "KGS"
        )
        tmp_matrix <- cbind("prodID" = 1:nrow(tmp_matrix), tmp_matrix)
        prodMatr[[i]] <<- tmp_matrix
      }
    },
    setDayEffoMatrGround = function(maxFG = max(rawEffort[[1]]$FishGround[!is.na(rawEffort[[1]]$FishGround)])) {
      dayEffoMatr <<- list()
      cat("\nCreating daily effort x Fishing Ground matrix\n", sep = "")
      for (j in names(rawEffort)) {
        cat(j, "... ", sep = "")
        tmp_dat <- rawEffort[[j]][rawEffort[[j]]$FishPoint == TRUE & rawEffort[[j]]$P_INT == 1 & !is.na(rawEffort[[j]]$Cell), c("I_NCEE", "DATE", "MonthNum", "FishGround", "FishPoint")]
        tmp_dat$DATE <- ceiling(tmp_dat$DATE)
        tmp_matrix <- dcast(tmp_dat,
          formula = I_NCEE + DATE + MonthNum ~ FishGround,
          fun.aggregate = sum, na.rm = TRUE, value.var = "FishPoint"
        )
        ## points to hours: interpolation interval 10 min
        tmp_matrix[, 4:ncol(tmp_matrix)] <- tmp_matrix[, 4:ncol(tmp_matrix)] / 6
        # miss_cols <- setdiff(as.character(unique(rawEffort[[j]]$FishGround[!is.na(rawEffort[[j]]$FishGround)])),
        #                      names(tmp_matrix)[4:ncol(tmp_matrix)])
        miss_cols <- setdiff(
          1:maxFG,
          names(tmp_matrix)[4:ncol(tmp_matrix)]
        )
        if (length(miss_cols) > 0) {
          # tmp_matrix[,miss_cols] <- 0
          tmp_matrix[, paste(miss_cols)] <- 0
          # tmp_matrix <- tmp_matrix[,c(1:4, 4+order(as.numeric(names(tmp_matrix)[5:ncol(tmp_matrix)])))]
          tmp_matrix <- tmp_matrix[, c(1:3, 3 + order(as.numeric(names(tmp_matrix)[4:ncol(tmp_matrix)])))]
        }
        dayEffoMatr[[j]] <<- tmp_matrix
      }
      cat("Done!\n", sep = "")
    },
    getLoa4Prod = function() {
      if (!is.null(productionIds) & !is.null(registerIds)) {
        tmp_reg <- rawRegister[, c("CFR", "Loa")]
        tmp_reg[, 1] <- substr(tmp_reg[, 1], 4, nchar(tmp_reg[1, 1]))

        tmp_pro <- data.frame("CFR" = productionIds[["All"]])
        tmp_pro[, 1] <- paste(unlist(lapply(mapply(rep, times = 9 - nchar(tmp_pro[, 1]), x = 0), paste, collapse = "")),
          tmp_pro[, 1],
          sep = ""
        )

        prodIdsLoa <<- merge(x = tmp_pro, y = tmp_reg, by = "CFR")
      }
    },
    plotLoaProd = function() {
      tmp_tab <- table(round(prodIdsLoa[, 2]))
      tmp_df <- data.frame(
        "Length" = names(tmp_tab),
        "Count" = as.numeric(tmp_tab)
      )
      ggplot(tmp_df, aes(x = Length, y = Count)) + geom_bar(stat = "identity") +
        geom_text(aes(y = Count, label = Count),
          position = position_dodge(width = 1),
          vjust = -.5, color = "black"
        ) +
        ggtitle("Count of Distinct Vessels") +
        ylab("N. of IDs")
    },
    readRegisterEU = function(reg_path) {
      cat("\n\tChecking EU Fleet Register format...", sep = "")
      two_rows <- readLines(con = reg_path, n = 2)
      last_char <- substr(two_rows[2], nchar(two_rows[2]), nchar(two_rows[2]))
      raw_fleet <- readLines(con = reg_path, n = -1)
      if (last_char == ";") {
        cat("\n\tTrailing character found! Cleaning...", sep = "")
        tmp_flee <- paste(unlist(lapply(strsplit(raw_fleet, split = ";"), paste, collapse = ";")), collapse = "\n")
      } else {
        tmp_flee <- paste(raw_fleet, collapse = "\n")
      }
      if (substr(reg_path, nchar(reg_path) - 12, nchar(reg_path)) != "_smart-ed.csv") {
        tmp_flee <- gsub("\\;", ",", tmp_flee)
        new_path <- paste(substr(reg_path, 1, nchar(reg_path) - 4), "_smart-ed",
          substr(reg_path, nchar(reg_path) - 3, nchar(reg_path)),
          sep = ""
        )
        cat("\nWriting edited Fleet register in:\n", new_path, "\n", sep = "")
        write(tmp_flee, file = new_path)
        reg_path <- new_path
      }
      cat("\nLoading file... ", sep = "")
      re_fleet <- read.csv(reg_path, stringsAsFactors = FALSE)
      cat("OK", sep = "")
      return(re_fleet)
    },
    cleanRegister = function() {
      cat("\n\tOrdering Fleet Register by CFR... ", sep = "")
      rawRegister$CFR <<- as.character(rawRegister$CFR)
      rawRegister <<- rawRegister[order(rawRegister$CFR), ]
      rawRegister$Country.Code <<- as.character(rawRegister$Country.Code)
      setRegIds()
      rawRegister$Loa <<- as.numeric(as.character(rawRegister$Loa))
      rawRegister$Power.Main <<- as.numeric(as.character(rawRegister$Power.Main))
      cat("OK\n", sep = "")
    },
    plotRegSum = function() {
      cat("Plotting Fleet register summary statistics... ", sep = "")
      def.par <- par(no.readonly = TRUE)
      layout(matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE))
      plotBarReg(regVar = "Gear.Main.Code", title = "Main Gear")
      plotBarReg(regVar = "Gear.Sec.Code", title = "Secondary Gear")
      plotBarReg(regVar = "Hull.Material", title = "Hull Material")
      plotBoxReg(regVar = "Construction.Year", title = "Year of Construction")
      plotBoxReg(regVar = "Loa", title = "Length Overall")
      plotBoxReg(regVar = "Power.Main", title = "Power")
      par(def.par)
      cat("Completed\n", sep = "")
    },
    plotBarReg = function(regVar, p_las = 2, title = regVar) {
      barplot(table(rawRegister[, regVar]), las = p_las, main = title)
    },
    plotBoxReg = function(regVar, title = regVar) {
      boxplot(rawRegister[, regVar], main = title)
    },
    setRegIds = function() {
      registerIds <<- rawRegister$CFR
    }
  )
)



#### SampleMap################################################
#' SampleMap
#'
#' The \code{SampleMap} class implements the class of SMART
#' to control geographical data.
#'
#' @docType class
#' @usage NULL
#' @keywords data
#' @return Object of \code{\link{R6Class}} with attributes and methods for the Environmental data.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field gridPath Stores the file path to the selected Environment grid shapefile.
#' @field gridName Stores the file name of the Environment grid shapefile.
#' @field gridShp Stores the SpatialPoligon object of the Environment grid shapefile.
#' @field gridBbox Stores the bounding box coordinates of the Environment grid shapefile.
#' @field gridBboxExt Stores the extended bounding box coordinates of the Environment grid shapefile.
#' @field gridBboxSP Stores the bounding box of the Environment grid as a SpatialPoligon.
#' @field areaGrid Stores the total area covered by the Environment grid.
#' @field areaStrata Stores the area covered by depth strata.
#' @field weightStrata Stores the area covered by depth strata relative to the total area.
#' @field harbDbf Stores coordinates and names of the harbours.
#' @field bioPath Stores the file path of the Substrate map.
#' @field bioName Stores the file name of the Substrate map.
#' @field bioShp Stores the SpatialPoligon object of the Substrate map.
#' @field bioDF Stores the data.frame representation of the Substrate map.
#' @field gridPolySet Stores the PolySet object of the Environment grid.
#' @field gridFortify Stores the fortified SpatialPoligon object of the Environment grid.
#' @field nCells Stores the number of cells in the Environment grid.
#' @field griCent Stores the coordinates of the cells' centroids.
#' @field gridBathy Stores the bathymetric matrix.
#' @field centDept Stores the depth of the cells' centroids.
#' @field clusInpu Stores the input data for the spatial clustering.
#' @field clusMat Stores the results of the spatial clustering.
#' @field indSil Stores the silhouette index of the spatial clustering result.
#' @field indCH Stores the Calinski-Harabasz index of the spatial clustering result.
#' @field cutFG Stores the number of cuts for the spatial clustering.
#' @field availData Stores the names of the variables for the spatial clustering.
#' @field rawInpu Stores the raw input for the spatial clustering.
#' @field cutResult Stores the summary data of the spatial clustering result.
#' @field cutResEffo Stores the average effort data from the spatial clustering result.
#' @field cutResShp Stores the SpatialPoligon object of the spatial clustering result.
#' @field cutResShpCent Stores the coordinates of the clusters' centroids.
#' @field cutResShpFort Stores the fortified SpatialPoligon object of the spatial clustering result.
#' @field fgWeigDist Stores the weighted distance between harbours and fishing grounds.
#' @field ggBioDF Stores the plot of the Substrate map.
#' @field ggDepth Stores the plot of the Bathymetric map.
#' @field ggDepthFGbox Stores the boxplot of the depth values of each fishing ground.
#' @field ggEffoFGbox Stores the boxplot of the effort values of each fishing ground.
#' @field ggEffoFGmap Stores the plot of the Effort map.
#' @field ggBioFGmat Stores the tilemap of the substrate values of each fishing ground.
#' @field ggCutFGmap Stores the plot of the Fishing ground configuration.
#' @field ggIchFGlin Stores the plot of the Calinski-Harabasz index.
#' @field ggSilFGlin Stores the plot of the silhouette index.
#' @field ggBetaFGmap Stores the plot of the Productivity map.
#' @field ggBetaFGbox Stores the boxplot of the Productivity values of each fishing ground.
#' @field ggProdFGmap Stores the plot of the Production map.
#' @field ggProdFGbox Stores the boxplot of the Production values of each fishing ground.
#' @field ggMapFgFishery Stores the plot of the Fishery data coordinates.
#' @field ggMapFgSurvey Stores the plot of the Survey data coordinates.
#' @field gooMap Stores the satellite view of the area of study.
#' @field gooMapPlot Stores the satellite plot of the area of study.
#' @field gooGrid Stores the plot of the Environment Grid.
#' @field gooBbox Stores the plot of the Bounding Box of the Environment Grid.
#' @field sampColScale Stores the color scale for the species plots.
#' @field plotRange Stores the plot ranges for the Environmental Grid.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{setAreaGrid()}}{This method is used to compute the total area covered by the environmental grid.}
#'   \item{\code{setAreaStrata(vectorStrata)}}{This method is used to compute the area covered by each depth strata.}
#'   \item{\code{setWeightStrata()}}{This method is used to compute the area covered by each depth strata relative to the total area of the grid.}
#'   \item{\code{loadHarbDbf(dbf_path)}}{This method is used to load a dbf file of coordinates and harbours names.}
#'   \item{\code{set_ggMapFgSurvey(rawSampCoo)}}{This method is used to setup the plot of the spatial distribution of survey data.}
#'   \item{\code{set_ggMapFgFishery(rawSampCoo)}}{This method is used to setup the plot of the spatial distribution of fishery data.}
#'   \item{\code{createGridBbox()}}{This method is used to setup the bounding box of the environment grid.}
#'   \item{\code{getGooMap()}}{This method is used to retrieve the satellite view of the area of study.}
#'   \item{\code{setGooPlot()}}{This method is used to setup the base plot of the area of study.}
#'   \item{\code{setPlotRange()}}{This method is used to setup the ranges of the base plot.}
#'   \item{\code{setGooGrid()}}{This method is used to setup the plot of the environment grid.}
#'   \item{\code{plotGooGrid()}}{This method is used to plot the environment grid.}
#'   \item{\code{plotGooGridData(grid_data)}}{This method is used to plot the environment grid.}
#'   \item{\code{setSampColScale(fac_col)}}{This method is used to setup the color scale for the species' plots.}
#'   \item{\code{plotGooSpeSur(poi_data)}}{This method is used to plot the spatial distribution of the survey data}
#'   \item{\code{plotGooSpeFis(poi_data)}}{This method is used to plot the spatial distribution of the fishery data}
#'   \item{\code{setGooBbox()}}{This method is used to setup the bounding box of the environment grid.}
#'   \item{\code{plotGooBbox()}}{This method is used to plot the bounding box of the environment grid.}
#'   \item{\code{setGridPath(path2grid)}}{This method is used to store the path to the grid file.}
#'   \item{\code{setGridName()}}{This method is used to store the name of the grid file.}
#'   \item{\code{loadGridShp()}}{This method is used to load the grid file.}
#'   \item{\code{setBioPath()}}{This method is used to store the path of the seabed substrates file.}
#'   \item{\code{setBioName()}}{This method is used to store the name of the seabed substrates file.}
#'   \item{\code{loadBioShp()}}{This method is used to load the seabed substrates file.}
#'   \item{\code{addBioShp(bio_path)}}{This method is used store the path and name of the seabed file and then load the SpatialPoligon object.}
#'   \item{\code{loadBioDF(bio_path)}}{This method is used to load a Data.Frame of seabed substrates.}
#'   \item{\code{plotBioDF()}}{This method is used to plot the map of substrates.}
#'   \item{\code{setGgBioDF()}}{This method is used to setup the plot of substrates.}
#'   \item{\code{ggplotBioDF()}}{This method is used to plot the map of substrates.}
#'   \item{\code{createPolySet()}}{This method is used to store the PolySet object of the Environment grid.}
#'   \item{\code{fortifyGridShp()}}{This method is used to fortify the SpatialPolygon od the Environment grid.}
#'   \item{\code{setNumCell()}}{This method is used to setup the number of cells in the grid.}
#'   \item{\code{setGridCenter()}}{This method is used to store the coordinates of cells centroids.}
#'   \item{\code{getGridBath()}}{This method is used to retrieve the bathymetric matrix.}
#'   \item{\code{saveGridBath(bathy_path)}}{This method is used to save the bathymetric matrix to file.}
#'   \item{\code{loadGridBath(bathy_path)}}{This method is used to load the bathymetric matrix from file.}
#'   \item{\code{getCentDept()}}{This method is used to assign the depth to each cell centroids.}
#'   \item{\code{setGgDepth(isoLine)}}{This method is used to setup the bathymetric plot.}
#'   \item{\code{ggplotGridBathy()}}{This method is used to plot the bathymetric map.}
#'   \item{\code{plotSamMap(title, celCol)}}{This method is used to plot the Environment map.}
#'   \item{\code{plotCoho(abbs)}}{This method is used to plot the spatial distribution of the species.}
#'   \item{\code{setClusInpu(whiData, howData)}}{This method is used to setup the input for the spatial clustering}
#'   \item{\code{calcFishGrou(numCuts, minsize, maxsize, modeska, skater_method, nei_queen)}}{This method is used to run the spatial clustering routine}
#'   \item{\code{plotFishGrou(ind_clu)}}{This method is used to plot the fishing ground configuration}
#'   \item{\code{setCutResult(ind_clu)}}{This method is used to choose a fishing ground configuration}
#'   \item{\code{setDepthFGbox()}}{This method is used to setup the boxplot of depth by fishing ground}
#'   \item{\code{setEffoFGbox()}}{This method is used to setup the boxplot of effort by fishing ground}
#'   \item{\code{setEffoFGmap()}}{This method is used to setup the map of effort by fishing ground}
#'   \item{\code{setBioFGmat()}}{This method is used to setup the tileplot of substrate by fishing ground}
#'   \item{\code{setCutFGmap()}}{This method is used to plot the fishing ground map}
#'   \item{\code{setIchFGlin(numCut)}}{This method is used to setup the plot of the Calinski-Harabasz index}
#'   \item{\code{setSilFGlin(numCut)}}{This method is used to setup the plot of the Silhouette index}
#'   }

SampleMap <- R6Class("sampleMap",
  portable = FALSE,
  class = TRUE,
  public = list(
    gridPath = NULL,
    gridName = NULL,
    gridShp = NULL,
    gridBbox = NULL,
    gridBboxExt = NULL,
    gridBboxSP = NULL,
    areaGrid = NULL,
    areaStrata = NULL,
    weightStrata = NULL,
    harbDbf = NULL,
    bioPath = NULL,
    bioName = NULL,
    bioShp = NULL,
    bioDF = NULL,
    gridPolySet = NULL,
    gridFortify = NULL,
    nCells = NULL,
    griCent = NULL, # gCenter
    gridBathy = NULL,
    centDept = NULL,
    clusInpu = NULL, # calcfish input
    clusMat = NULL, # matrix output calcfish
    indSil = NULL, # vect clusters silhouette output calcfish
    indCH = NULL, # vect index CH output calcfish
    cutFG = NULL,
    availData = NULL,
    rawInpu = NULL,
    cutResult = NULL,
    cutResEffo = NULL,
    cutResShp = NULL,
    cutResShpCent = NULL,
    cutResShpFort = NULL,
    fgWeigDist = NULL,
    gooEnv = NULL,
    ggBioDF = NULL,
    ggDepth = NULL,
    ggDepthFGbox = NULL,
    ggEffoFGbox = NULL,
    ggEffoFGmap = NULL,
    ggBioFGmat = NULL,
    ggCutFGmap = NULL,
    ggIchFGlin = NULL,
    ggSilFGlin = NULL,
    ggBetaFGmap = NULL,
    ggBetaFGbox = NULL,
    ggProdFGmap = NULL,
    ggProdFGbox = NULL,
    ggMapFgFishery = NULL,
    ggMapFgSurvey = NULL,
    gooMap = NULL,
    gooMapPlot = NULL,
    gooGrid = NULL,
    gooBbox = NULL,
    sampColScale = NULL,
    plotRange = NULL,
    initialize = function(grid_path = "") {
      if (grid_path != "") {
        setGridPath(grid_path)
        setGridName()
        loadGridShp()
        setNumCell()
        createPolySet()
        fortifyGridShp()
        setGridCenter()
        createGridBbox()
      }
    },
    exportEnv = function() {
      envOut <- list()
      envOut$gridName <- gridName
      envOut$gridShp <- gridShp
      envOut$nCells <- nCells
      envOut$gridPolySet <- gridPolySet
      envOut$gridFortify <- gridFortify
      envOut$griCent <- griCent
      envOut$gridBbox <- gridBbox
      envOut$gridBboxExt <- gridBboxExt
      envOut$gridBboxSP <- gridBboxSP
      envOut$gooMap <- gooMap
      envOut$gooMapPlot <- gooMapPlot
      envOut$plotRange <- plotRange
      envOut$gooGrid <- gooGrid
      envOut$gooBbox <- gooBbox
      envOut$gridBathy <- gridBathy
      envOut$centDept <- centDept
      envOut$ggDepth <- ggDepth
      envOut$bioDF <- bioDF
      envOut$ggBioDF <- ggBioDF
      return(envOut)
    },
    exportFG = function() {
      fgOut <- list()
      fgOut$cutFG <- cutFG
      fgOut$rawInpu <- rawInpu
      fgOut$clusMat <- clusMat
      fgOut$indSil <- indSil
      fgOut$indCH <- indCH
      return(fgOut)
    },
    importFG = function(fgList) {
      cutFG <<- fgList$cutFG
      rawInpu <<- fgList$rawInpu
      clusMat <<- fgList$clusMat
      indSil <<- fgList$indSil
      indCH <<- fgList$indCH
    },
    setAreaGrid = function() {
      cat("\n\nComputing Total Area... ", sep = "")
      clipDept <- as.SpatialGridDataFrame(gridBathy)
      tmp_grid <- gridShp
      proj4string(tmp_grid) <- proj4string(clipDept)
      naFind <- which(is.na(over(clipDept, tmp_grid))[, 1])
      if (length(naFind) > 0) clipDept[naFind] <- NA
      clipDept <- as.bathy(clipDept)
      areaGrid <<- get.area(clipDept, level.inf = -Inf, level.sup = 0)[[1]]
      cat("Completed!", sep = "")
    },
    setAreaStrata = function(vectorStrata = c(0, 10, 100, 1000, Inf)) {
      cat("\n\nComputing Area of Strata: ", sep = "")
      clipDept <- as.SpatialGridDataFrame(gridBathy)
      tmp_grid <- gridShp
      proj4string(tmp_grid) <- proj4string(clipDept)
      naFind <- which(is.na(over(clipDept, tmp_grid))[, 1])
      if (length(naFind) > 0) clipDept[naFind] <- NA
      clipDept <- as.bathy(clipDept)
      strataList <- list()
      for (stratum in 1:(length(vectorStrata) - 1)) {
        stratum_ith <- paste(vectorStrata[stratum],
          "-",
          vectorStrata[stratum + 1],
          sep = ""
        )
        cat("\n\t", stratum_ith, "... ", sep = "")
        strataList[[stratum_ith]] <- get.area(clipDept, level.inf = -vectorStrata[stratum + 1], level.sup = -vectorStrata[stratum])
        cat("Done!", sep = "")
      }
      areaStrata <<- strataList
      cat("\nCompleted!", sep = "")
    },
    setWeightStrata = function() {
      cat("\n\nComputing Strata Weighted Area... ", sep = "")
      weightStrata <<- unlist(lapply(areaStrata, "[[", 1)) / areaGrid
      cat("Completed!")
    },
    loadHarbDbf = function(dbf_path) {
      tmp_dbf <- read.dbf(file = dbf_path)
      colnames(tmp_dbf) <- c("XCOORD", "YCOORD", "Name")
      harbDbf <<- tmp_dbf
    },
    set_ggMapFgSurvey = function(rawSampCoo) {
      if (length(cutFG) > 0) {
        if (length(gridShp@polygons) == (cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(gridShp), cell_id = 1:length(gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- cutResShpCent
        }
      } else {
        tmp_coo <- cutResShpCent
      }

      ggMapFgSurvey <<- suppressMessages(
        gooMapPlot +
          geom_polygon(
            data = cutResShpFort,
            aes(x = long, y = lat, group = group),
            colour = "grey10", size = 0.1, alpha = 0.8
          ) +
          theme_tufte(base_size = 14, ticks = F) +
          theme(
            legend.position = "right",
            panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
            axis.text.x = element_text(size = 5),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_text(size = 5),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 5),
            axis.title.y = element_text(size = 7)
          ) +
          geom_jitter(
            data = rawSampCoo[, c("Lon", "Lat", "Specie")],
            aes(x = Lon, y = Lat, shape = Specie, color = Specie),
            size = 1, width = 0.1, height = 0.1, alpha = 0.35
          ) +
          geom_point(
            data = unique(rawSampCoo[, c("Lon", "Lat")]),
            aes(x = Lon, y = Lat),
            color = "darkolivegreen1", shape = 4, size = 0.5, alpha = 0.6
          ) +
          geom_text(
            data = tmp_coo,
            aes(label = FG, x = Lon, y = Lat),
            size = 2, color = "grey72"
          )
      )
    },
    set_ggMapFgFishery = function(rawSampCoo) {
      if (length(cutFG) > 0) {
        if (length(gridShp@polygons) == (cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(gridShp), cell_id = 1:length(gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- cutResShpCent
        }
      } else {
        tmp_coo <- cutResShpCent
      }

      ggMapFgFishery <<- suppressMessages(
        gooMapPlot +
          geom_polygon(
            data = cutResShpFort,
            aes(x = long, y = lat, group = group),
            colour = "grey10", size = 0.1, alpha = 0.8
          ) +
          theme(
            legend.position = "right",
            axis.text.x = element_text(size = 5),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_text(size = 5),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 5),
            axis.title.y = element_text(size = 7)
          ) +
          geom_jitter(
            data = rawSampCoo[, c("Lon", "Lat", "Specie")],
            aes(x = Lon, y = Lat, shape = Specie, color = Specie),
            size = 1, width = 0.1, height = 0.1, alpha = 0.35
          ) +
          geom_point(
            data = unique(rawSampCoo[, c("Lon", "Lat")]),
            aes(x = Lon, y = Lat),
            color = "darkolivegreen1", shape = 4, size = 0.5, alpha = 0.6
          ) +
          geom_text(
            data = tmp_coo,
            aes(label = FG, x = Lon, y = Lat),
            size = 2, color = "grey72"
          )
      )
    },
    createGridBbox = function() {
      gridBbox <<- bbox(gridShp)
      lon_range <- extendrange(range(gridBbox[1, ], na.rm = TRUE), f = 0.1)
      lat_range <- extendrange(range(gridBbox[2, ], na.rm = TRUE), f = 0.1)
      gridBboxExt <<- c(
        left = lon_range[1],
        bottom = lat_range[1],
        right = lon_range[2],
        top = lat_range[2]
      )
      polyext <- Polygon(cbind(
        c(gridBboxExt[["left"]], gridBboxExt[["left"]], gridBboxExt[["right"]], gridBboxExt[["right"]], gridBboxExt[["left"]]),
        c(gridBboxExt[["bottom"]], gridBboxExt[["top"]], gridBboxExt[["top"]], gridBboxExt[["bottom"]], gridBboxExt[["bottom"]])
      ))

      polypolyext <- Polygons(list(polyext), "s1")

      gridBboxSP <<- SpatialPolygons(list(polypolyext))
    },
    getGooMap = function() {
      gooMap <<- ggplot() +
        borders(
          fill = "black", colour = "black",
          xlim = gridBboxExt[c(1, 3)],
          ylim = gridBboxExt[c(2, 4)]
        ) +
        coord_map("bonne",
          xlim = gridBboxExt[c(1, 3)],
          ylim = gridBboxExt[c(2, 4)],
          lat0 = mean(gridBboxExt[c(2, 4)])
        )
      setGooPlot()
      setPlotRange()
    },
    setGooPlot = function() {
      gooMapPlot <<- gooMap + xlab("Longitude") + ylab("Latitude")
    },
    setPlotRange = function() {
      plotRange <<- data.frame(
        xmin = gridBboxExt[1],
        xmax = gridBboxExt[3],
        ymin = gridBboxExt[2],
        ymax = gridBboxExt[4]
      )
    },
    setGooGrid = function() {
      gooGrid <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = ""),
        size = 0.1,
        color = "gainsboro", data = gridFortify, alpha = 0.5
      ) +
        scale_fill_manual("Case Study Cells", values = "grey50") +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("Grid") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    },
    setGooEnv = function() {
      if (is.null(gooGrid)) {
        tmp_grid <- ggplot() + geom_blank() + ggtitle("No grid Loaded")
      } else {
        tmp_grid <- gooGrid
      }
      if (is.null(ggDepth)) {
        tmp_dept <- ggplot() + geom_blank() + ggtitle("No depth Loaded")
      } else {
        tmp_dept <- ggDepth
      }
      if (is.null(ggBioDF)) {
        tmp_bioc <- ggplot() + geom_blank() + ggtitle("No seabed Loaded")
      } else {
        tmp_bioc <- ggBioDF
      }

      gooEnv <<- suppressWarnings(grid.arrange(tmp_grid,
        tmp_dept,
        tmp_bioc,
        layout_matrix = matrix(1:3, 1, 3)
      ))
    },
    plotGooEnv = function() {
      suppressWarnings(grid.draw(gooEnv))
    },
    plotGooGrid = function() {
      suppressWarnings(print(gooGrid))
    },
    plotGooGridData = function(grid_data) {
      gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID),
        fill = "grey", size = 0.2,
        color = "gainsboro", data = grid_data, alpha = 0.5
      )
    },
    setSampColScale = function(fac_col) {
      myColors <- brewer.pal(length(fac_col), "Set1")
      names(myColors) <- fac_col
      sampColScale <<- scale_colour_manual(name = "Specie", values = myColors)
    },
    plotGooSpeSur = function(poi_data) {
      temp_pos <- suppressMessages(gooGrid + geom_jitter(
        data = poi_data,
        aes(x = Lon, y = Lat, shape = Specie, color = Specie),
        width = 0.05, height = 0.05, alpha = 0.95
      ) + sampColScale)
      suppressWarnings(print(temp_pos))
    },
    plotGooSpeFis = function(poi_data) {
      temp_pos <- suppressMessages(gooGrid + geom_jitter(
        data = poi_data,
        aes(x = Lon, y = Lat, shape = Specie, color = Specie),
        width = 0.05, height = 0.05, alpha = 0.95
      ))
      # temp_pos <- gooGrid + geom_jitter(data = poi_data,
      #                             aes(x = Lon, y = Lat, shape = Specie, color = Specie),
      #                             width = 0.05, height = 0.05, alpha = 0.95)
      # print(temp_pos)
      suppressWarnings(print(temp_pos))
    },
    setGooBbox = function() {
      text_x <- mean(gridBboxExt[c(1, 3)])
      text_y <- mean(gridBboxExt[c(2, 4)])
      gooBbox <<- suppressMessages(gooGrid + geom_rect(
        data = plotRange, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        color = "firebrick",
        fill = alpha("red", 0.2),
        inherit.aes = FALSE
      ) +
        annotate("label",
          x = text_x, y = text_y,
          label = "Bounding\nBox", family = "serif", fontface = "italic",
          colour = "firebrick", size = 6, fill = "grey80"
        ) +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    },
    plotGooBbox = function() {
      suppressWarnings(print(gooBbox))
    },
    setGridPath = function(path2grid) {
      gridPath <<- path2grid
    },
    setGridName = function() {
      tmp_name <- unlist(strsplit(gridPath, "/"))
      tmp_name <- tmp_name[length(tmp_name)]
      gridName <<- substr(tmp_name, 1, nchar(tmp_name) - 4)
    },
    loadGridShp = function() {
      gridShp <<- readOGR(gridPath)
    },
    setBioPath = function(path2bio) {
      bioPath <<- path2bio
    },
    setBioName = function() {
      tmp_name <- unlist(strsplit(bioPath, "/"))
      tmp_name <- tmp_name[length(tmp_name)]
      bioName <<- substr(tmp_name, 1, nchar(tmp_name) - 4)
    },
    loadBioShp = function() {
      bioShp <<- readOGR(bioPath)
    },
    addBioShp = function(bio_path) {
      setBioPath(bio_path)
      setBioName()
      loadBioShp()
    },
    loadBioDF = function(bio_path) {
      bioDF <<- readRDS(bio_path)
      setGgBioDF()
    },
    plotBioDF = function() {
      def.par <- par(no.readonly = TRUE)
      par(mar = c(2.5, 2.5, 3, 1))
      layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(6, 1))

      vec_bio <- apply(bioDF, 1, function(x) which(x == 1))
      color_clas <- rainbow(max(vec_bio))
      plotSamMap(title = "Biocenosis", celCol = color_clas[vec_bio])

      par(mar = c(5, 1, 1, 1))
      par(mar = c(1, 0, 1, 0))

      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), bty = "n", axes = FALSE, ann = FALSE)
      legend(x = 0, y = 0.5, legend = colnames(bioDF), fill = color_clas, bty = "n")
      par(def.par)
    },
    setGgBioDF = function() {
      cell_bed <- apply(bioDF, 1, which.max)

      zero_cell <- which(apply(bioDF, 1, sum) == 0)

      tmp_dat <- colnames(bioDF)[cell_bed]

      if (length(zero_cell) > 0) tmp_dat[zero_cell] <- "No Data"

      color_clas <- rainbow(max(cell_bed))
      names(tmp_dat) <- 1:length(tmp_dat)
      all_cell <- merge(
        x = gridPolySet$PID,
        data.frame(
          x = as.numeric(names(tmp_dat)), y = tmp_dat,
          stringsAsFactors = FALSE
        ), all = TRUE
      )
      all_cell[is.na(all_cell)] <- 0
      grid_data <- cbind(gridPolySet, Seabed = all_cell[, 2])
      ggBioDF <<- suppressMessages(gooMapPlot +
        geom_polygon(aes(x = X, y = Y, group = PID, fill = Seabed),
          size = 0.2,
          data = grid_data, alpha = 0.8
        ) +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("Seabed") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    ggplotBioDF = function() {
      suppressWarnings(print(ggBioDF))
    },
    createPolySet = function() {
      gridPolySet <<- as.data.frame(SpatialPolygons2PolySet(gridShp))
    },
    fortifyGridShp = function() {
      gridFortify <<- fortify(gridShp)
    },
    setNumCell = function() {
      nCells <<- length(gridShp@polygons)
    },
    setGridCenter = function() {
      griCent <<- coordinates(gridShp) # or gCentroid from rgeos
    },
    getGridBath = function() {
      lon_ran <- extendrange(griCent[, 1], f = 0.05)
      lat_ran <- extendrange(griCent[, 2], f = 0.05)
      gridBathy <<- getNOAA.bathy(
        lon1 = lon_ran[1],
        lon2 = lon_ran[2],
        lat1 = lat_ran[1],
        lat2 = lat_ran[2], resolution = 1
      )
      getCentDept()
      setGgDepth()
    },
    saveGridBath = function(bathy_path) {
      saveRDS(object = gridBathy, file = bathy_path)
    },
    loadGridBath = function(bathy_path) {
      gridBathy <<- readRDS(bathy_path)
      getCentDept()
      setGgDepth()
    },
    getCentDept = function() {
      centDept <<- get.depth(gridBathy, x = griCent[, 1], y = griCent[, 2], locator = FALSE)
    },
    setGgDepth = function(isoLine = c(-200, -1000)) {
      f_bathy <- fortify.bathy(gridBathy)
      f_bathy$z[f_bathy$z > 0] <- 0
      colnames(f_bathy) <- c("lon", "lat", "Depth")
      ggDepth <<- suppressMessages(gooMapPlot +
        geom_contour(aes(x = lon, y = lat, z = Depth, colour = factor(..level..)),
          data = f_bathy,
          linetype = "solid", size = 0.35,
          breaks = isoLine, alpha = 1
        ) +
        scale_colour_brewer(palette = "Accent", name = "Isobath") +
        guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1))) +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("Depth") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    ggplotGridBathy = function() {
      suppressWarnings(print(ggDepth))
    },
    plotSamMap = function(title = "", celCol = NULL) {
      par(mar = c(3, 3, 1.5, 0.5))
      plotPolys(gridPolySet,
        main = title, ylab = "", xlab = "", col = celCol, axes = TRUE,
        xlim = extendrange(c(gridShp@bbox[1, 1], gridShp@bbox[1, 2]), f = 0.05),
        ylim = extendrange(c(gridShp@bbox[2, 1], gridShp@bbox[2, 2]), f = 0.05)
      )
      mtext("Longitude", side = 1, line = 2.1, cex = 1.5)
      mtext("Latitude", side = 2, line = 2, cex = 1.5)
      map("worldHires", fill = T, col = "gainsboro", add = TRUE)
      # map.axes(cex.axis=0.8)
      map.scale(cex = 0.75, ratio = FALSE)
    },
    plotCoho = function(abbs) {
      x_rang <- extendrange(c(gridShp@bbox[1, 1], gridShp@bbox[1, 2]), f = 0.05)
      y_rang <- extendrange(c(gridShp@bbox[2, 1], gridShp@bbox[2, 2]), f = 0.07)
      plotPolys(gridPolySet,
        ylab = "", xlab = "",
        xlim = x_rang,
        ylim = y_rang
      )
      mtext("Longitude", side = 1, line = 2.1, cex = 1.1)
      mtext("Latitude", side = 2, line = 2, cex = 1.1)
      smoothScatter(griCent[rep.int(1:nrow(griCent), abbs + 1), ],
        colramp = colorRampPalette(c("white", "white", "blue", "purple"), bias = 0.77, alpha = 0.5),
        add = TRUE, nbin = 250, xlab = NULL, ylab = NULL, nrpoints = 0
      )
      plot(gridShp,
        ylab = "", xlab = "",
        add = TRUE, lwd = 0.25
      )
      map("worldHires",
        fill = T, col = "gainsboro",
        xlim = x_rang,
        ylim = y_rang, add = TRUE
      )
      map.scale(x = min(abs(x_rang)) + (diff(x_rang) / 10), y = min(abs(y_rang)) + (diff(y_rang) / 7), cex = 0.65, ratio = FALSE)
    },
    setClusInpu = function(howData = rep(1, 3)) {
      tmp_lst <- list()
      cat("\n   -   Seabed")
      tmp_lst <- c(tmp_lst, list(rawInpu[[1]] * howData[1]))
      cat("\t\t-   Set!")
      cat("\n   -   Effort")
      tmp_lst <- c(tmp_lst, list(vegan::decostand(log10(1 + rawInpu[[2]]), method = "range", MARGIN = 2) * howData[2]))
      cat("\t\t-   Set!")
      cat("\n   -   Depth")
      tmp_inpu <- -rawInpu[[3]]
      tmp_inpu[tmp_inpu < 0] <- 0
      tmp_lst <- c(tmp_lst, Depth = list(vegan::decostand(log10(1 + tmp_inpu), method = "range", MARGIN = 2) * howData[3]))
      cat("\t\t-   Set!\n")
      clusInpu <<- do.call(cbind, tmp_lst)
    },
    calcFishGrou = function(numCuts = 50,
                                minsize = 10,
                                maxsize = 100,
                                modeska = "S",
                                skater_method,
                                nei_queen = TRUE) {
      set.seed(123)
      # Build the neighboorhod list
      Grid.bh <- gridShp[1]
      ##  Construct neighbours list from polygon list
      bh.nb <- poly2nb(Grid.bh, queen = nei_queen)
      bh.mat <- cbind(rep(1:length(bh.nb), lapply(bh.nb, length)), unlist(bh.nb))
      # Compute the basic objects for clustering
      ##  Cost of each edge as the distance between nodes
      lcosts <- nbcosts(bh.nb, clusInpu)
      ##  Spatial weights for neighbours lists
      nb.w <- nb2listw(bh.nb, lcosts, style = modeska)
      ##  Find the minimal spanning tree
      mst.bh <- mstree(nb.w, ini = 1)
      clusMat <<- matrix(NA, nCells, numCuts)
      cat("\nClustering", sep = "")
      res1 <- skater(
        edges = mst.bh[, 1:2],
        data = clusInpu,
        ncuts = 1,
        crit = minsize,
        method = skater_method
      )
      clusMat[, 1] <<- res1$groups

      # Perform the first CC (without removing spurious clusters)
      for (nCuts in 2:numCuts) {
        cat(".", sep = "")
        ##  Spatial 'K'luster Analysis by Tree Edge Removal
        #                            res1 <- skater(mst.bh[,1:2], cells_data, ncuts = nCuts, minsize,
        #                                           method = skater_method)

        if (nCuts > ceiling(nrow(clusInpu) / maxsize)) {
          res1 <- skater(res1, clusInpu,
            ncuts = 1,
            crit = c(minsize, maxsize),
            method = skater_method
          )
        } else {
          res1 <- skater(res1, clusInpu,
            ncuts = 1,
            crit = minsize,
            method = skater_method
          )
        }
        clusMat[, nCuts] <<- res1$groups
      }
      cat(" Done!", sep = "")

      indSil <<- numeric(ncol(clusMat))
      indCH <<- numeric(ncol(clusMat))
      for (i in 1:ncol(clusMat)) {
        ##  Compute silhouette information according to a given clustering in k clusters
        indSil[i] <<- summary(silhouette(clusMat[, i], dist(clusInpu,
          method = skater_method
        )))$avg.width
        ##  Compute CH index for a given partition of a data set
        indCH[i] <<- get_CH(as.matrix(clusInpu), clusMat[, i])
      }
    },
    plotFishGrou = function(ind_clu) {
      def.par <- par(no.readonly = TRUE)
      layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE), c(1, 3), TRUE)
      plot(indCH, type = "l", ann = FALSE)
      title("CH")
      abline(v = ind_clu, col = "red", lty = 2)
      plot(indSil, type = "l", ann = FALSE)
      title("Silhouette")
      abline(v = ind_clu, col = "red", lty = 2)
      plotSamMap(
        title = paste("Max Width with ", ind_clu, " cuts", sep = ""),
        celCol = (rainbow(length(unique(clusMat[, ind_clu]))))[clusMat[, ind_clu]]
      )
      par(def.par)
    },
    # setCutResShpCent = function(){
    #   cutResShpCent <<- getLabBuffer(cutResShp)
    #   cutResShpCent$id <<- rownames(cutResShpCent)
    #   names(cutResShpCent) <<- c("Lon", "Lat", "FG")
    # },
    setCutResult = function(ind_clu) {
      # cutResult <<- data.frame(clusInpu, FG = as.factor(clusMat[,ind_clu]))
      cutResult <<- data.frame(do.call(cbind, rawInpu), FG = as.factor(clusMat[, ind_clu]))
      cutResEffo <<- data.frame(
        Effort = apply(cutResult[, grep("Year", colnames(cutResult))], 1, mean),
        Cluster = cutResult[, ncol(cutResult)]
      )
      cutResShp <<- unionSpatialPolygons(gridShp, IDs = clusMat[, ind_clu])

      cutResShp@plotOrder <<- 1:ind_clu

      # cutResShpCent <<- as.data.frame(coordinates(cutResShp))
      # cutResShpCent <<- as.data.frame(getLabBuffer(cutResShp))
      plot.new()
      cutResShpCent <<- as.data.frame(polygonsLabel(cutResShp, method = "inpolygon", doPlot = FALSE))
      # cutResShpCent$id <<- rownames(cutResShpCent)
      cutResShpCent$id <<- names(cutResShp)
      names(cutResShpCent) <<- c("Lon", "Lat", "FG")

      cutResShpFort <<- fortify(cutResShp)
      cutResShpFort$FG <<- as.factor(cutResShpFort$id)

      setDepthFGbox()
      setEffoFGbox()
      setEffoFGmap()
      setBioFGmat()
      setCutFGmap()
      setIchFGlin(numCut = ind_clu)
      setSilFGlin(numCut = ind_clu)
    },
    setDepthFGbox = function() {
      ggDepthFGbox <<- suppressMessages(ggplot(cutResult, aes(x = FG, y = Depth, group = FG)) +
        geom_boxplot(color = "grey23") +
        coord_flip() +
        xlab("Fishing Ground") +
        theme_tufte(base_size = 14, ticks = T) +
        ylim(NA, 0) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    setEffoFGbox = function() {
      ggEffoFGbox <<- suppressMessages(ggplot(cutResEffo, aes(x = Cluster, y = Effort, group = Cluster)) +
        geom_boxplot(color = "grey23") +
        coord_flip() +
        xlab("Fishing Ground") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
    },
    setEffoFGmap = function() {
      agg_eff <- aggregate(formula = Effort ~ Cluster, data = cutResEffo, FUN = mean)
      all_cell <- merge(
        x = cutResShpFort$id,
        data.frame(x = agg_eff$Cluster, y = agg_eff$Effort), all = TRUE
      )
      all_cell[is.na(all_cell)] <- 0
      grid_data <- cbind(cutResShpFort, Hours = all_cell[, 2])

      if (length(cutFG) > 0) {
        if (length(gridShp@polygons) == (cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(gridShp), cell_id = 1:length(gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- cutResShpCent
        }
      } else {
        tmp_coo <- cutResShpCent
      }

      ggEffoFGmap <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = Hours),
        colour = "black", size = 0.1,
        data = grid_data, alpha = 0.8
      ) +
        scale_fill_gradient(low = "Yellow", high = "coral", trans = "sqrt") +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        ggtitle("Average Effort Intensity") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    },
    setBioFGmat = function() {
      # ind_col <- which(make.names(colnames(bioDF)) %in% colnames(cutResult))
      # if(length(ind_col) > 0){
      # tmp_bio <- data.frame(FG = cutResult$FG, cutResult[,ind_col])
      tmp_bio <- data.frame(FG = cutResult$FG, bioDF)
      bio2plot <- melt(tmp_bio, id.vars = "FG", variable.name = "Substrate")
      bio2plot <- bio2plot[bio2plot$value == 1, 1:2]
      ggBioFGmat <<- suppressMessages(ggplot(bio2plot, aes(x = FG, y = Substrate, fill = Substrate)) +
        geom_tile() +
        coord_flip() +
        annotate("text",
          colour = "grey30", y = 1:length(levels(bio2plot$Substrate)),
          x = rep(4, length(levels(bio2plot$Substrate))),
          label = levels(bio2plot$Substrate),
          angle = rep(90, length(levels(bio2plot$Substrate)))
        ) +
        theme_tufte(base_size = 14, ticks = T) +
        xlab("Fishing Ground") +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10, colour = "white"),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        ))
      # }
    },
    setCutFGmap = function() {
      if (length(cutFG) > 0) {
        if (length(gridShp@polygons) == (cutFG + 1)) {
          tmp_coo <- data.frame(coordinates(gridShp), cell_id = 1:length(gridShp))
          colnames(tmp_coo) <- c("Lon", "Lat", "FG")
        } else {
          tmp_coo <- cutResShpCent
        }
      } else {
        tmp_coo <- cutResShpCent
      }

      ggCutFGmap <<- suppressMessages(gooMapPlot +
        geom_polygon(aes(x = long, y = lat, group = group, fill = FG),
          colour = "black", size = 0.1,
          data = cutResShpFort, alpha = 0.8
        ) +
        geom_text(aes(label = FG, x = Lon, y = Lat),
          data = tmp_coo, size = 2
        ) +
        scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(length(unique(cutResShpFort$FG)))) +
        ggtitle("Regions") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    },
    setIchFGlin = function(numCut) {
      ch_df <- data.frame(
        cut = 1:length(indCH),
        CH_index = indCH
      )
      ggIchFGlin <<- suppressMessages(ggplot(ch_df, aes(x = cut, y = CH_index)) +
        geom_line() +
        geom_vline(aes(xintercept = numCut),
          linetype = "dashed", size = 0.5, colour = "red"
        ) +
        ggtitle("Calinski-Harabasz") +
        ylab("Index") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_blank(),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    },
    setSilFGlin = function(numCut) {
      sil_df <- data.frame(
        cut = 1:length(indSil),
        Sil_index = indSil
      )
      ggSilFGlin <<- suppressMessages(ggplot(sil_df, aes(x = cut, y = Sil_index)) +
        geom_line() +
        geom_vline(aes(xintercept = numCut),
          linetype = "dashed", size = 0.5, colour = "red"
        ) +
        ggtitle("Silhouette") +
        ylab("Index") +
        theme_tufte(base_size = 14, ticks = T) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(size = 8),
          axis.title.x = element_blank(),
          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10)
        ))
    }
  )
)
