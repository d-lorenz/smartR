####SmartProject#############################################
#' SmartProject
#'
#' The \code{SmartProject} class implements the main class of
#'  SMART.
#'
#' @return This function returns the 'smartProject' object.
#'
#'
#############################################################

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
                          sampMap = NULL,
                          fleet = NULL,
                          setCostInput = function(){
                            if(is.null(fleet$effortIndex)) stop("Missing Effort Index")
                            if(is.null(fleet$daysAtSea)) stop("Missing Days at Sea Index")
                            if(is.null(fleet$effoAllLoa)) stop("Missing Production Index")
                            fleet$setInSpatial()
                            fleet$setInEffort()
                            setInProduction()
                          },
                          setInProduction = function(){
                            tmp_Prod <- data.frame(Year = fleet$effoProdAllLoa$Year,
                                                   I_NCEE = fleet$effoProdAllLoa$I_NCEE,
                                                   MonthNum = fleet$effoProdAllLoa$MonthNum,
                                                   Production = apply(fleet$effoProdAllLoa[,(4+sampMap$cutFG+1):ncol(fleet$effoProdAllLoa)],1, sum))
                            agg_Prod <- aggregate(Production ~ I_NCEE + Year, tmp_Prod, sum)
                            tmp_effoCost <- rawEconomy[,c("VessID", "Year", "ProductionCost")]
                            fleet$inProductionReg <<- merge(x = tmp_effoCost, y = agg_Prod,
                                                  by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year"))
                          },
                          setDaysAtSea = function(){
                            cat("\nProcessing year: ", sep = "")
                            for(year in names(fleet$rawEffort)){
                              cat(year, "... ", sep = "")
                              tmp_vmsY <- fleet$rawEffort[[year]][,c("I_NCEE", "DATE", "MonthNum")]
                              tmp_vmsY$DATE <- floor(tmp_vmsY$DATE)
                              tmp_vmsY <- tmp_vmsY[!duplicated(tmp_vmsY),]
                              out_tbl <- table(I_NCEE = tmp_vmsY$I_NCEE, Month = tmp_vmsY$MonthNum)
                              if(year == names(fleet$rawEffort)[1]){
                                tmp_days <- data.frame(effYear = year, out_tbl)
                              }else{
                                tmp_days <- rbind(tmp_days,
                                                  data.frame(effYear = year, out_tbl))
                              }
                            }
                            fleet$daysAtSea <<- suppressWarnings(merge(tmp_days, data.frame(I_NCEE = as.numeric(substr(fleet$rawRegister$CFR, 4, nchar(fleet$rawRegister$CFR))),
                                                                                            Loa = fleet$rawRegister$Loa,
                                                                                            Kw = fleet$rawRegister$Power.Main), all.x = TRUE))
                            cat(" Completed!", sep = "")
                          },
                          setEffortIndex = function(){
                            cat("\n\nComputing Effort Index... ", sep = "")
                            if(is.null(sampMap$fgWeigDist)) stop("Missing Harbours Weighted Distance")
                            if(is.null(sampMap$cutFG)) stop("Missing Fishing Grounds")
                            if(is.null(fleet$effoAllLoa)) stop("Missing Effort-LOA data")
                            tmp_ei <- apply(data.frame(mapply(`*`,
                                                              fleet$effoAllLoa[, 4:(sampMap$cutFG+4)],
                                                              sampMap$fgWeigDist)), 1, sum)
                            fleet$effortIndex <<- data.frame(fleet$effoAllLoa[,c(1:3,ncol(fleet$effoAllLoa))],
                                                             EffInd = tmp_ei)
                            cat("Completed!", sep = "")
                          },
                          setProductionIndex = function(){
                            fleet$prodIndex <<- data.frame(Year = fleet$effoProdAllLoa$Year,
                                                          I_NCEE = fleet$effoProdAllLoa$I_NCEE,
                                                          MonthNum = fleet$effoProdAllLoa$MonthNum,
                                                          Production = apply(fleet$effoProdAllLoa[,(4+sampMap$cutFG+1):ncol(fleet$effoProdAllLoa)],1, sum))
                          },
                          getHarbFgDist = function(){
                            cat("\nRunnnig Fishing ground average distances routine... ", cat = "")
                            fleet$setRegHarbs()
                            setRegHarbBox()
                            setFgWeigDist()
                            cat("\nFishing ground average distances routine completed!", cat = "")
                          },
                          setFgWeigDist = function(){
                            cat("\n\tSetting Fishing ground weighted distances... ", cat = "")
                            harb_fg_dist <- spDists(y = as.matrix(sampMap$cutResShpCent[,1:2]),
                                                    x = as.matrix(fleet$regHarbsBox[,2:3]), longlat = TRUE)
                            dimnames(harb_fg_dist) <- list(as.character(fleet$regHarbsBox$Name),
                                                           paste("FG", sampMap$cutResShpCent$FG, sep = ""))
                            harb_fg_dist <- data.frame(harb_fg_dist)
                            harb_wei_dist <- numeric(length = ncol(harb_fg_dist))
                            names(harb_wei_dist) <- names(harb_fg_dist)
                            for(i in 1:ncol(harb_fg_dist)){
                              harb_wei_dist[i] <- weighted.mean(harb_fg_dist[,i], fleet$regHarbsBox$relFreq)
                            }
                            sampMap$fgWeigDist <<- harb_wei_dist[order(as.numeric(substr(names(harb_wei_dist),3, nchar(names(harb_wei_dist)))))]
                            cat(" OK!", cat = "")
                          },
                          setRegHarbBox = function(){
                            cat("\n\tComputing Harbours-FishingGround distances...", cat = "")
                            tmp_dist <- gDistance(sampMap$gridShp,
                                                  SpatialPoints(fleet$regHarbsUni[,2:3]),
                                                  byid = TRUE)
                            fleet$regHarbsUni$shpDist <<- apply(tmp_dist,1,min)
                            fleet$regHarbsBox <<- fleet$regHarbsUni[fleet$regHarbsUni$shpDist < 0.5,]
                            harb_cur_box <- as.data.frame(table(fleet$vmsRegister[fleet$vmsRegister$Port.Name %in% fleet$regHarbsBox$Name,]$Port.Name))
                            colnames(harb_cur_box) <- c("Name", "absFreq")
                            harb_cur_box$relFreq <- harb_cur_box$absFreq/sum(harb_cur_box$absFreq)
                            fleet$regHarbsBox <<- merge(fleet$regHarbsBox, harb_cur_box)
                            cat(" OK!", cat = "")
                          },
                          loadSurveyLFD = function(csv_path) {
                            cat("\nLoading survey data...", sep = "")
                            rawDataSurvey <<- read.table(file = csv_path, sep = ";", dec = ".", colClasses = c("character", "numeric", "numeric", "factor", "numeric", "numeric", "numeric", "numeric"), header = TRUE)

                            surveyBySpecie <<- list()

                            cat("\nSetting Years... ", sep = "")
                            setYearSurvey()
                            cat(" from ", min(levels(yearInSurvey)[as.numeric(yearInSurvey)]), " to ", max(levels(yearInSurvey)[as.numeric(yearInSurvey)]),"\nSetting Species... ", sep = "")
                            setSpecieSurvey()
                            cat(" found: ", paste(specieInSurvey, collapse = " - "), "\nSplitting Species...", sep = "")
                            splitSpecieSurvey()
                            cat(" completed!", sep = "")
                          },
                          loadFisheryLFD = function(csv_path) {
                            cat("\nLoading fishery data...", sep = "")
                            rawDataFishery <<- read.table(file = csv_path, sep = ";", dec = ".", header = TRUE)

                            fisheryBySpecie <<- list()
                            cat("\nSetting Years... ", sep = "")
                            setYearFishery()
                            cat(" from ", min(levels(yearInFishery)[as.numeric(yearInFishery)]), " to ", max(levels(yearInFishery)[as.numeric(yearInFishery)]),"\nSetting Species... ", sep = "")
                            setSpecieFishery()
                            cat(" found: ", paste(specieInFishery, collapse = " - "), "\nSplitting Species...", sep = "")
                            splitSpecieFishery()
                            cat(" completed!", sep = "")
                          },
                          setYearSurvey = function(){yearInSurvey <<- sort(unique(rawDataSurvey[,"Year"]), decreasing = FALSE)},
                          setYearFishery = function(){yearInFishery <<- sort(unique(years(rawDataFishery[,"Date"])), decreasing = FALSE)},
                          loadMap = function(map_path){sampMap <<- SampleMap$new(map_path)},
                          createFleet = function(){fleet <<- FishFleet$new()},
                          setSpecieSurvey = function(){specieInSurvey <<- unique(rawDataSurvey[,"Specie"])},
                          setSpecieFishery = function(){specieInFishery <<- unique(rawDataFishery[,"Specie"])},
                          splitSpecieSurvey = function(){
                            if(length(specieInSurvey) == 1){
                              addSpecieSurvey(rawDataSurvey)
                            }else{
                              for(i in 1:length(specieInSurvey)){
                                addSpecieSurvey(rawDataSurvey[rawDataSurvey[,"Specie"] == specieInSurvey[i],])}}
                          },
                          splitSpecieFishery = function(){
                            if(length(specieInFishery) == 1){
                              addSpecieFishery(rawDataFishery)
                            }else{
                              for(i in 1:length(specieInFishery)){
                                addSpecieFishery(rawDataFishery[rawDataFishery[,"Specie"] == specieInFishery[i],])}}
                          },
                          addSpecieSurvey = function(sing_spe){surveyBySpecie <<- c(surveyBySpecie, SurveyBySpecie$new(sing_spe))},
                          addSpecieFishery = function(sing_spe){fisheryBySpecie <<- c(fisheryBySpecie, FisheryBySpecie$new(sing_spe))},
                          setSpreaDistAll = function(){
                            for(i in 1:length(fisheryBySpecie)){
                              fisheryBySpecie[[i]]$setSpreDistSing()
                            }
                          },
                          setSpatDistAll = function(){
                            for(i in 1:length(fisheryBySpecie)){
                              fisheryBySpecie[[i]]$setSpatDistSing()
                            }
                          },
                          setDepthSurvey = function(){
                            cat("\n\nSetting depth of survey data:", sep = "")
                            for(i in 1:length(surveyBySpecie)){
                              cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
                              surveyBySpecie[[i]]$setDepth(bathyMatrix = sampMap$gridBathy)
                              cat("Done!", sep = "")
                            }
                            cat("\nCompleted!", sep = "")
                          },
                          setStratumSurvey = function(vectorStrata = c(0, 10, 100, 1000, Inf)){
                            cat("\n\nSetting stratum of survey data:", sep = "")
                            for(i in 1:length(surveyBySpecie)){
                              cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
                              surveyBySpecie[[i]]$setStratum(vecStrata = vectorStrata)
                              cat("Done!", sep = "")
                            }
                            cat("\nCompleted!", sep = "")
                          },
                          setAbuAvgAll = function(){
                            cat("\n\nComputing average Number of individuals x Size x Stratum: ", sep = "")
                            for(i in 1:length(surveyBySpecie)){
                              cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
                              surveyBySpecie[[i]]$setAbuAvg()
                              cat("Done!", sep = "")
                            }
                            cat("\nCompleted!", sep = "")
                          },
                          setMeditsIndex = function(){
                            cat("\n\nComputing MEDITS index: ", sep = "")
                            for(i in 1:length(surveyBySpecie)){
                              cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
                              surveyBySpecie[[i]]$setIndSpe()
                              cat("Done!", sep = "")
                            }
                            cat("\nCompleted!", sep = "")
                          },
                          setStrataAbu = function(){
                            cat("\n\nComputing weighted Number of individuals x Size x Stratum: ", sep = "")
                            for(i in 1:length(surveyBySpecie)){
                              cat("\n\t", surveyBySpecie[[i]]$specie, "... ", sep = "")
                              surveyBySpecie[[i]]$abuAvg$weiFem <- surveyBySpecie[[i]]$abuAvg$Female*sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
                              surveyBySpecie[[i]]$abuAvg$weiMal <- surveyBySpecie[[i]]$abuAvg$Male*sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
                              surveyBySpecie[[i]]$abuAvg$weiUns <- surveyBySpecie[[i]]$abuAvg$Unsex*sampMap$weightStrata[surveyBySpecie[[i]]$abuAvg$Stratum]
                              cat("Done!", sep = "")
                            }
                            cat("\nCompleted!", sep = "")
                          },
                          setLFDPopSurvey = function(){
                            if(length(specieInSurvey) == 1){
                              calcLFDPopSurvey(1)
                            }else{
                              for(i in 1:length(specieInSurvey)){
                                calcLFDPopSurvey(i)
                              }}
                            # speDisPlot("All")
                          },
                          setLFDPopFishery = function(){
                            if(length(specieInFishery) == 1){
                              calcLFDPopFishery(1)
                            }else{
                              for(i in 1:length(specieInFishery)){
                                calcLFDPopFishery(i)
                              }}
                            # speDisPlot("All")
                          },
                          loadFleeEffoDbs = function(effort_path, met_nam, onBox = TRUE, perOnBox = 1){
                            cat("\n   ---   Extracting Effort data   ---", sep = "")
                            sort_files <- sort(effort_path)
                            fleet$rawEffort <<- list()
                            for(i in sort_files){
                              cat("\n\nLoading db: ", i, sep = "")
                              # cat("\nSelecting tracks in box...", sep = "")
                              tmp_eff <- fn$sqldf("select * from (select * from (select *, rowid as i_id from intrp) join (select distinct I_NCEE, T_NUM from intrp where I_NCEE in (select distinct I_NCEE from nn_clas where met_des = '`met_nam`') and LON > `sampMap$gridBboxSP@bbox[1,1]` and LON < `sampMap$gridBboxSP@bbox[1,2]` and LAT > `sampMap$gridBboxSP@bbox[2,1]` and LAT < `sampMap$gridBboxSP@bbox[2,2]`) using (I_NCEE, T_NUM)) join (select * from p_depth) using (i_id)", dbname = i)                             ### Over in B-Box
                              if(onBox){
                                in_box <- over(SpatialPoints(tmp_eff[,c("LON","LAT")]), sampMap$gridBboxSP)
                              }else{
                                in_box <- over(SpatialPoints(tmp_eff[,c("LON","LAT")]),
                                               unionSpatialPolygons(sampMap$gridShp,
                                                                    IDs = rep(1,
                                                                              length(sampMap$gridShp@polygons))))
                              }
                              cat("   -   Completed!", sep = "")

                              in_box[is.na(in_box)] <- 0
                              tmp_eff$in_box <- in_box
                              in_box_ping <- sqldf("select I_NCEE, T_NUM, sum(in_box) from tmp_eff group by I_NCEE, T_NUM")
                              all_ping <- sqldf("select I_NCEE, T_NUM, count(*) from tmp_eff group by I_NCEE, T_NUM")

                              ### Selecting tracks with at least X points in the bounding box
                              if(perOnBox > 100) perOnBox <- 1
                              if(perOnBox > 1) perOnBox <- perOnBox/100
                              cat("\nLoading tracks with at least ", perOnBox*100, "% of points in the bounding box...", sep = "")
                              perOnInd <- in_box_ping[,3]/all_ping[,3] >= perOnBox
                              if(sum(perOnInd) == 0){
                                cat("\nNo tracks with ", perOnBox*100, "% of points in the bounding box.\nNo tracking data loaded!", sep = "")
                              }else{
                                all_in_box <- in_box_ping[perOnInd,1:2]
                                all_sos <- sqldf("select * from tmp_eff join (select * from all_in_box) using (I_NCEE, T_NUM)")
                                cat("\nSaving Data", sep = "")
                                tmp_key <- names(which.max(table(years(all_sos$DATE))))
                                fleet$rawEffort[[tmp_key]] <<- all_sos
                              }
                            }
                          },
                          effPlot = function(whichYear){
                            if(whichYear == "All"){
                              all_sum <- apply(fleet$rawEffort, 1, sum)
                              round_perc <- 1+round(100*all_sum/max(all_sum))
                              # col_palette <- rev(heat.colors(100))
                              # cell_colors <- col_palette[round_perc]
                              distrPlotCols(cols = rev(heat.colors(101)), vals = round_perc,
                                            maxVal = ceiling(max(all_sum)),
                                            plotTitle = "Effort all years",
                                            legendUnits = "Hours")

                            }else{
                              num_col <- which(colnames(fleet$rawEffort) == whichYear)

                              yea_eff <- round(fleet$rawEffort[,num_col])
                              round_yea <- 1+100*yea_eff/max(yea_eff)

                              distrPlotCols(cols = rev(heat.colors(101)), vals = round_yea,
                                            maxVal = ceiling(max(yea_eff)),
                                            plotTitle = paste("Effort ", whichYear, sep = ""),
                                            legendUnits = "Hours")
                            }
                          },
                          speDisPlot = function(whoPlo){
                            if(whoPlo == "All"){
                              sampMap$plotSamMap("All species")
                              for(i in 1:length(specieInSurvey)){
                                points(surveyBySpecie[[i]]$rawLFD[,c("Lon","Lat")], pch = 20, col = 1+i, cex = 0.4)
                              }
                            }else{
                              sampMap$plotSamMap(whoPlo)
                              points(surveyBySpecie[[which(specieInSurvey == whoPlo)]]$rawLFD[,c("Lon","Lat")], pch = 20, col = 1+which(specieInSurvey == whoPlo), cex = 0.4)
                            }
                          },
                          plotGooSpe = function(whiSpe, whiSou){
                            if(whiSou == "Survey"){
                              if(whiSpe == "All"){
                                tmp_data <- unique(rawDataSurvey[,c("Specie", "Lat", "Lon")])
                              }else{
                                tmp_data <- unique(rawDataSurvey[which(rawDataSurvey$Specie == whiSpe),c("Specie", "Lat", "Lon")])
                              }
                              # levels(tmp_data[,1]) <- unique(rawDataSurvey[,"Specie"])

                              sampMap$plotGooSpeSur(tmp_data)
                            }else{
                              if(whiSpe == "All"){
                                tmp_data <- unique(rawDataFishery[,c("Specie", "Lat", "Lon")])
                              }else{
                                tmp_data <- unique(rawDataFishery[which(rawDataFishery$Specie == whiSpe),c("Specie", "Lat", "Lon")])
                              }
                              # levels(tmp_data[,1]) <- unique(rawDataFishery[,"Specie"])

                              sampMap$plotGooSpeFis(tmp_data)
                            }
                          },
                          distrPlotCols = function(cols = NULL, vals = NULL, maxVal = 100,
                                                   plotTitle = "NoTitle", legendUnits = "NoUnits"){
                            def.par <- par(no.readonly = TRUE)
                            par(mar = c(2.5,2.5,3,1))
                            layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(6,1))
                            sampMap$plotSamMap(title = plotTitle, celCol = cols[vals])
                            par(mar = c(5,3,5,1))
                            plot(NULL, xlim=c(0,1), ylim=c(0,1), bty="n", axes = FALSE, ann = FALSE, main = "Hours")
                            rect(0.25,seq(0.2,0.79, length.out = 100),
                                 0.55,seq(0.21,0.80, length.out = 100),
                                 col = cols, border = rainbow(1, alpha = 0.01))
                            mtext(ceiling(seq(from = 0, to = maxVal, length.out = 10)), side = 2,
                                  at = seq(0.21,0.80, length.out = 10), las = 2, cex = 1)
                            text(legendUnits, x = 0.25, y = 0.15)
                            par(def.par)
                          },
                          ggplotRawPoints = function(year){
                            tmp_dat <- fleet$rawEffort[[year]][sample(1:nrow(fleet$rawEffort[[year]]), min(c(50000, nrow(fleet$rawEffort[[year]])))),
                                                               c("LON","LAT","W_HARB")]
                            tmp_dat$Status <- factor(tmp_dat$W_HARB, levels = c("0", "1"),
                                                     labels = c("At sea", "In harbour"))
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot +
                                                           geom_point(data = tmp_dat,
                                                                      aes(x = LON, y = LAT, shape = Status, color = Status),
                                                                      size = 0.6, alpha = 0.3)+
                                                           geom_point(data = subset(tmp_dat, Status == "In harbour"),
                                                                      aes(x = LON, y = LAT, shape = Status, color = Status),
                                                                      size = 0.6, alpha = 0.3) +
                                                           scale_colour_manual(values = c("coral", "darkseagreen1")) +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]),
                                                                y = extendrange(sampMap$plotRange[3:4])) +
                                                           guides(colour = guide_legend(override.aes = list(size=3, alpha = 1))) +
                                                           ggtitle(paste("Sample raw points - ", year, sep = ""))+
                                                           theme_tufte(base_size = 14, ticks=T) +
                                                           theme(legend.position = "right",
                                                                 axis.text.x = element_text(size = 8),
                                                                 axis.title.x = element_text(size = 10),
                                                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                                 axis.text.y = element_text(size = 8),
                                                                 axis.title.y = element_text(size = 10),
                                                                 legend.text = element_text(size = 8),
                                                                 legend.title = element_text(size = 10)))
                            suppressWarnings(print(tmp_plot))
                          },
                          ggplotFgWeigDists = function(){
                            all_cell <- merge(x = sampMap$cutResShpFort$id,
                                              data.frame(x = as.numeric(substr(names(sampMap$fgWeigDist),3,
                                                                               nchar(names(sampMap$fgWeigDist)))),
                                                         y = sampMap$fgWeigDist), all = TRUE)
                            all_cell[is.na(all_cell)] <- 0
                            grid_data <- cbind(sampMap$cutResShpFort, DistAvg = all_cell[,2])

                            print(
                              suppressWarnings(
                                suppressMessages(
                                  sampMap$gooMapPlot +
                                    geom_polygon(aes(x = long, y = lat,
                                                     group = group, fill = DistAvg),
                                                 colour = "black", size = 0.1,
                                                 data = grid_data, alpha = 0.8) +
                                    scale_fill_gradient("Weighted\nDistance", low = "snow1", high = "orange1") +
                                    geom_text(aes(label = FG, x = Lon, y = Lat),
                                              data = sampMap$cutResShpCent, size = 2) +
                                    ggtitle("Average Distance x Fishing Ground") +
                                    xlab("Longitude") + ylab("Latitude") +
                                    lims(x = extendrange(sampMap$plotRange[1:2]),
                                         y = extendrange(sampMap$plotRange[3:4])) +
                                    geom_point(data = fleet$regHarbsBox,
                                               mapping = aes(x = Lon, y = Lat, size = absFreq),
                                               fill = NA, color = "tomato3", shape = 21) +
                                    scale_size_continuous("Number of\nVessels",
                                                          breaks = pretty(fleet$regHarbsBox$absFreq, 5),
                                                          range = c(1, 15)) +
                                    geom_label_repel(data = fleet$regHarbsBox, mapping = aes(x = Lon, y = Lat, label = Name),
                                                     size = 3, nudge_x = 0.1, nudge_y = 0.1, color = "grey3", fill ="grey89")
                                )
                              )
                            )
                          },
                          setAvailData = function(){
                            sampMap$availData <<- character(0)
                            sampMap$rawInpu <<- list()

                            cat("\n\nLoading available data:\n")
                            if(!is.null(sampMap$bioDF)){
                              sampMap$availData <<- c(sampMap$availData, "Seabed")
                              cat("\n   -   Seabed Category")
                              sampMap$rawInpu <<- c(sampMap$rawInpu, Seabed = list(data.frame(sampMap$bioDF)))
                              cat("\t\t-   Loaded!")
                            }

                            if(!is.null(fleet$rawEffort)){
                              sampMap$availData <<- c(sampMap$availData, "Effort")
                              cat("\n   -   Effort Distribution")
                              raw_effort <- numeric(length = sampMap$nCells)
                              for(i in names(fleet$rawEffort)){
                                tmp_effo <- as.data.frame(table(fleet$rawEffort[[i]]$Cell[which(fleet$rawEffort[[i]]$FishPoint)]))
                                names(tmp_effo) <- c("Cell", "Freq")
                                tmp_effo$Cell <- as.numeric(as.character(tmp_effo$Cell))
                                miss_rows <- as.numeric(setdiff(as.character(sampMap$gridShp@plotOrder), as.character(tmp_effo$Cell)))
                                if(length(miss_rows) > 0){
                                  tmp_effo <- rbind(tmp_effo, data.frame(Cell = miss_rows, Freq = 0))
                                  tmp_effo <- tmp_effo[order(tmp_effo[,1]),]
                                }
                                raw_effort <- cbind(raw_effort, tmp_effo[,2])
                                colnames(raw_effort)[ncol(raw_effort)] <- paste("Year_", i, sep = "")
                              }
                              sampMap$rawInpu <<- c(sampMap$rawInpu, Effort = list(raw_effort[,-1]))
                              cat("\t-   Loaded!")
                            }

                            if(!is.null(sampMap$griCent)){
                              sampMap$availData <<- c(sampMap$availData, "Depth")
                              cat("\n   -   Cell Depth")
                              sampMap$rawInpu <<- c(sampMap$rawInpu, Depth = list(sampMap$centDept[,3]))
                              cat("\t\t-   Loaded!\n")
                            }
                          },
                          predictProduction = function(specie){
                            Prod <- matrix(data = NA, nrow(fleet$effoAllLoa), ncol = sampMap$cutFG + 1)
                            lyears <- sort(as.numeric(as.character(unique(fleet$effoAllLoa$Year))))
                            thrZero <- mean(fleet$effoProdAllLoa[,specie][fleet$effoProdAllLoa[,specie] < fleet$specSett[[specie]]$threshold & fleet$effoProdAllLoa[,specie] > 0])
                            fgClms <- which(colnames(fleet$effoAllLoa) %in% as.character(seq(1, sampMap$cutFG + 1)))
                            datalog <- fleet$effoAllLoa
                            datalog$MonthNum <- as.factor(datalog$MonthNum)
                            datalog$Year <- as.factor(datalog$Year)
                            if(fleet$specLogit[[specie]]$logit$Name == "GLM"){
                              infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "response") > fleet$specLogit[[specie]]$logit$Cut)
                            }else{
                              infish <- which(predict(fleet$specLogit[[specie]]$logit$Model, datalog, type = "prob")[,2] > fleet$specLogit[[specie]]$logit$Cut)
                            }
                            # infish <- which(predict(fleet$specLogit[[specie]]$logit$logit_f, datalog, type="response") > fleet$specLogit[[specie]]$optCut)
                            for(i in 1:length(infish)){
                              idata <- as.numeric(fleet$effoAllLoa[infish[i], fgClms])
                              iloa <- as.numeric(fleet$effoAllLoa[infish[i], "Loa"])
                              iy <- which(lyears == fleet$effoAllLoa[infish[i], "Year"])
                              im <- as.numeric(as.character(fleet$effoAllLoa[infish[i], "MonthNum"]))
                              ib <- fleet$resNNLS[[specie]]$bmat[which((fleet$resNNLS[[specie]]$SceMat$YEAR == iy) & (fleet$resNNLS[[specie]]$SceMat$MONTH == im)),]
                              # Prod[infish[i]] <- sum(ib * idata * iloa) + mean(fleet$effoProdAllLoa[,specie][fleet$effoProdAllLoa[,specie] < fleet$specSett[[specie]]$threshold & fleet$effoProdAllLoa[,specie] > 0])
                              if(sum(ib*idata)>0){
                                Prod[infish[i],] <- (ib * idata * iloa) + ((ib*idata)/sum(ib*idata))*thrZero
                              }
                            }
                            Prod[is.na(Prod)] <- 0
                            colnames(Prod) <- paste("PR_", as.character(seq(1, ncol(Prod))), sep = "")
                            fleet$predProd[[specie]] <<- Prod
                          },
                          ggplotFishingPoints = function(year){
                            tmp_dat <- fleet$rawEffort[[year]][sample(1:nrow(fleet$rawEffort[[year]]), min(c(50000, nrow(fleet$rawEffort[[year]])))),c("LON","LAT","FishPoint")]
                            tmp_dat$Status <- factor(tmp_dat$FishPoint, levels = c("FALSE", "TRUE"), labels = c("Not fishing", "Fishing"))
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot +
                                                           geom_point(data = tmp_dat,
                                                                      aes(x = LON, y = LAT, color = Status), size = 0.25, alpha = 0.2)+
                                                           scale_colour_manual(values = c("coral", "darkseagreen1")) +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]), y = extendrange(sampMap$plotRange[3:4])) +
                                                           guides(colour = guide_legend(override.aes = list(size=3, alpha = 1))) +
                                                           ggtitle(paste("Sample fishing points - ", year, sep = ""))+
                                                           theme_tufte(base_size = 14, ticks=T) +
                                                           theme(legend.position = "right",
                                                                 axis.text.x = element_text(size = 8),
                                                                 axis.title.x = element_text(size = 10),
                                                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                                 axis.text.y = element_text(size = 8),
                                                                 axis.title.y = element_text(size = 10),
                                                                 legend.text = element_text(size = 8),
                                                                 legend.title = element_text(size = 10)))
                            suppressWarnings(print(tmp_plot))
                          },
                          setPlotBetaMeltYear = function(specie, year){
                            tmp_melt_sub <- subset(fleet$betaMeltYear[[specie]], Year == year)
                            all_cell <- merge(x = sampMap$cutResShpFort$id,
                                              data.frame(x = as.numeric(substr(as.character(tmp_melt_sub$FishGround),4,
                                                                               nchar(as.character(tmp_melt_sub$FishGround)))),
                                                         y = tmp_melt_sub$Productivity), all = TRUE)
                            all_cell[is.na(all_cell)] <- 0
                            grid_data <- cbind(sampMap$cutResShpFort, Beta = all_cell[,2])
                            sampMap$ggBetaFGmap <<- suppressMessages(sampMap$gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = Beta),
                                                                                                       colour = "black", size = 0.1,
                                                                                                       data = grid_data, alpha = 0.8) +
                                                                       scale_fill_gradient("Beta\nValues", low = "lightyellow", high = "mediumseagreen") +
                                                                       geom_text(aes(label = FG, x = Lon, y = Lat),
                                                                                 data = sampMap$cutResShpCent, size = 2) +
                                                                       # theme(legend.position='none') +
                                                                       ggtitle(paste("Betas x Fishing Ground - ", year, sep = "")) +
                                                                       xlab("Longitude") + ylab("Latitude") +
                                                                       lims(x = extendrange(sampMap$plotRange[1:2]),
                                                                            y = extendrange(sampMap$plotRange[3:4]))
                            )
                            sampMap$ggBetaFGbox <<- suppressMessages(ggplot(fleet$betaMeltYear[[specie]],
                                                                            aes(x = FishGround, y = Productivity,
                                                                                group = FishGround)) +
                                                                       geom_boxplot() +
                                                                       coord_flip() +
                                                                       geom_point(data = tmp_melt_sub,
                                                                                  aes(x = FishGround, y = Productivity,
                                                                                      fill = Productivity, group = FishGround),
                                                                                  size = 2, shape = 21, color = "grey40") +
                                                                       geom_line(data = tmp_melt_sub,
                                                                                 aes(x = FishGround, y = Productivity, group = Year),
                                                                                 color = "grey40") +
                                                                       scale_fill_gradient(low = "lightyellow", high = "mediumseagreen") +
                                                                       xlab("Fishing Ground") +
                                                                       theme(legend.position='none')
                            )
                          },
                          setPlotProdMeltYear = function(specie, year){
                            tmp_melt_sub <- subset(fleet$prodMeltYear[[specie]], Year == year)
                            all_cell <- merge(x = sampMap$cutResShpFort$id,
                                              data.frame(x = substr(as.character(tmp_melt_sub$FishGround),4, nchar(as.character(tmp_melt_sub$FishGround))),
                                                         y = tmp_melt_sub$Production), all = TRUE)
                            all_cell[is.na(all_cell)] <- 0
                            grid_data <- cbind(sampMap$cutResShpFort, Hours = all_cell[,2])
                            sampMap$ggProdFGmap <- suppressMessages(sampMap$gooMapPlot +
                                                                      geom_polygon(aes(x = long, y = lat, group = group, fill = Hours),
                                                                                   colour = "black", size = 0.1,
                                                                                   data = grid_data, alpha = 0.8) +
                                                                      scale_fill_gradient("Production\nValues",
                                                                                          low = "lightyellow", high = "slateblue1") +
                                                                      geom_text(aes(label = FG, x = Lon, y = Lat),
                                                                                data = sampMap$cutResShpCent, size = 2) +
                                                                      # theme(legend.position='none') +
                                                                      ggtitle(paste("Production x Fishing Ground - ", year, sep = "")) +
                                                                      xlab("Longitude") + ylab("Latitude") +
                                                                      lims(x = extendrange(sampMap$plotRange[1:2]),
                                                                           y = extendrange(sampMap$plotRange[3:4]))
                            )
                            sampMap$ggProdFGbox <<- suppressMessages(ggplot(fleet$prodMeltYear[[specie]], aes(x = FishGround, y = Production, group = FishGround)) +
                                                                       geom_boxplot() +
                                                                       coord_flip() +
                                                                       geom_point(data = tmp_melt_sub, aes(x = FishGround, y = Production,
                                                                                                           fill = Production, group = FishGround),
                                                                                  size = 2, shape = 21, color = "grey40") +
                                                                       geom_line(data = tmp_melt_sub, aes(x = FishGround, y = Production, group = Year),
                                                                                 color = "grey40") +
                                                                       scale_fill_gradient(low = "lightyellow", high = "slateblue1") +
                                                                       xlab("Fishing Ground") +
                                                                       theme(legend.position='none')
                            )
                          },
                          setCellPoin = function(){
                            num_cell <- getinfo.shape(sampMap$gridPath)$entities
                            sampMap$gridShp@plotOrder <- 1:num_cell
                            tmp_polygons <- SpatialPolygons(sampMap$gridShp@polygons)
                            cat("\nGridding year ", sep = "")
                            for(j in names(fleet$rawEffort)){
                              cat(j, "... ", sep = "")
                              fleet$rawEffort[[j]]$Cell <<- over(SpatialPoints(fleet$rawEffort[[j]][,c("LON","LAT")]), tmp_polygons)
                            }
                            cat("Done!", sep = "")
                          },
                          setTrackHarb = function(){
                            fleet$trackHarbs <<- list()
                            for(i in names(fleet$rawEffort)){
                              cat("\nLoading effort year: ", i, "... ", sep = "")
                              tmp_eff <- fleet$rawEffort[[i]]
                              tmp_harbs <- sqldf("select xCFR I_NCEE, xTnum T_NUM, LON_S, LAT_S, DATE_S, LON_E, LAT_E, DATE_E from
                                                 (select x.I_NCEE xCFR, LAT LAT_S, LON LON_S, DATE DATE_S, T_NUM xTnum from tmp_eff x where W_HARB = 1)
                                                 join
                                                 (select y.I_NCEE yCFR, LAT LAT_E, LON LON_E, DATE DATE_E, T_NUM yTnum from tmp_eff y where W_HARB = 1)
                                                 on xCFR = yCFR and xTnum = yTnum and DATE_S != DATE_E and DATE_S < DATE_E order by xCFR, xTnum, DATE_S")
                              cat("Done!", sep = "")
                              uni_harbs <- as.data.frame(unique(rbind(as.matrix(tmp_harbs[,c("LON_S","LAT_S")]),as.matrix(tmp_harbs[,c("LON_E","LAT_E")]))))
                              uni_harbs$Name <- ""
                              cat("\nSetting nearest harbor name... ", sep = "")
                              tmp_dist <- apply(spDists(x = as.matrix(uni_harbs[,1:2]),
                                                        y = as.matrix(sampMap$harbDbf[,1:2]), longlat = TRUE), 1, which.min)
                              uni_harbs$Name <- as.character(sampMap$harbDbf[tmp_dist,3])
                              cat("Done!", sep = "")
                              cat("\nSaving... ", sep = "")
                              fleet$trackHarbs[[i]] <<- sqldf("select I_NCEE, T_NUM, k.LON_S LON_S, k.LAT_S LAT_S, DATE_S, HARB_S, LON_E, LAT_E, DATE_E, Name HARB_E from (select I_NCEE, T_NUM, LON_S, LAT_S, DATE_S, Name HARB_S, LON_E, LAT_E, DATE_E from tmp_harbs join uni_harbs using (LON_S, LAT_S)) k join uni_harbs x on x.LON_S = LON_E and x.LAT_S = LAT_E")
                              cat("Done!", sep = "")
                            }

                          },
                          setFishGround = function(numCut){
                            sampMap$cutFG <<- numCut
                            sampMap$setCutResult(ind_clu = numCut)
                            tmp_clust <- cbind(Cell = 1:sampMap$nCells,
                                               FishGround = sampMap$clusMat[,numCut])
                            cat("\n\nSetting Fishing Ground year\n", sep = "")
                            for(j in names(fleet$rawEffort)){
                              cat(j, "... ", sep = "")
                              fleet$rawEffort[[j]]$FishGround <<- tmp_clust[fleet$rawEffort[[j]]$Cell,2]
                            }
                            cat("Done!\n", sep = "")
                          },
                          addFg2Fishery = function(){

                            cat("\n\nConnecting coordinates to fishing ground...", sep = "")

                            rawDataFishery$numFG <<- names(sampMap$cutResShp)[over(SpatialPoints(data.frame(Lon = rawDataFishery$Lon,
                                                                                                            Lat = rawDataFishery$Lat)),
                                                                                   sampMap$cutResShp)]

                            for(i in 1:length(fisheryBySpecie)){
                              fisheryBySpecie[[i]]$rawLFD$numFG <<- names(sampMap$cutResShp)[over(SpatialPoints(data.frame(Lon = fisheryBySpecie[[i]]$rawLFD$Lon,
                                                                                                                           Lat = fisheryBySpecie[[i]]$rawLFD$Lat)),
                                                                                                  sampMap$cutResShp)]
                            }

                            cat(" Done!", sep = "")
                          },
                          setWeekEffoMatrCell = function(){
                            fleet$weekEffoMatr <<- list()
                            for(j in names(fleet$rawEffort)){
                              cat("\n\nLoading year ", j, " ... ", sep = "")
                              tmp_dat <- fleet$rawEffort[[j]][fleet$rawEffort[[j]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[j]]$Cell),c("I_NCEE","T_NUM", "WeekNum", "Cell", "FishPoint")]
                              cat("Done!", sep = "")
                              tmp_dat$Cell <- as.factor(tmp_dat$Cell)
                              cat("\nCreating weekly fishing effort matrix... ", sep = "")
                              tmp_matrix <- dcast(tmp_dat,
                                                  I_NCEE + T_NUM + WeekNum ~ Cell, fun.aggregate = sum,
                                                  na.rm=TRUE, value.var = "FishPoint")
                              cat("Done!", sep = "")
                              cat("\nChecking... ", sep = "")
                              miss_cols <- setdiff(as.character(sampMap$gridShp@plotOrder), names(tmp_matrix)[4:ncol(tmp_matrix)])
                              if(length(miss_cols) > 0){
                                cat(length(miss_cols), " cells with no points... ", sep = "")
                                tmp_matrix[,miss_cols] <- 0
                                tmp_matrix <- tmp_matrix[,c(1:3, 3+order(as.numeric(names(tmp_matrix)[4:ncol(tmp_matrix)])))]
                              }
                              cat(" Done!", sep = "")
                              fleet$weekEffoMatr[[j]] <<- tmp_matrix
                            }
                          },
                          setWeekEffoMatrGround = function(){
                            fleet$weekEffoMatr <<- list()
                            for(j in names(fleet$rawEffort)){
                              cat("\n\nLoading year ", j, " ... ", sep = "")
                              tmp_dat <- fleet$rawEffort[[j]][fleet$rawEffort[[j]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[j]]$Cell),c("I_NCEE","T_NUM", "WeekNum", "MonthNum", "FishGround", "FishPoint")]
                              cat("Done!", sep = "")
                              tmp_dat$FishGround <- as.factor(tmp_dat$FishGround)
                              cat("\nCreating weekly fishing effort matrix... ", sep = "")
                              tmp_matrix <- dcast(tmp_dat,
                                                  I_NCEE + T_NUM + WeekNum + MonthNum ~ FishGround, fun.aggregate = sum,
                                                  na.rm=TRUE, value.var = "FishPoint")
                              cat("Done!", sep = "")
                              cat("\nChecking... ", sep = "")
                              miss_cols <- setdiff(as.character(unique(fleet$rawEffort[[j]]$FishGround[!is.na(fleet$rawEffort[[j]]$FishGround)])),
                                                   names(tmp_matrix)[5:ncol(tmp_matrix)])
                              if(length(miss_cols) > 0){
                                cat(length(miss_cols), " cells with no points... ", sep = "")
                                tmp_matrix[,miss_cols] <- 0
                                tmp_matrix <- tmp_matrix[,c(1:4, 4+order(as.numeric(names(tmp_matrix)[5:ncol(tmp_matrix)])))]
                              }
                              tmp_matrix <- sqldf("select * from tmp_matrix join (select I_NCEE, T_NUM, min(DATE) DATE_S, max(DATE) DATE_E from tmp_dat group by I_NCEE, T_NUM) using (I_NCEE, T_NUM)")
                              cat(" Done!", sep = "")
                              fleet$weekEffoMatr[[j]] <<- tmp_matrix
                            }
                          },
                          ggplotGridEffort = function(year){
                            tmp_dat <- table(fleet$rawEffort[[year]][fleet$rawEffort[[year]]$FishPoint == TRUE & !is.na(fleet$rawEffort[[year]]$Cell), c("Cell")])
                            all_cell <- merge(x = sampMap$gridPolySet$PID,
                                              data.frame(x = as.numeric(names(tmp_dat)), y = tmp_dat), all = TRUE)[,c(1,3)]
                            all_cell[is.na(all_cell)] <- 0
                            # grid_data <- cbind(sampMap$gridPolySet, LogCount = log10(all_cell[,2] + 1))
                            grid_data <- cbind(sampMap$gridPolySet, Count = all_cell[,2])
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID, fill = Count), size = 0.2,
                                                                                           data = grid_data, alpha = 0.8) +
                                                           scale_fill_gradient(low = "Yellow", high = "coral",
                                                                               trans = 'log10',
                                                                               breaks = trans_breaks('log10', function(x) 10^x),
                                                                               labels = trans_format('log10', math_format(10^.x))) +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]), y = extendrange(sampMap$plotRange[3:4])) +
                                                           ggtitle(paste("Fishing Effort - ", year, sep = "")))
                            suppressWarnings(print(tmp_plot))
                          },
                          getNnlsModel = function(specie, minobs, thr_r2){
                            data <- fleet$effoProdAllLoa
                            nFG <- sampMap$cutFG + 1
                            thrB <- fleet$specSett[[specie]]$threshold
                            thr0 <- mean(data[,specie][data[,specie] < thrB & data[,specie] > 0])

                            if(length(which(is.na(data)==TRUE,arr.ind=TRUE)[,1])>0)
                              data <- data[-which(is.na(data)==TRUE,arr.ind=TRUE)[,1],]
                            norow <- which(data[,which(colnames(data)==specie)]<=thrB)
                            X0 <- data[-norow,which(colnames(data) %in% c("Year","MonthNum","Loa",as.character(seq(1:nFG))))]
                            Y0 <- data[-norow,which(colnames(data)==specie)]

                            #Gen Matrix of scenaria and fill by membership
                            nY <- length(unique(X0$Year))
                            nM <- 12
                            membY <- as.numeric(as.factor(X0$Year)) #Membership delle osservazioni x anno
                            membM <- as.numeric(X0$MonthNum) #Membership delle osservazioni x mese
                            SceMat <- expand.grid(unique(membY),unique(membM))
                            colnames(SceMat) <-c("YEAR","MONTH")
                            SceList <- vector(mode="list", length=nrow(SceMat))
                            il <- 0
                            for(i in 1:nrow(SceMat)){
                              il <- il+1
                              SceList[[il]] <- which((membY==SceMat[il,1])&(membM==SceMat[il,2]))
                            }
                            lsce <- as.numeric(lapply(SceList,length))
                            wsce <- which(lsce>=minobs)
                            nSce <- length(wsce)

                            #Gen and fill the matrix of betas
                            bmat <- matrix(NA,length(unique(membY))*length(unique(membM)),nFG)
                            obsY <- fittedY <- vector(mode="list",length=nSce)
                            nnls_r2 <- rep(NA,nSce)
                            nfitted <- 0
                            nno <- 0
                            wcol <- which(colnames(X0) %in% as.character(seq(1,nFG)))

                            for(iS in wsce){
                              subX <- X0[SceList[[iS]],wcol]*(X0$Loa[SceList[[iS]]])
                              subY <- Y0[SceList[[iS]]]
                              zeroFG <- which(apply(subX,2,sum)==0)
                              eFG <- setdiff(1:nFG,zeroFG)
                              if(length(zeroFG)>0) subX <- subX[,-zeroFG]
                              if(min(dim(subX))>0){
                                nnls_m <- getNNLS(subX,subY, zeroFG)
                                if(abs(nnls_m$r2) > thr_r2){
                                  nnls_r2[iS] <- nnls_m$r2
                                  obsY[[iS]] <- nnls_m$obs
                                  fittedY[[iS]] <- nnls_m$fitted
                                  tcoo <- 12*(SceMat[iS,"YEAR"]-1)+SceMat[iS,"MONTH"]
                                  bmat[tcoo,eFG] <- nnls_m$betas
                                  if(length(zeroFG)>0) bmat[tcoo,zeroFG] <- 0
                                  nfitted <- nfitted + 1
                                }else{
                                  nno <- nno + 1
                                }
                              }else{
                                nno <- nno + 1
                              }
                            }
                            cat("\nNNLS: ", nSce, " actual scenarios - ", nfitted, " fitted", "(", floor(100*(nSce-nno)/nSce), "%)", sep = "")
                            blist <- vector(mode="list",length=4)
                            colnames(bmat) <- paste("BE_", ifelse(nchar(1:ncol(bmat)) == 2, 1:ncol(bmat), paste("0", 1:ncol(bmat), sep = "")), sep = "")

                            if(anyNA(bmat)){
                              zero_chk <- which(apply(bmat[which(!is.na(bmat),arr.ind = TRUE),], 2, sum,na.rm=TRUE) == 0)
                              if(length(zero_chk) > 0){
                                par_betas <- fillbetas(bmat[,-zero_chk])
                                zero_beta <- matrix(0, nrow = nrow(data.frame(bmat[,zero_chk])),
                                                    ncol = ncol(data.frame(bmat[,zero_chk])))
                                colnames(zero_beta) <- names(zero_chk)
                                full_betas <- cbind(par_betas, zero_beta)
                                bmat <- full_betas[,order(colnames(full_betas))]
                              }else{
                                bmat <- fillbetas(bmat)
                              }
                            }

                            if(any(bmat < 0)) bmat[which(bmat < 0)] <- 0

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
                          cohoDisPlot = function(whoSpe, whoCoh, whiYea, interp){
                            if(interp == FALSE){
                              if(whoCoh == "All"){
                                if(whiYea == "All"){
                                  # 1+round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,,],1,sum)/max(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,,],1,sum)), 2)*100
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - All years", sep = ""), legendUnits = "N.")
                                }else{
                                  # 1+round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)/max(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)), 2)*100
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }else{
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,whoCoh,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A[,whoCoh,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }
                            }else{
                              if(whoCoh == "All"){
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[,,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[,,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }else{
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[,whoCoh,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(surveyBySpecie[[whoSpe]]$Coh_A_Int[,whoCoh,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", specieInSurvey[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }
                            }
                          },
                          calcLFDPopSurvey = function(ind_num){
                            surveyBySpecie[[ind_num]]$LFDPop <<- array(dim=c(sampMap$nCells, length(surveyBySpecie[[ind_num]]$lengClas),length(surveyBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(surveyBySpecie[[ind_num]]$year)){
                              subLFD <- surveyBySpecie[[ind_num]]$rawLFD[which(surveyBySpecie[[ind_num]]$rawLFD$Year==surveyBySpecie[[ind_num]]$year[y]),]
                              poinOver <- as.numeric(sp::over(SpatialPoints(subLFD[,c("Lon","Lat")]), SpatialPolygons(sampMap$gridShp@polygons)))
                              subLFD <- cbind(subLFD[,c("Class", "Female", "Male")], poinOver)
                              colnames(subLFD) <- c("Class", "Female", "Male", "Cell")
                              for(IDcell in 1:sampMap$nCells){
                                if(length(which(subLFD[,"Cell"] == IDcell))>0){
                                  cell.data <- subLFD[which(subLFD[,"Cell"] == IDcell),]
                                  cell.LFD <- RecLFD(cell.data,
                                                     surveyBySpecie[[ind_num]]$lengClas,
                                                     length(unique(cell.data[,1])))
                                  surveyBySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- cell.LFD[1,]
                                  surveyBySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- cell.LFD[2,]
                                }else{
                                  surveyBySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- rep(0,length(surveyBySpecie[[ind_num]]$lengClas))
                                  surveyBySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- rep(0,length(surveyBySpecie[[ind_num]]$lengClas))
                                }}}},
                          calcLFDPopFishery = function(ind_num){
                            fisheryBySpecie[[ind_num]]$LFDPop <<- array(dim=c(sampMap$nCells, length(fisheryBySpecie[[ind_num]]$lengClas),length(fisheryBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(fisheryBySpecie[[ind_num]]$year)){
                              subLFD <- fisheryBySpecie[[ind_num]]$rawLFD[which(years(fisheryBySpecie[[ind_num]]$rawLFD$Date)==fisheryBySpecie[[ind_num]]$year[y]),]
                              poinOver <- as.numeric(sp::over(SpatialPoints(subLFD[,c("Lon","Lat")]), SpatialPolygons(sampMap$gridShp@polygons)))
                              subLFD <- cbind(subLFD[,c("Class", "Female", "Male")], poinOver)
                              colnames(subLFD) <- c("Class", "Female", "Male", "Cell")
                              for(IDcell in 1:sampMap$nCells){
                                if(length(which(subLFD[,"Cell"] == IDcell))>0){
                                  cell.data <- subLFD[which(subLFD[,"Cell"] == IDcell),]
                                  cell.LFD <- RecLFD(cell.data,
                                                     fisheryBySpecie[[ind_num]]$lengClas,
                                                     length(unique(cell.data[,1])))
                                  fisheryBySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- cell.LFD[1,]
                                  fisheryBySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- cell.LFD[2,]
                                }else{
                                  fisheryBySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- rep(0,length(fisheryBySpecie[[ind_num]]$lengClas))
                                  fisheryBySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- rep(0,length(fisheryBySpecie[[ind_num]]$lengClas))
                                }}}},
                          setCoh_A_Survey = function(){
                            if(length(specieInSurvey) == 1){
                              calcCoh_A_Survey(1)
                            }else{
                              for(i in 1:length(specieInSurvey)){
                                calcCoh_A_Survey(i)
                              }}
                          },
                          setCoh_A_Fishery = function(){
                            if(length(specieInFishery) == 1){
                              calcCoh_A_Fishery(1)
                            }else{
                              for(i in 1:length(specieInFishery)){
                                calcCoh_A_Fishery(i)
                              }}
                          },
                          calcCoh_A_Survey = function(ind_num){
                            Pop <- surveyBySpecie[[ind_num]]$LFDPop
                            LC <- surveyBySpecie[[ind_num]]$lengClas[-length(surveyBySpecie[[ind_num]]$lengClas)]
                            sp <- surveyBySpecie[[ind_num]]$specie
                            nc <- surveyBySpecie[[ind_num]]$nCoho
                            surveyBySpecie[[ind_num]]$Coh_A <<- array(dim=c(sampMap$nCells, nc, length(surveyBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(surveyBySpecie[[ind_num]]$year)){
                              for(sex in c(1:2)){
                                mms <- surveyBySpecie[[ind_num]]$mixPar[[sex]][[1]][y,]
                                sds <- surveyBySpecie[[ind_num]]$mixPar[[sex]][[2]][y,]
                                opt <- matrix(0,length(LC),nc)
                                for(ij in 1:sampMap$nCells){
                                  vv <- Pop[ij, , y, sex]
                                  coh.abb <- numeric(nc)
                                  if(sum(vv)>0){
                                    for(coh in c(1:nc)) opt[,coh] <- dnorm(LC,mms[coh],sds[coh])
                                    opt.ass <- apply(opt,1,which.max)
                                    for(coh in c(1:nc)) coh.abb[coh] <- sum(vv[which(opt.ass==coh)])
                                  }
                                  surveyBySpecie[[ind_num]]$Coh_A[ij,1:nc,y,sex] <- as.numeric(coh.abb)
                                }
                              }
                            }
                          },
                          calcCoh_A_Fishery = function(ind_num){
                            Pop <- fisheryBySpecie[[ind_num]]$LFDPop
                            LC <- fisheryBySpecie[[ind_num]]$lengClas[-length(fisheryBySpecie[[ind_num]]$lengClas)]
                            sp <- fisheryBySpecie[[ind_num]]$specie
                            nc <- fisheryBySpecie[[ind_num]]$nCoho
                            fisheryBySpecie[[ind_num]]$Coh_A <<- array(dim=c(sampMap$nCells, nc, length(fisheryBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(fisheryBySpecie[[ind_num]]$year)){
                              for(sex in c(1:2)){
                                mms <- fisheryBySpecie[[ind_num]]$mixPar[[sex]][[1]][y,]
                                sds <- fisheryBySpecie[[ind_num]]$mixPar[[sex]][[2]][y,]
                                opt <- matrix(0,length(LC),nc)
                                for(ij in 1:sampMap$nCells){
                                  vv <- Pop[ij, , y, sex]
                                  coh.abb <- numeric(nc)
                                  if(sum(vv)>0){
                                    for(coh in c(1:nc)) opt[,coh] <- dnorm(LC,mms[coh],sds[coh])
                                    opt.ass <- apply(opt,1,which.max)
                                    for(coh in c(1:nc)) coh.abb[coh] <- sum(vv[which(opt.ass==coh)])
                                  }
                                  fisheryBySpecie[[ind_num]]$Coh_A[ij,1:nc,y,sex] <- as.numeric(coh.abb)
                                }
                              }
                            }
                          },
                          intrpCoh_A_Survey = function(ind_num){
                            surveyBySpecie[[ind_num]]$Coh_A_Int <<- array(dim=c(sampMap$nCells, surveyBySpecie[[ind_num]]$nCoho,length(surveyBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(surveyBySpecie[[ind_num]]$year)){
                              for(sex in 1:2){
                                for(coh in 1:surveyBySpecie[[ind_num]]$nCoho){
                                  xdata <- cbind(sampMap$griCent, surveyBySpecie[[ind_num]]$Coh_A[,coh,y,sex])
                                  colnames(xdata) <- c("Lon","Lat","Coh")
                                  xdata <- as.data.frame(xdata)
                                  yea_poi <- surveyBySpecie[[ind_num]]$rawLFD[which(surveyBySpecie[[ind_num]]$rawLFD$Year == surveyBySpecie[[ind_num]]$year[y]),c("Lon", "Lat")]
                                  cMEDITS <- which(!is.na(over(sampMap$gridShp, SpatialPoints(unique(yea_poi)))))
                                  noMEDITS <- setdiff(c(1:sampMap$nCells),cMEDITS)
                                  Areacell <- 9.091279*11.112
                                  RateArea <- Areacell/100
                                  surveyBySpecie[[ind_num]]$Coh_A_Int[,coh,y,sex] <- IntInvDis(RateArea*xdata, cMEDITS, noMEDITS,
                                                                                               Refmax=5, Refmin=3,
                                                                                               sampMap$nCells,
                                                                                               sampMap$gridShp, graph=T, logplot=F)[,3]
                                }
                              }
                            }
                          },
                          intrpCoh_A_Fishery = function(ind_num){
                            fisheryBySpecie[[ind_num]]$Coh_A_Int <<- array(dim=c(sampMap$nCells, fisheryBySpecie[[ind_num]]$nCoho,length(fisheryBySpecie[[ind_num]]$year),2))
                            for(y in 1:length(fisheryBySpecie[[ind_num]]$year)){
                              for(sex in 1:2){
                                for(coh in 1:fisheryBySpecie[[ind_num]]$nCoho){
                                  xdata <- cbind(sampMap$griCent, fisheryBySpecie[[ind_num]]$Coh_A[,coh,y,sex])
                                  colnames(xdata) <- c("Lon","Lat","Coh")
                                  xdata <- as.data.frame(xdata)
                                  yea_poi <- fisheryBySpecie[[ind_num]]$rawLFD[which(fisheryBySpecie[[ind_num]]$rawLFD$Year == fisheryBySpecie[[ind_num]]$year[y]),c("Lon", "Lat")]
                                  cMEDITS <- which(!is.na(over(sampMap$gridShp, SpatialPoints(unique(yea_poi)))))
                                  noMEDITS <- setdiff(c(1:sampMap$nCells),cMEDITS)
                                  Areacell <- 9.091279*11.112
                                  RateArea <- Areacell/100
                                  fisheryBySpecie[[ind_num]]$Coh_A_Int[,coh,y,sex] <- IntInvDis(RateArea*xdata, cMEDITS, noMEDITS,
                                                                                                Refmax=5, Refmin=3,
                                                                                                sampMap$nCells,
                                                                                                sampMap$gridShp, graph=T, logplot=F)[,3]
                                }
                              }
                            }
                          }
                        ))



####SurveyBySpecie#################################################
#' SurveyBySpecie
#'
#' The \code{SurveyBySpecie} class implements the class of SMART to
#'  handle species samplings.
#'
#' @return This function returns the 'SurveyBySpecie' object.
#'
#############################################################

SurveyBySpecie <- R6Class("SurveyBySpecie",
                          portable = FALSE,
                          class = TRUE,
                          public = list(
                            specie = NULL,
                            year = NULL,
                            rawLFD = NULL,
                            abuAvg = NULL,
                            meditsIndex = NULL,
                            lengClas = NULL, #LClass
                            LFDPop = NULL,
                            mixPar = NULL, # MixtureP e ncohorts
                            nCoho = NULL,
                            prior = NULL,
                            Coh_A = NULL,
                            Coh_A_Int = NULL,
                            # Fis_Gro = NULL,
                            LWpar = NULL,
                            scorVec = NULL,
                            qMedits = NULL,
                            bRefs = NULL,
                            popGen = NULL,
                            selPar = NULL,
                            setRawData = function(raw_data){rawLFD <<- raw_data},
                            plotLFD = function(){
                              plotSpeAllYea(rawLFD)
                            },
                            initialize = function(sing_spe){
                              setRawData(sing_spe)
                              setYears()
                              setSpecie()
                              setLClass()
                            },
                            setYears = function(){year <<- sort(unique(rawLFD[,"Year"]), decreasing = FALSE)},
                            setSpecie = function(){specie <<- unique(rawLFD[,"Specie"])},
                            setLClass = function(){lengClas <<- seq(from = min(rawLFD[,"Class"]), to = max(rawLFD[,"Class"]), by = 1) },
                            setDepth = function(bathyMatrix){
                              rawLFD$Depth <<- get.depth(bathyMatrix, x = rawLFD$Lon, y = rawLFD$Lat, locator = FALSE)[,3]
                            },
                            setStratum = function(vecStrata = c(0, 10, 100, 1000, Inf)){
                              tmp_mem <- findInterval(x = -rawLFD$Depth, vec = vecStrata)
                              rawLFD$Stratum <<- factor(tmp_mem, levels = 1:(length(vecStrata)-1), labels = paste(vecStrata[-length(vecStrata)], vecStrata[-1], sep = " - "))
                            },
                            setIndSpe = function(){
                              meditsIndex <<- aggregate(weiFem ~ Class, data = abuAvg, sum)
                              meditsIndex <<- merge(x= meditsIndex, y = aggregate(weiMal ~ Class, data = abuAvg, sum), all = TRUE)
                              meditsIndex <<- merge(x= meditsIndex, y = aggregate(weiUns ~ Class, data = abuAvg, sum), all = TRUE)
                            },
                            setAbuAvg = function(){
                              tmp_aggFem <- aggregate(Female ~ Class + Stratum, data = rawLFD, mean)
                              tmp_aggMal <- aggregate(Male ~ Class + Stratum, data = rawLFD, mean)
                              tmp_aggUns <- aggregate(Unsex ~ Class + Stratum, data = rawLFD, mean)
                              tmp_all <- merge(x = tmp_aggFem, y = tmp_aggMal, all = TRUE)
                              tmp_all <- merge(x = tmp_all, y = tmp_aggUns, all = TRUE)
                              abuAvg <<- tmp_all
                            },
                            setNCoho = function(num_coh){nCoho <<- num_coh},
                            setPrior = function(f_linf, f_k, f_t0, m_linf, m_k, m_t0){
                              prior <<- list('Female' = list('Linf' = list('Mean' = f_linf[1], 'StD' = f_linf[2]),
                                                             'K' = list('Mean' = f_k[1], 'StD' = f_k[2]),
                                                             't0' = list('Mean' = f_t0[1], 'StD' = f_t0[2])),
                                             'Male' = list('Linf' = list('Mean' = m_linf[1], 'StD' = m_linf[2]),
                                                           'K' = list('Mean' = m_k[1], 'StD' = m_k[2]),
                                                           't0' = list('Mean' = m_t0[1], 'StD' = m_t0[2])))

                            },
                            setLWpar = function(aF, bF, aM, bM){
                              LWpar <<- array(dim=c(2,2))
                              LWpar[1,] <<- c(aF, bF)
                              LWpar[2,] <<- c(aM, bM)
                            },
                            setBrefs = function(b_value){
                              bRefs <<- b_value
                            },
                            setSelPar = function(L50, L75){
                              tmp_sel <- c(L50, L75)
                              names(tmp_sel) <- c("L50","L75")
                              selPar <<- tmp_sel
                            },
                            genMedmo = function(){

                              LCspe <- lengClas + (lengClas[2]-lengClas[1])/2

                              # !!
                              Areacell <- 9.091279*11.112
                              RateArea <- Areacell/100

                              TempArray <- GenPop(Abbmat = Coh_A_Int, num_cla = length(lengClas),
                                                  LCspe = LCspe, RA = RateArea, qMM = qMedits,
                                                  num_ye = year, num_coh = nCoho, MixtureP = mixPar)

                              #Check
                              cat(specie,"Biomassa", as.character(year[length(year)]), " =",
                                  sum(LFDtoBcell(LCspe = LCspe, abbF = TempArray[,,length(year),1], abbM = TempArray[,,length(year),2],
                                                 LWpar = LWpar))/1000000 ,"\n")

                              popGen <<- TempArray

                            },
                            calcMix = function(nAdap = 100, nSamp = 2000){

                              mixPar <<- list('Female' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)),
                                              'Male' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)))

                              for(sex in c('Female', 'Male')){
                                ind_cou = apply(LFDPop[,,,ifelse(sex == 'Female', 1, 2)], 2, sum)      # Counts of males, per length
                                num_ind = round(ind_cou/(9850/2))
                                ind_dis = rep(lengClas, num_ind) + runif(sum(num_ind), 0, 1)
                                Nclust = nCoho
                                N = length(ind_dis)
                                # Input formating for JAGS
                                alpha = rep(5, Nclust)
                                Z = rep(NA, N)  # initial allocations
                                Z[which.min(ind_dis)] = 1  # smallest value assigned to cluster 1
                                Z[which.max(ind_dis)] = Nclust  # highest value assigned to cluster Nclust
                                dataList = list(y = ind_dis, N = N, Nclust = Nclust, Z = Z, alpha = alpha)
                                model.str <- paste('model{
                                             # Likelihood:
                                             for(i in 1:N){
                                             y[i] ~ dnorm(mean[i], tau[Z[i]])
                                             mean[i] <- Linf * (1 - exp(-k * (Z[i] - t0)))
                                             Z[i] ~ dcat(p[1:Nclust])
                                             }
                                             # Prior:
                                             Linf ~ dnorm(', prior[[sex]][['Linf']][['Mean']],', ', 1/(prior[[sex]][['Linf']][['StD']])^2,')
                                             k ~ dnorm(', prior[[sex]][['K']][['Mean']],', ', 1/(prior[[sex]][['K']][['StD']])^2,')
                                             t0 ~ dnorm(', prior[[sex]][['t0']][['Mean']],', ', 1/(prior[[sex]][['t0']][['StD']])^2,')
                                             p ~ ddirch(alpha)
                                             for(clustIdx in 1:Nclust){
                                             tau[clustIdx] ~ dgamma(1.0E-5, 1.0E-5)
                                             }
                        }', sep = "")
                                jags = jags.model(textConnection(model.str), data = dataList, n.chains = 1, n.adapt = nAdap)
                                TT = nSamp
                                samples = jags.samples(jags, c('Linf', 'k', 't0', 'tau', 'p', 'Z'), TT)
                                # Estimates
                                # predicted length at age
                                LHat = mean(samples$Linf)
                                kHat = mean(samples$k)
                                t0Hat = mean(samples$t0)
                                means = LHat * (1 - exp(-kHat*((1:Nclust) - t0Hat)))
                                taus = matrix(as.numeric(samples$tau), ncol=Nclust, byrow=T)
                                sigma2s = 1/taus
                                tauHat = apply(taus, 2, mean)
                                sigma2Hat = apply(sigma2s, 2, mean)
                                ps = matrix(as.numeric(samples$p), ncol=Nclust, byrow=T)
                                pHat = apply(ps, 2, mean)
                                ma_zHat = numeric(length(lengClas))
                                for(iObs in 1:length(lengClas)){
                                  postProbs = pHat * dnorm(lengClas[iObs], means, sqrt(sigma2Hat))
                                  ma_zHat[iObs] = which.max(postProbs)
                                }
                                asc = seq(min(ind_dis), max(ind_dis), length=200)
                                densities = matrix(0, length(asc), Nclust)
                                dens = numeric(length(asc))
                                for(iasc in 1:length(asc)){
                                  densities[iasc,] = pHat * dnorm(asc[iasc], means, sqrt(sigma2Hat))
                                  dens[iasc] = sum(densities[iasc,])
                                }
                                dens2 = matrix(0, length(asc), length(means))
                                for(j in 1:length(means)){
                                  dens2[,j] = pHat[j] * dnorm(asc, means[j], sqrt(sigma2Hat[j]))
                                }
                                plot(range(asc), c(0, max(hist(ind_dis, plot = FALSE)$density)+0.02), type='n',
                                     main = paste(sex, specie,"- LFD", sep = " "),
                                     xlab = "Density",
                                     ylab = "Length", cex.lab = 0.5)
                                hist(ind_dis, breaks=30, freq=F, col='lightgray', add = TRUE)
                                for(j in 1:length(means)){
                                  polygon(c(min(asc),asc,max(asc)), c(0,dens2[,j],0), col=
                                            ifelse(sex == "Female", rgb(0.8,0.1,0.1,0.2), rgb(0.1,0.1,0.8,0.2)), border=F)
                                }
                                lines(asc, dens)
                                for(yea in 1:length(year)){
                                  a_yea_abb <- cbind(apply(LFDPop[,,yea,ifelse(sex == 'Female', 1, 2)], 2, sum), lengClas, ma_zHat)
                                  for(coho in 1:nCoho){
                                    coho_ind <- which(a_yea_abb[,3] == coho)
                                    sim_pop <- rep(lengClas[coho_ind], a_yea_abb[coho_ind,1])
                                    mixPar[[sex]][['Means']][yea,coho] <<- mean(sim_pop)
                                    mixPar[[sex]][['Sigmas']][yea,coho] <<- sd(sim_pop)
                                  }
                                }
                              }
                            }))



####FisheryBySpecie#################################################
#' FisheryBySpecie
#'
#' The \code{FisheryBySpecie} class implements the class of SMART to
#'  handle species samplings.
#'
#' @return This function returns the 'FisheryBySpecie' object.
#'
#############################################################

FisheryBySpecie <- R6Class("FisheryBySpecie",
                           portable = FALSE,
                           class = TRUE,
                           public = list(
                             specie = NULL,
                             year = NULL,
                             rawLFD = NULL,
                             lengClas = NULL, #LClass
                             LFDPop = NULL,
                             mixPar = NULL, # MixtureP e ncohorts
                             nCoho = NULL,
                             prior = NULL,
                             preMix = NULL,
                             spreDist = list(),
                             spreSpat = list(),
                             sprePlot = list(),
                             sampMcmc = list(),
                             groMixout = list(),
                             Coh_A = NULL,
                             Coh_A_Int = NULL,
                             # Fis_Gro = NULL,
                             LWpar = list(),
                             scorVec = NULL,
                             qMedits = NULL,
                             bRefs = NULL,
                             popGen = NULL,
                             selPar = NULL,
                             setRawData = function(raw_data){rawLFD <<- raw_data},
                             plotLFD = function(){
                               plotSpeAllYea(rawLFD)
                             },
                             initialize = function(sing_spe){
                               setRawData(sing_spe)
                               setYears()
                               setSpecie()
                               setLClass()
                             },
                             setYears = function(){year <<- sort(unique(years(rawLFD[,"Date"])), decreasing = FALSE)},
                             setSpecie = function(){specie <<- unique(rawLFD[,"Specie"])},
                             setLClass = function(){lengClas <<- seq(from = min(rawLFD[,"Class"]), to = max(rawLFD[,"Class"]), by = 1) },
                             setNCoho = function(num_coh){nCoho <<- num_coh},
                             setPrior = function(f_linf, f_k, f_t0, m_linf, m_k, m_t0){
                               prior <<- list('Female' = list('Linf' = list('Mean' = f_linf[1], 'StD' = f_linf[2]),
                                                              'K' = list('Mean' = f_k[1], 'StD' = f_k[2]),
                                                              't0' = list('Mean' = f_t0[1], 'StD' = f_t0[2])),
                                              'Male' = list('Linf' = list('Mean' = m_linf[1], 'StD' = m_linf[2]),
                                                            'K' = list('Mean' = m_k[1], 'StD' = m_k[2]),
                                                            't0' = list('Mean' = m_t0[1], 'StD' = m_t0[2])))

                             },
                             setLWpar = function(alphaVal, betaVal, sex){
                               LWpar[[sex]] <<- list(alpha = as.numeric(alphaVal), beta = as.numeric(betaVal))
                             },
                             setBrefs = function(b_value){
                               bRefs <<- b_value
                             },
                             setSelPar = function(L50, L75){
                               tmp_sel <- c(L50, L75)
                               names(tmp_sel) <- c("L50","L75")
                               selPar <<- tmp_sel
                             },
                             genMedmo = function(){

                               LCspe <- lengClas + (lengClas[2]-lengClas[1])/2

                               # !!
                               Areacell <- 9.091279*11.112
                               RateArea <- Areacell/100

                               TempArray <- GenPop(Abbmat = Coh_A_Int, num_cla = length(lengClas),
                                                   LCspe = LCspe, RA = RateArea, qMM = qMedits,
                                                   num_ye = year, num_coh = nCoho, MixtureP = mixPar)

                               #Check
                               cat(specie,"Biomassa", as.character(year[length(year)]), " =",
                                   sum(LFDtoBcell(LCspe = LCspe, abbF = TempArray[,,length(year),1], abbM = TempArray[,,length(year),2],
                                                  LWpar = LWpar))/1000000 ,"\n")

                               popGen <<- TempArray

                             },
                             calcMix = function(nAdap = 100, nSamp = 2000){

                               mixPar <<- list('Female' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)),
                                               'Male' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)))

                               for(sex in c('Female', 'Male')){
                                 ind_cou = apply(LFDPop[,,,ifelse(sex == 'Female', 1, 2)], 2, sum)      # Counts of males, per length
                                 # num_ind = round(ind_cou/(9850/2))
                                 num_ind = ind_cou
                                 ind_dis = rep(lengClas, num_ind) + runif(sum(num_ind), 0, 1)
                                 Nclust = nCoho
                                 N = length(ind_dis)
                                 # Input formating for JAGS
                                 alpha = rep(5, Nclust)
                                 Z = rep(NA, N)  # initial allocations
                                 Z[which.min(ind_dis)] = 1  # smallest value assigned to cluster 1
                                 Z[which.max(ind_dis)] = Nclust  # highest value assigned to cluster Nclust
                                 dataList = list(y = ind_dis, N = N, Nclust = Nclust, Z = Z, alpha = alpha)
                                 model.str <- paste('model{
                                                 # Likelihood:
                                                 for(i in 1:N){
                                                 y[i] ~ dnorm(mean[i], tau[Z[i]])
                                                 mean[i] <- Linf * (1 - exp(-k * (Z[i] - t0)))
                                                 Z[i] ~ dcat(p[1:Nclust])
                                                 }
                                                 # Prior:
                                                 Linf ~ dnorm(', prior[[sex]][['Linf']][['Mean']],', ', 1/(prior[[sex]][['Linf']][['StD']])^2,')
                                                 k ~ dnorm(', prior[[sex]][['K']][['Mean']],', ', 1/(prior[[sex]][['K']][['StD']])^2,')
                                                 t0 ~ dnorm(', prior[[sex]][['t0']][['Mean']],', ', 1/(prior[[sex]][['t0']][['StD']])^2,')
                                                 p ~ ddirch(alpha)
                                                 for(clustIdx in 1:Nclust){
                                                 tau[clustIdx] ~ dgamma(1.0E-5, 1.0E-5)
                                                 }
                            }', sep = "")
                                 jags = jags.model(textConnection(model.str), data = dataList, n.chains = 1, n.adapt = nAdap)
                                 TT = nSamp
                                 samples = jags.samples(jags, c('Linf', 'k', 't0', 'tau', 'p', 'Z'), TT)
                                 # Estimates
                                 # predicted length at age
                                 LHat = mean(samples$Linf)
                                 kHat = mean(samples$k)
                                 t0Hat = mean(samples$t0)
                                 means = LHat * (1 - exp(-kHat*((1:Nclust) - t0Hat)))
                                 taus = matrix(as.numeric(samples$tau), ncol=Nclust, byrow=T)
                                 sigma2s = 1/taus
                                 tauHat = apply(taus, 2, mean)
                                 sigma2Hat = apply(sigma2s, 2, mean)
                                 ps = matrix(as.numeric(samples$p), ncol=Nclust, byrow=T)
                                 pHat = apply(ps, 2, mean)
                                 ma_zHat = numeric(length(lengClas))
                                 for(iObs in 1:length(lengClas)){
                                   postProbs = pHat * dnorm(lengClas[iObs], means, sqrt(sigma2Hat))
                                   ma_zHat[iObs] = which.max(postProbs)
                                 }
                                 asc = seq(min(ind_dis), max(ind_dis), length=200)
                                 densities = matrix(0, length(asc), Nclust)
                                 dens = numeric(length(asc))
                                 for(iasc in 1:length(asc)){
                                   densities[iasc,] = pHat * dnorm(asc[iasc], means, sqrt(sigma2Hat))
                                   dens[iasc] = sum(densities[iasc,])
                                 }
                                 dens2 = matrix(0, length(asc), length(means))
                                 for(j in 1:length(means)){
                                   dens2[,j] = pHat[j] * dnorm(asc, means[j], sqrt(sigma2Hat[j]))
                                 }
                                 plot(range(asc), c(0, max(hist(ind_dis, plot = FALSE)$density)+0.02), type='n',
                                      main = paste(sex, specie,"- LFD", sep = " "),
                                      xlab = "Density",
                                      ylab = "Length", cex.lab = 0.5)
                                 hist(ind_dis, breaks=30, freq=F, col='lightgray', add = TRUE)
                                 for(j in 1:length(means)){
                                   polygon(c(min(asc),asc,max(asc)), c(0,dens2[,j],0), col=
                                             ifelse(sex == "Female", rgb(0.8,0.1,0.1,0.2), rgb(0.1,0.1,0.8,0.2)), border=F)
                                 }
                                 lines(asc, dens)
                                 for(yea in 1:length(year)){
                                   a_yea_abb <- cbind(apply(LFDPop[,,yea,ifelse(sex == 'Female', 1, 2)], 2, sum), lengClas, ma_zHat)
                                   for(coho in 1:nCoho){
                                     coho_ind <- which(a_yea_abb[,3] == coho)
                                     sim_pop <- rep(lengClas[coho_ind], a_yea_abb[coho_ind,1])
                                     mixPar[[sex]][['Means']][yea,coho] <<- mean(sim_pop)
                                     mixPar[[sex]][['Sigmas']][yea,coho] <<- sd(sim_pop)
                                   }
                                 }
                               }
                             },
                             setSprePlot = function(sampSex){

                               sprePlot[[sampSex]] <<- list(histLfdTot = set_ggHistLfdTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", "#63B8FF")),
                                                            histUtcTot = set_ggHistUtcTot(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", "#63B8FF")),
                                                            dotUtcSplit = set_ggDotUtcSplit(spreDist[[sampSex]]) + scale_color_manual(values = ifelse(sampSex == "Female", "#FF6A6A", "#63B8FF")),
                                                            histUtcLfd = set_ggHistUtcLfd(spreDist[[sampSex]]) + scale_fill_manual(values = ifelse(sampSex == "Female", "#FF6A6A", "#63B8FF")))

                               # if(sampSex == "Female"){
                               #   femalePlot <<- list()
                               #   femalePlot[["histLfdTot"]] <<- set_ggHistLfdTot(femaleSpre)
                               #   femalePlot[["histUtcTot"]] <<- set_ggHistUtcTot(femaleSpre)
                               #   femalePlot[["dotUtcSplit"]] <<- set_ggDotUtcSplit(femaleSpre)
                               #   femalePlot[["histUtcLfd"]] <<- set_ggHistUtcLfd(femaleSpre)
                               # }else{
                               #   malePlot <<- list()
                               #   malePlot[["histLfdTot"]] <<- set_ggHistLfdTot(maleSpre)
                               #   malePlot[["histUtcTot"]] <<- set_ggHistUtcTot(maleSpre)
                               #   malePlot[["dotUtcSplit"]] <<- set_ggDotUtcSplit(maleSpre)
                               #   malePlot[["histUtcLfd"]] <<- set_ggHistUtcLfd(maleSpre)
                               # }
                             },
                             setSpreDistSing = function(){
                               for(sex in c("Female", "Male")){
                                 tmp_spre = rawLFD[!is.na(rawLFD$numFG),c("Date","Class", "numFG", sex)]

                                 num_sex <- sum(tmp_spre[,4])
                                 cat("\nFound", num_sex, sex, as.character(specie), "samples", sep = " ")

                                 spreDist <- data.frame(UTC = rep(tmp_spre$Date, tmp_spre[,4]),
                                                        Length = rep(tmp_spre$Class, tmp_spre[,4]) + runif(num_sex, -0.5, 0.5),
                                                        NumFG = rep(tmp_spre$numFG, tmp_spre[,4]))

                                 spreDist$Year <- years(spreDist$UTC)
                                 spreDist$Month <- months(spreDist$UTC)

                                 spreDist[[sex]] <<- spreDist
                                 # if(sex == "Female"){
                                 #   femaleSpre <<- spreDist
                                 # }else{
                                 #   maleSpre <<- spreDist
                                 # }
                                 setSprePlot(sampSex = sex)
                               }
                             },
                             setSpatDistSing = function(){
                               for(sex in c("Female", "Male")){
                                 tmp_fishSpat <- rawLFD[!is.na(rawLFD$numFG) & rawLFD[,sex] > 0,c("Lon","Lat", "numFG", sex)]

                                 barploFgAll <- data.frame(table(tmp_fishSpat$numFG))
                                 barploFgAll <- barploFgAll[order(as.numeric(as.character(barploFgAll[,1]))),]
                                 barploFgAll$FG <- factor(barploFgAll$Var1, levels = barploFgAll$Var1)
                                 barploFgAll$relFreq = round(100*barploFgAll$Freq/sum(barploFgAll$Freq),1)

                                 spreSpat[[sex]] <<- barploFgAll
                                 # if(sex == "Female"){
                                 #   femaleSpat <<- barploFgAll
                                 # }else{
                                 #   maleSpat <<- barploFgAll
                                 # }
                                 setSpatPlot(sampSex = sex)
                               }
                             },
                             setSpatPlot = function(sampSex){
                               sprePlot[[sampSex]][["spatAbbTbl"]] <<- set_spatAbbTbl(spreSpat[[sampSex]])
                               sprePlot[[sampSex]][["spatAbsFreq"]] <<- set_spatAbsFreq(spreSpat[[sampSex]])
                               sprePlot[[sampSex]][["spatRelFreq"]] <<- set_spatRelFreq(spreSpat[[sampSex]])

                               # if(sampSex == "Female"){
                               #   femalePlot[["spatAbbTbl"]] <<- set_spatAbbTbl(femaleSpat)
                               #   femalePlot[["spatAbsFreq"]] <<- set_spatAbsFreq(femaleSpat)
                               #   femalePlot[["spatRelFreq"]] <<- set_spatRelFreq(femaleSpat)
                               # }else{
                               #   malePlot[["spatAbbTbl"]] <<- set_spatAbbTbl(maleSpat)
                               #   malePlot[["spatAbsFreq"]] <<- set_spatAbsFreq(maleSpat)
                               #   malePlot[["spatRelFreq"]] <<- set_spatRelFreq(maleSpat)
                               # }
                             },
                             setPreMix = function(){
                               # preMix <<- weight2number(rawLFD[,c("DATE","Class","Unsex")])
                             },
                             ggplotMcmcOut = function(selCompo = "MCMC", selSex = "Female"){

                               switch(selCompo,
                                      MCMC = suppressWarnings(grid.arrange(sprePlot[[selSex]][["traceChain"]],
                                                                           sprePlot[[selSex]][["scatLK"]],
                                                                           sprePlot[[selSex]][["cohoPreciGG"]],
                                                                           sprePlot[[selSex]][["cohoVariGG"]],
                                                                           layout_matrix = rbind(c(1,1,1,2),
                                                                                                 c(1,1,1,2),
                                                                                                 c(4,4,5,5)))),
                                      Key = suppressWarnings(grid.arrange(sprePlot[[selSex]][["ageLength"]],
                                                                          sprePlot[[selSex]][["ageLengthTbl"]],
                                                                          sprePlot[[selSex]][["cohoStatTbl"]],
                                                                          layout_matrix = rbind(c(1,1,2),
                                                                                                c(1,1,2),
                                                                                                c(1,1,3)))),
                                      Birth = suppressWarnings(grid.arrange(sprePlot[[selSex]][["histBirth"]],
                                                                            sprePlot[[selSex]][["lineCatch"]],
                                                                            sprePlot[[selSex]][["lineSurv"]],
                                                                            layout_matrix = rbind(c(1,1),
                                                                                                  c(1,1),
                                                                                                  c(2,3))))
                               )
                             },
                             setWeight = function(sexVal = "Female"){
                               groMixout[[sexVal]]$Weight <<- LWpar[[sexVal]][["alpha"]] * groMixout[[sexVal]]$Length ^ LWpar[[sexVal]][["beta"]]
                             },
                             calcMixDate = function(nAdap = 100, nSamp = 2000, sexDrop = "Female", curveSel = "von Bertalanffy"){
                               # mixPar <<- list('Female' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)),
                               #                 'Male' = list('Means' = matrix(NA, length(year), nCoho), 'Sigmas' = matrix(NA, length(year), nCoho)))
                               # for(sex in c("Female", "Male")){ }

                               outPalette <- rainbow(nCoho)

                               # ### FishBase data
                               # mut_popgrowth <- popgrowth("Mullus barbatus barbatus")
                               # ###

                               curDistri <- spreDist[[sexDrop]]

                               # if(sexDrop == "Female"){
                               #   curDistri <- femaleSpre
                               # }else{
                               #   curDistri <- maleSpre
                               # }

                               sub_idx <- sample(1:nrow(curDistri), size = nSamp)
                               sub_data <- curDistri[sub_idx,]

                               N <- length(sub_data$Length)
                               alpha = rep(1, nCoho)
                               Z = rep(NA, N)
                               Z[which.min(sub_data$Length)] = 1
                               Z[which.max(sub_data$Length)] = nCoho

                               dataList <- list(y = sub_data$Length,
                                                maxLeng = max(sub_data$Length),         ## !!!
                                                alpha = alpha,
                                                Z = Z,
                                                N = N,
                                                Nclust = nCoho)

                               inits = list(list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0),
                                            list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0),
                                            list(Linf = max(sub_data$Length), k = 0.5, t0 = 0.0))

                               tt = as.POSIXlt(chron(curDistri$UTC))$yday / 366

                               ### MCMC model setup
                               modelGrow <- ifelse(curveSel == "von Bertalanffy",
                                                   system.file("model/bertGrow.jags", package = "smartR"),
                                                   system.file("model/gompGrow.jags", package = "smartR"))

                               jags.m <- jags.model(modelGrow,
                                                    data = dataList,
                                                    inits = inits,
                                                    n.chains = 3,
                                                    n.adapt = nAdap)
                               ###

                               ### MCMC chain sampling
                               n.iter <- 500
                               obsNode <- c('Linf', 'k', 't0', 'tau', 'p')
                               samps <- coda.samples(jags.m, obsNode, n.iter = n.iter)
                               ###

                               sampMcmc[[sexDrop]] <<- samps

                               ### MCMC estimates
                               dfLinf <- data.frame(Parameter = "Linf",
                                                    Iter = 1:n.iter,
                                                    Chain = as.matrix(samps[,"Linf"], chains = TRUE)[,1],
                                                    Value = as.matrix(samps[,"Linf"], chains = TRUE)[,2])
                               dfKapp <- data.frame(Parameter = "Kappa",
                                                    Iter = 1:n.iter,
                                                    Chain = as.matrix(samps[,"k"], chains = TRUE)[,1],
                                                    Value = as.matrix(samps[,"k"], chains = TRUE)[,2])

                               ggdataSamps <- rbind(dfLinf, dfKapp)
                               ggdataSampScat <- cbind(dfLinf[,2:3],
                                                       Linf = dfLinf[,4],
                                                       Kappa = dfKapp[,4])

                               LHat = mean(as.matrix(samps[,"Linf"]))
                               kHat = mean(as.matrix(samps[,"k"]))
                               t0Hat = mean(as.matrix(samps[,"t0"]))
                               taus <- as.matrix(samps[,grep("tau" ,varnames(samps))])
                               sigma2s = 1/taus
                               sigma2Hat = apply(sigma2s, 2, mean)

                               ### age estimation
                               means.f = matrix(0, nrow(curDistri), nCoho)
                               zHat = numeric(nrow(curDistri))
                               for(iObs in 1:nrow(curDistri)){

                                 if(curveSel == "von Bertalanffy"){
                                   temp <- LHat * (1 - exp(-kHat*(((1:nCoho)-1+tt[iObs]))))
                                 }else{
                                   temp <- LHat *  exp(-(1/kHat * exp(-kHat * ((1:nCoho)-1+tt[iObs]))))
                                 }

                                 ## GGF
                                 ##  temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:nCoho) - 1 - t0Hat))))
                                 # temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:nCoho)-1+tt[iObs]))))  ##BEST
                                 ##  temp = LHat *  exp(-(1/kHat * exp(-kHat * ((1:nCoho)-1+tt[iObs] - t0Hat))))

                                 # VBGF
                                 # temp = LHat * (1 - exp(-kHat*(((1:nCoho)-1+tt[iObs]) - t0Hat)))

                                 means.f[iObs,] = temp
                                 postProbs = dnorm(curDistri$Length[iObs], temp, sqrt(sigma2Hat))
                                 zHat[iObs] = as.numeric(names(which.max(table(sample(1:nCoho, size = 150, prob = postProbs, replace = TRUE)))))
                               }

                               ages.f = zHat -1 + tt #- t0Hat
                               AA = floor(ages.f)

                               ### MCMC output
                               FGlabels = as.numeric(as.character(curDistri$NumFG))
                               FGnames = unique(FGlabels)
                               FG = numeric(length(FGlabels))
                               for(FGname in 1:length(FGnames)){
                                 idx_FG = which(FGlabels == FGnames[FGname])
                                 FG[idx_FG] = rep(FGname, length(idx_FG))
                               }
                               # nFG = length(unique(FG))

                               mix_out <- data.frame(Length = curDistri$Length,
                                                     Date = curDistri$UTC,
                                                     Day = tt,
                                                     Age = AA,
                                                     AgeNF = ages.f,
                                                     FG = FGlabels)

                               mix_out$Year <- years(mix_out$Date)
                               mix_out$Month <- as.numeric(months(mix_out$Date))
                               mix_out$MonthChar <- curDistri$Month
                               mix_out$Quarter <- as.numeric(quarters(mix_out$Date))
                               mix_out$Birth <- as.numeric(as.character(mix_out$Year)) - mix_out$Age

                               zeroedMonth <- ifelse(nchar(mix_out$Month) == 2, mix_out$Month, paste("0", mix_out$Month, sep = ""))
                               mix_out$CatcDate <- factor(paste(mix_out$Year,
                                                                zeroedMonth, sep = "-"),
                                                          levels = paste(rep(sort(unique(mix_out$Year)), each = 12),
                                                                         sort(unique(zeroedMonth)), sep = "-"))


                               groMixout[[sexDrop]] <<- mix_out

                               # mix_out$CorrBirth <- mix_out$Birth
                               # mix_out$CorrBirth[which(mix_out$AgeNF %% 1 + 0.25 > 1)] <- mix_out$CorrBirth[which(mix_out$AgeNF %% 1 + 0.25 > 1)] + 1
                               # mix_out$CorrBirth[which(mix_out$AgeNF - mix_out$Age > 0.81)] <- mix_out$CorrBirth[which(mix_out$AgeNF - mix_out$Age > 0.81)] - 1

                               growPath <- data.frame(Birth = rep(min(mix_out$Birth):(min(mix_out$Birth)+11), each = length(levels(mix_out$CatcDate))),
                                                      Date = rep(levels(mix_out$CatcDate), times = length(min(mix_out$Birth):(min(mix_out$Birth)+11))),
                                                      Length = NA)
                               growPath$Age <- as.numeric(strtrim(growPath$Date, 4)) - growPath$Birth + as.numeric(substr(growPath$Date, 6,7))/12

                               if(curveSel == "von Bertalanffy"){
                                 growPath$Length <- calcVonBert(LHat, kHat, growPath$Age)
                               }else{
                                 growPath$Length <- calcGomp(LHat, kHat, growPath$Age)
                               }

                               growPath$Date <- factor(growPath$Date, levels = levels(mix_out$CatcDate))
                               # growPath <- growPath[growPath$Age > 0,]
                               growPath <- growPath[growPath$Length > floor(min(mix_out$Length)),]

                               coho_AL <- ddply(mix_out, .(Age), summarise,
                                                coh.mean = mean(Length), coh.var = var(Length), coh.num = length(Length))
                               ###

                               ### MCMC calc birth
                               out_birth <- table(paste(mix_out$Year, mix_out$Quarter, sep = "_"),  mix_out$Birth)
                               birth_melt <- melt(out_birth)
                               names(birth_melt) <- c("Catch", "Birth", "Qty")
                               birth_melt$Catch <- factor(birth_melt$Catch, levels = paste(rep(levels(mix_out$Year), each = 4),
                                                                                           rep(1:4, times = length(levels(mix_out$Year))), sep = "_"))
                               birth_melt$Birth <- as.factor(birth_melt$Birth)
                               birth_melt <- birth_melt[birth_melt$Qty != 0,]
                               ###

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

                               ### MCMC chain Traceplot
                               sprePlot[[sexDrop]][["traceChain"]] <<- set_ggChainTrace(ggdataSamps)
                               ### MCMC chain scatterplot
                               sprePlot[[sexDrop]][["scatLK"]] <<- set_ggChainScatter(gg_DFscat = ggdataSampScat, meanL = LHat, meanK = kHat)
                               ### MCMC chain Boxplot Tau
                               sprePlot[[sexDrop]][["cohoPreciGG"]] <<- set_ggTausBox(df_taus = taus[,1:(max(AA)+1)], tauPalette = outPalette, numCoho = nCoho)
                               ### MCMC Boxplot Sigma
                               sprePlot[[sexDrop]][["cohoVariGG"]] <<- set_ggSigmaBox(df_sigma = sigma2s[,1:(max(AA)+1)], sigPalette = outPalette, numCoho = nCoho)

                               ### MCMC Plot Age-Length
                               sprePlot[[sexDrop]][["ageLength"]] <<- set_ggAgeLength(df_mix = mix_out, mixPalette = outPalette)
                               ### MCMC Age-Length Key
                               sprePlot[[sexDrop]][["ageLengthTbl"]] <<- set_tblAgeLength(df_mix = mix_out)
                               ### MCMC output cohort stats
                               sprePlot[[sexDrop]][["cohoStatTbl"]] <<- set_tblCohoStat(df_coho = coho_AL)

                               ### MCMC quarter vertical hist
                               sprePlot[[sexDrop]][["histBirth"]] <<- set_ggHistBirth(df_mix = mix_out, df_grow = growPath)
                               ### MCMC Catch * Quarters
                               sprePlot[[sexDrop]][["lineCatch"]] <<- set_ggCatchLine(df_birth = birth_melt)
                               ### MCMC Survivors * quarter
                               sprePlot[[sexDrop]][["lineSurv"]] <<- set_ggSurvLine(df_surv = surv_melt)
                             }))



####FishFleet################################################
#' FishFleet
#'
#' The \code{FishFleet} class implements the class of SMART
#' to manage fleet data.
#'
#' @return This function returns the 'fishFleet' object.
#'
#############################################################


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
                       inSpatialReg = NULL,
                       inEffortReg = NULL,
                       inProductionReg = NULL,
                       outSpatialReg = NULL,
                       outEffortReg = NULL,
                       outProductionReg = NULL,
                       plotSpatialReg = NULL,
                       plotEffortReg = NULL,
                       plotProductionReg = NULL,
                       setVmsRegister = function(){
                         vmsRegister <<- suppressWarnings(rawRegister[as.numeric(substr(rawRegister$CFR, 4, nchar(rawRegister$CFR[1]))) %in% effortIds$All,])
                       },
                       setRegHarbs = function(){
                         cat("\n\tGetting Harbours coordinates...\n", cat = "")
                         harb_cur_uni <- data.frame(Name = sort(unique(vmsRegister$Port.Name)), Lon = NA, Lat = NA)
                         harb_cur_uni[,2:3] <- geocode(as.character(harb_cur_uni[,1]), output = "latlon" , source = "google")
                         regHarbsUni <<- harb_cur_uni
                         cat("\n\t\tHarbours geocoding completed!", cat = "")
                       },
                       setBetaAvg = function(sel_specie){
                         tmp_df <- data.frame(Month = resNNLS[[sel_specie]]$SceMat$MONTH,
                                              resNNLS[[sel_specie]]$bmat)
                         betaAvg <<- aggregate(. ~ Month, tmp_df, mean)
                       },
                       setEffortAvg = function(){
                         tmp_effo <- aggregate(. ~ Year + MonthNum, effoAllLoa[,-c(2,ncol(effoAllLoa))], sum)
                         effortAvg <<- aggregate(. ~ MonthNum, tmp_effo[,-1], mean)
                       },
                       loadFleetRegis = function(register_path){
                         cat("\nLoading raw Fleet Register data... ", sep = "")
                         rawRegister <<- readRegisterEU(register_path)
                         cat("\nFleet Register loading completed!", sep = "")
                       },
                       loadMatEffort = function(effort_path){
                         cat("\nLoading Effort data... ", sep = "")
                         rawEffort <<- readRDS(effort_path)
                         cat("Done!", sep = "")
                       },
                       loadRawEconomy = function(economic_path){
                         cat("\nLoading Economic data... ", sep = "")
                         rawEconomy <<- read.csv(file = economic_path, stringsAsFactors = FALSE)
                         cat("Done!", sep = "")
                       },
                       setYearEconomy = function(){
                         rawEconomy$Year <<- as.numeric(substr(rawEconomy$DateStart, 7, nchar(rawEconomy$DateStart)))
                       },
                       setInSpatial = function(){
                         agg_EffInd <- aggregate(EffInd ~ I_NCEE + Year + Loa, effortIndex, sum)
                         tmp_spatCost <- rawEconomy[,c("VessID", "Year", "SpatialCost")]
                         inSpatialReg <<- merge(x = tmp_spatCost, y = agg_EffInd,
                                                by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year"))
                       },
                       setInEffort = function(){
                         agg_DaysAtSea <- aggregate(Freq ~ I_NCEE + effYear + Loa + Kw, daysAtSea, sum)
                         tmp_effoCost <- rawEconomy[,c("VessID", "Year", "EffortCost")]
                         inEffortReg <<- merge(x = tmp_effoCost, y = agg_DaysAtSea,
                                                by.x = c("VessID", "Year"), by.y = c("I_NCEE", "Year"))
                       },
                       getRegSpatial = function(){
                         outSpatialReg <<- lm(formula = SpatialCost ~ EffInd - 1, data = inSpatialReg)
                       },
                       getRegEffort = function(){
                         outEffortReg <<- lm(formula = EffortCost ~ Freq + Loa + Kw - 1, data = inEffortReg)
                       },
                       getRegProduction = function(){
                         outProductionReg <<- lm(formula = ProductionCost ~ Production - 1, data = inProductionReg)
                       },
                       getCostOutput = function(){
                         getRegSpatial()
                         getRegEffort()
                         getRegProduction()
                       },
                       setCostPlot = function(){
                         plotSpatialReg <<- ggplot_spatialRegression(df_spatialIn = inSpatialReg, reg_spatialOut = outSpatialReg)
                         plotEffortReg <<- ggplot_effortRegression(df_effortIn = inEffortReg, reg_effortOut = outEffortReg)
                         plotProductionReg <<- ggplot_productionRegression(df_productionIn = inProductionReg, reg_productionOut = outProductionReg)
                       },
                       loadProduction = function(production_path){
                         cat("\nLoading Production data... ", sep = "")
                         sort_files <- sort(production_path)
                         rawProduction <<- list()
                         for(i in 1:length(sort_files)){
                           tmp_mat <- read.csv2(sort_files[i])
                           tmp_key <- names(which.max(table(years(tmp_mat$UTC_S))))
                           rawProduction[[tmp_key]] <<- tmp_mat
                         }
                         cat("Done!", sep = "")
                       },
                       setFishPoinPara = function(speed_range, depth_range){
                         fishPoinPara <<- data.frame(min_spe = speed_range[1],
                                                     max_spe = speed_range[2],
                                                     min_dep = depth_range[1],
                                                     max_dep = depth_range[2])
                       },
                       setWeekMonthNum = function(){
                         cat("\nAdding week and month number to year ", sep = "")
                         for(j in names(rawEffort)){
                           cat(j, "... ", sep = "")
                           tmp_dat <- rawEffort[[j]][,c("DATE")]
                           tmp_date <- as.Date(chron(tmp_dat))
                           rawEffort[[j]]$WeekNum <<- as.numeric(format(tmp_date, "%V"))
                           rawEffort[[j]]$MonthNum <<- as.numeric(format(tmp_date, "%m"))
                         }
                         cat("Done!", sep = "")
                       },
                       setFishPoin = function(){
                         cat("\nComputing fishing points year ", sep = "")
                         for(j in names(rawEffort)){
                           cat(j, "... ", sep = "")
                           tmp_dat <- rawEffort[[j]][,c("SPE","DEPTH")]
                           tmp_dat$FishSpeed <- tmp_dat$SPE >= as.numeric(fishPoinPara[1]) & tmp_dat$SPE <= as.numeric(fishPoinPara[2])
                           tmp_dat$FishDepth <- tmp_dat$DEPTH <= as.numeric(fishPoinPara[3]) & tmp_dat$DEPTH >= as.numeric(fishPoinPara[4])
                           rawEffort[[j]]$FishPoint <<- tmp_dat$FishSpeed & tmp_dat$FishDepth
                         }
                         cat("Done!", sep = "")
                       },
                       plotFishPoinStat = function(){
                         tmp_stat <- data.frame()
                         for(j in names(rawEffort)){
                           tmp_sum <- sum(rawEffort[[j]]$FishPoint)
                           tmp_stat <- rbind(tmp_stat, cbind(j, c("Fishing", "Not Fishing"), rbind(tmp_sum, length(rawEffort[[j]]$FishPoint)-tmp_sum)))
                         }
                         colnames(tmp_stat) <- c("Year", "Status", "Value")
                         rownames(tmp_stat) <- NULL
                         tmp_stat$Value <- as.numeric(as.character(tmp_stat$Value))
                         print(tmp_stat)
                         fishStatPlot <- ggplot(data = tmp_stat, aes(x = Year, y = Value, fill = Status)) +
                           geom_bar(stat="identity", position = position_dodge(), colour="black") +
                           ggtitle("Number of Fishing Points each Year") +
                           scale_fill_manual(values=c("gainsboro", "grey50")) +
                           theme_tufte(base_size = 14, ticks=T) +
                           theme(legend.position = "right",
                                 axis.text.x = element_text(size = 8),
                                 axis.title.x = element_text(size = 10),
                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                 axis.text.y = element_text(size = 8),
                                 axis.title.y = element_text(size = 10),
                                 legend.text = element_text(size = 8),
                                 legend.title = element_text(size = 10))

                         print(fishStatPlot)
                       },
                       plotSpeedDepth = function(which_year, speed_range, depth_range){
                         tmp_dat <- rawEffort[[which_year]][,c("SPE","DEPTH")]
                         op <- par(no.readonly = TRUE)
                         par(mfrow = c(2,1), mar = c(3,0,2,0))
                         speed_hist <- hist(tmp_dat$SPE[which(tmp_dat$SPE <= quantile(tmp_dat$SPE, 0.99) & tmp_dat$SPE > 0)], 100, plot = FALSE)
                         plot(speed_hist, xlab = "Speed", main = "")
                         abline(v = speed_range[1], col = "red", lty = 2)
                         abline(v = speed_range[2], col = "red", lty = 2)
                         text(x = speed_range[1]+((speed_range[2]-speed_range[1])/2), y = max(speed_hist$counts)/2, labels = "FISHING", col = 2)
                         title(main = paste("Speed/Depth profile of ", which_year, sep = ""))
                         depth_hist <- hist(tmp_dat$DEPTH[which(tmp_dat$DEPTH >= quantile(tmp_dat$DEPTH, 0.01) & tmp_dat$DEPTH <= 0)], 100, plot = FALSE)
                         plot(depth_hist, xlab = "Depth", main = "")
                         abline(v = depth_range[1], col = "red", lty = 2)
                         abline(v = depth_range[2], col = "red", lty = 2)
                         text(x = depth_range[1]+((depth_range[2]-depth_range[1])/2), y = max(depth_hist$counts)/2, labels = "FISHING", col = 2)
                         par(op)
                       },
                       setEffortIds = function(){
                         cat("\nSetting Effort IDs x year\n", sep = "")
                         effortIds <<- list()
                         for(i in names(rawEffort)){
                           cat(i, "... ", sep = "")
                           tmp_ids <- unique(rawEffort[[i]][,1])
                           tmp_key <- i
                           effortIds[[tmp_key]] <<- tmp_ids
                         }
                         effortIds[["All"]] <<- unique(unlist(effortIds))
                         cat("Done!\n", sep = "")
                       },
                       setProdSpec = function(){
                         prodSpec <<- list()
                         cat("\nSetting species list x year\n", sep = "")
                         for(i in names(effoProdMont)){
                           cat(i, "... ", sep = "")
                           prodSpec[[i]] <<- colnames(effoProdMont[[i]])[ncol(dayEffoMatr[[i]]):ncol(effoProdMont[[i]])]
                           if(i == names(prodSpec)[1]){
                             prodSpec[["Cross"]] <<- prodSpec[[i]]
                           }else{
                             prodSpec[["Cross"]] <<- intersect(prodSpec[["Cross"]], prodSpec[[i]])
                           }
                         }
                         setSpecSett()
                         setNNLS()
                         cat("Done!\n", sep = "")
                       },
                       setSpecSett = function(){
                         specSett <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
                         names(specSett) <<- sort(prodSpec[["Cross"]])
                       },
                       setNNLS = function(){
                         resNNLS <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
                         names(resNNLS) <<- sort(prodSpec[["Cross"]])
                       },
                       setBetaMeltYear = function(specie){
                         tmp_df <- data.frame(Year = names(effoProd)[resNNLS[[specie]]$SceMat$YEAR],
                                              resNNLS[[specie]]$bmat)
                         tmp_df_agg <- aggregate(. ~ Year, tmp_df, sum)
                         betaMeltYear[[specie]] <<- melt(data = tmp_df_agg, id.vars = "Year",
                                                         measure.vars = c(2:ncol(tmp_df_agg)),
                                                         variable.name = "FishGround", value.name = "Productivity")
                       },
                       setProdMeltYear = function(specie){
                         tmp_df <- data.frame(Year = as.character(effoAllLoa[,1]),
                                              predProd[[specie]])
                         tmp_df_agg <- aggregate(. ~ Year, tmp_df, sum)
                         prodMeltYear[[specie]] <<- melt(data = tmp_df_agg, id.vars = "Year",
                                                         measure.vars = c(2:ncol(tmp_df_agg)),
                                                         variable.name = "FishGround", value.name = "Production")

                       },
                       plotTotProd = function(specie){
                         year_Prod <- aggregate(. ~ Year, prodMeltYear[[specie]][,c(1,3)], sum)
                         year_Prod[,1] <- as.numeric(as.character(year_Prod[,1]))
                         tot_prod <- ggplot_TotalProduction(year_Prod)
                         fg_prod <- ggplot_FGProduction(prodMeltYear[[specie]])
                         grid.arrange(tot_prod,
                                      fg_prod,
                                      layout_matrix = rbind(c(1,1,2),c(1,1,2)),
                                      top = "Production")
                       },
                       plotNNLS = function(specie, thresR2){
                         tmp_df <- data.frame(R2 = "R2",
                                              Values = as.numeric(resNNLS[[specie]][["nnls_r2"]]))
                         bp <- suppressMessages(
                           ggplot(tmp_df, aes(x = R2, y = Values)) +
                             geom_violin(fill = "grey30", colour = "grey90", alpha = 0.05) +
                             geom_boxplot(fill = "grey90", width = 0.5) +
                             stat_boxplot(geom ='errorbar', width = 0.25) +
                             theme(axis.text.x = element_blank()) +
                             ylim(0, 1) +
                             labs(title = "R2 values") +
                             geom_hline(aes(yintercept = thresR2), linetype="dashed", size = 0.5, colour = "red")
                         )
                         tmp_reg <- data.frame(Observed = resNNLS[[specie]]$obsY,
                                               Fitted = resNNLS[[specie]]$fittedY)

                         reg_p <- suppressMessages(
                           ggplot(tmp_reg, aes(y = Fitted, x = Observed)) +
                             geom_point(alpha = 0.25, size = 0.2) + stat_smooth(method = "lm") +
                             labs(title = "Observed VS Fitted") +
                             scale_x_log10() +
                             scale_y_log10() +
                             annotation_logticks()
                         )
                         suppressWarnings(
                           grid.arrange(reg_p, bp, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
                         )
                       },
                       setSpecSettItm = function(specie, thresh, brea, max_xlim){
                         specSett[[specie]] <<- data.frame(threshold = thresh,
                                                           breaks = brea,
                                                           max_x = max_xlim)
                       },
                       # setSpecLogitROCR = function(specie){
                       #   ROCRpred <- prediction(specLogit[[specie]]$logit$Predict, 1*(specLogit[[specie]]$landings > specSett[[specie]]$threshold))
                       #   specLogit[[specie]]$ROCRperf <<- performance(ROCRpred, "tpr", "fpr")
                       # },
                       # setSpecLogitOptCut = function(specie){
                       #   analysis <- pROC::roc(response = specLogit[[specie]]$landings, predictor = specLogit[[specie]]$logit$Predict)
                       #   tuning <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
                       #   specLogit[[specie]]$optCut <<- tuning[which.max(tuning[,2]),1]
                       # },
                       plotLogitROC = function(selSpecie){
                         plot(specLogit[[selSpecie]]$logit$Roc, print.cutoffs.at = seq(0,1,0.1),
                              text.adj = c(-0.2, 1.7), main = "Logit ROC results")
                       },
                       setSpecLogitConf = function(selSpecie, cutoff = specLogit[[selSpecie]]$logit$Cut){
                         predict <- factor(as.numeric(specLogit[[selSpecie]]$logit$Predict > cutoff))
                         # if(specLogit[[selSpecie]]$logit$Name == "GLM"){
                         #   predict <- factor(as.numeric(specLogit[[selSpecie]]$logit$Predict > cutoff))
                         # }else{
                         #   predict <- factor(as.numeric(specLogit[[selSpecie]]$logit$Predict[,2] > cutoff))
                         # }
                         truth <- factor(1*(specLogit[[selSpecie]]$Landings[-specLogit[[selSpecie]]$logit$Split] > specSett[[selSpecie]]$threshold))
                         tmp_Tbl <- table(predict, truth)
                         specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(tmp_Tbl)
                       },
                       setLogitTrain = function(selSpecie, train, cp_val = 0.01, cv_val = 2){
                         specLogit[[selSpecie]]$logit$Model <<- switch(specLogit[[selSpecie]]$logit$Name,
                                                                       GLM = {glm(Target ~ ., family = binomial(logit), data = train)},
                                                                       CART = {rpart::rpart(Target ~ ., data = train, method = "class",
                                                                                            control = rpart.control(cp = cp_val))},
                                                                       RF = {caret::train(Target ~ . , data = train, method = "rf",
                                                                                          trControl = trainControl(method = "cv", number = cv_val),
                                                                                          prox = TRUE, allowParallel = TRUE, metric = "Kappa",
                                                                                          maximize = TRUE)},
                                                                       NN = {   })
                       },
                       setLogitTest = function(selSpecie, test){
                         specLogit[[selSpecie]]$logit$Predict <<- switch(specLogit[[selSpecie]]$logit$Name,
                                                                         GLM  = {predict(specLogit[[selSpecie]]$logit$Model,
                                                                                         newdata = test, type = "response")},
                                                                         CART = {predict(specLogit[[selSpecie]]$logit$Model,
                                                                                         newdata = test, type="prob")[,2]},
                                                                         RF   = {predict(specLogit[[selSpecie]]$logit$Model,
                                                                                         newdata = test, type = "prob")[,2]},
                                                                         NN   = {   })
                       },
                       setLogitPred = function(selSpecie, test){
                         specLogit[[selSpecie]]$logit$Prediction <<- ROCR::prediction(specLogit[[selSpecie]]$logit$Predict, test$Target)
                         # if(specLogit[[selSpecie]]$logit$Name == "GLM"){
                         #   specLogit[[selSpecie]]$logit$Prediction <<- ROCR::prediction(specLogit[[selSpecie]]$logit$Predict,
                         #                                                                test$Target)
                         # }else{
                         #   specLogit[[selSpecie]]$logit$Prediction <<- ROCR::prediction(specLogit[[selSpecie]]$logit$Predict[,2],
                         #                                                                test$Target)
                         # }
                       },
                       setLogitCut = function(selSpecie){
                         perf <- ROCR::performance(specLogit[[selSpecie]]$logit$Prediction, "acc")
                         specLogit[[selSpecie]]$logit$Cut <<- perf@x.values[[1]][which.max(perf@y.values[[1]])]
                       },
                       setLogitRoc = function(selSpecie){
                         specLogit[[selSpecie]]$logit$Roc <<- ROCR::performance(specLogit[[selSpecie]]$logit$Prediction, "tpr", "fpr")
                       },
                       setLogitConf = function(selSpecie, test){
                         specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(as.factor(specLogit[[selSpecie]]$logit$Predict > specLogit[[selSpecie]]$logit$Cut),
                                                                                           test$Target)
                         # if(specLogit[[selSpecie]]$logit$Name == "GLM"){
                         #   specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(as.factor(specLogit[[selSpecie]]$logit$Predict > specLogit[[selSpecie]]$logit$Cut),
                         #                          test$Target)
                         # }else{
                         #   specLogit[[selSpecie]]$logit$Confusion <<- caret::confusionMatrix(as.factor(specLogit[[selSpecie]]$logit$Predict[,2] > specLogit[[selSpecie]]$logit$Cut),
                         #                          test$Target)
                         #   }
                       },
                       setSpecLogit = function(selSpecie, selModel = c("GLM", "CART", "RF", "NN")[1],
                                               cp = 0.01, cv = 2){
                         if(is.null(specLogit)) specLogit <<- list()
                         if(is.null(specLogit[[selSpecie]])) specLogit[[selSpecie]] <<- list()

                         tmp_mat <- getMatSpeLand(selSpecie)

                         colnames(tmp_mat)[ncol(tmp_mat)] <- "Target"
                         specLogit[[selSpecie]]$Landings <<- tmp_mat$Target
                         tmp_mat$Target <- as.factor(tmp_mat$Target > specSett[[selSpecie]]$threshold)

                         split = caret::createDataPartition(y = tmp_mat$Target, p = 0.7, list = FALSE)[,1]
                         train <- tmp_mat[split,]
                         test <- tmp_mat[-split,]
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
                       getMatSpeLand = function(specie){
                         tmp_mat <- effoProdAllLoa[,c(1,3:(ncol(dayEffoMatr[[1]])),
                                                      which(colnames(effoProdAllLoa) == "Loa"),
                                                      which(colnames(effoProdAllLoa) == specie))]
                         tmp_mat$MonthNum <- as.factor(tmp_mat$MonthNum)
                         return(tmp_mat)
                       },
                       setEffoProdAll = function(){
                         cat("\nSetting Effort x Production year\n", sep = "")
                         tmp_spe <- sort(prodSpec[["Cross"]])
                         for(i in names(effoProdMont)){
                           cat(i, "... ", sep = "")
                           tmp_nam <- colnames(effoProdMont[[i]])
                           tmp_cols <- which(tmp_nam %in% tmp_spe)
                           if(i == names(effoProdMont)[1]){
                             effoProdAll <<- cbind(Year = i, effoProdMont[[i]][,c(1:(ncol(dayEffoMatr[[i]])-1), tmp_cols[order(tmp_nam[tmp_cols])])])
                           }else{
                             effoProdAll <<- rbind(effoProdAll, cbind(Year = i, effoProdMont[[i]][,c(1:(ncol(dayEffoMatr[[i]])-1), tmp_cols[order(tmp_nam[tmp_cols])])]))
                           }
                         }
                         cat("Done!\n", sep = "")
                       },
                       setEffoAll = function(){
                         cat("\nSetting Effort x Year\n", sep = "")
                         for(i in names(effoMont)){
                           cat(i, "... ", sep = "")
                           if(i == names(effoMont)[1]){
                             effoAll <<- cbind(Year = i, effoMont[[i]])
                           }else{
                             effoAll <<- rbind(effoAll, cbind(Year = i, effoMont[[i]]))
                           }
                         }
                         cat("Done!", sep = "")
                       },
                       setEffoProdAllLoa = function(){
                         tmp_effoProd <- effoProdAll
                         tmp_loa <- rawRegister[,c("CFR","Loa")]
                         tmp_loa$CFR <- substr(tmp_loa$CFR, 4, nchar(tmp_loa$CFR[1]))
                         names(tmp_loa) <- c("I_NCEE", "Loa")
                         tmp_allLoa <- sqldf("select * from tmp_effoProd left join (select * from tmp_loa) using (I_NCEE)")
                         effoProdAllLoa <<- tmp_allLoa[-which(is.na(tmp_allLoa), arr.ind = TRUE)[,1],]
                       },
                       setEffoAllLoa = function(){
                         cat("\nSetting Effort LOA... ", sep = "")
                         tmp_effo <- effoAll
                         tmp_loa <- rawRegister[,c("CFR","Loa")]
                         tmp_loa$CFR <- substr(tmp_loa$CFR, 4, nchar(tmp_loa$CFR[1]))
                         names(tmp_loa) <- c("I_NCEE", "Loa")
                         tmp_Loa <- sqldf("select * from tmp_effo left join (select * from tmp_loa) using (I_NCEE)")
                         effoAllLoa <<- tmp_Loa[-which(is.na(tmp_Loa), arr.ind = TRUE)[,1],]
                         cat("Done!\n", sep = "")
                       },
                       setProdIds = function(){
                         cat("\nSetting Production IDs x year\n", sep = "")
                         productionIds <<- list()
                         for(i in names(rawProduction)){
                           cat(i, "... ", sep = "")
                           tmp_ids <- unique(rawProduction[[i]][,1])
                           tmp_key <- i
                           productionIds[[tmp_key]] <<- tmp_ids
                         }
                         productionIds[["All"]] <<- unique(unlist(productionIds))
                         cat("Done!\n", sep = "")
                       },
                       setIdsEffoProd = function(){
                         ###   Set IDs cross match effort/production
                         to_match <- names(effortIds)[names(effortIds) %in% names(productionIds)]
                         idsEffoProd <<- list()
                         for(i in to_match){
                           idsEffoProd[[i]] <<- effortIds[[i]][effortIds[[i]] %in% productionIds[[i]]]
                         }
                       },
                       plotCountIDsEffoProd = function(){
                         tmp_effo <- data.frame("Year" = names(effortIds),
                                                "Ids" = unlist(lapply(effortIds, length)),
                                                "Dataset" = "Effort")
                         tmp_prod <- data.frame("Year" = names(productionIds),
                                                "Ids" = unlist(lapply(productionIds, length)),
                                                "Dataset" = "Production")
                         tmp_comb <- data.frame("Year" = names(idsEffoProd),
                                                "Ids" = unlist(lapply(idsEffoProd, length)),
                                                "Dataset" = "Overlap")
                         tmp_df <- rbind(tmp_effo, tmp_prod, tmp_comb)
                         rownames(tmp_df) <- NULL
                         tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids, fill = Dataset)) +
                           geom_bar(position=position_dodge(), stat = "identity") +
                           geom_text(aes(y=Ids, label = Ids), position= position_dodge(width=1),
                                     vjust=2.5, color="grey20") +
                           ggtitle("Count of Distinct Vessels") +
                           ylab("N. of IDs") +
                           theme_tufte(base_size = 14, ticks=T) +
                           theme(legend.position = "right",
                                 axis.text.x = element_text(size = 8),
                                 axis.title.x = element_text(size = 10),
                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                 axis.text.y = element_text(size = 8),
                                 axis.title.y = element_text(size = 10),
                                 legend.text = element_text(size = 8),
                                 legend.title = element_text(size = 10))
                         print(tmp_plot)
                       },
                       plotCountIDsEffo = function(){
                         tmp_df <- data.frame("Year" = names(effortIds),
                                              "Ids" = unlist(lapply(unique(effortIds), length)))
                         tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
                           geom_text(aes(y=Ids, label = Ids), position= position_dodge(width=1),
                                     vjust=2.5, color="white") +
                           ggtitle("Count of Distinct Vessels - Effort Dataset") +
                           ylab("N. of IDs") +
                           theme_tufte(base_size = 14, ticks=T) +
                           theme(legend.position = "none",
                                 axis.text.x = element_text(size = 8),
                                 axis.title.x = element_text(size = 10),
                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                 axis.text.y = element_text(size = 8),
                                 axis.title.y = element_text(size = 10),
                                 legend.text = element_text(size = 8),
                                 legend.title = element_text(size = 10))
                         print(tmp_plot)
                       },
                       plotCountIDsProd = function(){
                         tmp_df <- data.frame("Year" = names(productionIds),
                                              "Ids" = unlist(lapply(unique(productionIds), length)))
                         tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
                           geom_text(aes(y=Ids, label = Ids), position= position_dodge(width=1),
                                     vjust=2.5, color="white") +
                           ggtitle("Count of Distinct Vessels - Production Dataset") +
                           ylab("N. of IDs") +
                           theme_tufte(base_size = 14, ticks=T) +
                           theme(legend.position = "none",
                                 axis.text.x = element_text(size = 8),
                                 axis.title.x = element_text(size = 10),
                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                 axis.text.y = element_text(size = 8),
                                 axis.title.y = element_text(size = 10),
                                 legend.text = element_text(size = 8),
                                 legend.title = element_text(size = 10))
                         print(tmp_plot)
                       },
                       setEffoProdMatr = function(){
                         effoProd <<- list()
                         cat("\nCreating Effort x Production matrix\n", sep = "")
                         for(i in intersect(names(dayEffoMatr), names(prodMatr))){
                           cat(i,"... ", sep = "")
                           tmp_effo <- dayEffoMatr[[i]]
                           tmp_prod <- prodMatr[[i]]
                           effoProd[[i]] <<- sqldf("select * from tmp_effo, tmp_prod where I_NCEE = NUMUE and DATE >= UTC_S and DATE <= UTC_E")
                         }
                         cat("Done!\n")
                       },
                       setEffoProdMont = function(){
                         effoProdMont <<- list()
                         cat("\nMatching Effort x FG with Production\n", sep = "")
                         for(i in names(effoProd)){
                           cat(i,"... ", sep = "")
                           dis_vesmon <- unique(effoProd[[i]][,c("I_NCEE", "MonthNum")])
                           effoProdMont[[i]] <<- data.frame(matrix(data = 0, nrow = nrow(dis_vesmon), ncol = ncol(effoProd[[i]])-5))
                           colnames(effoProdMont[[i]]) <<- c(colnames(dayEffoMatr[[i]])[-2], colnames(prodMatr[[i]])[-c(1:4)])
                           effoProdMont[[i]][,1:2] <<- dis_vesmon
                           for(j in 1:nrow(dis_vesmon)){
                             tmp_itm <- effoProd[[i]][which(effoProd[[i]]$I_NCEE == dis_vesmon[j,1] & effoProd[[i]]$MonthNum == dis_vesmon[j,2]),]
                             effoProdMont[[i]][j,3:(ncol(dayEffoMatr[[i]])-1)] <<- apply(unique(tmp_itm[,4:ncol(dayEffoMatr[[i]])]),2,sum)
                             tmp_prod_itm <- unique(tmp_itm[,c(ncol(dayEffoMatr[[i]])+1,(ncol(dayEffoMatr[[i]])+5):ncol(tmp_itm))])
                             effoProdMont[[i]][j,(ncol(dayEffoMatr[[i]])):ncol(effoProdMont[[i]])] <<- apply(tmp_prod_itm[,2:ncol(tmp_prod_itm)], 2, sum)
                           }
                         }
                         cat("Done!\n")
                       },
                       setEffoMont = function(){
                         effoMont <<- list()
                         cat("\nAggregating year by month\n", sep = "")
                         for(i in names(dayEffoMatr)){
                           cat(i,"... ", sep = "")
                           dis_vesmon <- unique(dayEffoMatr[[i]][,c("I_NCEE", "MonthNum")])
                           effoMont[[i]] <<- data.frame(matrix(data = 0, nrow = nrow(dis_vesmon), ncol = ncol(dayEffoMatr[[i]])-1))
                           colnames(effoMont[[i]]) <<- c(colnames(dayEffoMatr[[i]])[-2])
                           effoMont[[i]][,1:2] <<- dis_vesmon
                           for(j in 1:nrow(dis_vesmon)){
                             tmp_itm <- dayEffoMatr[[i]][which(dayEffoMatr[[i]]$I_NCEE == dis_vesmon[j,1] & dayEffoMatr[[i]]$MonthNum == dis_vesmon[j,2]),]
                             effoMont[[i]][j,3:(ncol(dayEffoMatr[[i]])-1)] <<- apply(unique(tmp_itm[,4:ncol(dayEffoMatr[[i]])]),2,sum)
                           }
                         }
                         cat("Done!")
                       },
                       setProdMatr = function(){
                         prodMatr <<- list()
                         for(i in names(rawProduction)){
                           tmp_prod <- rawProduction[[i]]
                           tmp_prod <- tmp_prod[tmp_prod$NUMUE %in% idsEffoProd[[i]],]
                           tmp_matrix <- dcast(tmp_prod,
                                               NUMUE + UTC_S + UTC_E ~ SPECIES, fun.aggregate = sum,
                                               na.rm=TRUE, value.var = "KGS")
                           tmp_matrix <- cbind("prodID" = 1:nrow(tmp_matrix), tmp_matrix)
                           prodMatr[[i]] <<- tmp_matrix
                         }
                       },
                       setDayEffoMatrGround = function(){
                         dayEffoMatr <<- list()
                         cat("\nCreating daily effort x Fishing Ground matrix\n", sep = "")
                         for(j in names(rawEffort)){
                           cat(j, "... ", sep = "")
                           tmp_dat <- rawEffort[[j]][rawEffort[[j]]$FishPoint == TRUE & rawEffort[[j]]$P_INT == 1 & !is.na(rawEffort[[j]]$Cell),c("I_NCEE","DATE", "MonthNum", "FishGround", "FishPoint")]
                           tmp_dat$DATE <- ceiling(tmp_dat$DATE)
                           tmp_matrix <- dcast(tmp_dat, formula = I_NCEE + DATE + MonthNum ~ FishGround,
                                               fun.aggregate = sum, na.rm=TRUE, value.var = "FishPoint")
                           ## points to hours: interpolation interval 10 min
                           tmp_matrix[,4:ncol(tmp_matrix)] <- tmp_matrix[,4:ncol(tmp_matrix)]/6
                           # miss_cols <- setdiff(as.character(unique(rawEffort[[j]]$FishGround[!is.na(rawEffort[[j]]$FishGround)])),
                           #                      names(tmp_matrix)[4:ncol(tmp_matrix)])
                           miss_cols <- setdiff(1:max(rawEffort[[j]]$FishGround[!is.na(rawEffort[[j]]$FishGround)]),
                                                names(tmp_matrix)[4:ncol(tmp_matrix)])
                           if(length(miss_cols) > 0){
                             # tmp_matrix[,miss_cols] <- 0
                             tmp_matrix[,paste(miss_cols)] <- 0
                             # tmp_matrix <- tmp_matrix[,c(1:4, 4+order(as.numeric(names(tmp_matrix)[5:ncol(tmp_matrix)])))]
                             tmp_matrix <- tmp_matrix[, c(1:3, 3 + order(as.numeric(names(tmp_matrix)[4:ncol(tmp_matrix)])))]
                           }
                           dayEffoMatr[[j]] <<- tmp_matrix
                         }
                         cat("Done!\n", sep = "")
                       },
                       getLoa4Prod = function(){
                         if(!is.null(productionIds) & !is.null(registerIds)){
                           tmp_reg <- rawRegister[,c("CFR", "Loa")]
                           tmp_reg[,1] <- substr(tmp_reg[,1], 4, nchar(tmp_reg[1,1]))

                           tmp_pro <- data.frame("CFR" = productionIds[["All"]])
                           tmp_pro[,1] <- paste(unlist(lapply(mapply(rep, times = 9-nchar(tmp_pro[,1]), x = 0), paste, collapse = "")),
                                                tmp_pro[,1], sep = "")

                           prodIdsLoa <<- merge(x = tmp_pro, y = tmp_reg, by = "CFR")
                         }
                       },
                       plotLoaProd = function(){
                         tmp_tab <- table(round(prodIdsLoa[,2]))
                         tmp_df <- data.frame("Length" = names(tmp_tab),
                                              "Count" = as.numeric(tmp_tab))
                         ggplot(tmp_df, aes(x = Length, y = Count)) + geom_bar(stat = "identity") +
                           geom_text(aes(y = Count, label = Count), position= position_dodge(width=1),
                                     vjust=-.5, color="black") +
                           ggtitle("Count of Distinct Vessels") +
                           ylab("N. of IDs")
                       },
                       #                        plotRawProduction = function(){
                       #                          for(i in 1:length(rawProduction)){
                       #                            if(i == 1){
                       #                            tmp_data <- cbind(tmp_lst[[i]], years(tmp_lst[[i]]$UTC_S))
                       #                            }else{
                       #                              tmp_data <- rbind(tmp_data,
                       #                                                cbind(tmp_lst[[i]], years(tmp_lst[[i]]$UTC_S)))
                       #                            }
                       #                          }
                       #
                       #                        },
                       readRegisterEU = function(reg_path){
                         cat("\n\tChecking EU Fleet Register format...", sep = "")
                         two_rows <- readLines(con = reg_path, n = 2)
                         last_char <- substr(two_rows[2], nchar(two_rows[2]), nchar(two_rows[2]))
                         raw_fleet <- readLines(con = reg_path, n = -1)
                         if(last_char == ";"){
                           cat("\n\tTrailing character found! Cleaning...", sep = "")
                           tmp_flee <- paste(unlist(lapply(strsplit(raw_fleet, split = ";"), paste, collapse = ";")), collapse = "\n")
                         }else{
                           tmp_flee <- paste(raw_fleet, collapse = "\n")
                         }
                         if(substr(reg_path, nchar(reg_path)-12, nchar(reg_path)) != "_smart-ed.csv"){
                           tmp_flee <- gsub("\\;",",", tmp_flee)
                           new_path <- paste(substr(reg_path, 1, nchar(reg_path)-4), "_smart-ed",
                                             substr(reg_path, nchar(reg_path)-3, nchar(reg_path)), sep = "")
                           cat("\nWriting edited Fleet register in:\n", new_path, "\n", sep = "")
                           write(tmp_flee, file = new_path)
                           reg_path <- new_path
                         }
                         cat("\nLoading file... ", sep = "")
                         re_fleet <- read.csv(reg_path, stringsAsFactors = FALSE)
                         cat("OK", sep = "")
                         return(re_fleet)
                       },
                       cleanRegister = function(){
                         cat("\n\tOrdering Fleet Register by CFR... ", sep = "")
                         rawRegister$CFR <<- as.character(rawRegister$CFR)
                         rawRegister <<- rawRegister[order(rawRegister$CFR),]
                         rawRegister$Country.Code <<- as.character(rawRegister$Country.Code)
                         setRegIds()
                         rawRegister$Loa <<- as.numeric(as.character(rawRegister$Loa))
                         rawRegister$Power.Main <<- as.numeric(as.character(rawRegister$Power.Main))
                         cat("OK\n", sep = "")
                       },
                       plotRegSum = function(){
                         cat("Plotting Fleet register summary statistics... ", sep = "")
                         def.par <- par(no.readonly = TRUE)
                         layout(matrix(c(1,2,3,4,5,6),2,3,byrow = TRUE))
                         plotBarReg(regVar = "Gear.Main.Code", title = "Main Gear")
                         plotBarReg(regVar = "Gear.Sec.Code", title = "Secondary Gear")
                         plotBarReg(regVar = "Hull.Material", title = "Hull Material")
                         plotBoxReg(regVar = "Construction.Year", title = "Year of Construction")
                         plotBoxReg(regVar = "Loa", title = "Length Overall")
                         plotBoxReg(regVar = "Power.Main", title = "Power")
                         par(def.par)
                         cat("Completed\n", sep = "")
                       },
                       plotBarReg = function(regVar, p_las = 2, title = regVar){
                         barplot(table(rawRegister[,regVar]), las = p_las, main = title)
                       },
                       plotBoxReg = function(regVar, title = regVar){
                         boxplot(rawRegister[,regVar], main = title)
                       },
                       setRegIds = function(){
                         registerIds <<- rawRegister$CFR
                       },
                       by_ship = NULL,
                       splitFleet = function(){
                         cat("Splitting Fleet into ships...\n", sep = "")
                         by_ship <<- apply(rawRegister, 1, buildShip)
                       },
                       buildShip = function(cur_ship){
                         tmp_ship <- FishShip$new()
                         tmp_ship$setShip(ship_id = cur_ship["CFR"],
                                          ship_loa = cur_ship["Loa"],
                                          ship_pow = cur_ship["Power.Main"],
                                          ship_act = NA,
                                          ship_met = NA,
                                          ship_m_gea = cur_ship["Gear.Main.Code"],
                                          ship_s_gea = cur_ship["Gear.Sec.Code"],
                                          ship_eff = NA,
                                          ship_sel = NA,
                                          ship_pro = NA)
                         return(tmp_ship)
                       }
                     )
)


FishShip <- R6Class("fishShip",
                    portable = FALSE,
                    class = TRUE,
                    public = list(
                      shipId = NULL,    # from VMS, Fleet Register or Survey
                      shipLoa = NULL,   # from Fleet Register
                      shipPow = NULL,   # from Fleet Register or Model
                      shipAct = NULL,   # from Fleet Register
                      shipMet = NULL,   # from Fleet Register
                      shipMaiGea = NULL,   # from Fleet Register
                      shipSecGea = NULL,   # from Fleet Register
                      shipEff = NULL,   # from VMS
                      shipSel = NULL,   # from Survey
                      shipPro = NULL,   # from Survey
                      setShip = function(ship_id = NA,
                                         ship_loa = NA,
                                         ship_pow = NA,
                                         ship_act = NA,
                                         ship_met = NA,
                                         ship_m_gea = NA,
                                         ship_s_gea = NA,
                                         ship_eff = NA,
                                         ship_sel = NA,
                                         ship_pro = NA){
                        setShipId(ship_id)
                        setShipLoa(ship_loa)
                        setShipPow(ship_pow)
                        setShipAct(ship_act)
                        setShipMet(ship_met)
                        setShipMaiGea(ship_m_gea)
                        setShipSecGea(ship_s_gea)
                        setShipEff(ship_eff)
                        setShipSel(ship_sel)
                        setShipPro(ship_pro)
                      },
                      setShipId = function(id){
                        shipId <<- id
                      },
                      setShipLoa = function(loa){
                        shipLoa <<- loa
                      },
                      setShipPow = function(pow){
                        shipPow <<- pow
                      },
                      setShipAct = function(act){
                        shipAct <<- act
                      },
                      setShipMet = function(met){
                        shipMet <<- met
                      },
                      setShipMaiGea = function(m_gea){
                        shipMaiGea <<- m_gea
                      },
                      setShipSecGea = function(s_gea){
                        shipSecGea <<- s_gea
                      },
                      setShipEff = function(eff){
                        shipEff <<- eff
                      },
                      setShipSel = function(sel){
                        shipSel <<- sel
                      },
                      setShipPro = function(pro){
                        shipPro <<- pro
                      }
                    )
)



####SampleMap################################################
#' SampleMap
#'
#' The \code{SampleMap} class implements the class of SMART
#' to control geographical data.
#'
#' @return This function returns the 'sampleMap' object.
#'
#############################################################


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
                       sCells = NULL, # cMedits
                       griCent = NULL, # gCenter
                       gridBathy = NULL,
                       centDept = NULL,
                       clusInpu = NULL, # calcfish input
                       clusMat = NULL, # matrix output calcfish
                       indSil = NULL, # vect clusters silhouette output calcfish
                       indCH = NULL, # vect index CH output calcfish
                       cutFG = NULL,
                       tmpCut = NULL,
                       availData = NULL,
                       rawInpu = NULL,
                       cutResult = NULL,
                       cutResEffo = NULL,
                       cutResShp = NULL,
                       cutResShpCent = NULL,
                       cutResShpFort = NULL,
                       fgWeigDist = NULL,
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
                       ggMapFgSamp = NULL,
                       gooMap = NULL,
                       gooMapPlot = NULL,
                       gooGrid = NULL,
                       gooBbox = NULL,
                       sampColScale = NULL,
                       plotRange = NULL,
                       initialize = function(grid_path){
                         setGridPath(grid_path)
                         setGridName()
                         loadGridShp()
                         setNumCell()
                         createPolySet()
                         fortifyGridShp()
                         setGridCenter()
                         createGridBbox()
                       },
                       setAreaGrid = function(){
                         cat("\n\nComputing Total Area... ", sep = "")
                         clipDept <- as.SpatialGridDataFrame(gridBathy)
                         tmp_grid <- gridShp
                         proj4string(tmp_grid) <- proj4string(clipDept)
                         clipDept[which(is.na(over(clipDept, tmp_grid)))] <- NA
                         clipDept <- as.bathy(clipDept)
                         areaGrid <<- get.area(clipDept, level.inf = -Inf, level.sup = 0)[[1]]
                         cat("Completed!", sep = "")
                       },
                       setAreaStrata = function(vectorStrata = c(0, 10, 100, 1000, Inf)){
                         cat("\n\nComputing Area of Strata: ", sep = "")
                         clipDept <- as.SpatialGridDataFrame(gridBathy)
                         tmp_grid <- gridShp
                         proj4string(tmp_grid) <- proj4string(clipDept)
                         clipDept[which(is.na(over(clipDept, tmp_grid)))] <- NA
                         clipDept <- as.bathy(clipDept)
                         strataList <- list()
                         for(stratum in 1:(length(vectorStrata)-1)){
                           stratum_ith <- paste(vectorStrata[stratum],
                                                "-",
                                                vectorStrata[stratum+1],
                                                sep = "")
                           cat("\n\t", stratum_ith, "... ", sep = "")
                           strataList[[stratum_ith]] <- get.area(clipDept, level.inf = -vectorStrata[stratum+1], level.sup = -vectorStrata[stratum])
                           cat("Done!", sep = "")
                         }
                         areaStrata <<- strataList
                         cat("\nCompleted!", sep = "")
                       },
                       setWeightStrata = function(){
                         cat("\n\nComputing Strata Weighted Area... ", sep = "")
                         weightStrata <<- unlist(lapply(areaStrata, "[[", 1))/areaGrid
                         cat("Completed!")
                       },
                       loadHarbDbf = function(dbf_path){
                         tmp_dbf <- read.dbf(file = dbf_path)
                         colnames(tmp_dbf) <- c("XCOORD", "YCOORD", "Name")
                         harbDbf <<- tmp_dbf
                       },
                       set_ggMapFgSamp = function(rawSampCoo){
                         ggMapFgSamp <<- suppressMessages(
                           gooMapPlot +
                             geom_polygon(data = cutResShpFort,
                                          aes(x = long, y = lat, group = group),
                                          colour = "grey10", size = 0.1, alpha = 0.8) +
                             lims(x = extendrange(plotRange[1:2]),
                                  y = extendrange(plotRange[3:4])) +
                             theme(legend.position = "right",
                                   axis.text.x = element_text(size = 5),
                                   axis.title.x = element_text(size = 7),
                                   axis.text.y = element_text(size = 5),
                                   legend.key.size = unit(0.5, "cm"),
                                   legend.text = element_text(size = 4),
                                   legend.title = element_text(size = 5),
                                   axis.title.y = element_text(size = 7)) +
                             geom_jitter(data = rawSampCoo[,c("Lon","Lat","Specie")],
                                         aes(x = Lon, y = Lat, shape = Specie, color = Specie),
                                         size = 1, width = 0.1, height = 0.1, alpha = 0.35) +
                             geom_point(data = unique(rawSampCoo[,c("Lon","Lat")]),
                                        aes(x = Lon, y = Lat),
                                        color = "darkolivegreen1", shape = 4, size = 0.5, alpha = 0.6) +
                             geom_text(data = cutResShpCent,
                                       aes(label = FG, x = Lon, y = Lat),
                                       size = 2, color = "grey72")
                         )
                       },
                       createGridBbox = function(){
                         gridBbox <<- bbox(gridShp)
                         gridBboxExt <<- make_bbox(lon = gridBbox[1,],
                                                   lat = gridBbox[2,],
                                                   f = 0.1)

                         polyext <- Polygon(cbind(c(gridBboxExt[["left"]],gridBboxExt[["left"]],gridBboxExt[["right"]],gridBboxExt[["right"]],gridBboxExt[["left"]]),
                                                  c(gridBboxExt[["bottom"]],gridBboxExt[["top"]],gridBboxExt[["top"]],gridBboxExt[["bottom"]],gridBboxExt[["bottom"]])))

                         polypolyext = Polygons(list(polyext), "s1")

                         gridBboxSP <<- SpatialPolygons(list(polypolyext))
                       },
                       getGooMap = function(){
                         gooMap <<- get_googlemap(center = c(lon = mean(gridPolySet$X), lat = mean(gridPolySet$Y)),
                                                  zoom = MaxZoom(latrange = c(gridBboxExt[2], gridBboxExt[4]),
                                                                 lonrange = c(gridBboxExt[1], gridBboxExt[3])),
                                                  size = c(640, 640), scale = 2, format = "png8", maptype = "hybrid",
                                                  color = "color")
                         setGooPlot()
                         setPlotRange()
                       },
                       setGooPlot = function(){
                         gooMapPlot <<- ggmap(gooMap) + xlab("Longitude") + ylab("Latitude")
                       },
                       setPlotRange = function(){
                         plotRange <<- data.frame(xmin=gridBboxExt[1],
                                                  xmax=gridBboxExt[3],
                                                  ymin=gridBboxExt[2],
                                                  ymax=gridBboxExt[4])
                       },
                       setGooGrid = function(){
                         gooGrid <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group),
                                                                                fill = 'grey', size = 0.1,
                                                                                color = 'gainsboro', data = gridFortify, alpha = 0.5) +
                                                        lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                        xlab("Longitude") + ylab("Latitude")+
                                                        theme_tufte(base_size = 14, ticks=T) +
                                                        theme(legend.position = "none",
                                                              axis.text.x = element_text(size = 8),
                                                              axis.title.x = element_text(size = 10),
                                                              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                              axis.text.y = element_text(size = 8),
                                                              axis.title.y = element_text(size = 10)))
                       },
                       plotGooGrid = function(){
                         suppressWarnings(print(gooGrid))
                       },
                       plotGooGridData = function(grid_data){

                         gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID),
                                                   fill = 'grey', size = 0.2,
                                                   color = 'gainsboro', data = grid_data, alpha = 0.5) +
                           coord_fixed(xlim = extendrange(plotRange[1:2]),
                                       ylim = extendrange(plotRange[3:4]), expand = TRUE)
                       },
                       setSampColScale = function(fac_col){
                         myColors <- brewer.pal(length(fac_col), "Set1")
                         names(myColors) <- fac_col
                         sampColScale <<- scale_colour_manual(name = "Specie",values = myColors)
                       },
                       plotGooSpeSur = function(poi_data){
                         temp_pos <- suppressMessages(gooGrid + geom_jitter(data = poi_data,
                                                                            aes(x = Lon, y = Lat, shape = Specie, color = Specie),
                                                                            width = 0.05, height = 0.05, alpha = 0.95) + sampColScale)
                         suppressWarnings(print(temp_pos))
                       },
                       plotGooSpeFis = function(poi_data){
                         temp_pos <- suppressMessages(gooGrid + geom_jitter(data = poi_data,
                                                                            aes(x = Lon, y = Lat, shape = Specie, color = Specie),
                                                                            width = 0.05, height = 0.05, alpha = 0.95))
                         # temp_pos <- gooGrid + geom_jitter(data = poi_data,
                         #                             aes(x = Lon, y = Lat, shape = Specie, color = Specie),
                         #                             width = 0.05, height = 0.05, alpha = 0.95)
                         # print(temp_pos)
                         suppressWarnings(print(temp_pos))
                       },
                       setGooBbox = function(){
                         text_x <- mean(gridBboxExt[c(1,3)])
                         text_y <- mean(gridBboxExt[c(2,4)])
                         gooBbox <<- suppressMessages(gooGrid + geom_rect(data=plotRange, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                                                          color="firebrick",
                                                                          fill = alpha('red', 0.2),
                                                                          inherit.aes = FALSE) +
                                                        annotate("label", x = text_x, y = text_y,
                                                                 label="Bounding\nBox", family="serif", fontface="italic",
                                                                 colour="firebrick", size=6, fill = "grey80") +
                                                        theme_tufte(base_size = 14, ticks=T) +
                                                        theme(legend.position = "none",
                                                              axis.text.x = element_text(size = 8),
                                                              axis.title.x = element_text(size = 10),
                                                              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                              axis.text.y = element_text(size = 8),
                                                              axis.title.y = element_text(size = 10)))
                       },
                       plotGooBbox = function(){
                         suppressWarnings(print(gooBbox))
                       },
                       setGridPath = function(path2grid){
                         gridPath <<- path2grid
                       },
                       setGridName = function(){
                         tmp_name <- unlist(strsplit(gridPath, "/"))
                         tmp_name <- tmp_name[length(tmp_name)]
                         gridName <<- substr(tmp_name, 1, nchar(tmp_name)-4)
                       },
                       loadGridShp = function(){
                         gridShp <<- readShapePoly(gridPath)
                       },
                       setBioPath = function(path2bio){
                         bioPath <<- path2bio
                       },
                       setBioName = function(){
                         tmp_name <- unlist(strsplit(bioPath, "/"))
                         tmp_name <- tmp_name[length(tmp_name)]
                         bioName <<- substr(tmp_name, 1, nchar(tmp_name)-4)
                       },
                       loadBioShp = function(){
                         bioShp <<- readShapePoly(bioPath)
                       },
                       addBioShp = function(bio_path){
                         setBioPath(bio_path)
                         setBioName()
                         loadBioShp()
                       },
                       loadBioDF = function(bio_path){
                         bioDF <<- readRDS(bio_path)
                         setGgBioDF()
                       },
                       plotBioDF = function(){
                         def.par <- par(no.readonly = TRUE)
                         par(mar=c(2.5,2.5,3,1))
                         layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(6,1))

                         vec_bio <- apply(bioDF, 1, function(x) which(x == 1))
                         color_clas <- rainbow(max(vec_bio))
                         plotSamMap(title = "Biocenosis", celCol = color_clas[vec_bio])

                         par(mar=c(5,1,1,1))
                         par(mar=c(1,0,1,0))

                         plot(NULL, xlim=c(0,1), ylim=c(0,1), bty="n", axes = FALSE, ann = FALSE)
                         legend(x = 0, y = 0.5, legend = colnames(bioDF), fill = color_clas, bty = "n")
                         par(def.par)
                       },
                       setGgBioDF = function(){
                         cell_bed <- apply(bioDF, 1, which.max)

                         zero_cell <- which(apply(bioDF, 1, sum) == 0)

                         tmp_dat <- colnames(bioDF)[cell_bed]

                         if(length(zero_cell) > 0) tmp_dat[zero_cell] <- "No Data"

                         color_clas <- rainbow(max(cell_bed))
                         names(tmp_dat) <- 1:length(tmp_dat)
                         all_cell <- merge(x = gridPolySet$PID,
                                           data.frame(x = as.numeric(names(tmp_dat)), y = tmp_dat,
                                                      stringsAsFactors = FALSE), all = TRUE)
                         all_cell[is.na(all_cell)] <- 0
                         grid_data <- cbind(gridPolySet, Seabed = all_cell[,2])
                         ggBioDF <<- suppressMessages(gooMapPlot +
                                                        geom_polygon(aes(x = X, y = Y, group = PID, fill = Seabed), size = 0.2,
                                                                     data = grid_data, alpha = 0.8) +
                                                        lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                        xlab("Longitude") + ylab("Latitude") +
                                                        theme_tufte(base_size = 14, ticks=T) +
                                                        theme(legend.position = "right",
                                                              axis.text.x = element_text(size = 8),
                                                              axis.title.x = element_text(size = 10),
                                                              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                              axis.text.y = element_text(size = 8),
                                                              axis.title.y = element_text(size = 10),
                                                              legend.text = element_text(size = 8),
                                                              legend.title = element_text(size = 10),
                                                              plot.title = element_blank()))
                       },
                       ggplotBioDF = function(){
                         suppressWarnings(print(ggBioDF))
                       },
                       createPolySet = function(){
                         gridPolySet <<- as.data.frame(SpatialPolygons2PolySet(gridShp))
                       },
                       fortifyGridShp = function(){
                         gridFortify <<- fortify(gridShp)
                       },
                       setNumCell = function(){
                         nCells <<- length(gridShp@polygons)
                       },
                       setGridCenter = function(){
                         griCent <<- coordinates(gridShp) # or gCentroid from rgeos
                       },
                       getGridBath = function(){
                         lon_ran <- extendrange(griCent[,1], f = 0.05)
                         lat_ran <- extendrange(griCent[,2], f = 0.05)
                         gridBathy <<- getNOAA.bathy(lon1 = lon_ran[1],
                                                     lon2 = lon_ran[2],
                                                     lat1 = lat_ran[1],
                                                     lat2 = lat_ran[2], resolution = 1)
                         getCentDept()
                         setGgDepth()
                       },
                       saveGridBath = function(bathy_path){
                         saveRDS(object = gridBathy, file = bathy_path)
                       },
                       loadGridBath = function(bathy_path){
                         gridBathy <<- readRDS(bathy_path)
                         getCentDept()
                         setGgDepth()
                       },
                       getCentDept = function(){
                         centDept <<- get.depth(gridBathy, x = griCent[,1], y = griCent[,2], locator = FALSE)
                       },
                       setGgDepth = function(isoLine = c(-200, -1000)){
                         f_bathy <- fortify.bathy(gridBathy)
                         f_bathy$z[f_bathy$z > 0] <- 0
                         colnames(f_bathy) <- c("lon", "lat", "Depth")
                         ggDepth <<- suppressMessages(gooMapPlot +
                                                        geom_contour(aes(z = Depth, colour = factor(..level..)), data = f_bathy,
                                                                     linetype = "solid", size = 0.35,
                                                                     breaks = isoLine, alpha = 1) +
                                                        scale_colour_brewer(palette = "Accent", name="Isobath") +
                                                        guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1))) +
                                                        lims(x = extendrange(plotRange[1:2]),
                                                             y = extendrange(plotRange[3:4])) +
                                                        xlab("Longitude") + ylab("Latitude") +
                                                        theme_tufte(base_size = 14, ticks=T) +
                                                        theme(legend.position = "right",
                                                              axis.text.x = element_text(size = 8),
                                                              axis.title.x = element_text(size = 10),
                                                              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                              axis.text.y = element_text(size = 8),
                                                              axis.title.y = element_text(size = 10),
                                                              legend.text = element_text(size = 8),
                                                              legend.title = element_text(size = 10),
                                                              plot.title = element_blank()))
                       },
                       ggplotGridBathy = function(){
                         suppressWarnings(print(ggDepth))
                       },
                       plotSamMap = function(title = "", celCol = NULL){
                         par(mar = c(3,3,1.5,0.5))
                         plotPolys(gridPolySet, main = title, ylab = "", xlab = "", col = celCol, axes = TRUE,
                                   xlim = extendrange(c(gridShp@bbox[1,1], gridShp@bbox[1,2]), f = 0.05),
                                   ylim = extendrange(c(gridShp@bbox[2,1], gridShp@bbox[2,2]), f = 0.05))
                         mtext("Longitude", side=1, line=2.1, cex = 1.5)
                         mtext("Latitude", side=2, line=2, cex = 1.5)
                         map("worldHires", fill = T, col = "gainsboro", add = TRUE)
                         # map.axes(cex.axis=0.8)
                         map.scale(cex = 0.75, ratio = FALSE)
                       },
                       plotCoho = function(abbs){
                         x_rang <- extendrange(c(gridShp@bbox[1,1], gridShp@bbox[1,2]), f = 0.05)
                         y_rang <- extendrange(c(gridShp@bbox[2,1], gridShp@bbox[2,2]), f = 0.07)
                         plotPolys(gridPolySet, ylab = "", xlab = "",
                                   xlim = x_rang,
                                   ylim = y_rang)
                         mtext("Longitude", side=1, line=2.1, cex = 1.1)
                         mtext("Latitude", side=2, line=2, cex = 1.1)
                         smoothScatter(griCent[rep.int(1:nrow(griCent), abbs+1),],
                                       colramp = colorRampPalette(c("white", "white", "blue", "purple"),bias = 0.77, alpha = 0.5),
                                       add = TRUE, nbin = 250, xlab = NULL, ylab = NULL, nrpoints = 0)
                         plot(gridShp, ylab = "", xlab = "",
                              add =TRUE, lwd = 0.25)
                         map("worldHires", fill = T, col = "gainsboro",
                             xlim = x_rang,
                             ylim = y_rang, add = TRUE)
                         map.scale(x = min(abs(x_rang))+(diff(x_rang)/10), y = min(abs(y_rang))+(diff(y_rang)/7), cex = 0.65, ratio = FALSE)

                       },
                       setClusInpu = function(whiData = rep(TRUE,3), howData = rep(1, 3)){
                         truIdx <- which(whiData == TRUE)
                         if(length(truIdx)>0){
                           tmp_lst <- list()
                           for(i in truIdx){
                             if(i == 1){
                               cat("\n   -   Seabed")
                               tmp_lst <- c(tmp_lst, list(rawInpu[[i]] * switch(howData[i], "0.5X" = 0.5, "1" = 1, "2X" = 2)))
                               cat("\t\t-   Set!")
                             } else if(i == 2){
                               cat("\n   -   Effort")
                               tmp_lst <- c(tmp_lst, list(vegan::decostand(log10(1 + rawInpu[[i]]), method = "range", MARGIN = 2) * switch(howData[i], "0.5X" = 0.5, "1" = 1, "2X" = 2)))
                               cat("\t\t-   Set!")
                             } else if(i == 3){
                               cat("\n   -   Depth")
                               tmp_inpu <- -rawInpu[[i]]
                               tmp_inpu[tmp_inpu < 0] <- 0
                               tmp_lst <- c(tmp_lst, Depth = list(vegan::decostand(log10(1 + tmp_inpu), method = "range", MARGIN = 2) * switch(howData[i], "0.5X" = 0.5, "1" = 1, "2X" = 2)))
                               cat("\t\t-   Set!\n")
                             }
                           }
                           clusInpu <<- do.call(cbind, tmp_lst)
                         }
                       },
                       calcFishGrou = function(numCuts = 50,
                                               minsize = 10,
                                               maxsize = 100,
                                               modeska = "S",
                                               skater_method,
                                               nei_queen = TRUE){
                         set.seed(123)
                         #Build the neighboorhod list
                         Grid.bh <- gridShp[1]
                         ##  Construct neighbours list from polygon list
                         bh.nb <- poly2nb(Grid.bh, queen = nei_queen)
                         bh.mat <- cbind(rep(1:length(bh.nb), lapply(bh.nb,length)), unlist(bh.nb))
                         #Compute the basic objects for clustering
                         ##  Cost of each edge as the distance between nodes
                         lcosts <- nbcosts(bh.nb, clusInpu)
                         ##  Spatial weights for neighbours lists
                         nb.w <- nb2listw(bh.nb, lcosts, style = modeska)
                         ##  Find the minimal spanning tree
                         mst.bh <- mstree(nb.w, ini = 1)
                         clusMat <<- matrix(NA, nCells, numCuts)
                         cat("\nClustering", sep = "")
                         res1 <- skater(edges = mst.bh[,1:2],
                                        data = clusInpu,
                                        ncuts = 1,
                                        crit = minsize,
                                        method = skater_method)
                         clusMat[,1] <<- res1$groups

                         #Perform the first CC (without removing spurious clusters)
                         for(nCuts in 2:numCuts){
                           cat(".", sep = "")
                           ##  Spatial 'K'luster Analysis by Tree Edge Removal
                           #                            res1 <- skater(mst.bh[,1:2], cells_data, ncuts = nCuts, minsize,
                           #                                           method = skater_method)

                           if(nCuts > ceiling(nrow(clusInpu)/maxsize)){
                             res1 <- skater(res1, clusInpu, ncuts = 1,
                                            crit = c(minsize, maxsize),
                                            method = skater_method)
                           }else{
                             res1 <- skater(res1, clusInpu, ncuts = 1,
                                            crit = minsize,
                                            method = skater_method)
                           }
                           clusMat[,nCuts] <<- res1$groups
                         }
                         cat(" Done!", sep = "")

                         indSil <<- numeric(ncol(clusMat))
                         indCH <<- numeric(ncol(clusMat))
                         for(i in 1:ncol(clusMat)){
                           ##  Compute silhouette information according to a given clustering in k clusters
                           indSil[i] <<- summary(silhouette(clusMat[,i],dist(clusInpu,
                                                                             method = skater_method)))$avg.width
                           ##  Compute CH index for a given partition of a data set
                           indCH[i] <<-  get_CH(as.matrix(clusInpu),clusMat[,i])
                         }
                       },
                       plotFishGrou = function(ind_clu){
                         def.par <- par(no.readonly = TRUE)
                         layout(matrix(c(1,3,2,3),2,2,byrow = TRUE), c(1,3), TRUE)
                         plot(indCH, type = "l", ann = FALSE)
                         title("CH")
                         abline(v = ind_clu, col = "red", lty = 2)
                         plot(indSil, type = "l", ann = FALSE)
                         title("Silhouette")
                         abline(v = ind_clu, col = "red", lty = 2)
                         plotSamMap(title = paste("Max Width with ", ind_clu, " cuts", sep = ""),
                                    celCol = (rainbow(length(unique(clusMat[,ind_clu]))))[clusMat[,ind_clu]])
                         par(def.par)
                       },
                       # setCutResShpCent = function(){
                       #   cutResShpCent <<- getLabBuffer(cutResShp)
                       #   cutResShpCent$id <<- rownames(cutResShpCent)
                       #   names(cutResShpCent) <<- c("Lon", "Lat", "FG")
                       # },
                       setCutResult = function(ind_clu){
                         tmpCut <<- ind_clu
                         # cutResult <<- data.frame(clusInpu, FG = as.factor(clusMat[,ind_clu]))
                         cutResult <<- data.frame(do.call(cbind, rawInpu), FG = as.factor(clusMat[,ind_clu]))
                         cutResEffo <<- data.frame(Effort = apply(cutResult[, grep("Year", colnames(cutResult))],1, mean),
                                                   Cluster = cutResult[,ncol(cutResult)])
                         cutResShp <<- unionSpatialPolygons(gridShp, IDs = clusMat[,ind_clu])

                         # num_cell <- getinfo.shape(cutResShp)$entities
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
                         setIchFGlin()
                         setSilFGlin()
                       },
                       setDepthFGbox = function(){
                         ggDepthFGbox <<- suppressMessages(ggplot(cutResult, aes(x = FG, y = Depth, group = FG)) +
                                                             geom_boxplot(color = "grey23") +
                                                             coord_flip() +
                                                             theme_tufte(base_size = 14, ticks=T) +
                                                             ylim(NA, 0) +
                                                             theme(legend.position = "none",
                                                                   axis.text.x = element_text(size = 8),
                                                                   axis.title.x = element_text(size = 10),
                                                                   panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                                   axis.text.y = element_text(size = 8),
                                                                   axis.title.y = element_text(size = 10),
                                                                   legend.text = element_text(size = 8),
                                                                   legend.title = element_text(size = 10)))
                       },
                       setEffoFGbox = function(){
                         ggEffoFGbox <<- suppressMessages(ggplot(cutResEffo, aes(x = Cluster, y = Effort, group = Cluster)) +
                                                            geom_boxplot(color = "grey23") +
                                                            coord_flip() +
                                                            theme_tufte(base_size = 14, ticks=T) +
                                                            theme(legend.position = "none",
                                                                  axis.text.x = element_text(size = 8),
                                                                  axis.title.x = element_text(size = 10),
                                                                  panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                                  axis.text.y = element_text(size = 8),
                                                                  axis.title.y = element_text(size = 10),
                                                                  legend.text = element_text(size = 8),
                                                                  legend.title = element_text(size = 10)))
                       },
                       setEffoFGmap = function(){
                         agg_eff <- aggregate(formula = Effort ~ Cluster, data = cutResEffo, FUN = mean)
                         all_cell <- merge(x = cutResShpFort$id,
                                           data.frame(x = agg_eff$Cluster, y = agg_eff$Effort), all = TRUE)
                         all_cell[is.na(all_cell)] <- 0
                         grid_data <- cbind(cutResShpFort, Hours = all_cell[,2])
                         ggEffoFGmap <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = Hours),
                                                                                    colour = "black", size = 0.1,
                                                                                    data = grid_data, alpha = 0.8) +
                                                            scale_fill_gradient(low = "Yellow", high = "coral", trans = "sqrt") +
                                                            geom_text(aes(label = FG, x = Lon, y = Lat),
                                                                      data = cutResShpCent, size = 2) +
                                                            lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                            theme(legend.position='none'))
                       },
                       setBioFGmat = function(){
                         # ind_col <- which(make.names(colnames(bioDF)) %in% colnames(cutResult))
                         # if(length(ind_col) > 0){
                         # tmp_bio <- data.frame(FG = cutResult$FG, cutResult[,ind_col])
                         tmp_bio <- data.frame(FG = cutResult$FG, bioDF)
                         bio2plot <- melt(tmp_bio, id.vars="FG", variable.name = "Substrate")
                         bio2plot <- bio2plot[bio2plot$value == 1,1:2]
                         ggBioFGmat <<- suppressMessages(ggplot(bio2plot, aes(x = FG, y = Substrate, fill = Substrate)) +
                                                           geom_tile() +
                                                           coord_flip() +
                                                           annotate("text", colour = "grey30", y = 1:length(levels(bio2plot$Substrate)),
                                                                    x = rep(4, length(levels(bio2plot$Substrate))),
                                                                    label = levels(bio2plot$Substrate),
                                                                    angle = rep(90, length(levels(bio2plot$Substrate)))) +
                                                           theme_tufte(base_size = 14, ticks=T) +
                                                           theme(legend.position = "none",
                                                                 axis.text.x = element_text(size = 8),
                                                                 axis.title.x = element_text(size = 10, colour = "white"),
                                                                 panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                                                 axis.text.y = element_text(size = 8),
                                                                 axis.title.y = element_text(size = 10),
                                                                 legend.text = element_text(size = 8),
                                                                 legend.title = element_text(size = 10)))
                         # }
                       },
                       setCutFGmap = function(){
                         ggCutFGmap <<- suppressMessages(gooMapPlot +
                                                           geom_polygon(aes(x = long, y = lat, group = group, fill = FG),
                                                                        colour = "black", size = 0.1,
                                                                        data = cutResShpFort, alpha = 0.8) +
                                                           geom_text(aes(label = FG, x = Lon, y = Lat),
                                                                     data = cutResShpCent, size = 2) +
                                                           lims(x = extendrange(plotRange[1:2]),
                                                                y = extendrange(plotRange[3:4])) +
                                                           theme(legend.position='none'))
                       },
                       setIchFGlin = function(){
                         ch_df <- data.frame(cut = 1:length(indCH),
                                             CH_index = indCH)
                         ggIchFGlin <<- suppressMessages(ggplot(ch_df, aes(x = cut, y = CH_index)) +
                                                           geom_line() +
                                                           geom_vline(aes(xintercept = tmpCut), linetype="dashed", size = 0.5, colour = "red"))
                       },
                       setSilFGlin = function(){
                         sil_df <- data.frame(cut = 1:length(indSil),
                                              Sil_index = indSil)
                         ggSilFGlin <<- suppressMessages(ggplot(sil_df, aes(x = cut, y = Sil_index)) +
                                                           geom_line() +
                                                           geom_vline(aes(xintercept = tmpCut), linetype="dashed", size = 0.5, colour = "red"))
                       }
                     ))

