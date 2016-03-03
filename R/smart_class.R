#############################################################
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
                          rawData = NULL,
                          years = NULL,
                          species = NULL,
                          bySpecie = NULL,
                          sampMap = NULL,
                          fleet = NULL,
                          loadRawLFD = function(csv_path) {
                            cat("Loading sampling data...\n", sep = "")
                            rawData <<- read.table(file = csv_path, sep = ";", dec = ".", colClasses = c("character", "numeric", "numeric", "factor", "numeric", "numeric", "numeric", "numeric"), header = TRUE)
                            cat("Setting Years... ", sep = "")
                            setYears()
                            cat(" from ", min(levels(years)[as.numeric(years)]), " to ", max(levels(years)[as.numeric(years)]),"\nSetting Species... ", sep = "")
                            setSpecies()
                            cat(" found: ", paste(species, collapse = " - "), "\nSplitting Species...", sep = "")
                            splitSpecies()
                            cat(" completed!", sep = "")
                          },
                          setYears = function(){years <<- sort(unique(rawData[,"Year"]), decreasing = FALSE)},
                          loadMap = function(map_path){sampMap <<- SampleMap$new(map_path)},
                          createFleet = function(){fleet <<- FishFleet$new()},
                          setSpecies = function(){species <<- unique(rawData[,"SPECIE"])},
                          splitSpecies = function(){
                            if(length(species) == 1){
                              addSpecie(rawData)
                            }else{
                              for(i in 1:length(species)){
                                addSpecie(rawData[rawData[,"SPECIE"] == species[i],])}}
                          },
                          addSpecie = function(sing_spe){bySpecie <<- c(bySpecie, BySpeLFD$new(sing_spe))},
                          setLFDPop = function(){
                            if(length(species) == 1){
                              calcLFDPop(1)
                            }else{
                              for(i in 1:length(species)){
                                calcLFDPop(i)
                              }}
                            # speDisPlot("All")
                          },
                          loadFleeEffoDbs = function(effort_path, met_nam, onBox = TRUE){
                            cat("\nLoading Effort data...\n", sep = "")
                            sort_files <- sort(effort_path)
                            fleet$rawEffort <<- list()
                            for(i in sort_files){
                              cat("\nLoading db: ", i, sep = "")
                              cat("\nSelecting tracks in box...", sep = "")
                              tmp_eff <- fn$sqldf("select * from (select * from (select *, rowid as i_id from intrp) join (select distinct I_NCEE, T_NUM from intrp where I_NCEE in (select distinct I_NCEE from nn_clas where met_des = '`met_nam`') and LON > `sampMap$gridBboxSP@bbox[1,1]` and LON < `sampMap$gridBboxSP@bbox[1,2]` and LAT > `sampMap$gridBboxSP@bbox[2,1]` and LAT < `sampMap$gridBboxSP@bbox[2,2]`) using (I_NCEE, T_NUM)) join (select * from p_depth) using (i_id)", dbname = i)                             ### Over in B-Box
                              if(onBox){
                                in_box <- over(SpatialPoints(tmp_eff[,c("LON","LAT")]), sampMap$gridBboxSP)
                              }else{
                                in_box <- over(SpatialPoints(tmp_eff[,c("LON","LAT")]),
                                               unionSpatialPolygons(sampMap$gridShp,
                                                                    IDs = rep(1,
                                                                              length(sampMap$gridShp@polygons)))
                                )
                              }
                              in_box[is.na(in_box)] <- 0
                              tmp_eff$in_box <- in_box

                              in_box_ping <- sqldf("select I_NCEE, T_NUM, sum(in_box) from tmp_eff group by I_NCEE, T_NUM")
                              all_ping <- sqldf("select I_NCEE, T_NUM, count(*) from tmp_eff group by I_NCEE, T_NUM")

                              ### Loading 100% only
                              cat("\nLoading 100% in box only...", sep = "")
                              all_in_box <- in_box_ping[which(in_box_ping[,3]/all_ping[,3] == 1),1:2]
                              all_sos <- sqldf("select * from tmp_eff join (select * from all_in_box) using (I_NCEE, T_NUM)")

                              cat("\nSaving Data", sep = "")
                              tmp_key <- names(which.max(table(years(all_sos$DATE))))
                              fleet$rawEffort[[tmp_key]] <<- all_sos
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
                              for(i in 1:length(species)){
                                points(bySpecie[[i]]$rawData[,c("LON","LAT")], pch = 20, col = 1+i, cex = 0.4)
                              }
                            }else{
                              sampMap$plotSamMap(whoPlo)
                              points(bySpecie[[which(species == whoPlo)]]$rawData[,c("LON","LAT")], pch = 20, col = 1+which(species == whoPlo), cex = 0.4)
                            }
                          },
                          plotGooSpe = function(whoPlo){
                            if(whoPlo == "All"){
                              tmp_data <- unique(rawData[,c("SPECIE", "LAT", "LON")])
                            }else{
                              tmp_data <- unique(rawData[which(rawData$SPECIE == whoPlo),c("SPECIE", "LAT", "LON")])
                              levels(tmp_data[,1]) <- unique(rawData[,"SPECIE"])
                            }
                            sampMap$plotGooGridPoi(tmp_data)
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
                            tmp_dat <- fleet$rawEffort[[year]][sample(1:nrow(fleet$rawEffort[[year]]), 100000),c("LON","LAT","W_HARB")]
                            tmp_dat$W_HARB <- as.factor(tmp_dat$W_HARB)
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot +
                                                           geom_point(data = tmp_dat,
                                                                      aes(x = LON, y = LAT, shape = W_HARB, color = W_HARB), size = 1, alpha = 0.2)+
                                                           scale_colour_manual(values = c("coral", "darkseagreen1")) +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]), y = extendrange(sampMap$plotRange[3:4])) +
                                                           guides(colour = guide_legend(override.aes = list(size=3, alpha = 1))))
                            suppressWarnings(print(tmp_plot))
                          }
                          ,
                          ggplotFishingPoints = function(year){
                            tmp_dat <- fleet$rawEffort[[year]][sample(1:nrow(fleet$rawEffort[[year]]), 100000),c("LON","LAT","FishPoint")]
                            tmp_dat$FishPoint <- as.factor(tmp_dat$FishPoint)
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot +
                                                           geom_point(data = tmp_dat,
                                                                      aes(x = LON, y = LAT, color = FishPoint), size = 0.25, alpha = 0.2)+
                                                           scale_colour_manual(values = c("coral", "darkseagreen1")) +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]), y = extendrange(sampMap$plotRange[3:4])) +
                                                           guides(colour = guide_legend(override.aes = list(size=3, alpha = 1))))
                            suppressWarnings(print(tmp_plot))
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
                            cat("\nSetting Fishing Ground year ", sep = "")
                            for(j in names(fleet$rawEffort)){
                              cat(j, "... ", sep = "")
                              fleet$rawEffort[[j]]$FishGround <<- tmp_clust[fleet$rawEffort[[j]]$Cell,2]
                            }
                            cat("Done!", sep = "")
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
                            grid_data <- cbind(sampMap$gridPolySet, LogCount = log(all_cell[,2] + 1))
                            tmp_plot <- suppressMessages(sampMap$gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID, fill = LogCount), size = 0.2,
                                                                                           data = grid_data, alpha = 0.8) +
                                                           scale_fill_gradient(low = "Yellow", high = "coral") +
                                                           lims(x = extendrange(sampMap$plotRange[1:2]), y = extendrange(sampMap$plotRange[3:4])))
                            suppressWarnings(print(tmp_plot))
                          },
                          getNnlsModel = function(specie, minobs, thr_r2){
                            data <- fleet$effoProdAllLoa
                            nFG <- sampMap$cutFG
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

                            if(anyNA(bmat)){
                              blist[[1]] <- fillbetas(bmat)
                            }else{
                              blist[[1]] <- bmat
                            }

                            blist[[2]] <- unlist(obsY)
                            blist[[3]] <- unlist(fittedY)
                            blist[[4]] <- unlist(nnls_r2)
                            blist[[5]] <- SceMat
                            blist[[6]] <- nfitted
                            blist[[7]] <- nSce
                            names(blist) <- c("bmat", "obsY", "fittedY", "nnls_r2", "SceMat", "nfitted", "nSce")
                            fleet$resNNLS[[specie]] <<- blist
                          },
                          cohoDisPlot = function(whoSpe, whoCoh, whiYea, interp){
                            if(interp == FALSE){
                              if(whoCoh == "All"){
                                if(whiYea == "All"){
                                  # 1+round(apply(bySpecie[[whoSpe]]$Coh_A[,,,],1,sum)/max(apply(bySpecie[[whoSpe]]$Coh_A[,,,],1,sum)), 2)*100
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A[,,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - All cohorts - All years", sep = ""), legendUnits = "N.")
                                }else{
                                  # 1+round(apply(bySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)/max(apply(bySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum)), 2)*100
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A[,,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }else{
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A[,whoCoh,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A[,whoCoh,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }
                            }else{
                              if(whoCoh == "All"){
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A_Int[,,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - All cohorts - All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A_Int[,,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - All cohorts - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }else{
                                if(whiYea == "All"){
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A_Int[,whoCoh,,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - Cohort: ", whoCoh, "- All years", sep = ""),
                                                legendUnits = "N.")
                                }else{
                                  yea_abb <- round(apply(bySpecie[[whoSpe]]$Coh_A_Int[,whoCoh,whiYea,],1,sum))
                                  round_yea <- 1+100*yea_abb/max(yea_abb)

                                  distrPlotCols(cols = rev(topo.colors(101)), vals = round_yea,
                                                maxVal = ceiling(max(yea_abb)),
                                                plotTitle = paste("Specie: ", species[whoSpe], " - Cohort: ", whoCoh, " - Year: ", whiYea, sep = ""),
                                                legendUnits = "N.")
                                }
                              }
                            }
                          },
                          calcLFDPop = function(ind_num){
                            bySpecie[[ind_num]]$LFDPop <<- array(dim=c(sampMap$nCells, length(bySpecie[[ind_num]]$lengClas),length(bySpecie[[ind_num]]$years),2))
                            for(y in 1:length(bySpecie[[ind_num]]$years)){
                              subLFD <- bySpecie[[ind_num]]$rawData[which(bySpecie[[ind_num]]$rawData$Year==bySpecie[[ind_num]]$years[y]),]
                              poinOver <- as.numeric(sp::over(SpatialPoints(subLFD[,c("LON","LAT")]), SpatialPolygons(sampMap$gridShp@polygons)))
                              subLFD <- cbind(subLFD[,c("LCLASS", "FEMALE", "MALE")], poinOver)
                              colnames(subLFD) <- c("LCLASS", "FEMALE", "MALE", "Cell")
                              for(IDcell in 1:sampMap$nCells){
                                if(length(which(subLFD[,"Cell"] == IDcell))>0){
                                  cell.data <- subLFD[which(subLFD[,"Cell"] == IDcell),]
                                  cell.LFD <- RecLFD(cell.data,
                                                     bySpecie[[ind_num]]$lengClas,
                                                     length(unique(cell.data[,1])))
                                  bySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- cell.LFD[1,]
                                  bySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- cell.LFD[2,]
                                }else{
                                  bySpecie[[ind_num]]$LFDPop[IDcell,,y,1] <<- rep(0,length(bySpecie[[ind_num]]$lengClas))
                                  bySpecie[[ind_num]]$LFDPop[IDcell,,y,2] <<- rep(0,length(bySpecie[[ind_num]]$lengClas))
                                }}}},
                          setCoh_A = function(){
                            if(length(species) == 1){
                              calcCoh_A(1)
                            }else{
                              for(i in 1:length(species)){
                                calcCoh_A(i)
                              }}
                          },
                          calcCoh_A = function(ind_num){

                            Pop <- bySpecie[[ind_num]]$LFDPop
                            LC <- bySpecie[[ind_num]]$lengClas[-length(bySpecie[[ind_num]]$lengClas)]
                            sp <- bySpecie[[ind_num]]$specie
                            nc <- bySpecie[[ind_num]]$nCoho
                            bySpecie[[ind_num]]$Coh_A <<- array(dim=c(sampMap$nCells, nc, length(bySpecie[[ind_num]]$years),2))
                            for(y in 1:length(bySpecie[[ind_num]]$years)){
                              for(sex in c(1:2)){
                                mms <- bySpecie[[ind_num]]$mixPar[[sex]][[1]][y,]
                                sds <- bySpecie[[ind_num]]$mixPar[[sex]][[2]][y,]
                                opt <- matrix(0,length(LC),nc)
                                for(ij in 1:sampMap$nCells){
                                  vv <- Pop[ij, , y, sex]
                                  coh.abb <- numeric(nc)
                                  if(sum(vv)>0){
                                    for(coh in c(1:nc)) opt[,coh] <- dnorm(LC,mms[coh],sds[coh])
                                    opt.ass <- apply(opt,1,which.max)
                                    for(coh in c(1:nc)) coh.abb[coh] <- sum(vv[which(opt.ass==coh)])
                                  }
                                  bySpecie[[ind_num]]$Coh_A[ij,1:nc,y,sex] <- as.numeric(coh.abb)
                                }
                              }
                            }
                          },
                          calcCoh_A_Int = function(ind_num){
                            bySpecie[[ind_num]]$Coh_A_Int <<- array(dim=c(sampMap$nCells, bySpecie[[ind_num]]$nCoho,length(bySpecie[[ind_num]]$years),2))
                            for(y in 1:length(bySpecie[[ind_num]]$years)){
                              for(sex in 1:2){
                                for(coh in 1:bySpecie[[ind_num]]$nCoho){
                                  xdata <- cbind(sampMap$griCent, bySpecie[[ind_num]]$Coh_A[,coh,y,sex])
                                  colnames(xdata) <- c("LON","LAT","Coh")
                                  xdata <- as.data.frame(xdata)
                                  yea_poi <- bySpecie[[ind_num]]$rawData[which(bySpecie[[ind_num]]$rawData$Year == bySpecie[[ind_num]]$years[y]),c("LON", "LAT")]
                                  cMEDITS <- which(!is.na(over(sampMap$gridShp, SpatialPoints(unique(yea_poi)))))
                                  noMEDITS <- setdiff(c(1:sampMap$nCells),cMEDITS)
                                  Areacell <- 9.091279*11.112
                                  RateArea <- Areacell/100
                                  bySpecie[[ind_num]]$Coh_A_Int[,coh,y,sex] <- IntInvDis(RateArea*xdata, cMEDITS, noMEDITS,
                                                                                         Refmax=5, Refmin=3,
                                                                                         sampMap$nCells,
                                                                                         sampMap$gridShp, graph=T, logplot=F)[,3]
                                }
                              }
                            }
                          }
                        ))



#############################################################
#' BySpeLFD
#'
#' The \code{BySpeLFD} class implements the class of SMART to
#'  handle species samplings.
#'
#' @return This function returns the 'sampleLFDbyspe' object.
#'
#############################################################

BySpeLFD <- R6Class("sampleLFDbyspe",
                    portable = FALSE,
                    class = TRUE,
                    public = list(
                      specie = NULL,
                      years = NULL,
                      rawData = NULL,
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
                      setRawData = function(raw_data){rawData <<- raw_data},
                      plotLFD = function(){
                        plotSpeAllYea(rawData)
                      },
                      initialize = function(sing_spe){
                        setRawData(sing_spe)
                        setYears()
                        setSpecie()
                        setLClass()
                      },
                      setYears = function(){years <<- sort(unique(rawData[,"Year"]), decreasing = FALSE)},
                      setSpecie = function(){specie <<- unique(rawData[,"SPECIE"])},
                      setLClass = function(){lengClas <<- seq(from = min(rawData[,"LCLASS"]), to = max(rawData[,"LCLASS"]), by = 1) },
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
                                            num_ye = years, num_coh = nCoho, MixtureP = mixPar)

                        #Check
                        cat(specie,"Biomassa", as.character(years[length(years)]), " =",
                            sum(LFDtoBcell(LCspe = LCspe, abbF = TempArray[,,length(years),1], abbM = TempArray[,,length(years),2],
                                           LWpar = LWpar))/1000000 ,"\n")

                        popGen <<- TempArray

                      },
                      calcMix = function(nAdap = 100, nSamp = 2000){

                        mixPar <<- list('Female' = list('Means' = matrix(NA, length(years), nCoho), 'Sigmas' = matrix(NA, length(years), nCoho)),
                                        'Male' = list('Means' = matrix(NA, length(years), nCoho), 'Sigmas' = matrix(NA, length(years), nCoho)))

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
                          for(yea in 1:length(years)){
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



#############################################################
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
                       rawEffort = NULL,
                       weekEffoMatr = NULL,
                       dayEffoMatr = NULL,
                       prodMatr = NULL,
                       effoProd = NULL,
                       effoProdMont = NULL,
                       effoProdAll = NULL,
                       trackHarbs = NULL,
                       rawSelectivity = NULL,
                       rawProduction = NULL,
                       registerIds = NULL,
                       productionIds = NULL,
                       prodIdsLoa = NULL,
                       prodSpec = NULL,
                       specSett = NULL,
                       specLogit = NULL,
                       effortIds = NULL,
                       idsEffoProd = NULL,
                       effoProdAllLoa = NULL,
                       resNNLS = NULL,
                       fishPoinPara = NULL,
                       loadFleetRegis = function(register_path){
                         cat("Loading raw Fleet Register data...\n", sep = "")
                         rawRegister <<- readRegisterEU(register_path)
                       },
                       loadMatEffort = function(effort_path){
                         cat("Loading Effort data...\n", sep = "")
                         rawEffort <<- readRDS(effort_path)
                       },
                       loadProduction = function(production_path){
                         cat("Loading Production data... ", sep = "")
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
                           scale_fill_manual(values=c("gainsboro", "grey50")) + theme_linedraw()

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
                         cat("\nSetting Effort IDs year ", sep = "")
                         effortIds <<- list()
                         for(i in names(rawEffort)){
                           cat(i, "... ", sep = "")
                           tmp_ids <- unique(rawEffort[[i]][,1])
                           tmp_key <- i
                           effortIds[[tmp_key]] <<- tmp_ids
                         }
                         effortIds[["All"]] <<- unique(unlist(effortIds))
                         cat("Done!", sep = "")
                       },
                       setProdSpec = function(){
                         prodSpec <<- list()
                         cat("\nSetting Species year ", sep = "")
                         for(i in names(effoProdMont)){
                           cat(i, "... ", sep = "")
                           prodSpec[[i]] <<- colnames(effoProdMont[[i]])[ncol(dayEffoMatr[[i]]):ncol(effoProdMont[[i]])]
                           if(i == names(prodSpec)[1]){
                             prodSpec[["Cross"]] <<- prodSpec[[i]]
                           }else{
                             prodSpec[["Cross"]] <<- intersect(prodSpec[["Cross"]], prodSpec[[i]])
                           }
                         }
                         cat("Done!", sep = "")
                       },
                       setSpecSett = function(){
                         specSett <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
                         names(specSett) <<- sort(prodSpec[["Cross"]])
                       },
                       setNNLS = function(){
                         resNNLS <<- vector(mode = "list", length = length(prodSpec[["Cross"]]))
                         names(resNNLS) <<- sort(prodSpec[["Cross"]])
                       },
                       plotNNLS = function(specie, thresR2){
                         tmp_df <- data.frame(R2 = "R2",
                                              Values = as.numeric(resNNLS[[specie]][["nnls_r2"]]))
                         bp <- ggplot(tmp_df, aes(x = R2, y = Values)) +
                           geom_violin(fill = "grey30", colour = "grey90", alpha = 0.05) +
                           geom_boxplot(fill = "grey90", width = 0.5) +
                           stat_boxplot(geom ='errorbar', width = 0.25) +
                           theme(axis.text.x = element_blank()) +
                           ylim(0, 1) +
                           labs(title = "R2 values") +
                           geom_hline(aes(yintercept = thresR2), linetype="dashed", size = 0.5, colour = "red")

                         tmp_reg <- data.frame(Observed = resNNLS[[specie]]$obsY,
                                               Fitted = resNNLS[[specie]]$fittedY)

                         reg_p <- ggplot(tmp_reg, aes(y = Fitted, x = Observed)) +
                           geom_point(alpha = 0.25, size = 0.2) + stat_smooth(method = "lm") +
                           labs(title = "Observed VS Fitted") +
                           scale_x_log10() +
                           scale_y_log10() +
                           annotation_logticks()

                         grid.arrange(reg_p, bp, layout_matrix = rbind(c(1,1,2),c(1,1,2)))
                       },
                       setSpecSettItm = function(specie, thresh, brea, max_xlim){
                         specSett[[specie]] <<- data.frame(threshold = thresh,
                                                           breaks = brea,
                                                           max_x = max_xlim)
                       },
                       setSpecLogitPredict = function(specie){
                         specLogit[[specie]]$predict <<- predict(specLogit[[specie]]$logit$logit_f, type = "response")
                       },
                       setSpecLogitROCR = function(specie){
                         ROCRpred <- prediction(specLogit[[specie]]$predict, 1*(specLogit[[specie]]$landings > specSett[[specie]]$threshold))
                         specLogit[[specie]]$ROCRperf <<- performance(ROCRpred, "tpr", "fpr")
                       },
                       setSpecLogitOptCut = function(specie){
                         analysis <- roc(response = specLogit[[specie]]$landings, predictor = specLogit[[specie]]$predict)
                         tuning <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
                         specLogit[[specie]]$optCut <<- tuning[which.max(tuning[,2]),1]
                       },
                       plotLogitROC = function(specie){
                         base_seq <- seq(specLogit[[specie]]$optCut-0.1,specLogit[[specie]]$optCut+0.1,0.05)
                         base_seq <- base_seq[-which(base_seq == specLogit[[specie]]$optCut)]
                         plot(specLogit[[specie]]$ROCRperf, main = "ROC curve - Tpr/Fpr",
                              print.cutoffs.at = c(base_seq ,specLogit[[specie]]$optCut),
                              text.cex = c(rep(0.85, length(base_seq)), 1.5),
                              text.col = c(rep("grey70", length(base_seq)), "black"),
                              text.adj = c(-0.2, 1.7))
                       },
                       setSpecLogitConf = function(specie, cutoff = specLogit[[specie]]$optCut){
                         predict <- factor(as.numeric(specLogit[[specie]]$predict > cutoff))
                         truth <- factor(1*(specLogit[[specie]]$landings > specSett[[specie]]$threshold))
                         tmp_Tbl <- table(predict, truth)
                         specLogit[[specie]]$confMatrix <<- confusionMatrix(tmp_Tbl)
                       },
                       setSpecLogit = function(specie){
                         if(is.null(specLogit)) specLogit <<- list()
                         if(is.null(specLogit[[specie]])) specLogit[[specie]] <<- list()
                         tmp_mat <- getMatSpeLand(specie)
                         specLogit[[specie]]$landings <<- tmp_mat[,ncol(tmp_mat)]
                         specLogit[[specie]]$logit <<- getLogit(Lit = specLogit[[specie]]$landings, X = tmp_mat[,1:(ncol(tmp_mat)-1)],
                                                                thrB = specSett[[specie]]$threshold,
                                                                ptrain = 80, ptest = 20)
                         setSpecLogitPredict(specie)
                         setSpecLogitROCR(specie)
                         setSpecLogitOptCut(specie)
                         # plotLogitROC(specie)
                         setSpecLogitConf(specie)
                       },
                       getMatSpeLand = function(specie){
                         tmp_mat <- effoProdAll[,c(1,3:(ncol(dayEffoMatr[[1]])),which(colnames(effoProdAll) == specie))]
                         tmp_mat$MonthNum <- as.factor(tmp_mat$MonthNum)
                         return(tmp_mat)
                       },
                       setEffoProdAll = function(){
                         cat("\nSetting effort/production year ", sep = "")
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
                         cat("Done!", sep = "")
                       },
                       setEffoProdAllLoa = function(){
                         tmp_effoProd <- effoProdAll
                         tmp_loa <- rawRegister[,c("CFR","Loa")]
                         tmp_loa$CFR <- substr(tmp_loa$CFR, 4, nchar(tmp_loa$CFR[1]))
                         names(tmp_loa) <- c("I_NCEE", "Loa")
                         effoProdAllLoa <<- sqldf("select * from tmp_effoProd left join (select * from tmp_loa) using (I_NCEE)")
                       },
                       setProdIds = function(){
                         cat("\nSetting Production IDs year ", sep = "")
                         productionIds <<- list()
                         for(i in names(rawProduction)){
                           cat(i, "... ", sep = "")
                           tmp_ids <- unique(rawProduction[[i]][,1])
                           tmp_key <- i
                           productionIds[[tmp_key]] <<- tmp_ids
                         }
                         productionIds[["All"]] <<- unique(unlist(productionIds))
                         cat("Done!", sep = "")
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
                           ylab("N. of IDs")
                         print(tmp_plot)
                       },
                       plotCountIDsEffo = function(){
                         tmp_df <- data.frame("Year" = names(effortIds),
                                              "Ids" = unlist(lapply(unique(effortIds), length)))
                         # names(tmp_df) <- c("Year", "Ids")
                         tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
                           geom_text(aes(y=Ids, label = Ids), position= position_dodge(width=1),
                                     vjust=2.5, color="white") +
                           ggtitle("Count of Distinct Vessels - Effort Dataset") +
                           ylab("N. of IDs")
                         print(tmp_plot)
                       },
                       plotCountIDsProd = function(){
                         tmp_df <- data.frame("Year" = names(productionIds),
                                              "Ids" = unlist(lapply(unique(productionIds), length)))
                         # names(tmp_df) <- c("Year", "Ids")
                         tmp_plot <- ggplot(tmp_df, aes(x = Year, y = Ids)) + geom_bar(stat = "identity") +
                           geom_text(aes(y=Ids, label = Ids), position= position_dodge(width=1),
                                     vjust=2.5, color="white") +
                           ggtitle("Count of Distinct Vessels - Production Dataset") +
                           ylab("N. of IDs")
                         print(tmp_plot)
                       },
                       setEffoProdMatr = function(){
                         effoProd <<- list()
                         for(i in names(rawEffort)){
                           tmp_effo <- dayEffoMatr[[i]]
                           tmp_prod <- prodMatr[[i]]
                           effoProd[[i]] <<- sqldf("select * from tmp_effo, tmp_prod where I_NCEE = NUMUE and DATE >= UTC_S and DATE <= UTC_E")
                         }
                       },
                       setEffoProdMont = function(){
                         effoProdMont <<- list()
                         cat("\nGenerating year ", sep = "")
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
                         cat("Done!")
                       },
                       setProdMatr = function(){
                         prodMatr <<- list()
                         for(i in names(rawEffort)){
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
                         cat("\nCreating weekly matrix for year ", sep = "")
                         for(j in names(rawEffort)){
                           cat(j, "... ", sep = "")
                           tmp_dat <- rawEffort[[j]][rawEffort[[j]]$FishPoint == TRUE & rawEffort[[j]]$P_INT == 1 & !is.na(rawEffort[[j]]$Cell),c("I_NCEE","DATE", "MonthNum", "FishGround", "FishPoint")]
                           tmp_dat$DATE <- ceiling(tmp_dat$DATE)
                           tmp_matrix <- dcast(tmp_dat, formula = I_NCEE + DATE + MonthNum ~ FishGround,
                                               fun.aggregate = sum, na.rm=TRUE, value.var = "FishPoint")
                           ## points to hours: interpolation interval 10 min
                           tmp_matrix[,4:ncol(tmp_matrix)] <- tmp_matrix[,4:ncol(tmp_matrix)]/6
                           miss_cols <- setdiff(as.character(unique(rawEffort[[j]]$FishGround[!is.na(rawEffort[[j]]$FishGround)])),
                                                names(tmp_matrix)[4:ncol(tmp_matrix)])
                           if(length(miss_cols) > 0){
                             tmp_matrix[,miss_cols] <- 0
                             tmp_matrix <- tmp_matrix[,c(1:4, 4+order(as.numeric(names(tmp_matrix)[5:ncol(tmp_matrix)])))]
                           }
                           dayEffoMatr[[j]] <<- tmp_matrix
                         }
                         cat("Done!", sep = "")
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
                         cat("Checking EU Fleet Register format...\n", sep = "")
                         two_rows <- readLines(con = reg_path, n = 2)
                         last_char <- substr(two_rows[2], nchar(two_rows[2]), nchar(two_rows[2]))
                         raw_fleet <- readLines(con = reg_path, n = -1)
                         if(last_char == ";"){
                           cat("Trailing character found! Cleaning...\n", sep = "")
                           tmp_flee <- paste(unlist(lapply(strsplit(raw_fleet, split = ";"), paste, collapse = ";")), collapse = "\n")
                         }else{
                           tmp_flee <- paste(raw_fleet, collapse = "\n")
                         }
                         if(substr(reg_path, nchar(reg_path)-12, nchar(reg_path)) != "_smart-ed.csv"){
                           tmp_flee <- gsub("\\;",",", tmp_flee)
                           new_path <- paste(substr(reg_path, 1, nchar(reg_path)-4), "_smart-ed",
                                             substr(reg_path, nchar(reg_path)-3, nchar(reg_path)), sep = "")
                           cat("\nWriting edited Fleet register in:\n", new_path, "\n\n", sep = "")
                           write(tmp_flee, file = new_path)
                           reg_path <- new_path
                         }
                         cat("Loading file... ", sep = "")
                         re_fleet <- read.csv(reg_path, stringsAsFactors = FALSE)
                         cat("OK\n", sep = "")
                         return(re_fleet)
                       },
                       cleanRegister = function(){
                         cat("Ordering Fleet Register by CFR... ", sep = "")
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



#############################################################
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
                       cutResult = NULL,
                       cutResEffo = NULL,
                       cutResShp = NULL,
                       cutResShpCent = NULL,
                       cutResShpFort = NULL,
                       ggDepthFGbox = NULL,
                       ggEffoFGbox = NULL,
                       ggEffoFGmap = NULL,
                       ggBioFGmat = NULL,
                       ggCutFGmap = NULL,
                       ggIchFGlin = NULL,
                       ggSilFGlin = NULL,
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
                       loadHarbDbf = function(dbf_path){
                         tmp_dbf <- read.dbf(file = dbf_path)
                         colnames(tmp_dbf) <- c("XCOORD", "YCOORD", "Name")
                         harbDbf <<- tmp_dbf
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
                         gooMapPlot <<- ggmap(gooMap)
                       },
                       setPlotRange = function(){
                         plotRange <<- data.frame(xmin=gridBboxExt[1],
                                                  xmax=gridBboxExt[3],
                                                  ymin=gridBboxExt[2],
                                                  ymax=gridBboxExt[4])
                       },
                       setGooGrid = function(){
                         #                          gooGrid <<- gooMapPlot + geom_polygon(aes(x = X, y = Y, group = PID),
                         #                                                                fill = 'grey', size = 0.2,
                         #                                                                color = 'gainsboro', data = gridPolySet, alpha = 0.5) +
                         #                            coord_fixed(xlim = extendrange(plotRange[1:2]),
                         #                                        ylim = extendrange(plotRange[3:4]), expand = TRUE)
                         gooGrid <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group),
                                                                                fill = 'grey', size = 0.2,
                                                                                color = 'gainsboro', data = gridFortify, alpha = 0.5) +
                                                        lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])))
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
                         sampColScale <<- scale_colour_manual(name = "SPECIE",values = myColors)
                       },
                       plotGooGridPoi = function(poi_data){
                         plotGooGrid() + geom_jitter(data = poi_data,
                                                     aes(x = LON, y = LAT, shape = SPECIE, color = SPECIE),
                                                     width = 0.05, height = 0.05, alpha = 0.95) + sampColScale
                       },
                       setGooBbox = function(){
                         text_x <- mean(gridBboxExt[c(1,3)])
                         text_y <- mean(gridBboxExt[c(2,4)])
                         gooBbox <<- gooGrid + geom_rect(data=plotRange, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                                         color="firebrick",
                                                         fill = alpha('red', 0.2),
                                                         inherit.aes = FALSE) +
                           annotate("label", x = text_x, y = text_y,
                                    label="Bounding\nBox", family="serif", fontface="italic",
                                    colour="firebrick", size=6, fill = "grey80")
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
                       ggplotBioDF = function(){
                         def.par <- par(no.readonly = TRUE)
                         cell_bed <- apply(bioDF, 1, function(x) which(x == 1))
                         tmp_dat <- colnames(bioDF)[cell_bed]
                         color_clas <- rainbow(max(cell_bed))
                         names(tmp_dat) <- 1:length(tmp_dat)
                         all_cell <- merge(x = gridPolySet$PID,
                                           data.frame(x = as.numeric(names(tmp_dat)), y = tmp_dat), all = TRUE)
                         all_cell[is.na(all_cell)] <- 0
                         grid_data <- cbind(gridPolySet, Seabed = all_cell[,2])
                         tmp_plot <- suppressMessages(gooMapPlot +
                                                        geom_polygon(aes(x = X, y = Y, group = PID, fill = Seabed), size = 0.2,
                                                                     data = grid_data, alpha = 0.8) +
                                                        lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                        xlab("Longitude") + ylab("Latitude") +
                                                        ggtitle("Seabed"))
                         suppressWarnings(print(tmp_plot))
                         par(def.par)
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
                       },
                       saveGridBath = function(bathy_path){
                         saveRDS(object = gridBathy, file = bathy_path)
                       },
                       loadGridBath = function(bathy_path){
                         gridBathy <<- readRDS(bathy_path)
                         getCentDept()
                       },
                       getCentDept = function(){
                         centDept <<- get.depth(gridBathy, x = griCent[,1], y = griCent[,2], locator = FALSE)
                       },
                       plotGridBathy = function(){
                         def.par <- par(no.readonly = TRUE)
                         f_bathy <- fortify.bathy(gridBathy)
                         f_bathy$z[f_bathy$z > 0] <- 0
                         colnames(f_bathy) <- c("lon", "lat", "Depth")
                         the_plot <- suppressMessages(gooMapPlot +
                                                        stat_contour(data = f_bathy, aes(x = lon, y = lat, z = Depth, colour = ..level..), size = 0.2, binwidth = 250) +
                                                        lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                        xlab("Longitude") + ylab("Latitude") +
                                                        ggtitle("Bathymetry"))
                         suppressWarnings(print(the_plot))
                         par(def.par)
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
                       setClusInpu = function(clus_data){
                         clusInpu <<- as.matrix(clus_data)
                       },
                       calcFishGrou = function(numCuts = 50,
                                               minsize = 10,
                                               modeska = "S",
                                               skater_method){
                         set.seed(123)
                         #Build the neighboorhod list
                         Grid.bh <- gridShp[1]
                         ##  Construct neighbours list from polygon list
                         bh.nb <- poly2nb(Grid.bh, queen = TRUE)
                         bh.mat <- cbind(rep(1:length(bh.nb), lapply(bh.nb,length)), unlist(bh.nb))
                         #Compute the basic objects for clustering
                         ##  Cost of each edge as the distance between nodes
                         lcosts <- nbcosts(bh.nb, clusInpu)
                         ##  Spatial weights for neighbours lists
                         nb.w <- nb2listw(bh.nb, lcosts, style = modeska)
                         ##  Find the minimal spanning tree
                         mst.bh <- mstree(nb.w, ini = 1)
                         clusMat <<- matrix(NA, nCells, numCuts)
                         cat("Clustering", sep = "")
                         res1 <- skater(mst.bh[,1:2], clusInpu, ncuts = 1, minsize,
                                        method = skater_method)
                         clusMat[,1] <<- res1$groups

                         #Perform the first CC (without removing spurious clusters)
                         for(nCuts in 2:numCuts){
                           cat(".", sep = "")
                           ##  Spatial 'K'luster Analysis by Tree Edge Removal
                           #                            res1 <- skater(mst.bh[,1:2], cells_data, ncuts = nCuts, minsize,
                           #                                           method = skater_method)
                           res1 <- skater(res1, clusInpu, ncuts = 1, minsize,
                                          method = skater_method)
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
                           indCH[i] <<-  get_CH(clusInpu,clusMat[,i])
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
                       setCutResult = function(ind_clu){
                         tmpCut <<- ind_clu
                         cutResult <<- data.frame(clusInpu, FG = as.factor(clusMat[,ind_clu]))
                         cutResEffo <<- data.frame(Effort = apply(cutResult[, grep("Year", colnames(cutResult))],1, sum),
                                                   Cluster = cutResult[,ncol(cutResult)])
                         cutResShp <<- unionSpatialPolygons(gridShp, IDs = clusMat[,ind_clu])
                         cutResShpCent <<- as.data.frame(coordinates(cutResShp))
                         cutResShpCent$id <<- rownames(cutResShpCent)
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
                                                             geom_boxplot() +
                                                             coord_flip() +
                                                             theme(legend.position='none'))
                       },
                       setEffoFGbox = function(){
                         ggEffoFGbox <<- suppressMessages(ggplot(cutResEffo, aes(x = Cluster, y = Effort, group = Cluster)) +
                                                            geom_boxplot() +
                                                            coord_flip() +
                                                            theme(legend.position='none'))
                       },
                       setEffoFGmap = function(){
                         agg_eff <- aggregate(formula = Effort ~ Cluster, data = cutResEffo, FUN = sum)
                         all_cell <- merge(x = cutResShpFort$id,
                                           data.frame(x = agg_eff$Cluster, y = agg_eff$Effort), all = TRUE)
                         all_cell[is.na(all_cell)] <- 0
                         grid_data <- cbind(cutResShpFort, Hours = all_cell[,2])
                         ggEffoFGmap <<- suppressMessages(gooMapPlot + geom_polygon(aes(x = long, y = lat, group = group, fill = Hours),
                                                                                    colour = "black", size = 0.1,
                                                                                    data = grid_data, alpha = 0.8) +
                                                            scale_fill_gradient(low = "Yellow", high = "coral") +
                                                            geom_text(aes(label = FG, x = Lon, y = Lat),
                                                                      data = cutResShpCent, size = 2) +
                                                            lims(x = extendrange(plotRange[1:2]), y = extendrange(plotRange[3:4])) +
                                                            theme(legend.position='none'))
                       },
                       setBioFGmat = function(){
                         tmp_bio <- data.frame(FG = cutResult$FG, cutResult[,which(make.names(colnames(bioDF)) %in% colnames(cutResult))])
                         bio2plot <- melt(tmp_bio, id.vars="FG", variable.name = "Substrate")
                         bio2plot <- bio2plot[bio2plot$value == 1,1:2]
                         ggBioFGmat <<- suppressMessages(ggplot(bio2plot, aes(x = FG, y = Substrate, fill = Substrate)) +
                                                           geom_tile() +
                                                           coord_flip() +
                                                           annotate("text", colour = "grey30", y = 1:length(levels(bio2plot$Substrate)),
                                                                    x = rep(4, length(levels(bio2plot$Substrate))),
                                                                    label = levels(bio2plot$Substrate),
                                                                    angle = rep(90, length(levels(bio2plot$Substrate)))) +
                                                           theme(legend.position = 'none', axis.text.x = element_text(colour = "white")))
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

