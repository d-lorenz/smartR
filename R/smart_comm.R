
###### TO DO
#
#  -- Add weight multiplication in fishing ground clustering
#  -- Add effort extraction from vmsbase


#
# r_pkgs <- c("R6", "maptools", "maps", "mapdata", "PBSmapping",
#             "plyr", "rjags", "lattice", "gWidgets2", "gWidgets2RGtk2")
# cbind(r_pkgs, unlist(lapply(r_pkgs, require, character.only=T)))

# my_sampling <- SmartProject$new()

# gh# my_sampling$sampMap$getGridBath()
# saveRDS(my_sampling$sampMap$gridBathy, "/Users/Lomo/Documents/Uni/R/smart/data/gsa16_bathy.rData")
# my_sampling$sampMap$gridBathy <- readRDS("/Users/Lomo/Documents/Uni/R/smart/data/gsa16_bathy.rData")
# my_sampling$sampMap$getCentDept()
# my_sampling$sampMap$plotGridBathy()

# my_sampling$loadRawLFD(csv_path = "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/SampleData_ed.csv")
# my_sampling$loadMap("/Users/Lomo/Documents/Uni/Lab/Proj/SMART_All/SMART1.0/Geo/Grid/GFCM_Grid_6min_GSA16.shp")
# my_sampling$sampMap$plotSamMap()
# my_sampling$setLFDPop()
# my_sampling$bySpecie[[1]]$setNCoho(3)
# my_sampling$bySpecie[[1]]$mixPar <- readRDS("/Users/Lomo/Documents/Uni/PhD/scripts/DPS_mixpar.rData")
# my_sampling$calcCoh_A(1)
# my_sampling$calcCoh_A_Int(1)
#

# my_sampling <- readRDS("/Users/Lomo/Documents/Uni/R/smart/data/smart_projects/test0.rData")
# my_sampling$sampMap$loadBioDF("/Users/Lomo/Documents/Uni/R/smart/data/BioM.rData")
# my_sampling$fleet$loadMatEffort("/Users/Lomo/Documents/Uni/R/smart/data/EFF_OTB_Hours_9years.rData")
# my_sampling$createFleet()



# my_sampling <- SmartProject$new()
#
# my_sampling$loadMap("/Users/Lomo/Documents/Uni/Lab/Proj/SMART_All/SMART1.0/Geo/Grid/GFCM_Grid_6min_GSA16.shp")
# my_sampling$loadRawLFD(csv_path = "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/SampleData_ed.csv")
# my_sampling$setLFDPop()
# my_sampling$bySpecie[[1]]$setNCoho(3)
# my_sampling$bySpecie[[1]]$mixPar <- readRDS("/Users/Lomo/Documents/Uni/PhD/scripts/DPS_mixpar.rData")
# my_sampling$calcCoh_A(1)
# my_sampling$calcCoh_A_Int(1)
# clus_data <- cbind(apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,1,,1], 1, sum),
# apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,2,,1], 1, sum),
# apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,3,,1], 1, sum),
# apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,1,,2], 1, sum),
# apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,2,,2], 1, sum),
# apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,3,,2], 1, sum))
# my_sampling$sampMap$setClusInpu(clus_data)
# my_sampling$sampMap$calcFishGrou(numCuts = 3, #max 50
# minsize = 10,
# modeska = "S",
# skater_method = "euclidean")
# my_sampling$sampMap$clusInpu


# clus_data <- cbind(apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,1,,1], 1, sum),
#                    apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,2,,1], 1, sum),
#                    apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,3,,1], 1, sum),
#                    apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,1,,2], 1, sum),
#                    apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,2,,2], 1, sum),
#                    apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,3,,2], 1, sum))
#

# clus_data <- cbind(clus_data,
#                    my_sampling$sampMap$bioDF)
#
# clus_data <- cbind(clus_data,
#                    my_sampling$fleet$rawEffort)
#
# clus_data <- cbind(clus_data,
#                    my_sampling$sampMap$centDept[,3])

# my_sampling$sampMap$setClusInpu(clus_data)


# my_sampling$sampMap$calcFishGrou(numCuts = 15,
#                                             minsize = 10,
#                                             modeska = "S",
#                                             "euclidean")
#
# my_sampling$sampMap$plotFishGrou(5)


#
# source("Documents/Uni/R/smart/smart_funct.R")
#
# source("Documents/Uni/R/smart/smart_class.R")
#
# source("Documents/Uni/R/smart/smart_gui.R")
#
#
#
#
# ########
# # Create new SampleLFD object
# my_sampling <- SampleLFD$new()
#
# ########
# # Load raw LFD data
# my_sampling$loadRawLFD(csv_path = "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/SampleData_ed.csv")
#
# ########
# # Load sampling grid
# my_sampling$loadMap("/Users/Lomo/Documents/Uni/Lab/Proj/SMART_All/SMART1.0/Geo/Grid/GFCM_Grid_6min_GSA16.shp")
#
# ########
# # Compute LFDPop
# my_sampling$setLFDPop()
#
# #PLOT
# my_sampling$sampMap$plotSamMap()
# plot(my_sampling$sampMap$gridShp, add = TRUE, lty = 0,
#      col = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$LFDPop[,,,], 1, sum)/max(apply(my_sampling$bySpecie[[1]]$LFDPop[,,,], 1, sum)), 2)*100])
#
# ########
# # Set number of cohorts
# my_sampling$bySpecie[[1]]$setNCoho(3)
#
# ########
# # Set priors
# my_sampling$bySpecie[[1]]$setPrior(c(35, 2.5), c(0.67, 0.2), c(-0.208, 0.01), c(30, 2.5), c(0.73, 0.2), c(-0.13, 0.01))
# my_raw_dat$bySpecie[[1]]$setPrior(c(35, 2.5), c(0.67, 0.2), c(-0.208, 0.01), c(30, 2.5), c(0.73, 0.2), c(-0.13, 0.01))
# # tmp_prior <- my_raw_dat$bySpecie[[1]]$prior
# # tmp_prior <- readRDS("/Users/Lomo/Documents/Uni/PhD/scripts/DPS_prio.rData")
# # my_sampling$bySpecie[[1]]$prior <- tmp_prior
# # saveRDS(tmp_prior, "/Users/Lomo/Documents/Uni/PhD/scripts/DPS_prio.rData")
#
# ########
# # Compute mixture
# my_sampling$bySpecie[[1]]$calcMix(nAdap = 100, nSamp = 200)
# # tmp_mixpar <- my_raw_dat$bySpecie[[1]]$mixPar
# # tmp_mixpar <- readRDS("/Users/Lomo/Documents/Uni/PhD/scripts/DPS_mixpar.rData")
# # my_sampling$bySpecie[[1]]$mixPar <- tmp_mixpar
# # saveRDS(tmp_mixpar, "/Users/Lomo/Documents/Uni/PhD/scripts/DPS_mixpar.rData")
#
# ########
# # Compute Coh_A
# my_sampling$calcCoh_A(1)
# #PLOT
# my_sampling$cohoDisPlot(whoSpe =  1, whoCoh = "All", whiYea = "All")
# my_sampling$cohoDisPlot(1, 1, "All", TRUE)
# my_sampling$cohoDisPlot(1, 2, 5)
# my_sampling$cohoDisPlot(1, 3, "All")
#
# # my_sampling$sampMap$plotSamMap(celCol = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$Coh_A[,,,],1,sum)/max(apply(my_sampling$bySpecie[[1]]$Coh_A[,,,],1,sum)), 2)*100])
# # plot(my_sampling$sampMap$gridShp, add = TRUE, lty = 0,
# #      col = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$Coh_A[,3,,],1,sum)/max(apply(my_sampling$bySpecie[[1]]$Coh_A[,3,,],1,sum)), 2)*100])
#
# ########
# # Set cell centers
# my_sampling$sampMap$griCent <- FindCenters(my_sampling$sampMap$gridPolySet,5)
# # #PLOT
# # my_sampling$sampMap$plotSamMap()
# # points(my_sampling$sampMap$griCent, pch = 3, cex = 0.8, col = "red")
# # #
#
# ########
# # Compute Coh_A_Int
# my_sampling$calcCoh_A_Int(1)
# #PLOT
# my_sampling$sampMap$plotSamMap()
# plot(my_sampling$sampMap$gridShp, add = TRUE, lty = 0,
#      col = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,,,], 1, sum)/max(apply(my_sampling$bySpecie[[1]]$Coh_A_Int[,,,], 1, sum)), 2)*100])
#
# ########
# # Set LWpar
# my_sampling$bySpecie[[1]]$setLWpar(aF = 0.0029, bF = 2.4818, aM = 0.0034, bM = 2.4096)
#
# ########
# # Set Brefs
# my_sampling$bySpecie[[1]]$setBrefs(4860)
#
# ########
# # Compute Qmedi
# my_sampling$bySpecie[[1]]$caliQmedi()
#
# ########
# # Compute Medmo
# my_sampling$bySpecie[[1]]$genMedmo()
# #PLOT
# my_sampling$sampMap$plotSamMap()
# plot(my_sampling$sampMap$gridShp, add = TRUE, lty = 0,
#      col = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$popGen[,,,], 1, sum)/max(apply(my_sampling$bySpecie[[1]]$popGen[,,,], 1, sum)), 2)*100])
#
# my_sampling$sampMap$plotSamMap()
# plot(my_sampling$sampMap$gridShp, add = TRUE, lty = 0,
#      col = rev(heat.colors(100))[1+round(apply(my_sampling$bySpecie[[1]]$popGen[,,,], 1, mean)/max(apply(my_sampling$bySpecie[[1]]$popGen[,,,], 1, mean)), 2)*100])
#
# ########
# # Set SelPar
# my_sampling$bySpecie[[1]]$setSelPar(L50 = 15, L75 = 18)
#
# ########
# # Compute Qcomm
# my_sampling$bySpecie[[1]]$calcQcom()
#
#
#
#
#
