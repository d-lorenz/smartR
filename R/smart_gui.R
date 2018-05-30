
#' SMART GUI
#'
#' The \code{smart_gui} function implements the main graphical user interface of SMART.
#'
#' @return This function does not return a value.
#'
#' @usage smart_gui()
#'
#' @export smart_gui
#'

smart_gui <- function(){

  my_project <<- SmartProject$new()
  # assign("my_project", SmartProject$new(),.GlobalEnv)

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

  # SURVEY DATA
  # pathSurvey <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Survey/survey_data_MEDITS.csv"
  pathSurvey <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Survey/smartSurveyInput_MUT-SoS.csv"

  # FISHERY DATA
  # pathFishery <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_merge_CampBiol.csv"
  pathFishery <- "/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_data_CampBiol_noExpLand.csv"

  # Length/Weight data
  pathLWrel <- "/Users/Lomo/Documents/Uni/R/smart/data/Length_Weight/lwRel_input.csv"

  pathClusMat <- "/Users/Lomo/Documents/Uni/R/smart/data/out/FG_cut/fg_sos/clusMat_BedEffDep.rData"
  rawDataPath <- "/Users/Lomo/Documents/Uni/R/mixture/fisherySampling.rData"

  # LANDINGS DATA
  pathLanding <- "/Users/Lomo/Documents/Uni/R/smart/data/Landings/LandAll.rData"

  # Economic Data
  pathCosts <- "/Users/Lomo/Documents/Uni/Lab/Data/Economics/sos_adri_cleanCosts.csv"

  my_project$createFleet()

  pre_dev <- length(dev.list())

  main_win <- gwindow(paste("SMART - Version ", "1.1", sep = ""),width = 1200, height= 600, visible = TRUE)
  big_g <- ggroup(horizontal = TRUE, container = main_win)

  ####   Left panel   ####

  lef_g <- ggroup(horizontal = FALSE, container = big_g)
  # addSpring(lef_g)
  # gimage("/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/SMART_Logo.2.jpg", container = lef_g)
  gimage(system.file("SMART_Logo.2.jpg", package="smartR"), container = lef_g)
  pro_eg <- gexpandgroup("Project", horizontal = FALSE, container = lef_g)
  gbutton(text = "New", container = pro_eg, handler = function(h,...){
    svalue(uti_gn) <- 1})
  gbutton(text = "Load", container = pro_eg, handler = function(h,...){
    svalue(uti_gn) <- 1})

  env_eg <- gexpandgroup("Environment", horizontal = FALSE, container = lef_g)
  gbutton(text = "Grid", container = env_eg, handler = function(h,...){
    svalue(uti_gn) <- 2})

  eff_eg <- gexpandgroup("Effort", horizontal = FALSE, container = lef_g)
  gbutton(text = "Load VMS Data", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 3})       #    svalue(uti_gn) <- 7})

  gbutton(text = "Fishing Grounds", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 4})       #    svalue(uti_gn) <- 8})

  gbutton(text = "Fleet Register", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 5})       #    svalue(uti_gn) <- 9})

  gbutton(text = "Production", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 6})       #    svalue(uti_gn) <- 10})


  raw_eg <- gexpandgroup("Resources", horizontal = FALSE, container = lef_g)
  gbutton(text = "Survey", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 7})
  gbutton(text = "Fishery", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 8})
  gbutton(text = "Mixture", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 9})
  gbutton(text = "Cohorts", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 10})

  sim_eg <- gexpandgroup("Simulation", horizontal = FALSE, container = lef_g)
  gbutton(text = "Simulate", container = sim_eg, handler = function(h,...){
    svalue(uti_gn) <- 11})
  
  ass_eg <- gexpandgroup("Assessment", horizontal = FALSE, container = lef_g)
  gbutton(text = "Assess", container = ass_eg, handler = function(h,...){
    svalue(uti_gn) <- 12})
  
  addSpring(lef_g)
  stat_bar <- gstatusbar("", container = lef_g, visible = TRUE)

  ####   Right panel   ####

  rig_g <- ggroup(horizontal = FALSE, container = big_g, expand = TRUE)
  uti_gn <- gnotebook(tab.pos = 3, container = rig_g, expand = TRUE)

  ####   Project   ####
  pro_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Project")
  pro_g_top <- gframe(horizontal = TRUE, container = pro_g)
  addSpace(pro_g_top, 2)
  addSpring(pro_g_top)
  gbutton("New", container = pro_g_top)
  addSpring(pro_g_top)
  gbutton("Load", container = pro_g_top, handler = function(h,...){
    load_path <- gfile(text = "Select Smart_Project file", type = "open", filter = list("R files" = list(patterns = c("*.rData"))))
    my_project <- readRDS(load_path)

    ### Update Sampling Status

    if(!is.null(my_project$rawDataSurvey)){ #update_pop_gui()

      raw_t[] <- my_project$rawDataSurvey[sample(1:nrow(my_project$rawDataSurvey), 100, replace = FALSE),]
      svalue(raw_l1) <- paste("Specie: ", paste(my_project$specieInSurvey, collapse = " - "))
      #   svalue(raw_l2) <- paste("Length Classes: from ",  min(my_project$LClass), " to ", max(my_project$LClass))
      svalue(raw_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInSurvey))), " to ", max(as.numeric(as.character(my_project$yearInSurvey))))
      # spec_drop[] <- my_project$specieInSurvey            ##  droplist from population tab
      spec_drop_mix[] <- my_project$specieInSurvey
      # spevie_drop[] <- c("All", my_project$specieInSurvey)
      # cohSpe_drop[] <- my_project$specieInSurvey
      # svalue(spec_drop) <- my_project$specieInSurvey[1]   ##  droplist from population tab
      # svalue(cohSpe_drop) <- my_project$specieInSurvey[1]
      # svalue(spevie_drop) <- "All"
      svalue(spec_drop_mix) <- my_project$specieInSurvey[1]
      year_drop[] <- c("All", as.character(my_project$yearInSurvey))
      cohYea_drop[] <- c("All", as.character(my_project$yearInSurvey))
      svalue(year_drop) <- my_project$yearInSurvey[1]
      svalue(cohYea_drop) <- "All"

      svalue(n_year_s) <- paste(length(my_project$yearInSurvey), " years", sep = "")
      svalue(mi_date_s) <- paste("From: ", min(as.numeric(as.character(my_project$yearInSurvey))), sep = "")
      svalue(ma_date_s) <- paste("To: ", max(as.numeric(as.character(my_project$yearInSurvey))), sep = "")
      svalue(n_spec_s) <- paste(length(my_project$specieInSurvey),
                                ifelse(length(my_project$specieInSurvey) == 1, " specie", " species"), sep = "")
      #       samp_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
      delete(samp_g, samp_g$children[[length(samp_g$children)]])
      add(samp_g, samp_sta_n)
    }

    ### Update grid Status

    if(!is.null(my_project$sampMap)){
      svalue(gri_l1) <- paste("Polyset: Loaded")
      svalue(gri_l2) <- paste("N. Cells: ", my_project$sampMap$nCells)
      svalue(gri_l3) <- paste("GCenter: Loaded")
      svalue(n_cell_g) <- paste("N. Cells: ", my_project$sampMap$nCells)
      delete(grid_g, grid_g$children[[length(grid_g$children)]])
      add(grid_g, grid_sta_n)
    }

    ### Update Effort Status

    if(!is.null(my_project$fleet$rawEffort)){
      effvie_drop[] <- c("All", colnames(my_project$fleet$rawEffort))
      svalue(effvie_drop) <- "All"
      #       effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
      delete(effo_g, effo_g$children[[length(effo_g$children)]])
      add(effo_g, effo_sta_n)
    }

    ### Update Fishing Grounds Status

    if(!is.null(my_project$sampMap$clusMat)){
      fg_plotCut[] <- 1:ncol(my_project$sampMap$clusMat)
      svalue(fg_plotCut) <- which.max(my_project$sampMap$indSil)
      #       figr_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
      delete(figr_g, figr_g$children[[length(figr_g$children)]])
      add(figr_g, figr_sta_n)
    }

    ### Update Register Status

    if(!is.null(my_project$fleet$rawRegister)){
      delete(regi_g, regi_g$children[[length(regi_g$children)]])
      add(regi_g, regi_sta_n)
    }

  })
  addSpring(pro_g_top)
  gbutton("Save", container = pro_g_top, handler = function(h,...){
    save_dest <- gfile(text = "Select file name and destination directory", type = "save",
                       initial.filename = "Smart_Project.rData", initial.dir = my_project$sampMap$gridPath)
    if(rev(unlist(strsplit(save_dest, "[.]")))[1] != "rData"){
      save_dest <- paste(save_dest, ".rData", sep = "")
    }
    saveRDS(my_project, save_dest)
  })
  addSpring(pro_g_top)
  addSpace(pro_g_top, 2)

  pro_g_mid <- gframe(text = "Data", horizontal = TRUE, container = pro_g)
  addSpace(pro_g_mid, 2)

  addSpring(pro_g_mid)
  # gbutton("Grid", container = pro_g_mid)
  grid_g <- gframe(text = "Environment", horizontal = FALSE, container = pro_g_mid)
  n_cell_g <- glabel("   ---", container = grid_g)
  addSpring(grid_g)
  grid_b <- gbutton(text = "Show data", container = grid_g, handler = function(h,..){
    svalue(uti_gn) <- 2
  })
  grid_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  grid_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
  add(grid_g, grid_sta)
  #   enabled(grid_b) <- FALSE

  addSpring(pro_g_mid)
  # gbutton("Sampling", container = pro_g_mid)
  samp_g <- gframe(text = "Survey", horizontal = FALSE, container = pro_g_mid)
  n_year_s <- glabel("   ---", container = samp_g)
  mi_date_s <- glabel("", container = samp_g)
  ma_date_s <- glabel("", container = samp_g)
  n_spec_s <- glabel("   ---", container = samp_g)
  addSpring(samp_g)
  samp_b <- gbutton(text = "Show data", container = samp_g, handler = function(h,..){
    svalue(uti_gn) <- 3
  })
  samp_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  samp_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
  add(samp_g, samp_sta)
  #   enabled(samp_b) <- FALSE

  addSpring(pro_g_mid)
  # gbutton("Effort", container = pro_g_mid)
  effo_g <- gframe(text = "Effort", horizontal = FALSE, container = pro_g_mid)
  # n_cell_g <- glabel("   ---", container = effo_g)
  addSpring(effo_g)
  effo_b <- gbutton(text = "Show data", container = effo_g, handler = function(h,..){
    svalue(uti_gn) <- 7
  })
  effo_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
  add(effo_g, effo_sta)
  #   enabled(effo_b) <- FALSE

  addSpring(pro_g_mid)
  # gbutton("Fishing Ground", container = pro_g_mid)
  figr_g <- gframe(text = "Fishing Ground", horizontal = FALSE, container = pro_g_mid)
  # n_cell_g <- glabel("   ---", container = effo_g)
  addSpring(figr_g)
  figr_b <- gbutton(text = "Show data", container = figr_g, handler = function(h,..){
    svalue(uti_gn) <- 8
  })
  figr_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  figr_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
  add(figr_g, figr_sta)
  #   enabled(effo_b) <- FALSE

  addSpring(pro_g_mid)
  # gbutton("Register", container = pro_g_mid)
  regi_g <- gframe(text = "Register", horizontal = FALSE, container = pro_g_mid)
  # n_cell_g <- glabel("   ---", container = regi_g)
  addSpring(regi_g)
  regi_b <- gbutton(text = "Show data", container = regi_g, handler = function(h,..){
    svalue(uti_gn) <- 9
  })
  regi_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  regi_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
  add(regi_g, regi_sta)
  #   enabled(regi_b) <- FALSE

  addSpring(pro_g_mid)
  # gbutton("Production", container = pro_g_mid)
  prod_g <- gframe(text = "Production", horizontal = FALSE, container = pro_g_mid)
  # n_cell_g <- glabel("   ---", container = prod_g)
  addSpring(prod_g)
  prod_b <- gbutton(text = "Show data", container = prod_g, handler = function(h,..){
    svalue(uti_gn) <- 10
  })
  prod_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  add(prod_g, prod_sta)
  #   enabled(prod_b) <- FALSE

  addSpring(pro_g_mid)
  addSpace(pro_g_mid, 2)


  ####   Environment   ####

  gri_g <- gvbox(container = uti_gn, label = "Grid", expand = TRUE)
  # addSpace(gri_g, 2, horizontal = TRUE)
  gri_g_top <- gframe(horizontal = TRUE, container = gri_g)
  addSpace(gri_g_top, 20)
  # addSpace(gri_g, 2, horizontal = TRUE)
  # addSpring(gri_g_top)
  gri_g_top1 <- ggroup(horizontal = FALSE, container = gri_g_top)
  addSpring(gri_g_top1)
  gri_g_top1_gri <- ggroup(horizontal = TRUE, container = gri_g_top1)
  # addSpring(gri_g_top1_gri)
  gbutton("Load Grid", container = gri_g_top1_gri, handler = function(h,...){
    svalue(stat_bar) <- "Loading Grid..."
    enabled(gri_g_top) <- FALSE
    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])

    my_project$loadMap(pathGridShp)

    ### automatic download of google map
    my_project$sampMap$getGooMap()       ### FIX add a check if sampMap is loaded
    svalue(stat_bar) <- "Downloading Google map..."
    my_project$sampMap$setGooGrid()
    my_project$sampMap$setGooBbox()

    svalue(gri_l1) <- paste("Polyset: Loaded")
    svalue(gri_l2) <- paste("N. Cells: ", my_project$sampMap$nCells)
    svalue(gri_l3) <- paste("GCenter: Loaded")
    svalue(stat_bar) <- "Plotting grid..."
    if(!is.null(my_project$sampMap$gooMap)){
      my_project$sampMap$plotGooGrid()
    }else{
      my_project$sampMap$plotSamMap(title = my_project$sampMap$gridName)
    }
    if(!is.null(my_project$rawDataSurvey)){
      svalue(stat_bar) <- "Splitting Survey Population..."
      my_project$setLFDPopSurvey()
    }
    # if(!is.null(my_project$rawDataFishery)){
    #   svalue(stat_bar) <- "Splitting Fishery Population..."
    #   my_project$setLFDPopFishery()
    # }
    enabled(gri_g_top) <- TRUE

    svalue(stat_bar) <- ""

    ### Update Grid Status
    svalue(n_cell_g) <- paste("N. Cells: ", my_project$sampMap$nCells)
    delete(grid_g, grid_g$children[[length(grid_g$children)]])
    add(grid_g, grid_sta_n)
  })
  # addSpring(gri_g_top1_gri)

  gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = gri_g_top1_gri,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+1])
           svalue(stat_bar) <- "Plotting grid..."
           enabled(gri_g_top) <- FALSE

           if(!is.null(my_project$sampMap$gooMap)){
             my_project$sampMap$plotGooGrid()
           }else{
             my_project$sampMap$plotSamMap(title = my_project$sampMap$gridName)
           }
           enabled(gri_g_top) <- TRUE

         })
  addSpring(gri_g_top1_gri)

  addSpring(gri_g_top1)

  gri_g_top1_dep <- ggroup(horizontal = TRUE, container = gri_g_top1)
  #   addSpring(gri_g_top1_dep)
  gbutton("Download Depth", container = gri_g_top1_dep, handler = function(h,...){
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Downloading depth..."
    enabled(gri_g_top) <- FALSE

    Sys.sleep(1)
    my_project$sampMap$getGridBath()
    svalue(stat_bar) <- "Plotting Bathymetry..."
    Sys.sleep(1)
    my_project$sampMap$ggplotGridBathy()
    svalue(stat_bar) <- ""
    enabled(gri_g_top) <- TRUE
  })
  addSpring(gri_g_top1_dep)
  gimage(system.file("ico/document-save-2.ico", package="smartR"), container = gri_g_top1_dep,
         handler = function(h,...){
           if(!is.null(my_project$sampMap$gridBathy)){
             save2path <- "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/bathy_test.rData"
             my_project$sampMap$saveGridBath(save2path)
           }
         })
  addSpring(gri_g_top1_dep)
  gbutton("Load Depth", container = gri_g_top1_dep, handler = function(h,...){
    dev.set(dev.list()[pre_dev+1])
    ## Get path to bathymetry rData
    svalue(stat_bar) <- "Loading depth..."
    enabled(gri_g_top) <- FALSE

    Sys.sleep(1)

    my_project$sampMap$loadGridBath(pathBathymetry)
    # my_project$sampMap$getCentDept()
    svalue(stat_bar) <- "Plotting Bathymetry..."
    Sys.sleep(1)
    my_project$sampMap$ggplotGridBathy()
    svalue(stat_bar) <- ""
    enabled(gri_g_top) <- TRUE

  })
  # addSpring(gri_g_top1_dep)
  gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = gri_g_top1_dep,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+1])
           my_project$sampMap$ggplotGridBathy()
         })
  addSpring(gri_g_top1_dep)

  addSpring(gri_g_top1)

  gri_g_top1_bio <- ggroup(horizontal = TRUE, container = gri_g_top1)
  # addSpring(gri_g_top1_bio)
  gbutton("Load Biocenosis", container = gri_g_top1_bio, handler = function(h,...){
    enabled(gri_g_top) <- FALSE

    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Loading biocenosis data.frame..."

    my_project$sampMap$loadBioDF(pathSeabed)

    if(!is.null(my_project$sampMap$gooMap)){
      my_project$sampMap$ggplotBioDF()
    }else{
      my_project$sampMap$plotBioDF()
    }
    svalue(stat_bar) <- ""
    enabled(gri_g_top) <- TRUE

  })
  # addSpring(gri_g_top1_bio)
  gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = gri_g_top1_bio,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+1])
           if(!is.null(my_project$sampMap$gooMap)){
             my_project$sampMap$ggplotBioDF()
           }else{
             my_project$sampMap$plotBioDF()
           }         })
  addSpring(gri_g_top1_bio)

  gri_g_top1_goo <- ggroup(horizontal = TRUE, container = gri_g_top1)
  # addSpring(gri_g_top1_goo)
  gbutton("Load Google map", container = gri_g_top1_goo, handler = function(h,...){
    enabled(gri_g_top) <- FALSE

    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Loading Google map..."
    my_project$sampMap$getGooMap()
    my_project$sampMap$setGooGrid()
    my_project$sampMap$setGooBbox()
    svalue(stat_bar) <- ""
    my_project$sampMap$plotGooGrid()
    enabled(gri_g_top) <- TRUE

  })
  # addSpring(gri_g_top1_goo)
  gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = gri_g_top1_goo,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+1])
           my_project$sampMap$plotGooGrid()
         })
  addSpring(gri_g_top1_goo)

  addSpring(gri_g_top1)

  addSpring(gri_g_top)

  gri_g_top2 <- ggroup(horizontal = FALSE, container = gri_g_top)
  addSpring(gri_g_top2)
  gri_l1 <- glabel("Polyset: ", container = gri_g_top2)
  gri_l2 <- glabel("N. Cells: ", container = gri_g_top2)
  gri_l3 <- glabel("GCenter: ", container = gri_g_top2)
  addSpring(gri_g_top2)

  addSpring(gri_g_top)

  gri_p <- ggraphics(container = gri_g, width = 550, height = 250, expand = TRUE)


  ####   Effort   ####

  eff_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Effort")
  eff_g_top <- gframe(horizontal = TRUE, container = eff_g)
  addSpace(eff_g_top, 2)
  addSpring(eff_g_top)
  eff_g_top1 <- ggroup(horizontal = FALSE, container = eff_g_top)
  addSpring(eff_g_top1)
  gbutton("Load from rData", container = eff_g_top1, handler = function(h,...){
    # my_project$fleet$loadMatEffort("/Users/Lomo/Documents/Uni/R/smart/data/EFF_OTB_Hours_9years.rData")
    #### SKIPPED LOADING rData
    #     tmp_files <- gfile(text = "Select Effort rData", type = "open",
    #                        initial.filename = NULL, initial.dir = getwd(), filter = list(),
    #                        multi = TRUE)
    #
    #     my_project$loadFleeEffoDbs(tmp_files)
    #     my_project$fleet$rawEffort <- readRDS(tmp_files)

    enabled(eff_g_top) <- FALSE

    cat("\nLoading effort from rData...", sep = "")
    svalue(stat_bar) <- "Loading effort from rData..."
    Sys.sleep(1)

    my_project$fleet$rawEffort <- readRDS(pathRawVMS)
    my_project$fleet$setEffortIds()
    # cat("   Done!", sep = "")
    svalue(stat_bar) <- ""

    effvie_drop[] <- names(my_project$fleet$rawEffort)
    svalue(effvie_drop) <- names(my_project$fleet$rawEffort)[1]
    dev.set(dev.list()[pre_dev+2])

    svalue(stat_bar) <- "Plotting raw points..."
    my_project$ggplotRawPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""

    # my_project$effPlot("All")

    enabled(eff_g_top) <- TRUE

    ### Update Effort Status
    effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    delete(effo_g, effo_g$children[[length(effo_g$children)]])
    add(effo_g, effo_sta_n)
  })
  addSpring(eff_g_top1)
  gbutton("Extract from VMSBASE", container = eff_g_top1, handler = function(h,...){

    enabled(eff_g_top) <- FALSE

    #### SKIPPED LOADING rData
    #     tmp_files <- gfile(text = "Select Effort DBs", type = "open",
    #                        initial.filename = NULL, initial.dir = getwd(), filter = list(),
    #                        multi = TRUE)
    #
    #     my_project$loadFleeEffoDbs(tmp_files)

    tmp_file <- "/Users/Lomo/Documents/Uni/PhD/TESI/SoS_vms/smart_rawEffort_new.rData"
    # tmp_file <- "/Users/Lomo/Documents/Uni/R/smart/data/RawEffort/rawEffort_seabedGrid_afterAll.rData"

    cat("\nLoading effort from vmsbase db...", sep = "")
    svalue(stat_bar) <- "Loading effort from vmsbase db..."
    Sys.sleep(1)
    my_project$fleet$rawEffort <- readRDS(tmp_file)
    my_project$fleet$setEffortIds()
    cat("   Done!", sep = "")
    svalue(stat_bar) <- ""

    effvie_drop[] <- names(my_project$fleet$rawEffort)
    svalue(effvie_drop) <- names(my_project$fleet$rawEffort)[1]
    dev.set(dev.list()[pre_dev+2])

    svalue(stat_bar) <- "Plotting Count of disinct vessels..."
    Sys.sleep(1)
    my_project$fleet$plotCountIDsEffo()
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

    ### Update Effort Status
    effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    delete(effo_g, effo_g$children[[length(effo_g$children)]])
    add(effo_g, effo_sta_n)
  })
  addSpring(eff_g_top1)
  gbutton("View Stats", container = eff_g_top1, handler = function(h,...){
    dev.set(dev.list()[pre_dev+2])

    enabled(eff_g_top) <- FALSE

    svalue(stat_bar) <- "Plotting Count of disinct vessels..."
    Sys.sleep(1)
    my_project$fleet$plotCountIDsEffo()
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top1)

  addSpring(eff_g_top)

  eff_g_top1b <- ggroup(horizontal = FALSE, container = eff_g_top)
  addSpring(eff_g_top1b)
  gbutton("Set Fishing Points", container = eff_g_top1b, handler = function(h,...){
    svalue(stat_bar) <- "Setting parameters..."

    enabled(eff_g_top) <- FALSE

    temp_dia <- gwindow(title="Set Fishing Points", visible = FALSE,
                        width = 650, height = 450, parent = main_win)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia)
    up_fra <- gframe(container = up_g, horizontal = TRUE)
    addSpring(up_fra)
    spe_fra <- gframe(text = "Speed Range", container = up_fra, horizontal = TRUE)
    spe_lay <- glayout(homogeneous = FALSE, spacing = 10, container = spe_fra)
    spe_lay[1,1, anchor = 0] <- "Min"
    spe_lay[2,1, anchor = 0] <- "Max"
    spe_lay[1,2, anchor = 0] <- gspinbutton(from = 0, to = 30, by = 1, value = 0, container = spe_lay,
                                            handler = function(...){
                                              my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                                                              speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                                                              depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
                                            })
    spe_lay[2,2, anchor = 0] <- gspinbutton(from = 0, to = 30, by = 1, value = 10, container = spe_lay,
                                            handler = function(...){
                                              my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                                                              speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                                                              depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
                                            })
    addSpring(up_fra)
    dep_fra <- gframe(text = "Depth Range", container = up_fra, horizontal = TRUE)
    dep_lay <- glayout(homogeneous = FALSE, spacing = 10, container = dep_fra)
    dep_lay[1,1, anchor = 0] <- "Min"
    dep_lay[2,1, anchor = 0] <- "Max"
    dep_lay[1,2, anchor = 0] <- gspinbutton(from = -5000, to = 0, by = 10, value = -500, container = dep_lay,
                                            handler = function(...){
                                              my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                                                              speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                                                              depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
                                            })
    dep_lay[2,2, anchor = 0] <- gspinbutton(from = -5000, to = 0, by = 10, value = 0, container = dep_lay,
                                            handler = function(...){
                                              my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                                                              speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                                                              depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
                                            })
    addSpring(up_fra)
    yea_fra <- gframe(text = "Year View", container = up_fra, horizontal = TRUE)
    yea_drop <- gcombobox(names(my_project$fleet$rawEffort), selected = 1,
                          editable = FALSE, container = yea_fra, expand = TRUE,
                          handler = function(...){
                            my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                                            speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                                            depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
                          })
    addSpring(up_fra)
    gbutton(text = "\n   Set!   \n", container = up_fra, handler = function(...){
      enabled(up_fra) <- FALSE

      plot(NULL, xlim = c(0,5), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")

      my_project$fleet$setFishPoinPara(speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                       depth_range = sort(unlist(lapply(dep_lay[1:2,2], svalue)), decreasing = TRUE))

      svalue(stat_bar) <- "Setting fishing points..."
      points(1,0, pch = 19, col = "grey20")
      text(1,0, labels = "Parameters", pos = 3, col = "grey20", srt = 45, offset = 1.5)
      svalue(int_progBar) <- 10
      Sys.sleep(1)

      my_project$fleet$setFishPoin()

      points(2,0, pch = 19, col = "grey20")
      text(2,0, labels = "Fishing Points", pos = 3, col = "grey20", srt = 45, offset = 1.5)
      svalue(stat_bar) <- ""
      svalue(int_progBar) <- 30
      Sys.sleep(1)
      svalue(stat_bar) <- "Setting fishing point cells..."
      Sys.sleep(1)

      my_project$setCellPoin()

      points(3,0, pch = 19, col = "grey20")
      text(3,0, labels = "Cell Assign", pos = 3, col = "grey20", srt = 45, offset = 1.5)
      svalue(stat_bar) <- "Adding week and month number to dataset..."
      svalue(int_progBar) <- 60
      Sys.sleep(1)

      my_project$fleet$setWeekMonthNum()

      points(4,0, pch = 19, col = "grey20")
      text(4,0, labels = "Week/Month\nAppend", pos = 3, col = "grey20", srt = 45, offset = 1.5)
      svalue(stat_bar) <- ""
      svalue(int_progBar) <- 90
      Sys.sleep(1)
      svalue(stat_bar) <- "Completed!"
      svalue(int_progBar) <- 100
      Sys.sleep(1)
      if(gconfirm("Fishing point selection completed!\nClose window?", title = "Confirm", icon = "question", parent = temp_dia)){

        enabled(eff_g_top) <- TRUE

        dispose(temp_dia)
      }else{
        enabled(up_fra) <- TRUE

        enabled(eff_g_top) <- TRUE
      }
    })
    addSpring(up_fra)
    fipo_gra <- ggraphics(width = 600, height = 400, container = up_g, expand = TRUE)

    bot_progStat <- ggroup(container = up_g, horizontal = TRUE)

    addSpring(bot_progStat)
    int_progBar <- gprogressbar(value = 0, container = bot_progStat)
    addSpring(bot_progStat)

    visible(temp_dia) <- TRUE

    my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                    speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                    depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
  })

  addSpring(eff_g_top1b)

  gbutton("View Stats", container = eff_g_top1b, handler = function(h,...){
    dev.set(dev.list()[pre_dev+2])
    svalue(stat_bar) <- "Plotting fishing points data summary..."

    enabled(eff_g_top) <- FALSE

    Sys.sleep(1)
    my_project$fleet$plotFishPoinStat()
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  # addSpring(eff_g_top1b)
  addSpring(eff_g_top1b)

  addSpring(eff_g_top)

  eff_g_top_IO <- ggroup(horizontal = FALSE, container = eff_g_top)
  addSpring(eff_g_top_IO)
  gbutton("Load AA data", container = eff_g_top_IO, handler = function(h,...){
    svalue(stat_bar) <- "Loading AA data..."

    enabled(eff_g_top) <- FALSE

    Sys.sleep(1)

    #### SKIPPED LOADING rData
    #     tmp_files <- gfile(text = "Select AA effort data", type = "open",
    #                        initial.filename = NULL, initial.dir = getwd(), filter = list(),
    #                        multi = TRUE)
    #

    cat("\nLoading effort from AA data...", sep = "")
    svalue(stat_bar) <- "Loading effort from AA data..."
    Sys.sleep(1)

    my_project$fleet$rawEffort <- readRDS(pathEffortAA)
    my_project$fleet$setEffortIds()

    effvie_drop[] <- names(my_project$fleet$rawEffort)
    svalue(effvie_drop) <- names(my_project$fleet$rawEffort)[1]
    dev.set(dev.list()[pre_dev+2])

    my_project$ggplotGridEffort(names(my_project$fleet$rawEffort)[1])

    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top_IO)
  gbutton("Save AA data", container = eff_g_top_IO, handler = function(h,...){
    svalue(stat_bar) <- "Saving AA data..."
    Sys.sleep(1)

    enabled(eff_g_top) <- FALSE

    #### SKIPPED SAVING rData
    #     tmp_files <- gfile(text = "Select AA effort data", type = "save",
    #                        initial.filename = NULL, initial.dir = getwd(), filter = list(),
    #                        multi = TRUE)
    #

    # tmp_file <- "/Users/Lomo/Documents/Uni/PhD/TESI/SoS_vms/smart_rawEffort_new.rData"
    tmp_file <- "/Users/Lomo/Documents/Uni/R/smart/data/RawEffort/rawEffort_seabedGrid_afterAll.rData"

    cat("\nSaving AA effort to rData...", sep = "")
    svalue(stat_bar) <- "Saving AA effort to rData..."
    Sys.sleep(1)

    saveRDS(my_project$fleet$rawEffort, tmp_file)

    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top_IO)

  addSpring(eff_g_top)

  eff_g_top2 <- gframe(text = "View", horizontal = TRUE, container = eff_g_top, expand = TRUE)
  addSpring(eff_g_top2)
  effvie_drop <- gcombobox(items = "Year", selected = 1, container = eff_g_top2, expand = TRUE, editable = FALSE)
  addSpring(eff_g_top2)

  eff_g_top2_ver <- ggroup(horizontal = FALSE, container = eff_g_top2)
  addSpring(eff_g_top2_ver)
  gbutton("Raw Points", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+2])
    svalue(stat_bar) <- "Plotting raw points..."

    enabled(eff_g_top) <- FALSE

    Sys.sleep(1)
    my_project$ggplotRawPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top2_ver)
  gbutton("Fishing Points", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+2])
    svalue(stat_bar) <- "Plotting fishing points..."

    enabled(eff_g_top) <- FALSE

    Sys.sleep(1)
    my_project$ggplotFishingPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top2_ver)
  gbutton("Gridded Points", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+2])
    svalue(stat_bar) <- "Plotting gridded points..."

    enabled(eff_g_top) <- FALSE

    Sys.sleep(1)
    my_project$ggplotGridEffort(svalue(effvie_drop))
    svalue(stat_bar) <- ""

    enabled(eff_g_top) <- TRUE

  })
  addSpring(eff_g_top2_ver)
  addSpring(eff_g_top2)
  # addSpring(eff_g_top1c)
  addSpring(eff_g_top)
  eff_p <- ggraphics(container = eff_g, width = 550, height = 250, expand = TRUE)


  ####   Fishing Grounds   ####

  fig_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Fishing Grounds")
  fig_g_top <- gframe(horizontal = TRUE, container = fig_g)

  addSpring(fig_g_top)
  fig_g_top_vars <- gframe(text = "Variables and Weights", horizontal = FALSE, container = fig_g_top, expand = TRUE)
  addSpring(fig_g_top_vars)
  gbutton("\nSelect\n", container = fig_g_top_vars, handler = function(h,...){

    temp_dia <- gwindow(title="Select variables and weights", visible = FALSE,
                        parent = main_win,
                        width = 350, height = 200)

    up_g_top <- ggroup(horizontal = FALSE, container = temp_dia)
    addSpring(up_g_top)
    up_g <- ggroup(horizontal = TRUE, container = up_g_top)

    addSpring(up_g)

    # speVars_gru <- ggroup(horizontal = TRUE, container = up_g)
    lyt <- glayout(container = up_g)

    lyt[1,2] <- "Variables"
    lyt[1,3] <- "      Weights      "
    # lyt[2,2] <- gcheckbox(c("Sampling Distribution"), checked = TRUE, container = lyt)
    # lyt[2,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[2,2] <- gcheckbox(c("Seabed"), checked = FALSE, container = lyt)
    lyt[2,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[3,2] <- gcheckbox(c("Fishing Effort"), checked = FALSE, container = lyt)
    lyt[3,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[4,2] <- gcheckbox(c("Bathymetry"), checked = FALSE, container = lyt)
    lyt[4,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)

    addSpring(up_g)
    addSpring(up_g_top)
    bot_g <- ggroup(horizontal = TRUE, container = up_g_top)
    addSpring(bot_g)

    gbutton("\nAccept\n", container = bot_g,
            handler = function(h,...){

              cat("\nInput for Fishing Grounds Clustering:\n")
              my_project$sampMap$setClusInpu(whiData = unlist(lapply(lyt[2:4,2], svalue)),
                                             howData = unlist(lapply(lyt[2:4,3], svalue)))

              dispose(temp_dia)
            })
    addSpring(bot_g)

    my_project$setAvailData()

    ifelse("Seabed" %in% my_project$sampMap$availData,
           lapply(lyt[2,], function(x) enabled(x) <- TRUE),
           lapply(lyt[2,], function(x) enabled(x) <- FALSE))
    ifelse("Effort" %in% my_project$sampMap$availData,
           lapply(lyt[3,], function(x) enabled(x) <- TRUE),
           lapply(lyt[3,], function(x) enabled(x) <- FALSE))
    ifelse("Depth" %in% my_project$sampMap$availData,
           lapply(lyt[4,], function(x) enabled(x) <- TRUE),
           lapply(lyt[4,], function(x) enabled(x) <- FALSE))

    visible(temp_dia) <- TRUE
  })

  addSpring(fig_g_top_vars)
  addSpring(fig_g_top)

  fig_g_top_dist <- gframe(text = "Distance Metric", horizontal = FALSE, container = fig_g_top, expand = TRUE)
  addSpring(fig_g_top_dist)
  fg_metr <- gcombobox(c("euclidean", "manhattan", "maximum",
                         "binary", "canberra", "minkowski"), selected = 1, editable = FALSE,
                       container = fig_g_top_dist, expand = TRUE)

  addSpring(fig_g_top_dist)
  addSpring(fig_g_top)

  fig_g_top_clus <- gframe(text = "Clustering", horizontal = TRUE, container = fig_g_top, expand = TRUE)
  addSpring(fig_g_top_clus)
  fg_maxCut <- gslider(from = 1, to = 50, horizontal = TRUE, by = 1,
                       container = fig_g_top_clus)
  addSpring(fig_g_top_clus)
  addSpring(fig_g_top)

  gbutton("\n   Run   \n", container = fig_g_top, handler = function(h,...){
    my_project$sampMap$calcFishGrou(numCuts = svalue(fg_maxCut), #max 50
                                    minsize = 10,
                                    modeska = "S",
                                    skater_method = svalue(fg_metr))
    dev.set(dev.list()[pre_dev+3])

    fg_plotCut[] <- 1:svalue(fg_maxCut)
    svalue(fg_plotCut) <- which.max(my_project$sampMap$indSil)

    ### Update Fishing Grounds Status
    delete(figr_g, figr_g$children[[length(figr_g$children)]])
    add(figr_g, figr_sta_n)
  })

  addSpring(fig_g_top)

  fig_g_top_plot <- gframe(text = "Plot", horizontal = TRUE, container = fig_g_top, expand = TRUE)
  addSpring(fig_g_top_plot)
  fg_plotCut <- gcombobox(items = 2:50,
                          container = fig_g_top_plot, handler = function(h,...){
                            dev.set(dev.list()[pre_dev+3])
                            my_project$sampMap$setCutResult(ind_clu = svalue(fg_plotCut))

                            suppressWarnings(grid.arrange(my_project$sampMap$ggIchFGlin,
                                                          my_project$sampMap$ggSilFGlin,
                                                          my_project$sampMap$ggCutFGmap,
                                                          my_project$sampMap$ggEffoFGmap,
                                                          my_project$sampMap$ggDepthFGbox,
                                                          my_project$sampMap$ggEffoFGbox,
                                                          my_project$sampMap$ggBioFGmat,
                                                          layout_matrix = rbind(c(1,3,3,5,6,7),c(2,4,4,5,6,7))))

                          })

  addSpring(fig_g_top_plot)

  addSpring(fig_g_top)

  gbutton("    Select\n      this\nPartitioning", container = fig_g_top, handler = function(h,...){
    my_project$setFishGround(numCut = svalue(fg_plotCut))
    my_project$sampMap$setCutResult(ind_clu = svalue(fg_plotCut))

    suppressWarnings(grid.arrange(my_project$sampMap$ggIchFGlin,
                                  my_project$sampMap$ggSilFGlin,
                                  my_project$sampMap$ggCutFGmap,
                                  my_project$sampMap$ggEffoFGmap,
                                  my_project$sampMap$ggDepthFGbox,
                                  my_project$sampMap$ggEffoFGbox,
                                  my_project$sampMap$ggBioFGmat,
                                  layout_matrix = rbind(c(1,3,3,5,6,7),c(2,4,4,5,6,7))))
  })
  addSpring(fig_g_top)

  addSpace(fig_g_top, 2)
  fisGro_p <- ggraphics(container = fig_g, width = 550, height = 250, expand = TRUE)


  ####   Register   ####

  reg_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Register")
  reg_g_top <- gframe(horizontal = TRUE, container = reg_g)
  addSpace(reg_g_top, 2)
  addSpring(reg_g_top)
  reg_g_top_raw <- ggroup(horizontal = FALSE, container = reg_g_top)
  addSpring(reg_g_top_raw)
  gbutton("Load EU register", container = reg_g_top_raw, handler = function(h,...){
    my_project$fleet$loadFleetRegis("/Users/Lomo/Documents/Uni/R/smart/data/Fleet/ITA_export_smart-ed.csv")
    my_project$fleet$cleanRegister()
    my_project$fleet$setVmsRegister()
    # my_project$fleet$splitFleet()
    dev.set(dev.list()[pre_dev+4])
    # my_project$fleet$plotRegSum()
    ggplot_registerDispatch(curRegister = my_project$fleet$rawRegister, selPlot = "Summary")

    ### Update Register Status
    if(!is.null(my_project$fleet$rawRegister)){
      delete(regi_g, regi_g$children[[length(regi_g$children)]])
      add(regi_g, regi_sta_n)
    }
  })
  addSpring(reg_g_top_raw)
  gbutton("View Raw Data", container = reg_g_top_raw, handler = function(h,...){
    temp_flee <- gbasicdialog(title="Explore Fleet Register", parent = main_win)
    tmp_data <- my_project$fleet$rawRegister
    tmp_data[which(is.na(tmp_data), arr.ind = TRUE)] <- "NA"
    flee_data <- gtable(tmp_data, container = temp_flee)
    size(temp_flee) <- c(600, 400)
    visible(temp_flee)
  })
  addSpring(reg_g_top_raw)
  addSpring(reg_g_top)
  reg_g_top_view <- gframe(text = "Plot Summary Data", horizontal = TRUE, container = reg_g_top, expand = TRUE)
  addSpace(reg_g_top_view, 10)
  sel_regSet <- gradio(c("All", "Vms"), selected = 1, horizontal = FALSE,
                       container = reg_g_top_view, handler = function(h,...){
                         dev.set(dev.list()[pre_dev+4])
                         if(svalue(sel_regSet) == "All"){
                           ggplot_registerDispatch(curRegister = my_project$fleet$rawRegister, selPlot = svalue(sel_regPlot))
                         }else{
                           ggplot_registerDispatch(curRegister = my_project$fleet$vmsRegister, selPlot = svalue(sel_regPlot))
                         }
                       })
  addSpace(reg_g_top_view, 10)
  sel_regPlot <- gcombobox(c("Summary", "Main Gear", "Secondary Gear", "Hull Material",
                             "Construction Year", "Length Over All", "Main Power"),
                           selected = 1, editable = FALSE, expand = TRUE, container = reg_g_top_view,
                           handler = function(h,...){
                             dev.set(dev.list()[pre_dev+4])
                             if(svalue(sel_regSet) == "All"){
                               ggplot_registerDispatch(curRegister = my_project$fleet$rawRegister, selPlot = svalue(sel_regPlot))
                             }else{
                               ggplot_registerDispatch(curRegister = my_project$fleet$vmsRegister, selPlot = svalue(sel_regPlot))
                             }
                           })
  addSpace(reg_g_top_view, 10)
  addSpring(reg_g_top)
  reg_g_top_harbs <- gframe(text = "Harbour Distance", horizontal = TRUE, container = reg_g_top, expand = TRUE)
  addSpace(reg_g_top_harbs, 10)
  gbutton("Get Harbours", container = reg_g_top_harbs, handler = function(h,...){
    my_project$fleet$setRegHarbs()
  })
  addSpace(reg_g_top_harbs, 10)

  reg_g_harb_ico <- ggroup(horizontal = FALSE, container = reg_g_top_harbs)
  addSpring(reg_g_harb_ico)
  gimage(system.file("ico/folder-man.png", package="smartR"), container = reg_g_harb_ico,
         handler = function(h,...){

           load_path <- gfile(text = "Select Harbour List file", type = "open", filter = list("R files" = list(patterns = c("*.rData"))))

           if(length(load_path) == 0) stop("Missing File to Load!")

           cat("\nLoading Harbour List from ", load_path, sep = "")

           my_project$fleet$loadFleetHarb(harb_path = load_path)

         })
  addSpring(reg_g_harb_ico)
  gimage(system.file("ico/document-save-2.ico", package="smartR"), container = reg_g_harb_ico,
         handler = function(h,...){
           if(is.null(my_project$fleet$regHarbsUni)) stop("Missing Harbour Data List")

           save_dest <- gfile(text = "Select file name and destination directory", type = "save",
                              initial.filename = "Smart_Harbour_List.rData", initial.dir = my_project$sampMap$gridPath)
           if(rev(unlist(strsplit(save_dest, "[.]")))[1] != "rData"){
             save_dest <- paste(save_dest, ".rData", sep = "")
           }

           my_project$fleet$saveFleetHarb(harb_path = save_dest)

           cat("\nHarbour List saved in: ", save_dest, sep = "")
         })
  addSpring(reg_g_harb_ico)

  addSpring(reg_g_top_harbs)
  gbutton("Set Weighted\nDistance", container = reg_g_top_harbs, handler = function(h,...){
    dev.set(dev.list()[pre_dev+4])
    my_project$getHarbFgDist()
    my_project$ggplotFgWeigDists()
  })
  addSpace(reg_g_top_harbs, 10)
  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"), container = reg_g_top_harbs,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+4])
           my_project$ggplotFgWeigDists()
         })
  addSpace(reg_g_top_harbs, 10)
  addSpring(reg_g_top)
  regGro_p <- ggraphics(container = reg_g, width = 550, height = 250, expand = TRUE)


  ####   Production   ####

  pro_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Production")
  pro_g_top <- gframe(horizontal = TRUE, container = pro_g)
  addSpace(pro_g_top, 2)
  # addSpring(pro_g_top)
  addSpace(pro_g_top, 40)
  pro_g_top1 <- ggroup(horizontal = FALSE, container = pro_g_top, expand = TRUE)
  addSpring(pro_g_top1)
  #   gbutton("Edit Raw Production", container = pro_g_top1, handler = function(h,...){
  #
  #   })
  gbutton("Load Landings", container = pro_g_top1, handler = function(h,...){
    # tmp_files <- gfile(text = "Select Landings Data", type = "open",
    #                    filter = list("csv files" = list(patterns = c("*.csv")),
    #                                  "All files" = list(patterns = c("*"))),
    #                    multi = TRUE)
    # my_project$fleet$loadProduction(tmp_files)
    enabled(pro_g_top) <- FALSE
    svalue(stat_bar) <- "Loading landings..."
    Sys.sleep(1)

    dev.set(dev.list()[pre_dev+5])
    my_project$fleet$rawProduction <- readRDS(pathLanding)

    svalue(stat_bar) <- "Setting Ids..."
    Sys.sleep(1)
    my_project$fleet$setProdIds()

    # my_project$fleet$plotCountIDsProd()

    my_project$fleet$setIdsEffoProd()

    svalue(stat_bar) <- "Plotting Ids..."
    Sys.sleep(1)
    my_project$fleet$plotCountIDsEffoProd()

    svalue(stat_bar) <- "Setting Production Matrix..."
    Sys.sleep(1)
    my_project$fleet$setProdMatr()
    my_project$fleet$setDayEffoMatrGround()
    my_project$fleet$setEffoProdMatr()
    my_project$fleet$setEffoProdMont()

    svalue(stat_bar) <- "Setting Species..."
    Sys.sleep(1)
    my_project$fleet$setProdSpec()
    my_project$fleet$setEffoProdAll()

    svalue(stat_bar) <- "Getting Loa..."
    Sys.sleep(1)
    my_project$fleet$setEffoProdAllLoa()

    svalue(stat_bar) <- ""
    Sys.sleep(1)
    # my_project$fleet$setSpecSett()
    spe_drop[] <- sort(names(my_project$fleet$specSett))
    provie_drop[] <- names(my_project$fleet$effoProdMont)
    svalue(spe_drop, index = TRUE) <- 1
    svalue(provie_drop, index = TRUE) <- 1
    enabled(spe_drop) <- TRUE

    enabled(pro_g_top) <- TRUE

  })
  addSpring(pro_g_top1)

  spe_fra <- gframe(text = "Specie", container = pro_g_top1, horizontal = TRUE, expand = TRUE)
  # addSpace(spe_fra, 20)
  spe_drop <- gcombobox("Specie list", selected = 1,
                        editable = FALSE, container = spe_fra, expand = TRUE)
  addSpring(pro_g_top1)

  # addSpace(spe_fra, 20)
  addSpace(pro_g_top, 40)
  # addSpring(pro_g_top)
  enabled(spe_drop) <- FALSE

  pro_g_top2 <- ggroup(horizontal = FALSE, container = pro_g_top)
  addSpring(pro_g_top2)

  gbutton("Set Threshold", container = pro_g_top2, handler = function(h,...){

    temp_dia <- gwindow(title="Set Landings Threshold", visible = FALSE,
                        parent = main_win,
                        width = 900, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    # addSpring(up_fra)
    addSpace(up_fra, 20)
    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    # addSpace(spe_fra, 20)
    addSpring(spe_fra)
    glabel(text = svalue(spe_drop), container = spe_fra)
    # spe_drop <- gcombobox(sort(my_project$fleet$prodSpec[["Cross"]]), selected = 1,
    #                       editable = FALSE, container = spe_fra, expand = TRUE,
    #                       handler = function(...){
    tmp_spe <- my_project$fleet$effoProdAll[,which(colnames(my_project$fleet$effoProdAll) == svalue(spe_drop))]
    tmp_spe <- tmp_spe[tmp_spe != 0]
    #                       })
    # addSpace(spe_fra, 20)
    addSpring(spe_fra)
    addSpace(up_fra, 20)
    # addSpring(up_fra)
    thr_fra <- gframe(text = "Threshold", container = up_fra, horizontal = TRUE)
    addSpace(thr_fra, 20)
    thr_spin <- gspinbutton(from = 0, to = 100,
                            by = 0.5, value = 0, container = thr_fra,
                            handler = function(...){
                              tmp_spe <- my_project$fleet$effoProdAll[,which(colnames(my_project$fleet$effoProdAll) == svalue(spe_drop))]
                              tmp_spe <- tmp_spe[tmp_spe != 0]

                              hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
                                   breaks = svalue(num_bre_spin),
                                   main = bquote(italic(.(svalue(spe_drop)))), xlab = "")
                              abline(v = svalue(thr_spin), col = 2, lwd = 3, lty = 2)
                            })
    addSpace(up_fra, 20)
    addSpace(thr_fra, 20)

    bou_fra <- gframe(text = "Limits and Breaks", container = up_fra, horizontal = FALSE)
    addSpace(bou_fra, 20)
    bou_gru <- ggroup(horizontal = TRUE, container = bou_fra)
    glabel("Max Value:", container = bou_gru)
    addSpring(bou_gru)
    max_x_spin <- gspinbutton(from = 100, to = 1000,
                              by = 10, value = 100, container = bou_gru,
                              handler = function(...){
                                tmp_spe <- my_project$fleet$effoProdAll[,which(colnames(my_project$fleet$effoProdAll) == svalue(spe_drop))]
                                tmp_spe <- tmp_spe[tmp_spe != 0]

                                hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
                                     breaks = svalue(num_bre_spin),
                                     main = bquote(italic(.(svalue(spe_drop)))), xlab = "")
                                abline(v = svalue(thr_spin), col = 2, lwd = 3, lty = 2)
                              })
    bou_gru2 <- ggroup(horizontal = TRUE, container = bou_fra)
    glabel("N. Breaks:", container = bou_gru2)
    addSpring(bou_gru2)
    num_bre_spin <- gspinbutton(from = 10, to = 1000,
                                by = 10, value = 100, container = bou_gru2,
                                handler = function(...){
                                  tmp_spe <- my_project$fleet$effoProdAll[,which(colnames(my_project$fleet$effoProdAll) == svalue(spe_drop))]
                                  tmp_spe <- tmp_spe[tmp_spe != 0]

                                  hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
                                       breaks = svalue(num_bre_spin),
                                       main = bquote(italic(.(svalue(spe_drop)))), xlab = "")
                                  abline(v = svalue(thr_spin), col = 2, lwd = 3, lty = 2)
                                })

    addSpace(bou_fra, 20)
    addSpring(up_fra)

    set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
    addSpring(set_gru_up)
    set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
    set_lab <- glabel(text = "Not set", container = set_gru)
    addSpring(set_gru_up)

    addSpace(up_fra, 20)

    gbutton(text = "\n   Set!   \n", container = up_fra, handler = function(...){

      my_project$fleet$setSpecSettItm(specie = svalue(spe_drop),
                                      thresh = svalue(thr_spin),
                                      brea = svalue(num_bre_spin),
                                      max_xlim = svalue(max_x_spin))

      svalue(set_lab) <- "Set"
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, land_sta_n)

    })
    addSpring(up_fra)

    gbutton(text = " Close\nWindow", container = up_fra, handler = function(...){
      dispose(temp_dia)
    })
    addSpace(up_fra, 20)
    addSpring(up_g)
    land_gra <- ggraphics(width = 600, height = 400, container = up_g, expand = TRUE)
    visible(temp_dia) <- TRUE

    land_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    land_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    add(set_gru, land_sta)

    tmp_spe <- my_project$fleet$effoProdAll[,which(colnames(my_project$fleet$effoProdAll) == svalue(spe_drop))]
    tmp_spe <- tmp_spe[tmp_spe != 0]

    if(is.null(my_project$fleet$specSett[[svalue(spe_drop)]])){
      max_x_spin[] <- seq(0, max(tmp_spe), by = 10)
      svalue(max_x_spin) <- quantile(tmp_spe, 0.95)
      thr_spin[] <- seq(0, svalue(max_x_spin), by = 0.5)
      svalue(thr_spin) <- quantile(tmp_spe, 0.05)
      svalue(num_bre_spin) <- 100
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, land_sta)
      svalue(set_lab) <- "Not set"
    }else{
      thr_spin[] <- seq(0, my_project$fleet$specSett[[svalue(spe_drop)]]$max_x, by = 0.5)
      svalue(thr_spin) <- my_project$fleet$specSett[[svalue(spe_drop)]]$threshold
      svalue(num_bre_spin) <- my_project$fleet$specSett[[svalue(spe_drop)]]$breaks
      svalue(max_x_spin) <- my_project$fleet$specSett[[svalue(spe_drop)]]$max_x
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, land_sta_n)
      svalue(set_lab) <- "Set"
    }

    hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
         breaks = svalue(num_bre_spin),
         main = bquote(italic(.(svalue(spe_drop)))), xlab = "")
    abline(v = svalue(thr_spin), col = 2, lwd = 3, lty = 2)

  })
  addSpring(pro_g_top)
  addSpring(pro_g_top2)
  gbutton("Get Logit", container = pro_g_top2, handler = function(h,...){

    temp_dia <- gwindow(title="Get Logit", visible = FALSE,
                        parent = main_win,
                        width = 900, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    addSpace(up_fra, 20)

    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpace(spe_fra, 10)
    glabel(text = svalue(spe_drop), container = spe_fra)
    addSpace(spe_fra, 10)

    addSpace(up_fra, 20)
    mod_fra <- gframe(text = "Model", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpace(mod_fra, 20)
    mod_radSel <- gradio(items = c("GLM", "CART", "RF", "NN"),
                         selected = 1, horizontal = FALSE, container = mod_fra, handler = function(...){
                           switch(svalue(mod_radSel),
                                  GLM = {
                                    lapply(list(par_modSel[1,2]), function(x) enabled(x) <- TRUE)
                                    lapply(par_modSel[2:4,2], function(x) enabled(x) <- FALSE)},
                                  CART = {
                                    lapply(list(par_modSel[2,2]), function(x) enabled(x) <- TRUE)
                                    lapply(par_modSel[c(1,3:4),2], function(x) enabled(x) <- FALSE)},
                                  RF = {
                                    lapply(list(par_modSel[3,2]), function(x) enabled(x) <- TRUE)
                                    lapply(par_modSel[c(1:2,4),2], function(x) enabled(x) <- FALSE)},
                                  NN = {
                                    lapply(list(par_modSel[4,2]), function(x) enabled(x) <- TRUE)
                                    lapply(par_modSel[1:3,2], function(x) enabled(x) <- FALSE)}
                           )
                         })
    addSpace(mod_fra, 20)

    addSpace(up_fra, 20)
    par_fra <- gframe(text = "Parameters", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpace(par_fra, 20)

    par_modSel <- glayout(container = par_fra)
    par_modSel[1,1:2] <- "None"
    par_modSel[2,1] <- "cp"
    par_modSel[2,2] <- gspinbutton(from = 0, to = 1, by = 0.001, value = 0.01, container = par_modSel)
    par_modSel[3,1] <- "CV"
    par_modSel[3,2] <- gspinbutton(from = 2, to = 10, by = 1, value = 5, container = par_modSel)
    par_modSel[4,1] <- "Iter"
    par_modSel[4,2] <- gspinbutton(from = 100, to = 1000, by = 100, value = 100, container = par_modSel)

    addSpace(par_fra, 20)

    lapply(par_modSel[2:4,2], function(x) enabled(x) <- FALSE)

    addSpring(up_fra)

    gbutton(text = "Get\nLogit", container = up_fra, handler = function(...){
      my_project$fleet$setSpecLogit(selSpecie = svalue(spe_drop),
                                    selModel = svalue(mod_radSel),
                                    cp = svalue(par_modSel[2,2]),
                                    cv = svalue(par_modSel[3,2]))

      my_project$fleet$plotLogitROC(selSpecie = svalue(spe_drop))

      svalue(thr_spin) <- round(my_project$fleet$specLogit[[svalue(spe_drop)]]$logit$Cut, 2)
      svalue(tmp_txt) <- capture.output({cat("\n")
        print(my_project$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)})
    })
    addSpace(up_fra, 20)

    thr_fra <- gframe(text = "Tune Cutoff", container = up_fra, expand = TRUE, horizontal = TRUE)
    addSpace(thr_fra, 20)
    thr_spin <- gslider(from = 0.01, to = 0.99,
                        by = 0.01, value = 0.5, container = thr_fra, expand = TRUE,
                        handler = function(...){
                          if(!is.null(my_project$fleet$specLogit[[svalue(spe_drop)]])){
                            my_project$fleet$setSpecLogitConf(selSpecie = svalue(spe_drop), cutoff = svalue(thr_spin))
                            svalue(tmp_txt) <- capture.output({cat("\n")
                              print(my_project$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)})
                          }
                        })
    addSpace(up_fra, 20)
    addSpace(thr_fra, 20)

    set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
    addSpring(set_gru_up)
    gbutton(text = "\n   Save   \n", container = set_gru_up, handler = function(...){
      svalue(set_lab) <- "Saved"
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, logi_sta_n)
    })
    set_lab <- glabel(text = "Not Saved", container = set_gru_up)
    set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
    addSpring(set_gru_up)
    addSpace(up_fra, 20)

    # addSpring(up_fra)
    gbutton(text = " Close\nWindow", container = up_fra, handler = function(...){
      dispose(temp_dia)
    })
    addSpace(up_fra, 20)

    bot_g <- ggroup(horizontal = TRUE, container = up_g)
    addSpace(bot_g, 10)
    bot_lef_g <- ggroup(horizontal = FALSE, container = bot_g)
    addSpring(bot_lef_g)
    tmp_txt <- gtext(text = NULL, width = 300, height = 350, container = bot_lef_g)
    addSpring(bot_lef_g)
    addSpace(bot_g, 10)
    logi_gra <- ggraphics(width = 300, height = 400, container = bot_g, expand = TRUE)
    visible(temp_dia) <- TRUE
    addSpace(bot_g, 10)

    logi_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    logi_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    add(set_gru, logi_sta)

    if(!is.null(my_project$fleet$specLogit[[svalue(spe_drop)]]$logit$Roc)){
      my_project$fleet$plotLogitROC(svalue(spe_drop))
      svalue(tmp_txt) <- capture.output({cat("\n")
        print(my_project$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)})
    }

  })
  addSpring(pro_g_top2)

  gbutton("Get NNLS", container = pro_g_top2, handler = function(h,...){



    temp_dia <- gwindow(title="Get NNLS", visible = FALSE,
                        parent = main_win,
                        width = 950, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia, expand = TRUE)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    # addSpring(up_fra)
    addSpace(up_fra, 20)
    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpring(spe_fra)
    glabel(text = svalue(spe_drop), container = spe_fra)
    # spe_drop <- gcombobox(sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))]), selected = 1,
    #                       editable = FALSE, container = spe_fra, expand = TRUE,
    #                       handler = function(...){
    #
    #                       })
    addSpring(spe_fra)
    addSpace(up_fra, 20)

    gbutton(text = " Get\nNNLS", container = up_fra, handler = function(...){

      if(is.null(my_project$fleet$effoProdAllLoa)) my_project$fleet$setEffoProdAllLoa()

      my_project$getNnlsModel(specie = svalue(spe_drop), minobs = svalue(obs_spin), thr_r2 = svalue(thr_spin))

      svalue(tmp_txt) <- paste("\n\nRaw Scenarios:\n\n\t",
                               nrow(my_project$fleet$resNNLS[[svalue(spe_drop)]]$bmat),
                               "\n\nWith at least ", svalue(obs_spin), " observations:\n\n\t",
                               my_project$fleet$resNNLS[[svalue(spe_drop)]]$nSce,
                               "\n\nFitted:\n\n\t",
                               my_project$fleet$resNNLS[[svalue(spe_drop)]]$nfitted,
                               "   (",
                               round(100*my_project$fleet$resNNLS[[svalue(spe_drop)]]$nfitted/my_project$fleet$resNNLS[[svalue(spe_drop)]]$nSce),
                               "%)\n\n",
                               sep = "")
      # provie_drop[] <- names(my_project$fleet$effoProdMont)
      # prospe_drop[] <- sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))])
      my_project$fleet$plotNNLS(specie = svalue(spe_drop), thresR2 = svalue(thr_spin))
    })
    addSpace(up_fra, 20)
    obs_fra <- gframe(text = "Min Observations", container = up_fra, expand = TRUE, horizontal = TRUE)
    addSpace(obs_fra, 20)
    obs_spin <- gspinbutton(from = 1, to = 100,
                            by = 1, value = 10, container = obs_fra, expand = TRUE,
                            handler = function(...){

                            })
    addSpace(up_fra, 20)
    addSpace(obs_fra, 20)

    thr_fra <- gframe(text = "R2 Threshold", container = up_fra, expand = TRUE, horizontal = TRUE)
    addSpace(thr_fra, 20)
    thr_spin <- gslider(from = 0, to = 1,
                        by = 0.01, value = 0, container = thr_fra, expand = TRUE,
                        handler = function(...){

                        })
    addSpace(up_fra, 20)
    addSpace(thr_fra, 20)

    gbutton(text = "\n   Save   \n", container = up_fra, handler = function(...){
      svalue(set_lab) <- "Saved"
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, logi_sta_n)
    })

    addSpace(up_fra, 20)
    set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
    addSpring(set_gru_up)
    set_lab <- glabel(text = "Not Saved", container = set_gru_up)
    set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
    addSpring(set_gru_up)
    addSpace(up_fra, 20)

    # addSpring(up_fra)
    gbutton(text = " Close\nWindow", container = up_fra, handler = function(...){
      dispose(temp_dia)
    })
    addSpace(up_fra, 20)
    # addSpace(up_g, 20)
    bot_g <- ggroup(horizontal = TRUE, container = up_g)
    addSpace(bot_g, 10)
    bot_lef_g <- ggroup(horizontal = FALSE, container = bot_g)
    addSpring(bot_lef_g)
    tmp_txt <- gtext(text = NULL, width = 200, height = 350, container = bot_lef_g)
    addSpring(bot_lef_g)
    addSpace(bot_g, 10)
    nnls_gra <- ggraphics(width = 300, height = 400, container = bot_g, expand = TRUE)
    visible(temp_dia) <- TRUE
    addSpace(bot_g, 10)

    logi_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    logi_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    add(set_gru, logi_sta)

  })
  addSpring(pro_g_top2)
  addSpring(pro_g_top)

  pro_g_topBeta <- ggroup(horizontal = FALSE, container = pro_g_top)
  addSpring(pro_g_topBeta)
  gbutton("Tune\nBetas", container = pro_g_topBeta, handler = function(h,...){



    temp_dia <- gwindow(title="Set Max Beta", visible = FALSE,
                        parent = main_win,
                        width = 950, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia, expand = TRUE)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    # addSpring(up_fra)
    addSpace(up_fra, 20)
    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpring(spe_fra)
    glabel(text = svalue(spe_drop), container = spe_fra)
    # spe_drop <- gcombobox(sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))]), selected = 1,
    #                       editable = FALSE, container = spe_fra, expand = TRUE,
    #                       handler = function(...){
    #
    #                       })

    addSpring(spe_fra)
    addSpace(up_fra, 20)

    maxb_fra <- gframe(text = "Max Beta", container = up_fra, expand = TRUE, horizontal = TRUE)

    addSpace(maxb_fra, 20)
    maxb_spin <- gslider(from = 1, to = ceiling(max(my_project$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity)),
                         by = 1, value = 0, container = maxb_fra, expand = TRUE,
                         handler = function(...){
                           Sys.sleep(5)
                           print(ggplot_betasBoxplot(df_YearFGprod = my_project$fleet$betaMeltYear[[svalue(spe_drop)]], int_hline = svalue(maxb_spin)))
                         })
    addSpace(up_fra, 20)
    addSpace(maxb_fra, 20)

    gbutton(text = "\n   Save   \n", container = up_fra, handler = function(...){

      my_project$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity[my_project$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity > svalue(maxb_spin)] <- svalue(maxb_spin)

      svalue(set_lab) <- "Saved"
      delete(set_gru, set_gru$children[[length(set_gru$children)]])
      add(set_gru, logi_sta_n)

      print(ggplot_betasBoxplot(df_YearFGprod = my_project$fleet$betaMeltYear[[svalue(spe_drop)]], int_hline = svalue(maxb_spin)))

    })

    addSpace(up_fra, 20)
    set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
    addSpring(set_gru_up)
    set_lab <- glabel(text = "Not Saved", container = set_gru_up)
    set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
    addSpring(set_gru_up)
    addSpace(up_fra, 20)

    # addSpring(up_fra)
    gbutton(text = " Close\nWindow", container = up_fra, handler = function(...){
      dispose(temp_dia)
    })
    addSpace(up_fra, 20)
    # addSpace(up_g, 20)
    bot_g <- ggroup(horizontal = TRUE, container = up_g)

    nnls_gra <- ggraphics(width = 300, height = 400, container = bot_g, expand = TRUE)
    visible(temp_dia) <- TRUE
    addSpace(bot_g, 10)

    logi_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    logi_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    add(set_gru, logi_sta)

    print(ggplot_betasBoxplot(df_YearFGprod = my_project$fleet$betaMeltYear[[svalue(spe_drop)]], int_hline = max(my_project$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity)))

  })
  addSpring(pro_g_topBeta)
  addSpring(pro_g_top)

  pro_g_top3 <- ggroup(horizontal = FALSE, container = pro_g_top)
  addSpring(pro_g_top3)
  gbutton("Predict\nProduction", container = pro_g_top3, handler = function(h,...){
    enabled(pro_g_top) <- FALSE
    Sys.sleep(1)
    my_project$fleet$setEffoMont()
    my_project$fleet$setEffoAll()
    my_project$fleet$setEffoAllLoa()
    my_project$predictProduction(svalue(spe_drop))
    my_project$fleet$setProdMeltYear(svalue(spe_drop))
    enabled(pro_g_top) <- TRUE
  })
  addSpring(pro_g_top3)

  addSpring(pro_g_top)
  pro_g_top2_view_g <- ggroup(horizontal = FALSE, container = pro_g_top, expand = TRUE)
  pro_g_top2_view <- gframe(text = "View", horizontal = TRUE, container = pro_g_top2_view_g, expand = TRUE)
  addSpring(pro_g_top2_view)
  provie_drop <- gcombobox(items = "Year", selected = 1, container = pro_g_top2_view,
                           expand = TRUE, editable = FALSE)
  addSpring(pro_g_top2_view)
  pro_g_top2_ver <- ggroup(horizontal = FALSE, container = pro_g_top2_view)
  addSpring(pro_g_top2_ver)
  gbutton("Betas", container = pro_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting Betas..."
    Sys.sleep(1)
    my_project$setPlotBetaMeltYear(specie = svalue(spe_drop), year = svalue(provie_drop))
    suppressWarnings(grid.arrange(my_project$sampMap$ggBetaFGmap,
                                  my_project$sampMap$ggBetaFGbox,
                                  layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))
    svalue(stat_bar) <- ""
  })
  addSpring(pro_g_top2_ver)
  gbutton("Production", container = pro_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting production..."
    Sys.sleep(1)
    my_project$setPlotProdMeltYear(specie = svalue(spe_drop), year = svalue(provie_drop))
    suppressWarnings(grid.arrange(my_project$sampMap$ggProdFGmap,
                                  my_project$sampMap$ggProdFGbox,
                                  layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))

    svalue(stat_bar) <- ""
  })
  addSpring(pro_g_top2_ver)
  gbutton("Total\nProduction", container = pro_g_top2_view, handler = function(h,...){
    my_project$fleet$plotTotProd(specie = svalue(spe_drop))
  })
  addSpring(pro_g_top2_ver)
  addSpring(pro_g_top2_view)
  addSpace(pro_g_top2_view_g, 7)
  addSpring(pro_g_top)
  proGro_p <- ggraphics(container = pro_g, width = 550, height = 250, expand = TRUE)


  ####     Survey     ####

  raw_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Survey")
  raw_g_top <- gframe(horizontal = TRUE, container = raw_g)
  addSpace(raw_g_top, 2)
  addSpring(raw_g_top)
  raw_g_top1 <- ggroup(horizontal = FALSE, container = raw_g_top)
  addSpring(raw_g_top1)
  gbutton("Load Sample", container = raw_g_top1, handler = function(h,...){
    svalue(stat_bar) <- "Loading Data..."
    Sys.sleep(1)
    my_project$loadSurveyLFD(csv_path = pathSurvey)

    if(!is.null(my_project$rawDataSurvey)){ #update_pop_gui()

      raw_t[] <- my_project$rawDataSurvey[sample(1:nrow(my_project$rawDataSurvey), 100, replace = FALSE),]
      svalue(raw_l1) <- paste("Specie: ", paste(my_project$specieInSurvey, collapse = " - "))
      #   svalue(raw_l2) <- paste("Length Classes: from ",  min(my_project$LClass), " to ", max(my_project$LClass))
      svalue(raw_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInSurvey))), " to ", max(as.numeric(as.character(my_project$yearInSurvey))))
      # spec_drop[] <- my_project$specieInSurvey
      spec_drop_mix[] <- my_project$specieInSurvey
      # spevie_drop[] <- c("All", my_project$specieInSurvey)
      # cohSpe_drop[] <- my_project$specieInSurvey
      # svalue(spec_drop) <- my_project$specieInSurvey[1]
      # svalue(cohSpe_drop) <- my_project$specieInSurvey[1]
      # svalue(spevie_drop) <- "All"
      svalue(spec_drop_mix) <- my_project$specieInSurvey[1]
      # year_drop[] <- c("All", as.character(my_project$yearInSurvey))
      # cohYea_drop[] <- c("All", as.character(my_project$yearInSurvey))
      # svalue(year_drop) <- my_project$yearInSurvey[1]
      # svalue(cohYea_drop) <- "All"

      # Update LWrel specie selection
      # assSpe_drop[] <- my_project$specieInSurvey
      # svalue(assSpe_drop) <- my_project$specieInSurvey[1]

      if(!is.null(my_project$sampMap$cutResShp)){
        my_project$addFg2Survey()
        my_project$setSpreaSurvey()
        my_project$setSpatSurvey()

        my_project$sampMap$set_ggMapFgSurvey(my_project$rawDataSurvey)
      }

      ### Update Sampling Status
      svalue(n_year_s) <- paste(length(my_project$yearInSurvey), " years", sep = "")
      svalue(mi_date_s) <- paste("From: ", min(as.numeric(as.character(my_project$yearInSurvey))), sep = "")
      svalue(ma_date_s) <- paste("To: ", max(as.numeric(as.character(my_project$yearInSurvey))), sep = "")
      svalue(n_spec_s) <- paste(length(my_project$specieInSurvey),
                                ifelse(length(my_project$specieInSurvey) == 1, " specie", " species"), sep = "")

      delete(samp_g, samp_g$children[[length(samp_g$children)]])
      add(samp_g, samp_sta_n)
    }
  })
  addSpring(raw_g_top1)
  addSpring(raw_g_top)

  gbutton("Open\nLFD\nViewer", container = raw_g_top, handler = function(h,...){
    
    temp_dia <- gwindow(title="Survey Length Frequency Distribution Viewer", visible = FALSE,
                        parent = main_win, width = 800, height = 500)
    
    pop_g <- ggroup(horizontal = FALSE, container = temp_dia, label = "Population")
    pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
    # addSpring(pop_g_top)
    lfdfra_g <- gframe("LFD data", horizontal = TRUE, container = pop_g_top, expand = TRUE)
    addSpring(lfdfra_g)
    
    spec_b <- gframe("Specie", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
    addSpring(lfdfra_g)
    addSpring(spec_b)
    spec_drop <- gcombobox(items = my_project$specieInSurvey,
                           selected = 1, container = spec_b, expand = TRUE,
                           editable = FALSE, handler = function(h,...){
                             
                             spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))
                             svalue(sex_drop) <- "Female"
                             suppressWarnings(grid.arrange(my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                                           layout_matrix = rbind(c(1,1,1,3),
                                                                                 c(2,2,2,4),
                                                                                 c(2,2,2,4))))
                           })
    addSpring(spec_b)
    sex_b <- gframe("Sex", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
    addSpring(lfdfra_g)
    addSpring(sex_b)
    sex_drop <- gcombobox(items = c("Female", "Male", "Unsex"),
                          selected = 1, container = sex_b, expand = TRUE,
                          editable = FALSE, handler = function(h,...){
                            spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))
                            
                            suppressWarnings(grid.arrange(my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                                          layout_matrix = rbind(c(1,1,1,3),
                                                                                c(2,2,2,4),
                                                                                c(2,2,2,4))))
                          })
    addSpring(sex_b)
    
    addSpring(lfdfra_g)
    addSpace(pop_g_top, 2)
    pop_p <- ggraphics(container = pop_g, width = 750, height = 450, expand = TRUE)
    addSpring(pop_g_top)
    
    gbutton("Close", container = pop_g_top, handler = function(h,...){
      dispose(temp_dia)
    })
    
    addSpring(pop_g_top)
    
    visible(temp_dia) <- TRUE
    spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))
    
    suppressWarnings(grid.arrange(my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                  layout_matrix = rbind(c(1,1,1,3),
                                                        c(2,2,2,4),
                                                        c(2,2,2,4))))
  })

  addSpring(raw_g_top)

  gbutton("View\nSpatial\nDistribution", container = raw_g_top, handler = function(h,...){

    temp_dia <- gwindow(title="Spatial Distribution of Survey sampling", visible = FALSE,
                        parent = main_win, width = 700, height = 500)

    pop_g <- ggroup(horizontal = FALSE, container = temp_dia)

    pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)

    addSpring(pop_g_top)
    spec_b <- gframe("Specie", horizontal = FALSE, container = pop_g_top, expand = TRUE)

    addSpring(spec_b)
    spec_drop <- gcombobox(items = as.character(my_project$specieInSurvey), selected = 1,
                           container = spec_b, editable = FALSE, handler = function(h,...){
                             # my_project$plotGooSpe(whiSpe = svalue(spec_drop), whiSou = "Fishery")
                             spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))

                             suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgSurvey,
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                           my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                           layout_matrix = rbind(c(NA,1,1),
                                                                                 c(4,1,1),
                                                                                 c(NA,2,3))
                             ))
                           })
    addSpring(spec_b)
    sex_b <- gframe("Sex", horizontal = FALSE, container = pop_g_top, expand = TRUE)
    addSpring(pop_g_top)
    addSpring(sex_b)
    sex_drop <- gcombobox(items = c("Female", "Male", "Unsex"),
                          selected = 1, container = sex_b, expand = TRUE,
                          editable = FALSE, handler = function(h,...){
                            spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))

                            suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgSurvey,
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                          my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                          layout_matrix = rbind(c(NA,1,1),
                                                                                c(4,1,1),
                                                                                c(NA,2,3))
                            ))

                          })
    addSpring(sex_b)
    addSpring(pop_g_top)

    gbutton("Close", container = pop_g_top, handler = function(h,...){
      dispose(temp_dia)
    })
    addSpring(pop_g_top)

    addSpace(pop_g_top, 10)

    pop_p <- ggraphics(container = pop_g, width = 650, height = 450, expand = TRUE)

    visible(temp_dia) <- TRUE

    spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))

    # my_project$plotGooSpe(whiSpe = "All", whiSou = "Fishery")
    suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgSurvey,
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                  my_project$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                  layout_matrix = rbind(c(NA,1,1),
                                                        c(4,1,1),
                                                        c(NA,2,3))
    ))
  })

  addSpring(raw_g_top)

  gbutton("   Get\nMEDITS index", container = raw_g_top, handler = function(h,...){

    strataVect <- c(0, 10, 50, 100, 200, 500, 800, Inf)
    icoStrata_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    icoStrata_on <- gimage(system.file("ico/user-available.png", package="smartR"))
    icoArea_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    icoArea_on <- gimage(system.file("ico/user-available.png", package="smartR"))
    icoMedit_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
    icoMedit_on <- gimage(system.file("ico/user-available.png", package="smartR"))

    temp_dia <- gwindow(title="Medits index calculator", visible = FALSE,
                        parent = main_win,
                        width = 900, height = 500)

    med_g <- ggroup(horizontal = FALSE, container = temp_dia)
    med_g_top <- gframe(horizontal = TRUE, container = med_g, spacing = 10)

    addSpace(med_g_top, 10)

    comfra_g <- gframe("Compute", horizontal = TRUE, container = med_g_top, expand = TRUE)
    addSpring(comfra_g)

    strata_f <- gframe("Strata", horizontal = FALSE, container = comfra_g, expand = TRUE)
    depth_b <- gbutton(text = "Set", container = strata_f, handler = function(h,...){
      my_project$setDepthSurvey()
      my_project$setStratumSurvey(vectorStrata = strataVect)
      delete(strata_f, strata_f$children[[length(strata_f$children)]])
      add(strata_f, icoStrata_on)
    })
    add(strata_f, icoStrata_off)

    addSpring(comfra_g)

    areas_f <- gframe("Areas", horizontal = FALSE, container = comfra_g, expand = TRUE)
    area_b <- gbutton(text = "Set", container = areas_f, handler = function(h,...){
      my_project$sampMap$setAreaGrid()
      my_project$sampMap$setAreaStrata(vectorStrata = strataVect)
      my_project$sampMap$setWeightStrata()
      delete(areas_f, areas_f$children[[length(areas_f$children)]])
      add(areas_f, icoArea_on)
    })
    add(areas_f, icoArea_off)

    addSpring(comfra_g)

    medInd_f <- gframe("MEDITS Index", horizontal = FALSE, container = comfra_g, expand = TRUE)
    medInd_b <- gbutton(text = "Set", container = medInd_f, handler = function(h,...){
      my_project$setAbuAvgAll()
      my_project$setStrataAbu()
      my_project$setMeditsIndex()
      delete(medInd_f, medInd_f$children[[length(medInd_f$children)]])
      add(medInd_f, icoMedit_on)

      specie_ind <- 1
      sex_sel <- "Female"
      tmp_abus <- data.frame(Class = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Class,
                             Stratum = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
                             Year = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Year)
      if(sex_sel == "Female"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
      }else if(sex_sel == "Male"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
      }else if(sex_sel == "Unsex"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }else if(sex_sel == "All"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }
      tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
      print(ggplot_meditsIndex(inMedits = tmp_abus))
    })
    add(medInd_f, icoMedit_off)

    addSpring(comfra_g)
    addSpring(med_g_top)
    plofra_g <- gframe("Show", horizontal = TRUE, container = med_g_top, expand = TRUE)
    addSpace(plofra_g, 10)
    specie_drop <- gdroplist(items = my_project$specieInSurvey, selected = 1, editable = FALSE, container = plofra_g, expand = TRUE, handler = function(...){
      specie_ind <- which(my_project$specieInSurvey == svalue(specie_drop))
      sex_sel <- svalue(sex_drop)
      tmp_abus <- data.frame(Class = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Class,
                             Stratum = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
                             Year = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Year)
      if(sex_sel == "Female"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
      }else if(sex_sel == "Male"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
      }else if(sex_sel == "Unsex"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }else if(sex_sel == "All"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }
      tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
      print(ggplot_meditsIndex(inMedits = tmp_abus))
    })
    addSpace(plofra_g, 10)
    sex_drop <- gdroplist(items = c("Female", "Male", "Unsex", "All"), selected = 1, editable = FALSE, container = plofra_g, expand = TRUE, handler = function(...){
      specie_ind <- which(my_project$specieInSurvey == svalue(specie_drop))
      sex_sel <- svalue(sex_drop)
      tmp_abus <- data.frame(Class = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Class,
                             Stratum = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
                             Year = my_project$surveyBySpecie[[specie_ind]]$abuAvg$Year)
      if(sex_sel == "Female"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
      }else if(sex_sel == "Male"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
      }else if(sex_sel == "Unsex"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }else if(sex_sel == "All"){
        tmp_abus$Index <- my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + my_project$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
      }
      tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
      print(ggplot_meditsIndex(inMedits = tmp_abus))
    })
    addSpace(plofra_g, 10)
    addSpring(med_g_top)
    pop_p <- ggraphics(container = med_g, width = 550, height = 250, expand = TRUE)
    gbutton("Close", container = med_g_top, handler = function(h,...){
      dispose(temp_dia)
    })
    addSpace(med_g_top, 10)
    visible(temp_dia) <- TRUE
  })

  addSpring(raw_g_top)

  raw_g_top2 <- ggroup(horizontal = FALSE, container = raw_g_top)
  raw_l1 <- glabel("Specie: ", container = raw_g_top2)
  raw_l3 <- glabel("Years: ", container = raw_g_top2)
  addSpring(raw_g_top)
  addSpace(raw_g_top, 2)
  addSpace(raw_g_top2, 2)

  blankDF = data.frame(Specie = character(0), Lat = numeric(0), Lon = numeric(0), Year = character(0), Class = numeric(0), Female = character(0), Male = character(0), Unsex = character(0), stringsAsFactors=FALSE)
  raw_t <- gtable(blankDF, container = raw_g, expand = TRUE)


  ####     Fishery     ####

  fis_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Fishery")
  fis_g_top <- gframe(horizontal = TRUE, container = fis_g)
  addSpace(fis_g_top, 2)
  addSpring(fis_g_top)
  fis_g_top1 <- ggroup(horizontal = FALSE, container = fis_g_top)
  addSpring(fis_g_top1)
  gbutton("Load Sample", container = fis_g_top1, handler = function(h,...){
    svalue(stat_bar) <- "Loading Data..."
    Sys.sleep(1)
    my_project$loadFisheryLFD(csv_path = pathFishery)

    if(!is.null(my_project$sampMap$cutResShp)){
      my_project$addFg2Fishery()
      my_project$setSpreaFishery()
      my_project$setSpatFishery()

      my_project$sampMap$set_ggMapFgFishery(my_project$rawDataFishery)
    }

    if(!is.null(my_project$rawDataFishery)){ #update_pop_gui()

      fis_t[] <- my_project$rawDataFishery[sample(1:nrow(my_project$rawDataFishery), 100, replace = FALSE),]
      svalue(fis_l1) <- paste("Specie: ", paste(my_project$specieInFishery, collapse = " - "))
      svalue(fis_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInFishery))), " to ", max(as.numeric(as.character(my_project$yearInFishery))))

      # Update LWrel specie selection
      # assSpe_drop[] <- my_project$specieInFishery
      # svalue(assSpe_drop) <- my_project$specieInFishery[1]

      svalue(stat_bar) <- ""

      # ### Update Sampling Status
      # svalue(n_year_s) <- paste(length(my_project$yearInFishery), " years", sep = "")
      # svalue(mi_date_s) <- paste("From: ", min(as.numeric(as.character(my_project$yearInFishery))), sep = "")
      # svalue(ma_date_s) <- paste("To: ", max(as.numeric(as.character(my_project$yearInFishery))), sep = "")
      # svalue(n_spec_s) <- paste(length(my_project$specieInFishery),
      #                           ifelse(length(my_project$specieInFishery) == 1, " specie", " species"), sep = "")
      #
      # delete(samp_g, samp_g$children[[length(samp_g$children)]])
      # add(samp_g, samp_sta_n)
    }
  })
  addSpring(fis_g_top1)

  addSpring(fis_g_top)
  fis_g_top2 <- ggroup(horizontal = FALSE, container = fis_g_top)
  addSpring(fis_g_top2)
  fis_l1 <- glabel("Specie: ", container = fis_g_top2)
  addSpace(fis_g_top2, 2)
  fis_l3 <- glabel("Years: ", container = fis_g_top2)
  addSpring(fis_g_top2)
  addSpring(fis_g_top)
  addSpace(fis_g_top, 2)

  gbutton("Open\nLFD\nViewer", container = fis_g_top, handler = function(h,...){

    temp_dia <- gwindow(title="Fishery Length Frequency Distribution Viewer", visible = FALSE,
                        parent = main_win, width = 800, height = 500)

    pop_g <- ggroup(horizontal = FALSE, container = temp_dia, label = "Population")
    pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
    # addSpring(pop_g_top)
    lfdfra_g <- gframe("LFD data", horizontal = TRUE, container = pop_g_top, expand = TRUE)
    addSpring(lfdfra_g)

    spec_b <- gframe("Specie", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
    addSpring(lfdfra_g)
    addSpring(spec_b)
    spec_drop <- gcombobox(items = my_project$specieInFishery,
                           selected = 1, container = spec_b, expand = TRUE,
                           editable = FALSE, handler = function(h,...){

                             spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))
                             svalue(sex_drop) <- "Female"
                             suppressWarnings(grid.arrange(my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                                           layout_matrix = rbind(c(1,1,1,3),
                                                                                 c(2,2,2,4),
                                                                                 c(2,2,2,4))))
                           })
    addSpring(spec_b)
    sex_b <- gframe("Sex", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
    addSpring(lfdfra_g)
    addSpring(sex_b)
    sex_drop <- gcombobox(items = c("Female", "Male", "Unsex"),
                          selected = 1, container = sex_b, expand = TRUE,
                          editable = FALSE, handler = function(h,...){
                            spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))

                            suppressWarnings(grid.arrange(my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                                          layout_matrix = rbind(c(1,1,1,3),
                                                                                c(2,2,2,4),
                                                                                c(2,2,2,4))))
                          })
    addSpring(sex_b)

    addSpring(lfdfra_g)
    addSpace(pop_g_top, 2)
    pop_p <- ggraphics(container = pop_g, width = 750, height = 450, expand = TRUE)
    addSpring(pop_g_top)

    gbutton("Close", container = pop_g_top, handler = function(h,...){
      dispose(temp_dia)
    })

    addSpring(pop_g_top)

    visible(temp_dia) <- TRUE
    spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))

    suppressWarnings(grid.arrange(my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                  layout_matrix = rbind(c(1,1,1,3),
                                                        c(2,2,2,4),
                                                        c(2,2,2,4))))
  })

  addSpring(fis_g_top)

  gbutton("View\nSpatial\nDistribution", container = fis_g_top, handler = function(h,...){

    temp_dia <- gwindow(title="Spatial Distribution of Fishery sampling", visible = FALSE,
                        parent = main_win, width = 700, height = 500)

    pop_g <- ggroup(horizontal = FALSE, container = temp_dia)

    pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)

    addSpring(pop_g_top)
    spec_b <- gframe("Specie", horizontal = FALSE, container = pop_g_top, expand = TRUE)

    addSpring(spec_b)
    spec_drop <- gcombobox(items = as.character(my_project$specieInFishery), selected = 1,
                           container = spec_b, editable = FALSE, handler = function(h,...){
                             # my_project$plotGooSpe(whiSpe = svalue(spec_drop), whiSou = "Fishery")
                             spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))

                             suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgFishery,
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                           my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                           layout_matrix = rbind(c(NA,1,1),
                                                                                 c(4,1,1),
                                                                                 c(NA,2,3))
                             ))
                           })
    addSpring(spec_b)
    sex_b <- gframe("Sex", horizontal = FALSE, container = pop_g_top, expand = TRUE)
    addSpring(pop_g_top)
    addSpring(sex_b)
    sex_drop <- gcombobox(items = c("Female", "Male", "Unsex"),
                          selected = 1, container = sex_b, expand = TRUE,
                          editable = FALSE, handler = function(h,...){
                            spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))

                            suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgFishery,
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                          my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                          layout_matrix = rbind(c(NA,1,1),
                                                                                c(4,1,1),
                                                                                c(NA,2,3))
                            ))

                          })
    addSpring(sex_b)
    addSpring(pop_g_top)

    gbutton("Close", container = pop_g_top, handler = function(h,...){
      dispose(temp_dia)
    })
    addSpring(pop_g_top)

    addSpace(pop_g_top, 10)

    pop_p <- ggraphics(container = pop_g, width = 650, height = 450, expand = TRUE)

    visible(temp_dia) <- TRUE

    spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))

    # my_project$plotGooSpe(whiSpe = "All", whiSou = "Fishery")
    suppressWarnings(grid.arrange(my_project$sampMap$ggMapFgFishery,
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                  my_project$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                  layout_matrix = rbind(c(NA,1,1),
                                                        c(4,1,1),
                                                        c(NA,2,3))
    ))
  })

  addSpring(fis_g_top)

  gbutton("Assign FG", container = fis_g_top, handler = function(h,...){
    if(!is.null(my_project$sampMap$cutResShp)){
      my_project$addFg2Fishery()
    }
    fis_t[] <- my_project$rawDataFishery[sample(1:nrow(my_project$rawDataFishery), 100, replace = FALSE),]
  })
  addSpring(fis_g_top)

  blankDF = data.frame(Specie = character(0),
                       Lat = numeric(0),
                       Lon = numeric(0),
                       Date = character(0),
                       Length = numeric(0),
                       Female = character(0),
                       Male = character(0),
                       Unsex = character(0), stringsAsFactors=FALSE)
  fis_t <- gtable(blankDF, container = fis_g, expand = TRUE)


  ####   Mixture   ####

  mix_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Mixture")
  mix_g_top <- gframe(horizontal = TRUE, container = mix_g) #if expand bad
  addSpace(mix_g_top, 20)
  cont_g <- gframe("Mixture Analysis", horizontal = TRUE, container = mix_g_top, expand = TRUE)
  addSpring(cont_g)
  sourceMix_r <- gradio(items = c("Survey", "Fishery"), horizontal = FALSE, container = cont_g, expand = TRUE, handler = function(...){
    if(svalue(sourceMix_r) == "Survey"){
      if(is.null(my_project$specieInSurvey)){
        spec_drop_mix[] <- "No data"
        svalue(spec_drop_mix) <- "No data"
      }else{
        spec_drop_mix[] <- my_project$specieInSurvey
        svalue(spec_drop_mix) <- my_project$specieInSurvey[1]
      }
    }else{
      if(is.null(my_project$specieInFishery)){
        spec_drop_mix[] <- "No data"
        svalue(spec_drop_mix) <- "No data"
      }else{
        spec_drop_mix[] <- my_project$specieInFishery
        svalue(spec_drop_mix) <- my_project$specieInFishery[1]
      }
    }
  })
  spec_mix_f <- gframe("Specie and Sex", horizontal = FALSE, container = cont_g, expand = TRUE)
  addSpring(spec_mix_f)
  spec_drop_mix <- gcombobox(items = "Specie", selected = 1, container = spec_mix_f, editable = FALSE, expand = TRUE)
  addSpring(spec_mix_f)
  sex_drop_mix <- gcombobox(items = c("Female", "Male", "Unsex"), selected = 1, container = spec_mix_f, editable = FALSE, expand = TRUE)
  addSpring(spec_mix_f)
  addSpring(cont_g)
  ncoh_f <- gframe("N. cohorts", horizontal = FALSE, container = cont_g)
  addSpring(ncoh_f)
  ncih_sb <- gspinbutton (from = 1, to = 100, by = 1, value = 3, digits = 0, container = ncoh_f)
  addSpring(ncoh_f)

  addSpring(cont_g)

  gcurv_f <- gframe("Growth Curve", horizontal = FALSE, container = cont_g)
  addSpring(gcurv_f)
  gcurv_r <- gradio(items = c("von Bertalanffy", "Gompertz"), horizontal = FALSE, container = gcurv_f, expand = TRUE)
  addSpring(gcurv_f)

  addSpring(cont_g)

  go_g <- gframe("MCMC sim", horizontal = TRUE, container = cont_g, expand = TRUE)
  addSpring(go_g)
  go_g_ada <- ggroup(horizontal = FALSE, container = go_g, expand = TRUE)
  addSpring(go_g_ada)
  glabel("N. adapt: ", container = go_g_ada)
  mc_niter <- gcombobox(c(100, 1000, 5000, 10000), selected = 2, container = go_g_ada, editable = FALSE, expand = TRUE)
  addSpring(go_g_ada)
  go_g_sam <- ggroup(horizontal = FALSE, container = go_g)
  addSpring(go_g_sam)
  glabel("Sample size: ", container = go_g_sam)
  mc_nsamp <- gcombobox(c(100, 1000, 5000, 10000), selected = 2, container = go_g_sam, editable = FALSE, expand = TRUE)
  addSpring(go_g_sam)
  addSpring(go_g)
  go_b <- gbutton ("   GO   ", container = go_g,
                   handler = function(h,...){
                     dev.set(dev.list()[pre_dev+6])
                     svalue(view_radio) <- "MCMC"
                     if(svalue(sourceMix_r) == "Survey"){
                       pre_mfrow <- par(c("mfrow", "mar"))
                       par(mfrow = c(2, 1))
                       par(mar = c(2,2,1.5,0.5))

                       ind_spe <- which(my_project$specieInSurvey == svalue(spec_drop_mix))
                       # Set number of cohorts
                       my_project$surveyBySpecie[[ind_spe]]$setNCoho(as.numeric(svalue(ncih_sb)))
                       # Compute mixture
                       # my_project$surveyBySpecie[[ind_spe]]$calcMix(nAdap = as.numeric(svalue(mc_niter)), nSamp = as.numeric(svalue(mc_nsamp)))
                       my_project$surveyBySpecie[[ind_spe]]$calcMixDate(nAdap = as.numeric(svalue(mc_niter)),
                                                                        nSamp = as.numeric(svalue(mc_nsamp)),
                                                                        sexDrop = svalue(sex_drop_mix),
                                                                        curveSel = svalue(gcurv_r))

                       my_project$surveyBySpecie[[ind_spe]]$ggplotMcmcOut(selSex = svalue(sex_drop_mix))
                       # Transform length to cohorts
                       # my_project$calcCoh_A_Survey(ind_spe)
                       # Interpolate cohorts
                       # my_project$intrpCoh_A_Survey(ind_spe)
                       cohCoh_drop[] <- c("All", seq(1, my_project$surveyBySpecie[[ind_spe]]$nCoho, by = 1))

                     }else{
                       pre_mfrow <- par(c("mfrow", "mar"))
                       par(mfrow = c(2, 4))
                       par(mar = c(2,2,1.5,0.5))

                       ind_spe <- which(my_project$specieInFishery == svalue(spec_drop_mix))
                       # Set number of cohorts
                       my_project$fisheryBySpecie[[ind_spe]]$setNCoho(as.numeric(svalue(ncih_sb)))
                       # Compute mixture
                       my_project$fisheryBySpecie[[ind_spe]]$calcMixDate(nAdap = as.numeric(svalue(mc_niter)),
                                                                         nSamp = as.numeric(svalue(mc_nsamp)),
                                                                         sexDrop = svalue(sex_drop_mix),
                                                                         curveSel = svalue(gcurv_r))

                       my_project$fisheryBySpecie[[ind_spe]]$ggplotMcmcOut(selSex = svalue(sex_drop_mix))

                       # Transform length to cohorts
                       # my_project$calcCoh_A_Fishery(ind_spe)
                       # Interpolate cohorts
                       # my_project$intrpCoh_A_Fishery(ind_spe)
                       # cohCoh_drop[] <- c("All", seq(1, my_project$fisheryBySpecie[[ind_spe]]$nCoho, by = 1))

                     }
                     # svalue(cohCoh_drop) <- "All"
                     par(pre_mfrow)
                   })
  addSpring(go_g)
  # save_b <- gbutton ("  SAVE  ", container = go_g)
  addSpring(cont_g)
  view_g <- gframe("View", horizontal = TRUE, container = cont_g, expand = TRUE)
  view_radio <- gradio(c("MCMC", "Key", "Birth"), selected = 1,
                       horizontal = FALSE, container = view_g,
                       handler = function(h,...){
                         dev.set(dev.list()[pre_dev+6])

                         if(svalue(sourceMix_r) == "Survey"){
                           my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_mix))]]$ggplotMcmcOut(selCompo = svalue(view_radio),
                                                                                                                                selSex = svalue(sex_drop_mix))
                         }else{
                           my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(spec_drop_mix))]]$ggplotMcmcOut(selCompo = svalue(view_radio),
                                                                                                                                  selSex = svalue(sex_drop_mix))
                         }
                       })

  addSpring(cont_g)
  addSpace(mix_g_top, 20)

  mix_p <- ggraphics(container = mix_g, width = 550, height = 250, expand = TRUE)


  ####   Cohorts   ####

  cohoP_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Cohorts")
  cohoP_g_top <- gframe(horizontal = TRUE, container = cohoP_g, spacing = 10)
  addSpace(cohoP_g_top, 2)
  # addSpring(cohoP_g_top)

  cohofra_g <- gframe("Cohort data", horizontal = TRUE, container = cohoP_g_top, expand = TRUE)
  # addSpring(cohofra_g)

  sourceCoh_r <- gradio(items = c("Survey", "Fishery"), horizontal = FALSE, container = cohofra_g, expand = TRUE, handler = function(...){
    if(svalue(sourceCoh_r) == "Survey"){
      if(is.null(my_project$specieInSurvey)){
        spec_drop_coh[] <- "No data"
        svalue(spec_drop_coh) <- "No data"
      }else{
        spec_drop_coh[] <- my_project$specieInSurvey
        svalue(spec_drop_coh) <- my_project$specieInSurvey[1]
      }
    }else{
      if(is.null(my_project$specieInFishery)){
        spec_drop_coh[] <- "No data"
        svalue(spec_drop_coh) <- "No data"
      }else{
        spec_drop_coh[] <- my_project$specieInFishery
        svalue(spec_drop_coh) <- my_project$specieInFishery[1]
      }
    }
  })

  cohSpe_b <- gframe("Specie", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohSpe_b)
  spec_drop_coh <- gcombobox(items = "Specie", selected = 1, container = cohSpe_b, editable = FALSE, handler = function(h,...){
    svalue(sexRadio_coh) <- "Female"
    svalue(gruRadio_coh) <- "Age"
    svalue(cohTyp_drop) <- "LFD"
    if(svalue(sourceCoh_r) == "Survey"){
      cohCoh_drop[] <- sort(unique(my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
    }else{
      cohCoh_drop[] <- sort(unique(my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
    }
    svalue(cohCoh_drop, index = TRUE) <- 1
  })
  addSpring(cohSpe_b)

  cohSex_b <- gframe("Sex", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohSex_b)
  sexRadio_coh <- gradio(items = c("Female", "Male", "Unsex"), selected = 1, container = cohSex_b, expand = TRUE, handler = function(h,...){
    if(svalue(sourceCoh_r) == "Survey"){
      cohCoh_drop[] <- sort(unique(my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
    }else{
      cohCoh_drop[] <- sort(unique(my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
    }
  })
  addSpring(cohSex_b)

  cohGru_b <- gframe("Group", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohGru_b)
  gruRadio_coh <- gradio(items = c("Age", "Birth"), selected = 1, container = cohGru_b, expand = TRUE, handler = function(h,...){
    if(svalue(gruRadio_coh) == "Age"){
      if(svalue(sourceCoh_r) == "Survey"){
        cohCoh_drop[] <- sort(unique(my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
      }else{
        cohCoh_drop[] <- sort(unique(my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Age))
      }
    }else{
      if(svalue(sourceCoh_r) == "Survey"){
        cohCoh_drop[] <- sort(unique(my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Birth))
      }else{
        cohCoh_drop[] <- sort(unique(my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(spec_drop_coh))]]$groMixout[[svalue(sexRadio_coh)]]$Birth))
      }
    }
  })
  addSpring(cohGru_b)

  cohCoh_b <- gframe("Cohort", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohCoh_b)
  cohCoh_drop <- gcombobox(items = "Cohort", selected = 1, container = cohCoh_b, editable = FALSE, handler = function(h,...){
    # svalue(sourceCoh_r)   # source = "Survey"
    # svalue(spec_drop_coh) # specie = 1
    # svalue(sexRadio_coh)     # sex = "Female"
    # svalue(gruRadio_coh)   # group = "Age"
    # svalue(cohTyp_drop)    #  type = "LFD"
    dev.set(dev.list()[pre_dev+7])

    if(svalue(sourceCoh_r) == "Survey"){
      cohSpe_ind <- which(my_project$specieInSurvey == svalue(spec_drop_coh))
      if(svalue(cohTyp_drop) == "LFD"){

      }else{

      }
    }else{
      cohSpe_ind <- which(my_project$specieInFishery == svalue(spec_drop_coh))
      if(svalue(cohTyp_drop) == "LFD"){
        if(svalue(gruRadio_coh) == "Age"){
          tmpLFD <- my_project$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]][my_project$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Age == svalue(cohCoh_drop),]
          tmpLFD$UTC <- tmpLFD$Date
          tmp_palette <- rainbow(max(cohCoh_drop[])+1)
          suppressWarnings(grid.arrange(set_ggHistLfdTot(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)+1]),
                                        set_ggHistUtcLfd(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)+1]),
                                        set_ggHistUtcTot(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)+1]),
                                        set_ggDotUtcSplit(inLfd = tmpLFD) + scale_color_manual(values = tmp_palette[svalue(cohCoh_drop)+1]),
                                        layout_matrix = rbind(c(1,1,1,3),
                                                              c(2,2,2,4),
                                                              c(2,2,2,4))))
        }else{
          tmpLFD <- my_project$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]][my_project$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Birth == svalue(cohCoh_drop),]
          tmpLFD$UTC <- tmpLFD$Date
          tmpLFD$Month <- factor(tmpLFD$MonthChar, levels = month.abb)

          tmp_palette <- brewer.pal(12, "Paired")
          suppressWarnings(grid.arrange(set_ggHistLfdTot(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)-min(cohCoh_drop[])]),
                                        set_ggHistUtcLfd(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)-min(cohCoh_drop[])]),
                                        set_ggHistUtcTot(inLfd = tmpLFD) + scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop)-min(cohCoh_drop[])]),
                                        set_ggDotUtcSplit(inLfd = tmpLFD) + scale_color_manual(values = tmp_palette[svalue(cohCoh_drop)-min(cohCoh_drop[])]),
                                        layout_matrix = rbind(c(1,1,1,3),
                                                              c(2,2,2,4),
                                                              c(2,2,2,4))))
        }
      }else{

      }
    }
  })
  addSpring(cohCoh_b)

  cohTyp_b <- gframe("Type", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohTyp_b)
  cohTyp_drop <- gcombobox(items = c("LFD", "Spatial"), selected = 1, container = cohTyp_b, editable = FALSE)
  addSpring(cohTyp_b)

  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"), container = cohofra_g,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+7])
           # my_project$cohoDisPlot(which(my_project$specieInSurvey == svalue(cohSpe_drop)),
           #                        ifelse(svalue(cohCoh_drop) == "All", "All", as.numeric(svalue(cohCoh_drop))),
           #                        ifelse(svalue(cohYea_drop) == "All", "All", which(my_project$yearInSurvey == svalue(cohYea_drop))),
           #                        ifelse(svalue(cohInt_r) == "Yes", TRUE, FALSE))
         })
  # addSpring(cohofra_g)
  # addSpring(cohoP_g_top)
  addSpace(cohoP_g_top, 2)
  cohPop_p <- ggraphics(container = cohoP_g, width = 550, height = 250, expand = TRUE)


  ####   Simulation   ####

  icoEffIndex_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  icoEffIndex_on <- gimage(system.file("ico/user-available.png", package="smartR"))
  icoSeaIndex_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  icoSeaIndex_on <- gimage(system.file("ico/user-available.png", package="smartR"))
  icoProdIndex_off <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  icoProdIndex_on <- gimage(system.file("ico/user-available.png", package="smartR"))

  sim_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Simulation")
  sim_g_top <- gframe(horizontal = TRUE, container = sim_g, spacing = 10)
  addSpace(sim_g_top, 20)

  sim_g_Effo <- gframe(text = "Effort Index", horizontal = FALSE, container = sim_g_top)
  gbutton("Get", container = sim_g_Effo, handler = function(h,...){
    my_project$setEffortIndex()
    dev.set(dev.list()[pre_dev+8])
    print(ggplot_effoIndBoxplot(df_EffoInde = my_project$fleet$effortIndex))

    if(!is.null(my_project$fleet$effortIndex)){
      delete(sim_g_Effo, sim_g_Effo$children[[length(sim_g_Effo$children)]])
      add(sim_g_Effo, icoEffIndex_on)
    }
  })
  add(sim_g_Effo, icoEffIndex_off)
  addSpace(sim_g_top, 10)

  sim_g_SeaDays <- gframe(text = "Days At Sea", horizontal = FALSE, container = sim_g_top)
  gbutton("Get", container = sim_g_SeaDays, handler = function(h,...){
    my_project$setDaysAtSea()
    dev.set(dev.list()[pre_dev+8])
    print(ggplot_seaDaysBoxplot(df_seaDays = my_project$fleet$daysAtSea))

    if(!is.null(my_project$fleet$daysAtSea)){
      delete(sim_g_SeaDays, sim_g_SeaDays$children[[length(sim_g_SeaDays$children)]])
      add(sim_g_SeaDays, icoSeaIndex_on)
    }
  })
  add(sim_g_SeaDays, icoSeaIndex_off)
  addSpace(sim_g_top, 10)

  sim_g_Prod <- gframe(text = "Production Index", horizontal = FALSE, container = sim_g_top)
  gbutton("Get", container = sim_g_Prod, handler = function(h,...){
    my_project$setProductionIndex()
    dev.set(dev.list()[pre_dev+8])
    print(ggplot_prodIndBoxplot(df_ProdInde = my_project$fleet$prodIndex))

    if(!is.null(my_project$fleet$prodIndex)){
      delete(sim_g_Prod, sim_g_Prod$children[[length(sim_g_Prod$children)]])
      add(sim_g_Prod, icoProdIndex_on)
    }
  })
  add(sim_g_Prod, icoProdIndex_off)

  addSpace(sim_g_top, 20)
  
  sim_g_top2 <- ggroup(horizontal = FALSE, container = sim_g_top)
  addSpring(sim_g_top2)
  gbutton("Set Cost Data", container = sim_g_top2, handler = function(h,...){

    tempWind_Cost <- gwindow(title = "Fishing Cost Data", visible = FALSE,
                             parent = main_win,
                             width = 800, height = 500)

    cost_g <- ggroup(horizontal = FALSE, container = tempWind_Cost, spacing = 15)
    cost_g_top <- gframe(horizontal = TRUE, container = cost_g, spacing = 20)

    addSpring(cost_g_top)
    gbutton("   Load\nCost Data", container = cost_g_top, handler = function(h,...){
      my_project$fleet$loadRawEconomy(economic_path = pathCosts)
      my_project$fleet$setYearEconomy()
    })
    addSpring(cost_g_top)
    gbutton("         Get\nCost Regression", container = cost_g_top, handler = function(h,...){
      my_project$setCostInput()
      my_project$fleet$getCostOutput()
      my_project$fleet$setCostPlot()
      suppressWarnings(grid.arrange(my_project$fleet$plotSpatialReg,
                                    my_project$fleet$plotEffortReg,
                                    my_project$fleet$plotProductionReg,
                                    layout_matrix = rbind(c(1,2,3),c(1,2,3)))
      )
    })
    addSpace(cost_g_top, 10)
    gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = cost_g_top,
           handler = function(h,...){
             suppressWarnings(grid.arrange(my_project$fleet$plotSpatialReg,
                                           my_project$fleet$plotEffortReg,
                                           my_project$fleet$plotProductionReg,
                                           layout_matrix = rbind(c(1,2,3),c(1,2,3)))
             )
           })
    addSpring(cost_g_top)
    gbutton("         Set\nCost Prediction", container = cost_g_top, handler = function(h,...){
      # my_project$fleet$effortIndex$spatialCostPred <- predict(spatialCostLm, my_project$fleet$effortIndex)
      # my_project$fleet$daysAtSea$effortCostPred <- predict(effortCostLm, my_project$fleet$daysAtSea)
      # tmp_Prod$prodCostPred <- predict(prodCostLm, tmp_Prod)
    })
    addSpace(cost_g_top, 10)
    gimage(system.file("ico/view-refresh-5.ico", package="smartR"), container = cost_g_top,
           handler = function(h,...){

           })
    addSpring(cost_g_top)

    visible(tempWind_Cost) <- TRUE

    cost_p <- ggraphics(container = cost_g, width = 550, height = 250, expand = TRUE)


  })
  addSpring(sim_g_top2)
  gbutton("Set Size Class", container = sim_g_top2, handler = function(h,...){

    if(is.null(my_project$fleet$ecoPrice)){
      tmp_df <- data.frame(Class = c("Small", "Medium", "Large"),
                           Units = factor(x = c("Weight", "Weight", "Weight"), levels = c("Length", "Weight")),
                           LowerBound = c(1, 5, 10),
                           UpperBound = c(5, 10, 20),
                           Price = c(2, 5, 10),
                           stringsAsFactors = FALSE, row.names = NULL)
    }else{
      tmp_df <- my_project$fleet$ecoPrice[[unique(c(my_project$specieInFishery, my_project$specieInSurvey))[1]]]
    }

    # out_SizeClass <- list()

    tempWind_Gain <- gwindow(title = "Size Class", visible = FALSE,
                             parent = main_win,
                             width = 600, height = 400)

    gain_g <- ggroup(horizontal = FALSE, container = tempWind_Gain, spacing = 15)
    gain_g_top <- gframe(horizontal = TRUE, container = gain_g, spacing = 20)

    addSpace(gain_g_top, 15)
    sel_specie <- gcombobox(items = unique(c(my_project$specieInFishery, my_project$specieInSurvey)),
                            selected = 1, container = gain_g_top, expand = TRUE, handler = function(...){
                              if(!is.null(my_project$fleet$ecoPrice[[svalue(sel_specie)]])){
                                cost_df[] <- my_project$fleet$ecoPrice[[svalue(sel_specie)]]
                              }else{
                                cost_df[] <-tmp_df
                              }
                            })
    addSpace(gain_g_top, 15)
    add_class <- gbutton(text = "Add Size Class", container = gain_g_top, handler = function(...){
      new_row <- data.frame(Class = "New Class",
                            Units = "Length",
                            LowerBound = 1,
                            UpperBound = 5,
                            Price = 2)
      cost_df[] <- rbind(cost_df[], new_row)
    })
    addSpring(gain_g_top)
    set_data <- gbutton(text = "Set Data", container = gain_g_top, handler = function(...){
      # out_SizeClass[[as.character(svalue(sel_specie))]] <<- cost_df[]
      my_project$fleet$setEcoPrice(sel_specie = svalue(sel_specie), price_df = cost_df[])
    })
    addSpring(gain_g_top)
    ioButt_g <- ggroup(horizontal = FALSE, container = gain_g_top, expand = TRUE)
    gimage(system.file("ico/document-save-2.ico", package="smartR"), container = ioButt_g)
    gimage(system.file("ico/folder-man.png", package="smartR"), container = ioButt_g)
    addSpring(gain_g_top)
    close_butt <- gbutton(text = "Close", container = gain_g_top, handler = function(...){
      delete(dafra_g, cost_df)
      dispose(tempWind_Gain)
    })
    addSpring(gain_g_top)

    dafra_g <- ggroup(horizontal = TRUE, container = gain_g, expand = TRUE)
    addSpace(dafra_g, 30)
    cost_df <- gdf(items = tmp_df, container = dafra_g, expand = TRUE)
    addSpace(dafra_g, 30)
    addSpace(gain_g,30)

    visible(tempWind_Gain) <- TRUE

  })
  addSpring(sim_g_top2)
  gbutton("Set length weight\nrelationship", container = sim_g_top2, handler = function(h,...){
    
    tempWind_LWrel <- gwindow(title="Length-Weight Relationship",
                              visible = FALSE,
                              parent = main_win,
                              width = 900, height = 600)
    
    lwRel_g <- ggroup(horizontal = TRUE, container = tempWind_LWrel, label = "LW relationship")
    addSpace(lwRel_g, 10)
    
    lwRel_g_top <- ggroup(horizontal = FALSE, container = lwRel_g)
    addSpace(lwRel_g_top, 10)
    
    assfra_g <- gframe("Input setup", horizontal = FALSE, container = lwRel_g_top)
    assfra_g$set_size(value = c(width = 200, height = 231))
    addSpace(assfra_g, 5)
    
    assSou_g <- gframe("Source", horizontal = FALSE, container = assfra_g)
    assSou_r <- gradio(c("Survey", "Fishery"), selected = 1, horizontal = FALSE, container = assSou_g,
                       handler = function(...){
                         if(svalue(assSou_r) == "Survey"){
                           if(is.null(my_project$specieInSurvey)){
                             assSpe_drop[] <- "No data"
                             svalue(assSpe_drop) <- "No data"
                           }else{
                             assSpe_drop[] <- my_project$specieInSurvey
                             svalue(assSpe_drop) <- my_project$specieInSurvey[1]
                           }
                         }else{
                           if(is.null(my_project$specieInFishery)){
                             assSpe_drop[] <- "No data"
                             svalue(assSpe_drop) <- "No data"
                           }else{
                             assSpe_drop[] <- my_project$specieInFishery
                             svalue(assSpe_drop) <- my_project$specieInFishery[1]
                           }
                         }
                       })
    addSpace(assfra_g, 10)
    
    assSpe_g <- gframe("Specie", horizontal = FALSE, container = assfra_g)
    assSpe_drop <- gcombobox(items = "Specie", selected = 1, container = assSpe_g, editable = FALSE)
    addSpace(assfra_g, 10)
    
    lwRel_f_sex <- gframe("Sex", horizontal = FALSE, container = assfra_g)
    lwRel_sex_drop <- gcombobox(items = c("Female", "Male", "Unsex"),
                                selected = 1, container = lwRel_f_sex, expand = TRUE,
                                editable = FALSE)
    addSpace(assfra_g, 10)
    
    gbutton("\t  Load\nWeighted Sample", container = assfra_g, handler = function(h,...){
      
      lw_data <- read.csv(pathLWrel)
      lw_fit <- nls(Weight ~ I(alpha * Length ^ beta),
                    data = lw_data[,c("Length", "Weight")],
                    start = list(alpha = 1, beta = 1))
      tmp_alpha <- valu_lyt[1,2]
      svalue(tmp_alpha) <- round(summary(lw_fit)$coefficients[1,1], 5)
      tmp_beta <- valu_lyt[2,2]
      svalue(tmp_beta) <- round(summary(lw_fit)$coefficients[2,1], 5)
      
      if(svalue(assSou_r) == "Survey"){
        my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(tmp_alpha), betaVal = svalue(tmp_beta), sex = svalue(lwRel_sex_drop))
      }else{
        my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(tmp_alpha), betaVal = svalue(tmp_beta), sex = svalue(lwRel_sex_drop))
      }
      
      print(
        ggplot() +
          geom_jitter(data = lw_data,
                      mapping = aes_(x = ~Length, y = ~Weight),
                      width = 0.5, size = 0.25, alpha = 0.25, color = "grey5") +
          theme_tufte(base_size = 14, ticks = F) +
          annotate("line", x = sort(unique(lw_data$Length)),
                   y = predict(lw_fit, list(Length = sort(unique(lw_data$Length)))),
                   linetype = 2, color = "firebrick", size = 0.8) +
          annotate("text", vjust = 1, hjust = 0, size = 8,
                   x = min(lw_data$Length),
                   y = quantile(lw_data$Weight, 0.999),
                   label = "Weight == alpha * Length ^ beta", parse = TRUE) +
          annotate("text", vjust = 1, hjust = 0, size = 7,
                   x = min(lw_data$Length) + 5,
                   y = quantile(lw_data$Weight, 0.999) + 10, parse = TRUE,
                   label = paste("alpha == ", svalue(valu_lyt[1,2]), sep = "")) +
          annotate("text", vjust = 1, hjust = 0, size = 7,
                   x = min(lw_data$Length) + 5,
                   y = quantile(lw_data$Weight, 0.999) - 10, parse = TRUE,
                   label = paste("beta == ", svalue(valu_lyt[2,2]), sep = "")) +
          theme(legend.position = "none",
                panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                axis.text.x = element_text(size = 9),
                axis.title.x = element_text(size = 10),
                axis.text.y = element_text(size = 9),
                axis.title.y = element_text(size = 10),
                axis.ticks.y = element_blank())
      )
      
    })
    
    addSpace(lwRel_g_top, 10)
    
    lwRel_f_valu <- gframe(text = "Values", horizontal = TRUE, container = lwRel_g_top, spacing = 10)
    addSpace(lwRel_f_valu, 10)
    valu_lyt <- glayout(container = lwRel_f_valu)
    valu_lyt[1,1] <- "alpha"
    valu_lyt[1,2] <- gedit(text = "0.01", width = 10, container = valu_lyt)
    valu_lyt[2,1] <- "beta"
    valu_lyt[2,2] <- gedit(text = "3.00", width = 10, container = valu_lyt)
    addSpace(lwRel_f_valu, 10)
    
    addSpace(lwRel_g_top, 10)
    
    gbutton("\nSet Weight\n", container = lwRel_g_top, handler = function(h,...){
      if(svalue(assSou_r) == "Fishery"){
        my_project$fisheryBySpecie[[which(my_project$specieInFishery == svalue(assSpe_drop))]]$setWeight(sexVal = svalue(lwRel_sex_drop))
      }else{
        my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(assSpe_drop))]]$setWeight(sexVal = svalue(lwRel_sex_drop))
      }
    })
    
    addSpring(lwRel_g_top)
    gbutton("Close", container = lwRel_g_top, handler = function(h,...){
      dispose(tempWind_LWrel)
    })
    addSpace(lwRel_g_top, 10)
    
    visible(tempWind_LWrel) <- TRUE
    
    addSpace(lwRel_g, 10)
    lwRel_p <- ggraphics(container = lwRel_g, width = 550, height = 550, expand = TRUE)
    addSpace(lwRel_g, 10)
    
  })
  addSpring(sim_g_top2)
  

  addSpace(sim_g_top, 20)
  
  sim_g_Sim <- gframe(text = "Scenario", horizontal = TRUE, container = sim_g_top, expand = TRUE)
  addSpace(sim_g_Sim, 10)
  sim_g_SimPar <- ggroup(horizontal = TRUE, container = sim_g_Sim)
  addSpring(sim_g_SimPar)
  sim_g_SimPar2 <- ggroup(horizontal = FALSE, container = sim_g_SimPar)
  addSpring(sim_g_SimPar2)
  sim_f_Thr <- gframe(text = "Threshold", horizontal = TRUE, container = sim_g_SimPar2)
  sim_Thr <- gslider(from = 0, to = 100, by = 1, value = 10, container = sim_f_Thr)
  addSpring(sim_g_SimPar2)
  sim_f_Mode <- gframe(text = "Effort Pattern", horizontal = TRUE, container = sim_g_SimPar2)
  addSpring(sim_f_Mode)
  sim_Mode <- gcombobox(items = c("flat", "flatDen", "ban", "banDen"),
                        selected = 1, container = sim_f_Mode, handler = function(h,...){
                          if(svalue(sim_Mode) %in% c("flatDen", "banDen")){
                            enabled(sim_f_Den) <- TRUE
                          }else{
                            enabled(sim_f_Den) <- FALSE
                          }
                          if(svalue(sim_Mode) %in% c("ban", "banDen")){
                            enabled(sim_f_Ban) <- TRUE
                          }else{
                            enabled(sim_f_Ban) <- FALSE
                          }
                        })
  addSpring(sim_f_Mode)
  addSpring(sim_g_SimPar2)
  addSpring(sim_g_SimPar)
  sim_g_SimPar3 <- ggroup(horizontal = FALSE, container = sim_g_SimPar)
  addSpring(sim_g_SimPar3)
  sim_f_Den <- gframe(text = "Density", horizontal = TRUE, container = sim_g_SimPar3)
  addSpring(sim_f_Den)
  sim_Den <- gspinbutton(from = 0.5, to = 5, by = 0.1, value = 1.1, container = sim_f_Den)
  addSpring(sim_f_Den)
  addSpring(sim_g_SimPar3)
  sim_Ban <- gbutton("Set Closed Area", container = sim_g_SimPar3, handler = function(h,...){

    tempWind_Area <- gwindow(title = "Set Closed Area",
                             # visible = FALSE,
                             parent = main_win,
                             width = 600, height = 600)
    
    bigGroup <- ggroup(horizontal = FALSE, container = tempWind_Area)
    
    smaGroup <- ggroup(horizontal = TRUE, container = bigGroup)
    addSpring(smaGroup)
    setArea <- gradio(items = c("Add", "Stop"),  selected = 2, container = smaGroup, handler = function(h,...){
      if(svalue(setArea) == "Stop"){
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }else{
        try({
          while(svalue(setArea) == "Add"){
            banned <- gglocator(n = 1)
            ban_lst <- unlist(over(SpatialPointsDataFrame(banned, banned),
                                   my_project$sampMap$cutResShp, returnList = TRUE))
            if(length(ban_lst) > 0){
              my_project$simBanFG$Banned[ban_lst] <- ifelse(my_project$simBanFG$Banned[ban_lst] == "Banned", "0", "Banned")
              
              grid_data$Banned[grid_data$FG == my_project$sampMap$cutResShp@polygons[[ban_lst[1]]]@ID] <- ifelse(grid_data$Banned[grid_data$FG == cutResShp@polygons[[ban_lst[1]]]@ID] == "Banned",
                                                                                               "0", "Banned")
              
              print(my_project$sampMap$gooMapPlot +
                      geom_polygon(aes(x = long, y = lat, group = group, fill = Banned),
                                   colour = "black", size = 0.1,
                                   data = grid_data, alpha = 0.8) +
                      scale_fill_manual("Banned Areas",
                                        values = c("Banned" = "red", "0" = "blue")) +
                      geom_text(aes(label = FG, x = Lon, y = Lat),
                                data = tmp_coo, size = 2) +
                      theme(legend.position='none') +
                      xlab("Longitude") + ylab("Latitude") +
                      scale_x_continuous(expand=c(0,0)) + 
                      scale_y_continuous(expand=c(0,0)))
            }
          }
        },silent = TRUE)
        
      }
    })
    
    addSpring(smaGroup)
    endArea <- gbutton(text = "\nClose\n", container = smaGroup, handler = function(h,...){
      # blockHandlers(obj = setArea)
      dispose(tempWind_Area)
    })
    addSpring(smaGroup)
    
    lwRel_p <- ggraphics(container = bigGroup, width = 550, height = 350, expand = TRUE)
    
    grid_data <- merge(x = my_project$sampMap$cutResShpFort,
                       y = my_project$simBanFG, all.x = TRUE)
    tmp_coo <- my_project$sampMap$cutResShpCent
    print(my_project$sampMap$gooMapPlot +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Banned),
                   colour = "black", size = 0.1,
                   data = grid_data, alpha = 0.8) +
      scale_fill_manual("Banned Areas",
                        values = c("Banned" = "red", "0" = "blue")) +
      geom_text(aes(label = FG, x = Lon, y = Lat),
                data = tmp_coo, size = 2) +
      theme(legend.position='none') +
      xlab("Longitude") + ylab("Latitude") +
      scale_x_continuous(expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)))
  })
  addSpring(sim_g_SimPar3)
  addSpring(sim_g_SimPar)
  addSpring(sim_g_Sim)
  gbutton("   Start\nSimulation", container = sim_g_Sim, handler = function(h,...){
    dev.set(dev.list()[pre_dev+8])
    cat("\n\nSimulating ", sep = "")
    my_project$simEffo <- NULL
    gc()
    my_project$genSimEffo()
    my_project$genSimEffo(method = svalue(sim_Mode), overDen = svalue(sim_Den), areaBan = svalue(sim_Ban))
    my_project$simulateFishery(thr0 = svalue(sim_Thr), effoMode = svalue(sim_Mode), effoBan = svalue(sim_Ban), effoDen = svalue(sim_Den))
    my_project$simProdAll()
    my_project$getSimTotalCost()
    my_project$getSimRevenue()
    my_project$getCostRevenue()
  })
  addSpring(sim_g_Sim)

  addSpace(sim_g_top, 10)
  gbutton(" View\nResult", container = sim_g_top, handler = function(h,...){
    dev.set(dev.list()[pre_dev+8])
    
  })
  addSpace(sim_g_top, 10)
  
  sim_p <- ggraphics(container = sim_g, width = 550, height = 250, expand = TRUE)

  
  
  ####   Assess   ####
  
  ass_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Assess")
  ass_g_top <- gframe(horizontal = TRUE, container = ass_g, spacing = 10)
  addSpace(ass_g_top, 10)
  
  addSpace(ass_g_top, 10)
  
  

  visible(pro_eg) <- TRUE
  visible(raw_eg) <- TRUE
  visible(eff_eg) <- TRUE
  visible(ass_eg) <- TRUE
  visible(sim_eg) <- TRUE
  # visible(main_win) <- TRUE

  svalue(uti_gn) <- 2
  svalue(uti_gn) <- 3
  svalue(uti_gn) <- 4
  svalue(uti_gn) <- 5
  svalue(uti_gn) <- 6
  svalue(uti_gn) <- 7
  svalue(uti_gn) <- 8
  svalue(uti_gn) <- 9
  svalue(uti_gn) <- 10
  svalue(uti_gn) <- 11
  svalue(uti_gn) <- 12

  svalue(uti_gn) <- 1
  # visible(main_win) <- TRUE
}
