
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

  my_project <- SmartProject$new()
  my_project$createFleet()

  pre_dev <- length(dev.list())

  main_win <- gwindow(paste("SMART - Version ", "1.1", sep = ""),width = 1200, height= 625, visible = FALSE)
  big_g <- ggroup(horizontal = TRUE, container = main_win)


  ##############################################################
  ####   Left panel   ##########################################
  ##############################################################

  lef_g <- ggroup(horizontal = FALSE, container = big_g)
  addSpring(lef_g)
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
  raw_eg <- gexpandgroup("Resources", horizontal = FALSE, container = lef_g)
  gbutton(text = "Survey", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 3})
  gbutton(text = "Fishery", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 4})
  gbutton(text = "Population", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 5})
  gbutton(text = "Mixture", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 6})
  gbutton(text = "Cohorts", container = raw_eg, handler = function(h,...){
    svalue(uti_gn) <- 7})

  eff_eg <- gexpandgroup("Effort", horizontal = FALSE, container = lef_g)
  gbutton(text = "Load VMS Data", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 8})

  gbutton(text = "Fishing Grounds", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 9})

  gbutton(text = "Fleet Register", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 10})

  gbutton(text = "Production", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 11})

  gbutton(text = "Selectivity", container = eff_eg, handler = function(h,...){
    svalue(uti_gn) <- 12})

  pre_eg <- gexpandgroup("Predictive", horizontal = FALSE, container = lef_g)
  gbutton(text = "Predict", container = pre_eg, handler = function(h,...){
    svalue(uti_gn) <- 13})
  sim_eg <- gexpandgroup("Simulation", horizontal = FALSE, container = lef_g)
  gbutton(text = "Simulate", container = sim_eg, handler = function(h,...){
    svalue(uti_gn) <- 14})
  addSpring(lef_g)
  stat_bar <- gstatusbar("", container = lef_g, visible = TRUE)


  ##############################################################
  ####   Right panel   #########################################
  ##############################################################

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
    my_project <<- readRDS(load_path)

    ### Update Sampling Status

    if(!is.null(my_project$rawDataSurvey)){ #update_pop_gui()

      raw_t[] <- my_project$rawDataSurvey[sample(1:nrow(my_project$rawDataSurvey), 100, replace = FALSE),]
      svalue(raw_l1) <- paste("Specie: ", paste(my_project$specieInSurvey, collapse = " - "))
      #   svalue(raw_l2) <- paste("Length Classes: from ",  min(my_project$LClass), " to ", max(my_project$LClass))
      svalue(raw_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInSurvey))), " to ", max(as.numeric(as.character(my_project$yearInSurvey))))
      spec_drop[] <- my_project$specieInSurvey
      spec_drop_mix[] <- my_project$specieInSurvey
      # spevie_drop[] <- c("All", my_project$specieInSurvey)
      cohSpe_drop[] <- my_project$specieInSurvey
      svalue(spec_drop) <- my_project$specieInSurvey[1]
      svalue(cohSpe_drop) <- my_project$specieInSurvey[1]
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
    svalue(uti_gn) <- 8
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
    svalue(uti_gn) <- 9
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
    svalue(uti_gn) <- 10
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
    svalue(uti_gn) <- 11
  })
  prod_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  add(prod_g, prod_sta)
  #   enabled(prod_b) <- FALSE


  addSpring(pro_g_mid)
  # gbutton("Selectivity", container = pro_g_mid)
  sele_g <- gframe(text = "Selectivity", horizontal = FALSE, container = pro_g_mid)
  # n_cell_g <- glabel("   ---", container = effo_g)
  addSpring(sele_g)
  sele_b <- gbutton(text = "Show data", container = sele_g, handler = function(h,..){
    svalue(uti_gn) <- 12
  })
  sele_sta <- gimage(system.file("ico/user-invisible.png", package="smartR"))
  add(sele_g, sele_sta)
  #   enabled(sele_b) <- FALSE

  addSpring(pro_g_mid)
  addSpace(pro_g_mid, 2)



  ##############################################################
  ####   Environment   #########################################
  ##############################################################

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
    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])
    my_project$loadMap("/Users/Lomo/Documents/Uni/Lab/Proj/SMART_All/SMART1.0/Geo/Grid/GFCM_Grid_6min_GSA16.shp")

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
      svalue(stat_bar) <- "Splitting Population..."
      my_project$setLFDPopSurvey()
    }
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
           if(!is.null(my_project$sampMap$gooMap)){
             my_project$sampMap$plotGooGrid()
           }else{
             my_project$sampMap$plotSamMap(title = my_project$sampMap$gridName)
           }
         })
  addSpring(gri_g_top1_gri)

  addSpring(gri_g_top1)

  gri_g_top1_dep <- ggroup(horizontal = TRUE, container = gri_g_top1)
  #   addSpring(gri_g_top1_dep)
  gbutton("Download Depth", container = gri_g_top1_dep, handler = function(h,...){
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Downloading depth..."
    Sys.sleep(1)
    my_project$sampMap$getGridBath()
    my_project$sampMap$getCentDept()
    svalue(stat_bar) <- "Plotting Bathymetry..."
    Sys.sleep(1)
    my_project$sampMap$ggplotGridBathy()
    svalue(stat_bar) <- ""
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
    Sys.sleep(1)

    load_path <- "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/bathy_test.rData"

    my_project$sampMap$loadGridBath(load_path)
    # my_project$sampMap$getCentDept()
    svalue(stat_bar) <- "Plotting Bathymetry..."
    Sys.sleep(1)
    my_project$sampMap$ggplotGridBathy()
    svalue(stat_bar) <- ""
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
    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Loading biocenosis data.frame..."
    my_project$sampMap$loadBioDF("/Users/Lomo/Documents/Uni/R/smart/data/BioM.rData")
    if(!is.null(my_project$sampMap$gooMap)){
      my_project$sampMap$ggplotBioDF()
    }else{
      my_project$sampMap$plotBioDF()
    }
    svalue(stat_bar) <- ""
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
    Sys.sleep(1)
    dev.set(dev.list()[pre_dev+1])
    svalue(stat_bar) <- "Loading Google map..."
    my_project$sampMap$getGooMap()
    my_project$sampMap$setGooGrid()
    my_project$sampMap$setGooBbox()
    svalue(stat_bar) <- ""
    my_project$sampMap$plotGooGrid()
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
  gri_l1 <- glabel("Polyset: ", container = gri_g_top2)
  gri_l2 <- glabel("N. Cells: ", container = gri_g_top2)
  gri_l3 <- glabel("GCenter: ", container = gri_g_top2)
  addSpring(gri_g_top)
  addSpace(gri_g_top2, 2)
  # gri_g_top3 <- gframe(text = "View", horizontal = TRUE, container = gri_g_top, expand = TRUE)
  # addSpring(gri_g_top3)
  # spevie_drop <- gcombobox(items = "Specie", selected = 1, container = gri_g_top3, expand = TRUE, editable = FALSE)
  # addSpring(gri_g_top3)
  # gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"), container = gri_g_top3,
  #        handler = function(h,...){
  #          dev.set(dev.list()[pre_dev+1])
  #          my_project$speDisPlot(svalue(spevie_drop))
  #        })
  # addSpring(gri_g_top3)
  addSpring(gri_g_top)
  gri_p <- ggraphics(container = gri_g, width = 600, height = 280, expand = TRUE)



  ##############################################################
  ####     Survey     ##########################################
  ##############################################################

  raw_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Survey")
  raw_g_top <- gframe(horizontal = TRUE, container = raw_g)
  addSpace(raw_g_top, 2)
  addSpring(raw_g_top)
  raw_g_top1 <- ggroup(horizontal = FALSE, container = raw_g_top)
  addSpring(raw_g_top1)
  gbutton("Load Sample", container = raw_g_top1, handler = function(h,...){
    svalue(stat_bar) <- "Loading Data..."
    Sys.sleep(1)
    my_project$loadSurveyLFD(csv_path = "/Users/Lomo/Documents/Uni/Lab/Proj/smart\ gui/SMART_GUI/SampleData_ed.csv")

    if(!is.null(my_project$rawDataSurvey)){ #update_pop_gui()

      raw_t[] <- my_project$rawDataSurvey[sample(1:nrow(my_project$rawDataSurvey), 100, replace = FALSE),]
      svalue(raw_l1) <- paste("Specie: ", paste(my_project$specieInSurvey, collapse = " - "))
      #   svalue(raw_l2) <- paste("Length Classes: from ",  min(my_project$LClass), " to ", max(my_project$LClass))
      svalue(raw_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInSurvey))), " to ", max(as.numeric(as.character(my_project$yearInSurvey))))
      spec_drop[] <- my_project$specieInSurvey
      spec_drop_mix[] <- my_project$specieInSurvey
      # spevie_drop[] <- c("All", my_project$specieInSurvey)
      cohSpe_drop[] <- my_project$specieInSurvey
      svalue(spec_drop) <- my_project$specieInSurvey[1]
      svalue(cohSpe_drop) <- my_project$specieInSurvey[1]
      # svalue(spevie_drop) <- "All"
      svalue(spec_drop_mix) <- my_project$specieInSurvey[1]
      year_drop[] <- c("All", as.character(my_project$yearInSurvey))
      cohYea_drop[] <- c("All", as.character(my_project$yearInSurvey))
      svalue(year_drop) <- my_project$yearInSurvey[1]
      svalue(cohYea_drop) <- "All"

      if(!is.null(my_project$sampMap)){
        svalue(stat_bar) <- "Splitting Population..."
        my_project$setLFDPopSurvey()
      }
      svalue(stat_bar) <- ""

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
  raw_g_top2 <- ggroup(horizontal = FALSE, container = raw_g_top)
  raw_l1 <- glabel("Specie: ", container = raw_g_top2)
  raw_l3 <- glabel("Years: ", container = raw_g_top2)
  addSpring(raw_g_top)
  addSpace(raw_g_top, 2)
  addSpace(raw_g_top2, 2)

  blankDF = data.frame(SPECIE = character(0), LAT = numeric(0), LON = numeric(0), Year = character(0), LCLASS = numeric(0), FEMALE = character(0), MALE = character(0), UNSEX = character(0), stringsAsFactors=FALSE)
  raw_t <- gtable(blankDF, container = raw_g, expand = TRUE)



  ##############################################################
  ####     Fishery     #########################################
  ##############################################################

  fis_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Fishery")
  fis_g_top <- gframe(horizontal = TRUE, container = fis_g)
  addSpace(fis_g_top, 2)
  addSpring(fis_g_top)
  fis_g_top1 <- ggroup(horizontal = FALSE, container = fis_g_top)
  addSpring(fis_g_top1)
  gbutton("Load Sample", container = fis_g_top1, handler = function(h,...){
    svalue(stat_bar) <- "Loading Data..."
    Sys.sleep(1)
    my_project$loadFisheryLFD(csv_path = "/Users/Lomo/Documents/Uni/R/smart/data/Resource - Fishery/fishery_data_CampBiol.csv")

    if(!is.null(my_project$rawDataFishery)){ #update_pop_gui()

      fis_t[] <- my_project$rawDataFishery[sample(1:nrow(my_project$rawDataFishery), 100, replace = FALSE),]
      svalue(fis_l1) <- paste("Specie: ", paste(my_project$specieInFishery, collapse = " - "))
      #   svalue(fis_l2) <- paste("Length Classes: from ",  min(my_project$LClass), " to ", max(my_project$LClass))
      svalue(fis_l3) <- paste("Years: from", min(as.numeric(as.character(my_project$yearInFishery))), " to ", max(as.numeric(as.character(my_project$yearInFishery))))
      # spec_drop[] <- my_project$specieInFishery
      # spec_drop_mix[] <- my_project$specieInFishery
      # spevie_drop[] <- c("All", my_project$specieInFishery)
      # cohSpe_drop[] <- my_project$specieInFishery
      # svalue(spec_drop) <- my_project$specieInFishery[1]
      # svalue(cohSpe_drop) <- my_project$specieInFishery[1]
      # svalue(spevie_drop) <- "All"
      # svalue(spec_drop_mix) <- my_project$specieInFishery[1]
      # year_drop[] <- c("All", as.character(my_project$yearInFishery))
      # cohYea_drop[] <- c("All", as.character(my_project$yearInFishery))
      # svalue(year_drop) <- my_project$yearInFishery[1]
      # svalue(cohYea_drop) <- "All"

      # if(!is.null(my_project$sampMap)){
      #   svalue(stat_bar) <- "Splitting Population..."
      #   my_project$setLFDPopSurvey()
      # }

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
  gbutton("Set length weight\nrelationship", container = fis_g_top1, handler = function(h,...){



  })
  addSpring(fis_g_top1)
  addSpring(fis_g_top)
  fis_g_top2 <- ggroup(horizontal = FALSE, container = fis_g_top)
  fis_l1 <- glabel("Specie: ", container = fis_g_top2)
  fis_l3 <- glabel("Years: ", container = fis_g_top2)
  addSpring(fis_g_top)
  addSpace(fis_g_top, 2)
  addSpace(fis_g_top2, 2)

  blankDF = data.frame(SPECIE = character(0), LAT = numeric(0), LON = numeric(0), Date = character(0), LCLASS = numeric(0), FEMALE = character(0), MALE = character(0), UNSEX = character(0), stringsAsFactors=FALSE)
  fis_t <- gtable(blankDF, container = fis_g, expand = TRUE)



  ##############################################################
  ####   Population   ##########################################
  ##############################################################

  pop_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Population")
  pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
  addSpring(pop_g_top)
  lfdfra_g <- gframe("LFD data", horizontal = TRUE, container = pop_g_top, expand = TRUE)
  addSpring(lfdfra_g)
  sourcePop_r <- gradio(items = c("Survey", "Fishery"), horizontal = FALSE, container = lfdfra_g, expand = TRUE, handler = function(...){
    if(svalue(sourcePop_r) == "Survey"){
      spec_drop[] <- my_project$specieInSurvey
      svalue(spec_drop) <- my_project$specieInSurvey[1]
      year_drop[] <- c("All", as.character(my_project$yearInSurvey))
      svalue(year_drop) <- my_project$yearInSurvey[1]
    }else{
      spec_drop[] <- my_project$specieInFishery
      svalue(spec_drop) <- my_project$specieInFishery[1]
      year_drop[] <- c("All", as.character(my_project$yearInFishery))
      svalue(year_drop) <- my_project$yearInFishery[1]
    }
  })
  addSpring(lfdfra_g)
  spec_b <- gframe("Specie", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
  addSpring(lfdfra_g)
  addSpring(spec_b)
  spec_drop <- gcombobox(items = "Specie", selected = 1, container = spec_b, editable = FALSE)
  addSpring(spec_b)
  year_b <- gframe("Year", horizontal = FALSE, container = lfdfra_g, expand = TRUE)
  addSpring(lfdfra_g)
  addSpring(year_b)
  year_drop <- gcombobox(items = "Year", selected = 1, container = year_b, editable = FALSE)
  addSpring(year_b)
  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"), container = lfdfra_g,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+2])
           if(svalue(sourcePop_r) == "Survey"){
             spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop))
             ifelse(svalue(year_drop) == "All", my_cel_dat <- my_project$surveyBySpecie[[spe_ind]]$rawLFD[,c("LCLASS","FEMALE","MALE")], my_cel_dat <- my_project$surveyBySpecie[[spe_ind]]$rawLFD[which(my_project$surveyBySpecie[[spe_ind]]$rawLFD[,"Year"] ==  svalue(year_drop)),c("LCLASS","FEMALE","MALE")])
             the_reclfd <- RecLFD(my_cel_dat, my_project$surveyBySpecie[[spe_ind]]$lengClas, 1)
           }else{
             spe_ind <- which(my_project$specieInFishery == svalue(spec_drop))
             ifelse(svalue(year_drop) == "All", my_cel_dat <- my_project$fisheryBySpecie[[spe_ind]]$rawLFD[,c("LCLASS","FEMALE","MALE")], my_cel_dat <- my_project$fisheryBySpecie[[spe_ind]]$rawLFD[which(years(my_project$fisheryBySpecie[[spe_ind]]$rawLFD[,"DATE"]) ==  svalue(year_drop)),c("LCLASS","FEMALE","MALE")])
             the_reclfd <- RecLFD(my_cel_dat, my_project$fisheryBySpecie[[spe_ind]]$lengClas, 1)
           }
           plotRecLFD(the_reclfd)
         })
  addSpring(lfdfra_g)
  addSpring(pop_g_top)
  addSpace(pop_g_top, 2)
  pop_p <- ggraphics(container = pop_g, width = 600, height = 280, expand = TRUE)



  ##############################################################
  ####   Mixture   #############################################
  ##############################################################

  mix_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Mixture")
  mix_g_top <- gframe(horizontal = TRUE, container = mix_g) #if expand bad
  addSpace(mix_g_top, 20)
  cont_g <- gframe("Mixture Analysis", horizontal = TRUE, container = mix_g_top, expand = TRUE)
  addSpring(cont_g)
  sourceMix_r <- gradio(items = c("Survey", "Fishery"), horizontal = FALSE, container = cont_g, expand = TRUE, handler = function(...){
    if(svalue(sourceMix_r) == "Survey"){
      spec_drop_mix[] <- my_project$specieInSurvey
      svalue(spec_drop_mix) <- my_project$specieInSurvey[1]
    }else{
      spec_drop_mix[] <- my_project$specieInFishery
      svalue(spec_drop_mix) <- my_project$specieInFishery[1]
    }
  })
  spec_mix_f <- gframe(" Specie ", horizontal = FALSE, container = cont_g, expand = TRUE)
  addSpring(spec_mix_f)
  spec_drop_mix <- gcombobox(items = "Specie", selected = 1, container = spec_mix_f, editable = FALSE, expand = TRUE)
  addSpring(spec_mix_f)
  addSpring(cont_g)
  ncoh_f <- gframe("N. cohorts", horizontal = FALSE, container = cont_g)
  addSpring(ncoh_f)
  ncih_sb <- gspinbutton (from = 1, to = 10, by = 1, value = 3, digits = 0, container = ncoh_f)
  addSpring(ncoh_f)
  addSpring(cont_g)
  prio_g <- ggroup(horizontal = TRUE, container = cont_g)
  addSpring(prio_g)
  prio_b <- gbutton ("Set Priors", container = prio_g,
                     handler = function(h,...){

                       spe_ind <- which(my_project$specieInSurvey == svalue(spec_drop_mix))
                       #                        temp_dia <- gbasicdialog(title="Set Growth Parameters", do.buttons = FALSE)
                       temp_dia <- gwindow(title="Set Growth Parameters", visible = FALSE,
                                           parent = main_win, width = 550, height = 400)
                       # size(temp_dia) <- c(550, 400)
                       up_g_top <- ggroup(horizontal = TRUE, container = temp_dia)
                       addSpring(up_g_top)
                       up_g <- ggroup(horizontal = FALSE, container = up_g_top)
                       addSpring(up_g)

                       fe_fra <- gframe("Female", container = up_g, fill = TRUE)
                       addSpace(fe_fra, 10)
                       fe_lay <- glayout(homogeneous = FALSE, spacing = 10, container = fe_fra)
                       fe_lay[1,2, anchor = 0] <- "Mean"
                       fe_lay[1,3, anchor = 0] <- "SD"
                       fe_lay[2,1, anchor = 0] <- "Linf"
                       fe_lay[3,1, anchor = 0] <- "K"
                       fe_lay[4,1, anchor = 0] <- "t0"
                       pre_pri <- length(my_project$surveyBySpecie[[1]]$prior)
                       fe_lay[2,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$Linf$Mean, "edit here"), container=fe_lay)
                       fe_lay[2,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$Linf$StD, "edit here"), container=fe_lay)
                       fe_lay[3,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$K$Mean, "edit here"), container=fe_lay)
                       fe_lay[3,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$K$StD, "edit here"), container=fe_lay)
                       fe_lay[4,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$t0$Mean, "edit here"), container=fe_lay)
                       fe_lay[4,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Female"]]$t0$StD, "edit here"), container=fe_lay)
                       addSpace(fe_fra, 10)
                       addSpace(up_g, 10)

                       ma_fra <- gframe("Male", container = up_g, fill = TRUE)
                       addSpace(ma_fra, 10)
                       ma_lay <- glayout(homogeneous = FALSE, spacing = 10, container = ma_fra)
                       ma_lay[1,2, anchor = 0] <- "Mean"
                       ma_lay[1,3, anchor = 0] <- "SD"
                       ma_lay[2,1, anchor = 0] <- "Linf"
                       ma_lay[3,1, anchor = 0] <- "K"
                       ma_lay[4,1, anchor = 0] <- "t0"
                       ma_lay[2,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$Linf$Mean, "edit here"), container=ma_lay)
                       ma_lay[2,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$Linf$StD, "edit here"), container=ma_lay)
                       ma_lay[3,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$K$Mean, "edit here"), container=ma_lay)
                       ma_lay[3,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$K$StD, "edit here"), container=ma_lay)
                       ma_lay[4,2] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$t0$Mean, "edit here"), container=ma_lay)
                       ma_lay[4,3] <- gedit(ifelse(pre_pri, my_project$surveyBySpecie[[spe_ind]]$prior[["Male"]]$t0$StD, "edit here"), container=ma_lay)
                       addSpace(ma_fra, 10)

                       addSpring(up_g)
                       addSpring(up_g_top)
                       bot_g <- ggroup(horizontal = TRUE, container = up_g)
                       addSpring(bot_g)
                       gbutton("  Load  \n  CSV  ", container = bot_g,
                               handler = function(h,...){
                                 g_par_file <- gfile(text = "Select a file...", type = "open",
                                                     initial.filename = NULL, initial.dir = getwd(),
                                                     filter = list("csv files" = list(patterns = c("*.csv", "*.txt")),
                                                                   "All files" = list(patterns = c("*"))), quote = TRUE)
                                 if(length(g_par_file) > 0){
                                   tmp_g <- read.table(g_par_file, sep = ";", dec = ".", header = TRUE,
                                                       colClasses = c("factor", rep("numeric",6)))
                                   fe_lay[2,2] <- gedit(tmp_g[1,2], container=fe_lay)
                                   fe_lay[2,3] <- gedit(tmp_g[1,3], container=fe_lay)
                                   fe_lay[3,2] <- gedit(tmp_g[1,4], container=fe_lay)
                                   fe_lay[3,3] <- gedit(tmp_g[1,5], container=fe_lay)
                                   fe_lay[4,2] <- gedit(tmp_g[1,6], container=fe_lay)
                                   fe_lay[4,3] <- gedit(tmp_g[1,7], container=fe_lay)
                                   ma_lay[2,2] <- gedit(tmp_g[2,2], container=ma_lay)
                                   ma_lay[2,3] <- gedit(tmp_g[2,3], container=ma_lay)
                                   ma_lay[3,2] <- gedit(tmp_g[2,4], container=ma_lay)
                                   ma_lay[3,3] <- gedit(tmp_g[2,5], container=ma_lay)
                                   ma_lay[4,2] <- gedit(tmp_g[2,6], container=ma_lay)
                                   ma_lay[4,3] <- gedit(tmp_g[2,7], container=ma_lay)
                                 }
                               })
                       addSpring(bot_g)

                       gbutton("\nAccept\n", container = bot_g,
                               handler = function(h,...){
                                 my_project$surveyBySpecie[[spe_ind]]$setPrior(as.numeric(unlist(lapply(fe_lay[2,2:3], svalue))),
                                                                               as.numeric(unlist(lapply(fe_lay[3,2:3], svalue))),
                                                                               as.numeric(unlist(lapply(fe_lay[4,2:3], svalue))),
                                                                               as.numeric(unlist(lapply(ma_lay[2,2:3], svalue))),
                                                                               as.numeric(unlist(lapply(ma_lay[3,2:3], svalue))),
                                                                               as.numeric(unlist(lapply(ma_lay[4,2:3], svalue))))

                                 ## to check
                                 dispose(temp_dia)
                                 # suppressMessages(dispose(temp_dia))
                               })
                       addSpring(bot_g)
                       visible(temp_dia) <- TRUE
                       #                        visible(temp_dia, set=TRUE)
                     })
  addSpring(prio_g)
  addSpring(cont_g)
  go_g <- gframe("MCMC sim", horizontal = TRUE, container = cont_g, expand = TRUE)
  addSpring(go_g)
  go_g_ada <- ggroup(horizontal = FALSE, container = go_g, expand = TRUE)
  addSpring(go_g_ada)
  glabel("N. adapt: ", container = go_g_ada)
  mc_niter <- gcombobox(c(100, 1000, 10000), selected = 2, container = go_g_ada, editable = FALSE, expand = TRUE)
  addSpring(go_g_ada)
  go_g_sam <- ggroup(horizontal = FALSE, container = go_g)
  addSpring(go_g_sam)
  glabel("Sample size: ", container = go_g_sam)
  mc_nsamp <- gcombobox(c(200, 2000, 20000), selected = 2, container = go_g_sam, editable = FALSE, expand = TRUE)
  addSpring(go_g_sam)
  addSpring(go_g)
  go_b <- gbutton ("   GO   ", container = go_g,
                   handler = function(h,...){
                     dev.set(dev.list()[pre_dev+3])
                     pre_mfrow <- par(c("mfrow", "mar"))
                     par(mfrow = c(2, 1))
                     par(mar = c(2,2,1.5,0.5))
                     my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_mix))]]$setNCoho(as.numeric(svalue(ncih_sb)))
                     my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_mix))]]$calcMix(nAdap = as.numeric(svalue(mc_niter)), nSamp = as.numeric(svalue(mc_nsamp)))

                     my_project$calcCoh_A_Survey(which(my_project$specieInSurvey == svalue(spec_drop_mix)))
                     my_project$intrpCoh_A_Survey(which(my_project$specieInSurvey == svalue(spec_drop_mix)))

                     cohCoh_drop[] <- c("All", seq(1, my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(spec_drop_mix))]]$nCoho, by = 1))
                     svalue(cohCoh_drop) <- "All"
                     par(pre_mfrow)
                   })
  addSpring(go_g)
  # save_b <- gbutton ("  SAVE  ", container = go_g)
  addSpring(cont_g)
  mix_p <- ggraphics(container = mix_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Cohorts   #############################################
  ##############################################################

  cohoP_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Cohorts")
  cohoP_g_top <- gframe(horizontal = TRUE, container = cohoP_g, spacing = 10)
  addSpring(cohoP_g_top)

  cohofra_g <- gframe("Cohort data", horizontal = TRUE, container = cohoP_g_top, expand = TRUE)
  addSpring(cohofra_g)
  cohSpe_b <- gframe("Specie", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohSpe_b)
  cohSpe_drop <- gcombobox(items = "Specie", selected = 1, container = cohSpe_b, editable = FALSE, handler = function(h,...){
    if(length(my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(cohSpe_drop))]]$nCoho) > 0){
      cohCoh_drop[] <- c("All", seq(1, my_project$surveyBySpecie[[which(my_project$specieInSurvey == svalue(cohSpe_drop))]]$nCoho, by = 1))
      svalue(cohCoh_drop) <- "All"
    }
  })
  addSpring(cohSpe_b)

  cohCoh_b <- gframe("Cohort", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohCoh_b)
  cohCoh_drop <- gcombobox(items = "Cohort", selected = 1, container = cohCoh_b, editable = FALSE)
  addSpring(cohCoh_b)

  cohYea_b <- gframe("Year", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohYea_b)
  cohYea_drop <- gcombobox(items = "Year", selected = 1, container = cohYea_b, editable = FALSE)
  addSpring(cohYea_b)

  cohInt_g <- gframe("Interpolated", horizontal = FALSE, container = cohofra_g, expand = TRUE)
  addSpring(cohofra_g)
  addSpring(cohInt_g)
  cohInt_r <- gradio(c("Yes", "No"), selected = 1, horizontal = TRUE, container = cohInt_g)
  addSpring(cohInt_g)

  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"), container = cohofra_g,
         handler = function(h,...){
           dev.set(dev.list()[pre_dev+4])
           my_project$cohoDisPlot(which(my_project$specieInSurvey == svalue(cohSpe_drop)),
                                  ifelse(svalue(cohCoh_drop) == "All", "All", as.numeric(svalue(cohCoh_drop))),
                                  ifelse(svalue(cohYea_drop) == "All", "All", which(my_project$yearInSurvey == svalue(cohYea_drop))),
                                  ifelse(svalue(cohInt_r) == "Yes", TRUE, FALSE))
         })
  addSpring(cohofra_g)
  addSpring(cohoP_g_top)
  addSpace(cohoP_g_top, 2)
  cohPop_p <- ggraphics(container = cohoP_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Effort   ##############################################
  ##############################################################

  ### TO DO:

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

    tmp_file <- "/Users/Lomo/Documents/Uni/PhD/TESI/SoS_vms/smart_rawEffort_new.rData"

    cat("\nLoading effort from rData...", sep = "")
    svalue(stat_bar) <- "Loading effort from rData..."
    Sys.sleep(1)

    my_project$fleet$rawEffort <- readRDS(tmp_file)
    my_project$fleet$setEffortIds()
    # cat("   Done!", sep = "")
    svalue(stat_bar) <- ""

    effvie_drop[] <- names(my_project$fleet$rawEffort)
    svalue(effvie_drop) <- names(my_project$fleet$rawEffort)[1]
    dev.set(dev.list()[pre_dev+5])

    svalue(stat_bar) <- "Plotting raw effort..."
    my_project$ggplotRawPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""

    # my_project$effPlot("All")

    ### Update Effort Status
    effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    delete(effo_g, effo_g$children[[length(effo_g$children)]])
    add(effo_g, effo_sta_n)
  })
  addSpring(eff_g_top1)
  gbutton("Extract from VMSBASE", container = eff_g_top1, handler = function(h,...){

    #### SKIPPED LOADING rData
    #     tmp_files <- gfile(text = "Select Effort DBs", type = "open",
    #                        initial.filename = NULL, initial.dir = getwd(), filter = list(),
    #                        multi = TRUE)
    #
    #     my_project$loadFleeEffoDbs(tmp_files)

    tmp_file <- "/Users/Lomo/Documents/Uni/PhD/TESI/SoS_vms/smart_rawEffort_new.rData"

    cat("\nLoading effort from rData...", sep = "")
    svalue(stat_bar) <- "Loading effort from vmsbase db..."
    Sys.sleep(1)
    my_project$fleet$rawEffort <- readRDS(tmp_file)
    my_project$fleet$setEffortIds()
    cat("   Done!", sep = "")
    svalue(stat_bar) <- ""

    effvie_drop[] <- names(my_project$fleet$rawEffort)
    svalue(effvie_drop) <- names(my_project$fleet$rawEffort)[1]
    dev.set(dev.list()[pre_dev+5])

    svalue(stat_bar) <- "Plotting Count of disinct vessels..."
    Sys.sleep(1)
    my_project$fleet$plotCountIDsEffo()
    svalue(stat_bar) <- ""

    ### Update Effort Status
    effo_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
    delete(effo_g, effo_g$children[[length(effo_g$children)]])
    add(effo_g, effo_sta_n)
  })
  addSpring(eff_g_top1)
  gbutton("View Stats", container = eff_g_top1, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting Count of disinct vessels..."
    Sys.sleep(1)
    my_project$fleet$plotCountIDsEffo()
    svalue(stat_bar) <- ""
  })

  addSpring(eff_g_top)

  eff_g_top1b <- ggroup(horizontal = FALSE, container = eff_g_top)
  addSpring(eff_g_top1b)
  gbutton("Set Fishing Points", container = eff_g_top1b, handler = function(h,...){
    svalue(stat_bar) <- "Setting parameters..."

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
      my_project$fleet$setFishPoinPara(speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                       depth_range = sort(unlist(lapply(dep_lay[1:2,2], svalue)), decreasing = TRUE))
      svalue(stat_bar) <- "Setting fishing points..."
      Sys.sleep(1)
      my_project$fleet$setFishPoin()
      svalue(stat_bar) <- ""
      Sys.sleep(1)
      svalue(stat_bar) <- "Setting fishing point cells..."
      Sys.sleep(1)
      my_project$setCellPoin()
      svalue(stat_bar) <- "Adding week and month number to dataset..."
      Sys.sleep(1)
      my_project$fleet$setWeekMonthNum()
      svalue(stat_bar) <- ""
      dispose(temp_dia)
    })
    addSpring(up_fra)
    fipo_gra <- ggraphics(width = 600, height = 400, container = up_g, expand = TRUE)
    visible(temp_dia) <- TRUE

    my_project$fleet$plotSpeedDepth(which_year = svalue(yea_drop),
                                    speed_range = unlist(lapply(spe_lay[1:2,2], svalue)),
                                    depth_range = unlist(lapply(dep_lay[1:2,2], svalue)))
  })

  addSpring(eff_g_top1b)

  gbutton("View Stats", container = eff_g_top1b, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting fishing points data summary..."
    Sys.sleep(1)
    my_project$fleet$plotFishPoinStat()
    svalue(stat_bar) <- ""
  })
  # addSpring(eff_g_top1b)
  addSpring(eff_g_top)

  addSpring(eff_g_top)

  eff_g_top2 <- gframe(text = "View", horizontal = TRUE, container = eff_g_top, expand = TRUE)
  addSpring(eff_g_top2)
  effvie_drop <- gcombobox(items = "Year", selected = 1, container = eff_g_top2, expand = TRUE, editable = FALSE)
  addSpring(eff_g_top2)

  eff_g_top2_ver <- ggroup(horizontal = FALSE, container = eff_g_top2)
  addSpring(eff_g_top2_ver)
  gbutton("Raw Effort", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting raw effort..."
    Sys.sleep(1)
    my_project$ggplotRawPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""
  })
  addSpring(eff_g_top2_ver)
  gbutton("Fishing Points", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting fishing points..."
    Sys.sleep(1)
    my_project$ggplotFishingPoints(svalue(effvie_drop))
    svalue(stat_bar) <- ""
  })
  addSpring(eff_g_top2_ver)
  gbutton("Gridded Effort", container = eff_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+5])
    svalue(stat_bar) <- "Plotting gridded effort..."
    Sys.sleep(1)
    my_project$ggplotGridEffort(svalue(effvie_drop))
    svalue(stat_bar) <- ""
  })
  addSpring(eff_g_top2_ver)
  addSpring(eff_g_top2)
  # addSpring(eff_g_top1c)
  addSpring(eff_g_top)
  eff_p <- ggraphics(container = eff_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Fishing Grounds   #####################################
  ##############################################################

  fig_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Fishing Grounds")
  fig_g_top <- gframe(horizontal = TRUE, container = fig_g)
  #   addSpace(fig_g_top, 2)
  #   clus_data <- NULL

  fig_g_top_vars <- gframe(text = "Variables and Weights", horizontal = FALSE, container = fig_g_top, expand = TRUE)
  addSpring(fig_g_top_vars)
  gbutton("\nSelect\n", container = fig_g_top_vars, handler = function(h,...){

    temp_dia <- gwindow(title="Select variables and weights", visible = FALSE,
                        parent = main_win, width = 550, height = 400)

    up_g_top <- ggroup(horizontal = FALSE, container = temp_dia)
    addSpring(up_g_top)
    up_g <- gframe(text = "Variables", horizontal = TRUE, container = up_g_top)

    addSpring(up_g)

    # speVars_gru <- ggroup(horizontal = TRUE, container = up_g)
    lyt <- glayout(container = up_g)

    lyt[1,2] <- "Variables"
    lyt[1,3] <- "      Weights      "
    lyt[2,2] <- gcheckbox(c("Sampling Distribution"), checked = TRUE, container = lyt)
    lyt[2,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[3,2] <- gcheckbox(c("Seabed"), checked = TRUE, container = lyt)
    lyt[3,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[4,2] <- gcheckbox(c("Fishing Effort"), checked = TRUE, container = lyt)
    lyt[4,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)
    lyt[5,2] <- gcheckbox(c("Bathymetry"), checked = TRUE, container = lyt)
    lyt[5,3] <- gcombobox(items = c("0.5X","1","2X"), selected = 2, container = lyt)

    addSpring(up_g)
    addSpring(up_g_top)
    bot_g <- ggroup(horizontal = TRUE, container = up_g_top)
    addSpring(bot_g)

    gbutton("\nAccept\n", container = bot_g,
            handler = function(h,...){
              clus_data <- numeric(length = my_project$sampMap$nCells)
              cat("\nInput for Fishing Grounds Clustering:")
              if(svalue(lyt[2,2])){ ### Resource
                cat("\n   -   Resource Distribution")
                tmp_res <- cbind(apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,1,,1], 1, sum),
                                 apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,2,,1], 1, sum),
                                 apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,3,,1], 1, sum),
                                 apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,1,,2], 1, sum),
                                 apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,2,,2], 1, sum),
                                 apply(my_project$surveyBySpecie[[1]]$Coh_A_Int[,3,,2], 1, sum))
                multi_fac_res <- switch(svalue(lyt[2,3]), "0.5X" = 0.5, "1" = 1, "2X" = 2)
                clus_data <- cbind(clus_data,
                                   tmp_res*multi_fac_res)
              }
              if(svalue(lyt[3,2])){ ### SeaBed
                cat("\n   -   Seabed Category")
                multi_fac_bed <- switch(svalue(lyt[3,3]), "0.5X" = 0.5, "1" = 1, "2X" = 2)
                clus_data <- cbind(clus_data, my_project$sampMap$bioDF*multi_fac_bed)
              }
              if(svalue(lyt[4,2])){ ### Effort
                cat("\n   -   Effort Distribution")
                multi_fac_eff <- switch(svalue(lyt[4,3]), "0.5X" = 0.5, "1" = 1, "2X" = 2)
                for(i in names(my_project$fleet$rawEffort)){
                  tmp_effo <- as.data.frame(table(my_project$fleet$rawEffort[[i]]$Cell[which(my_project$fleet$rawEffort[[i]]$FishPoint)]))
                  names(tmp_effo) <- c("Cell", "Freq")
                  tmp_effo$Cell <- as.numeric(as.character(tmp_effo$Cell))
                  miss_rows <- as.numeric(setdiff(as.character(my_project$sampMap$gridShp@plotOrder), as.character(tmp_effo$Cell)))
                  if(length(miss_rows) > 0){
                    # cat(length(miss_rows), " cells with no points... ", sep = "")
                    tmp_effo <- rbind(tmp_effo, data.frame(Cell = miss_rows, Freq = 0))
                    tmp_effo <- tmp_effo[order(tmp_effo[,1]),]
                  }
                  clus_data <- cbind(clus_data, tmp_effo[,2]*multi_fac_eff)
                  colnames(clus_data)[ncol(clus_data)] <- paste("Year_", i, sep = "")
                }
              }
              if(svalue(lyt[5,2])){ ### Bathymetry
                cat("\n   -   Bathymetry\n")
                multi_fac_dep <- switch(svalue(lyt[5,3]), "0.5X" = 0.5, "1" = 1, "2X" = 2)
                tmp_depth <- as.data.frame(my_project$sampMap$centDept[,3])
                names(tmp_depth) <- "Depth"
                clus_data <- cbind(clus_data, tmp_depth*multi_fac_dep)
              }
              clus_data <- clus_data[,-1]
              my_project$sampMap$setClusInpu(clus_data)
              dispose(temp_dia)
            })
    addSpring(bot_g)
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
    dev.set(dev.list()[pre_dev+6])

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
                            dev.set(dev.list()[pre_dev+6])
                            # my_project$sampMap$plotFishGrou(svalue(fg_plotCut))
                            my_project$sampMap$setCutResult(ind_clu = svalue(fg_plotCut))

                            suppressWarnings(grid.arrange(my_project$sampMap$ggIchFGlin,
                                                          my_project$sampMap$ggSilFGlin,
                                                          my_project$sampMap$ggCutFGmap,
                                                          my_project$sampMap$ggDepthFGbox,
                                                          my_project$sampMap$ggEffoFGbox,
                                                          my_project$sampMap$ggEffoFGmap,
                                                          my_project$sampMap$ggBioFGmat,
                                                          layout_matrix = rbind(c(1,3,3,4,5,7),c(2,6,6,4,5,7))))

                          })

  addSpring(fig_g_top_plot)

  addSpring(fig_g_top)

  gbutton("    Select\n      this\nPartitioning", container = fig_g_top, handler = function(h,...){
    my_project$setFishGround(numCut = svalue(fg_plotCut))
    my_project$sampMap$setCutResult(ind_clu = svalue(fg_plotCut))

  })
  addSpring(fig_g_top)

  addSpace(fig_g_top, 2)
  fisGro_p <- ggraphics(container = fig_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Register   ############################################
  ##############################################################

  reg_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Register")
  reg_g_top <- gframe(horizontal = TRUE, container = reg_g)
  addSpace(reg_g_top, 2)
  addSpring(reg_g_top)
  gbutton("Load EU register", container = reg_g_top, handler = function(h,...){
    my_project$fleet$loadFleetRegis("/Users/Lomo/Documents/Uni/Lab/Data/FLEET/ITA_export.csv")
    my_project$fleet$cleanRegister()
    # my_project$fleet$splitFleet()
    dev.set(dev.list()[pre_dev+7])
    my_project$fleet$plotRegSum()

    ### Update Register Status
    if(!is.null(my_project$fleet$rawRegister)){
      delete(regi_g, regi_g$children[[length(regi_g$children)]])
      add(regi_g, regi_sta_n)
    }
  })
  addSpring(reg_g_top)
  gbutton("View Fleet", container = reg_g_top, handler = function(h,...){
    temp_flee <- gbasicdialog(title="Explore Fleet Register", parent = main_win)
    tmp_data <- my_project$fleet$rawRegister
    tmp_data[which(is.na(tmp_data), arr.ind = TRUE)] <- "NA"
    flee_data <- gtable(tmp_data, container = temp_flee)
    size(temp_flee) <- c(600, 400)
    visible(temp_flee)
  })
  addSpring(reg_g_top)
  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"),
         container = reg_g_top, handler = function(h,...){
           dev.set(dev.list()[pre_dev+7])
           my_project$fleet$plotRegSum()
         })
  addSpring(reg_g_top)
  addSpace(reg_g_top, 2)
  regGro_p <- ggraphics(container = reg_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Production   ##########################################
  ##############################################################

  pro_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Production")
  pro_g_top <- gframe(horizontal = TRUE, container = pro_g)
  addSpace(pro_g_top, 2)
  addSpring(pro_g_top)
  pro_g_top1 <- ggroup(horizontal = FALSE, container = pro_g_top)
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

    dev.set(dev.list()[pre_dev+8])
    my_project$fleet$rawProduction <- readRDS("/Users/Lomo/Documents/Uni/PhD/TESI/landings/LandAll.rData")

    my_project$fleet$setProdIds()

    # my_project$fleet$plotCountIDsProd()

    my_project$fleet$setIdsEffoProd()

    my_project$fleet$plotCountIDsEffoProd()

    my_project$fleet$setProdMatr()
    my_project$fleet$setDayEffoMatrGround()
    my_project$fleet$setEffoProdMatr()
    my_project$fleet$setEffoProdMont()
    my_project$fleet$setProdSpec()
    my_project$fleet$setEffoProdAll()
    # my_project$fleet$setSpecSett()
  })
  addSpring(pro_g_top1)

  gbutton("Set Landings\nThresholds", container = pro_g_top1, handler = function(h,...){

    temp_dia <- gwindow(title="Set Landings Threshold", visible = FALSE,
                        parent = main_win,
                        width = 900, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    # addSpring(up_fra)
    addSpace(up_fra, 20)
    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpace(spe_fra, 20)
    spe_drop <- gcombobox(sort(my_project$fleet$prodSpec[["Cross"]]), selected = 1,
                          editable = FALSE, container = spe_fra, expand = TRUE,
                          handler = function(...){
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
    addSpace(spe_fra, 20)
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
  addSpring(pro_g_top1)
  addSpring(pro_g_top)

  pro_g_top2 <- ggroup(horizontal = FALSE, container = pro_g_top)
  addSpring(pro_g_top2)
  gbutton("Get Logit", container = pro_g_top2, handler = function(h,...){

    temp_dia <- gwindow(title="Get Logit", visible = FALSE,
                        parent = main_win,
                        width = 900, height = 500)

    up_g <- ggroup(horizontal = FALSE, container = temp_dia)
    up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
    # addSpring(up_fra)
    addSpace(up_fra, 20)
    spe_fra <- gframe(text = "Specie", container = up_fra, horizontal = TRUE, expand = TRUE)
    addSpace(spe_fra, 20)
    spe_drop <- gcombobox(sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))]), selected = 1,
                          editable = FALSE, container = spe_fra, expand = TRUE,
                          handler = function(...){
                            if(!is.null(my_project$fleet$specLogit[[svalue(spe_drop)]]$ROCRperf)){
                              my_project$fleet$plotLogitROC(svalue(spe_drop))
                              svalue(tmp_txt) <- capture.output({cat("\n")
                                print(my_project$fleet$specLogit[[svalue(spe_drop)]]$confMatrix)})
                            }
                          })
    addSpace(spe_fra, 20)
    addSpace(up_fra, 20)
    # addSpring(up_fra)

    gbutton(text = "Get\nLogit", container = up_fra, handler = function(...){
      my_project$fleet$setSpecLogit(svalue(spe_drop))
      my_project$fleet$plotLogitROC(svalue(spe_drop))

      svalue(thr_spin) <- round(my_project$fleet$specLogit[[svalue(spe_drop)]]$optCut, 2)
      svalue(tmp_txt) <- capture.output({cat("\n")
        print(my_project$fleet$specLogit[[svalue(spe_drop)]]$confMatrix)})
    })
    addSpace(up_fra, 20)

    thr_fra <- gframe(text = "Tune Cutoff", container = up_fra, expand = TRUE, horizontal = TRUE)
    addSpace(thr_fra, 20)
    thr_spin <- gslider(from = 0.01, to = 0.99,
                        by = 0.01, value = 0.5, container = thr_fra, expand = TRUE,
                        handler = function(...){
                          if(!is.null(my_project$fleet$specLogit[[svalue(spe_drop)]])){
                            my_project$fleet$setSpecLogitConf(specie = svalue(spe_drop), cutoff = svalue(thr_spin))
                            svalue(tmp_txt) <- capture.output({cat("\n")
                              print(my_project$fleet$specLogit[[svalue(spe_drop)]]$confMatrix)})
                          }
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

    addSpring(up_fra)
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
    addSpace(spe_fra, 20)
    spe_drop <- gcombobox(sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))]), selected = 1,
                          editable = FALSE, container = spe_fra, expand = TRUE,
                          handler = function(...){

                          })
    addSpace(spe_fra, 20)
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
      provie_drop[] <- names(my_project$fleet$effoProdMont)
      prospe_drop[] <- sort(names(my_project$fleet$specSett)[which(!unlist(lapply(my_project$fleet$specSett, is.null)))])
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

  pro_g_top3 <- ggroup(horizontal = FALSE, container = pro_g_top)
  gbutton("Predict\nProduction", container = pro_g_top3, handler = function(h,...){

    my_project$fleet$setEffoMont()
    my_project$fleet$setEffoAll()
    my_project$fleet$setEffoAllLoa()
    my_project$predictProduction(svalue(prospe_drop))
    my_project$fleet$setProdMeltYear(svalue(prospe_drop))

  })


  addSpring(pro_g_top3)
  addSpring(pro_g_top)

  pro_g_top2_view <- gframe(text = "View", horizontal = TRUE, container = pro_g_top, expand = TRUE)
  addSpring(pro_g_top2_view)
  provie_drop <- gcombobox(items = "Year", selected = 1, container = pro_g_top2_view,
                           expand = TRUE, editable = FALSE)
  prospe_drop <- gcombobox(items = "Specie", selected = 1, container = pro_g_top2_view,
                           expand = TRUE, editable = FALSE)
  addSpring(pro_g_top2_view)
  pro_g_top2_ver <- ggroup(horizontal = FALSE, container = pro_g_top2_view)
  addSpring(pro_g_top2_ver)
  gbutton("Betas", container = pro_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+8])
    svalue(stat_bar) <- "Plotting productivity..."
    Sys.sleep(1)
    my_project$setPlotBetaMeltYear(specie = svalue(prospe_drop), year = svalue(provie_drop))
    suppressWarnings(grid.arrange(my_project$sampMap$ggBetaFGmap,
                                  my_project$sampMap$ggBetaFGbox,
                                  layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))
    svalue(stat_bar) <- ""
  })
  addSpring(pro_g_top2_ver)
  gbutton("Production", container = pro_g_top2_ver, handler = function(h,...){
    dev.set(dev.list()[pre_dev+8])
    svalue(stat_bar) <- "Plotting production..."
    Sys.sleep(1)

    my_project$setPlotProdMeltYear(specie = svalue(prospe_drop), year = svalue(provie_drop))
    suppressWarnings(grid.arrange(my_project$sampMap$ggProdFGmap,
                                  my_project$sampMap$ggProdFGbox,
                                  layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2))))

    svalue(stat_bar) <- ""
  })
  addSpring(pro_g_top2_ver)
  addSpring(pro_g_top2_view)

  addSpring(pro_g_top)
  addSpace(pro_g_top, 2)
  proGro_p <- ggraphics(container = pro_g, width = 600, height = 280, expand = TRUE)


  ##############################################################
  ####   Selectivity   #########################################
  ##############################################################

  sel_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Selectivity")
  sel_g_top <- gframe(horizontal = TRUE, container = sel_g)
  addSpace(sel_g_top, 2)
  addSpring(sel_g_top)
  sel_g_top1 <- ggroup(horizontal = FALSE, container = sel_g_top)
  addSpring(sel_g_top1)
  gbutton("Edit Raw Selectivity", container = sel_g_top1, handler = function(h,...){

  })
  gbutton("Load Data", container = sel_g_top1, handler = function(h,...){

  })
  addSpring(sel_g_top1)
  addSpring(sel_g_top)
  gimage(system.file("ico/view-refresh-5_big.ico", package="smartR"),
         container = sel_g_top)
  addSpring(sel_g_top)
  addSpace(sel_g_top, 2)



  ####   Prediction   ##############################################################


  pre_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Prediction")



  ####   Simulation   ##############################################################


  sim_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Simulation")

  visible(pro_eg) <- TRUE
  visible(raw_eg) <- TRUE
  visible(eff_eg) <- TRUE
  visible(pre_eg) <- TRUE
  visible(sim_eg) <- TRUE
  visible(main_win) <- TRUE

  svalue(uti_gn) <- 2
  svalue(uti_gn) <- 5
  svalue(uti_gn) <- 6
  svalue(uti_gn) <- 7
  svalue(uti_gn) <- 8
  svalue(uti_gn) <- 9
  svalue(uti_gn) <- 10
  svalue(uti_gn) <- 11
  svalue(uti_gn) <- 12
  svalue(uti_gn) <- 13
  svalue(uti_gn) <- 14
  svalue(uti_gn) <- 1
  # visible(main_win) <- TRUE

}

###########################################

