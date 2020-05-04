
#' smartR GUI
#'
#' The \code{smartRgui} function implements the main graphical user interface
#' of smartR. Please, contact the package maintainer for detailed instructions.
#'
#' @param smartRstudy SmartProject class
#'
#' @return This function does not return a value.
#'
#' @examples
#' \dontrun{
#' yourSmartRstudy <- SmartProject$new()
#' smartRgui(smartRstudy = yourSmartRstudy)
#' }

smartRgui <- function(smartRstudy = NULL) {
  if (is.null(smartRstudy)) {
    stop("Initialize smartRstudy first!\n
         yourSmartRstudy <- SmartProject$new()\n
         smartRgui(smartRstudy = yourSmartRstudy)", call. = FALSE)
  } else {
    smartRstudy$createFleet()
    
    logoPNG <- readJPEG(source = system.file("smartRlogo.jpg",
                                             package = "smartR"
    ))
    pre_dev <- length(dev.list())
    
    main_win <- gwindow(paste("SMART - Version ", packageVersion("smartR"), sep = ""),
                        width = 1200,
                        height = 600, visible = TRUE
    )
    big_g <- ggroup(horizontal = TRUE, container = main_win)
    addSpace(big_g, 10)
    
    rig_g <- ggroup(horizontal = FALSE, container = big_g, expand = TRUE)
    uti_gn <- gnotebook(tab.pos = 3, container = rig_g, expand = TRUE)
    
    ####   Project   ####
    pro_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Project")
    pro_g_top <- gframe(horizontal = TRUE, container = pro_g)
    addSpace(pro_g_top, 2)
    addSpring(pro_g_top)
    gbutton("New", container = pro_g_top)
    addSpring(pro_g_top)
    gbutton("Load", container = pro_g_top, handler = function(h, ...) {
      load_path <- gfile(
        text = "Select Smart_Project file", type = "open",
        filter = list("R files" = list(patterns = c("*.rData")))
      )
      smartRstudy <- readRDS(load_path)
      
      ### Update Sampling Status
      
      if (!is.null(smartRstudy$rawDataSurvey)) {
        raw_t[] <- smartRstudy$rawDataSurvey[sample(1:nrow(smartRstudy$rawDataSurvey),
                                                    100,
                                                    replace = FALSE
        ), ]
        svalue(raw_l1) <- paste("Species: ", paste(smartRstudy$specieInSurvey,
                                                   collapse = " - "
        ))
        svalue(raw_l3) <- paste(
          "Years: from",
          min(as.numeric(as.character(smartRstudy$yearInSurvey))),
          " to ",
          max(as.numeric(as.character(smartRstudy$yearInSurvey)))
        )
        spec_drop_mix[] <- smartRstudy$specieInSurvey
        # spevie_drop[] <- c("All", smartRstudy$specieInSurvey)
        # cohSpe_drop[] <- smartRstudy$specieInSurvey
        # svalue(spec_drop) <- smartRstudy$specieInSurvey[1]   ##  droplist from population tab
        # svalue(cohSpe_drop) <- smartRstudy$specieInSurvey[1]
        # svalue(spevie_drop) <- "All"
        svalue(spec_drop_mix) <- smartRstudy$specieInSurvey[1]
        year_drop[] <- c("All", as.character(smartRstudy$yearInSurvey))
        cohYea_drop[] <- c("All", as.character(smartRstudy$yearInSurvey))
        svalue(year_drop) <- smartRstudy$yearInSurvey[1]
        svalue(cohYea_drop) <- "All"
        
        svalue(n_year_s) <- paste(length(smartRstudy$yearInSurvey),
                                  " years",
                                  sep = ""
        )
        svalue(mi_date_s) <- paste("From: ",
                                   min(as.numeric(as.character(smartRstudy$yearInSurvey))),
                                   sep = ""
        )
        svalue(ma_date_s) <- paste("To: ",
                                   max(as.numeric(as.character(smartRstudy$yearInSurvey))),
                                   sep = ""
        )
        svalue(n_spec_s) <- paste(length(smartRstudy$specieInSurvey),
                                  " species",
                                  sep = ""
        )
        #       samp_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
        delete(samp_g, samp_g$children[[length(samp_g$children)]])
        add(samp_g, samp_sta_n)
      }
      
      ### Update grid Status
      
      if (!is.null(smartRstudy$sampMap)) {
        svalue(n_cell_g) <- paste("N. Cells: ", smartRstudy$sampMap$nCells)
        delete(grid_g, grid_g$children[[length(grid_g$children)]])
        add(grid_g, grid_sta_n)
      }
      
      ### Update Effort Status
      
      if (!is.null(smartRstudy$fleet$rawEffort)) {
        effvie_drop[] <- c("All", colnames(smartRstudy$fleet$rawEffort))
        svalue(effvie_drop) <- "All"
        delete(effo_g, effo_g$children[[length(effo_g$children)]])
        add(effo_g, effo_sta_n)
      }
      
      ### Update Fishing Grounds Status
      
      if (!is.null(smartRstudy$sampMap$clusMat)) {
        fg_plotCut[] <- 1:ncol(smartRstudy$sampMap$clusMat)
        svalue(fg_plotCut) <- which.max(smartRstudy$sampMap$indSil)
        #       figr_sta_n <- gimage(system.file("ico/user-available.png", package="smartR"))
        delete(figr_g, figr_g$children[[length(figr_g$children)]])
        add(figr_g, figr_sta_n)
      }
      
      ### Update Register Status
      
      if (!is.null(smartRstudy$fleet$rawRegister)) {
        delete(regi_g, regi_g$children[[length(regi_g$children)]])
        add(regi_g, regi_sta_n)
      }
    })
    addSpring(pro_g_top)
    gbutton("Save", container = pro_g_top, handler = function(h, ...) {
      save_dest <- gfile(
        text = "Select file name and destination directory",
        type = "save",
        initial.filename = "Smart_Project.rData",
        initial.dir = smartRstudy$sampMap$gridPath
      )
      if (rev(unlist(strsplit(save_dest, "[.]")))[1] != "rData") {
        save_dest <- paste(save_dest, ".rData", sep = "")
      }
      saveRDS(smartRstudy, save_dest)
    })
    addSpring(pro_g_top)
    addSpace(pro_g_top, 2)
    
    pro_g_mid <- gframe(text = "Data", horizontal = TRUE, container = pro_g)
    addSpace(pro_g_mid, 2)
    
    addSpring(pro_g_mid)
    grid_g <- gframe(
      text = "Environment", horizontal = FALSE,
      container = pro_g_mid
    )
    n_cell_g <- glabel("   ---", container = grid_g)
    addSpring(grid_g)
    grid_b <- gbutton(
      text = "Show data", container = grid_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 2
      }
    )
    grid_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    grid_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(grid_g, grid_sta)
    
    addSpring(pro_g_mid)
    effo_g <- gframe(text = "Effort", horizontal = FALSE, container = pro_g_mid)
    addSpring(effo_g)
    effo_b <- gbutton(
      text = "Show data", container = effo_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 3
      }
    )
    effo_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    effo_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(effo_g, effo_sta)
    
    addSpring(pro_g_mid)
    figr_g <- gframe(
      text = "Fishing Ground", horizontal = FALSE,
      container = pro_g_mid
    )
    addSpring(figr_g)
    figr_b <- gbutton(
      text = "Show data", container = figr_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 4
      }
    )
    figr_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    figr_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(figr_g, figr_sta)
    
    addSpring(pro_g_mid)
    regi_g <- gframe(
      text = "Register", horizontal = FALSE,
      container = pro_g_mid
    )
    addSpring(regi_g)
    regi_b <- gbutton(
      text = "Show data", container = regi_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 5
      }
    )
    regi_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    regi_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(regi_g, regi_sta)
    
    addSpring(pro_g_mid)
    prod_g <- gframe(
      text = "Production", horizontal = FALSE,
      container = pro_g_mid
    )
    addSpring(prod_g)
    prod_b <- gbutton(
      text = "Show data", container = prod_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 6
      }
    )
    prod_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    add(prod_g, prod_sta)
    
    addSpring(pro_g_mid)
    samp_g <- gframe(text = "Survey", horizontal = FALSE, container = pro_g_mid)
    n_year_s <- glabel("   ---", container = samp_g)
    mi_date_s <- glabel("", container = samp_g)
    ma_date_s <- glabel("", container = samp_g)
    n_spec_s <- glabel("   ---", container = samp_g)
    addSpring(samp_g)
    samp_b <- gbutton(
      text = "Show data", container = samp_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 7
      }
    )
    samp_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    samp_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(samp_g, samp_sta)
    
    addSpring(pro_g_mid)
    fish_g <- gframe(text = "Fishery", horizontal = FALSE, container = pro_g_mid)
    n_yearF_s <- glabel("   ---", container = fish_g)
    mi_dateF_s <- glabel("", container = fish_g)
    ma_dateF_s <- glabel("", container = fish_g)
    n_specF_s <- glabel("   ---", container = fish_g)
    addSpring(fish_g)
    fish_b <- gbutton(
      text = "Show data", container = fish_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 8
      }
    )
    fish_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    fish_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(fish_g, fish_sta)
    
    addSpring(pro_g_mid)
    mixt_g <- gframe(text = "Mixture", horizontal = FALSE, container = pro_g_mid)
    addSpring(mixt_g)
    mixt_b <- gbutton(
      text = "Show data", container = mixt_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 9
      }
    )
    mixt_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    mixt_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(mixt_g, mixt_sta)
    
    addSpring(pro_g_mid)
    simu_g <- gframe(
      text = "Simulation", horizontal = FALSE,
      container = pro_g_mid
    )
    addSpring(simu_g)
    simu_b <- gbutton(
      text = "Show data", container = simu_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 11
      }
    )
    simu_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    simu_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(simu_g, simu_sta)
    
    addSpring(pro_g_mid)
    asse_g <- gframe(
      text = "Assessment", horizontal = FALSE,
      container = pro_g_mid
    )
    addSpring(asse_g)
    asse_b <- gbutton(
      text = "Show data", container = asse_g,
      handler = function(h, ..) {
        svalue(uti_gn) <- 12
      }
    )
    asse_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    asse_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    add(asse_g, asse_sta)
    
    
    addSpring(pro_g_mid)
    addSpace(pro_g_mid, 2)
    
    pro_p <- ggraphics(
      container = pro_g, width = 550, height = 250,
      expand = TRUE
    )
    
    ####   Environment   ####
    env1_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    env1_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    env2_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    env2_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    env3_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    env3_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    
    gri_g <- gvbox(container = uti_gn, label = "Environment", expand = TRUE)
    gri_g_top <- gframe(horizontal = TRUE, container = gri_g)
    addSpring(gri_g_top)
    gri_g_top1_gri <- gframe("Grid", horizontal = TRUE, container = gri_g_top)
    addSpace(gri_g_top1_gri, 10)
    gbutton("Load", container = gri_g_top1_gri, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          tmpGridfile <- gfile(
            text = "Select Grid Shapefile", type = "open",
            initial.filename = NULL, initial.dir = getwd(),
            filter = list(),
            multi = FALSE
          )
          
          svalue(stat_bar) <- "Loading Grid..."
          Sys.sleep(1)
          dev.set(dev.list()[pre_dev + 2])
          smartRstudy$loadMap(tmpGridfile)
          smartRstudy$sampMap$getGooMap()
          svalue(stat_bar) <- "Downloading Google map..."
          smartRstudy$sampMap$setGooGrid()
          smartRstudy$sampMap$setGooBbox()
          svalue(stat_bar) <- "Plotting..."
          smartRstudy$sampMap$setGooEnv()
          smartRstudy$sampMap$plotGooEnv()
          
          ### Update Grid Status
          svalue(n_cell_g) <- paste("N. Cells: ", smartRstudy$sampMap$nCells)
          delete(grid_g, grid_g$children[[length(grid_g$children)]])
          add(grid_g, grid_sta_n)
          delete(
            gri_g_top1_gri,
            gri_g_top1_gri$children[[length(gri_g_top1_gri$children)]]
          )
          add(gri_g_top1_gri, env1_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(gri_g_top1_gri, 10)
    gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
           container = gri_g_top1_gri,
           handler = function(h, ...) {
             enabled(gri_g_top) <- FALSE
             tryCatch(
               expr = {
                 dev.set(dev.list()[pre_dev + 2])
                 svalue(stat_bar) <- "Plotting..."
                 Sys.sleep(1)
                 smartRstudy$sampMap$setGooEnv()
                 smartRstudy$sampMap$plotGooEnv()
               },
               error = function(error_message) {
                 message("An error has occurred!")
                 message(error_message)
               },
               finally = {
                 enabled(gri_g_top) <- TRUE
                 svalue(stat_bar) <- ""
               }
             )
           }
    )
    addSpace(gri_g_top1_gri, 10)
    add(gri_g_top1_gri, env1_sta)
    
    addSpace(gri_g_top, 20)
    gri_g_top1_dep <- gframe("Depth", horizontal = TRUE, container = gri_g_top)
    addSpace(gri_g_top1_dep, 10)
    gbutton("Download", container = gri_g_top1_dep, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          dev.set(dev.list()[pre_dev + 2])
          svalue(stat_bar) <- "Downloading depth..."
          Sys.sleep(1)
          smartRstudy$sampMap$getGridBath()
          svalue(stat_bar) <- "Plotting..."
          Sys.sleep(1)
          smartRstudy$sampMap$setGooEnv()
          smartRstudy$sampMap$plotGooEnv()
          delete(
            gri_g_top1_dep,
            gri_g_top1_dep$children[[length(gri_g_top1_dep$children)]]
          )
          add(gri_g_top1_dep, env2_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(gri_g_top1_dep, 10)
    gbutton(" Save ", container = gri_g_top1_dep, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          if (!is.null(smartRstudy$sampMap$gridBathy)) {
            Sys.sleep(1)
            tmpOutBathy <- gfile(
              text = "Select output data name", type = "save",
              initial.filename = "Smart_Bathy_Matrix.RDS",
              initial.dir = smartRstudy$sampMap$gridPath
            )
            svalue(stat_bar) <- "Saving..."
            if (rev(unlist(strsplit(tmpOutBathy, "[.]")))[1] != "RDS") {
              tmpOutBathy <- paste(tmpOutBathy, ".RDS", sep = "")
            }
            Sys.sleep(1)
            smartRstudy$sampMap$saveGridBath(tmpOutBathy)
          }
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(gri_g_top1_dep, 10)
    gbutton("Load", container = gri_g_top1_dep, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          dev.set(dev.list()[pre_dev + 2])
          tmpBathyfile <- gfile(
            text = "Select Bathymetry Matrix", type = "open",
            initial.filename = NULL, initial.dir = getwd(),
            filter = list(),
            multi = FALSE
          )
          svalue(stat_bar) <- "Loading depth..."
          Sys.sleep(1)
          smartRstudy$sampMap$loadGridBath(tmpBathyfile)
          svalue(stat_bar) <- "Plotting..."
          Sys.sleep(1)
          smartRstudy$sampMap$setGooEnv()
          smartRstudy$sampMap$plotGooEnv()
          delete(
            gri_g_top1_dep,
            gri_g_top1_dep$children[[length(gri_g_top1_dep$children)]]
          )
          add(gri_g_top1_dep, env2_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(gri_g_top1_dep, 10)
    gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
           container = gri_g_top1_dep,
           handler = function(h, ...) {
             enabled(gri_g_top) <- FALSE
             tryCatch(
               expr = {
                 dev.set(dev.list()[pre_dev + 2])
                 svalue(stat_bar) <- "Plotting..."
                 Sys.sleep(1)
                 smartRstudy$sampMap$setGooEnv()
                 smartRstudy$sampMap$plotGooEnv()
               },
               error = function(error_message) {
                 message("An error has occurred!")
                 message(error_message)
               },
               finally = {
                 enabled(gri_g_top) <- TRUE
                 svalue(stat_bar) <- ""
               }
             )
           }
    )
    addSpace(gri_g_top1_dep, 10)
    add(gri_g_top1_dep, env2_sta)
    
    addSpace(gri_g_top, 20)
    gri_g_top1_bio <- gframe("Seabed", horizontal = TRUE, container = gri_g_top)
    addSpace(gri_g_top1_bio, 10)
    gbutton("Load", container = gri_g_top1_bio, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          tmpBiocfile <- gfile(
            text = "Select Biocenonsis Matrix", type = "open",
            initial.filename = NULL, initial.dir = getwd(),
            filter = list(),
            multi = FALSE
          )
          Sys.sleep(1)
          dev.set(dev.list()[pre_dev + 2])
          svalue(stat_bar) <- "Loading biocenosis data.frame..."
          smartRstudy$sampMap$loadBioDF(tmpBiocfile)
          svalue(stat_bar) <- "Plotting..."
          Sys.sleep(1)
          smartRstudy$sampMap$setGooEnv()
          smartRstudy$sampMap$plotGooEnv()
          delete(
            gri_g_top1_bio,
            gri_g_top1_bio$children[[length(gri_g_top1_bio$children)]]
          )
          add(gri_g_top1_bio, env3_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(gri_g_top1_bio, 10)
    gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
           container = gri_g_top1_bio,
           handler = function(h, ...) {
             enabled(gri_g_top) <- FALSE
             tryCatch(
               expr = {
                 dev.set(dev.list()[pre_dev + 2])
                 svalue(stat_bar) <- "Plotting..."
                 Sys.sleep(1)
                 smartRstudy$sampMap$setGooEnv()
                 smartRstudy$sampMap$plotGooEnv()
               },
               error = function(error_message) {
                 message("An error has occurred!")
                 message(error_message)
               },
               finally = {
                 enabled(gri_g_top) <- TRUE
                 svalue(stat_bar) <- ""
               }
             )
           }
    )
    addSpace(gri_g_top1_bio, 10)
    add(gri_g_top1_bio, env3_sta)
    
    addSpring(gri_g_top)
    gri_g_top2 <- gframe(
      text = "Environmental Asset", horizontal = TRUE,
      container = gri_g_top
    )
    addSpring(gri_g_top2)
    gbutton("Export", container = gri_g_top2, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          Sys.sleep(1)
          tmpOutEnvfiles <- gfile(
            text = "Select output data name", type = "save",
            initial.filename = "Smart_EnvAsset_List.RDS",
            initial.dir = smartRstudy$sampMap$gridPath
          )
          svalue(stat_bar) <- "Saving..."
          if (rev(unlist(strsplit(tmpOutEnvfiles, "[.]")))[1] != "RDS") {
            tmpOutEnvfiles <- paste(tmpOutEnvfiles, ".RDS", sep = "")
          }
          Sys.sleep(1)
          saveRDS(smartRstudy$sampMap$exportEnv(), tmpOutEnvfiles)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpring(gri_g_top2)
    gbutton("Import", container = gri_g_top2, handler = function(h, ...) {
      enabled(gri_g_top) <- FALSE
      tryCatch(
        expr = {
          dev.set(dev.list()[pre_dev + 2])
          Sys.sleep(1)
          tmpInEnvfiles <- gfile(
            text = "Select Environment Asset file",
            type = "open",
            initial.filename = NULL, initial.dir = getwd()
          )
          svalue(stat_bar) <- "Loading..."
          Sys.sleep(1)
          smartRstudy$importEnv(readRDS(tmpInEnvfiles))
          smartRstudy$sampMap$setGooEnv()
          smartRstudy$sampMap$plotGooEnv()
          delete(
            gri_g_top1_gri,
            gri_g_top1_gri$children[[length(gri_g_top1_gri$children)]]
          )
          add(gri_g_top1_gri, env1_sta_n)
          delete(
            gri_g_top1_dep,
            gri_g_top1_dep$children[[length(gri_g_top1_dep$children)]]
          )
          add(gri_g_top1_dep, env2_sta_n)
          delete(
            gri_g_top1_bio,
            gri_g_top1_bio$children[[length(gri_g_top1_bio$children)]]
          )
          add(gri_g_top1_bio, env3_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(gri_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpring(gri_g_top2)
    addSpace(gri_g_top, 10)
    gri_p <- ggraphics(
      container = gri_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Effort   ####
    eff1_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    eff1_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    eff2_sta <- gimage(system.file("ico/user-invisible.png", package = "smartR"))
    eff2_sta_n <- gimage(system.file("ico/user-available.png", package = "smartR"))
    
    eff_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Effort")
    eff_g_top <- gframe(horizontal = TRUE, container = eff_g)
    addSpace(eff_g_top, 2)
    addSpring(eff_g_top)
    eff_g_top1 <- gframe(text = "Load", horizontal = TRUE, container = eff_g_top)
    addSpace(eff_g_top1, 10)
    gbutton("from vmsbase DB", container = eff_g_top1, handler = function(h, ...) {
      enabled(eff_g_top) <- FALSE
      tryCatch(
        expr = {
          tmpVMSfiles <- gfile(
            text = "Select Effort DBs", type = "open",
            initial.filename = NULL, initial.dir = getwd(),
            filter = list(),
            multi = TRUE
          )
          svalue(stat_bar) <- "Loading effort from vmsbase db..."
          Sys.sleep(1)
          avaMet <- sqldf("select distinct met_des from nn_clas",
                          dbname = tmpVMSfiles
          )[, 1]
          
          temp_dia <- gwindow(
            title = "Load vmsbase DB data", visible = FALSE,
            # parent = main_win,
            width = 400, height = 200
          )
          up_ho <- ggroup(horizontal = TRUE, container = temp_dia)
          addSpace(obj = up_ho, 5)
          up_g <- ggroup(horizontal = FALSE, container = up_ho, expand = TRUE)
          addSpace(obj = up_g, 5)
          up_fra <- gframe(container = up_g, horizontal = FALSE, expand = TRUE)
          addSpace(obj = up_fra, 15)
          dbs_g <- ggroup(horizontal = TRUE, container = up_fra)
          addSpace(obj = dbs_g, 10)
          pathLab <- glabel(
            text = paste0(
              "Loaded DB:\t\t",
              unlist(strsplit(tmpVMSfiles, "/"))[length(unlist(strsplit(tmpVMSfiles, "/")))]
            ),
            container = dbs_g
          )
          addSpace(obj = up_fra, 30)
          met_g <- ggroup(horizontal = TRUE, container = up_fra)
          addSpace(obj = met_g, 10)
          glabel("Select Metier:\t", container = met_g)
          addSpace(obj = met_g, 10)
          metLab <- gcombobox(items = avaMet, container = met_g, expand = TRUE)
          addSpace(obj = met_g, 10)
          addSpace(obj = up_fra, 15)
          onBox_g <- ggroup(horizontal = TRUE, container = up_fra)
          addSpace(obj = onBox_g, 10)
          glabel("% on grid:\t", container = onBox_g)
          perOn_sli <- gslider(
            from = 0, to = 100, by = 1,
            value = 90, container = onBox_g
          )
          addSpace(obj = onBox_g, 10)
          addSpace(obj = up_fra, 10)
          addSpace(obj = up_g, 5)
          gbutton(text = "Run Query", container = up_g, handler = function(...) {
            enabled(temp_dia) <- FALSE
            Sys.sleep(1)
            smartRstudy$loadFleeEffoDbs(
              effort_path = tmpVMSfiles,
              met_nam = svalue(metLab), onBox = TRUE,
              perOnBox = svalue(perOn_sli)
            )
            cat("\n\n---\tExtraction completed!\t---\n")
            smartRstudy$fleet$setEffortIds()
            svalue(stat_bar) <- ""
            effvie_drop[] <- names(smartRstudy$fleet$rawEffort)
            svalue(effvie_drop) <- names(smartRstudy$fleet$rawEffort)[1]
            dev.set(dev.list()[pre_dev + 3])
            svalue(stat_bar) <- "Plotting Count of disinct vessels..."
            Sys.sleep(1)
            smartRstudy$fleet$plotCountIDsEffo()
            delete(effo_g, effo_g$children[[length(effo_g$children)]])
            add(effo_g, effo_sta_n)
            delete(eff_g_top1, eff_g_top1$children[[length(eff_g_top1$children)]])
            add(eff_g_top1, eff1_sta_n)
            dispose(temp_dia)
          })
          addSpace(obj = up_g, 5)
          addSpace(obj = up_ho, 5)
          visible(temp_dia) <- TRUE
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(eff_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(eff_g_top1, 10)
    gbutton("from rData", container = eff_g_top1, handler = function(h, ...) {
      enabled(eff_g_top) <- FALSE
      tryCatch(
        expr = {
          tmpVMSfile <- gfile(
            text = "Select Effort rData", type = "open",
            initial.filename = NULL, initial.dir = getwd(),
            filter = list(),
            multi = TRUE
          )
          cat("\nLoading effort from rData...", sep = "")
          svalue(stat_bar) <- "Loading effort from rData..."
          Sys.sleep(1)
          smartRstudy$fleet$rawEffort <- readRDS(tmpVMSfile)
          smartRstudy$fleet$setEffortIds()
          svalue(stat_bar) <- ""
          effvie_drop[] <- names(smartRstudy$fleet$rawEffort)
          svalue(effvie_drop) <- names(smartRstudy$fleet$rawEffort)[1]
          dev.set(dev.list()[pre_dev + 3])
          svalue(stat_bar) <- "Plotting raw points..."
          smartRstudy$ggplotRawPoints(svalue(effvie_drop))
          
          ### Update Effort Status
          delete(effo_g, effo_g$children[[length(effo_g$children)]])
          add(effo_g, effo_sta_n)
          delete(eff_g_top1, eff_g_top1$children[[length(eff_g_top1$children)]])
          add(eff_g_top1, eff1_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(eff_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(eff_g_top1, 10)
    gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
           container = eff_g_top1, handler = function(h, ...) {
             enabled(eff_g_top) <- FALSE
             tryCatch(
               expr = {
                 dev.set(dev.list()[pre_dev + 3])
                 svalue(stat_bar) <- "Plotting Count of disinct vessels..."
                 Sys.sleep(1)
                 smartRstudy$fleet$plotCountIDsEffo()
               },
               error = function(error_message) {
                 message("An error has occurred!")
                 message(error_message)
               },
               finally = {
                 enabled(eff_g_top) <- TRUE
                 svalue(stat_bar) <- ""
               }
             )
           }
    )
    addSpace(eff_g_top1, 10)
    add(eff_g_top1, eff1_sta)
    addSpring(eff_g_top)
    
    eff_g_top1b <- gframe(
      text = "Fishing Point", horizontal = TRUE,
      container = eff_g_top
    )
    addSpace(eff_g_top1b, 10)
    gbutton("Set", container = eff_g_top1b, handler = function(h, ...) {
      enabled(eff_g_top) <- FALSE
      tryCatch(
        expr = {
          svalue(stat_bar) <- "Setting parameters..."
          temp_dia <- gwindow(
            title = "Set Fishing Points", visible = FALSE,
            width = 650, height = 450, parent = main_win
          )
          up_g <- ggroup(horizontal = FALSE, container = temp_dia)
          up_fra <- gframe(container = up_g, horizontal = TRUE)
          addSpring(up_fra)
          spe_fra <- gframe(
            text = "Speed Range", container = up_fra,
            horizontal = TRUE
          )
          spe_lay <- glayout(
            homogeneous = FALSE, spacing = 10,
            container = spe_fra
          )
          spe_lay[1, 1, anchor = 0] <- "Min"
          spe_lay[2, 1, anchor = 0] <- "Max"
          spe_lay[1, 2, anchor = 0] <- gspinbutton(
            from = 0, to = 30, by = 1,
            value = 0, container = spe_lay,
            handler = function(...) {
              smartRstudy$fleet$plotSpeedDepth(
                which_year = svalue(yea_drop),
                speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                depth_range = unlist(lapply(dep_lay[1:2, 2], svalue))
              )
            }
          )
          spe_lay[2, 2, anchor = 0] <- gspinbutton(
            from = 0, to = 30, by = 1,
            value = 10, container = spe_lay,
            handler = function(...) {
              smartRstudy$fleet$plotSpeedDepth(
                which_year = svalue(yea_drop),
                speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                depth_range = unlist(lapply(dep_lay[1:2, 2], svalue))
              )
            }
          )
          addSpring(up_fra)
          dep_fra <- gframe(
            text = "Depth Range", container = up_fra,
            horizontal = TRUE
          )
          dep_lay <- glayout(
            homogeneous = FALSE, spacing = 10,
            container = dep_fra
          )
          dep_lay[1, 1, anchor = 0] <- "Min"
          dep_lay[2, 1, anchor = 0] <- "Max"
          dep_lay[1, 2, anchor = 0] <- gspinbutton(
            from = -5000, to = 0, by = 10,
            value = -500,
            container = dep_lay,
            handler = function(...) {
              smartRstudy$fleet$plotSpeedDepth(
                which_year = svalue(yea_drop),
                speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                depth_range = unlist(lapply(dep_lay[1:2, 2], svalue))
              )
            }
          )
          dep_lay[2, 2, anchor = 0] <- gspinbutton(
            from = -5000, to = 0, by = 10,
            value = 0, container = dep_lay,
            handler = function(...) {
              smartRstudy$fleet$plotSpeedDepth(
                which_year = svalue(yea_drop),
                speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                depth_range = unlist(lapply(dep_lay[1:2, 2], svalue))
              )
            }
          )
          addSpring(up_fra)
          yea_fra <- gframe(
            text = "Year View", container = up_fra,
            horizontal = TRUE
          )
          yea_drop <- gcombobox(names(smartRstudy$fleet$rawEffort),
                                selected = 1,
                                editable = FALSE, container = yea_fra,
                                expand = TRUE,
                                handler = function(...) {
                                  smartRstudy$fleet$plotSpeedDepth(
                                    which_year = svalue(yea_drop),
                                    speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                                    depth_range = unlist(lapply(dep_lay[1:2, 2], svalue))
                                  )
                                }
          )
          addSpring(up_fra)
          gbutton(
            text = "\n   Set!   \n", container = up_fra,
            handler = function(...) {
              enabled(up_fra) <- FALSE
              plot(NULL,
                   xlim = c(0, 5), ylim = c(0, 1), axes = FALSE,
                   xlab = "", ylab = ""
              )
              smartRstudy$fleet$setFishPoinPara(
                speed_range = unlist(lapply(spe_lay[1:2, 2], svalue)),
                depth_range = sort(unlist(lapply(dep_lay[1:2, 2], svalue)),
                                   decreasing = TRUE
                )
              )
              
              svalue(stat_bar) <- "Setting fishing points..."
              points(1, 0, pch = 19, col = "grey20")
              text(1, 0,
                   labels = "Parameters", pos = 3, col = "grey20", srt = 45,
                   offset = 1.5
              )
              svalue(int_progBar) <- 10
              Sys.sleep(1)
              smartRstudy$fleet$setFishPoin()
              points(2, 0, pch = 19, col = "grey20")
              text(2, 0,
                   labels = "Fishing Points", pos = 3, col = "grey20", srt = 45,
                   offset = 1.5
              )
              svalue(stat_bar) <- ""
              svalue(int_progBar) <- 30
              Sys.sleep(1)
              svalue(stat_bar) <- "Setting fishing point cells..."
              Sys.sleep(1)
              smartRstudy$setCellPoin()
              points(3, 0, pch = 19, col = "grey20")
              text(3, 0,
                   labels = "Cell Assign", pos = 3, col = "grey20", srt = 45,
                   offset = 1.5
              )
              svalue(stat_bar) <- "Adding week and month number to dataset..."
              svalue(int_progBar) <- 60
              Sys.sleep(1)
              smartRstudy$fleet$setWeekMonthNum()
              points(4, 0, pch = 19, col = "grey20")
              text(4, 0,
                   labels = "Week/Month\nAppend", pos = 3, col = "grey20",
                   srt = 45, offset = 1.5
              )
              svalue(stat_bar) <- ""
              svalue(int_progBar) <- 90
              Sys.sleep(1)
              svalue(stat_bar) <- "Completed!"
              svalue(int_progBar) <- 100
              Sys.sleep(1)
              delete(
                eff_g_top1b,
                eff_g_top1b$children[[length(eff_g_top1b$children)]]
              )
              add(eff_g_top1b, eff2_sta_n)
              if (gconfirm("Fishing point selection completed!\nClose window?",
                           title = "Confirm", icon = "question", parent = temp_dia
              )) {
                dispose(temp_dia)
              } else {
                enabled(up_fra) <- TRUE
              }
            }
          )
          addSpring(up_fra)
          fipo_gra <- ggraphics(
            width = 600, height = 400, container = up_g,
            expand = TRUE
          )
          bot_progStat <- ggroup(container = up_g, horizontal = TRUE)
          addSpring(bot_progStat)
          int_progBar <- gprogressbar(value = 0, container = bot_progStat)
          addSpring(bot_progStat)
          visible(temp_dia) <- TRUE
          smartRstudy$fleet$plotSpeedDepth(
            which_year = svalue(yea_drop),
            speed_range = unlist(lapply(
              spe_lay[1:2, 2],
              svalue
            )),
            depth_range = unlist(lapply(
              dep_lay[1:2, 2],
              svalue
            ))
          )
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          tryCatch(
            expr = {
              dev.set(dev.list()[pre_dev + 3])
              svalue(stat_bar) <- "Plotting fishing points data summary..."
              Sys.sleep(1)
              smartRstudy$fleet$plotFishPoinStat()
            },
            error = function(error_message) {
              message("An error has occurred!")
              message(error_message)
            }
          )
          enabled(eff_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(eff_g_top1b, 10)
    gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
           container = eff_g_top1b, handler = function(h, ...) {
             enabled(eff_g_top) <- FALSE
             tryCatch(
               expr = {
                 dev.set(dev.list()[pre_dev + 3])
                 svalue(stat_bar) <- "Plotting fishing points data summary..."
                 Sys.sleep(1)
                 smartRstudy$fleet$plotFishPoinStat()
               },
               error = function(error_message) {
                 message("An error has occurred!")
                 message(error_message)
               },
               finally = {
                 enabled(eff_g_top) <- TRUE
                 svalue(stat_bar) <- ""
               }
             )
           }
    )
    addSpace(eff_g_top1b, 10)
    add(eff_g_top1b, eff2_sta)
    
    addSpring(eff_g_top)
    
    eff_g_top2 <- gframe(
      text = "Maps", horizontal = TRUE, container = eff_g_top,
      expand = TRUE
    )
    addSpace(eff_g_top2, 10)
    effvie_drop <- gcombobox(
      items = "Year", selected = 1,
      container = eff_g_top2, expand = TRUE,
      editable = FALSE, handler = function(...) {
        enabled(eff_g_top) <- FALSE
        tryCatch(
          expr = {
            dev.set(dev.list()[pre_dev + 3])
            svalue(stat_bar) <- "Loading raw effort..."
            Sys.sleep(0.3)
            smartRstudy$ggplotRawPoints(svalue(effvie_drop))
            svalue(stat_bar) <- "Loading fishing points..."
            Sys.sleep(0.3)
            smartRstudy$ggplotFishingPoints(svalue(effvie_drop))
            svalue(stat_bar) <- "Loading gridded points..."
            Sys.sleep(0.3)
            smartRstudy$ggplotGridEffort(svalue(effvie_drop))
            svalue(stat_bar) <- "Plotting..."
            smartRstudy$setGgEff()
            smartRstudy$plotGgEff()
          },
          error = function(error_message) {
            message("An error has occurred!")
            message(error_message)
          },
          finally = {
            enabled(eff_g_top) <- TRUE
            svalue(stat_bar) <- ""
          }
        )
      }
    )
    addSpace(eff_g_top2, 10)
    addSpring(eff_g_top)
    
    eff_g_top3 <- gframe(
      text = "Effort Asset", horizontal = TRUE,
      container = eff_g_top
    )
    addSpace(eff_g_top3, 10)
    gbutton("Export", container = eff_g_top3, handler = function(h, ...) {
      enabled(eff_g_top) <- FALSE
      tryCatch(
        expr = {
          Sys.sleep(1)
          cat("\nSaving Effort Asset to rData...", sep = "")
          tmpOutEffFiles <- gfile(
            text = "Select output data name", type = "save",
            initial.filename = "Smart_EffAsset_List.RDS",
            initial.dir = smartRstudy$sampMap$gridPath
          )
          svalue(stat_bar) <- "Saving..."
          if (rev(unlist(strsplit(tmpOutEffFiles, "[.]")))[1] != "RDS") {
            tmpOutEffFiles <- paste(tmpOutEffFiles, ".RDS", sep = "")
          }
          Sys.sleep(1)
          saveRDS(smartRstudy$fleet$rawEffort, tmpOutEffFiles)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(eff_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(eff_g_top3, 5)
    gbutton("Import", container = eff_g_top3, handler = function(h, ...) {
      enabled(eff_g_top) <- FALSE
      tryCatch(
        expr = {
          Sys.sleep(1)
          tmpInEffFiles <- gfile(
            text = "Select Effort Asset file", type = "open",
            initial.filename = NULL, initial.dir = getwd()
          )
          svalue(stat_bar) <- "Loading..."
          Sys.sleep(1)
          smartRstudy$fleet$rawEffort <- readRDS(tmpInEffFiles)
          smartRstudy$fleet$setEffortIds()
          dev.set(dev.list()[pre_dev + 3])
          effvie_drop[] <- names(smartRstudy$fleet$rawEffort)
          svalue(effvie_drop) <- names(smartRstudy$fleet$rawEffort)[1]
          delete(effo_g, effo_g$children[[length(effo_g$children)]])
          add(effo_g, effo_sta_n)
          delete(eff_g_top1, eff_g_top1$children[[length(eff_g_top1$children)]])
          add(eff_g_top1, eff1_sta_n)
          delete(eff_g_top1b, eff_g_top1b$children[[length(eff_g_top1b$children)]])
          add(eff_g_top1b, eff2_sta_n)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          enabled(eff_g_top) <- TRUE
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(eff_g_top3, 10)
    addSpace(eff_g_top, 10)
    
    eff_p <- ggraphics(
      container = eff_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Fishing Grounds   ####
    
    fig_g <- ggroup(
      horizontal = FALSE, container = uti_gn,
      label = "Fishing Grounds"
    )
    fig_g_top <- gframe(horizontal = TRUE, container = fig_g)
    
    addSpring(fig_g_top)
    
    fig_g_top_dist <- gframe(
      text = "Distance Metric", horizontal = FALSE,
      container = fig_g_top, expand = TRUE
    )
    addSpring(fig_g_top_dist)
    fg_metr <- gcombobox(c(
      "euclidean", "manhattan", "maximum",
      "binary", "canberra", "minkowski"
    ),
    selected = 1,
    editable = FALSE,
    container = fig_g_top_dist, expand = TRUE
    )
    
    addSpring(fig_g_top_dist)
    addSpring(fig_g_top)
    
    fig_g_top_clus <- gframe(
      text = "Clustering", horizontal = TRUE,
      container = fig_g_top, expand = TRUE
    )
    addSpring(fig_g_top_clus)
    fg_maxCut <- gslider(
      from = 1, to = 50, horizontal = TRUE, by = 1,
      container = fig_g_top_clus
    )
    addSpring(fig_g_top_clus)
    addSpring(fig_g_top)
    
    gbutton("\n   Run   \n", container = fig_g_top, handler = function(h, ...) {
      smartRstudy$setAvailData()
      smartRstudy$sampMap$setClusInpu()
      
      smartRstudy$sampMap$calcFishGrou(
        numCuts = svalue(fg_maxCut),
        minsize = 10,
        modeska = "S",
        skater_method = svalue(fg_metr),
        nei_queen = FALSE
      )
      dev.set(dev.list()[pre_dev + 4])
      
      fg_plotCut[] <- 1:svalue(fg_maxCut)
      svalue(fg_plotCut) <- which.max(smartRstudy$sampMap$indSil)
      
      ### Update Fishing Grounds Status
      delete(figr_g, figr_g$children[[length(figr_g$children)]])
      add(figr_g, figr_sta_n)
    })
    
    addSpring(fig_g_top)
    
    fig_g_top_plot <- gframe(
      text = "Plot", horizontal = TRUE,
      container = fig_g_top, expand = TRUE
    )
    addSpring(fig_g_top_plot)
    fg_plotCut <- gcombobox(
      items = 2:50,
      container = fig_g_top_plot,
      handler = function(h, ...) {
        dev.set(dev.list()[pre_dev + 4])
        smartRstudy$sampMap$setCutResult(ind_clu = svalue(fg_plotCut))
        
        suppressWarnings(grid.arrange(
          smartRstudy$sampMap$ggSilFGlin,
          smartRstudy$sampMap$ggCutFGmap,
          smartRstudy$sampMap$ggEffoFGmap,
          smartRstudy$sampMap$ggDepthFGbox,
          smartRstudy$sampMap$ggEffoFGbox,
          smartRstudy$sampMap$ggBioFGmat,
          layout_matrix = rbind(
            c(1, 2, 2, 4, 5, 6),
            c(1, 3, 3, 4, 5, 6)
          )
        ))
      }
    )
    
    addSpring(fig_g_top_plot)
    
    addSpring(fig_g_top)
    
    gbutton("    Select\nPartitioning",
            container = fig_g_top,
            handler = function(h, ...) {
              smartRstudy$setFishGround(numCut = svalue(fg_plotCut))
              suppressWarnings(grid.arrange(
                smartRstudy$sampMap$ggSilFGlin,
                smartRstudy$sampMap$ggCutFGmap,
                smartRstudy$sampMap$ggEffoFGmap,
                smartRstudy$sampMap$ggDepthFGbox,
                smartRstudy$sampMap$ggEffoFGbox,
                smartRstudy$sampMap$ggBioFGmat,
                layout_matrix = rbind(
                  c(1, 2, 2, 4, 5, 6),
                  c(1, 3, 3, 4, 5, 6)
                )
              ))
            }
    )
    addSpring(fig_g_top)
    
    fig_g_top3 <- gframe(
      text = "Fishing Ground Asset", horizontal = TRUE,
      container = fig_g_top
    )
    addSpace(fig_g_top3, 10)
    gbutton("Export", container = fig_g_top3, handler = function(h, ...) {
      tryCatch(
        expr = {
          Sys.sleep(1)
          cat("\nSaving Fishing Ground Asset to rData...", sep = "")
          tmpOutFGFiles <- gfile(
            text = "Select output data name", type = "save",
            initial.filename = "Smart_FGAsset_List.RDS",
            initial.dir = smartRstudy$sampMap$gridPath
          )
          svalue(stat_bar) <- "Saving..."
          if (rev(unlist(strsplit(tmpOutFGFiles, "[.]")))[1] != "RDS") {
            tmpOutFGFiles <- paste(tmpOutFGFiles, ".RDS", sep = "")
          }
          Sys.sleep(1)
          saveRDS(smartRstudy$sampMap$exportFG(), tmpOutFGFiles)
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(fig_g_top3, 5)
    gbutton("Import", container = fig_g_top3, handler = function(h, ...) {
      tryCatch(
        expr = {
          Sys.sleep(1)
          tmpInEffFiles <- gfile(
            text = "Select Fishing Ground Asset file",
            type = "open",
            initial.filename = NULL, initial.dir = getwd()
          )
          svalue(stat_bar) <- "Loading..."
          Sys.sleep(1)
          smartRstudy$sampMap$importFG(readRDS(tmpInEffFiles))
          smartRstudy$setFishGround(numCut = smartRstudy$sampMap$cutFG)
          svalue(fg_plotCut) <- smartRstudy$sampMap$cutFG
        },
        error = function(error_message) {
          message("An error has occurred!")
          message(error_message)
        },
        finally = {
          svalue(stat_bar) <- ""
        }
      )
    })
    addSpace(fig_g_top3, 10)
    addSpace(fig_g_top, 10)
    
    fisGro_p <- ggraphics(
      container = fig_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Register   ####
    
    reg_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Register")
    reg_g_top <- gframe(horizontal = TRUE, container = reg_g)
    addSpace(reg_g_top, 2)
    addSpring(reg_g_top)
    reg_g_top_raw <- ggroup(horizontal = FALSE, container = reg_g_top)
    addSpring(reg_g_top_raw)
    gbutton("Load EU register",
            container = reg_g_top_raw,
            handler = function(h, ...) {
              tmpRegfiles <- gfile(
                text = "Select Fleet Register", type = "open",
                initial.filename = NULL, initial.dir = getwd(),
                filter = list(),
                multi = TRUE
              )
              enabled(reg_g_top) <- FALSE
              svalue(stat_bar) <- "Loading Fleet Register..."
              Sys.sleep(1)
              smartRstudy$fleet$loadFleetRegis(register_path = tmpRegfiles)
              smartRstudy$fleet$cleanRegister()
              smartRstudy$fleet$setVmsRegister()
              dev.set(dev.list()[pre_dev + 5])
              ggplot_registerDispatch(
                curRegister = smartRstudy$fleet$rawRegister,
                selPlot = "Summary"
              )
              
              ### Update Register Status
              if (!is.null(smartRstudy$fleet$rawRegister)) {
                delete(regi_g, regi_g$children[[length(regi_g$children)]])
                add(regi_g, regi_sta_n)
              }
              
              svalue(stat_bar) <- ""
              enabled(reg_g_top) <- TRUE
            }
    )
    addSpring(reg_g_top_raw)
    gbutton("View Raw Data",
            container = reg_g_top_raw,
            handler = function(h, ...) {
              temp_flee <- gbasicdialog(
                title = "Explore Fleet Register",
                parent = main_win
              )
              tmp_data <- smartRstudy$fleet$rawRegister
              tmp_data[which(is.na(tmp_data), arr.ind = TRUE)] <- "NA"
              flee_data <- gtable(tmp_data, container = temp_flee)
              size(temp_flee) <- c(600, 400)
              visible(temp_flee)
            }
    )
    addSpring(reg_g_top_raw)
    addSpring(reg_g_top)
    reg_g_top_view <- gframe(
      text = "Plot Summary Data", horizontal = TRUE,
      container = reg_g_top, expand = TRUE
    )
    addSpace(reg_g_top_view, 10)
    sel_regSet <- gradio(c("All", "Vms"),
                         selected = 1, horizontal = FALSE,
                         container = reg_g_top_view, handler = function(h, ...) {
                           dev.set(dev.list()[pre_dev + 5])
                           if (svalue(sel_regSet) == "All") {
                             ggplot_registerDispatch(
                               curRegister = smartRstudy$fleet$rawRegister,
                               selPlot = svalue(sel_regPlot)
                             )
                           } else {
                             ggplot_registerDispatch(
                               curRegister = smartRstudy$fleet$vmsRegister,
                               selPlot = svalue(sel_regPlot)
                             )
                           }
                         }
    )
    addSpace(reg_g_top_view, 10)
    sel_regPlot <- gcombobox(c(
      "Summary", "Main Gear",
      "Secondary Gear", "Hull Material",
      "Construction Year", "Length Over All",
      "Main Power"
    ),
    selected = 1, editable = FALSE, expand = TRUE,
    container = reg_g_top_view,
    handler = function(h, ...) {
      dev.set(dev.list()[pre_dev + 5])
      if (svalue(sel_regSet) == "All") {
        ggplot_registerDispatch(
          curRegister = smartRstudy$fleet$rawRegister,
          selPlot = svalue(sel_regPlot)
        )
      } else {
        ggplot_registerDispatch(
          curRegister = smartRstudy$fleet$vmsRegister,
          selPlot = svalue(sel_regPlot)
        )
      }
    }
    )
    addSpace(reg_g_top_view, 10)
    addSpring(reg_g_top)
    reg_g_top_harbs <- gframe(
      text = "Harbour Distance", horizontal = TRUE,
      container = reg_g_top
    )
    addSpace(reg_g_top_harbs, 10)
    gbutton("Get Harbours",
            container = reg_g_top_harbs,
            handler = function(h, ...) {
              smartRstudy$fleet$setRegHarbs()
              dev.set(dev.list()[pre_dev + 5])
              smartRstudy$getHarbFgDist()
              smartRstudy$ggplotFgWeigDists()
            }
    )
    addSpace(reg_g_top_harbs, 10)
    
    reg_g_harb_ico <- ggroup(horizontal = FALSE, container = reg_g_top_harbs)
    addSpring(reg_g_harb_ico)
    gimage(system.file("ico/folder-man.png", package = "smartR"),
           container = reg_g_harb_ico,
           handler = function(h, ...) {
             load_path <- gfile(
               text = "Select Harbour List file", type = "open",
               filter = list("R files" = list(patterns = c("*.rData")))
             )
             if (length(load_path) == 0) stop("Missing File to Load!")
             cat("\nLoading Harbour List from ", load_path, sep = "")
             smartRstudy$fleet$loadFleetHarb(harb_path = load_path)
             dev.set(dev.list()[pre_dev + 5])
             smartRstudy$getHarbFgDist()
             smartRstudy$ggplotFgWeigDists()
           }
    )
    addSpring(reg_g_harb_ico)
    gimage(system.file("ico/document-save-2.ico", package = "smartR"),
           container = reg_g_harb_ico,
           handler = function(h, ...) {
             if (is.null(smartRstudy$fleet$regHarbsUni)) {
               stop("Missing Harbour Data List")
             }
             save_dest <- gfile(
               text = "Select file name and destination directory",
               type = "save",
               initial.filename = "Smart_Harbour_List.rData",
               initial.dir = smartRstudy$sampMap$gridPath
             )
             if (rev(unlist(strsplit(save_dest, "[.]")))[1] != "rData") {
               save_dest <- paste(save_dest, ".rData", sep = "")
             }
             smartRstudy$fleet$saveFleetHarb(harb_path = save_dest)
             cat("\nHarbour List saved in: ", save_dest, sep = "")
           }
    )
    addSpring(reg_g_harb_ico)
    # addSpace(reg_g_top_harbs, 10)
    # gbutton("Set Weighted\nDistance",
    #         container = reg_g_top_harbs,
    #         handler = function(h, ...) {
    #           dev.set(dev.list()[pre_dev + 5])
    #           smartRstudy$getHarbFgDist()
    #           smartRstudy$ggplotFgWeigDists()
    #         }
    # )
    addSpace(reg_g_top_harbs, 10)
    gimage(system.file("ico/view-refresh-5_big.ico", package = "smartR"),
           container = reg_g_top_harbs,
           handler = function(h, ...) {
             dev.set(dev.list()[pre_dev + 5])
             smartRstudy$ggplotFgWeigDists()
           }
    )
    addSpace(reg_g_top_harbs, 10)
    addSpring(reg_g_top)
    regGro_p <- ggraphics(
      container = reg_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Production   ####
    
    pro_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Production")
    pro_g_top <- gframe(horizontal = TRUE, container = pro_g)
    addSpace(pro_g_top, 40)
    pro_g_top1 <- ggroup(
      horizontal = FALSE, container = pro_g_top,
      expand = TRUE
    )
    addSpring(pro_g_top1)
    gbutton("Load Landings", container = pro_g_top1, handler = function(h, ...) {
      tmpLandfiles <- gfile(
        text = "Select Landings Data", type = "open",
        filter = list(
          "csv files" = list(patterns = c("*.csv")),
          "All files" = list(patterns = c("*"))
        ),
        multi = TRUE
      )
      smartRstudy$fleet$loadProduction(tmpLandfiles)
      enabled(pro_g_top) <- FALSE
      svalue(stat_bar) <- "Loading landings..."
      Sys.sleep(1)
      dev.set(dev.list()[pre_dev + 6])
      svalue(stat_bar) <- "Setting Ids..."
      Sys.sleep(1)
      smartRstudy$fleet$setProdIds()
      smartRstudy$fleet$setIdsEffoProd()
      svalue(stat_bar) <- "Plotting Ids..."
      Sys.sleep(1)
      smartRstudy$fleet$plotCountIDsEffoProd()
      svalue(stat_bar) <- "Setting Production Matrix..."
      Sys.sleep(1)
      smartRstudy$fleet$setProdMatr()
      smartRstudy$fleet$setDayEffoMatrGround(maxFG = smartRstudy$sampMap$cutFG + 1)
      smartRstudy$fleet$setEffoProdMatr()
      smartRstudy$fleet$setEffoProdMont()
      svalue(stat_bar) <- "Setting Species..."
      Sys.sleep(1)
      smartRstudy$fleet$setProdSpec()
      smartRstudy$fleet$setEffoProdAll()
      svalue(stat_bar) <- "Getting Loa..."
      Sys.sleep(1)
      smartRstudy$fleet$setEffoProdAllLoa()
      svalue(stat_bar) <- ""
      Sys.sleep(1)
      spe_drop[] <- sort(names(smartRstudy$fleet$specSett))
      provie_drop[] <- names(smartRstudy$fleet$effoProdMont)
      svalue(spe_drop, index = TRUE) <- 1
      svalue(provie_drop, index = TRUE) <- 1
      enabled(spe_drop) <- TRUE
      enabled(pro_g_top) <- TRUE
    })
    addSpring(pro_g_top1)
    
    spe_fra <- gframe(
      text = "Species", container = pro_g_top1, horizontal = TRUE,
      expand = TRUE
    )
    spe_drop <- gcombobox("Species list",
                          selected = 1,
                          editable = FALSE, container = spe_fra, expand = TRUE
    )
    addSpring(pro_g_top1)
    addSpace(pro_g_top, 40)
    enabled(spe_drop) <- FALSE
    pro_g_top2 <- ggroup(horizontal = FALSE, container = pro_g_top)
    addSpring(pro_g_top2)
    
    gbutton("Set Threshold", container = pro_g_top2, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Set Landings Threshold", visible = FALSE,
        parent = main_win,
        width = 900, height = 500
      )
      
      up_g <- ggroup(horizontal = FALSE, container = temp_dia)
      up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
      addSpace(up_fra, 20)
      spe_fra <- gframe(
        text = "Species", container = up_fra, horizontal = TRUE,
        expand = TRUE
      )
      addSpring(spe_fra)
      glabel(text = svalue(spe_drop), container = spe_fra)
      tmp_spe <- smartRstudy$fleet$effoProdAll[, which(colnames(smartRstudy$fleet$effoProdAll) == svalue(spe_drop))]
      tmp_spe <- tmp_spe[tmp_spe != 0]
      addSpring(spe_fra)
      addSpace(up_fra, 20)
      thr_fra <- gframe(
        text = "Threshold", container = up_fra,
        horizontal = TRUE
      )
      addSpace(thr_fra, 20)
      thr_spin <- gspinbutton(
        from = 0, to = 100,
        by = 0.5, value = 0, container = thr_fra,
        handler = function(...) {
          tmp_spe <- smartRstudy$fleet$effoProdAll[, which(colnames(smartRstudy$fleet$effoProdAll) == svalue(spe_drop))]
          tmp_spe <- tmp_spe[tmp_spe != 0]
          
          hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
               breaks = svalue(num_bre_spin),
               main = bquote(italic(.(svalue(spe_drop)))),
               xlab = ""
          )
          abline(
            v = svalue(thr_spin), col = 2, lwd = 3,
            lty = 2
          )
        }
      )
      addSpace(up_fra, 20)
      addSpace(thr_fra, 20)
      
      bou_fra <- gframe(
        text = "Limits and Breaks", container = up_fra,
        horizontal = FALSE
      )
      addSpace(bou_fra, 20)
      bou_gru <- ggroup(horizontal = TRUE, container = bou_fra)
      glabel("Max Value:", container = bou_gru)
      addSpring(bou_gru)
      max_x_spin <- gspinbutton(
        from = 100, to = 1000,
        by = 10, value = 100, container = bou_gru,
        handler = function(...) {
          tmp_spe <- smartRstudy$fleet$effoProdAll[, which(colnames(smartRstudy$fleet$effoProdAll) == svalue(spe_drop))]
          tmp_spe <- tmp_spe[tmp_spe != 0]
          
          hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
               breaks = svalue(num_bre_spin),
               main = bquote(italic(.(svalue(spe_drop)))),
               xlab = ""
          )
          abline(
            v = svalue(thr_spin), col = 2, lwd = 3,
            lty = 2
          )
        }
      )
      bou_gru2 <- ggroup(horizontal = TRUE, container = bou_fra)
      glabel("N. Breaks:", container = bou_gru2)
      addSpring(bou_gru2)
      num_bre_spin <- gspinbutton(
        from = 10, to = 1000,
        by = 10, value = 100, container = bou_gru2,
        handler = function(...) {
          tmp_spe <- smartRstudy$fleet$effoProdAll[, which(colnames(smartRstudy$fleet$effoProdAll) == svalue(spe_drop))]
          tmp_spe <- tmp_spe[tmp_spe != 0]
          
          hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
               breaks = svalue(num_bre_spin),
               main = bquote(italic(.(svalue(spe_drop)))),
               xlab = ""
          )
          abline(
            v = svalue(thr_spin), col = 2,
            lwd = 3, lty = 2
          )
        }
      )
      addSpace(bou_fra, 20)
      addSpring(up_fra)
      
      set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
      addSpring(set_gru_up)
      set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
      set_lab <- glabel(text = "Not set", container = set_gru)
      addSpring(set_gru_up)
      
      addSpace(up_fra, 20)
      
      gbutton(
        text = "\n   Set!   \n", container = up_fra,
        handler = function(...) {
          smartRstudy$fleet$setSpecSettItm(
            species = svalue(spe_drop),
            thresh = svalue(thr_spin),
            brea = svalue(num_bre_spin),
            max_xlim = svalue(max_x_spin)
          )
          svalue(set_lab) <- "Set"
          delete(set_gru, set_gru$children[[length(set_gru$children)]])
          add(set_gru, land_sta_n)
        }
      )
      addSpring(up_fra)
      
      gbutton(
        text = " Close\nWindow", container = up_fra,
        handler = function(...) {
          dispose(temp_dia)
        }
      )
      addSpace(up_fra, 20)
      addSpring(up_g)
      land_gra <- ggraphics(
        width = 600, height = 400, container = up_g,
        expand = TRUE
      )
      visible(temp_dia) <- TRUE
      
      land_sta <- gimage(system.file("ico/user-invisible.png",
                                     package = "smartR"
      ))
      land_sta_n <- gimage(system.file("ico/user-available.png",
                                       package = "smartR"
      ))
      add(set_gru, land_sta)
      
      tmp_spe <- smartRstudy$fleet$effoProdAll[, which(colnames(smartRstudy$fleet$effoProdAll) == svalue(spe_drop))]
      tmp_spe <- tmp_spe[tmp_spe != 0]
      
      if (is.null(smartRstudy$fleet$specSett[[svalue(spe_drop)]])) {
        max_x_spin[] <- seq(0, max(tmp_spe), by = 10)
        svalue(max_x_spin) <- quantile(tmp_spe, 0.95)
        thr_spin[] <- seq(0, svalue(max_x_spin), by = 0.5)
        svalue(thr_spin) <- quantile(tmp_spe, 0.05)
        svalue(num_bre_spin) <- 100
        delete(set_gru, set_gru$children[[length(set_gru$children)]])
        add(set_gru, land_sta)
        svalue(set_lab) <- "Not set"
      } else {
        thr_spin[] <- seq(0, smartRstudy$fleet$specSett[[svalue(spe_drop)]]$max_x,
                          by = 0.5
        )
        svalue(thr_spin) <- smartRstudy$fleet$specSett[[svalue(spe_drop)]]$threshold
        svalue(num_bre_spin) <- smartRstudy$fleet$specSett[[svalue(spe_drop)]]$breaks
        svalue(max_x_spin) <- smartRstudy$fleet$specSett[[svalue(spe_drop)]]$max_x
        delete(set_gru, set_gru$children[[length(set_gru$children)]])
        add(set_gru, land_sta_n)
        svalue(set_lab) <- "Set"
      }
      
      hist(tmp_spe[tmp_spe <= svalue(max_x_spin)],
           breaks = svalue(num_bre_spin),
           main = bquote(italic(.(svalue(spe_drop)))), xlab = ""
      )
      abline(v = svalue(thr_spin), col = 2, lwd = 3, lty = 2)
    })
    addSpring(pro_g_top)
    addSpring(pro_g_top2)
    gbutton("Get Logit", container = pro_g_top2, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Get Logit", visible = FALSE,
        parent = main_win,
        width = 900, height = 500
      )
      
      up_g <- ggroup(horizontal = FALSE, container = temp_dia)
      up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
      addSpace(up_fra, 20)
      
      spe_fra <- gframe(
        text = "Species", container = up_fra, horizontal = TRUE,
        expand = TRUE
      )
      addSpace(spe_fra, 10)
      glabel(text = svalue(spe_drop), container = spe_fra)
      addSpace(spe_fra, 10)
      
      addSpace(up_fra, 20)
      mod_fra <- gframe(
        text = "Model", container = up_fra, horizontal = TRUE,
        expand = TRUE
      )
      addSpace(mod_fra, 20)
      mod_radSel <- gradio(
        items = c("GLM", "CART", "RF"),
        selected = 1, horizontal = FALSE, container = mod_fra,
        handler = function(...) {
          switch(svalue(mod_radSel),
                 GLM = {
                   lapply(
                     list(par_modSel[1, 2]),
                     function(x) enabled(x) <- TRUE
                   )
                   lapply(
                     par_modSel[2:3, 2],
                     function(x) enabled(x) <- FALSE
                   )
                 },
                 CART = {
                   lapply(
                     list(par_modSel[2, 2]),
                     function(x) enabled(x) <- TRUE
                   )
                   lapply(
                     par_modSel[c(1, 3), 2],
                     function(x) enabled(x) <- FALSE
                   )
                 },
                 RF = {
                   lapply(
                     list(par_modSel[3, 2]),
                     function(x) enabled(x) <- TRUE
                   )
                   lapply(
                     par_modSel[1:2, 2],
                     function(x) enabled(x) <- FALSE
                   )
                 }
          )
        }
      )
      addSpace(mod_fra, 20)
      
      addSpace(up_fra, 20)
      par_fra <- gframe(
        text = "Parameters", container = up_fra,
        horizontal = TRUE, expand = TRUE
      )
      addSpace(par_fra, 20)
      
      par_modSel <- glayout(container = par_fra)
      par_modSel[1, 1:2] <- ""
      par_modSel[2, 1] <- "cp"
      par_modSel[2, 2] <- gspinbutton(
        from = 0, to = 1, by = 0.001, value = 0.02,
        container = par_modSel
      )
      par_modSel[3, 1] <- "CV"
      par_modSel[3, 2] <- gspinbutton(
        from = 2, to = 10, by = 1, value = 2,
        container = par_modSel
      )
      
      addSpace(par_fra, 20)
      
      lapply(par_modSel[2:3, 2], function(x) enabled(x) <- FALSE)
      
      addSpring(up_fra)
      
      gbutton(text = " Get\nLogit", container = up_fra, handler = function(...) {
        tryCatch(
          expr = {
            smartRstudy$fleet$setSpecLogit(
              selSpecie = svalue(spe_drop),
              selModel = svalue(mod_radSel),
              cp = svalue(par_modSel[2, 2]),
              cv = svalue(par_modSel[3, 2])
            )
            smartRstudy$fleet$plotLogitROC(selSpecie = svalue(spe_drop))
            svalue(thr_spin) <- round(smartRstudy$fleet$specLogit[[svalue(spe_drop)]]$logit$Cut, 2)
            svalue(tmp_txt) <- capture.output({
              cat("\n")
              print(smartRstudy$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)
            })
          },
          error = function(error_message) {
            message("An error has occurred, try changing model parameters")
            message(error_message)
          },
          finally = {
            svalue(stat_bar) <- ""
          }
        )
      })
      addSpace(up_fra, 20)
      
      thr_fra <- gframe(
        text = "Tune Cutoff", container = up_fra, expand = TRUE,
        horizontal = TRUE
      )
      addSpace(thr_fra, 20)
      thr_spin <- gslider(
        from = 0.01, to = 0.99,
        by = 0.01, value = 0.5, container = thr_fra,
        expand = TRUE,
        handler = function(...) {
          if (!is.null(smartRstudy$fleet$specLogit[[svalue(spe_drop)]])) {
            smartRstudy$fleet$setSpecLogitConf(
              selSpecie = svalue(spe_drop),
              cutoff = svalue(thr_spin)
            )
            svalue(tmp_txt) <- capture.output({
              cat("\n")
              print(smartRstudy$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)
            })
          }
        }
      )
      addSpace(up_fra, 20)
      addSpace(thr_fra, 20)
      
      set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
      addSpring(set_gru_up)
      gbutton(
        text = "\n   Save   \n", container = set_gru_up,
        handler = function(...) {
          svalue(set_lab) <- "Saved"
          delete(set_gru, set_gru$children[[length(set_gru$children)]])
          add(set_gru, logi_sta_n)
        }
      )
      set_lab <- glabel(text = "Not Saved", container = set_gru_up)
      set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
      addSpring(set_gru_up)
      addSpace(up_fra, 20)
      
      gbutton(
        text = " Close\nWindow", container = up_fra,
        handler = function(...) {
          dispose(temp_dia)
        }
      )
      addSpace(up_fra, 20)
      
      bot_g <- ggroup(horizontal = TRUE, container = up_g)
      addSpace(bot_g, 10)
      bot_lef_g <- ggroup(horizontal = FALSE, container = bot_g)
      addSpring(bot_lef_g)
      tmp_txt <- gtext(
        text = NULL, width = 300, height = 350,
        container = bot_lef_g
      )
      addSpring(bot_lef_g)
      addSpace(bot_g, 10)
      logi_gra <- ggraphics(
        width = 300, height = 400, container = bot_g,
        expand = TRUE
      )
      visible(temp_dia) <- TRUE
      addSpace(bot_g, 10)
      
      logi_sta <- gimage(system.file("ico/user-invisible.png",
                                     package = "smartR"
      ))
      logi_sta_n <- gimage(system.file("ico/user-available.png",
                                       package = "smartR"
      ))
      add(set_gru, logi_sta)
      
      if (!is.null(smartRstudy$fleet$specLogit[[svalue(spe_drop)]]$logit$Roc)) {
        smartRstudy$fleet$plotLogitROC(svalue(spe_drop))
        svalue(tmp_txt) <- capture.output({
          cat("\n")
          print(smartRstudy$fleet$specLogit[[svalue(spe_drop)]]$logit$Confusion)
        })
      }
    })
    addSpring(pro_g_top2)
    
    gbutton("Get NNLS", container = pro_g_top2, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Get NNLS", visible = FALSE,
        parent = main_win,
        width = 950, height = 500
      )
      
      up_g <- ggroup(horizontal = FALSE, container = temp_dia, expand = TRUE)
      up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
      addSpace(up_fra, 20)
      spe_fra <- gframe(
        text = "Species", container = up_fra, horizontal = TRUE,
        expand = TRUE
      )
      addSpring(spe_fra)
      glabel(text = svalue(spe_drop), container = spe_fra)
      addSpring(spe_fra)
      addSpace(up_fra, 20)
      
      gbutton(text = " Get\nNNLS", container = up_fra, handler = function(...) {
        tryCatch(
          expr = {
            if (is.null(smartRstudy$fleet$effoProdAllLoa)) {
              smartRstudy$fleet$setEffoProdAllLoa()
            }
            smartRstudy$getNnlsModel(
              species = svalue(spe_drop),
              minobs = svalue(obs_spin),
              thr_r2 = svalue(thr_spin)
            )
            svalue(tmp_txt) <- paste("\n\nRaw Scenarios:\n\n\t",
                                     nrow(smartRstudy$fleet$resNNLS[[svalue(spe_drop)]]$bmat),
                                     "\n\nWith at least ",
                                     svalue(obs_spin), " observations:\n\n\t",
                                     smartRstudy$fleet$resNNLS[[svalue(spe_drop)]]$nSce,
                                     "\n\nFitted:\n\n\t",
                                     smartRstudy$fleet$resNNLS[[svalue(spe_drop)]]$nfitted,
                                     "   (",
                                     round(100 * smartRstudy$fleet$resNNLS[[svalue(spe_drop)]]$nfitted / smartRstudy$fleet$resNNLS[[svalue(spe_drop)]]$nSce),
                                     "%)\n\n",
                                     sep = ""
            )
            smartRstudy$fleet$plotNNLS(
              species = svalue(spe_drop),
              thresR2 = svalue(thr_spin)
            )
          },
          error = function(error_message) {
            message("An error has occurred, try changing number of minimum observation")
            message(error_message)
          },
          finally = {
            svalue(stat_bar) <- ""
          }
        )
      })
      addSpace(up_fra, 20)
      obs_fra <- gframe(
        text = "Min Observations", container = up_fra,
        expand = TRUE, horizontal = TRUE
      )
      addSpace(obs_fra, 20)
      obs_spin <- gspinbutton(
        from = 1, to = 100,
        by = 1, value = 3, container = obs_fra,
        expand = TRUE
      )
      addSpace(up_fra, 20)
      addSpace(obs_fra, 20)
      
      thr_fra <- gframe(
        text = "R2 Threshold", container = up_fra,
        expand = TRUE, horizontal = TRUE
      )
      addSpace(thr_fra, 20)
      thr_spin <- gslider(
        from = 0, to = 1,
        by = 0.01, value = 0, container = thr_fra,
        expand = TRUE
      )
      addSpace(up_fra, 20)
      addSpace(thr_fra, 20)
      
      gbutton(
        text = "\n   Save   \n", container = up_fra,
        handler = function(...) {
          svalue(set_lab) <- "Saved"
          delete(set_gru, set_gru$children[[length(set_gru$children)]])
          add(set_gru, logi_sta_n)
        }
      )
      
      addSpace(up_fra, 20)
      set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
      addSpring(set_gru_up)
      set_lab <- glabel(text = "Not Saved", container = set_gru_up)
      set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
      addSpring(set_gru_up)
      addSpace(up_fra, 20)
      
      gbutton(
        text = " Close\nWindow", container = up_fra,
        handler = function(...) {
          dispose(temp_dia)
        }
      )
      addSpace(up_fra, 20)
      bot_g <- ggroup(horizontal = TRUE, container = up_g)
      addSpace(bot_g, 10)
      bot_lef_g <- ggroup(horizontal = FALSE, container = bot_g)
      addSpring(bot_lef_g)
      tmp_txt <- gtext(
        text = NULL, width = 200, height = 350,
        container = bot_lef_g
      )
      addSpring(bot_lef_g)
      addSpace(bot_g, 10)
      nnls_gra <- ggraphics(
        width = 300, height = 400, container = bot_g,
        expand = TRUE
      )
      visible(temp_dia) <- TRUE
      addSpace(bot_g, 10)
      
      logi_sta <- gimage(system.file("ico/user-invisible.png",
                                     package = "smartR"
      ))
      logi_sta_n <- gimage(system.file("ico/user-available.png",
                                       package = "smartR"
      ))
      add(set_gru, logi_sta)
    })
    addSpring(pro_g_top2)
    addSpring(pro_g_top)
    
    pro_g_topBeta <- ggroup(horizontal = FALSE, container = pro_g_top)
    addSpring(pro_g_topBeta)
    gbutton("Tune\nBetas", container = pro_g_topBeta, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Set Max Beta", visible = FALSE,
        parent = main_win,
        width = 950, height = 500
      )
      
      up_g <- ggroup(horizontal = FALSE, container = temp_dia, expand = TRUE)
      up_fra <- gframe(container = up_g, horizontal = TRUE, expand = TRUE)
      addSpace(up_fra, 20)
      spe_fra <- gframe(
        text = "Species", container = up_fra, horizontal = TRUE,
        expand = TRUE
      )
      addSpring(spe_fra)
      glabel(text = svalue(spe_drop), container = spe_fra)
      
      addSpring(spe_fra)
      addSpace(up_fra, 20)
      maxb_fra <- gframe(
        text = "Max Beta", container = up_fra, expand = TRUE,
        horizontal = TRUE
      )
      addSpace(maxb_fra, 20)
      maxb_spin <- gslider(
        from = 1,
        to = ceiling(max(smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity)),
        by = 1, value = 0, container = maxb_fra,
        expand = TRUE,
        handler = function(...) {
          Sys.sleep(5)
          print(ggplot_betasBoxplot(
            df_YearFGprod = smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]],
            int_hline = svalue(maxb_spin)
          ))
        }
      )
      addSpace(up_fra, 20)
      addSpace(maxb_fra, 20)
      
      gbutton(
        text = "\n   Save   \n", container = up_fra,
        handler = function(...) {
          smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity[smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity > svalue(maxb_spin)] <- svalue(maxb_spin)
          svalue(set_lab) <- "Saved"
          delete(set_gru, set_gru$children[[length(set_gru$children)]])
          add(set_gru, logi_sta_n)
          print(ggplot_betasBoxplot(df_YearFGprod = smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]], int_hline = svalue(maxb_spin)))
        }
      )
      
      addSpace(up_fra, 20)
      set_gru_up <- ggroup(container = up_fra, horizontal = FALSE)
      addSpring(set_gru_up)
      set_lab <- glabel(text = "Not Saved", container = set_gru_up)
      set_gru <- ggroup(container = set_gru_up, horizontal = TRUE)
      addSpring(set_gru_up)
      addSpace(up_fra, 20)
      gbutton(
        text = " Close\nWindow", container = up_fra,
        handler = function(...) {
          dispose(temp_dia)
        }
      )
      addSpace(up_fra, 20)
      bot_g <- ggroup(horizontal = TRUE, container = up_g)
      nnls_gra <- ggraphics(
        width = 300, height = 400, container = bot_g,
        expand = TRUE
      )
      visible(temp_dia) <- TRUE
      addSpace(bot_g, 10)
      logi_sta <- gimage(system.file("ico/user-invisible.png",
                                     package = "smartR"
      ))
      logi_sta_n <- gimage(system.file("ico/user-available.png",
                                       package = "smartR"
      ))
      add(set_gru, logi_sta)
      print(ggplot_betasBoxplot(
        df_YearFGprod = smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]],
        int_hline = max(smartRstudy$fleet$betaMeltYear[[svalue(spe_drop)]]$Productivity)
      ))
    })
    addSpring(pro_g_topBeta)
    addSpring(pro_g_top)
    
    pro_g_top3 <- ggroup(horizontal = FALSE, container = pro_g_top)
    addSpring(pro_g_top3)
    gbutton("  Predict\nProduction",
            container = pro_g_top3,
            handler = function(h, ...) {
              enabled(pro_g_top) <- FALSE
              Sys.sleep(1)
              smartRstudy$fleet$setEffoMont()
              smartRstudy$fleet$setEffoAll()
              smartRstudy$fleet$setEffoAllLoa()
              smartRstudy$predictProduction(svalue(spe_drop))
              smartRstudy$fleet$setProdMeltYear(svalue(spe_drop))
              enabled(pro_g_top) <- TRUE
            }
    )
    addSpring(pro_g_top3)
    
    addSpring(pro_g_top)
    pro_g_top2_view_g <- ggroup(
      horizontal = FALSE, container = pro_g_top,
      expand = TRUE
    )
    pro_g_top2_view <- gframe(
      text = "View", horizontal = TRUE,
      container = pro_g_top2_view_g, expand = TRUE
    )
    addSpring(pro_g_top2_view)
    provie_drop <- gcombobox(
      items = "Year   ", selected = 1,
      container = pro_g_top2_view,
      expand = TRUE, editable = FALSE
    )
    provie_drop$set_size(value = c(width = 80))
    addSpring(pro_g_top2_view)
    pro_g_top2_ver <- ggroup(horizontal = FALSE, container = pro_g_top2_view)
    addSpring(pro_g_top2_ver)
    gbutton("Betas", container = pro_g_top2_ver, handler = function(h, ...) {
      dev.set(dev.list()[pre_dev + 6])
      svalue(stat_bar) <- "Plotting Betas..."
      Sys.sleep(1)
      smartRstudy$setPlotBetaMeltYear(
        species = svalue(spe_drop),
        year = svalue(provie_drop)
      )
      suppressWarnings(grid.arrange(smartRstudy$sampMap$ggBetaFGmap,
                                    smartRstudy$sampMap$ggBetaFGbox,
                                    layout_matrix = rbind(
                                      c(1, 1, 1, 2),
                                      c(1, 1, 1, 2)
                                    )
      ))
      svalue(stat_bar) <- ""
    })
    addSpring(pro_g_top2_ver)
    gbutton("Production", container = pro_g_top2_ver, handler = function(h, ...) {
      dev.set(dev.list()[pre_dev + 6])
      svalue(stat_bar) <- "Plotting production..."
      Sys.sleep(1)
      smartRstudy$setPlotProdMeltYear(
        species = svalue(spe_drop),
        year = svalue(provie_drop)
      )
      suppressWarnings(grid.arrange(smartRstudy$sampMap$ggProdFGmap,
                                    smartRstudy$sampMap$ggProdFGbox,
                                    layout_matrix = rbind(
                                      c(1, 1, 1, 2),
                                      c(1, 1, 1, 2)
                                    )
      ))
      svalue(stat_bar) <- ""
    })
    addSpring(pro_g_top2_ver)
    addSpring(pro_g_top2_view)
    gbutton("    Total\nProduction",
            container = pro_g_top2_view,
            handler = function(h, ...) {
              smartRstudy$fleet$plotTotProd(species = svalue(spe_drop))
            }
    )
    addSpring(pro_g_top2_ver)
    addSpring(pro_g_top2_view)
    addSpace(pro_g_top2_view_g, 7)
    addSpring(pro_g_top)
    proGro_p <- ggraphics(
      container = pro_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####     Survey     ####
    
    raw_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Survey")
    raw_g_top <- gframe(horizontal = TRUE, container = raw_g)
    addSpace(raw_g_top, 2)
    addSpring(raw_g_top)
    raw_g_top1 <- ggroup(horizontal = FALSE, container = raw_g_top)
    addSpring(raw_g_top1)
    gbutton("Load Sample", container = raw_g_top1, handler = function(h, ...) {
      svalue(stat_bar) <- "Loading Data..."
      Sys.sleep(1)
      tmpSurvfiles <- gfile(
        text = "Select Survey Data", type = "open",
        filter = list(
          "csv files" = list(patterns = c("*.csv")),
          "All files" = list(patterns = c("*"))
        ),
        multi = TRUE
      )
      smartRstudy$loadSurveyLFD(csv_path = tmpSurvfiles)
      
      if (!is.null(smartRstudy$rawDataSurvey)) {
        raw_t[] <- smartRstudy$rawDataSurvey[sample(1:nrow(smartRstudy$rawDataSurvey), 100, replace = FALSE), ]
        svalue(raw_l1) <- paste("Species: ", paste(smartRstudy$specieInSurvey, collapse = " - "))
        svalue(raw_l3) <- paste("Years: from", min(as.numeric(as.character(smartRstudy$yearInSurvey))), " to ", max(as.numeric(as.character(smartRstudy$yearInSurvey))))
        spec_drop_mix[] <- smartRstudy$specieInSurvey
        svalue(spec_drop_mix) <- smartRstudy$specieInSurvey[1]
        if (is.null(smartRstudy$specieInFishery)) {
          assSpe_drop[] <- smartRstudy$specieInSurvey
          svalue(assSpe_drop) <- smartRstudy$specieInSurvey[1]
        } else {
          assSpe_drop[] <- intersect(
            smartRstudy$specieInSurvey,
            smartRstudy$specieInFishery
          )
          svalue(assSpe_drop) <- smartRstudy$specieInSurvey[1]
        }
        
        ### Update Sampling Status
        svalue(n_year_s) <- paste(length(smartRstudy$yearInSurvey),
                                  " years",
                                  sep = ""
        )
        svalue(mi_date_s) <- paste("From: ",
                                   min(as.numeric(as.character(smartRstudy$yearInSurvey))),
                                   sep = ""
        )
        svalue(ma_date_s) <- paste("To: ",
                                   max(as.numeric(as.character(smartRstudy$yearInSurvey))),
                                   sep = ""
        )
        svalue(n_spec_s) <- paste(length(smartRstudy$specieInSurvey),
                                  " species",
                                  sep = ""
        )
        
        delete(samp_g, samp_g$children[[length(samp_g$children)]])
        add(samp_g, samp_sta_n)
        
        svalue(stat_bar) <- ""
      }
    })
    addSpring(raw_g_top1)
    addSpring(raw_g_top)
    gbutton("LFD Viewer", container = raw_g_top, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Survey Length Frequency Distribution Viewer",
        visible = FALSE,
        parent = main_win, width = 800, height = 500
      )
      
      pop_g <- ggroup(
        horizontal = FALSE, container = temp_dia,
        label = "Population"
      )
      pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
      lfdfra_g <- gframe("LFD data",
                         horizontal = TRUE, container = pop_g_top,
                         expand = TRUE
      )
      addSpring(lfdfra_g)
      
      spec_b <- gframe("Species",
                       horizontal = FALSE, container = lfdfra_g,
                       expand = TRUE
      )
      addSpring(lfdfra_g)
      addSpring(spec_b)
      spec_drop <- gcombobox(
        items = smartRstudy$specieInSurvey,
        selected = 1, container = spec_b, expand = TRUE,
        editable = FALSE, handler = function(h, ...) {
          spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
          sex_drop[] <- smartRstudy$surveyBySpecie[[spe_ind]]$speSex
          svalue(sex_drop) <- sex_drop[1]
        }
      )
      spec_drop$set_size(value = c(width = 150))
      addSpring(spec_b)
      sex_b <- gframe("Sex",
                      horizontal = FALSE, container = lfdfra_g,
                      expand = TRUE
      )
      addSpring(lfdfra_g)
      addSpring(sex_b)
      sex_drop <- gcombobox(
        items = c("Female", "Male", "Unsex"),
        selected = 1, container = sex_b, expand = TRUE,
        editable = FALSE, handler = function(h, ...) {
          spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
          suppressWarnings(grid.arrange(smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                        smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                        smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                        smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                        layout_matrix = rbind(
                                          c(1, 1, 1, 3),
                                          c(2, 2, 2, 4),
                                          c(2, 2, 2, 4)
                                        )
          ))
        }
      )
      addSpring(sex_b)
      
      addSpring(lfdfra_g)
      addSpace(pop_g_top, 2)
      pop_p <- ggraphics(
        container = pop_g, width = 750, height = 450,
        expand = TRUE
      )
      addSpring(pop_g_top)
      
      gbutton("Close", container = pop_g_top, handler = function(h, ...) {
        dispose(temp_dia)
      })
      
      addSpring(pop_g_top)
      spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
      sex_drop[] <- smartRstudy$surveyBySpecie[[1]]$speSex
      svalue(sex_drop) <- sex_drop[1]
      
      visible(temp_dia) <- TRUE
      
      suppressWarnings(grid.arrange(smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                    smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                    smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                    smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                    layout_matrix = rbind(
                                      c(1, 1, 1, 3),
                                      c(2, 2, 2, 4),
                                      c(2, 2, 2, 4)
                                    )
      ))
    })
    addSpring(raw_g_top)
    gbutton("Spatial Distribution",
            container = raw_g_top,
            handler = function(h, ...) {
              temp_dia <- gwindow(
                title = "Spatial Distribution of Survey sampling",
                visible = FALSE,
                parent = main_win, width = 700, height = 500
              )
              
              pop_g <- ggroup(horizontal = FALSE, container = temp_dia)
              pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
              addSpring(pop_g_top)
              spec_b <- gframe("Species",
                               horizontal = FALSE, container = pop_g_top,
                               expand = TRUE
              )
              
              addSpring(spec_b)
              spec_drop <- gcombobox(
                items = as.character(smartRstudy$specieInSurvey),
                selected = 1,
                container = spec_b, editable = FALSE,
                handler = function(h, ...) {
                  spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
                  sex_drop[] <- smartRstudy$surveyBySpecie[[spe_ind]]$speSex
                  svalue(sex_drop) <- sex_drop[1]
                }
              )
              spec_drop$set_size(value = c(width = 150))
              addSpring(spec_b)
              sex_b <- gframe("Sex",
                              horizontal = FALSE, container = pop_g_top,
                              expand = TRUE
              )
              addSpring(pop_g_top)
              addSpring(sex_b)
              sex_drop <- gcombobox(
                items = c("Female", "Male", "Unsex"),
                selected = 1, container = sex_b, expand = TRUE,
                editable = FALSE, handler = function(h, ...) {
                  spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
                  suppressWarnings(grid.arrange(smartRstudy$sampMap$ggMapFgSurvey,
                                                smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                layout_matrix = rbind(
                                                  c(NA, 1, 1),
                                                  c(4, 1, 1),
                                                  c(NA, 2, 3)
                                                )
                  ))
                }
              )
              addSpring(sex_b)
              addSpring(pop_g_top)
              
              gbutton("Close", container = pop_g_top, handler = function(h, ...) {
                dispose(temp_dia)
              })
              addSpring(pop_g_top)
              
              addSpace(pop_g_top, 10)
              sex_drop[] <- smartRstudy$surveyBySpecie[[1]]$speSex
              svalue(sex_drop) <- sex_drop[1]
              
              pop_p <- ggraphics(
                container = pop_g, width = 650, height = 450,
                expand = TRUE
              )
              visible(temp_dia) <- TRUE
              
              spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop))
              suppressWarnings(grid.arrange(smartRstudy$sampMap$ggMapFgSurvey,
                                            smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                            smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                            smartRstudy$surveyBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                            layout_matrix = rbind(
                                              c(NA, 1, 1),
                                              c(4, 1, 1),
                                              c(NA, 2, 3)
                                            )
              ))
            }
    )
    addSpring(raw_g_top)
    gbutton("MEDITS index", container = raw_g_top, handler = function(h, ...) {
      strataVect <- c(0, 10, 50, 100, 200, 500, 800, Inf)
      icoStrata_off <- gimage(system.file("ico/user-invisible.png",
                                          package = "smartR"
      ))
      icoStrata_on <- gimage(system.file("ico/user-available.png",
                                         package = "smartR"
      ))
      icoArea_off <- gimage(system.file("ico/user-invisible.png",
                                        package = "smartR"
      ))
      icoArea_on <- gimage(system.file("ico/user-available.png",
                                       package = "smartR"
      ))
      icoMedit_off <- gimage(system.file("ico/user-invisible.png",
                                         package = "smartR"
      ))
      icoMedit_on <- gimage(system.file("ico/user-available.png",
                                        package = "smartR"
      ))
      
      temp_dia <- gwindow(
        title = "Medits index calculator", visible = FALSE,
        parent = main_win,
        width = 900, height = 500
      )
      
      med_g <- ggroup(horizontal = FALSE, container = temp_dia)
      med_g_top <- gframe(horizontal = TRUE, container = med_g, spacing = 10)
      
      addSpace(med_g_top, 10)
      
      comfra_g <- gframe("Compute",
                         horizontal = TRUE, container = med_g_top,
                         expand = TRUE
      )
      addSpring(comfra_g)
      
      strata_f <- gframe("Strata",
                         horizontal = FALSE, container = comfra_g,
                         expand = TRUE
      )
      depth_b <- gbutton(
        text = "Set", container = strata_f,
        handler = function(h, ...) {
          smartRstudy$setDepthSurvey()
          smartRstudy$setStratumSurvey(vectorStrata = strataVect)
          delete(strata_f, strata_f$children[[length(strata_f$children)]])
          add(strata_f, icoStrata_on)
        }
      )
      add(strata_f, icoStrata_off)
      
      addSpring(comfra_g)
      
      areas_f <- gframe("Areas",
                        horizontal = FALSE, container = comfra_g,
                        expand = TRUE
      )
      area_b <- gbutton(
        text = "Set", container = areas_f,
        handler = function(h, ...) {
          smartRstudy$sampMap$setAreaGrid()
          smartRstudy$sampMap$setAreaStrata(vectorStrata = strataVect)
          smartRstudy$sampMap$setWeightStrata()
          delete(areas_f, areas_f$children[[length(areas_f$children)]])
          add(areas_f, icoArea_on)
        }
      )
      add(areas_f, icoArea_off)
      
      addSpring(comfra_g)
      
      medInd_f <- gframe("MEDITS Index",
                         horizontal = FALSE,
                         container = comfra_g, expand = TRUE
      )
      medInd_b <- gbutton(
        text = "Set", container = medInd_f,
        handler = function(h, ...) {
          smartRstudy$setAbuAvgAll()
          smartRstudy$setStrataAbu()
          smartRstudy$setMeditsIndex()
          delete(medInd_f, medInd_f$children[[length(medInd_f$children)]])
          add(medInd_f, icoMedit_on)
          specie_ind <- 1
          sex_sel <- sex_drop[1]
          tmp_abus <- data.frame(
            Class = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Class,
            Stratum = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
            Year = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Year
          )
          if (sex_sel == "Female") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
          } else if (sex_sel == "Male") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
          } else if (sex_sel == "Unsex") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
          } else if (sex_sel == "All") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
          }
          tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
          print(ggplot_meditsIndex(inMedits = tmp_abus))
        }
      )
      add(medInd_f, icoMedit_off)
      
      addSpring(comfra_g)
      addSpring(med_g_top)
      plofra_g <- gframe("Show",
                         horizontal = TRUE, container = med_g_top,
                         expand = TRUE
      )
      addSpace(plofra_g, 10)
      specie_drop <- gcombobox(
        items = smartRstudy$specieInSurvey, selected = 1,
        editable = FALSE, container = plofra_g,
        expand = TRUE, handler = function(...) {
          specie_ind <- which(smartRstudy$specieInSurvey == svalue(specie_drop))
          sex_drop[] <- smartRstudy$surveyBySpecie[[specie_ind]]$speSex
          sex_sel <- sex_drop[1]
          tmp_abus <- data.frame(
            Class = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Class,
            Stratum = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
            Year = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Year
          )
          if (sex_sel == "Female") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
          } else if (sex_sel == "Male") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
          } else if (sex_sel == "Unsex") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
          } else if (sex_sel == "All") {
            tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
          }
          tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
          svalue(sex_drop) <- sex_drop[1]
          
          # print(ggplot_meditsIndex(inMedits = tmp_abus))
        }
      )
      specie_drop$set_size(value = c(width = 150))
      addSpace(plofra_g, 10)
      sex_drop <- gcombobox(
        items = c("Female", "Male", "Unsex", "All"),
        selected = 1, editable = FALSE, container = plofra_g,
        expand = TRUE, handler = function(...) {
          specie_ind <- which(smartRstudy$specieInSurvey == svalue(specie_drop))
          sex_sel <- svalue(sex_drop)
          if (!is.null(smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg)) {
            tmp_abus <- data.frame(
              Class = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Class,
              Stratum = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Stratum,
              Year = smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$Year
            )
            if (sex_sel == "Female") {
              tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem
            } else if (sex_sel == "Male") {
              tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal
            } else if (sex_sel == "Unsex") {
              tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
            } else if (sex_sel == "All") {
              tmp_abus$Index <- smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiFem + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiMal + smartRstudy$surveyBySpecie[[specie_ind]]$abuAvg$weiUns
            }
            tmp_abus$Zeros <- as.factor(tmp_abus$Index == 0)
            print(ggplot_meditsIndex(inMedits = tmp_abus))
          }
        }
      )
      addSpace(plofra_g, 10)
      addSpring(med_g_top)
      pop_p <- ggraphics(
        container = med_g, width = 550, height = 250,
        expand = TRUE
      )
      gbutton("Close", container = med_g_top, handler = function(h, ...) {
        dispose(temp_dia)
      })
      addSpace(med_g_top, 10)
      sex_drop[] <- smartRstudy$surveyBySpecie[[1]]$speSex
      svalue(sex_drop) <- sex_drop[1]
      visible(temp_dia) <- TRUE
    })
    addSpring(raw_g_top)
    raw_g_top2 <- ggroup(horizontal = FALSE, container = raw_g_top)
    raw_l1 <- glabel("Species: ", container = raw_g_top2)
    raw_l3 <- glabel("Years: ", container = raw_g_top2)
    addSpace(raw_g_top, 10)
    addSpace(raw_g_top2, 2)
    
    blankDF <- data.frame(
      Species = character(0),
      Lat = numeric(0),
      Lon = numeric(0),
      Year = character(0),
      Class = numeric(0),
      Female = character(0),
      Male = character(0),
      Unsex = character(0),
      stringsAsFactors = FALSE
    )
    raw_t <- gtable(blankDF, container = raw_g, expand = TRUE)
    
    
    ####     Fishery     ####
    
    fis_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Fishery")
    fis_g_top <- gframe(horizontal = TRUE, container = fis_g)
    addSpace(fis_g_top, 2)
    addSpring(fis_g_top)
    fis_g_top1 <- ggroup(horizontal = FALSE, container = fis_g_top)
    addSpring(fis_g_top1)
    gbutton("Load Sample", container = fis_g_top1, handler = function(h, ...) {
      svalue(stat_bar) <- "Loading Data..."
      Sys.sleep(1)
      tmpFishfiles <- gfile(
        text = "Select Fishery Data", type = "open",
        filter = list(
          "csv files" = list(patterns = c("*.csv")),
          "All files" = list(patterns = c("*"))
        ),
        multi = TRUE
      )
      smartRstudy$loadFisheryLFD(csv_path = tmpFishfiles)
      
      if (!is.null(smartRstudy$rawDataFishery)) {
        fis_t[] <- smartRstudy$rawDataFishery[sample(1:nrow(smartRstudy$rawDataFishery),
                                                     100,
                                                     replace = FALSE
        ), ]
        svalue(fis_l1) <- paste("Species: ", paste(smartRstudy$specieInFishery,
                                                   collapse = " - "
        ))
        svalue(fis_l3) <- paste(
          "Years: from",
          min(as.numeric(as.character(smartRstudy$yearInFishery))),
          " to ",
          max(as.numeric(as.character(smartRstudy$yearInFishery)))
        )
        if (is.null(smartRstudy$specieInSurvey)) {
          assSpe_drop[] <- smartRstudy$specieInFishery
          svalue(assSpe_drop) <- smartRstudy$specieInFishery[1]
        } else {
          assSpe_drop[] <- intersect(
            smartRstudy$specieInSurvey,
            smartRstudy$specieInFishery
          )
          svalue(assSpe_drop) <- smartRstudy$specieInSurvey[1]
        }
        
        ### Update Fishery Status
        svalue(n_yearF_s) <- paste(length(smartRstudy$yearInFishery), " years",
                                   sep = ""
        )
        svalue(mi_dateF_s) <- paste("From: ",
                                    min(as.numeric(as.character(smartRstudy$yearInFishery))),
                                    sep = ""
        )
        svalue(ma_dateF_s) <- paste("To: ",
                                    max(as.numeric(as.character(smartRstudy$yearInFishery))),
                                    sep = ""
        )
        svalue(n_specF_s) <- paste(length(smartRstudy$specieInFishery),
                                   " species",
                                   sep = ""
        )
        
        delete(fish_g, fish_g$children[[length(fish_g$children)]])
        add(fish_g, fish_sta_n)
        
        svalue(stat_bar) <- ""
      }
    })
    addSpring(fis_g_top1)
    addSpring(fis_g_top)
    gbutton("LFD Viewer", container = fis_g_top, handler = function(h, ...) {
      temp_dia <- gwindow(
        title = "Fishery Length Frequency Distribution Viewer",
        visible = FALSE,
        parent = main_win, width = 800, height = 500
      )
      
      pop_g <- ggroup(
        horizontal = FALSE, container = temp_dia,
        label = "Population"
      )
      pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
      lfdfra_g <- gframe("LFD data",
                         horizontal = TRUE, container = pop_g_top,
                         expand = TRUE
      )
      addSpring(lfdfra_g)
      
      spec_b <- gframe("Species",
                       horizontal = FALSE, container = lfdfra_g,
                       expand = TRUE
      )
      addSpring(lfdfra_g)
      addSpring(spec_b)
      spec_drop <- gcombobox(
        items = smartRstudy$specieInFishery,
        selected = 1, container = spec_b, expand = TRUE,
        editable = FALSE, handler = function(h, ...) {
          spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
          sex_drop[] <- smartRstudy$fisheryBySpecie[[spe_ind]]$speSex
          svalue(sex_drop) <- sex_drop[1]
        }
      )
      spec_drop$set_size(value = c(width = 150))
      addSpring(spec_b)
      sex_b <- gframe("Sex",
                      horizontal = FALSE, container = lfdfra_g,
                      expand = TRUE
      )
      addSpring(lfdfra_g)
      addSpring(sex_b)
      sex_drop <- gcombobox(
        items = c("Female", "Male", "Unsex"),
        selected = 1, container = sex_b, expand = TRUE,
        editable = FALSE, handler = function(h, ...) {
          spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
          suppressWarnings(grid.arrange(smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                        smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                        smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                        smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                        layout_matrix = rbind(
                                          c(1, 1, 1, 3),
                                          c(2, 2, 2, 4),
                                          c(2, 2, 2, 4)
                                        )
          ))
        }
      )
      addSpring(sex_b)
      
      addSpring(lfdfra_g)
      addSpace(pop_g_top, 2)
      pop_p <- ggraphics(
        container = pop_g, width = 750, height = 450,
        expand = TRUE
      )
      addSpring(pop_g_top)
      
      gbutton("Close", container = pop_g_top, handler = function(h, ...) {
        dispose(temp_dia)
      })
      addSpring(pop_g_top)
      spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
      sex_drop[] <- smartRstudy$fisheryBySpecie[[1]]$speSex
      svalue(sex_drop) <- sex_drop[1]
      
      visible(temp_dia) <- TRUE
      
      suppressWarnings(grid.arrange(smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histLfdTot"]],
                                    smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcLfd"]],
                                    smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["histUtcTot"]],
                                    smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["dotUtcSplit"]],
                                    layout_matrix = rbind(
                                      c(1, 1, 1, 3),
                                      c(2, 2, 2, 4),
                                      c(2, 2, 2, 4)
                                    )
      ))
    })
    addSpring(fis_g_top)
    gbutton("Spatial Distribution",
            container = fis_g_top,
            handler = function(h, ...) {
              temp_dia <- gwindow(
                title = "Spatial Distribution of Fishery sampling",
                visible = FALSE, parent = main_win,
                width = 700, height = 500
              )
              pop_g <- ggroup(horizontal = FALSE, container = temp_dia)
              pop_g_top <- gframe(horizontal = TRUE, container = pop_g, spacing = 10)
              addSpring(pop_g_top)
              spec_b <- gframe("Species",
                               horizontal = FALSE, container = pop_g_top,
                               expand = TRUE
              )
              addSpring(spec_b)
              spec_drop <- gcombobox(
                items = as.character(smartRstudy$specieInFishery),
                selected = 1,
                container = spec_b, editable = FALSE,
                handler = function(h, ...) {
                  spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
                  sex_drop[] <- smartRstudy$fisheryBySpecie[[spe_ind]]$speSex
                  svalue(sex_drop) <- sex_drop[1]
                }
              )
              spec_drop$set_size(value = c(width = 150))
              addSpring(spec_b)
              sex_b <- gframe("Sex",
                              horizontal = FALSE, container = pop_g_top,
                              expand = TRUE
              )
              addSpring(pop_g_top)
              addSpring(sex_b)
              sex_drop <- gcombobox(
                items = c("Female", "Male", "Unsex"),
                selected = 1, container = sex_b, expand = TRUE,
                editable = FALSE, handler = function(h, ...) {
                  spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
                  suppressWarnings(grid.arrange(smartRstudy$sampMap$ggMapFgFishery,
                                                smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                                smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                                smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                                layout_matrix = rbind(
                                                  c(NA, 1, 1),
                                                  c(4, 1, 1),
                                                  c(NA, 2, 3)
                                                )
                  ))
                }
              )
              addSpring(sex_b)
              addSpring(pop_g_top)
              
              gbutton("Close", container = pop_g_top, handler = function(h, ...) {
                dispose(temp_dia)
              })
              addSpring(pop_g_top)
              addSpace(pop_g_top, 10)
              sex_drop[] <- smartRstudy$fisheryBySpecie[[1]]$speSex
              svalue(sex_drop) <- sex_drop[1]
              
              pop_p <- ggraphics(
                container = pop_g, width = 650, height = 450,
                expand = TRUE
              )
              visible(temp_dia) <- TRUE
              
              spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop))
              suppressWarnings(grid.arrange(smartRstudy$sampMap$ggMapFgFishery,
                                            smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbsFreq"]],
                                            smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatRelFreq"]],
                                            smartRstudy$fisheryBySpecie[[spe_ind]]$sprePlot[[svalue(sex_drop)]][["spatAbbTbl"]],
                                            layout_matrix = rbind(
                                              c(NA, 1, 1),
                                              c(4, 1, 1),
                                              c(NA, 2, 3)
                                            )
              ))
            }
    )
    addSpring(fis_g_top)
    
    fis_g_top2 <- ggroup(horizontal = FALSE, container = fis_g_top)
    fis_l1 <- glabel("Species: ", container = fis_g_top2)
    fis_l3 <- glabel("Years: ", container = fis_g_top2)
    addSpace(fis_g_top, 10)
    addSpace(fis_g_top2, 2)
    
    blankDF <- data.frame(
      Species = character(0),
      Lat = numeric(0),
      Lon = numeric(0),
      Date = character(0),
      Length = numeric(0),
      Female = character(0),
      Male = character(0),
      Unsex = character(0),
      stringsAsFactors = FALSE
    )
    fis_t <- gtable(blankDF, container = fis_g, expand = TRUE)
    
    
    ####   Mixture   ####
    
    mix_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Mixture")
    mix_g_top <- gframe(horizontal = TRUE, container = mix_g) # if expand bad
    addSpace(mix_g_top, 20)
    cont_g <- gframe("Mixture Analysis",
                     horizontal = TRUE,
                     container = mix_g_top, expand = TRUE
    )
    addSpring(cont_g)
    sourceMix_r <- gradio(
      items = c("Survey", "Fishery"), horizontal = FALSE,
      container = cont_g, expand = TRUE,
      handler = function(h, ...) {
        if (svalue(sourceMix_r) == "Survey") {
          if (is.null(smartRstudy$specieInSurvey)) {
            spec_drop_mix[] <- "No data"
            svalue(spec_drop_mix) <- "No data"
          } else {
            spec_drop_mix[] <- smartRstudy$specieInSurvey
            svalue(spec_drop_mix) <- smartRstudy$specieInSurvey[1]
            sex_drop_mix[] <- smartRstudy$surveyBySpecie[[1]]$speSex
            svalue(sex_drop_mix) <- sex_drop_mix[1]
          }
        } else {
          if (is.null(smartRstudy$specieInFishery)) {
            spec_drop_mix[] <- "No data"
            svalue(spec_drop_mix) <- "No data"
          } else {
            spec_drop_mix[] <- smartRstudy$specieInFishery
            svalue(spec_drop_mix) <- smartRstudy$specieInFishery[1]
            sex_drop_mix[] <- smartRstudy$fisheryBySpecie[[1]]$speSex
            svalue(sex_drop_mix) <- sex_drop_mix[1]
          }
        }
      }
    )
    spec_mix_f <- gframe("Species and Sex",
                         horizontal = FALSE,
                         container = cont_g, expand = TRUE
    )
    addSpring(spec_mix_f)
    spec_drop_mix <- gcombobox(
      items = "Species", selected = 1,
      container = spec_mix_f, editable = FALSE,
      expand = TRUE, handler = function(...) {
        if (svalue(sourceMix_r) == "Survey") {
          spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop_mix))
          sex_drop_mix[] <- smartRstudy$surveyBySpecie[[spe_ind]]$speSex
          svalue(sex_drop_mix) <- sex_drop_mix[1]
        } else {
          spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop_mix))
          sex_drop_mix[] <- smartRstudy$fisheryBySpecie[[spe_ind]]$speSex
          svalue(sex_drop_mix) <- sex_drop_mix[1]
        }
      }
    )
    spec_drop_mix$set_size(value = c(width = 150))
    addSpring(spec_mix_f)
    sex_drop_mix <- gcombobox(
      items = c("Female", "Male", "Unsex"), selected = 1,
      container = spec_mix_f, editable = FALSE,
      expand = TRUE
    )
    addSpring(spec_mix_f)
    addSpring(cont_g)
    ncoh_f <- gframe("N. cohorts", horizontal = FALSE, container = cont_g)
    addSpring(ncoh_f)
    ncih_sb <- gspinbutton(
      from = 1, to = 100, by = 1, value = 3, digits = 0,
      container = ncoh_f
    )
    addSpring(ncoh_f)
    
    addSpring(cont_g)
    
    gcurv_f <- gframe("Growth Curve", horizontal = FALSE, container = cont_g)
    addSpring(gcurv_f)
    gcurv_r <- gradio(
      items = c("von Bertalanffy", "Gompertz"),
      horizontal = FALSE, container = gcurv_f, expand = TRUE
    )
    addSpring(gcurv_f)
    
    addSpring(cont_g)
    
    go_g <- gframe("MCMC sim",
                   horizontal = TRUE, container = cont_g,
                   expand = TRUE
    )
    addSpring(go_g)
    go_g_ada <- ggroup(horizontal = FALSE, container = go_g, expand = TRUE)
    addSpring(go_g_ada)
    glabel("N. adapt: ", container = go_g_ada)
    mc_niter <- gcombobox(c(100, 1000, 5000, 10000),
                          selected = 2,
                          container = go_g_ada, editable = FALSE, expand = TRUE
    )
    addSpring(go_g_ada)
    go_g_sam <- ggroup(horizontal = FALSE, container = go_g)
    addSpring(go_g_sam)
    glabel("Sample size: ", container = go_g_sam)
    mc_nsamp <- gcombobox(c(100, 1000, 5000, 10000),
                          selected = 2,
                          container = go_g_sam, editable = FALSE, expand = TRUE
    )
    addSpring(go_g_sam)
    addSpring(go_g)
    go_b <- gbutton("   GO   ",
                    container = go_g,
                    handler = function(h, ...) {
                      dev.set(dev.list()[pre_dev + 7])
                      svalue(view_radio) <- "MCMC"
                      if (svalue(sourceMix_r) == "Survey") {
                        pre_mfrow <- par(c("mfrow", "mar"))
                        par(mfrow = c(2, 1))
                        par(mar = c(2, 2, 1.5, 0.5))
                        ind_spe <- which(smartRstudy$specieInSurvey == svalue(spec_drop_mix))
                        smartRstudy$surveyBySpecie[[ind_spe]]$setNCoho(as.numeric(svalue(ncih_sb)))
                        smartRstudy$surveyBySpecie[[ind_spe]]$calcMixDate(
                          nAdap = as.numeric(svalue(mc_niter)),
                          nSamp = as.numeric(svalue(mc_nsamp)),
                          sexDrop = svalue(sex_drop_mix),
                          curveSel = svalue(gcurv_r)
                        )
                        smartRstudy$surveyBySpecie[[ind_spe]]$ggplotMcmcOut(selSex = svalue(sex_drop_mix))
                        cohCoh_drop[] <- c("All", seq(1, smartRstudy$surveyBySpecie[[ind_spe]]$nCoho, by = 1))
                        
                        # Update Cohort droplist
                        blockHandlers(obj = cohCoh_drop)
                        spec_drop_coh[] <- smartRstudy$specieInSurvey
                        svalue(spec_drop_coh) <- smartRstudy$specieInSurvey[1]
                        unblockHandlers(obj = cohCoh_drop)
                      } else {
                        pre_mfrow <- par(c("mfrow", "mar"))
                        par(mfrow = c(2, 4))
                        par(mar = c(2, 2, 1.5, 0.5))
                        ind_spe <- which(smartRstudy$specieInFishery == svalue(spec_drop_mix))
                        smartRstudy$fisheryBySpecie[[ind_spe]]$setNCoho(as.numeric(svalue(ncih_sb)))
                        smartRstudy$fisheryBySpecie[[ind_spe]]$calcMixDate(
                          nAdap = as.numeric(svalue(mc_niter)),
                          nSamp = as.numeric(svalue(mc_nsamp)),
                          sexDrop = svalue(sex_drop_mix),
                          curveSel = svalue(gcurv_r)
                        )
                        smartRstudy$fisheryBySpecie[[ind_spe]]$ggplotMcmcOut(selSex = svalue(sex_drop_mix))
                      }
                      par(pre_mfrow)
                    }
    )
    addSpring(go_g)
    # save_b <- gbutton ("  SAVE  ", container = go_g)
    addSpring(cont_g)
    view_g <- gframe("View",
                     horizontal = TRUE, container = cont_g,
                     expand = TRUE
    )
    view_radio <- gradio(c("MCMC", "Key", "Birth"),
                         selected = 1,
                         horizontal = FALSE, container = view_g,
                         handler = function(h, ...) {
                           dev.set(dev.list()[pre_dev + 7])
                           if (svalue(sourceMix_r) == "Survey") {
                             smartRstudy$surveyBySpecie[[which(smartRstudy$specieInSurvey == svalue(spec_drop_mix))]]$ggplotMcmcOut(
                               selCompo = svalue(view_radio),
                               selSex = svalue(sex_drop_mix)
                             )
                           } else {
                             smartRstudy$fisheryBySpecie[[which(smartRstudy$specieInFishery == svalue(spec_drop_mix))]]$ggplotMcmcOut(
                               selCompo = svalue(view_radio),
                               selSex = svalue(sex_drop_mix)
                             )
                           }
                         }
    )
    addSpring(cont_g)
    addSpace(mix_g_top, 20)
    mix_p <- ggraphics(
      container = mix_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Cohorts   ####
    
    cohoP_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Cohorts")
    cohoP_g_top <- gframe(horizontal = TRUE, container = cohoP_g, spacing = 10)
    addSpace(cohoP_g_top, 2)
    cohofra_g <- gframe("Cohort data",
                        horizontal = TRUE,
                        container = cohoP_g_top,
                        expand = TRUE
    )
    addSpring(cohofra_g)
    sourceCoh_r <- gcombobox(
      items = c("Survey", "Fishery"), horizontal = FALSE,
      container = cohofra_g, expand = TRUE,
      editable = FALSE, handler = function(h, ...) {
        if (svalue(sourceCoh_r) == "Survey") {
          if (is.null(smartRstudy$specieInSurvey)) {
            blockHandlers(obj = spec_drop_coh)
            blockHandlers(obj = sexRadio_coh)
            blockHandlers(obj = cohCoh_drop)
            spec_drop_coh[] <- "No data"
            svalue(spec_drop_coh) <- spec_drop_coh[1]
            sexRadio_coh[] <- "No data"
            svalue(sexRadio_coh) <- sexRadio_coh[1]
            cohCoh_drop[] <- "No data"
            svalue(cohCoh_drop) <- cohCoh_drop[1]
            unblockHandlers(obj = spec_drop_coh)
            unblockHandlers(obj = sexRadio_coh)
            unblockHandlers(obj = cohCoh_drop)
          } else {
            spec_drop_coh[] <- smartRstudy$specieInSurvey
            svalue(spec_drop_coh) <- smartRstudy$specieInSurvey[1]
          }
        } else {
          if (is.null(smartRstudy$specieInFishery)) {
            blockHandlers(obj = spec_drop_coh)
            blockHandlers(obj = sexRadio_coh)
            blockHandlers(obj = cohCoh_drop)
            spec_drop_coh[] <- "No data"
            svalue(spec_drop_coh) <- spec_drop_coh[1]
            sexRadio_coh[] <- "No data"
            svalue(sexRadio_coh) <- sexRadio_coh[1]
            cohCoh_drop[] <- "No data"
            svalue(cohCoh_drop) <- cohCoh_drop[1]
            unblockHandlers(obj = spec_drop_coh)
            unblockHandlers(obj = sexRadio_coh)
            unblockHandlers(obj = cohCoh_drop)
          } else {
            spec_drop_coh[] <- smartRstudy$specieInFishery
            svalue(spec_drop_coh) <- smartRstudy$specieInFishery[1]
          }
        }
      }
    )
    addSpring(cohofra_g)
    cohSpe_b <- gframe("Species",
                       horizontal = FALSE, container = cohofra_g,
                       expand = TRUE
    )
    addSpring(cohofra_g)
    addSpring(cohSpe_b)
    spec_drop_coh <- gcombobox(
      items = "Species", selected = 1,
      container = cohSpe_b, editable = FALSE,
      handler = function(h, ...) {
        if (svalue(sourceCoh_r) == "Survey") {
          spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop_coh))
          sexRadio_coh[] <- smartRstudy$surveyBySpecie[[spe_ind]]$speSex
          svalue(sexRadio_coh) <- sexRadio_coh[1]
        } else {
          spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop_coh))
          sexRadio_coh[] <- smartRstudy$fisheryBySpecie[[spe_ind]]$speSex
          svalue(sexRadio_coh) <- sexRadio_coh[1]
        }
        svalue(cohCoh_drop, index = TRUE) <- 1
      }
    )
    addSpring(cohSpe_b)
    spec_drop_coh$set_size(value = c(width = 150))
    
    cohSex_b <- gframe("Sex",
                       horizontal = FALSE, container = cohofra_g,
                       expand = TRUE
    )
    addSpring(cohofra_g)
    addSpring(cohSex_b)
    sexRadio_coh <- gcombobox(
      items = c("Female", "Male", "Unsex"),
      selected = 1,
      container = cohSex_b,
      expand = TRUE, editable = FALSE,
      handler = function(h, ...) {
        if (svalue(sourceCoh_r) == "Survey") {
          spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop_coh))
          if (!is.null(names(smartRstudy$surveyBySpecie[[spe_ind]]$groMixout))) {
            cohCoh_drop[] <- sort(unique(smartRstudy$surveyBySpecie[[spe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Age))
            svalue(cohCoh_drop, index = TRUE) <- 1
          } else {
            blockHandlers(obj = cohCoh_drop)
            cohCoh_drop[] <- "Missing Mixture Results"
            unblockHandlers(obj = cohCoh_drop)
          }
        } else {
          spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop_coh))
          if (!is.null(names(smartRstudy$fisheryBySpecie[[spe_ind]]$groMixout))) {
            cohCoh_drop[] <- sort(unique(smartRstudy$fisheryBySpecie[[spe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Age))
            svalue(cohCoh_drop, index = TRUE) <- 1
          } else {
            blockHandlers(obj = cohCoh_drop)
            cohCoh_drop[] <- "Missing Mixture Results"
            unblockHandlers(obj = cohCoh_drop)
          }
        }
      }
    )
    addSpring(cohSex_b)
    
    cohCoh_b <- gframe("Cohort",
                       horizontal = FALSE, container = cohofra_g,
                       expand = TRUE
    )
    addSpring(cohofra_g)
    addSpring(cohCoh_b)
    cohCoh_drop <- gcombobox(
      items = "Age", selected = 1, container = cohCoh_b,
      editable = FALSE, handler = function(h, ...) {
        dev.set(dev.list()[pre_dev + 8])
        if (svalue(sourceCoh_r) == "Survey") {
          cohSpe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop_coh))
          if (!is.null(names(smartRstudy$surveyBySpecie[[cohSpe_ind]]$groMixout))) {
            tmpLFD <- smartRstudy$surveyBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]][smartRstudy$surveyBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Age == svalue(cohCoh_drop), ]
            tmpLFD$UTC <- tmpLFD$Date
            tmp_palette <- rainbow(max(cohCoh_drop[]) + 1)
            suppressWarnings(grid.arrange(set_ggHistLfdTot(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggHistUtcLfd(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggHistUtcTot(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggDotUtcSplit(inLfd = tmpLFD) +
                                            scale_color_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          layout_matrix = rbind(
                                            c(1, 1, 1, 3),
                                            c(2, 2, 2, 4),
                                            c(2, 2, 2, 4)
                                          )
            ))
          }
        } else {
          cohSpe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop_coh))
          if (!is.null(names(smartRstudy$fisheryBySpecie[[cohSpe_ind]]$groMixout))) {
            tmpLFD <- smartRstudy$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]][smartRstudy$fisheryBySpecie[[cohSpe_ind]]$groMixout[[svalue(sexRadio_coh)]]$Age == svalue(cohCoh_drop), ]
            tmpLFD$UTC <- tmpLFD$Date
            tmp_palette <- rainbow(max(cohCoh_drop[]) + 1)
            suppressWarnings(grid.arrange(set_ggHistLfdTot(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggHistUtcLfd(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggHistUtcTot(inLfd = tmpLFD) +
                                            scale_fill_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          set_ggDotUtcSplit(inLfd = tmpLFD) +
                                            scale_color_manual(values = tmp_palette[svalue(cohCoh_drop) + 1]),
                                          layout_matrix = rbind(
                                            c(1, 1, 1, 3),
                                            c(2, 2, 2, 4),
                                            c(2, 2, 2, 4)
                                          )
            ))
          }
        }
      }
    )
    cohCoh_drop$set_size(value = c(width = 150))
    addSpring(cohCoh_b)
    addSpring(cohofra_g)
    addSpace(cohoP_g_top, 2)
    cohPop_p <- ggraphics(
      container = cohoP_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    ####   Simulation   ####
    
    icoEffIndex_off <- gimage(system.file("ico/user-invisible.png",
                                          package = "smartR"
    ))
    icoEffIndex_on <- gimage(system.file("ico/user-available.png",
                                         package = "smartR"
    ))
    icoSeaIndex_off <- gimage(system.file("ico/user-invisible.png",
                                          package = "smartR"
    ))
    icoSeaIndex_on <- gimage(system.file("ico/user-available.png",
                                         package = "smartR"
    ))
    icoProdIndex_off <- gimage(system.file("ico/user-invisible.png",
                                           package = "smartR"
    ))
    icoProdIndex_on <- gimage(system.file("ico/user-available.png",
                                          package = "smartR"
    ))
    
    sim_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Simulation")
    sim_g_top <- gframe(horizontal = TRUE, container = sim_g, spacing = 10)
    addSpace(sim_g_top, 20)
    
    sim_g_Effo <- gframe(
      text = "Effort Index", horizontal = FALSE,
      container = sim_g_top
    )
    gbutton("Get", container = sim_g_Effo, handler = function(h, ...) {
      smartRstudy$setEffortIndex()
      dev.set(dev.list()[pre_dev + 9])
      print(ggplot_effoIndBoxplot(df_EffoInde = smartRstudy$fleet$effortIndex))
      
      if (!is.null(smartRstudy$fleet$effortIndex)) {
        delete(sim_g_Effo, sim_g_Effo$children[[length(sim_g_Effo$children)]])
        add(sim_g_Effo, icoEffIndex_on)
      }
    })
    add(sim_g_Effo, icoEffIndex_off)
    addSpace(sim_g_top, 10)
    
    sim_g_SeaDays <- gframe(
      text = "Days At Sea", horizontal = FALSE,
      container = sim_g_top
    )
    gbutton("Get", container = sim_g_SeaDays, handler = function(h, ...) {
      smartRstudy$setDaysAtSea()
      dev.set(dev.list()[pre_dev + 9])
      print(ggplot_seaDaysBoxplot(df_seaDays = smartRstudy$fleet$daysAtSea))
      
      if (!is.null(smartRstudy$fleet$daysAtSea)) {
        delete(sim_g_SeaDays, sim_g_SeaDays$children[[length(sim_g_SeaDays$children)]])
        add(sim_g_SeaDays, icoSeaIndex_on)
      }
    })
    add(sim_g_SeaDays, icoSeaIndex_off)
    addSpace(sim_g_top, 10)
    
    sim_g_Prod <- gframe(
      text = "Production Index", horizontal = FALSE,
      container = sim_g_top
    )
    gbutton("Get", container = sim_g_Prod, handler = function(h, ...) {
      smartRstudy$setProductionIndex()
      dev.set(dev.list()[pre_dev + 9])
      print(ggplot_prodIndBoxplot(df_ProdInde = smartRstudy$fleet$prodIndex))
      
      if (!is.null(smartRstudy$fleet$prodIndex)) {
        delete(sim_g_Prod, sim_g_Prod$children[[length(sim_g_Prod$children)]])
        add(sim_g_Prod, icoProdIndex_on)
      }
    })
    add(sim_g_Prod, icoProdIndex_off)
    
    addSpace(sim_g_top, 20)
    
    sim_g_top2 <- ggroup(horizontal = FALSE, container = sim_g_top)
    addSpring(sim_g_top2)
    gbutton("Set Cost Data", container = sim_g_top2, handler = function(h, ...) {
      tempWind_Cost <- gwindow(
        title = "Fishing Cost Data", visible = FALSE,
        parent = main_win,
        width = 800, height = 500
      )
      
      cost_g <- ggroup(
        horizontal = FALSE, container = tempWind_Cost,
        spacing = 15
      )
      cost_g_top <- gframe(horizontal = TRUE, container = cost_g, spacing = 20)
      
      addSpring(cost_g_top)
      gbutton("   Load\nCost Data",
              container = cost_g_top,
              handler = function(h, ...) {
                pathCosts <- gfile(
                  text = "Select Costs File", type = "open",
                  initial.filename = NULL, initial.dir = getwd(),
                  filter = list(),
                  multi = FALSE
                )
                smartRstudy$fleet$loadRawEconomy(economic_path = pathCosts)
                smartRstudy$fleet$setYearEconomy()
              }
      )
      addSpring(cost_g_top)
      gbutton("         Get\nCost Regression",
              container = cost_g_top,
              handler = function(h, ...) {
                smartRstudy$setCostInput()
                smartRstudy$fleet$getCostOutput()
                smartRstudy$fleet$setCostPlot()
                suppressWarnings(grid.arrange(smartRstudy$fleet$plotSpatialReg,
                                              smartRstudy$fleet$plotEffortReg,
                                              smartRstudy$fleet$plotProductionReg,
                                              layout_matrix = rbind(c(1, 2, 3), c(1, 2, 3))
                ))
              }
      )
      addSpace(cost_g_top, 10)
      gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
             container = cost_g_top,
             handler = function(h, ...) {
               suppressWarnings(grid.arrange(smartRstudy$fleet$plotSpatialReg,
                                             smartRstudy$fleet$plotEffortReg,
                                             smartRstudy$fleet$plotProductionReg,
                                             layout_matrix = rbind(
                                               c(1, 2, 3),
                                               c(1, 2, 3)
                                             )
               ))
             }
      )
      addSpring(cost_g_top)
      gbutton("         Set\nCost Prediction",
              container = cost_g_top,
              handler = function(h, ...) {
                # smartRstudy$fleet$effortIndex$spatialCostPred <- predict(spatialCostLm, smartRstudy$fleet$effortIndex)
                # smartRstudy$fleet$daysAtSea$effortCostPred <- predict(effortCostLm, smartRstudy$fleet$daysAtSea)
                # tmp_Prod$prodCostPred <- predict(prodCostLm, tmp_Prod)
              }
      )
      addSpace(cost_g_top, 10)
      gimage(system.file("ico/view-refresh-5.ico", package = "smartR"),
             container = cost_g_top,
             handler = function(h, ...) {
               
             }
      )
      addSpring(cost_g_top)
      
      visible(tempWind_Cost) <- TRUE
      
      cost_p <- ggraphics(
        container = cost_g, width = 550, height = 250,
        expand = TRUE
      )
    })
    addSpring(sim_g_top2)
    gbutton("Set Size Class", container = sim_g_top2, handler = function(h, ...) {
      if (is.null(smartRstudy$fleet$ecoPrice)) {
        tmp_df <- data.frame(
          Class = c("Small", "Large"),
          Units = factor(
            x = c("Length", "Length"),
            levels = c("Length", "Weight")
          ),
          LowerBound = c(1, 16),
          UpperBound = c(16, Inf),
          Price = c(7, 20),
          stringsAsFactors = FALSE, row.names = NULL
        )
      } else {
        tmp_df <- smartRstudy$fleet$ecoPrice[[unique(c(
          smartRstudy$specieInFishery,
          smartRstudy$specieInSurvey
        ))[1]]]
      }
      
      # out_SizeClass <- list()
      
      tempWind_Gain <- gwindow(
        title = "Size Class", visible = FALSE,
        parent = main_win,
        width = 700, height = 400
      )
      
      gain_g <- ggroup(
        horizontal = FALSE, container = tempWind_Gain,
        spacing = 15
      )
      gain_g_top <- gframe(horizontal = TRUE, container = gain_g, spacing = 20)
      
      addSpace(gain_g_top, 15)
      sel_specie <- gcombobox(
        items = unique(c(
          smartRstudy$specieInFishery,
          smartRstudy$specieInSurvey
        )),
        selected = 1, container = gain_g_top, expand = TRUE,
        handler = function(...) {
          if (!is.null(smartRstudy$fleet$ecoPrice[[svalue(sel_specie)]])) {
            cost_df[] <- smartRstudy$fleet$ecoPrice[[svalue(sel_specie)]]
          } else {
            cost_df[] <- tmp_df
          }
        }
      )
      sel_specie$set_size(value = c(width = 150))
      addSpace(gain_g_top, 15)
      add_class <- gbutton(
        text = "Add Size Class", container = gain_g_top,
        handler = function(...) {
          new_row <- data.frame(
            Class = "New Class",
            Units = "Length",
            LowerBound = 1,
            UpperBound = 5,
            Price = 2
          )
          cost_df[] <- rbind(cost_df[], new_row)
        }
      )
      addSpring(gain_g_top)
      set_data <- gbutton(
        text = "Set Data", container = gain_g_top,
        handler = function(...) {
          smartRstudy$fleet$setEcoPrice(
            sel_specie = svalue(sel_specie),
            price_df = cost_df[]
          )
        }
      )
      addSpring(gain_g_top)
      ioButt_g <- ggroup(
        horizontal = FALSE, container = gain_g_top,
        expand = TRUE
      )
      gimage(system.file("ico/document-save-2.ico", package = "smartR"),
             container = ioButt_g
      )
      gimage(system.file("ico/folder-man.png", package = "smartR"),
             container = ioButt_g
      )
      addSpring(gain_g_top)
      close_butt <- gbutton(
        text = "Close", container = gain_g_top,
        handler = function(...) {
          delete(dafra_g, cost_df)
          dispose(tempWind_Gain)
        }
      )
      addSpring(gain_g_top)
      
      dafra_g <- ggroup(horizontal = TRUE, container = gain_g, expand = TRUE)
      addSpace(dafra_g, 30)
      cost_df <- gdf(items = tmp_df, container = dafra_g, expand = TRUE)
      addSpace(dafra_g, 30)
      addSpace(gain_g, 30)
      
      visible(tempWind_Gain) <- TRUE
    })
    addSpring(sim_g_top2)
    gbutton("Set length weight\nrelationship",
            container = sim_g_top2,
            handler = function(h, ...) {
              tempWind_LWrel <- gwindow(
                title = "Length-Weight Relationship",
                visible = FALSE,
                parent = main_win,
                width = 900, height = 600
              )
              
              lwRel_g <- ggroup(
                horizontal = TRUE, container = tempWind_LWrel,
                label = "LW relationship"
              )
              addSpace(lwRel_g, 10)
              
              lwRel_g_top <- ggroup(horizontal = FALSE, container = lwRel_g)
              addSpace(lwRel_g_top, 10)
              
              assfra_g <- gframe("Target",
                                 horizontal = FALSE,
                                 spacing = 10,
                                 container = lwRel_g_top
              )
              # assfra_g$set_size(value = c(width = 200))
              addSpace(assfra_g, 5)
              
              assSou_g <- gframe("Source", horizontal = TRUE, container = assfra_g)
              addSpace(assSou_g, 5)
              assSou_r <- gcombobox(c("Survey", "Fishery"),
                                    selected = 1, expand = TRUE,
                                    horizontal = FALSE, container = assSou_g,
                                    handler = function(...) {
                                      if (svalue(assSou_r) == "Survey") {
                                        if (is.null(smartRstudy$specieInSurvey)) {
                                          assSpe_drop[] <- "No data"
                                          svalue(assSpe_drop) <- "No data"
                                        } else {
                                          assSpe_drop[] <- smartRstudy$specieInSurvey
                                          svalue(assSpe_drop) <- smartRstudy$specieInSurvey[1]
                                        }
                                      } else {
                                        if (is.null(smartRstudy$specieInFishery)) {
                                          assSpe_drop[] <- "No data"
                                          svalue(assSpe_drop) <- "No data"
                                        } else {
                                          assSpe_drop[] <- smartRstudy$specieInFishery
                                          svalue(assSpe_drop) <- smartRstudy$specieInFishery[1]
                                        }
                                      }
                                    }
              )
              addSpace(assSou_g, 5)
              addSpace(assfra_g, 10)
              
              assSpe_g <- gframe("Species", horizontal = TRUE, container = assfra_g)
              addSpace(assSpe_g, 5)
              assSpe_drop <- gcombobox(
                items = "Species", selected = 1,
                container = assSpe_g, editable = FALSE,
                handler = function(h, ...) {
                  if (svalue(assSou_r) == "Survey") {
                    spe_ind <- which(smartRstudy$specieInSurvey == svalue(spec_drop_mix))
                    lwRel_sex_drop[] <- smartRstudy$surveyBySpecie[[spe_ind]]$speSex
                    svalue(lwRel_sex_drop) <- lwRel_sex_drop[1]
                  } else {
                    spe_ind <- which(smartRstudy$specieInFishery == svalue(spec_drop_mix))
                    lwRel_sex_drop[] <- smartRstudy$fisheryBySpecie[[spe_ind]]$speSex
                    svalue(lwRel_sex_drop) <- lwRel_sex_drop[1]
                  }
                }
              )
              assSpe_drop$set_size(value = c(width = 200))
              addSpace(assSpe_g, 5)
              addSpace(assfra_g, 10)
              
              lwRel_f_sex <- gframe("Sex", horizontal = TRUE, container = assfra_g)
              addSpace(lwRel_f_sex, 5)
              lwRel_sex_drop <- gcombobox(
                items = c("Female", "Male", "Unsex"),
                selected = 1, container = lwRel_f_sex,
                editable = FALSE, expand = TRUE
              )
              addSpace(lwRel_f_sex, 5)
              addSpace(assfra_g, 10)
              
              lwRel_f_meth <- gframe(
                text = "Method",
                horizontal = FALSE,
                container = lwRel_g_top,
                spacing = 10
              )
              addSpace(lwRel_f_meth, 5)
              
              lwRel_f_valu <- gframe(
                text = "Assumption",
                horizontal = TRUE,
                container = lwRel_f_meth,
                spacing = 10
              )
              addSpring(lwRel_f_valu)
              valu_lyt <- glayout(container = lwRel_f_valu, spacing = 10)
              valu_lyt[1, 1] <- "alpha"
              valu_lyt[1, 2] <- gedit(text = "0.01", width = 10, container = valu_lyt)
              valu_lyt[2, 1] <- "beta"
              valu_lyt[2, 2] <- gedit(text = "3.00", width = 10, container = valu_lyt)
              addSpring(lwRel_f_valu)
              
              addSpace(lwRel_f_meth, 5)
              lwRel_f_esti <- gframe(
                text = "Estimation",
                horizontal = TRUE,
                container = lwRel_f_meth,
                spacing = 10
              )
              addSpring(lwRel_f_esti)
              gbutton("\t  Load\nWeighted Sample",
                      container = lwRel_f_esti,
                      handler = function(h, ...) {
                        pathLWrel <- gfile(
                          text = "Select Length-Weight file", type = "open",
                          initial.filename = NULL, initial.dir = getwd(),
                          filter = list(),
                          multi = FALSE
                        )
                        
                        lw_data <- read.csv(pathLWrel)
                        lw_fit <- nls(Weight ~ I(alpha * Length^beta),
                                      data = lw_data[, c("Length", "Weight")],
                                      start = list(alpha = 1, beta = 1)
                        )
                        tmpAlpha <- valu_lyt[1, 2]
                        tmpBeta <- valu_lyt[2, 2]
                        svalue(tmpAlpha) <- round(summary(lw_fit)$coefficients[1, 1], 5)
                        svalue(tmpBeta) <- round(summary(lw_fit)$coefficients[2, 1], 5)
                        
                        if (svalue(assSou_r) == "Survey") {
                          smartRstudy$surveyBySpecie[[which(smartRstudy$specieInSurvey == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(tmpAlpha), betaVal = svalue(tmpBeta), sex = svalue(lwRel_sex_drop))
                        } else {
                          smartRstudy$fisheryBySpecie[[which(smartRstudy$specieInFishery == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(tmpAlpha), betaVal = svalue(tmpBeta), sex = svalue(lwRel_sex_drop))
                        }
                        
                        print(
                          ggplot() +
                            geom_jitter(
                              data = lw_data,
                              mapping = aes_(x = ~Length, y = ~Weight),
                              width = 0.5, size = 0.25, alpha = 0.25,
                              color = "grey5"
                            ) +
                            theme_tufte(base_size = 14, ticks = F) +
                            annotate("line",
                                     x = sort(unique(lw_data$Length)),
                                     y = predict(
                                       lw_fit,
                                       list(Length = sort(unique(lw_data$Length)))
                                     ),
                                     linetype = 2, color = "firebrick", size = 0.8
                            ) +
                            annotate("text",
                                     vjust = 1, hjust = 0, size = 8,
                                     x = min(lw_data$Length),
                                     y = quantile(lw_data$Weight, 0.999),
                                     label = "Weight == alpha * Length ^ beta", parse = TRUE
                            ) +
                            annotate("text",
                                     vjust = 1, hjust = 0, size = 7,
                                     x = min(lw_data$Length) + 5,
                                     y = quantile(lw_data$Weight, 0.999) + 10, parse = TRUE,
                                     label = paste("alpha == ",
                                                   svalue(valu_lyt[1, 2]),
                                                   sep = ""
                                     )
                            ) +
                            annotate("text",
                                     vjust = 1, hjust = 0, size = 7,
                                     x = min(lw_data$Length) + 5,
                                     y = quantile(lw_data$Weight, 0.999) - 10, parse = TRUE,
                                     label = paste("beta == ",
                                                   svalue(valu_lyt[2, 2]),
                                                   sep = ""
                                     )
                            ) +
                            theme(
                              legend.position = "none",
                              panel.grid = element_line(
                                size = 0.5, linetype = 2,
                                colour = "grey20"
                              ),
                              axis.text.x = element_text(size = 9),
                              axis.title.x = element_text(size = 10),
                              axis.text.y = element_text(size = 9),
                              axis.title.y = element_text(size = 10),
                              axis.ticks.y = element_blank()
                            )
                        )
                      }
              )
              addSpring(lwRel_f_esti)
              
              addSpace(lwRel_g_top, 10)
              
              gbutton("\nSet Weight\n",
                      container = lwRel_g_top,
                      handler = function(h, ...) {
                        if (svalue(assSou_r) == "Survey") {
                          smartRstudy$surveyBySpecie[[which(smartRstudy$specieInSurvey == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(valu_lyt[1, 2]), betaVal = svalue(valu_lyt[2, 2]), sex = svalue(lwRel_sex_drop))
                        } else {
                          smartRstudy$fisheryBySpecie[[which(smartRstudy$specieInFishery == svalue(assSpe_drop))]]$setLWpar(alphaVal = svalue(valu_lyt[1, 2]), betaVal = svalue(valu_lyt[2, 2]), sex = svalue(lwRel_sex_drop))
                        }
                        if (svalue(assSou_r) == "Fishery") {
                          smartRstudy$fisheryBySpecie[[which(smartRstudy$specieInFishery == svalue(assSpe_drop))]]$setWeight(sexVal = svalue(lwRel_sex_drop))
                        } else {
                          smartRstudy$surveyBySpecie[[which(smartRstudy$specieInSurvey == svalue(assSpe_drop))]]$setWeight(sexVal = svalue(lwRel_sex_drop))
                        }
                      }
              )
              
              addSpring(lwRel_g_top)
              gbutton("Close", container = lwRel_g_top, handler = function(h, ...) {
                dispose(tempWind_LWrel)
              })
              addSpace(lwRel_g_top, 10)
              
              visible(tempWind_LWrel) <- TRUE
              
              svalue(assSou_r, index = TRUE) <- 1
              assSpe_drop[] <- smartRstudy$specieInSurvey
              svalue(assSpe_drop) <- smartRstudy$specieInSurvey[1]
              
              addSpace(lwRel_g, 10)
              lwRel_p <- ggraphics(
                container = lwRel_g, width = 550, height = 550,
                expand = TRUE
              )
              addSpace(lwRel_g, 10)
            }
    )
    addSpring(sim_g_top2)
    
    
    addSpace(sim_g_top, 20)
    
    sim_g_Sim <- gframe(
      text = "Scenario", horizontal = TRUE,
      container = sim_g_top, expand = TRUE
    )
    addSpace(sim_g_Sim, 10)
    sim_g_SimPar <- ggroup(horizontal = TRUE, container = sim_g_Sim)
    addSpring(sim_g_SimPar)
    sim_g_SimPar2 <- ggroup(horizontal = FALSE, container = sim_g_SimPar)
    addSpring(sim_g_SimPar2)
    sim_f_Thr <- gframe(
      text = "Threshold", horizontal = TRUE,
      container = sim_g_SimPar2
    )
    sim_Thr <- gslider(
      from = 0, to = 100, by = 1, value = 10,
      container = sim_f_Thr
    )
    addSpring(sim_g_SimPar2)
    addSpring(sim_g_SimPar)
    sim_g_SimPar3 <- ggroup(horizontal = FALSE, container = sim_g_SimPar)
    addSpring(sim_g_SimPar3)
    sim_f_Sca <- gframe(
      text = "Time Scale", horizontal = TRUE,
      container = sim_g_SimPar3
    )
    addSpring(sim_f_Sca)
    sim_Sca <- gradio(
      items = c("Year", "Season"), selected = 1,
      horizontal = TRUE, container = sim_f_Sca
    )
    addSpring(sim_f_Sca)
    addSpring(sim_g_SimPar3)
    addSpring(sim_g_SimPar3)
    sim_Ban <- gbutton("Set Closed Area",
                       container = sim_g_SimPar3,
                       handler = function(h, ...) {
                         tempWind_Area <- gwindow(
                           title = "Set Closed Area",
                           parent = main_win,
                           width = 800, height = 600
                         )
                         
                         bigGroup <- ggroup(horizontal = TRUE, container = tempWind_Area)
                         cloAre_p <- ggraphics(container = bigGroup, width = 600, height = 550)
                         
                         grid_data <- merge(
                           x = smartRstudy$sampMap$cutResShpFort,
                           y = smartRstudy$simBanFG, all.x = TRUE
                         )
                         tmp_coo <- smartRstudy$sampMap$cutResShpCent
                         print(smartRstudy$sampMap$gooMapPlot +
                                 geom_polygon(aes_(
                                   x = ~long, y = ~lat, group = ~group,
                                   fill = ~Banned
                                 ),
                                 colour = "black", size = 0.1,
                                 data = grid_data, alpha = 0.8
                                 ) +
                                 scale_fill_manual("Banned Areas",
                                                   values = c("Banned" = "red", "0" = "blue")
                                 ) +
                                 geom_text(aes_(label = ~FG, x = ~Lon, y = ~Lat),
                                           data = tmp_coo, size = 2
                                 ) +
                                 theme(legend.position = "none") +
                                 xlab("Longitude") + ylab("Latitude") +
                                 scale_x_continuous(expand = c(0, 0)) +
                                 scale_y_continuous(expand = c(0, 0)))
                         
                         smaGroup <- ggroup(horizontal = FALSE, container = bigGroup, expand = TRUE)
                         addSpace(smaGroup, 15)
                         
                         tmp_Ban <- smartRstudy$simBanFG
                         tmp_Ban$Banned <- factor(tmp_Ban$Banned, levels = c("0", "Banned"))
                         
                         cloDF_g <- ggroup(horizontal = TRUE, container = smaGroup, expand = TRUE)
                         addSpace(cloDF_g, 20)
                         close_df <- gdf(items = tmp_Ban, container = cloDF_g, expand = TRUE)
                         addSpace(cloDF_g, 20)
                         
                         addHandlerChanged(obj = close_df, , handler = function(...) {
                           if (sum(which(!smartRstudy$simBanFG$Banned == close_df[]$Banned)) > 0) {
                             idChan <- which(!smartRstudy$simBanFG$Banned == close_df[]$Banned)
                             cat("\nChanged FG ", idChan, ": ",
                                 as.character(smartRstudy$simBanFG$Banned[idChan]), " -> ",
                                 as.character(close_df[]$Banned[idChan]),
                                 sep = ""
                             )
                             smartRstudy$simBanFG$Banned <- as.character(close_df[]$Banned)
                             grid_data <- merge(
                               x = smartRstudy$sampMap$cutResShpFort,
                               y = smartRstudy$simBanFG, all.x = TRUE
                             )
                             tmp_coo <- smartRstudy$sampMap$cutResShpCent
                             print(smartRstudy$sampMap$gooMapPlot +
                                     geom_polygon(aes_(
                                       x = ~long, y = ~lat, group = ~group,
                                       fill = ~Banned
                                     ),
                                     colour = "black", size = 0.1,
                                     data = grid_data, alpha = 0.8
                                     ) +
                                     scale_fill_manual("Banned Areas",
                                                       values = c("Banned" = "red", "0" = "blue")
                                     ) +
                                     geom_text(aes_(label = ~FG, x = ~Lon, y = ~Lat),
                                               data = tmp_coo, size = 2
                                     ) +
                                     theme(legend.position = "none") +
                                     xlab("Longitude") + ylab("Latitude") +
                                     scale_x_continuous(expand = c(0, 0)) +
                                     scale_y_continuous(expand = c(0, 0)))
                           }
                         })
                         
                         addSpace(smaGroup, 15)
                         endArea <- gbutton(
                           text = "\nClose\n", container = smaGroup,
                           handler = function(h, ...) {
                             delete(cloDF_g, close_df)
                             dispose(tempWind_Area)
                           }
                         )
                         addSpace(smaGroup, 15)
                       }
    )
    addSpring(sim_g_SimPar3)
    addSpring(sim_g_SimPar)
    addSpring(sim_g_Sim)
    gbutton("   Start\nSimulation",
            container = sim_g_Sim,
            handler = function(h, ...) {
              dev.set(dev.list()[pre_dev + 9])
              svalue(sim_Res_radio) <- "Summary"
              cat("\n\nSimulating ", sep = "")
              smartRstudy$simEffo <- NULL
              gc()
              smartRstudy$genSimEffo()
              smartRstudy$genSimEffo(
                areaBan = as.numeric(smartRstudy$simBanFG$FG[smartRstudy$simBanFG$Banned == "Banned"])
              )
              smartRstudy$simulateFishery(
                thr0 = svalue(sim_Thr),
                effoBan = as.numeric(smartRstudy$simBanFG$FG[smartRstudy$simBanFG$Banned == "Banned"]),
                timeStep = svalue(sim_Sca)
              )
              smartRstudy$simProdAll()
              smartRstudy$getSimTotalCost()
              smartRstudy$getSimRevenue(timeScale = svalue(sim_Sca))
              smartRstudy$getCostRevenue()
              smartRstudy$setSimResults()
            }
    )
    addSpring(sim_g_Sim)
    
    addSpace(sim_g_top, 10)
    sim_Res <- gframe("View",
                      horizontal = TRUE, container = sim_g_top,
                      expand = TRUE
    )
    sim_Res_radio <- gradio(c("Summary", "Pattern", "Change"),
                            selected = 1,
                            horizontal = FALSE, container = sim_Res,
                            handler = function(h, ...) {
                              dev.set(dev.list()[pre_dev + 9])
                              switch(svalue(sim_Res_radio),
                                     Summary = {},
                                     Pattern = {
                                       suppressWarnings(grid.arrange(smartRstudy$simResPlot[["obsEffort"]],
                                                                     smartRstudy$simResPlot[["optEffort"]],
                                                                     layout_matrix = rbind(
                                                                       c(1, 1, 2, 2),
                                                                       c(1, 1, 2, 2)
                                                                     )
                                       ))
                                     },
                                     Change = {
                                       suppressWarnings(grid.arrange(smartRstudy$simResPlot[["absChange"]],
                                                                     smartRstudy$simResPlot[["relChange"]],
                                                                     layout_matrix = rbind(
                                                                       c(1, 1, 2, 2),
                                                                       c(1, 1, 2, 2)
                                                                     )
                                       ))
                                     }
                              )
                            }
    )
    
    addSpace(sim_g_top, 10)
    
    sim_p <- ggraphics(
      container = sim_g, width = 550, height = 250,
      expand = TRUE
    )
    
    
    
    ####   Assess   ####
    
    ass_g <- ggroup(horizontal = FALSE, container = uti_gn, label = "Assess")
    ass_g_top <- gframe(horizontal = TRUE, container = ass_g, spacing = 10)
    addSpace(ass_g_top, 10)
    assSM_rad <- gradio(
      items = c("Single", "Multi"), selected = 1,
      horizontal = FALSE,
      container = ass_g_top, handler = function(h, ...) {
        if (svalue(assSM_rad) == "Single") {
          enabled(assSpe_drop) <- TRUE
          enabled(assPre_but) <- FALSE
        } else {
          enabled(assSpe_drop) <- FALSE
          enabled(assPre_but) <- TRUE
        }
      }
    )
    addSpace(ass_g_top, 10)
    ass_g_spePred <- ggroup(horizontal = FALSE, container = ass_g_top)
    addSpring(ass_g_spePred)
    assSpe_drop <- gcombobox(
      items = "Species", selected = 1, editable = FALSE,
      container = ass_g_spePred,
      handler = NULL
    )
    assSpe_drop$set_size(value = c(width = 150))
    
    addSpring(ass_g_spePred)
    assPre_but <- gbutton(
      text = "Set Interaction", container = ass_g_spePred,
      handler = function(h, ...) {
        speLst <- intersect(smartRstudy$specieInFishery, smartRstudy$specieInSurvey)
        # speLst <- c("HKE", "MUT", "DPS")
        vertColDF <- data.frame(
          Type = c("Predator", "Prey", "None"),
          Color = c("firebrick", "cornflowerblue", "grey73")
        )
        iteTyp <- rep("None", length(speLst))
        iteChi <- rep("None", length(speLst))
        iteOm <- rep("None", length(speLst))
        # itePer <- rep("None", length(speLst))
        iteIte <- matrix(
          data = "None", nrow = length(speLst),
          ncol = length(speLst), byrow = TRUE
        )
        iteItePar <- matrix(
          data = 0, nrow = length(speLst),
          ncol = length(speLst), byrow = TRUE
        )
        
        tempAssInte <- gwindow(
          title = "Setup Interaction",
          visible = FALSE,
          parent = main_win,
          width = 500, height = 280
        )
        
        toptopAss_g <- ggroup(horizontal = TRUE, container = tempAssInte)
        addSpace(toptopAss_g, 10)
        topAss_g <- ggroup(
          horizontal = FALSE, container = toptopAss_g,
          expand = TRUE
        )
        addSpace(topAss_g, 10)
        
        speWid <- list()
        for (oneSpe in 1:length(speLst)) {
          speWid[[oneSpe]] <- list()
          speWid[[oneSpe]]$fra <- gframe(
            text = speLst[oneSpe], horizontal = FALSE,
            container = topAss_g
          )
          addSpace(speWid[[oneSpe]]$fra, 10)
          speWid[[oneSpe]]$gruA <- ggroup(
            horizontal = TRUE,
            container = speWid[[oneSpe]]$fra
          )
          addSpring(speWid[[oneSpe]]$gruA)
          glabel(text = "Type:", container = speWid[[oneSpe]]$gruA)
          speWid[[oneSpe]]$rad <- gradio(
            items = c("None", "Predator", "Prey"),
            horizontal = TRUE,
            container = speWid[[oneSpe]]$gruA,
            handler = function(h, ...) {
              for (i in 1:length(speLst)) {
                iteTyp[i] <- svalue(speWid[[i]]$rad)
                iteChi[i] <- svalue(speWid[[i]]$gruCeChi)
                iteOm[i] <- svalue(speWid[[i]]$gruCeOm)
                # itePer[i] <- svalue(speWid[[i]]$gruCePer)
                for (s in 1:length(speLst)) {
                  iteIte[i, s] <- as.character(svalue(speWid[[i]]$diet[[s]]$fraBrad))
                  iteItePar[i, s] <- svalue(speWid[[i]]$diet[[s]]$fraBage)
                }
              }
              
              vertColo <- as.character(vertColDF[match(iteTyp, vertColDF[, 1]), 2])
              firWhi <- which(iteIte != "None",
                              arr.ind = TRUE
              )
              edgeLty <- rep(1, nrow(firWhi))
              edgeLty[which(firWhi[, 1] == firWhi[, 2])] <- 2
              
              speGra <- make_empty_graph(n = length(speLst)) %>%
                set_vertex_attr(
                  name = "label",
                  value = speLst
                ) %>%
                add_edges(as.vector(t(firWhi)),
                          color = "firebrick",
                          lty = edgeLty
                ) %>%
                add_vertices(
                  nv = 1,
                  label = "Fishing\nFleet"
                ) %>%
                add_edges(c(rbind(
                  length(speLst) + 1,
                  1:length(speLst)
                )),
                color = "Black", lty = 1
                )
              plot(speGra,
                   main = "Interaction Graph",
                   edge.arrow.size = 0.5,
                   edge.width = 2,
                   vertex.color = c(
                     vertColo,
                     "White"
                   ),
                   vertex.size = c(
                     rep(
                       60,
                       length(vertColo)
                     ),
                     90
                   ),
                   layout = nodeLay,
                   vertex.frame.color = "gray",
                   vertex.label.color = "black",
                   vertex.label.cex = 1.2,
                   edge.curved = 0.1
              )
            }
          )
          addSpring(speWid[[oneSpe]]$gruA)
          speWid[[oneSpe]]$fraB <- gframe(
            text = "Diet", horizontal = FALSE,
            container = speWid[[oneSpe]]$fra
          )
          addSpring(speWid[[oneSpe]]$fraB)
          speWid[[oneSpe]]$diet <- list()
          for (eatSpe in 1:length(speLst)) {
            speWid[[oneSpe]]$diet[[eatSpe]] <- list()
            speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru <- ggroup(
              horizontal = TRUE,
              container = speWid[[oneSpe]]$fraB
            )
            addSpace(speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru, 10)
            glabel(
              text = speLst[eatSpe],
              container = speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru
            )
            speWid[[oneSpe]]$diet[[eatSpe]]$fraBrad <- gradio(
              items = c(
                "None",
                "All",
                "Greater than",
                "Smaller than"
              ),
              horizontal = TRUE,
              container = speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru,
              handler = function(h, ...) {
                for (i in 1:length(speLst)) {
                  iteTyp[i] <- svalue(speWid[[i]]$rad)
                  iteChi[i] <- svalue(speWid[[i]]$gruCeChi)
                  iteOm[i] <- svalue(speWid[[i]]$gruCeOm)
                  # itePer[i] <- svalue(speWid[[i]]$gruCePer)
                  for (s in 1:length(speLst)) {
                    iteIte[i, s] <- as.character(svalue(speWid[[i]]$diet[[s]]$fraBrad))
                    iteItePar[i, s] <- svalue(speWid[[i]]$diet[[s]]$fraBage)
                  }
                }
                
                vertColo <- as.character(vertColDF[match(
                  iteTyp,
                  vertColDF[, 1]
                ), 2])
                firWhi <- which(iteIte != "None",
                                arr.ind = TRUE
                )
                edgeLty <- rep(
                  1,
                  nrow(firWhi)
                )
                edgeLty[which(firWhi[, 1] == firWhi[, 2])] <- 2
                
                speGra <- make_empty_graph(n = length(speLst)) %>%
                  set_vertex_attr(
                    name = "label",
                    value = speLst
                  ) %>%
                  add_edges(as.vector(t(firWhi)),
                            color = "firebrick",
                            lty = edgeLty
                  ) %>%
                  add_vertices(
                    nv = 1,
                    label = "Fishing\nFleet"
                  ) %>%
                  add_edges(c(rbind(
                    length(speLst) + 1,
                    1:length(speLst)
                  )),
                  color = "Black",
                  lty = 1
                  )
                plot(speGra,
                     main = "Interaction Graph",
                     edge.arrow.size = 0.5,
                     edge.width = 2,
                     vertex.color = c(vertColo, "White"),
                     vertex.size = c(rep(60, length(vertColo)), 90),
                     layout = nodeLay,
                     vertex.frame.color = "gray",
                     vertex.label.color = "black",
                     vertex.label.cex = 1.2,
                     edge.curved = 0.1
                )
              }
            )
            glabel(
              text = "Age: ",
              container = speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru
            )
            speWid[[oneSpe]]$diet[[eatSpe]]$fraBage <- gspinbutton(
              from = 0,
              to = smartRstudy$fisheryBySpecie[[oneSpe]]$nCoho - 1,
              by = 1,
              container = speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru
            )
            addSpace(speWid[[oneSpe]]$diet[[eatSpe]]$fraBgru, 10)
          }
          addSpring(speWid[[oneSpe]]$fraB)
          
          speWid[[oneSpe]]$gruC <- ggroup(
            horizontal = TRUE,
            container = speWid[[oneSpe]]$fra
          )
          addSpring(speWid[[oneSpe]]$gruC)
          glabel(text = "Chi:", container = speWid[[oneSpe]]$gruC)
          speWid[[oneSpe]]$gruCeChi <- gedit(
            text = "0.0", horizontal = TRUE,
            width = 5,
            container = speWid[[oneSpe]]$gruC
          )
          
          glabel(text = "Omega:", container = speWid[[oneSpe]]$gruC)
          speWid[[oneSpe]]$gruCeOm <- gedit(
            text = "0.0", horizontal = TRUE,
            width = 5,
            container = speWid[[oneSpe]]$gruC
          )
          
          # glabel(text = "Perc:", container = speWid[[oneSpe]]$gruC)
          # speWid[[oneSpe]]$gruCePer <- gedit(text = "0.0", horizontal = TRUE, width = 5,
          #                                    container = speWid[[oneSpe]]$gruC)
          addSpring(speWid[[oneSpe]]$gruC)
          addSpace(speWid[[oneSpe]]$fra, 10)
        }
        
        but_g <- ggroup(horizontal = TRUE, container = topAss_g)
        addSpring(but_g)
        gbutton("Accept", container = but_g, handler = function(h, ...) {
          for (i in 1:length(speLst)) {
            iteTyp[i] <- svalue(speWid[[i]]$rad)
            iteChi[i] <- svalue(speWid[[i]]$gruCeChi)
            iteOm[i] <- svalue(speWid[[i]]$gruCeOm)
            # itePer[i] <- svalue(speWid[[i]]$gruCePer)
            for (s in 1:length(speLst)) {
              iteIte[i, s] <- as.character(svalue(speWid[[i]]$diet[[s]]$fraBrad))
              iteItePar[i, s] <- svalue(speWid[[i]]$diet[[s]]$fraBage)
            }
          }
          
          smartRstudy$setAssessInteract(
            intName = speLst,
            intType = iteTyp,
            intWho = iteIte,
            intQty = iteItePar,
            intChi = as.numeric(iteChi),
            intOm = as.numeric(iteOm)
          )
          dispose(tempAssInte)
        })
        addSpring(but_g)
        
        addSpace(topAss_g, 10)
        addSpace(toptopAss_g, 10)
        int_p <- ggraphics(
          container = toptopAss_g, width = 350, height = 250,
          expand = TRUE
        )
        addSpace(toptopAss_g, 10)
        
        visible(tempAssInte) <- TRUE
        
        for (i in 1:length(speLst)) {
          iteTyp[i] <- svalue(speWid[[i]]$rad)
          iteChi[i] <- svalue(speWid[[i]]$gruCeChi)
          iteOm[i] <- svalue(speWid[[i]]$gruCeOm)
          # itePer[i] <- svalue(speWid[[i]]$gruCePer)
          for (s in 1:length(speLst)) {
            iteIte[i, s] <- as.character(svalue(speWid[[i]]$diet[[s]]$fraBrad))
            iteItePar[i, s] <- svalue(speWid[[i]]$diet[[s]]$fraBage)
          }
        }
        
        vertColo <- as.character(vertColDF[match(iteTyp, vertColDF[, 1]), 2])
        firWhi <- which(iteIte != "None", arr.ind = TRUE)
        edgeLty <- rep(1, nrow(firWhi))
        edgeLty[which(firWhi[, 1] == firWhi[, 2])] <- 2
        
        speGra <- make_empty_graph(n = length(speLst)) %>%
          set_vertex_attr(name = "label", value = speLst) %>%
          add_edges(as.vector(t(firWhi)), color = "firebrick", lty = edgeLty) %>%
          add_vertices(nv = 1, label = "Fishing\nFleet") %>%
          add_edges(c(rbind(length(speLst) + 1, 1:length(speLst))),
                    color = "Black",
                    lty = 1
          )
        nodeLay <- layout_with_fr(speGra)
        
        plot(speGra,
             main = "Interaction Graph",
             edge.arrow.size = 0.5, edge.width = 2,
             vertex.color = c(vertColo, "White"),
             vertex.size = c(rep(60, length(vertColo)), 90), layout = nodeLay,
             vertex.frame.color = "gray", vertex.label.color = "black",
             vertex.label.cex = 1.2, edge.curved = 0.1
        )
      }
    )
    enabled(assPre_but) <- FALSE
    addSpring(ass_g_spePred)
    addSpring(ass_g_top)
    ass_Fore <- gframe("Forecast\nNext Year",
                       horizontal = FALSE,
                       container = ass_g_top, expand = TRUE
    )
    addSpring(ass_Fore)
    ass_Fore_radio <- gradio(c("No", "Yes"),
                             selected = 1, horizontal = TRUE,
                             container = ass_Fore
    )
    addSpring(ass_Fore)
    addSpace(ass_g_top, 10)
    assParS_but <- gbutton(
      text = "Set Input ", container = ass_g_top,
      handler = function(h, ...) {
        if (svalue(assSM_rad) == "Single") {
          tmpSpe <- svalue(assSpe_drop)
          smartRstudy$setAssessData(
            species = tmpSpe,
            forecast = ifelse(svalue(ass_Fore_radio) == "No",
                              FALSE, TRUE
            )
          )
          
          parIn_df <- data.frame(matrix(0,
                                        ncol = smartRstudy$assessData[[tmpSpe]]$Amax,
                                        nrow = 4
          ))
          colnames(parIn_df) <- paste0("Age ", (1:ncol(parIn_df)) - 1)
          rownames(parIn_df) <- c("M", "Mat", "F-Sel", "S-Sel")
          
          parIn_df[1, ] <- smartRstudy$assessData[[tmpSpe]]$M
          parIn_df[2, ] <- smartRstudy$assessData[[tmpSpe]]$Mat
          parIn_df[3, ] <- smartRstudy$assessData[[tmpSpe]]$Selex
          parIn_df[4, ] <- smartRstudy$assessData[[tmpSpe]]$SelexSurv[1, ]
          
          parZbef_df <- as.character(smartRstudy$assessData[[tmpSpe]]$PropZBeforeMat)
          
          tempAssData <- gwindow(
            title = "Custom Parameters",
            visible = FALSE,
            parent = main_win,
            width = 500, height = 280
          )
          
          toptopAss_g <- ggroup(horizontal = TRUE, container = tempAssData)
          addSpace(toptopAss_g, 10)
          topAss_g <- ggroup(
            horizontal = FALSE, container = toptopAss_g,
            expand = TRUE
          )
          addSpace(topAss_g, 10)
          
          parIn_f <- gframe(text = "", horizontal = TRUE, container = topAss_g)
          addSpace(parIn_f, 25)
          parIn_gdf <- gdf(items = parIn_df, container = parIn_f)
          parIn_gdf$set_size(value = c(height = 125))
          addSpace(parIn_f, 25)
          addSpace(topAss_g, 10)
          
          parZbef_f <- gframe(
            text = "Z before Maturity", horizontal = TRUE,
            container = topAss_g
          )
          addSpace(parZbef_f, 25)
          parZbef_ge <- gedit(text = parZbef_df, width = 15, container = parZbef_f)
          addSpace(parZbef_f, 25)
          addSpace(topAss_g, 10)
          
          but_g <- ggroup(horizontal = TRUE, container = topAss_g)
          addSpring(but_g)
          gbutton("Accept", container = but_g, handler = function(h, ...) {
            smartRstudy$assessData[[tmpSpe]]$M <- as.numeric(unlist(parIn_gdf[1, ]))
            smartRstudy$assessData[[tmpSpe]]$Mat <- as.numeric(unlist(parIn_gdf[2, ]))
            smartRstudy$assessData[[tmpSpe]]$Selex <- as.numeric(unlist(parIn_gdf[3, ]))
            smartRstudy$assessData[[tmpSpe]]$SelexSurv[1, ] <- as.numeric(unlist(parIn_gdf[4, ]))
            smartRstudy$assessData[[tmpSpe]]$PropZBeforeMat <- as.numeric(svalue(parZbef_ge))
            delete(parIn_f, parIn_gdf)
            dispose(tempAssData)
          })
          addSpring(but_g)
          
          addSpace(topAss_g, 10)
          addSpace(toptopAss_g, 10)
          visible(tempAssData) <- TRUE
        } else {
          parZbef_df <- list()
          parIn_df <- list()
          for (oneSpe in intersect(
            smartRstudy$specieInFishery,
            smartRstudy$specieInSurvey
          )) {
            smartRstudy$setAssessData(
              species = oneSpe,
              forecast = FALSE
            )
            parIn_df[[oneSpe]] <- data.frame(matrix(0,
                                                    ncol = smartRstudy$assessData[[oneSpe]]$Amax,
                                                    nrow = 4
            ))
            colnames(parIn_df[[oneSpe]]) <- paste0("Age ", (1:ncol(parIn_df[[oneSpe]])) - 1)
            rownames(parIn_df[[oneSpe]]) <- c("M", "Mat", "F-Sel", "S-Sel")
            parIn_df[[oneSpe]][1, ] <- smartRstudy$assessData[[oneSpe]]$M
            parIn_df[[oneSpe]][2, ] <- smartRstudy$assessData[[oneSpe]]$Mat
            parIn_df[[oneSpe]][3, ] <- smartRstudy$assessData[[oneSpe]]$Selex
            parIn_df[[oneSpe]][4, ] <- smartRstudy$assessData[[oneSpe]]$SelexSurv[1, ]
            parZbef_df[[oneSpe]] <- as.character(smartRstudy$assessData[[oneSpe]]$PropZBeforeMat)
          }
          tempAssData <- gwindow(
            title = "Review Assessment Data",
            parent = main_win,
            width = 800, height = 600
          )
          bigGroup <- ggroup(horizontal = FALSE, container = tempAssData)
          ass_gn <- gnotebook(tab.pos = 3, container = bigGroup, expand = TRUE)
          for (speName in 1:length(names(smartRstudy$assessData))) {
            assign(paste0("Tab", speName), ggroup(
              horizontal = FALSE, container = ass_gn,
              label = names(smartRstudy$assessData)[speName]
            ))
            assign(
              paste0("toptopAss", speName),
              ggroup(horizontal = TRUE, container = get(paste0("Tab", speName)))
            )
            addSpace(get(paste0("toptopAss", speName)), 10)
            assign(
              paste0("topAss", speName),
              ggroup(
                horizontal = FALSE, expand = TRUE,
                container = get(paste0("toptopAss", speName))
              )
            )
            addSpace(get(paste0("topAss", speName)), 10)
            assign(
              paste0("parIn", speName),
              gframe(
                text = "", horizontal = TRUE,
                container = get(paste0("topAss", speName))
              )
            )
            addSpace(get(paste0("parIn", speName)), 25)
            assign(
              paste0("parGdf", speName),
              gdf(
                items = parIn_df[[speName]],
                container = get(paste0("parIn", speName))
              )
            )
            get(paste0("parGdf", speName))$set_size(value = c(height = 125))
            addSpace(get(paste0("parIn", speName)), 25)
            addSpace(get(paste0("topAss", speName)), 10)
            assign(
              paste0("parZbef", speName),
              gframe(
                text = "Z before Maturity", horizontal = TRUE,
                container = get(paste0("topAss", speName))
              )
            )
            addSpace(get(paste0("parZbef", speName)), 25)
            assign(
              paste0("parZbefGe", speName),
              gedit(
                text = parZbef_df[[oneSpe]], width = 15,
                container = get(paste0("parZbef", speName))
              )
            )
            addSpace(get(paste0("parZbef", speName)), 25)
            addSpace(get(paste0("topAss", speName)), 10)
          }
          smaGroup <- ggroup(horizontal = TRUE, container = bigGroup)
          addSpring(smaGroup)
          endArea <- gbutton(
            text = "\nClose\n", container = smaGroup,
            handler = function(h, ...) {
              for (speName in 1:length(names(smartRstudy$assessData))) {
                smartRstudy$assessData[[speName]]$M <- as.numeric(unlist(get(paste0("parGdf", speName))[1, ]))
                smartRstudy$assessData[[speName]]$Mat <- as.numeric(unlist(get(paste0("parGdf", speName))[2, ]))
                smartRstudy$assessData[[speName]]$Selex <- as.numeric(unlist(get(paste0("parGdf", speName))[3, ]))
                smartRstudy$assessData[[speName]]$SelexSurv[1, ] <- as.numeric(unlist(get(paste0("parGdf", speName))[4, ]))
                smartRstudy$assessData[[speName]]$PropZBeforeMat <- as.numeric(svalue(get(paste0("parZbefGe", speName))))
                delete(get(paste0("parIn", speName)), get(paste0("parGdf", speName)))
              }
              dispose(tempAssData)
            }
          )
          addSpring(smaGroup)
        }
      }
    )
    addSpace(ass_g_top, 10)
    assParR_but <- gbutton(
      text = "Inspect Input", container = ass_g_top,
      handler = function(h, ...) {
        tempWind_AssData <- gwindow(
          title = "Review Assessment Data",
          parent = main_win,
          width = 800, height = 600
        )
        bigGroup <- ggroup(horizontal = FALSE, container = tempWind_AssData)
        ass_gn <- gnotebook(tab.pos = 3, container = bigGroup, expand = TRUE)
        for (s in 1:length(names(smartRstudy$assessData))) {
          assign(paste0("Tab", s), ggroup(
            horizontal = FALSE, container = ass_gn,
            label = names(smartRstudy$assessData)[s]
          ))
          assign(
            paste0("Txt", s),
            gtext(
              text = paste(capture.output(print(smartRstudy$assessData[[names(smartRstudy$assessData)[s]]])),
                           collapse = "\n"
              ), width = 300, height = 550,
              container = get(paste0("Tab", s))
            )
          )
        }
        smaGroup <- ggroup(horizontal = TRUE, container = bigGroup)
        addSpring(smaGroup)
        endArea <- gbutton(
          text = "\nClose\n", container = smaGroup,
          handler = function(h, ...) {
            dispose(tempWind_AssData)
          }
        )
        addSpring(smaGroup)
      }
    )
    addSpace(ass_g_top, 10)
    ass_Str_but <- gbutton(
      text = "Start", container = ass_g_top,
      handler = function(h, ...) {
        if (svalue(assSM_rad) == "Single") {
          smartRstudy$assSingle(species = svalue(assSpe_drop))
          smartRstudy$setPlotSingle(species = svalue(assSpe_drop))
          assRes_drop[] <- names(smartRstudy$assessData)
          svalue(assRes_drop) <- assRes_drop[][which(assRes_drop[] == svalue(assSpe_drop))]
          dev.set(dev.list()[pre_dev + 10])
          suppressWarnings(print(smartRstudy$assSinglePlot[[svalue(assSpe_drop)]]$SSB))
        } else {
          smartRstudy$assMulti()
          smartRstudy$setPlotMulti()
          assRes_drop[] <- names(smartRstudy$assessData)
          svalue(assRes_drop) <- assRes_drop[][which(assRes_drop[] == svalue(assSpe_drop))]
          dev.set(dev.list()[pre_dev + 10])
          suppressWarnings(print(smartRstudy$assMultiPlot[[svalue(assSpe_drop)]]$SSB))
        }
      }
    )
    ass_Str_but$set_size(value = c(width = 80))
    
    addSpring(ass_g_top)
    ass_Res <- gframe("View",
                      horizontal = TRUE, container = ass_g_top,
                      expand = TRUE
    )
    addSpace(ass_Res, 10)
    assRes_drop <- gcombobox(
      items = "    Species", selected = 1,
      editable = FALSE, container = ass_Res,
      handler = NULL
    )
    assRes_drop$set_size(value = c(width = 150))
    addSpace(ass_Res, 10)
    ass_Res_radio <- gradio(c("SSB", "OPSurvey", "OPCatch", "TotalCatch"),
                            selected = 1,
                            horizontal = FALSE, container = ass_Res,
                            handler = function(h, ...) {
                              dev.set(dev.list()[pre_dev + 10])
                              if (svalue(assSM_rad) == "Single") {
                                switch(svalue(ass_Res_radio),
                                       SSB = {
                                         suppressWarnings(print(smartRstudy$assSinglePlot[[svalue(assRes_drop)]]$SSB))
                                       },
                                       OPSurvey = {
                                         suppressWarnings(print(smartRstudy$assSinglePlot[[svalue(assRes_drop)]]$ObsPredSurv))
                                       },
                                       OPCatch = {
                                         suppressWarnings(print(smartRstudy$assSinglePlot[[svalue(assRes_drop)]]$ObsPredCAA))
                                       },
                                       TotalCatch = {
                                         suppressWarnings(print(smartRstudy$assSinglePlot[[svalue(assRes_drop)]]$totCatc))
                                       }
                                )
                              } else {
                                switch(svalue(ass_Res_radio),
                                       SSB = {
                                         suppressWarnings(print(smartRstudy$assMultiPlot[[svalue(assRes_drop)]]$SSB))
                                       },
                                       OPSurvey = {
                                         suppressWarnings(print(smartRstudy$assMultiPlot[[svalue(assRes_drop)]]$ObsPredSurv))
                                       },
                                       OPCatch = {
                                         suppressWarnings(print(smartRstudy$assMultiPlot[[svalue(assRes_drop)]]$ObsPredCAA))
                                       },
                                       TotalCatch = {
                                         suppressWarnings(print(smartRstudy$assMultiPlot[[svalue(assRes_drop)]]$totCatc))
                                       }
                                )
                              }
                            }
    )
    addSpace(ass_Res, 10)
    addSpace(ass_g_top, 10)
    
    ass_p <- ggraphics(
      container = ass_g, width = 550, height = 250,
      expand = TRUE
    )
    
    addSpace(big_g, 10)
    
    stat_bar <- gstatusbar("", container = rig_g, visible = TRUE)
    
    
    # visible(pro_eg) <- TRUE
    # visible(raw_eg) <- TRUE
    # visible(eff_eg) <- TRUE
    # visible(ass_eg) <- TRUE
    # visible(sim_eg) <- TRUE
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
    dev.set(dev.list()[pre_dev + 1])
    suppressWarnings(suppressMessages(ggplot() +
                                        geom_blank() +
                                        annotation_custom(rasterGrob(logoPNG),
                                                          xmin = 0, xmax = 1,
                                                          ymin = 0, ymax = 1
                                        )))
  }
}
