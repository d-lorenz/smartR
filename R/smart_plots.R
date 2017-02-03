
# SmartR Plots ----

## Fleet Register - Plot Dispatch ----
ggplot_registerDispatch = function(curRegister, selPlot){
  switch(selPlot,
         "Summary" = grid.arrange(ggplot_registerMainGear(df_Register = curRegister),
                                  ggplot_registerSecGear(df_Register = curRegister),
                                  ggplot_registerHullMaterial(df_Register = curRegister[!is.na(curRegister$Hull.Material),]),
                                  ggplot_registerConstYear(df_Register = curRegister),
                                  ggplot_registerLoa(df_Register = curRegister),
                                  ggplot_registerMainPower(df_Register = curRegister),
                                  layout_matrix = rbind(c(1,2,3),c(4,5,6))),
         "Main Gear" = print(ggplot_registerMainGear(df_Register = curRegister)),
         "Secondary Gear" = print(ggplot_registerSecGear(df_Register = curRegister)),
         "Hull Material" = print(ggplot_registerHullMaterial(df_Register = curRegister[!is.na(curRegister$Hull.Material),])),
         "Construction Year" = print(ggplot_registerConstYear(df_Register = curRegister)),
         "Length Over All" = print(ggplot_registerLoa(df_Register = curRegister)),
         "Main Power" = print(ggplot_registerMainPower(df_Register = curRegister)))
}


## Fleet Register - Main Gear ----
ggplot_registerMainGear <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_bar(data = df_Register,
                 mapping = aes_(~Gear.Main.Code)) +
        theme_tufte(base_size = 14, ticks=F) +
        ggtitle("Main Gear") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Fleet Register - Secondary Gear ----
ggplot_registerSecGear <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_bar(data = df_Register,
                 mapping = aes_(~Gear.Sec.Code)) +
        theme_tufte(base_size = 14, ticks=F) +
        ggtitle("Secondary Gear") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Fleet Register - Hull Material ----
ggplot_registerHullMaterial <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_bar(data = df_Register,
                 mapping = aes_(~factor(Hull.Material,
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Wood", "Metal", "Plastic", "Other", "Unknown")))) +
        theme_tufte(base_size = 14, ticks=F) +
        ggtitle("Hull Material") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 90),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Fleet Register - Construction Year ----
ggplot_registerConstYear <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_histogram(data = df_Register,
                       mapping = aes_(~Construction.Year),
                       bins = 100) +
        theme_tufte(base_size = 14, ticks=F) +
        scale_x_continuous(breaks = pretty(df_Register$Construction.Year),
                           limits = range(pretty(df_Register$Construction.Year))) +
        ggtitle("Construction Year") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 8),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Fleet Register - Length Over All ----
ggplot_registerLoa <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_histogram(data = df_Register,
                       mapping = aes_(~Loa),
                       bins = 100) +
        theme_tufte(base_size = 14, ticks=F) +
        scale_x_continuous(breaks = pretty(df_Register$Loa),
                           limits = range(pretty(df_Register$Loa))) +
        ggtitle("Length Over All") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 8),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Fleet Register - Main Engine Power ----
ggplot_registerMainPower <- function(df_Register){
  return(
    suppressMessages(
      ggplot() +
        geom_histogram(data = df_Register,
                       mapping = aes_(~Power.Main),
                       bins = 100) +
        theme_tufte(base_size = 14, ticks=F) +
        scale_x_continuous(breaks = pretty(df_Register$Power.Main),
                           limits = range(pretty(df_Register$Power.Main))) +
        ggtitle("Main Engine Power (Kw)") +
        theme(legend.position = "none",
              plot.title = element_text(size = 14),
              axis.text.x = element_text(size = 8),
              axis.title = element_blank(),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Production Total path ----
ggplot_TotalProduction <- function(df_Prod){
  return(
    suppressMessages(
      ggplot() +
        geom_line(data = df_Prod, mapping = aes_(x = ~Year, y = ~Production)) +
        ylab("Kilogram") +
        theme_tufte(base_size = 14, ticks=F) +
        theme(legend.position = "none",
              axis.text.x = element_text(size = 10),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank())
    )
  )
}

## Production by Fishing Ground path ----
ggplot_FGProduction <- function(df_FGProd){
  return(
    suppressMessages(
      ggplot() +
        geom_line(data = df_FGProd,
                  mapping = aes_(x = ~Year, y = ~Production,
                                 color = ~FishGround, group = ~FishGround)) +
        ylab("Kilogram") +
        theme_tufte(base_size = 14, ticks=F) +
        theme(legend.position = "right",
              axis.text.x = element_text(size = 10),
              panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_blank()) +
        labs(color = "Fishing Ground")
    )
  )
}

## MCMC Survivors * quarter ----
set_ggSurvLine <- function(df_surv){
  return(
    suppressMessages(
      ggplot(data = df_surv, aes_(x = ~Catch, y = ~Qty, group = ~Birth, color = ~Birth)) +
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
  )
}

## MCMC Catch * Quarters ----
set_ggCatchLine <- function(df_birth){
  return(
    suppressMessages(
      ggplot() +
        geom_line(data = df_birth, aes_(x = ~Catch, y = ~Qty, group = ~Birth, color = ~Birth)) +
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
  )
}

## MCMC Quarters Histogram ----
set_ggHistBirth <- function(df_mix, df_grow){
  return(
    suppressMessages(
      ggplot(data = df_mix,
             mapping = aes_(x = ~CatcDate, y = ~Length,
                            color = ~factor(Birth))) +
        scale_color_brewer(name = "Year of Birth", palette = "Paired") +
        geom_jitter(size = 0.05, height = 0, width = 0.9, alpha = 0.4) +
        scale_x_discrete(drop = FALSE) +
        geom_line(data = df_grow,
                  mapping = aes_(x = ~Date, y = ~Length, group = ~Birth),
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
  )
}

## MCMC Cohort Summary Table ----
set_tblCohoStat <- function(df_coho){
  cohSliTheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.5)),
    rowhead = list(fg_params=list(cex = 0.4)))
  return(tableGrob(round(df_coho, 2), theme = cohSliTheme, rows=NULL))
}

## MCMC Age-Length Table ----
set_tblAgeLength <- function(df_mix){
  ageLenTheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.5)),
    rowhead = list(fg_params=list(cex = 0.3)))
  return(tableGrob(table(round(df_mix$Length), df_mix$Age), theme = ageLenTheme))
}

## MCMC Plot Age-Length ----
set_ggAgeLength <- function(df_mix, mixPalette){
  return(
    suppressMessages(
      ggplot() +
        scale_x_continuous("Age", breaks = 0:max(ceiling(df_mix$Age))) +
        scale_y_continuous("Length", breaks = pretty(df_mix$Length, 10)) +
        geom_point(data = df_mix, aes_(x = ~AgeNF, y = ~Length, color = ~factor(Age)), size = 0.3) +
        geom_point(data = df_mix, aes_(x = ~Age, y = ~Length, fill = ~factor(Age)), shape = 21, color = "grey20", size = 1.2) +
        scale_color_manual(values = mixPalette) +
        scale_fill_manual(values = mixPalette) +
        theme_tufte(base_size = 14, ticks = FALSE) +
        theme(legend.position = "none",
              panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
              axis.text.x = element_text(size = 8),
              axis.title.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              axis.title.y = element_text(size = 8))
    )
  )
}

## MCMC Chains Boxplot Sigma ----
set_ggSigmaBox <- function(df_sigma, sigPalette, numCoho){
  cohoVari <- melt(sqrt(df_sigma))
  names(cohoVari) <- c("Iter", "Cohort", "Value")
  cohoVari$Cohort <- factor(as.numeric(cohoVari$Cohort), levels = 1:numCoho)
  stsVari <- boxplot.stats(cohoVari$Value)$stats ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display
  cohoVariGG <- suppressMessages(
    ggplot(cohoVari, aes_(x = ~Cohort, y = ~Value, fill = ~Cohort)) +
      geom_boxplot(alpha = 0.6, outlier.color = "grey30", outlier.size = 0.35, notch = TRUE) +
      ggtitle("SD") +
      scale_x_discrete(labels = 0:(numCoho-1)) +
      scale_fill_manual(values = sigPalette) +
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
  return(cohoVariGG)
}

## MCMC Chains Boxplot Tau ----
set_ggTausBox <- function(df_taus, tauPalette, numCoho){
  cohoPreci <- melt(df_taus)
  names(cohoPreci) <- c("Iter", "Cohort", "Value")
  cohoPreci$Cohort <- factor(as.numeric(cohoPreci$Cohort), levels = 1:(numCoho))
  stsPreci <- boxplot.stats(cohoPreci$Value)$stats ## from: http://stackoverflow.com/questions/21533158/remove-outliers-fully-from-multiple-boxplots-made-with-ggplot2-in-r-and-display

  cohoPreciGG <- suppressMessages(
    ggplot(cohoPreci, aes_(x = ~Cohort, y = ~Value, fill = ~Cohort)) +
      geom_boxplot(alpha = 0.6, outlier.color = "grey30", outlier.size = 0.35, notch = TRUE) +
      ggtitle("Precision") +
      scale_x_discrete(labels = 0:(numCoho-1)) +
      scale_fill_manual(values = tauPalette) +
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

  return(cohoPreciGG)
}

## MCMC Chains Traceplot ----
set_ggChainTrace <- function(df_LK){
  return(
    suppressMessages(
      ggplot(data = df_LK,
             mapping = aes_(x = ~Iter, y = ~Value, color = ~factor(Chain)))+
        geom_line(alpha = 0.7) +
        facet_wrap(~ Parameter, nrow = 3, ncol = 1, scales = "free", switch = "y") +
        scale_color_brewer(palette = "Dark2", "Chain") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(title = element_text(size = 10),
              legend.position = "right",
              legend.title = element_text(size = 7),
              panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
              axis.text.x = element_text(size = 6),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size = 6),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank())+
        guides(colour = guide_legend(override.aes = list(size = 3,
                                                         alpha = 0.9,
                                                         fill = NA)))
    )
  )
}

## MCMC Chains Scatterplot ----
set_ggChainScatter <- function(gg_DFscat, meanL, meanK){
  return(
    suppressMessages(
      ggplot()+
        geom_point(data = gg_DFscat,
                   mapping = aes_(x = ~Linf, y = ~Kappa, color = ~factor(Chain)),
                   size = 0.25, alpha = 0.25) +
        # annotate("point", x = mut_popgrowth$Loo, y = mut_popgrowth$K, color = "grey25", size = 0.7) +
        annotate("point", x = meanL, y = meanK, color = "goldenrod1",
                 shape = 42, size = 12, alpha = 0.9) +
        # annotate("point", x = mean(mut_popgrowth$Loo), y = mean(mut_popgrowth$K), color = "firebrick",
        #          shape = 20, size = 5, alpha = 0.9) +
        annotate("text", x = Inf, y = Inf,
                 label = paste("LHat = ", round(meanL, 2),
                               "\nKHat = ", round(meanK, 3), sep = ""),
                 hjust = 1, vjust = 1, color = "goldenrod1", fontface = "bold") +
        scale_color_brewer(palette = "Dark2", "Chain") +
        theme_tufte(base_size = 14, ticks = F) +
        theme(legend.position = "none",
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
  )
}

## Length Frequency Distribution Histogram ----
set_ggHistLfdTot <- function(inLfd){
  suppressMessages(ggplot(inLfd, aes_(x = ~Length, y = ~..count..)) +
                     geom_histogram(bins = 50, fill = "grey0", alpha = 0.7, col = "grey10") +
                     annotate("text", x = 0, y = Inf, hjust = 0, vjust = 1,
                              family="serif", label = "Absolute frequency of \nlength values") +
                     geom_vline(xintercept = mean(inLfd$Length), col = "grey90", lwd = 0.6, lty = 2) +
                     scale_x_continuous(breaks = pretty(inLfd$Length, 10)) +
                     theme_tufte(base_size=14, ticks=F) +
                     theme(legend.position = "none",
                           panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
                           axis.text.x = element_text(size = 5),
                           axis.title.x = element_text(size = 7),
                           axis.text.y = element_text(size = 5),
                           axis.title.y = element_blank(),
                           axis.ticks.y = element_blank()))
}

## UTC Coverage Histogram ----
set_ggHistUtcTot <- function(inLfd){
  suppressMessages(
    ggplot(data.frame(UTC = unique(inLfd$UTC)), aes_(x = ~UTC, y = ~..count..)) +
      geom_histogram(bins = 100, fill = "grey0", alpha = 0.7) +
      annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1,
               family="serif", label = "Time coverage") +
      theme_tufte(base_size = 14, ticks = F) +
      theme(legend.position = "none",
            panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
            axis.text.x = element_text(size = 5),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_x_chron(n = 10)
  )
}

## UTC split Dot Chart ----
set_ggDotUtcSplit <- function(inLfd){
  sampPunch <- melt(table(inLfd$Year, inLfd$Month))
  names(sampPunch) <- c("Year", "Month", "Frequency")
  sampPunch$Year <- factor(sampPunch$Year, levels = sort(unique(sampPunch$Year), decreasing = TRUE))
  suppressMessages(
    ggplot(sampPunch, aes_(x = ~Month, y = ~Year, size = ~log10(Frequency))) +
      geom_point() +
      scale_y_discrete(breaks = unique(sampPunch$Year), expand = c(0.1, 0.1)) +
      theme_tufte(base_size = 14, ticks = F) +
      theme(legend.position = "none",
            panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
            axis.text.x = element_text(size = 5),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
  )
}

## UTC LFD Histogram ----
set_ggHistUtcLfd <- function(inLfd){
  suppressMessages(
    ggplot(inLfd, aes_(x = ~Length, y = ~..count..)) +
      geom_histogram(bins = 30, fill = "grey1", alpha = 0.7) +
      facet_grid(Year~Month, switch = "y") +
      theme_few() +
      scale_x_continuous(breaks = pretty(inLfd$Length, 5)) +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 4),
            strip.text.x = element_text(size = 5),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_text(size = 4),
            strip.text.y = element_text(size = 5),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
  )
}

## Abundance Frequency Table ----
set_spatAbbTbl <- function(inSpat){
  out_FgTbl <- data.frame(FG = inSpat$FG,
                          AbsFreq = inSpat$Freq,
                          RelFreq = inSpat$relFreq)

  fgAbbTheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.6)),
    rowhead = list(fg_params=list(cex = 0.4)))

  return(tableGrob(out_FgTbl, theme = fgAbbTheme))
}

## Abundance Absolute Frequency Bars ----
set_spatAbsFreq <- function(inSpat){
  suppressMessages(
    ggplot(inSpat, aes_(x = ~FG, y = ~Freq)) +
      theme_tufte(base_size = 14, ticks = F) +
      geom_bar(width = 0.45, fill = "gray35", stat = "identity") +
      scale_y_continuous(breaks = pretty(inSpat$Freq, n = 5)) +
      ggtitle("Absolute number of obsevartions\nin each fishing ground") +
      annotate("text", x = 1:nrow(inSpat), y = inSpat$Freq+max(inSpat$Freq)/20,
               hjust = 0.5, family="serif", size = 3,
               label = ifelse(inSpat$Freq == 0, "", inSpat$Freq)) +
      theme(legend.position = "none",
            plot.title = element_text(size = 6),
            panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 5),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            axis.title.y = element_blank())
  )
}

## Abundance Relative Frequency Bars ----
set_spatRelFreq <- function(inSpat){
  suppressMessages(
    ggplot(inSpat, aes_(x = ~FG, y = ~relFreq)) +
      theme_tufte(base_size = 14, ticks = F) +
      geom_bar(width = 0.45, fill = "gray35", stat = "identity") +
      scale_y_continuous(breaks = pretty(inSpat$relFreq, n = 5)) +
      ggtitle("Relative number of obsevartions\nin each fishing ground") +
      annotate("text", x = 1:nrow(inSpat), y = inSpat$relFreq+max(inSpat$relFreq)/20,
               hjust = 0.5, family="serif", size = 3,
               label = ifelse(inSpat$relFreq == 0, "", inSpat$relFreq)) +
      theme(legend.position = "none",
            panel.grid = element_line(size = 1, linetype = 2, colour = "grey20"),
            plot.title = element_text(size = 6),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 5),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            axis.title.y = element_blank())
  )
}
