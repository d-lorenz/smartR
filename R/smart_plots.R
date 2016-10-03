

### MCMC chain Traceplot
set_ggChainTrace <- function(df_LK){
  return(
    suppressMessages(
      ggplot(data = df_LK,
             mapping = aes(x = Iter, y = Value, color = factor(Chain)))+
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
###

######
### MCMC chain scatterplot
set_ggChainScatter <- function(gg_DFscat){
  return(
    suppressMessages(
      ggplot()+
        geom_point(data = gg_DFscat,
                   mapping = aes(x = Linf, y = Kappa, color = factor(Chain)),
                   size = 0.25, alpha = 0.25) +
        # annotate("point", x = mut_popgrowth$Loo, y = mut_popgrowth$K, color = "grey25", size = 0.7) +
        annotate("point", x = LHat, y = kHat, color = "goldenrod1",
                 shape = 42, size = 12, alpha = 0.9) +
        # annotate("point", x = mean(mut_popgrowth$Loo), y = mean(mut_popgrowth$K), color = "firebrick",
        #          shape = 20, size = 5, alpha = 0.9) +
        annotate("text", x = Inf, y = Inf, label = paste("LHat = ", round(LHat, 2), "\nKHat = ", round(kHat, 3), sep = ""),
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
###



set_ggHistLfdTot <- function(inLfd){
  suppressMessages(ggplot(inLfd, aes(x = Length, y = ..count..)) +
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


set_ggHistUtcTot <- function(inLfd){
  suppressMessages(
    ggplot(data.frame(UTC = unique(inLfd$UTC)), aes(x = UTC, y = ..count..)) +
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


set_ggDotUtcSplit <- function(inLfd){
  sampPunch <- melt(table(inLfd$Year, inLfd$Month))
  names(sampPunch) <- c("Year", "Month", "Frequency")
  sampPunch$Year <- factor(sampPunch$Year, levels = sort(unique(sampPunch$Year), decreasing = TRUE))
  suppressMessages(
    ggplot(sampPunch, aes(x = Month, y = Year, size = log10(Frequency))) +
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


set_ggHistUtcLfd <- function(inLfd){
  suppressMessages(
    ggplot(inLfd, aes(x = Length, y = ..count..)) +
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


set_spatAbsFreq <- function(inSpat){
  suppressMessages(
    ggplot(inSpat, aes(x = FG, y = Freq)) +
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


set_spatRelFreq <- function(inSpat){
  suppressMessages(
    ggplot(inSpat, aes(x = FG, y = relFreq)) +
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
