

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
      facet_grid(Year~Month) +
      theme_few() +
      scale_x_continuous(breaks = pretty(inLfd$Length, 5)) +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_text(size = 5),
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


set_spatAbbFreq <- function(inSpat){
  suppressMessages(
    ggplot(inSpat, aes(x = FG, y = Freq)) +
      theme_tufte(base_size = 14, ticks = F) +
      geom_bar(width=0.25, fill = "gray35", stat = "identity") +
      theme(axis.title = element_blank()) +
      scale_y_continuous(breaks = pretty(inSpat$Freq, n = 5)) +
      geom_hline(yintercept = pretty(inSpat$Freq, n = 5),
                 col = "white", lwd = 1) +
      geom_hline(yintercept = pretty(inSpat$Freq, n = 5),
                 col = "grey75", lwd = 0.25, lty = 2) +
      geom_hline(yintercept = 0, col = "gray60", lwd = 0.6) +
      annotate("text", x = 5, y = max(inSpat$Freq)/2,
               hjust = 0.5,  family="serif", size = 4,
               label = c("Absolute number of obsevartions\nin each fishing ground")) +
      annotate("text", x = 1:nrow(inSpat), y = inSpat$Freq+1000,
               hjust = 0.5, family="serif", size = 3,
               label = ifelse(inSpat$Freq == 0, "", inSpat$Freq)) +
      # coord_flip() +
      theme(legend.position = "none",
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
      geom_bar(width=0.25, fill = "gray35", stat = "identity") +
      theme(axis.title = element_blank()) +
      scale_y_continuous(breaks = pretty(inSpat$relFreq, n = 5)) +
      geom_hline(yintercept = pretty(inSpat$relFreq, n = 5),
                 col = "white", lwd = 1) +
      geom_hline(yintercept = pretty(inSpat$relFreq, n = 5),
                 col = "grey75", lwd = 0.25, lty = 2) +
      geom_hline(yintercept = 0, col = "gray60", lwd = 0.6) +
      annotate("text", x = 5, y = max(inSpat$relFreq)/2,
               hjust = 0.5,  family="serif", size = 4,
               label = c("Relative number of obsevartions\nin each fishing ground")) +
      annotate("text", x = 1:nrow(inSpat), y = inSpat$relFreq+2,
               hjust = 0.5, family="serif", size = 3,
               label = ifelse(inSpat$relFreq == 0, "", inSpat$relFreq)) +
      # coord_flip() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 5),
            axis.title.y = element_blank())
  )
}
