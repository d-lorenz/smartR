

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
