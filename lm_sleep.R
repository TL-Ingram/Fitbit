# Does sleep onset time predict length of sleep
library(GGally)
ready_data$sSleep <- as_hms(as.POSIXct(ready_data$sSleep))
hms_clean <- ready_data %>%
     select(3,4,5,6) %>%
     mutate(day = as_date(ifelse(sSleep <="06:00", Sys.Date()+1, Sys.Date())),
            sSleep = ymd_hms(paste(day, sSleep))) %>%
     mutate(sSleep = if_else(hour(sSleep) > 4, sSleep - days(1), sSleep)) %>%
     filter(!(Sleep_hours < 4)) #%>%
     ggplot(aes(sSleep, Steps)) +
     geom_point(size = 4, alpha = 0.5, colour = "steelblue") +
     geom_line(stat = "smooth", size = 1.5, alpha = 0.7, colour = "black", se = FALSE, method = "loess") +
     theme_ipsum(
          axis_title_just = "cc",
          axis_title_face = "bold",
          axis_text_size = 16,
          axis_title_size = 18
     ) +
     theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.x = element_line("grey50"),
          axis.ticks = element_line(colour = "grey50", size = 0.2),
          axis.ticks.x = element_line(colour = "grey50", size = 0.2),
          axis.text.x = element_text(
               angle = 25,
               vjust = 1.0, hjust = 1.0
          ),
          plot.caption = element_text(
               size = 14,
               face = "italic", color = "black"
          ),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 16)
     ) #+
    #  labs(
    #       x = "Time of sleep onset",
    #       y = "Sleep duration (hours)",
    #       colour = "Sleep time",
    #       title = "Sleep onset against sleep duration",
    #       subtitle = "April 2022 : Present",
    #       caption = "Horizontal dotted line represents subjectively appreciated \
    # `good` sleep time quantity. \
    # Early = before midnight; Late = after midnight \
    # Linear model represented by black line"
    #  )
hms_clean
dev.off()
ggpairs(hms_clean)
