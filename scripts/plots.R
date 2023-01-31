# Plot: sleep duration and onset
sleep_ovd <- sleep_filter %>%
ggplot(aes(sSleep, Sleep_hours, label = sSleep, colour = h_o_s)) +
     geom_point(size = 4, alpha = 0.5) +
     geom_line(aes(sSleep, Sleep_hours), stat = "smooth", size = 1.5, alpha = 0.7, colour = "black", se = FALSE, method = "lm") +
     scale_colour_manual(values = c("blue", "red")) +
     expand_limits(y = 4:10) +
     scale_y_continuous(breaks = seq(4, 10, 1)) +
     # scale_x_date(
     #   breaks = date_breaks("7 days"),
     #   labels = date_format("%b %d")
     # ) +
     geom_hline(aes(yintercept = 7),
                size = 1, linetype = "dashed", colour = "#482677FF"
     ) +
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
     ) +
     labs(
          x = "Time of sleep onset",
          y = "Sleep duration (hours)",
          colour = "Sleep time",
          title = "Sleep onset against sleep duration",
          subtitle = "April 2022 : Present",
          caption = "Horizontal dotted line represents subjectively appreciated \
    `good` sleep time quantity. \
    Early = before midnight; Late = after midnight \
    Linear model represented by black line"
     )


# Plot: sleep duration distribution
sleep_dist <- sleep_filter %>%
     ggplot(aes(Sleep_hours)) +
     geom_density(fill = "blue", alpha = 0.1) +
     geom_line(stat = "density", size = 1.5, colour = "blue", alpha = 0.2) +
     geom_vline(aes(xintercept = median(Sleep_hours)), size = 1.5, alpha = 0.6) +
     geom_vline(aes(xintercept = quantile(Sleep_hours, 0.25)),
                linetype = "dashed", alpha = 0.6, size = 1.5
     ) +
     geom_vline(aes(xintercept = quantile(Sleep_hours, 0.75)),
                linetype = "dashed", alpha = 0.6, size = 1.5
     ) +
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
          plot.caption = element_text(
               size = 14,
               face = "italic", color = "black"
          )
     ) +
     labs(
          x = "Sleep duration (hours)",
          y = "Density",
          caption = paste0("n = ", n_sleep, 
                           "; Median = ", median_sleep,
                           "; Q1 = ", q1_sleep,
                           "; Q3 = ", q3_sleep,
                           ". Naps removed."),
          title = "Sleep duration distribution",
          subtitle = paste0(first_date, ": Present")
     )


# Facet plot: bind sleep plots rowwise
sleep_plots <- plot_grid(sleep_ovd, sleep_dist, nrow = 2, labels = "") %>%
     plot_grid()
pdf_null_device()
save_plot(
     plot = sleep_plots, here("plots", "sleep_plots.tiff"),
     dpi = 300, base_width = 16, base_height = 12
)
save_plot(
     plot = sleep_plots, "C:/Users/INGRAM_T/Dropbox/Auto_plots/fitbit_plot_one.tiff",
     dpi = 300, base_width = 16, base_height = 12
)
try(dev.off())


# Plot: resting heart rate time-series
rHR_change <- ready_data %>%
     distinct(Date, .keep_all = TRUE) %>%
     ggplot(aes(Date, rHR)) +
     geom_point(alpha = 0.8) +
     geom_line(size = 1.5, alpha = 0.5) +
     scale_x_date(date_labels = "%b-%d", breaks = "7 day") +
     scale_y_continuous(breaks = seq(50, 66, 1)) +
     theme_ipsum(
          axis_title_just = "cc",
          axis_title_face = "bold",
          axis_text_size = 16,
          axis_title_size = 18
     ) +
     theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          axis.text.x = element_text(
               angle = 25,
               vjust = 1.0, hjust = 1.0
          ),
          axis.line.x = element_line("grey50"),
          axis.ticks = element_line(colour = "grey50", size = 0.2),
          axis.ticks.x = element_line(colour = "grey50", size = 0.2),
          plot.caption = element_text(
               size = 14,
               face = "italic", color = "black"
          )
     ) +
     labs(
    title = "Mean daily resting heart rate",
    subtitle = paste0(first_date_steps, ": Present")
     )

# Plot: Daily step count time-series
steps_plot <- impute_steps %>%
     distinct(Date, .keep_all = TRUE) %>%
     ggplot(aes(Date, Steps)) +
     geom_col(alpha = 0.2, aes(fill = imputed)) +
     geom_path(aes(Date, threeday), size = 1.5, alpha = 0.5) +
     scale_fill_manual(values = c("grey65", "grey25")) +
     # scale_x_date(date_labels = "%b-%d", breaks = "7 day") +
     geom_hline(aes(yintercept = 10000),
                size = 1, linetype = "dashed", colour = "#482677FF", alpha = 0.3
     ) +
     theme_ipsum(
          axis_title_just = "cc",
          axis_title_face = "bold",
          axis_text_size = 16,
          axis_title_size = 18
     ) +
     theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(
               angle = 25,
               vjust = 1.0, hjust = 1.0
          ),
          axis.line.x = element_line("grey50"),
          axis.ticks = element_line(colour = "grey50", size = 0.2),
          axis.ticks.x = element_line(colour = "grey50", size = 0.2),
          legend.position = "bottom",
          plot.caption = element_text(
               size = 14,
               face = "italic", color = "black"
          )
     ) +
     labs(
          fill = "Data source",
          caption = "Dates without data: values imputed using mean steps for entire period.
          Path represents three-day moving average (mean).",
    title = "Daily step count",
    subtitle = paste0(first_date_steps, ": Present")
     )

# Facet plot: bind rHR and step plots rowwise
rHR_steps_plots <- plot_grid(rHR_plot, steps_plot, nrow = 2, labels = "") %>%
     plot_grid()
save_plot(
     plot = rHR_steps_plots, here("plots", "rHR_steps_plots.tiff"),
     dpi = 300, base_width = 16, base_height = 12
)

# Secondary plot output: Dropbox -----------------------------------------------
save_plot(
     plot = rHR_steps_plots, "C:/Users/INGRAM_T/Dropbox/Auto_plots/fitbit_plot_two.tiff",
     dpi = 300, base_width = 16, base_height = 12
)
try(dev.off())
