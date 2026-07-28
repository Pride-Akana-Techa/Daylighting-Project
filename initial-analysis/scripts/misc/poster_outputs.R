
####### Poster Outputs #####
monet <- c(
  "#A8C5D9",
  "#B9CBAE",
  "#7FA7A6",
  "#F3F1E6",
  "#C7D5E3"
)

# Van Gogh Sunflowers
sunflower <- c(
  "#F2C94C",
  "#1E4E8C",
  "#D4A04A",
  "#8A9B5B",
  "#F7F4E7"
)

academic <- c(
  "#0D1B3D",
  "#385C8E",
  "#BFC7D5",
  "#FFFFFF",
  "#F28E2B"
)

# Natural Muted
natural <- c(
  "#A6B89A",
  "#8CA3B7",
  "#DCCDB4",
  "#333333",
  "#F7F7F5"
)


shared_y_range <- range(
  c(ci_2024$lwr, ci_2024$upr, ci_2025$lwr, ci_2025$upr),
  na.rm = TRUE
)

shared_y_breaks <- pretty(shared_y_range, n = 8)
shared_y_limits <- range(shared_y_breaks)
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks, y_limits,    
                             base_size = 14,
                             label_nudge_days = 15){
  
  years_in_range <- seq(as.numeric(format(x_limits[1], "%Y")),
                        as.numeric(format(x_limits[2], "%Y")))
  year_positions <- lapply(years_in_range, function(yr) {
    yr_start <- max(as.Date(paste0(yr, "-01-01")), x_limits[1])
    yr_end   <- min(as.Date(paste0(yr, "-12-31")), x_limits[2])
    data.frame(year = as.character(yr), mid_date = yr_start + (yr_end - yr_start) / 2)
  }) |> bind_rows()
  
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.4) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1.8) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1.8) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black", linewidth = .5) +
    annotate("text", x = cutoff_date + label_nudge_days, y = Inf, label = event_label,
             hjust = 0, vjust = 1.5, fontface = "bold", size = 4, lineheight = 0.9) +
    annotate("text", x = year_positions$mid_date, y = -Inf, label = year_positions$year,
             vjust = 3.2, size = base_size * 0.32) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b",
                 limits = x_limits,
                 breaks = x_breaks,
                 expand = c(0.02, 0)) +
    theme_minimal(base_size = base_size, base_family = "Helvetica") +
    labs(title = model_label, x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 30, 10),
          axis.text.x = element_text(margin = margin(t = 2)),
          axis.ticks.length.x = unit(5, "pt")) +
    coord_cartesian(ylim = y_limits, clip = "off")  
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = NULL,
  line_color  = "#0072B2",
  event_label = "January 1st, 2024\nWarning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months"),
  y_breaks = shared_y_breaks,
  y_limits = shared_y_limits
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = NULL,
  line_color  = "#D55E00",
  event_label = "January 1st, 2025\nEnforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months"),
  y_breaks = shared_y_breaks,
  y_limits = shared_y_limits
)

p_2024
p_2025


ggsave("p_2024.jpg", plot = p_2024, width = 10, height = 10, units = "in", dpi = 300)
ggsave("p_2025.jpg", plot = p_2025, width = 10, height = 10, units = "in", dpi = 300)
