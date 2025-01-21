# Compute the Tail Dependence Coefficient at the 5% percentile (Probability of Extreme Losses)
compute_tdc_matrix <- function(data, percentile = 0.05) {
  n <- nrow(data)
  percentile_value <- quantile(data[[1]], probs = percentile)
  
  # Find the number of observations below the percentile value
  k <- sum(data[[1]] <= percentile_value)
  
  # Compute the TDC matrix for the lower tail
  tdc_mat <- FRAPO::tdc(data, method = "EmpTC", lower = TRUE, k = k)
  
  return(tdc_mat)
}


# Function to plot dependency and tail dependence coefficient matrices
plot_dep_tdc <- function(data, cor_levels, percentile = 0.05) {
  
  # Compute Spearman correlation matrix
  corr_res <- Hmisc::rcorr(as.matrix(data), type = "spearman")
  corr_mat <- corr_res$r
  
  # Compute TDC matrix
  tdc_mat <- compute_tdc_matrix(data, percentile = percentile)
  
  combined_mat <- matrix(NA, 12, 12)
  colnames(combined_mat) <- colnames(corr_mat)
  rownames(combined_mat) <- colnames(corr_mat)
  
  combined_mat[lower.tri(combined_mat)] <- tdc_mat[lower.tri(tdc_mat)]
  combined_mat[upper.tri(combined_mat)] <- corr_mat[upper.tri(corr_mat)]
  combined_mat_long <- reshape2::melt(combined_mat, na.rm = FALSE) |> 
    mutate(
      Var1 = factor(Var1, levels = cor_levels, ordered = TRUE),
      Var2 = factor(Var2, levels = cor_levels, ordered = TRUE)
    )
  
  # Plotting
  combined_mat_long |> ggplot( 
    aes(
      x = Var1, y = Var2, 
      fill = value, 
      label = ifelse(!is.na(value), round(value, 2), "")
    )
  ) +
    geom_tile(color = "white") + 
    geom_text(color = "darkgray", size = 9, na.rm = TRUE) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(limits = rev, expand = c(0,0)) +
    scale_fill_distiller(
      palette = "RdBu", 
      direction = 1, 
      limits = c(0, 1), 
      na.value = "transparent", 
      name = NULL, 
      guide = guide_colourbar(
        barwidth = 25, barheight = 1.5
      )
    ) + 
    labs(fill = NULL) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 90, vjust = 0.5, hjust = 1, size = 24
      ),
      axis.text.y = element_text(size = 24),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 34),
      legend.text = element_text(size = 24),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) -> joint_dep
  
  return(joint_dep)
}

# Wide format for stable periods, and pandemic periods for each sector
staples_stable_wide <- get_wide_prices(staples_stable)
staples_pandemic_wide <- get_wide_prices(staples_pandemic)

discretionary_stable_wide <- get_wide_prices(discretionary_stable)
discretionary_pandemic_wide <- get_wide_prices(discretionary_pandemic)

communication_stable_wide <- get_wide_prices(communication_stable)
communication_pandemic_wide <- get_wide_prices(communication_pandemic)


cor_levels <- colnames(
  Hmisc::rcorr(
    as.matrix(staples_stable_wide), 
    type = "spearman"
  )$r
)

# Example usage with the same structure as plot_dep_mat
cor_legend <- plot_dep_tdc(staples_stable_wide, cor_levels) +
  ggtitle("Stable Market")
cor_legend <- cowplot::get_plot_component(
  cor_legend, 'guide-box-bottom', return_all = TRUE
)

cor_title <- ggdraw() + 
  draw_label(
    "Spearman and tail dependence coefficient of log returns for the Consumer Staples sector",
    fontface = 'bold',
    size = 40,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

png(
  "cor_tdc_staples.png", 
  width = 1920, height = 1080
)
plot_grid(
  cor_title,
  NULL,
  plot_grid(
    plot_grid(
      plot_dep_tdc(staples_stable_wide, cor_levels, percentile = 0.05) +
        ggtitle("Stable Market") +
        theme(legend.position = "none"),
      plot_dep_tdc(staples_pandemic_wide, cor_levels, percentile = 0.05) +
        ggtitle("Pandemic and Downturn") +
        theme(legend.position = "none"),
      nrow = 1, rel_widths = c(1.4, 1.4)
    ),
    cor_legend,
    nrow = 2, rel_heights = c(1, 0.075)
  ),
  ncol = 1, rel_heights = c(0.1, 0.01, 1.4)
) -> joint_plot
print(joint_plot)
dev.off()

# Function to plot scaled closing prices of stocks
plot_stocks <- function(prices) {
  
  # Define the crisis periods
  crisis_periods <- data.frame(
    start = as.Date(
      c("2016-01-01", "2019-12-31")
    ),
    end = as.Date(
      c("2018-12-31", "2023-12-31")
    ),
    crisis = c(
      "Stable Market", "Pandemic and Downturn"
    )
  ) |> 
    mutate(crisis = factor(
      crisis, ordered = TRUE, 
      levels = c(
        "Stable Market", "Pandemic and Downturn"
      )
    )
    )
  
  # Custom color scale for stocks
  custom_colors <- c(
    "WMT" = "#FFA80A",   # Walmart
    "KR" = "#0A4EFF",   # Kroger
    "COST" = "#FF280A"    # Costco
  )
  
  other_stocks <- setdiff(unique(prices$symbol), names(custom_colors))
  custom_colors[other_stocks] <- "gray"
  
  # Define colors for crisis periods
  crisis_colors <- c(
    "Stable Market" = "#24f81e",
    "Pandemic and Downturn" = "#ffe700"
  )
  
  # Separate the data for special stocks and other stocks
  special_stocks <- prices |> filter(symbol %in% c("WMT", "KR", "COST"))
  other_stocks_data <- prices |> filter(!(symbol %in% c("WMT", "KR", "COST")))
  
  stock_plot <- ggplot() +
    # Plot the crisis periods
    geom_rect(
      data = crisis_periods, 
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = crisis),
      alpha = 0.45, inherit.aes = FALSE
    ) +
    # Plot the other stocks first
    geom_line(
      data = other_stocks_data, 
      aes(x = date, y = close_scaled, color = symbol, group = symbol), 
      linewidth = 1.5) +
    # Plot the special stocks on top
    geom_line(
      data = special_stocks, 
      aes(x = date, y = close_scaled, color = symbol, group = symbol), 
      linewidth = 1.5) +
    scale_color_manual(
      values = custom_colors,
      breaks = c("WMT", "KR", "COST"),
      labels = c("WMT", "KR", "COST")
    ) +
    scale_fill_manual(
      values = crisis_colors,
      breaks = c("Stable Market", "Pandemic and Downturn"),
      labels = c("Stable Market", "Pandemic and Downturn")
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      x = NULL,
      y = "Scaled closing prices",
      color = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 24),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 24),
      legend.box = "vertical",
      legend.box.just = "center",
      legend.spacing = unit(0.5, "cm"),
      legend.margin = margin(t = -10),
      panel.grid.minor = element_blank()
    ) + 
    guides(
      color = guide_legend(
        order = 1, 
        nrow = 1, 
        byrow = TRUE, 
        keyheight = unit(4, "lines"),
        override.aes = list(
          linetype = 1, 
          size = 20)
      ),
      fill = guide_legend(order = 2, override.aes = list(size = 12))
    )
  
  return(stock_plot)
}

# Function to plot periods
plot_periods <- function(periods, crisis_period, stock = c("WMT" = "#FFA80A")) {
  
  # Define the crisis periods
  crisis_periods <- data.frame(
    start = as.Date(
      c("2016-01-01", "2019-12-31")
    ),
    end = as.Date(
      c("2018-12-31", "2023-12-31")
    ),
    crisis = c(
      "Stable Market", "Pandemic and Downturn"
    )
  ) |> 
    mutate(crisis = factor(
      crisis, ordered = TRUE, 
      levels = c(
        "Stable Market", "Pandemic and Downturn"
      )
    )
    )
  
  # Define colors for crisis periods
  crisis_colors <- c(
    "Stable Market" = "#24f81e",
    "Pandemic and Downturn" = "#ffe700"
  )
  
  # Custom color scale for stocks
  custom_colors <- stock
  
  other_stocks <- setdiff(unique(periods$symbol), names(custom_colors))
  custom_colors[other_stocks] <- "gray"
  
  # Separate the data for special stocks and other stocks
  special_stocks <- periods |> filter(symbol %in% names(stock))
  other_stocks_data <- periods |> filter(!(symbol %in% names(stock)))
  
  ggplot() +
    # Plot the crisis periods
    geom_rect(
      data = filter(crisis_periods, crisis == crisis_period), 
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = crisis),
      alpha = 0.45, inherit.aes = FALSE
    ) +
    # Plot the other stocks first
    geom_line(
      data = filter(other_stocks_data, period == crisis_period),
      aes(x = date, y = close_lgret, group = symbol), 
      color = "gray", linewidth = 1.5) +
    # Plot the special stocks on top
    geom_line(
      data = filter(special_stocks, period == crisis_period),
      aes(x = date, y = close_lgret, color = symbol, group = symbol), 
      linewidth = 1.5) +
    scale_color_manual(
      values = custom_colors,
      breaks = names(stock),
      labels = names(stock)
    ) +
    scale_fill_manual(
      values = crisis_colors,
      breaks = c("Stable Market", "Pandemic and Downturn"),
      labels = c("Stable Market", "Pandemic and Downturn")
    ) +
    scale_x_date(breaks = date_ticks(5), date_labels = "%Y") +  
    scale_y_continuous(
      breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
      labels = c(-0.4, -0.2, 0, 0.2, 0.4),
      limits = c(-0.5, 0.5)
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = " "
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 24),
      legend.position = "none",
      plot.margin = margin(5.5, 15.5, 5.5, 5.5), 
      panel.grid.minor = element_blank(),
    )  -> plot_periods
  
  return(plot_periods)
}

# Generate the individual plots
plot1 <- plot_grid(
  plot_periods(staples_period, "Stable Market", c("WMT" = "#FFA80A")) + 
    labs(y = " ") +
    theme(axis.text.x = element_text(color = "white")),
  plot_periods(staples_period, "Pandemic and Downturn", c("WMT" = "#FFA80A")) +
    labs(y = " ") +
    theme(axis.text = element_text(color = "white")),
  nrow = 1,
  rel_widths = c(1, 1)
)

plot2 <- plot_grid(
  plot_periods(staples_period, "Stable Market", c("KR" = "#0A4EFF")) + 
    labs(y = "Log returns of closing prices") +
    theme(axis.text.x = element_text(color = "white")),
  plot_periods(staples_period, "Pandemic and Downturn", c("KR" = "#0A4EFF")) +
    labs(y = " ") +
    theme(axis.text = element_text(color = "white")),
  nrow = 1,
  rel_widths = c(1, 1)
)

plot3 <- plot_grid(
  plot_periods(staples_period, "Stable Market", c("COST" = "#FF280A")) + 
    labs(y = " "),
  plot_periods(staples_period, "Pandemic and Downturn", c("COST" = "#FF280A")) +
    labs(y = " ") +
    theme(axis.text.y = element_text(color = "white")),
  nrow = 1,
  rel_widths = c(1, 1)
)

# Combine all plots
log_returns_periods <- plot_grid(
  plot1,
  plot2,
  plot3,
  nrow = 3,
  rel_heights = c(1, 1, 1),
  align = 'v'
)

stocks_legend <- plot_stocks(staples_prices)
stocks_legend <- cowplot::get_plot_component(
  stocks_legend, 'guide-box-bottom', return_all = TRUE
)

stocks_title <- ggdraw() + 
  draw_label(
    "Log returns of selected Consumer Staples sector stocks in the S&P500",
    fontface = 'bold',
    size = 40,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

png(
  "log_returns_consumer.png", 
  width = 1920, height = 1080*0.9
)
plot_grid(
  stocks_title,
  NULL,
  plot_grid(
    plot_grid(
      plot_stocks(staples_prices) +
        theme(legend.position = "none"),
      log_returns_periods,
      ncol = 2
    ),
    NULL,
    stocks_legend,
    ncol = 1, rel_heights = c(1, 0.01, 0.1)),
  ncol = 1, rel_heights = c(0.1, 0.01, 1.4)
) -> stocks_main
print(stocks_main)
dev.off()



# Transform log returns into pseudo-observations (uniform margins)

# Function to plot the copula
plot_cop <- function(data, symbol_x, symbol_y, symbol_x_label, symbol_y_label) {
  
  pobs_log_returns <- pobs(as.matrix(data))
  pobs_log_returns <- as.data.frame(pobs_log_returns)
  names(pobs_log_returns) <- names(data)
  
  u <- pobs_log_returns[, symbol_x]
  v <- pobs_log_returns[, symbol_y]
  selectedCopula <- BiCopSelect(u, v)
  
  # Simulate data from the fitted copula
  simulated_data <- BiCopSim(
    1000, 
    family = selectedCopula$family, 
    par = selectedCopula$par, 
    par2 = selectedCopula$par2
  )
  
  # Create a data frame for ggplot2
  df <- data.frame(u = simulated_data[, 1], v = simulated_data[, 2])
  
  df_data <- pobs_log_returns[, c(symbol_x, symbol_y)]
  names(df_data) <- c("x", "y")
  
  # Create the contour plot without grid background
  ggplot(df, aes(x = u, y = v)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    geom_point(
      data = df_data,
      aes(x, y),
      shape = 19, size = 4, 
      color = "black", fill = "gray", 
      alpha = 0.4
    ) +
    scale_fill_viridis_c(alpha = 0.9) +
    labs(
      x = symbol_x_label, 
      y = symbol_y_label
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 24),
      plot.title = element_text(face = "bold", size = 34),
      plot.title.position = "plot",
      panel.grid.minor = element_blank()
    ) -> cop_plot
  
  return(cop_plot)
  
}  

png(
  "copula_consumer.png", 
  width = 720, height = 720
)

plot_cop(
  staples_stable_wide, 
  "HD", "LOW", "Log Return - Stock X", "Log Return - Stock Y"
) + ggtitle("Bivariate Stock Dependency via Copula") -> cop_plot
print(cop_plot)
dev.off()



percentile <- 0.05
data <- staples_pandemic_wide
# Compute Spearman correlation matrix
corr_res <- Hmisc::rcorr(as.matrix(data), type = "spearman")
corr_mat <- corr_res$r
View(corr_mat)

# Compute TDC matrix
tdc_mat <- compute_tdc_matrix(data, percentile = percentile)
View(tdc_mat)
