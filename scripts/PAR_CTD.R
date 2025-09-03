# ---- Packages ----
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# ---- Load and prepare data ----
df <- read_excel("/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Cladocora/Data/PAR_CTD_SdotYam.xlsx", sheet = "Sheet1")

# If multiple dates, choose the most recent
date_to_plot <- max(df$Date, na.rm = TRUE)

plot_df <- df %>%
  filter(Date == date_to_plot) %>%               # pick one cast/date
  filter(`Depth [m]` >= 1, `Depth [m]` <= 50) %>%  # 1–50 m
  select(Depth = `Depth [m]`, PAR)

# Helpful x-limits so the axis goes to ~0 on the right
x_max <- max(plot_df$PAR, na.rm = TRUE) * 1.05

# ---- Create plot ----
p <- ggplot(plot_df, aes(x = PAR, y = Depth)) +
  geom_point(size = 2) +
  # Depth increases downward; ticks on the RIGHT
  scale_y_reverse(
    limits = c(50, 0),
    breaks = seq(0, 100, 10),
    position = "right",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # PAR axis across the TOP, large values on the LEFT
  scale_x_reverse(
    limits = c(x_max, 0),
    position = "top",
    labels = scales::comma_format()  # show plain numbers
  ) +
  labs(
    #title = paste0("PAR Station 100 ", format(date_to_plot, "%m/%Y")),
    x = expression("PAR ("*mu*"mol m"^-2*" s"^-1*")"),
    y = "Depth (m)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# ---- Print to screen ----
print(p)

# ---- Save to specific folder ----
# Change this path to your preferred folder
save_folder <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Cladocora/Output"

# Save as PNG
ggsave(file.path(save_folder, "PAR_profile.png"), plot = p, width = 6, height = 8, dpi = 300)

# Save as PDF
ggsave(file.path(save_folder, "PAR_profile.pdf"), plot = p, width = 6, height = 8)


# ---- Extract PAR values at 10 m and 30 m ----
PAR_10 <- plot_df %>% filter(Depth == 10) %>% pull(PAR)
PAR_30 <- plot_df %>% filter(Depth == 30) %>% pull(PAR)

if (length(PAR_10) == 1 & length(PAR_30) == 1) {
  depletion_pct <- (PAR_10 - PAR_30) / PAR_10 * 100
  
  # Print values in console
  message(sprintf("PAR at 10 m: %.2f", PAR_10))
  message(sprintf("PAR at 30 m: %.2f", PAR_30))
  message(sprintf("Depletion from 10 m to 30 m: %.2f%%", depletion_pct))
  
  subtitle_txt <- sprintf("PAR(10 m) = %.1f | PAR(30 m) = %.1f | Depletion = %.1f%%",
                          PAR_10, PAR_30, depletion_pct)
} else {
  subtitle_txt <- "Missing PAR at 10 m or 30 m"
}
