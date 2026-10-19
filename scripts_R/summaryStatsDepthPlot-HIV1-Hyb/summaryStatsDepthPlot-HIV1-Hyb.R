# ---
# author: Laise de Moraes
# email: laise.moraes@fiocruz.br
# github: https://github.com/lpmor22
# date: Oct 19, 2026
# ---

if (!require(pacman)) install.packages("pacman") else library(pacman)

pacman::p_load(
  this.path,
  cowplot,
  dplyr,
  ggplot2,
  patchwork,
  purrr,
  tibble
)

setwd(dirname(this.path()))

input <- list.files(pattern = "\\.depth\\.tsv$", full.names = TRUE,
                    recursive = TRUE)

depth <- purrr::map_dfr(input, function(f) {
  if (file.info(f)$size == 0) return(NULL)
  df <- read.delim(f, sep = "\t", header = FALSE)
  if (nrow(df) == 0) return(NULL)
  id_sample <- strsplit(basename(f), "\\.")[[1]][1]
  data.frame(sample = id_sample, position = df$V2, depth = df$V3)})

stats_pos <- depth %>% dplyr::group_by(position) %>% dplyr::summarise(
  n = dplyr::n(),
  q1 = quantile(depth, .25, na.rm = TRUE),
  mean = mean(depth, na.rm = TRUE),
  med = median(depth, na.rm = TRUE),
  q3 = quantile(depth, .75, na.rm = TRUE),
  iqr = q3 - q1,
  lower = max(min(depth, na.rm = TRUE), q1 - 1.5 * iqr),
  upper = min(max(depth, na.rm = TRUE), q3 + 1.5 * iqr),
  .groups = "drop") %>%
  dplyr::mutate(dplyr::across(c(q1, mean, med, q3, lower, upper), ~ pmax(.x, .1)))

p_hist <- ggplot(depth, aes(x = depth)) +
  geom_histogram(fill = "#9AB6D1", colour = "#FFFFFF", linewidth = .2) +
  scale_x_log10(labels = scales::comma, breaks = c(1, 10, 100, 1000, 10000)) +
  labs(x = "", y = "Count") +
  theme_light(base_size = 14) +
  theme(panel.grid.minor = element_blank())
p_dens <- ggplot(depth, aes(x = depth)) +
  geom_density(fill = "#9AB6D1", alpha = .6, colour = "#274864", linewidth = .2) +
  scale_x_log10(labels = scales::comma, breaks = c(1, 10, 100, 1000, 10000)) +
  labs(x = "Depth (x)", y = "Density") +
  theme_light(base_size = 14) +
  theme(panel.grid.minor = element_blank())
plot_dist <- p_hist / p_dens + plot_layout(nrow = 2)
save_plot("distEvaluation.pdf", plot_dist, 
          base_height = 10, base_width = 20, device = "pdf")

depcov <- ggplot(stats_pos, aes(x = position)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Whiskers (1.5xIQR)"), colour = NA) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "Interquartile range (IQR)"), colour = NA) +
  geom_line(aes(y = mean, colour = "Mean"), linewidth = .3, linetype = "solid") +
  geom_line(aes(y = med, colour = "Median"), linewidth = .3) +
  geom_line(aes(y = q1), colour = "#9AB6D1", linewidth = .1) +
  geom_line(aes(y = q3), colour = "#9AB6D1", linewidth = .1) +
  geom_line(aes(y = lower), colour = "#D0DCEA", linewidth = .1) +
  geom_line(aes(y = upper), colour = "#D0DCEA", linewidth = .1) +
  scale_fill_manual(name = NULL, values = c("Interquartile range (IQR)" = "#B9CCDF",
                                            "Whiskers (1.5xIQR)" = "#E4ECF3"),
                    limits = c("Whiskers (1.5xIQR)", "Interquartile range (IQR)")) +
  scale_colour_manual(name = NULL,
                      values = c("Median" = "#274864",
                                 "Mean"   = "#FF0000"),
                      breaks = c("Median", "Mean")) +
  guides(fill = guide_legend(order = 1, override.aes = list(alpha = 1)),
         colour = guide_legend(order = 2, override.aes = list(linetype = c("solid", "solid")))) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000), labels = scales::comma,
                limits = c(1, 2000), oob = scales::squish, minor_breaks = NULL,
                expand = expansion(mult = .002)) +
  scale_x_continuous(expand = expansion(0, 0),
                     breaks = c(1, 634, 790, 2292, 5041, 5619, 5831, 6045,
                                6062, 6310, 8379, 8424, 8653, 8797, 9086,
                                9417, 9719, 9728),
                     guide = guide_axis(n.dodge = 2)) +
  coord_cartesian(xlim = c(0, 9800), expand = FALSE) +
  labs(x = NULL, y = "Per base coverage (x)") +
  theme_light(base_size = 10) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        legend.title    = element_blank()) +
  geom_hline(yintercept = 10, linetype = "dotted", colour = "#FF0000")

map2genome1 <- tribble(~"gene", ~"start", ~"end",
                       "5'LTR", 1, 634,
                       "gag", 790, 2292,
                       "vif", 5041, 5619,
                       "tat", 8379, 8424,
                       "nef", 8797, 9417)
map2plot1 <- map2genome1 %>% ggplot() +
  geom_rect(aes(xmin = start, xmax = end, ymin = 8, ymax = 10),
            linewidth = .2, fill = "#FF0000", colour = "#AEAEAE", alpha = .3) +
  geom_text(aes(x = (start + end) / 2, y = 9, label = gene), size = 4) +
  scale_x_continuous(expand = expansion(0, 0), limits = c(0, 9800)) +
  theme_void() + theme(legend.position = "none") +
  coord_cartesian(clip = "off")
map2genome2 <- tribble(~"gene", ~"start", ~"end",
                       "tat", 5831, 6045,
                       "vpu", 6062, 6310,
                       "rev", 8379, 8653,
                       "3'LTR", 9086, 9719)
map2plot2 <- map2genome2 %>% ggplot() +
  geom_rect(aes(xmin = start, xmax = end, ymin = 8, ymax = 10),
            linewidth = .2, fill = "#FF0000", colour = "#AEAEAE", alpha = .3) +
  geom_text(aes(x = (start + end) / 2, y = 9, label = gene), size = 4) +
  scale_x_continuous(expand = expansion(0, 0), limits = c(0, 9800)) +
  theme_void() + theme(legend.position = "none") +
  coord_cartesian(clip = "off")
map2genome3 <- tribble(~"gene", ~"start", ~"end",
                       "pol", 2085, 5096,
                       "vpr", 5559, 5850,
                       "rev", 5970, 6045,
                       "env", 6225, 8795)
map2plot3 <- map2genome3 %>% ggplot() +
  geom_rect(aes(xmin = start, xmax = end, ymin = 8, ymax = 10),
            linewidth = .2, fill = "#FF0000", colour = "#AEAEAE", alpha = .3) +
  geom_text(aes(x = (start + end) / 2, y = 9, label = gene), size = 4) +
  scale_x_continuous(expand = expansion(0, 0), limits = c(0, 9800)) +
  theme_void() + theme(legend.position = "none") +
  coord_cartesian(clip = "off")

plot <- depcov / map2plot1 / map2plot2 / map2plot3 +
  plot_layout(nrow = 4, heights = c(3, .3, .3, .3))

save_plot("summaryStatsDepthPlot-HIV1-Hyb.pdf", plot, base_height = 7, base_width = 20)