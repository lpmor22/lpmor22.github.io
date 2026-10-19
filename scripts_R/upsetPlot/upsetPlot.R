# ---
# author: Laise de Moraes
# email: laise.moraes@fiocruz.br
# github: https://github.com/lpmor22
# date: Oct 18, 2026
# ---

if (!require(pacman)) install.packages("pacman") else library(pacman)
if (!require(BiocManager)) install.packages("BiocManager")
if (!require(ComplexHeatmap)) install.packages("ComplexHeatmap")

p_load(
  this.path,
  ggplot2,
  UpSetR,
  cowplot,
  ComplexHeatmap
)

setwd(dirname(this.path()))

input <- "Sars2Nextclade.csv"

df <- read.csv(input, check.names = FALSE)

matrix <- t(table(stack(df)))
matrix <- t(matrix[, -1])

p <- upset(fromList(df), nintersects = NA, keep.order = TRUE, point.size = 4, line.size = 1,
           # sets = c("SAMPLE_01", "SAMPLE_02", "SAMPLE_03", "SAMPLE_04", "SAMPLE_05"),
           # mainbar.y.label = "Intersection Size",
           mainbar.y.max = 50,
           # matrix.color = "#ff7b00",
           main.bar.color = "#e71837",
           sets.bar.color = "#27357e",
           # sets.x.label = "Set Size",
           # mb.ratio = c(0.6, 0.4),
           order.by = "freq", number.angles = 0, text.scale = c(1, 1, 1, 1, 1, 1))

pdf(paste0(sub("\\.csv$", "", input), ".pdf"), width = 10, height = 5)
print(p)
dev.off()

png(paste0(sub("\\.csv$", "", input), ".png"), width = 10, height = 5, units = "in", res = 300)
print(p)
dev.off()