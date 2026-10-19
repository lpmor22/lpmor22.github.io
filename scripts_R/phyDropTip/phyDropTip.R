# ---
# author: Laise de Moraes
# email: laise.moraes@fiocruz.br
# github: https://github.com/lpmor22
# date: Oct 18, 2026
# ---

if (!require(pacman)) install.packages("pacman") else library(pacman)

p_load(
  this.path,
  ape,
  data.table,
  dplyr,
  rlang,
  treeio
)

setwd(dirname(this.path()))

input <- "SC2More80Cov.gisaid.aln.edited.nwk"
drop_tip <- c("EPI_ISL_11026046", "EPI_ISL_10101858", "EPI_ISL_9671457")

tree <- read.tree(input)
tree <- drop.tip(tree, drop_tip, trim.internal = TRUE)

write.tree(tree, file = sub("\\.nwk$", "-droped.nwk", input), append = FALSE)