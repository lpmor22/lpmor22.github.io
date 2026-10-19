# ---
# author: Laise de Moraes
# email: laise.moraes@fiocruz.br
# github: https://github.com/lpmor22
# date: Oct 18, 2026
# ---

if (!require(pacman)) install.packages("pacman") else library(pacman)

p_load(
  this.path,
  cowplot,
  ggplot2,
  ggtree,
  phangorn,
  readxl,
  tidyr
)

setwd(dirname(this.path()))

input1 <- "SC2More80Cov.aln.edited.nwk"
input2 <- "SC2More80Cov.aln.edited.xlsx"

tree <- read.tree(input1)
metadata <- read_excel(input2)

tree <- midpoint(tree)
tree <- reorder(tree)
tree <- ladderize(tree)
tree <- ggtree(tree, size = .4, color = "#8c8fae", ladderize = FALSE) +
  geom_rootedge(rootedge = .00001, size = .4)
tree <- tree %<+% metadata +
  geom_tippoint(aes(color = group2),
                shape = 19, size = 4, show.legend = TRUE) +
  scale_color_manual(na.translate = FALSE,
                     values = c(
                       "#660033",
                       "#9A6348",
                       "#D79B7D",
                       "#C0C741",
                       "#647D34",
                       "#E4943A",
                       "#CC3333",
                       "#D26471",
                       "#70377F",
                       "#7EC4C1",
                       "#34859D",
                       "#17434B",
                       "#441A3F",
                       "#584563",
                       "#C2C2DA"
                     )
  )

tree <- tree +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 40, 10, 40, unit = "pt"))

save_plot(sub("\\.nwk$", ".pdf", input1), tree, base_height = 5, base_width = 6)
save_plot(sub("\\.nwk$", ".png", input1), tree, base_height = 5, base_width = 6)