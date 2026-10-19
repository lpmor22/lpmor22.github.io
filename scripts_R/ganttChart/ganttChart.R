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
  dplyr,
  ggplot2,
)

setwd(dirname(this.path()))

input <- "KhouriousTeam2025.csv"

df <- read.csv(input, header = TRUE)

order <- unique(df$name)
df$name <- factor(df$name, levels = rev(order))

colors <- c(
  "PI" = "#e97a7f",
  "Research Associate" = "#ec90e1",
  "Postdoctoral Researcher" = "#b693ed",
  "Ph.D. Student" = "#8db5cd",
  "M.Sc. Student" = "#acd3ec",
  "Undergraduate Student" = "#98ebae"
)

df <- df %>% mutate(color = colors[type])
df$type <- factor(df$type, levels = names(colors))

chart <- ggplot(df,
                aes(x = as.Date(start, "%Y.%m.%d"),
                    xend = as.Date(end, "%Y.%m.%d"),
                    y = name,
                    yend = name,
                    color = type)) +
  geom_segment(linewidth = 6) +
  scale_color_manual(
    values = colors,
    breaks = names(colors)) +
  scale_x_date(
    expand = expansion(mult = c(0, 0)),
    date_breaks = "1 year",
    date_labels = "%Y") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(linetype = "solid", linewidth = .05),
    panel.grid.minor = element_line(linetype = "solid", linewidth = .05),
    legend.position = c(.15,.2),
    legend.title = element_blank(),
    legend.background = element_rect(color = "#BEBEBE", linewidth = .2)) +
  labs(x = NULL, y = NULL)
chart

save_plot(sub("\\.csv$", ".pdf", input), chart, base_height = 10, base_width = 12)
save_plot(sub("\\.csv$", ".png", input), chart, base_height = 10, base_width = 12)

# engaging team members

excl <- c(
  "Valdomiro Moitinho",
  "Maria da Purificação",
  "Lia Barbara Arruda",
  "Cibele Orge",
  "Lucas Campos",
  "Camila Brito",
  "Lucas Gentil",
  "Felipe Torres",
  "Kleverton Ribeiro",
  "Victoria Ribeiro",
  "Laís Cambuí",
  "Marina Cucco",
  "Raphaela Andrade",
  "Ellen Pimentel",
  "Agatha Morais",
  "Igor Silveira",
  "Giulia Botelho",
  "Ícaro Ströbel",
  "Jessica Lais Santos"
)

df <- df %>% filter(!name %in% excl)

chart <- ggplot(df,
                aes(x = as.Date(start, "%Y.%m.%d"),
                    xend = as.Date(end, "%Y.%m.%d"),
                    y = name,
                    yend = name,
                    color = type)) +
  geom_segment(linewidth = 6) +
  scale_color_manual(
    values = colors,
    breaks = names(colors)) +
  scale_x_date(
    expand = expansion(mult = c(0, 0)),
    date_breaks = "1 year",
    date_labels = "%Y") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(linetype = "solid", linewidth = .05),
    panel.grid.minor = element_line(linetype = "solid", linewidth = .05),
    legend.position = c(.15,.2),
    legend.title = element_blank(),
    legend.background = element_rect(color = "#BEBEBE", linewidth = .2)) +
  labs(x = NULL, y = NULL)
chart

save_plot(sub("\\.csv$", "etm.pdf", input), chart, base_height = 6.1, base_width = 12)
save_plot(sub("\\.csv$", "etm.png", input), chart, base_height = 6.1, base_width = 12)