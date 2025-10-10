library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(ggplot2)
library(ggtext)
library(cowplot)
library(grid)
library(grImport2)

# load data_a
load("fig3.RData")

# color_a
custom_colors <- c("#b61024",'#f4a08d',"#73889c")
group_colors <- c("#e2939c","#fff0eb","#bec8d3")

# hist plot
p_a_hist <-ggplot(dat_a_hist, aes(x = service_min, group = group)) +
  geom_step(
    data = dat_a_hist,
    aes(x = left, y = count, color = group),
    linewidth = 0.8,
    alpha = 0.7
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(title = glue("<span style='color:{custom_colors[1]};'>PreA-only</span> vs. <span style='color:{custom_colors[2]};'>PreA-human</span> vs. <span style='color:{custom_colors[3]};'>No-PreA</span>"),
       x = "Consulation duration (minutes)", y = "No. of patients") +
  theme_minimal() +
  theme(
    plot.title = element_markdown(hjust = 0.5, margin = margin(b = 0),size =25),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(color = "black", linewidth = 1),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.ticks.y = element_line(color = "black", linewidth = 1),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.y = element_text(size = 25, margin = margin(r = 0)),
    axis.text.x = element_text(size = 25, margin = margin(t = 0))
  ) +
  annotate("segment", y = -Inf, yend = 200, x = -Inf, xend = -Inf, color = "black", linewidth = 0.8) +
  annotate("segment", x = -Inf, xend = 30, y = -Inf, yend = -Inf, color = "black", linewidth = 0.8) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 40), expand = c(0.02, 0)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), expand = c(0.02, 0)) +
  coord_cartesian(clip = "off")

# a box plot
p_a_box <- ggplot(dat_a_box, aes(x = x, fill = x)) +
  geom_boxplot(
    aes(ymin = ymin,lower = lower,middle = middle,upper = upper,ymax = ymax),
    stat = "identity",
    width = 0.5,
    color = "black",
    linewidth = 0.5,
    outlier.shape = NA
  ) +
  geom_segment(aes(x = as.numeric(x) - 0.1, xend = as.numeric(x) + 0.1, y = ymin, yend = ymin),
               linewidth = 0.5, color = "black") +
  geom_segment(aes(x = as.numeric(x) - 0.1, xend = as.numeric(x) + 0.1, y = ymax, yend = ymax),
               linewidth = 0.5, color = "black") +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  theme_minimal() +
  labs(x = NULL, y = "Consultation duration (minutes)") +
  theme(
    legend.position = c(0.32, 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size =25),
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 25, color = "black"),
    axis.title = element_blank(),
    axis.title.y = element_text(size = 30),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
inset <- ggplotGrob(p_a_box)

# combine + save_a
svg("a.svg", width = 16.67, height = 9.24)
p_a <- ggdraw(p_a_hist) +
  draw_grob(inset, x = 0.39, y = 0.35, width = 0.5, height = 0.5)
print(p_a)
dev.off()
rm(list = setdiff(ls(), c("dat_b", "dat_c", "dat_d", "dat_e1", "dat_e2")))

# plot_b
p_b <- ggplot(dat_b, aes(x = variable, y = mean, fill = group)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.7,
    color = NA
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0,
    position = position_dodge(width = 0.7),
    color = "black",
    linewidth = 0.8
  ) +
  labs(
    x = NULL,
    y = "Likert scale of score",
  ) +
  scale_fill_manual(values = c(
    "No-PreA" = "#bec8d3",
    "PreA-human" = "#fff0eb",
    "PreA-only" = "#e2939c"
  ),  guide = guide_legend(title = NULL, direction = "horizontal")) +
  scale_y_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, by = 1),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 1.05),
    legend.justification = "center",
    legend.text = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 30, color = "black", angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 25, color = "black" ),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 30, color = "black"),
    axis.title.y = element_text(size = 31, color = "black", margin = margin(r = 15)),
    axis.ticks.length = unit(0.25, "cm"),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, colour = "black", linewidth = 0.5) +
  annotate("segment", x = -Inf, xend = -Inf, y = 0, yend = 5, colour = "black", linewidth = 0.5)

# save_b
svg("b.svg", width = 8.25, height = 5.78, bg = "transparent")
print(p_b)
dev.off()
rm(list = setdiff(ls(), c("dat_c", "dat_d", "dat_e1", "dat_e2")))

# plot_c
p_c <- ggplot(dat_c, aes(x = variable, y = mean, fill = mode)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0, linewidth = 0.8, color = "black", position = position_dodge(width = 0.7)) +
  labs(
    x = "Care coordination",
    y = NULL,
  ) +
  theme_minimal() +
  geom_segment(aes(x = -Inf, xend = Inf, y = 0, yend = 0),
               color = "black", linewidth = 0.5) +
  geom_segment(
    data = data.frame(x = 1),
    aes(x = x, xend = x, y =0, yend = 0 -0.2),
    color = "black", linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  geom_segment(aes(y = 0, yend = 5, x = -Inf, xend = -Inf),
               color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_fill_manual(values = c("No-PreA" = "#bec8d3","PreA-human" = "#fff0eb", "PreA-only" = "#e2939c"), 
                    guide = guide_legend(title = NULL) ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.title.y = element_text(size = 34, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.y = element_text(size = 29, color = "black"),
    axis.text.x = element_text(size = 34,color = "black",margin = margin(t = 18)),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    legend.position = 'None',
  ) +
  coord_cartesian(clip = "off")

# save_c
svg("c.svg", width = 3.84, height = 4.88, bg = "transparent")
print(p_c)
dev.off()
rm(list = setdiff(ls(), c("dat_d", "dat_e1", "dat_e2")))


# plot_d
p_d <- ggplot(dat_d, aes(x = Group, fill = Group)) +
  geom_boxplot(
    aes(ymin = ymin,lower = lower,middle = middle,upper = upper,ymax = ymax),
    stat = "identity", 
    width = 0.5,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c("#F8AE54", "#88CED8")) +
  geom_segment(
    aes(x = as.numeric(Group) - 0.1, xend = as.numeric(Group) + 0.1,
        y = ymin, yend = ymin),
    color = "black", linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.numeric(Group) - 0.1, xend = as.numeric(Group) + 0.1,
        y = ymax, yend = ymax),
    color = "black", linewidth = 0.5
  ) +
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10),expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 50), clip = "on")+
  labs(title = "",x = "",y = "No. of patients") +
  theme_minimal() +
  theme(
    legend.position = c(0.59, 1.2),
    legend.title = element_blank(),
    legend.text = element_text(size =25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title.y = element_text(size = 32, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.y = element_text(size = 25),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.margin = unit(c(3, 2, 1, 2), "cm")
  )+ coord_cartesian(clip = "on")

# save_d
svg("d.svg", width = 5.52, height = 6.42, bg = "transparent")
print(p_d)
dev.off()
rm(list = setdiff(ls(), c("dat_e1", "dat_e2")))

# plot_e1
bar <- function(D, show_x_axis = FALSE){
  size = 0.5
  label <- c()
  title_text <- ""
  
  if(unique(D$variable) == "Usefulness in decision-making"){
    label <- c("Very\nunfavourable", "Unfavour-\n-able", "Neutral", "Favour-\n-able", "Very\nfavourable")
    title_text <- "Usefulness in decision-making"
  }
  if(unique(D$variable) == "Ease of communication"){
    label <- c("Very\nunfavourable", "Unfavour-\n-able", "Neutral", "Favour-\n-able", "Very\nfavourable")
    title_text <- "Ease of communication"
  }
  if(unique(D$variable) == "Relief of workload"){
    label <- c("Very\nunfavourable", "Unfavour-\n-able", "Neutral", "Favour-\n-able", "Very\nfavourable")
    title_text <- "Relief of workload"
  }
  
  D$label <- label
  
  p <- ggplot(D, aes(x = as.factor(value), y = counts)) +
    geom_col(fill = "#FEE9D1", width = 0.8) +
    annotate("text", x = 0.5, y = 50, hjust = 0, label = title_text, size = 11) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(labels = if (show_x_axis) label else NULL) +
    scale_y_continuous(limits = c(0, 90), breaks = c(0, 25, 50), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = if (show_x_axis) element_text(size = 30, color = "black", vjust = 0.5) else element_blank(),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 33, color = "black"),
      axis.ticks.y = element_line(color = "black", linewidth = 0.5),
      axis.title.y = element_text(size = 30, color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      plot.margin = margin(t = -80, r = 0, b = 0, l = 0)
    ) +
    geom_segment(aes(x = -Inf, xend = 5.5, y = -Inf, yend = -Inf),
                 color = "black", linewidth = size) +
    geom_segment(aes(y = 0, yend = 60, x = -Inf, xend = -Inf),
                 color = "black", linewidth = size) +
    coord_cartesian(clip = "on")
  
  return(p)
}

Usefulness <- dat_e1[dat_e1$variable == 'Usefulness in decision-making',]
Ease <- dat_e1[dat_e1$variable == 'Ease of communication',]
Relief <- dat_e1[dat_e1$variable == 'Relief of workload',]

final_plot <- plot_grid(
  bar(Usefulness),
  bar(Ease),
  bar(Relief, show_x_axis = TRUE),
  nrow = 3,
  align = "v",
  rel_heights = c(2, 2, 2.5)
)
y_axis_label <- ggdraw() +
  draw_label("No. of physicians",
             x = 0.5, y = 0.5,
             angle = 90,  size = 38)
p_e1 <- plot_grid(
  y_axis_label, final_plot,
  ncol = 2,
  rel_widths = c(0.08, 1)
)  

# save_e1
svg("e1.svg", width = 12.02, height = 8.85)
print(p_e1)
dev.off()
rm(list = setdiff(ls(), c("dat_e2")))

# plot_e2
p <- ggplot(dat_e2, aes(x = as.factor(variable), y = proportions)) +
  geom_col(position = "dodge", fill = "#D6EAF8", width = 0.6) +
  scale_x_discrete(labels = c("Interpretation","Recording", "Across-discipline","Suggestion","Communication")) +
  labs(x = "", y = "Proportions") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 32, color = "black",angle = 45, hjust = 1, vjust = 1),
    plot.title = element_markdown(hjust = 0.5, face = "bold"),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_text(size = 32, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

# save_e2
svg("e2.svg", width = 6.03, height = 6.96)
print(p)
dev.off()
rm(list = ls())

# combine to fig3
svg_files <- c("a.svg", "b.svg", "c.svg", "d.svg", "e1.svg", "e2.svg")
pictures <- lapply(svg_files, readPicture)
pdf("combined.pdf", width = 10, height = 12)
grid.newpage()

draw_svg_at <- function(pic, x, y, width = 0.3, height = 0.3) {
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = c("center", "center")))
  grid.picture(pic)
  popViewport()
}

draw_label <- function(label, x, y) {
  grid.text(label, x = x, y = y, 
            gp = gpar(fontsize = 14, fontface = "bold", col = "black"))
}

draw_svg_at(pictures[[1]], x = 0.495, y = 0.84, width = 0.63, height = 0.3)
draw_label("a", x = 0.2, y = 0.975)
draw_svg_at(pictures[[2]], x = 0.35, y = 0.60)
draw_label("b", x = 0.2, y = 0.69)
draw_svg_at(pictures[[3]], x = 0.54, y = 0.618, width = 0.18, height = 0.125)
draw_label("c", x = 0.48, y = 0.69)
draw_svg_at(pictures[[4]], x = 0.69, y = 0.61, width = 0.3, height = 0.18)
draw_label("d", x = 0.63, y = 0.69)
draw_svg_at(pictures[[5]], x = 0.37, y = 0.41, width = 0.35, height = 0.3)
draw_label("e", x = 0.2, y = 0.50)
draw_svg_at(pictures[[6]], x = 0.66, y = 0.39, width = 0.25, height = 0.2)
dev.off()

input_pdf <- "combined.pdf"
output_pdf <- "fig3.pdf"
margins <- "10 10 10 10"
cmd <- sprintf('pdfcrop --margins "%s" %s %s', margins, input_pdf, output_pdf)
system(cmd)
rm(list = ls())
