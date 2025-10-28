library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(ggplot2)
library(ggtext)
library(cowplot)
library(grid)
library(grImport2)
library(plotrix) 

# load data_a
load("fig2.RData")

# color_a
custom_colors <- c("#b61024",'#f4a08d',"#73889c")
group_colors <- c("#e2939c","#fff0eb","#bec8d3")

# lab1, lab2, broken, lab3, lab4
rescale.x <- function(y, lab1, lab2, lab3, lab4) {
  x.new <- lab2 + (lab2 - lab1) + (y - lab3) / (lab4 - lab3) * (lab2 - lab1)
  return(x.new)
}

dat_a_hist <- dat_a_hist %>%
  mutate(left = ifelse(left > 27, rescale.x(left, 20, 25, 42, 45), left))
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
  annotate("segment", x = -Inf, xend = 27.35 , y = -Inf, yend = -Inf, color = "black", linewidth = 0.8) +
  annotate("segment", x = 27.65, xend = 35 , y = -Inf, yend = -Inf, color = "black", linewidth = 0.8) +
  geom_segment(x = 27.2, xend = 27.5, y = -6.8, yend = -1.2, color = "black") +
  geom_segment(x = 27.5, xend = 27.8, y = -6.8, yend = -1.2, color = "black") +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 40), expand = c(0.02, 0)) +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, by=5), labels = c(seq(0, 25, by = 5), 42, 45), expand = c(0.02, 0)) +
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
p_a <- ggdraw(p_a_hist) +
  draw_grob(inset, x = 0.39, y = 0.35, width = 0.5, height = 0.5)
ggsave("a.pdf", plot = p_a, width = 16.67, height = 9.24)
rm(list = setdiff(ls(), c("dat_b", "dat_c", "dat_d1", "dat_d2")))

# plot_b
dat_b$Group <- factor(dat_b$Group,
                      levels = c("Participating physicians", "Matched non-participating\nphysicians"),
                      labels = c("Participating physicians", "Matched non-participating physicians"))
p_b <- ggplot(dat_b, aes(x = Group, fill = Group)) +
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
    legend.position = c(0.69, 1.2),
    legend.title = element_blank(),
    legend.text = element_text(size =22),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title.y = element_text(size = 32, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.y = element_text(size = 25),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.margin = unit(c(3, 5.1, 1, 2), "cm")
  )

ggsave("b.pdf", plot = p_b, width = 7.52, height = 6.42)
rm(list = setdiff(ls(), c("dat_c", "dat_d1", "dat_d2")))

# plot_c
radar <- function(summary_data) {
  ppp <- matrix(summary_data$mean, nrow=3, ncol=6)
  colnames(ppp) <- c('Ease', 'Care coordination','Attentiveness', 'Regard', 'Satisfaction', 'Acceptability')
  rownames(ppp) <- unique(summary_data$group)
  ppp <- ppp[, c(2, 1, 3:6)]
  ppp <- ppp[, 6:1]

  error_ppp_y <- matrix(summary_data$sd, nrow=3, ncol=6)
  error_ppp_y <- error_ppp_y[, c(2, 1, 3:6)]
  error_ppp_y <- error_ppp_y[, 6:1]

  kl <- c(pi/6, pi/2, 5*pi/6, 7*pi/6, 9*pi/6, 11*pi/6)
  start <- 5*pi/6
  angle_offset_factor <- pi / 40
  rad_low_lim <- 1
  lwd <- 3
  color <- c("#b61024",'#f4a08d',"#73889c")

  radial.plot(ppp[1,], rp.type="p", radial.pos=kl - angle_offset_factor, label.pos=kl,
              start=start, labels=" ", radial.lim=c(rad_low_lim, 6),
              main="", line.col=color[1], lwd=lwd, radial.labels=NA)
  radius_for_labels <- c(7.1, 6.2, 6.2, 7.2, 6.1, 6.1) 
  text(x = cos(kl + start) * radius_for_labels,
       y = sin(kl + start) * radius_for_labels,
       labels = colnames(ppp),
       cex = 1.7, font = 1.8)
  
  max_radius <- 5
  angles <- seq(0, 2 * pi, length.out=200)
  lines(max_radius * cos(angles), max_radius * sin(angles), lwd=2)
  for (i in seq_along(angles)) {
    text_pos <-c(i-1,0)
    text(text_pos[2], text_pos[1], labels = c(1, 2, 3, 4, 5, NA)[i], col = "darkgray")
  }

  for (j in 2:3) {
    offset <- (j - 2) * angle_offset_factor 
    radial.plot(ppp[j,], rp.type="p", radial.pos=kl + offset, label.pos=kl,
                start=start, line.col=color[j], lwd=lwd, add=TRUE,
                radial.lim=c(rad_low_lim, 6), radial.labels=NA)
  }
  
  radius_offset <- c(0.68, 1.21, 1.0)
  angle_text_offset <- c(-0.15, 0, 0.15)
  for (j in 1:3) {
    for (i in 1:ncol(ppp)) {
      offset <- (j - 2) * angle_offset_factor
      lines(
        c(ppp[j,i] + error_ppp_y[j,i] - rad_low_lim, ppp[j,i] - error_ppp_y[j,i] - rad_low_lim) * cos(kl[i] + start + offset),
        c(ppp[j,i] + error_ppp_y[j,i] - rad_low_lim, ppp[j,i] - error_ppp_y[j,i] - rad_low_lim) * sin(kl[i] + start + offset),
        lwd=lwd, col=color[j])

      x_pos <- (ppp[j,i] - rad_low_lim) * cos(kl[i] + start + offset)
      y_pos <- (ppp[j,i] - rad_low_lim) * sin(kl[i] + start + offset)
      points(x_pos, y_pos, pch=21, col=color[j], bg="white", cex=1.5)

      text(
        (ppp[j,i]-rad_low_lim+radius_offset[j])*cos(kl[i]+start+offset*1.5+angle_text_offset[j]),
        (ppp[j,i]-rad_low_lim+radius_offset[j])*sin(kl[i]+start+offset*1.5+angle_text_offset[j]),
        labels=sprintf("%.2f", ppp[j,i]),
        cex=1.4, col="black"
      )
    }
  }

  legend(x=4.1, y=6.5, legend=rownames(ppp), col=color, pch=21, lty=1, pt.bg="white", cex=1.5, bty="n")
  par(xpd = TRUE)
}

pdf("c.pdf", width = 10, height = 6)
radar(dat_c)
dev.off()

# plot_d1
bar <- function(D, show_x_axis = FALSE){
  size = 0.5
  label <- c()
  title_text <- ""

  if(unique(D$variable) == "Usefulness in decision-making"){
    label <- c("Very\nunfavourable", "Unfavourable", "Neutral", "Favourable", "Very\nfavourable")
    title_text <- "Usefulness in decision-making"
  }
  if(unique(D$variable) == "Ease of communication"){
    label <- c("Very\nunfavourable", "Unfavourable", "Neutral", "Favourable", "Very\nfavourable")
    title_text <- "Ease of patient-physician communication"
  }
  if(unique(D$variable) == "Relief of workload"){
    label <- c("Very\nunfavourable", "Unfavourable", "Neutral", "Favourable", "Very\nfavourable")
    title_text <- "Relief of workload"
  }

  D$label <- label

  p <- ggplot(D, aes(x = as.factor(value), y = counts)) +
    geom_col(fill = "#FEE9D1", width = 0.8) +
    annotate("text", x = 0.5, y = 50, hjust = 0, label = title_text, size = 13) +
    labs(x = NULL, y = " ") +
    scale_x_discrete(labels = if (show_x_axis) label else NULL) +
    scale_y_continuous(limits = c(0, 60), breaks = c(0, 20, 40, 60), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = if (show_x_axis) element_text(size = 35, color = "black", vjust = 0.5) else element_blank(),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 36, color = "black"),
      axis.ticks.y = element_line(color = "black", linewidth = 0.5),
      axis.title.y = if (show_x_axis) element_text(size = 38, color = "black") else element_text(size = 36, color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      plot.margin = margin(t = 30, r = 0, b = 0, l = 0)
    ) +
    geom_segment(x = -Inf, xend = 5.5, y = -Inf, yend = -Inf,
                 color = "black", linewidth = size) +
    geom_segment(y = 0, yend = 60, x = -Inf, xend = -Inf,
                 color = "black", linewidth = size)

  return(p)
}

Usefulness <- dat_d1[dat_d1$variable == 'Usefulness in decision-making',]
Ease <- dat_d1[dat_d1$variable == 'Ease of communication',]
Relief <- dat_d1[dat_d1$variable == 'Relief of workload',]

final_plot <- plot_grid(
  bar(Usefulness),
  bar(Ease),
  bar(Relief, show_x_axis = TRUE),
  nrow = 3,
  align = "v",
  rel_heights = c(0.1, 0.1, 0.12)
)

y_axis_label <- ggdraw() +
  draw_label("No. of physicians",
             x = 0.85, y = 0.53,
             angle = 90,  size = 52)
p_d1 <- plot_grid(
  y_axis_label, final_plot,
  ncol = 2,
  rel_widths = c(0.1, 1)
)  

# save_d1
ggsave("d1.pdf", plot = p_d1, width = 17.02, height = 12.85)
rm(list = setdiff(ls(), c("dat_d2")))

# plot_d2
p <- ggplot(dat_d2, aes(x = as.factor(variable), y = proportions)) +
  geom_col(position = "dodge", fill = "#D6EAF8", width = 0.6) +
  scale_x_discrete(labels = c("Interpretation","Recording", "Across-discipline","Suggestion","Communication")) +
  labs(x = "", y = "Proportions(%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 28, color = "black",angle = 45, hjust = 1, vjust = 1),
    plot.title = element_markdown(hjust = 0.5, face = "bold"),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_text(size = 32, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.y = element_text(size = 32, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.line = element_line(color = "black", size = 0.5),
    plot.margin = margin(t = 30, r = 30, b = 10, l = 20)
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

# save_d2
ggsave("d2.pdf", plot = p, width = 6.03, height = 6.96)
rm(list = ls())

