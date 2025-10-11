library(dplyr)
library(ggplot2)
library(cowplot)

# load data
load("extended_fig1.RData")

# color
color1<-c('#ecc4c2','#fbe6cc')
color2<-c('#9BA3CA','#D0DCE8')

# plot_a1
p_a1<-ggplot(dat_extended_fig1_a, aes(x = mean_文本长度, y = 录音来源省, fill = area)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(xmin =  mean_文本长度 - 1.96*se_文本长度,
                    xmax = mean_文本长度 + 1.96*se_文本长度),
                width = 0.2,
                color = "gray40",
                position = position_dodge(width = 0.9)) +
  labs(title = "", x = "Chinese words", y = "") +
  theme_minimal() +
  scale_fill_manual(
    values = color1
  ) +
  scale_x_continuous(
    limits = c(0, 1400),
    breaks = seq(0, 1400, by = 200)
  ) +
  theme(legend.text = element_text(size = 12),
        legend.position = c(0.76, 0.76),
        legend.box.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.ticks.x.bottom = element_line(size = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  guides(fill = guide_legend(title = NULL, reverse = F))

# plot_a2
p_a2 <- ggplot(dat_extended_fig1_a, aes(x = mean_时长, y = 录音来源省, fill = area)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(xmin =  mean_时长 - 1.96*se_时长,
                    xmax = mean_时长 + 1.96*se_时长),
                width = 0.2,
                color = "gray40",
                position = position_dodge(width = 0.9)) +
  labs(title = "", x = "Consultation time", y = "") +
  theme_minimal() +
  scale_fill_manual(
    values = color1
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
  ) +
  theme(legend.text = element_text(size = 12),
        legend.position = 'none',
        legend.box.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.ticks.x.bottom = element_line(size = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  guides(fill = guide_legend(title = NULL, reverse = F))

# plot_b
p_b<-ggplot(dat_extended_fig1_b, aes(x = name, y = mean, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd),
                width = 0,
                color = "gray40",
                position = position_dodge(width = 0.9)) +
  labs(title = "", x = "", y = "Likert scale of score") +
  theme_minimal() +
  scale_fill_manual(
    values = color2
  ) +
  scale_y_continuous(
    limits = c(0, 6),
    breaks = seq(0,5, by =1)
  ) +
  theme(
    legend.position = c(0.675, 0.925),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size =13),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.ticks.x.bottom = element_line(size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  guides(fill = guide_legend(title = NULL, reverse = F))

# combine to extended fig1
p_a<-plot_grid(p_a1,p_a2,nrow = 2)
p<-plot_grid(p_a,p_b,nrow=1,labels = c('auto'))
ggsave('extended data_fig1.pdf',p,width=9,height =6)
rm(list = ls())
