library(dplyr)
library(ggplot2)
library(cowplot)
library(glue)
library(ggtext)

# load data
load("fig4.RData")

# color
color1<-c('#ecc4c2','#fbe6cc')
color2<-c('#9BA3CA','#D0DCE8')
color3<-c('#f8766d','#66d9dc')

rescale.x <- function(y, lab1, lab2, lab3, lab4) {
  x.new <- lab2 + (lab2 - lab1) + (y - lab3) / (lab4 - lab3) * (lab2 - lab1)
  return(x.new)
}

lendat <- lendat %>% 
  mutate(文本长度 = ifelse(文本长度 > 1800, rescale.x(文本长度, 1600, 1800, 2850, 3000), 文本长度))

timdat <- timdat %>% 
  mutate(录音时长 = ifelse(录音时长 > 14, rescale.x(录音时长, 13, 14, 20, 21), 录音时长))

# plot_a1
p_a1<-ggplot(dat_a, aes(x = mean_文本长度, y = 录音来源省, fill = area)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(xmin =  mean_文本长度 - 1.96*se_文本长度,
                    xmax = mean_文本长度 + 1.96*se_文本长度),
                width = 0.5,
                color = "black",
                position = position_dodge(width = 0.9)) +
  geom_jitter(
    data = lendat,
    aes(x = 文本长度, y = 录音来源省, color = area),
    width = 0, height = 0.05, size = 1.5,alpha = 0.6,show.legend = TRUE
  )+
  labs(title = "", x = "Chinese words", y = "") +
  theme_minimal() +
  scale_fill_manual(
    values = color1
  ) +
  scale_color_manual(values = color3)+
  scale_x_continuous(
    limits = c(0, 2200),
    breaks = seq(0, 2200, by = 200),
    labels = c(seq(0, 1800, by = 200), 2850, 3000)
  ) +
  theme(legend.position = "top",               
        legend.direction = "horizontal",      
        legend.justification = "center",       
        legend.text = element_text(size = 12),
        legend.title = element_blank(),        
        legend.box.margin = margin(0, 0, 0, 0), 
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.ticks.x.bottom = element_line(size = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        panel.border = element_blank())+
  geom_segment(x = 1870, xend = 1900, y = 0.2, yend = 0.6, color = "darkgray")+ 
  geom_segment(x = 1900, xend = 1930, y = 0.2, yend = 0.6, color = "darkgray")+
  geom_segment(aes(x = -Inf, xend = 1885, y = 0.4, yend = 0.4), color = "grey") +  
  geom_segment(aes(x = -Inf, xend = Inf, y = 11.6, yend = 11.6), color = "grey") + 
  geom_segment(aes(x = -Inf, xend = -Inf, y = 0.4, yend = 11.6), color = "grey") +  
  geom_segment(aes(x = Inf, xend = Inf, y = 0.4, yend = 11.6), color = "grey")+ 
  geom_segment(aes(x = 1915, xend = Inf, y = 0.4, yend = 0.4), color = "grey")+
  coord_cartesian(clip = "off")

# plot_a2
p_a2 <- ggplot(dat_a, aes(x = mean_时长, y = 录音来源省, fill = area)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(xmin =  mean_时长 - 1.96*se_时长,
                    xmax = mean_时长 + 1.96*se_时长),
                width = 0.5,
                color = "black",
                position = position_dodge(width = 0.9)) +
  geom_jitter(
    data = timdat,
    aes(x = 录音时长, y = 录音来源省, color = area),
    width = 0,height = 0.05,size = 1.5,alpha = 0.6
  ) +
  labs(title = "", x = "Consultation time", y = "") +
  theme_minimal() +
  scale_fill_manual(
    values = color1
  ) +
  scale_x_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, by = 1),
    labels = c(seq(0, 14, by = 1), 20, 21)
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
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        panel.border = element_blank())+
  geom_segment(x = 14.3, xend = 14.5, y = 0.2, yend = 0.6, color = "darkgray")+ 
  geom_segment(x = 14.5, xend = 14.7, y = 0.2, yend = 0.6, color = "darkgray")+
  geom_segment(aes(x = -Inf, xend = 14.4, y = 0.4, yend = 0.4), color = "grey") +  
  geom_segment(aes(x = -Inf, xend = Inf, y = 11.6, yend = 11.6), color = "grey") + 
  geom_segment(aes(x = -Inf, xend = -Inf, y = 0.4, yend = 11.6), color = "grey") +  
  geom_segment(aes(x = Inf, xend = Inf, y = 0.4, yend = 11.6), color = "grey")+ 
  geom_segment(aes(x = 14.6, xend = Inf, y = 0.4, yend = 0.4), color = "grey")+
  coord_cartesian(clip = "off")

p_a<-plot_grid(p_a1,p_a2,nrow = 2, rel_heights = c(1.2, 1.05))

# plot_b
bar<-function(D){
  size=1
  xlabel<-unique(D$Dimension)
  ggplot(D, aes(x = as.factor(Level), y = n, fill = Group)) +
    geom_col(position = "dodge") +
    labs(
      x = xlabel, y = "No. of participants", fill = "Group",
      title = ""
    ) +
    scale_fill_manual(values = color2) +
    theme_minimal() + 
    scale_y_continuous(limits = c(0,300)) +
    geom_segment(aes(x =1, xend = 5, y = -Inf, yend = -Inf), color = "darkgray",linewidth=size) +
    geom_segment(aes(y = 0, yend = 300, x = -Inf, xend = -Inf), color = "darkgray",linewidth=size)+
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_text(size = 12, color = "black", ,angle = 45, hjust = 1, vjust = 1),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = 12, color = "black"),
      axis.ticks.length = unit(0.25, "cm")
    )
}

test<-dat_b[dat_b$Dimension=='Test ordering',]
his<-dat_b[dat_b$Dimension=='History taking',]
dia<-dat_b[dat_b$Dimension=='Diagnosis',]



title_text <- glue("<span style='color:{color2[1]};'>Co-designed model</span> vs. ",
                   "<span style='color:{color2[2]};'>Data-tuned counterpart</span>")
title_plot <- ggplot() +
  geom_richtext(
    aes(x = 0.5, y = 0.5, label = title_text),inherit.aes = FALSE,
    hjust = 0.5,vjust = 0.5,fill = NA,      label.color = NA ) +theme_void() 


p_his<-bar(his)
p_test<-bar(test)
p_dia<-bar(dia)

p_b<-plot_grid(
  p_his, p_dia,p_test,
  ncol = 1, nrow = 3,
  labels = NULL
)

ggsave("legend_b.pdf", plot = title_plot, width = 4, height =0.5)
ggsave('a.pdf',p_a,width=7,height =8)
ggsave('b.pdf',p_b,width=4,height =8)

rm(list = ls())

