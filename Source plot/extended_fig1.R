library(dplyr)
library(ggplot2)
library(cowplot)

# load data
load("extended_fig1.RData")

# color
color1<-c('#ecc4c2','#fbe6cc')
color2<-c('#9BA3CA','#D0DCE8')

# data_a
result_a <- dat_extended_fig1_a %>%
  group_by(录音来源省) %>%
  summarise(
    东西部 = first(东西部),
    mean_文本长度   = round(mean(文本长度, na.rm = TRUE), 3),
    median_文本长度 = round(median(文本长度, na.rm = TRUE), 3),
    sd_文本长度 = round(sd(文本长度, na.rm = TRUE), 2),    
    se_文本长度 = round(sd(文本长度, na.rm = TRUE) / sqrt(n()), 3),
    Q1_文本长度 = round(quantile(文本长度, probs = 0.25, na.rm = TRUE), 3), 
    Q3_文本长度 = round(quantile(文本长度, probs = 0.75, na.rm = TRUE), 3), 
    mean_时长   = round(mean(录音时长, na.rm = TRUE), 3),
    median_时长 = round(median(录音时长, na.rm = TRUE), 3),
    sd_时长 = round(sd(录音时长, na.rm = TRUE), 2),          
    se_时长 = round(sd(录音时长, na.rm = TRUE) / sqrt(n()), 3),
    Q1_时长 = round(quantile(录音时长, probs = 0.25, na.rm = TRUE), 3),     
    Q3_时长 = round(quantile(录音时长, probs = 0.75, na.rm = TRUE), 3),     
    农村比例 = round(sum(城市农村 == "农村") / n(), 4),
    西部比例 = round(sum(东西部 == "西部") / n(), 4),
    sd_文本长度 = round(sd(文本长度, na.rm = TRUE), 2),      
    sd_时长 = round(sd(录音时长, na.rm = TRUE), 2),     
    count = n(),
    .groups = "drop"
  )
target_order <- c("北京", "湖北","山东", "重庆", "甘肃", "广西", "内蒙古", "陕西",  "山西", "四川", "新疆")
result_a <- result_a %>% arrange(match(录音来源省, target_order))
result_a$录音来源省 <- c("Beijing","Hubei","Shandong", "Chongqing", "Gansu", "Guangxi","Inner Mongolia","Shaanxi", "Shanxi",  "Sichuan", "Xinjiang")
result_a$area <- c("High-income","Low-income","High-income", "Low-income", "Low-income","Low-income", "Low-income", "Low-income", "Low-income", "Low-income", "Low-income")
result_a$area <- factor(result_a$area, levels = c("High-income", "Low-income"))
result_a$录音来源省<- factor(result_a$录音来源省, levels = rev(c("Beijing","Shandong",  "Chongqing", "Gansu", "Guangxi", "Hubei", "Inner Mongolia", "Shaanxi", "Shanxi", "Sichuan", "Xinjiang")))

# plot_a1
p_a1<-ggplot(result_a, aes(x = mean_文本长度, y = 录音来源省, fill = area)) +
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
p_a2 <- ggplot(result_a, aes(x = mean_时长, y = 录音来源省, fill = area)) +
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


# data_b
mean_values <- apply(dat_extended_fig1_b, 2, mean, na.rm = TRUE)
sd_values <- apply(dat_extended_fig1_b, 2, sd, na.rm = TRUE)
result_b <- data.frame(
  index = names(mean_values),
  mean = unname(mean_values),
  sd = unname(sd_values)
)
result_b$group <- ifelse(grepl("1", result_b$index), "Data-tuned counterpart", "Co-designed model")
result_b$name <- rep(c('Diagnosis','History taking','Test ordering'),each=2)
result_b$name <- factor(result_b$name,levels = c('History taking','Diagnosis','Test ordering'))

# plot_b
p_b<-ggplot(result_b, aes(x = name, y = mean, fill = group)) +
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
