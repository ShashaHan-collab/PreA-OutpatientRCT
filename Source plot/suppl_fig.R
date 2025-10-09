library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggsignif)
library(grImport2)
library(grid)

load("suppl_fig.RData")

# plot function
round_half_up_str <- function(x, digits = 2) {
  x <- as.numeric(x)
  x <- round(x + 10^(-digits - 5), digits)
  formatC(x, format = "f", digits = digits)
}
create_patient_plot_split <- function(data, name, keshi, timu, legend=FALSE) {
  custom_colors <- c("#e2939c", "#fff0eb", "#bec8d3")
  data <- filter(data, !!sym(name) == !!keshi)
  
  variables_ordered <- c(
    "Ease",
    "Attentiveness",
    "Regard",
    "Satisfaction",
    "Acceptability",
    "Care coordination"
  )
  x_pos_mapping <- c(
    "Ease" = 1,
    "Attentiveness" = 2,
    "Regard" = 3,
    "Satisfaction" = 4,
    "Acceptability" = 5,
    "Care coordination" = 6.5
  )
  
  data_long <- pivot_longer(
    data,
    cols = all_of(variables_ordered),
    names_to = "variable",
    values_to = "value"
  ) %>%
    na.omit() %>%
    mutate(
      group = factor(group, levels = c("PreA-only", "PreA-human", "No-PreA")),
      variable = factor(variable, levels = variables_ordered)
    )
  
  data_summary <- data_long %>%
    group_by(variable, group) %>%
    summarise(mean = mean(value), sd = sd(value), .groups = "drop") %>%
    mutate(x_pos = x_pos_mapping[as.character(variable)])
  
  comparisons <- list(c("PreA-human", "PreA-only"),c("No-PreA", "PreA-only"))
  offset_mapping <- c("PreA-only" = -0.7 / 3,"PreA-human" = 0,"No-PreA" = 0.7 / 3)
  all_comparisons <- list()
  
  var_comparisons <- map_dfr(variables_ordered, function(var) {
    current_data <- data_long %>% filter(variable == var)
    
    map_dfr(comparisons, function(pair) {
      tibble(
        variable = var,
        group1 = pair[1],
        group2 = pair[2],
        data_subset = list(current_data %>% filter(group %in% pair) %>% droplevels())
      )
    })
  })
  
  time_comparisons <- map_dfr(comparisons, function(pair) {
    tibble(
      variable = "Service_time_min",
      group1 = pair[1],
      group2 = pair[2],
      data_subset = list(data %>% filter(group %in% pair))
    )
  })
  
  all_comparisons <- bind_rows(var_comparisons, time_comparisons)
  all_comparisons <- all_comparisons %>%
    mutate(
      p.value = map2_dbl(variable, data_subset, function(var, df) {
        if (var == "Service_time_min") {
          wilcox.test(Service_time_min ~ group, data = df)$p.value
        } else {
          wilcox.test(value ~ group, data = df)$p.value
        }
      })
    ) %>%
    select(-data_subset) %>%
    mutate(row_id = row_number()) %>%
    group_by(group_id = if_else(row_id %% 2 == 1, "odd", "even")) %>%
    mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
    ungroup() %>%
    select(-row_id, -group_id) %>%
    mutate(label = case_when(
      p.adj < 0.001 ~ "P < 0.001",
      p.adj >= 0.001 & p.adj < 0.01 ~ paste0("P = ", round_half_up_str(p.adj, 3)),
      p.adj >= 0.01 ~ paste0("P = ", round_half_up_str(p.adj, 2)),
      TRUE ~ NA_character_
    ))
  
  signif_data <- map_dfr(variables_ordered, function(var) {
    current_data <- data_long %>% filter(variable == var)
    
    map_dfr(comparisons, function(pair) {
      tibble(
        variable = var,
        group1 = pair[1],
        group2 = pair[2]
      )
    })
  }) %>%
    mutate(
      y_pos = c(4.54, 5.04)[rep(1:2, times = n() / 2)],
      base_x = x_pos_mapping[as.character(variable)],
      x_start = base_x + offset_mapping[group2],
      x_end = base_x + offset_mapping[group1]
    )
  
  signif_data <- signif_data %>%
    mutate(
      p.value = all_comparisons$p.adj[1:nrow(signif_data)],
      label = all_comparisons$label[1:nrow(signif_data)]
    )
  
  left_vars <- variables_ordered[1:5]
  right_vars <- "Care coordination"
  
  summary_data <- data %>%
    group_by(group) %>%
    summarise(
      median = median(Service_time_min),
      Q1 = quantile(Service_time_min, 0.25),
      Q3 = quantile(Service_time_min, 0.75),
      .groups = "drop"
    ) %>%
    mutate(
      IQR = Q3 - Q1,
      lower_limit = Q1 - 1.5 * IQR,
      upper_limit = Q3 + 1.5 * IQR
    ) %>%
    rowwise() %>%
    mutate(
      bottom = min(data$Service_time_min[data$group == group & data$Service_time_min >= lower_limit], na.rm = TRUE),
      top    = max(data$Service_time_min[data$group == group & data$Service_time_min <= upper_limit], na.rm = TRUE),
      lower = Q1,
      upper = Q3
    ) %>%
    ungroup() %>%
    mutate(group = factor(group, levels = c("PreA-only", "PreA-human","No-PreA")))
  
  signif_data_b <- tibble(
    group1 = all_comparisons$group1[(nrow(all_comparisons)-1):nrow(all_comparisons)],
    group2 = all_comparisons$group2[(nrow(all_comparisons)-1):nrow(all_comparisons)],
    p.value = all_comparisons$p.value[(nrow(all_comparisons)-1):nrow(all_comparisons)],
    label = all_comparisons$label[(nrow(all_comparisons)-1):nrow(all_comparisons)]
  )
  
  y_max <- max(summary_data$top)
  signif_data_b <- signif_data_b %>%
    mutate(
      y_pos = c(13.22, 14.60),
      x_start = match(group1, levels(summary_data$group)),
      x_end = match(group2, levels(summary_data$group))
    )
  levels(summary_data$group)
  
  plot_box <- ggplot(summary_data, aes(x = group, y = median)) +
    geom_errorbar(
      aes(ymin = bottom, ymax = top),
      width = 0.1,
      linewidth = 0.5,
      color = "black"
    ) +
    geom_boxplot(
      aes(
        lower = lower,
        upper = upper,
        middle = median,
        ymin = bottom,
        ymax = top,
        fill = group
      ),
      stat = "identity",
      width = 0.5, 
      linewidth = 0.5,
      outlier.shape = NA,
      position = position_dodge(width = 0.8)
    ) +
    scale_x_discrete(expand = c(0, 0.1),limits = c("PreA-only", "PreA-human","No-PreA") ) +
    geom_signif(
      data = signif_data_b,
      aes(xmin = x_start, xmax = x_end,
          annotations = label, y_position = y_pos),
      textsize = 6.7,
      tip_length = 0.01,
      manual = TRUE,
      inherit.aes = FALSE
    ) +
    labs(
      x = NULL,
      y = "Consultation duration (mins)") +
    scale_fill_manual(values = c(
      "PreA-only" = {custom_colors[1]},
      "PreA-human" = {custom_colors[2]},
      "No-PreA" = {custom_colors[3]}
    )) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 28,  color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_text(size = 36, color = "black"),
      axis.title.y = element_text(size = 36, color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
    ) +
    scale_y_continuous(limits = c(0, 16),breaks = seq(0, 15, 5),expand = c(0, 0)) +
    geom_segment(aes(x = 0.4, xend = 3.5, y = -Inf, yend = -Inf),color = "black",linewidth = 0.5) +
    geom_segment(aes(y = 0, yend = 16, x = -Inf, xend = -Inf),color = "black",linewidth = 0.5)
  
  plot_base <- function(sub_data, sub_signif, show_y = TRUE) {
    p <- ggplot(sub_data, aes(x = x_pos, y = mean, fill = group)) +
      geom_col(position = position_dodge(0.6), width = 0.7) +
      geom_errorbar(
        aes(ymin = mean - sd, ymax = mean + sd),
        width = 0, position = position_dodge(0.6), linewidth = 0.8
      ) +
      labs(x = NULL, y = if (show_y) "Likert scale of score" else NULL) +
      scale_fill_manual(values = custom_colors) +
      scale_y_continuous(limits = c(0, 6.5),breaks = seq(0, 5, 1),expand = c(0, 0)) +
      scale_x_continuous(
        breaks = sub_data$x_pos,
        labels = as.character(sub_data$variable),
        limits = if (show_y) c(0.4, 5.6) else c(5.9, 7.1),
        expand = c(0, 0)
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = if (legend && show_y) c(0.5, 0.5) else "none",
        legend.direction = if (legend && show_y) "horizontal" else "vertical",
        legend.text = element_text(size = if (legend && show_y) 21 else 0),
        legend.key.size = unit(2, "lines"),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 36, color = gray(0.15), angle = 45, vjust = 1, hjust = 1, face = "plain"),
        axis.text.y = element_text(size = 28, color = "black"),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        axis.title.y = element_text(size = 36, color = "black"),
        axis.title.x = element_text(size = 36, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.border = element_blank()
      )
    if (nrow(sub_signif) > 0) {
      p <- p + geom_signif(
        data = sub_signif,
        aes(
          xmin = x_start,
          xmax = x_end,
          y_position = y_pos + 0.7,
          annotations = label,
          group = interaction(x_start, x_end)
        ),
        tip_length = 0.01,
        textsize = 7.5,
        manual = TRUE,
        inherit.aes = FALSE
      )
    }
    return(p)
  }
  
  plot_left <- plot_base(filter(data_summary, variable %in% left_vars),
                         filter(signif_data, variable %in% left_vars),
                         show_y = TRUE)
  plot_right <- plot_base(filter(data_summary, variable %in% right_vars),
                          filter(signif_data, variable %in% right_vars),
                          show_y = FALSE)
  
  left_bottom_label <- ggdraw() +
    draw_label(
      timu,                   
      size = 36,
      x = 0.18, y = 0.8,
      hjust = 0, vjust = 0
    )
  
  combined_plot <- plot_grid(
    plot_box, plot_left, plot_right,
    ncol = 3,
    rel_widths = c(1.2, 3.8, 1),
    align = "h",
    axis = "tb"
  )
  
  labeled_plot <- plot_grid(
    combined_plot,
    left_bottom_label,
    ncol = 1,
    rel_heights = c(1, 0.06)  
  )
  
  if (legend){
    legend_plot <- get_legend(plot_left)
    legend_only <- ggdraw(legend_plot)
    return(legend_only)
  } else {
    return (labeled_plot)
  }
}

# save sub_plot
svg("legend.svg", width = 9.53, height =1.40 )
p <- create_patient_plot_split(dat_suppl_fig,"worktype", "正在工作", 'Employed', legend=TRUE)
print(p)
dev.off()

plot_tasks <- list(
  list(file = "two_l.svg",   var = "age",          value = "l",      label = "Age<50"),
  list(file = "two_h.svg",   var = "age",          value = "h",      label = "Age≥50"),
  list(file = "two_ma.svg",  var = "sex",          value = "男性",     label = "Male"),
  list(file = "two_fe.svg",  var = "sex",          value = "女性",     label = "Female"),
  list(file = "two_pa.svg",  var = "patientself",  value = "患者本人", label = "Patients"),
  list(file = "two_ca.svg",  var = "patientself",  value = "患者家属", label = "Care partners"),
  list(file = "two_gui.svg", var = "hospital",     value = "桂林医院", label = "Guilin"),
  list(file = "two_gan.svg", var = "hospital",     value = "甘肃医院", label = "Gansu"),
  list(file = "three_em.svg",var = "worktype",     value = "正在工作", label = "Employed"),
  list(file = "three_un.svg",var = "worktype",     value = "待业",     label = "Unemployed"),
  list(file = "three_re.svg",var = "worktype",     value = "退休",     label = "Retired"),
  list(file = "three_co.svg",var = "edu",          value = "大学本科及以上", label = "College or above"),
  list(file = "three_hi.svg",var = "edu",          value = "中学",     label = "High school"),
  list(file = "three_pr.svg",var = "edu",          value = "小学及以下", label = "Primary school or below"),
  list(file = "three_h.svg", var = "income",       value = "大于5000元", label = ">5000 RMB/month"),
  list(file = "three_l.svg", var = "income",       value = "小于2000元", label = "≤2000 RMB/month"),
  list(file = "three_m.svg", var = "income",       value = "2000-5000元", label = "(2000,5000] RMB/month"),
  list(file = "four_nei.svg",var = "department",   value = "内科", label = "Medical"),
  list(file = "four_sur.svg",var = "department",   value = "外科", label = "Surgical"),
  list(file = "four_pe.svg", var = "department",   value = "儿科", label = "Pediatric"),
  list(file = "four.svg",    var = "department",   value = "其他科室", label = "Med-Surg")
)

for (task in plot_tasks) {
  svg(task$file, width = 13.41, height = 9.06, bg = "transparent")
  p <- create_patient_plot_split(dat_suppl_fig, task$var, task$value, task$label)
  print(p)
  dev.off()
}
rm(list = ls())

# combine to suppl_fig
# fig1,3,4
pdf_4_sub <- function(image_files, output_file){
  legend_pic <- readPicture("legend.svg")
  pictures <- lapply(image_files, readPicture)
  pdf("combined.pdf", width = 10, height = 12)
  grid.newpage()
  
  pushViewport(viewport(x = 0.335, y = 0.94, width = 0.3, height = 0.06, just = c("center", "top")))
  grid.picture(legend_pic)
  popViewport()
  
  draw_picture_at <- function(pic, x, y, width = 0.38, height = 0.18) {
    pushViewport(viewport(x = x, y = y, width = width, height = height, just = c("center", "center")))
    grid.picture(pic)
    popViewport()
  }
  
  draw_labeled_picture <- function(label, pic1, pic2, y, label_x = 0.18, label_y_offset = 0.09) {
    grid.text(label, x = label_x, y = y + label_y_offset, 
              gp = gpar(fontsize = 14, fontface = "bold"))
    draw_picture_at(pic1, x = 0.33, y = y)
    draw_picture_at(pic2, x = 0.69, y = y)
  }
  
  positions <- list(
    list(pic1 = pictures[[1]], pic2 = pictures[[2]], y = 0.82),
    list(pic1 = pictures[[3]], pic2 = pictures[[4]], y = 0.65)
  )
  
  for (pos in positions) {
    draw_labeled_picture(pos$label, pos$pic1, pos$pic2, y = pos$y)
  }
  
  dev.off()
  
  input_pdf <- "combined.pdf"
  output_pdf <- output_file
  margins <- "10 10 10 10"
  cmd <- sprintf('pdfcrop --margins "%s" %s %s', margins, input_pdf, output_pdf)
  system(cmd)
}

pdf_4_sub(c("two_l.svg", "two_h.svg", "two_ma.svg", "two_fe.svg"), "suppl_fig1.pdf")
pdf_4_sub(c("four_nei.svg", "four_sur.svg", "four.svg", "four_pe.svg"), "suppl_fig3.pdf")
pdf_4_sub(c("two_pa.svg", "two_ca.svg", "two_gui.svg", "two_gan.svg"), "suppl_fig4.pdf")
rm(list = ls())

# fig2
image_files <- c("three_pr.svg", "three_hi.svg", "three_co.svg", "three_em.svg", "three_un.svg", "three_re.svg", "three_l.svg", "three_m.svg", "three_h.svg")
legend_pic <- readPicture("legend.svg")
pictures <- lapply(image_files, readPicture)
pdf("combined.pdf", width = 18, height = 21.6)
grid.newpage()
pushViewport(viewport(x = 0.2, y = 0.94, width = 0.3, height = 0.06, just = c("center", "top")))
grid.picture(legend_pic)
popViewport()

draw_picture_at <- function(pic, x, y, width = 0.38, height = 0.18) {
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = c("center", "center")))
  grid.picture(pic)
  popViewport()
}

draw_labeled_picture <- function(label, pic1, pic2, pic3, y, label_x = 0.03, label_y_offset) {
  grid.text(label, x = label_x, y = y + label_y_offset, 
            gp = gpar(fontsize = 24.5, fontface = "bold"))
  draw_picture_at(pic1, x = 0.18, y = y)
  draw_picture_at(pic2, x = 0.5, y = y)
  draw_picture_at(pic3, x = 0.82, y = y)
}

positions <- list(
  list(label = "a", pic1 = pictures[[1]], pic2 = pictures[[2]], pic3 = pictures[[3]], y = 0.82, label_y_offset = 0.09),
  list(label = "b", pic1 = pictures[[4]], pic2 = pictures[[5]], pic3 = pictures[[6]], y = 0.65, label_y_offset = 0.09),
  list(label = "c", pic1 = pictures[[7]], pic2 = pictures[[8]], pic3 = pictures[[9]], y = 0.48, label_y_offset = 0.09)
)

for (pos in positions) {
  draw_labeled_picture(pos$label, pos$pic1, pos$pic2, pos$pic3, y = pos$y, label_y_offset = pos$label_y_offset)
}

dev.off()
input_pdf <- "combined.pdf"
output_pdf <- "suppl_fig2.pdf"
margins <- "10 10 10 10"
cmd <- sprintf('pdfcrop --margins "%s" %s %s', margins, input_pdf, output_pdf)
system(cmd)
rm(list = ls())