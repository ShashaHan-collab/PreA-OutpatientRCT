library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)
library(grImport2)
library(grid)
library(ggtext)

# load data
load("fig3.RData")

# plot_a function
sta_fun <- function(df_prop, subtitle, legend=FALSE){

  variables_ordered <- c(
    "PreA-only",
    "PreA-human",
    "PreA-only or PreA-human"
  )
  right_vars <- variables_ordered[1:2]
  left_vars <- "PreA-only or PreA-human"

  df_prop$Category <- factor(df_prop$Category, levels = c("Disagreement", "Agreement", "Blank"))
  df_prop$Group <- factor(df_prop$Group , levels = c("PreA-only or PreA-human", "PreA-only", "PreA-human"))

  custom_colors <- c(
    "Agreement" = "#fff5d7",
    "Blank" = "#eaeae9",
    "Disagreement" = "#daecf9"
  )

  pl <- function(df_prop, show_y){
    df_prop <- df_prop %>%
      mutate(Label = sprintf("%.1f", Proportion * 100))
    p <- ggplot(df_prop, aes(x = Group, y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", width = if(show_y) 1.2 else 0.7, color = "black") +
      scale_fill_manual(values = custom_colors) +
      scale_y_continuous(
        limits = c(0, 1.1),
        expand = c(0, 0),
        labels = function(x) x * 100
      ) +
      scale_x_discrete(
        labels = if (show_y) {
          c("PreA-only or PreA-human" = "PreA-assisted")
        } else {
          c(
            "PreA-only" = "PreA-only",
            "PreA-human" = "PreA-human"
          )
        }
      ) +
      labs(x = NULL, y = "Proportion(%)", fill = "Category") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = if(show_y){
          element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
        }else{
          element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
        },
        axis.text.y = element_text(size = 32, color = "black"),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.title.y = if(show_y) element_text(size = 38, color = "black") else element_text(size = 36, color = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.line = element_line(color = "black", size = 0.5),
        axis.line.y = element_blank(),
        legend.position = if (legend) "top" else "none",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size = if (legend) 30 else 0)
      )+annotate("segment",x =-Inf, xend = -Inf, y = 0, yend = 1,colour = "black",linewidth = 0.8)+
      annotate("segment",x = if(show_y) 0.1 else -Inf, xend = if(show_y) 6 else -Inf, y = 0, yend = 0,colour = "black",linewidth = 0.8)

    inner_labels <- df_prop %>% filter(Category != "Disagreement")

    p <- p + geom_text(
      data = inner_labels,
      aes(label = Label),
      position = position_stack(vjust = 0.5),
      size = 11,
      color = "black"
    )
    top_annot <- df_prop %>%
      group_by(Group) %>%
      mutate(y_top = sum(Proportion)) %>%
      filter(Category == "Disagreement")

    p <- p + geom_text(
      data = top_annot,
      aes(x = Group, y = y_top + 0.06, label = Label),
      size = 11,
      color = "black"
    )

    return(p)
  }

  p1l <- pl(filter(df_prop, Group  %in% left_vars), show_y = TRUE)
  p1r <- pl(filter(df_prop, Group  %in% right_vars), show_y = FALSE)
  inset1 <- ggplotGrob(p1r)
  left_top_label <- ggdraw() + draw_label(subtitle,size = 40,x = 0.1, y = 0.95,hjust = 0.3, vjust = 3)
  p1_combined <- ggdraw(p1l) + draw_grob(inset1, x = 0.38, y = 0.16, width = 0.6, height = 0.7)
  p <- plot_grid(left_top_label,p1_combined,ncol = 1,rel_heights = c(0.2, 1))
  return(if (legend) p1l else p)

}

# legend_a
p <- sta_fun(dat_a_history, "History taking", legend = TRUE)
legend_all <- get_plot_component(p, "guide-box", return_all = TRUE)
legend_only <- ggdraw(legend_all[[4]])

# save_a
ggsave("legenda.pdf", plot = legend_only, width = 10.78, height = 1.01)
ggsave("a1.pdf", plot = sta_fun(dat_a_history, "History taking"), width = 11.78, height = 9.62)
ggsave("a2.pdf", plot = sta_fun(dat_a_diag, "Diagnosis"), width = 11.78, height = 9.62)
ggsave("a3.pdf", plot = sta_fun(dat_a_test, "Test ordering"), width = 11.78, height = 9.62)
rm(list = setdiff(ls(), c("dat_b")))

# plot_b function
bar<-function(D, all = FALSE, legend = FALSE){
  size=1
  xlabel<-D$group
  x_title <- if(all) "Likert scale of score" else ""
  y_limit <- if (all) {500} else {300}
  ggplot(D, aes(x = as.factor(Level), y = n, fill = Title)) +
    geom_col(position = "dodge") +
    labs(
      x = x_title, y = "", fill = "Title",
      title = xlabel
    ) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(legend.position = "none",panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )+
    scale_y_continuous(limits = c(0,y_limit)) +
    geom_segment(aes(x =1, xend = 5, y = -Inf, yend = -Inf), color = "darkgray",linewidth=size) +
    geom_segment(aes(y = 0, yend = y_limit, x = -Inf, xend = -Inf), color = "darkgray",linewidth=size)+
    theme(
      legend.position = if(legend) "top" else "none",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = if (legend) 30 else 0),
      plot.title = element_text(size = 30, hjust = 0.5),
      axis.text.x = element_text(size = 30, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"),
      axis.title.x = element_text(size = 28, color = "black"),
      axis.text.y = element_text(size = 30, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"),
      axis.title.y = element_text(size = 36, color = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      plot.margin = if(all) unit(c(1, 0.5, 1, 0), "cm") else unit(c(1, 0.5, 1, -2), "cm")
    )+
    coord_flip(clip = "off")
}

custom_colors <- c("#b0c2dc","#ffe3eb")
test<-dat_b[dat_b$Dimension=='Test ordering',]
test_ass <- test[test$group=='PreA-assisted',]
test_on <- test[test$group=='PreA-only',]
test_hum <- test[test$group=='PreA-human',]
his<-dat_b[dat_b$Dimension=='History taking',]
his_ass <- his[his$group=='PreA-assisted',]
his_on <- his[his$group=='PreA-only',]
his_hum <- his[his$group=='PreA-human',]
dia<-dat_b[dat_b$Dimension=='Diagnosis',]
dia_ass <- dia[dia$group=='PreA-assisted',]
dia_on <- dia[dia$group=='PreA-only',]
dia_hum <- dia[dia$group=='PreA-human',]

p_h_legend<-bar(his_ass, all = TRUE, legend = TRUE)
legend_all <- get_plot_component(p_h_legend, "guide-box", return_all = TRUE)
legend_only <- ggdraw(legend_all[[4]])
ggsave("legendb.pdf", plot = legend_only, width = 10.78, height = 1.01)

top_blank <- ggplot() + theme_void()
p_h1<-bar(his_ass, all = TRUE)
p_h2<-bar(his_on)
p_h3<-bar(his_hum)
p_h123 <- plot_grid(p_h1, p_h2, p_h3, ncol=3, rel_widths = c(0.9, 0.6, 0.6))
p_h <- ggdraw() +
  draw_plot(plot_grid(top_blank, p_h123, nrow = 2, rel_heights = c(0.08, 0.8)),
            x = 0.03, y = 0, width = 0.97, height = 1)+
  draw_label("History taking", x = 0, y = 0.98, hjust = 0, vjust = 1, size = 36)+
  draw_label("No. of participants", x = 0.35, y = 0.07, hjust = 0, vjust = 1, size = 36)
ggsave("b1.pdf", plot = p_h, width = 13.78, height = 7.82)

p_dia1<-bar(dia_ass, all = TRUE)
p_dia2<-bar(dia_on)
p_dia3<-bar(dia_hum)
p_dia123 <- plot_grid(p_dia1, p_dia2, p_dia3, ncol=3, rel_widths = c(0.9, 0.6, 0.6))
p_dia <- ggdraw() +
  draw_plot(plot_grid(top_blank, p_dia123, nrow = 2, rel_heights = c(0.08, 0.8)),
            x = 0.03, y = 0, width = 0.97, height = 1)+
  draw_label("Diagnosis", x = 0, y = 0.98, hjust = 0, vjust = 1, size = 36)+
  draw_label("No. of participants", x = 0.35, y = 0.07, hjust = 0, vjust = 1, size = 36)
ggsave("b2.pdf", plot = p_dia, width = 13.78, height = 7.82)

p_test1<-bar(test_ass, all = TRUE)
p_test2<-bar(test_on)
p_test3<-bar(test_hum)
p_test123 <- plot_grid(p_test1, p_test2, p_test3, ncol=3, rel_widths = c(0.9, 0.6, 0.6))
p_test <- ggdraw() +
  draw_plot(plot_grid(top_blank, p_test123, nrow = 2, rel_heights = c(0.08, 0.8)),
            x = 0.03, y = 0, width = 0.97, height = 1)+
  draw_label("Test ordering", x = 0, y = 0.98, hjust = 0, vjust = 1, size = 36)+
  draw_label("No. of participants", x = 0.35, y = 0.07, hjust = 0, vjust = 1, size = 36)

ggsave("b3.pdf", plot = p_test, width = 13.78, height = 7.82)
