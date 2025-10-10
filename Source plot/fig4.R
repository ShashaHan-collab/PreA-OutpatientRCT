library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)
library(grImport2)
library(grid)
library(readxl)

# load data
load("fig4.RData")

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
    p <- ggplot(df_prop, aes(x = Group, y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", width = if(show_y) 1.2 else 0.7, color = "black") +
      scale_fill_manual(values = custom_colors) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1.1), 
        expand = c(0, 0)
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
      labs(x = NULL, y = "Proportion", fill = "Category") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = if(show_y){
          element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
        }else{
          element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
        },
        axis.text.y = element_text(size = 27, color = "black"),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.title.y = if(show_y) element_text(size = 38, color = "black") else element_blank(),
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
legend_plot <- get_legend(p)
legend_only <- ggdraw(legend_plot)
svg("legenda.svg", width = 10.78, height = 1.01, bg = "transparent")
print(legend_only)
dev.off()

# save_a
svg("a1.svg", width = 10.78, height = 9.62, bg = "transparent")
print(sta_fun(dat_a_history, "History taking"))
dev.off()

svg("a2.svg", width = 10.78, height = 9.62, bg = "transparent")
print(sta_fun(dat_a_diag, "Diagnosis"))
dev.off()

svg("a3.svg", width = 10.78, height =9.62, bg = "transparent")
print(sta_fun(dat_a_test, "Test ordering"))
dev.off()
rm(list = setdiff(ls(), c("dat_b_history", "dat_b_diag", "dat_b_test")))

# plot_b function
variables_ordered <- c(
  "PreA-only",
  "PreA-human",
  "PreA-only or PreA-human"
)
right_vars <- variables_ordered[1:2]
left_vars <- "PreA-only or PreA-human"
custom_colors <- c("PreA referral reports" = "#ffe3eb", "Physician notes" = "#b0c2dc")

pl <- function(df, show_y, legend=FALSE){
  p <- ggplot(df, aes(x = Group, y = mean, fill = Catagory)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(
      aes(ymin = mean - sd, ymax = mean + sd),
      position = position_dodge(width = 0.7),
      width = 0,
      color = "black"
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(
      limits = c(0, 5.5),
      breaks = seq(0, 5, 1),
      expand = c(0, 0)
    ) +
    scale_x_discrete(
      labels = if (show_y) {
        c("PreA-only or PreA-human" = "PreA-assisted")
      } else {
        c(
          "PreA-only" = "PreA-only ",
          "PreA-human" = " PreA-human"
        )
      }
    ) +
    labs(x = NULL, y = "Likert scale of score") +
    theme_classic(base_size = 14) +
    theme(
      legend.position = if(legend && show_y) "top" else "none",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = if (legend && show_y) 30 else 0),
      panel.grid = element_blank(),
      axis.text.x = if(show_y){
        element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
      }else{
        element_text(size = 32, color = "black", hjust = 0.5, vjust = 0.5)
      },
      axis.text.y = element_text(size = 28, color = "black" ),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = if(show_y) element_text(size = 38, color = "black") else element_blank(),
      axis.ticks.length = unit(0.25, "cm"),
      axis.line = element_line(color = "black", size = 0.5),
      axis.line.y = element_blank(),
    )+annotate("segment",x =-Inf, xend = -Inf, y = 0, yend = 5,colour = "black",linewidth = 0.8)+
    annotate("segment",x = 0.5, xend = if(show_y) 4 else -Inf, y = 0, yend = 0,colour = "black",linewidth = 0.8)
}

# plot b
p1l <- pl(filter(dat_b_history, Group  %in% left_vars), show_y = TRUE)
p1r <- pl(filter(dat_b_history, Group  %in% right_vars), show_y = FALSE)
inset1 <- ggplotGrob(p1r)
p2l <- pl(filter(dat_b_diag, Group  %in% left_vars), show_y = TRUE)
p2r <- pl(filter(dat_b_diag, Group  %in% right_vars), show_y = FALSE)
inset2 <- ggplotGrob(p2r)
p3l <- pl(filter(dat_b_test, Group  %in% left_vars), show_y = TRUE)
p3r <- pl(filter(dat_b_test, Group  %in% right_vars), show_y = FALSE)
inset3 <- ggplotGrob(p3r)

left_top_label1 <- ggdraw() +
  draw_label("History taking",size = 38,x = 0.1, y = 0.95,hjust = 0.3, vjust = 3)
left_top_label2 <- ggdraw() +
  draw_label("Diagnosis",size = 38,x = 0.1, y = 0.95,hjust = 0.3, vjust = 3)
left_top_label3 <- ggdraw() +
  draw_label("Test ordering",size = 38,x = 0.1, y = 0.95,hjust = 0.3, vjust = 3)

p1_combined <- ggdraw(p1l) +
  draw_grob(inset1, x = 0.42, y = 0.16, width = 0.5, height = 0.7)
p2_combined <- ggdraw(p2l) +
  draw_grob(inset2, x = 0.42, y = 0.16, width = 0.5, height = 0.7)
p3_combined <- ggdraw(p3l) +
  draw_grob(inset3, x = 0.42, y = 0.16, width = 0.5, height = 0.7)

labeled_plot1 <- plot_grid(left_top_label1,p1_combined ,ncol = 1,rel_heights = c(0.2, 1))
labeled_plot2 <- plot_grid(left_top_label2,p2_combined,ncol = 1,rel_heights = c(0.2, 1))
labeled_plot3 <- plot_grid(left_top_label3,p3_combined,ncol = 1,rel_heights = c(0.2, 1))

legend_plot <- get_legend(pl(filter(dat_b_history, Group  %in% left_vars), show_y = TRUE, legend=TRUE))
legend_only <- ggdraw(legend_plot)

# save_b
svg("legendb.svg", width = 10.78, height = 1.01, bg = "transparent")
print(legend_only)
dev.off()

svg("b1.svg", width = 10.78, height = 8.62, bg = "transparent")
print(labeled_plot1)
dev.off()

svg("b2.svg", width = 10.78, height = 8.62, bg = "transparent")
print(labeled_plot2)
dev.off()

svg("b3.svg", width = 10.78, height = 8.62, bg = "transparent")
print(labeled_plot3)
dev.off()
rm(list = ls())

# combine to fig4
image_files <- c("a1.svg", "b1.svg", "a2.svg", "b2.svg", "a3.svg", "b3.svg")
pictures <- lapply(image_files, readPicture)
legend_pic1 <- readPicture("legenda.svg")
legend_pic2 <- readPicture("legendb.svg")

pdf("combined.pdf", width = 10, height = 12)
grid.newpage()

pushViewport(viewport(x = 0.29, y = 0.98, width = 0.3, height = 0.06, just = c("center", "top")))
grid.picture(legend_pic1)
popViewport()
pushViewport(viewport(x = 0.555, y = 0.98, width = 0.3, height = 0.06, just = c("center", "top")))
grid.picture(legend_pic2)
popViewport()

draw_picture_at <- function(pic, x, y, width = 0.38, height = 0.18) {
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = c("center", "center")))
  grid.picture(pic)
  popViewport()
}
draw_labeled_picture <- function(label1, pic1, label2, pic2, y, label_x = 0.165, label_y_offset = 0.085) {
  grid.text(label1, x = label_x, y = y + label_y_offset,
            gp = gpar(fontsize = 10, fontface = "bold"))
  draw_picture_at(pic1, x = 0.28, y = y)

  grid.text(label2, x = 0.42, y = y + label_y_offset,
            gp = gpar(fontsize = 10, fontface = "bold"))
  draw_picture_at(pic2, x = 0.55, y = y)
}

positions <- list(
  list(label1 = "a", pic1 = pictures[[1]], label2 = "b", pic2 = pictures[[2]], y = 0.87),
  list(label1 = "",  pic1 = pictures[[3]], label2 = "",  pic2 = pictures[[4]], y = 0.71),
  list(label1 = "",  pic1 = pictures[[5]], label2 = "",  pic2 = pictures[[6]], y = 0.55)
)
for (pos in positions) {
  draw_labeled_picture(pos$label1, pos$pic1, pos$label2, pos$pic2, y = pos$y)
}
dev.off()

input_pdf <- "combined.pdf"
output_pdf <- "fig4.pdf"
margins <- "10 10 10 10"
cmd <- sprintf('pdfcrop --margins "%s" %s %s', margins, input_pdf, output_pdf)
system(cmd)
rm(list = ls())


