dbs_today %>%
  group_by(
    valuationdate,  CCY, assettype
  ) %>%
  summarise(
    UsedMarketCap = sum(UsedMarketCap, na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = c(CCY), values_from = c(UsedMarketCap))




library(ggplot2)
library(ggthemes)
library(ggrepel)
library(grid)


library(sparkline)
sparkline(c(1,2,7,6,5), type = "bar", barColor = "green")

perc_labs <- c("Recon \nItems", "Unrealised \nPositions")

confed_labs <- ur_dbs_today$obelixdbname

p <- 
  ggplot(dbs_today,
         aes(type, obelixdbname, 
             fill = cases)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient(low = "#e5f5e0", high = "#008B00") + #  mid = "#a1d99b"
  geom_text(aes(label = cases, fontface = "bold"), size = 5) +
  theme_fivethirtyeight() +                   # erases axis.title, text font not the actual one?
  theme(legend.position = "none",
        text = element_text(face = "bold", color = "black"),
        axis.title = element_text(),          # to add axis.title back into plot...
        axis.title.x.top = element_text(margin = margin(b = 10)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 0, size = 12),
        plot.margin = rep(grid::unit(0.75, "cm"), 4)
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0),
                   labels = perc_labs) +
  labs(x = "Daily Summary ...", y = "") + 
  scale_y_discrete(expand = c(0, 0),
                   labels = confed_labs) 
# +
#   annotation_custom(grob = textGrob(
#     label = expression(bold("Obelix DB")), # use bold() inside expression() to get BOLD text
#     gp = gpar(cex = 1.0, fontface = "bold", hjust = 0)), # also need to specify in fontface as well
#     ymin = 6.66, ymax = 6.66,
#     xmin = -0.23, xmax = -0.23) +     
#   annotation_custom(grob = linesGrob(), 
#                     xmin = 0.5, xmax = 4.5, ymin = 7, ymax = 7) + # top bar
#   annotation_custom(grob = linesGrob(gp = gpar(lwd = 3)),  
#                     xmin = -0.64, xmax = 4.5, ymin = 6.5, ymax = 6.5) # x-axis bar

pt <- ggplot_gtable(ggplot_build(p))
pt$layout$clip[pt$layout$name == "panel"] <- "off"  # so stuff outside the plot can be shown.

grid.newpage()
grid.draw(pt) # doesn't show anything...?

plot(pt) # use plot instead i guess...
