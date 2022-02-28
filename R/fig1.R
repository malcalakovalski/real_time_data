library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)

librarian::shelf(ggalt, cowplot)

#Create data to plot
timeline <- tribble( ~start_date, ~event, ~displ, ~Category,
                 ymd(20200121), "First reported Covid case in U.S. (1/21)", 1.3, "News",
                 ymd(20200123), "China lockdown (1/23)",1.2,"News",
                 ymd(20200222), "Italy lockdown (2/22)", 1.1, "News",
                 ymd(20200303), "Fed emergency rate cut by 1/2 ppt (3/3)", -1, "Monetary policy actions",
                 ymd(20200313), "President declares national emergency (3/13)",-0.9,"Fiscal policy actions",
                 ymd(20200315), "Fed emergency rate cut to 0 pct (3/15)", -0.8, "Monetary policy actions",
                 ymd(20200317), "First announcement of new Fed facilities (3/17)", -0.7, "Monetary policy actions",
                 ymd(20200319), "First state-wide lockdown order in U.S. (3/19)", 1, "News",
                 ymd(20200319), "Opening discussion of CARES act (3/19)", -0.6, "Fiscal policy actions",
                 ymd(20200326), "Initial UI claims data (3/26)", 0.9, "Data releases",
                 ymd(20200327), "CARES act passage (3/27)", -0.5, "Fiscal policy actions",
                 ymd(20200409), "Last announcement of new Fed facilities (4/9)", -0.4, "Monetary policy actions",
                 ymd(20200413), "First stimulus checks/UI go out (4/13)", -0.3, "Fiscal policy actions",
                 ymd(20200415), "March retail sales report (4/15)", 0.8, "Data releases",
                 ymd(20200508), "April employment situation (5/8)",0.7, "Data releases",
                 ymd(20200515), "April retail sales report (5/15)", 0.6, "Data releases",
                 ymd(20200528), "U.S. death toll surpasses 100,000 (5/28)",0.5,"News",
                 ymd(20200616), "May retail sales report (6/16)", 0.4, "Data releases",
                 ymd(20200730), "First read of GDP in Q2 (7/30)", 0.3, "Data releases"
)

# Order for the legend
category_levels = c("Monetary policy actions", "Fiscal policy actions", "Data releases", "News")
timeline %>%
  mutate(Category = factor(Category, levels = category_levels))



#Conditionally set whether text will be above or below the point
vjust <- ifelse(timeline$displ > 0, -1, 1.8)

#plot
p1 <-
  timeline %>%
  ggplot(aes(start_date, displ)) +
  geom_segment(aes(x = start_date, xend = start_date, y = 0, yend = displ, color = Category),
               size = .5, linetype = "solid") +
  geom_point(aes(color = Category)) +
  geom_text(aes(x = start_date, y = displ, label = event), data = data,
            hjust = 0, vjust = vjust, size = 2.5) +
  scale_color_manual(values = c("#F8766D","#00BA38","#619CFF", "#C77CFF")) +
  theme(rect = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 8,color = "#000000"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=9),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.7,face="bold")) +
  expand_limits(x = c(ymd(20191229), ymd(20200910)), y = 1.2) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9))  +
  ggtitle("Early Policy Responses to Covid-19 (2020)")

#and run the function from above
shift_axis(p1, ymd(20191229), ymd(20200910))
timeline

# Save manually using the export to pdf function in the plots panel in Rstudio (portrait mode) to get nicely formatted pdf (instead of using the following)
#ggsave(file = "brookings_timeline.pdf", timeline, cairo_pdf, height = 3, width = 3.3)
