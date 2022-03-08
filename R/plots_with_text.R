

# Setup ---------------------------------------------------------------------------------------
install.packages("librarian")
librarian::shelf(tidyverse, openxlsx, readxl, purrr, janitor, ggbrookings, lubridate, patchwork, magrittr, magick, ggalt, cowplot,ggpubr, ggtext, glue, ggplot2)
setwd("C:/Users/18145/OneDrive/Desktop/THP/Intro/generaleconomic/generalecon/real_time_data/")
source('R/utils.R')

theme_set(theme_thp(base_size = 12))
theme_update(
             panel.grid = ggplot2::element_line(
               colour = "#CCCCCC",
               size = 0.25,
               linetype = "solid"
             ),
             legend.position = 'bottom',
             legend.direction = 'horizontal',
             axis.line = ggplot2::element_line(),
             axis.line.x = ggplot2::element_line(),
             axis.line.y = ggplot2::element_blank(),
             axis.ticks = element_blank(),
             plot.caption = ggtext::element_textbox_simple(
               # font size "small"
               size = rel(0.85)))


path <- "data/brookings_paper_data.xlsx"


data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path) %>%
  map(.f = rename_dates, .x = .)





# Figure 1 ------------------------------------------------------------------------------------

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
) %>%
  mutate(Category = factor(Category, levels = c("Monetary policy actions", "Fiscal policy actions", "Data releases", "News")))



#Conditionally set whether text will be above or below the point
vjust <- ifelse(timeline$displ > 0, -1, 1.8)

#plot


timeline %>%
  ggplot(aes(start_date, displ)) +
  geom_segment(aes(x = start_date, xend = start_date, y = 0, yend = displ, color = Category),
               size = .5, linetype = "solid") +
  geom_point(aes(color = Category)) +
  geom_text(aes(x = start_date, y = displ, label = event), data = timeline,
            hjust = 0, vjust = vjust, size = 2.5) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9)) +
  scale_color_manual(values = unname(brookings_cols('THP_orange', 'THP_ltblue','THP_ltgreen', 'THP_purple'))) +
  theme_grey(base_size = 12, base_family = 'Calibri') +
  theme(rect = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        # axis.text.x = element_text(size = 7,color = "#000000"),
        # legend.text=element_text(size=7),
        # legend.title=element_text(size=9),
        # legend.key.size = unit(0.5, "cm"),
        legend.position = 'bottom',

        plot.title = ggtext::element_textbox_simple(
          # font size "large"
          family = 'Calibri',
          size = rel(1.33),
          color = "#007363",
          hjust = 0,
          vjust = 1,
        ),
        panel.grid = ggplot2::element_line(
          colour = "#FFFFFF",
          # size = 1,
          linetype = "dotted"
        )) +
  expand_limits(x = c(ymd(20191229), ymd(20200910)), y = 1.2) +
  labs(x = NULL,
       y = NULL,
       title = 'Figure 1.<br>Timeline of Data Releases and Early Policy Responses to COVID-19, January-September 2020') -> p1

#and run the function from above
shift_axis(p1, ymd(20191229), ymd(20200910))

path <- 'figures-with-text/fig1'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 8, device = cairo_pdf,  units = 'in', dpi = 300, bg = '#FFFFFF')

# Figure 2 ------------------------------------------------------------------------------------


data$figure2 %<>%
  mutate(x = if_else(date == '2020-02-08', 'ADP-FRB', 'BLS-CES'))

adp <- data$figure2 %>% select(date, contains('adp'), x)
bls <- data$figure2 %>% select(date, contains('bls'), x)


snapshot <- function(adp_until = '2020-03-24', bls_until = '2020-02-15', subtitle = 'Data as of the end of March',
                     type = 'employment'){
  adp <- adp %>% filter(date <= adp_until)
  bls <- bls %>% filter(date <= bls_until)

  if(type == 'employment'){
    ggplot(adp, aes(x = date, y = adp_frb_total, fill = x)) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_point(stroke = 2, color =  '#999999', size = 0.1, alpha = 0.9) +
      geom_line(color = '#999999', size = 0.5, alpha = 0.9) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_total),
                 shape = 1,
                 color = unname(brookings_cols('THP_orange')),
                 size = 4,
                 linetype = 3,
                 fill = NA) +
      guides(fill = guide_legend(
        override.aes = list(shape = c(21, 1),
                            linetype = 3,
                            color = c('#999999', brookings_colors['THP_orange']),
                            fill = c('#999999', NA),
                            size = c(2, 6)))) +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 1)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)'
           )
  }
  else{
    ggplot(adp, aes(x = date, y = adp_frb_leisure, fill = x)) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_point(stroke = 2, color = '#999999', size = 0.1, alpha = 0.9) +
      geom_line(color = '#999999', size = 0.5, alpha = 0.9) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_leisure),
                 shape = 1,
                 color = unname(brookings_cols('THP_orange')),
                 size = 4,
                 linetype = 3,
                 fill = NA) +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 1)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)') +
      guides(fill = guide_legend(
        override.aes = list(shape = c(21, 1),
                            linetype = 3,
                            color = c('#999999', brookings_colors['THP_orange']),
                            fill = c('#999999', NA))))
  }
}



p1 <- snapshot()
p2 <- snapshot(adp_until = '2020-04-24',
               bls_until = '2020-03-15',
               subtitle = 'Data as of the end of April') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())
p3 <- snapshot(adp_until = '2020-05-20',
               bls_until = '2020-05-20',
               subtitle = 'Data as of mid-May') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())

l1 <- snapshot(type = 'leisure')
l2 <- snapshot(adp_until = '2020-04-24',
               bls_until = '2020-03-15',
               subtitle = 'Data as of the end of April',
               type = 'leisure') +
  theme(axis.text.y = element_blank()) +
  labs(y = NULL)
l3 <- snapshot(adp_until = '2020-05-20',
               bls_until = '2020-05-20',
               subtitle = 'Data as of mid-May',
               type = 'leisure') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())




p <- ((p1 + p2 + p3 + plot_layout(guides='collect') & theme(legend.position = ("top"),
                                                            plot.title = element_textbox_simple(color = 'black', size = 12))) + plot_annotation(
                                                              title = 'Figure 2a.<br>Timing of ADP-FRB, LLP and BLS CES Private Paid Employment Data Releases'))

l <- (l1 + l2 + l3 + plot_layout(guides='collect') & theme(legend.position = 'none',
                                                           plot.title = element_textbox_simple(color = 'black', size = 12))) + plot_annotation(
                                                             title = 'Figure 2b.<br>Timing of ADP-FRB, LLP and BLS CES Leasure and Hospitality Data Releases',
                                                              
                                                           )

(wrap_elements(panel = p) +   ggtitle('Figure 2.<br>Snapshots of Employment Data'))/ wrap_elements(panel = l)+labs(caption="Source: ADP, Inc. (2020-2021), BLS CES(2020-2021); authors' calculations<br>Note:For ADP-FRB, paid  employment concept is plotted.")

path <- 'figures-with-text/fig2'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 10, device = cairo_pdf, units = 'in', dpi = 300)

# brookings_save('figures/fig2.png', size = 'large',)
# ggsave('figures/fig2.png')
#
# magick::image_resize(image_read('figures/fig2.png'), geometry = '1350x') %>%
#   image_write('figures/fig2.png')
#
# insertImage(wb,
#           file = 'figures/fig2.png',  sheet = 'figure2')


# Figure 3 ------------------------------------------------------------------------------------

snapshot2 <- function(census_until = '2020-03-01', alt_until = '2020-03-31',
                      subtitle = 'Data as of the end of March'){
  fig3_points <- data$figure3 %>%
    pivot_longer(-date) %>%
    filter(name %in% c('census_retail_sales', 'census_food_services'))


  fig3_points %>%
    filter(date <= census_until) %>%
    drop_na() %>%
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line(data = pivot_longer(data$figure3, -date) %>% filter(date <= alt_until),
              mapping = aes(x = date, y = value, color = name), size = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +

    scale_color_manual(values = unname(brookings_cols("THP_dkblue","THP_green", "THP_ltpurple", "THP_yellow", "THP_purple", "THP_orange")),
                       labels = c('Airport departures', 'Census food services & drinking places', 'Census retail sales', 'Fiserv retail sales', 'Restaurants reservations')) +
    scale_y_continuous(breaks = seq(-100, 30, 20),
                       limits = c(-100, 30)) +
    scale_x_date(limits = c(as_date('2020-01-01'), as_date('2020-05-20'))) +
    labs(x = subtitle, y = 'Percent change from the same period in 2019') +
    guides(color = guide_legend(
      nrow = 2,
      ncol = 3))

}



p1 <- snapshot2()
p2 <- snapshot2(census_until = '2020-03-15',
                alt_until = '2020-04-15',
                subtitle = 'Data as of mid-April') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())
p3 <- snapshot2(census_until = '2020-05-31',
                alt_until = '2020-05-31',
                subtitle = 'Data as of mid-May') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())


p1 + p2 + p3 + plot_annotation(title = 'Figure 3.<br>Snapshots of Consumer Spending (percent change from same period in 2019)',
                               caption = 'Source: Census Bureau 2020; Fiserv 2020; OpenTable 2020; Transportation Security Administration 2020;<br>Note: Airport departures, restaurant reservations, and Fiserv retail sales are seven-day moving averages.')+  plot_layout(guides='collect') & theme(legend.position = 'bottom')

path <- 'figures-with-text/fig3'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 8, device = cairo_pdf, units = 'in', dpi = 300)



# Figure 4 ------------------------------------------------------------------------------------

p1 <- data$figure4 %>%
  select(date, ends_with('total')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_total), color = '#666666') +
  geom_point(aes(y = bls_ces_total, fill = 'BLS-CES'),
             color = unname(brookings_cols('THP_orange'))) +
  scale_x_date(limits = c(as_date('2020-01-30'), as_date('2021-10-02')), date_labels = "%b\n%Y", date_breaks = '3 month') +
  scale_y_continuous(breaks = seq(-24, 8, by = 4),
                     limits = c(-24, 8), expand = expansion()) +
  labs(x = NULL,
       y = NULL) +
  geom_hline(yintercept = 0) +
  labs(y = 'Millions of jobs (monthly change)')

p2 <-data$figure4 %>%
  select(date, ends_with('leisure')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_leisure), color = '#666666') +
  geom_point(aes(y = bls_ces_leisure, fill = 'BLS-CES'),
             color = unname(brookings_cols('THP_orange'))) +
  scale_x_date(limits = c(as_date('2020-01-30'), as_date('2021-10-02')), date_labels = "%b\n%Y", date_breaks = '3 month') +
  scale_y_continuous(breaks = seq(-8, 2, by = 2),
                     limits = c(-8, 2.5), expand = expansion()) +
  labs(x = NULL,
       y = NULL) +
  geom_hline(yintercept = 0) +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

fig_number = 'Figure 4.'
title = 'Timing of ADP-FRB and BLS CES Employment Data Releases for Change in Employment, March 2020-September 2021'
caption = "Source: ADP, Inc. 2020 to 2021; BLS CES 2020 to 2021; authors' calculations<br>Note: For ADP-FRB, paid employment concept is plotted."

p1 + p2 + plot_annotation(title = glue('{fig_number}<br>{title}'),
                          caption = glue('{caption}')) + plot_layout(guides='collect') & theme(legend.position = 'bottom')

path <- 'figures-with-text/fig4'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 6, device = cairo_pdf, units = 'in', dpi = 300)

# Figure 5 ------------------------------------------------------------------------------------


p1 <- data$figure5_1 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = unname(brookings_cols('THP_purple', 'THP_ltgreen')),
                     labels = c('NY-NJ-CT', 'Other states')) +
  scale_x_date(limits = c(as_date('2020-03-01'), as_date('2020-07-05')), date_labels = "%b\n%Y", date_breaks = '1 month', expand = expansion()) +
  scale_y_continuous(breaks = seq(0, 500, 100),
                     limits = c(0, 500),
                     expand = expansion()) +
  labs(x = NULL,
       y = "Cases per one million people",
       title = 'Figure 5a.<br>COVID-19 Case Rates by Location<br>',
       caption = '<br>Source: Metropolitan Transportation Authority 2020; New York Times (n.d.)<br>Note: Seven-day moving averages of COVID-19 data are depicted.Weekly estimates of transportation data are seasonally adjusted.') +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12))

p2 <- data$figure5_2 %>%
  ggplot(aes(x = date, y = ny_riders, fill = ny_riders)) +
  geom_line(color = brookings_colors['THP_ltblue']) +
  geom_point(color = brookings_colors['THP_ltblue']) +
  scale_y_continuous(breaks = seq(0, 40, 5),
                     limits = c(0, 36),
                     expand = expansion()) +
  scale_x_date(limits = c(as_date('2020-02-01'), as_date('2020-07-02')), date_labels = "%b\n%Y", date_breaks = '1 month', expand = expansion()) +
  labs(x = NULL,
       y = "Millions of turnstile entries",
       title = 'Figure 5b.<br>Weekly New York City Subway Turnstile Entries<br>',
              caption = '')  +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12),
        legend.key.size = unit(0, 'in'),
        legend.text = element_text(color = NA))

fig_number = 'Figure 5.'
title = 'COVID-19 Case Rates and New York City Subway Turnstiles Entries, March-July 2020'
p1 + p2 + plot_annotation(title = glue('{fig_number}<br>{title}'))

path <- 'figures-with-text/fig5'
ggsave(glue::glue("{path}.pdf"), width = 9, height = 8, device = cairo_pdf)
# Figure 6 ------------------------------------------------------------------------------------


data$figure6_1 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('bottom', 'bottom_middle', 'top_middle', 'top'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_hline(yintercept = 100) +
  geom_line(size = 0.5) +
  scale_color_manual(values = unname(brookings_cols("THP_dkblue","THP_green", "THP_ltblue", "THP_yellow")),
                     labels = c('Bottom', 'Bottom-middle', 'Top-middle', 'Top')
  ) +
  scale_x_date(limits = c(as_date('2020-02-01'), as_date('2021-11-30')), date_labels = "%b\n%Y", date_breaks = '3 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(60, 105, 10),
                     limits = c(65, 105)) +
  labs(x = NULL,
       y = "Week ending February 15, 2020 = 100",
       title = 'Figure 6a.<br>ADP-FRB Employment by Wage Quartile',
       caption = "<br>Source: ADP, Inc. 2020 to 2021; Opportunity Insights 2020 to 2021 via Affinity; authors' calculations <br>Note: Seasonally adjusted seven-day moving averages are shown.") +
  guides(color = guide_legend(
    nrow = 2,
    ncol = 2)) +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12)) -> p1


data$figure6_2 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('low', 'middle', 'high'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_color_manual(values = unname(brookings_cols('THP_ltblue', 'THP_orange', 'THP_ltgreen')), labels = c('Low', 'Middle', 'High')) +
  scale_x_date(limits = c(as_date('2020-01-01'), as_date('2021-11-30')), date_labels = "%b\n%Y", date_breaks = '3 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(-40, 40, 10),
                     limits = c(-40, 40)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL,
       y = "Percent change from January 2020",
       title = '<br>Figure 6b.<br>Affinity Consumer Spending by Income<br>',
       caption = "") +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12)) -> p2
# Figure 6. Employment and Consumer Spending by Income, Indexed to February 2020


p1 + p2 + plot_annotation(title = 'Figure 6.<br>Employment and Consumer Spending by Income, Indexed to February 2020')

path <- 'figures-with-text/fig6'
ggsave(glue::glue("{path}.pdf"), width = 8.5, height = 8, device = cairo_pdf)
# Figure 7 ------------------------------------------------------------------------------------

# Tibble used to shade dates when stimulus checks were sent out
eip <- tribble(~'start', ~'end',
               '2020-04-15', '2020-04-25',
               '2021-01-04', '2021-01-10',
               '2021-03-17', '2021-03-23'
) %>%
  mutate(start = as_date(start),
         end = as_date(end))



break_vec <- c(as_date('2020-03-01'), seq(from = as_date('2020-06-01'),
                                          to = as_date('2021-09-01'),
                                          by = '3 months'),
               as_date('2021-12-01'))
# Plot
ggplot(data = data$figure7_1,
       aes(x = date, y = fiserv)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(color = '#666666'), size = 0.5, alpha = 0.9) +
  geom_line(data = data$figure7_2,
            mapping = aes(x = date, y = census,
                          color = unname(brookings_cols('THP_orange')))) +
  geom_point(data = data$figure7_2,
             size = 2,
             mapping = aes(x = date, y = census,
                           color = unname(brookings_cols('THP_orange')))) +
  labs(x = NULL,
       y = 'Percent change from the same period in 2019') +
  scale_color_manual(values = c('#999999', unname(brookings_cols('THP_orange'))),
                     labels = c('Fiserv, 7-day avg.', 'Census Bureau')) +
  scale_x_date(limits = c(as_date('2020-03-01'), as_date('2021-12-30')), date_labels = "%b\n%Y", expand = expansion(),
               breaks = break_vec) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(-30, 35, 10),
                     limits = c(-30, 35)) +
  geom_rect(data = eip,
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = '#666666', inherit.aes = FALSE, alpha = 0.4) +
  guides(color = guide_legend(
    override.aes = list(shape = c(NA, 16)))) +
  theme(legend.position = 'bottom') +
  labs(title = 'Figure 7.<br>Census and Fiserv Consumer Spending<br>',
       caption = 'Source: Census Bureau 2020 to 2021; Fiserv 2020 to 2021<br>Note: Bars indicate the week at which stimulus payment direct deposits commenced. Physical checks continued to be dispersed for 3-4 weeks after these dates.')



path <- 'figures-with-text/fig7'
ggsave(glue::glue("{path}.pdf"), width = 8.5, height = 8, device = cairo_pdf)

# Figure 8 ------------------------------------------------------------------------------------


data$figure8_1 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('pct_remote', 'pct_hybrid', 'pct_inperson'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_x_date(limits = c(as_date('2020-08-01'), as_date('2021-06-30')), date_labels = "%b\n%Y", date_breaks = '2 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(0, 80, 10),
                     limits = c(0, 75)) +
  scale_color_manual(values = unname(brookings_cols('THP_ltblue', 'THP_dkgreen', 'THP_purple')),
                     labels = c('Remote', 'Hybrid', 'In-person') ) +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12),
        plot.caption = element_textbox_simple(size=3,color="black")) +
  labs(x = NULL,
       y = 'Percent of students in public K-12 schools',
       title = '<br>Figure 8a.<br>Distribution of Students by School Instruction Modes, August 2020-June 2021<br>',
       caption = '<br>Source: Burbio, Inc. 2020 to 2021;Kastle Systems 2020 to 2021<br>Note: Based on employee key card entries into office buildings. 7-day average through Jan. 6, 2021 and weekly readings thereafter.<br><br>')  -> p1




data$figure8_2 %>%
  pivot_longer(-date) %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_x_date(limits = c(as_date('2020-03-01'), as_date('2021-10-30')), date_labels = "%b\n%Y", date_breaks = '3 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(0, 55, 10),
                     limits = c(0, 55)) +
  scale_color_manual(values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange', 'THP_ltblue')),
                     labels = c('Dallas', 'DC', 'NYC', 'SF') ) +
  theme(plot.title = element_textbox_simple(color = 'black', size = 12)) +
  labs(x = NULL,
       y = 'Percent of key card entries relative to February 2020',
       title = '<br>Figure 8b.<br>Kastle Key Card Office Building Entries, April 2020-October 2021<br>',
caption = "") -> p2

p1 + p2 + plot_annotation(title = 'Figure 8.<br>School Instruction Mode and Keycard Office Entries, 2020-21')

path <- 'figures-with-text/fig8'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 5, device = cairo_pdf)

# Figure 9 ------------------------------------------------------------------------------------

# We have to read this one in separately because the dates are not formatted in a way Lubridate can parse
data$figure9_2 <- read_excel("data/brookings_paper_data.xlsx", sheet = 'figure9_2') %>%
  dplyr::rename(date = dates) %>%
  mutate(date = as_date(glue::glue('{date}-15')))

data$figure9_1$opentable_pct_chg_2019
scales::rescale(x = data$figure9_2$lh_emp,
                to = range(data$figure9_1$opentable_pct_chg_2019))


ylim.prim <- c(-80, 20)
ylim.sec <- c(12, 17)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

ggplot() +
  geom_line(data = data$figure9_1,
            mapping = aes(x = date,
                          y =  opentable_pct_chg_2019, color = '#666666'),
            alpha = 0.4) +
  geom_line(data = data$figure9_2,
            mapping = aes(x = date,
                          y =a + b * lh_emp,
                          color = unname(brookings_cols('THP_orange')))) +
  geom_point(data = data$figure9_2,
             size = 2.5,
             mapping = aes(x = date,
                           y =a + b * lh_emp,
                           color = unname(brookings_cols('THP_orange')))) +
  scale_color_manual(labels = rev(c('OpenTable seatings (left axis)', 'Leisure & Hospitality (right axis)')),
                     values = rev(c('#000000', unname(brookings_cols('THP_orange')) ))) +
  theme(axis.ticks.y.right = element_line(color = unname(brookings_cols('THP_orange'))),
        axis.text.y.right = element_text(color = unname(brookings_cols('THP_orange'))),
        axis.title.y.right = element_text(color = unname(brookings_cols('THP_orange')))
  ) +
  scale_y_continuous('Percent change in reservations from 2019',
                     sec.axis = sec_axis(~(. - a) / b, name = 'Employment (millions)')) +
  annotate('text',
           x = as_date('2021-09-01'),
           y = -18,
           label = 'Sept. 7',
           size = 6) +
  scale_x_date(date_labels = '%b\n%Y',
               date_breaks = '2 months',
               limits = c(as_date('2020-06-01'), as_date('2021-09-25')),
               expand = expansion()) +
  guides(color = guide_legend(
    reverse = TRUE,
    override.aes = list(shape = c(NA, 18)))) +
  theme(legend.position = 'bottom',
        axis.ticks.y.right = element_blank()) +
  labs(x = NULL,
       title = 'Figure 9.<br>OpenTable Reservations and BLS Leisure and Hospitality Employment, June 2020-September 2021',
       caption = 'Source: BLS 2020 to 2021; OpenTable 2020 to 2021')

path <- 'figures-with-text/fig9'

ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)
# Figure 10 -----------------------------------------------------------------------------------


data$figure10_1 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_y_continuous(expand = expansion(),
                     limits = c(-5, 50),
                     breaks = seq(0, 50, 10)) +
  scale_x_date(date_labels = '%b\n%Y',
               date_breaks = '3 months',
               expand = expansion(),
               limits = c(as_date('2020-02-01'), as_date('2021-02-07'))) +
  scale_color_manual(labels = c('All', 'Education & Health', 'Leisure & Hospitality', 'Retail & Transportation'),
                     values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange', 'THP_purple'))) +
  labs(x =  NULL,
       y = 'Percent of businesses',
       title = 'Figure 10a.<br>Share of Businesses with No Transactions<br>',
       caption = 'Source: Opportunity Insights 2020 to 2021 via Womply; Homebase 2020 to 2021<br>Note: The sample is limited to businesses with a clock-in on February 15, 2020 and businesses with a transaction on February 15, 2020.<br>') +
  theme(legend.position = 'bottom',
        plot.title = element_textbox_simple(size = 10, color = 'black'),
        plot.caption = element_textbox_simple(size=3,color="black")) +
  # theme(legend.key.size = unit(0.25,"line")) +
  guides(color = guide_legend(nrow = 2,
                              ncol = 2)) -> p1

data$figure10_2 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_y_continuous(expand = expansion(),
                     limits = c(-5, 50),
                     breaks = seq(-0, 50, 10)) +
  scale_x_date(date_labels = '%b\n%Y',
               date_breaks = '3 months',
               expand = expansion(),
               limits = c(as_date('2020-02-01'), as_date('2021-02-07'))) +
  scale_color_manual(labels = c('2019', '2020'),
                     values = unname(brookings_cols('THP_yellow', 'THP_ltblue' ))) +
  labs(x =  NULL,
       y = NULL,
       title = 'Figure 10b.<br>Share of Businesses with No Clock-Ins<br>',
       caption = '') +
  theme(
        axis.text.y = element_blank(),
        plot.title = element_textbox_simple(size = 12, color = 'black')) -> p2


p1 + p2 + plot_annotation(title = 'Figure 10.<br>Measures of Small Business Closures')

path <- 'figures-with-text/fig10'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)

# Figure 11 -----------------------------------------------------------------------------------

data$figure11 %>% pivot_longer(-c(week_num, date)) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange')),
                     labels = c('2017-2019 average', '2020', '2021')) +
  scale_x_date(date_labels = '%b',
               date_breaks = '1 month',
               expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     limits = c(0, 550),
                     breaks = seq(0, 550, by = 100)) +
  labs(x = NULL,
       y = 'Applications in thousands') +
  theme(legend.position = 'bottom')  +
  labs(title = 'Figure 11.<br>Cumulative New Business Applications, Select Years',
       caption = 'Source: Census Bureau 2017 to 2021.<br>Note: Business applications with planned paid employees.')


path <- 'figures-with-text/fig11'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 7, device = cairo_pdf)

