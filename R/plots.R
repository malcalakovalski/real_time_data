
# Setup ---------------------------------------------------------------------------------------

librarian::shelf(tidyverse, openxlsx, readxl, purrr, janitor, ggbrookings, lubridate, patchwork, magrittr, magick, ggalt, cowplot,ggpubr, ggtext)

source('R/utils.R')


theme_set(theme_thp(base_size = 9))
theme_update(plot.margin = margin(3.5,7, 0, 7),
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
axis.ticks = element_blank())


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
                     ymd(20200303), "Fed emergency rate cut by 1/2 percentage point (3/3)", -1, "Monetary policy actions",
                     ymd(20200313), "President declares national emergency (3/13)",-0.9,"Fiscal policy actions",
                     ymd(20200315), "Fed emergency rate cut to 0 percent (3/15)", -0.8, "Monetary policy actions",
                     ymd(20200315), "Large-scale asset purchases (3/15)", -0.8, "Monetary policy actions",
                     ymd(20200317), "First announcement of new Fed facilities (3/17)", -0.7, "Monetary policy actions",
                     ymd(20200319), "First state-wide lockdown order in U.S. (3/19)", 1, "News",
                     ymd(20200319), "Opening discussion of CARES act (3/19)", -0.6, "Fiscal policy actions",
                     ymd(20200326), "Initial UI claims data (3/26)", 0.9, "Data releases",
                     ymd(20200327), "CARES act passage (3/27)", -0.5, "Fiscal policy actions",
                     ymd(20200409), "Last announcement of new Fed facilities (4/9)", -0.4, "Monetary policy actions",
                     ymd(20200413), "First stimulus checks/UI go out (4/13)", -0.3, "Fiscal policy actions",
                     ymd(20200415), "March retail sales report (4/15)", 0.8, "Data releases",
                     ymd(20200424), "PPP and Health Care Enhancement Act (4/24)", 0.8, "Fiscal policy actions",
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
  theme_grey() +
  theme(rect = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 7,color = "#000000"),
         legend.text=element_text(size=7),
         legend.title=element_text(size=9),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'bottom',
        panel.grid = ggplot2::element_line(
          colour = "#FFFFFF",
          # size = 1,
          linetype = "dotted"
        )) +
  expand_limits(x = c(ymd(20191229), ymd(20200910)), y = 1.2) +
  labs(x = NULL,
       y = NULL) -> p1

#and run the function from above
p1_shifted <- shift_axis(p1, ymd(20191229), ymd(20200910))
p1_shifted<-p1_shifted+labs(fill = "")
p1_shifted
path <- 'figures/fig1'
ggsave(glue::glue("{path}.pdf"), width = 7, height = 5, device = cairo_pdf,  units = 'in', dpi = 300, bg = '#FFFFFF')

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

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)',
           title = '<br>')
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

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)', title = '<br>') +
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



p <- (p1 + p2 + p3) + plot_layout(guides='collect') & theme(legend.position = 'none')


l <- (l1 + l2 + l3) + plot_layout(guides='collect')

p / l

path <- 'figures/fig2'
ggsave(glue::glue("{path}.pdf"), width = 6, height = 5, device = cairo_pdf, units = 'in', dpi = 300)

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

    scale_color_manual(values = unname(brookings_cols("THP_dkblue","THP_green", "THP_ltblue", "THP_yellow", "THP_purple", "THP_orange")),
                       labels = c('Airport departures', 'Census food services & drinking places', 'Census retail sales', 'Fiserv retail sales', 'Restaurants rsvs')) +
    scale_y_continuous(breaks = seq(-100, 30, 20),
                       limits = c(-100, 30)) +
    scale_x_date(limits = c(as_date('2020-01-01'), as_date('2020-05-20'))) +
    labs(x = subtitle, y = 'Percent change from the same period in 2019') +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.text = element_text(size = 7)) +
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

p1 + p2 + p3 +  plot_layout(guides='collect') & theme(legend.position = 'bottom')

path <- 'figures/fig3'
ggsave(glue::glue("{path}.pdf"), width = 5.5, height = 4, device = cairo_pdf, units = 'in', dpi = 300)



# Figure 4 ------------------------------------------------------------------------------------

p1 <- data$figure4 %>%
  select(date, ends_with('total')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_total), color = '#999999') +
  geom_point(aes(y = bls_ces_total, fill = 'BLS-CES'),
             color = unname(brookings_cols('THP_orange'))) +
  scale_x_date(limits = c(as_date('2020-01-30'), as_date('2021-10-02')), date_labels = "%b\n%Y", date_breaks = '3 month') +
  scale_y_continuous(breaks = seq(-24, 8, by = 4),
                     limits = c(-24, 8), expand = expansion()) +
  labs(x = NULL,
       y = 'Millions of jobs (monthly change)') +
  geom_hline(yintercept = 0) +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

p2 <-data$figure4 %>%
  select(date, ends_with('leisure')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_leisure), color = '#999999') +
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

p1 + p2 +  plot_layout(guides='collect') & theme(legend.position = 'bottom')

path <- 'figures/fig4'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 3, device = cairo_pdf, units = 'in', dpi = 300)

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
       y = "Cases per one million people") +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 7, 0, 7))

p2 <- data$figure5_2 %>%
  ggplot(aes(x = date, y = ny_riders)) +
  geom_line(color = brookings_colors['THP_ltblue']) +
  geom_point(color = brookings_colors['THP_ltblue']) +
  scale_y_continuous(breaks = seq(0, 40, 5),
                     limits = c(0, 40),
                     expand = expansion()) +
  scale_x_date(limits = c(as_date('2020-03-01'), as_date('2020-07-02')), date_labels = "%b\n%Y", date_breaks = '1 month', expand = expansion()) +
  labs(x = NULL,
       y = "Millions of turnstile entries") +
  theme(plot.margin = margin(0, 7, 0, 7))


p1 | p2

path <- 'figures/fig5'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)
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
       y = "Week ending February 15, 2020 = 100") +
  guides(color = guide_legend(
    nrow = 2,
    ncol = 2)) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 6)) -> p1


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
       y = "Percent change from January 2020") +
  theme(legend.position = 'bottom') -> p2

p1 | p2

path <- 'figures/fig6'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)
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
  geom_line(aes(color = '#999999'), size = 0.5, alpha = 0.9) +
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
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = 'grey70', inherit.aes = FALSE, alpha = 0.4) +
  guides(color = guide_legend(
    override.aes = list(shape = c(NA, 16)))) +
  theme(legend.position = 'bottom')

path <- 'figures/fig7'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 3.5, device = cairo_pdf)

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
  labs(x = NULL,
       y = 'Percent of students') +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.2, "cm")) -> p1

data$figure8_2 %>%
  pivot_longer(-date) %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 0.5) +
  scale_x_date(limits = c(as_date('2020-04-01'), as_date('2021-10-30')), date_labels = "%b\n%Y", date_breaks = '2 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(0, 55, 10),
                     limits = c(0, 55)) +
  scale_color_manual(values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange', 'THP_ltblue')),
                     labels = c('Dallas', 'DC', 'NYC', 'SF') ) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.2, "cm")) +
  labs(x = NULL,
       y = 'Percent change in key card entries from February 2020') -> p2

p1 | p2

path <- 'figures/fig8'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)

# Figure 9 ------------------------------------------------------------------------------------

# We have to read this one in separately because the dates are not formatted in a way Lubridate can parse
data$figure9_2 <- read_excel("data/brookings_paper_data.xlsx", sheet = 'figure9_2') %>%
  dplyr::rename(date = dates) %>%
  mutate(date = as_date(glue::glue('{date}-15')))

data$figure9_1$opentable_pct_chg_2019
scales::rescale(x = data$figure9_2$lh_emp,
                to = range(data$figure9_1$opentable_pct_chg_2019))


ylim.prim <- c(-80, 20)
ylim.sec <- c(12, 16)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

ggplot() +
  geom_line(data = data$figure9_1,
            mapping = aes(x = date,
                y =  opentable_pct_chg_2019, color = 'grey'),
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
           y = -16,
           label = 'Sept. 7',
           size = 3) +
  scale_x_date(date_labels = '%b\n%Y',
               date_breaks = '2 months',
               limits = c(as_date('2020-06-01'), as_date('2021-09-25')),
               expand = expansion()) +
  guides(color = guide_legend(
    reverse = TRUE,
    override.aes = list(shape = c(NA, 16)))) +
  theme(legend.position = 'bottom',
        axis.ticks.y.right = element_blank()) +
  labs(x = NULL)


path <- 'figures/fig9'

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
       y = 'Percent of businesses') +
  theme(legend.position = 'bottom') +
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
       y = NULL) +
  theme(legend.position = 'bottom',
        axis.text.y = element_blank()) -> p2


p1 + p2 & theme(legend.text = element_text(size = 4))

path <- 'figures/fig10'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 3.5, device = cairo_pdf)


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
       y = '(Thousands)') +
  theme(legend.position = 'bottom')

path <- 'figures/fig11'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf)
