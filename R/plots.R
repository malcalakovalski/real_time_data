
# Setup ---------------------------------------------------------------------------------------

librarian::shelf(tidyverse, openxlsx, readxl, purrr, janitor, ggbrookings, lubridate, patchwork, magrittr, magick, ggalt, cowplot)

source('R/utils.R')

theme_set(theme_thp(base_size = 9))

path <- "data/brookings_paper_data.xlsx"


data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path) %>%
  map(.f = rename_dates, .x = .)



wb <- createWorkbook()

names(data) %>%
  walk(~addWorksheet(wb, sheetName = .))

walk2(.x = names(data),
      .y = data,
      ~writeData(wb,
                 sheet = .x,
                 x = .y))


saveWorkbook(wb,
             'demo.xlsx',
             overwrite = TRUE)
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
  scale_color_manual(values = unname(brookings_cols('THP_orange', 'THP_ltblue','THP_ltgreen', 'THP_purple'))) +
  theme_thp(base_size = 3) +
  theme(rect = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 4,color = "#000000"),
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
  scale_x_date(breaks = scales::pretty_breaks(n = 9)) +
  labs(x = NULL,
       y = NULL) -> p1

#and run the function from above
shift_axis(p1, ymd(20191229), ymd(20200910))

path <- 'figures/fig1'
ggsave(glue::glue("{path}.pdf"), width = 7, height = 5, device = cairo_pdf, bg = '#F2F7FA', units = 'in', dpi = 300)

insertPlot(wb, 1, width = 7, height = 5, fileType = "png", units = "in")
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
      geom_hline(yintercept = 0, size = 1) +
      geom_point(stroke = 2, color =  '#999999', size = 0.25) +
      geom_line(color = '#999999', size = 1) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_total),
                 shape = 1,
                 color = unname(brookings_cols('THP_orange')),
                 size = 5,
                 linetype = 3,
                 fill = NA) +
      theme_thp(base_size = 9) +
      # guides(fill = guide_legend(
      #   override.aes = list(shape = c(21, 1),
      #                       linetype = 3,
      #                       color = c('#007363', "#6E2585"),
      #                       fill = c('#007363', NA),
      #                       size = c(3, 9)))) +
      theme(legend.position = 'none',
            legend.direction = 'vertical',
            panel.grid = ggplot2::element_line(
              colour = "#CCCCCC",
              size = 0.5,
              linetype = "dotted"
            ),)  +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 4)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)',
           title = '<br>')
  }
  else{
    ggplot(adp, aes(x = date, y = adp_frb_leisure, fill = x)) +
      geom_hline(yintercept = 0, size = 1) +
      geom_point(stroke = 2, color = '#999999', size = 0.25) +
      geom_line(color = '#999999', size = 1) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_leisure),
                 shape = 1,
                 color = unname(brookings_cols('THP_orange')),
                 size = 5,
                 linetype = 3,
                 fill = NA) +
      theme_thp(base_size = 9) +
      theme(legend.position = 'none',
            legend.direction = 'horizontal')  +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 4)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)', title = '<br>') +
      theme(legend.position = 'none',
            legend.direction = 'vertical',
            panel.grid = ggplot2::element_line(
              colour = "#CCCCCC",
              size = 0.5,
              linetype = "dotted"
            ),)
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
  guides(fill = guide_legend(
    override.aes = list(shape = c(21, 1),
                        linetype = 3,
                        color = c('#999999', brookings_colors['THP_orange']),
                        fill = c('#999999', NA),
                        size = c(3, 9)))) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = 'bottom') +
  labs(y = NULL)
l3 <- snapshot(adp_until = '2020-05-20',
               bls_until = '2020-05-20',
               subtitle = 'Data as of mid-May',
               type = 'leisure') +
  labs(y = NULL) +
  theme(axis.text.y = element_blank())

(p1 | p2 | p3) / (l1 | l2 | l3)


path <- 'figures/fig2'
ggsave(glue::glue("{path}.pdf"), width = 7, height = 6, device = cairo_pdf, bg = '#F2F7FA', units = 'in', dpi = 300)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)


ggsave('figures/fig2.png', width = 4.5, units = 'in')
# brookings_save('figures/fig2.png', size = 'large',)
# ggsave('figures/fig2.png')
#
# magick::image_resize(image_read('figures/fig2.png'), geometry = '1350x') %>%
#   image_write('figures/fig2.png')
#
# insertImage(wb,
#           file = 'figures/fig2.png',  sheet = 'figure2')


# Figure 3 ------------------------------------------------------------------------------------
update_geom_defaults(geom = 'line',
                     new = list(size = 1,
                                point = 2))

snapshot2 <- function(census_until = '2020-03-01', alt_until = '2020-03-31',
                      subtitle = 'Data as of the end of March'){
  fig3_points <- data$figure3 %>%
    pivot_longer(-date) %>%
    filter(name %in% c('census_retail_sales', 'census_food_services'))


  fig3_points %>%
    filter(date <= census_until) %>%
    drop_na() %>%
    ggplot(aes(x = date, y = value, color = name)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point(size = 3) +
    geom_line(data = pivot_longer(data$figure3, -date) %>% filter(date <= alt_until),
              mapping = aes(x = date, y = value, color = name)) +
    scale_color_manual(values = unname(brookings_cols("THP_dkblue","THP_green", "THP_ltblue", "THP_yellow", "THP_purple", "THP_orange")),
                       labels = c('Airport departures', 'Census food services & drinking places', 'Census retail sales', 'Fiserv retail sales', 'Restaurants rsvs')) +
    scale_y_continuous(breaks = seq(-100, 60, 20),
                       limits = c(-100, 60)) +
    scale_x_date(limits = c(as_date('2020-01-01'), as_date('2020-05-20'))) +
    labs(x = subtitle, y = 'Percent change from the same period in 2019') +
    theme(legend.position = 'bottom',
          legend.direction = 'vertical')

}



p1 <- snapshot2() + theme(legend.position = 'none')
p2 <- snapshot2(census_until = '2020-03-15',
                alt_until = '2020-04-15',
                subtitle = 'Data as of mid-April') +
  labs(y = NULL)
p3 <- snapshot2(census_until = '2020-05-31',
                alt_until = '2020-05-31',
                subtitle = 'Data as of mid-May') +
  theme(legend.position = 'none') +
  labs(y = NULL)

p1 | p2 | p3

path <- 'figures/fig3'
ggsave(glue::glue("{path}.pdf"), width = 8, height = 10, device = cairo_pdf, bg = '#F2F7FA', units = 'in', dpi = 300)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)


ggsave('figures/fig2.png', width = 4.5, units = 'in')


# Figure 4 ------------------------------------------------------------------------------------

p1 <- data$figure4 %>%
  select(date, ends_with('total')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_total), color = '#007363') +
  geom_point(aes(y = bls_ces_total, fill = 'BLS-CES'),
             color = '#6E2585') +
  scale_x_date(limits = c(as_date('2020-01-30'), as_date('2021-10-02')), date_labels = "%b\n%Y", date_breaks = '3 month') +
  scale_y_continuous(breaks = seq(-24, 8, by = 4),
                     limits = c(-24, 8), expand = expansion()) +
  labs(x = NULL,
       y = NULL) +
  geom_hline(yintercept = 0) +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

p2 <-data$figure4 %>%
  select(date, ends_with('leisure')) %>%
  ggplot(aes(x = date, lty = 'ADP-FRB')) +
  geom_line(aes(y = adp_frb_leisure), color = '#007363') +
  geom_point(aes(y = bls_ces_leisure, fill = 'BLS-CES'),
             color = '#6E2585') +
  scale_x_date(limits = c(as_date('2020-01-30'), as_date('2021-10-02')), date_labels = "%b\n%Y", date_breaks = '3 month') +
  scale_y_continuous(breaks = seq(-8, 2, by = 1),
                     limits = c(-8, 2.5), expand = expansion()) +
  labs(x = NULL,
       y = NULL) +
  geom_hline(yintercept = 0) +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

p1 | p2

path <- 'figures/fig4'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#F2F7FA', units = 'in', dpi = 300)

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
  theme(legend.position = 'bottom')

p2 <- data$figure5_2 %>%
  ggplot(aes(x = date, y = ny_riders)) +
  geom_line(color = brookings_colors['THP_ltblue']) +
  geom_point(color = brookings_colors['THP_ltblue']) +
  scale_y_continuous(breaks = seq(0, 40, 5),
                     limits = c(0, 40),
                     expand = expansion()) +
  scale_x_date(limits = c(as_date('2020-03-01'), as_date('2020-07-02')), date_labels = "%b\n%Y", date_breaks = '1 month', expand = expansion()) +
  labs(x = NULL,
       y = "Millions")

p1 | p2

path <- 'figures/fig5'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')
# Figure 6 ------------------------------------------------------------------------------------


data$figure6_1 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('bottom', 'bottom_middle', 'top_middle', 'top'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = unname(brookings_cols("THP_dkblue","THP_green", "THP_ltblue", "THP_yellow")),
                     labels = c('Bottom', 'Bottom-middle', 'Top-middle', 'Top')
) +
  scale_x_date(limits = c(as_date('2020-02-01'), as_date('2021-11-30')), date_labels = "%b\n%Y", date_breaks = '3 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(60, 110, 10),
                     limits = c(60, 110)) +
  geom_hline(yintercept = 100) +
  labs(x = NULL,
       y = "Week ending February 15, 2020 = 100") +
  theme(legend.position = 'bottom') -> p1

data$figure6_2 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('low', 'middle', 'high'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
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
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 6, device = cairo_pdf, bg = '#FAFAFA')
# Figure 7 ------------------------------------------------------------------------------------


  data$figure7_1 %>%
  ggplot(aes(x = date, y = fiserv)) +
  geom_line(color = unname(brookings_cols('THP_ltgreen'))) +
  geom_line(data = data$figure7_2,
            mapping = aes(x = date, y = census),
            color = unname(brookings_cols('THP_dkblue'))) +
  geom_point(data = data$figure7_2,
             mapping = aes(x = date, y = census),
             color = unname(brookings_cols('THP_dkblue'))) +
  labs(x = NULL,
       y = 'Percent change from the same period in 2019') +
  scale_x_date(limits = c(as_date('2020-01-01'), as_date('2022-01-30')), date_labels = "%b\n%Y", date_breaks = '3 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(-40, 60, 10),
                     limits = c(-40, 60))

path <- 'figures/fig7'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')

# Figure 8 ------------------------------------------------------------------------------------


data$figure8_1 %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c('pct_remote', 'pct_hybrid', 'pct_inperson'))) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_x_date(limits = c(as_date('2020-08-01'), as_date('2021-06-30')), date_labels = "%b\n%Y", date_breaks = '2 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(0, 100, 10),
                     limits = c(0, 100)) +
  scale_color_manual(values = unname(brookings_cols('THP_ltblue', 'THP_dkgreen', 'THP_purple')),
                    labels = c('Remote', 'Hybrid', 'In-person') ) +
  labs(x = NULL,
       y = 'Percent') +
  theme(legend.position = 'bottom') -> p1

data$figure8_2 %>%
  pivot_longer(-date) %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_x_date(limits = c(as_date('2020-04-01'), as_date('2021-10-30')), date_labels = "%b\n%Y", date_breaks = '2 month', expand = expansion()) +
  scale_y_continuous(expand = expansion(),
                     breaks = seq(0, 100, 10),
                     limits = c(0, 100)) +
  scale_color_manual(values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange', 'THP_ltblue')),
                     labels = c('Dallas', 'DC', 'NYC', 'SF') ) +
  theme(legend.position = 'bottom') +
  labs(x = NULL,
       y = 'Percent change from February 2020') -> p2

p1 | p2

path <- 'figures/fig8'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')

# Figure 9 ------------------------------------------------------------------------------------

data$figure9_1

path <- 'figures/fig9'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')
# Figure 10 -----------------------------------------------------------------------------------


data$figure10_1 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()

data$figure10_2 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()

path <- 'figures/fig10'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')


# Figure 11 -----------------------------------------------------------------------------------

data$figure11 %>% pivot_longer(-c(week_num, date)) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = unname(brookings_cols('THP_dkblue', 'THP_ltgreen', 'THP_orange')),
                     labels = c('2017-2019 average', '2020', '2021')) +
  scale_x_date(date_labels = '%b',
               date_breaks = '1 month',
               expand = expansion()) +
  scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = 'Thousands') +
  theme(legend.position = 'bottom')

path <- 'figures/fig11'
ggsave(glue::glue("{path}.pdf"), width = 4.5, height = 4, device = cairo_pdf, bg = '#FAFAFA')
