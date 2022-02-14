
# Setup ---------------------------------------------------------------------------------------

librarian::shelf(tidyverse, openxlsx, readxl, purrr, janitor, ggbrookings, lubridate, patchwork, magrittr, magick)

theme_set(theme_thp(base_size = 14))

path <- "data/brookings_paper_data.xlsx"

data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

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

# Figure 2 ------------------------------------------------------------------------------------


data$figure2 %<>%
  mutate(date = as_date(dates), .before = everything()) %>%
  select(-dates) %>%
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
      geom_point(stroke = 2, color =  '#007363', size = 5) +
      geom_line(color = '#007363', size = 1.5) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_total),
                 shape = 1,
                 color = '#6E2585',
                 size = 15,
                 linetype = 3,
                 fill = NA) +
      guides(fill = guide_legend(
        override.aes = list(shape = c(21, 1),
                            linetype = 3,
                            color = c('#007363', "#6E2585"),
                            fill = c('#007363', NA),
                            size = c(3, 9)))) +
      theme(legend.position = c(0.2, 0.27),
            legend.direction = 'vertical')  +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 4)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)',
           title = '<br>')
  }
  else{
    ggplot(adp, aes(x = date, y = adp_frb_leisure, fill = x)) +
      geom_hline(yintercept = 0, size = 1) +
      geom_point(stroke = 2, color =  '#007363', size = 5) +
      geom_line(color = '#007363', size = 1.5) +
      geom_point(data = bls,
                 mapping = aes(x = date, y  = bls_ces_leisure),
                 shape = 1,
                 color = '#6E2585',
                 size = 15,
                 linetype = 3,
                 fill = NA) +
      guides(fill = guide_legend(
        override.aes = list(shape = c(21, 1),
                            linetype = 3,
                            color = c('#007363', "#6E2585"),
                            fill = c('#007363', NA),
                            size = c(3, 9)))) +
      theme(legend.position = c(0.2, 0.27),
            legend.direction = 'vertical')  +
      scale_x_date(limits = c(as_date('2020-01-31'), as_date('2020-05-02'))) +
      scale_y_continuous(breaks = seq(4, -28, by = -4),
                         limits = c(-28, 4)) +

      labs(x = subtitle,y = 'Millions of jobs (Change since Feb. 15)', title = '<br>')
  }

}

p1 <- snapshot()
p2 <- snapshot(adp_until = '2020-04-24',
               bls_until = '2020-03-15',
               subtitle = 'Data as of the end of April')
p3 <- snapshot(adp_until = '2020-05-20',
               bls_until = '2020-05-20',
               subtitle = 'Data as of mid-May')

l1 <- snapshot(type = 'leisure')
l2 <- snapshot(adp_until = '2020-04-24',
               bls_until = '2020-03-15',
               subtitle = 'Data as of the end of April',
               type = 'leisure')
l3 <- snapshot(adp_until = '2020-05-20',
               bls_until = '2020-05-20',
               subtitle = 'Data as of mid-May',
               type = 'leisure')

(p1 | p2 | p3) / (l1 | l2 | l3)
ggsave('figures/fig2.png', width = 4.5, units = 'in')
brookings_save('figures/fig2.png', size = 'large',)
ggsave('figures/fig2.png')

magick::image_resize(image_read('figures/fig2.png'), geometry = '1350x') %>%
  image_write('figures/fig2.png')

insertImage(wb,
          file = 'figures/fig2.png',  sheet = 'figure2')
