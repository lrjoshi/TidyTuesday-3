library(tidyverse)
library(here)
library(ggtext)

df_cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
df_gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
df_ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")

df_all <- df_ram %>% 
        rename(name = chip_name, designer = manufacturer_s) %>% 
        select(name, date_of_introduction, transistor_count, designer) %>% 
        mutate(type = "RAM") %>% 
        bind_rows(df_gpu %>% 
                          rename(name = processor, designer = manufacturer_s) %>% 
                          select(name, date_of_introduction, transistor_count, designer) %>% 
                          mutate(type = "GPU")
                  ) %>% 
        bind_rows(df_cpu %>% 
                          rename(name = processor) %>% 
                          select(name, date_of_introduction, transistor_count, designer) %>% 
                          mutate(type = "CPU")
                  )

df_ratio <- df_all %>% 
        mutate(year_gap = date_of_introduction %/% 2 * 2) %>% 
        group_by(year_gap, type) %>% 
        summarize(max = max(transistor_count)) %>% 
        ungroup() %>% 
        group_by(type) %>% 
        mutate(lag = lag(max), 
               ratio = max / lag) %>% 
        ungroup() %>% 
        filter(!is.na(ratio))

df_pred <- df_ratio %>% 
        group_by(type) %>% 
        nest() %>% 
        ungroup() %>% 
        mutate(
                model = map(data, ~loess(ratio ~ year_gap, data = .x, 
                                         control = loess.control(surface = "direct"), span = 0.8)),
                pred = map(model, ~predict(.x, newdata = data.frame(year_gap = seq(2016, 2028, 2))))
               ) %>% 
        unnest(pred)

df_lbls <- tibble(x = c(1965.6, 1974.8, 1984), 
                  y = c(12, 4, 0.3), 
                  lbl = c("RAM", "CPU", "GPU"))

subtitle_lab <- 
"The Moore's Law observes that number of transistors in a densed circuit<br>
**doubles** about every two years. The following plot shows whether<br> this relationship holds
over time. According to the predictions from a simple<br> LOESS regression model, the relationship 
will remain **true** in the near future."

df_ratio %>% 
        ggplot(aes(year_gap, ratio, color = type)) +
        geom_point(size = 2) + 
        geom_smooth(se = F) + 
        geom_smooth(data = mutate(df_pred, year_gap = rep(seq(2016, 2028, 2), 3)),  
                    aes(year_gap, pred, color = type), 
                    se = F,  linetype = 2, method = "loess", span = .8) +
        geom_richtext(data = df_lbls, aes(x, y, label = lbl, color = lbl), 
                      fill = NA, label.color = NA) + 
        geom_hline(yintercept = 2, linetype = 2, size = 0.5, color = "#FFFFFF") + 
        annotate("text", x = 1965, y = 21, 
                 label = "R==frac(italic(max)*(N)[t+1], italic(max)*(N)[t])", parse = TRUE, 
                 size = 6, color = "#f7efd3", family = "Futura Condensed Medium") + 
        annotate("text", x = 1965, y = 3, label = "Moore's ratio = 2", color = "white", 
                 size = 6, family = "Futura Condensed Medium") + 
        annotate("text", x = c(2016, 2016, 2016), y = c(21.5, 20.5, 19.5), 
                 label = c("Relationship ratio is defined by R:", 
                           "maximum transistor count in next two-year period:", 
                           "maximum transistor count in current two-year period:"), 
                 hjust = 1, size = 5, color = "#f7efd3", family = "Futura Condensed Medium") +
        annotate("text", x = 2018 , y = c(21.5, 20.3, 19.3) , 
                 label = c("frac(italic(max)*(N)[t+1], italic(max)*(N)[t])", 
                           "italic(max)*(N)[t+1]", 
                           "italic(max)*(N)[t]"), 
                 parse = TRUE, size = 3.5, color = "#f7efd3", vjust = .4,
                 family = "Futura Condensed Medium") +
        scale_color_manual(values = c("#cd8b62", "#eed7a1", "#f7efd3")) +
        scale_x_continuous(breaks = seq(1962, 2028, 8), labels = seq(1962, 2028, 8)) +
        theme_classic(base_family = "Futura Condensed Medium", base_size = 16) +
        theme(plot.background = element_rect(fill = "#475c6c"), 
              plot.margin = margin(t = 20, l = 10, r = 20, b = 10), 
              plot.title = element_markdown(size = 28, color = "#f7efd3"),
              plot.subtitle = element_markdown(size = 20, color = "#eed7a1"),
              plot.caption = element_markdown(color = "#eed7a1"),
              panel.background = element_rect(fill = "#475c6c"),
              panel.grid = element_blank(), 
              legend.position = "none",
              axis.text = element_text(color = "#f7efd3"),
              axis.line = element_line(color = "#f7efd3"), 
              axis.title.y = element_text(angle = 0, vjust = 1, hjust = 1),
              axis.ticks = element_blank()
              ) +
        labs(x = "", y = "", 
             title = "The **substainability** of Moore's Law",
             subtitle = subtitle_lab, 
             caption = "**Data: Wikipedia | Graphic: @chucc900**") + 
        ggsave(here("Week 36", "moore's_law.png"), width = 33.9, height = 19.9, units = "cm")
