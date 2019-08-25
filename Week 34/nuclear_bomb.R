library(tidyverse)
library(here)
library(ggtext)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# scale_*_radioactive
source(here("Week 34", "scale_radioactive.R"))

explosions_depth <- nuclear_explosions %>% 
        select(year, country, type) %>% 
        mutate(country = case_when(country == "PAKIST" ~ "Pakistan",
                                   country == "USA" ~ "USA",
                                   country == "UK" ~ "United Kingdom",
                                   country == "USSR" ~ "Soviet Union",
                                   T ~ str_to_title(country)),
               type = ifelse(grepl("SHAFT|MINE|U[G|W]|TUNNEL|GALLERY", type), "U", "A")
               ) %>% 
        group_by(year, country, type) %>% 
        count() %>% 
        mutate(n = ifelse(type == "U", -n, n))

annotations <- tibble(x1 = c(1963, 1996, NA, NA), y1 = c(5, 0, NA, NA),
                      x2 = c(1963, 1996, NA, NA), y2 = c(110, 105, NA, NA),
                      x3 = c(1964, 1994.5, NA, NA), y3 = c(110, 105, NA, NA),
                      xtext = c(1968, 1989, 1946, 1946), ytext = c(104, 102, 115, -55),
                      text = c("**Partial Test Ban Treaty**<br>prohibiting tests in atmosphere,
                               <br>outer space, and underwater",
                               "**Comprehensive Test Ban Treaty**<br>prohibiting all tests",
                               "**Atmopsheric**", "**Underground**")
                      )

manual_pal <- set_names(radioactive_pal("mixed")(7), c("USA", "United Kingdom", "France", "China", 
                                                       "India", "Pakistan", "Soviet Union"))
p <- ggplot() + 
        geom_segment(data = annotations, aes(x = x1, y = y1, xend = x2, yend = y2), 
                     col = "white") +
        geom_segment(data = annotations, aes(x = x1, xend = x3, y = y3, yend = y3), 
                     col = "white") +
        geom_richtext(data = annotations, aes(x = xtext, y = ytext, label = text), 
                      size = 5.5, fill = NA, color = "white", label.color = NA,
                      family = "Baskerville") +
        geom_bar(data = explosions_depth, aes(x = year, y = n, col = country), 
                 stat = "identity", fill = NA) +
        scale_color_manual(values = manual_pal) +
        scale_x_continuous("", breaks = seq(1945, 2000, 1), expand = c(0.03, 0)) +
        guides(color = guide_legend(ncol = 2)) +
        theme_minimal(base_size = 16, base_family = "Baskerville") +
        labs(x = "", y = "", 
             title = str_to_title("the number of nuclear explosions"),
             subtitle = str_wrap("A re-design for statistical chart in SIPRI report (2000) tallying 
                                 the number of nuclear explosions conducted by countries 
                                 in the air and under ground.", 85), 
             caption = "data: Stockholm International Peace Research Institute|graph: @chucc900") +
        theme(panel.background = element_rect("#000000"), 
              plot.background = element_rect("#000000"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = c(0.7, 0.68),
              legend.text = element_text(size = 20, margin = margin(0, 5, -1, 0)),
              legend.title = element_blank(),
              legend.key = element_rect(fill = "#000000"),
              legend.key.size = unit(.8, "cm"), 
              text = element_text(color = "white"), 
              axis.text = element_text(color = "white"),
              axis.text.x = element_text(angle = 90), 
              axis.line.y = element_line(color = "white"),
              plot.margin = margin(40, 40, 10, 10),
              plot.title = element_text(size = 30, color = radioactive_cols()["gorse"], 
                                        face = "bold"),
              plot.subtitle = element_text(size = 20)
        )

ggsave(here("Week 34", "nuclear_explosions.png"), p, width = 20, height = 10)
