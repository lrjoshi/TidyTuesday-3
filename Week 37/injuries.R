library(tidyverse)
library(rvest)
library(magick)
library(cowplot)

df_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv", 
                               na = c("n/a", "NA", "N/A"))
date_parse <- quote(as_date(as.numeric(injury_date), origin = "1900-01-01"))

body_list <- read_html("https://www.enchantedlearning.com/wordlist/body.shtml") %>% 
        html_nodes(".wordlist-item") %>% 
        html_text() %>% 
        str_split(" ") %>% 
        reduce(c)
body_list <- c(body_list[!body_list %in% c("of", "the", "pinky")], 
               "forearm", "collarbone", "rib", "genital", "tooth")
body_list <- tolower(df_injuries$body_part) %>% 
        str_remove("[:punct:]") %>% 
        str_split(" |/") %>% 
        reduce(c) %>% 
        intersect(body_list) %>% 
        sort()

injured_head <- c("head", "face", "eye", "chin", "nose", "mouth", "forehead", "eyebrow", 
                  "ear", "jaw", "tooth", "eyelid", "lip", "teeth", "cheek")
injured_hand <- c("arm", "forearm", "finger", "hand", "thumb", "wrist")
injured_upperbody <- c("abdomen", "back", "body", "bone", "chest", "clavicle", "collar", 
                       "collarbone", "neck", "rib", "ribs", "shoulder", "skin", 
                       "spleen", "stomach", "torso", "elbow")
injured_lowerbody <- c("genital", "groin", "hip", "knee", "leg", "pelvis", "thigh")
injured_foot <- c("ankle", "foot", "toe")

# clean data
df_injures_body <- df_injuries %>% 
        mutate(body_part = tolower(body_part) %>% 
                       str_split(" |/") %>% 
                       map(str_remove, ",|\\.|&")) %>% 
        unnest() %>% 
        filter(body_part %in% body_list) %>% 
        mutate(body_part = ifelse(grepl("bone", body_part), "collarbone", body_part), 
               injured = case_when(body_part %in% injured_head ~ "head",
                                   body_part %in% injured_hand ~ "hand",
                                   body_part %in% injured_upperbody ~ "upper body", 
                                   body_part %in% injured_lowerbody ~ "lower body", 
                                   body_part %in% injured_foot ~ "foot")
               ) %>% 
        filter(!body_part == "collar") %>% 
        select(age, gender, body_part, injured)

points <- 
        data.frame(x = c(50, 75, 140, 75, 85), 
                   y = c(50, 135, 270, 325, 480), 
                   size = c(9, 12, 3, 6, 3))
segments <- 
        data.frame(x = points$x + c(60, 70, 40, 50, 60), y = points$y) %>% 
        bind_rows(points) %>% 
        mutate(group = rep(1:5, 2))
texts <- 
        df_injures_body %>% 
        group_by(injured) %>% 
        count() %>% 
        ungroup() %>% 
        slice(3, 5, 2, 4, 1) %>% 
        mutate(n = scales::percent(n / 583, accuracy = 2), 
               text = glue::glue("{n}"), 
               x = segments$x[1:5] + 6, y = segments$y[1:5])
annotations <- 
        df_injures_body %>% 
        group_by(injured) %>% 
        count(body_part) %>% 
        top_n(3, n) %>% 
        ungroup() %>% 
        mutate(img = here::here(glue::glue("Week 37/icon/{body_part}.png"))) %>% 
        group_by(injured) %>% 
        arrange(injured, desc(n)) %>% 
        ungroup() %>% 
        mutate(col = ifelse(body_part %in% c("ankle", "arm", "head", "knee", "shoulder"),
                            "red", "grey"), 
               x = c(195, 235, 275, 220, 255, 290, 165, 217, 265, 185, 225, 265, 203, 242, 283), 
               y = c(rep(497, 3), rep(285, 3), rep(70, 3), rep(353, 3), rep(150, 3)))

icons <- 
        annotations %>% 
        pull(img) %>% 
        set_names(annotations$body_part) %>% 
        imap(~{
                if(.y %in% c("ankle", "arm", "head", "knee", "shoulder")) {
                        image_read(.x) %>% image_fill("red", "+100+200", fuzz = 100)
                } else {
                        image_read(.x) %>% image_fill("grey", "+100+200", fuzz = 100)
                }
        })
sub <- str_wrap("More than 500 safety accidents happened in amusement parks according to data.world. 
Graphic below shows common injuries occur on upper body such as shoulder, back and neck.", 35)

figure <- image_read(here::here("Week 37", "figure512.png")) %>% 
        image_crop("512x512+210") %>% 
        image_negate() %>%
        image_background("#ff8247") %>% 
        image_ggplot()

main <- figure +
        geom_point(data = points, aes(x = x, y = y, size = I(size)), color = "#ff4500", alpha = .4) +
        geom_path(data = segments, aes(x = x, y = y, group = group)) + 
        geom_text(data = texts, aes(x, y, label = text), color = "red", size = 8, hjust = 0.15) + 
        geom_text(data = annotations, aes(x = x, y = y, col = I(col), label = n), size = 5, vjust = 0.7)

ggdraw(main) + 
        draw_image(icons$head, 
                   x = .79, y = 1, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$face, 
                   x = .85, y = 1, width = .3, height = .045, hjust = 1.4, vjust = 2.6) +
        draw_image(icons$eye, 
                   x = .91, y = 1, width = .3, height = .045, hjust = 1.4, vjust = 2.6) +
        draw_image(icons$shoulder, 
                   x = .83, y = .84, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$back, 
                   x = .88, y = .84, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$neck, 
                   x = .93, y = .84, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$arm %>% image_flop(), 
                   x = .855, y = .575, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$wrist, 
                   x = .895, y = .575, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$finger %>% image_flop(), 
                   x = .935, y = .575, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$knee, 
                   x = .81, y = .45, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$hip, 
                   x = .86, y = .45, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$leg, 
                   x = .91, y = .45, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$ankle, 
                   x = .82, y = .16, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$foot,
                   x = .87, y = .16, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_image(icons$toe, 
                   x = .92, y = .16, width = .3, height = .045, hjust = 1.4, vjust = 2.6) + 
        draw_text(text = "Injuries in amusement parks", x = .01, y = .96, 
                  size = 23, hjust = 0, family = "Optima", color = "#ff8247", fontface = "bold") +
        draw_text(text = sub, x = .01, y = .9, 
                  hjust = 0, vjust = .9, size = 18, family = "Optima") +
        draw_text(text = "data: data.world | graphic: @chucc900\nicons: www.flaticons.com", 
                  x = 0.7, y = 0.06, hjust = 0, vjust = .4, family = "Optima")

ggsave(here::here("Week 37", "injuries.png"), width = 32.5, height = 19.9, units = "cm")
