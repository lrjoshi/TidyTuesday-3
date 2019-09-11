library(tidyverse)
library(lubridate)
library(rvest)
library(magick)
library(cowplot)

df_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv", 
                               na = c("n/a", "NA", "N/A"))
df_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

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
        mutate(gender = toupper(gender), # fixed gender 'm'
               age = case_when(age == "30s" ~ 35, 
                               age == "mid-60s" ~ 65, 
                               age == "0" ~ NA_real_, # assume age 0 is missing data instead of actual new born
                               TRUE ~ as.numeric(age))
        ) %>% 
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

points <- data.frame(x = c(50, 75, 140, 75, 85), 
                     y = c(50, 135, 270, 325, 480))
segments <- data.frame(x = points$x + c(60, 70, 40, 50, 60), y = points$y) %>% 
        bind_rows(points) %>% 
        mutate(group = rep(1:5, 2))

figure <- image_read(here::here("Week 37", "figure512.png")) %>% 
        image_crop("512x512+210") %>% 
        image_background("grey20") %>% 
        image_negate()

image_ggplot(figure) +
        theme_classic() +
        geom_point(data = points, aes(x = x, y = y), size = 3, color = "red", alpha = .4) +
        geom_path(data = segments, aes(x = x, y = y, group = group))



        