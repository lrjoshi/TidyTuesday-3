# A calendar for nuclear activity reported by SIPRI
library(tidyverse)
library(here)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

source(here("Week 34", "scale_radioactive.R"))

calendar <- tibble(year = rep(1945:1998, each = 84), 
                     month = rep(1:12, 7*54),
                     country = rep(rep(unique(nuclear_explosions$country), each = 12), 54)) %>% 
        mutate(country = case_when(country == "PAKIST" ~ "Pakistan",
                                   country == "USA" ~ "USA",
                                   country == "UK" ~ "United Kingdom",
                                   country == "USSR" ~ "Soviet Union",
                                   T ~ str_to_title(country)))
nuc_timeline <- nuclear_explosions %>% 
        select(date_long, country) %>% 
        mutate(date_long = lubridate::ymd(date_long), 
               country = case_when(country == "PAKIST" ~ "Pakistan",
                                   country == "USA" ~ "USA",
                                   country == "UK" ~ "United Kingdom",
                                   country == "USSR" ~ "Soviet Union",
                                   T ~ str_to_title(country)),
               year = lubridate::year(date_long),
               month = lubridate::month(date_long)
               ) 

calendars <- calendar %>% 
        left_join(nuc_timeline, by = c("year", "month", "country")) %>% 
        mutate(n = ifelse(!is.na(date_long), 1, date_long)) %>% 
        group_by(year, month, country) %>% 
        summarize(n = sum(n)) %>% 
        ungroup() %>% 
        filter(!country %in% c("Pakistan", "India"), !(year == 1945 & month < 7)) %>% 
        group_by(country) %>% 
        group_split()
calendars <- set_names(calendars, c("China", "France", "Soviet Union", "United Kingdom", "USA"))

# filter observations before the first nuclear explosion took place in each country except US
calendars$China <- calendars$China %>% 
        filter(year >= 1964) %>% 
        filter(!(year == 1964 & month < 10))
calendars$France <- calendars$France %>% 
        filter(year >= 1960) %>% 
        filter(!(year == 1960 & month == 1))
calendars$`Soviet Union` <- calendars$`Soviet Union` %>% 
        filter(year >= 1949) %>% 
        filter(!(year == 1949 & month < 8))
calendars$`United Kingdom` <- calendars$`United Kingdom` %>% 
        filter(year >= 1952) %>% 
        filter(!(year == 1952 & month < 10))
calendar <- map_df(calendars, rbind) %>% arrange(year)

calendar %>% 
        ggplot(aes(year, month, fill = n)) +
        geom_tile() +
        scale_fill_radioactive(discrete = FALSE, palette = "hot") +
        scale_x_reverse() +
        facet_wrap(.~country) +
        coord_flip()
