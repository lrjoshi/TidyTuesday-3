# A calendar for nuclear activity reported by SIPRI
library(tidyverse)
library(gganimate)
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
calendars$USA <- calendars$USA %>% filter(!(year == 1945 & month < 7))
calendar <- map_df(calendars, rbind) %>% arrange(year) %>% filter(!year > 1996)

annodat <- function(direction = "right", year, month, stretch, 
                    group, country) {
        
        x1 <- year - .5; x2 <- year + .5
        y1 <- month - .5; y2 <- month + .5
        x <- c(x1, x2, x2, x1, x1, x1)
        y <- switch(direction, 
                    "left" = c(y1, y1, y2, y2, y1, stretch),
                    "right" = c(y2, y2, y1, y1, y2, stretch)
                    )

        d <- tibble(x, y, group = group, country = country)
        d
}

china_tests <- list(direction = c("right", "left", "left", "right", "left", "right"), 
                    year = c(1964, 1967, 1974, 1976, 1980, 1996), 
                    month = c(10, 6, 6, 11, 10, 7), 
                    stretch = c(13, -1, -1, 13, -1, 13),
                    group = 1:6,
                    country = "China"
                    )
france_tests <- list(direction = c("left", "right", "right", rep("left", 3)),
                     year = c(1960, 1961, 1966, 1968, 1975, 1996),
                     month = c(2, 11, 7, 8, 6, 1),
                     stretch = c(-1, 13, 13, -1, -1, -1),
                     group = 1:6,
                     country = "France"
                     )
ussr_tests <- list(direction = c(rep("right", 4), "left", "right"),
                   year = c(1949, 1953, 1955, 1961, 1965, 1990), 
                   month = c(8, 8, 11, 10, 1, 10), 
                   stretch = c(13, 13, 13, 13, -2, 13),
                   group = 1:6,
                   country = "Soviet Union"
                   )
uk_tests <- list(direction = c("left", "right", "left", "left", "right"), 
                 year = c(1952, 1953, 1956, 1958, 1991),
                 month = c(10, 10, 5, NA, 11),
                 stretch = c(8, 13, -1, -1, 13),
                 group = 1:5,
                 country = "United Kingdom"
                 )
usa_tests <- list(direction = c("right", "right", "left", "right", 
                                "left", "left", "left", "right"), 
                  year = c(1945, 1946, 1951, 1952, 1954, 1957, 1962, 1992), 
                  month = c(7, 7, 5, 11, 3, NA, 5, 9),
                  stretch = c(5, 13, -1, 13, -2, -2, -1, 13),
                  group = 8:1, 
                  country = "USA"
                  )

annodata <- list(china_tests, france_tests, ussr_tests, uk_tests, usa_tests) %>% 
                         map(pmap, annodat) %>% reduce(c) %>% map_df(rbind)

addendum <- annodata %>% 
        group_by(country, group) %>% 
        group_split() %>%
        .[c(22, 26)] %>% 
        set_names(paste("Operation", c("Grapple", "Plumbob")))

addendum$`Operation Grapple` <- addendum$`Operation Grapple` %>% 
        mutate(x = c(1956, 1958, 1958, 1956, 1956, 1956) + .5,
               y = c(c(12, 12, 0, 0, 12) + .5, 13))
addendum$`Operation Plumbob` <- addendum$`Operation Plumbob` %>% 
        mutate(y = c(c(10, 10, 4, 4, 10) + 0.5, -2))
addendum <- addendum %>% tibble() %>% unnest()

annodata <- annodata %>% 
        group_by(country, group) %>% 
        group_split() %>% 
        .[-c(22, 26)] %>% 
        tibble() %>% 
        unnest() %>% 
        bind_rows(addendum)

annotext <- annodata %>% 
        group_by(country, group) %>% 
        slice(6) %>% 
        ungroup() %>% 
        mutate(text = as.character(1:31),
               y = ifelse(text %in% c("19", "31"), y - .5, ifelse(y > 0, y + .5, y - .5))) %>% 
        select(x, y, group, country, text)

(p <- ggplot() +
        geom_tile(data = filter(calendar, !year > 1996), aes(year, month, fill = n), col = "white") + 
        geom_path(data = annodata, aes(x, y, group = group, color = country), size = 1) + 
        geom_text(data = annotext, aes(x, y, label = text, color = country), size = 5) +   
        scale_fill_gradientn(colours = radioactive_pal("hot")(5), breaks = c(1, 5, 8, 15, 20)) +
        guides(color = FALSE, 
               fill = guide_legend(title = "Tests/Explosions", reverse = TRUE, 
                                   direction = "horizontal", title.position = "top",
                                   keyheight = 1)) +
        scale_color_radioactive(palette = "mixed") +        
        scale_x_reverse(breaks = c(1996:1945)) +
        scale_y_continuous(breaks = 1:12, labels = month.abb,
                           limits = c(-4, 15), expand = c(-0.05, -.2)) + 
        coord_flip() +
        facet_wrap(~country, scales = "free_y") + 
        labs(x = "", y = "", 
             title = "The Nuclear Tests Calendar", 
             subtitle = "Below is the calendar chart during the period between\nthe first and the last test in each following nuclear state:\nChina, France, Soviet Union, United Kingdom and United States.", 
             caption = "Data: Stockholm Internation Peace Research Institute | Wikipedia\nGraph: @chucc900") +
        theme_minimal(base_size = 16, base_family = "Baskerville") +
        theme(text = element_text(color = "#FFFFFF"),
              plot.background = element_rect(fill = "#000000"),
              plot.title = element_text(size = 40, color = "#4BE3AC", face = "bold"),
              plot.subtitle = element_text(size = 30, color = "#FFFFFF"),
              plot.caption = element_text(size = 30),
              plot.margin = margin(20, 20, 10, 10),
              panel.background = element_rect(fill = "#000000"),
              axis.text.y = element_text(color = "#FFFFFF"),
              axis.text.x = element_text(angle = 90, color = "#FFFFFF", size = 20),
              axis.ticks = element_blank(),
              strip.text = element_text(color = "#FFFFFF", size = 20),
              strip.background = element_blank(),
              panel.grid = element_blank(),
              legend.position = c(.85, 1.1), 
              legend.background = element_blank(),
              legend.text = element_text(color = "#FFFFFF", size = 30),
              legend.title = element_text(color = "#FFFFFF", face = "bold", size = 30))
        )



ggsave(here("Week 34", "nuclear_calendar.png"), p, width = 20, height = 20)

# draw calendar
briefs <- readr::read_csv(here("Week 34", "nuclear_test_brief.csv"))
briefs <- briefs %>% 
        filter(grepl("^\\d+$", date)) %>% 
        mutate(day = as.numeric(substring(date, nchar(date) - 1, nchar(date))),
               date = format(lubridate::ymd(date), "%b %Y")) %>% 
        bind_rows(filter(briefs, !grepl("^\\d+$", date))) %>% 
        mutate(content = gsub(";", ",", content), 
               project = paste(id, project))

cal <- tibble(
        x0 = c(0, 0), y0 = c(.7, 0), x1 = c(1, 1), y1 = c(1, .7),
        part = c("h", "b")
)
ani <- ggplot(cal) +
        geom_rect(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1, fill = part), show.legend = F) + 
        # yearmon
        geom_text(data = briefs, aes(x = .5, y = .85, label = date), size = 20, color = "#FFFFFF") + 
        # day
        geom_text(data = briefs, aes(x = .5, y = .6, label = day), size = 35, na.rm = TRUE, color = "#FFFFFF") +
        # project name
        geom_text(data = briefs, aes(x = .05, y = .4, label = project, color = country), 
                  size = 8, hjust = 0, vjust = .5, show.legend = F, family = "Baskerville", fontface = "bold") + 
        # project brief
        geom_text(data = briefs, aes(x = .05, y = .2, label = str_wrap(content, 40)), 
                  size = 6, color = "#FFFFFF", hjust = 0, vjust = 0.2, family = "Baskerville", fontface = "bold") +
        scale_fill_manual(values = c("#000000", "#FF0000")) +
        scale_color_manual(values = radioactive_pal("mixed")(5)) +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(x = "", y = "") +
        theme(plot.background = element_rect(fill = "#000000"), 
              panel.background = element_rect(fill = "#000000", color = "#FFFFFF", size = 2), 
              plot.margin = margin(8, 15, 0, 0), 
              panel.grid = element_blank(), 
              axis.ticks = element_blank(), 
              axis.text = element_blank()) +
        transition_manual(frames = id)
anim_save(here("Week 34", "nuclear_calendar.gif"), 
          animate(ani, nframes = 31, fps = 1, duration = 46.5))
