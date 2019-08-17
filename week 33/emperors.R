library(tidyverse)
library(ggimage)
library(lubridate)
library(here)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
before_bc <- c("Augustus", "Tiberius", "Claudius", "Galba")

emperors_years <- emperors %>% 
        mutate_if(is.Date, list(year = year)) %>% 
        mutate_at(vars(reign_start_year), 
                  ~ifelse(name == "Augustus", -(.), .)) %>% 
        mutate(birth_year = ifelse(name %in% before_bc, -birth_year, birth_year))
emperors_missing_years <- emperors_years %>% 
        filter(is.na(birth)) %>% 
        mutate(birth_year = case_when(name == "Florian" ~ 232,
                                      name == "Numerian" ~ 254, 
                                      name == "Carinus" ~ 257,
                                      name == "Severus II" ~ 250,
                                      name == "Vetranio" ~ 325))
plot_data <- emperors_years %>% 
        filter(!is.na(birth)) %>% 
        bind_rows(emperors_missing_years)

dynasties <- plot_data %>% 
        group_by(dynasty) %>% 
                summarize(reign_start_year = min(reign_start_year),
                          reign_end_year = max(reign_end_year))

my_palette <- set_names(colorRampPalette(c("#018790", "#2B2726"))(8), unique(plot_data$dynasty))

timeline <- ggplot(plot_data, aes(y = 0)) +
        geom_segment(aes(x = reign_start_year, xend = reign_end_year, yend = 0, color = dynasty), 
                     size = 4) +
        scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), 
                           labels = c("62BC", "AD", "100AD", "200AD", "300AD", "400AD")) +
        expand_limits(x = c(-62, 450)) +
        scale_color_manual("Dynasty", values = my_palette, breaks = names(my_palette)) +
        labs(x = NULL, y = NULL, caption = "Data: Wikipedia via @geokaramanis | Reference from @jkaupp") +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              legend.position = "none")

# segment bars
bars <- ggplot(plot_data, aes(y = reorder(name, reign_start_year))) +
        geom_segment(aes(x = birth_year, xend = death_year, yend = name), 
                     size = 2, color = "grey90") +
        geom_segment(aes(x = reign_start_year, xend = reign_end_year, y = name, 
                         yend = name, color = dynasty), size = 2) +
        geom_segment(data = filter(plot_data, reign_start_year == reign_end_year), 
                     aes(x = reign_start_year - 0.5, xend = reign_end_year + .5, 
                         y = name, yend = name, color = dynasty), size = 2) +
        geom_text(aes(x = death_year, label = name), hjust = 0, size = 2, nudge_x = 2.5)

# annotations and arrows
bars <- bars +
        geom_curve(x = -40, xend = -26, y = 3, yend = 1, 
                   arrow = arrow(length = unit(0.07, "inch")), curvature = .7, color = "grey25") +
        annotate("text", x = -40, y = 3.8, label = "Longest in reign of 40 years", size = 2.4) +
        geom_curve(x = 430, xend = 418, y = 64, yend = 67, 
                   arrow = arrow(length = unit(.07, "inch")), color = "grey25") +
        annotate("text", x = 428, y = 63.6, label = "The youngest emperor", size = 2.4)

# colors, captions, and title
bars <- bars +
        scale_color_manual("Dynasty", values = my_palette, breaks = names(my_palette)) +
        scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), 
                           labels = c("62BC", "AD", "100AD", "200AD", "300AD", "400AD")) +
        expand_limits(x = c(-62, 450)) +
        labs(x = NULL, y = NULL,
             title = str_to_title("A tip of the iceberg of the Roman Emperors"),
             subtitle = str_wrap("This graph is reproduced from the work by Jake Kaupp.", 100)) +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              legend.position = c(.8, 0.5),
              legend.background = element_rect(fill = "white", colour = "white")
              )

p <- patchwork::wrap_plots(bars, timeline, ncol = 1, heights = c(0.9, 0.1))

ggsave(here("Week 33", "emperors.png"), p, width = 10, height = 8)



# network
library(geomnet)
rise_fall <- select(plot_data, rise, cause) %>% 
        filter(cause != "Unknown") %>% 
        add_count(rise, cause) %>% 
        group_by(rise) %>% 
        mutate(nn = n()) %>% 
        as.data.frame()

# network df
## distinct labels for edges and vertices
rise_label <- rise_fall %>% select(rise) %>% distinct() %>% rename(label = rise)
cause_label <- rise_fall %>% select(cause) %>% distinct() %>% rename(label = cause)

# vertices df
vertices <- full_join(rise_label, cause_label, by = "label") %>% 
        mutate(id = row_number()) %>% 
        select(id, everything())
rise_fall <- rename(rise_fall, weight = n, size = nn)

# edges df
edges <- left_join(rise_fall, vertices, by = c("rise" = "label")) %>% 
        rename(from = id)
edges <- edges %>% left_join(vertices, by = c("cause" = "label")) %>% 
        rename(to = id)

# network df
empnet <- merge(edges, vertices, by.x = "rise", by.y = "label", all = TRUE)
set.seed(1234)
netgraph <- ggplot(empnet, aes(from_id = rise, to_id = cause, 
       color = rise, 
       linewidth = weight / 10)) +
        geom_net(aes(size = ifelse(is.na(size), 1, size)),
                 layout.alg = "circle",
                 show.legend = FALSE, 
                 directed = TRUE, 
                 labelon = TRUE,
                 fontsize = 3, 
                 arrowsize = .5,
                 labelcolour = "black", 
                 ecolour = "grey60",
                 repel = TRUE) + 
        labs(title = str_to_title("the beginning and the end of emperors"),
             subtitle = str_wrap("Below is a network graph illustrating the rise and fall of emperors. It is distinguishable that quite a number of Roman emperors rose from birthright, and they fell because of the inevitable or assassinations.", 70),
             caption = "Data: Wikipedia via @geokaramanis | Graph by @scofirroto") +
        theme_net() +
        theme(text = element_text(family = "serif", size = 12),
              title = element_text(face = "bold"),
              panel.background = element_rect(fill = "beige"))
ggsave(here("Week 33", "network.png"), netgraph, width = 7.5, height = 8)
















