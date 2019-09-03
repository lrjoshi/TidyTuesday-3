library(ggraph)
library(tidygraph)
library(tidyverse)
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
emperors <- filter(emperors, cause != "Unknown")
nodes <- map(c("rise", "cause") , ~{
        emperors %>% distinct(!!sym(.x)) %>% rename(label = .x)
}) %>% 
        tibble() %>% 
        unnest() %>% 
        distinct() %>% 
        rowid_to_column("id")

edges <- emperors %>% 
        count(rise, cause, name = "weight") %>% 
        ungroup() %>% 
        left_join(nodes, by = c("rise" = "label")) %>% 
        rename(from = id) %>% 
        left_join(nodes, by = c("cause" = "label")) %>% 
        rename(to = id) %>% 
        select(from, to, weight)

tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>% 
        activate(edges) %>% 
        arrange(desc(weight)) %>% 
        mutate(color = names(edgecol)[match(from, edgecol)]) %>% 
        activate(nodes) %>% 
        mutate(degree = centrality_degree()) %>% 
        ggraph(layout = "linear") + 
        geom_text(data = data.frame(), aes(x = 12, y = 4.5), label = "use ggraph()", size = 7) +
        geom_node_point(aes(color = label, size = degree), show.legend = FALSE) +
        geom_edge_arc(aes(width = weight, color = color), 
                      alpha = .5, show.legend = FALSE) +
        geom_node_text(aes(label = label), repel = TRUE, size = 3) +
        scale_edge_width(range = c(0.1, 2)) +
        theme_graph() +
        labs(title = str_to_title("the beginning and the end of emperors"),
             subtitle = str_wrap("Below is a network graph illustrating the rise and fall of emperors. It is distinguishable that quite a number of Roman emperors rose from birthright, and they fell because of the inevitable or assassinations.", 70),
             caption = "Data: Wikipedia via @geokaramanis | Graph by @chucc900") +
        ggsave(here::here("Week 34", "emperors_ggraph.png"), height = 5, width = 12)
