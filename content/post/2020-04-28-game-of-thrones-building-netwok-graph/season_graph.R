time_limit <- 5

nodes <- got %>% 
  select(seasonNum, episodeNum, sceneNum, time, name) %>% 
  group_by(seasonNum, name) %>%
  summarise(time = sum(time) / 60) %>% 
  filter(time > time_limit) %>% 
  ungroup() %>% 
  group_by(seasonNum) %>% 
  group_split()


scenes_select <- got %>% 
  nest(names = name) %>% 
  mutate(id = 1:length(names)) %>% 
  select(id, seasonNum, time, names) %>% 
  unnest(names) %>% 
  group_by(seasonNum, name) %>% 
  mutate(total = sum(time) / 60) %>% 
  filter(total > time_limit) %>% 
  ungroup() %>% 
  select(-total) %>% 
  mutate(name_2 = name)

combination <- 
  scenes_select %>% 
  group_by(id, seasonNum, time) %>% 
  expand(name, name_2) %>% 
  ungroup() %>% 
  filter(name != name_2) %>% 
  group_by(seasonNum, name, name_2) %>% 
  summarise(time = sum(time)) %>% 
  rename(from = "name", to = "name_2", edge_time = time) %>% 
  ungroup() %>% 
  group_by(seasonNum) %>% 
  group_split()

graphs <- map2(nodes, combination, 
              ~tbl_graph(.x, .y, directed = FALSE) %>% 
                activate(edges) %>% 
                filter(!edge_is_multiple()) %>% 
                activate(nodes) %>% 
                mutate(group = group_infomap(weights = edge_time)))
graphs_plots <- graphs %>% 
  map2(1:8,
       ~.x %>% ggraph(layout = "kk", weights = edge_time) +
          geom_edge_density(aes(fill = edge_time)) +
          geom_edge_link(alpha = 0.1) +
          geom_node_point(aes(size = time, color = as_factor(group))) +
          geom_node_text(aes(label = name), repel = TRUE, size = 4) +
          labs(title = paste0("Game of Thrones : Season ", .y," Networking"),
               subtitle = glue("Node color correspond to cluster of characters,\\
                               characters that appear at least 5 minutes during the season")) +
          scale_color_got_d() +
          theme_graph() +
          theme(legend.position = "none",
                plot.title = element_text(family = "Cinzel")))

graphs_plots %>% 
  map2(1:8, ~ggsave(plot = .x,
                    here(paste0("static/plots/got/graph_season/season_", .y, ".pdf")),
                    device = cairo_pdf,
                    dpi = 320,
                    width = 20,
                    height = 20))

pdftools::pdf_combine(paste0(here("static/plots/got/graph_season"),
                             "/",
                             list.files(here("static/plots/got/graph_season"))),
                      here("static/plots/got/graph_season/all.pdf"))