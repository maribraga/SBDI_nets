library(tidyverse)
library(bipartite)
library(tidygraph)
library(ggraph)

raw_data <- read.table("data/report_data.txt", sep = "\t", header = T, fileEncoding = "ISO-8859-1")

n_distinct(raw_data$VärdCommon)
n_distinct(raw_data$Commonname)

tree_taxo <- read.csv("data/tree_taxonomy.csv")
n_distinct(tree_taxo$Scientific.name)

data_trees <- filter(raw_data, VärdSciName %in% tree_taxo$Scientific.name)

table(data_trees$Nyttjande) %>% sort()

filter(data_trees, Nyttjande == "NULL") %>% View()
filter(data_trees, Nyttjande == "Livsrum") %>% View()
filter(data_trees, Nyttjande == "Parasitism") %>% View()

data_parasitism <- filter(data_trees, Nyttjande == "Parasitism") %>% 
  select("Scientificname", "VärdSciName", "tal1") %>% 
  rename("to" = "Scientificname",
         "from" = "VärdSciName", 
         "weight" = "tal1")

n_distinct(data_parasitism$to)
n_distinct(data_parasitism$from)

nodes_para <- data.frame(name = c(unique(data_parasitism$to), unique(data_parasitism$from)),
                         type = c(rep("parasite",n_distinct(data_parasitism$to)), 
                                  rep("host",n_distinct(data_parasitism$from))))

parasit_graph <- tbl_graph(nodes = nodes_para, edges = data_parasitism)

ggraph(parasit_graph, layout = "kk") + 
  geom_edge_link(aes(edge_width = weight), color = "grey30", alpha = 0.8) + 
  geom_node_point(aes(shape = type, color = type), size = 3) +
  scale_color_manual(values = c("chartreuse3","deepskyblue3")) +
  scale_edge_width(range = c(0.7,1.4))


