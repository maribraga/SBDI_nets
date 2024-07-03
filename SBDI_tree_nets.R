
library(tidyverse)
library(bipartite)
library(tidygraph)
library(ggraph)


# Read data
# interaction data (fix names that start with space)
raw_data <- read.table("data/report_data.txt", sep = "\t", header = T, fileEncoding = "ISO-8859-1") %>% 
  mutate(Scientificname = str_replace(Scientificname,"^ ",""),
         VärdSciName = str_replace(VärdSciName, "ë", "e"))

# tree taxonomic data from the report's appendix 2
tree_taxo <- read.csv("data/tree_taxonomy.csv") %>% 
  filter(Taxonomic.rank %in% c("Genus", "Species")) %>% 
  rename(Species = "Scientific.name") %>% 
  mutate(Species = str_replace(Species, "ë", "e"),
         Genus = str_extract(Species,  "^\\w+"), .after = Species)
  
tree_spp <- tree_taxo %>% 
  filter(Taxonomic.rank == "Species") %>% 
  pull(Species) %>% 
  sort()

tree_genera <- tree_taxo %>% 
  pull(Genus) %>%
  unique() %>% 
  sort()

# attacker taxonomic data 
attacker_taxo <- raw_data %>% 
  select("Scientificname","Family", "OrganismGroup") %>% 
  unique() %>% 
  rename(Species = "Scientificname") %>% 
  mutate(Genus = str_extract(Species,  "^\\w+"), .after = Species)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_trees <- filter(raw_data, VärdSciName %in% tree_taxo$Species) %>% 
  select("Scientificname", "VärdSciName", "Nyttjande", "blankettnamn", "tal1") %>% 
  rename("parasite" = "Scientificname",
         "host" = "VärdSciName", 
         "type" = "Nyttjande",
         "tree_part" = "blankettnamn",
         "dependency" = "tal1") %>% 
  unique()

data_trees_spp <- data_trees %>% 
  filter(host %in% tree_spp)

data_trees_genus <- data_trees %>% 
  mutate(host_genus = str_extract(host,  "^\\w+"), .after = host) %>% 
  select(-host) %>% 
  unique()

#setdiff(data_trees_genus$host_genus, tree_genera)
#setdiff(tree_genera, data_trees_genus$host_genus)

# number of attackers by type of interaction 
table(data_trees$type) %>% sort(decreasing = T)
table(data_trees$tree_part) %>% sort(decreasing = T)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_para_spp <- filter(data_trees_spp, type == "Parasitism")

# n_distinct(data_para_spp$parasite)
# n_distinct(data_para_spp$host)
# nrow(data_para_spp)
# n_distinct(data_para_spp)

data_para_spp %>% 
  group_by(parasite, host) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# remove interactions that are recorded as both 1 and 2
duplicates_para <- data_para_spp %>% 
  select(-tree_part) %>% 
  unique() %>% 
  group_by(parasite, host) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# there are none, just different tree parts

data_para_spp_simple <- data_para_spp %>% 
  select(-tree_part) %>% 
  unique()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with ggraph

nodes_para_spp <- data.frame(name = c(unique(data_para_spp_simple$parasite), unique(data_para_spp_simple$host)),
                         type = c(rep("parasite",n_distinct(data_para_spp_simple$parasite)), 
                                  rep("host",n_distinct(data_para_spp_simple$host))))

para_spp_graph <- tbl_graph(nodes = nodes_para_spp, edges = data_para_spp_simple)

ggraph(para_spp_graph, layout = "kk") + 
  geom_edge_link(aes(edge_width = dependency), color = "grey50", alpha = 0.8) + 
  geom_node_point(aes(shape = type, color = type), size = 4) +
  geom_node_text(aes(label = name), repel = T, size = 2) +
  scale_color_manual(values = c("chartreuse3","deepskyblue3")) +
  scale_shape_manual(values = c("square", "circle")) +
  scale_edge_width(range = c(0.7,1.4)) +
  theme_graph()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## ggraph(para_spp_graph, layout = "auto") +
##   geom_edge_link(aes(edge_width = dependency), color = "grey50", alpha = 0.8) +
##   geom_node_point(aes(shape = type, color = type), size = 3) +
##   scale_color_manual(values = c("chartreuse3","deepskyblue3")) +
##   scale_shape_manual(values = c("square", "circle")) +
##   scale_edge_width(range = c(0.7,1.4))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite

adjm_para_spp <- frame2webs(mutate(data_para_spp_simple, webID = 1), varnames = c("host","parasite", "type", "dependency"))[[1]]

#plotweb(adjm_para_spp, text.rot = 90, y.lim = c(0, 2))
visweb(adjm_para_spp, type = "diagonal", labsize = 4)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## # modularity
## mod_para <- computeModules(adjm_parasitism)
## plotModuleWeb(mod_para)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_para_gen <- filter(data_trees_genus, type == "Parasitism")

n_distinct(data_para_gen$parasite)
n_distinct(data_para_gen$host_genus)
nrow(data_para_gen)
n_distinct(data_para_gen)

data_para_gen %>% 
  group_by(parasite, host_genus) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# remove interactions that are recorded as both 1 and 2
duplicates_para <- data_para_gen %>% 
  select(-tree_part) %>% 
  unique() %>% 
  group_by(parasite, host_genus) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# there are none, just different tree parts

data_para_gen_simple <- data_para_gen %>% 
  select(-tree_part) %>% 
  unique()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with ggraph

nodes_para_gen <- data.frame(name = c(unique(data_para_gen_simple$parasite), unique(data_para_gen_simple$host_genus)),
                             type = c(rep("parasite",n_distinct(data_para_gen_simple$parasite)), 
                                      rep("host",n_distinct(data_para_gen_simple$host_genus))))

para_gen_graph <- tbl_graph(nodes = nodes_para_gen, edges = data_para_gen_simple)

ggraph(para_gen_graph, layout = "kk") + 
  geom_edge_link(aes(edge_width = dependency), color = "grey50", alpha = 0.8) + 
  geom_node_point(aes(shape = type, color = type), size = 3) +
  geom_node_text(aes(label = name), repel = T, size = 2) +
  scale_color_manual(values = c("chartreuse3","deepskyblue3")) +
  scale_shape_manual(values = c("square", "circle")) +
  scale_edge_width(range = c(0.7,1.4)) +
  theme_graph()



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## ggraph(para_gen_graph, layout = "sugiyama") +
##   geom_edge_link(aes(edge_width = dependency), color = "grey50", alpha = 0.8) +
##   geom_node_point(aes(shape = type, color = type), size = 3) +
##   #geom_node_text(aes(label = name), angle = 90, hjust = 1) +
##   scale_color_manual(values = c("chartreuse3","deepskyblue3")) +
##   scale_shape_manual(values = c("square", "circle")) +
##   scale_edge_width(range = c(0.7,1.4)) +
##   theme_graph()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite

adjm_para_gen <- frame2webs(mutate(data_para_gen_simple, webID = 1), varnames = c("host_genus","parasite", "type", "dependency"))[[1]]

#plotweb(adjm_para_gen, text.rot = 90, y.lim = c(0, 2))
#visweb(adjm_para_gen, type = "diagonal", labsize = 4)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_herbivory <- filter(data_trees, Nyttjande == "Föda") %>%
  select("Scientificname", "VärdSciName", "tal1") %>%
  rename("herbivore" = "Scientificname",
         "host" = "VärdSciName",
         "weight" = "tal1")

n_distinct(data_herbivory$herbivore)
n_distinct(data_herbivory$host)
nrow(data_herbivory)
n_distinct(data_herbivory)

duplicates_herb <- data_herbivory %>%
  group_by(herbivore, host, weight) %>%
  summarise(n = n()) %>%
  filter(n > 1)

nrow(duplicates_herb)

raw_data %>%
  filter(Scientificname == "Aleurocystidiellum disciforme",
         Nyttjande == "Föda")

raw_data %>%
  filter(Scientificname == "Caloptilia cuculipennella",
         Nyttjande == "Föda")

data_herbivory <- unique(data_herbivory)

duplicates_herb <- data_herbivory %>%
  group_by(herbivore, host) %>%
  summarise(n = n()) %>%
  filter(n > 1)

View(duplicates_herb)

data_herbivory <- data_herbivory[-which(data_herbivory$herbivore == duplicates_herb$herbivore &
                                        data_herbivory$host == duplicates_herb$host &
                                        data_herbivory$weight == 1),]

to_remove <- c()
for(i in 1:nrow(duplicates_herb)) {
  row <- which(data_herbivory$herbivore == duplicates_herb$herbivore[i] &
                              data_herbivory$host == duplicates_herb$host[i] &
                              data_herbivory$weight == 1)
  to_remove <- c(to_remove, row)
}

data_herbivory <- data_herbivory[-to_remove,]
nrow(data_herbivory) == n_distinct(data_herbivory)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with ggraph

nodes_herb <- data.frame(name = c(unique(data_herbivory$herbivore), unique(data_herbivory$host)),
                         type = c(rep("herbivore",n_distinct(data_herbivory$herbivore)),
                                  rep("host",n_distinct(data_herbivory$host))))

herb_graph <- tbl_graph(nodes = nodes_herb, edges = data_herbivory)

ggraph(herb_graph, layout = "kk") +
  geom_edge_link(aes(edge_width = weight), color = "grey30", alpha = 0.8) +
  geom_node_point(aes(shape = type, color = type), size = 2) +
  scale_color_manual(values = c("deepskyblue3","chartreuse3")) +
  scale_edge_width(range = c(0.5,1))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite

adjm_herb <- frame2webs(mutate(data_herbivory, webID = 1), varnames = c("host","herbivore", "webID", "weight"))[[1]]

#plotweb(adjm_herb, text.rot = 90, y.lim = c(0, 2), labsize = 0.7)

#visweb(adjm_herb, prednames = F, preynames = F)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## # modularity
## mod_herb <- computeModules(adjm_herb)
## plotModuleWeb(mod_herb, displayBlabels = F, labsize = 0.4)
## 


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_herb_fam <- filter(data_trees, Nyttjande == "Föda") %>%
  select("Family", "VärdSciName", "tal1") %>%
  rename("herb_fam" = "Family",
         "host" = "VärdSciName",
         "weight" = "tal1")

n_distinct(data_herb_fam$herb_fam)
n_distinct(data_herb_fam$host)
nrow(data_herb_fam)
n_distinct(data_herb_fam)

data_herb_fam <- unique(data_herb_fam)

duplicates_herb_fam <- data_herb_fam %>%
  group_by(herb_fam, host) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# if there are interaction with weight 1 and 2, keep only 2
to_remove_fam <- c()
for(i in 1:nrow(duplicates_herb_fam)) {
  row <- which(data_herb_fam$herb_fam == duplicates_herb_fam$herb_fam[i] &
               data_herb_fam$host == duplicates_herb_fam$host[i] &
               data_herb_fam$weight == 1)
  to_remove_fam <- c(to_remove_fam, row)
}

data_herb_fam <- data_herb_fam[-to_remove_fam,]
n_distinct(data_herb_fam$herb_fam)
n_distinct(data_herb_fam$host)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with ggraph

nodes_herb_fam <- data.frame(name = c(unique(data_herb_fam$herb_fam), unique(data_herb_fam$host)),
                             type = c(rep("herb_fam",n_distinct(data_herb_fam$herb_fam)),
                                      rep("host",n_distinct(data_herb_fam$host))))

herb_fam_graph <- tbl_graph(nodes = nodes_herb_fam, edges = data_herb_fam)

ggraph(herb_fam_graph, layout = "auto") +
  geom_edge_link(aes(edge_width = weight), color = "grey30", alpha = 0.8) +
  geom_node_point(aes(shape = type, color = type), size = 3) +
  scale_color_manual(values = c("deepskyblue3","chartreuse3")) +
  scale_edge_width(range = c(0.5,1))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite

adjm_herb_fam <- frame2webs(mutate(data_herb_fam, webID = 1), varnames = c("host","herb_fam", "webID", "weight"))[[1]]

plotweb(adjm_herb_fam, text.rot = 90, y.lim = c(0, 2), labsize = 0.7)

visweb(adjm_herb_fam, labsize = 4)
visweb(adjm_herb_fam, labsize = 4, type = "diagonal")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## # modularity
## mod_herb_fam <- computeModules(adjm_herb_fam)
## plotModuleWeb(mod_herb_fam, labsize = 0.4)
## 


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_herb_spp_fam <- filter(data_trees, Nyttjande == "Föda") %>%
  select("Scientificname", "Family", "VärdSciName") %>%
  rename("herbivore" = "Scientificname",
         "herb_fam" = "Family",
         "host" = "VärdSciName") %>%
  unique() %>%
  group_by(herb_fam, host) %>%
  summarise(n_spp = n()) %>%
  mutate(logn = round(log(n_spp) + 1))



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite
adjm_herb_spp_fam <- frame2webs(mutate(data_herb_spp_fam, webID = 1), varnames = c("host","herb_fam", "webID", "n_spp"))[[1]]
adjm_herb_spp_fam_log <- frame2webs(mutate(data_herb_spp_fam, webID = 1), varnames = c("host","herb_fam", "webID", "logn"))[[1]]

#plotweb(adjm_herb_spp_fam_log, text.rot = 90, y.lim = c(0, 2), labsize = 0.7)
visweb(adjm_herb_spp_fam_log, labsize = 4)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## # modularity
## mod_herb_spp_fam <- computeModules(adjm_herb_spp_fam_log)
## plotModuleWeb(mod_herb_spp_fam, labsize = 0.4)
## 


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
shared_spp <- data_trees %>%
  select("Scientificname", "Family", "VärdSciName", "Nyttjande") %>%
  rename("herbivore" = "Scientificname",
         "herb_fam" = "Family",
         "host" = "VärdSciName",
         "type" = "Nyttjande") %>%
  unique()

adjm_shared <- frame2webs(shared_spp, varnames = c("host","herbivore", "type"))

one_mode_shared_parasites <- as.one.mode(adjm_shared$Parasitism, project = "lower")
image(one_mode_shared_parasites)

one_mode_shared_herbivores <- as.one.mode(adjm_shared$Föda, project = "lower")
image(one_mode_shared_herbivores)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with ggraph

nodes_herb_shared <- data.frame(name = c(unique(rownames(one_mode_shared_herbivores))))
shared_herb_graph <- tbl_graph(nodes = nodes_herb_shared, edges = web2edges(one_mode_shared_herbivores, is.one.mode = TRUE))

ggraph(shared_herb_graph, layout = "kk") +
  geom_edge_link(aes(edge_width = edge.weights), color = "grey30", alpha = 0.8) +
  geom_node_point(size = 3) +
  scale_edge_width(range = c(0.5,5))


nodes_para_shared <- data.frame(name = c(unique(rownames(one_mode_shared_parasites))))
shared_para_graph <- tbl_graph(nodes = nodes_para_shared, edges = web2edges(one_mode_shared_parasites, is.one.mode = TRUE))

ggraph(shared_para_graph, layout = "kk") +
  geom_edge_link(aes(edge_width = edge.weights), color = "grey50", alpha = 0.8) +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), nudge_y = 0.2) +
  scale_edge_width(range = c(0.5,3))



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_coleoptera <- data_herbivory <- filter(data_trees, Nyttjande == "Föda") %>%
  filter(OrganismGroup == "Skalbaggar") %>%
  select("Scientificname", "VärdSciName", "tal1") %>%
  rename("herbivore" = "Scientificname",
         "host" = "VärdSciName",
         "weight" = "tal1")

data_coleoptera <- unique(data_coleoptera)

# with bipartite

adjm_coleo <- frame2webs(mutate(data_coleoptera, webID = 1), varnames = c("host","herbivore", "webID", "weight"))[[1]]

plotweb(adjm_coleo, text.rot = 90, y.lim = c(0, 2), labsize = 0.7)

visweb(adjm_coleo, prednames = F, preynames = F, type= "diagonal")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_lepi <- data_herbivory <- filter(data_trees, Nyttjande == "Föda") %>%
  filter(OrganismGroup == "Fjärilar") %>%
  select("Scientificname", "VärdSciName", "tal1") %>%
  rename("herbivore" = "Scientificname",
         "host" = "VärdSciName",
         "weight" = "tal1")

data_lepi <- unique(data_lepi)

# with bipartite

adjm_lepi <- frame2webs(mutate(data_lepi, webID = 1), varnames = c("host","herbivore", "webID", "weight"))[[1]]

plotweb(adjm_lepi, text.rot = 90, y.lim = c(0, 2), labsize = 0.7)

visweb(adjm_lepi, prednames = F, preynames = F, type= "diagonal")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
data_myco <- filter(data_trees, Nyttjande == "Mykorrhiza") %>%
  select("Scientificname", "VärdSciName", "tal1") %>%
  rename("myco" = "Scientificname",
         "host" = "VärdSciName",
         "weight" = "tal1")

n_distinct(data_myco$myco)
n_distinct(data_myco$host)
nrow(data_myco)
n_distinct(data_myco)

data_myco <- unique(data_myco)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# with bipartite

adjm_myco <- frame2webs(mutate(data_myco, webID = 1), varnames = c("host","myco", "webID", "weight"))[[1]]

plotweb(adjm_myco, text.rot = 90, y.lim = c(0, 2), labsize = 0.4)

visweb(adjm_myco, labsize = 4)
visweb(adjm_myco, labsize = 4, type = "diagonal")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
filter(data_trees, Nyttjande == "Livsrum") %>%
  sample_n(200) %>%
  View()

