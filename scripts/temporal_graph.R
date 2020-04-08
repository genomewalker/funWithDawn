library(tidyverse)
library(tidygraph)
library(igraph)
library(readxl)
library(intergraph)
library(networkDynamic)
library(ndtv)
library(wesanderson)
library(tidytex)

# From https://kateto.net/network-visualization

# Read excel file
input_file <- "data/90_articles_BB.xlsx"
xls <- read_excel(input_file) %>%
  select(-TotalLinks, -URL) %>%
  rename(time = ID) %>%
  mutate(node1 = row_number())

nodes <- xls %>% select(node1, Article) %>%
  distinct() %>%
  #column_to_rownames("node1") %>%
  deframe()

# Massage data
data <- xls %>%
  select("node1", contains("link"), "time") %>%
  pivot_longer(cols = c(-node1, -time), names_to = "nlink", values_to = "node2") %>%
  filter(!is.na(node2)) %>%
  select(node1, node2, time) %>%
  mutate(node1 = recode(node1, !!!nodes),
         node2 = recode(node2, !!!nodes))

# Create graph
colors <- c(wes_palettes$Zissou1, wes_palettes$BottleRocket1)
names(colors) <- seq(1, length(colors))

g <- data %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify(edge.attr.comb = unique) %>%
  as_tbl_graph() %>%
  mutate(community = as.character(group_louvain())) %>%
  mutate(color = recode(community, !!!colors))

# Convert from igraph to network
gn <- asNetwork(g)

data_min <- min(data$time)
data_start <- 0
data_end <- max(data$time) - min(data$time)
# Get vertices for the dynamic network
vs <- data.frame(onset = 0, terminus = data_end, vertex.id = seq(1, vcount(g)))

# Get vertices
es <- data.frame(onset = (gn %e% "time") - data_min, terminus = data_end,
                 head = as.matrix(gn, matrix.type = "edgelist")[,1],
                 tail = as.matrix(gn, matrix.type = "edgelist")[,2])

# create dynamic network
g_dyn <- networkDynamic(base.net = gn, edge.spells = es, vertex.spells = vs)

# Compute and render graph
compute.animation(g_dyn,
                  animation.mode = "kamadakawai",
                  slice.par = list(start = 0,
                                   end = data_end,
                                   interval = 1,
                                   aggregate.dur = 1,
                                   rule = 'any')
)

html_file <- "results/90_articles_BB.html"
render.d3movie(g_dyn, usearrows = F,
               displaylabels = F,
               label = gn %v% "vertex.names",
               bg = "#ffffff",
               vertex.border = "#333333",
               vertex.cex = function(slice){0.5 + degree(slice, )/7},
               vertex.col = g_dyn %v% "color",
               #edge.lwd = (net3.dyn %e% "weight")/3,
               edge.col = '#55555599',
               vertex.tooltip = function(slice) {
                 paste("<b>Name:</b>", (slice %v% "vertex.names"), "<br>",
                       "<b>Community:</b>", (slice %v% "community"), "<br>",
                       "<b>Degree:</b>", degree(slice)
                 )},
               launchBrowser = T,
               filename = html_file,
               render.par = list(tween.frames = 30,
                                 show.time = F),
               plot.par = list(mar = c(0,0,0,0)),
               output.mode = 'HTML',
               slice.par = list(start = 0,
                                end = data_end,
                                interval = 4,
                                aggregate.dur = 4,
                                rule = 'any'))


