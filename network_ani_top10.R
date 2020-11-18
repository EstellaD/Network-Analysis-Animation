# Read in the data
source("initialization.R")
source("helpers.R")
dat <- readRDS("load_data/sampledata_15nodes.RDS")
dat$StartDate <- as.Date(dat$StartDate, format = "%Y-%m-%d")
dat$EndDate <- as.Date(dat$EndDate, format = "%Y-%m-%d")
top_n = 10

# This might take a long time depending on the size of the dataset.
# TODO: rewrite for-loop into dplyr
source("es_vs_generation.R")
# Before turning node names to numeric, get the top n nodes
# New links, nodes after removing non-top nodes
head(links)
head(nodes)

top_10_nodes <- node_freq_generation(links, top_n)

# New es and vs after removing non-top nodes
links <- node_filter_generation(links, nodes, top_10_nodes)[[1]]
nodes <- node_filter_generation(links, nodes, top_10_nodes)[[1]]

################ Old Version ##############
ids <- unique(nodes$nodename)
net <- network.initialize(length(ids))
# copy in the vertex names
network.vertex.names(net) <- ids

# Convert LineHead Node Names into Numeric to Plot
# This step has to be done here, not in es_vs_generation.R
# because we need labels for ids/network.vertex.names previously
links$from <- match(links$from, ids)
links$to <- match(links$to, ids)
links <- links[, c("TrtTrime", "TrtTime.1", "from", "to", "Weight")]
nodes$nodename <- match(nodes$nodename, ids)
nodes <- nodes[, c("TrtTime", "TrtTime.1", "nodename", "Size")]

# This step is very important that without forcing es&vs were tibble,
# networkDynamic only accepts data.frame, not tibble.
es <- as.data.frame(links)
vs <- as.data.frame(nodes)

# use the networkDynamic constructor, telling it to create dynamic attributes
netDyn <- networkDynamic(net, 
                         edge.spells = es,
                         vertex.spells = vs,
                         create.TEAs = TRUE,
                         edge.TEA.names = 'Weight',
                         vertex.TEA.names = 'Size'
                         )

###### Static Circle Coordinate
network.layout.animate.circle <- function(net, dist.mat = NULL,
                                          default.dist = NULL, seed.coords = NULL, layout.par = list(),
                                          verbose = FALSE){
  n <- network.size(net)
  x <- 10*cos(seq(0, 2*((n-1)/n)*pi, length.out = n))
  y <- 10*cos(seq(0, 2*((n-1)/n)*pi, length.out = n))
  return(cbind(x,y))
}

new_netDyn <- compute.animation(netDyn,
                                slice.par=list(start = 0, end = 12, interval = 1,
                                               aggregate.dur = 1, rule = 'any')
                                , animation.mode = 'circle')

render.d3movie(new_netDyn, usearrows = F,
               displaylabels = T, label = network.vertex.names(net),
               bg="#ffffff", vertex.border="#333333",
               vertex.cex = 'Size',
               # vertex.col = new_netDyn %v% "col",
               edge.lwd = 'Weight',
               edge.col = '#55555599',
               # vertex.tooltip = paste("<b>Node Name:</b>", ("nodename"), "<br>",
               #                        "<b>Node Name:</b>", ("node_type_label")),
               # edge.tooltip = paste("<b>Edge Weight:</b>", ~"Weight"),
               launchBrowser = T, filename="Node_Visual_Enhancement_Network_Dynamic.html",
               render.par = list(tween.frames=10, show.time=F),
               plot.par=list(mar=c(0,0,0,0)))













