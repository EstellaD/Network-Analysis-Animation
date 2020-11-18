# Get the top N used nodes
node_freq_generation <- function(es, top_n){
  headdf <- as.data.frame(table(es$from))
  taildf <- as.data.frame(table(es$to))
  node_freq <- headdf %>%
    full_join(taildf, by = "Var1") %>%
    mutate(Freq.x = replace_na(Freq.x, 0)) %>%
    mutate(Freq.y = replace_na(Freq.y, 0)) %>%
    mutate(Freq = Freq.x + Freq.y) %>%
    arrange(desc(Freq))
  
  top_nodes <- node_freq$Var1[1:top_n]
  return(top_nodes)
}

# Example
# top_10_nodes <- node_freq_generation(links, top_n = 10)


# Remove ES VS nodes if not in the top list
node_filter_generation <- function(es, vs, top_n_nodes){
  es_top <- es %>%
    filter(from %in% top_n_nodes) %>%
    filter(to %in% top_n_nodes) %>%
    
  vs_top <- vs %>%
    filter(nodename %in% top_n_nodes)
  
  return(list(es_top, vs_top))
}

# Example
# es_top <- node_filter_generation(links, nodes, top_10_nodes)[[1]]
# vs_top <- node_filter_generation(links, nodes, top_10_nodes)[[2]]





