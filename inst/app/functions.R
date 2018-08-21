new.topic.network <- function(stmFit, threshold, topic.names){
  #mod.out.corr <- topicCorr(stmFit, method = "simple", cutoff = threshold)
  cormat <- cor(stmFit$theta)
  adjmat <- ifelse(abs(cormat) > threshold,1,0)
  
  links2 <- as.matrix(adjmat)
  net2 <- graph_from_adjacency_matrix(links2, mode = "undirected")
  net2 <- igraph::simplify(net2,  remove.multiple = FALSE, remove.loops = TRUE) 

  data <- toVisNetworkData(net2)
  
  nodes <- data[[1]]
  edges <- data[[2]]
  
  # Community Detection
  clp <- cluster_label_prop(net2)
  nodes$community <- clp$membership
  qual_col_pals = RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col_vector <- c(col_vector, col_vector)
  col <- col_vector[nodes$community+1]
  
  links <- igraph::as_data_frame(net2, what="edges")
  nodes <- igraph::as_data_frame(net2, what="vertices")

  TopicProportions = colMeans(stmFit$theta)
  
  #visNetwork edits
  nodes$shape <- "dot"  
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title <- topic.names # Text on click
  nodes$label <- topic.names # Node label
  nodes$size <- (TopicProportions / max(TopicProportions)) * 40 # Node size
  nodes$borderWidth <- 2 # Node border width
  
  nodes$color.background <- col
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "royalblue"
  nodes$color.highlight.border <- "navyblue"
  nodes$id <- 1:nrow(nodes)
  
  v <- list(nodes, edges)
  
  return(v)
}



neigh.network <-  function(data_topic, comment, context_sent, stp){
  
  fulltext<-corpus(data_topic, text_field = comment)
  Tok_data <- quanteda::tokens(fulltext, include_docvars = TRUE)
  context_words <- unlist(strsplit(context_sent," "))
  context_words <- trimws(context_words)
  context_words <- context_words[1:5]
  
  context_data <- data.frame(topic_word = context_words, type = 99L)
  # context_data <- context_data[1:5,]
  
  dfa <- data.frame()
  
  for (i in 1:length(context_words)){
    
     Tok_data_custom <- Tok_data %>%
      tokens_remove("\\p{P}", valuetype = "regex", padding = FALSE) %>%
      tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
      tokens_select(context_words[i], padding = FALSE, window = 2, selection = "keep") %>%
      tokens_ngrams(n = 1L) %>%
      dfm()
    
    d2 <- as.data.frame(docfreq(Tok_data_custom, use.names=TRUE))
    names(d2) <- "n"
    d2["source"] <- rep(context_words[i], nrow(d2))
    d2["sink"] <- row.names(d2)
    d2 <- d2[complete.cases(d2),]
    
    d2 <- d2[which(d2$source != d2$sink),]
    d2 <- d2[order(d2$n,decreasing = TRUE),]
    
    dfa <- rbind(dfa, d2[1:5,])
  }
  
  dfs1 <- data.frame(source = unique(c(dfa$source, dfa$sink)), source_no = seq(0,length(unique(c(dfa$source, dfa$sink)))-1))
  dfs2 <- data.frame(sink = unique(c(dfa$source, dfa$sink)), sink_no = seq(0,length(unique(c(dfa$source, dfa$sink)))-1))
  dfs_t <- data.frame(topic_word = unique(c(dfa$source, dfa$sink)), word_no = seq(0,length(unique(c(dfa$source, dfa$sink)))-1))
  
  dfs <- merge(dfs_t, context_data, by = "topic_word", all.x = TRUE)
  
  if(!is.null(dfs)){
  dfs$type <- ifelse(is.na(dfs$type),"Neighbour", "Topic")
  }
  
  dfs <- dfs[order(dfs$word_no, decreasing = F),]
  node <- cbind(topic_word2=factor(dfs$topic_word, levels=dfs$topic_word), dfs) 
  
  dfa <- merge(dfa, dfs1, by = "source", all.x = TRUE)
  dfa <- merge(dfa, dfs2, by = "sink", all.x = TRUE)
  
  link <- dfa[order(dfa$source_no), c("source_no", "sink_no", "n")]
  names(link) <- c("from","to", "weight")
  node$size <- 20L
  pos <- grep("size", colnames(node))
  
  link <- link[complete.cases(link),]
  node <- node[complete.cases(node),]
  
  w <- list(link, node, pos)
  
  return(w)
}

