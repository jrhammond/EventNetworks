#'
#' Extract node-level statistics from a given event-network.
#'
#' INTERNAL FUNCTION: Intakes a given network object and returns a set
#'  of node-level statistics for output.
#'
#'  @param net_obj network object object containing a set of interactions.
#'
#'  @return net_stats Table of node-level statistics.
#'
#'  @keywords phoenix, event data
#'
#'  @import data.table
#'  @import countrycode
#'  @import reshape2
#'  @import statnet
#'  @import tsna
#'  @import plyr
#'  @import lubridate
#'  @import igraph
#'  @import intergraph
#'
#'  @export


extract_nodestats <- function(input_date = this_date, input_net = tsna_obj){

  ######
  #
  # Extract daily network and convert to igraph
  #
  ######

  ## Collapse to daily network
  net_obj <- network.collapse(input_net, at = input_date)
  ## Convert to igraph object via 'intergraph' for additional metrics
  daily_graph <- intergraph::asIgraph(net_obj)

  ######
  #
  # Extract a set of NODE-LEVEL statistics
  #
  ######

  ## Transitivity
  trans_dist <- igraph::transitivity(daily_graph)

  ## Degree
  # Indegree
  indegree_dist <- matrix(sna::degree(as.matrix.network(input_net)
                                      , cmode = 'indegree'
                                      , rescale = T), nrow = 1)
  indegree_dist[is.nan(indegree_dist)] <- 0
  dimnames(indegree_dist)[[2]] <- nodes
  # Outdegree
  outdegree_dist <- matrix(sna::degree(as.matrix.network(input_net)
                                       , cmode = 'outdegree'
                                       , rescale = T), nrow = 1)
  outdegree_dist[is.nan(outdegree_dist)] <- 0
  dimnames(outdegree_dist)[[2]] <- nodes

  ## Betweenness
  between_dist <- matrix(sna::betweenness(as.matrix.network(input_net)
                                          , gmode = 'digraph'
                                          , rescale = T), nrow = 1)
  between_dist[is.nan(between_dist)] <- 0
  dimnames(between_dist)[[2]] <- nodes

  return(c(indegree_dist, outdegree_dist, between_dist))
}

# foo <- c(20100101, 20100102)
# test <- ldply(foo, extract_nodestats, input_net = dailynets$code4)
