#'
#' Extract node-level statistics from a given event-network.
#'
#' INTERNAL FUNCTION: Intakes a given network object and returns a set
#'  of node-level statistics for output.
#'
#'  @param input_date A date in integer %Y%m%d format.
#'  @param event_dnet network object object containing a set of interactions.
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


extract_nodestats <- function(input_date = this_date, event_dnet = tsna_obj){

  ######
  #
  # Extract daily network and convert to igraph
  #
  ######

  ## Collapse to daily network
  net_obj <- network.collapse(event_dnet, at = input_date)

  ## Write a weird little workaround for the final day of an empty tsna
  ##  object: by default it is a zero-node network, which is odd.
  if(network.size(net_obj) == 0){
    filler <- matrix(rep(0, 255), nrow = 1)
    dimnames(filler)[[2]] <- nodes
    return(rbind(as.data.table(cbind(date = input_date
                                      , node_stat = 'trans', filler))
                 , as.data.table(cbind(date = input_date
                                      , node_stat = 'indegree', filler))
                 , as.data.table(cbind(date = input_date
                                      , node_stat = 'outdegree', filler))
                 , as.data.table(cbind(date = input_date
                                      , node_stat = 'between', filler))))
  }

  nodes <- network.vertex.names(net_obj)
  ## Convert to igraph object via 'intergraph' for additional metrics
  daily_graph <- intergraph::asIgraph(net_obj)

  ######
  #
  # Extract a set of NODE-LEVEL statistics
  #
  ######

  ## Transitivity
  trans_dist <- matrix(igraph::transitivity(daily_graph, type = 'local'
                                            , isolates = 'zero'), nrow = 1)
  dimnames(trans_dist)[[2]] <- nodes
  trans_dist <- as.data.table(cbind(date = input_date
                                    , node_stat = 'trans', trans_dist))

  ## Degree
  # Indegree
  indegree_dist <- matrix(sna::degree(as.matrix.network(event_dnet)
                                      , cmode = 'indegree'
                                      , rescale = T), nrow = 1)
  indegree_dist[is.nan(indegree_dist)] <- 0
  dimnames(indegree_dist)[[2]] <- nodes
  indegree_dist <- as.data.table(cbind(date = input_date
                                       , node_stat = 'indegree', indegree_dist))

  # Outdegree
  outdegree_dist <- matrix(sna::degree(as.matrix.network(event_dnet)
                                       , cmode = 'outdegree'
                                       , rescale = T), nrow = 1)
  outdegree_dist[is.nan(outdegree_dist)] <- 0
  dimnames(outdegree_dist)[[2]] <- nodes
  outdegree_dist <- as.data.table(cbind(date = input_date
                                        , node_stat = 'outdegree', outdegree_dist))

  ## Betweenness
  between_dist <- matrix(sna::betweenness(as.matrix.network(event_dnet)
                                          , gmode = 'digraph'
                                          , rescale = T), nrow = 1)
  between_dist[is.nan(between_dist)] <- 0
  dimnames(between_dist)[[2]] <- nodes
  between_dist <- as.data.table(cbind(date = input_date
                                      , node_stat = 'between', between_dist))


  return(data.table(rbind(trans_dist, indegree_dist
                         , outdegree_dist, between_dist)))
}
