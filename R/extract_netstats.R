#'
#' Extract network-level statistics from a given event-network.
#'
#' INTERNAL FUNCTION: Intakes a given network object and returns a set
#'  of network-level statistics for output.
#'
#'  @param net_obj network object object containing a set of interactions.
#'
#'  @return net_stats Table of network-level statistics.
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


extract_netstats <- function(input_date = this_date, input_net = tsna_obj){

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
  # Extract a set of NETWORK-LEVEL statistics
  #
  ######

  ## Mean degree
  # Since it's a mean, in- vs out-degree doesn't matter
  net_degree <- mean(sna::degree(as.matrix.network(net_obj), gmode = 'digraph'))

  ## Density
  net_density <- network.density(net_obj)

  ## Transitivity
  net_trans <- gtrans(net_obj, diag =  F, mode = 'digraph')

  ## Dyad census
  net_dyads <- sna::dyad.census(as.matrix.network(net_obj))
  dimnames(net_dyads)[[2]] <- paste0('dyad', dimnames(net_dyads)[[2]])

  ## Triad census
  net_triads <- sna::triad.census(as.matrix.network(net_obj), mode = 'digraph')
  dimnames(net_triads)[[2]] <- paste0('triad', dimnames(net_triads)[[2]])

  ## Community detection
  ic <- igraph::infomap.community(daily_graph)

  ## Network community modularity
  ic_mod <- igraph::modularity(ic)

  ## Number and size of N>1 communities detected
  num_ic <- length(igraph::sizes(ic)[igraph::sizes(ic) > 1])
  size_ic <- sort(igraph::sizes(ic)[igraph::sizes(ic) > 1], decreasing = T)

  ## Mean community size of N>1 communities
  meansize_ic <- mean(size_ic)

  ## Share of total ties that connect different communities
  share_crossings <- sum(igraph::crossing(ic, daily_graph) == T) /
    length(igraph::crossing(ic, daily_graph))

  ## Output network stats
  return(c(net_degree, net_density, net_trans, ic_mod
           , num_ic, meansize_ic
           , share_crossings, net_dyads, net_triads))

}
