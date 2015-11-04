#'
#' Extract dyad-level statistics from a given event-network.
#'
#' INTERNAL FUNCTION: Intakes a given network object and returns a set
#'  of dyad-level statistics for output.
#'
#'  @param net_obj network object object containing a set of interactions.
#'
#'  @return net_stats Table of dyad-level statistics.
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


extract_dyadstats <- function(input_date = this_date, input_net = tsna_obj){

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
  # Extract a set of DYAD-LEVEL statistics
  #
  ######

  ## Community detection
  ic <- igraph::infomap.community(daily_graph)

  ## Get community membership
  ic_membership <- igraph::membership(ic)

  ## Number and size of N>1 communities detected
  num_ic <- length(igraph::sizes(ic)[igraph::sizes(ic) > 1])
  size_ic <- sort(igraph::sizes(ic)[igraph::sizes(ic) > 1], decreasing = T)

  ## Convert to edgelist
  comm_ids <- (ic_membership[ic_membership %in% names(size_ic)])
  comm_members <- which(ic_membership %in% comm_ids)
  comm_ids <- as.integer(as.factor(comm_ids))
  comm_edgelist <- cbind(comm_ids, comm_members)

  ## Convert to bimodal adjacency matrix
  comm_membership <- matrix(0, length(unique(comm_ids)), 255)
  rownames(comm_membership) <- sort(unique(comm_ids))
  colnames(comm_membership) <- 1:255
  comm_membership[comm_edgelist[,]] <- 1

  ## Matrix multiply to get shared membership matrix
  comm_adj <-  t(comm_membership) %*% comm_membership

  ## Convert to daily edgelist
  comm_try <- try({
    comm_ties <- data.table(input_date, which(comm_adj == 1, arr.ind = T))
  }, silent = T)
  if(class(comm_try)[1] == 'try-error'){
    comm_ties <- data.table('input_date' = integer(), 'nodea' = integer()
                            , 'nodeb' = integer())
  }
  setnames(comm_ties, c('date', 'nodea', 'nodeb'))
  comm_ties <- comm_ties[nodea != nodeb]
  setkeyv(comm_ties, c('nodea', 'nodeb'))

  return(comm_ties)
}
