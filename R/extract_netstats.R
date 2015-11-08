#'
#' Extract network-level statistics from a given event-network.
#'
#' INTERNAL FUNCTION: Intakes a given network object and returns a set
#'  of network-level statistics for output.
#'
#' @param input_date A date in integer %Y%m%d format.
#' @param event_dnet network object object containing a set of interactions.
#'
#' @return net_stats Table of network-level statistics.
#'
#' @keywords phoenix, event data
#'
#' @import data.table
#' @import countrycode
#' @import reshape2
#' @import statnet
#' @import tsna
#' @import plyr
#' @import lubridate
#' @import igraph
#' @import intergraph
#'
#' @export


extract_netstats <- function(input_date = this_date, event_dnet = event_dnet, datelist = dates){

  ######
  #
  # Extract daily network and convert to igraph
  #
  ######

  ## Collapse to daily network
  net_obj <- network.collapse(event_dnet, at = input_date)

  ## Convert input date to an actual date object
  prev_date <- datelist[which(datelist %in% input_date) - 1]
  input_date <- as.Date(as.character(input_date), format = '%Y%m%d')

  if(network::network.edgecount(net_obj) == 0){
    return(data.table(date = input_date
                      , net_jaccard = 0, net_hamming = 0
                      , net_degree = 0, net_density = 0
                      , net_trans = 0, net_modularity = 0
                      , num_communities = 0, comm_meansize = 0
                      , xcomm_ties = 0
                      , dyads_mut = 0, dyads_asym = 0
                      , dyads_null = 0
                      , triads_003 = 0, triads_012 = 0
                      , triads_102 = 0, triads_021D = 0
                      , triads_021U = 0, triads_021C = 0
                      , triads_111D = 0, triads_111U = 0
                      , triads_030T = 0, triads_030C = 0
                      , triads_201 = 0, triads_120D = 0
                      , triads_120U = 0, triads_120C = 0
                      , triads_210 = 0, triads_300 = 0))
  }
  ## Convert to igraph object via 'intergraph' for additional metrics
  daily_graph <- intergraph::asIgraph(net_obj)

  ######
  #
  # Extract a set of NETWORK-LEVEL statistics
  #
  ######

  #### Changes from previous time period
  try_prev <- try({
    ## Get previous time period
    net_obj_t1 <- network.collapse(event_dnet, at = prev_date)

    ## Convert to matrices
    net_mat_t1 <- as.matrix.network(net_obj_t1)
    net_mat <- as.matrix.network(net_obj)

    ## Jaccard index
    net_overlap <- net_mat_t1 + net_mat
    net_intersect <- sum(net_overlap == 2)
    net_union <- sum(net_overlap >= 1)
    net_difference <- sum(net_overlap == 0)
    net_jaccard <- net_intersect / net_union

    ## Hamming distance
    net_hamming <- (net_intersect + net_difference) / length(net_mat)
  }, silent = T)
  if(class(try_prev)[1] == 'try-error'){
    net_jaccard <- NA
    net_hamming <- NA
  }

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
  return(data.table(date = input_date
                    , net_jaccard = net_jaccard, net_hamming = net_hamming
                    , net_degree = net_degree, net_density = net_density
                    , net_trans = net_trans, net_modularity = ic_mod
                    , num_communities = num_ic, comm_meansize = meansize_ic
                    , xcomm_ties = share_crossings
                    , dyads_mut = net_dyads[1], dyads_asym = net_dyads[2]
                    , dyads_null = net_dyads[3]
                    , triads_003 = net_triads[1], triads_012 = net_triads[2]
                    , triads_102 = net_triads[3], triads_021D = net_triads[4]
                    , triads_021U = net_triads[5], triads_021C = net_triads[6]
                    , triads_111D = net_triads[7], triads_111U = net_triads[8]
                    , triads_030T = net_triads[9], triads_030C = net_triads[10]
                    , triads_201 = net_triads[11], triads_120D = net_triads[12]
                    , triads_120U = net_triads[13], triads_120C = net_triads[14]
                    , triads_210 = net_triads[15], triads_300 = net_triads[16]
                    ))

}
