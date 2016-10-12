#' Download the ICEWS Dataset
#'
#' Download and unzip all of the data files for the ICEWS dataset from the
#' Harvard Dataverse into a given directory.
#'
#' @param destpath The path to the directory where ICEWS should go.
#'
#' @return NULL
#' @author Original code and concept: Tony Boyles
#' @note This function is still in development and may contain errors and change quickly.
#' @examples
#'
#' download_icews("~/ICEWS/")
#'
#' @rdname download_icews

#' @export
#' @import RCurl
#' @importFrom plyr l_ply progress_text
#'

## Get ICEWS links
get_icewslinks <- function(){
  ## Load dataverse package
  if(!'dataverse' %in% installed.packages()){
    devtools::install_github("iqss/dataverse-client-r")
  }
  library("dataverse")

  ## Set dataverse metadata: API key linked to phoenixNet account
  Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
  Sys.setenv("DATAVERSE_KEY" = "b95cd0bd-2295-4292-9402-bf52e34a95b7")

  ## Get ICEWS event data information
  icews_data <- get_dataset('doi:10.7910/DVN/28075')
  icews_repos <- data.table(
    label = icews_data$files$dataFile$filename
    , id = icews_data$files$dataFile$id
  )
  icews_repos <- icews_repos[grep('.tab', icews_repos$label), ]
  icews_metadata <- sapply(sapply(icews_repos$label, 'strsplit', '\\.'), '[[', 3)

  baseURL <- "https://dataverse.harvard.edu/api/access/datafile/"
  icews_repos[, url := paste0(baseURL, icews_repos$id)]

  return(icews_repos)
}


# given a list of links, download them and write to specified directory
dw_icewsfile <- function(link, destpath, metadata = link_data){

  filename <- paste0(destpath, '/', metadata[id %in% link, label])
  fullURL <- metadata[id %in% link, url]

  # download method
  if (.Platform$OS.type == 'windows') {
    download_method <- 'auto'
  } else{
    download_method <- 'curl'
  }

  download.file(fullURL, filename, method = download_method, quiet = T)
  try({unzip(filename, exdir = destpath, unzip = "internal", setTimes = FALSE)}
      , silent = T)


  # if(substr(filename, nchar(filename)-3, nchar(filename)) == '.zip'){
  #   unlink(temp)
  # }

}
