#'
#' Pre-process ICEWS actor and state fields to match CAMEO.
#'
#'  Intake ICEWS data and replace actor and state fields with
#'  short-form or numeric CAMEO codes. This function is for
#'  internal use, and is based on Phil Shcrodt's excellent
#'  Python module that does the same task
#'  (https://github.com/philip-schrodt/text_to_CAMEO/blob/master/text_to_CAMEO.py)
#'
#'  @return icews_data a formatted ICEWS data set.
#'
#'  @keywords phoenix, ICEWS, event data
#'

icews_cameo <- function(icews_data){
  require(data.table)
  require(countrycode)
  require(reshape2)
  require(statnet)
  require(tsna)
  require(plyr)
  require(lubridate)
}
