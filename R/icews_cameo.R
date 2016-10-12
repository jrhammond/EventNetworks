#'
#' Convert ICEWS state/actor codes into CAMEO format,
#'  and extract root codes from specific CAMEO event codes.
#'
#'  Intake a set of ICEWS data (read in after some pre-processing)
#'    and convert entries to CAMEO format using conversion tables created
#'    by Phil Schrodt (https://github.com/philip-schrodt/text_to_CAMEO)
#'
#'  @param icews ICEWS data as one large data.table
#'
#'  @return icews ICEWS data with several new CAMEO code columns.
#'
#'  @keywords phoenix, event data
#'
#'  @import data.table
#'  @import plyr
#'
#'  @export

icews_cameo <- function(icews){

  ######
  #
  # Read in data for conversions
  # (created by Phil Schrodt)
  #
  ######

  data(agents, envir = environment())
  data(states, envir = environment())

  ######
  #
  # Functions
  #
  ######

  ## Paste-function that can handle NA entries
  ## (http://stackoverflow.com/questions/13673894/suppress-nas-in-paste)
  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }

  ## Conversion function: intake list of sectors, return the CAMEO actor
  ##  code for the 'most important' actor in the list
  cameo_convert <- function(sector){
    this_source <- data.table(actor = unlist(strsplit(sector, ',')))
    this_codes <- merge(agents, this_source, sort = F, by = 'actor')
    if(any(sapply(this_codes[, code1], 'substr', 1, 3) %in% agentcodes)){
      return(agentcodes[min(which(agentcodes %in% this_codes[,code1]))])
    } else{
      return(NA_character_)
    }

  }

  ######
  #
  # Set up data storage objects
  #
  ######

  ## Ordered list of CAMEO agent codes to extract for the agent field
  agentcodes = c('GOV','MIL','REB','OPP', 'PTY', 'COP','JUD','SPY'
                 ,'IGO','MED','EDU','BUS','CRM','CVL','---')

  ## Tables of unique source/target sectors
  source_table <- data.table(Source.Sectors = unique(icews$Source.Sectors)
                             , source_codes = NA_character_)
  target_table <- data.table(Target.Sectors = unique(icews$Target.Sectors)
                             , target_codes = NA_character_)

  ######
  #
  # Convert ICEWS codes to CAMEO codes
  #
  ######

  ## Convert unique source/target sector codes
  source_table[, source_codes := ldply(source_table$Source.Sectors, cameo_convert)]
  target_table[, target_codes := ldply(target_table$Target.Sectors, cameo_convert)]
  icews <- merge(icews, source_table, by = 'Source.Sectors', all.x = T, sort = F)
  icews <- merge(icews, target_table, by = 'Target.Sectors', all.x = T, sort = F)

  ## Convert unique state codes
  setnames(states, c('Source.Country', 'source_isoc', 'source_cown'))
  icews <- merge(icews, states, by = 'Source.Country', all.x = T, sort = F)
  setnames(states, c('Target.Country', 'target_isoc', 'target_cown'))
  icews <- merge(icews, states, by = 'Target.Country', all.x = T, sort = F)

  ######
  #
  # Generate source/actor entity codes a la CAMEO
  #
  ######

  icews[, sourceactorentity := paste3(icews$source_isoc, icews$source_codes, sep = '')]
  icews[, targetactorentity := paste3(icews$target_isoc, icews$target_codes, sep = '')]

  ######
  #
  # Extract root codes from CAMEO codes
  #
  ######

  for(i in seq(10, 200, by = 10)){
    icews[eventcode %in% c(i:(i+9), c((i*10+11):(i*10+90))), rootcode := i/10]
  }

  ######
  #
  # Write out cleaned ICEWS data
  #
  ######

  return(icews)
}
