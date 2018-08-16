#-------------------------------------------------------------------------------
# tcplLvlCount: Load tcpl level counts
#-------------------------------------------------------------------------------

#' @title Load tcpl level counts
#'
#' @description
#' \code{tcplLvlCount} queries the tcpl databases and returns a data frame with
#' count totals for the given levels and data type.
#'
#' @param lvls Integer or list of Integers, The levels of data to load
#' @param type Character of length 1, the data type, "sc" or "mc"
#'
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data.
#'
#'
#' Leaving \code{lvls} NULL will return all data.
#' 
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' TCPLlite <- file.path(system.file(package = "tcpl"), "example")
#' tcplConf(db = TCPLlite, user = NA, host = NA, drvr = "tcplLite")
#' 
#' ## Get all counts for multiple-concentration data, note 'mc' is the 
#' ## default value for type
#' tcplLvlCount()
#' 
#' ## Get all counts for level 1 for multiple-concentration
#' tcplLvlCount(lvls = 1)
#' 
#' ## Get all counts for levels 4 through 6 for multiple-concentration
#' tcplLvlCount(lvls = 4:6)
#' 
#' 
#' ## Reset configuration
#' options(conf_store)
#'
#' @return A data.table containing data for the given fields.
#'
#' @seealso \code{\link{tcplQuery}}, \code{\link{data.table}}
#'
#' @import data.table
#' @export


tcplLvlCount <- function(lvls = NULL, type = "mc") {
  
  if(!type %in% c("sc","mc")){
    stop("'type' must be either 'mc' or 'sc'.")
  }
  
  
  if (is.null(lvls)) {
    if(type == "sc"){
      lvls <-  0:2
    }
    
    else{
      lvls  <-  0:6
    }
  }
  
  if(type=="sc"){
    lvls <- lvls[lvls %in% 0:2]
  }
  if(type=="mc"){
    lvls <- lvls[lvls %in% 0:6]
  }
  
  
  df <- tcplLoadAeid(add.fld = "acid")
  
  for(l in lvls){
    varname <- paste0(type,l)
    tblname <- paste0(strsplit(type,"")[[1]][1],l)
    identifier <- "acid"
    if( (l>2 & l<7) | (l==2 & type =="sc") )
      identifier <- "aeid"

    dat <- suppressWarnings(tcplQuery(query = paste0("SELECT ",identifier,",",tblname,"id FROM ",varname), db = getOption("TCPL_DB"), tbl=varname))
    df <- merge(df,dat[,.N, by = identifier], by = identifier, all.x = T)
    colnames(df)[colnames(df) == 'N'] <- varname
  
  }

  df[is.na(df)] <- 0
  df
}


#-------------------------------------------------------------------------------

