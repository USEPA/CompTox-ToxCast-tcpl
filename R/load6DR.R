#-------------------------------------------------------------------------------
# .load6DR: Load data for tcpl6
#-------------------------------------------------------------------------------

#' @title Load data for tcpl6
#' 
#' @description
#' \code{.load6DR} loads dose-response data for tcpl6.

.load6DR <- function(ae) {
  
  qformat <- 
    "
    SELECT
      mc4_agg.aeid,
      mc4_agg.m4id,
      mc4_agg.m3id,
      spid,
      logc,
      rval,
      resp,
      apid,
      rowi,
      coli,
      wllt,
      cndx,
      repi
    FROM 
      mc0,
      mc1,
      mc3,
      mc4_agg
    WHERE
      mc3.m3id = mc4_agg.m3id
      AND
      mc1.m1id = mc4_agg.m1id
      AND
      mc1.m0id = mc0.m0id
      AND
      mc0.m0id = mc4_agg.m0id
      AND
      mc4_agg.aeid = %s;
    "
  
  qstring <- sprintf(qformat, ae)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
  
  dat[]
  
}

#-------------------------------------------------------------------------------
