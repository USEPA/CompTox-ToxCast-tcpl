#-------------------------------------------------------------------------------
# tcplRun: Perform data processing
#-------------------------------------------------------------------------------

#' @title Perform data processing
#' 
#' @description
#' \code{tcplRun} is the function for performing the data processing, for both
#' single-concentration and multiple-concentration formats. 
#' 
#' @param asid Integer, assay source id
#' @param slvl Integer of length 1, the starting level to process
#' @param elvl Integer of length 1, the ending level to process
#' @param id Integer, rather than assay source id, the specific assay 
#' component or assay endpoint id(s) (optional)
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param mc.cores Integer of length 1, the number of cores to use, set to 1 
#' when using Windows operating system
#' @param outfile Character of length 1, the name of the log file (optional)
#' @param runname Character of length 1, the name of the run to be used in the 
#' oufile (optional)
#'                
#' @details
#' The \code{tcplRun} function is the core processing function within the 
#' package. The function acts as a wrapper for individual processing functions, 
#' (ie. \code{mc1}, \code{sc1}, etc.) that are not exported. If possible, the
#' processing is done in parallel by 'id' by utilizing the 
#' \code{\link{mclapply}} function within the parallel package. 
#' 
#' If slvl is less than 4, 'id' is interpreted as acid and if slvl is 4 or 
#' greater 'id' is interpreted as aeid. Must give either 'asid' or 'id'. If an 
#' id fails no results get loaded into the database, and the id does not get 
#' placed into the cue for subsequent level processing.
#' 
#' The 'type' parameter specifies what type of processing to complete: "mc" for
#' multiple-concentration processing, and "sc" for single-concentration 
#' processing.
#' 
#' @return A list containing the results from each level of processing. Each 
#' level processed will return a named logical vector, indicating the success 
#' of the processing for the id. 
#' 
#' @family data processing functions
#' @importFrom parallel detectCores
#' @export

tcplRun <- function(asid = NULL, slvl, elvl, id = NULL, type = "mc", 
                    mc.cores = NULL, outfile = NULL, runname = NULL) {
  
  ## Variable-binding to pass R CMD Check
  # acid <- aeid <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  user <- paste(Sys.info()[c("login", "user", "effective_user")], 
                collapse = ".")
  
  stime <- Sys.time()
  
  if (Sys.info()["sysname"] == "Windows") mc.cores <- 1
  
  if (length(slvl) > 1 | !is.numeric(slvl)) {
    stop("Invalid slvl - must be integer of length 1.")
  }
  
  if (is.null(elvl) | elvl < slvl) elvl <- slvl
  
  if (length(elvl) > 1 | !is.numeric(elvl)) {
    stop("Invalid elvl - must be integer of length 1.")
  }
  
  if (length(type) > 1 | !type %in% c("mc", "sc")) {
    stop ("Invalid 'type' value.")
  }
  
  if (!is.null(asid)) id <- tcplLoadAcid("asid", asid)$acid
  if (length(id) == 0) stop("No asid or id given.")
  id <- unique(id)
  
  if (!is.null(outfile)) {
    cat("Writing output to:", outfile, "\n")
    logcon <- file(outfile, open = "a")
    sink(logcon, append = TRUE)
    sink(logcon, append = TRUE, type="message")
    on.exit(sink.reset(), add = TRUE)
    on.exit(close.connection(logcon), add = TRUE)
    on.exit(cat("Output appended to log file:", outfile, "\n"), add = TRUE)
    cat("\n\n\n")
    cat("RUNDATE -- ", format(stime, "%y%m%d; %H:%M"), "\n",
        "USER    -- ", user, "\n",
        "TYPE    -- ", type, "\n",
        "LEVEL ", slvl, " TO ", "LEVEL ", elvl, "\n",
        "RUN     -- ", runname, "\n",
        sep = "")
    cat("\n\n")
  }
  
  if (is.null(mc.cores)) {
    ncores <- min(length(id), detectCores() - 1)
  } else {
    ncores <- mc.cores
  }
  
  names(id) <- paste0("ACID", id)
  res <- list()
  
  ## Multiple-concentration processing
  if (type == "mc") {
    
    ## Do level 1 processing
    if (slvl <= 1L) {
      res$l1 <- .multProc(id = id, lvl = 1L, type = "mc", ncores = ncores)
      res$l1_failed <- names(which(res$l1 != TRUE))
      id <- id[which(res$l1[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 1; processing errors ",
                "occured with all given acids by level 1.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l1_failed) > 0) {
        warning(length(res$l1_failed), " ids failed at level 1.")
      }
    }
    
    if (elvl <= 1L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Do level 2 processing
    if (slvl <= 2L) {
      res$l2 <- .multProc(id = id, lvl = 2L, type = "mc", ncores = ncores)
      res$l2_failed <- names(which(res$l2 != TRUE))
      id <- id[which(res$l2[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 2; processing errors ",
                "occured with all given acids by level 2.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l2_failed) > 0) {
        warning(length(res$l2_failed), " ids failed at level 2.")
      }
    }
    
    if (elvl == 2L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Do level 3 processing
    if (slvl <= 3L) {
      res$l3 <- .multProc(id = id, lvl = 3L, type = "mc", ncores = ncores)
      res$l3_failed <- names(which(res$l3 != TRUE))
      id <- id[which(res$l3[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 3; processing errors ",
                "occured with all given acids by level 3.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l3_failed) > 0) {
        warning(length(res$l3_failed), " ids failed at level 3.")
      }
    }
    
    if (elvl == 3L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Change ids from acid to aeid, if necessary
    if (slvl <  4L | !is.null(asid)) id <- tcplLoadAeid("acid", id)$aeid  
    names(id) <- paste0("AEID", id)
    if (is.null(mc.cores)) ncores <- min(length(id), detectCores() - 1)
        
    ## Do level 4 processing
    if (slvl <= 4L) {
      res$l4 <- .multProc(id = id, lvl = 4L, type = "mc", ncores = ncores)
      res$l4_failed <- names(which(res$l4 != TRUE))
      id <- id[which(res$l4[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 4; processing errors ",
                "occured with all given acids by level 4.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l4_failed) > 0) {
        warning(length(res$l4_failed), " ids failed at level 4.")
      }
    }
    
    if (elvl == 4L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Do level 5 processing
    if (slvl <= 5L) {
      res$l5 <- .multProc(id = id, lvl = 5L, type = "mc", ncores = ncores)
      res$l5_failed <- names(which(res$l5 != TRUE))
      id <- id[which(res$l5[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 5; processing errors ",
                "occured with all given acids by level 5.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l5_failed) > 0) {
        warning(length(res$l5_failed), " ids failed at level 5.")
      }
    }
    
    if (elvl == 5L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Do level 6 processing
    if (slvl <= 6L) {
      res$l6 <- .multProc(id = id, lvl = 6L, type = "mc", ncores = ncores)
      res$l6_failed <- names(which(res$l6 != TRUE))
      id <- id[which(res$l6[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 6; processing errors ",
                "occured with all given acids by level 6.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l6_failed) > 0) {
        warning(length(res$l6_failed), " ids failed at level 6.")
      }
    }
    
    if (elvl == 6L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("\n\nTotal processing time:", ttime, "\n\n")
    return(res)
    
  } ## END multiple-concentration processing
  
  ## Single-concentration processing
  if (type == "sc") {
    
    ## Do level 1 processing
    if (slvl <= 1L) {
      res$l1 <- .multProc(id = id, lvl = 1L, type = "sc", ncores = ncores)
      res$l1_failed <- names(which(res$l1 != TRUE))
      id <- id[which(res$l1[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 1; processing errors ",
                "occured with all given acids by level 1.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l1_failed) > 0) {
        warning(length(res$l1_failed), " ids failed at level 1.")
      }
    }
    
    if (elvl <= 1L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ## Change ids from acid to aeid, if necessary
    if (slvl <  2L | !is.null(asid)) id <- tcplLoadAeid("acid", id)$aeid  
    names(id) <- paste0("AEID", id)
    if (is.null(mc.cores)) ncores <- min(length(id), detectCores() - 1)
    
    ## Do level 2 processing
    if (slvl <= 2L) {
      res$l2 <- .multProc(id = id, lvl = 2L, type = "sc", ncores = ncores)
      res$l2_failed <- names(which(res$l2 != TRUE))
      id <- id[which(res$l2[names(id)] == TRUE)]
      if (length(id) == 0) {
        warning("Pipeline stopped early at level 2; processing errors ",
                "occured with all given acids by level 2.")
        ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
        ttime <- paste(unclass(ttime), units(ttime))
        cat("\n\nTotal processing time:", ttime, "\n\n")
        return(res)
      }
      if (length(res$l2_failed) > 0) {
        warning(length(res$l2_failed), " ids failed at level 2.")
      }
    }
    
    if (elvl == 2L) {
      ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("\n\nTotal processing time:", ttime, "\n\n")
      return(res)
    }
    
    ttime <- round(difftime(Sys.time(), stime, units = "min"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("\n\nTotal processing time:", ttime, "\n\n")
    return(res)
    
  } ## END single-concentration processing
  
}

#-------------------------------------------------------------------------------
