#-------------------------------------------------------------------------------
# mc5_mthds: Load list of cutoff methods (to be used at level 5).
#-------------------------------------------------------------------------------

#' @name MC5_Methods
#' @title Load list of level 5 multiple-concentration cutoff methods
#'
#' @description
#' \code{mc5_mthds} returns a list of additional activity cutoff methods
#' to be used during level 5 multiple-concentration processing.
#'
#' @param ae Integer of length 1, the assay endpoint id
#'
#' @return A list of functions
#'
#' @seealso \code{\link{mc5}}, \code{\link{Method functions}} to query what
#' methods get applied to each aeid
#'
#' @section Available Methods:
#'
#' More information about the level 5 multiple-concentration processing is
#' available in the package vignette, "Pipeline_Overview."
#'
#' \describe{
#'   \item{bmad3}{Add a cutoff value of 3 multiplied by the baseline median absolute deviation 
#'   (bmad) as defined at Level 4.}
#'   \item{pc20}{Add a cutoff value of 20. Typically for percent of control data.}
#'   \item{log2_1.2}{Add a cutoff value of log2(1.2). Typically for fold change data.}
#'   \item{log10_1.2}{Add a cutoff value of log10(1.2). Typically for fold change data.}
#'   \item{bmad5}{Add a cutoff value of 5 multiplied the baseline median absolute deviation (bmad). 
#'   By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad6}{Add a cutoff value of 6 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad10}{Add a cutoff value of 10 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{maxmed20pct}{Add a cutoff value of 20 percent of the maximum of all endpoint maximal 
#'   average response values (max_med).}
#'   \item{pc70}{Add a cutoff value of 70. Typically for percent of control data.}
#'   \item{pc50}{Add a cutoff value of 50. Typically for percent of control data.}
#'   \item{log2_2}{Add a cutoff value log2(2). Typically for fold change data.}
#'   \item{log10_2}{Add a cutoff value of log10(2). Typically for fold change data.}
#'   \item{neglog2_0.88}{Add a cutoff value of -1 times log2(0.88).}
#'   \item{coff_2.32}{Add a cutoff value of 2.32.}
#'   \item{loec.coff}{Method not yet updated for tcpl implementation. Identify the lowest observed 
#'   effective concentration (loec) compared to baseline.}
#'   \item{fc0.2}{Add a cutoff value of 0.2. Typically for zero centered fold change data.}
#'   \item{fc0.3}{Add a cutoff value of 0.3. Typically for zero centered fold change data.}
#'   \item{pc25}{Add a cutoff value of 25. Typically for percent of control data.}
#'   \item{pc30}{Add a cutoff value of 30. Typically for percent of control data.}
#'   \item{bmad1}{Add a cutoff value of 1 multiplied by baseline median absolute value (bmad). By 
#'   default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad2}{Add a cutoff value of 2 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{pc10}{Add a cutoff value of 10. Typically for percent of control data.}
#'   \item{pc05}{Add a cutoff value of 5. Typically for percent of control data.}
#'   \item{pc95}{Add a cutoff value of 95. Typically for percent of control data.}
#'   \item{bmad4}{Add a cutoff value of 4 multiplied the baseline median absolute deviation (bmad). 
#'   By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{fc0.5}{Add a cutoff value of 0.5. Typically for zero centered fold change data.}
#'   \item{ow_bidirectional_loss}{Overwrite winning model hitcalls to -1 and
#'   potency metrics to NA for models that were not fit in the negative
#'   direction. Typically used for assays where only negative responses are
#'   biologically relevant.}
#'   \item{ow_bidirectional_gain}{Overwrite winning model hitcalls to -1 and
#'   potency metrics to NA for models that were not fit in the positive
#'   direction. Typically used for assays where only positive responses are
#'   biologically relevant.}
#' }


mc5_mthds <- function(ae) {

  list(

    bmad3 = function() {

      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*3]))
      list(e1)

    },

    pc20 = function() {

      e1 <- bquote(coff <- c(coff, 20))
      list(e1)

    },

    log2_1.2 = function() {

      e1 <- bquote(coff <- c(coff, log2(1.2)))
      list(e1)

    },

    log10_1.2 = function() {

      e1 <- bquote(coff <- c(coff, log10(1.2)))
      list(e1)

    },

    bmad5 = function() {

      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*5]))
      list(e1)

    },

    bmad6 = function() {

      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*6]))
      list(e1)

    },

    bmad10 = function() {

      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*10]))
      list(e1)

    },

    maxmed20pct = function() {

      e1 <- bquote(coff <- c(coff, dat[ , max(max_med)*.20]))
      list(e1)

    },

    pc70 = function() {

      e1 <- bquote(coff <- c(coff, 70))
      list(e1)

    },

    pc50 = function() {

      e1 <- bquote(coff <- c(coff, 50))
      list(e1)

    },

    log2_2 = function() {

      e1 <- bquote(coff <- c(coff, log2(2)))
      list(e1)

    },

    log10_2 = function() {

      e1 <- bquote(coff <- c(coff, log10(2)))
      list(e1)

    },
    
    neglog2_0.88 = function() {
      
      e1 <- bquote(coff <- c(coff, -1*log2(0.88)))
      list(e1)
      
    },
    
    coff_2.32 = function() {
      
      e1 <- bquote(coff <- c(coff, 2.32))
      list(e1)
      
    },
    
    fc0.2 = function() {

      e1 <- bquote(coff <- c(coff, 0.2))
      list(e1)

    },
    
    fc0.3 = function() {

      e1 <- bquote(coff <- c(coff, 0.3))
      list(e1)

    },
    pc25 = function() {

      e1 <- bquote(coff <- c(coff, 25))
      list(e1)
    },
      
	pc30 = function() {

      e1 <- bquote(coff <- c(coff, 30))
      list(e1)
    },
    
  bmad1 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)]))
      list(e1)
    }, 
	
  bmad2 = function() {
  	  
	  e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*2]))
	  list(e1)
	},
	
	pc10 = function() {
	    
	  e1 <- bquote(coff <- c(coff, 10))
	  list(e1)
	},
	
	pc05 = function() {
	  
	  e1 <- bquote(coff <- c(coff, 5))
	  list(e1)
	},
	
	pc95 = function() {
	  
	  e1 <- bquote(coff <- c(coff, 95))
	  list(e1)
	},
	
	bmad4 = function() {
	  
	  e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*4]))
	  list(e1)
	  
	},
	
	fc0.5 = function() {
	  
	  e1 <- bquote(coff <- c(coff, 0.5))
	  list(e1)
	  
	},
	
	ow_bidirectional_loss = function() {
	  
	  top <- c("tp", "top")
	  potency <- c("ac5", "ac10", "ac20", "ac50", "ac1sd", "acc", "ac50_loss")
	  # get all endpoint sample m4ids where the top param is greater than 0
	  e1 <- bquote(top.gt0.m4ids <- dat[(hit_param %in% top) & hit_val > 0, unique(m4id)])
	  # set potency metrics to NA if found in m4id list
	  e2 <- bquote(dat$hit_val[dat$m4id %in% top.gt0.m4ids & dat$hit_param %in% potency] <- NA)
	  # set hitcall param and hitc to -1 if found in m4id list
	  e3 <- bquote(dat$hit_val[dat$m4id %in% top.gt0.m4ids & dat$hit_param == "hitcall"] <- -1)
	  e4 <- bquote(dat$hitc[dat$m4id %in% top.gt0.m4ids] <- -1)
	  list(e1, e2, e3, e4)
	  
	},
	
	ow_bidirectional_gain = function() {
	  
	  top <- c("tp", "top")
	  potency <- c("ac5", "ac10", "ac20", "ac50", "ac1sd", "acc", "ac50_loss")
	  # get all endpoint sample m4ids where the top param is greater than 0
	  e1 <- bquote(top.gt0.m4ids <- dat[(hit_param %in% top) & hit_val < 0, unique(m4id)])
	  # set potency metrics to NA if found in m4id list
	  e2 <- bquote(dat$hit_val[dat$m4id %in% top.gt0.m4ids & dat$hit_param %in% potency] <- NA)
	  # set hitcall param and hitc to -1 if found in m4id list
	  e3 <- bquote(dat$hit_val[dat$m4id %in% top.gt0.m4ids & dat$hit_param == "hitcall"] <- -1)
	  e4 <- bquote(dat$hitc[dat$m4id %in% top.gt0.m4ids] <- -1)
	  list(e1, e2, e3, e4)
	  
	}
	
  )
}

#-------------------------------------------------------------------------------
