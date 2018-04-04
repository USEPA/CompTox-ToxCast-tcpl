#' @name Configure functions
#' @rdname config_funcs
#' @title Functions for configuring the tcpl package
#'
#' @description
#' These functions are used to configure the tcpl settings.
#' 
#' @param drvr Character of length 1, which database driver to use
#' @param user Character of length 1, the database server username
#' @param pass Character of length 1, the database server password
#' @param host Character of length 1, the database server 
#' @param db   Character of length 1, the name of the tcpl database
#' @param show.pass Logical, should the password be returned
#' 
#' @details
#' Currently, the tcpl package only supports the "MySQL" and "SQLite" database
#' drivers.
#' 
#' \code{tcplConf} changes \code{options} to set the tcpl-specific options, 
#' most importantly to configure the connection to the tcpl databases. 
#' \code{tcplConf} will only change non-null values, and can be used to 
#' change a single value if needed. 
#' 
#' \code{tcplConfSave} modifies the TCPL.config file to reflect the current
#' tcpl settings.
#' 
#' \code{tcplConfList} lists the values assigned to the tcpl global options.
#' 
#' \code{tcplConfLoad} updates the tcpl settings to reflect the current 
#' configuration file.
#' 
#' \code{tcplConfDefault} changes the \code{options} to reflect the default
#' settings for the example SQLite database, but does not alter the 
#' configuration file.
#' 
#' \code{tcplConfReset} is used to generate the initial configuration script,
#' and can be used to reset or regenerate the configuration script by the user.
NULL
