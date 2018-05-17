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
#' The settings can be stored in a configuration file to make the using the 
#' package more user-friendly. To create the configuration file, the user must
#' first create a system environment variable ('TCPL_CONF') that points to to 
#' the file. There is more information about system environment variables in
#' \code{\link{Startup}} and \code{\link{Sys.getenv}}. Briefly, the user 
#' needs to modify the '.Renviron' file in their home directory. If the file
#' does not exist, create it, and add the following line:
#' 
#' TCPL_CONF=path/to/confFile.conf
#' 
#' Here 'path/to/confFile.conf' can be any path to a file. One suggestion would
#' be to include .tcplConf in the home directory, eg. TCPL_CONF=~/.tcplConf. 
#' Note, '~' may not indicate the home directory on every operating system.
#' Once the environment variable is added, the user can change the settings
#' using \code{tcplConf}, then save the settings to the file given by the
#' TCPL_CONF environment variable running \code{tcplConfSave()}. 
#' 
#' \code{tcplConf} changes \code{options} to set the tcpl-specific options, 
#' most importantly to configure the connection to the tcpl databases. 
#' \code{tcplConf} will only change non-null values, and can be used to 
#' change a single value if needed. 
#' 
#' \code{tcplConfSave} modifies the configuration file to reflect the current
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
