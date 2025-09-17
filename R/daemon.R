
#	FoodguardRanker: [description here]
#	Copyright (C) 2025 Shintaro Kinoshita <shintaro.kinoshita@e1datascience.co.uk>
#
#	This program is free software: you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation, either version 3 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program.  If not, see <http://www.gnu.org/licenses/>.

#'
#' R6 class as utility of Daemon which is available for all the class, members and functions.
#' @importFrom R6 R6Class
#'
DaemonUtility <- R6::R6Class( "DaemonUtility",
	public = list(
		#'
		#' @description
		#' Inilialise DaemonUtility - so far it does nothing.
		#'
		initialize = function() {},

		#'
		#' @description
		#' BOOOOOOOOOM !!! !!! !!!
		#'
		#' @param errorContent String - detailed error message.
		#'
		errorBomb = function( errorContent ) {
			# Set error message
			errorMessage <- paste0( "\n\nERROR!: ", errorContent )
			# R.I.P !
			stop( errorMessage )
		}
	)
)
# ANCHOR : DaemonUtility ends here.

#'
#' R6 class which contains all the available functions for prediction.
#' @importFrom R6 R6Class
#'
DaemonFunctionManager <- R6::R6Class( "DaemonFunctionManager",
	public = list(
		#'
		#' @field util Class - class of DaemonUtility.
		#'
		util = NULL,

		#'
		#' @description
		#' Inilialise DaemonUtility - so far it does nothing.
		#'
		initialize = function() {
			# Activate DaemonUtility class
			self$util <- DaemonUtility$new()
		}
	)
)
# ANCHOR : DaemonFunctionManager ends here.

#'
#' @title Activate daemon to open a port for REST API.
#'
#' @description
#' Description here.
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @importFrom plumber plumb
#' @importFrom fs path_package
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
daemon <- function() {
	# Read daemonConfig
	configFile <- fs::path_package( "foodguardRanker", "daemon", "daemonConfig" )
	configJson <- jsonlite::fromJSON( configFile )

	# Set router
	router <- plumber::plumb( dir = fs::path_package( "foodguardRanker", "daemon" ) )

	# Run
	router$run( host = configJson$Host, port = configJson$Port )
}
