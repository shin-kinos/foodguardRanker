
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
#' R6 class as utility of Model viewer which is available for all the class, members and functions.
#' @importFrom R6 R6Class
#'
ModelViewerUtility <- R6::R6Class( "ModelViewerUtility",
	public = list(
		#'
		#' @description
		#' Inilialise ModelViewerUtility - so far it does nothing.
		#'
		initialize = function() {},

		#'
		#' @description
		#' Read and organise overview of models available for all the products in JSON format.
		#'
		#' @importFrom jsonlite fromJSON
		#'
		#' @param overviewJsonFiles Vector of character - products overview JSON file paths.
		#'
		getOverviewTable = function( overviewJsonFiles ) {
			# Create empty result table
			overviewTable <- data.frame(
				Title              = character( 0 ),
				Product            = character( 0 ),
				ModellingType      = character( 0 ),
				AnalyticalDatasets = character( 0 ),
				Metadatasets       = character( 0 ),
				ID                 = character( 0 )
			)

			# Iterate JSON file paths to get overview information
			for ( overviewJson in overviewJsonFiles ) {
				# 1. Read JSON content
				jsonContent <- jsonlite::fromJSON( overviewJson )

				# 2. Update overview table
				overviewTable <- rbind( overviewTable,
					list(
						Title          = jsonContent$experimentInfo$title,
						Product        = jsonContent$experimentInfo$productName,
						ModellingType  = jsonContent$mlRankingInfo$modellingType,
						AnalyticalData = paste( names( jsonContent$mlRankingInfo$analyticalDataFiles ), collapse = "," ),
						Metadata       = paste( names( jsonContent$mlRankingInfo$metaDataFiles       ), collapse = "," ),
						ID             = jsonContent$id
					)
				)
			}

			return( overviewTable )
		},

		#'
		#' @description
		#' Get detailed information of one product in table format.
		#'
		#' @importFrom jsonlite fromJSON
		#'
		#' @param productDirPath String - product dir path (database/ID).
		#'
		getProductDetailsTable = function( productDirPath ) {
			# Read JSON content
			overviewFilePath <- paste0( productDirPath, "/", "overview.json" )
			jsonContent  <- jsonlite::fromJSON( overviewFilePath )

			detailsList <- list(
				Title              = jsonContent$experimentInfo$title,
				Description        = jsonContent$experimentInfo$description,
				Author             = jsonContent$experimentInfo$author,
				Product            = jsonContent$experimentInfo$productName,
				Date               = jsonContent$experimentInfo$date,
				ModellingType      = jsonContent$mlRankingInfo$modellingType,
				AnalyticalDatasets = names( jsonContent$mlRankingInfo$analyticalDataFiles ),
				MetaDatasets       = names( jsonContent$mlRankingInfo$metaDataFiles       ),
				MLlist             = jsonContent$mlRankingInfo$mlList,
				ID                 = jsonContent$id,
				FullID             = jsonContent$fullId
				# REVIEW : Should make it array as JSON format.
				# REVIEW : Commented them out at the moment.
				# Author             = paste( jsonContent$experimentInfo$author, collapse = ", " ),
				# AnalyticalDatasets = paste( names( jsonContent$mlRankingInfo$analyticalDataFiles ), collapse = ", " ),
				# MetaDatasets       = paste( names( jsonContent$mlRankingInfo$metaDataFiles       ), collapse = ", " ),
				# MLlist             = paste(        jsonContent$mlRankingInfo$mlList,                collapse = ", " )
			)

			# REVIEW : It should return list - NOT key-value
			# REVIEW : Commented them out at the moment.
			if ( FALSE ) {
				detailsKeyVal <- ""
				for ( key in names( detailsList ) ) {
					keyValRow     <- paste0( key, ":\t", detailsList[[ key ]] )
					detailsKeyVal <- paste( detailsKeyVal, keyValRow, "\n" )
				}
			}

			#return( detailsKeyVal )
			return( detailsList )
		},

		#'
		#' @description
		#' Get ML rankings of one experiment in table format.
		#'
		#' @param rankingsFilePath String - MODELS dir path (database/ID/rankings.csv).
		#'
		getMLmethodRankingsTable = function( rankingsFilePath ) {
			# Read `database/ID/rankings.csv`
			rankingsData <- read.csv( rankingsFilePath, header = TRUE )

			return( rankingsData )
		},

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
# ANCHOR : ModelViewerUtility ends here.

#'
#' R6 class which contains all the available functions for model viewer.
#' @importFrom R6 R6Class
#'
ModelViewerFunctionManager <- R6::R6Class( "ModelViewerFunctionManager",
	public = list(
		#'
		#' @field util Class - class of ModelViewerUtility.
		#'
		util = NULL,

		#'
		#' @description
		#' Inilialise ModelViewerFunctionManager - so far it does nothing.
		#'
		initialize = function() {
			# Activate ModelViewerUtility class
			self$util <- ModelViewerUtility$new()
		},

		#'
		#' @description
		#' Get products' overviews - it shows Title, Product, ModellingType,
		#' AnalyticalData, MetaData and ID. The output results are in the list format
		#' because it is used in the REST API functionality as well. This function is
		#' used for productsOverseer() (exported) function.
		#'
		#' @importFrom jsonlite fromJSON
		#'
		#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
		#' @param filter       Object (list) - list of searching filter for available products.
		#'
		getProductsOverview = function( databaseName, filter = NULL ) {
			# Check if dir `databaseName` exists. If FALSE, return error message.
			if ( !dir.exists( databaseName ) ) {
				#self$util$errorBomb( paste0( "Database '", databaseName, "' was not found." ) )
				return(
					list(
						success = FALSE,
						error   = paste0( "Database ", databaseName, " was not found." )
					)
				)
			}

			# Get all dirs in `databaseName`.
			productDirs <- list.files( databaseName, full.names = TRUE )
			productDirs <- productDirs[ file.info( productDirs )$isdir ]

			# Check if each dir has `overview.json`
			productDirs <- productDirs[ file.exists( paste0( productDirs, "/", "overview.json" ) ) ]

			# Create vector of experiment overview JSON file paths.
			overviewJsonFiles <- sapply( productDirs,
				function( productDir ) paste0( productDir, "/", "overview.json" )
			)

			# Get table of overview
			overviewTableData <- self$util$getOverviewTable( overviewJsonFiles )

			# Return result data as list
			overviewResults <- list(
				success = TRUE,
				data    = overviewTableData
			)

			# TODO : Let's filter the results - check if the format of input filter list is correct

			return( overviewResults )
		},

		#'
		#' @description
		#' Get more detialed information of the product - e.g., 'Title', 'Description',
		#' 'Author', 'Product', 'Date', 'ModellingType' and so on.
		#'
		#' @importFrom jsonlite fromJSON
		#'
		#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
		#' @param id           String - an ID which store models built by foodguardRanker::modelBuilder().
		#'
		getProductDetails = function( databaseName, id ) {
			# Check if dir `databaseName` exists. If FALSE, return error message.
			if ( !dir.exists( databaseName ) ) {
				#self$util$errorBomb( paste0( "Database '", databaseName, "' was not found." ) )
				return(
					list(
						success = FALSE,
						error   = paste0( "Database ", databaseName, " was not found." )
					)
				)
			}

			# Check if dir `databaseName/id` exists. If FALSE, return error message.
			productDirPath <- paste0( databaseName, "/", id )
			if ( !dir.exists( productDirPath ) ) {
				#self$util$errorBomb( paste0( "Experiment '", productDirPath, "' was not found." ) )
				return(
					list(
						success = FALSE,
						error   = paste0( "Product ", productDirPath, " was not found." )
					)
				)
			}

			# Get details results data
			detailsResultsData <- self$util$getProductDetailsTable( productDirPath )

			# Return result data as list
			detailsResults <- list(
				success = TRUE,
				data    = detailsResultsData
			)

			return( detailsResults )
		},

		#'
		#' @description
		#' Get ML rankings of specific product - it reads and shows the content of
		#' 'databaseName/id/rankings'.
		#'
		#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
		#' @param id           String - an ID which store models built by foodguardRanker::modelBuilder().
		#'
		getMLmethodRankings = function( databaseName, id ) {
			# Check if dir `databaseName/id` exists. If FALSE, return error message.
			if ( !dir.exists( databaseName ) ) {
				#self$util$errorBomb( paste0( "Experiment '", experimentDir, "' was not found." ) )
				return(
					list(
						success = FALSE,
						error   = paste0( "Database ", databaseName, " was not found." )
					)
				)
			}

			# Check if file `databaseName/id/rankings.csv` exists. If FALSE, return error message.
			rankingsFilePath <- paste0( databaseName, "/", id, "/", "rankings.csv" )
			if ( !file.exists( rankingsFilePath ) ) {
				#self$util$errorBomb( paste0( "In experiment '", experimentDir, "', file 'rankings.csv' was not found." ) )
				return(
					list(
						success = FALSE,
						error   = paste0( "Invalid input - check database or ID name." )
					)
				)
			}

			# Get rankings data
			rankingsResultsData <- self$util$getMLmethodRankingsTable( rankingsFilePath )

			# Return result data as list
			rankingsResults <- list(
				success = TRUE,
				data    = rankingsResultsData
			)

			return( rankingsResults )
		}
	)
)

#'
#' @title Show overviews of available experiments and their models.
#'
#' @description
#' It shows overviews of available products and their models.
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @importFrom jsonlite toJSON
#'
#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
#' @param type         String - data type of result experiment overviews. "list" or "json", default "list".
#'
#' @export
#'
productsOverseer <- function( databaseName, type = "list" ) {
	# Activate ModelViewerUtility class
	# REVIEW : Needy ???
	util <- ModelViewerUtility$new()

	# Call getProductsOverview from ModelViewerUtility class
	modelViewerFunctionManager <- ModelViewerFunctionManager$new()
	overviewResults <- modelViewerFunctionManager$getProductsOverview( databaseName )

	# If type == "json", convert into JSON
	if( tolower( type ) == "json" ) overviewResults <- jsonlite::toJSON( overviewResults, auto_unbox = TRUE )

	return( overviewResults )
}

#'
#' @title Show detailed information of a specific product and their models.
#'
#' @description
#' It shows more detailed information of the product given by ID which are written in 'ID/overview.json'.
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @importFrom jsonlite toJSON
#'
#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
#' @param id           String - an ID which store models built by foodguardRanker::modelBuilder().
#' @param type         String - data type of result experiment overviews. "list" or "json", default "list".
#'
#' @export
#'
productDetailsViewer <- function( databaseName, id, type = "list" ) {
	# Activate ModelViewerUtility class
	# REVIEW : Needy ???
	util <- ModelViewerUtility$new()

	# Call getProductDetails from ModelViewerUtility class
	modelViewerFunctionManager <- ModelViewerFunctionManager$new()
	detailsResults <- modelViewerFunctionManager$getProductDetails( databaseName, id )

	# If type == "json", convert into JSON
	if( tolower( type ) == "json" ) detailsResults <- jsonlite::toJSON( detailsResults, auto_unbox = TRUE )

	return( detailsResults )
}

#'
#' @title Show rankings of ML method in a specific experiment.
#'
#' @description
#' Show the whole information of the product's ML rankings - it gets the content of file
#' 'mlRankings.csv' by given ID and show them.
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @importFrom jsonlite toJSON
#'
#' @param databaseName String - a name of database which store models built by foodguardRanker::modelBuilder().
#' @param id           String - an ID which store models built by foodguardRanker::modelBuilder().
#' @param type         String - data type of result experiment overviews. "list" or "json", default "list".
#'
#' @export
#'
mlRankingsViewer <- function( databaseName, id, type = "list" ) {
	# Activate ModelViewerUtility class
	# REVIEW : Needy ???
	util <- ModelViewerUtility$new()

	# Call getMLmethodRankings from ModelViewerUtility class
	modelViewerFunctionManager <- ModelViewerFunctionManager$new()
	rankingsResults <- modelViewerFunctionManager$getMLmethodRankings( databaseName, id )

	# If type == "json", convert into JSON
	if( tolower( type ) == "json" ) rankingsResults <- jsonlite::toJSON( rankingsResults, auto_unbox = TRUE )

	return( rankingsResults )
}
