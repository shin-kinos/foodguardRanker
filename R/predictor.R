
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
#' R6 class as utility of Predictor which is available for all the class, members and functions.
#' @importFrom R6 R6Class
#'
PredictorUtility <- R6::R6Class( "PredictorUtility",
	public = list(
		#'
		#' @description
		#' Inilialise PredictorUtility - so far it does nothing.
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
# ANCHOR : PredictorUtility ends here.

#'
#' R6 class which contains all the available functions for prediction.
#' @importFrom R6 R6Class
#'
PredictorFunctionManager <- R6::R6Class( "PredictorFunctionManager",
	public = list(
		#'
		#' @field util Class - class of PredictorUtility.
		#'
		util = NULL,

		#'
		#' @description
		#' Inilialise PredictorUtility - so far it does nothing.
		#'
		initialize = function() {
			# Activate PredictorUtility class
			self$util <- PredictorUtility$new()
		},

		#'
		#' @description
		#' Description here.
		#'
		#' @param databaseName   String - a name of database which store models built by foodguardRanker::modelBuilder().
		#' @param id             String - an ID which store models built by foodguardRanker::modelBuilder().
		#' @param mlMethod       String - a name of ML method used for the prediction.
		#' @param analyticalData String - a name of analytical data used for the prediction.
		#' @param metadata       String - a name of metadata used for the prediction.
		#' @param inputSensorDf  Object (data.frame) - a data frame of input sensor data used for the prediction.
		#' @param type           String - data type of result experiment overviews. "list" or "json", default "table".
		#'
		predictFreshness = function(
			databaseName,   # Database name
			id,             # ID
			mlMethod,       # ML method
			analyticalData, # Analytical data
			metadata,       # Metadata
			inputSensorDf   # Input sensor
			) {
			# Check if dir `databaseName/id` exists. If FALSE, return error message.
			productDir <- paste0( databaseName, "/", id )
			if ( !dir.exists( productDir ) ) {
				return(
					list(
						success = FALSE,
						error   = paste0( "Product ID ", id, " was not found." )
					)
				)
			}

			# Check if file `productDir/MODELS` exists. If FALSE, return error message.
			modelsDirPath <- paste0( productDir, "/", "MODELS" )
			if ( !dir.exists( modelsDirPath ) ) {
				return(
					list(
						success = FALSE,
						error   = paste0( "In product ", productDir, ", dir MODELS was not found." )
					)
				)
			}

			# Check if file `productDir/MODELS/mlMethod-analyticalData-metadata.rds` exists.
			# If FALSE, return error message.
			modelFilePath <- paste0( modelsDirPath, "/", mlMethod, "-", analyticalData, "-", metadata, ".rds" )
			if ( !file.exists( modelFilePath ) ) {
				return(
					list(
						success = FALSE,
						error   = paste0(
							"Model not found - please check arguments again (",
							paste( mlMethod, analyticalData, metadata, collapse = ", " ),
							")"
						)
					)
				)
			}

			# Read RDS file of model
			model <- readRDS( modelFilePath )
			#str( model )

			# Get model's train dataset colnames
			# FIXME  : Some models (e.g., svmRadial) do not have model$finalModel$xNames.
			# FIXME  : Find a alternative
			# REVIEW : So far, decided to use model$coefnames, but keep investigating
			modelCols <- model$coefnames
			#print( modelCols )

			# Get input sensor's colnames
			inputSensorCols <- colnames( inputSensorDf )

			# Check and compare the column names of model and input sensors.
			# If not the same, return error message.
			if( setequal( modelCols, inputSensorCols ) == FALSE ) {
				return(
					list(
						success = FALSE,
						error   = "Column names of model data and input sensor were not identical."
					)
				)
			}

			# Predict !!!
			predictionResultsData <- predict( model, newdata = inputSensorDf )

			# Return results data as list
			predictionResults <- list(
				success = TRUE,
				data    = predictionResultsData
			)

			return( predictionResults )
		}
	)
)
# ANCHOR : PredictorFunctionManager ends here.

#'
#' @title Show overviews of available experiments and ther models.
#'
#' @description
#' Description here.
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @importFrom jsonlite toJSON
#'
#' @param databaseName   String - a name of database which store models built by foodguardRanker::modelBuilder().
#' @param id             String - an ID which store models built by foodguardRanker::modelBuilder().
#' @param mlMethod       String - a name of ML method used for the prediction.
#' @param analyticalData String - a name of analytical data used for the prediction.
#' @param metadata       String - a name of metadata used for the prediction.
#' @param inputSensor    String - a filename of input sensor data used for the prediction.
#' @param type           String - data type of result prediction. "list" or "json", default "list".
#'
#' @export
#'
predictor <- function(
	databaseName,   # Database name
	id,             # ID
	mlMethod,       # ML method
	analyticalData, # Analytical data
	metadata,       # Metadata
	inputSensor,    # Input sensor
	type = "list"   # Output type
) {
	# Activate PredictorUtility class
	# REVIEW : Needy ???
	util <- PredictorUtility$new()

	# Open `inputSensor` as data.frame, if file does
	# not exist, return error message
	if ( !file.exists( inputSensor ) ) {
		errorResult <- list(
			success = FALSE,
			error   = paste0( "Sensor data ", inputSensor, " was not found." )
		)
		if ( tolower( type ) == "json" ) return( jsonlite::toJSON( errorResult, auto_unbox = TRUE ) )
		else return( errorResult )
	}

	# Read `inputSensor` as data.frame
	inputSensorDf <- read.csv( inputSensor )

	# Call predictFreshness from PredictorFunctionManage class
	predictorFunctionManager <- PredictorFunctionManager$new()
	predictionResults        <- predictorFunctionManager$predictFreshness(
		databaseName,   # Database name
		id,             # ID
		mlMethod,       # ML method
		analyticalData, # Analytical data
		metadata,       # Metadata
		inputSensorDf   # Input sensor data.frame
	)

	# If type == "json", convert into JSON
	if( tolower( type ) == "json" ) predictionResults <- jsonlite::toJSON( predictionResults, auto_unbox = TRUE )

	return( predictionResults )
}
