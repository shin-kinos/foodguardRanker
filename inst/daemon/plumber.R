
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

library( "plumber"  )
library( "jsonlite" )
library( "fs"       )
library( "dplyr"    )
library( "mime"     )
library( "foodguardRanker" )

# GLOBAL VARIABLE FOR MODEL DB PATH
dbPath <- NULL

#* @plumber
function( router ) {
	message( "Loading database ..." )
	# Read daemonConfig
	configFile <- fs::path_package( "foodguardRanker", "daemon", "daemonConfig" )
	configJson <- jsonlite::fromJSON( configFile )

	# Set database path
	# NOTE: Be careful the GLOBAL VARIABLE is changed here
	dbPath <<- configJson$DbPath
	message( paste0( "Database: ", dbPath  ) )

	return( router )
}

#* @get /greeting
#*
#* @param name String - example query parameter
function( name = "World" ) {
	result <- list( message = unbox( paste0( "Hello, ", name, "!" ) ) )

	return( result )
}

#* @get /overview
#*
# [e.g.] % curl "http://127.0.0.1:8008/overview"
function() {
	message( paste0( "DbPath: ", dbPath ) )
	# Get overview
	overviewResults <- foodguardRanker::productsOverseer( databaseName = dbPath, type = "list" )
	# Unbox success property
	overviewResults$success <- unbox( overviewResults$success )

	return( overviewResults )
}

#* @get /details
#*
#* @param id String - an ID which store models built by foodguardRanker::modelBuilder().
# [e.g.] % curl "http://127.0.0.1:8008/details?id=26d1bf17656d"
function( id ) {
	# Get product details by ID
	detailsResults <- foodguardRanker::productDetailsViewer(
		databaseName = dbPath,
		id           = id,
		type         = "list"
	)

	# Unbox several properties
	detailsResults$success <- unbox( detailsResults$success )
	for ( property in names( detailsResults$data ) ) {
		if ( !( property %in% c( "Author", "AnalyticalDatasets", "MetaDatasets", "MLlist" ) ) ) {
			detailsResults$data[[ property ]] <- unbox( detailsResults$data[[ property ]] )
		}
	}

	return( detailsResults )
}

#* @get /mlrankings
#*
#* @param id String - an ID which store models built by foodguardRanker::modelBuilder().
# [e.g.] % curl "http://127.0.0.1:8008/mlrankings?id=26d1bf17656d"
function( id ) {
	# Get ML rankings by ID
	rankingsResults <- foodguardRanker::mlRankingsViewer(
		databaseName = dbPath,
		id           = id,
		type         = "list"
	)

	# Unbox success properties
	rankingsResults$success <- unbox( rankingsResults$success )

	return( rankingsResults )
}

#* @post /predict
#*
#* @param id             String - an ID which store models built by foodguardRanker::modelBuilder().
#* @param mlmethod       String - a name of ML method used for the prediction.
#* @param analyticaldata String - a name of analytical data used for the prediction.
#* @param metadata       String - a name of metadata used for the prediction.
#* @param req            Object - a request file data via curl's -F "file=@<yourSensorData.csv>" option.
# [e.g.] % curl -X POST -F "file=@myEnoseData.csv" "http://127.0.0.1:8008/predict?id=26d1bf17656d&mlmethod=svmRadial&analyticaldata=eNose&metadata=TVC"
function(
	id,
	mlmethod,
	analyticaldata,
	metadata,
	req
) {
	# REVIEW : Some sources say "req$files$file$datapath is enougth".
	# REVIEW : But I still do not believe in it!
	uploadedTempFile     <- mime::parse_multipart( req )
	uploadedTempFilePath <- uploadedTempFile$file$datapath
	message( paste0( "Uploaded temp file path: ", uploadedTempFilePath ) )

	predictionResults <- foodguardRanker::predictor(
		databaseName   = dbPath,                  # Database name
		id             = req$args$id,             # ID
		mlMethod       = req$args$mlmethod,       # ML method
		analyticalData = req$args$analyticaldata, # Analytical data
		metadata       = req$args$metadata,       # Metadata
		inputSensor    = uploadedTempFilePath,    # Input sensor
		type           = "list"                   # Output type
	)

	# Unbox success property
	predictionResults$success <- unbox( predictionResults$success )

	# Delete the uploaded temp file
	if ( file.exists( uploadedTempFilePath ) == TRUE ) {
		file.remove( uploadedTempFilePath )
		message( paste0( "Temp file ", uploadedTempFilePath, " was deleted." ) )
	}

	return( predictionResults )
}

#* @post /predict-auto
#*
#* @param product String - a name of product.
#* @param type    String - a type of model, regression or classification, default regression.
#* @param req     Object - a request file data via curl's -F "file=@<yourSensorData.csv>" option.
# [e.g.] % curl -X POST -F "file=@myEnoseData.csv" "http://127.0.0.1:8008/predict-auto?product=mincedbeef&type=regression"
function( predict, type = "regression", req ) {
	# Set default values
	if( is.null( req$args$type ) ) req$args$type <- type

	# REVIEW : Some sources say "req$files$file$datapath is enougth".
	# REVIEW : But I still do not believe in it!
	uploadedTempFile     <- mime::parse_multipart( req )
	uploadedTempFilePath <- uploadedTempFile$file$datapath
	message( paste0( "Uploaded temp file path: ", uploadedTempFilePath ) )

	autoPredictionResults <- foodguardRanker::autoPredictor(
		databaseName = dbPath,               # Database name
		productName  = req$args$product,     # Product name
		modelType    = req$args$type,        # Model type
		inputSensor  = uploadedTempFilePath, # Input sensor file path
		type         = "list"                # Output type
	)
	#print( autoPredictionResults )

	# Unbox several properties
	autoPredictionResults$success <- unbox( autoPredictionResults$success )
	for ( property in names( autoPredictionResults$data ) ) {
		if ( property != "freshness" ) {
			autoPredictionResults$data[[ property ]] <- unbox( autoPredictionResults$data[[ property ]] )
		}
	}

	# Delete the uploaded temp file
	if ( file.exists( uploadedTempFilePath ) == TRUE ) {
		file.remove( uploadedTempFilePath )
		message( paste0( "Temp file ", uploadedTempFilePath, " was deleted." ) )
	}

	return( autoPredictionResults )
}
