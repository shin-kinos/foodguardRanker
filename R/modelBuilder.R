
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
#' R6 class as utility of Model builer which is available for all the class, members and functions.
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#'
ModelBuilderUtility <- R6::R6Class( "ModelBuilderUtility",
	public = list(
		#'
		#' @field mlClassMethodEnum We do not use this anymore !!!
		#' @field outputDir         We do not use this anymore !!!
		#' @field experimentId      String - experiment ID as first 12 characters of fullExperimentId.
		#' @field fullExperimentId  String - full experiment ID as SHA256 value.
		#'
		mlClassMethodEnum = NULL, # FIXME : This field is NOT used anymore - will be removed very soon.
		outputDir         = NULL, # FIXME : This field is NOT used anymore - will be removed very soon.
		experimentId      = NULL,
		fullExperimentId  = NULL,

		#'
		#' @description
		#' Inilialise ModelBuilderUtility - so far it does nothing.
		#'
		initialize = function() {
			# Create pseudo Enum type ML method list
			# FIXME : This list should NOT be initialised.
			# TODO  : Will create separated private member function.
			self$mlClassMethodEnum <- list(
				pls           = "pls",
				knn           = "knn",
				svmRadial     = "svmRadial",
				svmLinear     = "svmLinear",
				svmPoly       = "svmPoly",
				cart          = "rpart",
				randomForest  = "rf",
				xgbTree       = "xgbTree",
				xgbLinear     = "xgbLinear",
				naiveBayes    = "naive_bayes",
				neuralNetwork = "nnet"
			)
		},

		#'
		#' @description
		#' Generate SHA256 string which will be full experiment ID - it generates
		#' full SHA256 hash (fullExperimentId) and its first 12 characters which
		#' will be experiment ID (experimentId).
		#'
		#' @importFrom digest digest
		#'
		generateExperimentIds = function() {
			# Create SHA256 hash
			hash <- digest(
				# 10 norm random numbers whose mean is a random numner and sd is a random number + current time
				paste0( paste0( deparse( rnorm( 10, mean = runif( 1 ), sd = runif( 1 ) ) ) ), Sys.time() ),
				algo = "sha256"
			)

			# NOTE : Use example experiment ID for debugging
			#hash <- "0aa2e5495dcd92231b72d2034faef7344ce106ad7dd949b6e738a1fba05de10e"

			# Set hash as experiment IDs
			fullExperimentId <- hash
			experimentId     <- substr( hash, 1, 12 )

			# Show hash (if required)
			#cat( paste0( "Experiment ID ". self$experimentId, " was generated." ) )

			return (
				list(
					experimentId     = experimentId,
					fullExperimentId = fullExperimentId
				)
			)
		},

		#'
		#' @description
		#' Initialise data frame of classification result log - we do not use this anymore !!!
		#'
		# FIXME : No need for initialisation - will be removed very soon.
		initClassResultLog = function() {
			return (
				data.frame(
					MLmethod       = character( 0 ), # String  : ML method
					AnalyticalData = character( 0 ), # String  : Analytical data type
					Metadata       = character( 0 ), # String  : Metadata type
					Prediction     =   integer( 0 ), # Integer : Predicted value
					Real           =   integer( 0 ), # Integer : Real value
					Iteration      =   integer( 0 )  # Integer : Iteration time
				)
			)
		},

		#'
		#' @description
		#' Initialise data frame of regression result log - we do not use this anymore !!!
		#'
		# FIXME : No need for initialisation - will be removed very soon.
		initClassResultStats = function() {
			return (
				data.frame(
					MLmethod            = character( 0 ), # String  : ML method
					AnalyticalData      = character( 0 ), # String  : Analytical data type
					Metadata            = character( 0 ), # String  : Metadata type
					MinAccuracy         =   numeric( 0 ), # Numeric : Min  accuracy (%)
					MeanAccuracy        =   numeric( 0 ), # Numeric : Mean accuracy (%)
					MaxAccuracy         =   numeric( 0 ), # Numeric : Max  accuracy (%)
					MinKappa            =   numeric( 0 ), # Numeric : Min  kappa [-1, 1]
					MeanKappa           =   numeric( 0 ), # Numeric : Mean kappa [-1, 1]
					MaxKappa            =   numeric( 0 ), # Numeric : Max  kappa [-1, 1]
					bestTunedParameters = character( 0 )  # String  : Stringified best tuned parameters
				)
			)
		},

		#'
		#' @description
		#' Get classification ML method name as caret's argument - it is mainly
		#' used for buildClassModel from MlModelBuilder class.
		#'
		#' @param mlMethod String - ML method name (coming from buildClassModel).
		#'
		getClassMlMethodArg = function( mlMethod ) {
			return (
				switch( mlMethod,
					pls           = "pls",         # Partial least squares
					knn           = "knn",         # k-Nearest neighbors
					svmRadial     = "svmRadial",   # SVM with radial basis function kernel
					svmLinear     = "svmLinear",   # SVM with linear kernel
					svmPoly       = "svmPoly",     # SVM with polynomial kernel
					cart          = "rpart",       # CART
					randomForest  = "rf",          # Random forest (with 'ada' pkg)
					xgbTree       = "xgbTree",     # eXtreme gradient boosting (tree based)
					xgbLinear     = "xgbLinear",   # eXtreme gradient boosting (linear based)
					naiveBayes    = "naive_bayes", # Naive bayes
					neuralNetwork = "nnet",        # Neural network
					NULL                           # Otherwise, return NULL
				)
			)
		},

		#'
		#' @description
		#' Get regression ML method name as caret's argument - it is mainly
		#' used for buildRegModel from MlModelBuilder class.
		#'
		#' @param mlMethod String - ML method name (coming from buildClassModel).
		#'
		getRegMlMethodArg = function( mlMethod ) {
			return (
				switch( mlMethod,
					lm           = "lm",        # Linear model
					glm          = "glm",       # Generalised Linear Model
					pls          = "pls",       # Partial least squares
					pcr          = "pcr",       # Principal component analysis
					lasso        = "lasso",     # Lasso regression (but actually it uses `glmnet` method)
					ridge        = "ridge",     # Ridge regression (but actually it uses `glmnet` method)
					elasticNet   = "enet",      # Elastic net (with `glmnet` and `RcppEigen` pkg)
					knn          = "knn",       # k-Nearest neighbors
					svmRadial    = "svmRadial", # SVM with radial basis function kernel
					svmLinear    = "svmLinear", # SVM with linear kernel
					svmPoly      = "svmPoly",   # SVM with polynomial kernel
					decisionTree = "rpart",     # CART
					randomForest = "rf",        # Random forest (with 'ada' pkg)
					xgbTree      = "xgbTree",   # eXtreme gradient boosting (tree based)
					xgbLinear    = "xgbLinear", # eXtreme gradient boosting (linear based)
					NULL                        # Otherwise, return NULL
				)
			)
		},

		#'
		#' @description
		#' Get result statistics result table for classification by using caret's
		#' confusionMatrix() function - it returns all the results of confusionMatrix()
		#' function anyway. It is mainly used for buildClassModel from MlModelBuilder class.
		#'
		#' @importFrom caret confusionMatrix
		#'
		#' @param predictions Vector of character or string - predicted values.
		#' @param reals       Vector of character or string - real values.
		#'
		getClassStats = function( predictions, reals ) {
			# REVIEW : It might be good if I don't 100% rely on confusionMatrix()
			# REVIEW : function for the stats calculation!

			# Create confusion matrix
			confusionMatrix <- caret::confusionMatrix( predictions, reals )

			# Print confusion matrix (if required)
			#print( confusionMatrix )

			# Return overall results of confusion matrix
			return ( confusionMatrix )
		},

		#'
		#' @description
		#' Get result statistics result table for regression: RMSE; R-Squared; MAE
		#' and Accuracy - it is mainly used for buildRegModel from MlModelBuilder class.
		#'
		#' @param predictions Vector of numeric - predicted values.
		#' @param reals       Vector of numeric - real values.
		#'
		getRegStats = function( predictions, reals ) {
			# TODO: Alpha/beta factors will be implemented.

			# RMSE
			rmse <- sqrt( mean( ( predictions - reals )^2 ) )

			# R-Squared
			residualSoS <- sum( ( predictions - reals )^2   )
			totalSoS    <- sum( ( reals - mean( reals ) )^2 )
			rSquared    <- 1 - ( residualSoS / totalSoS )

			# MAE
			mae <- mean( abs( predictions - reals ) )

			# Accuracy
			# NOTE : Regression's accuracy is calculated as:
			# NOTE : Accuracy = [ total number of TRUEs ] / [total number of samples ] * 100
			# NOTE : where { abs( prediction - real ) < 1.0: TRUE, otherwise: FALSE }
			accuracy <- sum( ifelse( abs( predictions - reals ) < 1, 1, 0 ) ) / length( reals )
			accuracy <- accuracy * 100

			return (
				list(
					RMSE     = rmse,
					RSquared = rSquared,
					MAE      = mae,
					Accuracy = accuracy
				)
			)
		},

		#'
		#' @description
		#' BOOOOOOOOOM !!! !!! !!!
		#'
		#' @param errorContent String - detailed error message.
		#'
		errorBomb = function( errorContent ) {
			# Set error message
			errorMessage <- paste0( "\n\nERROR!: ", errorContent, "\nPROGRAM HALTED !!!\n" )
			# R.I.P !
			stop( errorMessage )
		}
	)
)
# ANCHOR : ModelBuilderUtility ends here.

#'
#' R6 class to read ModelBuilderConfig file and parse the content.
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#'
ModelBuilderConfigParser <- R6::R6Class( "ModelBuilderConfigParser",
	public = list(
		#'
		#' @field experimentInfo List - list of experiment info read from config file.
		#' @field mlRankingInfo  List - list of ML ranking info read from config file.
		#' @field util           Class - class of ModelBuilderUtility.
		#'
		experimentInfo = NULL,
		mlRankingInfo  = NULL,
		util           = NULL,

		#'
		#' @description
		#' Initialise a ModelBuilderConfigParser object to read and extract input config JSON file.
		#' @param configFileName A string representing a name of input config JSON file, default ModelBuilderConfig.
		#'
		initialize = function( configFileName = "ModelBuilderConfig" ) {
			# REVIEW : Should I create a separated configFileReading member ???

			# Activate ModelBuilderUtility class
			self$util <- ModelBuilderUtility$new()

			# Check if configFileName exists, if FALSE, error bomb
			if ( !file.exists( configFileName ) ) { self$util$errorBomb( paste0( "No ", configFileName, " found." ) ) }

			# Read content of input config file
			configContent <- jsonlite::fromJSON( configFileName )

			# Organise config info related to experiment's overview as experimentInfo
			self$experimentInfo <- list(
				title        = configContent$Title,
				description  = configContent$Description,
				author       = configContent$Author,
				organisation = configContent$Organisation,
				productName  = configContent$ProductName,
				date         = configContent$Date,
				outputDir    = configContent$OutputDir
			)

			# Organise config info related to ML modelling as mlRankingInfo
			self$mlRankingInfo <- list(
				modellingType       = configContent$ModellingType,
				mlList              = configContent$MLlist,
				iteration           = configContent$Iteration,
				log10Conversion     = configContent$Log10Conversion,
				trainProportion     = configContent$TrainProportion,
				analyticalDataFiles = configContent$AnalyticalData,
				metaDataFiles       = configContent$Metadata,
				sampleColumnName    = configContent$SampleColumnName,
				trainPreProcess     = configContent$TrainPreprocess,
				knnMaxK             = configContent$KnnMaxK,
				knnKlength          = configContent$KnnKLength,
				svmRadialTuneLength = configContent$SVMRadialTuneLength,
				svmPolyTuneLength   = configContent$SVMPolyTuneLength,
				randomForestNtree   = configContent$RandomForestNtree,
				xgbLinearTuneLength = configContent$XgbLinearTuneLength,
				xgbTreeTuneLength   = configContent$XgbTreeTuneLength,
				lassoMinLambda      = configContent$LassoMinLambda,
				lassoMaxLambda      = configContent$LassoMaxLambda,
				lassoLambdaLength   = configContent$LassoLambdaLength,
				ridgeMinLambda      = configContent$RidgeMinLambda,
				ridgeMaxLambda      = configContent$RidgeMaxLambda,
				ridgeLambdaLength   = configContent$RidgeLambdaLength,
				enetTuneLength      = configContent$EnetTuneLength
			)
		},

		#'
		#' @description
		#' Print a content of input config file when starting program - it is used in
		#' modelBuilder function.
		#'
		showConfigSummary = function() {
			cat( "================================================================================" )
			cat( "\nEXPERIMENT OVERVIEW\n")
			cat( paste0( "    * Title           : ", self$experimentInfo$title      , "\n" ) )
			cat( paste0( "    * Description     : ", self$experimentInfo$description, "\n" ) )
			cat( paste0( "    * Author(s)       : ", paste( self$experimentInfo$author,       collapse = ", " ), "\n" ) )
			cat( paste0( "    * Organisation(s) : ", paste( self$experimentInfo$organisation, collapse = ", " ), "\n" ) )
			cat( paste0( "    * ProductName     : ", self$experimentInfo$productName, "\n" ) )
			cat( paste0( "    * Date            : ", self$experimentInfo$date,        "\n" ) )
			cat( paste0( "    * Output dir name : ", self$experimentInfo$outputDir,   "\n" ) )

			cat( "\nGENERAL ML MODDELLING STRATEGIES\n" )
			cat( paste0( "    * Modelling type   : ", self$mlRankingInfo$modellingType, "\n" ) )
			cat( paste0( "    * Models used      : ", paste( self$mlRankingInfo$mlList, collapse = ", " ), "\n" ) )
			cat( paste0( "    * Iteration        : ", self$mlRankingInfo$iteration,       "\n" ) )
			cat( paste0( "    * Log10 conversion : ", self$mlRankingInfo$log10Conversion, "\n" ) )
			cat( paste0( "    * Train proportion : ", self$mlRankingInfo$trainProportion, "\n" ) )

			cat( "\nINPUT ANALYTICAL/METADATA INFO\n" )
			cat( "    * Analytical data\n"            )
			for( dataType in names( self$mlRankingInfo$analyticalDataFiles ) ) {
				cat( paste0( "        - ", dataType ) )
				cat( paste0( " (", self$mlRankingInfo$analyticalDataFiles[[ dataType ]], ")\n" ) )
			}
			cat( "    * Metadata\n" )
			for( dataType in names( self$mlRankingInfo$metaDataFiles ) ) {
				cat( paste0( "        - ", dataType ) )
				cat( paste0( " (", self$mlRankingInfo$metaDataFiles[[ dataType ]], ")\n" ) )
			}
			cat( paste0( "    * Sample column name : ", self$mlRankingInfo$sampleColumnName, "\n" ) )

			cat( "\nADVANCED ML HYPER PARAMETER SETTINGS\n" )
			cat( paste0( "    * Train Preprocess               : ", paste( self$mlRankingInfo$trainPreProcess, collapse = ", " ), "\n" ) )
			cat( paste0( "    * k-NN max K                     : ", self$mlRankingInfo$knnMaxK,             "\n" ) )
			cat( paste0( "    * k-NN K length                  : ", self$mlRankingInfo$knnKlength,          "\n" ) )
			cat( paste0( "    * SVM (radial) tune length       : ", self$mlRankingInfo$svmRadialTuneLength, "\n" ) )
			cat( paste0( "    * SVM (polynomial) tune length   : ", self$mlRankingInfo$svmPolyTuneLength  , "\n" ) )
			cat( paste0( "    * Random forest tree number      : ", self$mlRankingInfo$randomForestNtree,   "\n" ) )
			cat( paste0( "    * XGB (tree-based) tune length   : ", self$mlRankingInfo$xgbTreeTuneLength,   "\n" ) )
			cat( paste0( "    * XGB (linear-based) tune length : ", self$mlRankingInfo$xgbLinearTuneLength, "\n" ) )
			cat( paste0( "    * Lasso min lambda               : ", self$mlRankingInfo$lassoMinLambda,      "\n" ) )
			cat( paste0( "    * Lasso max lambda               : ", self$mlRankingInfo$lassoMaxLambda,      "\n" ) )
			cat( paste0( "    * Lasso lambda length            : ", self$mlRankingInfo$lassoLambdaLength,   "\n" ) )
			cat( paste0( "    * Ridge min lambda               : ", self$mlRankingInfo$ridgeMinLambda,      "\n" ) )
			cat( paste0( "    * Ridge max lambda               : ", self$mlRankingInfo$ridgeMaxLambda,      "\n" ) )
			cat( paste0( "    * Ridge lambda length            : ", self$mlRankingInfo$ridgeLambdaLength,   "\n" ) )
			cat( paste0( "    * Elastic tune length            : ", self$mlRankingInfo$enetTuneLength,      "\n" ) )
			cat( "================================================================================" )
			cat( "\n\n" )
		},

		#'
		#' @description
		#' Check config file content - if any unexpected pieces of input information
		#' are detected, program is killed immediately with errorBomb member from
		#' ModelBuilderUtility class, BOOOOOOOOOM !!!
		#'
		checkConfigFileContent = function() {
			# Check if all experimental info is OK
			if ( self$experimentInfo$title       == ""           ) { self$util$errorBomb( "Title is required."                         ) }
			if ( self$experimentInfo$description == ""           ) { self$util$errorBomb( "Description is required."                   ) }
			if ( self$experimentInfo$productName == ""           ) { self$util$errorBomb( "Product name is required."                  ) }
			if ( self$experimentInfo$date        == ""           ) { self$util$errorBomb( "Experiment date is required."               ) }
			if ( length( self$experimentInfo$author       ) == 0 ) { self$util$errorBomb( "At least one author name is required."      ) }
			if ( length( self$experimentInfo$organisation ) == 0 ) { self$util$errorBomb( "At leat one organisation name is required." ) }
			if ( NA %in% self$experimentInfo$author              ) { self$util$errorBomb( "Invalid author name(s) included."           ) }
			if ( NA %in% self$experimentInfo$organisation        ) { self$util$errorBomb( "Invalid organisation name(s) included."     ) }
			if ( "" %in% self$experimentInfo$author              ) { self$util$errorBomb( "Invalid author name(s) included."           ) }
			if ( "" %in% self$experimentInfo$organisation        ) { self$util$errorBomb( "Invalid organisation name(s) included."     ) }

			# Check ML modelling info is OK
			if ( typeof( self$mlRankingInfo$iteration ) != "integer"                                  ) { self$util$errorBomb( "Iteration number must be integer."                              ) }
			if ( self$mlRankingInfo$iteration < 3                                                     ) { self$util$errorBomb( "Iteration number must be at least 3 or more."                   ) }
			if ( !is.logical( self$mlRankingInfo$log10Conversion )                                    ) { self$util$errorBomb( "Log 10 conversion must be boolean value (true or false)."       ) }
			if ( !( self$mlRankingInfo$trainProportion %in% c( 0.6, 0.7, 0.8 ) )                      ) { self$util$errorBomb( "Train proportion must be 0.6, 0.7 or 0.8."                      ) }
			if ( !( self$mlRankingInfo$modellingType %in% c( "classification", "regression" ) )       ) { self$util$errorBomb( "Modelling type must be \"classification\" or \"regression\"."   ) }
			if ( length( self$mlRankingInfo$mlList ) != length( unique( self$mlRankingInfo$mlList ) ) ) { self$util$errorBomb( "Duplicated ML methods - check \"MLlist\" in config file again." ) }

			# Check if input ML methods for classification are valid
			if ( self$mlRankingInfo$modellingType == "classification" ) {
				for ( mlMethod in self$mlRankingInfo$mlList ) {
					if ( is.null( self$util$getClassMlMethodArg( mlMethod ) ) ) {
						self$util$errorBomb( paste0( "Invalid classification ML method '", mlMethod, "' is detected." ) )
					}
				}
			}

			# Check if input ML methods for regression are valid
			if ( self$mlRankingInfo$modellingType == "regression" ) {
				for ( mlMethod in self$mlRankingInfo$mlList ) {
					if ( is.null( self$util$getRegMlMethodArg( mlMethod ) ) ) {
						self$util$errorBomb( paste0( "Invalid regression ML method '", mlMethod, "' is detected." ) )
					}
				}
			}
		}
	)
)
# ANCHOR : ModelBuilderConfigParser ends here.

#'
#' R6 class to read, purify and store input experimental datasets.
#' @importFrom R6 R6Class
#'
ModelBuilderInputDataManager <- R6::R6Class( "ModelBuilderInputDataManager",
	public = list(
		#'
		#' @field experimentInfo     List - experimental information extracted by ModelBuilderConfigParser$initialize.
		#' @field mlRankingInfo      List - ML rankings information extracted by ModelBuilderConfigParser$initialize.
		#' @field analyticalDatasets List - analytical data contents linked with their data type (e.g., self$analyticalDatasets$NIR).
		#' @field metaDatasets       List - meta data contents linked with their data type (e.g., self$metaDatasets$TVC).
		#' @field util               Class - class of ModelBuilderUtility.
		#'
		experimentInfo     = NULL,
		mlRankingInfo      = NULL,
		analyticalDatasets = NULL,
		metaDatasets       = NULL,
		util               = NULL,

		#'
		#' @description
		#' Initialise ModelBuilderInputDataManager - it only activates ModelBuilderUtility class.
		#'
		initialize = function() {
			# Activate ModelBuilderUtility class
			self$util <- ModelBuilderUtility$new()
		},

		#'
		#' @description
		#' Read and extract analytical and meta data files in the input config
		#' file - extracted data contents are stored in self$analyticalDatasets
		#' and self$metaDatasets.
		#'
		#' @param experimentInfo List - experimental information extracted by ModelBuilderConfigParser$initialize.
		#' @param mlRankingInfo  List - ML rankings information extracted by ModelBuilderConfigParser$initialize.
		#'
		readDatasets = function( experimentInfo, mlRankingInfo ) {
			# Store experiment overview info to self$mlRankingInfo
			self$experimentInfo <- experimentInfo

			# Store ML ranking info to self$mlRankingInfo
			self$mlRankingInfo <- mlRankingInfo

			# Set analytical/meta data filenames
			analyticalDataFiles <- self$mlRankingInfo$analyticalDataFiles
			metaDataFiles       <- self$mlRankingInfo$metaDataFiles

			# Set analytical/meta datasets as empty lists
			self$analyticalDatasets <- list()
			self$metaDatasets       <- list()

			# Extract and store analytical datasets
			for ( dataType in names( analyticalDataFiles ) ) {
				# Before reading data, check if file exists - if FALSE, terminate program
				if ( !file.exists( analyticalDataFiles[[ dataType ]] ) ) {
					self$util$errorBomb( paste0( "File ", analyticalDataFiles[[ dataType ]], " was not found." ) )
				}

				# Then read target CSV file
				cat( paste0( "Reading ", dataType, " data (", analyticalDataFiles[[ dataType ]], ") ... " ) )
				dataContent <- read.csv( analyticalDataFiles[[ dataType ]], header = TRUE, check.names = TRUE )
				self$analyticalDatasets[[ dataType ]] <- dataContent
				cat( "=> DONE\n")

				# NOTE : IMPORTANT - R does NOT allow colnames as numbers, and it will automatically
				# NOTE : add `X` at the beginning of them. This functionality should NOT be removed by
				# NOTE : `check.names = FALSE` option in `read.csv()` as they cause an error when modelling.
				# NOTE : Therefore, instead of `check.names = FALSE`, it shows waring message about it.
				dangerousColnames <- colnames( dataContent )[ grepl( "^X\\d+", colnames( dataContent ) ) ]
				if ( length( dangerousColnames ) >= 1 ) {
					if( length( dangerousColnames ) > 3 ) dangerousColnames <- dangerousColnames[ 1:3 ]
					cat( paste0( "WARNING in ", dataType, " data - the column names staring from numbers are NOT ALLOWED.\n" ) )
					cat( paste0( "They were automatically converted by adding 'X' such as ", paste( dangerousColnames, collapse = ", " ), " ...\n" ) )
				}
			}

			# Extract and store metaDatasets
			for ( dataType in names( metaDataFiles ) ) {
				# Before reading data, check if file exists - if FALSE, terminate program
				if ( !file.exists( metaDataFiles[[ dataType ]] ) ) {
					self$util$errorBomb( paste0( "File ", metaDataFiles[[ dataType ]], " was not found." ) )
				}

				# Then read target CSV file
				cat( paste0( "Reading ", dataType, " data (", metaDataFiles[[ dataType ]], ") ... " ) )
				dataContent <- read.csv( metaDataFiles[[ dataType ]], header = TRUE, check.names = TRUE )
				self$metaDatasets[[ dataType ]] <- dataContent
				cat( "=> DONE\n")

				# NOTE : IMPORTANT - R does NOT allow colnames starting from numbers, and it will automatically
				# NOTE : add `X` at the beginning of them. This functionality should NOT be removed by
				# NOTE : `check.names = FALSE` option in `read.csv()` as they cause an error when modelling.
				# NOTE : Therefore, instead of `check.names = FALSE`, it shows waring message about it.
				dangerousColnames <- colnames( dataContent )[ grepl( "^X\\d+", colnames( dataContent ) ) ]
				if ( length( dangerousColnames ) >= 1 ) {
					if( length( dangerousColnames ) > 3 ) dangerousColnames <- dangerousColnames[ 1:3 ]
					cat( paste0( "WARNING in ", dataType, " data - the column names staring from numbers are NOT ALLOWED.\n" ) )
					cat( paste0( "They were automatically converted by adding 'X' such as ", paste( dangerousColnames, collapse = ", " ), " ...\n" ) )
				}
			}
		},

		#'
		#' @description
		#' Check if extracted analytical and meta dataset are in valid formats
		#' to proceed to PCAa and modellings - actually they are checked by
		#' private$checkAnalyticalDataset and private$checkMetaDataset members.
		#'
		checkDatasets = function() {
			# Iterate analytical dataset(s)
			for ( dataType in names( self$analyticalDatasets ) ) {
				cat( paste0( "Checking ", dataType, " dataset ... " ) )
				private$checkAnalyticalDataset( dataType, self$analyticalDatasets[[ dataType ]] )
				cat( "=> DONE\n" )
			}

			# Iterate metadata dataset(s)
			for ( dataType in names( self$metaDatasets ) ) {
				cat( paste0( "Checking ", dataType, " dataset ... " ) )
				private$checkMetaDataset( dataType, self$metaDatasets[[ dataType ]] )
				cat( "=> DONE\n" )
			}
		},

		#'
		#' @description
		#' Purify datasets - set row names, remove NA values and omitted samples.
		#'
		#' @importFrom magrittr %>%
		#' @importFrom dplyr select
		#' @importFrom dplyr filter
		#' @importFrom dplyr if_any
		#' @importFrom dplyr everything
		#'
		purifyDatasets = function() {
			# Set sample column name
			sampleColumnName <- self$mlRankingInfo$sampleColumnName

			# For metadata, set log10 conversion flag
			modellingType <- self$mlRankingInfo$modellingType
			convertLog10  <- self$mlRankingInfo$log10Conversion

			# Purify analytical datasets with sample column names
			for ( dataType in names( self$analyticalDatasets ) ) {
				cat( paste0( "Purifying ", dataType, " dataset ... " ) )
				# 0. Check if this data contains `sampleColumnName` column,
				# NOTE : I decided to do this in `checkDatasets` member function, code was deleted

				# 1. Get duplicated samples (if found)
				samples    <- ( self$analyticalDatasets[[ dataType ]] )[[ sampleColumnName ]]
				dupSamples <- unique( samples[ duplicated( samples ) ] )

				# 2. Remove duplicated samples (leave only 1st ones)
				self$analyticalDatasets[[ dataType ]] <- self$analyticalDatasets[[ dataType ]][
					!duplicated( ( self$analyticalDatasets[[ dataType ]] )[[ sampleColumnName ]] ), ]

				# 3. Set row names
				rowNames <- ( self$analyticalDatasets[[ dataType ]] )[[ sampleColumnName ]]
				rownames( self$analyticalDatasets[[ dataType ]] ) <- rowNames

				# 4. Remove sample names column
				self$analyticalDatasets[[ dataType ]] <- self$analyticalDatasets[[ dataType ]] %>%
					dplyr::select( -{{ sampleColumnName }} ) # Select columns except EXCEPT FOR {{ sampleColumnName }}

				# 5. Find samples which contains NA values
				naSamples <- rownames(
					self$analyticalDatasets[[ dataType ]] %>%                          # Get dataset
						dplyr::filter( dplyr::if_any( dplyr::everything(), is.na ) ) ) # and find rows which contain NAs at any columns

				# 6. Remove NA samples
				self$analyticalDatasets[[ dataType ]] <- self$analyticalDatasets[[ dataType ]] %>%          # Get dataset
					dplyr::filter( !( row.names( self$analyticalDatasets[[ dataType ]] ) %in% naSamples ) ) # and filter samples which are NOT omitted samples
				#print( self$analyticalDatasets[[ dataType ]] )

				# 7. Show warning for NA samples if required
				cat( "=> DONE\n" )
				if ( length( naSamples ) >= 1 ) {
					cat( paste0( "WARNING: Following ", length( naSamples ), " samples in ", dataType, " were omitted as they contain NA values:\n" ) )
					cat( paste0( "    * ", paste( naSamples, collapse = "\n    * " ), "\n" ) )
				} else {
					cat( paste0( " - No omitted samples in ", dataType, ", good data!\n" ) )
				}

				# 8. Show warning for NA samples if required
				if ( length( dupSamples ) >= 1 ) {
					cat( paste0( "WARNING: Following ", length( dupSamples ), " sample duplications in ", dataType, " were uniqued:\n" ) )
					cat( paste0( "    * ", paste( dupSamples, collapse = "\n    * " ), "\n" ) )
				} else {
					cat( paste0( " - No duplicated samples in ", dataType, ", good data!\n" ) )
				}

				# 9. Foo bar...
			}

			# Purify meta datasets with sample column names
			for ( dataType in names( self$metaDatasets ) ) {
				cat( paste0( "Purifying ", dataType, " dataset ... " ) )
				# 0. Check if this data contains `sampleColumnName` column,
				# NOTE : I decided to do it in `checkDatasets` member function, code was deleted

				# 1. Get duplicated samples (if found)
				samples    <- ( self$metaDatasets[[ dataType ]] )[[ sampleColumnName ]]
				dupSamples <- unique( samples[ duplicated( samples ) ] )

				# 2. Remove duplicated samples (leave only 1st ones)
				self$metaDatasets[[ dataType ]] <- self$metaDatasets[[ dataType ]][
					!duplicated( ( self$metaDatasets[[ dataType ]] )[[ sampleColumnName ]] ), ]

				# 3. Set row names
				rowNames <- ( self$metaDatasets[[ dataType ]] )[[ sampleColumnName ]]
				rownames( self$metaDatasets[[ dataType ]] ) <- rowNames

				# 4. Remove sample names column
				self$metaDatasets[[ dataType ]] <- self$metaDatasets[[ dataType ]] %>%
					dplyr::select( -{{ sampleColumnName }} ) # Select columns except EXCEPT FOR {{ sampleColumnName }}

				# 5. Find samples which contains NA values
				naSamples <- rownames(
					self$metaDatasets[[ dataType ]] %>%                                # Get dataset
						dplyr::filter( dplyr::if_any( dplyr::everything(), is.na ) ) ) # and find rows which contain NAs at any columns

				# 6. Remove NA samples
				self$metaDatasets[[ dataType ]] <- self$metaDatasets[[ dataType ]] %>%                # Get dataset
					dplyr::filter( !( row.names( self$metaDatasets[[ dataType ]] ) %in% naSamples ) ) # and filter samples which are NOT omitted samples
				#print( self$metaDatasets[[ dataType ]] )

				# 7. Log 10 conversion (if required)
				if ( modellingType == "regression" && convertLog10 == TRUE ) {
					self$metaDatasets[[ dataType ]] <- log10( self$metaDatasets[[ dataType ]] )
					cat( paste0( "\nINFO: ", dataType, " was log10 converted." ) )
				}

				# 7. Show warning for NA samples if required
				cat( "\n=> DONE\n" )
				if ( length( naSamples ) >= 1 ) {
					cat( paste0( "WARNING: Following ", length( naSamples ), " samples in ", dataType, " were omitted as they contain NA values:\n" ) )
					cat( paste0( "    * ", paste( naSamples, collapse = "\n    * " ), "\n" ) )
				} else {
					cat( paste0( " - No omitted samples in ", dataType, ", good data!\n" ) )
				}

				# 8. Show warning for NA samples if required
				if ( length( dupSamples ) >= 1 ) {
					cat( paste0( "WARNING: Following ", length( dupSamples ), " sample duplications in ", dataType, " were uniqued:\n" ) )
					cat( paste0( "    * ", paste( dupSamples, collapse = "\n    * " ), "\n" ) )
				} else {
					cat( paste0( " - No duplicated samples in ", dataType, ", good data!\n" ) )
				}

				# 9. Foo bar ...
			}
			cat( "\n" )
		},

		#'
		#' @description
		#' Show overviews of purified analytical/metadata sets.
		#'
		showDatasetOverviews = function() {
			for ( dataType in names( self$analyticalDatasets ) ) {
				cat( paste0( "Overview of ", dataType, ":\n" ) )
				str( self$analyticalDatasets[[ dataType ]] )
				cat( "\n" )
			}

			for ( dataType in names( self$metaDatasets ) ) {
				cat( paste0( "Overview of ", dataType, ":\n" ) )
				str( self$metaDatasets[[ dataType ]] )
				cat( "\n" )
			}
		}
	),

	private = list(
		#
		# This is a PRIVATE member - no need for roxygen comments.
		# @description
		# Check if the format of analytical data are correct.
		#
		# @param dataType Comment here.
		# @param dataset  Comment here.
		#
		checkAnalyticalDataset = function( dataType, dataset ) {
			# Set sample column name
			sampleColumnName <- self$mlRankingInfo$sampleColumnName

			# 1. Check if this data contains `sampleColumnName` column,
			# if FALSE, terminate program with error message
			if( !sampleColumnName %in% colnames( dataset ) ) {
				self$util$errorBomb( paste0(
					"No column '", sampleColumnName, "' in ", dataType, " dataset - ",
					"check the file content again."
				) )
			}

			# 2. Check if any column contains special characters
			# REVIEW : Should it be error or just warning as tolalent option ???
			dangerousNames <- colnames( dataset )[ grepl( " |\\*|\\(|\\)|\\[|\\]", colnames( dataset )  ) ]
			if ( length( dangerousNames ) >= 1 ) {
				self$util$errorBomb(
					paste0(
						"Following column names contain unacceptable special characters:\n    * ",
						paste( dangerousNames, collapse = "\n    * " ), "\nCheck ", dataType,
						" dataset again and modify these column names."
					)
				)
			}

			# 3. Foo bar...
		},

		#
		# This is a PRIVATE member - no need for roxygen comments.
		# @description
		# Check if the format of analytical data are correct.
		#
		# @importFrom dplyr filter
		#
		# @param dataType Comment here.
		# @param dataset  Comment here.
		#
		checkMetaDataset = function( dataType, dataset ) {
			# Set sample column name
			sampleColumnName <- self$mlRankingInfo$sampleColumnName

			# Set modelling type
			modellingType <- self$mlRankingInfo$modellingType

			# 1. Check if this metadata has only 2 columns (sample names and their freshness indicators)
			if( ncol( dataset ) != 2 ) {
				self$util$errorBomb(
					paste0(
						"Meta dataset must have only 2 columns (sample names and their freshness indicators) - ",
						"check ", dataType, " dataset again."
					)
				)
			}

			# 2. Check if this data contains `sampleColumnName` column,
			# if FALSE, terminate program with error message
			if( !sampleColumnName %in% colnames( dataset ) ) {
				self$util$errorBomb(
					paste0(
						"No column '", sampleColumnName, "' in ", dataType, " dataset - ",
						"check the file content again."
					)
				)
			}

			# 3. Check if any column contains special characters
			# REVIEW : Should it be error or just warning as tolalent option ???
			dangerousNames <- colnames( dataset )[ grepl( " |\\*|\\(|\\)|\\[|\\]", colnames( dataset )  ) ]
			if ( length( dangerousNames ) >= 1 ) {
				self$util$errorBomb(
					paste0(
						"Following column names contain unacceptable special characters:\n    * ",
						paste( dangerousNames, collapse = "\n    * " ), "\nCheck ", dataType,
						" dataset again and modify these column names."
					)
				)
			}

			# 4. Check if this metadata is appropriate for classification
			if ( modellingType == "classification" ) {
				# 4.1. Extract sensory scores from dataset
				sensory <- dataset %>% dplyr::select( -{{ sampleColumnName }} )
				sensory <- sensory[ , 1 ]

				# 4.2. Convert it to factors
				sensory <- as.factor( sensory )

				# 4.3. If number of level if too many (e.g., more than half): give error
				if ( length( levels( sensory ) ) > length( sensory ) / 2 ) {
					self$util$errorBomb(
						paste0(
							"Too many factors - is ", dataType, " data given really for classification ??? ",
							"Check the file content again."
						)
					)
				}
			}

			# 5. Check if this metadata is appropriate for regression
			if ( modellingType == "regression" ) {
				# 5.1. Extract  from dataset
				bacterialCounts <- dataset %>% dplyr::select( -{{ sampleColumnName }} )
				bacterialCounts <- bacterialCounts[ , 1 ]

				# 5.2. If 'bacterialCounts' contains any non-numeric values: give error
				# REVIEW : Still not perfect if classification values are assigned as numbers:
				# REVIEW : e.g., ( Fresh = 1, Semi-fresh = 2, Spoiled = 3 )
				if ( !is.numeric( bacterialCounts ) ) {
					self$util$errorBomb(
						paste0(
							"Meta data for regression must be numeric values - ",
							"check ", dataType, " data file again."
						)
					)
				}
			}

			# 6. Foo bar...
		}
	)
)
# ANCHOR : ModelBuilderInputDataManager ends here.

#'
#' Child R6 class to build machine learning models and evaluate their performances.
#' @importFrom R6 R6Class
#'
MlModelBuilder <- R6::R6Class( "MlModelBuilder",
	# Inherit super class ModelBuilderInputDataManager
	inherit = ModelBuilderInputDataManager,

	public = list(
		#'
		#' @field experimentId        Comment here.
		#' @field fullExperimentId    Comment here.
		#' @field analyticalDataIds   Comment here.
		#' @field outputDir           Comment here.
		#' @field analyticalMetaTypes Comment here.
		#' @field mergedDataset       Comment here.
		#' @field resultTable         Comment here.
		#' @field allPerformances     Comment here.
		#' @field resultStatistics    Comment here.
		#' @field util                Comment here.
		#'
		# @field experimentInfo Comment here.
		# @field mlRankingInfo  Comment here.
		# experimentInfo    = NULL, # FIXME : Already defiend in super class - will be removed very soon
		# mlRankingInfo     = NULL, # FIXME : Already defiend in super class - will be removed very soon
		experimentId        = NULL,
		fullExperimentId    = NULL,
		analyticalDataIds   = NULL, # New property - this is used for automated prediction
		outputDir           = NULL,
		analyticalMetaTypes = NULL,
		mergedDataset       = NULL,
		resultTable         = NULL,
		allPerformances     = NULL,
		resultStatistics    = NULL,
		util                = NULL,

		#'
		#' @description
		#' Initialise MlModelBuilder class.
		#'
		#' @param experimentInfo Comment here.
		#' @param mlRankingInfo  Comment here.
		#'
		initialize = function( experimentInfo, mlRankingInfo ) {
			# Activate ModelBuilderUtility class
			self$util <- ModelBuilderUtility$new()

			# Omajinai (just kinda constructor of super class 'ModelBuilderInputDataManager' )
			super$initialize()

			# Read experiment overview information and
			# analytical/meta data files (by using super class' member function)
			super$readDatasets( experimentInfo, mlRankingInfo )
			#cat( "Inheritance from ModelBuilderInputDataManager DONE!\n" )
			#cat( "Can you get mlRankingInfo from super class ??\n" )
			#print( self$experimentInfo )
			#print( self$mlRankingInfo  )

			# Set analyticalMetaTypes as empty data frame
			self$analyticalMetaTypes <- data.frame()

			# Set all performances log table as empty data frame
			self$allPerformances <- data.frame()

			# Set result statistics table as empty data frame
			self$resultStatistics <- data.frame()

			# Set analytical data IDs as empty list
			self$analyticalDataIds <- list()
		},

		#'
		#' @description
		#' Get experiment IDs by converting random values into SHA256
		#' Check generateExperimentIds() function at ModelBuilderUtility class
		#' for more details.
		#'
		getExperimentIds = function() {
			experimentIds         <- self$util$generateExperimentIds()
			self$experimentId     <- experimentIds$experimentId
			self$fullExperimentId <- experimentIds$fullExperimentId
		},

		#'
		#' @description
		#' Create analytical data IDs - sensor's colnames are combined
		#' and changed into SHA 256. Thise IDs are used for automated
		#' prediction.
		#'
		#' @importFrom digest digest
		#'
		getAnalyticalDataIds = function() {
			# Iterate analytical dataset(s) and generate SHA256
			# based on the colnames
			for ( dataType in names( self$analyticalDatasets ) ) {
				cat( paste0( "Creating ", dataType, " ID ... " ) )
				# Combine colnames by ','
				combinedColNames <- paste( colnames( self$analyticalDatasets[[ dataType ]] ), collapse = "," )
				# Convert it into SHA256
				hash <- digest( combinedColNames, algo = "sha256" )
				# Append to `analyticalDataIds`
				self$analyticalDataIds[[ dataType ]] <- hash
				cat( paste0( "=> DONE (", hash, ")\n" ) )
			}
		},

		#'
		#' @description
		#' Create output dirs.
		#'
		createOutputDirs = function () {
			# If root output dir does not exist; create it
			rootOutputDir <- self$experimentInfo$outputDir
			if ( !dir.exists( rootOutputDir ) ) { dir.create( rootOutputDir ) }

			# Create this experiment's output dir
			self$outputDir <- paste0( rootOutputDir, "/", self$experimentId )
			cat( paste0( "Creating '", self$outputDir, "' ... " ) )

			# TODO : Will check if dir has already existed later on.
			# if ( !dir.exists( self$outputDir ) ) { cat( "Bah !" ) }

			dir.create( self$outputDir )
			cat( paste0( "=> DONE\n" ) )

			cat( paste0( "Creating '", self$outputDir, "/", "MODELs' ... " ) )
			dir.create( paste0( self$outputDir, "/", "MODELs" ) )
			cat( paste0( "=> DONE\n" ) )

			cat( paste0( "Creating '", self$outputDir, "/", "PCAs' ... " ) )
			dir.create( paste0( self$outputDir, "/", "PCAs" ) )
			cat( paste0( "=> DONE\n" ) )
		},

		#'
		#' @description
		#' Create overview of the experiment in JSON format.
		#'
		#' @importFrom jsonlite toJSON
		#'
		createOverView = function() {
			cat( paste0( "Creating '", self$outputDir, "/", "overview.json' ... " ) )

			# Organise all pieces of information for JSON
			outputData <- list(
				id                = self$experimentId,
				fullId            = self$fullExperimentId,
				analyticalDataIds = self$analyticalDataIds,
				experimentInfo    = self$experimentInfo,
				mlRankingInfo     = self$mlRankingInfo
			)

			# Convert it to JSON
			outputJson <- jsonlite::toJSON( outputData, pretty = TRUE, auto_unbox = TRUE )

			# Save
			write( outputJson, file = paste0( self$outputDir, "/", "overview.json" ) )

			cat( paste0( "=> DONE\n" ) )
		},

		#'
		#' @description
		#' Do PCA - output several PC scores (e.g., 1-4 scores) and their
		#' meta data sets in CSV format, plus score variance.
		#' 
		#' @importFrom magrittr %>%
		#' @importFrom dplyr    full_join
		#' @importFrom dplyr    select
		#'
		doPCAs = function() {
			# Set sample column name
			sampleColumnName <- self$mlRankingInfo$sampleColumnName

			# Set modelling type (classification or regression)
			modellingType <- self$mlRankingInfo$modellingType

			# Set analytical/meta datasets
			analyticalDatasets <- self$analyticalDatasets
			metaDatasets       <- self$metaDatasets

			# Iterate all analytical datasets
			for ( analyticalDataType in names( analyticalDatasets ) ) {
				# 1. Set output file name
				outputFileName <- paste0( self$outputDir, "/", "PCAs", "/", analyticalDataType, ".csv" )
				cat( paste0(  "Creating '", outputFileName, "' ... " ) )

				# 2. Do PCA and get PC scores
				analyticalDataset <- analyticalDatasets[[ analyticalDataType ]]
				pcaResult         <- prcomp( analyticalDataset, retx = TRUE, center = TRUE, scale = TRUE )
				pcScores          <- as.data.frame( round( pcaResult$x[ , 1:4 ], 5 ) )

				# 3. Set sample names
				pcScores[[ sampleColumnName ]] <- rownames( pcScores )

				# 4. Get PC score variance labels (%)
				variances <- pcaResult$sdev^2
				varPcs    <- c()
				for ( i in 1 : 4 ) {
					varPc  <- round( 100 * variances[ i ] / sum( variances ), 5   )
					varPcs <-     c( varPcs, paste0( "# PC", i, "=", varPc, "%" ) )
				}
				#print( varPcs )

				# 4. Combine matadata information to pcScores
				for ( metaDataType in names( metaDatasets ) ) {
					# 4.1. Get metadata set
					metaDataset <- metaDatasets[[ metaDataType ]]
					metaDataset[[ sampleColumnName ]] <- rownames( metaDataset )

					# 4.2. Change column name into 'metadataType'
					colnames( metaDataset )[ 1 ] <- metaDataType

					# 4.3. Purify metadata values:
					#      If classication; make them factors.
					#      If regression;   round them as 5 decimal digits
					metaDataset[[ metaDataType ]] <- switch( modellingType,
						"classification" = as.factor( metaDataset[[ metaDataType ]]    ),
						"regression"     =     round( metaDataset[[ metaDataType ]], 5 )
						# REVIEW : Should exception error handling be implemented ???
					)

					# 4.4 Merge them together
					# NOTE   : If unmatching samples, set NA.
					# TODO   : Will set warning messages for unmatching samples later on.
					# REVIEW : Still not sure I should put '{{ sampleColumnName }}' or 'sampleColumnName'
					# REVIEW : If something wrong happens, try add {{}} to 'sampleColumnName'
					pcScores <- dplyr::full_join( pcScores, metaDataset, by = sampleColumnName )
				}

				# 5. Move 'Sample' to 1st column
				sampleColumn <- pcScores %>% dplyr::select(  {{ sampleColumnName }} )
				pcScores     <- pcScores %>% dplyr::select( -{{ sampleColumnName }} )
				pcScores     <- cbind( sampleColumn, pcScores )
				#print( pcScores )

				# 6. Write output file - varPCs at the first place
				write( paste0( paste( varPcs, collapse = "\n" ), "\n" ), file = outputFileName, append = FALSE )

				# 7. Write output file colnames of PC scores
				write( paste( colnames( pcScores ), collapse = "," ), file = outputFileName, append = TRUE )

				# 8. Then append PCA scores
				sapply( 1 : nrow( pcScores ),
					function( row )
						write( paste( pcScores[ row, ], collapse = "," ), file = outputFileName, append = TRUE )
				)

				# TODO : Will set warning messages for unmatching samples later on
				cat( "=> DONE\n" )
			}
		},

		#'
		#' @description
		#' Get types of analytical/meta data.
		#'
		getAnalyticalMetaTypes = function() {
			# For loop to get all the combinations
			for ( analyticalDataType in names( self$analyticalDatasets ) ) {
				for ( metaDataType in names( self$metaDatasets ) ) {
					analyticalMetaType <- list(
						analyticalDataType = analyticalDataType,
						metaDataType       = metaDataType
					)
					self$analyticalMetaTypes <- rbind(
						self$analyticalMetaTypes,
						analyticalMetaType
					)
				}
			}
		},

		#'
		#' @description
		#' Do modellings.
		#'
		doModellings = function() {
			# Set modelling type (classification or regression)
			modellingType <- self$mlRankingInfo$modellingType

			# Set ML list
			mlList <- self$mlRankingInfo$mlList

			# FIXME : No need for initialisation - will be removed very soon
			# Initialise result log/statistics data frame:
			# Classification or regression ???
			if ( FALSE ) {
				#if ( modellingType == "classification" ) {
				#	self$resultLog        <- self$util$initClassResultLog
				#	self$resultStatistics <- self$util$initClassResultStats
				#} else if ( modellingType == "regression" ) {}
			}

			# FIXME : No need for initialisation - will be removed very soon
			# NOTE : Why 'write.table', not 'write.csv' ??? <= 'append = <bool>' is not available for 'write.csv'
			if ( FALSE ) {
				#write.table( self$resultLog, paste0( self$outputDir, "/", "resultLog.csv" ),
				#	sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, append = FALSE )
				#write.table( colnames( self$resultStatistics ), paste0( self$outputDir, "/", "rankings.csv" ),
				#	sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, append = FALSE )
			}

			# Iterate analytical-meta datasets over all of ML methods
			for ( i in 1 : nrow( self$analyticalMetaTypes ) ) {
				# 1. Refresh merged dataset
				self$mergedDataset <- NULL
				self$mergedDataset <- data.frame()

				# 2. Get target data types (e.g., 'eNose' vs 'TVC', 'VM' vs 'pseudomonads' and etc.)
				targetDataTypes <- ( self$analyticalMetaTypes )[ i, ]

				# 3. (Re)create merged data with target data types
				private$createMergedDataset( targetDataTypes )

				# 4. Go modelling over every single ML methods !!!
				for ( mlMethod in mlList ) {
					# Classification or regression ???
					if      ( modellingType == "classification" ) { private$buildClassModel( targetDataTypes, mlMethod ) }
					else if ( modellingType == "regression"     ) { private$buildRegModel(   targetDataTypes, mlMethod ) }
				}

				# 5. Foo bar...
			}

			# Print result Statistics
			cat( "\n=> DONE - FINAL RESULT STATISTICS:\n" )
			print( self$resultStatistics )

			# Print result log (if required)
			#print( self$allPerformance )
		}
	),

	private = list(
		#
		# This is a PRIVATE member - no need for roxygen comments.
		# @description
		# Marge analytical and meta dataset for modelling.
		#
		# @param targetDataTypes Comment here.
		#
		createMergedDataset = function( targetDataTypes ) {
			cat( paste0( "Merging ", targetDataTypes$analyticalDataType, " and ", targetDataTypes$metaDataType, " datasets ... " ) )

			# Set ML modelling type ('classification' or 'regression')
			modellingType <- self$mlRankingInfo$modellingType

			# Set target data types
			analyticalDataType <- targetDataTypes$analyticalDataType
			metaDataType       <- targetDataTypes$metaDataType

			# Set target analytical/meta datasets
			analyticalDataset <- self$analyticalDatasets[[ analyticalDataType ]]
			metaDataset       <- self$metaDatasets[[ metaDataType ]]

			# Change metadata column name into 'metadata'
			colnames( metaDataset )[ 1 ] <- "metadata"

			# Detect matched/unmatched samples
			matchedSamples   <- intersect( rownames( analyticalDataset ), rownames( metaDataset ) )
			unmatchedSamples <- c(
				setdiff( rownames( analyticalDataset ), rownames( metaDataset       ) ),
				setdiff( rownames( metaDataset       ), rownames( analyticalDataset ) )
			)

			# Filter datasets so that both of datasets have only matched samples
			analyticalDataset <- subset( analyticalDataset, rownames( analyticalDataset ) %in% matchedSamples )
			metaDataset       <- subset( metaDataset,       rownames( metaDataset )       %in% matchedSamples )

			# Merge analytical/meta datasets (and just in case omit NAs)
			mergedDataset <- cbind( metaDataset, analyticalDataset ) %>% na.omit
			#str( mergedDataset )

			# Set metadata's type (numeric for regression, and factor for classification)
			# REVIEW : This is VERY important - should a separated (private) member function be created ???
			# TODO   : Will add exception handling later on.
			if (      modellingType == "classification" ) { mergedDataset$metadata <- as.factor(  mergedDataset$metadata ) }
			else if ( modellingType == "regression"     ) { mergedDataset$metadata <- as.numeric( mergedDataset$metadata ) }
			else                                          { cat( "WEIRED!\n" ) }

			cat( "=> DONE\n" )
			# Show warning for unmatched samples if required
			if ( length( unmatchedSamples ) >= 1 ) {
				cat( paste0(
					"WARNING: Following unmatched ", length( unmatchedSamples ), " samples (between ",
					analyticalDataType, " and ", metaDataType, ") were omitted:\n    * ",
					paste( unmatchedSamples, collapse = "\n    * " ), "\n"
				) )
			} else {
				cat( paste0( "No unmatched samples between ", analyticalDataType, " and ", metaDataType, " - good data!\n" ) )
			}

			# Show overview of merged dataset if required <= NOT required - it will look too messy!
			#cat( "Overview of merged dataset:\n" )
			#str( mergedDataset )

			# All good? - assign it to the field!
			self$mergedDataset <- mergedDataset
		},

		#
		# This is a PRIVATE member - no need for roxygen comments.
		# @description
		# Build classification model.
		#
		# @importFrom caret createDataPartition
		# @importFrom magrittr %>%
		# @importFrom dplyr slice
		# @importFrom caret train
		#
		# @param targetDataTypes Comment here.
		# @param mlMethod        Comment here.
		#
		buildClassModel = function( targetDataTypes, mlMethod ) {
			cat( paste0( "\nBuilding a classification model ... (",
				targetDataTypes$analyticalDataType, ", ",
				targetDataTypes$metaDataType, ", ",
				mlMethod, ")"
			) )

			# Set target ML name as caret's arg
			mlMethodArg <- self$util$getClassMlMethodArg( mlMethod )

			# Set general params for modelling
			analyticalDataType <- targetDataTypes$analyticalDataType # Analytical data type
			metaDataType       <- targetDataTypes$metaDataType       # Meta data type
			mergedDataset      <- self$mergedDataset                 # Target merged data
			trainProportion    <- self$mlRankingInfo$trainProportion # Train proportion
			iteration          <- self$mlRankingInfo$iteration       # Iteration time

			# Set train preprocess arg
			trainPreProcess <- self$mlRankingInfo$trainPreProcess

			# Set advanced params for hyper parameter tuning (for k-NN)
			knnMaxK    <- self$mlRankingInfo$knnMaxK
			knnKlength <- self$mlRankingInfo$knnKlength
			knnKRange  <- round( seq( 3, knnMaxK, length = knnKlength ) )

			# Set advanced params for hyper parameter tuning (for SVMs)
			svmRadialTuneLength <- self$mlRankingInfo$svmRadialTuneLength           # SVM radial
			svmPolyTuneLength   <- self$mlRankingInfo$svmPolyTuneLength             # SVM poly

			# Set advanced params for hyper parameter tuning (for Random forest)
			randomForestNtree <- self$mlRankingInfo$randomForestNtree  # Comment here

			# Set advanced params for hyper parameter tuning (for XGBoosts)
			xgbLinearTuneLength <- self$mlRankingInfo$xgbLinearTuneLength
			xgbTreeTuneLength   <- self$mlRankingInfo$xgbTreeTuneLength

			cat( paste0( "\nFinding the best tuned parameter(s) ... \n" ) )

			# Set best tuned params
			bestTunedParams <- NULL

			# Create train-test partition index
			# REVIEW : Should it be coded by myself ???
			trainIndex <- caret::createDataPartition( mergedDataset$metadata, p = trainProportion )$Resample1

			# Create train/test data
			# REVIEW : Is testData needy ???
			trainData <- mergedDataset %>% dplyr::slice(  trainIndex )
			testData  <- mergedDataset %>% dplyr::slice( -trainIndex )

			# Build model (WITHOUT best tuned params)
			# NOTE   : Several methods need to slightly change the parameters of caret::train() function:
			# NOTE   : knn      : add `k`
			# NOTE   : SVMs     : add `tuneLength`
			# NOTE   : RF       : add `ntree`
			# NOTE   : XGBoosts : add `verbosity`
			# NOTE   : nnet     : add `trace` and `MaxNWts`
			# FIXME  : The nnet method might return error when inputting large sized data (e.g., 5MB~)
			# FIXME  : MaxNWts option needs to increase to solve this issue.
			# REVIEW : Should MaxNWts NOT be Inf ??? (if Inf, it will take ages!)
			# REVIEW : The number should be large enough (e.g., 15000, 30000, 99999) but no need to be Inf ???
			model <- switch( mlMethodArg,
				"knn"       = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid   = data.frame( k = knnKRange      ) ),
				"svmRadial" = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = svmRadialTuneLength              ),
				"svmPoly"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = svmPolyTuneLength                ),
				"rf"        = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, ntree      = randomForestNtree                ),
				"xgbLinear" = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = xgbLinearTuneLength              ),
				"xgbTree"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = xgbTreeTuneLength, verbosity = 0 ),
				"nnet"      = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, trace      = FALSE, MaxNWts = Inf             ),
				caret::train(               metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess )
			)

			# Assign best tuned params
			bestTunedParams <- model$bestTune

			cat( "=> DONE\n" )

			cat( "Model generated:\n" )
			print( model )

			cat( "Best tuned hyper parameter(s):\n" )
			print( bestTunedParams )

			# Do modelling with best tuned params
			trainIndex       <- NULL
			trainData        <- NULL
			testData         <- NULL
			reals            <- c()
			predictions      <- c()
			model            <- NULL
			resultAccuracies <- c()
			resultKappas     <- c()
			bestModel        <- NULL
			bestAccuracy     <- -999999

			cat( paste0( "Modelling with best tuned parameter(s) ...\n" ) )
			for ( i in 1 : iteration ) {
				cat( "Iteration: ", i, "/", iteration, " ...\n" )

				# 1. Create train-test partition index
				# REVIEW : Should it be coded by myself ???
				trainIndex <- createDataPartition( mergedDataset$metadata, p = trainProportion )$Resample1

				# 2. Create train/test data
				trainData <- mergedDataset %>% dplyr::slice(  trainIndex )
				testData  <- mergedDataset %>% dplyr::slice( -trainIndex )

				# 3. Get real data for testing
				reals <- testData$metadata

				# 4. Build model (WITHOUT best tuned params)
				# NOTE   : Several methods need to slightly change the parameters of caret::train() function:
				# NOTE   : XGB(tree) : add `verbosity`
				# NOTE   : nnet      : add `trace` and `MaxNWts`
				# FIXME  : The nnet method might return error when inputting large sized data (e.g., 5MB~)
				# FIXME  : MaxNWts option needs to increase to solve this issue.
				# REVIEW : Should MaxNWts NOT be Inf ??? (if Inf, it will take ages!)
				# REVIEW : The number should be large enough (e.g., 15000, 30000, 99999) but no need to be Inf ???
				model <- switch( mlMethodArg,
					"rf"        = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams, ntree = randomForestNtree    ),
					"xgbTree"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams, verbosity = 0                ),
					"nnet"      = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams, trace = FALSE, MaxNWts = Inf ),
					caret::train(               metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams                               )
				)

				# 5. Show modelling result (if required)
				#print( model )

				# 6. Predict test data
				predictions <- predict( model, newdata = testData )

				# 7. Get overall statistics
				resultStats <- self$util$getClassStats( predictions, reals )
				accuracy    <- resultStats$overall[ "Accuracy" ]
				kappa       <- resultStats$overall[ "Kappa"    ]

				# 8. If accuracy hits a new record, update best model
				if ( accuracy > bestAccuracy ) {
					bestModel    <- model
					bestAccuracy <- accuracy
				}

				# 9. Show comparison (if required)
				#cat( paste0( "Iteration   : ", i, "\n" ) )
				#cat( paste0( "Predictions : ", paste( predictions, collapse = ", " ), "\n" ) )
				#cat( paste0( "Reals       : ", paste( reals,       collapse = ", " ), "\n" ) )
				#cat( paste0( "Accuracy    : ", round( accuracy, 5 ) ), "\n"   )
				#cat( paste0( "Kappa       : ", round( kappa,    5 ) ), "\n\n" )

				# 10. Store result accuracy and kappa
				resultAccuracies <- c( resultAccuracies, accuracy )
				resultKappas     <- c( resultKappas,     kappa    )

				# 11. Organise performance log in this iteration
				rowLength <- length( reals )
				performanceRows <- list(
					MLmethod       = rep( mlMethod, rowLength ),
					AnalyticalData = rep( analyticalDataType, rowLength ),
					Metadata       = rep( metaDataType, rowLength ),
					Prediction     = as.integer( predictions ),
					Real           = as.integer( reals ),
					Iteration      = rep( i, rowLength )
				)

				# 12. Update all performance logs
				self$allPerformances <- rbind( self$allPerformances, performanceRows )

				# 13. Update 'allPerformances.csv'
				outputFilePath <- paste0( self$outputDir, "/", "allPerformances.csv" )
				#cat( paste0( "Adding new data to '", outputFilePath, "' ... " ) )
				write.csv( self$allPerformances, outputFilePath, row.names = FALSE, quote = FALSE )
				#cat( "=> DONE\n" )
			}

			# Create stringified best tuned params for the results
			bestTunedParamsStr <- sapply( names( bestTunedParams ),               # For loop over names( bestTunedParams )
				function( param )                                                 # 'param' is iterator
					paste0( param, "=",                                           # Paste 'param' and '='
						ifelse( typeof( bestTunedParams[[ param ]] ) == "double", # If type of param is double...
							round( bestTunedParams[[ param ]], 5 ),               # Round it with 5 digits
								bestTunedParams[[ param ]] ) ) ) %>%              # Otherwise, do nothing, and (%>%)
									paste( collapse = "|" )                       # Paste vector with '|'

			# Organise results
			resultStatsRow <- list(
				MLmethod            = mlMethod,
				AnalyticalData      = analyticalDataType,
				Metadata            = metaDataType,
				MinAccuracy         = round( min(  resultAccuracies ) * 100, 5 ),
				MeanAccuracy        = round( mean( resultAccuracies ) * 100, 5 ),
				MaxAccuracy         = round( max(  resultAccuracies ) * 100, 5 ),
				MinKappa            = round( min(  resultKappas     ),       5 ),
				MeanKappa           = round( mean( resultKappas     ),       5 ),
				MaxKappa            = round( max(  resultKappas     ),       5 ),
				bestTunedParameters = bestTunedParamsStr
			)

			# Show result row
			cat( "=> DONE\n" )
			cat( paste0( "Performance statistics (", analyticalDataType, ", ", metaDataType, ", ", mlMethod, "):\n" ) )
			print( as.data.frame( resultStatsRow ) )

			# Update result statistics
			self$resultStatistics <- rbind( self$resultStatistics, resultStatsRow )

			# Sort by 'MeanAccuracy'
			self$resultStatistics <- self$resultStatistics[ order( self$resultStatistics$MeanAccuracy, decreasing = TRUE ), ]

			# Update 'rankings.csv'
			#cat( paste0( "Adding new data to '", paste0( self$outputDir, "/", "rankings.csv" ), "' ... " ) )
			write.csv( self$resultStatistics, paste0( self$outputDir, "/", "rankings.csv" ), row.names = FALSE, quote = FALSE )
			#cat( "=> DONE\n" )

			# Save (the best performed) model as rds file
			# REVIEW : If required, will modify file name into more proper format ???
			# REVIEW : e.g., name should be all capitalised ???
			bestModelFileName <- paste0( paste( mlMethod, analyticalDataType, metaDataType, sep = "-" ), ".rds" )
			cat( paste0( "Saving best-performed model data '", bestModelFileName, "' ... " ) )
			saveRDS( bestModel, file = paste0( self$outputDir, "/", "MODELs", "/", bestModelFileName ) )
			cat( "=> DONE\n" )
		},

		#
		# This is a PRIVATE member - no need for roxygen comments.
		# @description
		# Build regression model.
		#
		# @importFrom caret createDataPartition
		# @importFrom magrittr %>%
		# @importFrom dplyr slice
		# @importFrom caret train
		#
		# @param targetDataTypes Comment here.
		# @param mlMethod        Comment here.
		#
		buildRegModel = function( targetDataTypes, mlMethod ) {
			cat( paste0( "\nBuilding a regression model ... (",
				targetDataTypes$analyticalDataType, ", ",
				targetDataTypes$metaDataType, ", ",
				mlMethod, ")"
			) )

			# Set target ML name as caret's arg
			mlMethodArg <- self$util$getRegMlMethodArg( mlMethod )

			# Set general params for modelling
			analyticalDataType <- targetDataTypes$analyticalDataType   # Analytical data type
			metaDataType       <- targetDataTypes$metaDataType         # Meta data type
			mergedDataset      <- self$mergedDataset                   # Target merged data
			trainProportion    <- self$mlRankingInfo$trainProportion   # Train proportion
			iteration          <- self$mlRankingInfo$iteration         # Iteration time

			# Set train preprocess arg
			trainPreProcess <- self$mlRankingInfo$trainPreProcess

			# Set advanced params for hyper parameter tuning (for k-NN)
			knnMaxK    <- self$mlRankingInfo$knnMaxK
			knnKlength <- self$mlRankingInfo$knnKlength
			knnKRange  <- round( seq( 3, knnMaxK, length = knnKlength ) )

			# Set advanced params for hyper parameter tuning (for SVMs)
			svmRadialTuneLength <- self$mlRankingInfo$svmRadialTuneLength # SVM radial
			svmPolyTuneLength   <- self$mlRankingInfo$svmPolyTuneLength   # SVM poly

			# Set advanced params for hyper parameter tuning (for Random forest)
			randomForestNtree <- self$mlRankingInfo$randomForestNtree  # Comment here

			# Set advanced params for hyper parameter tuning (for XGBoosts)
			xgbLinearTuneLength <- self$mlRankingInfo$xgbLinearTuneLength
			xgbTreeTuneLength   <- self$mlRankingInfo$xgbTreeTuneLength

			# Set advanced params for hyper parameter tuning (for Glmnet bros.)
			lassoMinLambda    <- self$mlRankingInfo$lassoMinLambda    # Max lambda (Lasso)
			lassoMaxLambda    <- self$mlRankingInfo$lassoMaxLambda    # Min lambda (Lasso)
			lassoLambdaLength <- self$mlRankingInfo$lassoLambdaLength # Len lambda (Lasso)
			ridgeMinLambda    <- self$mlRankingInfo$ridgeMinLambda    # Max lambda (Ridge)
			ridgeMaxLambda    <- self$mlRankingInfo$ridgeMaxLambda    # Min lambda (Ridge)
			ridgeLambdaLength <- self$mlRankingInfo$ridgeLambdaLength # Len lambda (Ridge)
			lassoLambdaRange  <- seq( lassoMinLambda, lassoMaxLambda, length = lassoLambdaLength )
			ridgeLambdaRange  <- seq( ridgeMinLambda, ridgeMaxLambda, length = ridgeLambdaLength )
			enetTuneLength    <- self$mlRankingInfo$enetTuneLength

			cat( paste0( "\nFinding the best tuned parameter(s) ...\n" ) )

			# Set best tuned params
			bestTunedParams <- NULL

			# Create train-test partition index
			# REVIEW : Should it be coded by myself ???
			trainIndex <- caret::createDataPartition( mergedDataset$metadata, p = trainProportion )$Resample1

			# Create train/test data
			# REVIEW : Is testData needy here ???
			trainData <- mergedDataset %>% dplyr::slice(  trainIndex )
			testData  <- mergedDataset %>% dplyr::slice( -trainIndex )

			# Build model (WITHOUT best tuned params)
			# NOTE   : Several methods need to slightly change the parameters of caret::train() function:
			# NOTE   : Glmnets    : add `alpha` and `lambda`
			# NOTE   : k-NN       : add `k`
			# NOTE   : SVMs       : add `tuneLength`
			# NOTE   : RF         : add `ntree`
			# NOTE   : XGB (tree) : and `verbosity`
			# REVIEW : About XGB (tree), what about `max_depth` and `subsample` ??? <= NO - tuneLength is enough
			model <- switch( mlMethodArg,
				"lasso"     = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneGrid   = data.frame( alpha = c( 1 ), lambda = lassoLambdaRange ) ),
				"ridge"     = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneGrid   = data.frame( alpha = c( 0 ), lambda = ridgeLambdaRange ) ),
				"enet"      = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneLength = enetTuneLength                   ),
				"knn"       = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid   = data.frame( k = knnKRange )      ),
				"svmRadial" = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = svmRadialTuneLength              ),
				"svmPoly"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = svmPolyTuneLength                ),
				"rf"        = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, ntree      = randomForestNtree                ),
				"xgbLinear" = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = xgbLinearTuneLength              ),
				"xgbTree"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneLength = xgbTreeTuneLength, verbosity = 0 ),
				caret::train(               metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess )
			)

			# Assign best tuned params
			bestTunedParams <- model$bestTune

			cat( "=> DONE\n" )

			cat( "Model generated:\n" )
			print( model )

			cat( "Best tuned hyper parameter(s):\n" )
			print( bestTunedParams )

			# Do modelling with best tuned params
			trainIndex       <- NULL
			trainData        <- NULL
			testData         <- NULL
			reals            <- c()
			predictions      <- c()
			model            <- NULL
			resultRmses      <- c()
			resultRsquareds  <- c()
			resultAccuracies <- c()
			resultMaes       <- c()
			bestModel        <- NULL
			bestRmse         <- 999999

			cat( paste0( "Modelling with best tuned parameters ...\n" ) )
			for ( i in 1 : iteration ) {
				cat( "Iteration: ", i, "/", iteration, " ...\n" )

				# 1. Create train-test partition index
				# REVIEW : Should it be coded by myself ???
				trainIndex <- caret::createDataPartition( mergedDataset$metadata, p = trainProportion )$Resample1

				# 2. Create train/test data
				trainData <- mergedDataset %>% dplyr::slice(  trainIndex )
				testData  <- mergedDataset %>% dplyr::slice( -trainIndex )

				# 3. Get real data for testing
				reals <- testData$metadata

				# 4. Build model (WITH best tuned params)
				# NOTE : Several methods need to slightly change the parameters of caret::train() function:
				# NOTE : Glmnets    : add `alpha` and `lambda`
				# NOTE : RF         : add `ntree`
				# NOTE : XGB (tree) : add `verbosity`
				model <- switch( mlMethodArg,
					"lasso"     = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams ),
					"ridge"     = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams ),
					"enet"      = caret::train( metadata ~ ., method =    "glmnet", data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams ),
					"rf"        = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams, ntree = randomForestNtree ),
					"xgbTree"   = caret::train( metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams, verbosity = 0 ),
					caret::train(               metadata ~ ., method = mlMethodArg, data = trainData, preProcess = trainPreProcess, tuneGrid = bestTunedParams )
				)

				# 5. Show modelling result (if required)
				#cat( "Model generated:\n" )
				#print( model )

				# 6. Predict test data
				predictions <- predict( model, newdata = testData )

				# 7. Get overall statistics
				resultStats <- self$util$getRegStats( predictions, reals )
				rmse        <- resultStats$RMSE
				rSquared    <- resultStats$RSquared
				accuracy    <- resultStats$Accuracy
				mae         <- resultStats$MAE

				# 8. If RMSE hits a new record, update best model
				if ( rmse < bestRmse ) {
					bestModel <- model
					bestRmse  <- rmse
				}

				# 9. Show comparison (if required)
				#cat( paste0( "Iteration   : ", i, "\n" ) )
				#cat( paste0( "Predictions : ", paste( round( predictions, 5 ), collapse = ", " ), "\n" ) )
				#cat( paste0( "Reals       : ", paste( round( reals,       5 ), collapse = ", " ), "\n" ) )
				#cat( paste0( "RMSE        : ", round( rmse,               5 ) ), "\n" )
				#cat( paste0( "Rsquared    : ", round( rSquared,           5 ) ), "\n" )
				#cat( paste0( "MAE         : ", round( mae,                5 ) ), "\n" )
				#cat( paste0( "Accuracy    : ", round( accuracy,           5 ) ), " %\n\n" )

				# 10. Store result RMSE, RSquared, accuracy and MAE
				resultRmses      <- c( resultRmses,      rmse     )
				resultRsquareds  <- c( resultRsquareds,  rSquared )
				resultAccuracies <- c( resultAccuracies, accuracy )
				resultMaes       <- c( resultMaes,       mae      )

				# 11. Organise performance log in this iteration
				rowLength <- length( reals )
				performanceRows <- list(
					MLmethod       = rep( mlMethod, rowLength ),
					AnalyticalData = rep( analyticalDataType, rowLength ),
					Metadata       = rep( metaDataType, rowLength ),
					Prediction     = round( predictions, 5 ),
					Real           = round( reals,       5 ),
					Iteration      = rep( i, rowLength )
				)

				# 12. Update all performance logs
				self$allPerformances <- rbind( self$allPerformances, performanceRows )

				# 13. Update 'allPerformances.csv'
				outputFilePath <- paste0( self$outputDir, "/", "allPerformances.csv" )
				#cat( paste0( "Adding new data to '", outputFilePath, "' ... " ) )
				write.csv( self$allPerformances, outputFilePath, row.names = FALSE, quote = FALSE )
				#cat( "=> DONE\n" )
			}

			# Create stringified best tuned params for the results
			bestTunedParamsStr <- sapply( names( bestTunedParams ),               # For loop over names( bestTunedParams )
				function( param )                                                 # 'param' is iterator
					paste0( param, "=",                                           # Paste 'param' and '='
						ifelse( typeof( bestTunedParams[[ param ]] ) == "double", # If type of param is double...
							round( bestTunedParams[[ param ]], 5 ),               # Round it with 5 digits
								bestTunedParams[[ param ]] ) ) ) %>%              # Otherwise, do nothing, and (%>%)
									paste( collapse = "|" )                       # Paste vector with '|'

			# Organise results
			resultStatsRow <- list(
				MLmethod            = mlMethod,
				AnalyticalData      = analyticalDataType,
				Metadata            = metaDataType,
				MinRMSE             = round( min(  resultRmses      ), 5 ),
				MeanRMSE            = round( mean( resultRmses      ), 5 ),
				MaxRMSE             = round( max(  resultRmses      ), 5 ),
				MinRsquared         = round( min(  resultRsquareds  ), 5 ),
				MeanRsquared        = round( mean( resultRsquareds  ), 5 ),
				MaxRsquared         = round( max(  resultRsquareds  ), 5 ),
				MeanMAE             = round( mean( resultMaes       ), 5 ),
				MeanAccuracy        = round( mean( resultAccuracies ), 5 ),
				bestTunedParameters = bestTunedParamsStr
			)

			# Show result row
			cat( "=> DONE\n" )
			cat( paste0( "Performance statistics (", analyticalDataType, ", ", metaDataType, ", ", mlMethod, "):\n" ) )
			print( as.data.frame( resultStatsRow ) )

			# Update result statistics
			self$resultStatistics <- rbind( self$resultStatistics, resultStatsRow )

			# Sort by 'MeanRMSE' (ascending)
			self$resultStatistics <- self$resultStatistics[ order( self$resultStatistics$MeanRMSE ), ]

			# Update 'rankings.csv'
			#cat( paste0( "Adding new data to '", paste0( self$outputDir, "/", "rankings.csv" ), "' ..." ) )
			write.csv( self$resultStatistics, paste0( self$outputDir, "/", "rankings.csv" ), row.names = FALSE, quote = FALSE )
			#cat( "=> DONE\n" )

			# Save (the best performed) model as rds file
			# REVIEW : If required, will modify file name into more proper format ???
			# REVIEW : e.g., name should be all capitalised ???
			bestModelFileName <- paste0( paste( mlMethod, analyticalDataType, metaDataType, sep = "-" ), ".rds" )
			cat( paste0( "Saving best-performed model data '", bestModelFileName, "' ... " ) )
			saveRDS( bestModel, file = paste0( self$outputDir, "/", "MODELs", "/", bestModelFileName ) )
			cat( "=> DONE\n" )
		}
	)
)
# ANCHOR : MlModelBuilder ends here.

#'
#' @title Build ML models and rank these perforamces for food quality assessments based on non-invasive sensors.
#'
#' @description
#' foodguardRanker::modelBuilder builds the ML (machine learning) models over all the combinations of the non-invasive sensors
#' (hereinafter, analytical data), food quality indicators (hereinafter, metadata) and ML methods. Then it compares these
#' performances with several statistics evaluations (e.g., accuracy for classification and RMSE for regression) as rankings.
#'
#' @param configFileName A string representing a name of input config JSON file, default "ModelBuilderConfig".
#'
#' @details
#' This product was funded by the European Union project FoodGuard, grant number 101136542
#'
#' @export
#'
modelBuilder <- function( configFileName = "ModelBuilderConfig" ) {
	# Start elapsed time
	startTime <- Sys.time()

	# Activate ModelBuilderUtility class
	# REVIEW : Is util Needy ???
	# FIXME  : Most probably NO, will remove it soon.
	util <- ModelBuilderUtility$new()

	# Initialise ModelBuilderConfigParser
	configParser <- ModelBuilderConfigParser$new( configFileName )
	# Show config file summary
	configParser$showConfigSummary()
	# Check config file content
	configParser$checkConfigFileContent()

	# Initialise MlModelBuilder
	mlModelBuilder <- MlModelBuilder$new(
		configParser$experimentInfo,
		configParser$mlRankingInfo
	)
	# Check if input datasets are in valid format
	mlModelBuilder$checkDatasets()
	# Purify input datasets (e.g., remove NA values)
	mlModelBuilder$purifyDatasets()
	# Show overviews of analytical/meta datasets
	mlModelBuilder$showDatasetOverviews()

	# If input datasets look ok; proceed to the modelling
	# Create unique experimental ID
	mlModelBuilder$getExperimentIds()
	# Create analytical data IDs
	mlModelBuilder$getAnalyticalDataIds()
	# Create output dirs
	mlModelBuilder$createOutputDirs()
	# Create experiment overview JSON
	mlModelBuilder$createOverView()

	# Do PCAs to all of the analytical datasets
	mlModelBuilder$doPCAs()
	# Before modelling, get all the combinations of analytical/meta data types
	mlModelBuilder$getAnalyticalMetaTypes()
	# Modellings go over the combinations of analytical/meta data types !!!
	mlModelBuilder$doModellings()

	# End elapsed time
	endTime     <- Sys.time()
	elapsedTime <- round( difftime( endTime, startTime, units = "secs" ), 1 )

	# Show output dir
	cat( paste0( "\n\nOutput data were saved in ", mlModelBuilder$outputDir, ".\n" ) )

	# Finale!
	cat( paste0( "\n\nPROGRAM FINISHED! (elapsed time : ", elapsedTime, " sec.)\n" ) )
}
# ANCHOR : modelBuilder ends here.
