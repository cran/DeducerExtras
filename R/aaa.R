# TODO: Add comment
# 
# Author: Ian
###############################################################################



RFunction <- NULL
RFunctionDialog <- NULL

Param<- NULL
ParamAny <- NULL
ParamVariable <- NULL
ParamMultipleVariables <- NULL
ParamLogical <- NULL
ParamCharacter<- NULL
ParamNumeric<-NULL
ParamRObject<- NULL
ParamRFunctionResult<- NULL
RFunctionList<- NULL
RFunctionListDialog <- NULL
RFunctionDialog <- NULL

.registerDeducerExtraDialog <- function(name,generator){
	if(!exists(".deducerExtrasDialogGenerators"))
		.deducerExtrasDialogGenerators <<- list()
	.deducerExtrasDialogGenerators[[name]] <<- generator
	
}

.getDeducerExtrasDialog <- function(name,newInstance=FALSE){
	if(!exists(".deducerExtrasDialogs"))
		.deducerExtrasDialogs <<- list()
	dialog <- .deducerExtrasDialogs[[name]]
	if(is.null(dialog) || newInstance){
		dialog <- .deducerExtrasDialogGenerators[[name]]()
		if(!newInstance)
			.deducerExtrasDialogs[[name]] <<- dialog
	}
	dialog
}
