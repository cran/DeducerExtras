# TODO: Add comment
# 
# Author: Ian
###############################################################################


#Load imports
RFunction <- J("org.rosuda.deducer.widgets.param.RFunction")
RFunctionDialog <- J("org.rosuda.deducer.widgets.param.RFunctionDialog")

Param<- J("org.rosuda.deducer.widgets.param.Param")
ParamAny <- J("org.rosuda.deducer.widgets.param.ParamAny")
ParamVariable <- J("org.rosuda.deducer.widgets.param.ParamVariable")
ParamMultipleVariables <- J("org.rosuda.deducer.widgets.param.ParamMultipleVariables")
ParamLogical <- J("org.rosuda.deducer.widgets.param.ParamLogical")
ParamCharacter<- J("org.rosuda.deducer.widgets.param.ParamCharacter")
ParamNumeric<- J("org.rosuda.deducer.widgets.param.ParamNumeric")
ParamRObject<- J("org.rosuda.deducer.widgets.param.ParamRObject")
ParamRFunctionResult<- J("org.rosuda.deducer.widgets.param.ParamRFunctionResult")
RFunctionList<- J("org.rosuda.deducer.widgets.param.RFunctionList")
RFunctionListDialog <- J("org.rosuda.deducer.widgets.param.RFunctionListDialog")
RFunctionDialog <- J("org.rosuda.deducer.widgets.param.RFunctionDialog")

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
