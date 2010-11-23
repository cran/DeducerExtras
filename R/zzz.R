




.First.lib <- function(libname, pkgname){
	if(.deducer == .jnull())
		return(NULL)
	
	RFunction <<- J("org.rosuda.deducer.widgets.param.RFunction")
	RFunctionDialog <<- J("org.rosuda.deducer.widgets.param.RFunctionDialog")
	
	Param<<- J("org.rosuda.deducer.widgets.param.Param")
	ParamAny <<- J("org.rosuda.deducer.widgets.param.ParamAny")
	ParamVariable <<- J("org.rosuda.deducer.widgets.param.ParamVariable")
	ParamMultipleVariables <<- J("org.rosuda.deducer.widgets.param.ParamMultipleVariables")
	ParamLogical <<- J("org.rosuda.deducer.widgets.param.ParamLogical")
	ParamCharacter<<- J("org.rosuda.deducer.widgets.param.ParamCharacter")
	ParamNumeric<<- J("org.rosuda.deducer.widgets.param.ParamNumeric")
	ParamRObject<<- J("org.rosuda.deducer.widgets.param.ParamRObject")
	ParamRFunctionResult<<- J("org.rosuda.deducer.widgets.param.ParamRFunctionResult")
	RFunctionList<<- J("org.rosuda.deducer.widgets.param.RFunctionList")
	RFunctionListDialog <<- J("org.rosuda.deducer.widgets.param.RFunctionListDialog")
	RFunctionDialog <<- J("org.rosuda.deducer.widgets.param.RFunctionDialog")
	
	
	.registerDeducerExtraDialog("Distribution quantiles",
			function() .makeDistributionDialog("quantile"))
	
	.registerDeducerExtraDialog("Distribution function values",
			function() .makeDistributionDialog("distribution"))
	
	.registerDeducerExtraDialog("Cumulative distribution function",
			function() .makeDistributionDialog("CDF"))
	
	.registerDeducerExtraDialog("Data summary",.makeSummaryDialog)
	
	.registerDeducerExtraDialog("Paired test",.makePairedTestDialog)
	
	.registerDeducerExtraDialog("Single proportion",.makeProportionDialog)
	
	.registerDeducerExtraDialog("Single proportion: Exact",.makeExactProportionDialog)
	
	.registerDeducerExtraDialog("k-sample proportion",.makeNProportionDialog)
	
	.registerDeducerExtraDialog("k-sample variance test",.makeEqualVarianceDialog)
	
	.registerDeducerExtraDialog("t-test power",.makeTTestPowerDialog)
	
	.registerDeducerExtraDialog("k-means cluster",.makeKMeansDialog)
	
	.registerDeducerExtraDialog("Apply k-means to data",.makeApplyKMeansDialog)
	
	.registerDeducerExtraDialog("Hierarchical cluster",.makeHClustDialog)
	
	.registerDeducerExtraDialog("Multi-dimensional scaling",.makeMDSDialog)
	
	
	gui.addSeperator <- function(){}
	if(.windowsGUI){
		winMenuAdd("Extras")
		gui.addMenuItem <- winMenuAddItem
	}else if(.jgr){
		DeducerMain$insertMenu(J("org.rosuda.JGR.JGR")$MAINRCONSOLE, "Extras",6L)
		#jgr.addMenu("Extras")
		gui.addMenuItem <- jgr.addMenuItem
		gui.addSeperator <- function () jgr.addMenuSeparator("Extras")
	}else
		gui.addMenuItem <- function(x,y,z){}
	
	deducer.addMenu("Extras")
	
	'%+%' <- function(x,y) paste(x,y,sep="")
	
	addMenuItem <- function(name){
		deducer.addMenuItem(name,,
				".getDeducerExtrasDialog('" %+% name %+% "')$run()","Extras")
		gui.addMenuItem("Extras",name,"deducer('"%+% name %+% "')")
	}
	
	addMenuItem("Distribution quantiles")
	addMenuItem("Distribution function values")
	addMenuItem("Cumulative distribution function")
	gui.addSeperator()
	deducer.addMenuItem("Load Data From Package",,
			".makePackageDataDialog()$run()","Extras")
	gui.addMenuItem("Extras","Load data from package","deducer('"%+% "Load Data From Package" %+% "')")
	addMenuItem("Data summary")
	gui.addSeperator()

	addMenuItem("Single proportion")
	addMenuItem("Single proportion: Exact")
	addMenuItem("k-sample proportion")
	gui.addSeperator()
	addMenuItem("Paired test")	
	addMenuItem("k-sample variance test")
	gui.addSeperator()
	addMenuItem("t-test power")
	gui.addSeperator()	
	addMenuItem("k-means cluster")
	addMenuItem("Apply k-means to data")
	addMenuItem("Hierarchical cluster")
	gui.addSeperator()
	addMenuItem("Multi-dimensional scaling")
}




