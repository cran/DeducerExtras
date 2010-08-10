# TODO: Add comment
# 
# Author: Ian
###############################################################################


########################################################################
#
#				Summarize data.frame
#
########################################################################
.makeSummaryDialog <- function(){
	summaryFunc <- new(RFunction,"summary")
	
	dataParam <- new(ParamRObject)
	dataParam$setRObjectClass("data.frame")
	dataParam$setName(.jnull())
	dataParam$setTitle("data")
	summaryFunc$add(dataParam)
	
	rfd <- new(RFunctionDialog, summaryFunc)
	rfd$setSize(250L,150L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}



########################################################################
#
#				paired test
#
########################################################################

.makePairedTestDialog <- function(){
	
	#functions for dialog
	rf <- new(RFunction,"t.test")
	rfw <- new(RFunction,"wilcox.test")
	
	
	#make parameters and add them to the functions
	pv1 <- new(ParamVariable,"x")
	pv1$setTitle("First")
	rf$add(pv1)
	rfw$add(pv1)
	
	
	pv2 <- new(ParamVariable,"y")
	pv2$setTitle("Second")
	rf$add(pv2)
	rfw$add(pv2)
	
	
	p <- new(ParamCharacter,"alternative","two.sided")
	p$setTitle("Type")
	p$setOptions(c("two.sided","less","greater"))
	p$setViewType(p$VIEW_COMBO)
	rf$add(p)
	rfw$add(p$clone())
	
	p <- new(ParamNumeric,"conf.level",.95)
	p$setLowerBound(0)
	p$setUpperBound(1)
	rf$add(p)
	rfw$add(p$clone())
	
	p <- new(ParamLogical,"paired",TRUE)
	p$setDefaultValue(FALSE)
	p$setViewType(p$VIEW_HIDDEN)
	rf$add(p)
	rfw$add(p)
	
	#make function list and add functions
	prf <- new(RFunctionList,"Paired tests");
	prf$setViewType(p$VIEW_RFUNCTION_PANEL)
	prf$addRFunction("t-test",rf,FALSE)
	prf$addRFunction("Wilcoxon signed rank",rfw,FALSE)
	prf$setRequiresVariableSelector(TRUE)
	prf$setActiveFunctions("t-test")
	#parameters common to all functions should go at the top
	globals <- .jarray(list(pv1,pv2),"org.rosuda.deducer.widgets.param.Param")
	prf$setGlobalParams(globals)
	
	
	#make dialog and display
	rfd <- new(RFunctionListDialog, prf )
	rfd$setSize(500L,520L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				Single proportion: asymptotic
#
########################################################################

.makeProportionDialog <- function(){
	oneSample <- new(RFunction,"one.sample.test")
	oneSample$setRequiresVariableSelector(TRUE)
	
	datParam <- new(ParamMultipleVariables,"variables")
	datParam$setFormat(datParam$FORMAT_VARIABLE)
	datParam$setTitle("Variables")
	oneSample$add(datParam)
	
	p <- new(ParamNumeric,"p")
	p$setValue(.5)
	p$setLowerBound(0)
	p$setUpperBound(1)
	oneSample$add(p)
	
	alt <- new(ParamCharacter,"alternative","two.sided")
	alt$setTitle("Type")
	alt$setOptions(c("two.sided","less","greater"))
	alt$setViewType(alt$VIEW_COMBO)
	oneSample$add(alt)
	
	
	func <- "func<-function(x,...) {
			y<-as.factor(na.omit(x))
			if(length(levels(y))>2) stop()
			prop.test(sum(y == rev(levels(y))[1]),n=length(y),...)
			}"
	test <- new(ParamAny,"test")
	test$setViewType(test$VIEW_HIDDEN)
	test$setValue(func)
	oneSample$add(test)
	
	conf <- new(ParamNumeric,"conf.level",.95)
	conf$setLowerBound(0)
	conf$setUpperBound(1)
	oneSample$add(conf)
	
	corr <- new(ParamLogical,"correct",FALSE)
	corr$setDefaultValue(TRUE)
	corr$setViewType(corr$VIEW_HIDDEN)
	oneSample$add(corr)
	
	rfd <- new(RFunctionDialog, oneSample)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				Single proportion: exact
#
########################################################################

.makeExactProportionDialog <- function(){
	oneSample <- new(RFunction,"one.sample.test")
	oneSample$setRequiresVariableSelector(TRUE)
	
	datParam <- new(ParamMultipleVariables,"variables")
	datParam$setFormat(datParam$FORMAT_VARIABLE)
	datParam$setTitle("Variables")
	oneSample$add(datParam)
	
	p <- new(ParamNumeric,"p")
	p$setValue(.5)
	p$setLowerBound(0)
	p$setUpperBound(1)
	oneSample$add(p)
	
	alt <- new(ParamCharacter,"alternative","two.sided")
	alt$setTitle("Type")
	alt$setOptions(c("two.sided","less","greater"))
	alt$setViewType(alt$VIEW_COMBO)
	oneSample$add(alt)
	
	
	func <- "func<-function(x,...) {
			y<-as.factor(na.omit(x))
			if(length(levels(y))>2) stop()
			binom.test(sum(y == rev(levels(y))[1]),n=length(y),...)
			}"
	test <- new(ParamAny,"test")
	test$setViewType(test$VIEW_HIDDEN)
	test$setValue(func)
	oneSample$add(test)
	
	conf <- new(ParamNumeric,"conf.level",.95)
	conf$setLowerBound(0)
	conf$setUpperBound(1)
	oneSample$add(conf)
	
	rfd <- new(RFunctionDialog, oneSample)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}



########################################################################
#
#				n proportions: asymptotic
#
########################################################################
.makeNProportionDialog <- function(){
	nSample <- new(RFunction,"prop.test")
	nSample$setRequiresVariableSelector(TRUE)
	
	tableFunc <- new(RFunction,"table")
	tableFunc$setRequiresVariableSelector(TRUE)
	nSample$add(tableFunc)
	
	var1 <- new(ParamVariable,"variable")
	var2 <- new(ParamVariable,"group")
	tableFunc$add(var2)
	tableFunc$add(var1)
	
	alt <- new(ParamCharacter,"alternative","two.sided")
	alt$setTitle("Type")
	alt$setOptions(c("two.sided","less","greater"))
	alt$setViewType(alt$VIEW_COMBO)
	nSample$add(alt)
	
	conf <- new(ParamNumeric,"conf.level",.95)
	conf$setLowerBound(0)
	conf$setUpperBound(1)
	nSample$add(conf)
	
	corr <- new(ParamLogical,"correct",FALSE)
	corr$setDefaultValue(TRUE)
	corr$setViewType(corr$VIEW_HIDDEN)
	nSample$add(corr)
	
	rfd <- new(RFunctionDialog, nSample)
	rfd$setSize(600L,400L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				k-sample Equal variance
#
########################################################################
.makeEqualVarianceDialog <- function(){
	levene <- new(RFunction,"levene.test")
	bartlett <- new(RFunction, "bartlett.test")
	
	var1 <- new(ParamVariable)
	var1$setTitle("Variable")
	var2 <- new(ParamVariable)
	var2$setTitle("Group")
	levene$add(var1)
	levene$add(var2)
	bartlett$add(var1)
	bartlett$add(var2)
	
	opt <- new(ParamCharacter,"option","mean")
	opt$setTitle("Method")
	opt$setOptions(c("mean", "median","trim.mean"))
	opt$setLabels(c("Levene (mean)", "Brown-Forsythe (median)","Trimmed mean"))
	opt$setViewType(opt$VIEW_COMBO)
	levene$add(opt)
	
	trim <- new(ParamNumeric,"trim.alpha")
	trim$setTitle("% trimmed from each tail")
	trim$setRequired(FALSE)
	trim$setLowerBound(0)
	trim$setUpperBound(.5)
	levene$add(trim)
	
	#make function list and add functions
	prf <- new(RFunctionList,"K-sample equality of variance");
	prf$setViewType(prf$VIEW_RFUNCTION_PANEL)
	prf$addRFunction("Levene's test",levene,FALSE)
	prf$addRFunction("Bartlett's test",bartlett,FALSE)
	prf$setRequiresVariableSelector(TRUE)
	prf$setActiveFunctions("Levene's test")
	#parameters common to all functions should go at the top
	globals <- .jarray(list(var1,var2),"org.rosuda.deducer.widgets.param.Param")
	prf$setGlobalParams(globals)
	
	
	#make dialog and display
	rfd <- new(RFunctionListDialog, prf )
	rfd$setSize(500L,570L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				k-means cluster
#
########################################################################
.makeKMeansDialog <- function(){
	naOmitFunc <- new(RFunction,"na.omit")
	naOmitFunc$setRequiresVariableSelector(TRUE)
	
	datParam <- new(ParamMultipleVariables,"object")
	datParam$setTitle("Variables")
	naOmitFunc$add(datParam)
	
	kmeansFunc <- new(RFunction,"kmeans")
	kmeansFunc$setTitle("Clustering")
	kmeansFunc$add(naOmitFunc)
	
	centersParam <- new(ParamNumeric,"centers")
	centersParam$setTitle("# of clusters")
	centersParam$setValue(2)
	centersParam$setLowerBound(1)
	kmeansFunc$add(centersParam)
	
	
	algParam <- new(ParamCharacter,"algorithm","Hartigan-Wong")
	algParam$setTitle("Type")
	algParam$setOptions(c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"))
	algParam $setViewType(algParam$VIEW_COMBO)
	kmeansFunc$add(algParam)
	
	centersParam <- new(ParamNumeric,"iter.max",10)
	centersParam$setTitle("maximum # of iterations")
	centersParam$setLowerBound(1)
	kmeansFunc$add(centersParam)
	
	clustFuncList <- new(RFunctionList,"k-means clustring");
	clustFuncList$setViewType(clustFuncList$VIEW_RFUNCTION_PANEL)
	clustFuncList$setRequiresVariableSelector(TRUE)
	clustFuncList$addRFunction("Cluster",kmeansFunc,TRUE,TRUE,TRUE,"kmeansResult")
	
	
	#make dialog and display
	rfd <- new(RFunctionListDialog, clustFuncList )
	rfd$setSize(550L,500L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}



########################################################################
#
#				Apply k-means
#
########################################################################

predict.kmeans <- function(object,data=NULL,...){
	if(is.null(data))
		return(object$cluster)
	centers <- object$centers
	vars <- colnames(centers)
	dat <- data[,vars,drop=FALSE]
	clusters <- rep(NA,nrow(dat))
	for(i in 1:nrow(dat)){
		obs <- dat[i,]
		dists <- apply(centers,1,function(x) dist(rbind(obs,x)))
		clust <- names(which.min(dists))
		if(length(clust)>0)
			clusters[i] <- clust
		else
			clusters[i] <- NA
		
	}
	as.numeric(clusters)
}

applyModel <- function(object,data,...) data.frame(data,predict(object,data=data,...))

.makeApplyKMeansDialog <- function(){

	assignFunc <- new(RFunction,"assign")
	assignFunc$setTitle("apply k-means")
	
	newDataParam <- new(ParamCharacter)
	newDataParam$setTitle("generated data name:")
	newDataParam$setValue("kmeansData")
	newDataParam$setName(.jnull())
	
	applyFunc <- new(RFunction,"applyModel")
	
	modelParam <- new(ParamRObject,"model")
	modelParam$setRObjectClass("kmeans")
	
	dataParam <- new(ParamRObject,"data")
	dataParam$setRObjectClass("data.frame")
	
	assignFunc$add(newDataParam)
	assignFunc$add(applyFunc)
	applyFunc$add(dataParam)
	applyFunc$add(modelParam)
	
	rfd <- new(RFunctionDialog,assignFunc)
	rfd$setSize(450L,250L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				hierarchical cluster
#
########################################################################

.makeHClustDialog <- function(){

	clustFuncList <- new(RFunctionList,"Hierarchical clustring");
	clustFuncList$setViewType(clustFuncList$VIEW_RFUNCTION_PANEL)
	clustFuncList$setRequiresVariableSelector(TRUE)
	
	distFunc <- new(RFunction,"dist")
	
	x <- new(ParamMultipleVariables,"x")
	x$setTitle("Data")
	
	method <- new(ParamCharacter,"method","euclidean")
	method$setTitle("Distance")
	method$setOptions(c("euclidean", "maximum","manhattan","canberra","binary","minkowski"))
	method$setViewType(method$VIEW_COMBO)
	
	hclustFunc <- new(RFunction,"hclust")
	
	hcMethod <- new(ParamCharacter,"method","complete")
	hcMethod$setTitle("Method")
	hcMethod$setOptions(c("ward", "single", "complete", 
					"average", "mcquitty", "median" , "centroid"))
	hcMethod$setViewType(hcMethod$VIEW_COMBO)
	
	dd <- new(ParamRFunctionResult,clustFuncList,"dist")
	dd$setName("d")
	
	plotFunc <- new(RFunction,"plot")
	
	clustResult <- new(ParamRFunctionResult,clustFuncList,"hclust")
	clustResult$setName(.jnull())
	
	cutreeFunc <- new(RFunction,"cutree")
	cutreeFunc$setTitle("Cluster membership")
	
	k <- new(ParamNumeric,"k")
	k$setTitle("# of clusters")
	k$setLowerBound(0)
	
	distFunc$add(x)
	distFunc$add(method)
	hclustFunc$add(dd)
	hclustFunc$add(hcMethod)
	plotFunc$add(clustResult)
	cutreeFunc$add(clustResult)
	cutreeFunc$add(k)
	
	clustFuncList$addRFunction("dist",distFunc,TRUE,FALSE,FALSE,"<auto>")
	clustFuncList$addRFunction("hclust",hclustFunc,TRUE,TRUE,TRUE,"<auto>")
	clustFuncList$addRFunction("Dendogram",plotFunc,FALSE,FALSE,FALSE,"<auto>")
	clustFuncList$addRFunction("Cluster groups",cutreeFunc)
	#make dialog and display
	rfd <- new(RFunctionListDialog, clustFuncList )
	rfd$setSize(470L,620L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}



########################################################################
#
#				Multi dimensional scaling
#
########################################################################


.makeMDSDialog <- function(){

	mdsFuncList <- new(RFunctionList,"Multi-dimensional scaling");
	mdsFuncList$setViewType(mdsFuncList$VIEW_RFUNCTION_PANEL)
	mdsFuncList$setRequiresVariableSelector(TRUE)
	
	distFunc <- new(RFunction,"dist")
	
	x <- new(ParamMultipleVariables,"x")
	x$setTitle("Data")
	
	method <- new(ParamCharacter,"method","euclidean")
	method$setTitle("Distance")
	method$setOptions(c("euclidean", "maximum","manhattan","canberra","binary","minkowski"))
	method$setViewType(method$VIEW_COMBO)
	
	dd <- new(ParamRFunctionResult,mdsFuncList,"Distance")
	dd$setName(.jnull())
	
	mdsFunc <- new(RFunction,"cmdscale")
	mdsFunc$setTitle("Scaling")
	
	res <- new(ParamRFunctionResult,mdsFuncList,"Scaling")
	res$setName(.jnull())
	
	k <- new(ParamNumeric,"k")
	k$setTitle("# of dimensions")
	k$setValue(2)
	k$setLowerBound(0)
	
	plotFunc <- new(RFunction,"plot")
	
	distFunc$add(x)
	distFunc$add(method)
	mdsFunc$add(dd)
	mdsFunc$add(k)
	plotFunc$add(res)
	
	mdsFuncList$addRFunction("Distance",distFunc,TRUE,FALSE,FALSE,"<auto>")
	mdsFuncList$addRFunction("Scaling",mdsFunc,TRUE,FALSE,TRUE,"<auto>")
	mdsFuncList$addRFunction("Plot first two dimensions",plotFunc,FALSE,FALSE,FALSE,"<auto>")
	#make dialog and display
	rfd <- new(RFunctionListDialog, mdsFuncList )
	rfd$setSize(520L,550L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}


########################################################################
#
#				Factor analysis
#
########################################################################



.makeFactorAnalysisDialog <- function(){
	
	.factorAnalysisCheckFunction <- function(state){
		#make sure at least two variables are selected
		if(length(state$variables)<2)
			return("Please select at least two variables")
		return("")
	}
	
	.factorAnalysisRunFunction <- function(state){
		#print(state) #a print statement is useful for debugging
		
		#make formula
		form <-paste( " ~ " , state$variables[1])
		for( var in state$variables[-1])
			form <- paste(form,"+",var)
		
		#make prcomp call
		cmd <- paste("pr.model <-prcomp(", form, ",", state$data)
		if("Center" %in%state$Transformation)
			cmd <- paste(cmd,", center=TRUE")
		if("Scale" %in%state$Transformation)
			cmd <- paste(cmd,",scale=TRUE")
		cmd <- paste(cmd,")")
		
		#always print model
		cmd <- paste (cmd,"\n","print(pr.model)")
		
		#output summary and plot if asked for
		if("Summary" %in% state$Output)
			cmd <- paste(cmd,"\n","summary(pr.model)")
		if("Scree Plot" %in% state$Output)
			cmd <- paste(cmd,"\n","screeplot(pr.model)")
		
		#execute command as if typed into console
		execute(cmd)
	}
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Factor Analysis")
	
	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)
	
	#add a list for the variables
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("variables")
	addComponent(dialog, variableList,100,900,450, 420)
	
	#options for transforming the variables
	transBoxes <- new(CheckBoxesWidget,"Transformation",c("Center","Scale"))
	addComponent(dialog, transBoxes,500,900,670, 540)
	transBoxes$setDefaultModel(c("Scale"))
	
	#output options
	outBoxes <- new(CheckBoxesWidget,"Output",c("Summary","Scree Plot"))
	addComponent(dialog, outBoxes,680,900,850, 540)
	dialog$setCheckFunction(toJava(.factorAnalysisCheckFunction))
	dialog$setRunFunction(toJava(.factorAnalysisRunFunction))
	return(dialog)
}



