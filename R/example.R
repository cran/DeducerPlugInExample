#
# Author: Ian Fellows
###############################################################################

makeFactorAnalysisDialog <- function(){
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
	dialog$setCheckFunction(".factorAnalysisCheckFunction")
	dialog$setRunFunction(".factorAnalysisRunFunction")
	return(dialog)
}

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

getFactorAnalysisDialog <- function(){
	if(!exists(".factorAnalysisDialog")){
		ex <- as.environment(match("package:DeducerPlugInExample", search()))
		#make factor analysis dialog
		.factorAnalysisDialog <- makeFactorAnalysisDialog()
		assign(".factorAnalysisDialog",.factorAnalysisDialog,ex)		
	}
	return(.factorAnalysisDialog)
}

getScatterPlotDialog <- function(){
	if(!exists(".scatterPlotDialog")){
		ex <- as.environment(match("package:DeducerPlugInExample", search()))
		#make scatter plot dialog
		PlotRDialog <- J("example.PlotRDialog")
		.scatterPlotDialog <- new(PlotRDialog)
		assign(".scatterPlotDialog",.scatterPlotDialog,ex)
	}
	return(.scatterPlotDialog)
}

.First.lib <- function(libname, pkgname) { 
	#loads example.jar
	.jpackage(pkgname,lib.loc=libname)
	
	#add menu items
	deducer.addMenu("Example")
	deducer.addMenuItem("Factor Analysis",,"getFactorAnalysisDialog()$run()","Example")
	deducer.addMenuItem("Scatter Plot",,"getScatterPlotDialog()$run()","Example")
	deducer.addMenuItem("Widget display",,"J('example.ExampleDialog')$run()","Example")
	if(.windowsGUI){
		winMenuAdd("Example")
		winMenuAddItem("Example", "Factor Analysis", "deducer('Factor Analysis')")
		winMenuAddItem("Example", "Scatter Plot", "deducer('Scatter Plot')")
		winMenuAddItem("Example", "Widget Display", "deducer('Widget display')")
	}else if(.jgr){
		jgr.addMenu("Example")
		jgr.addMenuItem("Example", "Factor Analysis", "deducer('Factor Analysis')")
		jgr.addMenuItem("Example", "Scatter Plot", "deducer('Scatter Plot')")
		jgr.addMenuItem("Example", "Widget Display", "deducer('Widget display')")
	}
}