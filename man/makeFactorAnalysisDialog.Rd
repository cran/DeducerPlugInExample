\name{makeFactorAnalysisDialog}
\alias{makeFactorAnalysisDialog}
\alias{makeListExampleDialog}
\alias{.factorAnalysisCheckFunction}
\alias{.factorAnalysisRunFunction}
\alias{makeScatterPlotDialog}
\title{
  makeFactorAnalysisDialog
}
\description{
  makes a Java Swing factor analysis dialog
}
\usage{
makeFactorAnalysisDialog()
makeListExampleDialog()
makeScatterPlotDialog()
.factorAnalysisRunFunction(state)
.factorAnalysisCheckFunction(state)
}

\arguments{
     \item{state}{ a list representing the current state of all of the widgets in the dialog}
 }
 
 \details{
 The makeFactorAnalysisDialog returns a Java object of class SimpleRDialog. 
 
 .factorAnalysisCheckFunction is called when the "Run" button is pressed. If the dialog passes the
 check, then .factorAnalysisRunFunction is called, which formats and executes the users selections.
 
 see http://www.deducer.org for details
 
 }