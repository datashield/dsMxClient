# ------------------------------------------------------------------------------
# Program: twinSatDS.R  
#  Author: Julia Isaeva/Hermine Maes/
#    Date: 24 04 2013 
#  Adapted to Horizontal-DataSHIELD syntax by Amadou Gaye
#    Date: 21 09 2014
# Univariate Twin Saturated model to estimate means and 
#(co)variances across multiple groups
# Matrix style model - Raw data - Continuous data
# Data stored in Opal
# -------|---------|---------|---------|---------|---------|---------|---------|

# load required packages
library('dsMxClient')

# load the logindata
data(logindata)

# login and assign data from opal datasource to R
opals <- datashield.login(logins=logindata,assign=TRUE)

# ------------------------------------------------------------------------------
# PREPARE DATA

# Check Data
ds.length("D$Birthyear")

# Select Variables for Analysis
selVars <- c("log_bmi1", "log_bmi2") # variable names
ntv <- 2                             # total number of variables

# Select Data for Analysis
ds.subset(x='D', subset='mz1', logicalOperator='zyg<', threshold=3)
ds.subset(x='mz1', subset='mzData', cols=c('log_bmi1','log_bmi2'))
ds.subset(x='D', subset='dz1', logicalOperator='zyg!=', threshold=3)
ds.subset(x='dz1', subset='dz2', logicalOperator='zyg!=', threshold=5)
ds.subset(x='dz2', subset='dz3', logicalOperator='zyg>', threshold=1)
ds.subset(x='dz3', subset='dzData', cols=c('log_bmi1','log_bmi2'))

# ------------------------------------------------------------------------------
# GRAPH TWIN CORRELATIONS
# graphing heatmap plots of twin 1 by twin 2
ds.heatmapPlot("mzData$log_bmi1", "mzData$log_bmi2")
ds.heatmapPlot("dzData$log_bmi1", "dzData$log_bmi2")

# Calculating Twin Correlations by Zygosity
ds.cor("mzData$log_bmi1", "mzData$log_bmi2")
ds.cor("dzData$log_bmi1", "dzData$log_bmi2")

# Calculating BMI Mean & Variance for each Individual (twin 1 & 
# twin 2) in MZ and DZ pairs
ds.mean("mzData$log_bmi1")
ds.mean("mzData$log_bmi2")
ds.mean("dzData$log_bmi1")
ds.mean("dzData$log_bmi2")

ds.var("mzData$log_bmi1")
ds.var("mzData$log_bmi2")
ds.var("dzData$log_bmi1")
ds.var("dzData$log_bmi2")

ds.cor("mzData")
ds.cor("dzData")

# PREPARE MODEL

# Saturated Model
# Set Starting Values
meanVal <- 20                        # start value for means
varVal <- 0.8                        # start value for variance
lBoundVal <- 0.0001                  # start value for lower bounds
varValD <- c(varVal, ntv, ntv)       # start values for diagonal of covariance matrix
varLBoundD <- c(lBoundVal, ntv, ntv) # lower bounds for diagonal of covariance matrix

# Algebra for expected Mean Matrices in MZ & DZ twins
ds.mxMatrix(type='Full', nrow=1, ncol=ntv, free=T, values=meanVal, labels=c("mMZ1","mMZ2"), name='expMeanMZ', newobj="meanMZ")
ds.mxMatrix(type='Full', nrow=1, ncol=ntv, free=T, values=meanVal, labels=c("mDZ1","mDZ2"), name='expMeanDZ', newobj="meanDZ")

# Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
#ds.mxMatrix(type='Symm', nrow=ntv, ncol=ntv, free=T, values=varValD, labels=c("vMZ1","cMZ21","vMZ2"), lbound=varLBoundD, name='expCovMZ', newobj="covMZ")
#ds.mxMatrix(type='Symm', nrow=ntv, ncol=ntv, free=T, values=varValD, labels=c("vDZ1","cDZ21","vDZ2"), lbound=varLBoundD, name='expCovDZ', newobj="covDZ")
datashield.assign(opals, 'ntv', quote(c(2)))
datashield.assign(opals, 'meanVal', quote(c(20)))
datashield.assign(opals, 'varVal', quote(c(0.8)))
datashield.assign(opals, 'lBoundVal', quote(c(0.0001)))
datashield.assign(opals, 'varValD', quote(diag(varVal, ntv, ntv)))
datashield.assign(opals, 'varLBoundD', quote(diag(lBoundVal, ntv, ntv)))
datashield.assign(opals, 'covMZ', quote(mxMatrix(type='Symm', nrow=ntv, ncol=ntv, free=T, values=varValD, labels=c("vMZ1","cMZ21","vMZ2"), lbound=varLBoundD, ubound=NA, byrow=F, dimnames=NA, name='expCovMZ')))
datashield.assign(opals, 'covDZ', quote(mxMatrix(type='Symm', nrow=ntv, ncol=ntv, free=T, values=varValD, labels=c("vDZ1","cDZ21","vDZ2"), lbound=varLBoundD, ubound=NA, byrow=F, dimnames=NA, name='expCovDZ')))

# Data objects for Multiple Groups
ds.mxData(observed="mzData", type='raw', newobj='dataMZ')
ds.mxData(observed="dzData", type='raw', newobj='dataDZ')

# Objective objects for Multiple Groups
ds.mxFIMLObjective(covariance="expCovMZ", means="expMeanMZ", dimnames=selVars, newobj="objMZ")
ds.mxFIMLObjective(covariance="expCovDZ", means="expMeanDZ", dimnames=selVars, newobj="objDZ")

# Combine Groups
ds.mxModel(model='MZ', list('meanMZ', 'covMZ', 'dataMZ', 'objMZ'), newobj="modelMZ")
ds.mxModel(model='DZ', list('meanDZ', 'covDZ', 'dataDZ', 'objDZ'), newobj="modelDZ")

# specify a specific model and an element of the model to be used
ds.mxAlgebra('MZ.objective+DZ.objective', name='minus2sumloglikelihood', newobj='minus2ll')

## TRANSLATING THE CODE ## The object "obj" is produced by using 
# the mxAlgebraObjective() function and contains the calculated 
# maximium likelihood value ("minus2sumloglikelihood"). Objective 
# functions are functions for which free parameter values are 
# chosen such that the value of the objective function is minimized.
ds.mxAlgebraObjective(algebra='minus2sumloglikelihood', newobj='obj')

## TRANSLATING THE CODE ## The object "twinSatModel" is the object 
# that contains all the matrices necessary for the complete model.  
# It uses the mxModel() function to name the model ("twinSat") 
# that contains all the objects that were used to build the model 
# (modelMZ,modelDZ,minus2ll,obj,ciCov,ciMean).  NOTE- This is the 
# equivalent of compiling the model.  When you run up until the 
# point, some basic problems in your building will be identififed.  
# However, you have not actually run the model at this point.  
# Additionally, you will see an object "twinSatModel" in your 
# directory.  You can use the information in this object to take 
# a look at the basics of your model (ie: did you fix parameters 
# appropriately), but there will be no estimated parameters at 
#this point.  You must run the model to get parameter estimates.
ds.mxModel(model='twinSat', list('modelMZ', 'modelDZ', 'minus2ll', 'obj'), newobj='twinSatModel')

# ------------------------------------------------------------------------------
# RUN MODEL

# Run Saturated Model
## TRANSLATING THE CODE ## The object "twinSatFit" is produced by 
# using the mxRun() function.  This function actually takes the 
# object that was produced by building your model and runs the 
# model to estimate parameters, get confidence intervals if requested 
# (intervals=FALSE).  NOTE- This function works by using an Mx OBJECT 
# (twinSatModel), not a previously defined matrix. 
ds.mxRun(model='twinSatModel', newobj='twinSatFit')

## TRANSLATING THE CODE ## The object "twinSatSumm" is generated 
# by the summary() function.  This function summarizes the key 
# information from the model including the data summary, a list 
# of the free parameters, various goodness-of-fit statistics, 
# as well as details about processing time.
ds.mxSummary('twinSatFit')



