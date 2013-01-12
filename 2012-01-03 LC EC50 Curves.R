#loads library required for extracting from excel files
require(gdata)
## install support for xlsx files
installXLSXsupport()
# loads library required for the 4 parameter non linear regression. 
#If you don't have it installed, type in the R console "install.packages("drc")
library(drc)


######################################
####                           #######
####                           #######
#### THINGS YOU NEED TO CHANGE #######
####                           #######
####                           #######
######################################


##### CHANGE THIS 				#####
##### IF YOU USE 				#####
##### DIFFERENT CONCENTRATIONS	#####
concVec <- c("10e-10","10e-9","10e-8","10e-7","10e-6","10e-5")

########### CHANGE THIS FILE LOCATION 	###############
########### TO WHERE YOUR EXPERIMENTAL  ###############
########### EXCEL FILES ARE. 			###############
########### for windows,     			###############
########### the "/" is "\"  			###############
## note that the perl scripts that gdata uses do not cope well
## with tilde expansion on unix machines. So use the full path.

directory <- "/Users/LC/Desktop/Andrew/data_for_testing"
pdfSaveDirectory <- "/Users/LC/Desktop/Andrew" 


######################################
####                           #######
####                           #######
####          FUNCTIONS        #######
###(setup for "actions" section)######
####                           #######
######################################

####### Converts excel sheets to csv files  #############
convertExcelToCsv <- function(dir){
	####### lifted directly from here: 
	####### http://stackoverflow.com/questions/
	####### 8188415/save-excel-spreadsheet-as-csv-with-r	

	 
	#creates vector of xls file names in directory. 
	excelFileNames <- list.files(dir, full.names=T, pattern="xls$")
	for(excelFileName in excelFileNames){
		plateSheet <- read.xls(excelFileName, sheet=2)
		csvFileName <- paste(excelFileName,"csv",sep=".")
		write.csv(plateSheet, file=csvFileName)
	
		#Use this if you want all sheets in excel file converted to csv
		#numSheets<-sheetCount(excelFile, verbose=TRUE)
		#for ( i in 1:numSheets) {
	 	#	mySheet <- read.xls(excelFile, sheet=i)
	 	#	write.csv(mySheet, file=paste(i, "csv", sep="."), row.names=FALSE)
		#}	
	}
}

#gets data out of all csv files and extracts correct area of spreadsheet
readPlatesFromCsvsInDirectory <- function(dir){
	
	csvFileList <- list.files(dir,full.names=T, pattern="csv$")
	allPlates <- list()
	for(i in seq_along(csvFileList)){
		plate <-read.csv(csvFileList[i], header=F, colClasses = "character")
		plate <- as.data.frame(plate)
		#gets only the 96 well plate area into the plate
		plate <- plate[7:14,2:13]
		#adds 96 well plate to list of all plates in directory
		#I don't know why double brackets does this
		allPlates[[i]] <-plate
	}
	return(allPlates)
}


#Finds minimum for plate and subtracts from whole plate
minimize <- function(rawPlate){
	
	#gets data frame out of list rawPlate	
	numericPlate <- as.data.frame(rawPlate[1])
	#converts each column to numeric
	numericPlate <- as.data.frame(
		sapply(numericPlate,FUN=function(col) as.numeric(col)))
	#finds minimum of entire plate
	plateMin <- min(numericPlate)
	#subtracts plateMin from each item in table
	subtracted <- as.data.frame(numericPlate-plateMin)
	return(subtracted)
}


#normalizes plates(scales each value 1-100%)
normalize <- function(exp){
	
	#takes average of column A for individual experiment
	colaavg <- mean(exp[,1])
	#normalizes experiment to column A average
	exp <- as.data.frame((exp/colaavg)*100)
	
	}


#Turns wide formatted plate into long format for regression function
reformat <- function(normalizedExp, concs){
	
	# Reshapes exp1 to be all in column format. This is required for the drm function.
	normalizedLong <- reshape(normalizedExp, varying=list(colnames(normalizedExp)), 		direction="long")
	#renames column names. Using the reshape function puts names we don't want. 
	colnames(normalizedLong) <- c("conc","abs","rep")
	#replace "1"-"6" in conc column with correct corresponding concentration value
	normalizedLong$conc <- sapply(normalizedLong$conc, 
		function(colindex) concs[colindex])
	#adds column "curve" to table (this is optional, for drm function)
	normalizedLong$curve <- 1
	#sorts table by rep (unnecessary, but you can if you want)
	#normalizedLong <- exp1Long[order(exp1Long[,3]),]
	#converts conc and abs column to numeric so they can be graphed
	normalizedLong$conc <- as.numeric(normalizedLong$conc)
	normalizedLong$abs <- as.numeric(normalizedLong$abs)

	return(normalizedLong)	
}


findLine <- function(normalizedData){
	#######THIS IS WHAT FINDS THE REGRESSION LINE############
	#graphs abs vs conc, curve 1, data is from table "normalizedData, 
	#uses function "LL.4() to find regression line)
	#documentation here: http://cran.r-project.org/web/packages/drc/drc.pdf
	#the curve property is for when you want to put more than one 
	#curve on one graph. To do this, put all the data in one dataframe with 
	#exp1long format and then change the curve column to express which curve 
	#you want each point to be on.
	
	pnl <- drm(abs~conc, curve, data = normalizedData, fct = LL.4())

	#to output all coefficients in R console, uncomment line below
	#print(pnl)
	return(pnl)
}
	

#gets "E" intercept into seperate variable
findIntercept<- function(regression){

	#######gets numeric value of coefficients in table
	#coef function puts calculated coefficients into vector
	coefs <- coef(regression)
	#assigns "E" to intercept
	intercept <- coefs[4]
	
	return(intercept)
}


plotting <- function(regression, intercept,expName,plateQuadrant,errorInfo){
	#puts together original file name + A-D depending on plate quadrant
	# plus pdf extention
	filename <- paste(expName,plateQuadrant,sep="-")
	filenameWithExtension <- paste(filename,"pdf", sep=".")
	#prints plot to pdf file
	#   if you want to test the program by printing to screen only,
	#   comment out the pdf() line below and the "dev.off()" line 
	#   at the end of the function
	pdf(paste(pdfSaveDirectory,filenameWithExtension,sep="/"))
	
	#graphs the calculated regression curve
	plot(regression,
		main=filename, 					#title of graph
		font=2,							#bold font
		font.lab=2,						#bold labels
		cex=2,							#increase size 2x
		ylab="% Activity", 				#title of y axis
		xlab="log[inhibitor] (M)", 		#title of x axis
		col="red4", 					#color of line
		lwd=3,							#line curve width 3x
		ylim=range(0,120),				#range of y axis
		xlim=range(10e-10,10e-5), 		#range of x axis
		axes=F,							#doesn't draw axes
		bty="n")						#gets rid of plot outline
		
	
	
		
	#draws custom x axis
	axis(1, 							#places x axis		
		at=as.numeric(concVec),			#where ticks go 
		labels=c(-9,-8,-7,-6,-5,-4),	#text for labels
		las=1,							#sets tick labels horizontal
		font.axis=2,					#bold labels
		cex.axis=2,						#increases size by 4x	
		lwd=3							#increases axis line width
		)
	#draws custom y axis
	axis(2, 							#places y axis
		at=c(0,20,40,60,80,100), 		#where ticks go
		labels=c(0,20,40,60,80,100), 	#tick labels
		las=1,							#sets tick labels horizontal
		font.axis=2,					#bold labels
		cex.axis=2,						#increases size by 4x
		lwd=3							#increases axis line width
		)
	#graphs EC50 point
	#points( intercept,					# x coord of point
			#50, 						# y coord of point
			#pch=18, 					# assigns triangle symbol
			#col="blue",					# assigns blue color
			#cex=2)
	
	#rounds intercept to 4 digits
	roundIntercept <- format(intercept, digits=4)
	
	#displays text at coordinates (intercept, 50)
	#the complicated code is to get the subscript "50"
	#text(intercept, 50,					#coordinates of text 
	#	substitute(EC[50]~"="~inter, 	#lays out which text to display
	#	list(inter=roundIntercept)), 	#plugs in number to intercept
	#	adj=c(1.1,0.5), 				#moves text a little down and to the left
	#	cex=0.8)						#makes size of text 20% smaller
	
	#sets up legend
	#legend(0.000003, 101,				#x and y coords 
	#	substitute(EC[50]~"="~inter,	#displays text and value
	#	list(inter=roundIntercept)),
	#	pch=18,							#displays triangle
	#	col="blue")						#makes triangle blue
		
	#displays subtitle
	#must go after where roundIntercept is assigned.
	mtext(substitute(EC[50]~"="~inter,	
			list(inter=roundIntercept)))
	
		
		

	#### ERROR BARS ####
	#converts error info into useable data
	concVec<- as.numeric(concVec)
	avgs <- errorInfo[,1]
	stdevs <- errorInfo[,2]
	
	#cycles through each point on plot through vector avgs
	for(i in 1:length(avgs)){
		#prints error bar above point
		arrows( concVec[i], 			# start x coord
				avgs[i], 				# start y coord
				concVec[i], 			# end x coord
				avgs[i]+stdevs[i], 		# end y coord
				lwd=3,
				angle=90, 				# angle of end cap
				length=0.03)			# length of end cap
		#prints error bar below point
		arrows( concVec[i], 			# start x coord
				avgs[i], 				# start y coord
				concVec[i], 			# end x coord
				avgs[i]-stdevs[i], 		# end y coord
				lwd=3,
				angle=90, 				# angle of end cap
				length=0.03)			# length of end cap
	}
	
	#turns off graphics device
	dev.off()
}

#makes dataframe for error bars
findAvgsAndStdev<- function(exp){
	avgsAndStdevs <- data.frame(avgs=sapply(exp,mean), stdevs=sapply(exp,sd))
	return(avgsAndStdevs)
}


#combines all functions
doAll <- function(exp,expName,plateQuadrant,i){
	normalizedExp <- normalize(exp)
	expLong <- reformat(normalizedExp,concVec)
	line <- findLine(expLong)
	intercept<-findIntercept(line)
	errorInfo<- findAvgsAndStdev(normalizedExp)
	graph <- plotting(line,intercept,expName,plateQuadrant,errorInfo)
}



######################################
####                           #######
####           ACTIONS         #######
####                           #######
####       where the things    #######
####        actually happen    #######
####                           #######
######################################

#converts excel files to csv files
convertExcelToCsv(directory)

#puts correct table from all csv files in directory 
#into one vector called allPlates
allPlates <- readPlatesFromCsvsInDirectory(directory)



#cycles through each item (one per csv file) in vector allPlates 
#for each item the table is split into four, regression analysis
#is done and a graph with regression line and EC50 is saved
for (i in seq_along(allPlates)){
	
	
	#Retreives original file names for generating new file names
	csvFileList <- list.files(directory,full.names=F, pattern="csv$")
	#assigns current file name
	currentPlateName<- csvFileList[i]
	#takes ".csv" off of ending
	currentPlateName<- sub(".xls.csv","",currentPlateName)
	
	currentPlate <- allPlates[i]
	
	
	#finds minimum of whole plate and subtracts from each cell
	minimizedPlate <- minimize(currentPlate)
	 
	

	#Divide whole plate into seperate experiments
	exp1 <- minimizedPlate[1:4,1:6]
	exp2 <- minimizedPlate[5:8,1:6]
	exp3 <- minimizedPlate[1:4,7:12]
	exp4 <- minimizedPlate[5:8,7:12]

	#sets up display page so 4 histograms show in a 2x2 grid
	#par(mfrow=c(2,2))
	
	doAll(exp1,currentPlateName,"A")
	doAll(exp2,currentPlateName,"B")
	doAll(exp3,currentPlateName,"C")
	doAll(exp4,currentPlateName,"D")
	
}
