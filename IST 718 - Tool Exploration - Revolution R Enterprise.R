## Tool Exploration

## Revolution R Enterprise

## Installing the package
install.packages("RevoScaleR")

## Loading the package
library(RevoScaleR)

## Current directory
getwd()

## Setting dataDir as current directory
dataDir <- getwd()

## Loading the file
AirlineCsv <- file.path(dataDir, "2007.csv")

## Converting the file format to xdf for fast processing
AirlineXdf <- file.path(dataDir, "2007.xdf")

## Having a look at the time required for processing
system.time(rxImport(inData = AirlineCsv, outFile = AirlineXdf, overwrite = TRUE))

## Turning off the progress report
rxOptions(reportProgress = 0)

## Exploring some variables
rxGetInfo(data = AirlineXdf, getVarInfo = TRUE, varsToKeep = c("ArrDelay", "DepDelay", "Distance", "DayOfWeek"))

## Summarizing some variables to compute descriptive statistics
rxSummary(formula = ~DepDelay + Distance, data = AirlineXdf)

## Visualizing the variable DepDelay
rxHistogram(~DepDelay, data = AirlineXdf)

## Zooming into this visualization
rxHistogram(~DepDelay, data = AirlineXdf, xAxisMinMax = c(-50, 250), numBreaks = 500, xNumTicks = 10)

## Creating a new Xdf file
newAirlineXdf <- file.path(dataDir, "2007_airSpeed.xdf")

## Creating a new variable airSpeed
system.time(rxDataStep(inData = AirlineXdf, outFile = newAirlineXdf,
                       varsToKeep = c("AirTime", "Distance", "DepDelay", "ArrDelay"),
                       transforms = list(airSpeed = Distance/AirTime),
                       overwrite = TRUE))

## Exploring the new Xdf file
rxGetInfo(data = newAirlineXdf, getVarInfo = TRUE)

## Summarising the new variable
rxSummary(formula = ~airSpeed, data = newAirlineXdf)

## Generating a histogram of this new variable
rxHistogram(~airSpeed, data = newAirlineXdf)

## Scale Airspeed to mph
rxDataStep(inData = newAirlineXdf, outFile = newAirlineXdf,
           varsToKeep = c("airSpeed"),
           transformVars = c("airSpeed"),
           transforms = list(airSpeed = airSpeed * 60),
           overwrite = TRUE)

## Visualizing airSpeed again
rxHistogram(~airSpeed, data = newAirlineXdf)

## Visualizing airSpeed in detail
rxHistogram(~airSpeed, data = newAirlineXdf,
            rowSelection = (airSpeed > 60) & (airSpeed < 750),
            scales = list(x = list(at = seq(100, 700, by = 100))),
            xlab = list(label = "Air Speed (mph)"),
            ylab = list(label = "Frequency"),
            numBreaks = 6000, xNumTicks = 40)

## Checking the relationship between airSpeed and DepDelay
rxCor(formula = ~DepDelay + ArrDelay + airSpeed, data = newAirlineXdf)

## Excluding the outliers and checking the relationship again
rxCor(formula = ~DepDelay + ArrDelay + airSpeed, data = newAirlineXdf,
      rowSelection = (airSpeed > 50) & (airSpeed < 800))

## Linear Model
system.time(reg1 <- rxLinMod(formula = airSpeed ~ DepDelay, data = newAirlineXdf,
                             rowSelection = (airSpeed > 50) & (airSpeed < 800)))

## Looking at the class of the model
class(reg1)

## Inspecting the elements of reg1
names(reg1)

## Viewing the summary of the linear model
print(summary(reg1), header = FALSE)

## Is there a non-linear relationship?

## Discretizing departure delay
rxDataStep(inData = newAirlineXdf, outFile = newAirlineXdf,
           varsToKeep = "DepDelay", transformVars = "DepDelay",
           transforms = list(F_DepDelay = cut(DepDelay, breaks = seq(from = -10, to = 100, by = 10))),
           append = "cols", overwrite = TRUE)

## Summarizing this categorical variable
print(rxSummary(~F_DepDelay, data = newAirlineXdf), header = FALSE)

## Creating a new model
reg2 <- rxLinMod(formula = airSpeed ~ F_DepDelay, data = newAirlineXdf,
                 rowSelection = (airSpeed > 50) & (airSpeed < 800), dropFirst = TRUE)

## Viewing the summary of this model
print(summary(reg2), header = FALSE)