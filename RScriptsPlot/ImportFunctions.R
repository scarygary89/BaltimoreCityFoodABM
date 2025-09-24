library(dplyr)
library(data.table)
library(stringr)

# Import output data from a parametric sensitivity analysis
# filePath: directory containing output files
# sensParmsList: string or vector of parameter names to match
# combo: if TRUE, match files with any parameter; if FALSE, exclude files with underscores
DetectAndImportSensitivity <- function(filePath, sensParmsList, combo = FALSE){
    outputfiles <- dir(filePath) # List files in directory
    if (combo == TRUE){
        importFiles <- outputfiles[str_detect(outputfiles, sensParmsList)]
    } else {
        importFiles <- outputfiles[str_detect(outputfiles, sensParmsList) & !str_detect(outputfiles, c("_"))]
    }
    sensitivityList <- list()
    for(i in importFiles){
        print(i) # Print file name for tracking
        sensitivityList[[i]] <- ImportData(paste0(filePath,'/',i)) # Import data
    }
    return(sensitivityList)
}

# Import model output data from scenario runs
# OutDirPath: directory containing output files
# Scenario: string to match scenario files
ImportDataForScenarios <- function(OutDirPath,Scenario){
    dirfiles <- dir(OutDirPath) # List files in directory
    scendirfiles <- dirfiles[str_detect(dirfiles, Scenario)] # Filter by scenario
    scenDataList <- list()
    for (fn in scendirfiles){
        print(fn) # Print file name for tracking
        pathf <- paste0(OutDirPath,'/',fn)
        scenDataList[[fn]] <- ImportData(pathf) # Import data
    }
    return(scenDataList)
}

# Import data from a directory of CSV files and organize by Race, Income, Poverty
# filePath: directory containing CSV files
ImportData <- function(filePath) {
    filesList <- dir(filePath) # List files in directory
    alldata <- lapply(filesList, function(x) fread(paste0(filePath,'/',x))) # Read all files
    names(alldata) <- filesList # Name list by file names

    # Extract race data
    racedata <- alldata[grepl("Race",names(alldata))]
    races <- sub(".csv", "",sub(".*Race_", "", names(racedata)))
    racezips <- substring(sub(".*FoodConsumption", "", names(racedata)),1,5)
    Racemat <- do.call(rbind, lapply(1:length(racedata), function(x) {
        df <- racedata[[x]]
        df <- cbind(df, race = races[x], zipcode = racezips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))

    # Extract income data
    incomedata <- alldata[grepl("Income",names(alldata))]
    incomes <- sub(".csv", "",sub(".*Income_", "", names(incomedata)))
    incomezips <- substring(sub(".*FoodConsumption", "", names(incomedata)),1,5)
    Incomemat <- do.call(rbind, lapply(1:length(incomedata), function(x) {
        df <- incomedata[[x]]
        df <- cbind(df, income = incomes[x], zipcode = incomezips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))

    # Extract poverty data
    povertydata <- alldata[grepl("Poverty",names(alldata))]
    povertys <- sub(".csv", "",sub(".*Poverty_", "", names(povertydata)))
    povertyzips <- substring(sub(".*FoodConsumption", "", names(povertydata)),1,5)
    Povertymat <- do.call(rbind, lapply(1:length(povertydata), function(x) {
        df <- povertydata[[x]]
        df <- cbind(df, poverty = povertys[x], zipcode = povertyzips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))
    # Return list of data frames for each category
    return(list(Race = Racemat, Income = Incomemat, Poverty = Povertymat))
}

# Create a key for sensitivity analysis file names and parameter values
# term1, term2: parameter names (term2 can be "none")
createSensitivityFileKey <- function(term1, term2) {
    Ranges1 = c(0.2,0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2)
    if (term2 != "none"){
        Ranges2 = c(0.2,0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2)
    } else {
        Ranges2 = c()
    }
    counti <- 0
    if (term2 != "none") {
        # Create combinations for two parameters
        for (i in Ranges1) {
            for (j in Ranges2){
                tl <- paste0(term1, "Sensitivity", i, "_", term2, "Sensitivity", j)
                if (counti > 0){
                    tempList <- rbind(tempList, cbind(file = tl, term1 = i, term2 = j))
                } else {
                    tempList <- cbind(file = tl, term1 = i, term2 = j)
                }
                counti <- counti + 1
            }
        }
    } else {
        # Create combinations for one parameter
        for (i in Ranges1) {
            tl <- paste0(term1, "Sensitivity", i)
            if (counti > 0){
                tempList <- rbind(tempList, cbind(file = tl, term1 = i))
            } else {
                tempList <- cbind(file = tl, term1 = i)
            }
            counti <- counti + 1
        }
    }
    return(tempList)
}

# Combine sensitivity analysis data into a long-format data frame
# term1, term2: parameter names
# dataList: list of imported data
# series: which series to extract from each data set
createLongDataSensitivtyAnalysis <- function(term1, term2, dataList, series){
    dataKey <- createSensitivityFileKey(term1, term2) # Get file key
    listNames <- dataKey[,'file']
    if (term2 != "none") {
        # Combine data for two parameters
        datalist <- lapply(listNames, function(x) {
            df <- dataList[[x]][[series]]
            df <- df %>% mutate(term1 = dataKey[listNames == x,"term1"], term2 = dataKey[listNames == x,"term2"])
            names(df)[names(df) == 'term1'] <- term1
            names(df)[names(df) == 'term2'] <- term2
            return(df)
        })
    } else {
        # Combine data for one parameter
        datalist <- lapply(listNames, function(x) {
            df <- dataList[[x]][[series]]
            df <- df %>% mutate(term1 = dataKey[listNames == x,"term1"])
            names(df)[names(df) == 'term1'] <- term1
            return(df)
        })
    }
    RunsCombo <- do.call(rbind, datalist) # Combine all data frames
    return(RunsCombo)
}

# Combine scenario data into a long-format data frame
# inputData: list of imported scenario data
# series: which series to extract from each data set
createLongData <- function(inputData, series){
    datalist <- lapply(names(inputData), function(x) {
        df <- inputData[[x]][[series]]
        df <- df %>% mutate(Scenario = x)
    })
    RunsCombo <- do.call(rbind, datalist) # Combine all data frames
    return(RunsCombo)
}
