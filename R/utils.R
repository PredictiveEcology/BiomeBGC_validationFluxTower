mergeData <- function(towerData, BiomeBGCData, timescale, outputVar, NEEpartitioningMethod = "DT", ustarThresMethod = "VUT", centralValue = "REF", confInt = NA, NAvalues = c(-9999, 0)){
  # Define which columns of towerData we need
  colToKeep <- determineColumns(outputVar, NEEpartitioningMethod, ustarThresMethod, centralValue, confInt)
  
  # Remove unsuitable entries,
  QC_column <- paste("NEE", ustarThresMethod, centralValue, "QC", sep = "_")
  
  # Sometimes variable ustar threshold (VUT) method is not available, if so, use the CUT
  if(QC_column %notin% colnames(towerData)){
    ustarThresMethod <- ifelse(ustarThresMethod == "VUT", "CUT", "VUT")
    colToKeep <- determineColumns(outputVar, NEEpartitioningMethod, ustarThresMethod, centralValue, confInt)
    QC_column <- paste("NEE", ustarThresMethod, centralValue, "QC", sep = "_")
  }
  
  towerData <- towerData[, c(colToKeep, QC_column)]
  
  ## remove data that was gap-filled at more than 50%
  highGapFilled <- towerData[,QC_column] < 0.5
  towerData[highGapFilled, colToKeep[-1]] <- -9999
  
  ## NA, usually -9999
  towerData[towerData <= -9990] <- NA
  
  if (timescale == "day"){
    
    out <- mergeDailyData(towerData, BiomeBGCData, outputVar, colToKeep)
    
  } else if (timescale == "month") {
    
    out <- mergeMonthlyData(towerData, BiomeBGCData, outputVar, colToKeep)
    
  } else {
    
    out <- mergeAnnualData(towerData, BiomeBGCData, outputVar, colToKeep, confInt)
    
  }
  
  return(out)
  
}

determineColumns <- function(outputVar, NEEpartitioningMethod, ustarThresMethod, centralValue, confInt){
  # Central value
  if (outputVar == "NEE") {
    estimateCol <- paste("NEE", ustarThresMethod, sep = "_")
  } else {
    estimateCol <- paste(outputVar, NEEpartitioningMethod, ustarThresMethod, sep = "_")
  }
  centralValueCol <- paste(estimateCol, centralValue, sep = "_")
  colToKeep <- c("TIMESTAMP", centralValueCol)
  
  # Add confidence interval columns if needed
  if (!is.na(confInt)){
    lowerBoundCol <- paste(estimateCol, formatC(100-confInt, width = 2, flag = 0), sep = "_")
    upperBoundCol <- paste(estimateCol, formatC(confInt, width = 2, flag = 0), sep = "_")
    colToKeep <- c(colToKeep, lowerBoundCol, upperBoundCol)
  }
  
  return(colToKeep)
}

mergeDailyData <- function(towerData, BiomeBGCData, outputVar, colToKeep){
  # format dates
  dates <- as.Date(as.character(towerData[,"TIMESTAMP"]), format = "%Y%m%d")
  
  # Removes february 29th as it is not simulated by Biome-BGC
  feb29 <- format(dates, "%d") == "29" & format(dates, "%m") == "02"
  towerData <- towerData[!feb29,]
  dates <- dates[!feb29]
  
  # Prepare tower data for merging
  years <- as.numeric(format(dates, "%Y"))
  days <- as.numeric(format(dates, "%j"))
  # -1 for days after Feb 29th on leap years
  julianDaysToRemoveOne <- (years %% 4 == 0 & years %% 400 != 0) & days > 60
  days[julianDaysToRemoveOne] <- days[julianDaysToRemoveOne] - 1
  
  # Create data.table
  towerData <- data.table(
    year = as.integer(years),
    day = as.integer(days),
    fluxTower = towerData[, colToKeep[2]]
  )
  
  if (outputVar == "NEE"){
    BBGCdata <- BiomeBGCData[, .(year, timestep, day, BBGC = -daily_nep)]
  } else if (outputVar == "RECO"){
    BBGCdata <- BiomeBGCData[, .(year, timestep, day, BBGC = daily_mr + daily_gr + daily_hr)]
  } else {
    BBGCdata <- BiomeBGCData[, .(year, timestep, day, BBGC = daily_gpp)]
  }
  
  # prepare output
  out <- merge(towerData, BBGCdata)
  
  # put in the same units (gC/m2/day) and rename columns
  out <- out[, .(timestep, year, day, fluxTower, BBGC = BBGC * 1000)]
  
  return(out)
}

mergeMonthlyData <- function(towerData, BiomeBGCData, outputVar, colToKeep){
  # format dates
  dates <- as.Date(paste0(as.character(towerData[,"TIMESTAMP"]), "01"), format = "%Y%m%d")
  
  # Prepare tower data for merging
  years <- format(dates, "%Y")
  months <- format(dates, "%m")
  towerData <- data.table(
    year = as.integer(years),
    month = as.integer(months),
    fluxTower = towerData[, colToKeep[2]]
  )
  
  if (outputVar == "NEE"){
    BBGCdata <- BiomeBGCData[, .(year, month, BBGC = -daily_nep)]
  } else if (outputVar == "RECO"){
    BBGCdata <- BiomeBGCData[, .(year, month, BBGC = daily_mr + daily_gr + daily_hr)]
  } else {
    BBGCdata <- BiomeBGCData[, .(year, month, BBGC = daily_gpp)]
  }
  
  out <- merge(towerData, BBGCdata)
  
  # put in the same units (gC/m2/day) and rename columns
  out <- out[, .(year, month, fluxTower, BBGC = BBGC * 1000)]
  
  return(out)
}

mergeAnnualData <- function(towerData, BiomeBGCData, outputVar, colToKeep, confInt){
  # Prepare tower data for merging
  years <- as.numeric(towerData[, "TIMESTAMP"])
  
  if (is.na(confInt)) {
    towerData <- data.table(year = as.integer(years), fluxTower = towerData[, colToKeep[2]])
    
  } else {
    towerData <- data.table(
      year = as.integer(years),
      fluxTower = towerData[, colToKeep[2]],
      fluxTower_min = towerData[, colToKeep[3]],
      fluxTower_max = towerData[, colToKeep[4]]
    )
    
  }
  
  if (outputVar == "NEE") {
    BBGCdata <- BiomeBGCData[, .(year, BBGC = -daily_nep)]
  } else if (outputVar == "RECO") {
    BBGCdata <- BiomeBGCData[, .(year, BBGC = daily_mr + daily_gr + daily_hr)]
  } else {
    BBGCdata <- BiomeBGCData[, .(year, BBGC = daily_gpp)]
  }
  
  out <- merge(towerData, BBGCdata) |> na.omit()
  
  # put in the same units (gC/m2/yr) and rename columns
  out <- out[, BBGC := BBGC * 1000 * 365]
  
  if (is.na(confInt)) {
    out <- out[, .(year, fluxTower, BBGC)]
  } else{
    out <- out[, .(year, fluxTower, fluxTower_min, fluxTower_max, BBGC)]
  }
  
  return(out)
}
