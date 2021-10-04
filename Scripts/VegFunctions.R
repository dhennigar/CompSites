# Function definitions for vegetation analysis
# To be sourced by 2021 or 2015 analysis scripts.
# Functions expect attributes "SPECIES_CODE", "PERCENT_COVER", and "ORIGIN".

VegImport <- function(filepath, year){
  # import vegetation data from a csv file
  df <- read.csv(filepath, fileEncoding = "UTF-8-BOM")
  df$PERCENT_COVER <- as.numeric(df$PERCENT_COVER)
  df <- subset(df, COMMUNITY == 1)
  
  if(year == "2015"){
    df <- df[1:8] # reduce issues with 2015 data importing
    df$MAX_LH_CM <- df$MAX_LH # rename Lyngbye height column
  }
 
  return(df)
}

VegLongToWide <- function(data){
  # convert veg data from long to wide format
  wideData <- data |>
    select(c(PLOT, SPECIES_CODE, PERCENT_COVER)) |>
    spread(SPECIES_CODE, PERCENT_COVER)
  wideData[is.na(wideData)] <- 0
  wideData <- wideData[-1]
  return(wideData)
}

VegUniqueSpecies <- function(data){
  # returns lists of unique species of each origin type.
  n <- unique(subset(data$SPECIES_CODE, data$ORIGIN == "N"))
  e <- unique(subset(data$SPECIES_CODE, data$ORIGIN == "E"))
  i <- unique(subset(data$SPECIES_CODE, data$ORIGIN == "I"))
  u <- unique(subset(data$SPECIES_CODE, data$ORIGIN == "U"))
  s <- unique(subset(data$SPECIES_CODE, data$ORIGIN == "S"))
  plants <- list(n, e, i, u, s)
  names(plants) <- c("native", "exotic", "invasive", "unknown", "substrate")
  return(plants)
}

VegPercentCoverStats <- function(data){
  # takes wide veg data and returns percent cover summary stats
  pcMean <- sapply(data, mean, na.rm = TRUE)
  pcSD <- sapply(data, sd, na.rm = TRUE)
  pcMax <- sapply(data, max, na.rm = TRUE)
  pcMin <- sapply(data, min, na.rm = TRUE)
  result <- data.frame(pcMean, pcSD, pcMax, pcMin)
  return(result)
}

VegRelativeAbundance <- function(wideData, plantLists){
  # takes wide data and plants list and returns relative abundance stats.
  
  n <- select(wideData, plantLists$native)
  e <- select(wideData, plantLists$exotic)
  i <- select(wideData, plantLists$invasive)
  u <- select(wideData, plantLists$unknown)
  total <- select(wideData, -plantLists$substrate)
  
  n_ra <- mean(rowSums(n)/rowSums(total), na.rm = TRUE)
  n_sd <- sd(rowSums(n)/rowSums(total), na.rm = TRUE)
  e_ra <- mean(rowSums(e)/rowSums(total), na.rm = TRUE)
  e_sd <- sd(rowSums(e)/rowSums(total), na.rm = TRUE)
  i_ra <- mean(rowSums(i)/rowSums(total), na.rm =TRUE)
  i_sd <- sd(rowSums(i)/rowSums(total), na.rm =TRUE)
  u_ra <- mean(rowSums(u)/rowSums(total), na.rm = TRUE)
  u_sd <- sd(rowSums(u)/rowSums(total), na.rm = TRUE)
  
  result <- data.frame(n_ra, n_sd,
                       e_ra, e_sd,
                       i_ra, i_sd,
                       u_ra, u_sd)
  
  return(result)
}

VegStats <- function(longData, wideData, plantLists){
  
  lyngbyeMean <- mean(longData$MAX_LH_CM, na.rm = TRUE)
  lyngbyeSD <- sd(longData$MAX_LH_CM, na.rm = TRUE)
  
  richnessMean <- mean(specnumber(select(wideData, -plantLists$substrate)))
  richnessSD <- sd(specnumber(select(wideData, -plantLists$substrate)))
  nativeRichnessMean <- mean(specnumber(select(wideData, -plantLists$native)))
  nativeRichnessSD <- sd(specnumber(select(wideData, plantLists$native)))
  
  if(length(plantLists$native) == 0){
    shannon <- NA
    shannonSD <- NA
    simpson <- NA
    simpsonSD <- NA
  } else {
    shannon <- mean(diversity(select(wideData, plantLists$native), index = "shannon"))
    shannonSD <- sd(diversity(select(wideData, plantLists$native), index = "shannon"))
    simpson <- mean(diversity(select(wideData, plantLists$native), index = "simpson"))
    simpsonSD <- sd(diversity(select(wideData, plantLists$native), index = "simpson"))
  }
  
  result <- data.frame(lyngbyeMean, lyngbyeSD,
                       richnessMean, richnessSD,
                       nativeRichnessMean, nativeRichnessSD,
                       shannon, shannonSD, simpson, simpsonSD)
  return(result)
}


