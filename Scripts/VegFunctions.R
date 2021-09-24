# Function definitions for vegetation analysis
#
#

VegImport <- function(filepath){
  # import vegetation data from a csv file
  df <- read.csv(filepath, fileEncoding = "UTF-8-BOM")
  df$PERCENT_COVER <- as.numeric(df$PERCENT_COVER)
  df <- subset(df, COMMUNITY == 1)
  return(df)
}

VegLongCleanup <- function(data, columnNames){
  # keep only necessary columns
  cleanData <- data |> select(columnNames)
  return(cleanData)
}

VegLongToWide <- function(data){
  # flip data to wide format
  wideData <- data |> spread(SPECIES_CODE, PERCENT_COVER)
  return(wideData)
}

VegUniqueSpecies <- function(data, origin){
  # returns list of species of given an origin
  speciesList <- subset(data$SPECIES_CODE, data$ORIGIN == origin)
  return(speciesList)
}

VegRelativeAbundance <- function(veg_subset, veg_total){
  ra <- mean(rowSums(veg_subset)/rowSums(veg_total))
  sd <- sd(rowSums(veg_subset)/rowSums(veg_total))
  result <- data.frame(ra, sd)
  return(result)
}


