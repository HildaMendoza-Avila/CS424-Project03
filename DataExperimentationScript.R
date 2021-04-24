# install.packages("tigris")
# install.packages("mapview")
# link to the assignment specifications: https://www.evl.uic.edu/aej/424/21Sproject3.html

library(readr)
library(dplyr)
library(sjmisc)
library(tigris)
library(mapview)
options(scipen = 100)
options(tigris_use_cache = TRUE)
mapviewOptions(fgb = TRUE)

energyData <- read_csv("energy-usage-2010.csv")


# BEGINNING OF DATA CLEANING --------------------------------------

# Replace all of the spaces ( ) and hyphens (-) in the column names with an underscore  (_).

columnClean <- function(x) { 
  colnames(x) <- gsub(" ", "_", colnames(x))
  colnames(x) <- gsub("-", "_", colnames(x)) 
  x 
}
# Remove unnecessary data columns
columnRemove <- function(x) {
  x <- subset(x, select = -c(KWH_STANDARD_DEVIATION_2010, KWH_1ST_QUARTILE_2010, KWH_2ND_QUARTILE_2010, KWH_3RD_QUARTILE_2010, KWH_SQFT_STANDARD_DEVIATION_2010, KWH_SQFT_1ST_QUARTILE_2010, KWH_SQFT_2ND_QUARTILE_2010, KWH_SQFT_3RD_QUARTILE_2010, THERM_STANDARD_DEVIATION_2010, THERM_1ST_QUARTILE_2010, THERM_2ND_QUARTILE_2010, THERM_3RD_QUARTILE_2010, THERMS_SQFT_STANDARD_DEVIATION_2010, THERMS_SQFT_1ST_QUARTILE_2010, THERMS_SQFT_2ND_QUARTILE_2010, THERMS_SQFT_3RD_QUARTILE_2010))
  x
}
energyData <- columnClean(energyData)
energyData <- columnRemove(energyData)

#Remove the functions since they will not be used again 
remove(columnClean)
remove(columnRemove)


# Remove all NA entries under CENSUS_BLOCK since we would not be able to map them
# Also removed all NA entries under COMMUNITY_AREA_NAME
energyData <- energyData[complete.cases(energyData[1:2]),]















# END OF DATA CLEANING --------------------------------------


# rose_island <- blocks(state = "AS", county = "Rose Island")

# Cook_county <- blocks(state = "IL", county = "Cook", year = 2010)

# AustinData <- subset(energyData, energyData$COMMUNITY_AREA_NAME == "Austin")

# mapview(AustinData)

# install.packages("tidycensus")

# library(tidycensus)
# census_api_key("60de26f7e16b417c84bc963d82a3d8f7f5194965", overwrite=TRUE, install = TRUE)
# # options(tigris_use_cache = TRUE)
# cook <- get_acs(geography = "tract",
#                 state = "IL", county = "Cook", geometry = TRUE)

# mapview(cook)



# electricity_palette <- magma(n = length(unique(bavaria$employment_rate)), direction = -1)

# mapview(Cook_county)


# mapView(energyData)
















# Get dataset with geometry set to TRUE
# Cook_values <- get_acs(geography = "tract", state = "IL", 
                        # geometry = TRUE)

# Map your data by the estimate column
# mapView(Cook_values,
        # zcol = "estimate")