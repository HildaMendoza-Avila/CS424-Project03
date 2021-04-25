
# Name: Hilda Mendoza-Avila

# Please uncomment the following lines to install all necessary libraries

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


# All NA entries for columns (5:KWH_JANUARY_2010 - 17:TOTAL_KWH, 19:ZERO_KWH_ACCOUNTS - 32:TOTAL_THERMS, and 34:KWH_TOTAL_SQFT - 57:OCCUPIED_HOUSING_UNITS) 
# inclusive should be replaced with 0 for ease of computation
energyData[5:17][is.na(energyData[5:17])] <- 0
energyData[19:32][is.na(energyData[19:32])] <- 0
energyData[34:57][is.na(energyData[34:57])] <- 0

# All NA entries for columns 18:ELECTRICITY_ACCOUNTS and 33:GAS_ACCOUNTS should be replaced with "Unknown"
energyData[18][is.na(energyData[18])] <- "Unknown"
energyData[33][is.na(energyData[33])] <- "Unknown"

# Create a 'Multiple' entry and correctly integrate the multiple values 

getNewEntryDataFrame <- function(){
  # print("Just Entered Function")
  fst <- curr_block_subset[1,]
  
  c <- c(fst$COMMUNITY_AREA_NAME, fst$CENSUS_BLOCK, "Multiple")
  n <- c(fst$CENSUS_BLOCK)
  
  print(n)
  
  # add the mean values for each column in the subset with multiple entries for the same census block
  for(colNum in 5:57){
    # print(colNum)
    if(colNum != 18 && colNum != 33){
      a <- curr_block_subset[colNum]
      vals <- a[[1,1]]
      
      if(nrow(a) == 2){
        vals <- append(vals, a[[2,1]])
      }
      if(nrow(a) > 2){
        for(x in 2:nrow(a)){
          vals <- append(vals, a[[x,1]])
        }
      }
      
      n <- append(n, mean(vals)) 
    }
  }
  
  data.frame(c[1], n[1], c[2], c[2], n[2], n[3], n[4], n[5], n[6], n[7], n[8], n[9], n[10], n[11], n[12], n[13], n[14], c[2],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24],n[25],n[26],n[27],n[28], c[2], n[29], n[30], n[31], n[32], n[33], n[34], n[35], n[36], n[37], n[38], n[39], n[40], n[41], n[42], n[43], n[44], n[45], n[46], n[47], n[48], n[49], n[50], n[51], n[52], n[53], n[54], n[55], n[56], n[57])
}

all_blocks <- energyData[2][[1,1]]
for(x in 2:nrow(energyData)){
  all_blocks <- append(all_blocks, energyData[2][[x,1]])
}


for(curr_census_block in all_blocks){
  curr_block_subset <- subset(energyData, CENSUS_BLOCK == curr_census_block)
  # print("for-loop")
  # print(curr_census_block)

  if(nrow(curr_block_subset) > 1){  # Add a new row to energyData
    # print(curr_census_block)

    # Create a Data Frame with the values of the new entry - Step 1
    multiples_entry <- getNewEntryDataFrame()

    # Name the columns of the Data Frame (same as the columns in energyData) - Step 2
    names(multiples_entry) <- c("COMMUNITY_AREA_NAME","CENSUS_BLOCK","BUILDING_TYPE","BUILDING_SUBTYPE","KWH_JANUARY_2010","KWH_FEBRUARY_2010","KWH_MARCH_2010","KWH_APRIL_2010","KWH_MAY_2010","KWH_JUNE_2010","KWH_JULY_2010","KWH_AUGUST_2010","KWH_SEPTEMBER_2010","KWH_OCTOBER_2010","KWH_NOVEMBER_2010","KWH_DECEMBER_2010","TOTAL_KWH","ELECTRICITY_ACCOUNTS","ZERO_KWH_ACCOUNTS","THERM_JANUARY_2010","THERM_FEBRUARY_2010","THERM_MARCH_2010","TERM_APRIL_2010", "THERM_MAY_2010","THERM_JUNE_2010","THERM_JULY_2010","THERM_AUGUST_2010","THERM_SEPTEMBER_2010","THERM_OCTOBER_2010","THERM_NOVEMBER_2010","THERM_DECEMBER_2010","TOTAL_THERMS","GAS_ACCOUNTS","KWH_TOTAL_SQFT","THERMS_TOTAL_SQFT","KWH_MEAN_2010","KWH_MINIMUM_2010","KWH_MAXIMUM_2010","KWH_SQFT_MEAN_2010","KWH_SQFT_MINIMUM_2010","KWH_SQFT_MAXIMUM_2010","THERM_MEAN_2010","THERM_MINIMUM_2010","THERM_MAXIMUM_2010","THERMS_SQFT_MEAN_2010","THERMS_SQFT_MINIMUM_2010","THERMS_SQFT_MAXIMUM_2010","TOTAL_POPULATION","TOTAL_UNITS","AVERAGE_STORIES","AVERAGE_BUILDING_AGE","AVERAGE_HOUSESIZE","OCCUPIED_UNITS","OCCUPIED_UNITS_PERCENTAGE","RENTER_OCCUPIED_HOUSING_UNITS","RENTER_OCCUPIED_HOUSING_PERCENTAGE","OCCUPIED_HOUSING_UNITS")

    # Using rbind() function to insert new row with "Multiple" information
    energyData <- rbind(energyData, multiples_entry)

  }
}


# END OF DATA CLEANING --------------------------------------

Cook_county <- blocks(state = "IL", county = "Cook", year = 2010)
names(Cook_county)[names(Cook_county) == "GEOID10"] <- "CENSUS_BLOCK"

initial_area <- subset(energyData, COMMUNITY_AREA_NAME == "Near West Side")
    
initial_map <- subset(Cook_county, CENSUS_BLOCK %in% initial_area$CENSUS_BLOCK)




# initial_area$MULTIPLE






# ta <- table(initial_area$CENSUS_BLOCK)
# initial_area[initial_area$CENSUS_BLOCK %in% names(ta)[ta > 1], ]




# initial_area[duplicated(initial_area$CENSUS_BLOCK) | duplicated(initial_area$CENSUS_BLOCK, fromLast=TRUE), ]

# install.packages("gdata")
# library(gdata)
# initial_area[duplicated2(initial_area$CENSUS_BLOCK), ]
# initial_area$CENSUS_BLOCK


# initial_map$TOTAL_KWH <- 







# getYearlyElectricityUsage <- function(census_block) {
#   # block_yearly_elec_entries <- subset(energyData, CENSUS_BLOCK %in% initial_map$CENSUS_BLOCK)
#   
#   
#   
#   
#   # block_yearly_elec_entries <- subset(energyData, CENSUS_BLOCK == census_block)
#   # yearlyElectricityUsageValue <- mean(block_yearly_elec_entries$TOTAL_KWH)
#   # yearlyElectricityUsageValue
# }




# initial_map$TOTAL_KWH 


# yearly_entries <- getYearlyElectricityUsage(initial_map$GEOID10)





# mapview(initial_map, zcol = "INTPTLAT10")





# chicago_blocks <- subset(Cook_county, GEOID10 %in% energyData$CENSUS_BLOCK)
# mapview(chicago_blocks)


















# initial Experimentation Code -------------------------------------------------


# rose_island <- blocks(state = "AS", county = "Rose Island")



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