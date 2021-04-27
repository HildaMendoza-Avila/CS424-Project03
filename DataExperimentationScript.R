
# Name: Hilda Mendoza-Avila

# Please uncomment the following lines to install all necessary libraries

# install.packages("tigris")
# install.packages("mapview")

# link to the assignment specifications: https://www.evl.uic.edu/aej/424/21Sproject3.html
library(shiny)
library(readxl)
library(ggplot2)
library(leaflet)
options(scipen = 100)
library(readr)
library(dplyr)
library(sjmisc)
library(viridis)
library(tigris)
library(mapview)
options(scipen = 100)
options(tigris_use_cache = TRUE)
mapviewOptions(fgb = TRUE)





# Define UI for app
ui <- fluidPage(
  # App title 
  titlePanel("Project 3 - We've Got the Power"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing stateDataset ----
      selectInput(inputId = "powerDataset",
                  label = "Choose a data filter:",
                  choices = c(
                    "Gas",
                    "Electricity ",
                    "Building Age ",
                    " Building Type",
                    "Building Height",
                    "Total Population"
                  )
      ) 
      # leafletOutput("mymap"),
      # mapview:::plainViewOutput("test")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: leaflet Map
      # TODO: add the Illinois map
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      # verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      # tableOutput("view")
      
    )
  )
)

# Define server logic required to draw a leaflet Map
server <- function(input, output) {
  
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
  # set up curr_area in order to call this function
  
  getNewEntryDataFrame <- function(){
    fst <- curr_area[curr_block_inds[1], ]  #energyData[curr_block_inds[1], ]       
    
    c <- c(fst$COMMUNITY_AREA_NAME, "Multiple")
    n <- c(fst$CENSUS_BLOCK)
    
    print(n)
    
    # add the mean values for each column in the subset with multiple entries for the same census block
    for(colNum in 5:57){
      if(colNum != 18 && colNum != 33){
        sum <- 0
        for(rowNum in curr_block_inds){
          sum <- sum + curr_area[[rowNum, colNum]]      #energyData[[rowNum, colNum]]
        }
        
        n <- append(n, (sum/(length(curr_block_inds))))
      }
    }
    
    data.frame(c[1], n[1], c[2], c[2], n[2], n[3], n[4], n[5], n[6], n[7], n[8], n[9], n[10], n[11], n[12], n[13], n[14], c[2],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24],n[25],n[26],n[27],n[28], c[2], n[29], n[30], n[31], n[32], n[33], n[34], n[35], n[36], n[37], n[38], n[39], n[40], n[41], n[42], n[43], n[44], n[45], n[46], n[47], n[48], n[49], n[50], n[51], n[52])
    
  }
  
  # END OF DATA CLEANING --------------------------------------
  
  Cook_county <- blocks(state = "IL", county = "Cook", year = 2010)
  names(Cook_county)[names(Cook_county) == "GEOID10"] <- "CENSUS_BLOCK"
  
  curr_area <- subset(energyData, COMMUNITY_AREA_NAME == "Near West Side")
  
  # Beginning of Dealing with Multiple entries -----------------------------------
  
  all_census_blocks <- curr_area[2]
  all_blocks <- all_census_blocks[1, ]     #energyData[[1,2]]
  for(x in 2:nrow(curr_area)){
    curr_val <- all_census_blocks[x, ]    #energyData[[x,2]]
    if((curr_val %in% all_blocks) == FALSE){             #(match(curr_val,all_blocks)) < 1){
      all_blocks <- append(all_blocks, curr_val)
    }
  }
  
  print("The size of all_blocks:")
  print(length(all_blocks))
  
  # i <- 0
  # j <- 0
  for(curr_census_block in all_blocks){
    curr_block_inds <- which(all_census_blocks$CENSUS_BLOCK == curr_census_block)#all_census_blocks %in% curr_census_block)
    # print(curr_census_block)
    # print(length(curr_block_inds))
    # print(curr_block_inds)
    # print("-----------------")
  
  
  
    # i <- i + 1
  
    if(length(curr_block_inds) > 1){   # Add a new row to curr_area
      # j <- j + 1
      # print(j)
  
      # Create a Data Frame with the values of the new entry - Step 1
      multiples_entry <- getNewEntryDataFrame()
  
      # Name the columns of the Data Frame (same as the columns in curr_area) - Step 2
      names(multiples_entry) <- c("COMMUNITY_AREA_NAME","CENSUS_BLOCK","BUILDING_TYPE","BUILDING_SUBTYPE","KWH_JANUARY_2010","KWH_FEBRUARY_2010","KWH_MARCH_2010","KWH_APRIL_2010","KWH_MAY_2010","KWH_JUNE_2010","KWH_JULY_2010","KWH_AUGUST_2010","KWH_SEPTEMBER_2010","KWH_OCTOBER_2010","KWH_NOVEMBER_2010","KWH_DECEMBER_2010","TOTAL_KWH","ELECTRICITY_ACCOUNTS","ZERO_KWH_ACCOUNTS","THERM_JANUARY_2010","THERM_FEBRUARY_2010","THERM_MARCH_2010","TERM_APRIL_2010", "THERM_MAY_2010","THERM_JUNE_2010","THERM_JULY_2010","THERM_AUGUST_2010","THERM_SEPTEMBER_2010","THERM_OCTOBER_2010","THERM_NOVEMBER_2010","THERM_DECEMBER_2010","TOTAL_THERMS","GAS_ACCOUNTS","KWH_TOTAL_SQFT","THERMS_TOTAL_SQFT","KWH_MEAN_2010","KWH_MINIMUM_2010","KWH_MAXIMUM_2010","KWH_SQFT_MEAN_2010","KWH_SQFT_MINIMUM_2010","KWH_SQFT_MAXIMUM_2010","THERM_MEAN_2010","THERM_MINIMUM_2010","THERM_MAXIMUM_2010","THERMS_SQFT_MEAN_2010","THERMS_SQFT_MINIMUM_2010","THERMS_SQFT_MAXIMUM_2010","TOTAL_POPULATION","TOTAL_UNITS","AVERAGE_STORIES","AVERAGE_BUILDING_AGE","AVERAGE_HOUSESIZE","OCCUPIED_UNITS","OCCUPIED_UNITS_PERCENTAGE","RENTER_OCCUPIED_HOUSING_UNITS","RENTER_OCCUPIED_HOUSING_PERCENTAGE","OCCUPIED_HOUSING_UNITS")
  
      # Using rbind() function to insert new row with "Multiple" information
      curr_area <- rbind(curr_area, multiples_entry)
      # print("Just Binded:")
      # print(multiples_entry)
  
    }
  }
  
  # End of Dealing with Multiple entries  ----------------------------------------
  
  # current_blocks <- merge(current_blocks, curr_area, by = "CENSUS_BLOCK")
  
  multiples_subset <- subset(curr_area, BUILDING_TYPE == "Multiple")
  
  curr_area <- curr_area %>% distinct(CENSUS_BLOCK, .keep_all = TRUE)
  curr_area <- rbind(curr_area, multiples_subset)
  
  # current_blocks <- subset(Cook_county, CENSUS_BLOCK %in% multiples_subset$CENSUS_BLOCK)
  
  current_blocks <- subset(Cook_county, CENSUS_BLOCK %in% curr_area$CENSUS_BLOCK)
  # mapview(current_blocks)
  
  current_blocks <- merge(current_blocks, curr_area, by = "CENSUS_BLOCK")
  
  
  # Beginning of Experimentation area --------------------------------------------
  
  
  max_curr <- max(current_blocks$TOTAL_KWH)
  min_curr <- min(current_blocks$TOTAL_KWH)
  mapview(current_blocks, zcol = "TOTAL_KWH", at = seq(max_curr, min_curr, -(max_curr-min_curr)/5), legend = TRUE)

  
  
  # End of Experimentation area --------------------------------------------------
  
  # 
  # output$test <- mapview:::renderPlainView({
  #   
  # })
  
}



# Create Shiny app
shinyApp(ui = ui, server = server)
