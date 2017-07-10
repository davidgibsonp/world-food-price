require(dplyr)
require(readr)
require(data.table)
require(reshape)
require(tidyr)
require(tibble)
require(ggplot2)
require(rmarkdown)

# To Skip Cleaning Load
food <- read.csv("data/clean/food_data.csv")
# 
# CURENCY CLEANING-------------------------------------------
codes <- read.csv("data/dirty/curency_codes.csv")

# Remove unused columns and incorect codes
codes$NumericCode <- NULL
codes$MinorUnit <- NULL
codes$WithdrawalDate <- NULL
codes$Entity <- NULL

# Remove unused rows MUST BE BETTER WAY
codes <- codes[codes$AlphabeticCode != "XXX", ]
codes <- codes[codes$AlphabeticCode != "XTS", ]
codes <- codes[codes$AlphabeticCode != "BUK", ]
codes <- codes[codes$AlphabeticCode != "", ]

# Remove duplicates in AlphabeticCode's
codes <-
  codes %>%
  group_by(Currency, AlphabeticCode) %>%
  count(AlphabeticCode)

# Removes generated n column generated from above
codes$n <- NULL
codes$X <- NULL

# Exports cleaned currency codes
write.csv(codes, file = "data/clean/cleaned_currency_codes.csv", row.names=FALSE)
rm(codes)

# PPP CLEANING-------------------------------------------
# Retrieved from http://data.worldbank.org/indicator/PA.NUS.PPP?end=2008&start=2008&view=bar&year_low_desc=true
ppp <- read.csv("data/dirty/ppp-data-last-update-5-26-17.csv", header = TRUE, na.strings = "")

# Remove unused years
ppp <- ppp[, -c(3:44)] # !!!ONLY RUN ONCE!!!

ppp$X2016 <- NULL

# Renaming
ppp <- rename(ppp, c(Country.Name="country"))
ppp <- rename(ppp, c(Country.Code="country_code"))

# manually renamed "Egypt" "Yemen" "Lao People's Democratic Republic" "Tanzania" "Gambia" reb of congo to congo "Cape Verde"  "State of Palestine" & "Kyrgyzstan"

# Disable scientific notation
options(scipen = 50)

# Export cleaned data
write.csv(ppp, file = "data/clean/ppp_df.csv", row.names=FALSE)


# FOOD DATA CLEANING-------------------------------------------

# Load Data
food <- read.csv("data/dirty/WFPVAM_FoodPrices_13-03-2017.csv")
# og_food <- read.csv("data/dirty/WFPVAM_FoodPrices_13-03-2017.csv")
# Delete Unused Columns/Rows
food$mp_commoditysource <- NULL
food$adm0_id <- NULL
food$adm1_id <- NULL
food$mkt_id <- NULL
food$cur_id <- NULL
food$pt_id <- NULL
food$um_id <- NULL
food$cm_id <- NULL

food <- food[food$mp_year>1999,]
food <- food[food$mp_year<2016,]

# Rename Columnsl;
food <- rename(food, c(adm0_name="country"))
food <- rename(food, c(adm1_name="city"))
food <- rename(food, c(mkt_name="market"))
food <- rename(food, c(cm_name="food_name"))
food <- rename(food, c(cur_name="currency"))
food <- rename(food, c(pt_name="seller_type"))
food <- rename(food, c(um_name="unit"))
food <- rename(food, c(mp_month="month"))
food <- rename(food, c(mp_year="year"))
food <- rename(food, c(mp_price="price"))

# Add date column
convert_month <- function(number){
  #Converts month to the mm format
  if(number %in% 1:9){
    out <- paste0('0',number)
  }else{
    out <- as.character(number)
  }
  return(out)
}

food$month <- sapply(food$month, convert_month)
food$date <- as.Date(paste0(food$year,'-',food$month,'-01'))


# Replace "Somaliland Shilling" with "SOS"
food$currency[food$currency == "Somaliland Shilling"] <- "SOS"

# Add country code with left join
region <- read.csv("data/clean/country_region_code_incomegroup.csv")
region$ppp_country_name <- NULL #delete ppp country name
region <- rename(region, c(code="country_code"))
region <- rename(region, c(incomegroup="country_income_group"))
region <- rename(region, c(food_country_name="country"))

food <- left_join(food, region, by = c("country" = "country")) #The join
rm(region)

# Joing food groups data
group <- read.csv("data/clean/food_groups.csv")
food <- left_join(food, group, by = c("food_name" = "food_name")) #The join
rm(group)

# Delete rows that are not food BETTER WAY THAN MAKING THEM NA THEN ROMOVE NA
food <- food[food$food_group != "Not Food", ]
food <- food[food$food_group != "Loaf", ]
food <- food[food$food_group != "Sack", ]
food <- food[food$food_group != "Bunch", ]
food <- food[complete.cases(food), ]



# Add PPP and unfied price
# IMPROVE BY NEED TO EDIT FOR TO MATCH COUNTRY_CODE NOT COUNTRY
ppp <- read.csv("data/clean/ppp_df.csv")

get_ppp_factor <- function(country, year){
  year <- paste0("X",as.character(year))
  return(ppp[ppp$country==country, year])
}

ppp_country_year <- apply(food[,c('country','year')], 1, function(x) get_ppp_factor(x[1],x[2]))
ppp_country_year <- lapply(ppp_country_year, function(x) ifelse(is.null(x),NA,x))
ppp_country_year <- unlist(ppp_country_year)

food$ppp_factor <- ppp_country_year
rm(ppp_country_year)
rm(ppp)

# Calculates a unified price column
food$unified_price <- food$price/food$ppp_factor # u_price "unified price"

# Unify units
food$unit <- as.character(food$unit)

food$unit[food$unit == "11.5 KG"] <- "11500 G"
food$unit[food$unit == "100 KG"] <- "100000 G"
food$unit[food$unit == "91 KG"] <- "91000 G"
food$unit[food$unit == "50 KG"] <- "50000 G"
food$unit[food$unit == "45 KG"] <- "45000 G"
food$unit[food$unit == "1.8 KG"] <- "1800 G"
food$unit[food$unit == "2 KG"] <- "2000 G"
food$unit[food$unit == "5 KG"] <- "5000 G"
food$unit[food$unit == "12.5 KG"] <- "12500 G"
food$unit[food$unit == "1.5 KG"] <- "1500 G"
food$unit[food$unit == "10 KG"] <- "10000 G"
food$unit[food$unit == "12 KG"] <- "12000 G"
food$unit[food$unit == "60 KG"] <- "60000 G"
food$unit[food$unit == "25 KG"] <- "25000 G"
food$unit[food$unit == "3 KG"] <- "3000 G"
food$unit[food$unit == "3.5 KG"] <- "3500 G"
food$unit[food$unit == "90 KG"] <- "90000 G"
food$unit[food$unit == "Cuartilla"] <- "2721.55422 G"
food$unit[food$unit == "MT"] <- "1000000 G"
food$unit[food$unit == "Pound"] <- "453.592 G"
food$unit[food$unit == "Libra"] <- "453.592 G"
food$unit[food$unit == "KG"] <- "1000 G"
food$unit[food$unit == "Marmite"] <- "2500 G"
food$unit[food$unit == "L"] <- "1 L"
food$unit[food$unit == "500 ML"] <- "0.5 L"
food$unit[food$unit == "750 ML"] <- "0.75 L"
food$unit[food$unit == "Gallon"] <- "3.78541 L"

food$unit[food$unit == "Dozen"] <- "12 Unit"
food$unit[food$unit == "Unit"] <- "1 Unit"
food$unit[food$unit == "Head"] <- "1 Unit"
food$unit[food$unit == "Packet"] <- "1 Unit"
food$unit[food$unit == "100 Tubers"] <- "100 Unit"
food$unit[food$unit == "Marmite"] <- "1 Marmite"

# Split the units it unit_type
food <-
  food %>%
  separate(unit, c("unit", "unit_type"), " ")

# Create price_per_one_unit
food$unit <- as.numeric(food$unit)
food$price_per_one_unit <- food$unified_price/food$unit

# Removes rows with any NA value - needed PPP for all Syria and some Afgan/Sudan years
# HOW TO REMOVE ROWS WHERE A SPECIFIC COLUMN HAS NA
# food <- food[complete.cases(food), ]

# Find top foods
# top <- as.data.frame(table(food$food_name))

# Add import column
import <- food[grep("import", food$food_name), ]
not_import <- food[!grepl("import", food$food_name), ]
local <- food[grepl("local", food$food_name), ]

local$import <- "Local"
import$import <- "Import"
not_import$import <- "Not Import"

food <- do.call("rbind", list(import, not_import, local))
rm(import, not_import, local)

# Unify top 7 foods
food_groups <- food
food_groups$food_name <- as.character(food_groups$food_name)
food_groups$food_group <- as.character(food_groups$food_name)

food_groups$food_group[food_groups$food_name ==   "Rice"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (basmatibroken)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (coarse)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (estaquilla)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (glutinousfirst quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (glutinoussecond quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (glutinousunmilled)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (good quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (high qualitylocal)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (high quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (local)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (long grain)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (low qualitylocal)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (low quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (white)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (tchako)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (medium grain)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (mixedlow quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (ordinaryfirst quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (ordinarysecond quality)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (ordinaryunmilled)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (paddylong grainlocal)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (paddy)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (red nadu)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (regularmilled)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (small grainimported)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (whiteimported)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (denikassiaimported)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (medium grainimported)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (long grainimported)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (importedEgyptian)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (importedIndian)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (importedTanzanian)"] <-"Rice"
food_groups$food_group[food_groups$food_name ==   "Rice (imported)"] <-"Rice"

food_groups$food_group[food_groups$food_name ==   "Beans"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (black)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (butter)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (catarino)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (dry)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (favadry)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (greenfresh)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (kidney red)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (kidney white)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (kidney)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (magnum)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (mung)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (niebe)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (redfresh)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (red)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (silk red)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (string)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (sugar-red)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (sugar)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans (white)"] <-"Beans"
food_groups$food_group[food_groups$food_name ==   "Beans(mash)"] <-"Beans"

food_groups$food_group[food_groups$food_name ==   "Sorghum"] <-"Sorghum"
food_groups$food_group[food_groups$food_name ==   "Sorghum (food aid)"] <-"Sorghum"
food_groups$food_group[food_groups$food_name ==   "Sorghum (red)"] <-"Sorghum"
food_groups$food_group[food_groups$food_name ==   "Sorghum (taghalit)"] <-"Sorghum"
food_groups$food_group[food_groups$food_name ==   "Sorghum (white)"] <-"Sorghum"

food_groups$food_group[food_groups$food_name ==   "Oil"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (cooking)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (cotton)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (groundnut)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (maize)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (mustard)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (olive)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (palm)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (soybean)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (sunflower)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (vegetablelocal)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (vegetable)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (mixedimported)"] <-"Oil"
food_groups$food_group[food_groups$food_name ==   "Oil (vegetableimported)"] <- "Oil"

food_groups$food_group[food_groups$food_name ==   "Maize"] <- "Maize"
food_groups$food_group[food_groups$food_name ==   "Maize (local)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize (white)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize (yellow)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (local)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (whitebreakfast)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (whitefirst grade)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (whiteroller)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (whitewith bran)"] <-"Maize"
food_groups$food_group[food_groups$food_name ==   "Maize meal (whitewithout bran)"] <-"Maize"
food_groups$food_group[food_groups$food_name =="Maize (imported)"] <-"Maize"

# # Run this if you want to unifiy top 7 foods
food <- food_groups
rm(food_groups)

# Disable scientific notation
options(scipen = 50)
View(food)
# Reorder data for viewability
food <- food[c("country", "country_code", "country_income_group", "region", "city", "market", "date", "year", "month", "seller_type", "import", "food_group", "food_name", "price_per_one_unit", "unit_type","price", "unit", "currency", "unified_price", "ppp_factor")]

# Export Cleaned Data
write.csv(food, file = "data/clean/food_data.csv", row.names=FALSE)

# Load Functions For Analysis-------------------------------------------

convert_month <- function(number){
  #Converts month to the mm format
  if(number %in% 1:9){
    out <- paste0('0',number)
  }else{
    out <- as.character(number)
  }
  return(out)
}

# Average functions
import_type_average <- function(grouped_food_df){
  import_type <- grouped_food_df[0,]
  for (x in unique(grouped_food_df$date)){
    df <- filter(grouped_food_df, date==x)
    empty_df <- df[1,]
    empty_df$price_per_one_unit <- mean(df$price_per_one_unit)
    empty_df$unified_price <- mean(df$unified_price)
    empty_df$ppp_factor <- mean(df$ppp_factor)
    empty_df$price <- mean(df$price)
    empty_df$city <- NULL
    empty_df$country_income_group <- NULL
    empty_df$country_code <- NULL
    empty_df$curency <- NULL
    empty_df$seller_type <- NULL
    empty_df$market <- NULL
    empty_df$region <- NULL
    import_type <- rbind(empty_df, import_type)
  }
  import_type
}

national_average_fun <- function(country_df_with_out_national_average){
  stdv <- country_df_with_out_national_average$monthly_inflation
  stdv <- stdv[is.numeric(stdv)]
  stdv <- sd(stdv)
  national_average_df <- country_df_with_out_national_average[0,]
  for (x in unique(country_df_with_out_national_average$date)){
    df <- filter(country_df_with_out_national_average, date==x)
    empty_df <- df[1,]
    empty_df$price_per_one_unit <- mean(df$price_per_one_unit)
    empty_df$unified_price <- mean(df$unified_price)
    empty_df$ppp_factor <- mean(df$ppp_factor)
    empty_df$price <- mean(df$price)
    empty_df$stdv <- stdv
    empty_df$city <- ""
    empty_df$market <- "National Average"
    national_average_df <- rbind(empty_df, national_average_df)
  }
  national_average_df
}

# national_average_fun <- function(country_df_with_out_national_average){
#   national_average_df <- country_df_with_out_national_average[0,]
#   for (x in unique(country_df_with_out_national_average$date)){
#     df <- filter(country_df_with_out_national_average, date==x)
#     empty_df <- df[1,]
#     empty_df$price_per_one_unit <- mean(df$price_per_one_unit)
#     empty_df$unified_price <- mean(df$unified_price)
#     empty_df$ppp_factor <- mean(df$ppp_factor)
#     empty_df$price <- mean(df$price)
#     empty_df$city <- ""
#     empty_df$market <- "National Average"
#     national_average_df <- rbind(empty_df, national_average_df)
#   }
#   national_average_df
# }

world_average <- function(grouped_food_df){
  world_average <- grouped_food_df[0,]
  for (x in unique(grouped_food_df$date)){
    df <- filter(grouped_food_df, date==x)
    empty_df <- df[1,]
    empty_df$price_per_one_unit <- mean(df$price_per_one_unit)
    empty_df$unified_price <- mean(df$unified_price)
    empty_df$ppp_factor <- mean(df$ppp_factor)
    empty_df$price <- mean(df$price)
    empty_df$country <- "World"
    empty_df$city <- NULL
    empty_df$country_income_group <- NULL
    empty_df$country_code <- NULL
    empty_df$seller_type <- NULL
    empty_df$market <- NULL
    empty_df$region <- NULL
    world_average <- rbind(empty_df, world_average)
  }
  world_average
}

region_average <- function(grouped_food_df){
  region_average <- grouped_food_df[0,]
  for (x in unique(grouped_food_df$date)){
    df <- filter(grouped_food_df, date==x)
    empty_df <- df[1,]
    empty_df$price_per_one_unit <- mean(df$price_per_one_unit)
    empty_df$unified_price <- mean(df$unified_price)
    empty_df$ppp_factor <- mean(df$ppp_factor)
    empty_df$price <- mean(df$price)
    empty_df$country <- NULL
    empty_df$city <- NULL
    empty_df$country_income_group <- NULL
    empty_df$country_code <- NULL
    empty_df$seller_type <- NULL
    empty_df$market <- NULL
    empty_df$region <- ""
    region_average <- rbind(empty_df, region_average)
  }
  region_average
}

seller_type_average <- function(grouped_food_df){ 
  Farm_Gate <- filter(grouped_food_df, seller_type=="Farm Gate")
  Farm_Gate <- seller_type_average(Farm_Gate)
  if (is.data.frame(Farm_Gate) && nrow(Farm_Gate)==0) { 
  }else {  
    Farm_Gate$seller_type <- "Farm Gate"
  }
  
  Producer <- filter(grouped_food_df, seller_type=="Producer")
  Producer <- seller_type_average(Producer)
  if (is.data.frame(Producer) && nrow(Producer)==0) {
    
  }else {  
    Producer$seller_type <- "Producer"
  }
  
  Retail <- filter(grouped_food_df, seller_type=="Retail")
  Retail <- seller_type_average(Retail)
  if (is.data.frame(Retail) && nrow(Retail)==0) { 
  }else {
    Retail$seller_type <- "Retail"
  }
  
  
  Wholesale <- filter(grouped_food_df, seller_type=="Wholesale")
  Wholesale <- seller_type_average(Wholesale)
  if (is.data.frame(Wholesale) && nrow(Wholesale)==0) {
  }else {
    Wholesale$seller_type <- "Wholesale"
  }
  
  grouped_food_df <- do.call("rbind", list(Farm_Gate, Producer, Retail, Wholesale ))
  grouped_food_df
}

grouping_by_region_average <- function(grouped_food_df){ 
  East_Asia_Pacific <- filter(grouped_food_df, region=="East Asia & Pacific")
  East_Asia_Pacific <- region_average(East_Asia_Pacific)
  if (is.data.frame(East_Asia_Pacific) && nrow(East_Asia_Pacific)==0) { 
  }else {  
    East_Asia_Pacific$region <- "East Asia & Pacific"
  }
  
  Europe_Central_Asia <- filter(grouped_food_df, region=="Europe & Central Asia")
  Europe_Central_Asia <- region_average(Europe_Central_Asia)
  if (is.data.frame(Europe_Central_Asia) && nrow(Europe_Central_Asia)==0) {
    
  }else {  
    Europe_Central_Asia$region <- "Europe & Central Asia"
  }
  
  Latin_America_Caribbean <- filter(grouped_food_df, region=="Latin America & Caribbean")
  Latin_America_Caribbean <- region_average(Latin_America_Caribbean)
  if (is.data.frame(Latin_America_Caribbean) && nrow(Latin_America_Caribbean)==0) { 
  }else {
    Latin_America_Caribbean$region <- "Latin America & Caribbean"
  }
  
  
  Middle_East_North_Africa <- filter(grouped_food_df, region=="Middle East & North Africa")
  Middle_East_North_Africa <- region_average(Middle_East_North_Africa)
  if (is.data.frame(Middle_East_North_Africa) && nrow(Middle_East_North_Africa)==0) {
  }else {
    Middle_East_North_Africa$region <- "Middle East & North Africa"
  }
  
  South_Asia <- filter(grouped_food_df, region=="South Asia")
  South_Asia <- region_average(South_Asia)
  if (is.data.frame(South_Asia) && nrow(South_Asia)==0) {
  }else {  
    South_Asia$region <- "South Asia"
  }
  
  Sub_Saharan_Africa <- filter(grouped_food_df, region=="Sub-Saharan Africa")
  Sub_Saharan_Africa <- region_average(Sub_Saharan_Africa)
  if (is.data.frame(Sub_Saharan_Africa) && nrow(Sub_Saharan_Africa)==0) {
  }else {
    Sub_Saharan_Africa$region <- "Sub-Saharan Africa"
  }
  
  grouped_food_df <- do.call("rbind", list(East_Asia_Pacific, Europe_Central_Asia, Latin_America_Caribbean, Middle_East_North_Africa, South_Asia, Sub_Saharan_Africa))
  grouped_food_df
}

# Grouping functions
import_type_grouping <- function(grouped_food_df){
  not_import <- filter(grouped_food_df, import=="Not Import")
  import <- filter(grouped_food_df, import=="Import")
  local <- filter(grouped_food_df, import=="Local")
    
  not_import <- import_type_average(not_import)
  import <- import_type_average(import)
  local <- import_type_average(local)
  
  grouped_food_df <- do.call("rbind", list(not_import, import, local))
  grouped_food_df
}


df_country_food <- function(country_name, foodName){
  df <- filter(food, country==country_name, food_name==foodName)
  
  if(dim(df)[1] < 10){
    return("No data for this country/food")
  }else{
    df$month <- sapply(df$month, convert_month)
    df$date <- as.Date(paste0(df$year,'-',df$month,'-01'))
    df$monthly_inflation <- 100*(df$price/lag(df$price) - 1)
  }
}

food_group_by <- function(food_to_group){
  grouping <- data.frame(matrix(ncol = 20, nrow = 0))
  df_single_country_food <- function(country_name, foodName){
    df <- filter(food, country==country_name, food_name==foodName)
    if(dim(df)[1] < 10){
      
    }else{
      df$month <- sapply(df$month, convert_month)
      df$date <- as.Date(paste0(df$year,'-',df$month,'-01'))
      df$monthly_inflation <- 100*(df$price/lag(df$price) - 1)
      df
    }
  }
  
  for (i in unique(food$country)){
    df <- df_single_country_food(i, food_to_group)
    grouping <- rbind(df, grouping)
  }
  grouping
}


food_group_by <- function(food_to_group){
  df_single_country_food <- function(country_name, foodGroup){
    df <- filter(food, country==country_name, food_group==foodGroup)
    if(dim(df)[1] < 10){
      
    }else{
      df$month <- sapply(df$month, convert_month)
      df$date <- as.Date(paste0(df$year,'-',df$month,'-01'))
      df$monthly_inflation <- 100*(df$price/lag(df$price) - 1)
      df
    }
  }
  
  grouping <- data.frame(matrix(ncol = 20, nrow = 0))
  for (i in unique(food$country)){
    df <- df_single_country_food(i, food_to_group)
    grouping <- rbind(df, grouping)
  }
  grouping
}






show_no_nat_avg <- function(grouped_food_df){
  no_nat_avg <- filter(grouped_food_df, market!="National Average")
  no_nat_avg <- as.data.frame(table(no_nat_avg$country))
  no_nat_avg[no_nat_avg$Freq != 0, ]
}

show_no_stdv <- function(grouped_food_df){
  no_nat_avg <- filter(grouped_food_df, is.na(stdv))
  no_nat_avg <- filter(no_nat_avg, is.na(monthly_inflation))
  no_nat_avg <- as.data.frame(table(no_nat_avg$country))
  no_nat_avg[no_nat_avg$Freq != 0, ]
}

# Plotting functions
plot_price <- function(country_name, foodName){
  df <- df_country_food(country_name, foodName)
  if(is.character(df)){
    return('No data to plot')
  }else{
    print({
      ggplot(df, aes(x=date, y=price_per_one_unit)) +
        geom_line() + 
        scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
        ggtitle(paste0(country_name,', ', foodName))
    })
  }
}

plot_group_price <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_group_inflation_by_country <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=monthly_inflation, col=country)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}



plot_region_price <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=region)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_region_inflation <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=monthly_inflation, col=region)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_seller_type_price <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=seller_type)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
}

plot_seller_type_inflation <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=monthly_inflation, col=seller_type)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
}

plot_group_inflation <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=monthly_inflation)) +
    geom_line(alpha = 0.5) + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    geom_smooth()
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_group_inflation_hist <- function(food_group_df){
  ggplot(food_group_df, aes(x=monthly_inflation)) +
    geom_histogram(bins = 25, color='black', fill='white') 
}

plot_region_price_facet <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=region)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ region) +
    theme(legend.position="none") +
     scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
}

plot_region_price_facet_A <- function(food_group_df){
  food_group_df <- filter(food_group_df, region!="Middle East & North Africa")
  food_group_df <- filter(food_group_df, region!="Sub-Saharan Africa")
  food_group_df <- filter(food_group_df, region!="Latin America & Caribbean")
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=region)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ region) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
}

plot_region_price_facet_B <- function(food_group_df){
  food_group_df <- filter(food_group_df, region!="East Asia & Pacific")
  food_group_df <- filter(food_group_df, region!="South Asia")
  food_group_df <- filter(food_group_df, region!="Europe & Central Asia")
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=region)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ region) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
}

plot_import_countries_price <- function(countries_and_imports_df){
  ggplot(countries_and_imports_df, aes(x=date, y=price_per_one_unit, col=import)) +
    geom_line(alpha = 0.5) + 
    facet_grid(.~country) +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
}

plot_import_countries_inflation <- function(countries_and_imports_df){
  plot <-
    ggplot(countries_and_imports_df, aes(x=country, y= monthly_inflation, col=import)) +
    geom_boxplot() 
  # facet_grid(.~country)
  plot
  plot + coord_flip()
}
plot_import_countries_inflation(rice_countries_and_imports)

plot_group_seller_type_by_price <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=seller_type)) +
    geom_line() + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_inflation <- function(country_name, foodName){
  df <- df_country_food(country_name, foodName)
  if(is.character(df)){
    return('No data to plot')
  }else{
    print({
      ggplot(df, aes(x=date, y=monthly_inflation)) +
        geom_line() + 
        scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
        ggtitle(paste0(country_name,', ', foodName, ', Monthly inflation'))
    })
  }
}

plot_inflation_hist <- function(country_name, foodName){
  df <- df_country_food(country_name, foodName)
  if(is.character(df)){
    return('No data to plot')
  }else{
    print({
      ggplot(df, aes(x=monthly_inflation)) +
        geom_histogram(bins = 25, color='black', fill='white') + 
        ggtitle(paste0(country_name,', ', foodName, ', Histogram of Monthly inflation'))
    })
  }
}

plot_group_price_facet <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ country) +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
}

plot_import_type_price <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=price_per_one_unit, col=import)) +
    geom_line() + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_import_type_inflation <- function(food_group_df){
  ggplot(food_group_df, aes(x=date, y=monthly_inflation, col=import)) +
    geom_line() + 
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") 
  # ggtitle(paste0(food_group_df,'World Wide Price'))
}

plot_country_box <- function(food_group_df){
  plot <-
    ggplot(food_group_df, aes(x = country, y = monthly_inflation, col=region)) +
    geom_boxplot() 
  plot + coord_flip()
}

plot_world_box <- function(food_group_df){
  plot <-
    ggplot(food_group_df, aes(x = country, y = monthly_inflation)) +
    geom_boxplot()
  plot + coord_flip()
}

plot_region_box <- function(food_group_df){
  plot <-
    ggplot(food_group_df, aes(x = region, y = monthly_inflation)) +
    geom_boxplot()
  plot + coord_flip()
}

plot_all_box <- function(food_group_df){
  plot <-
    ggplot(food_group_df, aes(x = food_group, y = monthly_inflation)) +
    geom_boxplot()
  plot + coord_flip()
} 

facet_price_county_by_East_Asia_Pacific <- function(grouped_df){
  East_Asia_Pacific <- filter(grouped_df, region=="East Asia & Pacific")
  ggplot(East_Asia_Pacific, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('East Asia & Pacific')
}

facet_price_county_by_Latin_America_Caribbean <- function(grouped_df){
  facet_county_by_Latin_America_Caribbean <- filter(grouped_df, region=="Latin America & Caribbean")
  ggplot(facet_county_by_Latin_America_Caribbean, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('Latin America & Caribbean')
}

facet_price_county_by_South_Asia <- function(grouped_df){
  South_Asia <- filter(grouped_df, region=="South Asia")
  ggplot(South_Asia, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('South Asia')
}

facet_price_county_by_Europe_Central_Asia <- function(grouped_df){
  Europe_Central_Asia <- filter(grouped_df, region=="Europe & Central Asia")
  ggplot(Europe_Central_Asia, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('Europe & Central Asia')
}

facet_price_county_by_Middle_East_North_Africa <- function(grouped_df){
  Middle_East_North_Africa <- filter(grouped_df, region=="Middle East & North Africa")
  ggplot(Middle_East_North_Africa, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('Middle East & North Africa')
}

facet_price_county_by_Sub_Saharan_Africa <- function(grouped_df){
  Sub_Saharan_Africa <- filter(grouped_df, region=="Sub-Saharan Africa")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Liberia")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Nigeria")
  ggplot(Sub_Saharan_Africa, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('Sub-Saharan Africa')
}

facet_price_county_by_Sub_Saharan_Africa_lib_nig <- function(grouped_df){
  lib <- filter(grouped_df, country=="Liberia")
  nig <- filter(grouped_df, country=="Nigeria")
  df <- rbind(lib, nig)
  ggplot(df, aes(x=date, y=price_per_one_unit, col=country)) +
    geom_line() +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "2 year", date_labels = "%m-%Y") +
    ggtitle('Sub-Saharan Africa Liberia and Nigeria')
}

facet_inflation_county_by_East_Asia_Pacific <- function(grouped_df){
  East_Asia_Pacific <- filter(grouped_df, region=="East Asia & Pacific")
  ggplot(East_Asia_Pacific, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('East Asia & Pacific')
}

facet_inflation_county_by_Latin_America_Caribbean <- function(grouped_df){
  facet_county_by_Latin_America_Caribbean <- filter(grouped_df, region=="Latin America & Caribbean")
  ggplot(facet_county_by_Latin_America_Caribbean, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Latin America & Caribbean')
}

facet_inflation_county_by_South_Asia <- function(grouped_df){
  South_Asia <- filter(grouped_df, region=="South Asia")
  ggplot(South_Asia, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('South Asia')
}

facet_inflation_county_by_Europe_Central_Asia <- function(grouped_df){
  Europe_Central_Asia <- filter(grouped_df, region=="Europe & Central Asia")
  ggplot(Europe_Central_Asia, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Europe & Central Asia')
}

facet_inflation_county_by_Middle_East_North_Africa <- function(grouped_df){
  Middle_East_North_Africa <- filter(grouped_df, region=="Middle East & North Africa")
  ggplot(Middle_East_North_Africa, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Middle East & North Africa')
}

facet_inflation_county_by_Sub_Saharan_Africa <- function(grouped_df){
  Sub_Saharan_Africa <- filter(grouped_df, region=="Sub-Saharan Africa")
  ggplot(Sub_Saharan_Africa, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Sub-Saharan Africa')
}

facet_inflation_county_by_Sub_Saharan_Africa_B <- function(grouped_df){
  Sub_Saharan_Africa <- filter(grouped_df, region=="Sub-Saharan Africa")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Nigeria")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Senegal")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Benin")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Cote d'Ivoire")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="United Republic of Tanzania")
  
  ggplot(Sub_Saharan_Africa, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Sub-Saharan Africa')
  
}       

facet_inflation_county_by_Sub_Saharan_Africa_A <- function(grouped_df){
  Sub_Saharan_Africa <- filter(grouped_df, region=="Sub-Saharan Africa")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Ghana")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Liberia")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Somalia")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Guinea-Bissau")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Cote d'Ivori")
  Sub_Saharan_Africa <- filter(Sub_Saharan_Africa, country!="Swaziland")
  ggplot(Sub_Saharan_Africa, aes(x=monthly_inflation, col=country)) +
    geom_histogram(bins = 25, color='black', fill='white') +
    facet_grid(. ~ country) +
    theme(legend.position="none") +
    ggtitle('Sub-Saharan Africa')
}


# IDENTIFY OUTLIERS-------------------
# Liberia extremely high rice/oil
# food <- food[food$country != "Liberia", ]
# South Sudan high flour/beans
# Nigeria high sorghum/beans/millet

# Cleaning for Rice Analysis---------------------------------
rice <- food_group_by("Rice")
# Remove outlier
rice_no_lib <- filter(rice, country!="Liberia")

# Find world average price for rice and plot price and inflation
rice_world_avg <- world_average(rice)
rice_world_avg_no_lib <- world_average(rice)

# Calulate price across regions
rice_avg_region <- grouping_by_region_average(rice)
rice_avg_region_no_lib <- grouping_by_region_average(rice_no_lib)

# Boxplot withouth outlers
# Filteres out outliers Armenia Rwanda Syrian Arab Republic Timor-Leste Bolivia Liberia
rice_box <- rice 
rice_box <- filter(rice_box, country!="Liberia")
rice_box <- filter(rice_box, country!="Armenia")
rice_box <- filter(rice_box, country!="Rwanda")
rice_box <- filter(rice_box, country!="Syrian Arab Republic")
rice_box <- filter(rice_box, country!="Timor-Leste")
rice_box <- filter(rice_box, country!="Bolivia")

# Finds average by seller
# rice_avg_seller <- seller_type_average(rice)
# View(rice_avg_seller)

# Import type
# table(rice$import)
rice_import <- import_type_grouping(rice)

# Group import type and find averege for local and import countries 
# identify countries with local entries
local <- rice[grepl("local", rice$food_name), ]
# View(as.data.frame(table(local$country)))

Mali <- national_average_fun(filter(local, country=="Mali"))
Guinea <- national_average_fun(filter(local, country=="Guinea"))
Chad <- national_average_fun(filter(local, country=="Chad"))

local <- do.call("rbind", list(Mali, Guinea, Chad))
local$import <- "Local"
rm(Mali, Guinea, Chad)

import <- rice[grepl("import", rice$food_name), ]

Mali <- national_average_fun(filter(import, country=="Mali"))
Guinea <- national_average_fun(filter(import, country=="Guinea"))
Chad <- national_average_fun(filter(import, country=="Chad"))

import <- do.call("rbind", list(Mali, Guinea, Chad))
import$import <- "Import"
rm(Mali, Guinea, Chad)

not_local <- rice[!grepl("local", rice$food_name), ]
not_import <- rice[!grepl("import", rice$food_name), ]

neither <- rbind(not_local, not_import)
rm(not_import, not_local)

Mali <- national_average_fun(filter(neither, country=="Mali"))
Guinea <- national_average_fun(filter(neither, country=="Guinea"))
Chad <- national_average_fun(filter(neither, country=="Chad"))

neither <- do.call("rbind", list(Mali, Guinea, Chad))
neither$import <- "Not Listed"
rm(Mali, Chad, Guinea)

rice_countries_and_imports <- do.call("rbind", list(neither, import, local))
rm(neither, import, local)



# Find countries without a nation average 
# plot_group_price(rice) #If average price is not calculated 
# show_no_nat_avg(rice)

# Calculate national average for these countries
Afghanistan <- national_average_fun(filter(rice, country=="Afghanistan"))
Algeria <- national_average_fun(filter(rice, country=="Algeria  "))
Armenia <- national_average_fun(filter(rice, country=="Armenia"))
Bangladesh <- national_average_fun(filter(rice, country=="Bangladesh "))
Benin <- national_average_fun(filter(rice, country=="Benin"))
Bolivia <- national_average_fun(filter(rice, country=="Bolivia"))
Burkina_Faso <- national_average_fun(filter(rice, country=="Burkina Faso "))
Cameroon <- national_average_fun(filter(rice, country=="Cameroon "))
Cape_Verde <- national_average_fun(filter(rice, country=="Cape Verde "))
Central_African_Republic <- national_average_fun(filter(rice, country=="Central African Republic "))
Chad <- national_average_fun(filter(rice, country=="Chad "))
Colombia <- national_average_fun(filter(rice, country=="Colombia "))
Cote_dIvoire <- national_average_fun(filter(rice, country=="Cote d'Ivoire"))
Democratic_Republic_Congo <- national_average_fun(filter(rice, country=="Democratic Republic of the Congo  "))
Djibouti <- national_average_fun(filter(rice, country=="Djibouti "))
El_Salvador <- national_average_fun(filter(rice, country=="El Salvador"))
Ethiopia <- national_average_fun(filter(rice, country=="Ethiopia "))
Ghana <- national_average_fun(filter(rice, country=="Ghana"))
Guinea <- national_average_fun(filter(rice, country=="Guinea "))
Guinea_Bissau <- national_average_fun(filter(rice, country=="Guinea-Bissau"))
Haiti <- national_average_fun(filter(rice, country=="Haiti"))
India <- national_average_fun(filter(rice, country=="India"))
Iran <- national_average_fun(filter(rice, country=="Iran  (Islamic Republic of)  "))
Iraq <- national_average_fun(filter(rice, country=="Iraq "))
Kyrgyzstan <- national_average_fun(filter(rice, country=="Kyrgyzstan "))
Liberia <- national_average_fun(filter(rice, country=="Liberia"))
Madagascar <- national_average_fun(filter(rice, country=="Madagascar  "))
Malawi <- national_average_fun(filter(rice, country=="Malawi  "))
Mali <- national_average_fun(filter(rice, country=="Mali  "))
Mauritania <- national_average_fun(filter(rice, country=="Mauritania  "))
Mozambique <- national_average_fun(filter(rice, country=="Mozambique  "))
Myanmar <- national_average_fun(filter(rice, country=="Myanmar"))
Nepal <- national_average_fun(filter(rice, country=="Nepal"))
Niger <- national_average_fun(filter(rice, country=="Niger"))
Nigeria <- national_average_fun(filter(rice, country=="Nigeria"))
Pakistan <- national_average_fun(filter(rice, country=="Pakistan "))
Peru <- national_average_fun(filter(rice, country=="Peru "))
Philippines <- national_average_fun(filter(rice, country=="Philippines"))
Rwanda <- national_average_fun(filter(rice, country=="Rwanda  "))
Senegal <- national_average_fun(filter(rice, country=="Senegal"))
Somalia <- national_average_fun(filter(rice, country=="Somalia"))
Sri_Lanka <- national_average_fun(filter(rice, country=="Sri Lanka"))
Syrian_Arab_Republic <- national_average_fun(filter(rice, country=="Syrian Arab Republic "))
Tajikistan <- national_average_fun(filter(rice, country=="Tajikistan "))
Timor_Leste <- national_average_fun(filter(rice, country=="Timor-Leste"))
Ukraine <- national_average_fun(filter(rice, country=="Ukraine"))
United_Republic_Tanzania <- national_average_fun(filter(rice, country=="United Republic of Tanzania"))
Yemen <- national_average_fun(filter(rice, country=="Yemen"))
Zambia <- national_average_fun(filter(rice, country=="Zambia  "))

# rbind the newly calculated national averages
# rice_list <- list(Afghanistan, Algeria, Armenia, Bangladesh, Benin, Bolivia, Burkina_Faso, Cameroon, Cape_Verde, Central_African_Republic, Chad, Colombia, Cote_dIvoire, Democratic_Republic_Congo, Djibouti, El_Salvador, Ethiopia, Ghana, Guinea, Guinea_Bissau, Haiti, India, Iran, Iraq, Kyrgyzstan, Liberia, Madagascar, Malawi, Mali, Mauritania, Mozambique, Myanmar, Nepal, Niger, Nigeria, Pakistan, Peru, Philippines, Rwanda, Senegal, Somalia, Sri_Lanka, Syrian_Arab_Republic, Tajikistan, Timor_Leste, Ukraine, United_Republic_Tanzania, Yemen, Zambia)
rice2 <- do.call("rbind", list(Afghanistan, Algeria, Armenia, Bangladesh, Benin, Bolivia, Burkina_Faso, Cameroon, Cape_Verde, Central_African_Republic, Chad, Colombia, Cote_dIvoire, Democratic_Republic_Congo, Djibouti, El_Salvador, Ethiopia, Ghana, Guinea, Guinea_Bissau, Haiti, India, Iran, Iraq, Kyrgyzstan, Liberia, Madagascar, Malawi, Mali, Mauritania, Mozambique, Myanmar, Nepal, Niger, Nigeria, Pakistan, Peru, Philippines, Rwanda, Senegal, Somalia, Sri_Lanka, Syrian_Arab_Republic, Tajikistan, Timor_Leste, Ukraine, United_Republic_Tanzania, Yemen, Zambia))
rm(Afghanistan, Algeria, Armenia, Bangladesh, Benin, Bolivia, Burkina_Faso, Cameroon, Cape_Verde, Central_African_Republic, Chad, Colombia, Cote_dIvoire, Democratic_Republic_Congo, Djibouti, El_Salvador, Ethiopia, Ghana, Guinea, Guinea_Bissau, Haiti, India, Iran, Iraq, Kyrgyzstan, Liberia, Madagascar, Malawi, Mali, Mauritania, Mozambique, Myanmar, Nepal, Niger, Nigeria, Pakistan, Peru, Philippines, Rwanda, Senegal, Somalia, Sri_Lanka, Syrian_Arab_Republic, Tajikistan, Timor_Leste, Ukraine, United_Republic_Tanzania, Yemen, Zambia)

# Filter out the countries without national averge
rice <- filter(rice, market=="National Average")

# Add stdv column 
rice$stdv <- NA

# rbind maize and maize2
rice <- rbind(rice, rice2)
rm(rice2)

# Show countries without standard dev
# show_no_stdv(rice)

# Cleaning for Maize Analysis---------------------------------
maize <- food_group_by("Maize")

# Find world average price for rice and plot price and inflation
maize_world_avg <- world_average(maize)

# Calulate price across regions NOT WORKING
maize_avg_region <- grouping_by_region_average(maize)

# Import type No import avail
# table(maize$import)
# maize_import <- import_type_grouping(maize)

# Find countries without a nation average 
# plot_group_price(rice) #If average price is not calculated
# show_no_nat_avg(maize)

# Calculate national average for these countries
Benin <- national_average_fun(filter(maize, country=="Benin"))
Burkina_Faso <- national_average_fun(filter(maize, country=="Burkina Faso"))
Burundi <- national_average_fun(filter(maize, country=="Burundi"))
Cameroon <- national_average_fun(filter(maize, country=="Cameroon"))
Central_African_Republic <- national_average_fun(filter(maize, country=="Central African Republic"))
Chad <- national_average_fun(filter(maize, country=="Chad"))
Colombia <- national_average_fun(filter(maize, country=="Colombia"))
Cote_dIvoire <- national_average_fun(filter(maize, country=="Cote d'Ivoire"))
Democratic_Republic_Congo <- national_average_fun(filter(maize, country=="Democratic Republic of the Congo"))
El_Salvador <- national_average_fun(filter(maize, country=="El Salvador"))
Ethiopia <- national_average_fun(filter(maize, country=="Ethiopia"))
Gambia <- national_average_fun(filter(maize, country=="Gambia"))
Ghana <- national_average_fun(filter(maize, country=="Ghana"))
Guatemala <- national_average_fun(filter(maize, country=="Guatemala"))
Guinea_Bissau <- national_average_fun(filter(maize, country=="Guinea-Bissau"))
Haiti <- national_average_fun(filter(maize, country=="Haiti"))
Kenya <- national_average_fun(filter(maize, country=="Kenya"))
Kyrgyzstan <- national_average_fun(filter(maize, country=="Kyrgyzstan"))
Lesotho <- national_average_fun(filter(maize, country=="Lesotho"))
Malawi <- national_average_fun(filter(maize, country=="Malawi"))
Mali <- national_average_fun(filter(maize, country=="Mali"))
Mozambique <- national_average_fun(filter(maize, country=="Mozambique"))
Myanmar <- national_average_fun(filter(maize, country=="Myanmar"))
Niger <- national_average_fun(filter(maize, country=="Niger"))
Nigeria <- national_average_fun(filter(maize, country=="Nigeria"))
Peru <- national_average_fun(filter(maize, country=="Peru"))
Rwanda <- national_average_fun(filter(maize, country=="Rwanda"))
Senegal <- national_average_fun(filter(maize, country=="Senegal"))
Somalia <- national_average_fun(filter(maize, country=="Somalia"))
South_Sudan <- national_average_fun(filter(maize, country=="South Sudan"))
Tajikistan <- national_average_fun(filter(maize, country=="Tajikistan"))
Timor_Leste <- national_average_fun(filter(maize, country=="Timor-Leste"))
Uganda <- national_average_fun(filter(maize, country=="Uganda"))
United_Republic_Tanzania <- national_average_fun(filter(maize, country=="United Republic of Tanzania"))
Zambia <- national_average_fun(filter(maize, country=="Zambia"))
Zimbabwe <- national_average_fun(filter(maize, country=="Zimbabwe"))

# rbind the newly calculated national averages
maize2 <- do.call("rbind", list(Benin, Burkina_Faso, Burundi, Cameroon, Central_African_Republic, Chad, Colombia, Cote_dIvoire, Democratic_Republic_Congo, El_Salvador, Ethiopia, Gambia, Ghana, Guatemala, Guinea_Bissau, Haiti, Kenya, Kyrgyzstan, Lesotho, Malawi, Mali, Mozambique, Myanmar, Niger, Nigeria, Peru, Rwanda, Senegal, Somalia, South_Sudan, Tajikistan, Timor_Leste, Uganda, United_Republic_Tanzania, Zambia, Zimbabwe))
rm(Benin, Burkina_Faso, Burundi, Cameroon, Central_African_Republic, Chad, Colombia, Cote_dIvoire, Democratic_Republic_Congo, El_Salvador, Ethiopia, Gambia, Ghana, Guatemala, Guinea_Bissau, Haiti, Kenya, Kyrgyzstan, Lesotho, Malawi, Mali, Mozambique, Myanmar, Niger, Nigeria, Peru, Rwanda, Senegal, Somalia, South_Sudan, Tajikistan, Timor_Leste, Uganda, United_Republic_Tanzania, Zambia, Zimbabwe)

# Filter out the countries without national averge
maize <- filter(maize, market=="National Average")

# Add stdv column 
maize$stdv <- NA

# rbind maize and maize2
maize <- rbind(maize, maize2)
rm(maize2)

# Show countries without standard dev
# show_no_stdv(maize)


# Cleaning for Sorghum Analysis---------------------------------
sorghum <- food_group_by("Sorghum")

# Find world average price for rice and plot price and inflation
sorghum_world_avg <- world_average(sorghum)

# Calulate price across regions
sorghum_avg_region <- grouping_by_region_average(sorghum)

# No import types avail
# table(sorghum$import)
# sorghum_import <- import_type_grouping(sorghum)

# Find countries without a nation average 
# show_no_nat_avg(sorghum)

# Calculate national average for these countries
Benin <- national_average_fun(filter(sorghum, country=="Benin"))
Burkina_Faso <- national_average_fun(filter(sorghum, country=="Burkina Faso"))
Cameroon <- national_average_fun(filter(sorghum, country=="Cameroon"))
Chad <- national_average_fun(filter(sorghum, country=="Chad"))
Djibouti <- national_average_fun(filter(sorghum, country=="Djibouti"))
Ethiopia <- national_average_fun(filter(sorghum, country=="Ethiopia"))
Gambia <- national_average_fun(filter(sorghum, country=="Gambia"))
Guinea_Bissau <- national_average_fun(filter(sorghum, country=="Guinea-Bissau"))
Kenya <- national_average_fun(filter(sorghum, country=="Kenya"))
Mali <- national_average_fun(filter(sorghum, country=="Mali"))
Mauritania <- national_average_fun(filter(sorghum, country=="Mauritania"))
Niger <- national_average_fun(filter(sorghum, country=="Niger"))
Nigeria <- national_average_fun(filter(sorghum, country=="Nigeria"))
Rwanda <- national_average_fun(filter(sorghum, country=="Rwanda"))
Senegal <- national_average_fun(filter(sorghum, country=="Senegal"))
Somalia <- national_average_fun(filter(sorghum, country=="Somalia"))
South_Sudan <- national_average_fun(filter(sorghum, country=="South Sudan"))
Sudan <- national_average_fun(filter(sorghum, country=="Sudan"))
Uganda <- national_average_fun(filter(sorghum, country=="Uganda"))
Zambia <- national_average_fun(filter(sorghum, country=="Zambia"))
Zimbabwe <- national_average_fun(filter(sorghum, country=="Zimbabwe"))

# rbind the newly calculated national averages
sorghum2 <- do.call("rbind", list(Benin, Burkina_Faso , Cameroon, Chad, Djibouti, Ethiopia , Gambia , Guinea_Bissau , Kenya, Mali , Mauritania, Niger , Nigeria, Rwanda , Senegal , Somalia, South_Sudan, Sudan , Uganda, Zambia, Zimbabwe ))
rm(Benin, Burkina_Faso , Cameroon, Chad, Djibouti, Ethiopia , Gambia , Guinea_Bissau , Kenya, Mali , Mauritania, Niger , Nigeria, Rwanda , Senegal , Somalia, South_Sudan, Sudan , Uganda, Zambia, Zimbabwe )

# Filter out the countries without national averge
sorghum <- filter(sorghum, market=="National Average")

# Add stdv column 
sorghum$stdv <- NA

# rbind sorghum and sorghum2
sorghum <- rbind(sorghum, sorghum2)
rm(sorghum2)

# Show countries without standard dev
# show_no_stdv(sorghum)


# Cleaning for Beans Analysis---------------------------------
beans <- food_group_by("Beans")

# Find world average price for rice and plot price and inflation
beans_world_avg <- world_average(beans)

# Calulate price across regions
beans_avg_region <- grouping_by_region_average(beans)

# Import typer
# table(beans$import)
beans_import <- import_type_grouping(beans)

# Find countries without a nation average 
# show_no_nat_avg(beans)

# Calculate national average for these countries
Algeria <- national_average_fun(filter(beans, country=="Algeria"))
Benin <- national_average_fun(filter(beans, country=="Benin"))
Burkina_Faso <- national_average_fun(filter(beans, country=="Burkina Faso"))
Burundi <- national_average_fun(filter(beans, country=="Burundi"))
Cameroon <- national_average_fun(filter(beans, country=="Cameroon"))
Colombia <- national_average_fun(filter(beans, country=="Colombia"))
Congo <- national_average_fun(filter(beans, country=="Congo"))
Democratic_Republic_Congo <- national_average_fun(filter(beans, country=="Democratic Republic of the Congo"))
Djibouti <- national_average_fun(filter(beans, country=="Djibouti"))
El_Salvador <- national_average_fun(filter(beans, country=="El Salvador"))
Gambia <- national_average_fun(filter(beans, country=="Gambia"))
Guatemala <- national_average_fun(filter(beans, country=="Guatemala"))
Guinea <- national_average_fun(filter(beans, country=="Guinea"))
Haiti <- national_average_fun(filter(beans, country=="Haiti"))
Kenya <- national_average_fun(filter(beans, country=="Kenya"))
Kyrgyzstan <- national_average_fun(filter(beans, country=="Kyrgyzstan"))
Lebanon <- national_average_fun(filter(beans, country=="Lebanon"))
Lesotho <- national_average_fun(filter(beans, country=="Lesotho"))
Malawi <- national_average_fun(filter(beans, country=="Malawi"))
Mali <- national_average_fun(filter(beans, country=="Mali"))
Mozambique <- national_average_fun(filter(beans, country=="Mozambique"))
Niger <- national_average_fun(filter(beans, country=="Niger"))
Nigeria <- national_average_fun(filter(beans, country=="Nigeria"))
Pakistan <- national_average_fun(filter(beans, country=="Pakistan"))
Philippines <- national_average_fun(filter(beans, country=="Philippines"))
Rwanda <- national_average_fun(filter(beans, country=="Rwanda"))
South_Sudan <- national_average_fun(filter(beans, country=="South Sudan"))
Syrian_Arab_Republic <- national_average_fun(filter(beans, country=="Syrian Arab Republic"))
Timor_Leste <- national_average_fun(filter(beans, country=="Timor-Leste"))
Uganda <- national_average_fun(filter(beans, country=="Uganda"))
United_Republic_Tanzania <- national_average_fun(filter(beans, country=="United Republic of Tanzania"))
Yemen <- national_average_fun(filter(beans, country=="Yemen"))
Zambia <- national_average_fun(filter(beans, country=="Zambia"))
Zimbabwe <- national_average_fun(filter(beans, country=="Zimbabwe"))

# rbind the newly calculated national averages
beans2 <- do.call("rbind", list(Algeria, Benin, Burkina_Faso, Burundi, Cameroon, Colombia, Congo, Democratic_Republic_Congo, Djibouti, El_Salvador, Gambia, Guatemala, Guinea, Haiti, Kenya, Kyrgyzstan, Lebanon, Lesotho, Malawi, Mali, Mozambique, Niger, Nigeria, Pakistan, Philippines, Rwanda, South_Sudan, Syrian_Arab_Republic, Timor_Leste, Uganda, United_Republic_Tanzania, Yemen, Zambia, Zimbabwe))
rm(Algeria, Benin, Burkina_Faso, Burundi, Cameroon, Colombia, Congo, Democratic_Republic_Congo, Djibouti, El_Salvador, Gambia, Guatemala, Guinea, Haiti, Kenya, Kyrgyzstan, Lebanon, Lesotho, Malawi, Mali, Mozambique, Niger, Nigeria, Pakistan, Philippines, Rwanda, South_Sudan, Syrian_Arab_Republic, Timor_Leste, Uganda, United_Republic_Tanzania, Yemen, Zambia, Zimbabwe)

# Filter out the countries without national averge
beans <- filter(beans, market=="National Average")


# Add stdv column 
beans$stdv <- NA

# rbind beans and beans2
beans <- rbind(beans, beans2)
rm(beans2)

# Show countries without standard dev aslong as they are 1 its ok
# show_no_stdv(beans)


# Cleaning for Millet Analysis---------------------------------
millet <- food_group_by("Millet")

# Find world average price for rice and plot price and inflation
millet_world_avg <- world_average(millet)

# Calulate price across regions
millet_avg_region <- grouping_by_region_average(millet)

# Import typer not avail
# table(millet$import)
# millet_import <- import_type_grouping(millet)

# Find countries without a nation average 
# show_no_nat_avg(millet)

# Calculate national average for these countries
Benin <- national_average_fun(filter(millet, country=="Benin"))  
Burkina_Faso <- national_average_fun(filter(millet, country=="Burkina Faso"))
Central_African_Republic  <- national_average_fun(filter(millet, country=="Central African Republic"))
Chad <- national_average_fun(filter(millet, country=="Chad")) 
Djibouti <- national_average_fun(filter(millet, country=="Djibouti")) 
Gambia <- national_average_fun(filter(millet, country=="Gambia"))
Guinea_Bissau <- national_average_fun(filter(millet, country=="Guinea-Bissau"))  
Mali <- national_average_fun(filter(millet, country=="Mali"))
Niger <- national_average_fun(filter(millet, country=="Niger"))
Nigeria <- national_average_fun(filter(millet, country=="Nigeria")) 
Sudan <- national_average_fun(filter(millet, country=="Sudan"))
Uganda <- national_average_fun(filter(millet, country=="Uganda")) 
Zambia <- national_average_fun(filter(millet, country=="Zambia")) 
Zimbabwe <- national_average_fun(filter(millet, country=="Zimbabwe"))


# # rbind the newly calculated national averages
millet2 <- do.call("rbind", list(Benin, Burkina_Faso, Central_African_Republic, Chad, Djibouti, Gambia, Guinea_Bissau, Mali, Niger, Nigeria, Sudan, Uganda, Zambia, Zimbabwe))
rm(Benin, Burkina_Faso, Central_African_Republic, Chad, Djibouti, Gambia, Guinea_Bissau, Mali, Niger, Nigeria, Sudan, Uganda, Zambia, Zimbabwe)

# Filter out the countries without national averge
millet <- filter(millet, market=="National Average")

# Add stdv column 
millet$stdv <- NA

# rbind millet and millet2
millet <- rbind(millet, millet2)
rm(millet2)

# Show countries without standard dev
# show_no_stdv(millet)


# Cleaning for Oil Analysis---------------------------------
oil <- food_group_by("Oil")

# Find world average price for rice and plot price and inflation
oil_world_avg <- world_average(oil)

# Calulate price across regions
oil_avg_region <- grouping_by_region_average(oil)

# Import typer not avail
# table(oil$import)
# oil_import <- import_type_grouping(oil)


# Find countries without a nation average 
# show_no_nat_avg(oil)

# Calculate national average for these countries
Algeria <- national_average_fun(filter(oil, country=="Algeria"))
Armenia <- national_average_fun(filter(oil, country=="Armenia"))
Bangladesh  <- national_average_fun(filter(oil, country=="Bangladesh"))
Cambodia  <- national_average_fun(filter(oil, country=="Cambodia"))
Central_African_Republic  <- national_average_fun(filter(oil, country=="Central African Republic"))
Colombia  <- national_average_fun(filter(oil, country=="Colombia"))
Congo <- national_average_fun(filter(oil, country=="Congo"))
Cote_dIvoire  <- national_average_fun(filter(oil, country=="Cote d'Ivoire"))
Democratic_Republic_Congo <- national_average_fun(filter(oil, country=="Democratic Republic of the Congo"))
Djibouti  <- national_average_fun(filter(oil, country=="Djibouti"))
Gambia  <- national_average_fun(filter(oil, country=="Gambia"))
Guinea  <- national_average_fun(filter(oil, country=="Guinea"))
Guinea_Bissau <- national_average_fun(filter(oil, country=="Guinea-Bissau"))
India <- national_average_fun(filter(oil, country=="India"))
Iran  <- national_average_fun(filter(oil, country=="Iran(Islamic Republic of)"))
Iraq  <- national_average_fun(filter(oil, country=="Iraq"))
Kenya <- national_average_fun(filter(oil, country=="Kenya"))
Kyrgyzstan  <- national_average_fun(filter(oil, country=="Kyrgyzstan"))
Lao_Peoples_Democratic_Republic <- national_average_fun(filter(oil, country=="Lao People's Democratic Republic"))
Lebanon <- national_average_fun(filter(oil, country=="Lebanon"))
Lesotho <- national_average_fun(filter(oil, country=="Lesotho"))
Liberia <- national_average_fun(filter(oil, country=="Liberia"))
Madagascar  <- national_average_fun(filter(oil, country=="Madagascar"))
Mauritania  <- national_average_fun(filter(oil, country=="Mauritania"))
Myanmar <- national_average_fun(filter(oil, country=="Myanmar"))
Pakistan  <- national_average_fun(filter(oil, country=="Pakistan"))
Peru  <- national_average_fun(filter(oil, country=="Peru"))
South_Sudan <- national_average_fun(filter(oil, country=="South Sudan"))
State_Palestine <- national_average_fun(filter(oil, country=="State of Palestine"))
Syrian_Arab_Republic  <- national_average_fun(filter(oil, country=="Syrian Arab Republic"))
Tajikistan  <- national_average_fun(filter(oil, country=="Tajikistan"))
Ukraine <- national_average_fun(filter(oil, country=="Ukraine"))
Yemen <- national_average_fun(filter(oil, country=="Yemen"))
Zimbabwe  <- national_average_fun(filter(oil, country=="Zimbabwe"))


# rbind the newly calculated national averages
oil2 <- do.call("rbind", list(Algeria, Armenia, Bangladesh, Cambodia, Central_African_Republic, Colombia, Congo, Cote_dIvoire, Democratic_Republic_Congo, Djibouti, Gambia, Guinea, Guinea_Bissau, India, Iran, Iraq, Kenya, Kyrgyzstan, Lao_Peoples_Democratic_Republic, Lebanon, Lesotho, Liberia, Madagascar, Mauritania, Myanmar, Pakistan, Peru, South_Sudan, State_Palestine, Syrian_Arab_Republic, Tajikistan, Ukraine, Yemen, Zimbabwe))
rm(Algeria, Armenia, Bangladesh, Cambodia, Central_African_Republic, Colombia, Congo, Cote_dIvoire, Democratic_Republic_Congo, Djibouti, Gambia, Guinea, Guinea_Bissau, India, Iran, Iraq, Kenya, Kyrgyzstan, Lao_Peoples_Democratic_Republic, Lebanon, Lesotho, Liberia, Madagascar, Mauritania, Myanmar, Pakistan, Peru, South_Sudan, State_Palestine, Syrian_Arab_Republic, Tajikistan, Ukraine, Yemen, Zimbabwe)

# Filter out the countries without national averge
oil <- filter(oil, market=="National Average")

# Add stdv column 
oil$stdv <- NA

# rbind oil and oil2
oil <- rbind(oil, oil2)
rm(oil2)

# Show countries without standard dev
# show_no_stdv(oil)

# Top 6
# top_6_world <- do.call("rbind", list(rice_world_avg, maize_world_avg, sorghum_world_avg, beans_world_avg, sorghum_world_avg, millet_world_avg, oil_world_avg))
# top_6_region <- do.call("rbind", list(rice_avg_region, maize_avg_region, sorghum_avg_region, beans_avg_region, sorghum_avg_region, millet_avg_region, oil_avg_region))
# top_6 <- do.call("rbind", list(rice, maize, sorghum, beans, millet, oil))
# write.csv(top_6, file = "data/clean/top_6.csv", row.names=FALSE)






maize
Haiti
Peru
Gambia
