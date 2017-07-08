# TO DO 
# Create natioan average function to apply to everything in food
food <- read.csv("data/clean/food_data.csv")

# QUESTIONS TO ASK
# 1 What is the viriation in the price across the globe 
# 2 What is times of year is food the most expensive in regions/countries
# -if all go up in certain time of year 
# -st dev  
# -calc percentage change in each moneht 
# 3 Is a low income country really have cheaper food
# 4 Is food cheapest at farm/producer, wholesale or retail 
# 5 Inflation rate with unified currenct
# 6 Compare inflation rate of sam food across different countries
# -calc rate of change to see inflation
# -price of current / price of previous month 

#------------------RICE ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(rice_world_avg)
plot_group_price(rice_world_avg_no_lib)

plot_group_inflation(rice_world_avg)
plot_group_inflation_hist(rice_world_avg)

# Calulate price across regions
plot_region_price(rice_avg_region)
plot_region_inflation(rice_avg_region)
plot_region_price_facet(rice_avg_region)

plot_region_price(rice_avg_region_no_lib)
plot_region_inflation(rice_avg_region_no_lib)
plot_region_price_facet(rice_avg_region_no_lib)

# Plot Import and not import 
plot_import_type_price(rice_import)
plot_import_type_inflation(rice_import)

# Plot seller typer
plot_seller_type_price(rice)
plot_seller_type_inflation(rice)

# Plot price across countries
plot_group_price(rice)
plot_group_price_facet(rice)
plot_group_inflation(rice)
plot_group_inflation_by_country(rice)

# # box plot infaltion
plot_region_box(rice_avg_region)
plot_world_box(rice_world_avg)
View(rice)

#------------------Maize ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(maize_world_avg)
plot_group_inflation(maize_world_avg)

# Calulate price across regions
plot_region_price(maize_avg_region)
plot_region_inflation(maize_avg_region)
plot_region_price_facet(maize_avg_region)

# Import not avail

# Plot price across countries
plot_group_price(maize)
plot_group_price_facet(maize)
plot_group_inflation(maize)
plot_group_inflation_by_country(maize)

# # box plot infaltion
# plot_region_box(maize_avg_region)
# plot_all_box(maize)
# plot_world_box(maize_world_avg)


#------------------SORGHUM ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(sorghum_world_avg)
plot_group_inflation(sorghum_world_avg)

# Calulate price across regions
plot_region_price(sorghum_avg_region)
plot_region_inflation(sorghum_avg_region)
plot_region_price_facet(sorghum_avg_region)

# Import not avail 
plot_import_type_price(sorghum_import)
plot_import_type_inflation(sorghum_import)

# Plot price across countries
plot_group_price(sorghum)
plot_group_price(filter(sorghum, country!="Nigeria"))
plot_group_price_facet(sorghum)
plot_group_inflation(sorghum)
plot_group_inflation_by_country(sorghum)

#------------------BEANS ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(beans_world_avg)
plot_group_inflation(beans_world_avg)

# Calulate price across regions
plot_region_price(beans_avg_region)
plot_region_inflation(beans_avg_region)
plot_region_price_facet(beans_avg_region)

# Plot Import and not import 
plot_import_type_price(beans_import)
plot_import_type_inflation(beans_import)

# Plot price across countries
plot_group_price(beans)
plot_group_price(filter(beans, country!="Nigeria"))
plot_group_price_facet(beans)
plot_group_inflation(beans)
plot_group_inflation_by_country(beans)

#------------------MILLET ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(millet_world_avg)
plot_group_inflation(millet_world_avg)

# Calulate price across regions
plot_region_price(millet_avg_region)
plot_region_inflation(millet_avg_region)
plot_region_price_facet(millet_avg_region)

# Plot Import and not import 
plot_import_type_price(millet_import)
plot_import_type_inflation(millet_import)

# Plot price across countries
plot_group_price(millet)
plot_group_price_facet(millet)
plot_group_inflation(millet)
plot_group_inflation_by_country(millet)


#------------------OIL ANALYSIS--------------------------------
# Find world average price for rice and plot price and inflation
plot_group_price(oil_world_avg)
plot_group_inflation(oil_world_avg)

# Calulate price across regions
plot_region_price(oil_avg_region)
plot_region_inflation(oil_avg_region)
plot_region_price_facet(oil_avg_region)

# Import not avail 

# Plot price across countries
plot_group_price(oil)
plot_group_price_facet(oil)
plot_group_inflation(oil)
plot_group_inflation_by_country(oil)


