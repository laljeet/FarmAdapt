library(CropScapeR)
library(tidyverse)

library(tmap)
library(tmaptools)
library(FarmAdapt)

options(scipen = 9999999)

rm(list = ls(all.names = TRUE))

setwd("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/CDL")

loc <- c("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/CDL")
#*******************************************************************************************#
#*Create one FIPS file with 4 and 5 digit fips code                                        #
#******************************************************************************************#
#*
#*This is saved as FIPS data and can be called in future directly from the package ########
#*

FIPS <- read.csv("https://raw.githubusercontent.com/laljeet/MY_files/main/FIPS2.csv")
FIPS$fips <- sprintf("%05d", FIPS$fips)
FIPS$fips <- as.character(FIPS$fips)
FIPS <- FIPS[,-c(1)]
FIPS$fips1 <- as.character(as.numeric(FIPS$fips))
usethis::use_data(FIPS, overwrite = FALSE)

################################################################################################
load("./data/FIPS.rda")
keep.states <- c("AZ","CA", "CO", "NV","UT","NM")

fips_csv_AL <- FIPS %>%
     dplyr::filter(state %in% keep.states)

data_list <- list()
Year = 2017

library(progress)

# Create a progress bar object
pb <- progress_bar$new(
  format = "[:bar] :percent :elapsedfull",
  total = nrow(fips_csv_AL),
  clear = FALSE,
  width = 60
)

# Loop with progress bar
for (i in 1:nrow(fips_csv_AL)) {
  data_list[[i]] <- CropScapeR::GetCDLComp(aoi = fips_csv_AL$fips[i], year1 = Year, year2 = Year, type = 'f')

  # Update the progress bar
  pb$tick()
}


# Convert list to dataframe
AL_counties <- purrr::map_df(data_list, tibble::as_tibble)
AL_counties <- AL_counties[,-1]
usethis::use_data(AL_counties, overwrite = FALSE)


####################################################################################################
# Look up table for all land uses


Lookup_table <- read_csv("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/CDL/Lookup_table.csv")
usethis::use_data(Lookup_table, overwrite = FALSE)

load("./data/Lookup_table.rda")
Remove_LC_codes <- c(81:143,61, 63,64,65,152, 171,190,195) # 61 is fallow

Remove_layers <-subset(Lookup_table, !(LC_code %in% Remove_LC_codes))


# Remove the landuses from crop dataset
load("./data/AL_counties.rda")
AL_county_crops <- subset(AL_counties, (To %in% Remove_layers$LC_type))

usethis::use_data(AL_county_crops, overwrite = FALSE)

#write.csv(AL_county_crops, paste0(keep.states,"AL_county_crops",Year,".csv"))

#######################################################################################
# Crop with Max acreage in each county
#######################################################################################

crops <- AL_county_crops
unique(crops$To)
by_aoi <- split(crops, crops$aoi)

# Apply to each aoi
max_rows <- lapply(by_aoi, get_max_row)

# Bind rows into single dataframe
Max_crop_county <- do.call(rbind, max_rows)


usethis::use_data(Max_crop_county, overwrite = FALSE)


#######################################################################################
# Crops that cover 90% of acreage per county
#######################################################################################

# Apply to each aoi
top_rows_list <- lapply(by_aoi, fn_get_top_crops_acerage)

# Bind into single dataframe
Top_90pct_crop_county <- do.call(rbind, top_rows_list)

rownames(Top_90pct_crop_county) <- NULL

usethis::use_data(Top_90pct_crop_county, overwrite = FALSE)
# save(Top_90pct_crop_county,file="Top_90pct_crop_countyOct.Rdata")

########################################################################################################
# Plots
rm(list = ls(all.names = TRUE))
library(sf)
load("./data/Max_crop_county.rda")
load("./data/FIPS.rda")
load("./data/shape.rda")

FIPS$fips1 <- NULL

crop_data <- Max_crop_county
crop_data <- crop_data[,c(1,3,4)]

# Label columns
colnames(crop_data) <- c( "Crop", "Acres","fips")
crop_data$fips <- sprintf("%05d", crop_data$fips)


crop_data <- left_join(crop_data, FIPS , by = "fips")

# shape <- st_as_sf(shape)
# usethis::use_data(shape, overwrite = TRUE)



states.keep <- c("CALIFORNIA","Nevada" ,
                 "Utah"  ,
                 "Colorado",
                 "Arizona",
                 "New Mexico" )
states.keep2 <- toupper(states.keep)
subset_shape <- subset(shape, Name %in% states.keep2)

subset_shape<- left_join(subset_shape, crop_data[,-c(6)], by = c("GEOID"= "fips"))

tmap_mode("view")
tmap_mode("plot")

library(tmap)

map <- tm_shape(subset_shape) +
  tm_fill(col = "Acres",
          n = 5, style = "jenks",
          palette = "Blues",  # Set a color palette
          # na.color = "grey",  # Color for NA values
          # textNA = "No Data",
          id = "NAME") +
  tm_borders() +
  tm_text("NAME", size = 0.5)

  map<- map +
  tm_layout(main.title =  "Maximum acreage crop in each county")

map



# tmap_save(map, "Max_acreage.png",  width = 10, height = 8, units = 'in')


tmap_mode("plot")
map <- tm_shape(subset_shape) +
  tm_borders() +
  tm_fill("Crop",  palette = "Paired",
          # n=5, style="jenks",
          textNA = "No Data",
          # alpha = 0.8,
          id = "NAME")
# +
#   tm_text("Crop", size = 1)
map<- map +
  tm_layout( main.title =  "Maximum acreage crop in each county")

map
# tmap_save(map, "Max_acreage_Crop.png",  width = 10, height = 8, units = 'in')

# tmap_save(map, "Max_acreage_Crop.html")


##############################################################################################################
rm(list = ls(all.names = TRUE))


load("./data/Top_90pct_crop_county.rda")

## Top 90% crops gives the important and high acreage crops WITHIN each county
#3 Let's get crops between different counties that are common and result in high acreage.

crop_data <- Top_90pct_crop_county
crop_data <- crop_data[,c(1,3,4)]

# Label columns
colnames(crop_data) <- c( "Crop", "Acres","fips")

# Get all unique crops
all_crops <- unique(crop_data$Crop)

# Count occurrence of each crop
crop_counts <- crop_data %>%
  count(Crop)

# Arrange by decreasing n to get most repeated crops
top_crops <- crop_counts %>%
  mutate(Pct = round(n/sum(n),2)) %>%
  arrange(desc(n))


top_crops <- fn_get_top_crops_occurrence(top_crops,0.90)
top_crops$Crop <- gsub("/","_", top_crops$Crop)

top_crops <- top_crops$Crop
top_crops <- gsub("/","_", top_crops)


crop_data$Crop <- gsub("/", "_", crop_data$Crop)

crop_data <- crop_data %>%
  filter(Crop %in% top_crops)


unique(crop_data$Crop)





load("./data/FIPS.rda")
FIPS <- FIPS[,-c(5)]
crop_data <- left_join(crop_data, FIPS , by = "fips")
#######################################################################


load("./data/shape.rda")
states.keep <- c("Nevada" ,
                 "Utah"  ,
                 "Colorado",
                 "Arizona",
                 "New Mexico",
                 "California")
states.keep2 <- toupper(states.keep)


crop_data_list <- split(crop_data, f = crop_data$Crop)
crop_data_list <- lapply(crop_data_list, function(df) {df$fips <- sprintf("%05d", as.integer(df$fips)); return(df)})
############################################################################################

# Apply the function to each dataframe in the list
Unique_crops_states <- lapply(crop_data_list, fn_get_unique_values)

# Bind all dataframes into a single dataframe
Unique_crops_states <- do.call(rbind, Unique_crops_states)
rownames(Unique_crops_states) <- NULL

# usethis::use_data(Unique_crops_states, overwrite = FALSE)
# crop_data_final <- crop_data
# usethis::use_data(crop_data_final, overwrite = FALSE)
# usethis::use_data(crop_data_list, overwrite = FALSE)

# write.csv(Unique_crops_states, "Unique_crops_states.csv", row.names = FALSE)

################################################################################################

rm(list = ls())

load("./data/crop_data_list.rda")
load("./data/shape.rda")
states.keep <- c("Nevada" ,
                 "Utah"  ,
                 "Colorado",
                 "Arizona",
                 "New Mexico",
                 "California")
states.keep2 <- toupper(states.keep)
pram <- names(crop_data_list)
i=1
for (i in 1:length(pram)) {
  subset_shape <- subset(shape, Name %in% states.keep2)

  subset_shape <- left_join(subset_shape, crop_data_list[[i]][,-c(6)], by = c("GEOID"= "fips"))


  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Acres",
            n=5, style="jenks",
            textNA = "No Data",
            alpha = 0.8,
            id = "Acres")
  # +
  #   tm_text("Acres", size = 0.35)

  map<- map +
    tm_layout( main.title =  paste0(pram[i]," Acerage"))


  tmap_save(map, paste0(getwd(),"/data-raw/plots/", pram[i],"_acreage.png"),  width = 6, height = 4, units = 'in')

}

# Saved till here. Plots are created.

# Here data is downloaded from CDL and crops are selected based on the occurrence at county level.




#####################################################
rm(list = ls(all.names = TRUE))
load("./data/crop_data_list.rda")
load("./data/shape.rda")
load("./data/crop_data_final.rda")


Crops_max <- unique(crop_data_final$Crop)
# Crop_demand <- read.csv("demand.csv")
# df_split <- Crop_demand %>%
#   separate(Demand, into = c("Lower_Bound", "Upper_Bound"), sep = "-", convert = TRUE)
#
# pattern <- paste(Crops_max, collapse = "|")

# Subset df_split to include only rows with Crop names partially matching Crops_max
# df_matched <- df_split[grep(pattern, df_split$Crop), ]

# df_matched[,c(2,3)] <- round(df_matched[,c(2,3)]*0.00328084,2) # Converted to feet
# rm(df_split,Crop_demand,pattern)

# crop_data2 <- crop_data %>%
#   left_join(df_matched, by = "Crop") %>%
#   mutate(
#     Upper_bound_water = round(Acres * Upper_Bound,0),
#     Lower_bound_water = round(Acres  * Lower_Bound,0),
#     Avg_water_use = (Upper_bound_water + Lower_bound_water) / 2
#   )


 Crop_demand <- read.csv("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/CDL/Unique_crops_states_demand.csv") #Load this
  # usethis::use_data(Crop_demand, overwrite = TRUE)

Crop_demand <- FarmAdapt::Crop_demand

crop_data2 <- crop_data_final %>%
  left_join(Crop_demand, by = c("Crop", "Name")) %>%
  mutate(
    Avg_water_use = Demand * Acres,
    fips = sprintf("%05d", as.numeric(fips))
  )


crop_data2$Avg_water_use <- crop_data2$Demand*crop_data2$Acres

crop_data2$fips <- as.numeric(crop_data2$fips)
crop_data2$fips <- sprintf("%05d", crop_data2$fips)
crop_data2$fips <- as.character(crop_data2$fips)


# Handle the double cropping. We grabbed the bigger crop (water demanding)

crop_data_na <-  crop_data2 %>%
  filter(is.na(Avg_water_use))

# Use sapply with strsplit to split each entry by "_" and then extract the last element
crop_data_na$Crop2 <- sapply(strsplit(crop_data_na$Crop, "_"), function(x) x[length(x)])

crop_data_na <- crop_data_na[,-c(7)] %>%
  left_join(Crop_demand, by = c("Crop2" = "Crop", "Name"))

crop_data_na$Avg_water_use <- crop_data_na$Demand*crop_data_na$Acres

crop_data_na <- crop_data_na[-8]
crop_data_na <- crop_data_na[,c(1:6,8,7)]

crop_data2 <- crop_data2 %>%
  filter(!is.na(Avg_water_use))
crop_data2 <- rbind.data.frame(crop_data2,crop_data_na)

Crop_demand_final <- crop_data2
usethis::use_data(Crop_demand_final, overwrite = TRUE)

 write.csv(crop_data2, paste0(loc,"/water_avaliable_oct.csv"))


###############################################################
# Water_avaliable.csv contained water deamnd based on FAO. Load that file if that is needed.
# Water_avaliableoct.csv contains water demands from FRIS survey and literature. The values here are higher

 rm(list = ls(all.names = TRUE))
 load("./data/crop_data_list.rda")
 load("./data/shape.rda")
 load("./data/Crop_demand_final.rda")

# # colnames(crop_data2)[7] <- "Avg_water_use"
# load("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")

states.keep <- c("Nevada" ,
                 "Utah"  ,
                 "Colorado",
                 "Arizona",
                 "New Mexico",
                 "California")
states.keep2 <- toupper(states.keep)


crop_data_list <- split(Crop_demand_final, f = Crop_demand_final$Crop)
pram <- names(crop_data_list)

crop_data_df <- bind_rows(crop_data_list)


tmap_list<- list()
tmap_mode("plot")
i = 2
library(sf)
library(viridis)
# Loop through each crop
for (i in 1:length(crop_data_list)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape <- left_join(subset_shape, crop_data_list[[i]][,-c(6)],  c("GEOID" = "fips"))
  # max_value <- max(subset_shape@data[20],na.rm=TRUE)
  # tbreaks <- 10^(0:ceiling(log10(max_value)))
  # tbreaks <- c(0,tbreaks)



  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Avg_water_use",
            title = "Irrigation Amounts \n (Acre-feet)",
             n=7, style="jenks",
            # breaks = tbreaks,
            textNA = "No Data",
             alpha = 0.8,
            id = "name"
    ) +
    # tm_text("Crop", size = 0.5)+
    tm_layout(main.title = paste0(names(crop_data_list)[i]))



  tmap_list[[i]] <- map


  tmap_save(tmap_list[[i]],paste0(getwd(),"/data-raw/plots/Demands/", names(crop_data_list)[i], " Water_demands.png"), width = 8, height = 6, units = 'in')

}

# Arrange the plots using tmap_arrange
combined_plot <- tmap_arrange(tmap_list[[1]],tmap_list[[2]],tmap_list[[3]],tmap_list[[4]],tmap_list[[5]],tmap_list[[6]],
                              ncol = 3,nrow = 3)

combined_plot
tmap_save(combined_plot,"combined_plot_acreage2.png", width = 12, height = 10, units = 'in')


tmap_save(tmap_list[[1]],"Alfafa.png", width = 8, height = 6, units = 'in')


save(crop_data_df, file = "Acerage_demand_data_oct.Rdata")


##########################################################################
# Cost to fallow land 500$\

rm(list = ls(all.names = TRUE))
load("./data/crop_data_list.rda")
load("./data/shape.rda")
load("./data/Crop_demand_final.rda")


cost_per_acre_foot <- 500
# Crop_demand_final <- bind_rows(crop_data_list)
Crop_demand_final$Acres_Fallowed <- Crop_demand_final$Avg_water_use / cost_per_acre_foot

##Plot

load("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/Irrigation_data/county_shapefile.Rdata")

states.keep <- c("Nevada" ,
                 "Utah"  ,
                 "Colorado",
                 "Arizona",
                 "New Mexico",
                 "California")
states.keep2 <- toupper(states.keep)


crop_data_list <- split(Crop_demand_final, f = Crop_demand_final$Crop)
pram <- names(crop_data_list)

tmap_list<- list()
tmap_mode("plot")
i=1
for (i in 1:length(crop_data_list)) {
  subset_shape <- subset(shape, Name %in% states.keep2)
  subset_shape <- sp::merge(subset_shape, crop_data_list[[i]][,-c(6)], by.x = "GEOID", by.y = "fips")
  # max_value <- max(subset_shape@data[20],na.rm=TRUE)
  # tbreaks <- 10^(0:ceiling(log10(max_value)))
  # tbreaks <- c(0,tbreaks)

  map <- tm_shape(subset_shape) +
    tm_borders() +
    tm_fill("Acres_Fallowed",
            title = "Acres Fallowed",
            n=5, style="kmeans",
            # breaks = tbreaks,
            textNA = "No Data",
            # alpha = 0.8,
            id = "name"
    ) +

    tm_credits("Assuming it costs 500$ to fallow",
               position = c("right","bottom"))+
    # tm_text("Crop", size = 0.5)+
    tm_layout(main.title = paste0(names(crop_data_list)[i],  " (Possible Acres available to fallow per 500$)"))



  tmap_list[[i]] <- map


  tmap_save(tmap_list[[i]],paste0(getwd(),"/Plots/Acre fallowed/",names(crop_data_list)[i], "Acres_fallowed.png"), width = 8, height = 6, units = 'in')

}


####################################################################################################################
# Optimization
#####################################################
rm(list = ls(all.names = TRUE))

# setwd("C:/Users/Sangha/OneDrive - Clemson University/NAU/Projects/SNOWPACS/data/CDL")
options(scipen = 9999999)
library(tidyverse)

load("./data/Crop_demand_final.rda")
library(lpSolve)

# Assuming 'data' has columns: State, County, Crop, Demand_acre_feet

# Parameters
cost_per_acre_feet <- 500  # Cost to save one acre-foot of water
budget_per_state <- 50e6  # $50 million
max_allocation_percentage <- 0.10  # Maximum budget allocation per crop/county
min_demand_threshold <- 1000  # Minimum demand threshold in acre-feet

data <- Crop_demand_final

# Objective function
num_vars <- nrow(data)
objective <- rep(1, num_vars)

# Initial Budget Constraints
states <- unique(data$state)
num_states <- length(states)
constraint_matrix <- matrix(0, nrow = num_states, ncol = num_vars)
constraint_dir <- rep("<=", num_states)
constraint_rhs <- rep(budget_per_state / cost_per_acre_feet, num_states)  # Convert budget to acre-feet

for (i in 1:num_states) {
  state_indices <- which(data$state == states[i])
  constraint_matrix[i, state_indices] <- data$Avg_water_use[state_indices] / cost_per_acre_feet
}

# Maximum Allocation Constraints
# Add a row per crop/county combination to the constraints matrix
max_allocation_rows <- rep(max_allocation_percentage * budget_per_state / cost_per_acre_feet, num_vars)
constraint_matrix <- rbind(constraint_matrix, diag(num_vars) * data$Avg_water_use / cost_per_acre_feet)
constraint_dir <- c(constraint_dir, rep("<=", num_vars))
constraint_rhs <- c(constraint_rhs, max_allocation_rows)

# Minimum Size Constraint
# Adjust objective coefficients to 0 for crop/county combinations below the threshold
objective[data$Avg_water_use < min_demand_threshold] <- 0

# Define and solve the LP model
lp_model <- lp("max", objective, constraint_matrix, constraint_dir, constraint_rhs, all.int = TRUE)

# Solution
solution <- lp_model$solution
print(solution)


















































####OLD and works


library(lpSolve)
library(readr)
data <- Crop_demand_final

# Constants
cost_per_acre_foot <- 500
total_budget <- 25000000
max_budget_per_county <- 0.05 * total_budget  # 5% of the total budget
max_budget_per_crop <- 0.010 * total_budget  # 1% of the total budget
max_budget_per_state <- 0.25 * total_budget # Maximum budget per state

# Number of decision variables
num_dec_vars <- nrow(data)


obj_fun_acres <- data$Acres

# The objective function coefficients (average water saved per unit of money spent)
obj_fun_profit  <- data$Avg_water_use / cost_per_acre_foot
# Identify unique states

# Weights
weight_profit <- 1
weight_acres <- 0


obj_fun <- weight_profit * obj_fun_profit + weight_acres * obj_fun_acres


#########################################################################################
const_mat <- matrix(1, nrow = 1, ncol = num_dec_vars)
dir <- c("<=")
rhs <- c(total_budget)
print(dim(const_mat))

counties <- unique(data$name)
for (county in counties) {
  county_constraint <- ifelse(data$name == county, 1, 0)
  const_mat <- rbind(const_mat, county_constraint)
  dir <- c(dir, "<=")
  rhs <- c(rhs, max_budget_per_county)
}
print(dim(const_mat))

crops <- unique(data$Crop)
for (crop in crops) {
  crop_constraint <- ifelse(data$Crop == crop, 1, 0)
  const_mat <- rbind(const_mat, crop_constraint)
  dir <- c(dir, "<=")
  rhs <- c(rhs, max_budget_per_crop)
}
print(dim(const_mat))


states <- unique(data$Name)
for (state in states) {
  state_constraint <- ifelse(data$Name == state, 1, 0)
  const_mat <- rbind(const_mat, state_constraint)
  dir <- c(dir, "<=")
  rhs <- c(rhs, max_budget_per_state)
}
print(dim(const_mat))

# Solve the linear program
lp_solution <- lp("max", obj_fun, const_mat, dir, rhs, all.bin = FALSE)

# Print the results
print(round(lp_solution$solution,0))

# Get the non-zero solutions and their indices
non_zero_solutions <- lp_solution$solution[lp_solution$solution > 0]
non_zero_indices <- which(lp_solution$solution > 0)

# Map the indices back to your data to understand what these solutions correspond to
optimal_allocations2 <- data[non_zero_indices, ]
optimal_allocations2$allocation <- round(non_zero_solutions,0)




# # Create a constraint matrix
# # Start with the budget constraint
# const_mat <- matrix(1, nrow = 1, ncol = num_dec_vars)
# dir <- c("<=")  # direction of the budget constraint
# rhs <- c(total_budget)  # right-hand side of the budget constraint
#
# # Add constraints for each county
# counties <- unique(data$name)
# for (county in counties) {
#   county_constraint <- ifelse(data$name == county, 1, 0)
#   const_mat <- rbind(const_mat, county_constraint)
#   dir <- c(dir, "<=")
#   rhs <- c(rhs, max_budget_per_county)
# }
# print(dim(const_mat))
# # Add constraints for each crop
# crops <- unique(data$Crop)
# for (crop in crops) {
#   crop_constraint <- ifelse(data$Crop == crop, 1, 0)
#   const_mat <- rbind(const_mat, crop_constraint)
#   dir <- c(dir, "<=")
#   rhs <- c(rhs, max_budget_per_crop)
# }
# print(dim(const_mat))
# states <- unique(data$Name)
# # Add constraints for each state
# for (state in states) {
#   state_constraint <- ifelse(data$State == state, 1, 0)
#   const_mat <- rbind(const_mat, state_constraint)
#   dir <- c(dir, "<=")
#   rhs <- c(rhs, max_budget_per_state)
# }
# print(dim(const_mat))


