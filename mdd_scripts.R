library("reshape2")
library("zoo")
library("xts")
library("rio")
library("parsedate")
library("janitor")
library("dplyr")
library("tidyverse")

library('csodata')





#Industrial Production Update

#Get the data from the CSO - MIM = Monthly Indsutrial Production (CSO calls it MIM)
MIM <- cso_get_data(
  "MIM04",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
#Clean and subset to seasonally adjusted index
MIM <- clean_names(MIM)
MIM <- subset(MIM, statistic=="Industrial Production Index (Seasonally Adjusted)",
              select=c(month, industry_sector_nace_rev_2, value)) 

#Read in CSV with rebasing factors
rebase_mim <- read.csv('rebase_factors_mim.csv')
MIM$industry_sector_nace_rev_2 <- as.character(MIM$industry_sector_nace_rev_2)
#Merge sheets
MIM <- merge(MIM, rebase_mim, by ="industry_sector_nace_rev_2")
#Rebase using factors
MIM <- MIM %>% mutate(value_r = value*factor)
#Subset and change sheet from long to wide
MIM_spread <- subset(MIM, select=c(value_r, month, industry_sector_nace_rev_2))
MIM_spread <- spread(MIM_spread, industry_sector_nace_rev_2, value_r)
MIM_spread <- tail(MIM_spread, n=24)

#Write CSV with same order as input sheet
write.csv(MIM_spread[,c("month",
                        "Traditional sector (05 to 17,181,19,22 to 25,28 to 31,321 to 324,329,33,35)",
                        "Food products (10)",
                        "Paper and paper products, printing and reproduction of recorded media (17,18)",
                        "Transport equipment (29,30)",
                        "Other foods (102 to 104,108)",
                        "Grain mill and starch products; prepared animal feeds (106,109)",
                        "Meat and meat products (101)",
                        "Dairy products (105)",
                        "Bakery and farinaceous products (107)",
                        "Wood and wood products, except furniture (16)",
                        "Rubber and plastic products (22)",
                        "Other non-metallic mineral products (23)"
)], file="indprod.csv",row.names=TRUE)
MIM <- select(MIM_spread, c("month",
                "Traditional sector (05 to 17,181,19,22 to 25,28 to 31,321 to 324,329,33,35)",
                "Food products (10)",
                "Paper and paper products, printing and reproduction of recorded media (17,18)",
                "Transport equipment (29,30)",
                "Other foods (102 to 104,108)",
                "Grain mill and starch products; prepared animal feeds (106,109)",
                "Meat and meat products (101)",
                "Dairy products (105)",
                "Bakery and farinaceous products (107)",
                "Wood and wood products, except furniture (16)",
                "Rubber and plastic products (22)",
                "Other non-metallic mineral products (23)"
))

ur <- cso_get_data(
  "MUM02",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
ur <- clean_names(ur)
ur <- subset(ur, lower_and_upper_bound=="Upper Bound (COVID-19 Adjusted MUR)" &
             age_group=="15 - 74 years" &
             statistic=="Monthly Unemployment Rate",
                   select=c(month, sex, value)) 
ur <- spread(ur, sex, value)
ur <- clean_names(ur)
write.csv(ur, file="ur_covid.csv",row.names=TRUE)



vehicles <- cso_get_data(
  "TEM01",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
vehicles <- clean_names(vehicles)
vehicles <- subset(vehicles, statistic=="Vehicles Licensed for the First Time",
              select=c(month, taxation_class, value)) 
vehicles <- spread(vehicles, taxation_class, value)
vehicles <- clean_names(vehicles)
vehicle <- tail(vehicles, n=24)
write.csv(vehicles[,c("month",
                        "all_vehicles",
                        "new_vehicles"
)], file="vehicles.csv",row.names=TRUE)
vehicles <- select(vehicles, c("month",
                                "all_vehicles",
                                "new_vehicles"
))

rppi <- cso_get_data(
  "HPM09",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
rppi <- clean_names(rppi)
rppi <- subset(rppi, statistic=="Residential Property Price Index",
                   select=c(month, type_of_residential_property, value)) 
rppi <- spread(rppi, type_of_residential_property, value)
rppi <- clean_names(rppi)
rppi <- tail(rppi, n=24)

write.csv(rppi[,c("national_all_residential_properties",
                      "national_houses",
                      "national_apartments"
)], file="rppi.csv",row.names=TRUE)

rppi <- select(rppi, c("month",
                       "national_all_residential_properties",
                               "national_houses",
                               "national_apartments"
))


nhgr <- cso_get_data(
  "HSM10",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
nhgr <- clean_names(nhgr)
nhgr <- subset(nhgr, statistic=="New House Guarantee Registrations",
               select=c(month, value)) 
nhgr <- tail(nhgr, n=24)

write.csv(nhgr, file="nhgr.csv",row.names=TRUE)

cpi <- cso_get_data(
  "CPM03",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
cpi <- clean_names(cpi)
cpi <- subset(cpi, statistic=="Consumer Price Index (Base Dec 2016=100)",
               select=c(month, selected_sub_indices, value)) 
cpi <- spread(cpi, selected_sub_indices, value)
cpi <- clean_names(cpi)
cpi <- tail(cpi, n=24)

write.csv(cpi[,c("month", 
                 "cpi_excluding_mortgage_interest",
                  "goods",
                  "services"
)], file="cpi.csv",row.names=TRUE)

cpi <- select(cpi, c("month", 
                     "cpi_excluding_mortgage_interest",
                     "goods",
                     "services"
))


#netexports
nx <- cso_get_data(
  "TSM06",
  wide_format = "long",
  include_ids = FALSE,
  id_list = NULL,
  use_factors = TRUE,
  cache = FALSE
)
nx <- clean_names(nx)
nx <- subset(nx, commodity_group=="Machinery and transport equipment (7)" | commodity_group=="Road vehicles (78)",
              select=c(month, commodity_group, statistic, value)) 
nx <- spread(nx, statistic, value)
nx <- clean_names(nx)
nx <- mutate(nx,net=value_of_imports-value_of_exports)
nx <- spread(nx, commodity_group, net)
nx <- nx %>% 
  rename(
    netimp_mte = "Machinery and transport equipment (7)",
    netimp_road= "Road vehicles (78)"
  )
nx <- clean_names(nx)
nx <- subset(nx, select = c(month, netimp_mte, netimp_road)) 
nx <- nx %>%
  group_by(month) %>%
  summarise_each(funs(first(.[!is.na(.)])))
nx <- tail(nx, n=24)
write.csv(nx[,c("month", 
                 "netimp_mte",
                 "netimp_road"
)], file="nx.csv",row.names=TRUE)

nx <- select(nx, c("month", 
                   "netimp_mte",
                   "netimp_road"
))

mdd_data <- left_join(MIM_spread, ur, vehicles, rppi, nhgr, cpi, nx, by = "month", suffix=c("x", "y"))

mdd_data <- left_join(MIM, ur, by ="month") %>%
  left_join(., vehicles, by="month") %>%
  left_join(., rppi, by="month") %>% 
  left_join(., nhgr, by="month") %>% 
  left_join(., cpi, by="month") %>% 
  left_join(., nx, by="month") 
  
write.csv(mdd_data, file="mdd_data.csv", row.names = TRUE)


