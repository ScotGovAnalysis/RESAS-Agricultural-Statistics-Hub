# Import and save nitrogen data as Rda
#### 
####
#################################
# Load necessary libraries
library(here)
library(readxl)
library(stringr)
library(tidyverse)
source(here("Economy/FBS", "fbs_utility.R"))
library(RtoSQLServer)


#data ------

n_bal<-read_table_from_db(server=server,
                  database=database, 
                  schema=schema, 
                  table_name="N_bal") %>% 
  rename_with(~ sub("^X", "", .)) %>% 
  rename_with(~ gsub("_", "-", .))


nue<-read_table_from_db(server=server,
                          database=database, 
                          schema=schema, 
                          table_name="nue") %>% 
  rename_with(~ sub("^X", "", .)) %>% 
  rename_with(~ gsub("_", "-", .))


# remove X from year columns


n_balance<-n_bal %>% 
  mutate(n_type="n_balance") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value") %>% 
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = value)


nue<-nue%>% 
  mutate(n_type="nue") %>% 
  pivot_longer(
    cols = `2019-20`:fbs_current_year,
    names_to = "year",
    values_to="value") %>% 
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = value)


nitrogen_data<-dplyr::bind_rows(n_balance, nue) %>% 
  rename(farm_type=`Farm-type`)%>% 
  rename(Lower=`95% CI (lower limit)`,
         Upper=`95% CI (upper limit)`,
         Median=`Average (median)`) %>% 
  data.frame()


#save

save(nitrogen_data, file="Data/nitrogen_data.Rda")
