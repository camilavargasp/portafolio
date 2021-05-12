### Functions to run tnc_gear project figures

##set up
library(tidyverse)
library(janitor)
library(here)


##Cleans Watson

get_watson_data <- function(year_query = NULL,
                            include_ind = TRUE,
                            include_non_ind = TRUE){
  
  if(include_ind){
    
    watson_index_raw_ind <- read_csv(here::here("data/tnc_plastic_gear/watson_index_per_fishing_event_ind.csv")) %>%
      clean_names() %>%
      rename(year = i_year) %>%
      mutate(id = as.character(id)) %>%
      mutate(productive_sector = "industrial") %>%
      distinct()
  }
  
  if(include_non_ind){
    
    watson_index_raw_Nind <- read_csv(here::here("data/tnc_plastic_gear/watson_index_per_fishing_event_Nind.csv")) %>%
      clean_names() %>%
      rename(year = i_year) %>%
      mutate(id = as.character(id)) %>%
      mutate(productive_sector = "non_industrial") %>%
      distinct()
  }
  
  if(include_ind & include_non_ind){
    watson_index_all <- rbind(watson_index_raw_ind, watson_index_raw_Nind) %>%
      distinct() %>%
      rename(watson_country_id = c_number,
             taxon_key = taxonkey)
    return(watson_index_all)
  }
  
  if(include_ind & !include_non_ind){
    
    return(watson_index_raw_ind)
  }
  
  if(include_non_ind & !include_ind){
    
    return(watson_index_raw_Nind)
  }
  
  
}

watson_clean <- get_watson_data()


##claans FAO

get_fao_landings <- function(year_query =NULL,
                             include_inland = FALSE,
                             include_aquaculture = FALSE) {
  
  fao_data <- read_csv(here::here("data/tnc_plastic_gear/fao_global_production_1950_2018.csv"))
  
  fao_landings <- fao_data %>%
    rename(country = "Country (Country)",
           species = "ASFIS species (ASFIS species)",
           fishing_area_name = "FAO major fishing area (FAO major fishing area)",
           production_source = "Detailed production source (Detailed production source)",
           unit = "Unit (Unit)") %>%
    #select(1:5, 66:71) %>% ##Note this line select years from 2010 to 2015
    gather(key = year, value = landings, 6:74) %>%
    mutate(landings = ifelse(landings == "...", NA, landings)) %>%
    mutate(landings = str_remove(landings, "[A-Z]+")) %>%
    mutate(landings = str_trim(landings, side = "both")) %>%
    mutate(landings = as.numeric(landings)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(species = str_to_lower(species)) %>%
    mutate(species = str_remove(species, "\\[")) %>%
    mutate(species = str_remove(species, "\\]")) %>% ## Remove brackets from species that their en_name is thier sci_name
    filter(!str_detect(country, "Totals"),
           unit != "Number")
  
  if(!include_inland) {
    fao_landings <- dplyr::filter(fao_landings,
                                  !str_detect(fishing_area_name, "Inland waters"))
  }
  if(!include_aquaculture) {
    fao_landings <- dplyr::filter(fao_landings,
                                  production_source=="Capture production")
  }
  
  
  if(!is.null(year_query)) {
    # print("filtering")
    # print(count(fao_landings))
    
    fao_landings <- fao_landings %>% dplyr::filter(year == year_query)
    # print(count(fao_landings))
  }
  print(count(fao_landings))
  #print(unique(fao_landings$year))
  return(fao_landings)
}

fao_clean <- get_fao_landings()



