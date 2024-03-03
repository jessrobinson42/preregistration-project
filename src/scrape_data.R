#load libraries
library(pacman)
p_load(tidyverse, janitor, rvest, httr, jsonlite, tictoc)
       
#documentation: https://developer.osf.io/#operation/registrations_list
#example: https://api.osf.io/v2/registrations/?page=856


#Query all registrations 
attributes <- list()
ids <- list()

pages <- 1:15870

tic()
for (i in pages) {
  
  registrations <- list()
  returns <- list()
  
registrations[[i]] <- GET(paste0("https://api.osf.io/v2/registrations/?page=", i))
returns[[i]] <- fromJSON(rawToChar(registrations[[i]]$content))

ids[[i]] <- returns[[i]]$data$id %>% as_tibble() %>% rename(id = value)

attributes[[i]] <- returns[[i]]$data$attributes %>%
  filter(registration == TRUE) %>%
  select(title, description, date_registered, ia_url, subjects) %>%
  bind_cols(ids[[i]])

print(paste("Completed: Page", i))

#save space, a little hacky
rm(registrations)
rm(returns)

}
toc()

#bind attributes data
attributes_data <- bind_rows(attributes)

rm(attributes)

#filter for just sociology data
sociology_data <-attributes_data %>% 
  group_by(id) %>%
  mutate(subject_string = paste(unlist(subjects), collapse='')) %>%
  ungroup() %>%
  filter(str_detect(subject_string, "Sociology"))

rm(attributes_data)

write_csv(sociology_data %>% select(id, title, description, date_registered, ia_url), "sociology_registrations.csv") 
#can use this list of all sociology registrations for all future API calls!

#create sosc registration id list
id_list <- sociology_data$id %>% unique()


#pull contributor names
cont_ids <- list()
contributors <- list()

for (i in id_list) {
#get contributors 
  contributors_query <- list()
  returns_cont <- list()
  
contributors_query[[i]] <- GET(paste0("https://api.osf.io/v2/registrations/", i, "/contributors"))
returns_cont[[i]] <- fromJSON(rawToChar(contributors_query[[i]]$content))

cont_ids[[i]] <- returns_cont[[i]]$data$id %>% as_tibble()

contributors[[i]] <- returns_cont[[i]]$data$embeds$users$data$attributes %>% 
  select(full_name) %>% 
  bind_cols(cont_ids[[i]])

rm(contributors_query)
rm(returns_cont)

}

contributors_data <- bind_rows(contributors) %>% separate(value, c("id", "contributor_id"))
write_csv(contributors_data, "authors.csv")

binded <- sociology_data %>% 
  left_join(contributors_data, by = "id")  %>%
  select(id, contributor_id, full_name, title, description, date_registered, ia_url)
write_csv(binded, "full_data.csv")
