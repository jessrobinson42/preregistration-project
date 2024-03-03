#load libraries
library(pacman)
p_load(tidyverse, janitor, rvest, httr, jsonlite)
       
#documentation: https://developer.osf.io/#operation/registrations_list
##https://github.com/J535D165/datahugger/blob/338781bf45adb70cd99709e81343d0605e04a26d/docs/development.md?plain=1#L21
#https://api.osf.io/v2/registrations/?page=856



registrations <- list()
returns <- list()
attributes <- list()
ids <- list()

pages <- 200:400

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

#bind attributes data
attributes_data <- bind_rows(attributes)

rm(attributes)

sociology_data <-attributes_data %>% 
  group_by(id) %>%
  mutate(subject_string = paste(unlist(subjects), collapse='')) %>%
  ungroup() %>%
  filter(str_detect(subject_string, "Sociology"))

rm(attributes_data)

write_csv(sociology_data, "sociology_registrations.csv") 
#can use this list of all sociology registrations for all future API calls!

#create sosc registration id list
id_list <- sociology_data$id %>% unique()

cont_ids <- list()
contributors <- list()

for (i in id_list) {
#get contributors 
contributors_query <- GET(paste0("https://api.osf.io/v2/registrations/", i, "/contributors"))
returns_cont <- fromJSON(rawToChar(contributors_query$content))

cont_ids[[i]] <- returns_cont$data$id %>% as_tibble()

contributors[[i]] <- returns_cont$data$embeds$users$data$attributes %>% 
  unnest_wider(education) %>% 
  select(full_name, institution) %>% 
  bind_cols(cont_ids[[i]])
}

contributors_data <- bind_rows(contributors) %>% separate(value, c("id", "contributor_id"))

write_csv(contributors_data, "authors.csv")


binded <- sociology_data %>% 
  left_join(contributors_data, by = "id")  %>%
  select(id, contributor_id, full_name, institution, title, description, date_registered, ia_url)
write_csv(binded, "full_data.csv")
