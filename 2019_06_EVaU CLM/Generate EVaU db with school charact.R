# Metadata ----------------------------------------------------------------
# Title: Generate a dataset with EVaU results from PDF and school char
# Purpose: Extract data and combine based on a character key
# Author(s): @pablocal
# Date Created: 2019-06-20
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------

rm(list = ls())

library(tidyverse)
library(tabulizer)
library(rvest)
library(stringdist)


# 1.1 Extract data from the PDF with EVaU results --------------------------

tables <- extract_tables("data/EvAU_18_CLM.pdf", encoding = "UTF-8") # extract from PDF

data_evau <- map(tables, as_tibble) %>% # convert the list into a data frame
  bind_rows()
head(data_evau, 10)


# 1.2 Clean data ----------------------------------------------------------

data_evau <- data_evau %>% 
  mutate(text = paste(V1, V2, V3, sep = " "),
         text = str_remove_all(text, "NA")) %>% 
  filter(str_starts(text, "Asignatura") | str_starts(text, "\\("))

data_evau <- data_evau %>% 
  rowwise() %>% 
  mutate(
    subject = ifelse(str_starts(text, "Asignatura") == T, text, NA_character_),
    subject = str_sub(subject, 12, str_length(subject)),
    center_code = str_sub(text, 2, 5),
    center_name = str_sub(text, 8, str_locate(text, "\\:")[1]-1),
    pass = str_sub(text, str_locate(text, "\\:")[1]+1, str_locate(text, "\\:")[1]+3),
    fail = str_sub(text, str_locate(text, "suspensos")[1]-4, str_locate(text, "suspensos")[1]-1),
    fail = str_remove_all(fail, "y"),
    average_mark = str_sub(text, str_locate(text, "Media:")[2]+1, str_locate(text, "Media:")[2]+6)
  ) %>% 
  select(-starts_with("V"), -text)

data_evau$subject <- zoo::na.locf(data_evau$subject)

data_evau <- data_evau %>% 
  filter(!is.na(average_mark)) %>% 
  mutate(pass = as.integer(pass),
         fail = as.integer(fail),
         average_mark = as.double(str_replace(average_mark, "\\,", "\\.")))



# 2.1 Web scrape to get schools links  ---------------------------

extract_links <- function(i){
  
  url <- html_session(paste0("http://www.educa.jccm.es/educacion/cm/educa_jccm/BBDD_ACCESS.1.1.tkContent.27265/tkListResults?formName=SQLQueriesSearcher&nshow.sqlResults=1&position.sqlResults=", i, "&idQuery=961"))
  
  name <- read_html(url) %>% 
    html_node(".campListNOMBRE a") %>% 
    html_text()
  
  url_school <- read_html(url) %>% 
    html_node(".campListNOMBRE a") %>% 
    html_attr("href")
  
  return_df <- tibble(name = str_to_lower(name), url_school = url_school)
  
  return(tryCatch(return_df, error = function(e) NULL))
}

list_map <- 1:1753
links_school <- map_df(list_map, extract_links)


# 2.2 Clean school links --------------------------------------------------

data_evau_to_match <- data_evau %>% 
  mutate(name_match = str_remove_all(center_name, 'COLEGIO SALESIANO|COLEGIO|I.E.S.|CENTRO F.P. ESP.|ESCUELA DE ARTES|ESCUELA DE ARTE|\\.|\\"'),
         name_match = str_to_lower(name_match),
         name_match = str_trim(name_match, side = "both")) %>% 
  group_by(center_name) %>% 
  summarise(name_match = first(name_match))

links_school$match_index <- as.integer(rownames(links_school))

match_index <- amatch(data_evau_to_match$name_match, links_school$name, maxDist = 4)

data_evau_to_match <- cbind(data_evau_to_match, match_index) %>% 
  left_join(links_school, by = "match_index")


# 2.3 Extract school information ------------------------------------------

extract_school_info <- function(url_school){
  
  url <- html_session(paste0("http://www.educa.jccm.es", url_school))
  
  fields <- read_html(url) %>% 
    html_nodes(".fieldDetailView") %>% 
    html_text() %>% 
    str_remove_all(": ")
  
  charact <- read_html(url) %>% 
    html_nodes(".valueDetailView") %>% 
    html_text() %>% 
    str_remove_all("\t|\n|\r")
  
  return_df <- tibble()
  return_df <- rbind(return_df, charact)
  colnames(return_df) <- fields
  
  return_df$url_school <- url_school
  
  return(tryCatch(return_df, error = function(e) NULL))
}


schools_urls <- unique(data_evau_to_match$url_school)[!is.na(unique(data_evau_to_match$url_school))]
school_data <- map_df(schools_urls, extract_school_info)

# 3. Match files ----------------------------------------------------------

school_data <- left_join(data_evau_to_match, school_data, by = "url_school") %>% 
  select(-name_match, -match_index, -name, -url_school, -Imagen, -`Situación`, -`Teléfono`, -Fax, -Email, -Web, -Nif, -`NIF/CIF`)

final_df <- left_join(data_evau, school_data, by = "center_name") %>% 
  filter(!is.na(`Código del Centro`))

head(final_df, 10)



