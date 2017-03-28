### Scrape Project Details

# Each project has a page on the website, with some basic details, such as project name,
# project timeline, and project budget. This code crawls through a list of all projects, 
# and grabs the basic info

## TODO: Need to account for multi-phase projects

library(rvest); library(dplyr); library(stringr)
library(tidyr); library(readr)

page_html <- read_html("http://renewatlantabond.com/project/muse-street-2/")

max_search_page_num <- 
  read_html("http://renewatlantabond.com/page/1/?s&post_type=project#038;post_type=project") %>%
  html_nodes(".page-numbers") %>% html_attr("href") %>% str_extract("(?<=page/)\\d+") %>%
  as.numeric() %>% max(na.rm=T)

project_page_links <- vector(mode = "list", length = max_search_page_num)
for(i in seq_along(project_page_links)){
  print(paste0("Grabbing search page ", i))
  search_page_url <- paste0("http://renewatlantabond.com/page/",
                            i,
                            "/?s&post_type=project#038;post_type=project")
  project_page_links[[i]] <- 
    read_html(search_page_url) %>%
    html_nodes(".search-title a") %>%
    html_attr("href")
}
project_page_links <- unlist(project_page_links)
rm(i, max_search_page_num)

project_details <- vector(mode = "list", length = length(project_page_links))

for(i in seq_along(project_details)){
  project_details[[i]] <- 
    read_html(project_page_links[i]) %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    .[str_detect(., "[\\w+ \\w+]: .+")] %>%
    str_replace_all("[\\t\\n]+", "")
}

project_info <- 
  lapply(project_details, function(x) str_split(x, ": ")) %>% 
  lapply(function(x) data_frame(field = unlist(lapply(x, function(y) y[1])),
                                value = unlist(lapply(x, function(y) y[2]))))
names(project_info) <- project_page_links
project_info_temp <- vector(mode = "list", length = length(project_info))
for(i in seq_along(project_info)){
  project_info_temp[[i]] <- bind_cols(
    data_frame(project_page = rep(project_page_links[i], times = nrow(project_info[[i]]))),
      project_info[[i]])
}
project_details_df <- 
  bind_rows(project_info_temp) %>% 
  spread(field, value) %>%
  select(-`Project Type`, -`Phase 1`, -`Phase 2`)

write_csv(project_details_df, "project_details.csv", na = "")