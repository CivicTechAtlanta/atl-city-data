### Scrape Project Details

# Each project has a page on the website, with some basic details, such as project name,
# project timeline, and project budget. This code crawls through a list of all projects, 
# and grabs the basic info

## TODO: Need to account for multi-phase projects

library(rvest); library(dplyr); library(stringr)
library(tidyr); library(readr); library(lubridate)

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
  print(paste0("Grabbing project page ", i))
  project_page_html <- read_html(project_page_links[i])
  colon_separated_list <- 
    project_page_html %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    .[str_detect(., "[\\w+ \\w+]: .+")] %>%
    str_replace_all("[\\t\\n]+", "")
  
  pct_complete <- 
    project_page_html %>%
    html_nodes("li") %>%
    html_text() %>%
    .[!str_detect(., "[\\w+ \\w+]: .+")] %>%
    .[str_detect(., "% [Cc]omplete")]
  
  project_details[[i]]$text <-
    c(colon_separated_list, paste0("Pct Complete: ", pct_complete))
    
  project_details[[i]]$project_type <- 
    project_page_html %>% html_nodes(".singletermname") %>%
    html_text()
}

project_info <- 
  lapply(project_details, function(x) str_split(x$text, ": ")) %>% 
  lapply(function(x) data_frame(field = unlist(lapply(x, function(y) y[1])),
                                value = unlist(lapply(x, function(y) y[2]))))
names(project_info) <- project_page_links

project_info2 <- 
  lapply(project_details, function(x) ifelse(length(x$project_type) == 0,
                                             NA, x$project_type))

project_info_temp <- vector(mode = "list", length = length(project_info))
for(i in seq_along(project_info)){
  print(i)
  project_info_temp[[i]] <- 
    bind_cols(
    data_frame(project_page = rep(project_page_links[i], times = nrow(project_info[[i]]))),
    data_frame(project_type = rep(project_info2[[i]], times = nrow(project_info[[i]]))),
      project_info[[i]])
}

project_details_df <- 
  bind_rows(project_info_temp) %>% 
  spread(field, value) %>% 
  select(project_name = `Project Name`, project_type, pct_complete = `Pct Complete`,
         project_budget = `Project Budget`,
         project_start = `Project Start`, construction_start = `Construction Start`,
         project_completion = `Project completion`, project_page) %>% 
  mutate(project_budget_error_flag = 
           ifelse(project_budget == "$" | project_budget == "$00,000,000",
                  TRUE, FALSE),
         project_budget = parse_number(project_budget),
         project_start = mdy(paste0(str_sub(project_start, 1, 2), "/1/", 
                                    str_sub(project_start, -4, -1))),
         construction_start = mdy(paste0(str_sub(construction_start, 1, 2), "/1/", 
                                         str_sub(construction_start, -4, -1))),
         project_completion = mdy(paste0(str_sub(project_completion, 1, 2), "/1/", 
                                         str_sub(project_completion, -4, -1))),
         pct_complete = .01 * as.numeric(str_extract(pct_complete, "\\d+")),
         project_status = ifelse(is.na(pct_complete), "Not Started",
                                 ifelse(pct_complete == 1, "Completed", "In Process")))

write_csv(project_details_df, "renew-atlanta/project-info/project_details.csv", na = "")
