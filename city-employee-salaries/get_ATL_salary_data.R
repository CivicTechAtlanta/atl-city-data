## Get Atlanta Salary Data & parse into nice table format (also, export to csv)

library(pdftools); library(readr); library(dplyr)
library(stringr); library(tidyr)

# Source: https://www.muckrock.com/foi/atlanta-325/city-employee-salary-information-atlanta-20915/

# Download ATL Gov Salary file from the web
download.file(url = "https://d3gn0r3afghep.cloudfront.net/foia_files/2016/01/22/1-14-16_MR20915_RES.pdf", 
              destfile = "~/1-14-16_MR20915_RES.pdf")

# Read data from PDF and save to a txt file
write_lines(pdf_text("~/1-14-16_MR20915_RES.pdf"), "atlanta_salaries.txt")
salary_raw <- read_lines("atlanta_salaries.txt")
salary_raw <- salary_raw[!str_detect(salary_raw, "\\d{2}/\\d{2}/\\d{4} +\\d{1,2} of 80")]
salary_raw <- salary_raw[!str_detect(salary_raw, "City of Atlanta - DHR")]

# names
employee <- 
  str_trim(salary_raw) %>% str_extract("^[\\w-,'\\. \\(\\)]+(?= +\\d{2} )") %>% str_trim()

# ages
age <- 
  str_trim(salary_raw) %>% str_extract("(?<= {2})\\d{2}(?= )") %>% .[!is.na(.)]

header_rows <- which(str_detect(salary_raw, " ?NAME +Age +Sex +(Ethnic Origin) +(JOB TITLE) +Organization +(ANNUAL SALARY)"))
header_groups <- 
  left_join(data_frame(row_id = seq_along(salary_raw)), data_frame(header_rows, header_row_group = header_rows), 
          by = c("row_id" = "header_rows")) %>% fill(header_row_group)
position_name <- str_locate(salary_raw[header_rows], "NAME")
position_age <- str_locate(salary_raw[header_rows], "Age")
position_sex <- str_locate(salary_raw[header_rows], "Sex")
position_ethnic <- str_locate(salary_raw[header_rows], "Ethnic Origin")
position_title <- str_locate(salary_raw[header_rows], "JOB TITLE")
position_organization <- str_locate(salary_raw[header_rows], "Organization")
position_salary <- str_locate(salary_raw[header_rows], "ANNUAL SALARY")

salary_raw_df <- bind_cols(header_groups, data_frame(salary_raw))

atl_salary <- vector(mode = "list", length = (salary_raw_df %>% filter(nchar(salary_raw) > 0) %>% nrow()) - length(header_rows))

for(i in seq_along(header_rows)){
  text <- 
    salary_raw_df %>% 
    filter(header_row_group == header_rows[i] & row_id != header_row_group & nchar(salary_raw) > 0) %>%
    .$salary_raw
  positions <- rbind(position_name[i,1], position_age[i,1], position_sex[i,1], position_ethnic[i,1],
                     position_title[i,1], position_organization[i,1], position_salary[i,1]) %>%
    as.data.frame() %>%
    mutate(end = lead(start) - 1)
  atl_salary[[i]] <- read_fwf(paste(text, collapse = "\n"), 
           col_positions = fwf_positions(start = positions[,"start"], end = positions[,"end"]))
  names(atl_salary[[i]]) <- c("Name", "Age", "Sex", "Ethnic Origin", "Job Title", "Organization", "Annual Salary")
}

atl_salary_data <- bind_rows(atl_salary)
names(atl_salary_data) <- tolower(make.names(names(atl_salary_data)))
atl_salary_data <- 
  atl_salary_data %>% 
  mutate(age = as.integer(age), annual.salary = as.numeric(annual.salary))

write_csv(atl_salary_data, "atl_salary_data_2015.csv")