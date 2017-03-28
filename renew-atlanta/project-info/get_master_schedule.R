### Get Master Schedule

# The Master Schedule is a list of all the projects with their corresponding
# dates and other metadata such as council district(s) and project category.
# This info is provided on Renew Atlanta's website as a PDF file with paged tabular data;
# table headers are tab-delimited, table data is fixed width

library(pdftools)

# Get raw text of master schedule (convert from PDF to text file)
master_schedule <- pdf_text("http://renewatlantabond.com/wp-content/uploads/2017/02/RATL-Master-Schedule-Update-12-2016-Final.pdf")
write_lines(master_schedule, "project-info/master_schedule.txt")