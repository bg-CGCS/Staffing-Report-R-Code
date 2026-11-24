# Clear environment
rm(list = ls())

# Load package
library(rmarkdown)

# Sample districts (keep district_leaid numeric to match read.csv output)
district_info <- data.frame(
  district_name = "Jackson Public Schools",
  district_leaid = 2802190,
  state_name = "Mississippi",
  select_year = 2425,
  full_year_name = "2024-25",
  stringsAsFactors = FALSE
)

# Paths
template_file <- "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/Staffing-Report-R-Code/Staffing Report Markdown.Rmd"
output_path   <- "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/Print Reports/2024_25"

# Generate reports
for (i in seq_len(nrow(district_info))) {
  current_district <- district_info$district_name[i]
  current_state    <- district_info$state_name[i]
  current_leaid    <- district_info$district_leaid[i]
  
  safe_file_name <- gsub("[^[:alnum:]]+", "_", current_district)
  
  rmarkdown::render(
    input = template_file,
    output_file = paste0(
      "Staffing_Report_",
      safe_file_name,
      "_",
      Sys.Date(),
      ".pdf"
    ),
    output_dir = output_path,
    knit_root_dir = dirname(template_file),
    params = list(
      district_name = current_district,
      district_leaid = current_leaid,
      state_name = current_state,
      select_year = district_info$select_year[i],
      full_year_name = district_info$full_year_name[i]
    ),
    envir = new.env(parent = globalenv())
  )
  
  message("Completed report for: ", current_district)
}

message("All reports generated successfully.")