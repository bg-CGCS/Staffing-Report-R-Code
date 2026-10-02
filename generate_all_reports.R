# Clear environment
#rm(list = ls())

# Load package
library(rmarkdown)

# Sample districts
district_info <- data.frame(
  district_name = c(
    "Los Angeles Unified School District" ),
  state_name = c("California"),
  stringsAsFactors = FALSE)


# Absolute path to Rmd template
template_file <- "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/R Code/Staffing Report Markdown.Rmd"

# Output folder
output_path <- "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/Print Reports/2024_25"

# Generate reports
for (i in seq_len(nrow(district_info))) {
  current_district <- district_info$district_name[i]
  current_state <- district_info$state_name[i]
  
  safe_file_name <- gsub("[^[:alnum:]]+", "_", current_district)
  
  rmarkdown::render(
    input = template_file,
    output_format = "pdf_document",
    
    output_file = paste0("Staffing_Report_", safe_file_name, "_", Sys.Date(), ".pdf"),
    
    output_dir = output_path,
    
    params = list(district_name = current_district, state_name = current_state),
    
    envir = new.env(parent = globalenv())
  )
  
  message("Completed report for: ", current_district)
}

message("All reports generated successfully.")

