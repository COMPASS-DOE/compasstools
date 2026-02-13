## code to prepare `Lx_variables` dataset goes here

# `variables_metadata.csv` is direct from the
# sensor data processing pipeline
Lx_variables <- readr::read_csv("data-raw/variables_metadata.csv")
Lx_variables <- Lx_variables[c("research_name", "final_units", "description")]

usethis::use_data(Lx_variables, overwrite = TRUE)
