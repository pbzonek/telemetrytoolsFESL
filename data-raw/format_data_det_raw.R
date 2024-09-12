#format raw detection data
data_det_raw <- readRDS("data-raw/files-raw/data_det_raw_2024-09-10.rds")


usethis::use_data(data_det_raw, overwrite = TRUE)

