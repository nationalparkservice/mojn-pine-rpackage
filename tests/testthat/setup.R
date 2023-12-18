# Load data from database
loadPine(test_path("test_db.accdb"))
db_data <- filterPine()  # All data from db

# Write data to temporary csv files for testing
dir <- withr::local_tempdir(clean = FALSE)
writePine(data_dir = dir, dictionary_dir = dir)
loadPine(dir, dictionary_dir = dir)  # load data from csv
csv_data <- filterPine()  # All data from csv

withr::defer(unlink(dir, recursive = TRUE), teardown_env())
