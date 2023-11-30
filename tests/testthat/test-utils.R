skip_if_not(file.exists(here::here("tests", "testthat", "test_db.accdb")), message = "Skipped - could not find tests/testthat/test_db.accdb.")

data_names <- validDataTables()  # Get data table names
metadata_names <- validMetadataTables()  # Get metadata table names

# Check that reading data tables from db and csv yields same result
for (tbl in data_names) {
  test_that(paste0("Data read from ", tbl, ".csv matches data read from database"), {
    csv <- csv_data$data[[tbl]]
    db <- db_data$data[[tbl]]
    expect_true(all.equal(csv, db))
  })
}

# Check that reading metadata tables from db and csv yields same result
for (tbl in metadata_names) {
  test_that(paste0("Metadata read from ", tbl, ".csv matches metadata read from database"), {
    csv <- csv_data$metadata[[tbl]]
    db <- db_data$metadata[[tbl]]
    expect_true(all.equal(csv, db))
  })
}
