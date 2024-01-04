#' @importFrom magrittr %>% %<>%

# Create package environment to load data into
pkg_globals <- new.env(parent = emptyenv())

#' Get data table from package global env
#'
#' Calls get() with an error handler that reminds user to load data and defaults to using pkg env
#'
#' @param data_name A string that is either a table name (see [validDataTables()]), "data" (all data), "metadata" (all metadata), "all" (all data and metadata)
#'
#' @return A tibble.
#'
get_w_err <- function(data_name) {
  tryCatch({data <- get(data_name, pkg_globals)},
           error = function(e) {
             if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
               stop(paste0("Could not find data. Did you remember to call `loadPine()`?\n\tOriginal error: ", e$message))
             }
             else {e}
           })
  return(data)
}

#' Get dataset from package global env
#'
#' @param data_name A string that is either a table name (see [validDataTables()]), "data" (all data), "metadata" (all metadata), "all" (all data and metadata)
#'
#' @return A nested list containing lists `data` and `metadata`.
#'
get_data <- function(data_name = "all") {
  # Get valid data and metadata table names
  data_table_names <- validDataTables()
  metadata_table_names <- validMetadataTables()

  # Fetch data only
  if (length(data_name) == 1 && data_name == "data") {
    metadata_table_names <- c()
    # Fetch all metadata
  } else if (length(data_name) == 1 && data_name == "metadata") {
    data_table_names <- c()
    # Fetch only specified tables
  } else if (all(data_name %in% data_table_names | data_name %in% metadata_table_names)) {
    data_table_names <- data_name[data_name %in% data_table_names]
    metadata_table_names <- data_name[data_name %in% metadata_table_names]
  } else if (!(length(data_name) == 1 && data_name == "all")) {  # data_name not in valid name lists and not "all"
    stop("Invalid data_name argument. Options are 'all' (all data and metadata), 'data' (all data), 'metadata' (all metadata), or a vector of data and/or metadata table names. Use `validDataTables()` and `validMetadataTables()` to see all valid table names.")
  }

  data <- sapply(data_table_names, function(table_name) {get_w_err(table_name)}, USE.NAMES = TRUE, simplify = FALSE)
  metadata <- sapply(metadata_table_names, function(table_name) {get_w_err(table_name)}, USE.NAMES = TRUE, simplify = FALSE)

  return(list(data = data, metadata = metadata))
}

#' List valid data table names
#'
#' @return A character vector of valid table names
#' @export
#'
validDataTables <- function() {
  data_table_names <- get_w_err("data_tables")
  return(data_table_names)
}

#' List valid metadata table names
#'
#' @return A character vector of valid table names
#' @export
#'
validMetadataTables <- function() {
  metadata_table_names <- get_w_err("metadata_tables")
  return(metadata_table_names)
}

#' Load five needle pine data
#'
#' Execute this function every time you load the fiveneedlepine package.
#'
#' @param data_path Path to either a five needle pine database backend (.accdb) or a folder containing CSV data files exported by `fetchaccess::fetchFromAccess()`.
#' @param dictionary_dir Ignored unless `data_path` points to a folder of CSV files. The path to a folder containing data dictionary files exported by `fetchaccess::fetchFromAccess()`.
#' @param dictionary_filenames Names of data dictionary files. Typically you will want to leave these as the default.
#'
#' @return A nested list containing lists `data` and `metadata`.
#' @export
#'
#' @examples
#' \dontrun{
#' #' # Load from CSV and data dictionary files
#' LoadPine("M:/Monitoring/Pine/DataExports", "M:/Monitoring/Pine/DataExports/DataDictionary")
#'
#' # Load from Access db
#' LoadPine("M:/Monitoring/Pine/DataExports/FNP_BackEnd.accdb")
#' }
loadPine <- function(data_path, dictionary_dir = data_path, dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                                                    attributes = "data_dictionary_attributes.txt",
                                                                                    categories = "data_dictionary_categories.txt")) {
  # Normalize paths
  data_path <- normalizePath(data_path, mustWork = FALSE)
  dictionary_dir <- normalizePath(dictionary_dir, mustWork = FALSE)
  # Check whether paths point to .accdb or folder of csv/txt
  is_csv <- (!is.null(data_path) && !grepl("\\..+$", basename(data_path), ignore.case = TRUE)) &&
    (!is.null(dictionary_dir) && !grepl("\\..+$", basename(dictionary_dir), ignore.case = TRUE))  # Are the data and metadata paths folders (presumably containing csv (data) and txt (metadata) files)?
  is_accdb <- !is.null(data_path) && grepl("*.\\.accdb$", data_path, ignore.case = TRUE)  # Is the data an .accdb file?

  if (is_accdb) {
    ## Option 1: Read data from Access database
    all_tables <- fetchaccess::fetchFromAccess(data_path, data_prefix = "qExport_", lookup_prefix = c("tlu_", "tlu"), lookup_regex = "(^tlu_.*)|(^tbl_Sites$)", as.is = FALSE, add_r_classes = TRUE)
    all_tables <- list(data = all_tables$data,
                       metadata = all_tables$metadata)

  } else if (is_csv) {
    ## Option 2: Read data from CSV
    # Load data dictionary
    dict_tables <- readr::read_tsv(file.path(dictionary_dir, dictionary_filenames["tables"]), show_col_types = FALSE) %>%
      dplyr::mutate(dplyr::across(dplyr::where(~ !is.character(.x)), as.character))  # Make all metadata columns character
    dict_attributes <- readr::read_tsv(file.path(dictionary_dir, dictionary_filenames["attributes"]), show_col_types = FALSE) %>%
      dplyr::mutate(dplyr::across(dplyr::where(~ !is.character(.x)), as.character))  # Make all metadata columns character
    dict_categories <- readr::read_tsv(file.path(dictionary_dir, dictionary_filenames["categories"]), show_col_types = FALSE) %>%
      dplyr::mutate(dplyr::across(dplyr::where(~ !is.character(.x)), as.character))  # Make all metadata columns character

    # Create column type spec
    col_spec <- fetchaccess::makeColSpec(dict_attributes)


    # Read from CSV
    data <- mapply(function(table_name, file_name) {
      df <- readr::read_csv(file.path(data_path, file_name), col_types = col_spec[[table_name]]) %>%
        tibble::as_tibble()  # convert to tibble to get rid of extra attributes that read_csv adds so that there is no difference b/w reading from access vs csv
    }, dict_tables$tableName, dict_tables$fileName)

    names(data) <- dict_tables$tableName

    all_tables <- list(data = data,
                       metadata = list(tables = dict_tables,
                                       fields = dict_attributes,
                                       categories = dict_categories))
  } else {
    stop("data_path and/or metadata_path are invalid")
  }

  # Load data and metadata into package environment
  lapply(names(all_tables$data), function(table_name) { assign(table_name, all_tables$data[[table_name]], envir = pkg_globals) })
  lapply(names(all_tables$metadata), function(table_name) { assign(table_name, all_tables$metadata[[table_name]], envir = pkg_globals) })
  assign("metadata_tables", names(all_tables$metadata), envir = pkg_globals)
  assign("data_tables", names(all_tables$data), envir = pkg_globals)

  invisible(all_tables)
}


#' Write pine data to CSV
#'
#' @inheritParams fetchaccess::writeToFiles
#' @param ... Arguments to pass to [filterPine()]
#'
#' @export
#'
writePine <- function(data_dir = here::here("data"), dictionary_dir = data_dir,
                      dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                               attributes = "data_dictionary_attributes.txt",
                                               categories = "data_dictionary_categories.txt"),
                      verbose = FALSE, ...)
{
  fetchaccess::writeToFiles(all_tables = filterPine(...), data_dir = data_dir, dictionary_dir = dictionary_dir, lookup_dir = NA, dictionary_filenames = dictionary_filenames, verbose = verbose)
}

#' List valid values for filtering data
#'
#' @return A named list of valid filters for this dataset
#' @export
#'
validFilters <- function() {
  tbls <- get_data(c("Visit", "Site", "categories"))
  metadata <- tbls$metadata
  data <- tbls$data
  filters <- list()
  filters$network <- dplyr::filter(metadata$categories, attributeName == "Network")$code
  filters$park <- dplyr::filter(metadata$categories, attributeName == "Park")$code
  filters$sample_frame <- dplyr::filter(metadata$categories, attributeName == "SampleFrame")$code
  filters$panel <- unique(data$Site$Panel)
  filters$site_code <- unique(data$Site$SiteCode)
  filters$visit_year <- unique(lubridate::year(data$Visit$VisitDate))
  filters$flag <- dplyr::filter(metadata$categories, attributeName == "Flag")$code
  filters$protected_status <- dplyr::filter(metadata$categories, attributeName == "ProtectedStatus")$code
  filters$data_processing_level <- dplyr::filter(metadata$categories, attributeName == "DataProcessingLevel")$code
  return(filters)
}

#' Filter some or all of the pine dataset
#'
#' Use [validFilters()] to see list of valid values for filter arguments.
#'
#' @inheritParams get_data
#' @inheritParams filterOne
#' @param network Character vector of four letter network code(s)
#' @param park Character vector of four letter park code(s)
#' @param sample_frame Character vector of sample frame(s)
#' @param panel Character vector of panel(s)
#' @param site_code Character vector of site code(s)
#' @param visit_year Chatacter vector of visit year(s)
#' @param flag Character vector of data quality flag(s)
#' @param protected_status Character vector of protected status(es)
#' @param data_processing_level Character vector of data processing level(s)
#'
#' @return
#' @export
#'
filterPine <- function(data_name = "all", network, park, sample_frame, panel, site_code, visit_year, flag, protected_status, data_processing_level, case_sensitive = FALSE, silent = FALSE) {
  dataset <- get_data(data_name)
  data <- dataset$data
  metadata <- dataset$metadata

  # Convert visit year and panel to character in case they are passed in as numeric
  if (!missing(visit_year)) {
    visit_year <- as.character(visit_year)
  }
  if (!missing(panel)) {
    panel <- as.character(panel)
  }
  # Map filter values to column names
  all_filter_cols <- list(Network = getFilter(network),
                          Park = getFilter(park),
                          SampleFrame = getFilter(sample_frame),
                          Panel = getFilter(panel),
                          SiteCode = getFilter(site_code),
                          VisitDate = getFilter(visit_year),
                          Flag = getFilter(flag),
                          ProtectedStatus = getFilter(protected_status),
                          DataProcessingLevel = getFilter(data_processing_level))
  # Get only columns to be filtered
  filter_cols <- all_filter_cols[!is.na(all_filter_cols)]

  data_names <- names(data)
  data <- lapply(names(data), function(data_name){
    df <- filterOne(data[[data_name]], data_name, filter_cols = filter_cols, case_sensitive = case_sensitive, silent = silent)
    return(df)
  })
  names(data) <- data_names

  dataset <- list(data = data,
                  metadata = metadata)

  if (length(dataset$data) == 1 && length(dataset$metadata) == 0) {
    dataset <- dataset$data[[1]]
  } else if (length(dataset$data) == 0 && length(dataset$metadata) == 1) {
    dataset <- dataset$metadata[[1]]
  }

  return(dataset)
}


#' Helper function for filterPine
#'
#' @param arg Filter argument
#' @param no_arg TRUE if arg is missing
#' @param several.ok Allow multiple filter options?
#'
#' @return NA if arg is missing, char vector of filters otherwise.
#'
getFilter <- function(arg, no_arg = missing(arg), several.ok = TRUE) {
  if (no_arg) {
    arg <- NA
  } else {
    arg <- match.arg(arg, choices = validFilters()[[deparse(substitute(arg))]], several.ok)  # deparse(substitute(arg)) gets the original argument name as a string (e.g. "park")
  }
  return(arg)
}


#' Filter a single dataframe
#' @description Helper function for filterPine, not to be used outside that function
#'
#' @param data A tibble of pine data
#' @param data_name The name of the data table (see `names(GetColSpec())` for valid options)
#' @param filter_cols Named vector where names are column names and values are values to filter on. This is created in the filterPine function.
#' @param case_sensitive Should non-numeric filters be treated as case-sensitive?
#' @param silent Suppress informational messages?
#'
#' @return A tibble of filtered data
#'
filterOne <- function(data, data_name, filter_cols, case_sensitive, silent) {
  # Iterate through each column to be filtered and filter the dataset on the value provided
  # Note: the !!as.symbol(col) syntax takes a character string (the column name) and causes it to be evaluated as a data variable that references a dataframe column
  cols_filtered <- c()
  for (col in names(filter_cols)) {
    if (col %in% names(data)) {  # Only filter if the column is present in the dataframe
      cols_filtered <- c(cols_filtered, col)  # Use this to keep track of which columns were actually filtered
      filter_value <- filter_cols[[col]]  # Value(s) to filter on
      if (col == "VisitDate") {
        data <- dplyr::filter(data, lubridate::year(!!as.symbol(col)) %in% filter_value)  # Filtering by year
      } else if (is.character(data[[col]]) & !case_sensitive) {
        data <- dplyr::filter(data, tolower(!!as.symbol(col)) %in% tolower(filter_value))  # Case-insensitive filtering
      } else {
        data <- dplyr::filter(data, !!as.symbol(col) %in% filter_value)  # Case-sensitive and non-character filtering
      }
      if (nrow(data) == 0) {  # Stop filtering if we end up with an empty dataframe
        warning("There are no data that match all of the filters provided.")
        break
      }
    }
  }
  if (!silent & length(filter_cols) > 0) {
    col_list <- paste(cols_filtered, collapse = ", ")
    message(paste("Filtered", data_name, "on columns:", ifelse(col_list != "", col_list, "[none]")))
  }

  return(data)
}
