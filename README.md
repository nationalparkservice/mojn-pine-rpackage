Five Needle Pine Guide
================
2024-06-21

The goal of the fiveneedlepine package is to provide QC, visualization,
and publication assistance for data from the multi-network five needle
pine protocol.

### Installation

If you already have a GitHub token for RStudio setup run the code below
to download the fiveneedlepine package. If you haven’t generated a
GitHub token for RStudio, follow the guide
[here](https://frbcesab.github.io/rsetup/chapters/github-token.html) to
set one up and then run the following code to download the
fiveneedlepine package.

``` r
# install.packages("devtools")
devtools::install_github(repo = "nationalparkservice/mojn-pine-rpackage")
```

### Metadata

Before running any of the functions in the package you should check that
the data in you pine Access database is filled out and up to date.
Instructions for filling out Access metadata can be found
[here](https://github.com/wright13/imd-fetchaccess-package).

### Functions

A short summary of the most important functions in this package. For
more information see function documentation.

- loadPine(): load five needle pine data and metadata from an Access
  database or CSVs into RStudio
- writePine(): write pine data and metadata to CSVs
- filterPine(): filter pine data
- validDataTables(): return list of valid pine data table names
- validFilters(): return list of valid filters for pine
- validMetadataTables(): return a list of valid pine metadata table
  names

In addition to these functions the fiveneedlepine package has some
quality control functions and functions that calculate various metrics.
To learn more about these functions see package documentation. The
package also has scripts for quality control and visualizations. You can
find these scripts in the script folder of the [GitHub
repo](https://github.com/nationalparkservice/mojn-pine-rpackage) or if
you cloned the GitHub repo to your desktop you can open the files from
there. Download, open, and then run the .qmd file to see the results.
