Untitled
================

## GitHub Documents

``` r
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
```

    ## Downloading GitHub repo ropensci/tabulizerjars@HEAD

    ## 
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ## * checking for file ‘/private/var/folders/jw/4t4swxld5c5f_5xhv0_bzbr00000gn/T/RtmpAoDBoh/remotes86d14fec01b4/ropensci-tabulizerjars-d1924e0/DESCRIPTION’ ... OK
    ## * preparing ‘tabulizerjars’:
    ## * checking DESCRIPTION meta-information ... OK
    ## * checking for LF line-endings in source and make files and shell scripts
    ## * checking for empty or unneeded directories
    ## * building ‘tabulizerjars_1.0.1.tar.gz’

    ## Warning in i.p(...): installation of package
    ## '/var/folders/jw/4t4swxld5c5f_5xhv0_bzbr00000gn/T//RtmpAoDBoh/file86d1f63f96a/tabulizerjars_1.0.1.tar.gz'
    ## had non-zero exit status

    ## Downloading GitHub repo ropensci/tabulizer@HEAD

    ## 
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ## * checking for file ‘/private/var/folders/jw/4t4swxld5c5f_5xhv0_bzbr00000gn/T/RtmpAoDBoh/remotes86d135fd24e7/ropensci-tabulapdf-7325a8d/DESCRIPTION’ ... OK
    ## * preparing ‘tabulapdf’:
    ## * checking DESCRIPTION meta-information ... OK
    ## * checking for LF line-endings in source and make files and shell scripts
    ## * checking for empty or unneeded directories
    ## Removed empty directory ‘tabulapdf/docs’
    ## * building ‘tabulapdf_1.0.5-4.tar.gz’

    ## Warning in i.p(...): installation of package
    ## '/var/folders/jw/4t4swxld5c5f_5xhv0_bzbr00000gn/T//RtmpAoDBoh/file86d17e81d561/tabulapdf_1.0.5-4.tar.gz'
    ## had non-zero exit status

``` r
# This file should be sourced at the beginning of individual city files to
# set up the right data and output paths
# the input path should point to a local copy of the Google Drive folder
# run XX Makefile to sync your local copy with the current most updated copy of the google drive folder
rm(list=ls())

list.of.packages <- c("ggplot2", "lubridate","readxl","plyr","dplyr","tidyverse","tidylog","stringr", "this.path")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library(here)
#library(rmarkdown)
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readxl)
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats 1.0.0     ✔ stringr 1.5.1
    ## ✔ purrr   1.0.2     ✔ tibble  3.2.1
    ## ✔ readr   2.1.5     ✔ tidyr   1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::arrange()   masks plyr::arrange()
    ## ✖ purrr::compact()   masks plyr::compact()
    ## ✖ dplyr::count()     masks plyr::count()
    ## ✖ dplyr::desc()      masks plyr::desc()
    ## ✖ dplyr::failwith()  masks plyr::failwith()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::id()        masks plyr::id()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::mutate()    masks plyr::mutate()
    ## ✖ dplyr::rename()    masks plyr::rename()
    ## ✖ dplyr::summarise() masks plyr::summarise()
    ## ✖ dplyr::summarize() masks plyr::summarize()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidylog)
```

    ## Warning: package 'tidylog' was built under R version 4.3.3

    ## 
    ## Attaching package: 'tidylog'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     drop_na, fill, gather, pivot_longer, pivot_wider, replace_na,
    ##     separate_wider_delim, separate_wider_position,
    ##     separate_wider_regex, spread, uncount
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     add_count, add_tally, anti_join, count, distinct, distinct_all,
    ##     distinct_at, distinct_if, filter, filter_all, filter_at, filter_if,
    ##     full_join, group_by, group_by_all, group_by_at, group_by_if,
    ##     inner_join, left_join, mutate, mutate_all, mutate_at, mutate_if,
    ##     relocate, rename, rename_all, rename_at, rename_if, rename_with,
    ##     right_join, sample_frac, sample_n, select, select_all, select_at,
    ##     select_if, semi_join, slice, slice_head, slice_max, slice_min,
    ##     slice_sample, slice_tail, summarise, summarise_all, summarise_at,
    ##     summarise_if, summarize, summarize_all, summarize_at, summarize_if,
    ##     tally, top_frac, top_n, transmute, transmute_all, transmute_at,
    ##     transmute_if, ungroup
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     count, mutate, rename, summarise, summarize
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(stringr)
library(this.path)
```

    ## Warning: package 'this.path' was built under R version 4.3.3

    ## 
    ## Attaching package: 'this.path'
    ## 
    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

``` r
# disable scientific notation
options(scipen = 999)

# Avoid conflicts
#here = here::here
summarize = dplyr::summarize
rename = dplyr::rename
count = dplyr::count

# functions to set path in files that call setup.R
get_inpath <- function(base_path){
  original_path <- file.path(base_path, 'original/')
  intermediate_path <- file.path(base_path, 'intermediate/')
  raw_data_path <- if(dir.exists(intermediate_path)) intermediate_path else original_path
  return(raw_data_path)
}

get_outpath <- function(base_path) {
  out_data_path <- file.path(base_path, 'final/')
  return(out_data_path)
}
```

``` r
# Set up the path to the data
base_path <- "C:/Users/Owner/Documents/Police_Settlements/"
raw_data_path <- get_inpath(base_path)
out_data_path <- get_outpath(base_path)
```
