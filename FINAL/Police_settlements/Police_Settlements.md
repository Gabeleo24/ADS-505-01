Untitled
================

``` r
# Disable scientific notation
options(scipen = 999)

# Avoid conflicts
summarize <- dplyr::summarize
rename <- dplyr::rename
count <- dplyr::count

# Define the base path to your CSV files
df <- "/Users/gabrielmancillas/Desktop/ADS 505-01/FINAL/Police_settlements/Locations/selected_data.csv"
```

``` r
#load the library
library(readr)

# Load the data from the CSV file
data <- read_csv(df)
```

    ## Rows: 117120 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): summary_allegations, location, state
    ## dbl  (3): amount_awarded, calendar_year, incident_year
    ## dttm (1): closed_date
    ## date (2): incident_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Display the first few rows of the data to verify and column names
head(data)
```

    ## # A tibble: 6 × 9
    ##   summary_allegations               incident_date filed_date closed_date        
    ##   <chr>                             <date>        <date>     <dttm>             
    ## 1 Department: Police; Summary Alle… NA            NA         2014-09-10 00:00:00
    ## 2 Department: Police; Summary Alle… NA            NA         2014-08-21 00:00:00
    ## 3 Department: Police; Summary Alle… NA            NA         2014-07-18 00:00:00
    ## 4 Department: Police; Summary Alle… NA            NA         2014-09-19 00:00:00
    ## 5 Department: Courts; Summary Alle… NA            NA         2015-01-12 00:00:00
    ## 6 Department: Police; Summary Alle… NA            NA         2015-03-23 00:00:00
    ## # ℹ 5 more variables: amount_awarded <dbl>, location <chr>,
    ## #   calendar_year <dbl>, incident_year <dbl>, state <chr>

``` r
# Install dplyr if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load dplyr for piping functionality
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Define a function to check for these special keywords and replace the summary_allegations
replace_with_keyword <- function(text) {
  if (grepl("civil rights", text, ignore.case = TRUE)) {
    return("Civil Rights")
  } else if (grepl("declaratory judgment", text, ignore.case = TRUE)) {
    return("Declaratory Judgment")
  } else if (grepl("litigation", text, ignore.case = TRUE)) {
    return("Litigation")
  } else if (grepl("personal injury", text, ignore.case = TRUE)) {
    return("Personal Injury")
  } else if (grepl("negligence", text, ignore.case = TRUE)) {
    return("Negligence")
  } else if (grepl("presuit", text, ignore.case = TRUE)) {
    return("Presuit")
  } else if (grepl("brutality", text, ignore.case = TRUE)) {
    return("Brutality")
  } else if (grepl("cruel & unusual", text, ignore.case = TRUE)) {
    return("Cruel & Unusual")
  } else if (grepl("due process", text, ignore.case = TRUE)) {
    return("Due Process")
  } else if (grepl("errors & omissions", text, ignore.case = TRUE)) {
    return("Errors & Omissions")
  } else if (grepl("fail to render care", text, ignore.case = TRUE)) {
    return("Fail to Render Care")
  } else if (grepl("failure to enforce", text, ignore.case = TRUE)) {
    return("Failure to Enforce")
  } else if (grepl("false arrest", text, ignore.case = TRUE)) {
    return("False Arrest")
  } else if (grepl("improper auto procedure", text, ignore.case = TRUE)) {
    return("Improper Auto Procedure")
  } else if (grepl("miscellaneous", text, ignore.case = TRUE)) {
    return("Miscellaneous")
  } else if (grepl("negligent supervision", text, ignore.case = TRUE)) {
    return("Negligent Supervision")
  } else if (grepl("search & seizure", text, ignore.case = TRUE)) {
    return("Search & Seizure")
  } else {
    return(text)  # If no keyword is found, return the original text
  }
}

# Apply the function to the summary_allegations column
data <- data %>%
  mutate(summary_allegations = sapply(summary_allegations, replace_with_keyword))
#show the data
print(data)
```

    ## # A tibble: 117,120 × 9
    ##    summary_allegations              incident_date filed_date closed_date        
    ##    <chr>                            <date>        <date>     <dttm>             
    ##  1 Department: Police; Summary All… NA            NA         2014-09-10 00:00:00
    ##  2 Department: Police; Summary All… NA            NA         2014-08-21 00:00:00
    ##  3 Department: Police; Summary All… NA            NA         2014-07-18 00:00:00
    ##  4 Department: Police; Summary All… NA            NA         2014-09-19 00:00:00
    ##  5 Department: Courts; Summary All… NA            NA         2015-01-12 00:00:00
    ##  6 Department: Police; Summary All… NA            NA         2015-03-23 00:00:00
    ##  7 Department: Police; Summary All… NA            NA         2015-09-22 00:00:00
    ##  8 Department: Police; Summary All… NA            NA         2015-11-24 00:00:00
    ##  9 Department: Courts; Summary All… NA            NA         2016-02-03 00:00:00
    ## 10 Department: Courts; Summary All… 2016-07-15    NA         2016-10-27 00:00:00
    ## # ℹ 117,110 more rows
    ## # ℹ 5 more variables: amount_awarded <dbl>, location <chr>,
    ## #   calendar_year <dbl>, incident_year <dbl>, state <chr>

# Define the replace_with_keyword function

replace_with_keyword \<- function(text) { if (grepl(“excessive force”,
text, ignore.case = TRUE)) { return(“Excessive Force”) } else if
(grepl(“unlawful arrest”, text, ignore.case = TRUE)) { return(“Unlawful
Arrest”) } else if (grepl(“unlawful entry”, text, ignore.case = TRUE)) {
return(“Unlawful Entry”) } else if (grepl(“unlawful seizure”, text,
ignore.case = TRUE)) { return(“Unlawful Seizure”) } else if
(grepl(“malicious prosecution”, text, ignore.case = TRUE)) {
return(“Malicious Prosecution”) } else if (grepl(“42 U.S.C.\|1983 case”,
text, ignore.case = TRUE)) { return(“Civil Rights Violation”) } else if
(grepl(“4th Amendment Violations”, text, ignore.case = TRUE)) {
return(“4th Amendment Violations”) } else { return(text) \# Return the
original text if no match } }

# Apply the function to rows 1 to 42

data \<- data %\>% slice(1:42) %\>% \# Select rows 1 to 42
mutate(summary_allegations = sapply(summary_allegations,
replace_with_keyword))

# If you want to apply the transformation only to rows 1-42 and leave the rest unchanged:

combined_data \<- data %\>% mutate(summary_allegations =
ifelse(row_number() \<= 42, sapply(summary_allegations,
replace_with_keyword), summary_allegations))

# View the transformed data

table(head(combined_data\$summary_allegations, 42)) \# View the first 42
rows


    ```r
    library(dplyr)

    # Define a function to standardize summary allegations
    replace_with_keyword <- function(text) {
      if (grepl("civil rights", text, ignore.case = TRUE)) {
        return("Civil Rights")
      } else if (grepl("wrongful arrest", text, ignore.case = TRUE)) {
        return("Wrongful Arrest")
      } else if (grepl("negligence", text, ignore.case = TRUE)) {
        return("Negligence")
      } else if (grepl("police shooting", text, ignore.case = TRUE)) {
        return("Police Shooting")
      } else if (grepl("wrongful demolition", text, ignore.case = TRUE)) {
        return("Wrongful Demolition")
      } else if (grepl("other", text, ignore.case = TRUE)) {
        return("Other")
      } else {
        return(text)  # Return original if no match is found
      }
    }

    # Apply the function to the summary_allegations column
    data <- data %>%
      mutate(summary_allegations = sapply(summary_allegations, replace_with_keyword))

    # Group by the new, standardized summary_allegations and count occurrences
    grouped_data <- data %>%
      group_by(summary_allegations) %>%
      summarise(count = n())

    # View the grouped data with counts
    print(grouped_data)

    ## # A tibble: 361 × 2
    ##    summary_allegations                                                     count
    ##    <chr>                                                                   <int>
    ##  1 14-3511                                                                     3
    ##  2 2001: Excessive Force (Police)                                            189
    ##  3 2005: Unlawful Arrest (Police)                                             48
    ##  4 2010: Unlawful Entry (Police)                                               6
    ##  5 2015: Unlawful seizure of property (Police)                                 3
    ##  6 2060: Alleges Malicious Prosecution (Police)                                6
    ##  7 42 U.S.C. sec. 1983, Unlawful Arrest; Malicious Prosecution; First Ame…     3
    ##  8 4th amendment/seizure                                                       3
    ##  9 : 1983 case                                                                 3
    ## 10 : ADR-Claim 684, Bankruptcy Court Order, Chapter 9,                         3
    ## # ℹ 351 more rows

``` r
# show the entire data
print(data)
```

    ## # A tibble: 117,120 × 9
    ##    summary_allegations incident_date filed_date closed_date        
    ##    <chr>               <date>        <date>     <dttm>             
    ##  1 Other               NA            NA         2014-09-10 00:00:00
    ##  2 Other               NA            NA         2014-08-21 00:00:00
    ##  3 Other               NA            NA         2014-07-18 00:00:00
    ##  4 Wrongful Arrest     NA            NA         2014-09-19 00:00:00
    ##  5 Wrongful Arrest     NA            NA         2015-01-12 00:00:00
    ##  6 Other               NA            NA         2015-03-23 00:00:00
    ##  7 Other               NA            NA         2015-09-22 00:00:00
    ##  8 Wrongful Demolition NA            NA         2015-11-24 00:00:00
    ##  9 Wrongful Arrest     NA            NA         2016-02-03 00:00:00
    ## 10 Other               2016-07-15    NA         2016-10-27 00:00:00
    ## # ℹ 117,110 more rows
    ## # ℹ 5 more variables: amount_awarded <dbl>, location <chr>,
    ## #   calendar_year <dbl>, incident_year <dbl>, state <chr>

``` r
head(data)
```

    ## # A tibble: 6 × 9
    ##   summary_allegations incident_date filed_date closed_date        
    ##   <chr>               <date>        <date>     <dttm>             
    ## 1 Other               NA            NA         2014-09-10 00:00:00
    ## 2 Other               NA            NA         2014-08-21 00:00:00
    ## 3 Other               NA            NA         2014-07-18 00:00:00
    ## 4 Wrongful Arrest     NA            NA         2014-09-19 00:00:00
    ## 5 Wrongful Arrest     NA            NA         2015-01-12 00:00:00
    ## 6 Other               NA            NA         2015-03-23 00:00:00
    ## # ℹ 5 more variables: amount_awarded <dbl>, location <chr>,
    ## #   calendar_year <dbl>, incident_year <dbl>, state <chr>
