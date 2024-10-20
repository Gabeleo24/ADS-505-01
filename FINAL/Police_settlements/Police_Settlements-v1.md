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
library(dplyr)

# Step 1: Clean the summary_allegations column to remove unnecessary text before applying keyword replacement
data <- data %>%
  mutate(summary_allegations = gsub(".*Summary Allegations: ", "", summary_allegations))  # Remove department info

# Step 2: Define the function to standardize summary allegations
replace_with_keyword <- function(text) {
  if (grepl("civil rights|equity|constituional claim|constitutional violations", text, ignore.case = TRUE)) {
    return("Civil Rights")
  } else if (grepl("wrongful arrest|prisoner complaint common law", text, ignore.case = TRUE)) {
    return("Wrongful Arrest")
  } else if (grepl("Violation of constitutional rights:", text, ignore.case = TRUE)) {
    return("Violation of Constitutional Rights")
  } else if (grepl("Uncoded", text, ignore.case = TRUE)) {
    return("Uncoded")
  } else if (grepl("excessive force", text, ignore.case = TRUE)) {
    return("Excessive Force")
  } else if (grepl("search & seizure", text, ignore.case = TRUE) | grepl("unlawful search", text, ignore.case = TRUE)) {
    return("Search & Seizure")
  } else if (grepl("false imprisonment", text, ignore.case = TRUE)) {
    return("False Imprisonment")
  } else if (grepl("assault and battery|Assault/Slander/Liable|property damages", text, ignore.case = TRUE)) {
    return("Assault & Battery")
  } else if (grepl("malicious prosecution", text, ignore.case = TRUE)) {
    return("Malicious Prosecution")
  } else if (grepl("negligence", text, ignore.case = TRUE)) {
    return("Negligence")
  } else if (grepl("police shooting", text, ignore.case = TRUE)) {
    return("Police Shooting")
  } else if (grepl("unlawful detention", text, ignore.case = TRUE)) {
    return("Unlawful Detention")
  } else if (grepl("police misconduct|officer misconduct|police activity", text, ignore.case = TRUE)) {
    return("Police/Officer Misconduct")
  } else if (grepl("motor Vehicle Incident|Struck by Police Cruiser|car accident|Breathalyzer|auto", text, ignore.case = TRUE)) {
    return("Motor Vehicle Incident")
  } else if (grepl("failure to enforce", text, ignore.case = TRUE)) {
    return("Failure to Enforce")
} else if (grepl("errors & omissions", text, ignore.case = TRUE)) {
    return("Errors & Omissions")
  } else if (grepl("malicious prosecution", text, ignore.case = TRUE)) {
    return("Malicious Prosecution")
  } else if (grepl("negligent|failure to provide medical care|fail to render medcare|failure to prevent attempts of suicide", text, ignore.case = TRUE)) {
    return("Negligence")
  } else if (grepl("miscellaneous|trash container|whistle blower action", text, ignore.case = TRUE)) {
    return("Miscellaneous")
  } else if (grepl("improper auto procedure|improper procedure|impropr procedure", text, ignore.case = TRUE)) {
    return("Improper Auto Procedure")
  } else if (grepl("cruel", text, ignore.case = TRUE)) {
    return("Wrongful Arrest")
  } else if (grepl("brutality|Personal Injury: Exessive force|T.R.O.", text, ignore.case = TRUE)) {
    return("Excessive Force")
  } else if (grepl("false arrest|false imprisonment", text, ignore.case = TRUE)) {
    return("False Imprisonment")
  } else if (grepl("due process", text, ignore.case = TRUE)) {
    return("Other")
  } else if (grepl("DUI stop", text, ignore.case = TRUE)) {
    return("Civil Rights")
  } else if (grepl("illegal search|seizure", text, ignore.case = TRUE)) {
    return("Search & Seizure")
  } else if (grepl("reversed conviction|Unjust Imprisonment Act claim", text, ignore.case = TRUE)) {
    return("Wrongful Arrest")
  } else if (grepl("litigation|presuit|Subrogation claim", text, ignore.case = TRUE)) {
    return("Litigation")  
  } else if (grepl("sexual harrasement|sexual assault", text, ignore.case = TRUE)) {
    return("Sexual Harassment")
  } else if (grepl("personal injury", text, ignore.case = TRUE)) {
    return("Personal Injury")
  } else if (grepl("declaratory judgment", text, ignore.case = TRUE)) {
    return("Declaratory Judgment")
  } else if (grepl("Employment Complaint-Common Law", text, ignore.case = TRUE)) {
    return("Employment Complaint")
  } else if (grepl("Violation of constitutional rights", text, ignore.case = TRUE)) {
    return("Violation of constitutional rights")
  } else if (grepl("Discrimination - sex", text, ignore.case = TRUE)) {
    return("Race Discrimination")
  } else {
    return(text)  # Return the original text if no match is found
  }
}

# Step 3: Apply the function to the cleaned summary_allegations column
data <- data %>%
  mutate(summary_allegations = sapply(summary_allegations, replace_with_keyword))

# Step 4: Save the updated dataset as a new CSV file
write.csv(data, "updated_police_settlements.csv", row.names = FALSE)

# Optional: View the first few rows to inspect the saved file
head(data, 100) %>% 
  knitr::kable()
```

| summary_allegations       | incident_date | filed_date | closed_date | amount_awarded | location | calendar_year | incident_year | state |
|:--------------------------|:--------------|:-----------|:------------|---------------:|:---------|--------------:|--------------:|:------|
| Other                     | NA            | NA         | 2014-09-10  |        1970.04 | NA       |          2014 |            NA | GA    |
| Other                     | NA            | NA         | 2014-08-21  |         179.00 | NA       |          2014 |            NA | GA    |
| Other                     | NA            | NA         | 2014-07-18  |         100.00 | NA       |          2014 |            NA | GA    |
| Wrongful Arrest           | NA            | NA         | 2014-09-19  |       17500.00 | NA       |          2014 |            NA | GA    |
| Wrongful Arrest           | NA            | NA         | 2015-01-12  |         907.00 | NA       |          2015 |            NA | GA    |
| Other                     | NA            | NA         | 2015-03-23  |        1682.00 | NA       |          2015 |            NA | GA    |
| Other                     | NA            | NA         | 2015-09-22  |       20000.00 | NA       |          2015 |            NA | GA    |
| Wrongful Demolition       | NA            | NA         | 2015-11-24  |         538.50 | NA       |          2015 |            NA | GA    |
| Wrongful Arrest           | NA            | NA         | 2016-02-03  |         500.00 | NA       |          2016 |            NA | GA    |
| Other                     | 2016-07-15    | NA         | 2016-10-27  |         500.00 | NA       |          2016 |          2016 | GA    |
| Civil Rights              | 2015-12-01    | NA         | 2016-10-25  |        1215.00 | NA       |          2016 |          2015 | GA    |
| Wrongful Arrest           | 2015-07-20    | NA         | 2017-04-18  |        1750.00 | NA       |          2017 |          2015 | GA    |
| Police Shooting           | 2017-01-29    | NA         | 2017-05-12  |         500.00 | NA       |          2017 |          2017 | GA    |
| Civil Rights              | 2017-03-10    | NA         | 2017-07-12  |        1300.00 | NA       |          2017 |          2017 | GA    |
| Wrongful Arrest           | 2015-02-20    | NA         | 2017-11-20  |       75000.00 | NA       |          2017 |          2015 | GA    |
| Wrongful Arrest           | 2017-02-02    | NA         | 2017-10-18  |         826.72 | NA       |          2017 |          2017 | GA    |
| Civil Rights              | 2017-06-22    | NA         | 2017-10-04  |       50000.00 | NA       |          2017 |          2017 | GA    |
| Civil Rights              | 2016-11-20    | NA         | 2018-01-29  |         182.00 | NA       |          2018 |          2016 | GA    |
| Civil Rights              | 2017-09-03    | NA         | 2018-11-15  |       19000.00 | NA       |          2018 |          2017 | GA    |
| Police Shooting           | 2018-06-20    | NA         | 2018-12-26  |        1205.00 | NA       |          2018 |          2018 | GA    |
| Other                     | 2018-04-25    | NA         | 2019-05-14  |         500.00 | NA       |          2019 |          2018 | GA    |
| Civil Rights              | 2017-10-28    | NA         | 2019-05-28  |       50000.00 | NA       |          2019 |          2017 | GA    |
| Civil Rights              | 2018-08-29    | NA         | 2019-05-28  |        6000.00 | NA       |          2019 |          2018 | GA    |
| Wrongful Arrest           | 2018-09-05    | NA         | 2020-05-19  |       15000.00 | NA       |          2020 |          2018 | GA    |
| Other                     | 2019-04-28    | NA         | 2019-09-26  |        1040.00 | NA       |          2019 |          2019 | GA    |
| Other                     | 2019-04-02    | NA         | 2019-07-26  |         365.00 | NA       |          2019 |          2019 | GA    |
| Wrongful Arrest           | 2019-09-25    | NA         | 2020-05-08  |      130000.00 | NA       |          2020 |          2019 | GA    |
| Civil Rights              | NA            | NA         | 2014-07-15  |       30000.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-08-01  |      200000.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-08-09  |      200000.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-08-06  |       47500.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-09-08  |       32000.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-11-24  |      175000.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2014-10-17  |         500.00 | NA       |          2014 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-03-24  |       25000.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-03-24  |        7000.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-04-01  |       20000.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-05-31  |       60000.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-05-01  |        7500.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2015-07-28  |      160000.00 | NA       |          2015 |            NA | GA    |
| Negligence                | NA            | NA         | 2015-11-20  |         200.00 | NA       |          2015 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2016-02-17  |     2000000.00 | NA       |          2016 |            NA | GA    |
| Miscellaneous             | NA            | NA         | 2016-06-14  |        2700.00 | NA       |          2016 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2016-06-28  |      130000.00 | NA       |          2016 |            NA | GA    |
| Other                     | NA            | NA         | 2016-07-14  |       20000.00 | NA       |          2016 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2016-10-25  |       12772.00 | NA       |          2016 |            NA | GA    |
| Negligence                | NA            | NA         | 2016-11-15  |       15000.00 | NA       |          2016 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2017-03-14  |        3000.00 | NA       |          2017 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2017-05-09  |       15000.00 | NA       |          2017 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2017-05-09  |      150000.00 | NA       |          2017 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2017-06-13  |      165000.00 | NA       |          2017 |            NA | GA    |
| Negligence                | NA            | NA         | 2017-06-27  |        6000.00 | NA       |          2017 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2017-08-10  |      174000.00 | NA       |          2017 |            NA | GA    |
| Motor Vehicle Incident    | NA            | NA         | 2017-08-29  |       27500.00 | NA       |          2017 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-02-27  |       22500.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-02-27  |       18000.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-02-27  |      150000.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-01-09  |       25000.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-08-20  |       75000.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2018-08-20  |        5000.00 | NA       |          2018 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2019-04-15  |        4000.00 | NA       |          2019 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2019-05-20  |        5250.00 | NA       |          2019 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2019-09-16  |      250000.00 | NA       |          2019 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2019-09-20  |       75000.00 | NA       |          2019 |            NA | GA    |
| Civil Rights              | NA            | NA         | 2019-10-21  |       48000.00 | NA       |          2019 |            NA | GA    |
| Police/Officer Misconduct | NA            | 2015-05-26 | 2015-08-25  |        6000.00 | NA       |          2015 |            NA | MD    |
| Civil Rights              | NA            | NA         | 2015-09-09  |     6400000.00 | NA       |          2015 |            NA | MD    |
| Malicious Prosecution     | NA            | 2015-06-15 | 2015-10-26  |       18000.00 | NA       |          2015 |            NA | MD    |
| Malicious Prosecution     | NA            | 2015-03-16 | 2015-12-17  |       15000.00 | NA       |          2015 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |       20000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |        7000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-02-04 | 2016-06-21  |       19000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-11-18 | 2016-07-20  |       16045.55 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-06-24 | 2016-08-01  |       18500.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-06-05 | 2016-08-22  |      100000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-02-06 | 2016-09-07  |       24000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-08-13 | 2016-10-12  |      100000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-08-20 | 2016-10-19  |       24500.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-07-07 | 2016-10-21  |        6500.00 | NA       |          2016 |            NA | MD    |
| Malicious Prosecution     | NA            | 2015-12-07 | 2016-10-21  |       12000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-03-26 | 2016-10-26  |       60000.00 | NA       |          2016 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-05-26 | 2016-11-04  |       20000.00 | NA       |          2016 |            NA | MD    |
| Malicious Prosecution     | NA            | 2015-09-10 | 2016-11-10  |       12000.00 | NA       |          2016 |            NA | MD    |
| False Imprisonment        | NA            | 2015-05-06 | 2016-12-19  |       45000.00 | NA       |          2016 |            NA | MD    |
| Malicious Prosecution     | NA            | 2015-08-21 | 2017-01-03  |       40000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-07-30 | 2017-01-19  |        5000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-12-29 | 2017-03-02  |       57500.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-10-06 | 2017-03-17  |        7500.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-01-07 | 2017-04-04  |        2000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-02-29 | 2017-04-13  |       15000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-02-23 | 2017-04-14  |       50000.00 | NA       |          2017 |            NA | MD    |
| False Imprisonment        | NA            | 2015-09-03 | 2017-04-19  |       50000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-12-10 | 2017-05-08  |       15000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-04-19 | 2017-05-09  |       10000.00 | NA       |          2017 |            NA | MD    |
| Civil Rights              | NA            | 2016-02-03 | 2017-05-17  |       20000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-12-21 | 2017-05-17  |       14000.00 | NA       |          2017 |            NA | MD    |
| Civil Rights              | NA            | 2016-05-31 | 2017-05-19  |      187100.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2015-04-24 | 2017-06-02  |       24000.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-05-04 | 2017-06-05  |        2500.00 | NA       |          2017 |            NA | MD    |
| Police/Officer Misconduct | NA            | 2016-07-29 | 2017-06-22  |       65000.00 | NA       |          2017 |            NA | MD    |
