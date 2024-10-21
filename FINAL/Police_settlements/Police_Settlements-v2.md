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

    ## Rows: 273280 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): summary_allegations, location, state, city
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

    ## # A tibble: 6 × 10
    ##   summary_allegations               incident_date filed_date closed_date        
    ##   <chr>                             <date>        <date>     <dttm>             
    ## 1 Department: Police; Summary Alle… NA            NA         2014-09-10 00:00:00
    ## 2 Department: Police; Summary Alle… NA            NA         2014-08-21 00:00:00
    ## 3 Department: Police; Summary Alle… NA            NA         2014-07-18 00:00:00
    ## 4 Department: Police; Summary Alle… NA            NA         2014-09-19 00:00:00
    ## 5 Department: Courts; Summary Alle… NA            NA         2015-01-12 00:00:00
    ## 6 Department: Police; Summary Alle… NA            NA         2015-03-23 00:00:00
    ## # ℹ 6 more variables: amount_awarded <dbl>, location <chr>,
    ## #   calendar_year <dbl>, incident_year <dbl>, state <chr>, city <chr>

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

replace_with_keyword <- function(text) {
  # Define a named vector of patterns and their corresponding replacements
  patterns <- c(
    "civil rights|equity|constituional claim|constitutional violations|CIVIL RIGHTS CLAIMS|Civil Rights Violation" = "Civil Rights",
    "wrongful arrest|prisoner complaint common law|reversed conviction|Unjust Imprisonment Act claim" = "Wrongful Arrest",
    "Violation of constitutional rights:" = "Violation of Constitutional Rights",
    "Uncoded" = "Uncoded",
    "excessive force|brutality|Personal Injury: Exessive force|T.R.O." = "Excessive Force",
    "search & seizure|unlawful search|illegal search|seizure" = "Search & Seizure",
    "false imprisonment|False Arrest/Detention/Imprisonment|false arrest" = "False Imprisonment",
    "assault and battery|Assault/Slander/Liable|property damage|Excessive Force (Police)|Law Enforcement Related Injury - Assault & Battery/Excessive Force" = "Assault & Battery",
    "malicious prosecution" = "Malicious Prosecution",
    "negligence|negligent|failure to provide medical care|fail to render medcare|failure to prevent attempts of suicide" = "Negligence",
    "Disability/Medical Discrimination" = "Disability/Medical Discrimination",
    "police shooting|Wrongful Death" = "Police Shooting",
    "unlawful detention" = "Unlawful Detention",
    "police misconduct|officer misconduct|police activity|Police False Arrest-" = "Police/Officer Misconduct",
    "motor Vehicle Incident|Struck by Police Cruiser|car accident|Breathalyzer|auto" = "Motor Vehicle Incident",
    "failure to enforce" = "Failure to Enforce",
    "errors & omissions" = "Errors & Omissions",
    "miscellaneous|trash container|whistle blower action" = "Miscellaneous",
    "improper auto procedure|improper procedure|impropr procedure" = "Improper Auto Procedure",
    "cruel" = "Wrongful Arrest",
    "due process" = "Other",
    "DUI stop" = "Civil Rights",
    "litigation|presuit|Subrogation claim" = "Litigation",
    "sexual harrasement|sexual assault" = "Sexual Harassment",
    "personal injury|PEACE OFFICER/POLICE ACTION (PI)" = "Personal Injury",
    "declaratory judgment" = "Declaratory Judgment",
    "Employment Complaint-Common Law|(non-labor/employment)" = "Employment Complaint",
    "Violation of constitutional rights:|Amendments" = "Violation of constitutional rights",
    "Discrimination - sex" = "Race Discrimination"
  )
  
  # Iterate through the patterns and apply the replacements
  for (pattern in names(patterns)) {
    if (grepl(pattern, text, ignore.case = TRUE)) {
      return(patterns[[pattern]])
    }
  }
  
  # Handle NA values
  if (is.na(text)) {
    return("Not Reported")
  }
  
  # Return the original text if no match is found
  return(text)
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

| summary_allegations       | incident_date | filed_date | closed_date | amount_awarded | location | calendar_year | incident_year | state | city      |
|:--------------------------|:--------------|:-----------|:------------|---------------:|:---------|--------------:|--------------:|:------|:----------|
| Other                     | NA            | NA         | 2014-09-10  |        1970.04 | NA       |          2014 |            NA | GA    | Atlanta   |
| Other                     | NA            | NA         | 2014-08-21  |         179.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Other                     | NA            | NA         | 2014-07-18  |         100.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Wrongful Arrest           | NA            | NA         | 2014-09-19  |       17500.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Wrongful Arrest           | NA            | NA         | 2015-01-12  |         907.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Other                     | NA            | NA         | 2015-03-23  |        1682.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Other                     | NA            | NA         | 2015-09-22  |       20000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Wrongful Demolition       | NA            | NA         | 2015-11-24  |         538.50 | NA       |          2015 |            NA | GA    | Atlanta   |
| Wrongful Arrest           | NA            | NA         | 2016-02-03  |         500.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Other                     | 2016-07-15    | NA         | 2016-10-27  |         500.00 | NA       |          2016 |          2016 | GA    | Atlanta   |
| Civil Rights              | 2015-12-01    | NA         | 2016-10-25  |        1215.00 | NA       |          2016 |          2015 | GA    | Atlanta   |
| Wrongful Arrest           | 2015-07-20    | NA         | 2017-04-18  |        1750.00 | NA       |          2017 |          2015 | GA    | Atlanta   |
| Police Shooting           | 2017-01-29    | NA         | 2017-05-12  |         500.00 | NA       |          2017 |          2017 | GA    | Atlanta   |
| Civil Rights              | 2017-03-10    | NA         | 2017-07-12  |        1300.00 | NA       |          2017 |          2017 | GA    | Atlanta   |
| Wrongful Arrest           | 2015-02-20    | NA         | 2017-11-20  |       75000.00 | NA       |          2017 |          2015 | GA    | Atlanta   |
| Wrongful Arrest           | 2017-02-02    | NA         | 2017-10-18  |         826.72 | NA       |          2017 |          2017 | GA    | Atlanta   |
| Civil Rights              | 2017-06-22    | NA         | 2017-10-04  |       50000.00 | NA       |          2017 |          2017 | GA    | Atlanta   |
| Civil Rights              | 2016-11-20    | NA         | 2018-01-29  |         182.00 | NA       |          2018 |          2016 | GA    | Atlanta   |
| Civil Rights              | 2017-09-03    | NA         | 2018-11-15  |       19000.00 | NA       |          2018 |          2017 | GA    | Atlanta   |
| Police Shooting           | 2018-06-20    | NA         | 2018-12-26  |        1205.00 | NA       |          2018 |          2018 | GA    | Atlanta   |
| Other                     | 2018-04-25    | NA         | 2019-05-14  |         500.00 | NA       |          2019 |          2018 | GA    | Atlanta   |
| Civil Rights              | 2017-10-28    | NA         | 2019-05-28  |       50000.00 | NA       |          2019 |          2017 | GA    | Atlanta   |
| Civil Rights              | 2018-08-29    | NA         | 2019-05-28  |        6000.00 | NA       |          2019 |          2018 | GA    | Atlanta   |
| Wrongful Arrest           | 2018-09-05    | NA         | 2020-05-19  |       15000.00 | NA       |          2020 |          2018 | GA    | Atlanta   |
| Other                     | 2019-04-28    | NA         | 2019-09-26  |        1040.00 | NA       |          2019 |          2019 | GA    | Atlanta   |
| Other                     | 2019-04-02    | NA         | 2019-07-26  |         365.00 | NA       |          2019 |          2019 | GA    | Atlanta   |
| Wrongful Arrest           | 2019-09-25    | NA         | 2020-05-08  |      130000.00 | NA       |          2020 |          2019 | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-07-15  |       30000.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-08-01  |      200000.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-08-09  |      200000.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-08-06  |       47500.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-09-08  |       32000.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-11-24  |      175000.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2014-10-17  |         500.00 | NA       |          2014 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-03-24  |       25000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-03-24  |        7000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-04-01  |       20000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-05-31  |       60000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-05-01  |        7500.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2015-07-28  |      160000.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Negligence                | NA            | NA         | 2015-11-20  |         200.00 | NA       |          2015 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2016-02-17  |     2000000.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Miscellaneous             | NA            | NA         | 2016-06-14  |        2700.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2016-06-28  |      130000.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Other                     | NA            | NA         | 2016-07-14  |       20000.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2016-10-25  |       12772.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Negligence                | NA            | NA         | 2016-11-15  |       15000.00 | NA       |          2016 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2017-03-14  |        3000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2017-05-09  |       15000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2017-05-09  |      150000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2017-06-13  |      165000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Negligence                | NA            | NA         | 2017-06-27  |        6000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2017-08-10  |      174000.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Motor Vehicle Incident    | NA            | NA         | 2017-08-29  |       27500.00 | NA       |          2017 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-02-27  |       22500.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-02-27  |       18000.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-02-27  |      150000.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-01-09  |       25000.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-08-20  |       75000.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2018-08-20  |        5000.00 | NA       |          2018 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2019-04-15  |        4000.00 | NA       |          2019 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2019-05-20  |        5250.00 | NA       |          2019 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2019-09-16  |      250000.00 | NA       |          2019 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2019-09-20  |       75000.00 | NA       |          2019 |            NA | GA    | Atlanta   |
| Civil Rights              | NA            | NA         | 2019-10-21  |       48000.00 | NA       |          2019 |            NA | GA    | Atlanta   |
| False Imprisonment        | NA            | 2015-05-26 | 2015-08-25  |        6000.00 | NA       |          2015 |            NA | MD    | Baltimore |
| Civil Rights              | NA            | NA         | 2015-09-09  |     6400000.00 | NA       |          2015 |            NA | MD    | Baltimore |
| Malicious Prosecution     | NA            | 2015-06-15 | 2015-10-26  |       18000.00 | NA       |          2015 |            NA | MD    | Baltimore |
| Malicious Prosecution     | NA            | 2015-03-16 | 2015-12-17  |       15000.00 | NA       |          2015 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |       20000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |        7000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2016-02-04 | 2016-06-21  |       19000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-11-18 | 2016-07-20  |       16045.55 | NA       |          2016 |            NA | MD    | Baltimore |
| Search & Seizure          | NA            | 2015-06-24 | 2016-08-01  |       18500.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2015-06-05 | 2016-08-22  |      100000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-02-06 | 2016-09-07  |       24000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-08-13 | 2016-10-12  |      100000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-08-20 | 2016-10-19  |       24500.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-07-07 | 2016-10-21  |        6500.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Malicious Prosecution     | NA            | 2015-12-07 | 2016-10-21  |       12000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-03-26 | 2016-10-26  |       60000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2016-05-26 | 2016-11-04  |       20000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Malicious Prosecution     | NA            | 2015-09-10 | 2016-11-10  |       12000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-05-06 | 2016-12-19  |       45000.00 | NA       |          2016 |            NA | MD    | Baltimore |
| Malicious Prosecution     | NA            | 2015-08-21 | 2017-01-03  |       40000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-07-30 | 2017-01-19  |        5000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-12-29 | 2017-03-02  |       57500.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2015-10-06 | 2017-03-17  |        7500.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2016-01-07 | 2017-04-04  |        2000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2016-02-29 | 2017-04-13  |       15000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2016-02-23 | 2017-04-14  |       50000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-09-03 | 2017-04-19  |       50000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-12-10 | 2017-05-08  |       15000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2016-04-19 | 2017-05-09  |       10000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Civil Rights              | NA            | 2016-02-03 | 2017-05-17  |       20000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-12-21 | 2017-05-17  |       14000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Civil Rights              | NA            | 2016-05-31 | 2017-05-19  |      187100.00 | NA       |          2017 |            NA | MD    | Baltimore |
| False Imprisonment        | NA            | 2015-04-24 | 2017-06-02  |       24000.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2016-05-04 | 2017-06-05  |        2500.00 | NA       |          2017 |            NA | MD    | Baltimore |
| Police/Officer Misconduct | NA            | 2016-07-29 | 2017-06-22  |       65000.00 | NA       |          2017 |            NA | MD    | Baltimore |

``` r
# Filter out rows where summary_allegations have not been converted to the standardized list
standardized_list <- c("Civil Rights", "Wrongful Arrest", "Violation of Constitutional Rights", "Uncoded", 
                       "Excessive Force", "Search & Seizure", "False Imprisonment", "Assault & Battery", 
                       "Malicious Prosecution", "Negligence", "Disability/Medical Discrimination", "Police Shooting", 
                       "Unlawful Detention", "Police/Officer Misconduct", "Motor Vehicle Incident", "Failure to Enforce", 
                       "Errors & Omissions", "Miscellaneous", "Improper Auto Procedure", "Other", 
                       "Litigation", "Sexual Harassment", "Personal Injury", "Declaratory Judgment", 
                       "Employment Complaint", "Race Discrimination", "Not Reported", "Wrongful Death", 
                       "Improper Procedure", "Cruel and Unusual Punishment")

data <- data %>%
    filter(summary_allegations %in% standardized_list)

# Optional: View the first few rows to inspect the filtered data
head(data, 1000) %>% 
    knitr::kable()
```

| summary_allegations       | incident_date | filed_date | closed_date | amount_awarded | location                                            | calendar_year | incident_year | state | city       |
|:--------------------------|:--------------|:-----------|:------------|---------------:|:----------------------------------------------------|--------------:|--------------:|:------|:-----------|
| Other                     | NA            | NA         | 2014-09-10  |        1970.04 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Other                     | NA            | NA         | 2014-08-21  |         179.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Other                     | NA            | NA         | 2014-07-18  |         100.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Wrongful Arrest           | NA            | NA         | 2014-09-19  |       17500.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Wrongful Arrest           | NA            | NA         | 2015-01-12  |         907.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Other                     | NA            | NA         | 2015-03-23  |        1682.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Other                     | NA            | NA         | 2015-09-22  |       20000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Wrongful Arrest           | NA            | NA         | 2016-02-03  |         500.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Other                     | 2016-07-15    | NA         | 2016-10-27  |         500.00 | NA                                                  |          2016 |          2016 | GA    | Atlanta    |
| Civil Rights              | 2015-12-01    | NA         | 2016-10-25  |        1215.00 | NA                                                  |          2016 |          2015 | GA    | Atlanta    |
| Wrongful Arrest           | 2015-07-20    | NA         | 2017-04-18  |        1750.00 | NA                                                  |          2017 |          2015 | GA    | Atlanta    |
| Police Shooting           | 2017-01-29    | NA         | 2017-05-12  |         500.00 | NA                                                  |          2017 |          2017 | GA    | Atlanta    |
| Civil Rights              | 2017-03-10    | NA         | 2017-07-12  |        1300.00 | NA                                                  |          2017 |          2017 | GA    | Atlanta    |
| Wrongful Arrest           | 2015-02-20    | NA         | 2017-11-20  |       75000.00 | NA                                                  |          2017 |          2015 | GA    | Atlanta    |
| Wrongful Arrest           | 2017-02-02    | NA         | 2017-10-18  |         826.72 | NA                                                  |          2017 |          2017 | GA    | Atlanta    |
| Civil Rights              | 2017-06-22    | NA         | 2017-10-04  |       50000.00 | NA                                                  |          2017 |          2017 | GA    | Atlanta    |
| Civil Rights              | 2016-11-20    | NA         | 2018-01-29  |         182.00 | NA                                                  |          2018 |          2016 | GA    | Atlanta    |
| Civil Rights              | 2017-09-03    | NA         | 2018-11-15  |       19000.00 | NA                                                  |          2018 |          2017 | GA    | Atlanta    |
| Police Shooting           | 2018-06-20    | NA         | 2018-12-26  |        1205.00 | NA                                                  |          2018 |          2018 | GA    | Atlanta    |
| Other                     | 2018-04-25    | NA         | 2019-05-14  |         500.00 | NA                                                  |          2019 |          2018 | GA    | Atlanta    |
| Civil Rights              | 2017-10-28    | NA         | 2019-05-28  |       50000.00 | NA                                                  |          2019 |          2017 | GA    | Atlanta    |
| Civil Rights              | 2018-08-29    | NA         | 2019-05-28  |        6000.00 | NA                                                  |          2019 |          2018 | GA    | Atlanta    |
| Wrongful Arrest           | 2018-09-05    | NA         | 2020-05-19  |       15000.00 | NA                                                  |          2020 |          2018 | GA    | Atlanta    |
| Other                     | 2019-04-28    | NA         | 2019-09-26  |        1040.00 | NA                                                  |          2019 |          2019 | GA    | Atlanta    |
| Other                     | 2019-04-02    | NA         | 2019-07-26  |         365.00 | NA                                                  |          2019 |          2019 | GA    | Atlanta    |
| Wrongful Arrest           | 2019-09-25    | NA         | 2020-05-08  |      130000.00 | NA                                                  |          2020 |          2019 | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-07-15  |       30000.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-08-01  |      200000.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-08-09  |      200000.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-08-06  |       47500.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-09-08  |       32000.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-11-24  |      175000.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2014-10-17  |         500.00 | NA                                                  |          2014 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-03-24  |       25000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-03-24  |        7000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-04-01  |       20000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-05-31  |       60000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-05-01  |        7500.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2015-07-28  |      160000.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Negligence                | NA            | NA         | 2015-11-20  |         200.00 | NA                                                  |          2015 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2016-02-17  |     2000000.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Miscellaneous             | NA            | NA         | 2016-06-14  |        2700.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2016-06-28  |      130000.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Other                     | NA            | NA         | 2016-07-14  |       20000.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2016-10-25  |       12772.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Negligence                | NA            | NA         | 2016-11-15  |       15000.00 | NA                                                  |          2016 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2017-03-14  |        3000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2017-05-09  |       15000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2017-05-09  |      150000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2017-06-13  |      165000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Negligence                | NA            | NA         | 2017-06-27  |        6000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2017-08-10  |      174000.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Motor Vehicle Incident    | NA            | NA         | 2017-08-29  |       27500.00 | NA                                                  |          2017 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-02-27  |       22500.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-02-27  |       18000.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-02-27  |      150000.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-01-09  |       25000.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-08-20  |       75000.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2018-08-20  |        5000.00 | NA                                                  |          2018 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2019-04-15  |        4000.00 | NA                                                  |          2019 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2019-05-20  |        5250.00 | NA                                                  |          2019 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2019-09-16  |      250000.00 | NA                                                  |          2019 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2019-09-20  |       75000.00 | NA                                                  |          2019 |            NA | GA    | Atlanta    |
| Civil Rights              | NA            | NA         | 2019-10-21  |       48000.00 | NA                                                  |          2019 |            NA | GA    | Atlanta    |
| False Imprisonment        | NA            | 2015-05-26 | 2015-08-25  |        6000.00 | NA                                                  |          2015 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | NA         | 2015-09-09  |     6400000.00 | NA                                                  |          2015 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-06-15 | 2015-10-26  |       18000.00 | NA                                                  |          2015 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-03-16 | 2015-12-17  |       15000.00 | NA                                                  |          2015 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |       20000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-01-27 | 2016-01-29  |        7000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-02-04 | 2016-06-21  |       19000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-11-18 | 2016-07-20  |       16045.55 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Search & Seizure          | NA            | 2015-06-24 | 2016-08-01  |       18500.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-06-05 | 2016-08-22  |      100000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-02-06 | 2016-09-07  |       24000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-08-13 | 2016-10-12  |      100000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-08-20 | 2016-10-19  |       24500.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-07-07 | 2016-10-21  |        6500.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-12-07 | 2016-10-21  |       12000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-03-26 | 2016-10-26  |       60000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-05-26 | 2016-11-04  |       20000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-09-10 | 2016-11-10  |       12000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-05-06 | 2016-12-19  |       45000.00 | NA                                                  |          2016 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-08-21 | 2017-01-03  |       40000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-07-30 | 2017-01-19  |        5000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-12-29 | 2017-03-02  |       57500.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-10-06 | 2017-03-17  |        7500.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-01-07 | 2017-04-04  |        2000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-02-29 | 2017-04-13  |       15000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-02-23 | 2017-04-14  |       50000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-09-03 | 2017-04-19  |       50000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-12-10 | 2017-05-08  |       15000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-04-19 | 2017-05-09  |       10000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2016-02-03 | 2017-05-17  |       20000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-12-21 | 2017-05-17  |       14000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2016-05-31 | 2017-05-19  |      187100.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-04-24 | 2017-06-02  |       24000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-05-04 | 2017-06-05  |        2500.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-07-29 | 2017-06-22  |       65000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-01-11 | 2017-06-27  |       24000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2017-01-27 | 2017-06-30  |       90000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-05-01 | 2017-07-11  |       50000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-09-29 | 2017-07-14  |       15000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-03-28 | 2017-07-20  |      100000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2016-04-13 | 2017-07-24  |      400000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-05-06 | 2017-08-17  |       55000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-05-10 | 2017-08-28  |      135000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-11-18 | 2017-08-29  |       70000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Malicious Prosecution     | NA            | 2015-03-10 | 2017-08-31  |       80000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-08-01 | 2017-09-27  |       20000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-09-08 | 2017-09-28  |        7000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-03-20 | 2017-10-27  |      135000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2016-06-14 | 2017-10-31  |       20000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-08-10 | 2017-12-07  |       24000.00 | NA                                                  |          2017 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-04-24 | 2018-01-12  |        1000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2016-10-27 | 2018-01-23  |       41135.79 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-09-15 | 2018-02-01  |       15001.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-04-19 | 2018-02-05  |       75000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-12-20 | 2018-05-01  |       15000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2017-05-09 | 2018-05-10  |       20000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2016-02-04 | 2018-05-16  |       51729.91 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2017-04-24 | 2018-07-25  |        5000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-02-18 | 2018-08-14  |        9287.20 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2017-07-14 | 2018-10-16  |      144999.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2017-08-23 | 2018-11-02  |       15000.00 | NA                                                  |          2018 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2017-11-22 | 2019-01-02  |       15000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2017-12-22 | 2019-02-28  |       40000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-10-28 | 2019-02-28  |        8750.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-03-12 | 2019-03-13  |       45000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2017-08-08 | 2019-03-25  |        1200.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2017-11-22 | 2019-04-08  |       15000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| False Imprisonment        | NA            | 2015-05-15 | 2019-05-21  |        8500.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-01-03 | 2019-06-17  |       25000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-05-16 | 2019-07-09  |       20000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-04-18 | 2019-07-22  |       18000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-08-30 | 2019-08-06  |        5000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-07-05 | 2019-08-19  |       12000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2019-05-22 | 2019-09-17  |       15000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2017-10-30 | 2019-10-02  |       75000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2013-08-05 | 2019-10-16  |     8000000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-08-29 | 2019-10-30  |       17000.00 | NA                                                  |          2019 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2019-03-27 | 2020-04-20  |       20000.00 | NA                                                  |          2020 |            NA | MD    | Baltimore  |
| Civil Rights              | NA            | 2018-10-18 | 2020-04-22  |      125000.00 | NA                                                  |          2020 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2018-03-23 | 2020-04-24  |      200000.00 | NA                                                  |          2020 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2016-09-19 | 2020-04-24  |      400000.00 | NA                                                  |          2020 |            NA | MD    | Baltimore  |
| Police/Officer Misconduct | NA            | 2015-12-28 | 2020-06-02  |      261000.00 | NA                                                  |          2020 |            NA | MD    | Baltimore  |
| Not Reported              | NA            | NA         | 2010-01-14  |        2000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | NA          |       85000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2010-05-05  |        6000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2010-05-27  |       12000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2010-08-14  |       25000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2010-10-10  |       15000.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2010-12-28  |       16950.00 | NA                                                  |          2010 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2011-03-01  |       11000.00 | NA                                                  |          2011 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2011-10-28  |       34500.00 | NA                                                  |          2011 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-02-16  |     1400000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-03-05  |      170000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-04-18  |       60000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | NA          |       33000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-07-11  |        6000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-07-19  |      150000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-09-12  |       30000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-10-09  |       25000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2012-10-12  |      115000.00 | NA                                                  |          2012 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-01-25  |       20000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-05-03  |       50000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-05-03  |      239139.50 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-06-26  |       85000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | NA          |       10000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-09-20  |     3000000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-10-22  |       75000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2013-10-25  |       12000.00 | NA                                                  |          2013 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2014-03-14  |     5000000.00 | NA                                                  |          2014 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2014-05-15  |       50000.00 | NA                                                  |          2014 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2014-06-02  |      100000.00 | NA                                                  |          2014 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2014-12-12  |       25000.00 | NA                                                  |          2014 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | NA          |       75000.00 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | NA          |       18500.00 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2015-06-09  |       29000.00 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2015-06-10  |       52500.00 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2015-10-15  |        5000.00 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2015-11-18  |      179892.36 | NA                                                  |          2015 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2016-11-07  |       45000.00 | NA                                                  |          2016 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2016-03-16  |       18000.00 | NA                                                  |          2016 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2016-06-06  |      120000.00 | NA                                                  |          2016 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2016-07-15  |       12500.00 | NA                                                  |          2016 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2016-09-12  |       12500.00 | NA                                                  |          2016 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2017-05-05  |        5000.00 | NA                                                  |          2017 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2017-07-05  |        5000.00 | NA                                                  |          2017 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2017-07-06  |       60000.00 | NA                                                  |          2017 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2018-02-16  |       15000.00 | NA                                                  |          2018 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2018-08-14  |        5000.00 | NA                                                  |          2018 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2018-09-25  |       10000.00 | NA                                                  |          2018 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2018-10-30  |      250000.00 | NA                                                  |          2018 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2018-12-20  |       10000.00 | NA                                                  |          2018 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2019-01-30  |      100000.00 | NA                                                  |          2019 |            NA | MA    | Boston     |
| Not Reported              | NA            | NA         | 2019-05-19  |       15000.00 | NA                                                  |          2019 |            NA | MA    | Boston     |
| False Imprisonment        | NA            | NA         | NA          |        2500.00 | Police Van, Telephone Calls, Written Communications |            NA |            NA | MA    | Cambridge  |
| Search & Seizure          | 2006-12-18    | NA         | NA          |       50000.00 | Cambridge, MA                                       |            NA |          2006 | MA    | Cambridge  |
| Civil Rights              | 2013-11-14    | NA         | NA          |       54000.00 | Insomnia Cookies, Harvard Square, Cambridge, MA     |            NA |          2013 | MA    | Cambridge  |
| Malicious Prosecution     | 2012-06-15    | NA         | NA          |        7500.00 | Cambridgeside Galleria, Cambridge, MA               |            NA |          2012 | MA    | Cambridge  |
| Negligence                | 2011-03-14    | 2013-03-20 | 2016-06-03  |           0.00 | NA                                                  |          2016 |          2011 | SC    | Charleston |
| Civil Rights              | 2009-01-09    | 2010-12-03 | 2014-04-07  |           0.00 | NA                                                  |          2014 |          2009 | SC    | Charleston |
| Miscellaneous             | 2009-01-06    | 2010-09-29 | 2012-04-04  |       12500.00 | NA                                                  |          2012 |          2009 | SC    | Charleston |
| Wrongful Arrest           | 2013-02-15    | 2014-10-24 | 2016-04-15  |       25000.00 | NA                                                  |          2016 |          2013 | SC    | Charleston |
| Civil Rights              | 2017-06-10    | 2017-10-27 | 2019-08-08  |       65000.00 | NA                                                  |          2019 |          2017 | SC    | Charleston |
| False Imprisonment        | 2009-10-08    | 2012-02-06 | 2013-04-15  |        8000.00 | NA                                                  |          2013 |          2009 | SC    | Charleston |
| Excessive Force           | 2016-04-18    | 2016-05-16 | 2019-12-15  |        4000.00 | NA                                                  |          2019 |          2016 | SC    | Charleston |
| False Imprisonment        | 2008-09-14    | 2010-09-14 | 2012-06-18  |       23000.00 | NA                                                  |          2012 |          2008 | SC    | Charleston |
| Negligence                | 2013-03-15    | 2015-05-12 | 2017-01-06  |      150000.00 | NA                                                  |          2017 |          2013 | SC    | Charleston |
| Civil Rights              | 2009-04-27    | 2011-05-03 | 2013-02-28  |       50000.00 | NA                                                  |          2013 |          2009 | SC    | Charleston |
| Negligence                | 2011-03-14    | 2013-03-20 | 2016-06-03  |       80000.00 | NA                                                  |          2016 |          2011 | SC    | Charleston |
| False Imprisonment        | 2012-05-21    | 2014-01-23 | 2015-03-17  |        8000.00 | NA                                                  |          2015 |          2012 | SC    | Charleston |
| Search & Seizure          | 2016-02-13    | 2016-04-28 | 2017-06-13  |       10000.00 | NA                                                  |          2017 |          2016 | SC    | Charleston |
| Errors & Omissions        | 2013-03-27    | 2014-03-05 | 2015-02-18  |        5000.00 | NA                                                  |          2015 |          2013 | SC    | Charleston |
| Civil Rights              | 2013-04-16    | 2013-06-14 | 2019-06-24  |       10000.00 | NA                                                  |          2019 |          2013 | SC    | Charleston |
| False Imprisonment        | 2013-11-17    | 2015-08-11 | 2016-06-29  |        3000.00 | NA                                                  |          2016 |          2013 | SC    | Charleston |
| False Imprisonment        | 2013-06-26    | 2014-04-09 | 2015-01-29  |       10000.00 | NA                                                  |          2015 |          2013 | SC    | Charleston |
| Wrongful Arrest           | 2010-02-01    | 2010-02-09 | 2014-11-12  |      600000.00 | NA                                                  |          2014 |          2010 | SC    | Charleston |
| Search & Seizure          | 2008-12-26    | 2011-07-06 | 2013-10-11  |        6000.00 | NA                                                  |          2013 |          2008 | SC    | Charleston |
| Failure to Enforce        | 2012-04-01    | 2012-06-05 | 2013-12-05  |           0.00 | NA                                                  |          2013 |          2012 | SC    | Charleston |
| False Imprisonment        | 2014-08-27    | 2015-10-21 | 2017-02-16  |       57500.00 | NA                                                  |          2017 |          2014 | SC    | Charleston |
| False Imprisonment        | 2014-07-04    | 2016-07-25 | 2018-07-18  |       80000.00 | NA                                                  |          2018 |          2014 | SC    | Charleston |
| Miscellaneous             | 2011-08-23    | 2013-07-18 | 2015-12-23  |           0.00 | NA                                                  |          2015 |          2011 | SC    | Charleston |
| Excessive Force           | 2008-01-20    | 2010-03-05 | 2010-07-28  |        5500.00 | NA                                                  |          2010 |          2008 | SC    | Charleston |
| False Imprisonment        | 2017-11-29    | 2018-01-10 | 2019-01-24  |       85000.00 | NA                                                  |          2019 |          2017 | SC    | Charleston |
| False Imprisonment        | 2007-10-26    | 2010-02-23 | 2011-01-24  |        4000.00 | NA                                                  |          2011 |          2007 | SC    | Charleston |
| Miscellaneous             | 2012-07-06    | 2013-11-19 | 2014-12-03  |        1000.00 | NA                                                  |          2014 |          2012 | SC    | Charleston |
| False Imprisonment        | 2012-11-19    | 2014-05-07 | 2018-03-08  |      100000.00 | NA                                                  |          2018 |          2012 | SC    | Charleston |
| Other                     | 2011-08-11    | 2014-01-16 | 2014-05-05  |           0.00 | NA                                                  |          2014 |          2011 | SC    | Charleston |
| Civil Rights              | 2013-12-12    | 2014-02-06 | 2017-05-05  |           0.00 | NA                                                  |          2017 |          2013 | SC    | Charleston |
| False Imprisonment        | 2010-11-11    | 2015-04-27 | 2015-07-02  |           0.00 | NA                                                  |          2015 |          2010 | SC    | Charleston |
| Other                     | 2011-04-14    | 2014-04-29 | 2018-08-06  |           0.00 | NA                                                  |          2018 |          2011 | SC    | Charleston |
| False Imprisonment        | 2006-11-01    | 2008-07-31 | 2011-11-07  |           0.00 | NA                                                  |          2011 |          2006 | SC    | Charleston |
| Excessive Force           | 2006-10-20    | 2008-12-18 | 2010-04-19  |           0.00 | NA                                                  |          2010 |          2006 | SC    | Charleston |
| Civil Rights              | 2008-07-14    | 2009-03-27 | 2010-05-28  |           0.00 | NA                                                  |          2010 |          2008 | SC    | Charleston |
| Excessive Force           | 2007-11-25    | 2009-09-29 | 2011-06-23  |           0.00 | NA                                                  |          2011 |          2007 | SC    | Charleston |
| Search & Seizure          | 2010-02-24    | 2010-03-30 | 2011-10-03  |           0.00 | NA                                                  |          2011 |          2010 | SC    | Charleston |
| Excessive Force           | 2009-04-28    | 2010-09-14 | 2013-08-01  |           0.00 | NA                                                  |          2013 |          2009 | SC    | Charleston |
| False Imprisonment        | 2008-06-08    | 2011-04-15 | 2012-04-24  |           0.00 | NA                                                  |          2012 |          2008 | SC    | Charleston |
| False Imprisonment        | 2010-02-08    | 2011-05-24 | 2011-09-07  |           0.00 | NA                                                  |          2011 |          2010 | SC    | Charleston |
| False Imprisonment        | 2011-07-25    | 2011-10-14 | 2012-07-30  |           0.00 | NA                                                  |          2012 |          2011 | SC    | Charleston |
| Miscellaneous             | 2010-10-14    | 2012-12-12 | 2014-08-11  |           0.00 | NA                                                  |          2014 |          2010 | SC    | Charleston |
| False Imprisonment        | 2015-06-08    | 2017-08-04 | 2018-07-23  |           0.00 | NA                                                  |          2018 |          2015 | SC    | Charleston |
| False Imprisonment        | 2012-11-19    | 2014-05-07 | 2018-03-08  |       25000.00 | NA                                                  |          2018 |          2012 | SC    | Charleston |
| False Imprisonment        | 2012-12-05    | 2013-02-15 | 2014-06-04  |        5000.00 | NA                                                  |          2014 |          2012 | SC    | Charleston |
| Negligence                | 2015-05-25    | 2017-06-09 | 2019-04-15  |       22500.00 | NA                                                  |          2019 |          2015 | SC    | Charleston |
| False Imprisonment        | 2008-05-06    | 2010-05-19 | 2011-10-03  |           0.00 | NA                                                  |          2011 |          2008 | SC    | Charleston |
| False Imprisonment        | 2010-12-02    | 2012-06-15 | 2013-03-11  |       20000.00 | NA                                                  |          2013 |          2010 | SC    | Charleston |
| False Imprisonment        | 2011-06-02    | 2012-06-29 | 2014-03-19  |           0.00 | NA                                                  |          2014 |          2011 | SC    | Charleston |
| Failure to Enforce        | 2014-02-01    | 2014-07-03 | 2017-07-27  |           0.00 | NA                                                  |          2017 |          2014 | SC    | Charleston |
| Negligence                | 2009-06-28    | 2011-07-18 | 2012-09-04  |           0.00 | NA                                                  |          2012 |          2009 | SC    | Charleston |
| False Imprisonment        | 2012-07-05    | 2013-10-29 | 2015-10-30  |           0.00 | NA                                                  |          2015 |          2012 | SC    | Charleston |
| Motor Vehicle Incident    | 2005-12-10    | 2007-12-20 | 2012-07-16  |           0.00 | NA                                                  |          2012 |          2005 | SC    | Charleston |
| False Imprisonment        | 2015-01-25    | 2015-10-20 | 2017-03-29  |       40000.00 | NA                                                  |          2017 |          2015 | SC    | Charleston |
| False Imprisonment        | 2015-01-25    | 2015-10-20 | 2017-03-29  |        2500.00 | NA                                                  |          2017 |          2015 | SC    | Charleston |
| False Imprisonment        | 2015-01-25    | 2015-10-20 | 2017-03-29  |        2500.00 | NA                                                  |          2017 |          2015 | SC    | Charleston |
| Other                     | 2015-06-20    | 2016-03-10 | 2016-12-30  |         250.00 | NA                                                  |          2016 |          2015 | SC    | Charleston |
| Negligence                | 2014-05-05    | 2014-11-12 | 2015-12-21  |           0.00 | NA                                                  |          2015 |          2014 | SC    | Charleston |
| Negligence                | 2016-01-10    | 2017-09-20 | 2018-05-04  |           0.00 | NA                                                  |          2018 |          2016 | SC    | Charleston |
| Excessive Force           | NA            | NA         | 2019-01-29  |      160000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-10-02  |      100000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2019-05-30  |       70000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-09-24  |       60000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-03-18  |       99000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-03-31  |       22500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-01-28  |       16000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2010-11-19  |     2133000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-12-27  |       23000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-10-14  |     1376446.02 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-12-03  |       56750.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-05-18  |       10000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-15  |     8506616.42 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-02-26  |       51000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-03-15  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-01-06  |       18000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-02-19  |        2616.80 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2013-01-14  |      325000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-03-31  |       60000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-02-04  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-04-29  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-09  |      750000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-10-27  |        1000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-08  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-04-26  |        5500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-11-02  |        3564.33 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-10-16  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-12  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-03-11  |       15000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-07-25  |       46250.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-10-06  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-02-10  |       75000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-02-16  |       12000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-05-21  |     1500000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-05-11  |        2750.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-03-25  |       43000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-05-24  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-12  |       30000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-09-22  |      500000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-24  |         500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2013-09-30  |       60000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-04-22  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-09-23  |       30000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-11-06  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-12-17  |        2397.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2014-07-08  |        1000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-24  |        2718.73 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-24  |     1000000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-17  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-04-14  |     1616500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-05-09  |      115000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-11-07  |     2400000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2012-06-12  |      950000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-03-15  |        1150.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-22  |     1135840.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-14  |        3000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-05-14  |    16500000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-11-04  |     3500000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-07-25  |      425000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-07-25  |      575000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-04-16  |       99000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-03-11  |           1.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-30  |      315000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-06-16  |     1043750.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-06-16  |      397143.67 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-22  |      700000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-09  |     1328269.68 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2012-06-05  |       65000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2013-02-07  |    15000000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2011-12-16  |     2025000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-09-27  |       19998.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-02-20  |      125000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-10-19  |     4100000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-02-27  |       70000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2014-07-18  |     1100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2014-05-23  |      796500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-10-15  |      100000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-07-26  |      550000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-03-29  |      880000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-10-05  |      200000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-10-05  |       26000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-01-13  |     1553000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2011-09-06  |     1250000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-05-04  |      100000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-01-29  |     2200000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2014-01-21  |      485000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-08  |      430000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-08-10  |     1465000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-13  |      850000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-23  |        2500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-04-29  |        3000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2010-12-16  |      400000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2012-01-31  |     3600000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-12-03  |       21000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-08-22  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-08-30  |       60000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-20  |       85000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-11-17  |      100000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-02-21  |       45000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-11-08  |      547500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-08-26  |        9000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-11-12  |      200000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-03-12  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-12-04  |      500000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-04-29  |      244980.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-07-31  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-11-28  |        2000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-12-06  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-11-30  |      560000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-04-23  |        6000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-04-21  |       46000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-08  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-18  |       21000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-07-20  |       55000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-11-24  |       54000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-07-13  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-12-01  |        9000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-01-31  |        9000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-11-28  |      290000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-01-19  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2010-04-05  |        3000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-08-27  |       60000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-11-26  |      473000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-05  |      225000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-02-13  |      200000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2010-01-22  |       45000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2014-07-08  |        5000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-05-26  |      100000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-06-16  |      167200.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-02-02  |       15000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-08-15  |        9500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-03-16  |       65000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-09  |       99000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-06  |       72500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-12-20  |       99000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-04-14  |       10000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-03-25  |     1800000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-04-06  |       20000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-07-29  |      246618.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-09-30  |      100000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-04-09  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2010-04-15  |       35000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-12-16  |       55000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-04-04  |      392000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2010-10-28  |        3125.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-02-10  |       22642.40 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-12-23  |      122896.08 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-02-18  |      215000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2010-06-11  |       30000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |      395000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2010-10-28  |        3125.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-03-08  |       27500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-07-27  |       32500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-09-09  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-04-25  |       30000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-12-16  |     1000000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-10-21  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2010-04-13  |       35021.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-05-06  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-03-31  |      700000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-07-19  |        1000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-23  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-01-03  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2015-05-01  |      340000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2015-01-20  |       95000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-04-25  |       95000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-11-07  |     6500000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-02-06  |     4500000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-10-21  |       50000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-11-03  |       75000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-06-29  |        7500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-04-14  |       59999.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-09-09  |       50000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-01-21  |      185000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-09-02  |        5000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-02-23  |        2477.81 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2017-03-28  |      336833.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-05-20  |       40000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-06-17  |        2019.48 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-14  |       20000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-11-01  |       70000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-10-19  |        5400.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-27  |       85000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-31  |        1000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2010-03-17  |        3000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-01-27  |        8500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-01-26  |        8500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-14  |       12800.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2012-04-19  |       99999.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2012-08-20  |      250000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2012-04-10  |      250000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2010-07-23  |       80000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-04-01  |       16500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2010-12-29  |       18000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-07-18  |        6000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-02-01  |        2500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2010-01-11  |       87000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-02-02  |       25000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-22  |      150000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-03-31  |        6000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-10-19  |        1500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-07-07  |       10000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-06-03  |       55000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-06-15  |       60000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-12-02  |      150000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2011-08-10  |      125680.64 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-01-17  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-12  |      375000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-04-25  |        2500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-20  |       80000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2017-04-26  |      370000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-09-07  |       65000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-01-06  |       12000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-03-24  |        4100.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-13  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-23  |       40000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-06-30  |      143152.56 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-10-26  |        7500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-03  |       50000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-04-15  |        6500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-11-21  |      128170.79 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-10-18  |        7500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-09-23  |        3000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-10-25  |       50000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-10-20  |       15000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2014-12-16  |     7625000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-04-29  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-30  |       12000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-07-22  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-09-13  |        1000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-28  |      120000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-04-10  |       13500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-04-26  |       99000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-04-12  |        6000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-04-22  |       99000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-06-01  |      450000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-03-02  |       47500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-11-02  |        1000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-11-30  |       15000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-07-15  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-08-17  |        3000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-01-23  |       80000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-02-01  |       37500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-08-30  |       45000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-14  |        8000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-05-05  |        3500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-17  |       42500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-12  |       85000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-07-25  |       12960.86 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-03-22  |       25000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-13  |        5000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-01-21  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-19  |       44000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-27  |       75000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-18  |       75000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-14  |       30000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-02-11  |       10000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-11-22  |       98000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-09-08  |       80000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-09-08  |       25000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-03  |       96000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-09-26  |      155000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-05-12  |       14310.53 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-07-21  |       22000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-06-15  |       99000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-20  |       87250.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-02-18  |       50000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-12-20  |       17000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-06-27  |      152000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-11-17  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-03-17  |       95000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-09-25  |        5000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-03-22  |      125000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-07-20  |        3023.35 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-15  |       69500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-02-14  |      161000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-05  |      165000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-09-12  |       10000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-05-09  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-06  |       23000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-02-13  |    10250000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-08  |        7500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-03  |       20000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-12-01  |       14999.89 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-10  |      390000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-09-17  |       40000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-03-24  |      112500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-02-02  |       38000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-08-11  |       16000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-05-29  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-05-29  |      550000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-02-03  |       17500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-27  |       29000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-10-27  |       95000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-01  |       85000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-08-21  |       80000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-08-04  |       38000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-08-04  |       15250.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-12-20  |        9500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-02-03  |        7500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-17  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-12-19  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-10-17  |       25000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-03-02  |       18500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-09-16  |       25000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-11-03  |      100000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-09-28  |      330000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-09-22  |       50000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2010-08-31  |       67500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-19  |        2000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-08  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2011-12-12  |       20000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2014-06-30  |      640000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2012-01-24  |    26882946.20 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-02-17  |        4257.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-11-23  |        5000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2010-09-15  |      100000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2010-11-24  |        1500.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-04-09  |       40000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2019-07-31  |      100000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-08-18  |      558000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-23  |       12500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-29  |      409936.82 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2010-11-09  |       17000.00 | NA                                                  |          2010 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-11-09  |       50000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-11-06  |        9000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-02-08  |       18000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2019-04-18  |       75000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-02-14  |       22500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-12-12  |       18000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-01-17  |       95000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-10-25  |       85000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-02-13  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-05-05  |        4000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-05  |        1000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-05  |      185000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-12  |       31000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-05-10  |         273.18 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-06-15  |       10000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-03-30  |      325000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-12-15  |        3000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-05-10  |        9600.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-08-10  |       12000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-12-23  |       32500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-10  |      200000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-05-20  |       30000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-05-24  |        8273.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-09-07  |       22500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-05-26  |       40000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-11-04  |       25000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-11-26  |        4500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2017-08-21  |       70000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2012-10-09  |        1000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-04-26  |      117000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-02-13  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-05  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2013-05-15  |        1000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-07-27  |       15000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-10-30  |     6150000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-07-31  |      160000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-02-06  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-01-04  |       20000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-09-12  |        3000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-20  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-10-31  |        1500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-31  |        3000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-06-10  |        2000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2012-08-01  |     5375000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-23  |       99999.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-09-28  |       40000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-05-13  |       40000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-06-15  |      117000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-12-14  |       82500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-04-09  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-10-26  |       34576.32 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-09-10  |       22197.97 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-03-20  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-06-11  |       40000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-30  |     4567828.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-30  |        1000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-08-05  |        7500.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-09-29  |        8000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-07-26  |      860000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-30  |      112000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-12  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-06-21  |        1500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-01-24  |       15000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-06-18  |       99999.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-08-26  |       35000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-04-18  |       20000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-04-18  |      317712.24 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-08-11  |       15000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-07-24  |       81500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2011-08-18  |       50000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-08-30  |        2000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-02-04  |       82500.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2011-06-20  |        3000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-07-03  |       50000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-25  |       30000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-05-18  |       18000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-02-27  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-22  |         950.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-08-10  |     1300000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-08  |        3000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-09-18  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-07  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-01-25  |       12000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-10-07  |        3091.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-12-16  |       70000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-07  |       35395.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2011-05-18  |        2165.69 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-11-01  |       77500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-28  |       40000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-10-16  |     6375000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-06-06  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-25  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-07-08  |       25000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-07-27  |       20000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-01-03  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-01-05  |       67500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-17  |       75000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-03  |       25000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-27  |           1.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-03-26  |     1200000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Civil Rights              | NA            | NA         | 2011-02-14  |        8000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2011-07-20  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-11-08  |       50600.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-16  |       40000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2012-08-03  |     1800000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-13  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-29  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-28  |        7000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-08-19  |       55004.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-02-07  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-12-23  |       20000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-03-02  |       65000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-10-16  |      292474.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-04-30  |       10578.80 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-06  |        1500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-21  |       45000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-07-11  |       10000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-02-06  |       75000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-02-11  |      145000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-01-30  |       99000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-28  |       99000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-10-15  |       37197.63 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-22  |       90000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-03-09  |        1000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-02-03  |      220000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-15  |        7500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-08-23  |       16250.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-20  |       76000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2011-11-28  |        5000.00 | NA                                                  |          2011 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-04-11  |       80000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-04-10  |        5500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-06-01  |       25000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-02-08  |       55002.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-13  |      400000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-21  |       99000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-04-02  |           1.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2015-03-18  |      415000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-29  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-02-17  |       10000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-05  |       99000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-05-16  |      100000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2013-05-29  |       14682.67 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-01-12  |       99000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-09  |        1500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2017-08-17  |      184033.25 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-10-24  |       27500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2018-04-03  |       30129.25 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-06-07  |      435000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-01-30  |       30000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-03-22  |      100000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-09  |       60000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-25  |       15000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-07-17  |       88000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-28  |       70000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-14  |       10000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-10-07  |       50000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-08-12  |      190000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-02-25  |      156000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-24  |       35000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-21  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-01-23  |        3500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-14  |       80011.17 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-09-03  |        6000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-26  |      102952.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-17  |       37500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2012-04-20  |       20000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-13  |       22500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-04-03  |       50000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2012-09-12  |     3400000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-04-01  |      100000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-29  |       19000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-20  |       10000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-12-05  |        2500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2018-05-09  |         500.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-29  |        5000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-09-25  |       33000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-02-19  |     4100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-04-07  |       13000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-26  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-09-11  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-07-18  |        7500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-26  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-01-11  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-25  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-07-05  |       15001.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-12-07  |       50000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-10-22  |       29000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-06-18  |       10000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-02  |        1000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-31  |      612501.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-09-12  |        1000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-21  |       25000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-16  |       10500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2017-08-28  |       20000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-27  |       20000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2019-05-13  |      300000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-02-19  |       57500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-20  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-19  |       55000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-08-08  |    10000000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-05-27  |       20158.96 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-01-27  |      750000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2013-09-19  |     6150000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2014-08-28  |     1200000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2013-04-08  |       18000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2015-07-14  |        7250.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-05-03  |     3500000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-05-03  |         812.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-05-03  |       32802.36 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-07-14  |       99000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-12-04  |       10000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-06-20  |        1000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-01-23  |        2122.40 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-04-22  |       40000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-08  |       27570.52 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-14  |       65000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-11-30  |        3932.25 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-05-07  |        3500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-06-09  |       20000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-09-15  |      100000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-01-06  |       55000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-29  |      160001.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-28  |       70000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-08-15  |        5000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-07  |      529929.01 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2013-11-26  |        2000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-02-15  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2018-09-21  |       25001.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2019-09-11  |      335641.25 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2019-12-12  |       35000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-10-25  |       12000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-05-29  |       36215.42 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-12-05  |       15000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-06-19  |       99999.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2018-07-25  |      564349.37 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2018-07-25  |      290000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2018-09-10  |       21548.78 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2019-04-25  |       42000.00 | NA                                                  |          2019 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2014-12-22  |       27500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-05-08  |       24000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-06-14  |       50001.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-11-01  |       11000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-23  |        1000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-10-22  |       16000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-05-02  |      495000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-03  |        9000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-01-07  |       60000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-11-19  |        1500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-13  |       20000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2012-11-29  |       64000.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2012-06-12  |       15002.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-02-04  |       40000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-04-09  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-04-11  |       99999.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-11-21  |      127543.70 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-10  |       35001.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-02-10  |        2650.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-14  |       25000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-04-04  |        5000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-10-08  |       35000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-05-15  |        7143.10 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2012-10-01  |        7500.00 | NA                                                  |          2012 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-07  |       30000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-10-17  |       31000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-09-27  |       50000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-14  |       23500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-12  |        2900.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-01-08  |       62500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-11-25  |       10000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-11-02  |       92200.91 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-11-09  |       20374.18 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-09-10  |        3150.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-09-10  |       10893.75 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-09-10  |       90335.05 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-10-31  |      255349.89 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-01  |       28500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-11-20  |       20000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-04-19  |        3750.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-09-12  |      100000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-04-30  |     2000000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-08-08  |        9000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-02-04  |       45000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-10-24  |       80000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-09-03  |      126357.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-04-03  |       55000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-12-05  |      115000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-12-05  |     2250000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-12-05  |      394000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-12-05  |      720000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-04-19  |       99999.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-03-09  |       78000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-09-10  |       85000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-06-20  |       30000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-03-23  |       19000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-07-17  |       48750.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-04-12  |       20824.35 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-19  |       65000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-03-26  |       22500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-05-22  |        5000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2018-07-02  |     3500000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |      624588.43 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |      781875.04 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |     1489098.43 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |     2539832.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |     3800000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2017-09-12  |      264606.10 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-08-12  |       11000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-17  |       55000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-03-28  |        4000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-04-29  |       19000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-18  |        5000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-07-01  |       42000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-11  |       23000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-23  |        7500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-03-28  |        3750.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-11-07  |       75500.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-07-03  |       45000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-05  |       65000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-01-22  |       65000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-04-03  |        3500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-11-26  |       11000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-11-15  |         500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-11-14  |      200000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-05-07  |       25000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-03-23  |        3100.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-04-11  |       35000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-11-12  |       20000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-04-12  |        3500.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-09-12  |       75000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-12-18  |       75436.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Negligence                | NA            | NA         | 2014-09-05  |        1000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-01-24  |       25000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-03-14  |       25000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-06-08  |       70000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-05-15  |       10000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-03-24  |       20000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-11-10  |       36000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-08-15  |       70000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-11-05  |        5750.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-02-27  |       15000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-10  |      100000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-04-01  |       30000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-05-30  |       16000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2017-12-13  |      170000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2017-12-13  |     8080000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-09-29  |        2000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2018-04-05  |     8080000.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2017-10-24  |       10000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-09-30  |        1750.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-02-10  |        4500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-11-08  |       12500.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2017-10-04  |      800000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2016-02-26  |      300000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2016-04-25  |        1000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-29  |       30000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-12-04  |       10000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-01-09  |       32000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-02-25  |       14000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-13  |       90000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2014-04-01  |        3000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-04-25  |        1350.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-01-17  |       20000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-10-22  |       12500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-04-22  |     4950000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-10  |       99000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-06-30  |       13500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-07-26  |        5000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2016-02-01  |       25000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-04-22  |     1000000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-03-27  |     4500000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-20  |       10000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-04-27  |       30000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2016-01-29  |      625000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2017-12-13  |     6750000.00 | NA                                                  |          2017 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-10-24  |       75000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-27  |      925000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2015-01-21  |       25000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-02-17  |        3500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2013-10-23  |        2100.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-07-10  |       30000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-01-07  |       18500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-06-18  |     1250000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-05-23  |       16000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2016-06-24  |       67500.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-10-29  |       40000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-12-18  |       71000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-06-10  |       17500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-09-23  |       10000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-10-08  |       30000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2013-12-04  |       75000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-07-16  |       10000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-09-10  |        5175.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-11-07  |      250001.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-03-11  |       95000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2015-01-20  |        3500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2015-06-18  |     1000000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-01-15  |       18000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Malicious Prosecution     | NA            | NA         | 2014-10-28  |        5000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-12-01  |      220000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-08-28  |       35554.84 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-09-26  |       40003.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-04-04  |        1500.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2018-04-04  |       98500.00 | NA                                                  |          2018 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-04-17  |        7500.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2014-03-05  |        5000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2013-10-29  |       37094.70 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-08-25  |       75000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Wrongful Arrest           | NA            | NA         | 2015-01-13  |       80000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2014-03-18  |      202257.82 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Excessive Force           | NA            | NA         | 2016-02-24  |     1000000.00 | NA                                                  |          2016 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-02-19  |       40000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-03-12  |       35000.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |
| Police/Officer Misconduct | NA            | NA         | 2013-11-13  |       50000.00 | NA                                                  |          2013 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-03-18  |        5000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| False Imprisonment        | NA            | NA         | 2014-03-21  |       14000.00 | NA                                                  |          2014 |            NA | IL    | Chicago    |
| Search & Seizure          | NA            | NA         | 2015-11-12  |        9500.00 | NA                                                  |          2015 |            NA | IL    | Chicago    |

``` r
# Count the number of unique summary_allegations
summary_counts <- data %>%
  count(summary_allegations)

# Display the summary counts
summary_counts %>% 
  knitr::kable()
```

| summary_allegations                |     n |
|:-----------------------------------|------:|
| Assault & Battery                  |   189 |
| Civil Rights                       | 80836 |
| Declaratory Judgment               |    14 |
| Disability/Medical Discrimination  |   168 |
| Employment Complaint               |    21 |
| Errors & Omissions                 |     7 |
| Excessive Force                    |  5649 |
| Failure to Enforce                 |    21 |
| False Imprisonment                 |  6993 |
| Litigation                         |   147 |
| Malicious Prosecution              |   756 |
| Miscellaneous                      |   413 |
| Motor Vehicle Incident             |   329 |
| Negligence                         |  1435 |
| Not Reported                       | 14049 |
| Other                              |   154 |
| Personal Injury                    |   238 |
| Police Shooting                    |   455 |
| Police/Officer Misconduct          |   581 |
| Race Discrimination                |    77 |
| Search & Seizure                   |  2086 |
| Sexual Harassment                  |   175 |
| Uncoded                            |     7 |
| Violation of Constitutional Rights |   994 |
| Wrongful Arrest                    |   434 |

``` r
#show the data into a new csv file 
write.csv(data, "updated_police_settlements.csv", row.names = FALSE)
```
