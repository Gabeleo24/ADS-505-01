Setting up all data
================

``` r
# I want all data to look like this format ["plaintiff_name","claim_number","summary_allegations","incident_date","filed_date","plaintiff_attorney","closed_date","amount_awarded","location","calendar_year","incident_year","filed_year","city","state","other_expenses","collection","total_incurred","court","docket_number","matter_name","case_outcome"]
# if the data does not have a column, I want it to be NA
# if the data is not correctly formatted, I want it to change to the correct format such as [] 

# I want to create a function that will take in a data frame and return a data frame that is formatted correctly

# Define the format_data function
format_data <- function(df) {
    required_columns <- c("plaintiff_name", "claim_number", "summary_allegations", "incident_date", "filed_date", 
                          "plaintiff_attorney", "closed_date", "amount_awarded", "location", "calendar_year", 
                          "incident_year", "filed_year", "city", "state", "other_expenses", "collection", 
                          "total_incurred", "court", "docket_number", "matter_name", "case_outcome")
    
    # Add missing columns with NA values
    for (col in required_columns) {
        if (!col %in% colnames(df)) {
            df[[col]] <- NA
        }
    }
    
    # Reorder columns to match the required format
    df <- df[, required_columns]
    
    return(df)
}

# Load and format all CSV files in the specified directory
library(readr)
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
directory <- "/Users/gabrielmancillas/Desktop/ADS 505-01/FINAL/Police_settlements/Locations"
file_list <- list.files(directory, pattern = "*.csv", full.names = TRUE)

all_data <- lapply(file_list, function(file) {
    df <- read_csv(file)
    formatted_df <- format_data(df)
    return(formatted_df)
})
```

    ## Rows: 65 Columns: 24

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): claim_number, plaintiff_name, summary_allegations, claim_or_lawsu...
    ## dbl   (4): amount_awarded, calendar_year, incident_year, amount_demanded
    ## lgl   (7): filed_date, filed_year, location, other_expenses, collection, tot...
    ## date  (3): incident_date, closed_date, denied_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 82 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): matter_name, court, docket_number, plaintiff_attorney, summary_all...
    ## dbl  (3): amount_awarded, calendar_year, filed_year
    ## lgl  (7): incident_date, incident_year, other_expenses, collection, total_in...
    ## date (2): filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 51 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): matter_name, docket_number, court, plaintiff_attorney, city, state
    ## dbl   (2): amount_awarded, calendar_year
    ## lgl  (12): summary_allegations, plaintiff_name, case_outcome, filed_year, fi...
    ## date  (1): closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 4 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, incident_date_full, docket_number, court, plaintiff_n...
    ## dbl  (3): incident_year, filed_year, amount_awarded
    ## lgl  (9): calendar_year, filed_date, closed_date, other_expenses, collection...
    ## date (1): incident_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 59 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, docket_number, claim_number, plaintiff_name, plaintif...
    ## dbl  (6): calendar_year, incident_year, filed_year, amount_awarded, other_ex...
    ## lgl  (5): collection, case_outcome, court, matter_name, location
    ## date (3): incident_date, filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1515 Columns: 24
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): city, state, case_outcome, docket_number, matter_name, summary_al...
    ## dbl   (2): calendar_year, amount_awarded
    ## lgl  (12): incident_date, incident_year, filed_date, filed_year, other_expen...
    ## date  (1): closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 54 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): docket_number, matter_name, case_outcome, summary_allegations, ci...
    ## dbl   (3): amount_awarded, calendar_year, filed_year
    ## lgl  (10): incident_date, incident_year, other_expenses, collection, total_i...
    ## date  (2): filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 142 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): city, state, case_outcome, docket_number, plaintiff_name, matter_name
    ## dbl  (3): calendar_year, amount_awarded, other_expenses
    ## lgl (12): incident_date, incident_year, filed_date, filed_year, closed_date,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 30 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): plaintiff_name, defendant, docket_number, court, location, summary...
    ## dbl  (4): amount_awarded, calendar_year, incident_year, filed_year
    ## lgl  (6): other_expenses, total_incurred, claim_number, collection, matter_n...
    ## date (3): closed_date, incident_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 117120 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): plaintiff_name, claim_number, summary_allegations, plaintiff_atto...
    ## dbl   (7): amount_awarded, calendar_year, incident_year, filed_year, other_e...
    ## dttm  (1): closed_date
    ## date  (2): incident_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 371 Columns: 24
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): matter_name, longer_description, status, case_outcome, city, dock...
    ## dbl   (3): amount_awarded, calendar_year, filed_year
    ## lgl  (10): incident_date, incident_year, other_expenses, collection, total_i...
    ## date  (3): filed_date, closed_date, disposition_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 503 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): city, state, case_outcome, claim_number, matter_name, location, s...
    ## dbl   (2): calendar_year, amount_awarded
    ## lgl  (11): incident_date, incident_year, filed_date, filed_year, other_expen...
    ## date  (1): closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 58 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): city, state, claim_number, summary_allegations
    ## dbl  (6): calendar_year, incident_year, amount_awarded, other_expenses, coll...
    ## lgl  (9): filed_date, filed_year, case_outcome, docket_number, court, plaint...
    ## date (2): incident_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 237 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): matter_name, docket_number, summary_allegations, plaintiff_attorne...
    ## dbl  (5): amount_awarded, year, calendar_year, incident_year, filed_year
    ## lgl  (7): other_expenses, total_incurred, claim_number, collection, case_out...
    ## date (3): incident_date, filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 8 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, case_outcome, docket_number, court, plaintiff_name, p...
    ## dbl  (3): incident_year, filed_year, amount_awarded
    ## lgl  (7): calendar_year, closed_date, other_expenses, collection, total_incu...
    ## date (2): incident_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 997 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): fiscal_year, city, state, docket_number, claim_number, matter_name...
    ## dbl  (4): incident_year, filed_year, amount_awarded, calendar_year
    ## lgl  (7): other_expenses, collection, total_incurred, case_outcome, court, p...
    ## date (3): incident_date, filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 254 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): claim_number, plaintiff_name, matter_name, case_outcome, plaintiff...
    ## dbl  (2): calendar_year, amount_awarded
    ## lgl (12): closed_date, incident_date, incident_year, filed_date, filed_year,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 87 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, case_outcome, docket_number, claim_number, plaintiff_...
    ## dbl  (4): calendar_year, filed_year, amount_awarded, collection
    ## lgl  (7): incident_date, incident_year, other_expenses, total_incurred, cour...
    ## date (3): filed_date, closed_date, opened_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 56 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, case_outcome, docket_number, court, plaintiff_attorne...
    ## dbl  (3): calendar_year, filed_year, amount_awarded
    ## lgl  (8): incident_date, incident_year, other_expenses, collection, total_in...
    ## date (2): filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 28 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): city, state, case_outcome, docket_number, court, plaintiff_name, m...
    ## dbl  (4): calendar_year, incident_year, filed_year, amount_awarded
    ## lgl  (7): filed_date, other_expenses, collection, total_incurred, claim_numb...
    ## date (2): incident_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 32632 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): plaintiff_name, claim_number, summary_allegations, plaintiff_attor...
    ## dbl  (4): amount_awarded, calendar_year, incident_year, filed_year
    ## lgl  (7): other_expenses, collection, total_incurred, court, docket_number, ...
    ## dttm (1): closed_date
    ## date (2): incident_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 108 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): city, state, docket_number, claim_number, plaintiff_name, plaintif...
    ## dbl  (6): calendar_year, incident_year, filed_year, amount_awarded, other_ex...
    ## lgl  (5): collection, case_outcome, court, matter_name, location
    ## date (3): incident_date, filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 38 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): city, state, docket_number, plaintiff_name, location, summary_alle...
    ## dbl  (4): calendar_year, incident_year, filed_year, amount_awarded
    ## lgl  (8): other_expenses, collection, total_incurred, case_outcome, claim_nu...
    ## date (3): incident_date, filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 69 Columns: 31
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): claim_number, defendant, lawsuit_status, defendant_attorney, plai...
    ## dbl   (7): total_reimbursed, incident_year, calendar_year, filed_year, amoun...
    ## lgl   (7): matter_name, docket_number, court, case_outcome, other_expenses, ...
    ## date  (5): trial_date, incident_date, date_received, closed_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1424 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (5): city, state, docket_number, plaintiff_name, location
    ## dbl   (3): calendar_year, incident_year, amount_awarded
    ## lgl  (11): filed_date, filed_year, other_expenses, collection, total_incurre...
    ## date  (2): incident_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 10 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): court, matter_name, docket_number, summary_allegations, city, state
    ## dbl   (3): amount_awarded, calendar_year, incident_year
    ## lgl  (10): case_number, location, total_incurred, other_expenses, filed_date...
    ## date  (2): incident_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 2 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): city, state, case_outcome, court, plaintiff_name, location, summa...
    ## dbl   (3): calendar_year, incident_year, amount_awarded
    ## lgl  (10): filed_date, filed_year, closed_date, other_expenses, collection, ...
    ## date  (1): incident_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 87 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (5): city, state, docket_number, matter_name, summary_allegations
    ## dbl   (3): calendar_year, filed_year, amount_awarded
    ## lgl  (11): incident_date, incident_year, other_expenses, collection, total_i...
    ## date  (2): filed_date, closed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
    ## Rows: 40 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): city, state, case_outcome, docket_number, claim_number, court, pla...
    ## dbl  (2): calendar_year, amount_awarded
    ## lgl (12): incident_date, incident_year, filed_date, filed_year, closed_date,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 14 Columns: 24
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): city, state, case_outcome, docket_number, court, plaintiff_attorn...
    ## dbl   (4): calendar_year, incident_year, filed_year, amount_awarded
    ## lgl   (6): other_expenses, collection, total_incurred, claim_number, plainti...
    ## date  (4): incident_date, filed_date, closed_date, voucher_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 15 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): plaintiff_name, defendant, case_outcome, summary_allegations, dock...
    ## dbl  (3): amount_awarded, calendar_year, filed_year
    ## lgl  (9): matter_name, court, incident_date, location, incident_year, other_...
    ## date (2): closed_date, filed_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Combine all data frames into one
combined_data <- bind_rows(all_data)

# Save the combined data to a new CSV file
output_csv_path <- file.path(directory, "combined_data.csv")
write_csv(combined_data, output_csv_path)

# Print the path of the saved CSV file to verify
print(paste("Combined data saved to:", output_csv_path))
```

    ## [1] "Combined data saved to: /Users/gabrielmancillas/Desktop/ADS 505-01/FINAL/Police_settlements/Locations/combined_data.csv"

``` r
# Only keep the columns that are needed for the analysis
selected_columns <- c("summary_allegations", "incident_date", "filed_date", "closed_date", "amount_awarded", "location", "calendar_year", "incident_year", "state", "city")
df_selected <- combined_data %>% select(all_of(selected_columns))

# Save the selected data to a new CSV file
output_csv_path <- file.path(directory, "selected_data.csv")
write_csv(df_selected, output_csv_path)

# Print the path of the saved CSV file to verify
print(paste("Selected data saved to:", output_csv_path))
```

    ## [1] "Selected data saved to: /Users/gabrielmancillas/Desktop/ADS 505-01/FINAL/Police_settlements/Locations/selected_data.csv"
