Vignette about contacting US Treasury API
================
Sarat Bantupalli
October 10, 2023

output: <https://saratbantupalli.github.io/ST558_Project-2/> use this to
render: rmarkdown::render(input = “README.Rmd”, output_format =
“html_document”, output_file = “README.md”)

This document is a vignette to contact an API and summarize the data. I
demonstrated it by connecting to the publicly available fiscal data from
the [US Federal
Treasury](https://fiscaldata.treasury.gov/api-documentation/). The data
is open source and does not require an API key to access it.

I have built few functions to interact with some of the endpoints in the
API, extract the data and summarize it.

# Required Packages

We need the following packages to retrieve the data and summarize it.

- `tidyverse`: for data manipulation and plotting  
- `httr`: to connect to the API and obtain the data in JSON format  
- `jsonlite`: to parse data obtained in JSON data format

``` r
library(tidyverse)
library(httr)
library(jsonlite)
```

# API structure

The API has well written documentation that describes how to access
different data sets. Based on this documentation, the components that
make up a full valid API request include:

- Base URL, which is constant across all API URLs  
- Endpoint  
- Parameters and Filters (optional)

## Base URL

The base URL for all APIs is constant.

`https://api.fiscaldata.treasury.gov/services/api/fiscal_service/`

## Endpoints

List of all available data sets can be found on the [Treasury
website](https://fiscaldata.treasury.gov/datasets/). This page
summarizes how to obtain data from the endpoint of interest. The
endpoints of interest that were included here:

- average interest rate on US Treasury securities: monthly interest
  rates in different treasury securities including bonds and notes

## Parameters and Filters

This is optional part of the API URL to filter data.

Although this part of the API URL is optional, I had an issue with the
number of observations I could pull from the API. **By default, the API
limits 100 observations of data to be exchanged**. To get around this
issue the *pagination* option for the API URL was set to 10000. It gives
me a great reflection that *each API is unique*.

# API Interaction Functions

## Monthly Average Interest Rate

Here I created a function, `avg_interest` that queries the *average
interest rates on US treasury securities* API. There are 2 broad
categories of securities: marketable and non-marketable. *Marketable*
securities are the ones that the public can purchase on the open market
and might be of interest as an individual. Personally, I am interested
in this type of securities as a safe bet for investing. By creating this
function, I wanted to see the trends in marketable securities.

Using this function, the user can customize their query to include any
of the following 7 types of marketable securities: Treasury Bills,
Treasury Notes, Treasury Bonds, Treasury Inflation-Protected Securities
(TIPS), Treasury Floating Rate Notes (FRN), Federal Financing Bank, and
Total Marketable. The default type of security is *all*, where the
function outputs average interest rates for all marketable securities.

``` r
avg_interest <- function(security_type = "all") {
  # Here we are joining different parts of the API URL we are interested in.
  # The base URL for queries in the API is constant.
  base_url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
  # The Endpoint of the average interest rates API
  api_endpoint <- "v2/accounting/od/avg_interest_rates"
  # Optional Pagination field
  pagination_data <- "?page[number]=1&page[size]=10000"
  # Here the complete URL for API was constructed by pasting the base_url and api_endpoint url
  output_url <- paste0(base_url,api_endpoint,pagination_data)
  #The GET function from httr package was used to connect to the API and get the data 
  raw_data <- GET(output_url)
  # The raw data in JSON format is converted to a data frame in these 2 steps
  parsed_data <- fromJSON(rawToChar(raw_data$content))
  parsed_data <- data.frame((parsed_data$data))
  # # Their are certain observations that have a value "null". The rows with value "null" are removed then 
  # the data is filtered to only save the securities which are Marketable
  parsed_data <- parsed_data %>% filter(avg_interest_rate_amt != "null") %>% 
    filter(security_type_desc == "Marketable") %>%
    select(date = record_date, security = security_desc, 
           avg_interest = avg_interest_rate_amt)
  
  # Filtering data based on the type of security chosen by the user
  # If the user specified security_type is not equal to "all", the function returns data with the specified security_type
  if(security_type != "all") {
    parsed_data <- parsed_data %>% filter(security == security_type)
  }
  # If user specified security_type is "all", no filtering of data is done
  else {
    
  }
  
  return(parsed_data)
}
```
