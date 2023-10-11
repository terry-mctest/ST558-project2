ST558 Fall 2023, Project 2
================
Terry McTest
2023-10-11

<br>

This vignette focuses on a custom function that was developed for use in
conjunction with a particular API (Application Programming Interface) in
order to query, parse, and return well-structured data. After using said
function to obtain data, an exploratory data analysis will be performed.
<br>

# The Official Carbon Intensity API for Great Britain

The Carbon Intensity API provides information regarding regional carbon
intensity of the electricity system in Great Britain. Since carbon
intensity varies by hour, day, and season (due changes in demand as well
as variation in the means of generating electricity), these data are
stored at the micro-level, e.g. individual carbon intensity readings for
each half-hour period over a number of years. Per the \[National Grid’s
Carbon Intensity API website\]
(<https://carbon-intensity.github.io/api-definitions/?shell#carbon-intensity-api-v2-0-0>):

> National Grid’s Carbon Intensity API provides an indicative trend of
> regional carbon intensity of the electricity system in Great Britain
> (GB) up to 2 days ahead of real-time. It provides programmatic and
> timely access to both forecast and estimated carbon intensity data.
> The Carbon Intensity forecast includes CO2 emissions related to
> electricity generation only. The includes emissions from all large
> metered power stations, interconnector imports, transmission and
> distribution losses, and accounts for national electricity demand,
> embedded wind and solar generation.

The API consists of three primary endpoints:

- *National-level carbon intensity*. This endpoint includes a continuous
  measure of forecasted CO2 emissions per kilowatt hour of electricity
  produced; a continuous measure of actual CO2 emissions produced; and a
  categorical measure of actual CO2 emissions produced.
- *National-level electricity-generation mix*. This endpoint includes
  the percent distribution of how electricity is generated across
  various generation sources, including biomass, coal, imports, gas,
  nuclear, hydro, solar, wind, and “other”.
- *Regional-level carbon intensity & electricity-generation mix*. This
  endpoint includes (most) measures made available in the above
  endpoints, though measures in the regional-level endpoint are stored
  at a regional level (i.e. for each of 18 regions of Great Britain) as
  opposed to being stored at a national level. <br>

# Function for Returning Data From the API

Before proceeding, we should make note of the specific R packages that
are employed by either the custom function presented here, and/or by the
exploratory data analysis to follow:

``` r
library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(tidyverse)
```

<br>

A function intended to return data from the Carbon Intensity (CI) API
must be capable of navigating certain features of this API:

- The CI API needs to know the window of time for which data is desired,
  even if that “window” is essentially a specific point in time
  (e.g. “right now”) as opposed to an actual extended window.
- The CI API is comprised of data objects nested within data objects
  (e.g. data frames within data frames), and these structures must be
  un-nested and re-structured to facilitate use of the data.
- In general terms, the national data is structured as one record per
  each half-hour period contained within the specified window of time,
  while the regional data is structured with 18 records (one for each
  region) per each half-hour period contained within the specified
  window of time. The function must account for whether one or both
  types of data are desired (i.e. national-level data and/or
  regional-level data).

The function presented here addresses the above features of the CI API
by first requesting specification of the following arguments:

- `from_dt` and `to_dt`: These are required arguments, each representing
  a timestamp which bookends the window of time for which data is
  desired.
- `want_int`, `want_gen`, `want_reg_wide`, and `want_reg_long`: These
  are optional arguments through which the end user specifies (by
  setting the applicable argument equal to 1) whether they are
  requesting national-level intensity data; national-level
  generation-mix data; regional-level data (in wide format); and/or
  regional-level data (in long format).

The output of the CI API function call (named simply `carbon` as shown
here) is a list comprised of either: one wide dataset + one long
dataset; a wide dataset only; or a long dataset only. More specifically,
by selectively setting the aforementioned optional arguments to 1, the
user will receive a wide dataset comprised of either:

1.  national-level intensity data + national-level generation-mix data +
    regional-level data;
2.  national-level intensity data + national-level generation-mix data;
3.  national-level intensity data + regional-level data;
4.  national-level generation-mix data + regional-level data;
5.  national-level intensity data only;
6.  national-level generation-mix data only; or
7.  regional-level data only

Each version of the wide dataset listed above can be output with or
without an accompanying long dataset of regional-level data, for a total
of (7 x 2) + 1 = 15 data request options/combinations (the “+1” option
would be the long dataset of regional-level data with no wide dataset).
Notably, each of these 15 options/combinations could be used in
conjunction with an infinite number of time windows, providing the user
with an even greater number of options for each data request.

The `carbon` function also converts the bookend timestamps (which are
actually textstrings in “YYYY-MM-DDTHH:MMZ” format as required to access
the CI API) to simple quasi-date numeric values (e.g. YYYYMM) to
facilitate the forthcoming exploratory data analysis (though a proper
date value may be required for more rigorous analyses).

The code chunk to follow shows R syntax for the entire `carbon`
function, annotated with comments to describe the purpose of each
sub-chunk of code:

``` r
carbon <- function(from_dt, to_dt, want_int=0, want_gen=0, want_reg_wide=0, want_reg_long=0)

{
  
#national-level carbon intensity data/endpoint

if(want_int==1)
{
base = "https://api.carbonintensity.org.uk/intensity/"
slash = "/"
int_url = paste0(base, from_dt, slash, to_dt)

int_api <- GET(int_url)
parsed_int <- fromJSON(rawToChar(int_api$content))
#str(parsed_int$data)
#as_tibble(parsed_int$data)

int <-
  parsed_int$data %>%
  as_tibble() %>%
  unnest(intensity)
}

  

#national-level electricity-generation mix data/endpoint

if(want_gen==1)
{
base = "https://api.carbonintensity.org.uk/generation/"
slash = "/"
gen_url = paste0(base, from_dt, slash, to_dt)
  
gen_api <- GET(gen_url)
parsed_gen <- fromJSON(rawToChar(gen_api$content))
#str(parsed_gen$data)
#as_tibble(parsed_gen$data)

gen <-
  parsed_gen$data %>%
  as_tibble() %>%
  unnest(generationmix) %>%
  pivot_wider(names_from = fuel, values_from = perc)
}


  
#regional-level data/endpoint
  
if(want_reg_wide==1 | want_reg_long==1)
{
base = "https://api.carbonintensity.org.uk/regional/intensity/"
slash = "/"
reg_url = paste0(base, from_dt, slash, to_dt)
  
regional_api <- GET(reg_url)
parsed_regional <- fromJSON(rawToChar(regional_api$content))
#str(parsed_regional$data)
#as_tibble(parsed_regional$data)
  
reg_long <-
  parsed_regional$data %>%
  as_tibble() %>%
  unnest(regions) %>%
  unnest(intensity) %>%
  unnest(generationmix) %>%
  pivot_wider(names_from = fuel, values_from = perc)
}
  
if(want_reg_wide==1)
{
reg_wide <-
 reg_long %>%
  select(from, to, regionid, forecast, index, biomass, coal, imports, gas, nuclear, other, hydro, solar, wind) %>%
  pivot_wider(names_from = regionid, 
    values_from = c(forecast, index, biomass, coal, imports, gas, nuclear, other, hydro, solar, wind))
}
  
  

#compile the *wide* dataset for outputting
  
if(want_int==1 & want_gen==1 & want_reg_wide==1)
  {carbon_wide <- full_join(int, gen, by=c("to"="to", "from"="from"))
   carbon_wide <- full_join(carbon_wide, reg_wide, by=c("to"="to", "from"="from"))}
  
else if(want_int==1 & want_gen==1)
  {carbon_wide <- full_join(int, gen, by=c("to"="to", "from"="from"))}

else if(want_int==1 & want_reg_wide==1)
  {carbon_wide <- full_join(int, reg_wide, by=c("to"="to", "from"="from"))}
  
else if(want_gen==1 & want_reg_wide==1)
  {carbon_wide <- full_join(gen, reg_wide, by=c("to"="to", "from"="from"))}

else if(want_int==1)
  {carbon_wide <- int}
  
else if(want_gen==1)
  {carbon_wide <- gen}

else if(want_reg_wide==1)
  {carbon_wide <- reg_wide}


  

#to facilitate downstream analyses, derive a set of more-usable date/season vars

if(want_int==1 | want_gen==1 | want_reg_wide==1)
{
carbon_wide$yyyy = substring(carbon_wide$from, first=1, last=4)
carbon_wide$mm = substring(carbon_wide$from, first=6, last=7)
carbon_wide$yyyymm = paste0(carbon_wide$yyyy, carbon_wide$mm)

carbon_wide$season =
  if_else(carbon_wide$mm %in% c("04", "05", "06"), "spring",
  if_else(carbon_wide$mm %in% c("07", "08", "09"), "summer",
  if_else(carbon_wide$mm %in% c("10", "11", "12"), "fall",
  if_else(carbon_wide$mm %in% c("01", "02", "03"), "winter", NA))))
}

if(want_reg_long==1)
{
reg_long$yyyy = substring(reg_long$from, first=1, last=4)
reg_long$mm = substring(reg_long$from, first=6, last=7)
reg_long$yyyymm = paste0(reg_long$yyyy, reg_long$mm)

reg_long$season =
  if_else(reg_long$mm %in% c("04", "05", "06"), "spring",
  if_else(reg_long$mm %in% c("07", "08", "09"), "summer",
  if_else(reg_long$mm %in% c("10", "11", "12"), "fall",
  if_else(reg_long$mm %in% c("01", "02", "03"), "winter", NA))))
}
  

  
    
#final output, which is either:
#(1) a list comprised of one long dataset + one wide dataset, (2) a single wide dataset, or (3) a single long dataset
# (the long dataset will always be regional data)
  
if(want_reg_long==1 & (want_int==1 | want_gen==1 | want_reg_wide==1))
  {carbon_dat <- list(reg_long=reg_long, carbon_wide=carbon_wide)}
  
else if(want_int==1 | want_gen==1 | want_reg_wide==1)
  {carbon_dat <- carbon_wide}

else if(want_reg_long==1)
  {carbon_dat <- reg_long}
  
}
```

<br>

# Using the Custom Function to Return Data

``` r
jan2019 <- carbon(from_dt="2019-01-08T12:00Z", to_dt="2019-01-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
feb2019 <- carbon(from_dt="2019-02-08T12:00Z", to_dt="2019-02-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
mar2019 <- carbon(from_dt="2019-03-08T12:00Z", to_dt="2019-03-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
apr2019 <- carbon(from_dt="2019-04-08T12:00Z", to_dt="2019-04-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
may2019 <- carbon(from_dt="2019-05-08T12:00Z", to_dt="2019-05-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jun2019 <- carbon(from_dt="2019-06-08T12:00Z", to_dt="2019-06-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jul2019 <- carbon(from_dt="2019-07-08T12:00Z", to_dt="2019-07-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
aug2019 <- carbon(from_dt="2019-08-08T12:00Z", to_dt="2019-08-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
sep2019 <- carbon(from_dt="2019-09-08T12:00Z", to_dt="2019-09-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
oct2019 <- carbon(from_dt="2019-10-08T12:00Z", to_dt="2019-10-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
nov2019 <- carbon(from_dt="2019-11-08T12:00Z", to_dt="2019-11-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
dec2019 <- carbon(from_dt="2019-12-08T12:00Z", to_dt="2019-12-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

jan2020 <- carbon(from_dt="2020-01-08T12:00Z", to_dt="2020-01-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
feb2020 <- carbon(from_dt="2020-02-08T12:00Z", to_dt="2020-02-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
mar2020 <- carbon(from_dt="2020-03-08T12:00Z", to_dt="2020-03-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
apr2020 <- carbon(from_dt="2020-04-08T12:00Z", to_dt="2020-04-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
may2020 <- carbon(from_dt="2020-05-08T12:00Z", to_dt="2020-05-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jun2020 <- carbon(from_dt="2020-06-08T12:00Z", to_dt="2020-06-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jul2020 <- carbon(from_dt="2020-07-08T12:00Z", to_dt="2020-07-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
aug2020 <- carbon(from_dt="2020-08-08T12:00Z", to_dt="2020-08-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
sep2020 <- carbon(from_dt="2020-09-08T12:00Z", to_dt="2020-09-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
oct2020 <- carbon(from_dt="2020-10-08T12:00Z", to_dt="2020-10-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
nov2020 <- carbon(from_dt="2020-11-08T12:00Z", to_dt="2020-11-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
dec2020 <- carbon(from_dt="2020-12-08T12:00Z", to_dt="2020-12-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

jan2021 <- carbon(from_dt="2021-01-08T12:00Z", to_dt="2021-01-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
feb2021 <- carbon(from_dt="2021-02-08T12:00Z", to_dt="2021-02-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
mar2021 <- carbon(from_dt="2021-03-08T12:00Z", to_dt="2021-03-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
apr2021 <- carbon(from_dt="2021-04-08T12:00Z", to_dt="2021-04-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
may2021 <- carbon(from_dt="2021-05-08T12:00Z", to_dt="2021-05-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jun2021 <- carbon(from_dt="2021-06-08T12:00Z", to_dt="2021-06-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jul2021 <- carbon(from_dt="2021-07-08T12:00Z", to_dt="2021-07-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
aug2021 <- carbon(from_dt="2021-08-08T12:00Z", to_dt="2021-08-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
sep2021 <- carbon(from_dt="2021-09-08T12:00Z", to_dt="2021-09-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
oct2021 <- carbon(from_dt="2021-10-08T12:00Z", to_dt="2021-10-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
nov2021 <- carbon(from_dt="2021-11-08T12:00Z", to_dt="2021-11-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
dec2021 <- carbon(from_dt="2021-12-08T12:00Z", to_dt="2021-12-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

jan2022 <- carbon(from_dt="2022-01-08T12:00Z", to_dt="2022-01-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
feb2022 <- carbon(from_dt="2022-02-08T12:00Z", to_dt="2022-02-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
mar2022 <- carbon(from_dt="2022-03-08T12:00Z", to_dt="2022-03-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
apr2022 <- carbon(from_dt="2022-04-08T12:00Z", to_dt="2022-04-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
may2022 <- carbon(from_dt="2022-05-08T12:00Z", to_dt="2022-05-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jun2022 <- carbon(from_dt="2022-06-08T12:00Z", to_dt="2022-06-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
jul2022 <- carbon(from_dt="2022-07-08T12:00Z", to_dt="2022-07-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
aug2022 <- carbon(from_dt="2022-08-08T12:00Z", to_dt="2022-08-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
sep2022 <- carbon(from_dt="2022-09-08T12:00Z", to_dt="2022-09-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
oct2022 <- carbon(from_dt="2022-10-08T12:00Z", to_dt="2022-10-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
nov2022 <- carbon(from_dt="2022-11-08T12:00Z", to_dt="2022-11-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)
dec2022 <- carbon(from_dt="2022-12-08T12:00Z", to_dt="2022-12-21T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)


wide <-
  bind_rows(
  jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019,
  jan2020, feb2020, mar2020, apr2020, may2020, jun2020, jul2020, aug2020, sep2020, oct2020, nov2020, dec2020,
  jan2021, feb2021, mar2021, apr2021, may2021, jun2021, jul2021, aug2021, sep2021, oct2021, nov2021, dec2021,
  jan2022, feb2022, mar2022, apr2022, may2022, jun2022, jul2022, aug2022, sep2022, oct2022, nov2022, dec2022)
```

<br>

``` r
jan2019 <- carbon(from_dt="2019-01-08T12:00Z", to_dt="2019-01-21T12:00Z", want_reg_long=1)
feb2019 <- carbon(from_dt="2019-02-08T12:00Z", to_dt="2019-02-21T12:00Z", want_reg_long=1)
mar2019 <- carbon(from_dt="2019-03-08T12:00Z", to_dt="2019-03-21T12:00Z", want_reg_long=1)
apr2019 <- carbon(from_dt="2019-04-08T12:00Z", to_dt="2019-04-21T12:00Z", want_reg_long=1)
may2019 <- carbon(from_dt="2019-05-08T12:00Z", to_dt="2019-05-21T12:00Z", want_reg_long=1)
jun2019 <- carbon(from_dt="2019-06-08T12:00Z", to_dt="2019-06-21T12:00Z", want_reg_long=1)
jul2019 <- carbon(from_dt="2019-07-08T12:00Z", to_dt="2019-07-21T12:00Z", want_reg_long=1)
aug2019 <- carbon(from_dt="2019-08-08T12:00Z", to_dt="2019-08-21T12:00Z", want_reg_long=1)
sep2019 <- carbon(from_dt="2019-09-08T12:00Z", to_dt="2019-09-21T12:00Z", want_reg_long=1)
oct2019 <- carbon(from_dt="2019-10-08T12:00Z", to_dt="2019-10-21T12:00Z", want_reg_long=1)
nov2019 <- carbon(from_dt="2019-11-08T12:00Z", to_dt="2019-11-21T12:00Z", want_reg_long=1)
dec2019 <- carbon(from_dt="2019-12-08T12:00Z", to_dt="2019-12-21T12:00Z", want_reg_long=1)

jan2020 <- carbon(from_dt="2020-01-08T12:00Z", to_dt="2020-01-21T12:00Z", want_reg_long=1)
feb2020 <- carbon(from_dt="2020-02-08T12:00Z", to_dt="2020-02-21T12:00Z", want_reg_long=1)
mar2020 <- carbon(from_dt="2020-03-08T12:00Z", to_dt="2020-03-21T12:00Z", want_reg_long=1)
apr2020 <- carbon(from_dt="2020-04-08T12:00Z", to_dt="2020-04-21T12:00Z", want_reg_long=1)
may2020 <- carbon(from_dt="2020-05-08T12:00Z", to_dt="2020-05-21T12:00Z", want_reg_long=1)
jun2020 <- carbon(from_dt="2020-06-08T12:00Z", to_dt="2020-06-21T12:00Z", want_reg_long=1)
jul2020 <- carbon(from_dt="2020-07-08T12:00Z", to_dt="2020-07-21T12:00Z", want_reg_long=1)
aug2020 <- carbon(from_dt="2020-08-08T12:00Z", to_dt="2020-08-21T12:00Z", want_reg_long=1)
sep2020 <- carbon(from_dt="2020-09-08T12:00Z", to_dt="2020-09-21T12:00Z", want_reg_long=1)
oct2020 <- carbon(from_dt="2020-10-08T12:00Z", to_dt="2020-10-21T12:00Z", want_reg_long=1)
nov2020 <- carbon(from_dt="2020-11-08T12:00Z", to_dt="2020-11-21T12:00Z", want_reg_long=1)
dec2020 <- carbon(from_dt="2020-12-08T12:00Z", to_dt="2020-12-21T12:00Z", want_reg_long=1)

jan2021 <- carbon(from_dt="2021-01-08T12:00Z", to_dt="2021-01-21T12:00Z", want_reg_long=1)
feb2021 <- carbon(from_dt="2021-02-08T12:00Z", to_dt="2021-02-21T12:00Z", want_reg_long=1)
mar2021 <- carbon(from_dt="2021-03-08T12:00Z", to_dt="2021-03-21T12:00Z", want_reg_long=1)
apr2021 <- carbon(from_dt="2021-04-08T12:00Z", to_dt="2021-04-21T12:00Z", want_reg_long=1)
may2021 <- carbon(from_dt="2021-05-08T12:00Z", to_dt="2021-05-21T12:00Z", want_reg_long=1)
jun2021 <- carbon(from_dt="2021-06-08T12:00Z", to_dt="2021-06-21T12:00Z", want_reg_long=1)
jul2021 <- carbon(from_dt="2021-07-08T12:00Z", to_dt="2021-07-21T12:00Z", want_reg_long=1)
aug2021 <- carbon(from_dt="2021-08-08T12:00Z", to_dt="2021-08-21T12:00Z", want_reg_long=1)
sep2021 <- carbon(from_dt="2021-09-08T12:00Z", to_dt="2021-09-21T12:00Z", want_reg_long=1)
oct2021 <- carbon(from_dt="2021-10-08T12:00Z", to_dt="2021-10-21T12:00Z", want_reg_long=1)
nov2021 <- carbon(from_dt="2021-11-08T12:00Z", to_dt="2021-11-21T12:00Z", want_reg_long=1)
dec2021 <- carbon(from_dt="2021-12-08T12:00Z", to_dt="2021-12-21T12:00Z", want_reg_long=1)

jan2022 <- carbon(from_dt="2022-01-08T12:00Z", to_dt="2022-01-21T12:00Z", want_reg_long=1)
feb2022 <- carbon(from_dt="2022-02-08T12:00Z", to_dt="2022-02-21T12:00Z", want_reg_long=1)
mar2022 <- carbon(from_dt="2022-03-08T12:00Z", to_dt="2022-03-21T12:00Z", want_reg_long=1)
apr2022 <- carbon(from_dt="2022-04-08T12:00Z", to_dt="2022-04-21T12:00Z", want_reg_long=1)
may2022 <- carbon(from_dt="2022-05-08T12:00Z", to_dt="2022-05-21T12:00Z", want_reg_long=1)
jun2022 <- carbon(from_dt="2022-06-08T12:00Z", to_dt="2022-06-21T12:00Z", want_reg_long=1)
jul2022 <- carbon(from_dt="2022-07-08T12:00Z", to_dt="2022-07-21T12:00Z", want_reg_long=1)
aug2022 <- carbon(from_dt="2022-08-08T12:00Z", to_dt="2022-08-21T12:00Z", want_reg_long=1)
sep2022 <- carbon(from_dt="2022-09-08T12:00Z", to_dt="2022-09-21T12:00Z", want_reg_long=1)
oct2022 <- carbon(from_dt="2022-10-08T12:00Z", to_dt="2022-10-21T12:00Z", want_reg_long=1)
nov2022 <- carbon(from_dt="2022-11-08T12:00Z", to_dt="2022-11-21T12:00Z", want_reg_long=1)
dec2022 <- carbon(from_dt="2022-12-08T12:00Z", to_dt="2022-12-21T12:00Z", want_reg_long=1)


long <-
  bind_rows(
  jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019,
  jan2020, feb2020, mar2020, apr2020, may2020, jun2020, jul2020, aug2020, sep2020, oct2020, nov2020, dec2020,
  jan2021, feb2021, mar2021, apr2021, may2021, jun2021, jul2021, aug2021, sep2021, oct2021, nov2021, dec2021,
  jan2022, feb2022, mar2022, apr2022, may2022, jun2022, jul2022, aug2022, sep2022, oct2022, nov2022, dec2022)
```

<br>

# Exploratory Data Analysis

``` r
library(tidyverse)
#ggplot(wide, aes(x = yyyymm, y = actual, color = area_name)) + geom_line()
#ggplot(wide, aes(x = yyyymm, y = actual)) + geom_line()
```

\#scatterplot: forecast x actual (by year?) \#line chart: *actual* over
time (for national) \#line chart: *psuedo-actual* over time (for
regions) \#stacked bar: generation mix (by time, and/or by region?)
\#ggradar? generation mix (by time, and/or by region?) \#cowplot? actual
by season? (for national) \`\`\`
