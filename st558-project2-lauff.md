ST558 Fall 2023, Project 2
================
Terry McTest
2023-10-11

<br>

This vignette focuses on a custom function that was developed to contact
a particular API (Application Programming Interface) to query, parse,
and return well-structured data. After using said function to obtain
data, an exploratory data analysis will be performed. <br>

# The Official Carbon Intensity API for Great Britain

The Carbon Intensity API provides information regarding regional carbon
intensity of the electricity system in Great Britain. Since carbon
intensity varies by hour, day, and season (due changes in demand as well
as changes in means of generating electricity), these data are stored at
the micro-level, e.g. individual carbon intensity readings for each
half-hour period over a number of years. Per the \[National Grid’s
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

The API consists of four primary endpoints:

- National-level carbon intensity
- National-level electricity-generation mix
- National statistics
- Regional-level carbon intensity & electricity-generation mix <br>

# Function for Returning Data From the API

``` r
library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
```

``` r
carbon <- function(from_dt, to_dt, want_int=0, want_gen=0, want_reg_wide=0, want_reg_long=0)

{
  
#national-level data: carbon intensity

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

  

#national-level data: generation mix

if(want_gen==1)
{
base = "https://api.carbonintensity.org.uk/generation/"
slash = "/"
gen_url = paste0(base, from_dt, slash, to_dt)
  
gen_api <- GET(gen_url)
parsed_gen <- fromJSON(rawToChar(gen_api$content))
#str(parsed_gen$data)
as_tibble(parsed_gen$data)

gen <-
  parsed_gen$data %>%
  as_tibble() %>%
  unnest(generationmix) %>%
  pivot_wider(names_from = fuel, values_from = perc)
}


  
#regional-level data
  
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


  

#to facilitate downstream analyses, derive a few more-usable date/season vars
  
carbon_wide$yyyy = substring(carbon_wide$from, first=1, last=4)
carbon_wide$mm = substring(carbon_wide$from, first=6, last=7)
carbon_wide$yyyymm = paste0(carbon_wide$yyyy, carbon_wide$mm)

carbon_wide$season =
  if_else(carbon_wide$mm %in% c("04", "05", "06"), "spring",
  if_else(carbon_wide$mm %in% c("07", "08", "09"), "summer",
  if_else(carbon_wide$mm %in% c("10", "11", "12"), "fall",
  if_else(carbon_wide$mm %in% c("01", "02", "03"), "winter", NA))))

reg_long$yyyy = substring(reg_long$from, first=1, last=4)
reg_long$mm = substring(reg_long$from, first=6, last=7)
reg_long$yyyymm = paste0(reg_long$yyyy, reg_long$mm)

reg_long$season =
  if_else(reg_long$mm %in% c("04", "05", "06"), "spring",
  if_else(reg_long$mm %in% c("07", "08", "09"), "summer",
  if_else(reg_long$mm %in% c("10", "11", "12"), "fall",
  if_else(reg_long$mm %in% c("01", "02", "03"), "winter", NA))))


  
    
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

``` r
jan2019 <- carbon(from_dt="2019-01-10T12:00Z", to_dt="2019-01-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

feb2019 <- carbon(from_dt="2019-02-10T12:00Z", to_dt="2019-02-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

mar2019 <- carbon(from_dt="2019-03-10T12:00Z", to_dt="2019-03-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

apr2019 <- carbon(from_dt="2019-04-10T12:00Z", to_dt="2019-04-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

may2019 <- carbon(from_dt="2019-05-10T12:00Z", to_dt="2019-05-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

jun2019 <- carbon(from_dt="2019-06-10T12:00Z", to_dt="2019-06-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

jul2019 <- carbon(from_dt="2019-07-10T12:00Z", to_dt="2019-07-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

aug2019 <- carbon(from_dt="2019-08-10T12:00Z", to_dt="2019-08-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

sep2019 <- carbon(from_dt="2019-09-10T12:00Z", to_dt="2019-09-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

oct2019 <- carbon(from_dt="2019-10-10T12:00Z", to_dt="2019-10-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

nov2019 <- carbon(from_dt="2019-11-10T12:00Z", to_dt="2019-11-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)

dec2019 <- carbon(from_dt="2019-12-10T12:00Z", to_dt="2019-12-20T12:00Z", want_int=1, want_gen=1, want_reg_wide=1)


year2019 <-
  bind_rows(jan2019, feb2019, mar2019, apr2019, may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019)
```

``` r
mean(year2019$forecast_1)
```

    ## [1] 100.0561

``` r
mean(year2019$forecast_2)
```

    ## [1] 33.27374

``` r
mean(year2019$forecast_3)
```

    ## [1] 63.7694

``` r
mean(year2019$forecast_4)
```

    ## [1] 45.05839

``` r
mean(year2019$forecast_5)
```

    ## [1] 268.4652

``` r
mean(year2019$forecast_6)
```

    ## [1] 236.8051

``` r
mean(year2019$forecast_7)
```

    ## [1] 338.9783

``` r
mean(year2019$forecast_8)
```

    ## [1] 180.9532

``` r
mean(year2019$forecast_9)
```

    ## [1] 363.0371

``` r
mean(year2019$forecast_10)
```

    ## [1] 149.5137

``` r
mean(year2019$forecast_11)
```

    ## [1] 129.6942

``` r
mean(year2019$forecast_12)
```

    ## [1] 258.442

``` r
mean(year2019$forecast_13)
```

    ## [1] 216.3463

``` r
mean(year2019$forecast_14)
```

    ## [1] 236.7277

``` r
mean(year2019$forecast_15)
```

    ## [1] 202.9335

``` r
mean(year2019$forecast_16)
```

    ## [1] 59.69196

``` r
mean(year2019$forecast_17)
```

    ## [1] 279.7741

``` r
mean(year2019$forecast_18)
```

    ## [1] 214.0347

``` r
year2019 <-
  year2019 %>%
  group_by(index) %>%
  mutate(mean_actual = mean(actual, na.rm = TRUE))

#year2019$yyyy = substring(year2019$from, first=1, last=4)
#year2019$mm = substring(year2019$from, first=6, last=7)
#year2019$yyyymm = paste0(year2019$yyyy, year2019$mm)

year2019 %>%
  group_by(yyyymm) %>%
  summarize(mean(actual, na.rm = TRUE))
```

    ## # A tibble: 12 × 2
    ##    yyyymm `mean(actual, na.rm = TRUE)`
    ##    <chr>                         <dbl>
    ##  1 201901                         257.
    ##  2 201902                         196.
    ##  3 201903                         182.
    ##  4 201904                         213.
    ##  5 201905                         223.
    ##  6 201906                         219.
    ##  7 201907                         231.
    ##  8 201908                         178.
    ##  9 201909                         196.
    ## 10 201910                         207.
    ## 11 201911                         264.
    ## 12 201912                         218.

``` r
tb <- table(year2019$index)
prop.table(tb)
```

    ## 
    ##        high         low    moderate   very high 
    ## 0.205994456 0.207726958 0.578135828 0.008142758

``` r
#scatterplot: forecast x actual (by year?)
#line chart: *actual* over time (for national)
#line chart: *psuedo-actual* over time (for regions)
#stacked bar: generation mix (by time, and/or by region?)
#ggradar? generation mix (by time, and/or by region?)
#cowplot?  actual by season? (for national)
```
