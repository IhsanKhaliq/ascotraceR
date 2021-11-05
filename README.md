README
================

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# ascotraceR: A weather driven model to simulate the spread of Ascochyta blight in chickpea over a growing season

The goal of of *ascotraceR* is to develop a weather driven model to
simulate the spread of Ascochyta blight disease in chickpea over a
growing season.

This model is adapted from a model developed by [(Diggle *et al.*
2002)](https://doi.org/10.1094/PHYTO.2002.92.10.1110) for simulating the
spread of anthracnose in a lupin field. The model is run using local
weather data, and simulates both host and pathogen related activities.
The growth of chickpea is described in terms of development of growing
points, and disease progress is measured by quantifying the loss of
healthy growing points. As the model runs, it keeps a continuous track
of non-infected, latent, infected and sporulating growing points
(lesions). The ascotraceR’s minimum input requirements are location
specific weather data and a list of input variables.

## Quick start

*ascotraceR* is not yet on CRAN. You may install it from GitHub this
way.

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("adamhsparks/epicrop",
                        build_vignettes = TRUE
)
```

Once installed you can simulate disease in a chickpea paddock.

Load the library.

``` r
library("ascotraceR")
```

Import the weather data

``` r
# weather data
Newmarracarra <- read.csv(
  system.file("extdata",
              "1998_Newmarracarra_weather_table.csv", package = "ascotraceR")
)

head(Newmarracarra)
```

    ##   day hours_in_day mean_daily_temp wet_hours rain_mm ws ws_sd wd wd_sd
    ## 1  60            9              23         0      NA NA    NA NA    NA
    ## 2  60            9              23         0      NA NA    NA NA    NA
    ## 3  60            9              23         0      NA NA    NA NA    NA
    ## 4  60            9              23         0      NA NA    NA NA    NA
    ## 5  60            9              23         0      NA NA    NA NA    NA
    ## 6  60            9              23         0      NA NA    NA NA    NA
    ##            Local.Time      Location
    ## 1 1998-03-02 15:00:00 Newmarracarra
    ## 2 1998-03-02 16:00:00 Newmarracarra
    ## 3 1998-03-02 17:00:00 Newmarracarra
    ## 4 1998-03-02 18:00:00 Newmarracarra
    ## 5 1998-03-02 19:00:00 Newmarracarra
    ## 6 1998-03-02 20:00:00 Newmarracarra

``` r
# weather station location information
station_data <-
  system.file("extdata", "stat_dat.csv", package = "ascotraceR")
```

### Wrangle weather data

A function, `format_weather()`, is provided to convert raw weather data
into the format appropriate for the model. It is mandatory to use this
function to ensure weather is properly formatted data before running the
model.

``` r
weather_dat <- format_weather(
  x = Newmarracarra,
  POSIXct_time = "Local.Time",
  temp = "mean_daily_temp",
  ws = "ws",
  wd_sd = "wd_sd",
  rain = "rain_mm",
  wd = "wd",
  station = "Location",
  time_zone = "Australia/Perth",
  lonlat_file = station_data
)
```

### Simulate Ascochyta blight spread

A function, `trace_asco()`, is provided to simulate the spread of
Ascochyta blight in a chickpea field over a growing season.

``` r
# Predict Ascochyta blight spread for the year 1988 at Newmarracarra
traced <- trace_asco(
  weather = weather_dat,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-06-10",
  sowing_date = as.POSIXct("1998-06-09"),
  harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
  time_zone = "Australia/Perth",
  primary_infection_foci = "centre"
)
```

### Summarise the output

You can easily get summary statistics for the whole paddock over the
simulated season and area under the disease progress curve, AUDPC, using
`summarise_trace()`.

``` r
summarise_trace(traced)
```

    ##      i_day    new_gp susceptible_gp exposed_gp infectious_gp     i_date day
    ##   1:     1 40.000000       40.00000          0        0.0000 1998-06-09 160
    ##   2:     2  4.900325       44.90023          0        0.0001 1998-06-10 161
    ##   3:     3  4.049094       48.94932          0        0.0001 1998-06-11 162
    ##   4:     4  5.040696       53.99001          0        0.0001 1998-06-12 163
    ##   5:     5  4.512701       58.50272          0        0.0001 1998-06-13 164
    ##  ---                                                                       
    ##  98:    98 18.093468     4754.64897          0        0.0043 1998-09-14 257
    ##  99:    99 16.815333     4771.46360          0        0.0050 1998-09-15 258
    ## 100:   100 13.015206     4784.47881          0        0.0050 1998-09-16 259
    ## 101:   101 14.685692     4799.16340          0        0.0061 1998-09-17 260
    ## 102:   102 15.898207     4815.06091          0        0.0068 1998-09-18 261
    ##       cdd cwh     cr gp_standard  AUDPC
    ##   1:    0   0   0.00    40.00000 0.1108
    ##   2:   19   9  45.99    44.90033 0.1108
    ##   3:   33  18  50.43    48.94942 0.1108
    ##   4:   49  30  75.08    53.99013 0.1108
    ##   5:   62  41  89.07    58.50284 0.1108
    ##  ---                                   
    ##  98: 1221 266 323.10  4754.65131 0.1108
    ##  99: 1233 268 323.50  4771.46642 0.1108
    ## 100: 1243 269 323.70  4784.48146 0.1108
    ## 101: 1255 269 323.70  4799.16688 0.1108
    ## 102: 1269 269 323.70  4815.06474 0.1108

## Reference

> Diggle AJ, Salam MU, Thomas GJ, Yang H, O’connell M, Sweetingham M,
> 2002. AnthracnoseTracer: a spatiotemporal model for simulating the
> spread of anthracnose in a lupin field. Phytopathology 92, 1110-21.
