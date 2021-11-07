README
================

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# ascotraceR: A weather driven model to simulate the spread of Ascochyta blight in chickpea over a growing season

The goal of of *ascotraceR* is to develop a weather driven model to
simulate the spread of Ascochyta blight disease in a chickpea field over
a growing season.

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
remotes::install_github("IhsanKhaliq/ascotraceR",
                        build_vignettes = TRUE
)
```

Once installed you can simulate disease spread in a chickpea paddock.

Load the library.

``` r
library("ascotraceR")
library("data.table")
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

Import the weather data

``` r
# weather data
Billa_Billa <- fread(
  system.file(
    "extdata",
    "2020_Billa_Billa_weather_data_ozforecast.csv",
    package = "ascotraceR"
  )
)

# format time column
Billa_Billa[, local_time := dmy_hm(local_time)]

# specify the station coordinates of the Billa Billa weather station
Billa_Billa[, c("lat", "lon") := .(-28.1011505, 150.3307084)]

head(Billa_Billa)
```

    ##    day          local_time assessment_number mean_daily_temp wind_ km_h   ws
    ## 1:   1 2020-06-04 00:00:00                NA             4.1        0.9 0.25
    ## 2:   1 2020-06-04 00:15:00                NA             3.9        0.6 0.17
    ## 3:   1 2020-06-04 00:30:00                NA             3.9        2.0 0.56
    ## 4:   1 2020-06-04 00:45:00                NA             4.3        1.2 0.33
    ## 5:   1 2020-06-04 01:00:00                NA             3.7        2.4 0.67
    ## 6:   1 2020-06-04 01:15:00                NA             3.5        1.3 0.36
    ##    ws_sd  wd wd_sd cummulative_rain_since_9am rain_mm wet_hours    location
    ## 1:    NA 215    NA                          0       0        NA Billa_Billa
    ## 2:    NA 215    NA                          0       0        NA Billa_Billa
    ## 3:    NA 215    NA                          0       0        NA Billa_Billa
    ## 4:    NA 215    NA                          0       0        NA Billa_Billa
    ## 5:    NA 215    NA                          0       0        NA Billa_Billa
    ## 6:    NA 215    NA                          0       0        NA Billa_Billa
    ##          lat      lon
    ## 1: -28.10115 150.3307
    ## 2: -28.10115 150.3307
    ## 3: -28.10115 150.3307
    ## 4: -28.10115 150.3307
    ## 5: -28.10115 150.3307
    ## 6: -28.10115 150.3307

### Wrangle weather data

A function, `format_weather()`, is provided to convert raw weather data
into the format appropriate for the model. It is mandatory to use this
function to ensure weather data is properly formatted before running the
model.

``` r
Billa_Billa <- format_weather(
  x = Billa_Billa,
  POSIXct_time = "local_time",
  temp = "mean_daily_temp",
  ws = "ws",
  wd_sd = "wd_sd",
  rain = "rain_mm",
  wd = "wd",
  station = "location",
  time_zone = "Australia/Brisbane",
  lon = "lon",
  lat = "lat"
)
```

### Simulate Ascochyta blight spread

A function, `trace_asco()`, is provided to simulate the spread of
Ascochyta blight in a chickpea field over a growing season.

``` r
# Predict Ascochyta blight spread for the year 2020 at Billa Billa
traced <- trace_asco(
  weather = Billa_Billa,
  paddock_length = 20,
  paddock_width = 20,
  initial_infection = as.POSIXct("2020-07-17"),
  sowing_date = as.POSIXct("2020-06-04"),
  harvest_date = as.POSIXct("2020-10-27"),
  time_zone = "Australia/Brisbane",
  seeding_rate = 40,
  gp_rr = 0.0065,
  spores_per_gp_per_wet_hour = 0.6,
  #min = 0.15 - max = 0.6 (step 0.05)
  latent_period_cdd = 150,
  primary_infection_intensity = 100,
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

    ##      i_day      new_gp susceptible_gp exposed_gp infectious_gp     i_date day
    ##   1:     1 40.00000000       40.00000          0        0.0000 2020-06-04 156
    ##   2:     2  2.77147768       42.77148          0        0.0000 2020-06-05 157
    ##   3:     3  3.33508742       46.10657          0        0.0000 2020-06-06 158
    ##   4:     4  3.73065442       49.83722          0        0.0000 2020-06-07 159
    ##   5:     5  4.48312422       54.32034          0        0.0000 2020-06-08 160
    ##  ---                                                                         
    ## 143:   143  0.07730998     4980.05665          0        1.7000 2020-10-24 298
    ## 144:   144  0.05574863     4980.11240          0        1.7000 2020-10-25 299
    ## 145:   145  0.05055379     4980.16295          0        1.7000 2020-10-26 300
    ## 146:   146  0.03830102     4980.19875          0        1.7025 2020-10-27 301
    ## 147:   147  0.03495816     4980.23371          0        1.7025 2020-10-28 302
    ##             cdd cwh   cr gp_standard    AUDPC
    ##   1:    0.00000   0  0.0    40.00000 115.8462
    ##   2:   10.74583   0  0.0    42.77148 115.8462
    ##   3:   22.84583   0  0.0    46.10657 115.8462
    ##   4:   35.41042   0  0.0    49.83722 115.8462
    ##   5:   49.38958   1  0.6    54.32034 115.8462
    ##  ---                                         
    ## 143: 2133.81645  65 76.2  4980.06672 115.8462
    ## 144: 2154.64770  73 94.4  4980.12110 115.8462
    ## 145: 2176.49562  73 94.4  4980.17042 115.8462
    ## 146: 2195.63104  73 94.4  4980.20748 115.8462
    ## 147: 2215.57687  74 97.0  4980.24131 115.8462

## Reference

> Diggle AJ, Salam MU, Thomas GJ, Yang H, O’connell M, Sweetingham M,
> 2002. AnthracnoseTracer: a spatiotemporal model for simulating the
> spread of anthracnose in a lupin field. Phytopathology 92, 1110-21.
