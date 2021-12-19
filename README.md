README
================

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![tic](https://github.com/IhsanKhaliq/ascotraceR/actions/workflows/tic.yml/badge.svg)](https://github.com/IhsanKhaliq/ascotraceR/actions/workflows/tic.yml)

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
  initial_infection = "2020-07-17",
  sowing_date = "2020-06-04",
  harvest_date = "2020-10-27",
  time_zone = "Australia/Brisbane",
  seeding_rate = 40,
  gp_rr = 0.0065,
  spores_per_gp_per_wet_hour = 0.6,
  latent_period_cdd = 150,
  primary_inoculum_intensity = 100,
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
    ##   1:     1 16000.00000       16000.00          0             0 2020-06-04 156
    ##   2:     2  1108.59107       17108.59          0             0 2020-06-05 157
    ##   3:     3  1334.03497       18442.63          0             0 2020-06-06 158
    ##   4:     4  1492.26177       19934.89          0             0 2020-06-07 159
    ##   5:     5  1793.24969       21728.14          0             0 2020-06-08 160
    ##  ---                                                                         
    ## 143:   143    31.00028     1992022.25          8           719 2020-10-24 298
    ## 144:   144    22.35445     1992044.61          9           719 2020-10-25 299
    ## 145:   145    20.27138     1992064.88          9           719 2020-10-26 300
    ## 146:   146    16.22720     1992073.11          1           727 2020-10-27 301
    ## 147:   147    14.81122     1992087.92          1           727 2020-10-28 302
    ##             cdd cwh   cr gp_standard   AUDPC
    ##   1:    0.00000   0  0.0    40.00000 48039.5
    ##   2:   10.74583   0  0.0    42.77148 48039.5
    ##   3:   22.84583   0  0.0    46.10657 48039.5
    ##   4:   35.41042   0  0.0    49.83722 48039.5
    ##   5:   49.38958   1  0.6    54.32034 48039.5
    ##  ---                                        
    ## 143: 2133.81645  65 76.2  4980.06672 48039.5
    ## 144: 2154.64770  73 94.4  4980.12110 48039.5
    ## 145: 2176.49562  73 94.4  4980.17042 48039.5
    ## 146: 2195.63104  73 94.4  4980.20748 48039.5
    ## 147: 2215.57687  74 97.0  4980.24131 48039.5

## Reference

> Diggle AJ, Salam MU, Thomas GJ, Yang H, O’connell M, Sweetingham M,
> 2002. AnthracnoseTracer: a spatiotemporal model for simulating the
> spread of anthracnose in a lupin field. Phytopathology 92, 1110-21.
