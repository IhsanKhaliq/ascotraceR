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
Billa_Billa[, local_time := lubridate::dmy_hm(local_time)]

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

# calculate the number of wet_hours in a day
Billa_Billa[, wet_hours := sum(rain > 0), by = .(YYYY, MM, DD)]
```

    ## Warning in `[.data.table`(Billa_Billa, , `:=`(wet_hours, sum(rain > 0)), :
    ## Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the
    ## data.table so that := can add this new column by reference. At an earlier point,
    ## this data.table has been copied by R (or was created manually using structure()
    ## or similar). Avoid names<- and attr<- which in R currently (and oddly) may
    ## copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?
    ## setnames and ?setattr. If this message doesn't help, please report your use case
    ## to the data.table issue tracker so the root cause can be fixed or this message
    ## improved.

``` r
Billa_Billa[rain > 0, .(times, rain, wet_hours)]
```

    ##                   times rain wet_hours
    ##  1: 2020-06-07 17:00:00  0.6         1
    ##  2: 2020-06-10 18:00:00  0.8         1
    ##  3: 2020-06-14 05:00:00  0.2         4
    ##  4: 2020-06-14 06:00:00  0.4         4
    ##  5: 2020-06-14 07:00:00  0.2         4
    ##  6: 2020-06-14 08:00:00  5.8         4
    ##  7: 2020-06-21 14:00:00  1.0         1
    ##  8: 2020-07-03 16:00:00  0.2         1
    ##  9: 2020-07-11 09:00:00  0.6         3
    ## 10: 2020-07-11 10:00:00  0.2         3
    ## 11: 2020-07-11 21:00:00  0.2         3
    ## 12: 2020-07-12 05:00:00  0.2         1
    ## 13: 2020-07-25 14:00:00  0.8         5
    ## 14: 2020-07-25 16:00:00  0.2         5
    ## 15: 2020-07-25 18:00:00  1.0         5
    ## 16: 2020-07-25 19:00:00  0.8         5
    ## 17: 2020-07-25 22:00:00  0.2         5
    ## 18: 2020-07-26 05:00:00  0.2         5
    ## 19: 2020-07-26 06:00:00  1.2         5
    ## 20: 2020-07-26 07:00:00  0.2         5
    ## 21: 2020-07-26 12:00:00  0.2         5
    ## 22: 2020-07-26 13:00:00  0.2         5
    ## 23: 2020-07-29 05:00:00  0.2         2
    ## 24: 2020-07-29 09:00:00  0.2         2
    ## 25: 2020-08-07 10:00:00  3.8         5
    ## 26: 2020-08-07 11:00:00  8.0         5
    ## 27: 2020-08-07 12:00:00  3.6         5
    ## 28: 2020-08-07 13:00:00  1.2         5
    ## 29: 2020-08-07 14:00:00  0.6         5
    ## 30: 2020-08-09 22:00:00  0.2         1
    ## 31: 2020-08-14 21:00:00  1.4         3
    ## 32: 2020-08-14 22:00:00  2.4         3
    ## 33: 2020-08-14 23:00:00  1.8         3
    ## 34: 2020-08-15 00:00:00  1.8         4
    ## 35: 2020-08-15 01:00:00  0.6         4
    ## 36: 2020-08-15 02:00:00  2.0         4
    ## 37: 2020-08-15 03:00:00  0.8         4
    ## 38: 2020-08-16 06:00:00  0.2         1
    ## 39: 2020-08-17 05:00:00  0.2         1
    ## 40: 2020-09-19 17:00:00  0.2         2
    ## 41: 2020-09-19 18:00:00  0.2         2
    ## 42: 2020-09-20 08:00:00  0.2         1
    ## 43: 2020-09-21 16:00:00  0.4         4
    ## 44: 2020-09-21 17:00:00  0.2         4
    ## 45: 2020-09-21 22:00:00  1.6         4
    ## 46: 2020-09-21 23:00:00  0.4         4
    ## 47: 2020-09-30 10:00:00  0.4         4
    ## 48: 2020-09-30 11:00:00  0.2         4
    ## 49: 2020-09-30 15:00:00  0.6         4
    ## 50: 2020-09-30 16:00:00  0.4         4
    ## 51: 2020-10-12 15:00:00  7.6         4
    ## 52: 2020-10-12 16:00:00  4.2         4
    ## 53: 2020-10-12 17:00:00  1.6         4
    ## 54: 2020-10-12 21:00:00  0.2         4
    ## 55: 2020-10-18 16:00:00  0.4         6
    ## 56: 2020-10-18 17:00:00  1.0         6
    ## 57: 2020-10-18 19:00:00  2.6         6
    ## 58: 2020-10-18 20:00:00  0.6         6
    ## 59: 2020-10-18 21:00:00  1.0         6
    ## 60: 2020-10-18 22:00:00  0.4         6
    ## 61: 2020-10-19 05:00:00  0.2         1
    ## 62: 2020-10-22 00:00:00  5.6         2
    ## 63: 2020-10-22 06:00:00  0.2         2
    ## 64: 2020-10-23 22:00:00  1.0         2
    ## 65: 2020-10-23 23:00:00  0.4         2
    ## 66: 2020-10-24 00:00:00  1.4         8
    ## 67: 2020-10-24 01:00:00  9.8         8
    ## 68: 2020-10-24 02:00:00  0.6         8
    ## 69: 2020-10-24 03:00:00  2.6         8
    ## 70: 2020-10-24 04:00:00  2.8         8
    ## 71: 2020-10-24 08:00:00  0.4         8
    ## 72: 2020-10-24 13:00:00  0.2         8
    ## 73: 2020-10-24 18:00:00  0.4         8
    ## 74: 2020-10-27 13:00:00  2.6         1
    ##                   times rain wet_hours

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
    ## 143:   143  0.07769936     4980.05457          0        1.8625 2020-10-24 298
    ## 144:   144  0.05602949     4980.11060          0        1.8625 2020-10-25 299
    ## 145:   145  0.05080854     4980.16141          0        1.8625 2020-10-26 300
    ## 146:   146  0.03880323     4980.19521          0        1.8675 2020-10-27 301
    ## 147:   147  0.03541661     4980.23063          0        1.8675 2020-10-28 302
    ##             cdd cwh   cr gp_standard    AUDPC
    ##   1:    0.00000   0  0.0    40.00000 125.1187
    ##   2:   10.74583   0  0.0    42.77148 125.1187
    ##   3:   22.84583   0  0.0    46.10657 125.1187
    ##   4:   35.41042   0  0.0    49.83722 125.1187
    ##   5:   49.38958   1  0.6    54.32034 125.1187
    ##  ---                                         
    ## 143: 2133.81645  65 76.2  4980.06672 125.1187
    ## 144: 2154.64770  73 94.4  4980.12110 125.1187
    ## 145: 2176.49562  73 94.4  4980.17042 125.1187
    ## 146: 2195.63104  73 94.4  4980.20748 125.1187
    ## 147: 2215.57687  74 97.0  4980.24131 125.1187

## Reference

> Diggle AJ, Salam MU, Thomas GJ, Yang H, O’connell M, Sweetingham M,
> 2002. AnthracnoseTracer: a spatiotemporal model for simulating the
> spread of anthracnose in a lupin field. Phytopathology 92, 1110-21.
