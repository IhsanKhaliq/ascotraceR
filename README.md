README
================

<!-- badges: start -->

[![tic](https://github.com/IhsanKhaliq/ascotraceR/workflows/tic/badge.svg?branch=master)](https://github.com/IhsanKhaliq/ascotraceR/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/311562210.svg)](https://zenodo.org/badge/latestdoi/311562210)
<!-- badges: end -->

# ascotraceR: An R package resource to simulate the spatiotemporal spread of Ascochyta blight in a chickpea field over a growing season

The goal of of *ascotraceR* is to develop a weather driven model to
simulate the spread of Ascochyta blight disease in a chickpea field over
a growing season.

This model is adapted from a model developed by [(Diggle *et al.*
2002)](https://doi.org/10.1094/PHYTO.2002.92.10.1110) for simulating the
spread of anthracnose in a lupin field. The model is run using local
weather data. The ascotraceR model simulates the pathogen related
processes of conidial production, dispersal, successful deposition and
infection on chickpea plants. Host related processes of growth are
simulated in terms of development of growing points. The model divides
the paddock into 1 square metre cells (observation quadrats/units) and
simulates chickpea growth and *A. rabiei* activities in each cell.
Initially, there is one growing point per sown seed when seed are sown.
Chickpea growth is then described in terms of increase in the number of
growing points. Conidia are dispersed from infested stubble by rain
splash or wind driven rain when rainfall threshold is reached. Rainfall
threshold refers to the minimum amount of rainfall required to disperse
conidia from pycnidia and to provide sufficient duration of moisture for
conidia to germinate and penetrate into the host tissues. After
penetrating host tissues, conidia produce infected growing points.
Infected growing points become sporulating lesions after completion of a
latent period. The length of the latent period is a function of
temperature, and the number of conidia produced per sporulating growing
point depends on the level of resistance of the chickpea cultivar. As
the model runs, it keeps a continuous track of non-infected, latent,
infected and sporulating growing points (lesions). The ascotraceR’s
minimum input requirements are location specific weather data and a list
of input variables.

## Quick start

*ascotraceR* is available on CRAN. To install the latest release, just
run

``` r
install.packages("ascotraceR", repos="http://cran.us.r-project.org")
```

    ## installing the source package 'ascotraceR'

Alternatively, you may install the development version from GitHub this
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

set.seed(3)
```

Import the weather data.

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
    ## 143:   143    30.69713     1992023.87          4           669 2020-10-24 298
    ## 144:   144    22.13582     1992046.01          5           669 2020-10-25 299
    ## 145:   145    20.07310     1992066.08          5           669 2020-10-26 300
    ## 146:   146    15.58156     1992077.66          1           673 2020-10-27 301
    ## 147:   147    14.22171     1992091.88          4           673 2020-10-28 302
    ##             cdd cwh   cr gp_standard   AUDPC
    ##   1:    0.00000   0  0.0    40.00000 45870.5
    ##   2:   10.74583   0  0.0    42.77148 45870.5
    ##   3:   22.84583   0  0.0    46.10657 45870.5
    ##   4:   35.41042   0  0.0    49.83722 45870.5
    ##   5:   49.38958   1  0.6    54.32034 45870.5
    ##  ---                                        
    ## 143: 2133.81645  65 76.2  4980.06672 45870.5
    ## 144: 2154.64770  73 94.4  4980.12110 45870.5
    ## 145: 2176.49562  73 94.4  4980.17042 45870.5
    ## 146: 2195.63104  73 94.4  4980.20748 45870.5
    ## 147: 2215.57687  74 97.0  4980.24131 45870.5

## Code of Conduct

Please note that the ascotraceR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Reference

> Diggle AJ, Salam MU, Thomas GJ, Yang H, O’connell M, Sweetingham M,
> 2002. AnthracnoseTracer: a spatiotemporal model for simulating the
> spread of anthracnose in a lupin field. Phytopathology 92, 1110-21.
