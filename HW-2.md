README
================

**Load in and Prepare the Data**

``` r
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
library(ggplot2)
library(blscrapeR)
library(readr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
df <- get_bls_county()
WIunemployment = df %>% filter(fips_state == 55)

bridges = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   STRUCTURE_NUMBER_008 = col_character(),
    ##   ROUTE_NUMBER_005D = col_character(),
    ##   HIGHWAY_DISTRICT_002 = col_character(),
    ##   COUNTY_CODE_003 = col_character(),
    ##   FEATURES_DESC_006A = col_character(),
    ##   CRITICAL_FACILITY_006B = col_logical(),
    ##   FACILITY_CARRIED_007 = col_character(),
    ##   LOCATION_009 = col_character(),
    ##   LRS_INV_ROUTE_013A = col_character(),
    ##   LAT_016 = col_character(),
    ##   LONG_017 = col_character(),
    ##   MAINTENANCE_021 = col_character(),
    ##   OWNER_022 = col_character(),
    ##   FUNCTIONAL_CLASS_026 = col_character(),
    ##   DESIGN_LOAD_031 = col_character(),
    ##   RAILINGS_036A = col_character(),
    ##   TRANSITIONS_036B = col_character(),
    ##   APPR_RAIL_036C = col_character(),
    ##   APPR_RAIL_END_036D = col_character(),
    ##   NAVIGATION_038 = col_character()
    ##   # ... with 41 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 3 parsing failures.
    ##   row                     col               expected actual                                                          file
    ##  5739 OTHR_STATE_STRUC_NO_099 no trailing characters   B010 'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'
    ## 11175 OPR_RATING_METH_063     a double                 F    'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'
    ## 11175 INV_RATING_METH_065     a double                 F    'https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt'

``` r
br = bridges %>% group_by(COUNTY_CODE_003) %>% summarize(meanADT = mean(ADT_029)) %>% left_join(WIunemployment,by = c("COUNTY_CODE_003" = "fips_county"))
```

**Build up the linear models**

``` r
model_unemployed = lm(br$meanADT ~ WIunemployment$unemployed)
model_unemployed_rate = lm(br$meanADT ~ WIunemployment$unemployed_rate)
summary(model_unemployed)
```

    ## 
    ## Call:
    ## lm(formula = br$meanADT ~ WIunemployment$unemployed)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6396.3 -1288.7  -551.3   753.0  7757.8 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               1609.6443   294.6952   5.462 6.77e-07 ***
    ## WIunemployment$unemployed    1.6058     0.1146  14.017  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2117 on 70 degrees of freedom
    ## Multiple R-squared:  0.7373, Adjusted R-squared:  0.7335 
    ## F-statistic: 196.5 on 1 and 70 DF,  p-value: < 2.2e-16

``` r
summary(model_unemployed_rate)
```

    ## 
    ## Call:
    ## lm(formula = br$meanADT ~ WIunemployment$unemployed_rate)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5104.5 -2052.3  -818.3   916.8 17713.8 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      9957.5     1625.6   6.125  4.7e-08 ***
    ## WIunemployment$unemployed_rate  -1589.1      404.4  -3.930 0.000197 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3738 on 70 degrees of freedom
    ## Multiple R-squared:  0.1807, Adjusted R-squared:  0.169 
    ## F-statistic: 15.44 on 1 and 70 DF,  p-value: 0.0001974

**Draw linear model line**

``` r
model_unemployed %>% ggplot(aes(br$meanADT, WIunemployment$unemployed)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Average ADT", y = "Unemployed Number")
```

![](HW-2_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
model_unemployed_rate %>% ggplot(aes(br$meanADT, WIunemployment$unemployed_rate)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Average ADT", y = "Unemployed Rate")
```

![](HW-2_files/figure-markdown_github/unnamed-chunk-3-2.png)
