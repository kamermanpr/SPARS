---
title: "Supplement 2"
subtitle: "Experiment 1 -- Group-level summary of measures of central tendency"
author: "Tory Madden and Peter Kamerman"
date: "16 Jul 2018"
output: 
  html_document:
    keep_md: true
    theme: yeti
    hightlight: pygments
    toc: true
    toc_depth: 5
    toc_float: true
    code_folding: show
---



----

Here we provide group-level summary values for measures of central tendency and variance for SPARS ratings at each laser stimulus intensity. Please see Supplement 3 for definitions of these measures.

----

# Import and inspect data


```r
# Import
data <- read_rds('./data-cleaned/SPARS_A.rds')

# Inspect
glimpse(data)
```

```
## Observations: 1,927
## Variables: 19
## $ PID               <chr> "ID01", "ID01", "ID01", "ID01", "ID01", "ID0...
## $ block             <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A",...
## $ block_order       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ trial_number      <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1...
## $ intensity         <dbl> 3.75, 1.50, 3.25, 1.50, 3.00, 2.75, 1.00, 2....
## $ intensity_char    <chr> "3.75", "1.50", "3.25", "1.50", "3.00", "2.7...
## $ rating            <dbl> -10, -40, -10, -25, -20, -25, -40, 2, -40, -...
## $ rating_positive   <dbl> 40, 10, 40, 25, 30, 25, 10, 52, 10, 40, 54, ...
## $ EDA               <dbl> 18315.239, 13904.177, 11543.449, 20542.834, ...
## $ age               <dbl> 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, ...
## $ sex               <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,...
## $ panas_positive    <dbl> 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, ...
## $ panas_negative    <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, ...
## $ dass42_depression <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ dass42_anxiety    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ dass42_stress     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ pcs_magnification <dbl> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,...
## $ pcs_rumination    <dbl> 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, ...
## $ pcs_helplessness  <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, ...
```

----

# Clean data


```r
# Basic clean-up
data %<>%
  # Select required columns
  select(PID, block, block_order, intensity, 
         intensity_char, rating, rating_positive) 
```

----

# Tabulate measures of central tendency and variance


```r
# Specify tri.mean function (average of the median and mid-hinge)
tri.mean <- function(x) {
  # Calculate quantiles
  q1 <- quantile(x, probs = 0.25, na.rm = TRUE)[[1]]
  q2 <- median(x, na.rm = TRUE)
  q3 <- quantile(x, probs = 0.75, na.rm = TRUE)[[1]]
  # Calculate trimean
  tm <- (q2 + ((q1 + q3) / 2)) / 2
  # Convert to integer
  tm <- as.integer(round(tm))
  return(tm)
}
```


```r
# Calculate group-level centrality and variance measures 
central <- data %>% 
  group_by(intensity_char) %>%
  summarise(mean = round(mean(rating_positive, 
                              na.rm = TRUE), 1),
            median = round(median(rating_positive, 
                                 na.rm = TRUE), 1),
            geometric_mean = round(psych::geometric.mean(rating_positive,
                                                         na.rm=TRUE), 1),
            tri_mean = round(tri.mean(rating_positive), 1),
            sd = round(sd(rating_positive, 
                          na.rm = TRUE), 1),
            Q25 = round(quantile(rating_positive,
                                 probs =  0.25, 
                                 na.rm = TRUE), 1),
            Q75 = round(quantile(rating_positive, 
                                 probs = 0.75, 
                                 na.rm = TRUE), 1)) %>%
  ungroup()

# Pretty column names
colnames(central) <- c("Intensity", "Arithmetic mean", "Median", 
                       "Geometric mean", "Tukey trimean", 
                       "Standard deviation", "Lower quartile boundary", 
                       "Upper quartile boundary")

# Print table
knitr::kable(central, 
      align = rep('c', 8),
      caption = "Group-level measures of central tendency and variance for SPARS ratings at each laser intensity (J)") %>%
  kable_styling("striped") %>%
  row_spec(1:13, 
           align = "c", 
           color = "black")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<caption>Group-level measures of central tendency and variance for SPARS ratings at each laser intensity (J)</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> Intensity </th>
   <th style="text-align:center;"> Arithmetic mean </th>
   <th style="text-align:center;"> Median </th>
   <th style="text-align:center;"> Geometric mean </th>
   <th style="text-align:center;"> Tukey trimean </th>
   <th style="text-align:center;"> Standard deviation </th>
   <th style="text-align:center;"> Lower quartile boundary </th>
   <th style="text-align:center;"> Upper quartile boundary </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 1.00 </td>
   <td style="text-align:center;color: black;text-align: center;"> 22.1 </td>
   <td style="text-align:center;color: black;text-align: center;"> 15.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 18 </td>
   <td style="text-align:center;color: black;text-align: center;"> 20.8 </td>
   <td style="text-align:center;color: black;text-align: center;"> 1.8 </td>
   <td style="text-align:center;color: black;text-align: center;"> 41.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 1.25 </td>
   <td style="text-align:center;color: black;text-align: center;"> 28.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 30.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 28 </td>
   <td style="text-align:center;color: black;text-align: center;"> 21.6 </td>
   <td style="text-align:center;color: black;text-align: center;"> 5.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 48.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 1.50 </td>
   <td style="text-align:center;color: black;text-align: center;"> 34.1 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 36 </td>
   <td style="text-align:center;color: black;text-align: center;"> 22.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 10.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 53.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 1.75 </td>
   <td style="text-align:center;color: black;text-align: center;"> 37.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 43.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40 </td>
   <td style="text-align:center;color: black;text-align: center;"> 21.7 </td>
   <td style="text-align:center;color: black;text-align: center;"> 20.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 53.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 2.00 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 47.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 43 </td>
   <td style="text-align:center;color: black;text-align: center;"> 19.9 </td>
   <td style="text-align:center;color: black;text-align: center;"> 25.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 53.2 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 2.25 </td>
   <td style="text-align:center;color: black;text-align: center;"> 42.6 </td>
   <td style="text-align:center;color: black;text-align: center;"> 48.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 0.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 45 </td>
   <td style="text-align:center;color: black;text-align: center;"> 17.6 </td>
   <td style="text-align:center;color: black;text-align: center;"> 30.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 55.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 2.50 </td>
   <td style="text-align:center;color: black;text-align: center;"> 44.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 49.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 37.2 </td>
   <td style="text-align:center;color: black;text-align: center;"> 47 </td>
   <td style="text-align:center;color: black;text-align: center;"> 18.8 </td>
   <td style="text-align:center;color: black;text-align: center;"> 35.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 55.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 2.75 </td>
   <td style="text-align:center;color: black;text-align: center;"> 46.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 51.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 49 </td>
   <td style="text-align:center;color: black;text-align: center;"> 18.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 55.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 3.00 </td>
   <td style="text-align:center;color: black;text-align: center;"> 48.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 52.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 42.2 </td>
   <td style="text-align:center;color: black;text-align: center;"> 51 </td>
   <td style="text-align:center;color: black;text-align: center;"> 19.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 40.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 60.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 3.25 </td>
   <td style="text-align:center;color: black;text-align: center;"> 54.2 </td>
   <td style="text-align:center;color: black;text-align: center;"> 55.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 50.6 </td>
   <td style="text-align:center;color: black;text-align: center;"> 56 </td>
   <td style="text-align:center;color: black;text-align: center;"> 15.8 </td>
   <td style="text-align:center;color: black;text-align: center;"> 47.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 65.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 3.50 </td>
   <td style="text-align:center;color: black;text-align: center;"> 62.3 </td>
   <td style="text-align:center;color: black;text-align: center;"> 61.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 59.8 </td>
   <td style="text-align:center;color: black;text-align: center;"> 62 </td>
   <td style="text-align:center;color: black;text-align: center;"> 15.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 54.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 70.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 3.75 </td>
   <td style="text-align:center;color: black;text-align: center;"> 62.2 </td>
   <td style="text-align:center;color: black;text-align: center;"> 65.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 59.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 64 </td>
   <td style="text-align:center;color: black;text-align: center;"> 16.4 </td>
   <td style="text-align:center;color: black;text-align: center;"> 54.5 </td>
   <td style="text-align:center;color: black;text-align: center;"> 70.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;color: black;text-align: center;"> 4.00 </td>
   <td style="text-align:center;color: black;text-align: center;"> 67.1 </td>
   <td style="text-align:center;color: black;text-align: center;"> 65.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 66.1 </td>
   <td style="text-align:center;color: black;text-align: center;"> 65 </td>
   <td style="text-align:center;color: black;text-align: center;"> 11.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 60.0 </td>
   <td style="text-align:center;color: black;text-align: center;"> 70.5 </td>
  </tr>
</tbody>
</table>

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.0 (2018-04-23)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.5
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2   kableExtra_0.9.0 ggridges_0.5.0   forcats_0.3.0   
##  [5] stringr_1.3.1    dplyr_0.7.6      purrr_0.2.5      readr_1.1.1     
##  [9] tidyr_0.8.1      tibble_1.4.2     ggplot2_3.0.0    tidyverse_1.2.1 
## [13] magrittr_1.5    
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.4  reshape2_1.4.3    haven_1.1.2      
##  [4] lattice_0.20-35   colorspace_1.3-2  viridisLite_0.3.0
##  [7] htmltools_0.3.6   yaml_2.1.19       rlang_0.2.1      
## [10] pillar_1.2.3      foreign_0.8-70    glue_1.2.0       
## [13] withr_2.1.2       modelr_0.1.2      readxl_1.1.0     
## [16] bindr_0.1.1       plyr_1.8.4        munsell_0.5.0    
## [19] gtable_0.2.0      cellranger_1.1.0  rvest_0.3.2      
## [22] psych_1.8.4       evaluate_0.10.1   knitr_1.20       
## [25] parallel_3.5.0    highr_0.7         broom_0.4.5      
## [28] Rcpp_0.12.17      scales_0.5.0.9000 backports_1.1.2  
## [31] jsonlite_1.5      mnormt_1.5-5      hms_0.4.2        
## [34] digest_0.6.15     stringi_1.2.3     grid_3.5.0       
## [37] rprojroot_1.3-2   cli_1.0.0         tools_3.5.0      
## [40] lazyeval_0.2.1    crayon_1.3.4      pkgconfig_2.0.1  
## [43] xml2_1.2.0        lubridate_1.7.4   assertthat_0.2.0 
## [46] rmarkdown_1.10    httr_1.3.1        rstudioapi_0.7   
## [49] R6_2.2.2          nlme_3.1-137      compiler_3.5.0
```
