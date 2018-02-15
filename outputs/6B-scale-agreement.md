---
title: "SPARS trial B"
subtitle: "Agreement between the SPARS and the CNRS_P/NRS_NP"
author: "Peter Kamerman and Tory Madden"
date: "15 February 2018"
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

This script assesses the agreement between the SPARS and the scaled NRS_NP and CNRS_P.[^1]

Because the three measurement scales were not used concurrently to rate each stimulus, we could not assess agreement using a standard procedure such as the Bland-Altman method. Instead, for each participant, and at each stimulus intensity, we generated 50,000 bootstrap resamples _(sampled with replacement)_ of ratings for each of the scales. We then calculated the median rating for each scale for each of the resamples and subtracted the values as follows: 

$$ Median_{(SPARS)} - Median_{(CNRS\_P / NRS\_NP)} $$

These data were then used to calculate a 95% bootstrap confidence interval (bias-corrected and accelerated method, BCa [^2]) of the difference in medians at each stimulus intensity. Confidence intervals that included 0 were interpreted as indicating agreement between the two measurements scale at a particular stimulus intensity. Values less than 0 indicate that SPARS under-estimated stimulus intensity compared to CNRS_P, and _visa versa_ for values greater than 0. 

To get a feel for rating agreement at the group-level, we used the point estimates of the differences in medians (see above) to calculate a 95% bootstrap interval _(50,000 resamples, BCa method)_ for the cohort at each stimulus intensity.

[^1]: Raw scores for the CNRS_P and NRS_NP scales (both rated on 0 to 100 scales) were converted to SPARS equivalent ranges (-50 to +50). CNRS_P ratings were a converted to a 0 to 50 range by dividing 2, such that after the scaling, 0 = _no pain_ and 50 = _worst pain you can imagine_. NRS_NP ratings were converted to a -50 to 0 range by subtracting 100, and then dividing 2, such that after the scaling, -50 = _no sensation_ and 0 = _pain_. This is equivalent to the negative range of the SPARS.

[^2]: Bias-corrected accelerated intervals work best for larger samples (>25), and therefore we incresaed our resample rate to 50,000 to help identify a stable distribution.

----

# Import and inspect data


```r
# Import
data <- read_rds('./data-cleaned/SPARS_B.rds')

# Inspect
glimpse(data)
```

```
## Observations: 2,256
## Variables: 10
## $ PID               <chr> "ID01", "ID01", "ID01", "ID01", "ID01", "ID0...
## $ scale             <chr> "SPARS", "SPARS", "SPARS", "SPARS", "SPARS",...
## $ block_number      <int> 2, 2, 2, 4, 4, 4, 6, 6, 6, 8, 8, 8, 11, 11, ...
## $ trial_number      <int> 9, 15, 23, 7, 20, 25, 9, 18, 22, 3, 17, 23, ...
## $ intensity         <dbl> 2.25, 2.25, 2.25, 2.25, 2.25, 2.25, 2.25, 2....
## $ intensity_char    <chr> "2.25", "2.25", "2.25", "2.25", "2.25", "2.2...
## $ intensity_rank    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ rating            <dbl> -31, -20, -48, -48, -21, -23, -48, -45, -47,...
## $ rating_positive   <dbl> 19, 30, 2, 2, 29, 27, 2, 5, 3, 0, 1, 3, 50, ...
## $ rating_equivalent <dbl> -31, -20, -48, -48, -21, -23, -48, -45, -47,...
```

----

# Clean and transform data


```r
############################################################
#                                                          #
#                          Clean                           #
#                                                          #
############################################################
data %<>%
    # Select required columns
    select(PID, scale, intensity_rank, rating_equivalent) 

############################################################
#                                                          #
#                Define bootstrap function                 #
#                                                          #
############################################################
# Participant-level bootstrap function
boot_deltaP <- function(d, i) {
    foo <- d[i, ]
    # Extract a random sample of SPARS rating data, and calculate a median
    spars <- foo[foo$scale == 'SPARS', ]
    spars <- spars$rating_equivalent
    spars <- median(spars, na.rm = TRUE)
    # Extract a random sample of OTHER rating data, and calculate a median
    other <- foo[foo$scale == 'OTHER', ]
    other <- other$rating_equivalent
    other <- median(other, na.rm = TRUE)
    # Calculate the difference in medians
    spars - other
}

# Group-level bootstrap function
boot_deltaG <- function(d, i) {
    foo <- d[i, ]
    # Extract a random sample of delta median estimates and calculate the median
    median(foo$statistic, na.rm = TRUE)
}

############################################################
#                                                          #
#                    Generate core data                    #
#                                                          #
############################################################
# Generate SPRS vs CNRS_P data
cnrs_agreement <- data %>%
    # Remove NRS_NP data
    filter(scale != 'NRS_NP') %>%
    # Recode CNRS_P to OTHER
    mutate(scale = case_when(
        scale == 'CNRS_P' ~ 'OTHER',
        TRUE ~ 'SPARS'
    )) %>%
    # Group and nest by PID and stimulus intensity
    group_by(PID, intensity_rank) %>%
    nest()

# Generate SPRS vs NRS_NP data
nrs_agreement <- data %>%
    # Remove NRS_NP data
    filter(scale != 'CNRS_P') %>%
    # Remove ID01 (no NRS_NP data)
    filter(PID != 'ID01') %>%
    # Recode CNRS_P to OTHER
    mutate(scale = case_when(
        scale == 'NRS_NP' ~ 'OTHER',
        TRUE ~ 'SPARS'
    )) %>%
    # Group and nest by PID and stimulus intensity
    group_by(PID, intensity_rank) %>%
    nest()
```

----

# Agreement between SPARS and CNRS_P

### Participant-level agreement


```r
# Set random seed
set.seed(1234)

# Perform bootstrap
cnrs_agreementP <- cnrs_agreement %>%
    mutate(boot = map(.x = data,
                      ~ boot::boot(data = .x, 
                                   statistic = boot_deltaP,
                                   R = 10000,
                                   stype = 'i')))

# Calculate 95% ci
cnrs_agreementP %<>%
    mutate(boot_ci = map(.x = boot,
                         ~ boot::boot.ci(boot.out = .x,
                                         type = 'bca')))

# Extract data
cnrs_agreementP %<>%
   mutate(statistic = map(.x = boot_ci,
                          ~ .x$t0),
          lower_ci = map(.x = boot_ci,
                         ~ .x$bca[[4]]),
          upper_ci = map(.x = boot_ci,
                         ~ .x$bca[[5]]))

# Clean-up
cnrs_agreementP %<>%
    select(-data, -boot, -boot_ci) %>%
    unnest() %>%
    # Add colour coding column
    mutate(includes_zero = case_when(
        lower_ci <= 0 & upper_ci >= 0 ~ 'yes',
        TRUE ~ 'no'
    ))
    

# Plot
ggplot(data = cnrs_agreementP) +
    aes(x = as.factor(intensity_rank),
        y = statistic,
        ymin = lower_ci,
        ymax = upper_ci,
        fill = includes_zero,
        colour = includes_zero) +
    geom_crossbar() +
    geom_hline(yintercept = 0,
               linetype = 2) +
    scale_fill_brewer(name = 'Interval includes zero: ',
                      type = 'qual',
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Interval includes zero: ',
                        type = 'qual',
                        palette = 'Dark2') +
    labs(title = 'Participant-level agreement between SPARS and CNRS_P ratings',
         subtitle = 'Bars: 95% confidence interval of the difference in median\nIntervals including zero interpreted as evidence for agreement',
         x = 'Stimulus intensity (rank)',
         y = expression(Delta~median~rating)) +
    facet_wrap(~ PID) +
    theme(legend.position = 'top')
```

<img src="figures/6B-scale-agreement/cnrs_agreementP-1.png" width="864" style="display: block; margin: auto;" />

There are significant participant-level discrepancies in agreement, but particularly at the lower stimulus intensities. In all cases (at the low end of the intensity range), the SPARS underestimated CNRS_P ratings, but this is likely an artefact of the two instruments, where SPARS continues to record (using a negative range) non-painful stimuli, while the CNRS_P reaches a floor value of zero when the stimulus intensity is no longer perceived as painful. When there was agreement, it tended to be in the upper ranges of stimulus intensity. There was significant heterogeneity across participants. 

### Group-level


```r
# Set random seed
set.seed(1234)

# Perform bootstrap
cnrs_agreementG <- cnrs_agreementP %>%
    group_by(intensity_rank) %>%
    nest() %>% 
    mutate(boot = map(.x = data,
                      ~ boot::boot(data = .x, 
                                   statistic = boot_deltaG,
                                   R = 10000,
                                   stype = 'i')))

# Calculate 95% ci
cnrs_agreementG %<>%
    mutate(boot_ci = map(.x = boot,
                         ~ boot::boot.ci(boot.out = .x,
                                         type = 'bca')))

# Extract data
cnrs_agreementG %<>%
   mutate(statistic = map(.x = boot_ci,
                          ~ .x$t0),
          lower_ci = map(.x = boot_ci,
                         ~ .x$bca[[4]]),
          upper_ci = map(.x = boot_ci,
                         ~ .x$bca[[5]]))

# Clean-up
cnrs_agreementG %<>%
    select(-data, -boot, -boot_ci) %>%
    unnest() %>%
    # Add colour coding column
    mutate(includes_zero = case_when(
        lower_ci <= 0 & upper_ci >= 0 ~ 'yes',
        TRUE ~ 'no'
    ))
    

# Plot
ggplot(data = cnrs_agreementG) +
    aes(x = as.factor(intensity_rank),
        y = statistic,
        ymin = lower_ci,
        ymax = upper_ci,
        fill = includes_zero,
        colour = includes_zero) +
    geom_crossbar() +
    geom_hline(yintercept = 0,
               linetype = 2) +
    scale_fill_brewer(name = 'Interval includes zero: ',
                      type = 'qual',
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Interval includes zero: ',
                        type = 'qual',
                        palette = 'Dark2') +
    labs(title = 'Group-level agreement between SPARS and CNRS_P ratings',
         subtitle = 'Bars: 95% confidence interval of the difference in median\nIntervals including zero interpreted as evidence for agreement',
         x = 'Stimulus intensity (rank)',
         y = expression(Delta~median~rating)) +
    theme(legend.position = 'top')
```

<img src="figures/6B-scale-agreement/cnrs_agreementG-1.png" width="864" style="display: block; margin: auto;" />

As expected, agreement between the two scales is poor at the low range of stimulus intensities. At higher stimulus intensities, the upper limits of the 95% confidence intervals tend to approach zero, but except for the 8J stimulus, the intervals do not include zero (we believe that the 8J finding results from sampling variation rather than being a true effect). Across the range of stimulus intensities, the width of the interval is large, and reflects high inter-individual variation in SPARS and CNRS_P ratings at each stimulus intensity. 

----

# Agreement between SPARS and NRS_NP

### Participant-level agreement


```r
# Set random seed
set.seed(1234)

# Perform bootstrap
nrs_agreementP <- nrs_agreement %>%
    mutate(boot = map(.x = data,
                      ~ boot::boot(data = .x, 
                                   statistic = boot_deltaP,
                                   R = 10000,
                                   stype = 'i')))

# Calculate 95% ci
nrs_agreementP %<>%
    mutate(boot_ci = map(.x = boot,
                         ~ boot::boot.ci(boot.out = .x,
                                         type = 'bca')))

# Extract data
nrs_agreementP %<>%
   mutate(statistic = map(.x = boot_ci,
                          ~ .x$t0),
          lower_ci = map(.x = boot_ci,
                         ~ .x$bca[[4]]),
          upper_ci = map(.x = boot_ci,
                         ~ .x$bca[[5]]))

# Clean-up
nrs_agreementP %<>%
    select(-data, -boot, -boot_ci) %>%
    unnest() %>%
    # Add colour coding column
    mutate(includes_zero = case_when(
        lower_ci <= 0 & upper_ci >= 0 ~ 'yes',
        TRUE ~ 'no'
    ))
    

# Plot
ggplot(data = nrs_agreementP) +
    aes(x = as.factor(intensity_rank),
        y = statistic,
        ymin = lower_ci,
        ymax = upper_ci,
        fill = includes_zero,
        colour = includes_zero) +
    geom_crossbar() +
    geom_hline(yintercept = 0,
               linetype = 2) +
    scale_fill_brewer(name = 'Interval includes zero: ',
                      type = 'qual',
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Interval includes zero: ',
                        type = 'qual',
                        palette = 'Dark2') +
    labs(title = 'Participant-level agreement between SPARS and NRS_NP ratings',
         subtitle = 'Bars: 95% confidence interval of the difference in median\nIntervals including zero interpreted as evidence for agreement',
         x = 'Stimulus intensity (rank)',
         y = expression(Delta~median~rating)) +
    facet_wrap(~ PID) +
    theme(legend.position = 'top')
```

<img src="figures/6B-scale-agreement/nrs_agreementP-1.png" width="864" style="display: block; margin: auto;" />

As with the SPARS/CNRS_P data, there are significant participant-level discrepancies in agreement, but in this case the problem is mostly apparent at the higher stimulus intensities. In all cases (at the high end of the intensity range), the SPARS over-estimated NRS_NP ratings, but this is likely an artefact of the two instruments, where SPARS continues to record (over its positive range) painful stimuli, while the NRS_NP reaches a ceiling value of zero when the stimulus intensity is first perceived as painful. When there was agreement, it tended to be in the lower ranges of stimulus intensity. There was significant heterogeneity across participants. 

### Group-level


```r
# Set random seed
set.seed(1234)

# Perform bootstrap
nrs_agreementG <- nrs_agreementP %>%
    group_by(intensity_rank) %>%
    nest() %>% 
    mutate(boot = map(.x = data,
                      ~ boot::boot(data = .x, 
                                   statistic = boot_deltaG,
                                   R = 10000,
                                   stype = 'i')))

# Calculate 95% ci
nrs_agreementG %<>%
    mutate(boot_ci = map(.x = boot,
                         ~ boot::boot.ci(boot.out = .x,
                                         type = 'bca')))

# Extract data
nrs_agreementG %<>%
   mutate(statistic = map(.x = boot_ci,
                          ~ .x$t0),
          lower_ci = map(.x = boot_ci,
                         ~ .x$bca[[4]]),
          upper_ci = map(.x = boot_ci,
                         ~ .x$bca[[5]]))

# Clean-up
nrs_agreementG %<>%
    select(-data, -boot, -boot_ci) %>%
    unnest() %>%
    # Add colour coding column
    mutate(includes_zero = case_when(
        lower_ci <= 0 & upper_ci >= 0 ~ 'yes',
        TRUE ~ 'no'
    ))
    

# Plot
ggplot(data = nrs_agreementG) +
    aes(x = as.factor(intensity_rank),
        y = statistic,
        ymin = lower_ci,
        ymax = upper_ci,
        fill = includes_zero,
        colour = includes_zero) +
    geom_crossbar() +
    geom_hline(yintercept = 0,
               linetype = 2) +
    scale_fill_brewer(name = 'Interval includes zero: ',
                      type = 'qual',
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Interval includes zero: ',
                        type = 'qual',
                        palette = 'Dark2') +
    labs(title = 'Group-level agreement between SPARS and NRS_NP ratings',
         subtitle = 'Bars: 95% confidence interval of the difference in median\nIntervals including zero interpreted as evidence for agreement',
         x = 'Stimulus intensity (rank)',
         y = expression(Delta~median~rating)) +
    theme(legend.position = 'top')
```

<img src="figures/6B-scale-agreement/nrs_agreementG-1.png" width="864" style="display: block; margin: auto;" />

Compared to the SPARS/CNRS_P group-level agreement, group-level agreement for the SPARS/NRS_NP was better (more intervals included zero, the exceptions being the three highest intensity ratings), but the intervals were wide, and the inclusion of zero was quite marginal in most cases. In general, the SPARS ratings over-estimated NRS_NP ratings.

----

# Session information

```r
sessionInfo()
```

```
## R version 3.4.3 (2017-11-30)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.3
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2       psych_1.7.8        boot_1.3-20       
##  [4] forcats_0.2.0      stringr_1.2.0      dplyr_0.7.4       
##  [7] purrr_0.2.4        readr_1.1.1        tidyr_0.8.0       
## [10] tibble_1.4.2       ggplot2_2.2.1.9000 tidyverse_1.2.1   
## [13] magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.3   reshape2_1.4.3     haven_1.1.1       
##  [4] lattice_0.20-35    colorspace_1.3-2   htmltools_0.3.6   
##  [7] yaml_2.1.16        rlang_0.1.6        pillar_1.1.0      
## [10] foreign_0.8-69     glue_1.2.0         RColorBrewer_1.1-2
## [13] modelr_0.1.1       readxl_1.0.0       bindr_0.1         
## [16] plyr_1.8.4         munsell_0.4.3      gtable_0.2.0      
## [19] cellranger_1.1.0   rvest_0.3.2        codetools_0.2-15  
## [22] evaluate_0.10.1    labeling_0.3       knitr_1.19        
## [25] parallel_3.4.3     broom_0.4.3        Rcpp_0.12.15      
## [28] scales_0.5.0.9000  backports_1.1.2    jsonlite_1.5      
## [31] mnormt_1.5-5       hms_0.4.1          digest_0.6.15     
## [34] stringi_1.1.6      grid_3.4.3         rprojroot_1.3-2   
## [37] cli_1.0.0          tools_3.4.3        lazyeval_0.2.1    
## [40] crayon_1.3.4       pkgconfig_2.0.1    xml2_1.2.0        
## [43] lubridate_1.7.1    assertthat_0.2.0   rmarkdown_1.8     
## [46] httr_1.3.1         rstudioapi_0.7     R6_2.2.2          
## [49] nlme_3.1-131       compiler_3.4.3
```
