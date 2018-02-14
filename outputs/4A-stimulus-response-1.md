---
title: "SPARS trial A"
subtitle: "Descriptive plots of the SPARS stimulus-response relationship"
author: "Peter Kamerman"
date: "14 Feb 2018"
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

This script is part 1 of our analysis of the stimulus-response characteristics of the SPARS. This script generates exploratory plots of the relationship between stimulus intensity and SPARS rating.

Modelling of the stimulus-response relationship is described in _"outputs/4A-stimulus-response-2.html"_, the diagnostics on the final linear mixed model are described in _"outputs/4A-stimulus-response-3.html"_, the stability of the model is described in _"outputs/4A-stimulus-response-4.html"_, and the sensitivity of the scale to changes in stimulus intensity are described in _"outputs/4A-stimulus-reponse-3.html"_.

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

# Clean and process data

We performed a basic clean-up of the data, and then calculated _Tukey trimean_ at each stimulus intensity for each participant (participant average), and finally the _median_ of the trimeans at each stimulus intensity across participants (group average). 


```r
############################################################
#                                                          #
#                          Clean                           #
#                                                          #
############################################################
data %<>%
  # Select required columns
  select(PID, block, block_order, trial_number, intensity, intensity_char, rating) 

############################################################
#                                                          #
#                Calculate 'Tukey trimean'                 #
#                                                          #
############################################################
# Define tri.mean function
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

############################################################
#                                                          #
#                    Generate core data                    #
#                                                          #
############################################################
# Calculate the participant average 
data_tm <- data %>% 
  group_by(PID, intensity) %>%
  summarise(tri_mean = tri.mean(rating)) %>%
  ungroup()

# Calculate the group average
data_group <- data_tm %>%
  group_by(intensity) %>%
  summarise(median = median(tri_mean)) %>%
  ungroup()
```

----

# Exploratory plots

### Group-level stimulus response curve


```r
# Plot
data_tm %>%
  ggplot(data = .) +
  aes(x = intensity,
      y = tri_mean) +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#666666', 
              size = 0.6) +
  geom_point(data = data_group,
             aes(y = median),
             shape = 21,
             size = 4,
             fill = '#D55E00') +
  labs(title = 'Group-level stimulus-response plots',
       subtitle = 'Black circles: participant-level Tukey trimeans | Orange circles: group-level median | Grey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5))
```

<img src="figures/4A-stimulus-response-1/sr_group-1.png" width="672" style="display: block; margin: auto;" />

### Participant-level stimulus response curves

#### All trials


```r
# Plot
data %>%
  ggplot(data = .) +
  aes(x = intensity,
      y = rating) +
  geom_point() +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#666666',
              size = 0.6) +
  geom_point(data = data_tm,
             aes(y = tri_mean),
             shape = 21,
             size = 3,
             fill = '#D55E00') +
  labs(title = 'Participant-level stimulus-response plot',
       subtitle = 'Black circles: individual experimental blocks | Orange circles: Tukey trimean | Grey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~ PID, ncol = 4)
```

<img src="figures/4A-stimulus-response-1/sr_participants-1.png" width="864" style="display: block; margin: auto;" />

#### Trials by experimental block


```r
# Process data
data_block <- data %>%
  # Rename blocks
  mutate(block = sprintf('Block: %s (order: %i)', block, block_order)) %>%
  # Nest by PID
  group_by(PID) %>%
  nest() %>%
  # Generate plots
  mutate(plots = map2(.x = data,
                      .y = unique(PID),
                      ~ ggplot(data = .x) +
                        aes(x = intensity,
                            y = rating) +
                        geom_point() +
                        geom_smooth(method = 'loess',
                                    se = FALSE,
                                    colour = '#666666',
                                    size = 0.6) +
                        labs(title = paste(.y, ': Participant-level stimulus-response plots conditioned on experimental block'),
                             subtitle = 'Black circles: individual data points | Grey line: loess curve',
                             x = 'Stimulus intensity (J)',
                             y = 'SPARS rating [-50 to 50]') +
                        scale_y_continuous(limits = c(-50, 50)) +
                        facet_wrap(~ block, ncol = 2)))

# Print plots
walk(.x = data_block$plots, ~ print(.x))
```

<img src="figures/4A-stimulus-response-1/sr_participants2-1.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-2.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-3.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-4.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-5.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-6.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-7.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-8.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-9.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-10.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-11.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-12.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-13.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-14.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-15.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-16.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-17.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-18.png" width="768" style="display: block; margin: auto;" /><img src="figures/4A-stimulus-response-1/sr_participants2-19.png" width="768" style="display: block; margin: auto;" />

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
##  [1] bindrcpp_0.2       patchwork_0.0.1    forcats_0.2.0     
##  [4] stringr_1.2.0      dplyr_0.7.4        purrr_0.2.4       
##  [7] readr_1.1.1        tidyr_0.8.0        tibble_1.4.2      
## [10] ggplot2_2.2.1.9000 tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-131       lubridate_1.7.1    httr_1.3.1        
##  [4] rprojroot_1.3-2    tools_3.4.3        TMB_1.7.12        
##  [7] backports_1.1.2    DT_0.4             R6_2.2.2          
## [10] sjlabelled_1.0.7   lazyeval_0.2.1     colorspace_1.3-2  
## [13] nnet_7.3-12        withr_2.1.1.9000   tidyselect_0.2.3  
## [16] mnormt_1.5-5       emmeans_1.1        compiler_3.4.3    
## [19] cli_1.0.0          rvest_0.3.2        xml2_1.2.0        
## [22] sandwich_2.4-0     labeling_0.3       effects_4.0-0     
## [25] scales_0.5.0.9000  lmtest_0.9-35      mvtnorm_1.0-7     
## [28] psych_1.7.8        blme_1.0-4         digest_0.6.15     
## [31] foreign_0.8-69     minqa_1.2.4        rmarkdown_1.8     
## [34] stringdist_0.9.4.6 pkgconfig_2.0.1    htmltools_0.3.6   
## [37] lme4_1.1-15        htmlwidgets_1.0    pwr_1.2-1         
## [40] rlang_0.1.6        readxl_1.0.0       rstudioapi_0.7    
## [43] shiny_1.0.5        bindr_0.1          zoo_1.8-1         
## [46] jsonlite_1.5       sjPlot_2.4.1       modeltools_0.2-21 
## [49] bayesplot_1.4.0    Matrix_1.2-12      Rcpp_0.12.15      
## [52] munsell_0.4.3      abind_1.4-5        prediction_0.2.0  
## [55] merTools_0.3.0     stringi_1.1.6      multcomp_1.4-8    
## [58] yaml_2.1.16        snakecase_0.8.1    carData_3.0-0     
## [61] MASS_7.3-48        plyr_1.8.4         grid_3.4.3        
## [64] parallel_3.4.3     sjmisc_2.7.0       crayon_1.3.4      
## [67] lattice_0.20-35    ggeffects_0.3.1    haven_1.1.1       
## [70] splines_3.4.3      sjstats_0.14.1     hms_0.4.1         
## [73] knitr_1.19         pillar_1.1.0       estimability_1.2  
## [76] reshape2_1.4.3     codetools_0.2-15   stats4_3.4.3      
## [79] glue_1.2.0         evaluate_0.10.1    modelr_0.1.1      
## [82] httpuv_1.3.5       nloptr_1.0.4       cellranger_1.1.0  
## [85] gtable_0.2.0       assertthat_0.2.0   mime_0.5          
## [88] coin_1.2-2         xtable_1.8-2       broom_0.4.3       
## [91] survey_3.33        coda_0.19-1        survival_2.41-3   
## [94] arm_1.9-3          glmmTMB_0.2.0      TH.data_1.0-8
```
