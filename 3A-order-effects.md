---
title: "SPARS trial A"
subtitle: "Effect of stimulus order and blocking on SPARS rating"
author: "Peter Kamerman"
date: "31 Jan 2018"
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

The experimental protocol called for participants to be exposed to 13 stimuli, evenly spaced at 0.25J intervals over the range 1.00J to 4.00J. Each stimulus intensity was applied 8 times, giving a total of 104 exposures (trials). To prevent learning effects, the 104 trials were randomised across 4 experimental blocks (26 trials per block). 

Despite using a randomized block approach, we still wanted to assess whether there were any:

- Trial order effects (does the intensity of the preceding stimulus affect the rating of a stimulus?)  
- Block order effects (does the rating of a given stimulus intenisty change across experimental blocks?)

----

# Import and inspect data

```r
# Import
data <- read_rds('./data/SPARS_A.rds')

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

# Clean data and process


```r
############################################################
#                                                          #
#                          Clean                           #
#                                                          #
############################################################
data %<>%
  # Select required columns
  select(PID, block, block_order, trial_number, 
         intensity, rating, rating_positive) %>%
  # Format block and trial number type
  mutate(block_order = as.integer(block_order),
         trial_number = as.integer(trial_number)) %>%
  # 'Back-up' intensity data as a continuous variable before factorizing
  mutate(intensity_cont = intensity) %>% 
  # Format intensity data as an ordered factor
  mutate(intensity = sprintf('%s%.2f%s', 'Stimulus: ', intensity, 'J'),
         intensity = factor(intensity,
                            levels = paste0('Stimulus: ',
                                            as.character(format(seq(from = 1,
                                                                to = 4, 
                                                                by = 0.25),
                                                                digits = 3)),
                                            'J'),
                            ordered = TRUE)) 

############################################################
#                                                          #
#              Lagged stimulus intensity data              #
#               (lag 1: preceding stimulus)                #
#                                                          #
############################################################
data_lag <- data %>%
  # Drop unneeded columns (block_order and positive_intensity)
  select(-rating_positive) %>%
  # Get lag-1 stimulus intensity by PID
  group_by(PID) %>%
  mutate(intensity_lag = lag(intensity_cont)) %>%
  # Ungroup and remove incomplete cases greated by lag function (i.e., first trial)
  ungroup() %>%
  filter(complete.cases(.)) 
```

----

# Summary

The figure below summarises the exposure (number of trials) of each participant at each stimulus intensity. 


```r
data %>%
  ggplot(data = .) +
  aes(x = intensity,
      fill = factor(block_order)) +
  geom_bar() +
  scale_fill_viridis_d(name = 'Experimental block:') +
  facet_wrap(~PID, ncol = 4) +
  labs(title = 'Number of trials per participant at each stimulus intensity, stratified by experimental block',
       y = 'Number of trials',
       x = 'Stimulus intensity (J)') +
  theme_bw() +
  theme(legend.position = 'top')
```

<img src="./figures/3A-order-effects/summary-1.png" width="672" style="display: block; margin: auto;" />

Participants _ID05_, _ID06_, _ID12_, and _ID15_ did not complete the trial per protocol.

----

# Trial order effects

**Question:** Is the SPARS rating for a given stimulus intensity dependent on the intensity of the preceding stimulus? 

The analysis includes data from experimental trials, ignoring experimental blocks. Because each stimulus intensity is repeated multiple times in each participant, a hierarchical analysis by study participant and stimulus intensity was conducted. 

### User-defined function

Function to calculate the _Tukey trimeans_ for plots.


```r
# Trimean
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

### Participant-level exploratory plots

Participant-level plots of SPARS rating against the intensity of the preceding stimulus, across the range of stimuli used (1.00J to 4.00J). 


```r
# Plot data
plot_data <- data_lag %>%
  # Nest by ID
  group_by(PID) %>%
  nest() %>%
  # Calculate trimean 
  mutate(trimean = map(.x = data,
                       ~ group_by(.data = .x,
                                  intensity) %>%
                         summarise(tm = tri.mean(rating))))

# Plot
plot_trial <- plot_data %>%
  mutate(plot = pmap(.l = list(data, trimean, unique(PID)),
                    ~ ggplot(data = ..1) +
                      aes(x = intensity_lag,
                          y = rating) +
                      geom_point() +
                      geom_smooth(se = FALSE,
                                  colour = '#666666',
                                  size = 0.6,
                                  na.rm = TRUE) +
                      geom_hline(data = ..2, 
                                 aes(yintercept = tm),
                                 colour = 'red',
                                 size = 0.6,
                                 linetype = 2) +
                      labs(title = paste0(..3, ': SPARS rating versus intensity of the previous stimulus, at different stimulus intensities'),
                           subtitle = 'Red line (dashed): Tukey trimean of SPARS rating | Grey line (solid): Loess curve',
                           y = 'SPARS rating [-50 to 50]',
                           x = 'Intensity of previous stimulus (J)') +
                      scale_y_continuous(limits = c(-50, 50)) +
                      scale_x_continuous(limits = c(1, 4)) +
                      facet_wrap(~intensity, 
                                 ncol = 4) +
                      theme_bw()))

# Print plots
walk(.x = plot_trial$plot, ~ print(.x))
```

<img src="./figures/3A-order-effects/plots_trial-1.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-2.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-3.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-4.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-5.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-6.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-7.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-8.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-9.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-10.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-11.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-12.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-13.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-14.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-15.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-16.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-17.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-18.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/plots_trial-19.png" width="672" style="display: block; margin: auto;" />

### Conclusion 

Visual inspection of the figures shows the loess curve (grey curve) oscillating around the Tukey trimean (dashed red line), for all participants and across almost all stimulus intensities, indicating no systematic relationship between the rating of a stimulus and the intensity of the previous stimulus. 

**No further analysis undertaken.**

----

# Block order effects

**Question:** Is the SPARS rating for a given stimulus intensity associated with experimental block? 

The analysis includes data from experimental blocks, ignoring trials. Because each stimulus intensity is repeated multiple times in each participant, a hierarchical analysis by study participant and stimulus intensity was conducted. 

### Participant-level exploratory plots


```r
# Plot
plot_block <- plot_data %>%
  mutate(plot = pmap(.l = list(data, trimean, unique(PID)),
                    ~ ggplot(data = ..1) +
                      aes(x = block_order,
                          y = rating) +
                      geom_point() +
                      geom_smooth(se = FALSE,
                                  colour = '#666666',
                                  size = 0.6,
                                  na.rm = TRUE) +
                      geom_hline(data = ..2, 
                                 aes(yintercept = tm),
                                 colour = 'red',
                                 size = 0.6,
                                 linetype = 2) +
                      labs(title = paste0(..3, ': SPARS rating versus experimental block number, at different stimulus intensities'),
                           subtitle = 'Red line (dashed): Tukey trimean of SPARS rating | Grey line (solid): Loess curve',
                           y = 'SPARS rating [-50 to 50]',
                           x = 'Block number') +
                      scale_y_continuous(limits = c(-50, 50)) +
                      scale_x_continuous(limits = c(1, 4)) +
                      facet_wrap(~intensity, 
                                 ncol = 4) +
                      theme_bw()))

# Print plots
walk(.x = plot_block$plot, ~ print(.x))
```

<img src="./figures/3A-order-effects/process_block-1.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-2.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-3.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-4.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-5.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-6.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-7.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-8.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-9.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-10.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-11.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-12.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-13.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-14.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-15.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-16.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-17.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-18.png" width="672" style="display: block; margin: auto;" /><img src="./figures/3A-order-effects/process_block-19.png" width="672" style="display: block; margin: auto;" />

### Conclusion

Visual inspection of the figures shows the loess curve (grey curve) oscillating around the Tukey trimean (dashed red line), for all participants and across almost all stimulus intensities, indicating no systematic relationship between the rating of a stimulus and the experimental block. 

**No further analysis undertaken.**

----

# Session information

```
## R version 3.4.3 (2017-11-30)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.2
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
##  [1] bindrcpp_0.2       robustlmm_2.1-4    lme4_1.1-15       
##  [4] Matrix_1.2-12      forcats_0.2.0      stringr_1.2.0     
##  [7] dplyr_0.7.4        purrr_0.2.4        readr_1.1.1       
## [10] tidyr_0.8.0        tibble_1.4.2       ggplot2_2.2.1.9000
## [13] tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.3  reshape2_1.4.3    splines_3.4.3    
##  [4] haven_1.1.1       lattice_0.20-35   colorspace_1.3-2 
##  [7] viridisLite_0.2.0 htmltools_0.3.6   yaml_2.1.16      
## [10] rlang_0.1.6       nloptr_1.0.4      pillar_1.1.0     
## [13] foreign_0.8-69    glue_1.2.0        modelr_0.1.1     
## [16] readxl_1.0.0      bindr_0.1         plyr_1.8.4       
## [19] robustbase_0.92-8 munsell_0.4.3     gtable_0.2.0     
## [22] cellranger_1.1.0  rvest_0.3.2       codetools_0.2-15 
## [25] psych_1.7.8       evaluate_0.10.1   labeling_0.3     
## [28] knitr_1.19        parallel_3.4.3    DEoptimR_1.0-8   
## [31] broom_0.4.3       Rcpp_0.12.15      xtable_1.8-2     
## [34] scales_0.5.0.9000 backports_1.1.2   jsonlite_1.5     
## [37] mnormt_1.5-5      hms_0.4.1         digest_0.6.14    
## [40] stringi_1.1.6     grid_3.4.3        rprojroot_1.3-2  
## [43] cli_1.0.0         tools_3.4.3       lazyeval_0.2.1   
## [46] crayon_1.3.4      pkgconfig_2.0.1   MASS_7.3-48      
## [49] xml2_1.2.0        lubridate_1.7.1   minqa_1.2.4      
## [52] assertthat_0.2.0  rmarkdown_1.8     httr_1.3.1       
## [55] rstudioapi_0.7    R6_2.2.2          nlme_3.1-131     
## [58] compiler_3.4.3
```
