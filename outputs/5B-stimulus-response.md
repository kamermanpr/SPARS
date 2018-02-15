---
title: "SPARS trial B"
subtitle: "Descriptive plots of the stimulus-response relationship for the SPARS, NRS_NP, and CNRS_P"
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

This script assesses and contrasts the stimulus-response characteristics of the SPARS, NRS_NP, and CNRS_P, through the use of descriptive plots.

The three scales measure the following ranges of somatic sensation:  

- CNRS_P: 0 _(no pain)_ to 100 _(worst pain you can imagine)_
_
- NRS_NP: 0 _(no sensation)_ to 100 _(pain)_

- SPARS: -50 _(no sensation)_, 0 _(pain threshold)_, +50 _(worst pain you can imagine)_

Because the the stimulus range was centred on the pre-determined pain threshold of each participant (compared to the fixed range of intensities used in Trial A), all analyses use the rank order of the nine stimulus intensities each participant was exposed to rather than the absolute intensities of the stimuli used. 

The experimental design involved exposing each participant to four successive experimental blocks of 27 trials (laser stimulations) each for each of the three measurement scales. The sequence of stimulus intensities used within each block was pre-determined, and differed between blocks. The order of in which the measurement scales were assessed was randomized, but for convenience of reporting, the plots are always shwon in the order: CNRS_P, NRS_NP, and SPARS.

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
  select(PID, scale, block_number, intensity_rank, rating, rating_equivalent) 

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
# Add sequential block numbers, tagged by scale
data %<>%
    # Group by ID and scale
    group_by(PID, scale) %>% 
    # Arrange blocks
    arrange(block_number) %>%
    # Add sequential 
    mutate(block_sequential = dense_rank(block_number),
           block_sequential = paste0(scale, ' - ', block_sequential)) %>%
    ungroup()

# Calculate the participant trimean for each scale (using 'rating_equivalent')
data_tm <- data %>% 
  group_by(PID, scale, intensity_rank) %>%
  summarise(tri_mean = tri.mean(rating_equivalent)) %>%
  ungroup()
```

----

# Exploratory plots

### Raw intensity ratings (participant-level)

Data are shown on their original scales: **CNRS_P:** 0 to 100, **NRS_NP:** 0 to 100, and **SPARS:** -50 to +50.


```r
# Generate plots
plot_raw <- data %>%
  group_by(PID) %>%
  nest() %>%
  mutate(plots = map2(.x = data,
                      .y = PID,
                      ~ ggplot(data = .x) +
                          aes(x = intensity_rank,
                              y = rating,
                              colour = scale,
                              fill = scale) +
                          geom_smooth(se = FALSE) +
                          geom_point() +
                          scale_y_continuous(limits = c(-50, 100)) +
                          scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
                          scale_fill_brewer(name = 'Scale',
                                            type = 'qual', 
                                            palette = 'Dark2') +
                          scale_colour_brewer(name = 'Scale',
                                              type = 'qual', 
                                              palette = 'Dark2') +
                          labs(title = paste0(.y, ': Raw participant-level stimulus-response ratings on the CNRS_P, NRS_NP and SPARS'),
                               subtitle = 'Plots are conditioned on measurement scale and experimental block\nCNRS_P: 0 (no pain) to 100 (worst pain you can imagine)\nNRS_NP: 0 (no sensation) to 100 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                               x = 'Stimulus intensity (rank)',
                               y = 'Intensity rating') +
                          geom_hline(yintercept = 0, linetype = 2) +
                          facet_wrap(~ block_sequential)))

# Print plots
walk(plot_raw$plots, ~ print(.x))
```

<img src="figures/5B-stimulus-response/raw_ratings-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/raw_ratings-7.png" width="864" style="display: block; margin: auto;" />

### Equivalent units ratings (participant-level)

Raw scores for the CNRS_P and NRS_NP scales (both rated on 0 to 100 scales) were converted to SPARS equivalent ranges (-50 to +50). The scaling of the CNRS_P and NRS_NP were as follows:

- CNRS_P: raw 0 to 100 scores were converted to a 0 to 50 range by dividing 2, such that after the scaling, 0 = _no pain_ and 50 = _worst pain you can imagine_. This is equivalent to the positive range of the SPARS.

- NRS_NP: raw 0 to 100 scores were converted to a -50 to 0 range by subtracting 100, and then dividing 2, such that after the scaling, -50 = _no sensation_ and 0 = _pain_. This is equivalent to the negative range of the SPARS.


```r
# Generate plots
plot_equi <- data %>%
  group_by(PID) %>%
  nest() %>%
  mutate(plots = map2(.x = data,
                      .y = PID,
                      ~ ggplot(data = .x) +
                          aes(x = intensity_rank,
                              y = rating_equivalent,
                              colour = scale,
                              fill = scale) +
                          geom_smooth(se = FALSE) +
                          geom_point() +
                          scale_y_continuous(limits = c(-50, 50)) +
                          scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
                          scale_fill_brewer(name = 'Scale',
                                            type = 'qual', 
                                            palette = 'Dark2') +
                          scale_colour_brewer(name = 'Scale',
                                              type = 'qual', 
                                              palette = 'Dark2') +
                          labs(title = paste0(.y, ': Scaled participant-level stimulus-response ratings on the CNRS_P, NRS_NP and SPARS'),
                               subtitle = 'Plots are conditioned on measurement scale and experimental block\nCNRS_P: 0 (no pain) to 50 (worst pain you can imagine)\nNRS_NP: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                               x = 'Stimulus intensity (rank)',
                               y = 'Scaled intensity rating (-50 to 50)') +
                          geom_hline(yintercept = 0, linetype = 2) +
                          facet_wrap(~ block_sequential)))

# Print plots
walk(plot_equi$plots, ~ print(.x))
```

<img src="figures/5B-stimulus-response/equivalent_ratings-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/equivalent_ratings-7.png" width="864" style="display: block; margin: auto;" />

### Tukey trimean plots (participant-level)

For each participant, we calculated the Tukey trimean of the intensity rating at each stimulus intensity for each of the measurement scales. The -50 to 50 scaled units were used in the calculation for the CNRS_P and NRS_NP. 


```r
# Generate plot
plot_tm <- data_tm %>%
    # Nest
    group_by(PID) %>%
    nest() %>%
    # Plot
    mutate(plot = map2(.x = data,
                       .y = PID,
                       ~ ggplot(data = ..1) +
                           aes(x = intensity_rank,
                               y = tri_mean) +
                           geom_smooth(method = 'loess',
                                       se = FALSE,
                                       colour = '#666666') +
                           geom_point(shape = 21,
                                      size = 3,
                                      fill = 'orange') +
                           scale_fill_brewer(name = 'Scale',
                                             type = 'qual', 
                                             palette = 'Dark2') +
                           scale_colour_brewer(name = 'Scale',
                                               type = 'qual', 
                                               palette = 'Dark2') +
                           scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
                           scale_y_continuous(limits = c(-50, 50)) +
                           labs(title = paste0(.y, ': Scaled participant-level stimulus-response rating Tukey trimeans on the CNRS_P, NRS_NP and SPARS'),
                                subtitle = 'Orange points: Tukey trimeans | Grey curve: loess curve\nPlots are conditioned on the three scales\nCNRS_P: 0 (no pain) to 50 (worst pain you can imagine)\nNRS_NP: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                                x = 'Stimulus intensity (rank)',
                                y = 'Scaled intensity rating (-50 to 50)') +
                           geom_hline(yintercept = 0, linetype = 2) +
                           facet_wrap(~ scale, ncol = 3)))

# Print plots
walk(plot_tm$plot, ~ print(.x))
```

<img src="figures/5B-stimulus-response/trimean_plots-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/5B-stimulus-response/trimean_plots-7.png" width="864" style="display: block; margin: auto;" />

### Summary plots (group-level)


```r
# Generate plot
data_tm %>%
  ggplot(data = .) +
    aes(x = intensity_rank,
        y = tri_mean,
        colour = scale,
        fill = scale) +
    geom_smooth(method = 'loess',
                se = FALSE) +
    geom_point(size = 2,
               position = position_dodge(width = 0.2)) +
    scale_fill_brewer(name = 'Measurement scale',
                      type = 'qual', 
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Measurement scale',
                        type = 'qual', 
                        palette = 'Dark2') +
    scale_y_continuous(limits = c(-50, 50)) +
    scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
    labs(title = 'Scaled group-level stimulus-response ratings on the CNRS_P, NRS_NP and SPARS',
         subtitle = 'Points (dodged for clarity): Tukey trimeans | Curves: loess lines\nCNRS_P: 0 (no pain) to 50 (worst pain you can imagine)\nNRS_NP: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
         x = 'Stimulus intensity (rank)',
         y = 'Scaled intensity rating (-50 to 50)') +
    geom_hline(yintercept = 0, linetype = 2) 
```

<img src="figures/5B-stimulus-response/summary_plot1-1.png" width="864" style="display: block; margin: auto;" />



```r
# Generate plot
data %>%
  ggplot(data = .) +
    aes(x = factor(intensity_rank),
        y = rating_equivalent,
        colour = scale,
        fill = scale) +
    geom_boxplot(alpha = 0.6) +
    scale_fill_brewer(name = 'Measurement scale',
                      type = 'qual', 
                      palette = 'Dark2') +
    scale_colour_brewer(name = 'Measurement scale',
                        type = 'qual', 
                        palette = 'Dark2') +
    scale_y_continuous(limits = c(-50, 50)) +
    labs(title = 'Scaled group-level stimulus-response ratings on the CNRS_P, NRS_NP and SPARS',
         subtitle = 'CNRS_P: 0 (no pain) to 50 (worst pain you can imagine)\nNRS_NP: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
         x = 'Stimulus intensity (rank)',
         y = 'Scaled intensity rating (-50 to 50)') +
    geom_hline(yintercept = 0, linetype = 2)
```

<img src="figures/5B-stimulus-response/summary_plot2-1.png" width="864" style="display: block; margin: auto;" />

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
##  [1] bindrcpp_0.2       forcats_0.2.0      stringr_1.2.0     
##  [4] dplyr_0.7.4        purrr_0.2.4        readr_1.1.1       
##  [7] tidyr_0.8.0        tibble_1.4.2       ggplot2_2.2.1.9000
## [10] tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.15       RColorBrewer_1.1-2 cellranger_1.1.0  
##  [4] pillar_1.1.0       compiler_3.4.3     plyr_1.8.4        
##  [7] bindr_0.1          tools_3.4.3        digest_0.6.15     
## [10] lubridate_1.7.1    jsonlite_1.5       evaluate_0.10.1   
## [13] nlme_3.1-131       gtable_0.2.0       lattice_0.20-35   
## [16] pkgconfig_2.0.1    rlang_0.1.6        psych_1.7.8       
## [19] cli_1.0.0          rstudioapi_0.7     yaml_2.1.16       
## [22] parallel_3.4.3     haven_1.1.1        xml2_1.2.0        
## [25] httr_1.3.1         knitr_1.19         hms_0.4.1         
## [28] tidyselect_0.2.3   rprojroot_1.3-2    grid_3.4.3        
## [31] glue_1.2.0         R6_2.2.2           readxl_1.0.0      
## [34] foreign_0.8-69     rmarkdown_1.8      modelr_0.1.1      
## [37] reshape2_1.4.3     backports_1.1.2    scales_0.5.0.9000 
## [40] htmltools_0.3.6    rvest_0.3.2        assertthat_0.2.0  
## [43] mnormt_1.5-5       colorspace_1.3-2   labeling_0.3      
## [46] stringi_1.1.6      lazyeval_0.2.1     munsell_0.4.3     
## [49] broom_0.4.3        crayon_1.3.4
```
