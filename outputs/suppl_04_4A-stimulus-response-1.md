---
title: "SPARS trial A"
subtitle: "Descriptive plots of the SPARS stimulus-response relationship"
author: "Peter Kamerman and Tory Madden"
date: "16 June 2018"
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

Modelling of the stimulus-response relationship is described in _"outputs/4A-stimulus-response-2.html"_, the diagnostics on the final linear mixed model are described in _"outputs/4A-stimulus-response-3.html"_, the stability of the model is described in _"outputs/4A-stimulus-response-4.html"_, the sensitivity of the scale to changes in stimulus intensity are described in _"outputs/4A-stimulus-reponse-5.html"_, and the variance in ratings at each stimulus intensity is described in _"outputs/4A-stimulus-reponse-6.html"_.

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

# Clean and transform data

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
  labs(title = 'Group-level stimulus-response plot',
       subtitle = 'Black circles: participant-level Tukey trimeans | Orange circles: group-level median | Grey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5))
```

<img src="figures/4A-stimulus-response-1/sr_group-1.png" width="960" style="display: block; margin: auto;" />

```r
## Publication plot
p <- data_tm %>%
    ggplot(data = .) +
    aes(x = intensity,
        y = tri_mean) +
    geom_segment(x = 0.8, xend = 4, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2) +
    geom_point(position = position_jitter(width = 0.05)) +
    geom_smooth(method = 'loess',
                se = FALSE,
                colour = '#FFA500', 
                size = 1) +
    geom_point(data = data_group,
               aes(y = median),
               shape = 21,
               size = 5,
               fill = '#FFA500') +
    geom_segment(x = 0.8, xend = 0.8, 
                 y = -50.25, yend = 50.25, 
                 size = 1.2) +
    geom_segment(x = 0.995, xend = 4.006, 
                 y = -55, yend = -55, 
                 size = 1.2) +
    labs(x = 'Stimulus intensity (J)',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25), 
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.8, 4.2), 
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 4, by = 0.5)) +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16,
                                   colour = '#000000'),
          axis.title = element_text(size = 16))

ggsave(filename = 'figures/figure_3.pdf',
       plot = p,
       width = 6,
       height = 5)
```

### Participant-level stimulus response curves

#### All trials


```r
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA))


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
       subtitle = 'Black circles: individual experimental blocks | Orange circles: Tukey trimean | \nGrey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~ PID, ncol = 3) +
  theme_bw()
```

<img src="figures/4A-stimulus-response-1/sr_participants-1.png" width="576" style="display: block; margin: auto;" />

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
##  [1] bindrcpp_0.2.2     patchwork_0.0.1    forcats_0.3.0     
##  [4] stringr_1.3.1      dplyr_0.7.5        purrr_0.2.5       
##  [7] readr_1.1.1        tidyr_0.8.1        tibble_1.4.2      
## [10] ggplot2_2.2.1.9000 tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.4  reshape2_1.4.3    haven_1.1.1      
##  [4] lattice_0.20-35   colorspace_1.3-2  htmltools_0.3.6  
##  [7] yaml_2.1.19       rlang_0.2.1       pillar_1.2.3     
## [10] foreign_0.8-70    glue_1.2.0        withr_2.1.2      
## [13] modelr_0.1.2      readxl_1.1.0      bindr_0.1.1      
## [16] plyr_1.8.4        munsell_0.4.3     gtable_0.2.0     
## [19] cellranger_1.1.0  rvest_0.3.2       psych_1.8.4      
## [22] evaluate_0.10.1   labeling_0.3      knitr_1.20       
## [25] parallel_3.5.0    broom_0.4.4       Rcpp_0.12.17     
## [28] scales_0.5.0.9000 backports_1.1.2   jsonlite_1.5     
## [31] mnormt_1.5-5      hms_0.4.2         digest_0.6.15    
## [34] stringi_1.2.2     grid_3.5.0        rprojroot_1.3-2  
## [37] cli_1.0.0         tools_3.5.0       lazyeval_0.2.1   
## [40] crayon_1.3.4      pkgconfig_2.0.1   xml2_1.2.0       
## [43] lubridate_1.7.4   assertthat_0.2.0  rmarkdown_1.9    
## [46] httr_1.3.1        rstudioapi_0.7    R6_2.2.2         
## [49] nlme_3.1-137      compiler_3.5.0
```
