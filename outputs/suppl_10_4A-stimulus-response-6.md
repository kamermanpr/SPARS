---
title: "Supplement 10"
subtitle: "Experiment 1 -- Rating variance in the SPARS"
author: "Peter Kamerman"
date: "04 Oct 2018"
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

This script is part 6 of our analysis of the stimulus-response characteristics of the SPARS. In these analyses we assessed intra-individual and inter-individual variability in scoring on the SPARS at each pain intensity. 

We used the _median absolute deviation_ (MAD), which is a robust measure of dispersion, to assess variance. To calculate intra-individual variance for each stimulus intensity, we calculated the MAD using the ratings from repeated stimuli at each stimulus intensity. We also used the MAD for calculating inter-individual variance for each scale and at each intensity, using the median ratings by individuals at each intensity as the input.

**Median absolute deviation**  

$$MAD = median(|X_i - median(X)|)$$ 

Descriptive plots of the data are provided in _"outputs/suppl\_05\_4A-stimulus-response-1.html"_, modelling of the stimulus-response relationship is described in _"outputs/suppl\_06\_4A-stimulus-response-2.html"_, the diagnostics on the final linear mixed model are described in _"outputs/suppl\_07\_4A-stimulus-response-3.html"_, the stability of the model is described in _"outputs/suppl\_08\_4A-stimulus-response-4.html"_, the sensitivity of the scale to changes in stimulus intensity are described in _"outputs/suppl\_09\_4A-stimulus-reponse-5.html"_.

----

# Import and clean/transform data


```r
############################################################
#                                                          #
#                          Import                          #
#                                                          #
############################################################
data <- read_rds('./data-cleaned/SPARS_A.rds')

############################################################
#                                                          #
#                          Clean                           #
#                                                          #
############################################################
data %<>%
  # Select required columns
  select(PID, intensity, rating) 
```

----

# Intra-individual variation (participant-level)


```r
# Calculate MAD
data_varianceP <- data %>%
    group_by(PID, intensity) %>%
    summarise(MAD = mad(x = rating,
                        constant = 1, 
                        na.rm = TRUE)) %>%
    ungroup()

# Plot
ggplot(data = data_varianceP) +
    aes(y = MAD,
        x = sprintf('%.02f', intensity)) +
    geom_point(shape = 21,
               size = 4,
               stroke = 1,
               fill = '#FFFFFF') +
    geom_hline(yintercept = 10,
               linetype = 2) +
    labs(title = 'Participant-level median absolute deviation (MAD) for the SPARS',
         subtitle = 'Dotted line: 10% of scale range (100 units)',
         y = 'Median absolute deviation',
         x = 'Stimulus intensity (J)') +
    scale_y_continuous(limits = c(0, 25)) +
    facet_wrap(~ PID, ncol = 3) +
    theme(axis.text.x = element_text(angle = -90))
```

<img src="figures/suppl_10_4A-stimulus-response-6/varianceP-1.png" width="864" style="display: block; margin: auto;" />

There is no clear pattern in intra-individual variance in intensity rating within the scale. Variance can be high on a case-by-case basis, but most MAD value are < 10% of the scale range (100 units, -50 to 50). 

----

# Inter-individual variation (group-level)


```r
# Process data for plotting
data_varianceG <- data %>% 
    # Calculate median rating at each stimulus intensity for each scale per PID
    group_by(PID, intensity) %>%
    summarise(median = median(rating, 
                              na.rm = TRUE)) %>%
    # Calculate group MAD
    group_by(intensity) %>%
    summarise(MAD = mad(median,
                        constant = 1,
                        na.rm = TRUE)) %>%
    ungroup()
    

# Plot
ggplot(data = data_varianceG) +
    aes(y = MAD,
        x = sprintf('%.02f', intensity)) +
    geom_point(shape = 21,
               size = 4,
               stroke = 1,
               fill = '#FFFFFF') +
    geom_hline(yintercept = 10,
               linetype = 2) +
    labs(title = 'Group-level median absolute deviation (MAD) for the SPARS',
         subtitle = 'Dotted line: 10% of scale range (100 units)',
         y = 'Median absolute deviation',
         x = 'Stimulus intensity (J)') +
    scale_y_continuous(limits = c(0, 25)) +
    theme(axis.text.x = element_text(angle = -90))
```

<img src="figures/suppl_10_4A-stimulus-response-6/varianceG-1.png" width="672" style="display: block; margin: auto;" />

Other than an 'odd' value at 1J, there is a trend for high inter-individual MAD values at low stimulus intensities, and for this variance to decrease as stimulus intensity increases. 

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.14
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
##  [1] bindrcpp_0.2.2  forcats_0.3.0   stringr_1.3.1   dplyr_0.7.6    
##  [5] purrr_0.2.5     readr_1.1.1     tidyr_0.8.1     tibble_1.4.2   
##  [9] ggplot2_3.0.0   tidyverse_1.2.1 magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.19     cellranger_1.1.0 pillar_1.3.0     compiler_3.5.1  
##  [5] plyr_1.8.4       bindr_0.1.1      tools_3.5.1      digest_0.6.17   
##  [9] lubridate_1.7.4  jsonlite_1.5     evaluate_0.11    nlme_3.1-137    
## [13] gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.2  rlang_0.2.2     
## [17] cli_1.0.1        rstudioapi_0.8   yaml_2.2.0       haven_1.1.2     
## [21] withr_2.1.2      xml2_1.2.0       httr_1.3.1       knitr_1.20      
## [25] hms_0.4.2        rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.4
## [29] glue_1.3.0       R6_2.2.2         readxl_1.1.0     rmarkdown_1.10  
## [33] modelr_0.1.2     backports_1.1.2  scales_1.0.0     htmltools_0.3.6 
## [37] rvest_0.3.2      assertthat_0.2.0 colorspace_1.3-2 labeling_0.3    
## [41] stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0    broom_0.5.0     
## [45] crayon_1.3.4
```
