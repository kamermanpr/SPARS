---
title: "SPARS trial B"
subtitle: "Descriptive plots of the stimulus-response relationship for the SPARS, SRS, and pain NRS"
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

This script assesses and contrasts the stimulus-response characteristics of the SPARS, sensation rating scale (SRS), and pain NRS, through the use of descriptive plots.

The three scales measure the following ranges of somatic sensation:

- pain NRS: 0 _(no pain)_ to 100 _(worst pain you can imagine)_

- SRS: 0 _(no sensation)_ to 100 _(pain)_

- SPARS: -50 _(no sensation)_, 0 _(pain threshold)_, +50 _(worst pain you can imagine)_

Because the the stimulus range was centred on the pre-determined pain threshold of each participant (compared to the fixed range of intensities used in Trial A), all analyses use the rank order of the nine stimulus intensities each participant was exposed to rather than the absolute intensities of the stimuli used.

The experimental design involved exposing each participant to four successive experimental blocks of 27 trials (laser stimulations) each for each of the three measurement scales. The sequence of stimulus intensities used within each block was pre-determined, and differed between blocks. The order of in which the measurement scales were assessed was randomized, but for convenience of reporting, the plots are always shown in the order: pain NRS_P, SRS, and SPARS.

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
  select(PID, scale, block_number, intensity_rank, rating, rating_equivalent) %>%
  # Recode scales to CNRS_NP = NRS, NRS_NP = SRS
  mutate(scale = case_when(
      scale == 'NRS_NP' ~ 'SRS',
      scale == 'CNRS_P' ~ 'NRS',
      scale == 'SPARS' ~ 'SPARS'
  ))

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

Data are shown on their original scales: **pain NRS:** 0 to 100, **SRS:** 0 to 100, and **SPARS:** -50 to +50.


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
                          labs(title = paste0(.y, ': Raw participant-level stimulus-response ratings on the pain NRS, SRS and SPARS'),
                               subtitle = 'Plots are conditioned on measurement scale and experimental block\npain NRS: 0 (no pain) to 100 (worst pain you can imagine)\nSRS: 0 (no sensation) to 100 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                               x = 'Stimulus intensity (rank)',
                               y = 'Intensity rating') +
                          geom_hline(yintercept = 0, linetype = 2) +
                          facet_wrap(~ block_sequential)))

# Print plots
walk(plot_raw$plots, ~ print(.x))
```

<img src="figures/1B-stimulus-response-2/raw_ratings-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/raw_ratings-7.png" width="864" style="display: block; margin: auto;" />

### Equivalent units ratings (participant-level)

Raw scores for the pain NRS and SRS scales (both rated on 0 to 100 scales) were converted to SPARS equivalent ranges (-50 to +50). The scaling of the pain NRS and SRS were as follows:

- pain NRS: raw 0 to 100 scores were converted to a 0 to 50 range by dividing 2, such that after the scaling, 0 = _no pain_ and 50 = _worst pain you can imagine_. This is equivalent to the positive range of the SPARS.

- SRS: raw 0 to 100 scores were converted to a -50 to 0 range by subtracting 100, and then dividing 2, such that after the scaling, -50 = _no sensation_ and 0 = _pain_. This is equivalent to the negative range of the SPARS.


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
                          labs(title = paste0(.y, ': Scaled participant-level stimulus-response ratings on the pain NRS, SRS and SPARS'),
                               subtitle = 'Plots are conditioned on measurement scale and experimental block\npain NRS: 0 (no pain) to 50 (worst pain you can imagine)\nSRS: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                               x = 'Stimulus intensity (rank)',
                               y = 'Scaled intensity rating (-50 to 50)') +
                          geom_hline(yintercept = 0, linetype = 2) +
                          facet_wrap(~ block_sequential)))

# Print plots
walk(plot_equi$plots, ~ print(.x))
```

<img src="figures/1B-stimulus-response-2/equivalent_ratings-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/equivalent_ratings-7.png" width="864" style="display: block; margin: auto;" />

### Tukey trimean plots (participant-level)

For each participant, we calculated the Tukey trimean of the intensity rating at each stimulus intensity and for each of the measurement scales. The scaled versions of the pain NRS and SRS were used.


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
                           labs(title = paste0(.y, ': Scaled participant-level stimulus-response rating Tukey trimeans on the pain NRS, SRS and SPARS'),
                                subtitle = 'Orange points: Tukey trimeans | Grey curve: loess curve\nPlots are conditioned on the three scales\npain NRS: 0 (no pain) to 50 (worst pain you can imagine)\nSRS: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
                                x = 'Stimulus intensity (rank)',
                                y = 'Scaled intensity rating (-50 to 50)') +
                           geom_hline(yintercept = 0, linetype = 2) +
                           facet_wrap(~ scale, ncol = 3)))

# Print plots
walk(plot_tm$plot, ~ print(.x))
```

<img src="figures/1B-stimulus-response-2/trimean_plots-1.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-2.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-3.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-4.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-5.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-6.png" width="864" style="display: block; margin: auto;" /><img src="figures/1B-stimulus-response-2/trimean_plots-7.png" width="864" style="display: block; margin: auto;" />

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
    scale_fill_manual(name = 'Measurement scale',
                      values = c('#009E73', '#FFA500', '#56B4E9')) +
    scale_colour_manual(name = 'Measurement scale',
                        values = c('#009E73', '#FFA500', '#56B4E9')) +
    scale_y_continuous(limits = c(-50, 50)) +
    scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
    labs(title = 'Scaled group-level stimulus-response ratings on the pain NRS, SRS and SPARS',
         subtitle = 'Points (dodged for clarity): Tukey trimeans | Curves: loess lines\npain NRS: 0 (no pain) to 50 (worst pain you can imagine)\nSRS: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
         x = 'Stimulus intensity (rank)',
         y = 'Scaled intensity rating (-50 to 50)') +
    geom_hline(yintercept = 0, linetype = 2)
```

<img src="figures/1B-stimulus-response-2/summary_plot1-1.png" width="864" style="display: block; margin: auto;" />

```r
# Publication plot
p <- data_tm %>%
    ggplot(data = .) +
    aes(x = intensity_rank,
        y = tri_mean,
        colour = scale) +
    geom_segment(x = 0.8, xend = 9,
                 y = 0, yend = 0,
                 size = 0.6,
                 linetype = 2,
                 colour = '#000000') +
    geom_point(size = 2,
               position = position_dodge(width = 0.4)) +
    geom_smooth(method = 'loess',
                se = FALSE,
                size = 1) +
    geom_segment(x = 0.7, xend = 0.7,
                 y = -50.15, yend = 50.15,
                 size = 1.2,
                 colour = '#000000') +
    geom_segment(x = 0.995, xend = 9.006,
                 y = -55, yend = -55,
                 size = 1.2,
                 colour = '#000000') +
    labs(x = 'Relative stimulus intensity',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25),
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.7, 9.2),
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 9, by = 1)) +
    scale_fill_manual(name = 'Scale',
                      values = c('#009E73', '#FFA500', '#56B4E9')) +
    scale_colour_manual(name = 'Scale',
                        values = c('#009E73', '#FFA500', '#56B4E9')) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 12),
          legend.background = element_rect(colour = '#000000'),
          legend.position = c(0.11, 0.9),
          legend.title = element_blank(),
          axis.text = element_text(size = 16,
                                    colour = '#000000'),
          axis.title = element_text(size = 16,
                                    colour = '#000000'))

ggsave(filename = 'figures/figure_11.pdf',
       plot = p,
       width = 6,
       height = 5)
```


```r
# Generate plot
data %>%
  ggplot(data = .) +
    aes(x = factor(intensity_rank),
        y = rating_equivalent,
        colour = scale,
        fill = scale) +
    geom_boxplot(alpha = 0.6) +
    scale_fill_manual(name = 'Measurement scale',
                      values = c('#009E73', '#FFA500', '#56B4E9')) +
    scale_colour_manual(name = 'Measurement scale',
                        values = c('#009E73', '#FFA500', '#56B4E9')) +
    scale_y_continuous(limits = c(-50, 50)) +
    labs(title = 'Scaled group-level stimulus-response ratings on the pain NRS, SRS and SPARS',
         subtitle = 'pain NRS: 0 (no pain) to 50 (worst pain you can imagine)\nSRS: -50 (no sensation) to 0 (pain)\nSPARS: -50 (no sensation), 0 (pain threshold), +50 (worst pain you can imagine)',
         x = 'Stimulus intensity (rank)',
         y = 'Scaled intensity rating (-50 to 50)') +
    geom_hline(yintercept = 0, linetype = 2)
```

<img src="figures/1B-stimulus-response-2/summary_plot2-1.png" width="864" style="display: block; margin: auto;" />

----

# Summary

In general, the ratings provided on the SPARS were less than those provided on the pain NRS (even at higher stimulus intensities), and greater than those provided on the SRS (even at lower stimulus intensities). This indicates that the extra dimensionality of the SPARS compared to the other two scales (the SPARS allowing the ratings of stimulus intensities from _no sensation_ to _worst pain imaginable_) leads to a compression of the numeric value of the ratings into a narrower band around 0 (pain threshold) compared to the two polar scales (measuring either noxious or non-noxious stimulus ranges).

However, the trend lines indicate that the SPARS scale is reporting the same information as each of the other two scales in their respective domains (sensation of noxious and non-noxious stimuli), but the sensitivity of the SPARS, in each of the two domains, may be lower than they achieved from the two domain-specific scales.

(Please also see: _outputs/6B-scale-agreement.html_)

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
##  [1] bindrcpp_0.2.2     forcats_0.3.0      stringr_1.3.1
##  [4] dplyr_0.7.5        purrr_0.2.5        readr_1.1.1
##  [7] tidyr_0.8.1        tibble_1.4.2       ggplot2_2.2.1.9000
## [10] tidyverse_1.2.1    magrittr_1.5
##
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.4   reshape2_1.4.3     haven_1.1.1
##  [4] lattice_0.20-35    colorspace_1.3-2   htmltools_0.3.6
##  [7] yaml_2.1.19        rlang_0.2.1        pillar_1.2.3
## [10] foreign_0.8-70     glue_1.2.0         withr_2.1.2
## [13] RColorBrewer_1.1-2 modelr_0.1.2       readxl_1.1.0
## [16] bindr_0.1.1        plyr_1.8.4         munsell_0.4.3
## [19] gtable_0.2.0       cellranger_1.1.0   rvest_0.3.2
## [22] psych_1.8.4        evaluate_0.10.1    labeling_0.3
## [25] knitr_1.20         parallel_3.5.0     broom_0.4.4
## [28] Rcpp_0.12.17       scales_0.5.0.9000  backports_1.1.2
## [31] jsonlite_1.5       mnormt_1.5-5       hms_0.4.2
## [34] digest_0.6.15      stringi_1.2.2      grid_3.5.0
## [37] rprojroot_1.3-2    cli_1.0.0          tools_3.5.0
## [40] lazyeval_0.2.1     crayon_1.3.4       pkgconfig_2.0.1
## [43] xml2_1.2.0         lubridate_1.7.4    assertthat_0.2.0
## [46] rmarkdown_1.9      httr_1.3.1         rstudioapi_0.7
## [49] R6_2.2.2           nlme_3.1-137       compiler_3.5.0
```