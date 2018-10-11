---
title: "Supplement 9"
subtitle: "Experiment 1 -- Sensitivity of the SPARS to changes in stimulus intensity"
author: "Peter Kamerman"
date: "11 Oct 2018"
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

This script is part 5 of our analysis of the stimulus-response characteristics of the SPARS. In this analysis we examined whether there is a linear relationship between change in stimulus intensity and change in rating magnitude between two successive stimuli, irrespective of the direction of the change?

Descriptive plots of the data are provided in _"outputs/suppl\_05\_4A-stimulus-response-1.html"_, modelling of the stimulus-response relationship is described in _"outputs/suppl\_06\_4A-stimulus-response-2.html"_, the diagnostics on the final linear mixed model are described in _"outputs/suppl\_07\_4A-stimulus-response-3.html"_, the stability of the model is described in _"outputs/suppl\_08\_4A-stimulus-response-4.html"_, and the variance in ratings at each stimulus intensity is described in _"outputs/suppl\_10\_4A-stimulus-reponse-6.html"_.

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
  select(PID, block, block_order, trial_number, intensity, intensity_char, rating) 

############################################################
#                                                          #
#          Define Wald CI function for robust LMM          #
#                                                          #
############################################################
# Adapted from code provided Ben Bolker on StackExchange: https://stats.stackexchange.com/questions/233800/how-can-i-get-confidence-intervals-for-fixed-effects-using-the-rlmer-function-r

confint.rlmerMod <- function(object, level = 0.95) {
  # Extract beta coefficients
  beta <- fixef(object)
  # Extract names of coefficients
  parm <- names(beta)
  # Extract standard errors for the coefficients
  se <- sqrt(diag(vcov(object)))
  # Set level of confidence interval
  z <- qnorm((1 + level) / 2)
  # Calculate CI
  ctab <- cbind(beta - (z * se), 
                beta + (z * se))
  # label column names
  colnames(ctab) <- c(paste(100 * ((1 - level) / 2), '%'),
                      paste(100 * ((1 + level) / 2), '%'))
  # Output
  return(as.data.frame(ctab[parm, ]))
  }

############################################################
#                                                          #
#                    Generate core data                    #
#                                                          #
############################################################
# Calculate lagged data 
data_scale <- data %>%
    # Get lag-1 stimulus intensity and FEST rating by PID
    group_by(PID) %>%
    mutate(intensity_lag = lag(intensity),
           rating_lag = lag(rating)) %>%
    # Ungroup and remove incomplete cases greated by lag function
    ungroup() %>%
    filter(complete.cases(.))

# Calculate change in stimulus intensity and SPARS rating between successive stimuli
data_scale %<>%
    mutate(intensity_delta = intensity - intensity_lag,
           rating_delta = rating - rating_lag) %>%
    # Determine the direction of the change in stimulus intensity
    mutate(change_direction = case_when(
        intensity_delta > 0 ~ 'up',
        intensity_delta < 0 ~ 'down',
        intensity_delta == 0 ~ 'no change'
        ))

# Filter out 'no change' change_direction
data_reduced <- data_scale %>%
    filter(change_direction != 'no change') 
```

----

# Exporatory plots 

### Group-level


```r
# Generate the plots for each individual
data_scale %>%
    # Filter out 'no change'
    filter(change_direction != 'no change') %>% 
    ggplot(data = .) +
    aes(y = rating_delta,
        x = intensity_delta,
        colour = change_direction) +
    geom_point() +
    geom_smooth(method = 'lm') +
    scale_color_manual(name = 'Direction of intensity change: ',
                       labels = c('Down', 'Up'),
                       values = c("#999999", "#323232")) +
    scale_x_continuous(limits = c(-3.5, 3.5), 
                       breaks = seq(from = -3, to = 3, by = 1),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(-70, 70), 
                       breaks = seq(from = -60, to = 60, by = 20),
                                    expand = c(0,0)) +
    labs(title = 'Group-level plot: Change in SPARS rating vs Change in stimulus intensity \nbetween two successive stimuli',
         subtitle = 'Grey points: Individual responses for all trials | Coloured line: Linear trend for context\nDark grey points/line: When a stimulus was of less intensity than the preceding stimulus | \nLight grey points/line: When a stimulus was of greater intensity than the preceding stimulus.\nData have been omitted when successive stimuli were of the same intensity (zero change).',
         x = expression(Delta~stimulus~intensity~(J)),
         y = expression(Delta~SPARS~rating~(predicted))) +
    theme(legend.position = 'top')
```

<img src="figures/suppl_09_4A-stimulus-response-5/exploratory_plots-1.png" width="672" style="display: block; margin: auto;" />

```r
## Publication plot
p <- data_scale %>%
    # Filter out 'no change'
    filter(change_direction != 'no change') %>% 
    ggplot(data = .) +
    aes(y = rating_delta,
        x = intensity_delta,
        colour = change_direction) +
    geom_point() +
    geom_smooth(method = 'lm',
                se = FALSE) +
    geom_segment(x = -3.3, xend = 3.05, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2,
                 colour = '#000000') +
    geom_segment(x = -3.3, xend = -3.3, 
                 y = -90.23, yend = 90.23, 
                 size = 1.2,
                 colour = '#000000') +
    geom_segment(x = -3.005, xend = 3.005, 
                 y = -98, yend = -98, 
                 size = 1.2,
                 colour = '#000000') +
    labs(x = 'Change in stimulus intensity (J)',
         y = 'Change in SPARS rating') +
    scale_colour_manual(name = 'Direction of change: ',
                        values = c("#999999", "#323232"),
                        labels = c('Decreasing intensity', 'Increasing intensity')) +
    scale_y_continuous(limits = c(-98, 90.25), 
                       expand = c(0, 0),
                       breaks = c(-90, -60, -30, 0, 30, 60, 90)) +
    scale_x_continuous(limits = c(-3.3, 3.2), 
                       expand = c(0, 0),
                       breaks = seq(from = -3, to = 3, by = 1)) +
    theme_bw() +
    theme(legend.title = element_text(size = 14, 
                                      face = 'bold'),
          legend.text = element_text(size = 12),
          legend.background = element_rect(colour = '#000000'),
          legend.position = c(0.23, 0.895),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16,
                                   colour = '#000000'),
          axis.title = element_text(size = 16,
                                    colour = '#000000'))

ggsave(filename = 'figures/figure_7.pdf',
       plot = p,
       width = 6,
       height = 5)
```

### Participant-level

**Participant-level plot: Change in SPARS rating vs change in stimulus intensity between two successive stimuli**  
Coloured points: Per trial responses | Coloured line: Linear trend for the group  
<span style="#009E79">Green points/line</span>: When a stimulus was less than the preceding stimulus intensity | <span style="E25E1E">Orange points/line</span>: When a stimulus was greater than the preceding stimulus intensity.  
Data when successive stimuli were of the same intensity (zero change) have been omitted.


```r
# Generate the plots for each individual
scale_plots <- data_scale %>%
    # Filter out 'no change'
    filter(change_direction != 'no change') %>% 
    # Nest data by PID
    group_by(PID) %>%
    nest() %>%
    # Plot data
    mutate(plot = map2(.x = data,
                       .y = unique(PID),
                       ~ ggplot(data = .x) +
                           aes(y = rating_delta,
                               x = intensity_delta,
                               colour = change_direction) +
                       geom_point() +
                       geom_smooth(method = 'lm') +
                       scale_color_manual(name = 'Direction of\nintensity change',
                                          labels = c('Down', 'Up'),
                                          values = c('#999999', '#323232')) +
                       scale_x_continuous(limits = c(-3.5, 3.5), 
                                          breaks = seq(from = -3, to = 3, by = 1),
                                          expand = c(0,0)) +
                       scale_y_continuous(limits = c(-70, 70), 
                                          breaks = seq(from = -60, to = 60, by = 20),
                                          expand = c(0,0)) +
                       labs(subtitle = paste0('[', .y, ']'),
                            x = expression(Delta~stimulus~intensity~(J)),
                            y = expression(Delta~SPARS~rating)) +
                       theme(legend.position = 'none'))) 

# Print plots
wrap_plots(scale_plots$plot, ncol = 4)
```

<img src="figures/suppl_09_4A-stimulus-response-5/exploratory_plots2-1.png" width="960" style="display: block; margin: auto;" />

----

# Linear mixed model regression

To examine whether the relationship between change in rating intensity and change in stimulus intensity was affected by the direction of the change in stimulus intensity (i.e., whether the preceding stimulus was greater than or less than the current stimulus), we fit a linear mixed model regression (with random intercept) that included an interaction term between change in stimulus intensity and the direction of the change in stimulus intensity ('up' or 'down'):

$$\Delta~SPARS~rating \thicksim \Delta~stimulus~intenisty~\ast~direction~of~change~+~(1~|~participant)$$

### Generate Robust LMM

Preliminary analysis indicated that influence points may be an issue, and so we modelled the data using robust methods.

We generated two models. The first model included an interaction term between change in stimulus intensity and direction of change, and the second model did not. Comparing the models allows one you check whether the slope of the SPARS rating vs change in stimulus intensity relationship was dependent on the direction of the change in stimulus intensity.


```r
# Linear mixed model with interaction
scale_lmm <- rlmer(rating_delta ~ intensity_delta * change_direction + (1 | PID),
                   method = 'DASvar', 
                   data = data_reduced)

# Linear mixed model without interaction
scale_lmm2 <- rlmer(rating_delta ~ intensity_delta + change_direction + (1 | PID),
                    method = 'DASvar', 
                    data = data_reduced)

# Inspect models
## With interaction
summary(scale_lmm)
```

```
## Robust linear mixed model fit by DASvar 
## Formula: rating_delta ~ intensity_delta * change_direction + (1 | PID) 
##    Data: data_reduced 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.3441 -0.6419  0.0219  0.6425  3.9253 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  PID      (Intercept)   0.0     0.00   
##  Residual             226.9    15.06   
## Number of obs: 1778, groups: PID, 19
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                          1.5932     0.9628   1.655
## intensity_delta                     13.8459     0.7199  19.233
## change_directionup                  -1.8224     1.4222  -1.281
## intensity_delta:change_directionup  -0.9675     1.0292  -0.940
## 
## Correlation of Fixed Effects:
##             (Intr) intns_ chng_d
## intnsty_dlt  0.850              
## chng_drctnp -0.677 -0.575       
## intnsty_d:_ -0.594 -0.699 -0.051
## 
## Robustness weights for the residuals: 
##  1397 weights are ~= 1. The remaining 381 ones are summarized as
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.310   0.584   0.749   0.740   0.906   0.999 
## 
## Robustness weights for the random effects: 
##  All 19 weights are ~= 1.
## 
## Rho functions used for fitting:
##   Residuals:
##     eff: smoothed Huber (k = 1.345, s = 10) 
##     sig: smoothed Huber, Proposal II (k = 1.345, s = 10) 
##   Random Effects, variance component 1 (PID):
##     eff: smoothed Huber (k = 1.345, s = 10) 
##     vcp: smoothed Huber, Proposal II (k = 1.345, s = 10)
```

```r
ci_interaction <- confint.rlmerMod(object = scale_lmm)
knitr::kable(ci_interaction,
             caption = 'Interaction model: Wald 95% confidence intervals of fixed effects')
```



Table: Interaction model: Wald 95% confidence intervals of fixed effects

                                           2.5 %       97.5 %
-----------------------------------  -----------  -----------
(Intercept)                           -0.2939063    3.4802539
intensity_delta                       12.4348856   15.2568704
change_directionup                    -4.6098420    0.9651317
intensity_delta:change_directionup    -2.9847235    1.0497943

```r
## Without interaction
summary(scale_lmm2)
```

```
## Robust linear mixed model fit by DASvar 
## Formula: rating_delta ~ intensity_delta + change_direction + (1 | PID) 
##    Data: data_reduced 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.3861 -0.6245  0.0396  0.6267  3.9241 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  PID      (Intercept)   0.0     0.00   
##  Residual             226.8    15.06   
## Number of obs: 1778, groups: PID, 19
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)          1.0767     0.7740   1.391
## intensity_delta     13.3467     0.5143  25.951
## change_directionup  -1.8502     1.4198  -1.303
## 
## Correlation of Fixed Effects:
##             (Intr) intns_
## intnsty_dlt  0.755       
## chng_drctnp -0.881 -0.856
## 
## Robustness weights for the residuals: 
##  1397 weights are ~= 1. The remaining 381 ones are summarized as
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.307   0.583   0.742   0.740   0.905   0.998 
## 
## Robustness weights for the random effects: 
##  All 19 weights are ~= 1.
## 
## Rho functions used for fitting:
##   Residuals:
##     eff: smoothed Huber (k = 1.345, s = 10) 
##     sig: smoothed Huber, Proposal II (k = 1.345, s = 10) 
##   Random Effects, variance component 1 (PID):
##     eff: smoothed Huber (k = 1.345, s = 10) 
##     vcp: smoothed Huber, Proposal II (k = 1.345, s = 10)
```

```r
ci_main <- confint.rlmerMod(object = scale_lmm2)
knitr::kable(ci_main,
             caption = 'Main effects model: Wald 95% confidence intervals of fixed effects')
```



Table: Main effects model: Wald 95% confidence intervals of fixed effects

                           2.5 %       97.5 %
-------------------  -----------  -----------
(Intercept)           -0.4403447    2.5938338
intensity_delta       12.3386421   14.3546592
change_directionup    -4.6329263    0.9325214

```r
## Compare the models
### Prepare for plotting
ci_interaction %<>%
    rownames_to_column(var = 'Fixed effects') %>%
    mutate(model = 'interaction') %>% 
    filter(`Fixed effects` != 'intensity_delta:change_directionup')

ci_main %<>%
    rownames_to_column(var = 'Fixed effects') %>%
    mutate(model = 'main effects')

ci <- bind_rows(ci_main, ci_interaction)

ci %>% 
    mutate(`Fixed effects` = fct_relevel(`Fixed effects`,
                                         '(Intercept)', 
                                         'intensity_delta', 
                                         'change_directionup')) %>%
    ggplot(data = .) +
    aes(x = `Fixed effects`,
        y = (`2.5 %` + `97.5 %`) / 2,
        ymin = `2.5 %`,
        ymax = `97.5 %`,
        colour = model,
        fill = model) +
    geom_crossbar(position = position_dodge(width = 1),
                  alpha = 0.7) +
    scale_fill_manual(name = 'Model', 
                      labels = c('Interaction', 'Main effects only'),
                      values = c('#999999', '#323232')) +
    scale_colour_manual(name = 'Model', 
                        labels = c('Interaction', 'Main effects only'),
                        values = c('#999999', '#323232')) +
    labs(title = 'Comparison of the Wald 95% confidence intervals of the fixed effects (intercept and main effects)',
         subtitle = 'The middle line in each bar shows the mid-point between the interval edges',
         y = 'Units')
```

<img src="figures/suppl_09_4A-stimulus-response-5/segmented_lmm-1.png" width="864" style="display: block; margin: auto;" />

Using the confidence intervals of the fixed effect estimates to assess significance (intervals that excluded zero were considered significant at the 5% level), we assessed the interaction term to be not significant. When comparing the models, the fixed effect estimates for the two models were similar (see the plot above), and therefore we conclude that the direction of the change in stimulus intensity between two successive stimuli does not influence the slope of the relationship between change in stimulus intensity and change in SPARS rating.

### Model diagnostics

A basic diagnostic assessment did not identify any violations of the model assumptions.


```r
# Generate plots
p_list <- list(plot(scale_lmm2, which = 1), 
               plot(scale_lmm2, which = 2),
               plot(scale_lmm2, which = 3))
x <- plot(scale_lmm2, which = 1) +
    scale_colour_viridis_c()
x
```

```
## NULL
```

```r
str(x)
```

```
##  NULL
```

```r
class(x)
```

```
## [1] "NULL"
```

```r
# Print plots
walk(p_list, ~print(.x))
```

<img src="figures/suppl_09_4A-stimulus-response-5/diagnostics-1.png" width="672" style="display: block; margin: auto;" /><img src="figures/suppl_09_4A-stimulus-response-5/diagnostics-2.png" width="672" style="display: block; margin: auto;" /><img src="figures/suppl_09_4A-stimulus-response-5/diagnostics-3.png" width="672" style="display: block; margin: auto;" />

### Plot of predicted values


```r
# Predicted values from the main effects model
## Generate 'newdata'
new_data <- data_reduced %>%
    select(intensity_delta, change_direction, PID)

## Get predictions
yhat <- data_frame(y = predict(object = scale_lmm2,
                               newdata = new_data))

## Bind to 'new_data/extra_data'
predicted <- bind_cols(yhat, new_data) %>%
    rename(x = intensity_delta,
           group = change_direction)

## Plot
ggplot(data = predicted) +
    aes(x = x,
        y = y,
        colour = group) +
    geom_line() +
    geom_point() +
    scale_color_manual(name = 'Direction of\nintensity change',
                       labels = c('Down', 'Up'),
                       values = c('#999999', '#323232')) +
    scale_x_continuous(limits = c(-3, 3), 
                       breaks = seq(from = -3, to = 3, by = 1)) +
    scale_y_continuous(limits = c(-60, 60), 
                       breaks = seq(from = -60, to = 60, by = 20)) +
    labs(title = 'Plot of predicted change in SPARS ratings vs change stimulus intensity',
         x = expression(Delta~stimulus~intensity~(J)),
         y = expression(Delta~SPARS~rating~(predicted)))
```

<img src="figures/suppl_09_4A-stimulus-response-5/predicted-1.png" width="672" style="display: block; margin: auto;" />

----

# Summary

There is linear scaling between change in stimulus intensity and change in SPARS rating, and the sensitivity of this response (the slope of the curve) is consistent across increasing and decreasing changes in stimulus intensities. The estimate for change in the change in stimulus intensity (intensity_delta) main effect was 13.3 (95% CI: 12.3 to 14.4), indicating that a single unit change (1 J) in stimulus intensity results in 12.3 to 14.4 unit change in SPARS rating.

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
##  [1] bindrcpp_0.2.2     influence.ME_0.9-9 sjPlot_2.6.0      
##  [4] robustlmm_2.2-1    lmerTest_3.0-1     lme4_1.1-18-1     
##  [7] Matrix_1.2-14      patchwork_0.0.1    forcats_0.3.0     
## [10] stringr_1.3.1      dplyr_0.7.6        purrr_0.2.5       
## [13] readr_1.1.1        tidyr_0.8.1        tibble_1.4.2      
## [16] ggplot2_3.0.0      tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-137       lubridate_1.7.4    httr_1.3.1        
##  [4] rprojroot_1.3-2    numDeriv_2016.8-1  TMB_1.7.14        
##  [7] tools_3.5.1        backports_1.1.2    R6_2.2.2          
## [10] sjlabelled_1.0.14  lazyeval_0.2.1     colorspace_1.3-2  
## [13] nnet_7.3-12        withr_2.1.2        mnormt_1.5-5      
## [16] tidyselect_0.2.4   emmeans_1.2.4      compiler_3.5.1    
## [19] cli_1.0.1          rvest_0.3.2        xml2_1.2.0        
## [22] sandwich_2.5-0     labeling_0.3       effects_4.0-3     
## [25] scales_1.0.0       psych_1.8.4        DEoptimR_1.0-8    
## [28] mvtnorm_1.0-8      robustbase_0.93-3  ggridges_0.5.1    
## [31] digest_0.6.17      foreign_0.8-71     minqa_1.2.4       
## [34] rmarkdown_1.10     stringdist_0.9.5.1 pkgconfig_2.0.2   
## [37] htmltools_0.3.6    highr_0.7          pwr_1.2-2         
## [40] rlang_0.2.2        readxl_1.1.0       rstudioapi_0.8    
## [43] bindr_0.1.1        zoo_1.8-4          jsonlite_1.5      
## [46] modeltools_0.2-22  bayesplot_1.6.0    Rcpp_0.12.19      
## [49] munsell_0.5.0      prediction_0.3.6   fastGHQuad_1.0    
## [52] stringi_1.2.4      multcomp_1.4-8     yaml_2.2.0        
## [55] snakecase_0.9.2    carData_3.0-2      MASS_7.3-50       
## [58] plyr_1.8.4         grid_3.5.1         parallel_3.5.1    
## [61] sjmisc_2.7.5       crayon_1.3.4       lattice_0.20-35   
## [64] ggeffects_0.5.0    haven_1.1.2        splines_3.5.1     
## [67] sjstats_0.17.1     hms_0.4.2          knitr_1.20        
## [70] pillar_1.3.0       estimability_1.3   codetools_0.2-15  
## [73] stats4_3.5.1       glue_1.3.0         evaluate_0.11     
## [76] data.table_1.11.8  modelr_0.1.2       nloptr_1.2.1      
## [79] cellranger_1.1.0   gtable_0.2.0       assertthat_0.2.0  
## [82] coin_1.2-2         xtable_1.8-3       broom_0.5.0       
## [85] survey_3.33-2      coda_0.19-1        viridisLite_0.3.0 
## [88] survival_2.42-6    glmmTMB_0.2.2.0    TH.data_1.0-9
```
