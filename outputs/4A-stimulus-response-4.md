---
title: "SPARS trial A"
subtitle: "Regression diagnostics for the best-fit linear mixed model of the SPARS stimulus-response relationship"
author: "Peter Kamerman"
date: "13 Feb 2018"
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

This script is part 4 of our analysis of the stimulus-response characteristics of the SPARS. This script generates diagnostics on the final (best) linear mixed model of the relationship between stimulus intensity and SPARS rating.

Descriptive plots of the data are provided in _4A-stimulus-response-1.html_, modelling of the stimulus-response relationship is described in _4A-stimulus-response-2.html_, and the scale attributes are described in _4A-stimulus-reponse-3.html_.

----

# Import and cleaning

For details on the importing and preliminary cleaning of the data, please refer to: _4A-stimulus-response-1.html_.




----

# Diagnostics 

The final (best) model was a cubic model. Diagnostics were run on this model only, and we examined level 1 residuals (conditional / fixed effects), and level 2 residuals (random effects) and influence points [^1].

[^1]: Loy A, Hofmann H. HLMdiag: A suite of diagnostics for hierarchical linear models in R. J. Stat. Softw. 2014;56:1–28. [Available](https://www.jstatsoft.org/article/view/v056i05/v56i05.pdf)




### Generate residuals 


```r
# Level 1 residuals
## Standardized
lmm_resid1 <- HLMresid(lmm3b,
                       level = 1,
                       type = 'LS',
                       standardize = TRUE)

# Semi-standardized residuals (used for assessing homoscedasticity)
lmm_ssresid1 <- HLMresid(lmm3b,
                         level = 1,
                         type = 'LS',
                         standardize = 'semi')

# Level 2 residuals
## Standardized
lmm_resid2 <- HLMresid(lmm3b,
                       level = 'PID',
                       type = 'EB') 
```

### Level 1 residuals: linearity

The relationship between predictor(s) and outcome for a linear model should be linear. This relationship can be observed by plotting the level 1 standardized residuals against the predictors. The scatter of residuals should show no pattern, and be centered around 0.


```r
# Standardized residuals vs intensity
ggplot(data = lmm_resid1) +
    aes(x = `poly(intensity, 3)`[, 1],
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2,
               linetype = 2) +
    geom_hline(yintercept = 2,
               linetype = 2) +
    labs(title = 'Cubic model: Level 1 residuals vs intensity',
         subtitle = 'Assess linearity of the intensity term | Blue line: linear regression line',
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = 'Stimulus intensity')
```

<img src="figures/4A-stimulus-response-4/resid1_linearity-1.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^2
ggplot(data = lmm_resid1) +
    aes(x = `poly(intensity, 3)`[, 2],
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2,
               linetype = 2) +
    geom_hline(yintercept = 2,
               linetype = 2) +
    labs(title = expression(paste('Cubic model: Level 1 residuals vs ', intensity^2)),
         subtitle = expression(paste('Assess linearity of the ', intensity^2, ' term | Blue line: linear regression line')),
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = expression(Stimulus~intensity^2)) 
```

<img src="figures/4A-stimulus-response-4/resid1_linearity-2.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^3
ggplot(data = lmm_resid1) +
    aes(x = `poly(intensity, 3)`[, 3],
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2,
               linetype = 2) +
    geom_hline(yintercept = 2,
               linetype = 2) +
    labs(title = expression(paste('Cubic model: Level 1 residuals vs ', intensity^3)),
         subtitle = expression(paste('Assess linearity of the ', intensity^3, ' term | Blue line: linear regression line')),
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = expression(Stimulus~intensity^3)) 
```

<img src="figures/4A-stimulus-response-4/resid1_linearity-3.png" width="672" style="display: block; margin: auto;" />

The regression curve for the quadratic term shows some signs of deviating from slope = 0, but otherwise the model specification (in terms of linearity) looks okay. Based on the overall pictire, we accept that the condition of linearity for the cubic model.

### Level 1 residuals: homoscedasticity

The variance of residuals should be constant across the range of the predictor(s). This relationship can be observed by plotting the level 1 semi-standardized residuals against the predictors. Like the assessment of linearity, the residuals should be centered on 0, and show no pattern in the scatter of points.


```r
# Standardized residuals vs intensity
ggplot(data = lmm_ssresid1) +
    aes(x = `poly(intensity, 3)`[, 1],
        y = semi.std.resid) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = 'Cubic model: Level 1 residuals vs intensity',
         subtitle = 'Assess homoscedasticity for the intensity term',
         y = 'Semi-standardized residuals',
         x = 'Stimulus intensity')
```

<img src="figures/4A-stimulus-response-4/resid1_variance-1.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^2
ggplot(data = lmm_ssresid1) +
    aes(x = `poly(intensity, 3)`[, 2],
        y = semi.std.resid) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = expression(paste('Cubic model: Level 1 residuals vs ', intensity^2)),
         subtitle = expression(paste('Assess homoscedasticity for the ', intensity^2, ' term')),
         y = 'Semi-standardized residuals',
         x = expression(Stimulus~intensity^2)) 
```

<img src="figures/4A-stimulus-response-4/resid1_variance-2.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^3
ggplot(data = lmm_ssresid1) +
    aes(x = `poly(intensity, 3)`[, 3],
        y = semi.std.resid) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = expression(paste('Cubic model: Level 1 residuals vs ', intensity^3)),
         subtitle = expression(paste('Assess homoscedasticity for the ', intensity^3, ' term')),
         y = 'Semi-standardized residuals',
         x = expression(Stimulus~intensity^3)) 
```

<img src="figures/4A-stimulus-response-4/resid1_variance-3.png" width="672" style="display: block; margin: auto;" />

There is no obvious pattern to the scatter of residuals across any of the fixed effect terms. So we accept that the residuals are homoscedastic in the cubic model.

### Level 1 residuals: residual distribution

Residuals should be normally distributed. There are various methods of examining the distribution, and we have chosen the QQ-plot method, which plots the quantiles of the standardized residuals against a theoretical (Gaussian) quantile distribution. Points should line on the line of identity of the two sets of quantiles follow the same distribution. 


```r
# Standardized residuals vs intensity
ggplot_qqnorm(x = lmm_resid1$std.resid, 
              line = "rlm") +
    labs(title = 'Cubic model: QQ-plot of level 1 residuals',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Standardized residuals')
```

<img src="figures/4A-stimulus-response-4/resid1_ditribution-1.png" width="672" style="display: block; margin: auto;" />

There is minor deviation at the extremes, but on the whole, we are satisfied that the cubic model fits the assumption of normally sdistributed residuals. 

### Level 2 residuals: residual distribution

Level 2 residuals can be used to identify predictors that should be included in the model, but since we are only assessing the effect of stimulus strength on SPARS rating, we have only assessed whether the level 2 residuals (intercept and slope) meet the assumption of being normally distributed (assessed using QQ-plots).


```r
# Generate QQplots 
qq1 <- ggplot_qqnorm(x = lmm_resid2$`(Intercept)`, 
              line = "rlm") +
    labs(title = 'Cubic model: QQ-plot of level 2 residuals (Intercept)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

qq2 <- ggplot_qqnorm(x = lmm_resid2$intensity, 
              line = "rlm") +
    labs(title = 'Cubic model: QQ-plot of level 2 residuals (slope: intensity)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

# Plot
qq1 + qq2
```

<img src="figures/4A-stimulus-response-4/resid2_linearity-1.png" width="672" style="display: block; margin: auto;" />

Although the data are sparse, we are satisfied that the level 2 residuals for the intercept and the slope of the cubic model fit the assumption of being normally sdistributed.

### influence points

We assessed three aspects of influence (data that significantly model coefficients):

- The variance component (random effects) was assesed using the relative variance change metric, which calculates the impact of deleting observational units of the variance of the residuals, random intercept, random slope, and covariance of the random slope and random intercept.

- Leverage was used to assess fitted values. The assessment involves assessing the rate of change in the predicted response with respect to the observed response.

- Cook's Distance was used to assess the influence of fixed effects. The metric measures the distance between the fixed effects estimates obtained from the full model to that obtained from the reduced data (observations removed). 

In all cases, we treated the individual (indicated using PID) as the unit of observation, and we used internal scaling to set the diagnostic cutoffs for each metric. The cutoffs were determined as: $3^{rd}~Quartile + (3 \cdot IQR)$.


```r
# Prepare relative variance change (RCV)
influence_rvc <- rvc(lmm3b, 
                     group = 'PID')

# Prepare Cook's distance
influence_cooks <- cooks.distance(lmm3b, 
                                  group = 'PID')

# Prepare leverage 
## (Assessed at the level of PID, and not the individual observation)
influence_leverage <- leverage(lmm3b,
                               level = 'PID')
```

### Random effects

Estimation of the variance component was undertaken by calculating relative variance change (RCV). RVC is close to zero when deletion of observational units from the model does not have a large infuence on the variance component.
 

```r
# Plot
dotplot_diag(x = influence_rvc[ , 1], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the residual variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_random-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 2], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random intercept variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_random-2.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 3], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_random-3.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 4], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope and intercept covariance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_random-4.png" width="672" style="display: block; margin: auto;" />

One value (PID11) is below the cutoff for the relative variance change for random slope and intercept covariance. The extent of the deviation is minor, and was ignored.

### Fitted values

Assessing whether observations are unusual with regard to the fitted values
and explanatory variables using leverage. We assessed leverage at two levels: i) fixed effects, and ii) unconfounded (by fixed effects) random effects.


```r
dotplot_diag(x = influence_leverage[, 2], 
             cutoff = "internal",
             name = "leverage") + 
    labs(title = 'Leverage: fixed effects',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Leverage',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_leverage-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_leverage[, 4], 
             cutoff = "internal",
             name = "leverage") + 
    labs(title = 'Leverage: unconfounded random effects',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Leverage',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_leverage-2.png" width="672" style="display: block; margin: auto;" />

Participants 5 and 12 were identified as having high leverage. Their data is highlighted in the plot below. Data look okay. 


```r
data_tm %>%
    mutate(faux_colour = case_when(
        PID == 'ID05' ~ 'high leverage ID05',
        PID == 'ID12' ~ 'high leverage ID12',
        TRUE ~ 'low leverage'
    )) %>%
    ggplot(data = .) +
    aes(x = intensity,
        y = tri_mean,
        colour = faux_colour) +
    geom_point(position = position_jitter(width = 0.05)) +
    geom_smooth(aes(colour = faux_colour), 
                method = 'loess',
                se = FALSE, 
                size = 0.6) +
    scale_colour_manual(name = 'Leverage points',
                        values = c('#0072B2', '#D55E00', '#999999')) +
    labs(title = 'Inspection of high-leverage participants',
        x = 'Stimulus intensity (J)',
        y = 'SPARS rating [-50 to 50]') +
    scale_y_continuous(limits = c(-50, 50)) +
    scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5))
```

<img src="figures/4A-stimulus-response-4/leverage plot-1.png" width="672" style="display: block; margin: auto;" />

#### Fixed effects

Influence points were assessed by calculating Cook's Distance metrics.


```r
# Plot data
dotplot_diag(x = influence_cooks, 
             cutoff = "internal",
             name = "cooks.distance") + 
    labs(title = 'Influence: Cooks Distance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Cooks Distance',
         x = 'Participant ID')
```

<img src="figures/4A-stimulus-response-4/influence_fixed-1.png" width="672" style="display: block; margin: auto;" />

Based on There are no influential fixed effects.

### Summary

The cubic model is well-specified.

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
##  [1] bindrcpp_0.2       patchwork_0.0.1    HLMdiag_0.3.1     
##  [4] lme4_1.1-15        Matrix_1.2-12      forcats_0.2.0     
##  [7] stringr_1.2.0      dplyr_0.7.4        purrr_0.2.4       
## [10] readr_1.1.1        tidyr_0.8.0        tibble_1.4.2      
## [13] ggplot2_2.2.1.9000 tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-131       lubridate_1.7.1    httr_1.3.1        
##  [4] rprojroot_1.3-2    TMB_1.7.12         tools_3.4.3       
##  [7] backports_1.1.2    DT_0.4             R6_2.2.2          
## [10] sjlabelled_1.0.7   lazyeval_0.2.1     mgcv_1.8-23       
## [13] colorspace_1.3-2   nnet_7.3-12        withr_2.1.1.9000  
## [16] tidyselect_0.2.3   mnormt_1.5-5       emmeans_1.1       
## [19] compiler_3.4.3     cli_1.0.0          rvest_0.3.2       
## [22] xml2_1.2.0         sandwich_2.4-0     labeling_0.3      
## [25] effects_4.0-0      scales_0.5.0.9000  lmtest_0.9-35     
## [28] mvtnorm_1.0-7      psych_1.7.8        blme_1.0-4        
## [31] digest_0.6.15      foreign_0.8-69     minqa_1.2.4       
## [34] rmarkdown_1.8      stringdist_0.9.4.6 pkgconfig_2.0.1   
## [37] htmltools_0.3.6    htmlwidgets_1.0    pwr_1.2-1         
## [40] rlang_0.1.6        readxl_1.0.0       rstudioapi_0.7    
## [43] shiny_1.0.5        RLRsim_3.1-3       bindr_0.1         
## [46] zoo_1.8-1          jsonlite_1.5       sjPlot_2.4.1      
## [49] modeltools_0.2-21  bayesplot_1.4.0    Rcpp_0.12.15      
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
