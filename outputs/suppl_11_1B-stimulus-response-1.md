---
title: "Supplement 11"
subtitle: "Experiment 2 -- Stimulus-response characteristics of the SPARS"
author: "Peter Kamerman"
date: "24 Nov 2018"
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

This analysis examines the stimulus-response characteristics of the SPARS.

Unlike Trial A, where participants were exposed to a prescribed range of stimulus intensities (1 to 4J, at 0.25J intervals), in Trial B, all participants were exposed to 9 stimulus intensities (0.25J interval), but the range of stimuli intensities were calibrated against the sensitivity of each participant. For example, a more sensitive participant may have been exposed to 9 stimuli from 1.75J to 3.75J, another participant may be exposed to stimuli from 2.5J to 4.5J.

This design makes performing group-level analyses difficult (the extremes will have fewer observations), and so we transformed the exposure intensities into an  relative scale by ranking (1 to 9) an ordered list of 9 stimulus intensities for each participant. This brought everyone onto the same 1 to 9 scale. 

For transparency, we have performed exploratory plots using the raw stimulus intensity data and the relative intensity data. However, the regression analysis was performed using the relative intensity data only.   

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

# Clean and process data

We performed a basic clean-up of the data, and then calculated _Tukey trimean_ at each stimulus intensity for each participant (participant average), and finally the _median_ of the trimeans at each stimulus intensity across participants (group average). 


```r
############################################################
#                                                          #
#                          Clean                           #
#                                                          #
############################################################
data %<>%
    # Rename block_number
    rename(block = block_number) %>% 
    # Select SPARS scale
    filter(scale == 'SPARS') %>% 
    ungroup() %>%
    arrange(PID)

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

# Calculate the participant average based on 'raw' intensity
data_tm <- data %>% 
  group_by(PID, intensity) %>%
  summarise(tri_mean = tri.mean(rating)) %>% 
  ungroup()

# Calculate the group average based on 'raw' intensity
data_group <- data_tm %>%
  group_by(intensity) %>%
  summarise(median = median(tri_mean)) %>%
  ungroup() 

# Calculate the participant average based on 'relative' intensity
data_tmR <- data %>% 
  group_by(PID, intensity_rank) %>%
  summarise(tri_mean = tri.mean(rating)) %>%
  ungroup()

# Calculate the group average based on 'relative' intensity
data_groupR <- data_tmR %>%
  group_by(intensity_rank) %>%
  summarise(median = median(tri_mean)) %>%
  ungroup() 
```

----

# Stimulus exposure ranges


```r
knitr::kable(data %>% 
                 group_by(PID) %>% 
                 summarise(`Minimum stimulus intensity` = min(intensity),
                           `Maximum stimulus intensity` = max(intensity)),
             caption = 'Range of stimulus intensities covered in each participant')
```



Table: Range of stimulus intensities covered in each participant

PID     Minimum stimulus intensity   Maximum stimulus intensity
-----  ---------------------------  ---------------------------
ID01                          2.25                         4.25
ID02                          2.25                         4.25
ID03                          2.50                         4.50
ID04                          2.50                         4.50
ID05                          2.50                         4.50
ID06                          1.75                         3.75
ID07                          2.25                         4.25

# Exploratory plots

### Group-level stimulus response curve


```r
# Plot (y.axis = raw stimulus intensity)
data_tm %>%
  ggplot(data = .) +
  aes(x = intensity,
      y = tri_mean) +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#656565', 
              size = 0.6) +
  geom_point(data = data_group,
             aes(y = median),
             shape = 21,
             size = 5,
             stroke = 1,
             fill = '#999999') +
  labs(title = 'Group-level stimulus-response plots (raw intensity)',
       subtitle = 'Black circles: participant-level Tukey trimeans | Grey circles: group-level median | Grey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5))
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_group-1.png" width="672" style="display: block; margin: auto;" />

```r
# Plot (y.axis = relative stimulus intensity)
data_tmR %>%
  ggplot(data = .) +
  aes(x = intensity_rank,
      y = tri_mean) +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#656565', 
              size = 0.6) +
  geom_point(data = data_groupR,
             aes(y = median),
             shape = 21,
             stroke = 1,
             size = 5,
             fill = '#999999') +
  labs(title = 'Group-level stimulus-response plots (relative intensity)',
       subtitle = 'Black circles: participant-level Tukey trimeans | Grey circles: group-level median | Grey line: loess curve\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
       x = 'Relative stimulus intensity',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 9, by = 1))
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_group-2.png" width="672" style="display: block; margin: auto;" />

```r
# Publication plot
p <- data_tmR %>%
    ggplot(data = .) +
    aes(x = intensity_rank,
        y = tri_mean) +
    geom_segment(x = 0.8, xend = 9, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2) +
    geom_point(position = position_jitter(width = 0.05)) +
    geom_smooth(method = 'loess',
                se = FALSE,
                colour = '#656565', 
                size = 1) +
    geom_point(data = data_groupR,
               aes(y = median),
               shape = 21,
               stroke = 1,
               size = 5,
               fill = '#999999') +
    geom_segment(x = 0.8, xend = 0.8, 
                 y = -50.15, yend = 50.15, 
                 size = 1.2) +
    geom_segment(x = 0.995, xend = 9.006, 
                 y = -55, yend = -55, 
                 size = 1.2) +
    labs(x = 'Relative stimulus intensity',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25), 
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.8, 9.2), 
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 9, by = 1)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16,
                                    colour = '#000000'),
          axis.title = element_text(size = 16,
                                    colour = '#000000'))

ggsave(filename = 'figures/figure_8.pdf',
       plot = p,
       width = 6,
       height = 5)
```

### Participant-level stimulus response curves

#### All trials


```r
# Plot (y.axis = raw stimulus intensity)
data %>%
  ggplot(data = .) +
  aes(x = intensity,
      y = rating) +
  geom_point() +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#656565',
              size = 0.6) +
  geom_point(data = data_tm,
             aes(y = tri_mean),
             shape = 21,
             size = 3,
             fill = '#999999') +
  labs(title = 'Participant-level stimulus-response plot (raw intensity)',
       subtitle = 'Black circles: individual experimental blocks | Grey circles: Tukey trimean | Grey line: loess curve',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~ PID, ncol = 4)
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_participants-1.png" width="864" style="display: block; margin: auto;" />

```r
# Plot (y.axis = rank stimulus intensity)
data %>%
  ggplot(data = .) +
  aes(x = intensity_rank,
      y = rating) +
  geom_point() +
  geom_smooth(method = 'loess',
              se = FALSE,
              colour = '#656565',
              size = 0.6) +
  geom_point(data = data_tmR,
             aes(y = tri_mean),
             shape = 21,
             size = 3,
             fill = '#999999') +
  labs(title = 'Participant-level stimulus-response plot (relative intensity)',
       subtitle = 'Black circles: individual experimental blocks | Grey circles: Tukey trimean | Grey line: loess curve\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
       x = 'Relative stimulus intensity',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~ PID, ncol = 4)
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_participants-2.png" width="864" style="display: block; margin: auto;" />

#### Trials by experimental block


```r
# Process data (raw stimulus intensity)
data_block <- data %>%
  # Rename blocks
  #mutate(block = sprintf('Block: %s (order: %i)', block, block_order)) %>%
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
                                    colour = '#656565',
                                    size = 0.6) +
                        labs(title = paste(.y, ': Participant-level stimulus-response plots conditioned on experimental block (raw intensity'),
                             subtitle = 'Black circles: individual data points | Grey line: loess curve',
                             x = 'Stimulus intensity (J)',
                             y = 'SPARS rating [-50 to 50]') +
                        scale_y_continuous(limits = c(-50, 50)) +
                        facet_wrap(~ block, ncol = 2)))

# Print plots
walk(.x = data_block$plots, ~ print(.x))
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-1.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-2.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-3.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-4.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-5.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-6.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-7.png" width="768" style="display: block; margin: auto;" />

```r
# Process data (relative stimulus intensity)
data_blockR <- data %>%
  # Rename blocks
  #mutate(block = sprintf('Block: %s (order: %i)', block, block_order)) %>%
  # Nest by PID
  group_by(PID) %>%
  nest() %>%
  # Generate plots
  mutate(plots = map2(.x = data,
                      .y = unique(PID),
                      ~ ggplot(data = .x) +
                        aes(x = intensity_rank,
                            y = rating) +
                        geom_point() +
                        geom_smooth(method = 'loess',
                                    se = FALSE,
                                    colour = '#656565',
                                    size = 0.6) +
                        labs(title = paste(.y, ': Participant-level stimulus-response plots conditioned on experimental block (relative intensity'),
                             subtitle = 'Black circles: individual data points | Grey line: loess curve\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
                             x = 'Relative stimulus intensity',
                             y = 'SPARS rating [-50 to 50]') +
                        scale_y_continuous(limits = c(-50, 50)) +
                        facet_wrap(~ block, ncol = 2)))

# Print plots
walk(.x = data_blockR$plots, ~ print(.x))
```

<img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-8.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-9.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-10.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-11.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-12.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-13.png" width="768" style="display: block; margin: auto;" /><img src="figures/suppl_11_1B-stimulus-response-1/sr_participants2-14.png" width="768" style="display: block; margin: auto;" />

----

# Linear mixed model regression

To allow for a curvilinear relationship between stimulus intensity and rating, we modelled the data using polynomial regression, with 1^st^ (linear), 2^nd^ (quadratic), and 3^rd^ (cubic) order orthogonal polynomials. For each polynomial expression, we modelled the random effects as random intercept only, and as random intercept and slope. 

The random intercept only and random intercept and slope models were compared using the loglikelihood test, and the better model taken forward. Diagnostics were run on the final model only, and we examined level 1 residuals (conditional / fixed effects), and level 2 residuals (random effects) and influence points [^1].

[^1]: Loy A, Hofmann H. HLMdiag: A suite of diagnostics for hierarchical linear models in R. J. Stat. Softw. 2014;56:1–28. [Available](https://www.jstatsoft.org/article/view/v056i05/v56i05.pdf)

### 1st-order (linear) polynomial


```r
# Intercept only
lmm1 <- lmer(tri_mean ~ intensity_rank + (1 | PID),
             data = data_tmR,
             REML = TRUE)

# Intercept and slope
lmm1b <- lmer(tri_mean ~ intensity_rank + (intensity_rank | PID),
              data = data_tmR,
              REML = TRUE)

# Better model?
anova(lmm1, lmm1b)
```

```
## Data: data_tmR
## Models:
## lmm1: tri_mean ~ intensity_rank + (1 | PID)
## lmm1b: tri_mean ~ intensity_rank + (intensity_rank | PID)
##       Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
## lmm1   4 495.89 504.46 -243.94   487.89                            
## lmm1b  6 475.23 488.08 -231.61   463.23 24.66      2  4.418e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Anova of better model
Anova(lmm1b,
      type = 2,
      test.statistic = 'F')
```

```
## Analysis of Deviance Table (Type II Wald F tests with Kenward-Roger df)
## 
## Response: tri_mean
##                     F Df Df.res   Pr(>F)   
## intensity_rank 28.612  1      6 0.001746 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
tab_model(lmm1b,
          auto.label = FALSE,
          dv.labels = "Response", 
          string.pred = "Coefficients",
          pred.labels = c('(Intercept)', 
                          'Intensity'),
          string.stat = 'Estimate',
          string.ci = '95% CI',
          string.p = 'p-value',
          show.icc = FALSE,
          show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">Response</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Coefficients</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">95% CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p-value</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-38.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-56.10&nbsp;&ndash;&nbsp;-21.65</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">6.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">3.81&nbsp;&ndash;&nbsp;8.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">58.33</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">509.90</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>11</sub> <sub>PID.intensity_rank</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">7.86</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&rho;<sub>01</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">-0.96</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">63</td>
</tr>

</table>

### 2nd-order (quadratic) polynomial


```r
# Intercept only
lmm2 <- lmer(tri_mean ~ poly(intensity_rank, 2) + (1 | PID),
             data = data_tmR,
             REML = TRUE)

# Intercept and slope
lmm2b <- lmer(tri_mean ~ poly(intensity_rank, 2) + (intensity_rank | PID),
              data = data_tmR,
              REML = TRUE)

# Better model?
anova(lmm2, lmm2b)
```

```
## Data: data_tmR
## Models:
## lmm2: tri_mean ~ poly(intensity_rank, 2) + (1 | PID)
## lmm2b: tri_mean ~ poly(intensity_rank, 2) + (intensity_rank | PID)
##       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## lmm2   5 495.50 506.22 -242.75   485.50                             
## lmm2b  7 472.71 487.71 -229.35   458.71 26.793      2  1.521e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Anova for better model
Anova(lmm2b,
      type = 2,
      test.statistic = 'F')
```

```
## Analysis of Deviance Table (Type II Wald F tests with Kenward-Roger df)
## 
## Response: tri_mean
##                             F Df Df.res    Pr(>F)    
## poly(intensity_rank, 2) 15.85  2 13.527 0.0002848 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
tab_model(lmm2b,
          auto.label = FALSE,
          dv.labels = "Response", 
          string.pred = "Coefficients",
          pred.labels = c('(Intercept)', 
                          'Intensity (linear)',
                          'Intensity (quadratic)'),
          string.stat = 'Estimate',
          string.ci = '95% CI',
          string.p = 'p-value',
          show.icc = FALSE,
          show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">Response</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Coefficients</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">95% CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p-value</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-8.83</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-16.47&nbsp;&ndash;&nbsp;-1.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.024</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity (linear)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">123.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">78.03&nbsp;&ndash;&nbsp;168.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity (quadratic)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">15.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.42&nbsp;&ndash;&nbsp;30.31</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.031</strong></td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">54.30</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">512.02</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>11</sub> <sub>PID.intensity_rank</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">7.93</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&rho;<sub>01</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">-0.96</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">63</td>
</tr>

</table>

### 3rd-order (cubic) polynomial


```r
# Intercept only
lmm3 <- lmer(tri_mean ~ poly(intensity_rank, 3) + (1 | PID),
             data = data_tmR,
             REML = TRUE)

# Intercept and slope
lmm3b <- lmer(tri_mean ~ poly(intensity_rank, 3) + (intensity_rank | PID),
              data = data_tmR,
              REML = TRUE)

# Better model?
anova(lmm3, lmm3b)
```

```
## Data: data_tmR
## Models:
## lmm3: tri_mean ~ poly(intensity_rank, 3) + (1 | PID)
## lmm3b: tri_mean ~ poly(intensity_rank, 3) + (intensity_rank | PID)
##       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## lmm3   6 497.33 510.19 -242.66   485.33                             
## lmm3b  8 474.37 491.52 -229.19   458.37 26.956      2  1.402e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Anova for better model
Anova(lmm3b,
      type = 2,
      test.statistic = 'F')
```

```
## Analysis of Deviance Table (Type II Wald F tests with Kenward-Roger df)
## 
## Response: tri_mean
##                              F Df Df.res    Pr(>F)    
## poly(intensity_rank, 3) 10.617  3 21.094 0.0001842 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
tab_model(lmm3b,
          auto.label = FALSE,
          dv.labels = "Response", 
          string.pred = "Coefficients",
          pred.labels = c('(Intercept)', 
                          'Intensity (linear)',
                          'Intensity (quadratic)',
                          'Intensity (cubic)'),
          string.stat = 'Estimate',
          string.ci = '95% CI',
          string.p = 'p-value',
          show.icc = FALSE,
          show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">Response</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Coefficients</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">95% CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p-value</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-8.83</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-16.47&nbsp;&ndash;&nbsp;-1.18</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.024</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity (linear)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">123.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">78.03&nbsp;&ndash;&nbsp;168.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity (quadratic)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">15.87</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.32&nbsp;&ndash;&nbsp;30.41</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.033</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Intensity (cubic)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-4.22</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">-18.76&nbsp;&ndash;&nbsp;10.33</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.570</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">55.08</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">511.61</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>11</sub> <sub>PID.intensity_rank</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">7.92</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&rho;<sub>01</sub> <sub>PID</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">-0.96</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">63</td>
</tr>

</table>

### Compare models


```r
knitr::kable(broom::tidy(anova(lmm1b, lmm2b, lmm3b)),
             caption = 'Linear model vs quadratic model and cubic model')
```



Table: Linear model vs quadratic model and cubic model

term     df        AIC        BIC      logLik   deviance   statistic   Chi.Df     p.value
------  ---  ---------  ---------  ----------  ---------  ----------  -------  ----------
lmm1b     6   475.2254   488.0842   -231.6127   463.2254          NA       NA          NA
lmm2b     7   472.7085   487.7104   -229.3542   458.7085   4.5169285        1   0.0335610
lmm3b     8   474.3731   491.5182   -229.1866   458.3731   0.3353413        1   0.5625307

### Plot the model

```r
# Plot quadratic model
predicted <- ggeffect(model = lmm2b,
                      terms = 'intensity_rank',
                      ci.lvl = 0.95) 

ggplot() +
    geom_ribbon(data = predicted,
                aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                fill = '#CCCCCC') + 
    geom_line(data = predicted,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = predicted,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = data_groupR,
               aes(x = intensity_rank,
                   y = median),
               shape = 21,
               stroke = 1,
               size = 5,
               fill = '#FFFFFF') +
  labs(title = 'Quadratic model (95% CI): Predicted values vs stimulus intensity_rank',
       subtitle = 'Black circles/line: predicted values | White circles: group-level median\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
       x = 'Relative stimulus intensity',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 9, by = 1))
```

<img src="figures/suppl_11_1B-stimulus-response-1/lmm_plot-1.png" width="672" style="display: block; margin: auto;" />

```r
# Publication plot
quad <- ggplot(data = predicted) +
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                fill = '#CCCCCC') + 
    geom_segment(x = 0.8, xend = 9, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2) +
    geom_line(aes(x = x,
                  y = predicted),
              size = 0.8) +
    geom_point(aes(x = x,
                  y = predicted),
               size = 1.5) +
    geom_point(data = data_groupR,
               aes(x = intensity_rank,
                   y = median),
               shape = 21,
               size = 5,
               stroke = 1,
               fill = '#FFFFFF') +
    geom_segment(x = 0.8, xend = 0.8, 
                 y = -50.15, yend = 50.15, 
                 size = 1.2) +
    geom_segment(x = 0.995, xend = 9.006, 
                 y = -55, yend = -55, 
                 size = 1.2) +
    annotate(geom = 'text', 
             x = 1, y = 47, 
             label = 'Quadratic model', 
             size = 7, 
             hjust = 0) +
    labs(x = 'Relative stimulus intensity',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25), 
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.8, 9.2), 
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 9, by = 1)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16,
                                   colour = '#000000'),
          axis.title = element_text(size = 16,
                                    colour = '#000000'))

# Plot cubic model
predicted_cubic <- ggeffect(model = lmm3b,
                            terms = 'intensity_rank',
                            ci.lvl = 0.95)

ggplot() +
    geom_ribbon(data = predicted_cubic,
                aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                fill = '#CCCCCC') + 
    geom_line(data = predicted_cubic,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = predicted_cubic,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = data_groupR,
               aes(x = intensity_rank,
                   y = median),
               shape = 21,
               stroke = 1,
               size = 5,
               fill = '#FFFFFF') +
  labs(title = 'Cubic model (95% CI): Predicted values vs stimulus intensity_rank',
       subtitle = 'Black circles/line: predicted values | White circles: group-level median\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
       x = 'Relative stimulus intensity',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 9, by = 1))
```

<img src="figures/suppl_11_1B-stimulus-response-1/lmm_plot-2.png" width="672" style="display: block; margin: auto;" />

```r
# Publication plot
cubic <- ggplot(data = predicted_cubic) +
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                fill = '#CCCCCC') + 
    geom_segment(x = 0.8, xend = 9, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2) +
    geom_line(aes(x = x,
                  y = predicted),
              size = 0.8) +
    geom_point(aes(x = x,
                  y = predicted),
               size = 1.5) +
    geom_point(data = data_groupR,
               aes(x = intensity_rank,
                   y = median),
               shape = 21,
               stroke = 1, 
               size = 5,
               fill = '#FFFFFF') +
    geom_segment(x = 0.8, xend = 0.8, 
                 y = -50.15, yend = 50.15, 
                 size = 1.2) +
    geom_segment(x = 0.995, xend = 9.006, 
                 y = -55, yend = -55, 
                 size = 1.2) +
    annotate(geom = 'text', 
             x = 1, y = 47, 
             label = 'Cubic model', 
             size = 7, 
             hjust = 0) +
    labs(x = 'Relative stimulus intensity',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25), 
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.8, 9.2), 
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 9, by = 1)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 16,
                                     colour = '#000000'),
          axis.title.x = element_text(size = 16,
                                      colour = '#000000'),
          axis.text.y = element_text(size = 16,
                                     colour = '#FFFFFF'),
          axis.title.y = element_text(size = 16,
                                      colour = '#FFFFFF'))

p <- quad + cubic

ggsave(filename = 'figures/figure_9.pdf',
       plot = p,
       width = 12,
       height = 5)
```

The quadratic and cubic models were better fits than the linear model, and did not differ significantly from each other. Therefore we took the simpler of the two models (quadratic) for further inspection, performing diagnostics on the model to confirm that the model was properly specified.

### Diagnostics on the quadratic model

#### Generate residuals 


```r
# Level 1 residuals
## Standardized
lmm_resid1 <- HLMresid(lmm2b,
                       level = 1,
                       type = 'LS',
                       standardize = TRUE)

# Semi-standardized residuals (used for assessing homoscedasticity)
lmm_ssresid1 <- HLMresid(lmm2b,
                         level = 1,
                         type = 'LS',
                         standardize = 'semi')

# Level 2 residuals
## Standardized
lmm_resid2 <- HLMresid(lmm2b,
                       level = 'PID',
                       type = 'EB') 
```

#### Level 1 residuals: linearity

The relationship between predictor(s) and outcome for a linear model should be linear. This relationship can be observed by plotting the level 1 standardized residuals against the predictors. The scatter of residuals should show no pattern, and be centred around 0.


```r
# Standardized residuals vs intensity_rank
ggplot(data = lmm_resid1) +
    aes(x = `poly(intensity_rank, 2)`[, 1],
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm',
                colour = '#000000') +
    geom_hline(yintercept = 0,
               colour = '#656565',
               linetype = 2) +
    geom_hline(yintercept = -2,
               colour = '#656565',
               linetype = 2) +
    geom_hline(yintercept = 2,
               colour = '#656565',
               linetype = 2) +
    labs(title = 'Quadratic model: Level 1 residuals vs intensity_rank',
         subtitle = 'Assess linearity of the intensity_rank term | Black line: linear regression line',
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = 'Stimulus intensity_rank')
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid1_linearity-1.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^2
ggplot(data = lmm_resid1) +
    aes(x = `poly(intensity_rank, 2)`[, 2],
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm',
                colour = '#000000') +
    geom_hline(yintercept = 0,
               colour = '#656565',
               linetype = 2) +
    geom_hline(yintercept = -2,
               colour = '#656565',
               linetype = 2) +
    geom_hline(yintercept = 2,
               colour = '#656565',
               linetype = 2) +
    labs(title = expression(paste('Quadratic model: Level 1 residuals vs ', intensity^2)),
         subtitle = 'Assess linearity of the intensity_rank^2 term | Black line: linear regression line\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = expression(Relative~stimulus~intensity^2)) 
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid1_linearity-2.png" width="672" style="display: block; margin: auto;" />

Based on the plot of the linear and quadratic terms' residuals, we accept that the condition of linearity for the quadratic model.

#### Level 1 residuals: homoscedasticity

The variance of residuals should be constant across the range of the predictor(s). This relationship can be observed by plotting the level 1 semi-standardized residuals against the predictors. Like the assessment of linearity, the residuals should be centred on 0, and show no pattern in the scatter of points.


```r
# Standardized residuals vs intensity_rank
ggplot(data = lmm_ssresid1) +
    aes(x = `poly(intensity_rank, 2)`[ ,1],
        y = semi.std.resid) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = 'Quadratic model: Level 1 residuals vs intensity_rank',
         subtitle = 'Assess homoscedasticity for the intensity_rank term',
         y = 'Semi-standardized residuals',
         x = 'Relative stimulus intensity')
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid1_variance-1.png" width="672" style="display: block; margin: auto;" />

```r
# Standardized residuals vs intensity^2
ggplot(data = lmm_ssresid1) +
    aes(x = `poly(intensity_rank, 2)`[, 2],
        y = semi.std.resid) +
    geom_point() +
    geom_smooth(method = 'lm',
                colour = '#000000') +
    geom_hline(yintercept = 0,
               colour = '#656565',
               linetype = 2) +
    labs(title = expression(paste('Quadratic model: Level 1 residuals vs ', intensity^2)),
         subtitle = 'Assess homoscedasticity for the intensity_rank^2 term | Black line: linear regression line\nRelative intensity was calculated using the rank of the ordered (ascending) stimulus intensities each participant was exposed to.',
         y = 'Semi-standardized residuals',
         x = expression(Relative~stimulus~intensity^2)) 
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid1_variance-2.png" width="672" style="display: block; margin: auto;" />

There is no obvious pattern to the scatter of residuals across any of the fixed effect terms. So we accept that the residuals are homoscedastic in the quadratic model.

### Level 1 residuals: residual distribution

Residuals should be normally distributed. There are various methods of examining the distribution, and we have chosen the QQ-plot method, which plots the quantiles of the standardized residuals against a theoretical (Gaussian) quantile distribution. Points should line on the line of identity of the two sets of quantiles follow the same distribution. 


```r
# Standardized residuals vs intensity_rank
ggplot_qqnorm(x = lmm_resid1$std.resid, 
              line = "rlm") +
    labs(title = 'Quadratic model: QQ-plot of level 1 residuals',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Standardized residuals')
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid1_ditribution-1.png" width="672" style="display: block; margin: auto;" />

There is minor deviation at the extremes (possibly a thin left tail), but on the whole, we are satisfied that the quadratic model fits the assumption of normally distributed residuals. 

#### Level 2 residuals: residual distribution

Level 2 residuals can be used to identify predictors that should be included in the model, but since we are only assessing the effect of stimulus strength on SPARS rating, we have only assessed whether the level 2 residuals (intercept and slope) meet the assumption of being normally distributed (assessed using QQ-plots).


```r
# Generate QQplots 
qq1 <- ggplot_qqnorm(x = lmm_resid2$`(Intercept)`, 
              line = "rlm") +
    labs(title = 'Quadratic model: QQ-plot of level 2 residuals (Intercept)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

qq2 <- ggplot_qqnorm(x = lmm_resid2$intensity_rank, 
              line = "rlm") +
    labs(title = 'Quadratic model: QQ-plot of level 2 residuals (slope: intensity_rank)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

# Plot
qq1 + qq2
```

<img src="figures/suppl_11_1B-stimulus-response-1/resid2_linearity-1.png" width="672" style="display: block; margin: auto;" />

Although the data are sparse, we are satisfied that the level 2 residuals for the intercept and the slope of the quadratic model fit the assumption of being normally distributed.

### influence points

We assessed three aspects of influence (data that significantly model coefficients):

- The variance component (random effects) was assessed using the relative variance change metric, which calculates the impact of deleting observational units of the variance of the residuals, random intercept, random slope, and covariance of the random slope and random intercept.

- Leverage was used to assess fitted values. The assessment involves assessing the rate of change in the predicted response with respect to the observed response.

- Cook's Distance was used to assess the influence of fixed effects. The metric measures the distance between the fixed effects estimates obtained from the full model to that obtained from the reduced data (observations removed). 

In all cases, we treated the individual (indicated using PID) as the unit of observation, and we used internal scaling to set the diagnostic cut-offs for each metric. The cut-offs were determined as: $3^{rd}~Quartile + (3 \cdot IQR)$.


```r
# Prepare relative variance change (RCV)
influence_rvc <- rvc(lmm1b, 
                     group = 'PID')

# Prepare Cook's distance
influence_cooks <- cooks.distance(lmm1b, 
                                  group = 'PID')

# Prepare leverage 
## (Assessed at the level of PID, and not the individual observation)
influence_leverage <- leverage(lmm1b,
                               level = 'PID')
```

#### Random effects

Estimation of the variance component was undertaken by calculating relative variance change (RCV). RVC is close to zero when deletion of observational units from the model does not have a large influence on the variance component.
 

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

<img src="figures/suppl_11_1B-stimulus-response-1/influence_random-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 2], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random intercept variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/suppl_11_1B-stimulus-response-1/influence_random-2.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 3], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/suppl_11_1B-stimulus-response-1/influence_random-3.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 4], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope and intercept covariance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/suppl_11_1B-stimulus-response-1/influence_random-4.png" width="672" style="display: block; margin: auto;" />

One value (PID11) is below the cut-off for the relative variance change for random slope and intercept covariance. The extent of the deviation is minor, and was ignored.

#### Fitted values

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

<img src="figures/suppl_11_1B-stimulus-response-1/influence_leverage-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_leverage[, 4], 
             cutoff = "internal",
             name = "leverage") + 
    labs(title = 'Leverage: unconfounded random effects',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Leverage',
         x = 'Participant ID')
```

<img src="figures/suppl_11_1B-stimulus-response-1/influence_leverage-2.png" width="672" style="display: block; margin: auto;" />

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

<img src="figures/suppl_11_1B-stimulus-response-1/influence_fixed-1.png" width="672" style="display: block; margin: auto;" />

There are no influential fixed effects.

### Summary

The linear is well-specified.

----

# Quantile mixed model regression


```r
# Quantile model with 2.5, 25, 50, 75, and 97.5% quantiles
qmm <- lqmm(fixed = tri_mean ~ poly(intensity_rank, 2),
            random = ~ intensity_rank,
            group = PID,
            data = data_tmR,
            tau = c(0.025, 0.25, 0.5, 0.75, 0.975))

# Summary 
summary(qmm)
```

```
## Call: lqmm(fixed = tri_mean ~ poly(intensity_rank, 2), random = ~intensity_rank, 
##     group = PID, tau = c(0.025, 0.25, 0.5, 0.75, 0.975), data = data_tmR)
## 
## tau = 0.025
## 
## Fixed effects:
##                             Value Std. Error lower bound upper bound
## (Intercept)               -69.078     25.256    -119.832     -18.325
## poly(intensity_rank, 2)1  128.368     21.709      84.743     171.993
## poly(intensity_rank, 2)2   11.399     12.710     -14.143      36.940
##                           Pr(>|t|)    
## (Intercept)               0.008657 ** 
## poly(intensity_rank, 2)1 3.183e-07 ***
## poly(intensity_rank, 2)2  0.374194    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.25
## 
## Fixed effects:
##                             Value Std. Error lower bound upper bound
## (Intercept)              -17.4995    10.2057    -38.0086      3.0096
## poly(intensity_rank, 2)1 121.5351    21.3951     78.5401    164.5302
## poly(intensity_rank, 2)2   5.7801    13.1036    -20.5527     32.1128
##                           Pr(>|t|)    
## (Intercept)                0.09272 .  
## poly(intensity_rank, 2)1 7.242e-07 ***
## poly(intensity_rank, 2)2   0.66108    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.5
## 
## Fixed effects:
##                             Value Std. Error lower bound upper bound
## (Intercept)               -4.4901     7.3449    -19.2502      10.270
## poly(intensity_rank, 2)1 121.6123    22.5542     76.2880     166.937
## poly(intensity_rank, 2)2  13.7000    13.1490    -12.7239      40.124
##                           Pr(>|t|)    
## (Intercept)                 0.5438    
## poly(intensity_rank, 2)1 1.992e-06 ***
## poly(intensity_rank, 2)2    0.3026    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.75
## 
## Fixed effects:
##                             Value Std. Error lower bound upper bound
## (Intercept)                2.4089    11.1206    -19.9389      24.757
## poly(intensity_rank, 2)1 118.7761    25.0152     68.5061     169.046
## poly(intensity_rank, 2)2  17.9548    13.6470     -9.4699      45.380
##                           Pr(>|t|)    
## (Intercept)                 0.8294    
## poly(intensity_rank, 2)1 1.824e-05 ***
## poly(intensity_rank, 2)2    0.1944    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.975
## 
## Fixed effects:
##                             Value Std. Error lower bound upper bound
## (Intercept)               60.2116    23.9208     12.1409      108.28
## poly(intensity_rank, 2)1 117.4004    22.2664     72.6545      162.15
## poly(intensity_rank, 2)2  17.2735    11.3490     -5.5331       40.08
##                           Pr(>|t|)    
## (Intercept)                0.01515 *  
## poly(intensity_rank, 2)1 3.019e-06 ***
## poly(intensity_rank, 2)2   0.13443    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:
## [1] 585.5 (df = 6) 509.7 (df = 6) 522.3 (df = 6) 524.7 (df = 6)
## [5] 562.1 (df = 6)
```

```r
# Get predicted values
## Level 0 (conditional, note difference to the lmer diagnostics)
quant_predict <- as.data.frame(predict(qmm, level = 0))
names(quant_predict) <- paste0('Q', c(2.5, 25, 50, 75, 97.5))

# Join with 'central_lmm'
data_lqmm <- data_tmR %>%
  bind_cols(quant_predict)

# Trim prediction to upper and lower limits of the scale
data_lqmm %<>%
  mutate_if(is.numeric,
            funs(ifelse(. > 50,
                        yes = 50,
                        no = ifelse(. < -50,
                                    yes = -50,
                                    no = .))))

# Plot
ggplot(data = data_lqmm) +
  aes(x = intensity_rank,
      y = Q50) +
  geom_ribbon(aes(ymin = `Q2.5`,
                  ymax = `Q97.5`),
              fill = '#CCCCCC') +
  geom_ribbon(aes(ymin = `Q25`,
                  ymax = `Q75`),
              fill = '#656565') +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point(size = 5,
             shape = 21,
             stroke = 1,
             fill = '#FFFFFF',
             colour = '#000000') +
  labs(title = paste('Quantile regression'),
       subtitle = 'Open circles: 50th percentile (median) | Dark grey band: interquartile range | Light grey band: 95% prediction interval',
       x = 'Realtive stimulus intensity',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity_rank)) 
```

<img src="figures/suppl_11_1B-stimulus-response-1/quantile-1.png" width="672" style="display: block; margin: auto;" />

```r
## With original data
ggplot(data = data_lqmm) +
  aes(x = intensity_rank,
      y = Q50) +
  geom_ribbon(aes(ymin = `Q2.5`,
                  ymax = `Q97.5`),
              fill = '#CCCCCC') +
  geom_ribbon(aes(ymin = `Q25`,
                  ymax = `Q75`),
              fill = '#656565') +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point(data = data_tmR,
             aes(y = tri_mean),
             position = position_jitter(width = 0.03)) +
  geom_point(size = 5,
             stroke = 1,
             shape = 21,
             fill = '#FFFFFF',
             colour = '#000000') +
  labs(title = paste('Quantile regression (with original Tukey trimean data)'),
       subtitle = 'Open circles: 50th percentile (median) | Dark grey band: interquartile range | Light grey band: 95% prediction interval',
       x = 'Stimulus intensity_rank (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity_rank)) 
```

<img src="figures/suppl_11_1B-stimulus-response-1/quantile-2.png" width="672" style="display: block; margin: auto;" />

```r
# Publication plot
p <- ggplot(data = data_lqmm) +
    aes(x = intensity_rank,
        y = Q50) +
    geom_ribbon(aes(ymin = `Q2.5`,
                    ymax = `Q97.5`),
                fill = '#CCCCCC') +
    geom_ribbon(aes(ymin = `Q25`,
                    ymax = `Q75`),
                fill = '#656565') +
    geom_segment(x = 0.8, xend = 9, 
                 y = 0, yend = 0, 
                 size = 0.6,
                 linetype = 2) +
    geom_point(data = data_tmR,
               aes(y = tri_mean),
               position = position_jitter(width = 0.03)) +
    geom_point(size = 5,
               shape = 21,
               stroke = 1,
               fill = '#FFFFFF',
               colour = '#000000') +
    geom_segment(x = 0.8, xend = 0.8, 
                 y = -50.15, yend = 50.15, 
                 size = 1.2) +
    geom_segment(x = 0.995, xend = 9.006, 
                 y = -55, yend = -55, 
                 size = 1.2) +
    labs(x = 'Relative stimulus intensity',
         y = 'SPARS rating (-50 to 50)') +
    scale_y_continuous(limits = c(-55, 50.25), 
                       expand = c(0, 0),
                       breaks = c(-50, -25, 0, 25, 50)) +
    scale_x_continuous(limits = c(0.8, 9.2), 
                       expand = c(0, 0),
                       breaks = seq(from = 1, to = 9, by = 1)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16,
                                   colour = '#000000'),
          axis.title = element_text(size = 16,
                                    colour = '#000000'))

ggsave(filename = 'figures/figure_10.pdf',
       plot = p,
       width = 6,
       height = 5)
```

The response is consistent across the range of stimulus intensities, but the prediction interval is extremely broad. 

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Debian GNU/Linux 9 (stretch)
## 
## Matrix products: default
## BLAS: /usr/lib/openblas-base/libblas.so.3
## LAPACK: /usr/lib/libopenblasp-r0.2.19.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C             
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2  ggeffects_0.5.0 car_3.0-2       carData_3.0-1  
##  [5] sjPlot_2.6.0    HLMdiag_0.3.1   lqmm_1.5.4      lme4_1.1-18-1  
##  [9] Matrix_1.2-14   patchwork_0.0.1 forcats_0.3.0   stringr_1.3.1  
## [13] dplyr_0.7.6     purrr_0.2.5     readr_1.1.1     tidyr_0.8.1    
## [17] tibble_1.4.2    ggplot2_3.0.0   tidyverse_1.2.1 magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] TH.data_1.0-9      minqa_1.2.4        colorspace_1.3-2  
##  [4] modeltools_0.2-22  rio_0.5.10         ggridges_0.5.0    
##  [7] sjlabelled_1.0.13  rprojroot_1.3-2    estimability_1.3  
## [10] snakecase_0.9.2    rstudioapi_0.7     glmmTMB_0.2.2.0   
## [13] mvtnorm_1.0-8      lubridate_1.7.4    coin_1.2-2        
## [16] xml2_1.2.0         codetools_0.2-15   splines_3.5.1     
## [19] mnormt_1.5-5       knitr_1.20         sjmisc_2.7.4      
## [22] effects_4.0-3      bayesplot_1.6.0    jsonlite_1.5      
## [25] nloptr_1.0.4       pbkrtest_0.4-7     broom_0.5.0       
## [28] compiler_3.5.1     httr_1.3.1         sjstats_0.17.0    
## [31] emmeans_1.2.3      backports_1.1.2    assertthat_0.2.0  
## [34] lazyeval_0.2.1     survey_3.33-2      cli_1.0.0         
## [37] htmltools_0.3.6    tools_3.5.1        SparseGrid_0.8.2  
## [40] gtable_0.2.0       glue_1.3.0         reshape2_1.4.3    
## [43] Rcpp_0.12.18       cellranger_1.1.0   nlme_3.1-137      
## [46] psych_1.8.4        openxlsx_4.1.0     rvest_0.3.2       
## [49] stringdist_0.9.5.1 MASS_7.3-50        zoo_1.8-3         
## [52] scales_1.0.0       hms_0.4.2          parallel_3.5.1    
## [55] sandwich_2.5-0     pwr_1.2-2          TMB_1.7.14        
## [58] yaml_2.2.0         curl_3.2           stringi_1.2.4     
## [61] highr_0.7          zip_1.0.0          rlang_0.2.2       
## [64] pkgconfig_2.0.2    evaluate_0.11      lattice_0.20-35   
## [67] prediction_0.3.6   bindr_0.1.1        labeling_0.3      
## [70] tidyselect_0.2.4   plyr_1.8.4         R6_2.2.2          
## [73] multcomp_1.4-8     RLRsim_3.1-3       pillar_1.3.0      
## [76] haven_1.1.2        foreign_0.8-70     withr_2.1.2       
## [79] mgcv_1.8-24        abind_1.4-5        survival_2.42-3   
## [82] nnet_7.3-12        modelr_0.1.2       crayon_1.3.4      
## [85] rmarkdown_1.10     grid_3.5.1         readxl_1.1.0      
## [88] data.table_1.11.4  digest_0.6.16      xtable_1.8-3      
## [91] stats4_3.5.1       munsell_0.5.0
```
