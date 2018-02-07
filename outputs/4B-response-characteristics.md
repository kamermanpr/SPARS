---
title: "SPARS trial B"
subtitle: "Stimulus-response characteristics of the SPARS"
author: "Peter Kamerman"
date: "07 Feb 2018"
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

This analysis examines the stimulus-response characateristics of the SPARS.

----

# Import and inspect data

```r
# Import
data <- read_rds('./data-cleaned/SPARS_B.rds')

# Inspect
glimpse(data)
```

```
## Observations: 2,268
## Variables: 8
## $ PID             <chr> "ID01", "ID01", "ID01", "ID01", "ID01", "ID01"...
## $ scale           <chr> "SPARS", "SPARS", "SPARS", "SPARS", "SPARS", "...
## $ block_number    <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2...
## $ trial_number    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,...
## $ intensity       <dbl> 3.75, 4.00, 3.00, 2.50, 2.50, 3.25, 4.25, 3.00...
## $ intensity_char  <chr> "3.75", "4.00", "3.00", "2.50", "2.50", "3.25"...
## $ rating          <int> 3, 4, -16, -16, -2, 0, 15, -1, -31, 5, 8, 2, 1...
## $ rating_positive <dbl> 53, 54, 34, 34, 48, 50, 65, 49, 19, 55, 58, 52...
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
    select(PID, scale, block_number, trial_number, 
           intensity, intensity_char, rating) %>% 
    # Rename block_number
    rename(block = block_number) %>% 
    # Select SPARS scale
    filter(scale == 'SPARS') %>%
    # Remove incomplete cases
    filter(complete.cases(.)) 

# Xtabulate readings per individuals per stimulus intensity
xtabs(~ PID + intensity, 
      data = data)
```

```
##       intensity
## PID    1.75  2 2.25 2.5 2.75  3 3.25 3.5 3.75  4 4.25 4.5
##   ID01    0  0   12  12   12 12   12  12   12 12   12   0
##   ID02    0  0   12  12   12 12   12  11   12 12   12   0
##   ID03    0  0    0  12   12 12   12  12   12 12   12  11
##   ID04    0  0    0  11   12 12   12  12   12 12   12  12
##   ID05    0  0    0  12   12 12   12  12   12 12   12  12
##   ID06   12 12   12  12   12 12   12  12   12  0    0   0
##   ID07    0  0   12  12   12 12   12  11   12 12   12   0
```

```r
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

<img src="figures/4B-response-characteristics/sr_group-1.png" width="672" style="display: block; margin: auto;" />

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

<img src="figures/4B-response-characteristics/sr_participants-1.png" width="864" style="display: block; margin: auto;" />

#### Trials by experimental block


```r
# Process data
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

<img src="figures/4B-response-characteristics/sr_participants2-1.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-2.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-3.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-4.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-5.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-6.png" width="768" style="display: block; margin: auto;" /><img src="figures/4B-response-characteristics/sr_participants2-7.png" width="768" style="display: block; margin: auto;" />

----

# Linear mixed model regression

To allow for a curvilinear relationship between stimulus intensity and rating, we modelled the data using polynomial regression, with 1^st^ (linear), 2^nd^ (quadratic), and 3^rd^ (cubic) order orthogonal polynomials. For each polynomial expression, we modelled the random effects as random intercept only, and as random intercept and slope. 

The random intercept only and random intercept and slope models were compared using the logliklihood test, and the better model taken foward. Diagnostics were run on the final model only, and we examined level 1 residuals (conditional / fixed effects), and level 2 residuals (random effects) and influence points [^1].

[^1]: Loy A, Hofmann H. HLMdiag: A suite of diagnostics for hierarchical linear models in R. J. Stat. Softw. 2014;56:1–28. [Available](https://www.jstatsoft.org/article/view/v056i05/v56i05.pdf)

### 1st-order (linear) polynomial


```r
# Intercept only
lmm1 <- lmer(tri_mean ~ intensity + (1 | PID),
             data = data_tm,
             REML = TRUE)

# Intercept and slope
lmm1b <- lmer(tri_mean ~ intensity + (intensity | PID),
              data = data_tm,
              REML = TRUE)

# Better model?
anova(lmm1, lmm1b)
```

```
## Data: data_tm
## Models:
## lmm1: tri_mean ~ intensity + (1 | PID)
## lmm1b: tri_mean ~ intensity + (intensity | PID)
##       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## lmm1   4 500.92 509.49 -246.46   492.92                             
## lmm1b  6 478.61 491.47 -233.31   466.61 26.309      2  1.936e-06 ***
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
##                F Df Df.res   Pr(>F)   
## intensity 27.193  1 5.9981 0.001988 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
sjt.lmer(lmm1b,
         show.header = TRUE,
         string.dv = "Response", 
         string.pred = "Coefficients",
         depvar.labels = '',
         pred.labels = 'intensity',
         string.est = 'Estimate',
         string.ci = '95% CI',
         string.p = 'p-value',
         show.icc = FALSE,
         show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;border-bottom:double;">
<tr>
<td style="padding:0.2cm; border-top:double;" rowspan="2"><em>Coefficients</em></td>
<td colspan="4" style="padding:0.2cm; border-top:double; text-align:center; border-bottom:1px solid;"><em>Response</em></td>
</tr>

<td style=" padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; " colspan="3"></td>
</tr>
<tr>
<td style="padding:0.2cm; font-style:italic;">&nbsp;</td>
<td style="padding-left:0.5em; padding-right:0.5em; font-style:italic;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">Estimate</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">95% CI</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">p&#45;value</td> 
</tr>
<tr>
<td colspan="5" style="padding:0.2cm; text-align:left; border-top:1px solid; font-weight:bold; text-align:left;">Fixed Parts</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">(Intercept)</td>
<td style="padding-left:0.5em; padding-right:0.5em; ">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">&#45;88.37</td>
<td style="padding:0.2cm; text-align:center; ">&#45;126.22&nbsp;&ndash;&nbsp;&#45;50.52</td>
<td style="padding:0.2cm; text-align:center; ">.004</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">intensity</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">23.82</td>
<td style="padding:0.2cm; text-align:center; ">14.87&nbsp;&ndash;&nbsp;32.77</td>
<td style="padding:0.2cm; text-align:center; ">.002</td>
</tr><tr>
<td colspan="5" style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; font-weight:bold; text-align:left; padding-top:0.5em;">Random Parts</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&sigma;<sup>2</sup></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">57.994</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&tau;<sub>00, PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">2436.976</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&rho;<sub>01</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">&#45;0.986</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">N<sub>PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">7</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;">Observations</td>
<td style="padding-left:0.5em; padding-right:0.5em; border-top:1px solid;">&nbsp;</td><td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:center; border-top:1px solid;" colspan="3">63</td>
</tr>
</table>

### 2nd-order (quadratic) polynomial


```r
# Intercept only
lmm2 <- lmer(tri_mean ~ poly(intensity, 2) + (1 | PID),
             data = data_tm,
             REML = TRUE)

# Intercept and slope
lmm2b <- lmer(tri_mean ~ poly(intensity, 2) + (intensity | PID),
              data = data_tm,
              REML = TRUE)

# Better model?
anova(lmm2, lmm2b)
```

```
## Data: data_tm
## Models:
## lmm2: tri_mean ~ poly(intensity, 2) + (1 | PID)
## lmm2b: tri_mean ~ poly(intensity, 2) + (intensity | PID)
##       Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
## lmm2   5 494.68 505.40 -242.34   484.68                            
## lmm2b  7 477.28 492.28 -231.64   463.28  21.4      2  2.254e-05 ***
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
##                         F Df Df.res   Pr(>F)    
## poly(intensity, 2) 16.729  2 12.929 0.000259 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
sjt.lmer(lmm2b,
         show.header = TRUE,
         string.dv = "Response", 
         string.pred = "Coefficients",
         depvar.labels = '',
         pred.labels = 'intensity',
         string.est = 'Estimate',
         string.ci = '95% CI',
         string.p = 'p-value',
         show.icc = FALSE,
         show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;border-bottom:double;">
<tr>
<td style="padding:0.2cm; border-top:double;" rowspan="2"><em>Coefficients</em></td>
<td colspan="4" style="padding:0.2cm; border-top:double; text-align:center; border-bottom:1px solid;"><em>Response</em></td>
</tr>

<td style=" padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; " colspan="3"></td>
</tr>
<tr>
<td style="padding:0.2cm; font-style:italic;">&nbsp;</td>
<td style="padding-left:0.5em; padding-right:0.5em; font-style:italic;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">Estimate</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">95% CI</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">p&#45;value</td> 
</tr>
<tr>
<td colspan="5" style="padding:0.2cm; text-align:left; border-top:1px solid; font-weight:bold; text-align:left;">Fixed Parts</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">(Intercept)</td>
<td style="padding-left:0.5em; padding-right:0.5em; ">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">&#45;9.60</td>
<td style="padding:0.2cm; text-align:center; ">&#45;20.20&nbsp;&ndash;&nbsp;1.01</td>
<td style="padding:0.2cm; text-align:center; ">.128</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">poly(intensity, 2)1</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">129.72</td>
<td style="padding:0.2cm; text-align:center; ">84.01&nbsp;&ndash;&nbsp;175.44</td>
<td style="padding:0.2cm; text-align:center; ">.002</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">poly(intensity, 2)2</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">18.19</td>
<td style="padding:0.2cm; text-align:center; ">0.10&nbsp;&ndash;&nbsp;36.28</td>
<td style="padding:0.2cm; text-align:center; ">.097</td>
</tr><tr>
<td colspan="5" style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; font-weight:bold; text-align:left; padding-top:0.5em;">Random Parts</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&sigma;<sup>2</sup></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">54.199</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&tau;<sub>00, PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">2086.049</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&rho;<sub>01</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">&#45;0.975</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">N<sub>PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">7</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;">Observations</td>
<td style="padding-left:0.5em; padding-right:0.5em; border-top:1px solid;">&nbsp;</td><td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:center; border-top:1px solid;" colspan="3">63</td>
</tr>
</table>

### 3rd-order (cubic) polynomial


```r
# Intercept only
lmm3 <- lmer(tri_mean ~ poly(intensity, 3) + (1 | PID),
             data = data_tm,
             REML = TRUE)

# Intercept and slope
lmm3b <- lmer(tri_mean ~ poly(intensity, 3) + (intensity | PID),
              data = data_tm,
              REML = TRUE)

# Better model?
anova(lmm3, lmm3b)
```

```
## Data: data_tm
## Models:
## lmm3: tri_mean ~ poly(intensity, 3) + (1 | PID)
## lmm3b: tri_mean ~ poly(intensity, 3) + (intensity | PID)
##       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## lmm3   6 496.65 509.51 -242.33   484.65                             
## lmm3b  8 478.99 496.13 -231.49   462.99 21.668      2  1.971e-05 ***
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
##                         F Df Df.res   Pr(>F)    
## poly(intensity, 3) 10.923  3 20.145 0.000179 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print better model
sjt.lmer(lmm3b,
         show.header = TRUE,
         string.dv = "Response", 
         string.pred = "Coefficients",
         depvar.labels = '',
         pred.labels = 'intensity',
         string.est = 'Estimate',
         string.ci = '95% CI',
         string.p = 'p-value',
         show.icc = FALSE,
         show.r2 = FALSE)
```

<table style="border-collapse:collapse; border:none;border-bottom:double;">
<tr>
<td style="padding:0.2cm; border-top:double;" rowspan="2"><em>Coefficients</em></td>
<td colspan="4" style="padding:0.2cm; border-top:double; text-align:center; border-bottom:1px solid;"><em>Response</em></td>
</tr>

<td style=" padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; " colspan="3"></td>
</tr>
<tr>
<td style="padding:0.2cm; font-style:italic;">&nbsp;</td>
<td style="padding-left:0.5em; padding-right:0.5em; font-style:italic;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">Estimate</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">95% CI</td>
<td style="padding:0.2cm; text-align:center; font-style:italic; ">p&#45;value</td> 
</tr>
<tr>
<td colspan="5" style="padding:0.2cm; text-align:left; border-top:1px solid; font-weight:bold; text-align:left;">Fixed Parts</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">(Intercept)</td>
<td style="padding-left:0.5em; padding-right:0.5em; ">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">&#45;9.66</td>
<td style="padding:0.2cm; text-align:center; ">&#45;20.31&nbsp;&ndash;&nbsp;0.99</td>
<td style="padding:0.2cm; text-align:center; ">.127</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">poly(intensity, 3)1</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">130.27</td>
<td style="padding:0.2cm; text-align:center; ">83.93&nbsp;&ndash;&nbsp;176.61</td>
<td style="padding:0.2cm; text-align:center; ">.002</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">poly(intensity, 3)2</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">17.09</td>
<td style="padding:0.2cm; text-align:center; ">&#45;1.44&nbsp;&ndash;&nbsp;35.63</td>
<td style="padding:0.2cm; text-align:center; ">.122</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left;">poly(intensity, 3)3</td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td>
<td style="padding:0.2cm; text-align:center; ">4.52</td>
<td style="padding:0.2cm; text-align:center; ">&#45;11.82&nbsp;&ndash;&nbsp;20.86</td>
<td style="padding:0.2cm; text-align:center; ">.608</td>
</tr><tr>
<td colspan="5" style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; font-weight:bold; text-align:left; padding-top:0.5em;">Random Parts</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&sigma;<sup>2</sup></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">54.908</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&tau;<sub>00, PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">2141.360</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">&rho;<sub>01</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">&#45;0.976</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;">N<sub>PID</sub></td>
<td style="padding-left:0.5em; padding-right:0.5em;">&nbsp;</td><td style="padding:0.2cm; text-align:center; padding-top:0.1cm; padding-bottom:0.1cm;" colspan="3">7</td>
</tr>

<tr>
<td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;">Observations</td>
<td style="padding-left:0.5em; padding-right:0.5em; border-top:1px solid;">&nbsp;</td><td style="padding:0.2cm; padding-top:0.1cm; padding-bottom:0.1cm; text-align:center; border-top:1px solid;" colspan="3">63</td>
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
lmm1b     6   478.6094   491.4682   -233.3047   466.6094          NA       NA          NA
lmm2b     7   477.2819   492.2839   -231.6410   463.2819   3.3274885        1   0.0681308
lmm3b     8   478.9857   496.1308   -231.4929   462.9857   0.2961873        1   0.5862826

### PLot the model

```r
predicted <- ggeffects::ggpredict(model = lmm1b,
                                  terms = 'intensity',
                                  ci.lvl = 0.95) 
ggplot() +
    geom_ribbon(data = predicted,
                aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                fill = '#cccccc') + 
    geom_line(data = predicted,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = predicted,
              aes(x = x,
                  y = predicted)) +
    geom_point(data = data_group,
               aes(x = intensity,
                   y = median),
               shape = 21,
               size = 4,
               fill = '#D55E00') +
  labs(title = 'Cubic model (95% CI): Predicted values vs stimulus intensity',
       subtitle = 'Black circles/line: predicted values | Orange circles: group-level median',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5))
```

<img src="figures/4B-response-characteristics/lmm_plot-1.png" width="672" style="display: block; margin: auto;" />

The cubic model has the best fit. The resulting curvilinear response function is _steepest_ at the extremes and  _flattens out_ in the mid-ranges of stumulus intensity. We performed diagnostics on this model to confirm that the model was properly specified.

### Diagnostics on the cubic model

#### Generate residuals 


```r
# Level 1 residuals
## Standardized
lmm_resid1 <- HLMresid(lmm1b,
                       level = 1,
                       type = 'LS',
                       standardize = TRUE)

# Semi-standardized residuals (used for assessing homoscedasticity)
lmm_ssresid1 <- HLMresid(lmm1b,
                         level = 1,
                         type = 'LS',
                         standardize = 'semi')

# Level 2 residuals
## Standardized
lmm_resid2 <- HLMresid(lmm1b,
                       level = 'PID',
                       type = 'EB') 
```

#### Level 1 residuals: linearity

The relationship between predictor(s) and outcome for a linear model should be linear. This relationship can be observed by plotting the level 1 standardized residuals against the predictors. The scatter of residuals should show no pattern, and be centered around 0.


```r
# Standardized residuals vs intensity
ggplot(data = lmm_resid1) +
    aes(x = intensity,
        y = std.resid) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -2,
               linetype = 2) +
    geom_hline(yintercept = 2,
               linetype = 2) +
    labs(title = 'Linear model: Level 1 residuals vs intensity',
         subtitle = 'Assess linearity of the intensity term | Blue line: linear regression line',
         caption = 'The regression line should be centered on 0\n~95% of points should be betwen -2 and +2',
         y = 'Standardized residuals',
         x = 'Stimulus intensity')
```

<img src="figures/4B-response-characteristics/resid1_linearity-1.png" width="672" style="display: block; margin: auto;" />

The regression curve for the quadratic term shows some signs of deviating from slope = 0, but otherwise the model specification (in terms of linearity) looks okay. Based on the overall pictire, we accept that the condition of linearity for the cubic model.

#### Level 1 residuals: homoscedasticity

The variance of residuals should be constant across the range of the predictor(s). This relationship can be observed by plotting the level 1 semi-standardized residuals against the predictors. Like the assessment of linearity, the residuals should be centered on 0, and show no pattern in the scatter of points.


```r
# Standardized residuals vs intensity
ggplot(data = lmm_ssresid1) +
    aes(x = intensity,
        y = semi.std.resid) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = 'Linear model: Level 1 residuals vs intensity',
         subtitle = 'Assess homoscedasticity for the intensity term',
         y = 'Semi-standardized residuals',
         x = 'Stimulus intensity')
```

<img src="figures/4B-response-characteristics/resid1_variance-1.png" width="672" style="display: block; margin: auto;" />

There is no obvious pattern to the scatter of residuals across any of the fixed effect terms. So we accept that the residuals are homoscedastic in the cubic model.

### Level 1 residuals: residual distribution

Residuals should be normally distributed. There are various methods of examining the distribution, and we have chosen the QQ-plot method, which plots the quantiles of the standardized residuals against a theoretical (Gaussian) quantile distribution. Points should line on the line of identity of the two sets of quantiles follow the same distribution. 


```r
# Standardized residuals vs intensity
ggplot_qqnorm(x = lmm_resid1$std.resid, 
              line = "rlm") +
    labs(title = 'Linear model: QQ-plot of level 1 residuals',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Standardized residuals')
```

<img src="figures/4B-response-characteristics/resid1_ditribution-1.png" width="672" style="display: block; margin: auto;" />

There is minor deviation at the extremes, but on the whole, we are satisfied that the cubic model fits the assumption of normally sdistributed residuals. 

#### Level 2 residuals: residual distribution

Level 2 residuals can be used to identify predictors that should be included in the model, but since we are only assessing the effect of stimulus strength on SPARS rating, we have only assessed whether the level 2 residuals (intercept and slope) meet the assumption of being normally distributed (assessed using QQ-plots).


```r
# Generate QQplots 
qq1 <- ggplot_qqnorm(x = lmm_resid2$`(Intercept)`, 
              line = "rlm") +
    labs(title = 'Linear model: QQ-plot of level 2 residuals (Intercept)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

qq2 <- ggplot_qqnorm(x = lmm_resid2$intensity, 
              line = "rlm") +
    labs(title = 'Linear model: QQ-plot of level 2 residuals (slope: intensity)',
         subtitle = 'Assessing whether residuals follow a normal distribution',
         x = 'Theoretical quantiles',
         y = 'Residuals') 

# Plot
qq1 + qq2
```

<img src="figures/4B-response-characteristics/resid2_linearity-1.png" width="672" style="display: block; margin: auto;" />

Although the data are sparse, we are satisfied that the level 2 residuals for the intercept and the slope of the cubic model fit the assumption of being normally sdistributed.

### influence points

We assessed three aspects of influence (data that significantly model coefficients):

- The variance component (random effects) was assesed using the relative variance change metric, which calculates the impact of deleting observational units of the variance of the residuals, random intercept, random slope, and covariance of the random slope and random intercept.

- Leverage was used to assess fitted values. The assessment involves assessing the rate of change in the predicted response with respect to the observed response.

- Cook's Distance was used to assess the influence of fixed effects. The metric measures the distance between the fixed effects estimates obtained from the full model to that obtained from the reduced data (observations removed). 

In all cases, we treated the individual (indicated using PID) as the unit of observation, and we used internal scaling to set the diagnostic cutoffs for each metric. The cutoffs were determined as: $3^{rd}~Quartile + (3 \cdot IQR)$.


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

<img src="figures/4B-response-characteristics/influence_random-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 2], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random intercept variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4B-response-characteristics/influence_random-2.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 3], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope variance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4B-response-characteristics/influence_random-3.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_rvc[ , 4], 
             cutoff = 'internal',
             name = 'rvc') + 
    labs(title = 'Relative variance change for the random slope and intercept covariance',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Relative variance change',
         x = 'Participant ID')
```

<img src="figures/4B-response-characteristics/influence_random-4.png" width="672" style="display: block; margin: auto;" />

One value (PID11) is below the cutoff for the relative variance change for random slope and intercept covariance. The extent of the deviation is minor, and was ignored.

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

<img src="figures/4B-response-characteristics/influence_leverage-1.png" width="672" style="display: block; margin: auto;" />

```r
dotplot_diag(x = influence_leverage[, 4], 
             cutoff = "internal",
             name = "leverage") + 
    labs(title = 'Leverage: unconfounded random effects',
         subtitle = 'Cutoffs determined by measures of internal scaling',
         y = 'Leverage',
         x = 'Participant ID')
```

<img src="figures/4B-response-characteristics/influence_leverage-2.png" width="672" style="display: block; margin: auto;" />

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

<img src="figures/4B-response-characteristics/influence_fixed-1.png" width="672" style="display: block; margin: auto;" />

Based on There are no influential fixed effects.

### Summary

The linear is well-specified.

# Quantile mixed model regression


```r
# Quantile model with 2.5, 25, 50, 75, and 97.5% quantiles
qmm <- lqmm(fixed = tri_mean ~ intensity,
            random = ~ intensity,
            group = PID,
            data = data_tm,
            tau = c(0.025, 0.25, 0.5, 0.75, 0.975))

# Summary 
summary(qmm)
```

```
## Call: lqmm(fixed = tri_mean ~ intensity, random = ~intensity, group = PID, 
##     tau = c(0.025, 0.25, 0.5, 0.75, 0.975), data = data_tm)
## 
## tau = 0.025
## 
## Fixed effects:
##                 Value Std. Error lower bound upper bound Pr(>|t|)   
## (Intercept)  -73.2187    24.0096   -121.4677     -24.970 0.003691 **
## intensity      8.6827    25.4745    -42.5103      59.876 0.734684   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.25
## 
## Fixed effects:
##                 Value Std. Error lower bound upper bound  Pr(>|t|)    
## (Intercept)  -73.5633    21.8599   -117.4923     -29.634 0.0014936 ** 
## intensity     21.4525     5.6575     10.0832      32.822 0.0004108 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.5
## 
## Fixed effects:
##                 Value Std. Error lower bound upper bound Pr(>|t|)   
## (Intercept)  -69.5025    22.4577   -114.6329     -24.372 0.003251 **
## intensity     17.8927     5.1781      7.4868      28.299 0.001144 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.75
## 
## Fixed effects:
##                 Value Std. Error lower bound upper bound Pr(>|t|)   
## (Intercept)  -67.5221    22.6926   -113.1246     -21.919 0.004532 **
## intensity     20.0009     5.8392      8.2666      31.735 0.001251 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.975
## 
## Fixed effects:
##                 Value Std. Error lower bound upper bound Pr(>|t|)   
## (Intercept)  -63.9822    23.3414   -110.8884     -17.076 0.008521 **
## intensity     23.5842     7.7880      7.9336      39.235 0.003916 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Null model (likelihood ratio):
## [1] 47.83 (p = 4.641e-12) 78.24 (p = 0.000e+00) 33.09 (p = 8.818e-09)
## [4] 49.46 (p = 2.025e-12) 39.11 (p = 4.014e-10)
## AIC:
## [1] 569.6 (df = 5) 499.6 (df = 5) 516.2 (df = 5) 530.5 (df = 5)
## [5] 550.5 (df = 5)
```

```r
# Get predicted values
## Level 0 (conditional, note difference to the lmer diagnostics)
quant_predict <- as.data.frame(predict(qmm, level = 0))
names(quant_predict) <- paste0('Q', c(2.5, 25, 50, 75, 97.5))

# Join with 'central_lmm'
data_lqmm <- data_tm %>%
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
  aes(x = intensity,
      y = Q50) +
  geom_ribbon(aes(ymin = `Q2.5`,
                  ymax = `Q97.5`),
              fill = '#E69F00') +
  geom_ribbon(aes(ymin = `Q25`,
                  ymax = `Q75`),
              fill = '#56B4E9') +
  geom_point(size = 3,
             shape = 21,
             fill = '#FFFFFF',
             colour = '#000000') +
  geom_hline(yintercept = 0,
             linetype = 2) +
  labs(title = paste('Quantile regression'),
       subtitle = 'Open circles: 50th percentile (median) | Blue band: interquartile range | Orange band: 95% prediction interval',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity)) 
```

<img src="figures/4B-response-characteristics/quantile-1.png" width="672" style="display: block; margin: auto;" />

```r
## With original data
ggplot(data = data_lqmm) +
  aes(x = intensity,
      y = Q50) +
  geom_ribbon(aes(ymin = `Q2.5`,
                  ymax = `Q97.5`),
              fill = '#E69F00') +
  geom_ribbon(aes(ymin = `Q25`,
                  ymax = `Q75`),
              fill = '#56B4E9') +
  geom_point(data = data_tm,
             aes(y = tri_mean),
             position = position_jitter(width = 0.03)) +
  geom_point(size = 3,
             shape = 21,
             fill = '#FFFFFF',
             colour = '#000000') +
  geom_hline(yintercept = 0,
             linetype = 2) +
  labs(title = paste('Quantile regression (with original Tukey trimean data)'),
       subtitle = 'Open circles: 50th percentile (median) | Blue band: interquartile range | Orange band: 95% prediction interval',
       x = 'Stimulus intensity (J)',
       y = 'SPARS rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity)) 
```

<img src="figures/4B-response-characteristics/quantile-2.png" width="672" style="display: block; margin: auto;" />

The response clearly varies across the quantiles, becoming wider as the intensity increases. 

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
## [1] methods   stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
##  [1] bindrcpp_0.2       car_2.1-6          sjPlot_2.4.1      
##  [4] HLMdiag_0.3.1      lqmm_1.5.3         lme4_1.1-15       
##  [7] Matrix_1.2-12      patchwork_0.0.1    forcats_0.2.0     
## [10] stringr_1.2.0      dplyr_0.7.4        purrr_0.2.4       
## [13] readr_1.1.1        tidyr_0.8.0        tibble_1.4.2      
## [16] ggplot2_2.2.1.9000 tidyverse_1.2.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##   [1] TH.data_1.0-8      minqa_1.2.4        colorspace_1.3-2  
##   [4] modeltools_0.2-21  sjlabelled_1.0.7   rprojroot_1.3-2   
##   [7] estimability_1.2   snakecase_0.8.1    rstudioapi_0.7    
##  [10] glmmTMB_0.2.0      MatrixModels_0.4-1 DT_0.4            
##  [13] mvtnorm_1.0-7      lubridate_1.7.1    coin_1.2-2        
##  [16] xml2_1.2.0         codetools_0.2-15   splines_3.4.3     
##  [19] mnormt_1.5-5       knitr_1.19         sjmisc_2.7.0      
##  [22] effects_4.0-0      bayesplot_1.4.0    jsonlite_1.5      
##  [25] nloptr_1.0.4       ggeffects_0.3.1    pbkrtest_0.4-7    
##  [28] broom_0.4.3        shiny_1.0.5        compiler_3.4.3    
##  [31] httr_1.3.1         sjstats_0.14.1     emmeans_1.1       
##  [34] backports_1.1.2    assertthat_0.2.0   lazyeval_0.2.1    
##  [37] survey_3.33        cli_1.0.0          quantreg_5.34     
##  [40] htmltools_0.3.6    tools_3.4.3        SparseGrid_0.8.2  
##  [43] coda_0.19-1        gtable_0.2.0       glue_1.2.0        
##  [46] reshape2_1.4.3     merTools_0.3.0     Rcpp_0.12.15      
##  [49] carData_3.0-0      cellranger_1.1.0   nlme_3.1-131      
##  [52] psych_1.7.8        lmtest_0.9-35      rvest_0.3.2       
##  [55] mime_0.5           stringdist_0.9.4.6 MASS_7.3-48       
##  [58] zoo_1.8-1          scales_0.5.0.9000  hms_0.4.1         
##  [61] parallel_3.4.3     sandwich_2.4-0     SparseM_1.77      
##  [64] pwr_1.2-1          TMB_1.7.12         yaml_2.1.16       
##  [67] stringi_1.1.6      highr_0.6          blme_1.0-4        
##  [70] rlang_0.1.6        pkgconfig_2.0.1    arm_1.9-3         
##  [73] evaluate_0.10.1    lattice_0.20-35    prediction_0.2.0  
##  [76] bindr_0.1          labeling_0.3       htmlwidgets_1.0   
##  [79] tidyselect_0.2.3   plyr_1.8.4         R6_2.2.2          
##  [82] multcomp_1.4-8     RLRsim_3.1-3       withr_2.1.1.9000  
##  [85] pillar_1.1.0       haven_1.1.1        foreign_0.8-69    
##  [88] mgcv_1.8-23        survival_2.41-3    abind_1.4-5       
##  [91] nnet_7.3-12        modelr_0.1.1       crayon_1.3.4      
##  [94] rmarkdown_1.8      grid_3.4.3         readxl_1.0.0      
##  [97] digest_0.6.15      xtable_1.8-2       httpuv_1.3.5      
## [100] stats4_3.4.3       munsell_0.4.3
```
