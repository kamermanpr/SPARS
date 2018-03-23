# FEST 2015
Peter Kamerman  
`r format(Sys.Date(), '%d %b %Y')`  



----

The response characateristics of FEST were examined at the level of each participant. Within each participant, we examined:

1. The stability of the variance in ratings at each stumulus intensity across the scale (i.e., is there a relationship between stimulus intensity and rating precision)  
2. Whether response charateristics of the lower half (innoccuous sensation: -50 to 0)he upper half of the scale (noxious sensation: 0 to 50).

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

# Clean and process data

Basic clean-up of the data, then calculate _Tukey trimean_ at each stimulus intensity for each participant (participant average), and finally the _median_ of the trimeans at each stimulus intensity across participants (group average). 


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
       y = 'FEST rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 0.5)) +
  theme_bw()
```

<img src="./figures/04-response-characteristics/sr_group-1.png" width="672" style="display: block; margin: auto;" />

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
       y = 'FEST rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~ PID, ncol = 4) +
  theme_bw()
```

<img src="./figures/04-response-characteristics/sr_participants-1.png" width="864" style="display: block; margin: auto;" />

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
                             y = 'FEST rating [-50 to 50]') +
                        scale_y_continuous(limits = c(-50, 50)) +
                        facet_wrap(~ block, ncol = 2) +
                        theme_bw()))

# Print plots
walk(.x = data_block$plots, ~ print(.x))
```

<img src="./figures/04-response-characteristics/sr_participants2-1.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-2.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-3.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-4.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-5.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-6.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-7.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-8.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-9.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-10.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-11.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-12.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-13.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-14.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-15.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-16.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-17.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-18.png" width="768" style="display: block; margin: auto;" /><img src="./figures/04-response-characteristics/sr_participants2-19.png" width="768" style="display: block; margin: auto;" />

----

# Linear mixed model regression

For each participant, stimuli were applied in four experimental blocks. Although there does not appear to be an order effect for trial or experimental block at a given stimulus intenisty (see: 03-order-effects.*), the blocks should be treated as observational units, and included as a random effect when analysing the data. 

#### Linear mixed model regression

To allow for a curvilinear relationship between stimulus intensity and rating, we modelled the data using polynomial regression, using 1^st^ (linear), 2^nd^ (quadratic), and 3^rd^ (cubic) order orthogonal polynomials. For each polynomial expression, we modelled the random effects as random intercept only, and as random intercept and slope. 

The random intercept only and random intercept and slope models were compared using logliklihood test, and the better model taken foward. Regression diagnostics were only run on the three best models (one from each of the polynomial expressions). Diagnostics examined level 1 (conditional / fixed effects), and level 2 (random effects) residuals and outliers, based on the approaches recommended by Loy and Hofmann [^1].

[^1]: Loy A, Hofmann H. HLMdiag: A suite of diagnostics for hierarchical linear models in R. J. Stat. Softw. 2014;56:1–28. [Available](https://www.jstatsoft.org/article/view/v056i05/v56i05.pdf)

**First order polynomial**


```r
# Model 1 (linear)
## Intercept
lmm1 <- lmerTest::lmer(tri_mean ~ intensity + (1 | PID),
                       data = data_tm,
                       REML = FALSE)

## Intercept and slope
lmm1b <- lmerTest::lmer(tri_mean ~ intensity + (intensity | PID),
                       data = data_tm,
                       REML = FALSE)

## Better model?
anova(lmm1, lmm1b)
```

```
## Data: data_tm
## Models:
## lmm1: tri_mean ~ intensity + (1 | PID)
## lmm1b: tri_mean ~ intensity + (intensity | PID)
##       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## lmm1   4 1814.7 1828.7 -903.37   1806.7                             
## lmm1b  6 1733.6 1754.6 -860.79   1721.6 85.146      2  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(lmm1b)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: tri_mean ~ intensity + (intensity | PID)
##    Data: data_tm
## 
##      AIC      BIC   logLik deviance df.resid 
##   1733.6   1754.6   -860.8   1721.6      238 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.0490 -0.4434  0.0144  0.5190  3.5999 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  PID      (Intercept) 598.33   24.461        
##           intensity    34.06    5.836   -0.89
##  Residual              42.54    6.522        
## Number of obs: 244, groups:  PID, 19
## 
## Fixed effects:
##             Estimate Std. Error      df t value Pr(>|t|)    
## (Intercept)  -39.765      5.737  18.959  -6.931 1.33e-06 ***
## intensity     14.126      1.413  18.849   9.999 5.70e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr)
## intensity -0.885
```

```r
## Diagnostics on lmm1b
### Level 1
```

# Model 2 (quadratic)
## Intercept
lmm2 <- lmerTest::lmer(tri_mean ~ poly(intensity, 2) + (1 | PID),
                       data = data_tm,
                       REML = FALSE)

## Intercept and slope
lmm2b <- lmerTest::lmer(tri_mean ~ poly(intensity, 2) + (intensity | PID),
                       data = data_tm,
                       REML = FALSE)

## Better model?
anova(lmm2, lmm2b)
summary(lmm2b)

# Model 3 (cubic)
## Intercept
lmm3 <- lmerTest::lmer(tri_mean ~ poly(intensity, 3) + (1 | PID),
                       data = data_tm,
                       REML = FALSE)

## Intercept and slope
lmm3b <- lmerTest::lmer(tri_mean ~ poly(intensity, 3) + (intensity | PID),
                       data = data_tm,
                       REML = FALSE)

## Better model?
anova(lmm3, lmm3b)
summary(lmm3b)

# Compare models
## Linear vs quadratic
anova(lmm1b, lmm2b) 

## Linear vs cubic
anova(lmm1b, lmm3b)

## Quadratic vs cubic
anova(lmm2b, lmm3b)

# Plot the final model
library(sjPlot)
set_theme(base = theme_bw())

## Plot random effects
sjp.lmer(fit = lmm3b,
         type = 're',
         y.offset = 0.4,  
         title = 'Random effects for the cubic model (lmm3b)')

## Plot fixed effects
sjp.lmer(fit = lmm3b,
         type = 'fe',
         title = 'Fixed effects coefficients for the cubic model (lmm3b)')

## Plot fixed effect slopes
sjp.lmer(fit = lmm3b,
         type = 'poly',
         poly.term = 'intensity',
         vars = 'intensity',
         title = 'Fixed effect curve for the cubic model (lmm3b)')
```



Best model is a cubic model with random slope and intercept. 

# Quantile mixed model regression


```r
# Quantile model with 2.5, 25, 50, 75, and 97.5% quantiles
(qmm <- lqmm(fixed = tri_mean ~ poly(intensity, 3),
            random = ~ intensity,
            group = PID,
            data = data_tm,
            tau = c(0.025, 0.25, 0.5, 0.75, 0.975)))
```

```
## Call: lqmm(fixed = tri_mean ~ poly(intensity, 3), random = ~intensity, 
##     group = PID, tau = c(0.025, 0.25, 0.5, 0.75, 0.975), data = data_tm)
## 
## Fixed effects:
##                      tau = 0.025  tau = 0.250  tau = 0.500  tau = 0.750
## (Intercept)          -36.3724     -16.0624       3.2873      19.0218   
## poly(intensity, 3)1  204.7079     205.0663     204.0394     203.2674   
## poly(intensity, 3)2   11.5495       0.8431       2.2389       5.9630   
## poly(intensity, 3)3   26.7629      21.9243      22.1176      22.6834   
##                      tau = 0.975
## (Intercept)           22.0604   
## poly(intensity, 3)1  188.9824   
## poly(intensity, 3)2   22.3598   
## poly(intensity, 3)3   12.1005   
## 
## Covariance matrix of the random effects:
## tau = 0.025
## (Intercept)   intensity 
##       34350      526600 
## tau = 0.25
## (Intercept)   intensity 
##       176.1     12556.7 
## tau = 0.5
## (Intercept)   intensity 
##       184.3      9242.1 
## tau = 0.75
## (Intercept)   intensity 
##       154.2     16544.0 
## tau = 0.975
## (Intercept)   intensity 
##       35725      647116 
## 
## Residual scale parameter: 0.8663 (tau = 0.025)  2.7370 (tau = 0.25)  3.3857 (tau = 0.5)  2.6850 (tau = 0.75)  0.7178 (tau = 0.975) 
## Log-likelihood: -1145.0 (tau = 0.025)   -938.8 (tau = 0.25)   -921.9 (tau = 0.5)   -949.7 (tau = 0.75)  -1099.1 (tau = 0.975) 
## 
## Number of observations: 244 
## Number of groups: 19
```

```r
# Summary 
summary(qmm)
```

```
## Call: lqmm(fixed = tri_mean ~ poly(intensity, 3), random = ~intensity, 
##     group = PID, tau = c(0.025, 0.25, 0.5, 0.75, 0.975), data = data_tm)
## 
## tau = 0.025
## 
## Fixed effects:
##                        Value Std. Error lower bound upper bound  Pr(>|t|)
## (Intercept)         -36.3724    21.8495    -80.2805      7.5358   0.10236
## poly(intensity, 3)1 204.7079    20.4237    163.6650    245.7508 1.865e-13
## poly(intensity, 3)2  11.5495    21.5284    -31.7135     54.8124   0.59406
## poly(intensity, 3)3  26.7629    14.4574     -2.2903     55.8161   0.07018
##                        
## (Intercept)            
## poly(intensity, 3)1 ***
## poly(intensity, 3)2    
## poly(intensity, 3)3 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.25
## 
## Fixed effects:
##                         Value Std. Error lower bound upper bound  Pr(>|t|)
## (Intercept)         -16.06242    5.93660   -27.99246     -4.1324  0.009352
## poly(intensity, 3)1 205.06628   22.73242   159.38378    250.7488 5.488e-12
## poly(intensity, 3)2   0.84314   10.68665   -20.63249     22.3188  0.937436
## poly(intensity, 3)3  21.92427    7.13662     7.58269     36.2659  0.003466
##                        
## (Intercept)         ** 
## poly(intensity, 3)1 ***
## poly(intensity, 3)2    
## poly(intensity, 3)3 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.5
## 
## Fixed effects:
##                        Value Std. Error lower bound upper bound Pr(>|t|)
## (Intercept)           3.2873     7.0190    -10.8180      17.393 0.641619
## poly(intensity, 3)1 204.0394    23.0548    157.7090     250.370 9.87e-12
## poly(intensity, 3)2   2.2389    10.5106    -18.8831      23.361 0.832204
## poly(intensity, 3)3  22.1176     7.5871      6.8708      37.364 0.005347
##                        
## (Intercept)            
## poly(intensity, 3)1 ***
## poly(intensity, 3)2    
## poly(intensity, 3)3 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.75
## 
## Fixed effects:
##                        Value Std. Error lower bound upper bound  Pr(>|t|)
## (Intercept)          19.0218     7.4165      4.1179      33.926   0.01344
## poly(intensity, 3)1 203.2674    23.4178    156.2075     250.327 1.777e-11
## poly(intensity, 3)2   5.9630    10.2024    -14.5394      26.465   0.56158
## poly(intensity, 3)3  22.6834     7.7276      7.1543      38.212   0.00506
##                        
## (Intercept)         *  
## poly(intensity, 3)1 ***
## poly(intensity, 3)2    
## poly(intensity, 3)3 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## tau = 0.975
## 
## Fixed effects:
##                        Value Std. Error lower bound upper bound Pr(>|t|)
## (Intercept)          22.0604    37.0679    -52.4303      96.551   0.5545
## poly(intensity, 3)1 188.9824    22.9848    142.7927     235.172 8.76e-11
## poly(intensity, 3)2  22.3598    13.2475     -4.2621      48.982   0.0978
## poly(intensity, 3)3  12.1005     8.3602     -4.7000      28.901   0.1542
##                        
## (Intercept)            
## poly(intensity, 3)1 ***
## poly(intensity, 3)2 .  
## poly(intensity, 3)3    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Null model (likelihood ratio):
## [1] 132.7 (p = 0) 270.8 (p = 0) 271.1 (p = 0) 247.2 (p = 0) 188.1 (p = 0)
## AIC:
## [1] 2304 (df = 7) 1892 (df = 7) 1858 (df = 7) 1913 (df = 7) 2212 (df = 7)
```

```r
# Get predicted values
## Level 0 (population)
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
       y = 'FEST rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity)) +
  theme_bw()
```

<img src="./figures/04-response-characteristics/quantile-1.png" width="672" style="display: block; margin: auto;" />

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
       y = 'FEST rating [-50 to 50]') +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(breaks = unique(data_lqmm$intensity)) +
  theme_bw()
```

<img src="./figures/04-response-characteristics/quantile-2.png" width="672" style="display: block; margin: auto;" />

Good stability across the quantiles.

----

# Session information

```r
sessionInfo()
```

```
## R version 3.4.3 (2017-11-30)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: OS X El Capitan 10.11.6
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2       HLMdiag_0.3.1      sjPlot_2.4.0      
##  [4] lqmm_1.5.3         lme4_1.1-14        Matrix_1.2-12     
##  [7] dplyr_0.7.4        purrr_0.2.4        readr_1.1.1       
## [10] tidyr_0.7.2        tibble_1.4.2       ggplot2_2.2.1.9000
## [13] tidyverse_1.1.1    magrittr_1.5      
## 
## loaded via a namespace (and not attached):
##   [1] TH.data_1.0-8       minqa_1.2.4         colorspace_1.3-2   
##   [4] modeltools_0.2-21   sjlabelled_1.0.7    rprojroot_1.2      
##   [7] htmlTable_1.9       estimability_1.2    snakecase_0.8.1    
##  [10] base64enc_0.1-3     glmmTMB_0.2.0       DT_0.2             
##  [13] mvtnorm_1.0-7       lubridate_1.6.0     coin_1.2-2         
##  [16] xml2_1.1.1          codetools_0.2-15    splines_3.4.3      
##  [19] mnormt_1.5-5        knitr_1.17          sjmisc_2.7.0       
##  [22] effects_4.0-0       bayesplot_1.4.0     Formula_1.2-2      
##  [25] jsonlite_1.5        nloptr_1.0.4        ggeffects_0.3.1    
##  [28] broom_0.4.2         cluster_2.0.6       shiny_1.0.5        
##  [31] compiler_3.4.3      httr_1.3.1          sjstats_0.14.1     
##  [34] emmeans_1.1         backports_1.1.1     assertthat_0.2.0   
##  [37] lazyeval_0.2.1      survey_3.33         cli_1.0.0          
##  [40] acepack_1.4.1       htmltools_0.3.6     tools_3.4.3        
##  [43] lmerTest_2.0-33     SparseGrid_0.8.2    coda_0.19-1        
##  [46] gtable_0.2.0        glue_1.1.1          reshape2_1.4.3     
##  [49] merTools_0.3.0      Rcpp_0.12.15        carData_3.0-0      
##  [52] cellranger_1.1.0    nlme_3.1-131        psych_1.7.8        
##  [55] lmtest_0.9-35       stringr_1.2.0       rvest_0.3.2        
##  [58] mime_0.5            stringdist_0.9.4.6  MASS_7.3-47        
##  [61] zoo_1.8-0           scales_0.5.0.9000   hms_0.3            
##  [64] parallel_3.4.3      sandwich_2.4-0      RColorBrewer_1.1-2 
##  [67] pwr_1.2-1           TMB_1.7.12          yaml_2.1.14        
##  [70] gridExtra_2.3       rpart_4.1-11        latticeExtra_0.6-28
##  [73] stringi_1.1.5       checkmate_1.8.5     blme_1.0-4         
##  [76] rlang_0.1.6.9003    pkgconfig_2.0.1     arm_1.9-3          
##  [79] evaluate_0.10.1     lattice_0.20-35     prediction_0.2.0   
##  [82] bindr_0.1           htmlwidgets_0.9     labeling_0.3       
##  [85] tidyselect_0.2.2    plyr_1.8.4          R6_2.2.2           
##  [88] Hmisc_4.0-3         multcomp_1.4-8      RLRsim_3.1-3       
##  [91] pillar_1.1.0        haven_1.1.0         foreign_0.8-69     
##  [94] withr_2.1.1.9000    mgcv_1.8-22         survival_2.41-3    
##  [97] abind_1.4-5         nnet_7.3-12         modelr_0.1.1       
## [100] crayon_1.3.4        rmarkdown_1.6       grid_3.4.3         
## [103] readxl_1.0.0        data.table_1.10.4-3 forcats_0.2.0      
## [106] digest_0.6.15       xtable_1.8-2        httpuv_1.3.5       
## [109] stats4_3.4.3        munsell_0.4.3
```
