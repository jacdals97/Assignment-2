Assignment 2
============

In this assignment you will have to discuss a few important questions
(given the data you have). More details below. The assignment submitted
to the teachers consists of: - a report answering and discussing the
questions (so we can assess your conceptual understanding and ability to
explain and critically reflect) - a link to a git repository with all
the code (so we can assess your code)

Part 1 - Basic description of language development - Describe your
sample (n, age, gender, clinical and cognitive features of the two
groups) and critically assess whether the groups (ASD and TD) are
balanced - Describe linguistic development (in terms of MLU over time)
in TD and ASD children (as a function of group). - Describe how parental
use of language (in terms of MLU) changes over time. What do you think
is going on? - Include individual differences in your model of language
development (in children). Identify the best model.

Part 2 - Model comparison - Discuss the differences in performance of
your model in training and testing data - Which individual differences
should be included in a model that maximizes your ability to
explain/predict new data? - Predict a new kid’s performance (Bernie) and
discuss it against expected performance of the two groups

Part 3 - Simulations to plan a new study - Report and discuss a power
analyses identifying how many new kids you would need to replicate the
results

The following involves only Part 1.

Learning objectives
-------------------

-   Summarize and report data and models
-   Critically apply mixed effects (or multilevel) models
-   Explore the issues involved in feature selection

Quick recap
===========

Autism Spectrum Disorder is often related to language impairment.
However, this phenomenon has not been empirically traced in detail: i)
relying on actual naturalistic language production, ii) over extended
periods of time.

We therefore videotaped circa 30 kids with ASD and circa 30 comparison
kids (matched by linguistic performance at visit 1) for ca. 30 minutes
of naturalistic interactions with a parent. We repeated the data
collection 6 times per kid, with 4 months between each visit. We
transcribed the data and counted: i) the amount of words that each kid
uses in each video. Same for the parent. ii) the amount of unique words
that each kid uses in each video. Same for the parent. iii) the amount
of morphemes per utterance (Mean Length of Utterance) displayed by each
child in each video. Same for the parent.

This data is in the file you prepared in the previous class.

NB. A few children have been excluded from your datasets. We will be
using them next week to evaluate how good your models are in assessing
the linguistic development in new participants.

This RMarkdown file includes 1) questions (see above). Questions have to
be answered/discussed in a separate document that you have to directly
send to the teachers. 2) A break down of the questions into a guided
template full of hints for writing the code to solve the exercises. Fill
in the code and the paragraphs as required. Then report your results in
the doc for the teachers.

REMEMBER that you will have to have a github repository for the code and
send the answers to Kenneth and Riccardo without code (but a link to
your github/gitlab repository). This way we can check your code, but you
are also forced to figure out how to report your analyses :-)

Before we get going, here is a reminder of the issues you will have to
discuss in your report:

1- Describe your sample (n, age, gender, clinical and cognitive features
of the two groups) and critically assess whether the groups (ASD and TD)
are balanced 2- Describe linguistic development (in terms of MLU over
time) in TD and ASD children (as a function of group). 3- Describe how
parental use of language (in terms of MLU) changes over time. What do
you think is going on? 4- Include individual differences in your model
of language development (in children). Identify the best model.

Let’s go
========

### Loading the relevant libraries

Load necessary libraries : what will you need? - e.g. something to deal
with the data - e.g. mixed effects models - e.g. something to plot with

### Define your working directory and load the data

If you created a project for this class and opened this Rmd file from
within that project, your working directory is your project directory.

If you opened this Rmd file outside of a project, you will need some
code to find the data: - Create a new variable called locpath
(localpath) - Set it to be equal to your working directory - Move to
that directory (setwd(locpath)) - Load the data you saved last time (use
read\_csv(fileName))

### Characterize the participants (Exercise 1)

Identify relevant variables: participants demographic characteristics,
diagnosis, ADOS, Verbal IQ, Non Verbal IQ, Socialization, Visit, Number
of words used, Number of unique words used, mean length of utterance in
both child and parents.

Make sure the variables are in the right format.

Describe the characteristics of the two groups of participants and
whether the two groups are well matched.

``` r
clean %>% subset(VISIT == 1) %>% group_by(Diagnosis, Gender) %>% summarise(Agem = mean(Age), n()) #checking age, diagnosis and gender balances 
```

    ## # A tibble: 4 x 4
    ## # Groups:   Diagnosis [2]
    ##   Diagnosis Gender  Agem `n()`
    ##   <fct>     <fct>  <dbl> <int>
    ## 1 ASD       F       33.4     4
    ## 2 ASD       M       32.9    25
    ## 3 TD        F       20.2     6
    ## 4 TD        M       20.4    26

``` r
clean %>% subset(VISIT == 1) %>% group_by(Diagnosis, Ethnicity) %>% summarise(n()) #checking ethnicity balance
```

    ## # A tibble: 8 x 3
    ## # Groups:   Diagnosis [2]
    ##   Diagnosis Ethnicity        `n()`
    ##   <fct>     <fct>            <int>
    ## 1 ASD       African American     2
    ## 2 ASD       Bangladeshi          1
    ## 3 ASD       Lebanese             1
    ## 4 ASD       White               22
    ## 5 ASD       White/Asian          1
    ## 6 ASD       White/Latino         2
    ## 7 TD        Asian                1
    ## 8 TD        White               31

``` r
clean %>% subset(VISIT == 1) %>% group_by(Diagnosis) %>% summarise(mean(MOT_MLU), mean(CHI_MLU), mean(MullenRawv1), mean(ExpressiveLangRawv1), mean(Socialization), mean(ADOSv1), mean(tokens_CHI), mean(types_CHI)) #MChecking differences of preictors between the ASd and TD kids
```

    ## # A tibble: 2 x 9
    ##   Diagnosis `mean(MOT_MLU)` `mean(CHI_MLU)` `mean(MullenRaw~
    ##   <fct>               <dbl>           <dbl>            <dbl>
    ## 1 ASD                  3.35            1.30             26.9
    ## 2 TD                   3.73            1.31             26  
    ## # ... with 5 more variables: `mean(ExpressiveLangRawv1)` <dbl>,
    ## #   `mean(Socialization)` <dbl>, `mean(ADOSv1)` <dbl>,
    ## #   `mean(tokens_CHI)` <dbl>, `mean(types_CHI)` <dbl>

Let’s test hypothesis 1: Children with ASD display a language impairment (Exercise 2)
-------------------------------------------------------------------------------------

### Hypothesis: The child’s MLU changes: i) over time, ii) according to diagnosis

Let’s start with a simple mixed effects linear model

Remember to plot the data first and then to run a statistical test. -
Which variable(s) should be included as fixed factors? - Which
variable(s) should be included as random factors?

``` r
clean$VISIT <- as.numeric(clean$VISIT)
clean$SUBJ <- as.factor(clean$SUBJ)

#Plotting the effect of visit on CHI_MLU across diagnosis using bar plot 
ggplot(clean, aes(VISIT, CHI_MLU, fill = Diagnosis))+
  geom_bar(stat = "summary", position = "dodge")+
  geom_errorbar(stat = "summary", position = position_dodge(width = 0.9))
```

    ## No summary function supplied, defaulting to `mean_se()
    ## No summary function supplied, defaulting to `mean_se()

![](A2_P1_LanguageASD_instructions_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#Plotting the linear relationship between visit and CHI_MLU across diagnosis
ggplot(clean, aes(VISIT, CHI_MLU, colour = Diagnosis))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0)
```

![](A2_P1_LanguageASD_instructions_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
#Highlightinh individual differences between children in the two conditions
ggplot(clean, aes(VISIT, CHI_MLU, color = SUBJ))+
  facet_wrap(.~Diagnosis)+
  geom_smooth(method = lm, alpha = 0)
```

![](A2_P1_LanguageASD_instructions_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
#Creating base model in which child MLU is predicted by the fixed effect of the interaction between visit and diagnosis. 
#The model is both a random intercept and random slope model. Each participant has been assigned their own intercept and are allowed the vary independently
model1 <- lmer(CHI_MLU ~ VISIT*Diagnosis + (1+VISIT|SUBJ), clean, REML = FALSE)
summary(model1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: CHI_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    572.5    603.4   -278.2    556.5      344 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.48471 -0.53247 -0.08812  0.44178  2.73713 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.29423  0.5424        
    ##           VISIT       0.01122  0.1059   -0.16
    ##  Residual             0.16063  0.4008        
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)        1.30459    0.12273  10.629
    ## VISIT              0.10046    0.02680   3.749
    ## DiagnosisTD       -0.21693    0.16953  -1.280
    ## VISIT:DiagnosisTD  0.25331    0.03712   6.823
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnsTD
    ## VISIT       -0.443              
    ## DiagnosisTD -0.724  0.321       
    ## VISIT:DgnTD  0.320 -0.722 -0.445

``` r
#Creating null model
model0 <- lmer(CHI_MLU ~ 1 + (1+VISIT|SUBJ), clean, REML = FALSE)
summary(model0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: CHI_MLU ~ 1 + (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    659.3    678.6   -324.6    649.3      347 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17666 -0.53016 -0.09769  0.45334  2.64293 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.4279   0.6541        
    ##           VISIT       0.0815   0.2855   -0.56
    ##  Residual             0.1613   0.4016        
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  1.54204    0.07727   19.96

Convergence troubleshoot

``` r
tt <- getME(model1,"theta")
ll <- getME(model1,"lower")
min(tt[ll==0]) #output should be more than 10^-6
```

    ## [1] 0.2609699

``` r
#using package numDeriv to calculate gradients
derivs1 <- model1@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
```

    ## [1] 6.299289e-05

``` r
#restarting from last fit
ss <- getME(model1,c("theta","fixef"))
m2 <- update(model1,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
```

How would you evaluate whether the model is a good model?

``` r
#Comparing null model and model 1
anova(model0, model1)
```

    ## Data: clean
    ## Models:
    ## model0: CHI_MLU ~ 1 + (1 + VISIT | SUBJ)
    ## model1: CHI_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## model0  5 659.27 678.59 -324.64   649.27                             
    ## model1  8 572.46 603.37 -278.23   556.46 92.811      3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Computing marginal and conditional R-squared for null model
r.squaredGLMM(model0)
```

    ## Warning: 'r.squaredGLMM' now calculates a revised statistic. See the help
    ## page.

    ##      R2m       R2c
    ## [1,]   0 0.8505565

``` r
#Computing marginal and conditional R-squared for model 1
r.squaredGLMM(model1)
```

    ##            R2m       R2c
    ## [1,] 0.3571568 0.8150111

``` r
#adding fitted values to data frame
clean$fit <- fitted.values(model1)

#Plotting fitted values against actual values
ggplot(clean, aes(fit, CHI_MLU))+
  geom_point()+
  labs(x = "Predicted values", y = "Child MLU")
```

![](A2_P1_LanguageASD_instructions_files/figure-markdown_github/unnamed-chunk-3-1.png)

Not too good, right? Let’s check whether a growth curve model is better.
Remember: a growth curve model assesses whether changes in time can be
described by linear, or quadratic, or cubic (or… etc.) components. First
build the different models, then compare them to see which one is
better.

``` r
#Has been excluded
```

Exciting right? Let’s check whether the model is doing an alright job at
fitting the data. Plot the actual CHI\_MLU data against the predictions
of the model fitted(model).

Now it’s time to report our results. Remember to report: - the estimates
for each predictor (beta estimate, standard error, p-value) - A plain
word description of the results - A plot of your model’s predictions
(and some comments on whether the predictions are sensible)

\[REPORT THE RESULTS\] Linguistic development of children MLU is
affected by … \[COMPLETE\]

Let’s test hypothesis 2: Parents speak equally to children with ASD and TD (Exercise 3)
---------------------------------------------------------------------------------------

### Hypothesis: Parental MLU changes: i) over time, ii) according to diagnosis

``` r
#MOT_MLU model, MOT_MLU predicted by the fixed effect of the interaction between visit and diagnosis. 
#The model is both a random intercept and random slope model. Each participant has been assigned their own intercept and are allowed the vary independently 
MOTmodel1 <- lmer(MOT_MLU ~ VISIT*Diagnosis + (1+VISIT|SUBJ), clean, REML = T)
summary(MOTmodel1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MOT_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ## REML criterion at convergence: 514.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.87895 -0.58880 -0.03781  0.52942  2.93499 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.34036  0.5834        
    ##           VISIT       0.01165  0.1080   -0.70
    ##  Residual             0.14715  0.3836        
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)        3.31423    0.12745  26.005
    ## VISIT              0.09925    0.02655   3.738
    ## DiagnosisTD        0.35620    0.17602   2.024
    ## VISIT:DiagnosisTD  0.04030    0.03677   1.096
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnsTD
    ## VISIT       -0.755              
    ## DiagnosisTD -0.724  0.547       
    ## VISIT:DgnTD  0.545 -0.722 -0.756

``` r
#Creating null model
MOTmodel0 <- lmer(MOT_MLU ~ 1 + (1+VISIT|SUBJ), clean, REML = T, control = lmerControl())
summary(MOTmodel0)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MOT_MLU ~ 1 + (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ## REML criterion at convergence: 552.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.79425 -0.52074 -0.03688  0.54144  2.69636 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.5297   0.7278        
    ##           VISIT       0.0256   0.1600   -0.75
    ##  Residual             0.1477   0.3843        
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  3.91193    0.06547   59.75

``` r
#Comparing models using anova
anova(MOTmodel0, MOTmodel1)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: clean
    ## Models:
    ## MOTmodel0: MOT_MLU ~ 1 + (1 + VISIT | SUBJ)
    ## MOTmodel1: MOT_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ##           Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## MOTmodel0  5 558.66 577.98 -274.33   548.66                             
    ## MOTmodel1  8 513.48 544.39 -248.74   497.48 51.177      3  4.486e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Plotting the linear relationship between visit and MOT_MLU across diagnosis
ggplot(clean, aes(VISIT, MOT_MLU, colour = Diagnosis))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0)
```

![](A2_P1_LanguageASD_instructions_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Adding new variables (Exercise 4)

Your task now is to figure out how to best describe the children
linguistic trajectory. The dataset contains a bunch of additional
demographic, cognitive and clinical variables (e.g.verbal and non-verbal
IQ). Try them out and identify the statistical models that best
describes your data (that is, the children’s MLU). Describe how you
selected the best model and send the code to run the model to Riccardo
and Kenneth

``` r
#Very very naughty data exploration using the dredge tool

#Specifying complex model
chi_model1 <- lmer(CHI_MLU ~ VISIT*Diagnosis*MOT_MLU*ExpressiveLangRawv1 + (1+VISIT|SUBJ), clean, REML =F, control=lmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))

summary(chi_model1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: CHI_MLU ~ VISIT * Diagnosis * MOT_MLU * ExpressiveLangRawv1 +  
    ##     (1 + VISIT | SUBJ)
    ##    Data: clean
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    447.6    524.9   -203.8    407.6      332 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3624 -0.5445 -0.0615  0.5082  3.1537 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.048241 0.21964       
    ##           VISIT       0.005603 0.07485  -0.33
    ##  Residual             0.135817 0.36853       
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                    1.875860   1.186854   1.581
    ## VISIT                                         -0.065549   0.387888  -0.169
    ## DiagnosisTD                                   -0.305512   2.602438  -0.117
    ## MOT_MLU                                       -0.377476   0.312846  -1.207
    ## ExpressiveLangRawv1                           -0.094830   0.066138  -1.434
    ## VISIT:DiagnosisTD                             -0.107598   0.758045  -0.142
    ## VISIT:MOT_MLU                                 -0.003721   0.099806  -0.037
    ## DiagnosisTD:MOT_MLU                           -0.064919   0.667193  -0.097
    ## VISIT:ExpressiveLangRawv1                      0.013437   0.021437   0.627
    ## DiagnosisTD:ExpressiveLangRawv1               -0.017508   0.128532  -0.136
    ## MOT_MLU:ExpressiveLangRawv1                    0.039029   0.016823   2.320
    ## VISIT:DiagnosisTD:MOT_MLU                      0.173642   0.185350   0.937
    ## VISIT:DiagnosisTD:ExpressiveLangRawv1          0.010331   0.037388   0.276
    ## VISIT:MOT_MLU:ExpressiveLangRawv1             -0.001160   0.005370  -0.216
    ## DiagnosisTD:MOT_MLU:ExpressiveLangRawv1        0.006599   0.032469   0.203
    ## VISIT:DiagnosisTD:MOT_MLU:ExpressiveLangRawv1 -0.007425   0.009079  -0.818

    ## 
    ## Correlation matrix not shown by default, as p = 16 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
search2 <- dredge(chi_model1) #using dredge to select model with lowest AICc
```

    ## Fixed term is "(Intercept)"

``` r
subset(search2, delta == 0)
```

    ## Global model call: lmer(formula = CHI_MLU ~ VISIT * Diagnosis * MOT_MLU * ExpressiveLangRawv1 + 
    ##     (1 + VISIT | SUBJ), data = clean, REML = F, control = lmerControl(optimizer = "bobyqa", 
    ##     optCtrl = list(maxfun = 2e+05)))
    ## ---
    ## Model selection table 
    ##      (Int) Dgn      ELR MOT_MLU      VIS Dgn:ELR Dgn:VIS ELR:MOT_MLU
    ## 2528 1.594   + -0.07555 -0.2959 -0.07596       +       +     0.03384
    ##       ELR:VIS Dgn:ELR:VIS df  logLik AICc delta weight
    ## 2528 0.008567           + 14 -206.39  442     0      1
    ## Models ranked by AICc(x) 
    ## Random terms (all models): 
    ## '1 + VISIT | SUBJ'

``` r
#Specifying the dredged model
dredge_model <- lmer(CHI_MLU ~ VISIT*Diagnosis*ExpressiveLangRawv1 + MOT_MLU + ExpressiveLangRawv1:MOT_MLU + (1+VISIT|SUBJ), clean, REML =F) 
summary(dredge_model)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: CHI_MLU ~ VISIT * Diagnosis * ExpressiveLangRawv1 + MOT_MLU +  
    ##     ExpressiveLangRawv1:MOT_MLU + (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    440.8    494.9   -206.4    412.8      338 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4450 -0.5673 -0.0632  0.4907  3.1497 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.040344 0.20086       
    ##           VISIT       0.004745 0.06888  -0.17
    ##  Residual             0.139494 0.37349       
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                                        Estimate Std. Error t value
    ## (Intercept)                            1.593784   0.562204   2.835
    ## VISIT                                 -0.075962   0.055193  -1.376
    ## DiagnosisTD                           -0.787337   0.354094  -2.224
    ## ExpressiveLangRawv1                   -0.075547   0.030197  -2.502
    ## MOT_MLU                               -0.295844   0.147243  -2.009
    ## VISIT:DiagnosisTD                      0.669756   0.100448   6.668
    ## VISIT:ExpressiveLangRawv1              0.008567   0.002919   2.934
    ## DiagnosisTD:ExpressiveLangRawv1        0.017612   0.017608   1.000
    ## ExpressiveLangRawv1:MOT_MLU            0.033836   0.007481   4.523
    ## VISIT:DiagnosisTD:ExpressiveLangRawv1 -0.022913   0.004988  -4.594
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnsTD ExpLR1 MOT_ML VISIT:DgTD VISIT:E DTD:EL
    ## VISIT       -0.095                                                      
    ## DiagnosisTD -0.144  0.371                                               
    ## ExprssvLnR1 -0.952  0.098  0.135                                        
    ## MOT_MLU     -0.936 -0.147 -0.049  0.875                                 
    ## VISIT:DgnTD  0.243 -0.516 -0.659 -0.218 -0.127                          
    ## VISIT:ExLR1  0.110 -0.919 -0.344 -0.127  0.108  0.484                   
    ## DgnsTD:ELR1  0.182 -0.361 -0.952 -0.194  0.006  0.634      0.396        
    ## ELR1:MOT_ML  0.910  0.124  0.043 -0.938 -0.949  0.112     -0.111  -0.010
    ## VISIT:DTD:E -0.245  0.510  0.629  0.245  0.128 -0.954     -0.564  -0.667
    ##             ELR1:M
    ## VISIT             
    ## DiagnosisTD       
    ## ExprssvLnR1       
    ## MOT_MLU           
    ## VISIT:DgnTD       
    ## VISIT:ExLR1       
    ## DgnsTD:ELR1       
    ## ELR1:MOT_ML       
    ## VISIT:DTD:E -0.120

``` r
anova(dredge_model)
```

    ## Analysis of Variance Table
    ##                                     Df Sum Sq Mean Sq  F value
    ## VISIT                                1 34.870  34.870 249.9719
    ## Diagnosis                            1  3.579   3.579  25.6581
    ## ExpressiveLangRawv1                  1 20.458  20.458 146.6614
    ## MOT_MLU                              1  8.880   8.880  63.6554
    ## VISIT:Diagnosis                      1  9.185   9.185  65.8416
    ## VISIT:ExpressiveLangRawv1            1  0.247   0.247   1.7716
    ## Diagnosis:ExpressiveLangRawv1        1  0.737   0.737   5.2843
    ## ExpressiveLangRawv1:MOT_MLU          1  2.232   2.232  16.0027
    ## VISIT:Diagnosis:ExpressiveLangRawv1  1  2.944   2.944  21.1016

``` r
r.squaredGLMM((dredge_model))
```

    ##            R2m      R2c
    ## [1,] 0.7299261 0.839008

``` r
#This is the model we ended up with in the end. We found it by specifying a lot of different models and using anova to test which model was better.
own_model <- lmer(CHI_MLU ~ VISIT*Diagnosis + ExpressiveLangRawv1 + MOT_MLU + (1+VISIT|SUBJ), clean, REML =F) 
summary(own_model)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: CHI_MLU ~ VISIT * Diagnosis + ExpressiveLangRawv1 + MOT_MLU +  
    ##     (1 + VISIT | SUBJ)
    ##    Data: clean
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    472.5    511.1   -226.2    452.5      342 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2536 -0.5788 -0.0687  0.4845  3.1749 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.064870 0.25470       
    ##           VISIT       0.008515 0.09228  -0.34
    ##  Residual             0.146086 0.38221       
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error t value
    ## (Intercept)         -0.964021   0.200200  -4.815
    ## VISIT                0.066472   0.024858   2.674
    ## DiagnosisTD         -0.527282   0.115789  -4.554
    ## ExpressiveLangRawv1  0.065652   0.006987   9.396
    ## MOT_MLU              0.341446   0.049073   6.958
    ## VISIT:DiagnosisTD    0.239463   0.033841   7.076
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnsTD ExpLR1 MOT_ML
    ## VISIT       -0.125                            
    ## DiagnosisTD -0.103  0.480                     
    ## ExprssvLnR1 -0.445  0.032 -0.149              
    ## MOT_MLU     -0.696 -0.195 -0.119 -0.192       
    ## VISIT:DgnTD  0.232 -0.696 -0.637  0.014 -0.057

``` r
anova(own_model)
```

    ## Analysis of Variance Table
    ##                     Df  Sum Sq Mean Sq F value
    ## VISIT                1 27.8113 27.8113 190.377
    ## Diagnosis            1  2.4311  2.4311  16.641
    ## ExpressiveLangRawv1  1 17.3923 17.3923 119.055
    ## MOT_MLU              1  7.9450  7.9450  54.386
    ## VISIT:Diagnosis      1  7.3148  7.3148  50.072

``` r
r.squaredGLMM((own_model))
```

    ##            R2m       R2c
    ## [1,] 0.6632222 0.8255512

``` r
#Using the anova function, models are compared in increasing order of complexity 
anova(own_model, dredge_model)
```

    ## Data: clean
    ## Models:
    ## own_model: CHI_MLU ~ VISIT * Diagnosis + ExpressiveLangRawv1 + MOT_MLU + 
    ## own_model:     (1 + VISIT | SUBJ)
    ## dredge_model: CHI_MLU ~ VISIT * Diagnosis * ExpressiveLangRawv1 + MOT_MLU + 
    ## dredge_model:     ExpressiveLangRawv1:MOT_MLU + (1 + VISIT | SUBJ)
    ##              Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
    ## own_model    10 472.46 511.09 -226.23   452.46                         
    ## dredge_model 14 440.78 494.87 -206.39   412.78 39.676      4  5.049e-08
    ##                 
    ## own_model       
    ## dredge_model ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#dredge model gives a smaller AIC and BIC but we stick with the other model as it is easier to interpret and because we found it in a less malicious way.
```

How you could choose which predictors to include

The forbidden interval - (lasso/elasticnet) sum up the beta estimates
divide each betaestimate with the sum if the values are between 1 and -1
then drop that predictor-estimate
