Sex and Age but not Body Mass Index (BMI) are Significant Predictors of
Serum Retinol Binding Protein 4 (RBP4) and Transthyretin (TTR)
Concentrations
================
Aprajita S. Yadav
2025-06-20

- [Load Data](#load-data)
  - [Determine Normality](#determine-normality)
  - [Determine Variable Correlation](#determine-variable-correlation)
- [RBP4 Model](#rbp4-model)
  - [RBP4 Final Model](#rbp4-final-model)
  - [RBP4 Model Plots](#rbp4-model-plots)
  - [Post-hoc analysis of RBP4 and
    eGFR](#post-hoc-analysis-of-rbp4-and-egfr)
- [TTR Model](#ttr-model)
  - [TTR Final Model](#ttr-final-model)
  - [TTR Model Plots](#ttr-model-plots)
- [Retinol Model](#retinol-model)
  - [Retinol Final Model](#retinol-final-model)
  - [Retinol Model Plots](#retinol-model-plots)
  - [Sensitivity Analysis of Retinol and
    BMI](#sensitivity-analysis-of-retinol-and-bmi)
  - [Retinol Regressions Omitting Participant with Retinol Deficiency
    and Apparent Liver
    Fibrosis](#retinol-regressions-omitting-participant-with-retinol-deficiency-and-apparent-liver-fibrosis)
- [Genotype effect on Retinol, RBP4, and
  TTR](#genotype-effect-on-retinol-rbp4-and-ttr)
- [Table S2, S3](#table-s2-s3)
- [Figure 2](#figure-2)

<!-- For PDF -->

# Load Data

## Determine Normality

for Shapiro-Wilk test p-value\<0.05 indicates variables does not have
normal distribution

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20normality-2.png)<!-- -->![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20normality-1.png)<!-- -->

<!-- For PDF -->

## Determine Variable Correlation

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20if%20variables%20correlate%20with%20each%20other-1.png)<!-- -->![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20if%20variables%20correlate%20with%20each%20other-2.png)<!-- -->![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20if%20variables%20correlate%20with%20each%20other-3.png)<!-- -->![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/determine%20if%20variables%20correlate%20with%20each%20other-4.png)<!-- -->

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/variable%20matrix-1.png)<!-- -->

<!-- For PDF -->

# RBP4 Model

``` r
#log transform RBP4 concentrations
vao$LRBP4<-log10(vao$RBP4)

#linear regression with BMI, Sex, Age and HOMAIR
model1=lm(LRBP4~BMI+Sex+Age+LHOMAIR,vao)
vif(model1)
```

    ##      BMI      Sex      Age  LHOMAIR 
    ## 1.597502 1.292650 1.233974 1.259206

``` r
#remove one variable at a time
model2=lm(LRBP4~BMI+Sex+Age,vao) #remove HOMAIR
model3=lm(LRBP4~BMI+Sex+LHOMAIR, vao) #remove Age
model4=lm(LRBP4~BMI+Age+LHOMAIR, vao) #remove Sex
model5=lm(LRBP4~Sex+Age+LHOMAIR, vao) #remove BMI

AIC<-AIC(model1,model2,model3,model4,model5) #summary of AIC
AIC_df<-data.frame(
  Model=c("Model 1: Full Model", "Model 2: No HOMA-IR", "Model 3: No Age",
          "Model 4: No Sex", "Model 5: No BMI"),
  AIC = round(AIC$AIC, 3),
  DF = AIC$df)

aic_table <- AIC_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aic_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/RBP4%20model%20building-1.png)<!-- -->

``` r
##remove BMI (lowest AIC), add interaction terms
model6=lm(LRBP4~Sex+Age+LHOMAIR+Age*Sex+LHOMAIR*Sex,vao)
summary(model6)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + LHOMAIR + Age * Sex + LHOMAIR * 
    ##     Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21429 -0.07825  0.02313  0.06886  0.20592 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)     -0.071668   0.135146  -0.530  0.60058   
    ## SexMale          0.507136   0.207240   2.447  0.02177 * 
    ## Age              0.008636   0.002998   2.880  0.00803 **
    ## LHOMAIR         -0.048056   0.045468  -1.057  0.30065   
    ## SexMale:Age     -0.009698   0.004385  -2.212  0.03636 * 
    ## SexMale:LHOMAIR  0.072444   0.113449   0.639  0.52892   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1123 on 25 degrees of freedom
    ## Multiple R-squared:  0.4636, Adjusted R-squared:  0.3563 
    ## F-statistic: 4.322 on 5 and 25 DF,  p-value: 0.005699

``` r
AIC(model6,model5)
```

<div class="kable-table">

|        |  df |       AIC |
|:-------|----:|----------:|
| model6 |   7 | -40.24879 |
| model5 |   5 | -36.19628 |

</div>

``` r
#remove interaction terms one at a time
model7=lm(LRBP4~Sex+Age+LHOMAIR+LHOMAIR*Age,vao)
model8=lm(LRBP4~Sex+Age+LHOMAIR+Age*Sex,vao)
summary(model7)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + LHOMAIR + LHOMAIR * Age, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.24923 -0.06776  0.01125  0.07561  0.20708 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.183427   0.135373   1.355   0.1871  
    ## SexMale      0.106354   0.047271   2.250   0.0331 *
    ## Age          0.002726   0.002839   0.960   0.3458  
    ## LHOMAIR     -0.156739   0.218925  -0.716   0.4804  
    ## Age:LHOMAIR  0.002452   0.004827   0.508   0.6157  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1248 on 26 degrees of freedom
    ## Multiple R-squared:  0.3113, Adjusted R-squared:  0.2054 
    ## F-statistic: 2.939 on 4 and 26 DF,  p-value: 0.03955

``` r
summary(model8)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + LHOMAIR + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21447 -0.05720  0.02090  0.07151  0.20229 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -0.087232   0.131407  -0.664  0.51264   
    ## SexMale      0.571891   0.178663   3.201  0.00359 **
    ## Age          0.008906   0.002934   3.035  0.00541 **
    ## LHOMAIR     -0.036421   0.041179  -0.884  0.38456   
    ## SexMale:Age -0.010753   0.004015  -2.678  0.01267 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.111 on 26 degrees of freedom
    ## Multiple R-squared:  0.4549, Adjusted R-squared:  0.371 
    ## F-statistic: 5.424 on 4 and 26 DF,  p-value: 0.002596

``` r
AIC(model6,model7,model8) #remove HOMAIR x Age 
```

<div class="kable-table">

|        |  df |       AIC |
|:-------|----:|----------:|
| model6 |   7 | -40.24879 |
| model7 |   6 | -34.50246 |
| model8 |   6 | -41.74725 |

</div>

``` r
AIC_RBP4_iterm<-AIC(model6,model7,model8) #summary of AIC
AIC_df_RBP4_iterm<-data.frame(
  Model=c("Model 6: Includes interaction terms", "Model 7: Remove Age x Sex", 
          "Model 8: Remove HOMA-IR x Sex"),
  AIC = round(AIC_RBP4_iterm$AIC,3),
  DF = AIC_RBP4_iterm$df)

aic_RBP4_iterm_table <- AIC_df_RBP4_iterm %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aic_RBP4_iterm_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/RBP4%20model%20building-2.png)<!-- -->

``` r
#remove HOMA-IR
model9=lm(LRBP4~Sex+Age+Age*Sex,vao)
summary(model9)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21781 -0.06921  0.01956  0.07634  0.19094 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -0.135948   0.118824  -1.144  0.26262   
    ## SexMale      0.591144   0.176615   3.347  0.00241 **
    ## Age          0.009749   0.002764   3.527  0.00152 **
    ## SexMale:Age -0.011126   0.003977  -2.798  0.00938 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1106 on 27 degrees of freedom
    ## Multiple R-squared:  0.4385, Adjusted R-squared:  0.3761 
    ## F-statistic: 7.027 on 3 and 27 DF,  p-value: 0.001216

``` r
AIC(model8,model9) # removing HOMA-IR leads to lower AIC
```

<div class="kable-table">

|        |  df |       AIC |
|:-------|----:|----------:|
| model8 |   6 | -41.74725 |
| model9 |   5 | -42.82834 |

</div>

``` r
#does adding BMI back into the model improve the model?
model10=lm(LRBP4~Sex+Age+Age*Sex+BMI,vao)
summary(model10)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + Age * Sex + BMI, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.195947 -0.053725  0.009708  0.071413  0.190375 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -0.007224   0.187983  -0.038  0.96964   
    ## SexMale      0.557203   0.181412   3.071  0.00494 **
    ## Age          0.008911   0.002932   3.040  0.00534 **
    ## BMI         -0.002252   0.002542  -0.886  0.38370   
    ## SexMale:Age -0.010792   0.004011  -2.691  0.01229 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.111 on 26 degrees of freedom
    ## Multiple R-squared:  0.4549, Adjusted R-squared:  0.3711 
    ## F-statistic: 5.425 on 4 and 26 DF,  p-value: 0.002593

``` r
AIC(model9,model10)
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| model9  |   5 | -42.82834 |
| model10 |   6 | -41.75058 |

</div>

``` r
anova(model9,model10) #no, both from ANOVA and AIC, BMI does not improve the model
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |         F |   Pr(\>F) |
|-------:|----------:|----:|----------:|----------:|----------:|
|     27 | 0.3302089 |  NA |        NA |        NA |        NA |
|     26 | 0.3205299 |   1 |  0.009679 | 0.7851149 | 0.3837037 |

</div>

``` r
#does the interaction term improve the model?
model11=lm(LRBP4~Sex+Age,vao)
AIC(model9,model11)
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| model9  |   5 | -42.82834 |
| model11 |   4 | -36.93776 |

</div>

``` r
anova(model9,model11) #yes
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |       F |   Pr(\>F) |
|-------:|----------:|----:|-----------:|--------:|----------:|
|     27 | 0.3302089 |  NA |         NA |      NA |        NA |
|     28 | 0.4259237 |  -1 | -0.0957148 | 7.82626 | 0.0093797 |

</div>

``` r
#is this the simplest model?
model12=lm(LRBP4~Sex,vao) #Sex alone
model13=lm(LRBP4~Age,vao) #Age alone

#summary of AIC
 AICR_3 <- AIC(model9,model12,model13)
 AICR_3df <- data.frame(
   Model = c("Age + Sex + Age x Sex", "Age", "Sex"),
   AIC = round(AICR_3$AIC, 3), 
   DF = AICR_3$df)
 AICR_3df %>%  flextable() %>%
   set_header_labels(Model = "Model Description", AIC = "AIC Value",
                     DF = "Degrees of Freedom") %>%
   add_header_lines(values = "AIC Comparison of Models for RBP4") %>%
   align(part = "header", align = "center") %>%
   add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
fontsize(part = "footer", size = 8) %>%
set_table_properties(layout = "autofit")
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/RBP4%20model%20building-3.png)<!-- -->

``` r
#summary of anova results
 p_values<-data.frame(
   Comparison=c("Age+Sex+AgexSex v Age", "Age+Sex+AgexSex v Sex"),
   P_value=c(round(anova(model9,model12)$"Pr(>F)"[2],3),
             round(anova(model9,model13)$"Pr(>F)"[2],3)))
 
 p_values %>% flextable() %>%
     set_header_labels(Comparison="Models", P_value="P-value: Probability >F") %>%
     add_header_lines(values="Is this the simplest model that fits the data best?") %>%
     align(part="header", align="center" ) %>%
     set_table_properties(layout = "autofit") %>%
     add_footer_lines(values = "Note: p<0.05 cut-off for retaining more complex model") %>%
     fontsize(part = "footer", size = 8) 
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/RBP4%20model%20building-4.png)<!-- -->

``` r
#lower AIC, and anova p<0.05 for the more complex model of Age + Sex + Age x Sex
##final model##
summary(model9)
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ Sex + Age + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21781 -0.06921  0.01956  0.07634  0.19094 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -0.135948   0.118824  -1.144  0.26262   
    ## SexMale      0.591144   0.176615   3.347  0.00241 **
    ## Age          0.009749   0.002764   3.527  0.00152 **
    ## SexMale:Age -0.011126   0.003977  -2.798  0.00938 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1106 on 27 degrees of freedom
    ## Multiple R-squared:  0.4385, Adjusted R-squared:  0.3761 
    ## F-statistic: 7.027 on 3 and 27 DF,  p-value: 0.001216

``` r
#variance inflation factors 
vif(model9,type=c("predictor")) #because this model has interaction terms we need to use the GVIF
```

<div class="kable-table">

|     | GVIF |  Df | GVIF^(1/(2\*Df)) | Interacts With | Other Predictors |
|:----|-----:|----:|-----------------:|:---------------|:-----------------|
| Sex |    1 |   3 |                1 | Age            | –                |
| Age |    1 |   3 |                1 | Sex            | –                |

</div>

## RBP4 Final Model

> RBP4 Final Model: RBP4 ~ Age + Sex + Age x Sex (p=0.009)

## RBP4 Model Plots

<img src="R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/plots for RBP4-1.png" width="50%" /><img src="R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/plots for RBP4-2.png" width="50%" />

<!-- For PDF -->

## Post-hoc analysis of RBP4 and eGFR

``` r
summary(lm(LRBP4~eGFR,vao))
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ eGFR, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.277030 -0.051103  0.009991  0.070393  0.232258 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  0.673248   0.222715   3.023  0.00519 **
    ## eGFR        -0.003267   0.002029  -1.611  0.11810   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1364 on 29 degrees of freedom
    ## Multiple R-squared:  0.0821, Adjusted R-squared:  0.05045 
    ## F-statistic: 2.594 on 1 and 29 DF,  p-value: 0.1181

``` r
summary(lm(LRBP4~eGFR*Sex,vao))
```

    ## 
    ## Call:
    ## lm(formula = LRBP4 ~ eGFR * Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.20730 -0.06922  0.01266  0.07022  0.23966 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.832255   0.224461   3.708 0.000954 ***
    ## eGFR         -0.005153   0.002057  -2.505 0.018573 *  
    ## SexMale      -0.546204   0.470518  -1.161 0.255861    
    ## eGFR:SexMale  0.006133   0.004256   1.441 0.161035    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1206 on 27 degrees of freedom
    ## Multiple R-squared:  0.3319, Adjusted R-squared:  0.2576 
    ## F-statistic: 4.471 on 3 and 27 DF,  p-value: 0.0113

> eGFR is not a significant correlate for RBP4 (p=0.12) eGFR as an
> interaction term with sex is also not significant (p=0.16)

<!-- For PDF -->

# TTR Model

``` r
vao$LTTR<-log10(vao$TTR)

#linear regression with BMI, Sex, Age and HOMA-IR
Tmodel1=lm(LTTR~BMI+Sex+Age+LHOMAIR,vao)
summary(Tmodel1)
```

    ## 
    ## Call:
    ## lm(formula = LTTR ~ BMI + Sex + Age + LHOMAIR, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.221583 -0.037441  0.002198  0.057781  0.207524 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.3295758  0.1437198   9.251 1.04e-09 ***
    ## BMI         -0.0021577  0.0023340  -0.924   0.3637    
    ## SexMale      0.0955022  0.0418519   2.282   0.0309 *  
    ## Age          0.0007957  0.0019475   0.409   0.6862    
    ## LHOMAIR     -0.0137659  0.0377648  -0.365   0.7184    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09806 on 26 degrees of freedom
    ## Multiple R-squared:  0.3276, Adjusted R-squared:  0.2242 
    ## F-statistic: 3.167 on 4 and 26 DF,  p-value: 0.0302

``` r
vif(Tmodel1)
```

    ##      BMI      Sex      Age  LHOMAIR 
    ## 1.597502 1.292650 1.233974 1.259206

``` r
#remove one variable at a time
Tmodel2=lm(LTTR~BMI+Sex+Age,vao) #remove HOMA-IR
Tmodel3=lm(LTTR~BMI+Sex+LHOMAIR, vao) #remove Age
Tmodel4=lm(LTTR~BMI+Age+LHOMAIR, vao) #remove Sex
Tmodel5=lm(LTTR~Sex+Age+LHOMAIR, vao) #remove BMI

AIC(Tmodel1,Tmodel2,Tmodel3,Tmodel4,Tmodel5) #summary of AIC
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| Tmodel1 |   6 | -49.45143 |
| Tmodel2 |   5 | -51.29340 |
| Tmodel3 |   5 | -51.25302 |
| Tmodel4 |   5 | -45.79239 |
| Tmodel5 |   5 | -50.44884 |

</div>

``` r
AICT<-AIC(Tmodel1,Tmodel2,Tmodel3,Tmodel4,Tmodel5) #summary of AIC
AICT_df<-data.frame(
  Model=c("Model 1: Full Model", "Model 2: No HOMA-IR", "Model 3: No Age", 
          "Model 4: No Sex", "Model 5: No BMI"),
  AIC = round(AICT$AIC,3), DF = AICT$df)

aicT_table <- AICT_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for TTR") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aicT_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR%20model%20building-1.png)<!-- -->

``` r
##remove HOMA-IR (lowest AIC)
#add interaction terms for Age and Sex which are not correlated
Tmodel6=lm(LTTR~Sex+Age+BMI+Age*Sex,vao)
summary(Tmodel6)
```

    ## 
    ## Call:
    ## lm(formula = LTTR ~ Sex + Age + BMI + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.202202 -0.031876 -0.006347  0.048701  0.199926 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.150995   0.151538   7.595 4.61e-08 ***
    ## SexMale      0.421700   0.146241   2.884  0.00779 ** 
    ## Age          0.004725   0.002363   1.999  0.05613 .  
    ## BMI         -0.001957   0.002049  -0.955  0.34846    
    ## SexMale:Age -0.007491   0.003233  -2.317  0.02864 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08951 on 26 degrees of freedom
    ## Multiple R-squared:  0.4398, Adjusted R-squared:  0.3537 
    ## F-statistic: 5.104 on 4 and 26 DF,  p-value: 0.003592

``` r
AIC(Tmodel6,Tmodel2)
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| Tmodel6 |   6 | -55.11268 |
| Tmodel2 |   5 | -51.29340 |

</div>

``` r
anova(Tmodel6,Tmodel2)
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|----------:|---------:|----------:|
|     26 | 0.2082919 |  NA |        NA |       NA |        NA |
|     27 | 0.2513029 |  -1 | -0.043011 | 5.368841 | 0.0286386 |

</div>

``` r
##inclusion of interaction term improves model
###Model now TTR~Sex+BMI+Age+Age*Sex

#remove terms
Tmodel7=lm(LTTR~Sex+Age+Age*Sex,vao) # remove BMI
Tmodel8=lm(LTTR~Sex+BMI,vao) # remove Age
Tmodel9=lm(LTTR~Age+BMI,vao) #remove Sex

AICT_iterm<-AIC(Tmodel6,Tmodel7,Tmodel8,Tmodel9) #summary of AIC
AICT_iterm_df<-data.frame(
  Model=c("Full Model: Sex+Age+BMI+AgexSex",
          "Remove BMI: Sex+Age+AgexSex",
          "Remove Age: Sex+BMI",
          "Remove Sex: Age+BMI"),
  AIC = round(AICT_iterm$AIC,3), DF = AICT_iterm$df)

aicT_iterm_table <- AICT_iterm_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for TTR") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aicT_iterm_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR%20model%20building-2.png)<!-- -->

``` r
#lowest AIC removing BMI
#confirm with anova
anova(Tmodel7,Tmodel8)
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|----------:|---------:|----------:|
|     27 | 0.2155956 |  NA |        NA |       NA |        NA |
|     28 | 0.2538616 |  -1 | -0.038266 | 4.792217 | 0.0374116 |

</div>

``` r
anova(Tmodel6,Tmodel7) # removing BMI leads to lower AIC, stat. sig.
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|-----------:|---------:|----------:|
|     26 | 0.2082919 |  NA |         NA |       NA |        NA |
|     27 | 0.2155956 |  -1 | -0.0073038 | 0.911693 | 0.3484596 |

</div>

``` r
###Model now TTR ~ Sex + Age + Age * Sex

#does adding HOMA-IR back into the model improve the model?
Tmodel10=lm(LTTR~Sex+Age+Age*Sex+LHOMAIR,vao)
summary(Tmodel10)
```

    ## 
    ## Call:
    ## lm(formula = LTTR ~ Sex + Age + Age * Sex + LHOMAIR, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.219463 -0.034099 -0.000104  0.048686  0.205280 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.060046   0.107323   9.877 2.74e-10 ***
    ## SexMale      0.442935   0.145917   3.036   0.0054 ** 
    ## Age          0.005092   0.002397   2.124   0.0433 *  
    ## LHOMAIR     -0.015603   0.033632  -0.464   0.6465    
    ## SexMale:Age -0.007621   0.003279  -2.324   0.0282 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09069 on 26 degrees of freedom
    ## Multiple R-squared:  0.425,  Adjusted R-squared:  0.3365 
    ## F-statistic: 4.803 on 4 and 26 DF,  p-value: 0.004906

``` r
AIC(Tmodel7, Tmodel10)
```

<div class="kable-table">

|          |  df |       AIC |
|:---------|----:|----------:|
| Tmodel7  |   5 | -56.04428 |
| Tmodel10 |   6 | -54.29986 |

</div>

``` r
anova(Tmodel7, Tmodel10) #no, both from ANOVA and AIC, HOMA does not improve the model
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |         F |   Pr(\>F) |
|-------:|----------:|----:|----------:|----------:|----------:|
|     27 | 0.2155956 |  NA |        NA |        NA |        NA |
|     26 | 0.2138255 |   1 | 0.0017702 | 0.2152428 | 0.6465498 |

</div>

``` r
#does the interaction term improve the model?
Tmodel11=lm(LTTR~Sex+Age,vao)
AIC(Tmodel7,Tmodel11)
```

<div class="kable-table">

|          |  df |       AIC |
|:---------|----:|----------:|
| Tmodel7  |   5 | -56.04428 |
| Tmodel11 |   4 | -51.95223 |

</div>

``` r
anova(Tmodel7,Tmodel11) #yes
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |        F |  Pr(\>F) |
|-------:|----------:|----:|-----------:|---------:|---------:|
|     27 | 0.2155956 |  NA |         NA |       NA |       NA |
|     28 | 0.2624138 |  -1 | -0.0468181 | 5.863243 | 0.022454 |

</div>

``` r
#is this the simplest model?
Tmodel12=lm(LTTR~Sex,vao) #Sex alone
Tmodel13=lm(LTTR~Age,vao) #Age alone


AICT2<-AIC(Tmodel7,Tmodel11,Tmodel12,Tmodel13) #summary of AIC
AICT2_df<-data.frame(
  Model=c("Sex + Age + Sex x Age",
          "Sex + Age","Sex", "Age"),
  AIC = round(AICT2$AIC,3), DF = AICT2$df)

aicT2_table <- AICT2_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for TTR") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values="Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aicT2_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR%20model%20building-3.png)<!-- -->

``` r
 #summary of anova results
  p_values<-data.frame(
    Comparison=c("Age+Sex+AgexSex v Age+Sex", "Age+Sex+AgexSex v Age", 
                 "Age+Sex+AgexSex v Sex"),
    P_value=c(round(anova(Tmodel7,Tmodel11)$"Pr(>F)"[2],3),
              round(anova(Tmodel7,Tmodel12)$"Pr(>F)"[2],3),
              round(anova(Tmodel7,Tmodel13)$"Pr(>F)"[2],3)))
  
  p_values %>% flextable() %>%
      set_header_labels(Comparison="Models", P_value="P-value: Probability >F") %>%
      add_header_lines(values="Is this the simplest model that fits the data best?") %>%
      align(part="header", align="center" ) %>%
      set_table_properties(layout = "autofit") %>%
      add_footer_lines(values = "Note: p<0.05 cut-off for retaining more complex model") %>%
      fontsize(part = "footer", size = 8) 
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR%20model%20building-4.png)<!-- -->

``` r
  ##final model for TTR
summary(Tmodel7)
```

    ## 
    ## Call:
    ## lm(formula = LTTR ~ Sex + Age + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.219703 -0.039820  0.003317  0.047080  0.200418 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.039175   0.096013  10.823 2.53e-11 ***
    ## SexMale      0.451183   0.142710   3.162  0.00385 ** 
    ## Age          0.005453   0.002233   2.442  0.02145 *  
    ## SexMale:Age -0.007781   0.003213  -2.421  0.02245 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08936 on 27 degrees of freedom
    ## Multiple R-squared:  0.4202, Adjusted R-squared:  0.3558 
    ## F-statistic: 6.522 on 3 and 27 DF,  p-value: 0.00184

``` r
#variance inflation factors 
vif(Tmodel7,type=c("predictor")) #because this model has interaction terms we need to use the GVIF
```

<div class="kable-table">

|     | GVIF |  Df | GVIF^(1/(2\*Df)) | Interacts With | Other Predictors |
|:----|-----:|----:|-----------------:|:---------------|:-----------------|
| Sex |    1 |   3 |                1 | Age            | –                |
| Age |    1 |   3 |                1 | Sex            | –                |

</div>

## TTR Final Model

> TTR Final Model: TTR ~ Age + Sex + Age x Sex (p=0.02)

## TTR Model Plots

<img src="R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR plots-1.png" width="50%" /><img src="R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/TTR plots-2.png" width="50%" />

<!-- For PDF -->

# Retinol Model

``` r
vao$LROL<-log10(vao$Sretinol)

#linear regression with BMI, Sex, Age and HOMAIR
Rmodel1=lm(LROL~BMI+Sex+Age+LHOMAIR,vao)
vif(Rmodel1)
```

    ##      BMI      Sex      Age  LHOMAIR 
    ## 1.597502 1.292650 1.233974 1.259206

``` r
#remove one variable at a time
Rmodel2=lm(LROL~BMI+Sex+Age,vao) #remove HOMA-IR
Rmodel3=lm(LROL~BMI+Sex+LHOMAIR, vao) #remove Age
Rmodel4=lm(LROL~BMI+Age+LHOMAIR, vao) #remove Sex
Rmodel5=lm(LROL~Sex+Age+LHOMAIR, vao) #remove BMI

AIC(Rmodel1,Rmodel2,Rmodel3,Rmodel4,Rmodel5) #summary of AIC
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| Rmodel1 |   6 | -37.11641 |
| Rmodel2 |   5 | -38.95172 |
| Rmodel3 |   5 | -37.12858 |
| Rmodel4 |   5 | -37.23750 |
| Rmodel5 |   5 | -38.03374 |

</div>

``` r
AICR<-AIC(Rmodel1,Rmodel2,Rmodel3,Rmodel4,Rmodel5) #summary of AIC
AICR_df<-data.frame(
  Model=c("Model 1: Full Model", "Model 2: No HOMA-IR", "Model 3: No Age",
          "Model 4: No Sex", "Model 5: No BMI"),
  AIC = AICR$AIC,
  DF = AICR$df)
aicR_table <- AICR_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for Retinol") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aicR_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20model%20builing-1.png)<!-- -->

``` r
##remove HOMA-IR (lowest AIC), add interaction term
Rmodel6=lm(LROL~Age+BMI+Sex+Age*Sex,vao)
summary(Rmodel6)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Age + BMI + Sex + Age * Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.23533 -0.07032  0.00921  0.05459  0.24907 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -0.005764   0.197897  -0.029   0.9770  
    ## Age          0.005830   0.003086   1.889   0.0701 .
    ## BMI         -0.002747   0.002676  -1.026   0.3142  
    ## SexMale      0.281136   0.190980   1.472   0.1530  
    ## Age:SexMale -0.004974   0.004222  -1.178   0.2494  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1169 on 26 degrees of freedom
    ## Multiple R-squared:  0.3144, Adjusted R-squared:  0.2089 
    ## F-statistic:  2.98 on 4 and 26 DF,  p-value: 0.03764

``` r
#can we justify inclusion of interaction terms?
AIC(Rmodel6,Rmodel2)
```

<div class="kable-table">

|         |  df |       AIC |
|:--------|----:|----------:|
| Rmodel6 |   6 | -38.56389 |
| Rmodel2 |   5 | -38.95172 |

</div>

``` r
anova(Rmodel6,Rmodel2) 
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|-----------:|---------:|----------:|
|     26 | 0.3552323 |  NA |         NA |       NA |        NA |
|     27 | 0.3741953 |  -1 | -0.0189629 | 1.387927 | 0.2494263 |

</div>

``` r
#cannot justify interaction terms

#remove each term
Rmodel7=lm(LROL~Age+BMI,vao) #remove Sex
Rmodel8=lm(LROL~Age+Sex,vao) # remove BMI
Rmodel9=lm(LROL~BMI+Sex,vao) # remove Age

p_values<-data.frame(
  Comparison=c("Age+Sex+BMI v Age+BMI", "Age+Sex+BMI v Age+Sex", 
               "Age+Sex+BMI v BMI+Sex"),
  P_value=c(round(anova(Rmodel2,Rmodel7)$"Pr(>F)"[2],3),
            round(anova(Rmodel2,Rmodel8)$"Pr(>F)"[2],3),
            round(anova(Rmodel2,Rmodel9)$"Pr(>F)"[2],3)))
  
  p_values %>% flextable() %>%
    set_header_labels(Comparison="Comparison", P_value="P-value: Probability >F") %>%
    add_header_lines(values="ANOVA Comparison") %>%
    align(part="header", align="center" ) %>%
    set_table_properties(layout = "autofit") %>%
    add_footer_lines(values = "Note: p<0.05 cut-off for retaining more complex model") %>%
    fontsize(part = "footer", size = 8) 
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20model%20builing-2.png)<!-- -->

``` r
{AICR_2<-AIC(Rmodel2,Rmodel7,Rmodel8,Rmodel9) #summary of AIC
AICR_2df<-data.frame(
  Model=c("Age + BMI + Sex","Age + BMI","Age + Sex", 
          "BMI + Sex"),
  AIC = AICR_2$AIC, DF = AICR_2$df)

aicR_2_table <- AICR_2df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for Retinol") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")}
aicR_2_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20model%20builing-3.png)<!-- -->

``` r
#Age and Sex has the lowest AIC
#single regressions
Rmodel10=lm(LROL~Age,vao) 
Rmodel11=lm(LROL~Sex,vao)
AIC(Rmodel11, Rmodel10,Rmodel8)
```

<div class="kable-table">

|          |  df |       AIC |
|:---------|----:|----------:|
| Rmodel11 |   3 | -37.36999 |
| Rmodel10 |   3 | -37.28122 |
| Rmodel8  |   4 | -39.50958 |

</div>

``` r
anova(Rmodel10,Rmodel8) #not better than Age alone
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |       F |   Pr(\>F) |
|-------:|----------:|----:|----------:|--------:|----------:|
|     29 | 0.4493028 |  NA |        NA |      NA |        NA |
|     28 | 0.3920143 |   1 | 0.0572885 | 4.09189 | 0.0527329 |

</div>

``` r
anova(Rmodel11,Rmodel8) #not better than Sex alone
```

<div class="kable-table">

| Res.Df |       RSS |  Df | Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|----------:|---------:|----------:|
|     29 | 0.4480181 |  NA |        NA |       NA |        NA |
|     28 | 0.3920143 |   1 | 0.0560038 | 4.000128 | 0.0552816 |

</div>

``` r
summary(Rmodel10)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Age, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.31522 -0.04495  0.01325  0.06391  0.26545 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -0.040082   0.097843  -0.410   0.6851  
    ## Age          0.004689   0.002225   2.107   0.0438 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1245 on 29 degrees of freedom
    ## Multiple R-squared:  0.1328, Adjusted R-squared:  0.1029 
    ## F-statistic: 4.441 on 1 and 29 DF,  p-value: 0.04385

``` r
summary(Rmodel11)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Sex, data = vao)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32125 -0.04191  0.00414  0.05189  0.23507 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.12539    0.02779   4.512 9.83e-05 ***
    ## SexMale      0.09938    0.04666   2.130   0.0418 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1243 on 29 degrees of freedom
    ## Multiple R-squared:  0.1353, Adjusted R-squared:  0.1055 
    ## F-statistic: 4.537 on 1 and 29 DF,  p-value: 0.04178

``` r
#what about BMI alone
Rmodel12=lm(LROL~BMI,vao)
summary(Rmodel12)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ BMI, data = vao)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.254853 -0.039187  0.007799  0.053518  0.304417 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.382573   0.089540   4.273  0.00019 ***
    ## BMI         -0.005809   0.002274  -2.555  0.01615 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1208 on 29 degrees of freedom
    ## Multiple R-squared:  0.1837, Adjusted R-squared:  0.1555 
    ## F-statistic: 6.526 on 1 and 29 DF,  p-value: 0.01615

``` r
#best p-value for BMI alone

{AICR_3<-AIC(Rmodel7,Rmodel8,Rmodel9,Rmodel10,Rmodel11,Rmodel12) #summary of AIC
AICR_3df<-data.frame(
  Model=c("Age + BMI","Age + Sex", 
          "BMI + Sex", "Age", "Sex", "BMI"),
  AIC = AICR_3$AIC, DF = AICR_3$df)

aicR_3_table <- AICR_3df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for Retinol") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")}
aicR_3_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20model%20builing-4.png)<!-- -->

``` r
#BMI alone also has lowest AIC
##final model is ROL ~ BMI
```

## Retinol Final Model

> Retinol Final Model: Retinol ~ BMI (p=0.016)

## Retinol Model Plots

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20plots-1.png)<!-- -->

<!-- For PDF -->

## Sensitivity Analysis of Retinol and BMI

``` r
vaono23 <-vao %>% filter(ID!= "51-3523") #omit 23 from data set
Rmodel12no23<-lm(LROL~BMI,vaono23)
summary(Rmodel12no23)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ BMI, data = vaono23)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.258376 -0.049593  0.003565  0.047807  0.293192 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.321821   0.086169   3.735 0.000852 ***
    ## BMI         -0.003968   0.002226  -1.783 0.085468 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1114 on 28 degrees of freedom
    ## Multiple R-squared:  0.1019, Adjusted R-squared:  0.06987 
    ## F-statistic: 3.178 on 1 and 28 DF,  p-value: 0.08547

> Omission of participant with liver fibrosis and overt retinol
> deficiency from the regression model increased the p-value of
> correlation between BMI and retinol to p=0.09

## Retinol Regressions Omitting Participant with Retinol Deficiency and Apparent Liver Fibrosis

``` r
Rno23model1=lm(LROL~BMI+Sex+Age+LHOMAIR,vaono23)
summary(Rno23model1)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ BMI + Sex + Age + LHOMAIR, data = vaono23)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.255391 -0.062874 -0.002688  0.052888  0.230952 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.0639189  0.1612890   0.396    0.695
    ## BMI         -0.0008989  0.0027014  -0.333    0.742
    ## SexMale      0.0677670  0.0466130   1.454    0.158
    ## Age          0.0028660  0.0021708   1.320    0.199
    ## LHOMAIR     -0.0193169  0.0420587  -0.459    0.650
    ## 
    ## Residual standard error: 0.1092 on 25 degrees of freedom
    ## Multiple R-squared:  0.2294, Adjusted R-squared:  0.1061 
    ## F-statistic:  1.86 on 4 and 25 DF,  p-value: 0.1489

``` r
#remove one variable at a time
Rno23model2=lm(LROL~BMI+Sex+Age,vaono23) #remove HOMAIR
Rno23model3=lm(LROL~BMI+Sex+LHOMAIR, vaono23) #remove Age
Rno23model4=lm(LROL~BMI+Age+LHOMAIR, vaono23) #remove Sex
Rno23model5=lm(LROL~Sex+Age+LHOMAIR, vaono23) #remove BMI

AICR<-AIC(Rno23model1, Rno23model2, Rno23model3, Rno23model4,Rno23model5) #summary of AIC
AICR_df<-data.frame(
  Model=c("Model 1: Full Model", "Model 2: No HOMA-IR", "Model 3: No Age", 
          "Model 4: No Sex", "Model 5: No BMI"),
  AIC = AICR$AIC,
  DF = AICR$df)
aicR_table <- AICR_df %>%
  flextable() %>%
  set_header_labels(Model = "Model Description", AIC = "AIC Value",
                    DF="Degrees of Freedom") %>%
  add_header_lines(values = "AIC Comparison of Models for Retinol (Omitting 23)") %>%
  align(part="header", align="center" ) %>%
  add_footer_lines(values = "Note: Lower AIC values indicate a better model.") %>%
  fontsize(part = "footer", size = 8) %>%
  set_table_properties(layout = "autofit")
aicR_table
```

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/retinol%20regression%20omitting%20one%20participant-1.png)<!-- -->

``` r
#remove BMI, add interaction terms
Rno23model6=lm(LROL~Sex+Age+LHOMAIR+Sex*Age,vaono23)
Rno23model7=lm(LROL~Sex+Age+Sex*Age,vaono23) #remove HOMA
Rno23model8=lm(LROL~Sex+Age,vaono23) #remove HOMA and Sex * Age
Rno23model9=lm(LROL~Sex+LHOMAIR,vaono23) #remove Age
Rno23model10=lm(LROL~Age+LHOMAIR,vaono23) #remove Sex

AIC(Rno23model5, Rno23model6,Rno23model7,Rno23model8,Rno23model9,Rno23model10)
```

<div class="kable-table">

|              |  df |       AIC |
|:-------------|----:|----------:|
| Rno23model5  |   5 | -43.08092 |
| Rno23model6  |   6 | -42.14048 |
| Rno23model7  |   5 | -43.85300 |
| Rno23model8  |   4 | -44.68606 |
| Rno23model9  |   4 | -42.70304 |
| Rno23model10 |   4 | -41.45723 |

</div>

``` r
#model 8 has the lowest AIC
#Sex + Age
summary(Rno23model8)
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Sex + Age, data = vaono23)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.26566 -0.06596 -0.01165  0.04795  0.21500 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -0.004218   0.085347  -0.049   0.9609  
    ## SexMale      0.076579   0.040293   1.901   0.0681 .
    ## Age          0.003450   0.001926   1.791   0.0845 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.106 on 27 degrees of freedom
    ## Multiple R-squared:  0.2157, Adjusted R-squared:  0.1576 
    ## F-statistic: 3.713 on 2 and 27 DF,  p-value: 0.03763

``` r
#how does this compare to the predictors alone
Rno23model11=lm(LROL~Sex,vaono23) #Sex alone
Rno23model12=lm(LROL~Age,vaono23) #Age alone
AIC(Rno23model8, Rno23model11, Rno23model12)
```

<div class="kable-table">

|              |  df |       AIC |
|:-------------|----:|----------:|
| Rno23model8  |   4 | -44.68606 |
| Rno23model11 |   3 | -43.31838 |
| Rno23model12 |   3 | -42.91924 |

</div>

``` r
anova(Rno23model8,Rno23model11) #not better than Sex alone
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|-----------:|---------:|----------:|
|     27 | 0.3033459 |  NA |         NA |       NA |        NA |
|     28 | 0.3393832 |  -1 | -0.0360373 | 3.207581 | 0.0845163 |

</div>

``` r
anova(Rno23model8,Rno23model12) #not better than Age alone
```

<div class="kable-table">

| Res.Df |       RSS |  Df |  Sum of Sq |        F |   Pr(\>F) |
|-------:|----------:|----:|-----------:|---------:|----------:|
|     27 | 0.3033459 |  NA |         NA |       NA |        NA |
|     28 | 0.3439287 |  -1 | -0.0405828 | 3.612165 | 0.0680846 |

</div>

``` r
summary(Rno23model11) #p>0.05
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Sex, data = vaono23)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.287994 -0.050487  0.001027  0.044673  0.235073 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.14230    0.02526   5.634 4.92e-06 ***
    ## SexMale      0.08247    0.04171   1.977   0.0579 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1101 on 28 degrees of freedom
    ## Multiple R-squared:  0.1225, Adjusted R-squared:  0.09117 
    ## F-statistic: 3.909 on 1 and 28 DF,  p-value: 0.05794

``` r
summary(Rno23model12) #p>0.05
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ Age, data = vaono23)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.291619 -0.052172  0.005761  0.063472  0.261440 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 0.010978   0.088847   0.124   0.9025  
    ## Age         0.003749   0.002007   1.868   0.0723 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1108 on 28 degrees of freedom
    ## Multiple R-squared:  0.1108, Adjusted R-squared:  0.079 
    ## F-statistic: 3.488 on 1 and 28 DF,  p-value: 0.07233

``` r
summary(lm(LROL~BMI,vaono23)) 
```

    ## 
    ## Call:
    ## lm(formula = LROL ~ BMI, data = vaono23)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.258376 -0.049593  0.003565  0.047807  0.293192 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.321821   0.086169   3.735 0.000852 ***
    ## BMI         -0.003968   0.002226  -1.783 0.085468 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1114 on 28 degrees of freedom
    ## Multiple R-squared:  0.1019, Adjusted R-squared:  0.06987 
    ## F-statistic: 3.178 on 1 and 28 DF,  p-value: 0.08547

> No significant correlates for retinol after omission participant with
> gross morphology of liver fibrosis and overt retinol deficiency

<!-- For PDF -->

# Genotype effect on Retinol, RBP4, and TTR

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/anova%20for%20genotypes-1.png)<!-- -->

# Table S2, S3

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/table%20s2%20and%20s3-1.png)<!-- -->![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/table%20s2%20and%20s3-2.png)<!-- -->

<!-- For PDF -->

# Figure 2

![](R_Markdown_for_RBP4_Paper_updatedMay_files/figure-gfm/figure%202-1.png)<!-- -->
