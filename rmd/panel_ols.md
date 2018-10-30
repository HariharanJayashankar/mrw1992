Extending MRW
================

Set the working directory before we start anything (wd = 1992 root folder)

Importing the csv we made in python

``` r
library(dplyr)
library(plm)
library(lmtest)
setwd("C:\\Users\\Administrator\\Documents\\personal\\mrw1992")
df = read.csv('data\\panel_mrw.csv')
```

Regressions
===========

The first model is the base model done in MRW. This is without the human capital variable added.

We do everything in a panel data style. The main equation we estimate is:

![\\log(Y\_{it}/L\_{it})= a\_0 + a\_i + gt + \\frac{\\alpha}{(1 - \\alpha)}log(s\_{it}) - \\frac{\\alpha}{(1 + \\alpha)}log(n\_{it} + g\_{it} + \\delta\_{it}) + \\epsilon\_{it}](https://latex.codecogs.com/png.latex?%5Clog%28Y_%7Bit%7D%2FL_%7Bit%7D%29%3D%20a_0%20%2B%20a_i%20%2B%20gt%20%2B%20%5Cfrac%7B%5Calpha%7D%7B%281%20-%20%5Calpha%29%7Dlog%28s_%7Bit%7D%29%20-%20%5Cfrac%7B%5Calpha%7D%7B%281%20%2B%20%5Calpha%29%7Dlog%28n_%7Bit%7D%20%2B%20g_%7Bit%7D%20%2B%20%5Cdelta_%7Bit%7D%29%20%2B%20%5Cepsilon_%7Bit%7D "\log(Y_{it}/L_{it})= a_0 + a_i + gt + \frac{\alpha}{(1 - \alpha)}log(s_{it}) - \frac{\alpha}{(1 + \alpha)}log(n_{it} + g_{it} + \delta_{it}) + \epsilon_{it}")

Here ![\\alpha\_i](https://latex.codecogs.com/png.latex?%5Calpha_i "\alpha_i") is our country fixed effect.

![\\alpha\_i](https://latex.codecogs.com/png.latex?%5Calpha_i "\alpha_i") is essentially our ![logA(0)](https://latex.codecogs.com/png.latex?logA%280%29 "logA(0)") but this time we assume that it changes across countries. Which makes sense. Institutions, culture, and whatever else we assume our TFP to have is very likely to be country specific.

Remember that in the original paper MRW assumed ![logA(0) = a + \\epsilon](https://latex.codecogs.com/png.latex?logA%280%29%20%3D%20a%20%2B%20%5Cepsilon "logA(0) = a + \epsilon"). We simply extend this bit and decompose ![a](https://latex.codecogs.com/png.latex?a "a") to ![a\_0 + a\_i](https://latex.codecogs.com/png.latex?a_0%20%2B%20a_i "a_0 + a_i"). So our equation for ![logA(0)](https://latex.codecogs.com/png.latex?logA%280%29 "logA(0)") becomes:

![ logA(0) = a\_0 + a\_i + \\epsilon\_{it} ](https://latex.codecogs.com/png.latex?%20logA%280%29%20%3D%20a_0%20%2B%20a_i%20%2B%20%5Cepsilon_%7Bit%7D%20 " logA(0) = a_0 + a_i + \epsilon_{it} ")

Additionally, in a simple cross country regression, the ![gt](https://latex.codecogs.com/png.latex?gt "gt") term didn't matter and it turned into our constant. In a panel setting, it will matter, so we will add that into our regression as well. We will take t as the number of years since the first year we observed a country ![i](https://latex.codecogs.com/png.latex?i "i").

``` r
mdl <- plm(ly ~ gt + ls + lngd, data=df, index=c("country", "year"), model="within")
coeftest(mdl, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##       Estimate Std. Error t value  Pr(>|t|)    
    ## gt    0.144183   0.041387  3.4837 0.0004986 ***
    ## ls    0.441914   0.045646  9.6813 < 2.2e-16 ***
    ## lngd -0.335402   0.063407 -5.2897 1.276e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
implied_alpha <- mdl$coeff[2]/(1 + mdl$coeff[2])
cat("Implied alpha: ", implied_alpha)
```

    ## Implied alpha:  0.3064771

Implied alpha turns out to be 0.3064771. The implied alpha seems to be good. But there are still 2 issues- one is that the R-squared and adjusted R-sqares are too low (~.18) which means these variables can barely explain the variation in output per capita. The other issue is that the absolute coefficients for ![log(savings)](https://latex.codecogs.com/png.latex?log%28savings%29 "log(savings)") and ![log(n + g + d)](https://latex.codecogs.com/png.latex?log%28n%20%2B%20g%20%2B%20d%29 "log(n + g + d)") don't seem to be the same, although the signs are as the Solow model predicts.

![gt](https://latex.codecogs.com/png.latex?gt "gt") seems to be highly significant in this regression.

``` r
mdl_restr = plm(ly ~ gt + (ls_lngd), data=df, index=c("country", "year"), model="within")
coeftest(mdl_restr, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##           Estimate Std. Error t value Pr(>|t|)    
    ## gt      -0.0062015  0.0104248 -0.5949   0.5519    
    ## ls_lngd  0.4166679  0.0450781  9.2432   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
implied_alpha <- mdl_restr$coeff[2]/(1 + mdl_restr$coeff[2])
cat("Implied alpha: ", implied_alpha)
```

    ## Implied alpha:  0.2941182

The implied alpha is still very similar but if we do an F test to test whether our unrestricted and restricted models are similar, we reject the hypothesis that they are the same. Which means the restriction of the magnitude of the coefficients doesn't seem to hold.

``` r
pFtest(mdl, mdl_restr)
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  ly ~ gt + ls + lngd
    ## F = 1.6784, df1 = 340, df2 = 5139, p-value = 7.026e-13
    ## alternative hypothesis: significant effects

Adding Human Capital
====================

Lets see if adding human capital to our regressions changes any results.

The model differences are almost entirely similar to what changes we had made in the base model. Our main equation to estimate here becomes:

![
log\\left(\\frac{Y(t)}{L(t)}\\right) = a\_0 + a\_i + gt - \\left(\\frac{\\alpha + \\beta}{1 - \\alpha - \\beta}\\right)log(n + g + \\delta) + \\left(\\frac{\\alpha}{1 - \\alpha - \\beta}\\right)log(s\_k) + \\left(\\frac{\\beta}{1 - \\alpha - \\beta}\\right)log(s\_h) + \\epsilon\_it
](https://latex.codecogs.com/png.latex?%0Alog%5Cleft%28%5Cfrac%7BY%28t%29%7D%7BL%28t%29%7D%5Cright%29%20%3D%20a_0%20%2B%20a_i%20%2B%20gt%20-%20%5Cleft%28%5Cfrac%7B%5Calpha%20%2B%20%5Cbeta%7D%7B1%20-%20%5Calpha%20-%20%5Cbeta%7D%5Cright%29log%28n%20%2B%20g%20%2B%20%5Cdelta%29%20%2B%20%5Cleft%28%5Cfrac%7B%5Calpha%7D%7B1%20-%20%5Calpha%20-%20%5Cbeta%7D%5Cright%29log%28s_k%29%20%2B%20%5Cleft%28%5Cfrac%7B%5Cbeta%7D%7B1%20-%20%5Calpha%20-%20%5Cbeta%7D%5Cright%29log%28s_h%29%20%2B%20%5Cepsilon_it%0A "
log\left(\frac{Y(t)}{L(t)}\right) = a_0 + a_i + gt - \left(\frac{\alpha + \beta}{1 - \alpha - \beta}\right)log(n + g + \delta) + \left(\frac{\alpha}{1 - \alpha - \beta}\right)log(s_k) + \left(\frac{\beta}{1 - \alpha - \beta}\right)log(s_h) + \epsilon_it
")

``` r
mdl_hc <- plm(ly ~ gt + ls + lngd + lschool, data=df, index=c("country", "year"), model="within")
coeftest(mdl_hc, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##          Estimate Std. Error t value Pr(>|t|)    
    ## gt      -0.030842   0.019142 -1.6112   0.1072    
    ## ls       0.022530   0.018415  1.2235   0.2212    
    ## lngd     0.016265   0.025081  0.6485   0.5167    
    ## lschool  4.129583   0.135291 30.5238   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
implied_alpha <- mdl_hc$coeff[1]/(1 + mdl_hc$coeff[1] + mdl_hc$coeff[3])
cat("Implied alpha: ", implied_alpha)
```

    ## Implied alpha:  -0.03129875

``` r
mdl_hc_restr <- plm(ly ~ gt + ls_lngd + lsch_lngd, data=df, index=c("country", "year"), model="within")
coeftest(mdl_hc_restr, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##           Estimate Std. Error t value  Pr(>|t|)    
    ## gt        0.270454   0.044499  6.0778 1.307e-09 ***
    ## ls_lngd   0.369371   0.040427  9.1368 < 2.2e-16 ***
    ## lsch_lngd 0.611919   0.067798  9.0257 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
implied_alpha <- mdl_hc_restr$coeff[1]/(1 + mdl_hc_restr$coeff[1] + mdl_hc_restr$coeff[2])
cat("Implied alpha: ", implied_alpha)
```

    ## Implied alpha:  0.1649284

Testing the two models

``` r
pFtest(mdl_hc, mdl_hc_restr)
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  ly ~ gt + ls + lngd + lschool
    ## F = 14898, df1 = 1, df2 = 5138, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

Once again, we clearly reject the null that both the models are similar, which means our restriction doesn't hold. Our implied alpha values weren't even close to what we should expect once we added in human capital.

Classic versus Augmented
------------------------

The results from the original MRW quickly turned around once we jumped to a panel setting. A positive outcome here (for the Solow model) was that we got the implied ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\alpha") values quite easily in the base regression itself. Adding human capital only made the equation worse.

Ofcourse, every equation estimated here is filled to the brim with endogeneity issues. But it is very likely that human capital and general country fixed effects are very highly correlated, which kind of explains these results.

But we still have the low R sqaures to deal with in the original regressions. It's not the biggest concern, but it doesn mean that we have a long way to go from Solow to getting to a point where we explain a lot of cross country GDP differences.

Rate of Convergence
===================

Let's go ahead and try and estimate the rate of convergence in a Solow model.

The equation that we will try and estimate is (adapted from Islam, 1995). The equation is a bit different because Islam points out the MRW get their equations by using output per effective labour but in practice just use the usual output per capita. Getting an equation which has output per capita on the LHS is inevitably going to put ![f(A)](https://latex.codecogs.com/png.latex?f%28A%29 "f(A)") on the right hand side, where f is some function who's domain is A:

![
\\log(y\_{it}) = (1 - e^{-\\lambda\*\\tau})\\frac{\\alpha}{1-\\alpha} - (1 - e^){-\\lambda \* \\tau}\\frac{\\alpha}{1 - \\alpha}\\log(n + g + \\delta) + e^{-\\lambda \* \\tau} \\log(y\_{i(t-1)}) + (1 - e^{- \\lambda \\tau})\\log(A(0)) + g(t\_2 - e^{-\\lambda\\tau}t\_1)
](https://latex.codecogs.com/png.latex?%0A%5Clog%28y_%7Bit%7D%29%20%3D%20%281%20-%20e%5E%7B-%5Clambda%2A%5Ctau%7D%29%5Cfrac%7B%5Calpha%7D%7B1-%5Calpha%7D%20-%20%281%20-%20e%5E%29%7B-%5Clambda%20%2A%20%5Ctau%7D%5Cfrac%7B%5Calpha%7D%7B1%20-%20%5Calpha%7D%5Clog%28n%20%2B%20g%20%2B%20%5Cdelta%29%20%2B%20e%5E%7B-%5Clambda%20%2A%20%5Ctau%7D%20%5Clog%28y_%7Bi%28t-1%29%7D%29%20%2B%20%281%20-%20e%5E%7B-%20%5Clambda%20%5Ctau%7D%29%5Clog%28A%280%29%29%20%2B%20g%28t_2%20-%20e%5E%7B-%5Clambda%5Ctau%7Dt_1%29%0A "
\log(y_{it}) = (1 - e^{-\lambda*\tau})\frac{\alpha}{1-\alpha} - (1 - e^){-\lambda * \tau}\frac{\alpha}{1 - \alpha}\log(n + g + \delta) + e^{-\lambda * \tau} \log(y_{i(t-1)}) + (1 - e^{- \lambda \tau})\log(A(0)) + g(t_2 - e^{-\lambda\tau}t_1)
")

Let's run a normal OLS routine to estimate this first and see how the results are.

``` r
mdl_dp <- plm(ly ~ lag(ly, 1) + ls + lngd,
    data=df, index=c("country", "year"), model="within")
coeftest(mdl_dp, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error  t value  Pr(>|t|)    
    ## lag(ly, 1) 0.9946188  0.0014784 672.7785 < 2.2e-16 ***
    ## ls         0.0148226  0.0021767   6.8097 1.092e-11 ***
    ## lngd       0.0400142  0.0025621  15.6180 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
cat( "implied lambda: ",-log(coef(mdl_dp)[1]+ 1))
```

    ## implied lambda:  -0.6904529

Alright, a straightforward panel regression with fixed effects gives us effect that a country i in period t with a growth rate x tends to grow 0.9946188 percent faster in t+1. Not a point in favour of Solow but let's try to at least try and extract out the endogeneity here.

Since this is a classic dynamic panel model, we run into the endogeneity problem if we do a fixed effects regression. The main issue is, once we try to estimate a fixed effects regression, the within difference of ![y\_{it-1}](https://latex.codecogs.com/png.latex?y_%7Bit-1%7D "y_{it-1}") will be correlated to ![\\epsilon\_{it}](https://latex.codecogs.com/png.latex?%5Cepsilon_%7Bit%7D "\epsilon_{it}").

Hence we try an apply the Arrellano-Bond estimator to get a good estimate of the lag of y.

The Arrellano-Bond estimator uses all lagged values of the first difference of y as instruments for the first difference of y.

``` r
mdl_ab <- pgmm(dynformula(ly~ls + lngd, lag = list(1, 0, 0)),
     data = df, index=c("country", "year"),
     effect = "twoways", model = "twostep",
     gmm.inst = ~ly, lag.gmm = list(c(1:15)))
```

    ## Warning in pgmm(dynformula(ly ~ ls + lngd, lag = list(1, 0, 0)), data =
    ## df, : the second-step matrix is singular, a general inverse is used

``` r
summary(mdl_ab, robust = TRUE)
```

    ## Warning in vcovHC.pgmm(object): a general inverse is used

    ## Twoways effects Two steps model
    ## 
    ## Call:
    ## pgmm(formula = dynformula(ly ~ ls + lngd, lag = list(1, 0, 0)), 
    ##     data = df, effect = "twoways", model = "twostep", index = c("country", 
    ##         "year"), gmm.inst = ~ly, lag.gmm = list(c(1:15)))
    ## 
    ## Unbalanced Panel: n = 116, T = 22-63, N = 5721
    ## 
    ## Number of Observations Used: 4786
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -0.4686751 -0.0096214  0.0000000  0.0007452  0.0108884  0.6271665 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z-value  Pr(>|z|)    
    ## lag(ly, 1)  0.7915997  0.1248862  6.3386 2.319e-10 ***
    ## ls         -0.0026122  0.0025167 -1.0380    0.2993    
    ## lngd        0.0362218  0.0042666  8.4896 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Sargan test: chisq(823) = 72.22324 (p-value = 1)
    ## Autocorrelation test (1): normal = -3.603741 (p-value = 0.00031367)
    ## Autocorrelation test (2): normal = -2.396614 (p-value = 0.016547)
    ## Wald test for coefficients: chisq(3) = 78.83623 (p-value = < 2.22e-16)
    ## Wald test for time dummies: chisq(61) = 556.4955 (p-value = < 2.22e-16)

From here, we can go ahead and find out what the implied ![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\lambda") value is.

``` r
implied_lambda <- -log(coef(mdl_ab)[1] + 1)
cat("Implied Lambda: ", implied_lambda)
```

    ## Implied Lambda:  -0.5831089

The implied lambda we get is -0.5831089. The negative rate is definitely peculiar but that it's negative isn't surprising at this point honestly. The coefficient of our estimate for $t\_{it-1} was positive, which means countries which have a high GDP in period t tend to have a 79.1599726 higher GDP the next year. This is not consistent for the Solow model (remember this is inspite of controllng for savings). This is not evidence in favour of conditional convergence.

One thing to note is that we didn't use human capital here. Let's include that and see how well it goes. Once again, following Islam (1995), the equation we will try and estimate is:

![
\\log(y\_{it}) = (1 - e^{-\\lambda\*\\tau})\\frac{\\alpha}{1-\\alpha} - (1 - e^){-\\lambda \* \\tau}\\frac{\\alpha}{1 - \\alpha}\\log(n + g + \\delta) + e^{-\\lambda \* \\tau} \\log(y\_{i(t-1)}) + (1 - e^{-\\lambda\\tau})\\frac{\\phi}{1 - \\alpha}log(h^\\\*) + (1 - e^{- \\lambda \\tau})\\log(A(0)) + g(t\_2 - e^{-\\lambda\\tau}t\_1)
](https://latex.codecogs.com/png.latex?%0A%5Clog%28y_%7Bit%7D%29%20%3D%20%281%20-%20e%5E%7B-%5Clambda%2A%5Ctau%7D%29%5Cfrac%7B%5Calpha%7D%7B1-%5Calpha%7D%20-%20%281%20-%20e%5E%29%7B-%5Clambda%20%2A%20%5Ctau%7D%5Cfrac%7B%5Calpha%7D%7B1%20-%20%5Calpha%7D%5Clog%28n%20%2B%20g%20%2B%20%5Cdelta%29%20%2B%20e%5E%7B-%5Clambda%20%2A%20%5Ctau%7D%20%5Clog%28y_%7Bi%28t-1%29%7D%29%20%2B%20%281%20-%20e%5E%7B-%5Clambda%5Ctau%7D%29%5Cfrac%7B%5Cphi%7D%7B1%20-%20%5Calpha%7Dlog%28h%5E%5C%2A%29%20%2B%20%281%20-%20e%5E%7B-%20%5Clambda%20%5Ctau%7D%29%5Clog%28A%280%29%29%20%2B%20g%28t_2%20-%20e%5E%7B-%5Clambda%5Ctau%7Dt_1%29%0A "
\log(y_{it}) = (1 - e^{-\lambda*\tau})\frac{\alpha}{1-\alpha} - (1 - e^){-\lambda * \tau}\frac{\alpha}{1 - \alpha}\log(n + g + \delta) + e^{-\lambda * \tau} \log(y_{i(t-1)}) + (1 - e^{-\lambda\tau})\frac{\phi}{1 - \alpha}log(h^\*) + (1 - e^{- \lambda \tau})\log(A(0)) + g(t_2 - e^{-\lambda\tau}t_1)
")

Lets do the standard OLS first:

``` r
mdl_dphc <- plm(ly ~ lag(ly, 1) + ls + lngd + lschool,
    data=df, index=c("country", "year"), model="within")
coeftest(mdl_dphc, vcov.=function(x) vcovHC(x, type="sss"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error  t value  Pr(>|t|)    
    ## lag(ly, 1) 0.9854910  0.0037543 262.4985 < 2.2e-16 ***
    ## ls         0.0138784  0.0021965   6.3184 2.869e-10 ***
    ## lngd       0.0399173  0.0025532  15.6341 < 2.2e-16 ***
    ## lschool    0.0472583  0.0174600   2.7067  0.006819 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
-log(coef(mdl_dphc)[1]+ 1)
```

    ## lag(ly, 1) 
    ## -0.6858662

The coefficient of our lagged ![y\_i](https://latex.codecogs.com/png.latex?y_i "y_i") doesnt change much from our previous standard OLS. Lets see if Arellano-Bond will yield any different results

``` r
mdl_abhc <- pgmm(dynformula(ly~ls + lngd + lschool, lag = list(1, 0, 0, 0)),
     data = df, index=c("country", "year"),
     effect = "twoways", model = "twostep",
     gmm.inst = ~ly, lag.gmm = list(c(1:15)))
```

    ## Warning in pgmm(dynformula(ly ~ ls + lngd + lschool, lag = list(1, 0, 0, :
    ## the second-step matrix is singular, a general inverse is used

``` r
summary(mdl_abhc, robust = TRUE)
```

    ## Warning in vcovHC.pgmm(object): a general inverse is used

    ## Twoways effects Two steps model
    ## 
    ## Call:
    ## pgmm(formula = dynformula(ly ~ ls + lngd + lschool, lag = list(1, 
    ##     0, 0, 0)), data = df, effect = "twoways", model = "twostep", 
    ##     index = c("country", "year"), gmm.inst = ~ly, lag.gmm = list(c(1:15)))
    ## 
    ## Unbalanced Panel: n = 116, T = 22-63, N = 5721
    ## 
    ## Number of Observations Used: 4786
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -0.4659340 -0.0092681  0.0000000  0.0006192  0.0108849  0.6252824 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z-value  Pr(>|z|)    
    ## lag(ly, 1)  0.7745781  0.1313189  5.8985 3.669e-09 ***
    ## ls         -0.0028651  0.0026651 -1.0751    0.2824    
    ## lngd        0.0352224  0.0044390  7.9347 2.111e-15 ***
    ## lschool     0.8201699  0.9269695  0.8848    0.3763    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Sargan test: chisq(823) = 71.35621 (p-value = 1)
    ## Autocorrelation test (1): normal = -3.460823 (p-value = 0.00053853)
    ## Autocorrelation test (2): normal = -2.374084 (p-value = 0.017593)
    ## Wald test for coefficients: chisq(4) = 70.15946 (p-value = 2.1005e-14)
    ## Wald test for time dummies: chisq(61) = 521.0322 (p-value = < 2.22e-16)

``` r
implied_lambda <- -log(coef(mdl_abhc)[1] + 1)
cat("Implied Lambda: ", implied_lambda)
```

    ## Implied Lambda:  -0.5735627

Now our results again doesn't change MUCH. The rate of convergence is still negative and we still get the rate that a higher GDP increases future growth rate. But what's interesting here is that adding human capital didn't do anything to our estimates. The coefficient for human capital is significant (at the 5% level) in our standard OLS and adding human capital decreased our estimate for lagged gdp.

Once we came to the Arellano-Bond estimator, human capital really lost it's significance along with our savings rate.

The third fact here is that the arellano bond estimate without human capital didn't seem all that different from our arellano bond estimate with human capital. This means that human capital is significantly correlated with lagged y. (Remember that when going through Arellano-Bond we also use the other control variables as instruments for our endogenous variables, which means we use human capital to help predict the lagged GDP).

Some random notes about the Arellano-Bond estimator which should be noted: 1. It assumes exogeneity of the lag of y\_{it-k} for k&gt;=2 in this case. This is a fairly strong assumption, but it is (ever so slightly) better than doing a straightforward regression without any internal instruments. The results are believable to the extent that we believe $E(y\_{i, t-k}\_{i,t} | X) = 0 for k&gt;=1. 2. The test of overidentifying restrictions is valid (we don't reject the null that the instruments are exogenous). It isn't definitive proof of exogeneity though (the high p value suggests that we may simply not have any statistical power to say anything in the test here.) 3. It's really key to note here that theoretically, the reason why we can start the instruments from just the second lag of ![y\_i](https://latex.codecogs.com/png.latex?y_i "y_i") is that we assume ![E(\\epsilon\_{it} \\epsilon\_{it-1})](https://latex.codecogs.com/png.latex?E%28%5Cepsilon_%7Bit%7D%20%5Cepsilon_%7Bit-1%7D%29 "E(\epsilon_{it} \epsilon_{it-1})") for some time period t. This translates to there being no persistance in shocks. This is unlikely to be true (it's hard to imagine a big shock like a bank run for example, happening in a year not persisting to the next year). But we could modify this assumption, and all it would require us to do is start the instruments from an earlier lag. 4. Point 3 leads nicely into how the Arellano Bond estimate requires that we be in the Goldilox zone of not our lags being strong enough to predict our endogenous variable, but too much correlation runs the risk of the instrument not satisfying the exclusion restriction. This is obviously present in normal IV estimation as well, but in this case it is much more persistent since we only use the lag of the endogenous variable as the instrument. We kind of have to choose a time period which isn't too far away as to not be related to our endogenous variable but not close enough to not satisfy the exclusion restriction.

Additionally, the limit of using instruments only till 15 lags was arbitrary. Using all potential lags was taking too long to compute but ideally one would go through all posibilities and see how the estimate and identification tests change through instruments.

Conclusion
----------

Regressions here have huge issues. But the results here definitely call into question the Solow model's specifics. Conditional convergence doesn't really seem to be a thing.
