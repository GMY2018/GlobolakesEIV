
![alt text](GlobolakesLogo1.png) 
![alt text](ConsortiumLogos.png) 


## **App Details**


This app applied different forms of errors-in-variables (EIV) regression; namely;

* Deming Regression
* Weighted Deming Regression
* Modified Deming Regression
* Weighted Modified

Each of these approaches allows for imprecision in both the reference and test method measurements by choosing the line that minimises the sum of squared differences from the observations to the line that are in a direction at right angles to the line. 

Deming regression incorporates information using an error variability ratio, \(zlambda\) If \(lambda=0\) ordinary least squares regression is obtained and when \(\lambda=1\) this is equivalent to orthogonal regression.  In practice \(\lambda\) controls the angle of distances whose sum of squares is minimised. By allowing \(\lambda\) to vary unequal error variances can be accounted for and least squares can be used to minimise the residual error.

While standard Deming regression assumes a homoscedastic error structure, weighted Deming regression introduces a set of weights in the sum of squared deviations and cross products.  Standard errors can be obtained for the slope estimate computed using Deming regression, standard errors for weighted Deming regression can be obtained using jacknife or bootstrap procedures. Confidence limits for the estimates computed within this app are calculated from 1000 bootstrap samples.  

If the error variances associated with Variable 1 and Variable 2, v1 and v2 respectively, are assumed to be constant then their ratio is also constant. This ratio can be expressed as \(\lambda\) =  v1/v2.

For Deming Regression and Weighted Deming Regression there are 3 options for specifing the error variance ratio  \(\lambda\).  

* **\(\lambda\)=1** This is the default. It assumes that imprecsision is present and equal on both variables.  
* **Estimated \(\lambda\)**; To esimtated \(\lambda\) standard regression models are fitted to the data in both directions; treating the Variable 1 as the explanatory and the Variable 2 as the response and vice versa. Subsequently, the variance of the residuals of each of these models is calculated and the ratio of these variances used as \(\lambda\). While this does not directly estimate error variance, it estimates the quantity of variability of the errors associated with one variable after removing the effect of the other.
* **User Specified \(\lambda\)**; Prior knowledge or values caculated from replicate data can be input here. 

For Modified Deming Regression and Weighted Modified Deming Regression a group ID column containing different values of lambda.  Assuming the errors have arisen from K different error populations, there will be K different error variances associated with the variables.

Denoting these different error variance ratios \(\lambda_1\),... \(\lambda_k\) then the column specified should contain the \(\lambda_k\), one corresponding to each observation has to be specified

## **App Details**

* **Log Transform?** Applies a natural log transformation to both variables.  In this context the log transformation is designed to account for non-constant variance in the data. For this reason, it is not recommended that the log transform be applied to data when weighted regression is used as here the weighted are designed to account for non-constant variance. 

* **Show residual vs fit plot** Shows the plot of resiudals versus fitted values corresponding to the EIV regression.  This plot can be used as a diagnostic for non-constant variance.  Ideally, the residuals should be randomly and evenly distributed above and below the zero line, with no evidence of heteroscedasticity. 

* **Show x=y reference line (grey)** Displays line of equality.

* **Show uncertainty associated with EIV regression estimate** Displays an estimated confidence band around the fitted regresion line.

* **Show standard regression line assuming no error on Variable 1 (orange)** Displays standard linear regression line (estimated by ordinarly least least squares). 