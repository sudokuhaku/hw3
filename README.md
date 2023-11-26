# hw3
## Overview
hw3 is a package that can help to calculate a linear regression estimates, standard error (se), t-vlaues, and p-vlaues that allows for quick regression calculation. 
Model: Y = β0 + β1x + ε and follows linear regression assumptions: linearity, independence of errors, normality of errors, and equal variances. The model follows Ordinary Least Squares regression (OLS) to solve for the estimates.
There is no need to call summary or additional function as it will immediately display the outputs in a dataframe. The model allows for intercept or no intercept model. 
* ```create_linear``` calculates the linear regression estimates, standard error (se), t-vlaues, and p-vlaues in a nice dataframe format

## Installation
```
# if you have not installed "devtools" package, install the package
install.packages("devtools")
library(devtools)
devtools::install_github("sudokuhaku/hw3")
```

## Usage
For a simple linear regression with continous response and independent variable.
dplyr and mtcars are downloaded with the hw3 package. 
The code below is the default. 

```
#SLR simple linear regression.
#intercept=1 means intercept model.
#Therefore, will display 2 parameters.
create_linear(response=mtcars$mpg,
  covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat),
  df=mtcars,
  intercept=1)

#allows more than one covariates.
#intercept=0 means no intercept.
#Therefore, will display only 3 parameters.


create_linear(response= (mtcars$mpg),
  covariates=data.frame(wt=mtcars$wt, cyl=mtcars$cyl, am=mtcars$am),
  df=mtcars,
  intercept=0)
```
***
Thank you! :)
Resources:
https://online.stat.psu.edu/stat500/lesson/9/9.2/9.2.3

