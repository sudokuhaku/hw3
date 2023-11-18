#' Create a linear regression
#'
#' @param response our dependent response variable; defaults to mtcars$mpg.
#' @param covariates our independent response variable or variables; defaults to covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat).
#' @param df dataset that our variables are from; defaults to mtcars from dplyr package.
#' @param intercept Intercept (set to 1) or no intercept (set to 0) linear regression.
#'
#' @return the beta estimates, se, t_values, p_values
#' @export
#'
#' @examples
#' create_linear(response=mtcars$mpg, covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat), df=mtcars, intercept=1)


create_linear <- function(response=mtcars$mpg, covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat), df=mtcars, intercept=1) {
  if (length(response) != nrow(covariates)) {
    print(
      "The length of the response do not match with the length of the covariates. Please check the data again for missing data. :( "
    )
  } else{
    #β=[(X^T)X)^-1][(X^T)*Y]
    y <- response

    #if there is no intercept in the model.
    if (intercept == 0) {
      x <- cbind(as.matrix(covariates))

      #transpose(x)*(x) and use solve() to get the inverse
      x_t_x <- t(x) %*% x
      inv_x_t_x <- solve(x_t_x)
      x_t_y <- t(x) %*% y
      #get the estimates
      beta <- inv_x_t_x %*% x_t_y

      #find the residuals to find the se, t-value, p-value
      y_hat <- x %*% beta
      #difference between the obs y - estimated y
      resid <- y - y_hat

      #find the sum of residuals (RSS), SE, t-value, p-value
      degreesfreedom <- length(resid) - length(beta)
      rss <- sum(resid ^ 2)
      #variance
      var <- diag(solve(t(x) %*% x) * rss / degreesfreedom)
      #get SE by taking square root of variance
      se <- sqrt(var)
      #t-value
      t_v <- beta / se
      # 2-tailed t-test to get the p-value
      p_values <- (1 - pt(abs(t_v), degreesfreedom)) * 2

      #no intercept
      cov_names <- names(covariates)
      coef_names <- c(cov_names)

    } else{
      #if there is intercept in the model.
      x <- cbind(1, as.matrix(covariates))

      #transpose(x)*(x) and use solve() to get the inverse
      x_t_x <- t(x) %*% x
      inv_x_t_x <- solve(x_t_x)
      x_t_y <- t(x) %*% y
      #get the estimates
      beta <- inv_x_t_x %*% x_t_y

      #find the residuals to find the se, t-value, p-value
      y_hat <- x %*% beta
      #difference between the obs y - estimated y
      resid <- y - y_hat

      #find the sum of residuals (RSS), SE, t-value, p-value
      degreesfreedom <- length(resid) - length(beta)
      rss <- sum(resid ^ 2)
      #variance
      var <- diag(solve(t(x) %*% x) * rss / degreesfreedom)
      #get SE by taking square root of variance
      se <- sqrt(var)
      #t-value
      t_v <- beta / se
      # 2-tailed t-test to get the p-value
      p_values <- (1 - pt(abs(t_v), degreesfreedom)) * 2

      cov_names <- names(covariates)
      #This regression estimates has the intercept!
      coef_names <- c("intercept", cov_names)
    }

    #results stored in a dataframe
    #remove the row names using row.names=NULL
    #better to use data.frame than cbind to avoid row mismatch :)
    results <-
      data.frame(
        coefficient = coef_names,
        estimates = beta,
        se,
        t_values = t_v,
        p_values,
        row.names = NULL
      )
    print(results)

  }
}




