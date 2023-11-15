create_linear <- function(response, covariates, df, intercept) {
  if (length(response) != nrow(covariates)) {
    print(
      "The length of the response do not match with the length of the covariates. Please check the data again for missing data."
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
      beta <- inv_x_t_x %*% x_t_y

      #find the residuals
      y_hat <- x %*% beta
      resid <- y - y_hat

      #find the sum of residuals (RSS), SE, t-value, p-value
      degreesfreedom <- length(resid) - length(beta)
      rss <- sum(resid ^ 2)
      var <- diag(solve(t(x) %*% x) * rss / degreesfreedom)
      se <- sqrt(var)
      t_v <- beta / se

      # 2-tailed t-test to get the p-value
      p_values <- (1 - pt(abs(t_v), degreesfreedom)) * 2
      cov_names <- names(covariates)
      coef_names <- c(cov_names)

    } else{
      #if there is intercept in the model.
      x <- cbind(1, as.matrix(covariates))

      #transpose(x)*(x) and use solve() to get the inverse
      x_t_x <- t(x) %*% x
      inv_x_t_x <- solve(x_t_x)
      x_t_y <- t(x) %*% y
      beta <- inv_x_t_x %*% x_t_y

      #find the residuals
      y_hat <- x %*% beta
      resid <- y - y_hat

      #find the sum of residuals (RSS), SE, t-value, p-value
      degreesfreedom <- length(resid) - length(beta)
      rss <- sum(resid ^ 2)
      var <- diag(solve(t(x) %*% x) * rss / degreesfreedom)
      se <- sqrt(var)
      t_v <- beta / se
      # 2-tailed t-test to get the p-value
      p_values <- (1 - pt(abs(t_v), degreesfreedom)) * 2

      cov_names <- names(covariates)
      coef_names <- c("intercept", cov_names)
    }

    #results
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
