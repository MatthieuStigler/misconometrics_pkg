#' Auxiliary regression of all covariates on one (or more) variable of interest
#'
#' Given a regression of y on x and covariates, estimate the correlation between all covariates and the variable x
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param object output from regression
#' @param var_main a character, name of the variable of interest
#' @param var_controls a character, name of the covariates
#' @return An object of the same type
#' @export
#' @examples
#'   model_lm <- lm(y ~ lag.quarterly.revenue + price.index + income.level + market.potential, data=freeny)
#'   res_lm <- reg_aux(object=model_lm, var_main = "lag.quarterly.revenue")




reg_aux <- function(...)
  UseMethod("reg_aux")

#' @export
reg_aux.default <- function(object, var_main, var_controls = NULL) {

  if(is.null(var_controls)) {
    var_all  <- attr(terms(object), "term.labels")
    var_controls <-  var_all[!var_all %in% var_main]
  }

  string_formula <- sprintf("%s ~ %s", var_controls, paste(var_main, collapse=" + "))
  reg_aux_all <- lapply(string_formula, function(x) update(object, as.formula(x)))
  names(reg_aux_all) <- var_controls
  reg_aux_all
}

#' @export
reg_aux.lm <- function(object, var_main, var_controls = NULL, method = c("update", "update_lmfit",  "sweep"),
                       add_vcov = FALSE) {

  method <-  match.arg(method)

  if(is.null(var_controls)) {
    var_all <- attr(terms(object), "term.labels")
    var_controls <-  var_all[var_all!= var_main]
  }

  if(method == "update") {
    string_formula <- sprintf("cbind(%s) ~ %s", toString(var_controls), paste(var_main, collapse=" + "))
    res <- update(object, as.formula(string_formula))
    old_class <- class(res)
    class(res) <-  c("reg_aux_lm", "reg_aux", old_class)
  } else  if(method=="update_lmfit") {
    MM <-  model.matrix(object)
    X <-  MM[, c("(Intercept)", var_main)]
    Y <- MM[, var_controls]

    res <- lm.fit(X, Y)
    class(res) <-  c("reg_aux_lm", "reg_aux")

    if(add_vcov) {
      rss <- crossprod(res$residuals)
      resvar <- rss/res$df.residual
      p1 <- 1L:res$rank
      R <- chol2inv(res$qr$qr[p1, p1, drop = FALSE])
      # Rinv <- diag(rowSums(backsolve(res$qr$qr, diag(res$rank))^2)) not faster, unlike:
      # https://stackoverflow.com/questions/39568978/how-to-calculate-variance-of-least-squares-estimator-using-qr-decomposition-in-r
      VC <-  resvar %x% R
      VC_names <-  paste(rep(var_controls, each = length(var_main)+1),
                         rep(c("(Intercept)", var_main), times = length(var_controls)), sep=":")
      colnames(VC) <- rownames(VC) <- VC_names
      res$vcov <-   VC
    }

  } else {

    require(ISR3)
    XX <-  crossprod(qr.R(object$qr))
    which_main <- which(var_main == colnames(XX))
    var_regs <- c(1, which_main)
    sweep_lm <- SWP(XX, var_regs)
    coef <-  sweep_lm[var_regs, - var_regs]
    res <-  list(coefficients = coef)
    if(add_vcov) {
      N <- object$df.residual + object$rank
      df.residual <- N - 2
      S <- sweep_lm[-var_regs, -var_regs]
      VC <-  (-S/df.residual) %x% sweep_lm[var_regs, var_regs]
      VC_names <-  paste(rep(var_controls, each = length(var_main)+1),
                         rep(c("(Intercept)", var_main), times = length(var_controls)), sep=":")
      colnames(VC) <- rownames(VC) <- VC_names
      res$vcov <- VC
      res$df.residual <-  df.residual
    }
    class(res) <-  c("reg_aux_lm", "reg_aux")
  }
  attr(res, "method") <-  method
  res
}



#' @export
reg_aux.felm <-  function(object, var_main, var_controls = NULL, method = c("update", "sweep"),
                          add_vcov = FALSE) {

  method <-  match.arg(method)

  if(is.null(var_controls)) {
    var_all <- attr(terms(object), "term.labels")
    var_controls <-  var_all[var_all!= var_main]
  }

  if(method == "update") {
    string_formula <- sprintf("%s ~ %s", paste(var_controls, collapse=" + "), paste(var_main, collapse=" + "))
    res <- update(object, as.formula(string_formula))
    old_class <- class(res)
    class(res) <-  c("reg_aux", old_class)
  } else  if(method=="sweep") {
    require(ISR3)
    vc_get_raw <-  function(x, type="iid") vcov(x, type = type) / summary(x)$rse^2
    vc_raw <- vc_get_raw(object)
    which_main <- which(var_main == colnames(vc_raw))
    which_controls <- which(colnames(vc_raw) %in% var_controls)

    sweep_lm <- RSWP(vc_raw, which_controls)
    coef <-  sweep_lm[which_main, which_controls]
    res <-  list(coefficients = coef)
    if(add_vcov) {
      # N <- object$df.residual + object$rank
      df.residual <- object$df.residual - 1
      S <- sweep_lm[-which_main, -which_main, drop = FALSE]
      VC <-  (-S/df.residual) %x% sweep_lm[which_main, which_main, drop = FALSE]
      VC_names <-  paste(rep(var_controls, each = length(var_main)),
                         rep(var_main, times = length(var_controls)), sep=":")
      colnames(VC) <- rownames(VC) <- VC_names
      res$vcov <- VC
      res$df.residual <-  df.residual
      class(res) <-  c("reg_aux")
    }

  }
  attr(res, "method") <-  method
  res
}

summary.reg_aux_lm <- function(object, ...) {


  method <-  attr(object, "method")
  if(method %in% c("sweep", "update_lmfit")) {
    se_all <- sqrt(diag(object$vcov))
    se <-  se_all #[-seq(1, by =2, length.out = length(se_all)/2)]
    est <- c(object$coefficients)
    tval <- est/se
    rdf <-  object$df.residual
    object$coefficients <- cbind(Estimate = est,
                                 `Std. Error` = se,
                                 `t value` = tval,
                                 `Pr(>|t|)` = 2 * pt(abs(tval), rdf, lower.tail = FALSE))
    return(object)
  } else {
    return(stats:::summary.mlm(object))
  }


}

vcov.reg_aux_lm <- function(object, ...) {


  method <-  attr(object, "method")
  if(method %in% c("sweep", "update_lmfit")) {
    object$vcov
  } else {
    NextMethod(object)
  }

}

