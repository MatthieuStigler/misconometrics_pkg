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
#' @param method whether to use the standard update() funciton, or a faster sweep one
#' @param add_vcov for method with sweep, wether to compute also the vcov
#' @param ... unused
#' @return An object of the same type
#' @export
#' @examples
#'   model_lm <- lm(y ~ lag.quarterly.revenue + price.index + income.level + market.potential,
#'                  data=freeny)
#'   res_lm <- reg_aux(object=model_lm, var_main = "lag.quarterly.revenue")




reg_aux <- function(object, ...)
  UseMethod("reg_aux")

#' @export
#' @describeIn reg_aux Default method
#' @importFrom stats update terms
reg_aux.default <- function(object, var_main, var_controls = NULL, ...) {

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
#' @describeIn reg_aux Method for `felm` object
#' @importFrom stats as.formula lm.fit model.matrix
#' @importFrom ISR3 RSWP
reg_aux.lm <- function(object, var_main, var_controls = NULL, method = c("update", "update_lmfit",  "sweep"),
                       add_vcov = FALSE, ...) {

  method <-  match.arg(method)
  if(method == "sweep" & !requireNamespace("ISR3", quietly = TRUE)) {
    stop("Package 'ISR3' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.null(var_controls)) {
    var_all <- attr(terms(object), "term.labels")
    var_controls <-  var_all[!var_all %in% var_main]
  }

  if(method == "update") {
    string_formula <- sprintf("cbind(%s) ~ %s", toString(var_controls), paste(var_main, collapse=" + "))
    res <- stats::update(object, as.formula(string_formula))
    old_class <- class(res)
    class(res) <-  c("reg_aux_lm", "reg_aux", old_class)
  } else  if(method=="update_lmfit") {
    MM <-  model.matrix(object)
    X <-  MM[, c("(Intercept)", var_main)]
    Y <- MM[, var_controls]

    res <- stats::lm.fit(X, Y)
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

    XX <-  crossprod(qr.R(object$qr))
    which_main <- which(colnames(XX) %in% var_main)
    var_regs <- c(1, which_main)
    sweep_lm <- ISR3::SWP(XX, var_regs)
    coef <-  sweep_lm[var_regs, - var_regs]
    res <-  list(coefficients = coef)
    if(add_vcov) {
      N <- object$df.residual + object$rank
      df.residual <- N - length(var_regs)
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
#' @describeIn reg_aux Method for `felm` object
reg_aux.felm <-  function(object, var_main, var_controls = NULL, method = c("update", "sweep"),
                          add_vcov = FALSE, ...) {

  method <-  match.arg(method)
  if(method == "sweep" & !requireNamespace("ISR3", quietly = TRUE)) {
    stop("Package 'ISR3' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.null(var_controls)) {
    var_all <- attr(terms(object), "term.labels")
    var_controls <-  var_all[! var_all %in%  var_main]
  }

  if(method == "update") {
    string_formula <- sprintf("%s ~ %s", paste(var_controls, collapse=" + "), paste(var_main, collapse=" + "))
    res <- update(object, as.formula(string_formula))
    old_class <- class(res)
    class(res) <-  c("reg_aux", old_class)

    } else if(method == "sweep") {
    vc_get_raw <-  function(x, type="iid") vcov(x, type = type) / summary(x)$rse^2
    vc_raw <- vc_get_raw(object)
    which_main <- which(colnames(vc_raw) %in% var_main)
    which_controls <- which(colnames(vc_raw) %in% var_controls)

    sweep_lm <- ISR3::RSWP(vc_raw, which_controls)
    coef <-  sweep_lm[which_main, which_controls]
    res <-  list(coefficients = coef)
    if(add_vcov) {
      # N <- object$df.residual + object$rank
      df.residual <- object$df.residual - length(which_main)
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

#' @export
#' @importMethodsFrom stats summary.mlm
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
                                 `Pr(>|t|)` = 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE))
    return(object)
  } else {
    return(summary(object))
  }


}

#' @export
#' @describeIn reg_aux Vcov method for some of the reg_aux output
#' @importFrom stats vcov
vcov.reg_aux_lm <- function(object, ...) {


  method <-  attr(object, "method")
  if(method %in% c("sweep", "update_lmfit")) {
    object$vcov
  } else {
    NextMethod(object)
  }

}

