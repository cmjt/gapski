#' Extract void point process parameter estimates
#'
#' Extracts estimated and derived parameters from a model fitted using
#' \link{fit.gap}.
#'
#' @param object A fitted model from \link{fit.gap}.
#' @param ... Other parameters (for S3 generic compatibility).
#'
#' @method coef gapski
#'
#' @export
coef.gapski <- function(object, ...){
    parm <- 1:3
    object$pars[parm]
}

#' Extracts void point process parameter confidence intervals.
#'
#' Extracts confidence intervals for estimated and derived parameters
#' from a model fitted using \link{fit.gap}, then bootstrapped using
#' \link{boot.gap}.
#'
#' Calculation of confidence intervals is via a normal approximation,
#' whereby standard errors are calculated from the standard deviations
#' of the parameter estimates across the bootstrap
#' resamples. Bootstrap parameter estimates can be found in the
#' \code{boot} component of the model object, so alternative
#' confidence interval methods can be calculated by hand.
#'
#' @param object A fitted model from \link{fit.gap}, bootstrapped using
#' \link{boot.gap}.
#' @param parm A vector of parameter names, specifying which
#' parameters are to be given confidence intervals.
#' @param level The confidence level required.
#' @param method A character string specifying the method used to
#' calculate confidence intervals. Choices are "normal", for a normal
#' approximation, and "percentile", for the percentile method.
#' @param ... Other parameters (for S3 generic compatability).
#'
#' @method confint boot.gapski
#'
#' @export
confint.boot.gapski <- function(object, parm = c("R", "D", "lambda"), level = 0.95, method = "percentile", ...){
    if (is.null(parm)){
        parm <- 1:length(object$se)
    }
    if (method == "normal"){
        ests <- coef(object)[parm]
        ses <- object$se[parm]
        out <- cbind(ests + qnorm((1 - level)/2)*ses,
                     ests - qnorm((1 - level)/2)*ses)
    } else if (method == "percentile"){
        out <- t(apply(object$boots[, parm, drop = FALSE], 2, quantile, probs = c((1 - level)/2, 1 - (1 - level)/2),
                       na.rm = TRUE))
    }
    colnames(out) <- c(paste(100*(1 - level)/2, "%"),
                       paste(100*((1 + level)/2), "%"))
    out
}

#' Summarising gapski model fits
#'
#' Provides a useful summary of the model fit.
#'
#' @param object A fitted model from \link{fit.gap}.
#' @param ... Other parameters (for S3 generic compatibility).
#' @inheritParams coef.gapski
#' 
#' @method summary gapski
#'
#' @export
summary.gapski <- function(object, ...){
    parm <- 1:3
    coefs <- coef(object)[parm]
    ses <- object$se[parm]
    if (is.null(ses)){
        ses <- rep(NA, length(coefs))
        names(ses) <- names(coefs)[parm]
    }
    out <- cbind(coefs,ses)
    rownames(out) <- names(coefs)
    colnames(out) <- c("coefs" ,"se")
    class(out) <- c("summary.gapski", class(out))
    out
}

#' Plotting the gapski model fit.
#'
#'
#' @param object A fitted model from \link{fit.gap}.
#' @inheritParams coef.gapski
#' @method plot gapski
#'
#' @export

plot.gapski <- function(object=NULL, ...){
    parm <- 1:3
    coefs <- coef(object)[parm]
    xx <- seq(0,0.5,length.out=100)
    dims<-ncol(object$args$lims)
    plot(xx,palm.intensity(xx,coefs[3],coefs[2],coefs[1],dims),type="l",ylab=expression(lambda[0](r,theta)),xlab=expression(r))
}
   



