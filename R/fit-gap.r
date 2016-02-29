#' Fitting a model to a Matern void point process
#'
#' @return An object with information that can be extracted via other
#' utility functions.
#'
#' @param points A matrix containing locations of observed points,
#' where each row corresponds to a point and each column corresponds
#' to a dimension.
#' @param lims A matrix with two columns, corresponding to the upper
#' and lower limits of each dimension, respectively.
#' @param sv A named vector with the starting values \code{R} and \code{D} in optimisation.
#' @param bounds A named vector of vectors specifying the bounds of the  parameters \code{R.bounds} and \code{D.bounds} in
#' optimisation.
#' @param trace Logical, if \code{TRUE}, parameter values are printed
#' to the screen for each iteration of the optimisation procedure.
#'
#' @export
fit.gap <- function(points = NULL, lims = NULL, sv = NULL, R.bounds = NULL,D.bounds=NULL, trace = FALSE){
    ## Calculating survey area.
    dims <- nrow(lims)
    n.points<-nrow(points)
    area <- prod(apply(lims, 1, diff))
    ## Calculating distances using pbc function from Ben's package.
    dists <- nspp:::pbc_distances(points = points, lims = lims)
    ## Sorting out start values.
    R.sv <- sv["R"]
    D.sv <- sv["D"]
    lambda.sv <-n.points/area
    sv <- c(R.sv, D.sv, lambda.sv)
    names(sv) <- c("R", "D", "lambda")
    ## Sorting out bounds.
    R.bounds <- R.bounds
    D.bounds <- D.bounds
    lambda.bounds <- c(0, nrow(points))
    lower <- c(R.bounds[1], D.bounds[1], lambda.bounds[1])
    upper <-c(R.bounds[2], D.bounds[2], lambda.bounds[2])
    palm.intensity=palm.intensity
    integrand=integrand
    mmpp.ll=mmpp.ll
    if (any(is.nan(log(sv)))) traceback()
    fit <-  optimx(par = log(sv), fn = mmpp.ll,
                   method = "L-BFGS-B",
                   lower = log(lower),
                   upper = log(upper),
                   n.points = n.points, dists = dists,
                   d = dims,
                   par.names = names(sv), palm.intensity = palm.intensity,
                   trace = trace, integrand = integrand)
    ## Extracting child.disp and nu estimates.
    opt.pars <- exp(coef(fit)[1, ])
    names(opt.pars) <- names(sv)
    R <- opt.pars["R"]
    D <- opt.pars["D"]
    lambda <- opt.pars["lambda"]
    pars <- c(R,D,lambda)
    names(pars) <- c("R", "D", "lambda")
    out <- list(pars = pars, args = args)
    class(out) <- "gapski"
    out
}
