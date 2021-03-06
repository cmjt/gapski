#' Bootstrapping a void process model
#'
#' Carries out a bootstrap for void process models
#' fitted by \link{fit.gap}().
#'
#' @return Summary of the model considered, with added information from the
#' bootstrap procedure.
#'
#' @param fit A fitted object from \link{fit.gap}().
#' @param N The number of bootstrap resamples.
#' @param prog Logical, if \code{TRUE}, a progress bar is printed to
#' the console.
#' 
#' @export
boot.gap <- function(fit, N, prog = TRUE){
    ## Extracting information.
    args <- fit$args    
    pars <- fit$pars
    n.pars <- length(pars)
    lims <- args$lims
    dims <- nrow(lims)
    #bootstrapping 
    boots <- matrix(0, nrow = N, ncol = n.pars)
    ## Setting up progress bar.
    if (prog){
        pb <- txtProgressBar(min = 0, max = N, style = 3)
    }
    for (i in 1:N){
        #browser()
        args$points <- tryCatch(sim.gap(pars=pars[c("R","D","lambda")],lims = lims, d = dims),error=function(e) e)
        if(!inherits(args$points, "error")){
            args$D.sv<-pars["D"]
            args$trace<-FALSE
            fit.boot <- tryCatch(do.call("fit.gap", args),error=function(e) e)
            if(inherits(fit.boot,"error")) next
            boots[i, ] <- fit.boot$pars
        }
        ## Updating progress bar.
        if (prog){
            setTxtProgressBar(pb, i)
        }
    }
    if (prog){
        close(pb)
    }
    colnames(boots) <- names(pars)
    boots <- boots[boots[,1]!=0,] # dirty gets rid of zero entries
    # warning to say full bootstrap was not carried out
    if(nrow(boots)!=N){
        warning(paste("Standard errors only based on", nrow(boots), "simulations",sep = " "))
    }
    fit$boots <- boots
    fit$se <- apply(na.omit(boots), 2, sd)
    class(fit) <- c("boot.gapski", class(fit))
    fit
}
