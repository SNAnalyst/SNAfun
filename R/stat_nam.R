
#' Network autocorrelation model
#' 
#' Run the network autocorrelation model
#' 
#' This function can run the lagged network autocorrelation model, the 
#' error/disturbances model, and the combined model.
#' 
#' 
#' In particular, \code{model = "lag"} provides Maximum likelihood estimation 
#' of spatial simultaneous autoregressive lag models of the form:
#' 
#' \deqn{y = \rho W y + X \beta + \varepsilon}{y = rho W y + X beta + e}
#' 
#' where \eqn{\rho}{rho} is found by \code{optimize()} first, and \eqn{\beta}{beta} 
#' and other parameters by generalized least squares subsequently 
#' (one-dimensional search using optim performs badly on some platforms). 
#' With one of the sparse matrix methods, larger numbers of observations can be 
#' handled.
#' 
#' When \code{model = "error"}, the Maximum likelihood estimation is performed 
#' of spatial simultaneous autoregressive error models of the form:
#' 
#' \deqn{y = X \beta + u, u = \lambda W u + \varepsilon}{y = X beta + u, u = lambda W u + e}
#' 
#' where \eqn{\lambda}{lambda} is found by \code{optimize()} first, and 
#' \eqn{\beta}{beta} and other parameters by generalized least squares subsequently. 
#' With one of the sparse matrix methods, larger numbers of observations can be 
#' handled.
#' 
#' When \code{model = "combined"}, Maximum likelihood estimation is performed of 
#' spatial simultaneous autoregressive \dQuote{SAC/SARAR} models of the form:
#' 
#' \deqn{y = \rho W1 y + X \beta + u, u = \lambda W2 u + \varepsilon}{y = rho W1 y + X beta + u, u = lambda W2 u + e}
#' 
#' where \eqn{\rho}{rho} and \eqn{\lambda}{lambda} are found by \code{nlminb} 
#' or \code{optim()} first, and \eqn{\beta}{beta} and other parameters by generalized 
#' least squares subsequently.
#' 
#' The actual fitting of the model is performed through the \code{\link[spatialreg]{lagsarlm}}, 
#' \code{\link[spatialreg]{errorsarlm}}, \code{\link[spatialreg]{sacsarlm}} functions and the 
#' handling of the weight matrix through the \code{\link[spdep]{mat2listw}} function.
#' This function wraps these functions and sets some defaults that may be 
#' useful to network analysis, but not necessarily always ideal for spatial 
#' analysis. Also, the functions in the \pkg{spatialreg} package are more 
#' flexible than was it exposed through this wrapper. So, for fitting a full 
#' model, refer to these functions directly. 
#' Do note, though, that those functions handle the weight matrix in a way that 
#' is uncommon to the network analyst, and this wrapper function makes accessing 
#' these models a lot more straightforward.
#' Also note that the original functions do not allow graphs of class 
#' \code{network} or \code{igraph} to be utilized as weight matrices, which is 
#' another benefit is our function which does make this possible.
#' 
#' A useful \code{summary} function is implemented within the \pkg{spatialreg} package.
#'
#' @param formula a symbolic description of the model to be fit. 
#' @param data an optional data frame containing the variables in the model. 
#' By default the variables are taken from the environment which the function is called.
#' @param W Spatial weight matrix for the lagged model or the error model. 
#' This can be a matrix or a graph of class \code{network} or \code{igraph}.
#' @param W2 Spatial weight matrix for the error, in case of a combined model 
#' (otherwise, it is discarded). This can be a matrix or a graph of 
#' class \code{network} or \code{igraph}
#' @param model character, either \code{lag}, \code{error}, or \code{combined}
#' @param na.action a function (default \code{options("na.action")}), 
#' can also be \code{na.omit} or \code{na.exclude} with consequences for residuals 
#' and fitted values - in these cases the weights list will be subsetted to remove 
#' \code{NAs} in the data. It may be necessary to set \code{zero.policy} to 
#' \code{TRUE} because this subsetting may create no-neighbour observations. 
#' @param Durbin default \code{FALSE} (spatial lag model); 
#' if \code{TRUE}, full spatial Durbin model; if a formula object, 
#' the subset of explanatory variables to lag
#' @param quiet default \code{NULL}, use !verbose global option value; 
#' if \code{FALSE}, reports function values during optimization.
#' @param zero.policy if \code{TRUE}, the default, assign zero to the lagged value of 
#' vertices without neighbours, if \code{FALSE} assign \code{NA} - causing 
#' \code{spatialreg::lagsarlm()} to terminate with an error
#' @param check_vars if \code{TRUE}, the default, a check is performed as to 
#' whether all variables on the right hand side of the formula are non-constant.
#' Note that the function will add an intercept by default, so constants are 
#' not wanted or needed on the rhs of the formula.
#'
#' @return an object of class \code{Sarlm}
#' @export
#' @examples
#' # Simulate data for the lagged model
#' aantal_vars <- 10
#' nobs <- 50
#' rho = .3
#' coefs <- rnorm(aantal_vars)
#' x <- rnorm(nobs*aantal_vars, sd = 8)
#' x <- matrix(x, ncol = aantal_vars)
#' colnames(x) <- LETTERS[1:ncol(x)]
#' w <- sample(c(0, 1), nobs*nobs, replace = TRUE, prob = c(.8, .2))
#' w <- matrix(w, ncol = nobs)
#' diag(w) <- 0
#' w <- w/rowSums(w)    # redundant
#' eps <- rnorm(nobs, sd = 5)
#' ie <- matrix(0, ncol = nobs, nrow = nobs)
#' diag(ie) <- 1
#' y <- solve(ie - rho*w) %*% (x %*% coefs + eps)
#' ix <- data.frame(y, x)
#' mod <- stat_nam(y ~ ., data = ix, W = w)
#' stat_nam_summary(mod)
#' stat_nam_summary(mod, correlation = TRUE)
#' plot_nam(mod)
#' 
#' #### Now a full combined model
#' #Draw the AR matrix
#' w1 <- snafun::create_random_graph(100, strategy = "gnp", p = .2)
#' #Draw the MA matrix
#' w2 <- snafun::create_random_graph(100, strategy = "gnp", p = .2)
#' x <- matrix(rnorm(100*5),100,5) #Draw some covariates
#' r1 <- 0.2                       #Set the model parameters
#' r2 <- 0.1
#' sigma <- 0.1
#' beta <- rnorm(6)
#' #Assemble y from its components:
#' nu <- rnorm(100, 0, sigma)          #Draw the disturbances
#' # only for the simulation, row-standardized weights are needed
#' ww1 <- snafun::to_matrix(w1)
#' ww1 <- ww1/rowSums(ww1)
#' ww2 <- snafun::to_matrix(w2)
#' ww2 <- ww2/rowSums(ww2)
#' xx <- cbind(1, x)
#' e <- qr.solve(diag(100) - r2 * ww2, nu) #Draw the effective errors
#' y <- qr.solve(diag(100) - r1 * ww1, xx %*% beta + e)  #Compute y
#' ix <- data.frame(y, x)
#' mod <- stat_nam(y ~ ., data = ix, W = w1, W2 = w2, model = "combined")
#' stat_nam_summary(mod)
#' stat_nam_summary(mod, correlation = TRUE)
#' plot_nam(mod)
#' 
#' # fit the disturbances model
#' mod <- stat_nam(y ~ ., data = ix, W = w2, model = "error")
#' stat_nam_summary(mod)
#' stat_nam_summary(mod, correlation = TRUE)
#' plot_nam(mod)
#' 
#' \dontrun{
#' # Model from Doreian (1980)
#' data(huk, package = "SNA4DSData")
#' x <- as.matrix(cbind(Intcpt = 1, hukYX[, -1]))
#' lnam1 <- sna::lnam(y = hukYX$y, x = x, W1 = hukW)
#' lnam2 <- sna::lnam(y = hukYX$y, x = x, W2 = hukW)
#' lnam3 <- sna::lnam(y = hukYX$y, x = x, W = hukW, W2 = hukW)
#' 
#' # For comparison, the models with a row standardized W 
#' lnam1 <- sna::lnam(y = hukYX$y, x = x, W1 = hukWstd)
#' lnam2 <- sna::lnam(y = hukYX$y, x = x, W2 = hukWstd)
#' lnam3 <- sna::lnam(y = hukYX$y, x = x, W = hukWstd, W2 = hukWstd)
#' 
#' # Same model with our function
#' nam1 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "lag")
#' nam2 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "error")
#' nam3 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, W2 = hukWstd, model = "combined")
#' }
stat_nam <- function(formula, data = list(), W, 
                     W2 = NULL,
                     model = c("lag", "error", "combined"),
                     na.action, 
                     Durbin = FALSE,
                     quiet = TRUE,
                     zero.policy = TRUE,
                     check_vars = TRUE) {
    
    model <- model[1]
    if (!model %in% c("lag", "error", "combined")) {
        stop("The only options for 'model' are 'lag', 'error', or 'combined'")
    }
    
    if (!inherits(formula, "formula")) 
        formula <- stats::as.formula(formula)

    if (!snafun::is_network(W) & !snafun::is_igraph(W) & !is.matrix(W)) {
        stop("W should be a matrix or be of class 'network' or 'igraph'")
    } 
    W <- snafun::to_matrix(W)  # W is now a matrix
    if (!all(rowSums(W) - 1 < .1e-5)) {
        message("You supplied a weight matrix that was not row-normalized\n",
                "      this is still automatically done for this analysis.")
    }
    W <- spdep::mat2listw(W, style = "W")
    
    if (!is.null(W2)) {
        if (!snafun::is_network(W2) & !snafun::is_igraph(W2) & !is.matrix(W2)) {
            stop("W2 should be a matrix or be of class 'network' or 'igraph'")
        } 
        W2 <- snafun::to_matrix(W2)  # W2 is now a matrix
        if (!all(rowSums(W2) - 1 < .1e-5)) {
            message("You supplied a weight matrix (W2) that was not row-normalized\n",
                    "      this is still automatically done for this analysis.")
        }
        W2 <- spdep::mat2listw(W2, style = "W")
    }
    

    if(check_vars) {
        vars <- attr(stats::terms(formula, data = data), "term.labels")
        modeldata <- data[, vars]
        sds <- sapply(modeldata, stats::sd)
        sd_nul <- which(sds == 0)
        if (length(sd_nul) > 0) {
            warning("There are ", length(sd_nul), "constant variable(s), please remove.")
        }
    }
    
    if (model == "lag") {
        spatialreg::lagsarlm(formula = formula, data = data,
                             listw = W, na.action = na.action,
                             Durbin = Durbin,
                             quiet = quiet, 
                             zero.policy = zero.policy
        )  
    } else if (model == "error") {
        spatialreg::errorsarlm(formula = formula, data = data,
                             listw = W, na.action = na.action,
                             Durbin = Durbin,
                             quiet = quiet, 
                             zero.policy = zero.policy
        )  
    } else if (model == "combined") {
        spatialreg::sacsarlm(formula = formula, data = data,
                               listw = W, listw2 = W2, na.action = na.action,
                               Durbin = Durbin, 
                               quiet = quiet, 
                               zero.policy = zero.policy
        )  
    } 
}



#' Network autocorrelation results
#' 
#' Extract the summary of a network autocorrelation model analysis
#' 
#' This function wraps functions from the \pkg{spatialreg} package to summarize 
#' the results from an analysis with the \code{\link[snafun]{stat_nam}} function. 
#' 
#'
#' @param x the output object from \code{\link[snafun]{stat_nam}}
#' @param correlation logical; if \code{TRUE} (the default), the correlation 
#' matrix of the estimated parameters including sigma is returned and printed
#' @param R2 logical; if \code{TRUE} (the default), the Nagelkerke 
#' pseudo R-squared is reported
#' @param digits the number of significant digits to use when printing
#' @param signif.stars ;ogical; if \code{TRUE} (the default), "significance stars" 
#' are printed for each coefficient
#'
#' @return prints the summary to the console and invisibly returns the 
#' summary object
#' @export
stat_nam_summary <- function(x, correlation = TRUE, R2 = TRUE,
                        digits = 3, signif.stars = TRUE) {
    if (!inherits(x, "Sarlm")) {
        stop("This function only works on the output of a network autocorrelation analysis")
    }
    summary.Sarlm <- utils::getFromNamespace("summary.Sarlm", "spatialreg")
    print.summary.Sarlm <- utils::getFromNamespace("print.summary.Sarlm", "spatialreg")
    
    summ <- summary.Sarlm(x, 
                                       correlation = correlation,
                                       Nagelkerke = R2)
    
    print.summary.Sarlm(summ, 
          digits = digits, 
          signif.stars = signif.stars)
    invisible(summ)
}






#' Plot diagnostics for the network autorrelation model
#' 
#' Plot diagnostics for the network autorrelation model
#' 
#' Plots diagnostics For the output of \code{\link[snafun]{stat_nam}}. 
#'
#' @param x output from \code{\link[snafun]{stat_nam}}
#' @param ... ignored
#'
#' @return nothing is returned, the plots are displayed in the console
#' @export
plot_nam <- function (x, ...) {
    r <- stats::residuals(x)
    
    if (is.null(x$na.action)) {
        f <- x$fitted.values  
    } else {
        f <- stats::napredict(x$na.action, x$fitted.values)
    }
    d <- x$residuals
    sdr <- stats::sd(r)
    ci <- c(-1.959964, 1.959964)
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow = c(2, 2))
    plot(x$y, f, ylab = expression(hat(y)), xlab = expression(y), 
         main = "Fitted vs. Observed Values")
    graphics::abline(ci[1] * sdr, 1, lty = 3)
    graphics::abline(0, 1, lty = 2)
    graphics::abline(ci[2] * sdr, 1, lty = 3)
    
    plot(f, d, ylab = expression(hat(nu)), xlab = expression(hat(y)), 
         ylim = c(min(ci[1] * x$sigma, d), max(ci[2] * x$sigma, d)), main = "Fitted Values vs. Estimated Disturbances")
    graphics::abline(h = c(ci[1] * x$sigma, 0, ci[2] * x$sigma), lty = c(3, 
                                                               2, 3))
    stats::qqnorm(r, main = "Normal Q-Q Residual Plot")
    stats::qqline(r)
    
    plot(c(0, 1), c(0, 1), type = "n",
         xlab = NA, ylab = NA, axes = FALSE)
    graphics::text(0, .5, "Diagnostics for the fitted\nNetwork Autocorrelation Model",
         pos = 4)
    invisible()
}

