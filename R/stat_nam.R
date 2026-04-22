
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
#' Weight handling in this wrapper follows a few network-analysis-oriented
#' conventions:
#'
#' \itemize{
#'   \item \code{W} and \code{W2} may be supplied as matrices, edgelists,
#'   \code{igraph} objects, or \code{network} objects, but they must represent
#'   square one-mode weight matrices after conversion.
#'   \item Non-zero rows are row-standardized internally through
#'   \code{spdep::mat2listw(style = "W")}. A message is shown only when those
#'   non-zero rows are not already standardized.
#'   \item Zero rows are allowed when \code{zero.policy = TRUE}; these encode
#'   vertices without neighbours and are not treated as malformed input.
#'   \item Diagonal values are preserved. If self-weights are present, they are
#'   part of the fitted network autocorrelation model rather than being removed
#'   silently.
#'   \item Edge weights are preserved during conversion. For edgelists, the
#'   first two columns are treated as sender and receiver, and the third column
#'   (if present) is used as the numeric weight.
#' }
#'
#' @param formula a symbolic description of the model to be fit. 
#' @param data an optional data frame containing the variables in the model. 
#' By default the variables are taken from the environment which the function is called.
#' @param W Spatial weight matrix for the lagged model or the error model. 
#' This can be a square matrix, an edgelist data frame, or a graph of class
#' \code{network} or \code{igraph}. Edge weights and diagonal values are kept
#' as supplied and are row-standardized internally through
#' \code{\link[spdep]{mat2listw}}.
#' @param W2 Spatial weight matrix for the error, in case of a combined model 
#' (otherwise, it is discarded). This can be a square matrix, an edgelist data
#' frame, or a graph of class \code{network} or \code{igraph}. As for
#' \code{W}, edge weights and diagonal values are preserved before internal
#' row-standardization.
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
#' @name stat_nam
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
#' # Model from Doreian (1980), fitted with the snafun wrapper
#' data(huk, package = "SNA4DSData")
#' nam1 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "lag")
#' nam2 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, model = "error")
#' nam3 <- snafun::stat_nam(y ~ ., data = hukYX, W = hukWstd, W2 = hukWstd, model = "combined")
#' }
NULL

#' Prepare a weight matrix for network autocorrelation models
#'
#' Convert supported network inputs to the \code{listw} format expected by
#' \pkg{spatialreg}. The helper keeps user-supplied edge weights and diagonal
#' values intact, checks that the resulting matrix is square and matches the
#' number of observations, and lets \code{spdep::mat2listw(style = "W")}
#' perform the actual row-standardization.
#'
#' @param x A square matrix, edgelist data frame, \code{network}, or
#' \code{igraph} object.
#' @param arg_name Name of the calling argument, used in informative error
#' messages.
#' @param n_observations Expected number of rows in the fitted model frame.
#' @param zero.policy Logical scalar passed through to
#' \code{\link[spdep]{mat2listw}}.
#' @param tolerance Numeric tolerance used to decide whether non-zero rows are
#' already row-standardized.
#'
#' @return An object of class \code{listw}.
#' @keywords internal
#' @noRd
prepare_nam_weight_listw <- function(x, arg_name, n_observations,
                                     zero.policy,
                                     tolerance = sqrt(.Machine$double.eps)) {
    if (!inherits(x, "network") &&
        !inherits(x, "igraph") &&
        !is.matrix(x) &&
        !is.data.frame(x)) {
        stop(arg_name,
             " should be a square matrix, an edgelist data frame, or be of class ",
             "'network' or 'igraph'")
    }

    weight_matrix <- snafun::to_matrix(x)
    if (!is.matrix(weight_matrix) ||
        nrow(weight_matrix) != ncol(weight_matrix)) {
        stop(arg_name, " should represent a square one-mode weight matrix")
    }
    if (nrow(weight_matrix) != n_observations) {
        stop(arg_name, " should have ", n_observations,
             " rows and columns, matching the number of observations in the model")
    }
    if (!is.numeric(weight_matrix)) {
        stop(arg_name, " should contain numeric weights")
    }
    if (anyNA(weight_matrix)) {
        stop(arg_name, " should not contain missing values")
    }

    # The spatialreg backend expects row-standardized weights, but users in
    # network analysis often provide raw adjacency-style matrices. We therefore
    # only message when a genuinely non-zero row is not already standardized and
    # let spdep do the canonical row normalization.
    row_sums <- base::rowSums(weight_matrix)
    non_zero_rows <- row_sums > tolerance
    if (any(non_zero_rows) &&
        any(abs(row_sums[non_zero_rows] - 1) > tolerance)) {
        message("You supplied a weight matrix",
                if (identical(arg_name, "W2")) " (W2)" else "",
                " that was not row-normalized\n",
                "      this is still automatically done for this analysis.")
    }

    spdep::mat2listw(weight_matrix,
                     style = "W",
                     zero.policy = zero.policy)
}


#' Check whether NAM design columns vary
#'
#' Build the right-hand-side design matrix and warn when one or more columns are
#' constant. This is more robust than checking the raw variables directly,
#' because factors and transformed terms may expand to multiple columns.
#'
#' @param formula Model formula passed to \code{stat_nam()}.
#' @param model_frame Model frame constructed from \code{formula} and
#' \code{data}.
#'
#' @return Invisibly returns \code{NULL}; it is used for its warning side
#' effect.
#' @keywords internal
#' @noRd
check_nam_design_variation <- function(formula, model_frame) {
    rhs_terms <- stats::delete.response(stats::terms(formula,
                                                     data = model_frame))
    rhs_matrix <- stats::model.matrix(rhs_terms, data = model_frame)
    keep_columns <- colnames(rhs_matrix) != "(Intercept)"
    rhs_matrix <- rhs_matrix[, keep_columns, drop = FALSE]

    if (ncol(rhs_matrix) == 0L) {
        return(invisible(NULL))
    }

    constant_columns <- apply(rhs_matrix, 2, function(column) {
        column <- column[!is.na(column)]
        length(unique(column)) <= 1L
    })
    if (any(constant_columns)) {
        warning("There are ", sum(constant_columns),
                " constant design-matrix column(s) on the right hand side: ",
                paste(colnames(rhs_matrix)[constant_columns], collapse = ", "),
                ". Please remove them.")
    }

    invisible(NULL)
}


#' Fit a network autocorrelation model via spatialreg
#'
#' Assemble the argument list for \pkg{spatialreg} while preserving the
#' optional \code{na.action} argument only when the user supplied it.
#'
#' @param fit_function Fitting function from \pkg{spatialreg}.
#' @param formula,data,listw,listw2,Durbin,quiet,zero.policy Model arguments
#' forwarded to the selected backend.
#' @param na_action Optional \code{na.action} value supplied by the user.
#' @param na_action_supplied Logical scalar indicating whether the user
#' explicitly supplied \code{na.action}.
#'
#' @return The fitted \code{Sarlm} object returned by \pkg{spatialreg}.
#' @keywords internal
#' @noRd
fit_nam_spatialreg <- function(fit_function, formula, data, listw,
                               listw2 = NULL, Durbin, quiet, zero.policy,
                               na_action = NULL,
                               na_action_supplied = FALSE) {
    fit_arguments <- list(
        formula = formula,
        data = data,
        listw = listw,
        Durbin = Durbin,
        quiet = quiet,
        zero.policy = zero.policy
    )
    if (isTRUE(na_action_supplied)) {
        fit_arguments$na.action <- na_action
    }
    if (!is.null(listw2)) {
        fit_arguments$listw2 <- listw2
    }

    do.call(fit_function, fit_arguments)
}

#' @rdname stat_nam
#' @export
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

    na_action_supplied <- !missing(na.action)
    if (na_action_supplied) {
        na_action_value <- na.action
    } else {
        na_action_value <- NULL
    }

    if (identical(model, "combined") && is.null(W2)) {
        stop("For model = 'combined', you need to supply 'W2'")
    }

    model_frame <- stats::model.frame(formula = formula,
                                      data = data,
                                      na.action = stats::na.pass)
    n_observations <- nrow(model_frame)

    W <- prepare_nam_weight_listw(x = W,
                                  arg_name = "W",
                                  n_observations = n_observations,
                                  zero.policy = zero.policy)

    if (!is.null(W2)) {
        W2 <- prepare_nam_weight_listw(x = W2,
                                       arg_name = "W2",
                                       n_observations = n_observations,
                                       zero.policy = zero.policy)
    }

    if (isTRUE(check_vars)) {
        check_nam_design_variation(formula = formula,
                                   model_frame = model_frame)
    }

    if (identical(model, "lag")) {
        fit_nam_spatialreg(
            fit_function = spatialreg::lagsarlm,
            formula = formula,
            data = data,
            listw = W,
            Durbin = Durbin,
            quiet = quiet,
            zero.policy = zero.policy,
            na_action = na_action_value,
            na_action_supplied = na_action_supplied
        )
    } else if (identical(model, "error")) {
        fit_nam_spatialreg(
            fit_function = spatialreg::errorsarlm,
            formula = formula,
            data = data,
            listw = W,
            Durbin = Durbin,
            quiet = quiet,
            zero.policy = zero.policy,
            na_action = na_action_value,
            na_action_supplied = na_action_supplied
        )
    } else {
        fit_nam_spatialreg(
            fit_function = spatialreg::sacsarlm,
            formula = formula,
            data = data,
            listw = W,
            listw2 = W2,
            Durbin = Durbin,
            quiet = quiet,
            zero.policy = zero.policy,
            na_action = na_action_value,
            na_action_supplied = na_action_supplied
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

