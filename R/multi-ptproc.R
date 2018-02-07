## Constructor function

ptproc <- function(pts = NULL, cond.int, params,
                   fixed = rep(NA, length(params)), condition = NULL,
                   ranges = NULL, data = NULL, is.pois = FALSE) {

    cif <- match.fun(cond.int)
    fun.name <- toupper(deparse(substitute(cond.int)))
    stopifnot(length(params) == length(fixed))

    if(is.null(names(params)))
        names(params) <- paste("p", 1:length(params), sep ="")

    if(is.null(pts)) {  ## For simulation
        cat("Point process model for simulation only\n")

        if(is.null(ranges))
            stop("`ranges' needs to be specified")
        ppm <- structure(list(cond.int = cif,
                              params = params,
                              ranges = ranges,
                              model = fun.name,
                              data = data,
                              is.pois = is.pois),
                         class = "ptprocSim")
    }
    else {  ## For fitting
        names(fixed) <- names(params)

        if(is.vector(pts) || is.data.frame(pts))
            pts <- as.matrix(pts)
        
        if(is.null(ranges)) 
            ranges <- apply(pts, 2, range)        

        ppm <- structure(list(pts = pts,
                              cond.int = cif,
                              params = params,
                              fixed = fixed,
                              ranges = ranges,
                              model = fun.name,
                              condition = condition,
                              data = data,
                              is.pois = is.pois),
                         class = "ptprocInit")
    }
    ppm
}

residuals.ptprocFit <- function(object, type = c("ordinary", "approx"),
                                m = NULL, K = NULL, R = 1, ...) {
    ppobj <- object
    type <- match.arg(type)
    pts <- getPts(ppobj)
    
    if(is.null(pts))
        stop("Cannot construct residual process when `pts' is NULL")
    if(R < 1)
        stop("`R' must be >= 1")
    res <- vector("list", length = R)
    
    if(type == "ordinary") {
        if(is.null(m))
            stop("`m' must be specified for ordinary thinned residuals")
        ci <- evalCIF(ppobj)

        if(any(ci < m))
            stop("Some conditional intensity values < m")
        p <- m / ci
        
        for(i in 1:R) {
            coin <- rbinom(length(ci), 1, p)
            keep <- pts[coin == 1, , drop = FALSE]
            colnames(keep) <- colnames(getPts(ppobj))
            res[[i]] <- keep
        }
    }
    else {  ## approx
        if(is.null(K))
            stop("`K' must be specified for approximate thinned residuals")
        ci <- evalCIF(ppobj)        
        p <- (1 / ci) / sum(1 / ci)        
        idx <- 1:NROW(pts)

        for(i in 1:R) {
            r.idx <- sort(sample(idx, K, rep = FALSE, prob = p))
            keep <- pts[r.idx, , drop = FALSE]
            attr(keep, "rate") <- K / prod(apply(ranges(object), 2, diff))
            colnames(keep) <- colnames(getPts(ppobj))
            res[[i]] <- keep
        }
    }
    if(R == 1)
        res <- res[[1]]
    attr(res, "rate") <- switch(type, ordinary = m,
                                approx = K/prod(apply(ranges(object),2,diff)))
    attr(res, "type") <- type
    res
}

evalCIF.default <- function(x, xpts = getPts(x), ...) {
    ppobj <- x
    if(!is.matrix(xpts))
        xpts <- as.matrix(xpts)
    cif <- getCIF(ppobj)
    ci <- cif(params = params(ppobj), eval.pts = xpts,
              pts = getPts(ppobj), data = getData(ppobj), ...)
    ci
}

integrateCIF.default <- function(x, TT = ranges(x), ...) {
    ppobj <- x
    cif <- getCIF(ppobj)
    int <- cif(params = params(ppobj), eval.pts = NULL,
               pts = getPts(ppobj), data = getData(ppobj), TT = TT, ...)
    as.numeric(int)
}

logLik.ptprocInit <- logLik.ptprocFit <- function(object, negative = FALSE, ...) {
    ci <- evalCIF(object)

    if(any(ci < 0)) 
        stop("Some conditional intensity values < 0")
    L1 <- sum(log(ci))
    L2 <- integrateCIF(object)
    LL <- L1 - L2
    if(negative)
        LL <- -LL    
    attr(LL, "df") <- (length(params(object)) - length(na.omit(fixed(object))))
    class(LL) <- "logLik"
    LL
}

ptproc.fit <- function(ppobj, optim.control = list(), method = "Nelder-Mead",
                       alpha = 0, ...) {
    if(!inherits(ppobj, "ptprocInit")) 
        stop("Only use with \"ptprocInit\" objects!")
    initial.params <- params(ppobj)
    
    fixed <- fixed(ppobj)
    mask <- is.na(fixed)
    pstart <- params(ppobj)[mask]
    optim.logLik <- make.optim.logLik(ppobj, alpha)

    fit <- optim(pstart, optim.logLik, method = method,
                 control = optim.control, ...)
    fitted.params <- params(ppobj)
    fitted.params[mask] <- fit$par

    structure(list(ptprocInit = ppobj,
                   params = fitted.params,
                   hessian = fit$hessian,
                   convergence = fit$convergence),
              class = "ptprocFit")
}

make.optim.logLik <- function(ppobj, alpha) {
    ## Here ppobj is an object of class "ptprocInit"
    optim.logLik <- function(params) {
        fixed <- fixed(ppobj)
        mask <- is.na(fixed)
        params(ppobj)[mask] <- params
        condition <- condition(ppobj)
        returnflag <- NULL
        LL <- 0

        if(!is.null(condition)) {
            if(!is.expression(condition))
                warning("condition must be an expression")
            else
                eval(condition)    
        }
        if(!is.null(returnflag))
            return(returnflag)
        LL <- LL + logLik(ppobj, negative = TRUE)
    }
}

## (7/7/03) Need to revise this for better non-Poisson simulation;
## maybe introduce some notion of a "list history", a la Daley &
## Vere-Jones (2003), rather than do surgery on sim.obj. -RDP

ptproc.sim <- function(ppobj, M) {
    if(!inherits(ppobj, "ptprocSim"))
        ppobj <- as.ptprocSim(ppobj)
    ranges <- ranges(ppobj)
    n <- ceiling(M * prod(apply(ranges, 2, diff)))

    ## Simulate homogeneous Poisson with rate M
    u <- sapply(1:dim(ppobj), function(i) runif(n,ranges[1,i],ranges[2,i]))

    if(is.null(dim(u)))
        u <- matrix(u, byrow = TRUE, ncol = dim(ppobj))

    ## Order by the first column (in case they are times)
    u <- u[order(u[,1]), , drop = FALSE]

    if(ppobj$is.pois) {
        ci <- evalCIF(ppobj, xpts = u)

        if(any(ci > M))
            stop("Some conditional intensity values > M")    
        coin <- rbinom(n, 1, ci / M)
        u.keep <- u[coin > 0, , drop = FALSE]
    }
    else {
        u.keep <- NULL
        sim.obj <- ppobj
        sim.obj$pts <- NULL
        
        for(i in 1:n) {
            ci <- evalCIF(sim.obj, xpts = u[i, , drop = FALSE])
            
            if(ci > M) {
                cat("Conditional intensity value > M;  Stopping.\n")
                return(u.keep)
            }
            coin <- rbinom(1, 1, ci / M)
            
            if(coin > 0) {
                u.keep <- rbind(u.keep, u[i, ])
                
                if(is.null(sim.obj$pts))
                    sim.obj$pts <- u[i, , drop = FALSE]
                else
                    sim.obj$pts <- rbind(sim.obj$pts, u[i, ])
            }    
        }
    }
    u.keep
}

penalty <- function(code = NULL, condition) {
    str.condition <- deparse(condition)
    str.condition <- paste("if(", str.condition, ") returnflag <- alpha",
                           sep = "")

    if(!is.null(code)) {
        if(!is.expression(code))
            code <- as.expression(code)
        str.code <- do.call("paste", list(lapply(code, deparse),
                                          collapse = "; "))
        full.condition <- parse(text = paste(str.code, str.condition,
                                sep = "; "))
    }
    else 
        full.condition <- parse(text = str.condition)
    full.condition
}



## Print and Summary methods

print.ptprocInit <- function(x, digits = getOption("digits") - 3, ...) {
    cat("Model name:", x$model, "\n\n")
    cat("Initial Parameter Values:\n")
    print(format(params(x), digits = digits), print.gap = 2, quote = FALSE)
    cat("\nFixed Parameters:\n")
    fp <- as.numeric(fixed(x))
    names(fp) <- names(fixed(x))
    print(fp)

    if(!is.null(condition(x))) {
        e <- paste(deparse(condition(x), width = 20)[1],
                   "...", sep = "")
        cat("\nCondition:", e, "\n")
    }
    invisible(x)
}

print.ptprocFit <- function(x, digits = getOption("digits") - 3, ...) {
    cat("Model name:", extract.ptprocInit(x)$model, "\n\n")
    cat("Fitted Parameter Values:\n")
    print(format(params(x), digits = digits),
          print.gap = 2, quote = FALSE)
    cat("\nInitial Parameter Values:\n")
    print(format(params(extract.ptprocInit(x)), digits = digits), 
          print.gap = 2, quote = FALSE)
    cat("\nFixed Parameters:\n")
    fp <- as.numeric(fixed(x))
    names(fp) <- names(fixed(x))
    print(fp)

    if(!is.null(condition(x))) {
        e <- paste(deparse(condition(x), width = 20)[1], "...", sep = "")
        cat("\nCondition:", e, "\n")
    }
    invisible(x)
}

print.ptprocSim <- function(x, digits = getOption("digits") - 3, ...) {
    cat("Model name:", x$model, "\n\n")
    cat("Parameter Values:\n")
    print(format(params(x), digits = digits),
          print.gap = 2, quote = FALSE)
    invisible(x)
}

summary.ptprocFit <- function(object, ...) {
    npts <- NROW(getPts(object))
    vol <- prod(apply(ranges(object), 2, diff))
    poisson.AIC <- -2 * (npts * log(npts / vol) - npts) + 2
    model.AIC <- AIC(object)

    s <- structure(list(poisson.AIC = poisson.AIC, model.AIC = model.AIC,
                        ppobj = object), class = "summary.ptprocFit")
    s
}

print.summary.ptprocFit <- function(x, digits = getOption("digits") - 3, ...) {
    cat("Model name:", extract.ptprocInit(x$ppobj)$model, "\n\n")
    cat("Fitted Parameter Values:\n")
    print(format(params(x$ppobj), digits = digits),
          print.gap = 2, quote = FALSE)
    cat("\nModel AIC:\t", x$model.AIC)
    cat("\nH. Pois. AIC:\t", x$poisson.AIC)
    cat("\n")
    invisible(x)
}

## summary.ptprocInit <- function(object, ...) {
##     structure(list(ppobj = object), class = "summary.ptprocInit")
## }

## print.summary.ptprocInit <- function(x, ...) {
##     print(x$ppobj)
## }

