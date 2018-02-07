## Some methods

dim.ptprocInit <- dim.ptprocSim <- function(x) {
    NCOL(ranges(x))
}

dim.ptprocFit <- function(x) {
    dim(extract.ptprocInit(x))
}

extract.ptprocInit.ptprocFit <- function(x, ...) {
    x$ptprocInit
}
    
getPts.ptprocInit <- function(x, ...) {
    x$pts
}

getPts.ptprocFit <- function(x, ...) {
    getPts(extract.ptprocInit(x))
}

getPts.ptprocSim <- function(x, ...) {
    x$pts
}

getData.ptprocInit <- getData.ptprocSim <- function(x, ...) {
    x$data
}

getData.ptprocFit <- function(x, ...) {
    getData(extract.ptprocInit(x))
}

ranges.ptprocInit <- ranges.ptprocSim <- function(object, ...) {
    object$ranges
}

ranges.ptprocFit <- function(object, ...) {
    ranges(extract.ptprocInit(object))
}

getCIF.ptprocInit <- getCIF.ptprocSim <- function(object, ...) {
    object$cond.int
}

getCIF.ptprocFit <- function(object, ...) {
    getCIF(extract.ptprocInit(object))
}

"setCIF<-.ptprocInit" <- function(x, value) {
    if(!is.function(value))
        stop("Right hand side should be a function")
    x$cond.int <- value
    x
}

fixed.ptprocInit <- function(x, ...) {
    x$fixed
}

fixed.ptprocFit <- function(x, ...) {
    fixed(extract.ptprocInit(x))
}

"fixed<-.ptprocInit" <- function(x, value) {
    if(length(value) == 1)
        x$fixed <- rep(value, length(coef(x)))
    else        
        x$fixed <- value
    x
}

params.default <- function(x, ...) {
    x$params
}

"params<-.ptprocInit" <- "params<-.ptprocSim" <- function(x, value) {
    x$params <- value
    x
}


condition.ptprocInit <- function(object, ...) {
    object$condition
}

condition.ptprocFit <- function(object, ...) {
    condition(extract.ptprocInit(object))
}

"condition<-.ptprocInit" <- function(x, value) {
    if(!is.expression(value) && !is.null(value))
        stop("Right hand side needs to be an R expression")
    x$condition <- value
    x
}

as.ptprocInit.ptprocFit <- function(x, ...) {
    structure(list(pts = getPts(x),
                   cond.int = getCIF(x),
                   params = params(x),
                   fixed = fixed(x),
                   ranges = ranges(x), 
                   condition = condition(x),
                   data = getData(x),
                   is.pois = x$is.pois),
              class = c("ptprocInit", "ptproc"))
}


as.ptprocSim.ptprocFit <- function(x, ...) {
    structure(list(cond.int = getCIF(x),
                   params = params(x),
                   ranges = ranges(x),
                   model = extract.ptprocInit(x)$model,
                   data = getData(x),
                   is.pois = extract.ptprocInit(x)$is.pois),
              class = c("ptprocSim", "ptproc"))
}

as.ptprocSim.ptprocInit <- function(x, ...) {
    structure(list(cond.int = getCIF(x),
                   params = params(x),
                   ranges = ranges(x),
                   model = x$model,
                   data = getData(x),
                   is.pois = x$is.pois),
              class = c("ptprocSim", "ptproc"))
}

