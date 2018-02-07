extract.ptprocInit <- function(x, ...) {
    ## Return "ptprocInit" object from fitted model
    UseMethod("extract.ptprocInit")
}

getPts <- function(x, ...) {
    ## Get the original event points
    UseMethod("getPts")
}

getData <- function(x, ...) {
    ## Get extra data object
    UseMethod("getData")
}

ranges <- function(object, ...) {
    UseMethod("ranges")
}

getCIF <- function(object, ...) {
    UseMethod("getCIF")
}

"setCIF<-" <- function(x, value) {
    UseMethod("setCIF<-")
}

fixed <- function(x, ...) {
    UseMethod("fixed")
}   

"fixed<-" <- function(x, value) {
    UseMethod("fixed<-")
}

params <- function(x, ...) {
    UseMethod("params")
}    

"params<-" <- function(x, value) {
    UseMethod("params<-")
}

condition <- function(object, ...) {
    ## Get conditions/constraints placed on optimization
    UseMethod("condition")
}

"condition<-" <- function(x, value) {
    ## Assign conditions (R expressions)
    UseMethod("condition<-")
}

as.ptprocInit <- function(x, ...) {
    UseMethod("as.ptprocInit")
}

as.ptprocSim <- function(x, ...) {
    UseMethod("as.ptprocSim")
}

evalCIF <- function(x, xpts, ...) {
    UseMethod("evalCIF")
}

integrateCIF <- function(x, TT, ...) {
    UseMethod("integrateCIF")
}

