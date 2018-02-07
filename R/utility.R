## Some simple functions for 1-d residual diagnostics and other things

logSurv <- function(res, theoretical = TRUE, xlab = "Interevent time",
                    ylab = "Cumulative number", ...) {
    u <- diff(res)
    plot(sort(u), length(u):1, log="y", xlab = xlab, ylab = ylab,
         main="Log Survivor Plot\nof Interevent Times", ...)
    
    if(theoretical) 
        lines(sort(u), length(u) * (1 - pexp(sort(u), attr(res, "rate"))))
    invisible()
}


stationarity <- function(res, h, std = TRUE, xlab = "Transformed time",
                         ylab="Standardized # of events per interval",
                         type = "b", ...) {
    nbins <- ceiling((max(res) - min(res)) / h)
    breaks <- seq(min(res), max(res), len = nbins)
    b <- hist(res, breaks, plot = FALSE)$counts
    std.hi <- 1:3
    std.lo <- -std.hi
    m <- h * attr(res, "rate")
    scaled.freq <- (b - m) / sqrt(m)
    plot(breaks[-length(breaks)], scaled.freq,
         ylim = range(c(std.hi, std.lo, scaled.freq)),
         xlab = xlab, ylab = ylab,
         main = paste("Stationarity Plot\nh = ", h, sep=""),
         type = type, ...)
    if(std)
        abline(h=c(std.hi,std.lo), lty=3)
    invisible()
}


        
make.box <- function(ppobj, idx = 1:2) {
    ranges <- ranges(ppobj)
    a <- c(ranges[,idx[1]], rev(ranges[,idx[1]]))
    b <- c(rep(ranges[1,idx[2]],2),rep(ranges[2,idx[2]],2))
    cbind(a,b)
}

