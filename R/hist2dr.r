new_hist2dr <- function(
    hist = table(NULL), 
    xbrks = numeric(0), 
    ybrks = numeric(0), 
    xbins = factor(),
    ybins = factor()
) {

    H <- structure(
        list(
            hist = hist,
            xbrks = xbrks,
            ybrks = ybrks,
            xbins = xbins,
            ybins = ybins
        ),
        class = "hist2dr"
    )

    return(H)

}

#' @export
hist2dr <- function(x, y, xbrks, ybrks) {

    xBins <- factor(
        findInterval(x, xbrks, all.inside = TRUE), 
        levels = 1:(length(xbrks) - 1) # bins = breaks - 1
    )
    yBins <- factor(
        findInterval(y, ybrks, all.inside = TRUE), 
        levels = 1:(length(ybrks) - 1) # bins = breaks - 1
    )

    H <- new_hist2dr(table(xBins, yBins), xbrks, ybrks, xBins, yBins)

    return(H)

}

#' @export
plot.hist2dr <- function(H, zbrks, clrs, xlab = "", ylab = "", gridAlpha = 0.05, ...) {

    par(pty = "s")
    image(H$xbrks, H$ybrks, H$hist, col = clrs, breaks = zbrks, xlab = xlab, ylab = ylab)

    gridcol <- scales::alpha("black", gridAlpha)
    abline(v = H$xbrks, col = gridcol)
    abline(h = H$ybrks, col = gridcol)

}

#' @export
plotColorLegend <- function(brks, clrs) {

    n <- length(brks) - 1
    bins <- sapply(1:n, function(i) {
        paste0("(", brks[i], ",", brks[i + 1], "]")
    })
    bins[1] <- gsub("\\(", "\\[", bins[1])

    plot(0, type = 'n', axes = FALSE, ann = FALSE)
    legend("center", legend = bins, fill = clrs)

}

