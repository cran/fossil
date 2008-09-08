`earth.poly` <-
function(x,plot.map=FALSE,...) {
    x<-unique(x)
    rownames.x<-rownames(x)
    nr <- nrow(x)
    bm <- matrix(, nr, nr)
    for (i in 1:nr) {
        for (j in 1:nr) {
            if (i == j)
                bm[i, j] <- -1
            else bm[i, j] <- earth.bear(x[i, 1], x[i, 2], x[j,
                1], x[j, 2])
        }
    }
    qm <- matrix(, nr, 4)
    for (i in 1:nr) {
        qm[i, 1] <- length(bm[1, ][bm[i, ] >= 0]) - length(bm[i,
            ][bm[i, ] >= 90])
        qm[i, 2] <- length(bm[1, ][bm[i, ] >= 90]) - length(bm[i,
            ][bm[i, ] >= 180])
        qm[i, 3] <- length(bm[1, ][bm[i, ] >= 180]) - length(bm[i,
            ][bm[i, ] >= 270])
        qm[i, 4] <- length(bm[1, ][bm[i, ] >= 270])
    }
    id <- numeric(nr)
    for (i in 1:nr) {
        for (j in 1:4) {
            if (length(qm[i, ][qm[i, ] > 0]) <= 2)
                id[i] <- i
            else {
                if (length(qm[i, ][qm[i, ] > 0]) == 3) {
                  if (length(qm[i, 2:3][qm[i, 2:3] > 0]) == 2) {
                    if ((max(bm[i, ]) - min(bm[i, ][bm[i, ] >=
                      0])) < 180)
                      id[i] <- i
                  }
                  else {
                    if ((360 - min(bm[i, ][bm[i, ] > 180]) +
                      max(bm[i, ][bm[i, ] < 180])) < 180)
                      id[i] <- i
                  }
                }
            }
        }
    }
    mi <- id[id > 0]
    prpt <- mi[1]
    snpts <- mi[-1]
    pt <- bm[prpt, snpts]
    if ((max(pt) - min(pt)) > 180) {
        pt[pt < 180] <- pt[pt < 180] + 360
        rpt <- rank(pt)
    }
    else rpt <- rank(pt)
    ind <- numeric(length(pt))
    for (i in 1:length(pt)) ind[rpt[i]] <- snpts[i]
    ta <- numeric(1)
    for (i in 1:(length(pt) - 1)) {
        k <- i + 1
        ta <- ta + earth.tri(x[prpt, 1], x[prpt, 2], x[ind[i],
            1], x[ind[i], 2], x[ind[k], 1], x[ind[k], 2])
    }
    if (plot.map == TRUE) {
        loc.map(x, ...)
        lp <- c(prpt, ind)
        polygon(x[lp, 2], x[lp, 1])
    }
    attr(ta, "vertices") <- rownames.x[mi]
    return(ta)
}

