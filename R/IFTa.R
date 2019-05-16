#' the method of instrument confounding variable
#' @name IFTa
#' @param X a vector X (concrete / continue)
#' @param Y a vector Y (concrete / continue) with the same length of X
#' @param M1 a vector M1 (binary) with the same length of X
#' @param M2 a vector M2 (binary) with the same length of X
#' @param R1 bootstrap times
#' @param R2 permutation times
#' @return vector
#' @export
#' @author houlei

IFTa <- function(X, Y, M1, M2, R1=1000, R2=1000) {

    data <- data.frame(X, Y, M1, M2)

    b3 <- boot(data, statistic = diff_se, R1)
    se <- sd(b3$t, na.rm = TRUE)

    b4 <- boot(data, statistic = diff_pval, R2)
    pvalue <- mean(abs(b4$t) > abs(b4$t0), na.rm = TRUE)

    rr <- c(b3$t0, se, pvalue)
    names(rr) <- c("beta", "se", "pvalue")

    return(rr)
}

diff_se <- function(d1, i) {

    fdata <- d1[i, ]
    beta <- IFT(fdata)

    return(beta)
}

diff_pval <- function(d1, i){
    fdata <- d1
    fdata$Y <- d1$Y[i]
    beta <- IFT(fdata)

    return(beta)
}

IFT <- function(fdata) {
    M1 <- fdata$M1
    M2 <- fdata$M2

    x10 <- mean(fdata$X[M1 == 1 & M2 == 0], na.rm = TRUE)
    x00 <- mean(fdata$X[M1 == 0 & M2 == 0], na.rm = TRUE)
    x01 <- mean(fdata$X[M1 == 0 & M2 == 1], na.rm = TRUE)
    x11 <- mean(fdata$X[M1 == 1 & M2 == 1], na.rm = TRUE)

    y10 <- mean(fdata$Y[M1 == 1 & M2 == 0], na.rm = TRUE)
    y00 <- mean(fdata$Y[M1 == 0 & M2 == 0], na.rm = TRUE)
    y01 <- mean(fdata$Y[M1 == 0 & M2 == 1], na.rm = TRUE)
    y11 <- mean(fdata$Y[M1 == 1 & M2 == 1], na.rm = TRUE)

    if(any(is.na(c(x00, x10, x11, x01)))){
        stop("invalid conditional expectation")
    } else if(any(is.na(c(y10, y11, y00, y01)))){
        stop("invalid conditional expectation")
    }else {
        a <- -y10 + y11 + y00 - y01
        b <- x00 - x10 + x11 - x01

        if (b == 0) stop("matrix nonrank")
        else {
            beta <- a / b
            return(beta)
        }
    }
}
