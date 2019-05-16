#' meta multicenter data
#' @name datashare
#' @param Xa a dataframe/matrix of multicenter data, including X (concrete / continue)
#' @param Ya a dataframe/matrix of multicenter data with the same dim of Xa, including Y (concrete / continue)
#' @param M1a a dataframe/matrix of multicenter data with the same dim of Xa, including M1 binary
#' @param M2a a dataframe/matrix of multicenter data with the same dim of Xa, including M2 binary
#' @param R1 bootstrap times
#' @param R2 permutation times
#' @importFrom metafor rma
#' @importFrom boot boot
#' @return vector
#' @export
#' @author houlei

datashare <- function(Xa, Ya, M1a, M2a,  R1=1000, R2=1000){

  if(!(ncol(Ya) == ncol(Xa) & ncol(M1a) == ncol(Xa) & ncol(M2a) == ncol(Xa))){

    stop("inconsistent number of centers")

  } else{

    result <- NULL
    for(n in 1:ncol(Xa)) {
      data1 <- data.frame(X = Xa[,n],
                          Y = Ya[,n],
                          M1 = M1a[,n],
                          M2 = M2a[,n])
      m12 <- apply(data1[,3:4], 1, function(x) any(is.na(x)))
      if (all(m12)) {
        stop(paste0("center ", n, " has too many missing values"))
        continue
      } else if (any(m12)) {
        data1 <- data1[!m12, ]
        warnings("remove rows with missing values")
        dd <- IFTa(X = data1$X, Y = data1$Y, M1 = data1$M1, M2 = data1$M2, R1 = R1, R2 = R2)
      } else {
        dd <- IFTa(X = data1$X, Y = data1$Y, M1 = data1$M1, M2 = data1$M2, R1 = R1, R2 = R2)
      }
      result <- rbind(result, c(dd[1], dd[2]))
    }
    result1 <- rma(result[, 1], result[, 2])

    return(result1)
  }
}
