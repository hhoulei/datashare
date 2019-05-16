
library(devtools)
install_github("hhoulei/IFT")
library(datashare)

###test
DataGeneration <- function(N,p1,p2,a0,a1,a2,a3,r,b0,b1,b2,b3,b4){
  M1 <- rbinom(N,1,p1)
  M2 <- rbinom(N,1,p2)
  U <- runif(N,-1,1)
  X <- a0 + a1*M1 + a2*M2 + a3* U + r*M1*M2 + rnorm(N,0,1)
  Y <- b0 + b1*X + b2*M1 + b3*M2 + b4*U + rnorm(N,0,1)
  
  data_simulation <- data.frame(X,M1,M2,U,Y)
  return(data_simulation)
}
NN=100;N=1000;p1=0.5;p2=0.5;a0=0.1;a1=0.1;a2=0.1;a3=0.1;r=0.2;b0=0.1;b1=0.5;b2=0.1;b3=0.1;b4=0.1;R1=100;R2=100
fdata <- DataGeneration(N,p1,p2,a0,a1,a2,a3,r,b0,b1,b2,b3,b4)
fdataa <- fdata[order(abs(fdata$U)),]
nn <- seq(1, (N+1), N/5)
fdt <- list()
for(m in 1:(length(nn)-1)){
  fdt[[m]] <- fdataa[nn[m]:(nn[m+1]-1),]
}
Xa <- Ya <- M1a <- M2a <- NULL
for(i in 1:length(fdt)){
  Xa <- cbind(Xa, fdt[[i]]$X)
  Ya <- cbind(Ya, fdt[[i]]$Y)
  M1a <- cbind(M1a, fdt[[i]]$M1)
  M2a <- cbind(M2a, fdt[[i]]$M2)
}
tt <-datashare(Xa, Ya, M1a, M2a,10,10)
