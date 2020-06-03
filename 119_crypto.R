library(tawny)
library(corrplot)
library(ggplot2)
require(ggplot2)
#install.packages("ape")
#install.packages("emstreeR")
require(emstreeR)
require(stats)
require(ape)
library(ape)
library(emstreeR)
#install.packages("igraph")
#library("igraph")
#detach("package:igraph", unload=TRUE)
#install.packages("RMThreshold")
library("RMThreshold")
#install.packages("plotly")
library("plotly")
#crypto_119 = read.csv('/home/harshal/Works/Practicum_dataset/119_crypto')
crypto_119 = read.csv('/home/harshal/Works/Practicum_dataset/60_crypto_150')
## 119 CCurrency
crypto_119$X <- NULL
crypto_119
return_crypto_119 <- apply(crypto_119,2,function(x) diff(log2(x), lag=1))
corrplot(cor(return_crypto_119), method = "shade")

## 119 Correlation matrix
crypto_119_cor <- cor(crypto_119)
#crypto_119_cor <- cor(return_crypto_119)
crypto_119_cor

corrplot(crypto_119_cor, method = "shade")
#plot(density(crypto_119_cor))
#plot(density(cor(return_crypto_119)))

## 119 CCs Eigenvalues
es <- eigen(crypto_119_cor, symmetric = TRUE, only.values = FALSE)
e.values <- es$values
e.vectors <- es$vectors
e.values
e.vectors
## End
write.csv(e.vectors, file = "119_evectors.csv")
## EigenSpectrum
rm.ev.density(em$values, nr.breaks = 200)
rm.ev.density(e.values, nr.breaks = 150, wigner=F)
axis(1, seq(0,25, by=1))

## Findings of EigenValue Distribution
### Calculate the Bulk of Eigenvalues
Q = 150/59  ## Q = L/N where, L is length of time and N is number of CCs
lambda_min = 1 + 1/Q - 2/sqrt(Q) # 0.01
lambda_max = 1 + 1/Q + 2/sqrt(Q) # 3.59
### 3rd, 4rd and 47th Eigenvalues are of interest


## Normalize the vectors
new_vectors <- e.vectors

for (i in 1:59) {
  sum = sum(new_vectors[,i])
  for (j in 1:59){
    new_vectors[j,i] = new_vectors[j,i]/sum
  }
} 

new_vectors

hist(new_vectors[,14], col=rgb(0,0,1,0.5), prob=T, breaks = 30, main="Second-Largest-Eigenvalue", xlab = "Eigenvector-components")
curve(dnorm(x, mean=mean(new_vectors[,6]), sd=sd(new_vectors[,6])), add=TRUE, lwd=2)
hist(new_vectors[,2], col=rgb(1,0,0,0.5), prob=T, breaks = 30, add=T)
curve(dnorm(x, mean=mean(new_vectors[,2]), sd=sd(new_vectors[,2])), add=TRUE, lwd=2)

legend("topright", legend=c("Lambda-7","Lambda-2"), col=c(rgb(0,0,1,0.5),
                                                          rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


hist(e.vectors[,117], col=rgb(0,0,1,0.5), breaks = 40, main="Non-Normalized-Evector-1")
hist(e.vectors[,3], col=rgb(1,0,0,0.5), breaks = 30, add=T)
legend("topright", legend=c("Bulk-Lambda-2","Lambda-3"), col=c(rgb(0,0,1,0.5),
                                                               rgb(1,0,0,0.5)), pt.cex=2, pch=15 )

ks.test(new_vectors[,2],new_vectors[,14])
ks.test(e.vectors[,2],e.vectors[,20])



## Eigenvector Components
### some value from the bulk lambda- < lambda > lambda+
vector_den<-density(new_vectors[,6], bw = "SJ", kernel = c("gaussian"))
hist(e.vectors[,1], breaks = 50)
lines(vector_den, col = "red", lwd = 1.5)



## Inverse Participation Ratio
IPR <- vector(mode="numeric", length=59)  ## Initialize Empty Vector

## Calculate IPR using Formula Sum of Each Vector Components to the power 4
for (i in 1:59){
  IPR[i] = 0
  IPR[i]=IPR[i]+sum(e.vectors[,i]^4)
}
## Calculate Average Inverse Participation Ratio
sum(IPR)/59 ## 0.08 Avearge value of IPR

plot(e.values, IPR, type = "b")
abline(h = 0.12)
axis(2, seq(0,0.9, by = 0.01))

## Plotly plot
data<- data.frame(e.values, IPR)
plot_ly(data, x=e.values, y= IPR, mode = 'lines+markers') %>%
  layout(xaxis = list(range = c(-5,30)))



## MST start
crypto_60_150_corr <- crypto_119_cor
dist_mat <- crypto_60_150_corr
#dist_mat2 <- crypto_60_150_corr

## Distance formula d(i,j) = 1-p(i,j)^2
for (i in 1:119){
  for (j in 1:119){
    dist_mat[i,j] = (1-crypto_60_150_corr[i,j]^2)  
  }
  
}
## Distance formula d(i,j) = sqrt(2(1-p(ij))
for (i in 1:119){
  for (j in 1:119){
    dist_mat2[i,j] = sqrt(2*(1-crypto_60_150_corr[i,j]))
  }
  
}
## Distance matrix for MST
dist_mat

dim(dist_mat) <- c(rownames(crypto_60_150_corr),colnames(crypto_60_150_corr))
## To see row names
row1 <- rownames(dist_mat)
col1 <- colnames(dist_mat)
## Calculate MST
minTree <- mst(dist_mat)
minTree
## Plot MST
abc <- plot(minTree, graph="nsca")

## Create matrix from mst
minTree_mat <- matrix(minTree, nrow = 119, ncol = 119)

## mask names for rows and columns in the created matrix
rownames(minTree_mat) <- row1
colnames(minTree_mat) <- col1
minTree_mat
## Store matrix into csv file
write.csv(minTree_mat, "~/minTreeMat.csv")

row1

