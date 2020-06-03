#Crypto Stuff
install.packages('tawny')
install.packages('corrplot')
install.packages('ggplot2')
options(max.print=1000)
library(tawny)
library(corrplot)
crypto_dataset = read.csv('/home/harshal/Works/Practicum_dataset/pmohun-complete-historical-cryptocurrency-financial-data/data/consolidated_coin_data.csv')

crypto_60 = read.csv('/home/harshal/Works/Practicum_dataset/60_crypto')
str(crypto_dataset)
summary(crypto_dataset)
head(crypto_dataset)

# (Bitcoin, Litecoin, Ripple, Ethereum, and Dash based on the market capital)
summary(crypto_dataset$Currency == 'bitcoin')
summary(crypto_dataset$Currency == 'litecoin')
summary(crypto_dataset$Currency == 'ripple')
summary(crypto_dataset$Currency == 'ethereum')
summary(crypto_dataset$Currency == 'dash')

bitcoin = read.csv('/home/harshal/Works/Practicum_dataset/bitcoin')
litecoin = read.csv('/home/harshal/Works/Practicum_dataset/litecoin')
ripple = read.csv('/home/harshal/Works/Practicum_dataset/ripple')
dash = read.csv('/home/harshal/Works/Practicum_dataset/dash')
ethereum  = read.csv('/home/harshal/Works/Practicum_dataset/ethereum')
bitcoin$X <- NULL
litecoin$X <- NULL
ripple$X <- NULL
dash$X <- NULL
ethereum$X <- NULL


require(tawny)
data(sp500.subset)

h <- sp500.subset
str(h)
p <- tawny.types::TawnyPortfolio(h,150)
p
r1 <- denoise(p, SampleDenoiser())
r2 <- denoise(p, EmpiricalDenoiser())
r3 <- denoise(p, ShrinkageDenoiser())
r4 <- denoise(p, RandomMatrixDenoiser())
head(r4)


ws.1 <-optimizePortfolio(h, 150, RandomMatrixDenoiser())
head(ws.1[,1:5])

corrplot(r4, method = "shade" )
matrix = as.data.frame(r4)
colnames(matrix) <- "pearson" 
library(ggplot2)
require(ggplot2)
pearson.p <- function(r, n) {
  pofr <- ((1-r^2)^((n-4)/2))/beta(a = 1/2, b = (n-2)/2)
  return(pofr)
}
g <- NULL
g <- ggplot(data = matrix, mapping = aes(x=pearson))
g <- g + xlim(-1,1)  # actual limits of pearsons r
g <- g + geom_histogram(mapping = aes(y = ..density..))
g <- g + stat_function(fun = pearson.p, colour = "red", args = list(n = nrow(matrix)))
g

install.packages('ppcc')
library(ppcc)

qn <- ppccTest(r4, "qnorm")
qn
abc <- dnorm(r4,-1,1)
head(abc[,0:5])
hist(r4)
lines(r4, col = "red")


## 5 CCs
bitcoin_new <-data.frame(bitcoin$Date, bitcoin$Close)
#bitcoin_new
litecoin_new <- data.frame(litecoin$Date, litecoin$Close)
dash_new <- data.frame(dash$Date, dash$Close)
ethereum_new <- data.frame(ethereum$Date, ethereum$Close)
ripple_new <- data.frame(ripple$Date, ripple$Close)

bitcoin$Close[1:500]
bitcoin_new

close_16_18 <- data.frame(bitcoin$Close[1:500], litecoin$Close[1:500], dash$Close[1:500], ethereum$Close[1:500], ripple$Close[1:500])
close_16_18

cor_matrix <- cor(close_16_18)
cor_matrix

mat <- cor_matrix
den_close_16_18 <- denoise(close_16_18, RandomMatrixDenoiser())
cor.clean(es,lambda.plus = 0.78, h=NULL)
den_mat
mat
## End


## Eigen stuff
optimizePortfolio()
p.optimize(close_16_18, den_mat)

colnames(close_16_18)

es <- cor.empirical(mat)
new_mat <- cor.clean(es, lambda.plus = 2 ,h = NULL)
corrplot(new_mat, method = "shade")
es <- eigen(mat, symmetric=TRUE, only.values=FALSE)

e.values <- es$values
e.vectors <- es$vectors
e.values
## End 

## 60 CCurrency
crypto_60$X <- NULL
crypto_60
## 60 Correlation matrix
crypto_60_cor <- cor(crypto_60)
crypto_60_cor

corrplot(crypto_60_cor, method = "shade")
## 60 CCs Eigenvalues
es <- eigen(crypto_60_cor, symmetric = TRUE, only.values = FALSE)
e.values <- es$values
e.vectors <- es$vectors
e.values
## End


## 60 CCs Auto denoise stuff
new <- dnorm(e.values)
plot(e.values, new, type='s')
hist(e.values, freq=T)

den_crypto_60 <- denoise(crypto_60_cor, RandomMatrixDenoiser())
corrplot(den_crypto_60, method = "shade")
e.values



#install.packages("RMThreshold")
library("RMThreshold")
#install.packages("plotly")
library("plotly")

## RMThreshold on Crypto
set.seed(777)
res <- rm.matrix.validation(crypto_60_cor) # ok
res <- rm.get.threshold(crypto_60_cor) # threshold about 3.19
rm.show.plots(res$comparison.plots)
cleaned.matrix <- rm.denoise.mat(crypto_60_cor, threshold = 0.76)
cleaned.matrix <- rm.discard.zeros(cleaned.matrix)
cleaned.matrix

corrplot(cleaned.matrix, method = "shade")
corrplot(crypto_60_cor, method = "shade")
es <- eigen(crypto_60_cor, symmetric = T, only.values = F)
es$values
plot(es$values, type = 's')
## Example RMT Threshold
set.seed(777)
random.mat <- create.rand.mat(size = 60, distrib = c("unif"))$rand.matrix
random.mat <- as.matrix(random.mat)
random.mat

corrplot(random.mat, method = "shade")

res <- rm.matrix.validation(random.mat) # ok
res <- rm.get.threshold(random.mat) # threshold about 3.19
rm.show.plots(res$comparison.plots)
cleaned.matrix <- rm.denoise.mat(random.mat, threshold = 0.78)
cleaned.matrix <- rm.discard.zeros(cleaned.matrix)
cleaned.matrix
corrplot(cleaned.matrix, method = "shade")  

em <- eigen(random.mat)

ln <- diff(log(crypto_60$dash), lag = 1)
plot(ln, type='l')
plot(crypto_60$dash, type='l')


## Fresh start
rm.ev.density(em$values, nr.breaks = 200)
rm.ev.density(e.values, nr.breaks = 300)
axis(1, seq(0,59, by=1))
## Findings of EigenValue Distribution
### Calculate the Bulk of Eigenvalues
Q = 400/59  ## Q = L/N where, L is length of time and N is number of CCs
lambda_min = 1 + 1/Q - 2/sqrt(Q) # 0.37
lambda_max = 1 + 1/Q + 2/sqrt(Q) # 1.91
### 3rd, 4rd and 47th Eigenvalues are of interest

## Normalize the vectors
new_vectors <- e.vectors

for (i in 1:59) {
  sum = sum(new_vectors[,i])
  for (j in 1:59){
    new_vectors[j,i] = new_vectors[j,i]/sum
  }
} 
hist(new_vectors[,2], col=rgb(0,0,1,0.5), prob=T, breaks = 40, main="Normalized-lambda-1")
curve(dnorm(x, mean=mean(new_vectors[,2]), sd=sd(new_vectors[,2])), add=TRUE, lwd=2)
hist(new_vectors[,3], col=rgb(1,0,0,0.5), prob=T, breaks = 40, add=T)
curve(dnorm(x, mean=mean(new_vectors[,3]), sd=sd(new_vectors[,3])), add=TRUE, lwd=2)

legend("topright", legend=c("Lambda-2","Lambda-3"), col=c(rgb(0,0,1,0.5),
      rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


hist(e.vectors[,1], col=rgb(0,0,1,0.5), breaks = 40, main="Non-Normalized-Evector-1")
hist(e.vectors[,3], col=rgb(1,0,0,0.5), breaks = 30, add=T)
legend("topright", legend=c("Bulk-Lambda-2","Lambda-3"), col=c(rgb(0,0,1,0.5),
     rgb(1,0,0,0.5)), pt.cex=2, pch=15 )

ks.test(new_vectors[,3],new_vectors[,47])
ks.test(e.vectors[,3],e.vectors[,47])


## Import some new libs for viz
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")



## Eigenvector Components
### some value from the bulk lambda- < lambda > lambda+
vector_den<-density(e.vectors[,47], bw = "SJ", kernel = c("gaussian"))
hist(e.vectors[,1], breaks = 60)
lines(vector_den, col = "red", lwd = 1.5)


## Interesting Eigenvectors
hist(e.vectors[,2], breaks = 20)
hist(e.vectors[,4], breaks = 20)
hist(e.vectors[,5], breaks = 50)
axis(1, seq(-2, 2, by = 0.2))
hist(e.vectors[,47], breaks = 20)

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
abline(h = 0.08)
axis(2, seq(0,0.9, by = 0.01))

## Plotly plot
data<- data.frame(e.values, IPR)
plot_ly(data, x=e.values, y= IPR, mode = 'lines+markers') %>%
        layout(xaxis = list(range = c(-5,59)))


sum(e.vectors[,3])
