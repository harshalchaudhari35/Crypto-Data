library("RMThreshold")
library(corrplot)
## Import all Combined from 1st window
c1_red = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_red_first_window')
c1_purple = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_purple_first_window')
c1_orange = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_orange_first_window')
c1_green = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_green_first_window')
c1_brown = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_brown_first_window')
c1_blue = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_blue_first_window')
c1_black= read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_black_first_window')
c1_Lgreen = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/First_window/Combined_Community_Lgreen_first_window')

## Import all communities from 2nd window
c2_red = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_red_second_window')
c2_purple = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_purple_second_window')
c2_orange = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_orange_second_window')
c2_green = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_green_second_window')
c2_brown = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_brown_second_window')
c2_blue = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_blue_second_window')
c2_black= read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_black_second_window')
c2_Lgreen = read.csv('/home/harshal/Works/Practicum_dataset/Combined_Communities/Second_window/Combined_Community_Lgreen_second_window')

## Cleanup Variables
c1_red$X <- NULL
c1_purple$X <- NULL
c1_orange$X <- NULL
c1_green$X <- NULL
c1_brown$X <- NULL
c1_blue$X <- NULL
c1_black$X <- NULL
c1_Lgreen$X <- NULL

c2_red$X <- NULL
c2_purple$X <- NULL
c2_orange$X <- NULL
c2_green$X <- NULL
c2_brown$X <- NULL
c2_blue$X <- NULL
c2_black$X <- NULL
c2_Lgreen$X <- NULL
## Correlation of communities
c1_red_cor = cor(c1_red)
c1_purple_cor = cor(c1_purple)
c1_orange_cor = cor(c1_orange)
c1_green_cor = cor(c1_green)
c1_brown_cor = cor(c1_brown)
c1_blue_cor = cor(c1_blue)
c1_black_cor = cor(c1_black)
c1_Lgreen_cor = cor(c1_Lgreen)

c2_red_cor = cor(c2_red)
c2_purple_cor = cor(c2_purple)
c2_orange_cor = cor(c2_orange)
c2_green_cor = cor(c2_green)
c2_brown_cor = cor(c2_brown)
c2_blue_cor = cor(c2_blue)
c2_black_cor = cor(c2_black)
c2_Lgreen_cor = cor(c2_Lgreen)

## Correlation plot

par(mfrow=c(1,2))
corrplot(c1_red_cor, method = "shade", title = "w1_red", mar=c(0,0,2,0))
corrplot(c2_red_cor, method = "shade", title = "w2_red", mar=c(0,0,2,0))

corrplot(c1_purple_cor, method = "shade", title = "w1_purple", mar=c(0,0,2,0))
corrplot(c2_purple_cor, method = "shade", title = "w2_purple", mar=c(0,0,2,0))

corrplot(c1_blue_cor, method = "shade", title = "w1_blue", mar=c(0,0,2,0))
corrplot(c2_blue_cor, method = "shade", title = "w2_blue", mar=c(0,0,2,0))

corrplot(c1_orange_cor, method = "shade", title = "w1_orange", mar=c(0,0,2,0))
corrplot(c2_orange_cor, method = "shade", title = "w2_orange", mar=c(0,0,2,0))

corrplot(c1_green_cor, method = "shade", title = "w1_green", mar=c(0,0,2,0))
corrplot(c2_green_cor, method = "shade", title = "w2_green", mar=c(0,0,2,0))

corrplot(c1_black_cor, method = "shade", title = "w1_black", mar=c(0,0,2,0))
corrplot(c2_black_cor, method = "shade", title = "w2_black", mar=c(0,0,2,0))

corrplot(c1_brown_cor, method = "shade", title = "w1_brown", mar=c(0,0,2,0))
corrplot(c2_brown_cor, method = "shade", title = "w2_brown", mar=c(0,0,2,0))

corrplot(c1_Lgreen_cor, method = "shade", title = "w1_Lgreen", mar=c(0,0,2,0))
corrplot(c2_Lgreen_cor, method = "shade", title = "w2_Lgreen", mar=c(0,0,2,0))



## Get Eigen
red1 <- eigen(c1_red_cor, symmetric = TRUE, only.values = FALSE)
red2 <- eigen(c2_red_cor, symmetric = TRUE, only.values = FALSE)

purple1 <- eigen(c1_purple_cor, symmetric = TRUE, only.values = FALSE)
purple2 <- eigen(c2_purple_cor, symmetric = TRUE, only.values = FALSE)

blue1 <- eigen(c1_blue_cor, symmetric = TRUE, only.values = FALSE)
blue2 <- eigen(c2_blue_cor, symmetric = TRUE, only.values = FALSE)

orange1 <- eigen(c1_orange_cor, symmetric = TRUE, only.values = FALSE)
orange2 <- eigen(c2_orange_cor, symmetric = TRUE, only.values = FALSE)

black1 <- eigen(c1_black_cor, symmetric = TRUE, only.values = FALSE)
black2 <- eigen(c2_black_cor, symmetric = TRUE, only.values = FALSE)

brown1 <- eigen(c1_brown_cor, symmetric = TRUE, only.values = FALSE)
brown2 <- eigen(c2_brown_cor, symmetric = TRUE, only.values = FALSE)

green1 <- eigen(c1_green_cor, symmetric = TRUE, only.values = FALSE)
green2 <- eigen(c2_green_cor, symmetric = TRUE, only.values = FALSE)

Lgreen1 <- eigen(c1_Lgreen_cor, symmetric = TRUE, only.values = FALSE)
Lgreen2 <- eigen(c2_Lgreen_cor, symmetric = TRUE, only.values = FALSE)

## Check Eigen Changes
red2$values - red1$values
purple2$values - purple1$values
orange2$values - orange1$values
blue2$values - blue1$values
green2$values - green1$values
black2$values - black1$values
brown2$values - brown1$values
Lgreen2$values - Lgreen1$values
# 