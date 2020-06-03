library("RMThreshold")
library(corrplot)
## Import all communities from 1st window
c1_red = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_red_first_window')
c1_purple = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_purple_first_window')
c1_orange = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_orange_first_window')
c1_green = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_green_first_window')
c1_brown = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_brown_first_window')
c1_blue = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_blue_first_window')
c1_black= read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_black_first_window')
c1_Lgreen = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/First_window/Community_Lgreen_first_window')

## Import all communities from 2nd window
c2_red = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_red_second_window')
c2_purple = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_purple_second_window')
c2_orange = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_orange_second_window')
c2_green = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_green_second_window')
c2_brown = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_brown_second_window')
c2_blue = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_blue_second_window')
c2_black= read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_black_second_window')
c2_Lgreen = read.csv('/home/harshal/Works/Practicum_dataset/Communities_dataset/Second_window/Community_Lgreen_second_window')

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
corrplot(c1_red_cor, method = "shade")
corrplot(c2_red_cor, method = "shade")

es1 <- eigen(c1_red_cor, symmetric = TRUE, only.values = FALSE)
es1
es2 <- eigen(c2_red_cor, symmetric = TRUE, only.values = FALSE)
es2
