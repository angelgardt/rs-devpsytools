pgks <- c("mirt", "Rcpp")
install.packages(pkgs[!pkgs %in% installed.packages()])
# devtools::install_github("masurp/ggmirt")

library(tidyverse)
library(mirt)
library(ggmirt)


test1_data <- read_csv("test1.csv")
test2_data <- read_csv("test2.csv")

str(test1_data)
str(test2_data)
summary(test1_data)
summary(test2_data)

test1_data %>% select(-id, -group) -> test1
test2_data %>% select(-id, -group) -> test2

ncol(test1)
model1 <- 'F1 = 1-17'
fit3PL1 <- mirt(data = test1, 
               model = model1,
               itemtype = "3PL", 
               verbose = FALSE)

fit3PL1
summary(fit3PL1)

params3PL1 <- coef(fit3PL1, IRTpars = TRUE, simplify = TRUE)
round(params3PL1$items, 2)

M2(fit3PL1)
itemfit(fit3PL1)
itemfit(fit3PL1, fit_stats = "infit")

itemfitPlot(fit3PL1)
itempersonMap(fit3PL1)

tracePlot(fit3PL1)
tracePlot(fit3PL1, facet = F, legend = T) + scale_color_brewer(palette = "Set3")

itemInfoPlot(fit3PL1, facet = T)
itemInfoPlot(fit3PL1) + scale_color_brewer(palette = "Set3")

testInfoPlot(fit3PL1, adj_factor = 2)

scaleCharPlot(fit3PL1)
conRelPlot(fit3PL1)
marginal_rxx(fit3PL1)


ncol(test2)
model2 <- 'F1 = 1-20'
fit3PL2 <- mirt(data = test2, 
                model = model2,
                itemtype = "3PL", 
                verbose = FALSE)

test2 %>% select(-score4, -score10) -> test2

ncol(test2)
model2 <- 'F1 = 1-18'
fit3PL2 <- mirt(data = test2, 
                model = model2,
                itemtype = "3PL", 
                verbose = FALSE)

fit3PL2
summary(fit3PL2)

params3PL2 <- coef(fit3PL2, IRTpars = TRUE, simplify = TRUE)
round(params3PL2$items, 2)

M2(fit3PL2)
itemfit(fit3PL2)
itemfit(fit3PL2, fit_stats = "infit")

itemfitPlot(fit3PL2)
itempersonMap(fit3PL2)

tracePlot(fit3PL2)
itemInfoPlot(fit3PL2, facet = T)

testInfoPlot(fit3PL2, adj_factor = 2)
scaleCharPlot(fit3PL2)
conRelPlot(fit3PL2)
marginal_rxx(fit3PL2)
