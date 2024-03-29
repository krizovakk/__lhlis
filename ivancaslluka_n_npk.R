# 04/2021
# Využít linear-plateau (LP) model, ale na ose X by nebyly varianty hnojení, ale roky. 
# Chtěl bych dokázat, že změna klimatu ovlivňuje různé lokality jinak. Někde pozitivně, jinde negativně. 

# Můžu Vás poprosit i o analýzu vlivu hnojení? Pro každou lokalitu (Ivanovice, Čáslav, Lukavec) by byly dva grafy, 
# v jednom grafu by byly varianty Control, NPK1, NPK2 a NPK3, v druhém pak Control, N1 a N2. 
# Klasicky, jako v předchozích článcích, na ose x varianty hnojení, na ose y pak výnosy.

# 1980-2018
# loc: Ivanovice, Caslav, Lukavec

# x = dose
# y = yield

# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("minpack.lm")
# install.packages("extrafont")

require(tidyverse) #zakladni balik  
require(readxl) #nacitani excelovych tabulek
require(reshape2) #prevod na long format
require(easynls) #linplat model auto
library(minpack.lm) # nlsLM = more reliable model - singular gradient solution (https://gradcylinder.org/linear-plateau/)
library(rcompanion) #plotPredy / manual linplat
library(extrafont)

font_import()
yloadfonts(device="win")        #Register fonts for Windows bitmap output
fonts()                       #vector of font family names


windowsFonts(A = windowsFont("Times New Roman")) # https://statisticsglobe.com/change-font-of-plot-in-r


# ivan <- read_excel("red/weather.xlsx", sheet = 1)
# casl <- read_excel("red/weather.xlsx", sheet = 2) 
# luka <- read_excel("red/weather.xlsx", sheet = 3) 

n <- read_excel("red/N.xlsx", sheet = 1) # N+NPK in one file

n$year <- as.factor(n$year)
n$loc <- as.factor(n$loc)
n$var <- as.factor(n$var)

# EASY NLS / linplat model AUTO -------------------------------------------------------------

npkdose <- npk %>%
  mutate(dose = ifelse(var == "NPK1", 40,
                       ifelse(var == "NPK2", 80,
                              ifelse(var == "NPK3", 120, 0))))
ivan_m <- npkdose %>% 
  filter(loc == "Ivanovice") %>% 
  select(dose, yield) %>% 
  group_by(dose)

# LINPLAT auto

# install.packages("easynls")
require(easynls)

df_ivan <- data.frame(ivan_m$dose, ivan_m$yield)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_ivan, model=1) #linear
nlsplot(df_ivan, model=2) #quadratic
nlsplot(df_ivan, model=3) #linear-plateau

## RESULTS

nlsfit(df_ivan, model=3)

nlsplot(df_ivan, model=3, xlab = "Year", ylab = "Yield [t ha-1]")

# LINPLAT MANUAL ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

# install.packages("rcompanion") # quite long installation, 5 minutes
library(rcompanion) # for plot part

# Ivanovice

ivanN <- n %>% 
  filter(loc == "Ivanovice")

library(minpack.lm)

lp = function(x, a, b, c) {
  if_else(condition = x < c,
          true = a + b * x,
          false = a + b * c)}

ini_fit <- lm(data = ivanN, formula = yield ~ dose)
ini_a <- ini_fit$coef[[1]]
ini_b <- ini_fit$coef[[2]]
ini_c <- mean(ivanN$dose)

model_ivanN <<- nlsLM(
  formula = yield ~ lp(dose, a, b, c),
  data = ivanN,
  start = list(a = ini_a, b = ini_c, c = ini_c)
)

summary(model_ivanN)

par(mar=c(4,5,3,2))
plotPredy(data  = ivanN,
          x     = dose,
          y     = yield,
          model = model_ivanN,
          # main  = "Ivanovice",
          xlab  = expression("N dose [ kg "~ha^'-1'~"]"),
          ylab  = expression("Grain Yield [ t "~ha^'-1'~"]"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "A")
title("Ivanovice", adj = 0, line = 1, cex.main = 1.3, family = "A")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = ivanN$dose, labels = ivanN$dose, 
     las = 1, cex.axis = 1.3, family = "A")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 4.593+0.106[x−30.934]", side = 3, line = 1,                # data fetched from model summary
      outer = FALSE, cex = 1.5, col = "blue", family = "A")

# dev.copy(device = png, filename = 'plots/ivanovice_n.png', 
#          width = 600, height = 400) 
dev.copy(device = png, filename = 'plots/ivanovice_n.png', 
         width = 1600, height = 1000, res=200) 
dev.off()

# Caslav

caslN <- n %>% 
  filter(loc == "Caslav")

ini_fit <- lm(data = caslN, formula = yield ~ dose)
ini_a <- ini_fit$coef[[1]]
ini_b <- ini_fit$coef[[2]]
ini_c <- mean(caslN$dose)

model_caslN <<- nlsLM(
  formula = yield ~ lp(dose, a, b, c),
  data = caslN,
  start = list(a = ini_a, b = ini_c, c = ini_c)
)

summary(model_caslN)

par(mar=c(4,5,3,2))
plotPredy(data  = caslN,
          x     = dose,
          y     = yield,
          model = model_caslN,
          # main  = "Caslav",
          xlab  = expression("N dose [ kg "~ha^'-1'~"]"),
          ylab  = expression("Grain Yield [ t "~ha^'-1'~"]"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "A")
title("Caslav", adj = 0, line = 1, cex.main = 1.3, family = "A")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = caslN$dose, labels = caslN$dose, 
     las = 1, cex.axis = 1.3, family = "A")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 4.210+0.077[x-32.034]", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "A")

dev.copy(device = png, filename = 'plots/caslav_n.png', 
         width = 1600, height = 1000, res=200)
dev.off()

# Lukavec

lukaN <- n %>% 
  filter(loc == "Lukavec")

ini_fit <- lm(data = lukaN, formula = yield ~ dose)
ini_a <- ini_fit$coef[[1]]
ini_b <- ini_fit$coef[[2]]
ini_c <- mean(lukaN$dose)

model_lukaN <<- nlsLM(
  formula = yield ~ lp(dose, a, b, c),
  data = lukaN,
  start = list(a = ini_a, b = ini_c, c = ini_c)
)


summary(model_lukaN)

par(mar=c(4,5,3,2))
plotPredy(data  = lukaN,
          x     = dose,
          y     = yield,
          model = model_lukaN,
          #main  = "Lukavec",
          xlab  = expression("N dose [ kg "~ha^'-1'~"]"),
          ylab  = expression("Grain Yield [ t "~ha^'-1'~"]"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "A")
title("Lukavec", adj = 0, line = 1, cex.main = 1.3, family = "A")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = lukaN$dose, labels = lukaN$dose, 
     las = 1, cex.axis = 1.3, family = "A")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 2.495+0.074[x-45.600]", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "A")

dev.copy(device = png, filename = 'plots/lukavec_n.png', 
         width = 1600, height = 1000, res=200)
dev.off()

# ================ END OF THE MAIN ANALYSIS ===============================

# random effect -----------------------------------------------------------

# install.packages("nlme")
require(nlme)

fit.lm    = lm(gyield ~ dose, data=alfa) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(alfa$dose)

model_alfag_rand = nlme(gyield ~ linplat(dose, a, b, c),
           random = a+b~1|year,
           fixed = a+b+c~ 1,
           data = alfa,
           start = c(a=6.72, b=0.00907, c=46))

summary(model_alfag_rand) # FURTHER it does not work :/

par(mar=c(4,5,3,2))
plotPredy(data  = alfa,
          x     = dose,
          y     = gyield,
          model = model_alfag_rand,
          main  = "",
          xlab  = expression("N dose ( kg "~ha^'-1'~")"),
          ylab  = expression("Grain Yield ( t "~ha^'-1'~")"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "Times")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = alfa$dose, labels = alfa$dose, 
     las = 1, cex.axis = 1.3, family = "Times")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 6.659+0.014(x-44.592)", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "Times")

dev.copy(device = png, filename = 'plots/linplat_alfa_grain.png', 
         width = 600, height = 400) 
dev.off()

# singular gradient issue  SOLVED !!!-------------------------------------------------

install.packages("minpack.lm")
library(minpack.lm)

lp = function(x, a, b, c) {
  if_else(condition = x < c,
          true = a + b * x,
          false = a + b * c)}

ini_fit <- lm(data = ivanNPK, formula = yield ~ dose)
ini_a <- ini_fit$coef[[1]]
ini_b <- ini_fit$coef[[2]]
ini_c <- mean(ivanNPK$dose)

lp_model <<- nlsLM(
  formula = yield ~ lp(dose, a, b, c),
  data = ivanNPK,
  start = list(a = ini_a, b = ini_c, c = ini_c)
)

print(summary(lp_model))

par(mar=c(4,5,3,2))
plotPredy(data  = ivanNPK,
          x     = dose,
          y     = yield,
          model = lp_model,
          # main  = "Ivanovice",
          xlab  = expression("N dose (NPK) ( kg "~ha^'-1'~")"),
          ylab  = expression("Grain Yield ( t "~ha^'-1'~")"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "A")
title("Ivanovice", adj = 0, line = 1, cex.main = 1.3, family = "A")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = ivanNPK$dose, labels = ivanNPK$dose, 
     las = 1, cex.axis = 1.3, family = "A")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 5.111+0.035(x-46.594)", side = 3, line = 1,                # data fetched from model summary
      outer = FALSE, cex = 1.5, col = "blue", family = "A")

dev.copy(device = png, filename = 'plots/ivanovice_npk.png', 
         width = 600, height = 400) 
dev.off()




