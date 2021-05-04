# 04/2021
# Využít linear-plateau (LP) model, ale na ose X by nebyly varianty hnojení, ale roky. 
# Chtěl bych dokázat, že změna klimatu ovlivňuje různé lokality jinak. Někde pozitivně, jinde negativně. 

# 1980-2018
# loc: Ivanovice, Caslav, Lukavec

# x = year
# y = yield

# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")

require(tidyverse) #zakladni balik  
require(readxl) #nacitani excelovych tabulek
require(reshape2) #prevod na long format
require(easynls) #linplat model auto
library(rcompanion) #plotPredy / manual linplat


ivan <- read_excel("red/weather.xlsx", sheet = 1) 
casl <- read_excel("red/weather.xlsx", sheet = 2) 
luka <- read_excel("red/weather.xlsx", sheet = 3) 

ivan$yield <- as.numeric(ivan$yield)
casl$yield <- as.numeric(casl$yield)
luka$yield <- as.numeric(luka$yield)

# factors

ivan$year <- as.factor(ivan$year)
casl$year <- as.factor(casl$year)
luka$year <- as.factor(luka$year)

# Times New Roman import --------------------------------------------------

# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

# OR

# install.packages("ggtext")
require(ggtext)

plot <- plot + theme(
  legend.text = ggtext::element_markdown(family = 'Times', face='bold')
)


# explorative -------------------------------------------------------------

ggplot(ivan, aes(year, yield))+
  geom_point()+
  labs("year", "yield")+
  theme_minimal()

# dataframe - means -------------------------------------------------------------

ivan_m <- ivan %>% 
  select(year, yield) %>% 
  group_by(year)

# LINPLAT auto

# install.packages("easynls")
require(easynls)

df_ivan <- data.frame(ivan$year, ivan$yield)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_ivan, model=1) #linear
nlsplot(df_ivan, model=2) #quadratic
nlsplot(df_ivan, model=3) #linear-plateau

## RESULTS

nlsfit(df_ivan, model=3)

nlsplot(df_ivan, model=3, xlab = "Year", ylab = "Yield [t ha-1]")

# LINPLAT ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

# install.packages("rcompanion") # quite long installation, 5 minutes
library(rcompanion) # for plot part

# Ivanovice ------------------------------------------------------------

fit.lm    = lm(yield ~ year, data=ivan) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(ivan$year)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_ivan = nls(yield ~ linplat(year, a, b, clx), # Find best fit parameters
                data = ivan,
                start = list(a   = a.ini,
                             b   = b.ini,
                             clx = clx.ini),
                trace = FALSE,
                nls.control(maxiter = 1000))

summary(model_ivan)

par(mar=c(4,5,3,2))
plotPredy(data  = ivan,
          x     = year,
          y     = yield,
          model = model_ivan,
          main  = "",
          xlab  = expression("Year"),
          ylab  = expression("Grain Yield ( t "~ha^-1~")"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "Times")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = ivan$year, labels = ivan$year, 
     las = 1, cex.axis = 1.3, family = "Times")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 6.659+0.014(x-44.592)", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "Times")

dev.copy(device = png, filename = 'plots/ivanovice.png', 
         width = 600, height = 400) 
dev.off()

# Caslav ------------------------------------------------------------

fit.lm    = lm(yield ~ year, data=casl) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(casl$year)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_casl = nls(yield ~ linplat(year, a, b, clx), # Find best fit parameters
                 data = casl,
                 start = list(a   = a.ini,
                              b   = b.ini,
                              clx = clx.ini),
                 trace = FALSE,
                 nls.control(maxiter = 1000))

summary(model_casl)

par(mar=c(4,5,3,2))
plotPredy(data  = casl,
          x     = year,
          y     = yield,
          model = model_casl,
          main  = "",
          xlab  = expression("Year"),
          ylab  = expression("Grain Yield ( t "~ha^-1~")"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "Times")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = casl$year, labels = casl$year, 
     las = 1, cex.axis = 1.3, family = "Times")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 6.659+0.014(x-44.592)", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "Times")

dev.copy(device = png, filename = 'plots/caslav.png', 
         width = 600, height = 400) 
dev.off()

# Lukavec ------------------------------------------------------------

fit.lm    = lm(yield ~ year, data=luka) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(luka$year)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_luka = nls(yield ~ linplat(year, a, b, clx), # Find best fit parameters
                 data = luka,
                 start = list(a   = a.ini,
                              b   = b.ini,
                              clx = clx.ini),
                 trace = FALSE,
                 nls.control(maxiter = 1000))

summary(model_luka)

par(mar=c(4,5,3,2))
plotPredy(data  = luka,
          x     = year,
          y     = yield,
          model = model_luka,
          main  = "",
          xlab  = expression("Year"),
          ylab  = expression("Grain Yield ( t "~ha^-1~")"),
          xaxt  = "n",
          cex   = .9, # velikost bodu
          cex.lab= 1.5, # velikost popisku osy
          cex.axis=1.4, # velikost tick mark popisku
          cex.main=1.5,
          family = "Times")
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = luka$year, labels = luka$year, 
     las = 1, cex.axis = 1.3, family = "Times")
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 6.659+0.014(x-44.592)", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "Times")

dev.copy(device = png, filename = 'plots/lukavec.png', 
         width = 600, height = 400) 
dev.off()


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
          xlab  = expression("N dose ( kg "~ha^-1~")"),
          ylab  = expression("Grain Yield ( t "~ha^-1~")"),
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