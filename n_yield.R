# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")

require(tidyverse) #zakladni balik  
require(readxl) #nacitani excelovych tabulek
require(reshape2) #prevod na long format
require(easynls) #linplat model auto
library(rcompanion) #plotPredy / manual linplat

# alfa alfa as preceding crop 

alfa <- read_excel("red/alfa.xlsx") 
alfa$year <- as.factor(alfa$year)
alfa$fert <- factor(alfa$fert, levels = c("Control", "N 40", "N 55", "N 60", "N 75"), 
                    labels = c("0", "40", "55", "60", "75"))

# potato as preceding crop

pota <- read_excel("red/potato.xlsx") 
pota$year <- as.factor(pota$year)
pota$fert <- factor(pota$fert, levels = c("Control", "N 40", "N 55", "N 60", "N 75"), 
                    labels = c("0", "40", "55", "60", "75"))

# Times New Roman import --------------------------------------------------

# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  


ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +     
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme_bw() +
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) #Times New Roman, 12pt, Bold
#example taken from the Github project page

# OR

# install.packages("ggtext")
require(ggtext)

plot <- plot + theme(
  legend.text = ggtext::element_markdown(family = 'Times', face='bold')
)

# alfa explor -------------------------------------------------------------

ggplot(alfa, aes(fert, gyield, colour=year))+
  geom_jitter(size = 3)+
  labs(title = "Alfalfa", colour = "", 
       x = expression("N dose [ kg "~ha^-1~"]"), 
       y = expression("grain yield [ t "~ha^-1~"]"))+
  theme_minimal(base_size = 20)+
  theme(text=element_text(family="Times New Roman", face="bold"))
 
# ggsave("plots/overview_alfa.png", height = 6, width = 9, dpi = 300)  

# potato explor -----------------------------------------------------------

ggplot(pota, aes(fert, gyield, colour=year))+
  geom_jitter(size = 3)+
  labs(title = "Potato", colour = "", 
       x = expression("N dose [ kg "~ha^-1~"]"), 
       y = expression("grain yield [ t "~ha^-1~"]"))+
  theme_minimal(base_size = 20)+
  theme(text=element_text(family="Times New Roman", face="bold"))

# ggsave("plots/overview_pota.png", height = 6, width = 9, dpi = 300)

# linplat model -----------------------------------------------------------

# install.packages("easynls")
require(easynls)

# create dataframe for Alfalfa GRAIN
df_alfag <- data.frame(alfa$dose, alfa$gyield)

## create dataframe for Potato GRAIN
df_potag <- data.frame(pota$fert, pota$gyield)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_alfag, model=1) #linear
nlsplot(df_alfag, model=2) #quadratic
nlsplot(df_alfag, model=3) #linear-plateau

## RESULTS

nlsfit(df_tube, model=3)
nlsfit(df_top, model=3)

nlsplot(df_tube, model=3, xlab = "N dose [kg ha-1]", ylab = "tuber yield [t ha-1]")
nlsplot(df_top, model=3, xlab = "N dose [kg ha-1]", ylab = "top yield [t ha-1]")



# linplat manual ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

install.packages("rcompanion") # quite long installation, 5 minutes
library(rcompanion) # for plot part

# Alfa grain

fit.lm    = lm(gyield ~ dose, data=alfa) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(alfa$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_alfag = nls(gyield ~ linplat(dose, a, b, clx), # Find best fit parameters
                data = alfa,
                start = list(a   = a.ini,
                             b   = b.ini,
                             clx = clx.ini),
                trace = FALSE,
                nls.control(maxiter = 1000))

summary(model_alfag)

par(mar=c(4,5,3,2))
plotPredy(data  = alfa,
          x     = dose,
          y     = gyield,
          model = model_alfag,
          main  = "",
          xlab  = expression("N dose [ kg "~ha^-1~"]"),
          ylab  = expression("top yield [ t "~ha^-1~"]"),
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
mtext("y = 6.72+0.00907(x-46)", side = 3, line = 1,
      outer = FALSE, cex = 1.5, col = "blue", family = "Times")

dev.copy(device = png, filename = 'plots/linplat_alfa.png', 
         width = 600, height = 400) 
dev.off()
