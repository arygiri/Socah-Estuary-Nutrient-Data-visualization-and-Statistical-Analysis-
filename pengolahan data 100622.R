library(readxl)
library(tidyverse)
library(ggplot2)
#buka data
data <- read_excel("Data lengkap praktikum osekim 2022.xlsx", 
                   sheet = "Sheet2")
head(data)
tail(data)
str(data)
data$Station <- as.factor(data$Stasiun)
str(data)
fix(data)

############visualisasi data#########
qplot(x = Station, y = Phosphate, geom = "point", data = data) +
  facet_grid(.~Position, labeller = label_both)

ggsave("qplot data fosfat.png", width = 7, height = 2.5, dpi=1000)

#########summary data#######
datasum <- data %>%
  group_by(Station) %>%
  summarise(mean.DO = mean(DO),se.DO = sd(DO),
            mean.PH = mean(PH),se.PH = sd(PH),
            mean.Salinitas = mean(Salinitas),se.Salinitas = sd(Salinitas),
            mean.Kecerahan = mean(Kecerahan),se.Kecerahan = sd(Kecerahan),
            mean.Klorofil = mean(Klorofil),se.Klorofil = sd(Klorofil),
            mean.Ammonia = mean(Ammonia),se.Ammonia = sd(Ammonia),
            mean.Ammonium = mean(Ammonium),se.Ammonium = sd(Ammonium),
            mean.Nitrat = mean(Nitrat),se.Nitrat = sd(Nitrat),
            mean.Nitrit = mean(Nitrit),se.p = sd(Nitrit),
            mean.p = mean(Fosfat),se.p = sd(Fosfat))
write.csv(datasum, "data osekim mean dan se.csv")

##############################anova
library(ggpubr)
ggboxplot(data, x = "Station", y = "Fosfat", 
          color = "Station", palette = c("#00AFBB", "#E7B800", "#FC4E07", "Blue"),
          order = c("1", "2", "3","4"),
          ylab = "Phosphate (mg/L)", xlab = "Station")

# Compute the analysis of variance
res.aovfos <- aov(Fosfat ~ Station, data = data)
# Summary of the analysis
summary(res.aovfos)

# Compute the analysis of variance
res.aovAmmonia <- aov(Ammonia ~ Station, data = data)
# Summary of the analysis
summary(res.aovAmmonia)

# Compute the analysis of variance
res.aovAmmonium <- aov(Ammonium ~ Station, data = data)
# Summary of the analysis
summary(res.aovAmmonium)

# Compute the analysis of variance
res.aovNitrat <- aov(Nitrat ~ Station, data = data)
# Summary of the analysis
summary(res.aovNitrat)

#################Kruskal Wallis#################################
install.packages("DescTools")
require(DescTools)
install.packages("FSA")
require(FSA)
require(rcompanion)
library(multcompView)
kruskal.test(Ammonia ~ Stasiun, data = data)
kruskal.test(Ammonium ~ Stasiun, data = data)
kruskal.test(Nitrit ~ Stasiun, data = data)
kruskal.test(Fosfat ~ Stasiun, data = data)
kruskal.test(Nitrat ~ Stasiun, data = data)


############Barplot
ggbarplot(data, x = "Station",
          y = "Fosfat",
          merge = TRUE,
          color = "Station",
          ylab = "Phosphate (mg/L)", 
          add = "mean_se", palette = "jco",
          position = position_dodge(0.8))


plot.p <- ggplot(data2, aes(x=Station, y=mean.p, fill=Position, ymax=20, ymin=0)) + 
  geom_col(width = .65, position = position_dodge(.8), colour="black")+
  geom_errorbar(aes(ymin=mean.p-se.p, ymax=mean.p+se.p),width=.2,
                position=position_dodge(.8), color="black")+
  labs(x="Station", y="Phosphate (然)", fill=NULL)+
  scale_fill_manual(values=c("white", "#D1D5DB", "#6B7280"))+
  scale_y_continuous(expand=expansion(0), limits = c(0,20), breaks = seq(0,20,5))+
  
  theme(
    plot.margin = unit(c(0.7, 0.7, 0.7, 0.7), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.ticks.x = element_blank(), legend.position = "none"
)
    
plot.p

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

ggsave("bar_phosphate3.jpg", width=9, height = 5, dpi=700)



###########################################################################
################################### NITRATE################################
###########################################################################
library(readxl)
library(tidyverse)
library(xlsx)
#buka data
data.n <- read_excel("NPA_1912.xlsx", sheet = "nitrat")
head(data.n)
str(data.n)
data.n$Station <- as.factor(data$Station)
data.n$Position <- factor(data$Position, levels = c("Surface", "Middle", "Bottom"))
str(data.n)
fix(data.n)

############visualisasi data#########
qplot(x = Station, y = Nitrate, geom = "point", data = data.n) +
  facet_grid(.~Position, labeller = label_both)

ggsave("qplot data nitrat.jpg", width = 7, height = 2.5, dpi=1000)
#########summary data#######
#####SD lebih besar dari mean maka pakai se
'''
Standard Error (SE) is the standard deviation of the mean of the variable, 
calculated as the SD divided by the square root of the sample size. By 
construction, the SE is smaller than the SD. With a very large sample size, the 
SE tends toward 0.
'''

data.n2 <- data.n %>%
  group_by(Station, Position) %>%
  summarise(mean.n = mean(Nitrate),
            se.n = sd(Nitrate)/sqrt(2))


write.csv(data.n2, file = "nitrat mean dan se.csv")

#################Kruskal wallis#################################
require(DescTools)
install.packages("FSA")
require(FSA)
require(rcompanion)
require(multcompView)
Summarize(Nitrate~ Position,
          data = data.n)

#histoggram

library(lattice)

histogram(~ Nitrate | Position,
          data=data.n,
          layout=c(1,3)) 

#uji mann withney kruskal wallis
kruskal.test(Nitrate ~ Position,
             data = data.n) #hasil tdk signifikan utk nitrate



plot.n <- ggplot(data.n2, aes(x=Station, y=mean.n, fill=Position, ymin=0, ymax=40)) + 
  geom_col(width = .65, position = position_dodge(.8), colour="black")+
  geom_errorbar(aes(ymin=mean.n-se.n, ymax=mean.n+se.n),width=.2,
                position=position_dodge(.8), color="black")+
  labs(x="Station", y="Nitrate (然)", fill=NULL)+
  scale_fill_manual(values=c("white", "#D1D5DB", "#6B7280"))+
  scale_y_continuous(expand=expansion(0),limits = c(0, 40), breaks =seq(0, 40, 10))+
  
  theme(
    plot.margin = unit(c(0.7, 0.7, 0.7, 0.7), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.ticks.x = element_blank(), legend.position = "none"
)
plot.n

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

plot.n
ggsave("bar_nitrate3.jpg", width=9, height = 5, dpi=700)

###########################################################################
######################### ######### NITRITE################################
###########################################################################
library(readxl)
library(tidyverse)
library(xlsx)
#buka data
data.ni <- read_excel("NPA_1912.xlsx", sheet = "nitrit")
head(data.ni)
str(data.ni)
data.ni$Station <- as.factor(data$Station)
data.ni$Position <- factor(data$Position, levels = c("Surface", "Middle", "Bottom"))
str(data)
fix(data)

############visualisasi data#########
qplot(x = Station, y = Nitrite, geom = "point", data = data.ni) +
  facet_grid(.~Position, labeller = label_both)

ggsave("qplot data nitrit.jpg", width = 7, height = 2.5, dpi=1000)
#########summary data#######
data.ni2 <- data.ni %>%
  group_by(Station, Position) %>%
  summarise(mean.ni = mean(Nitrite),
            se.ni = sd(Nitrite)/sqrt(2))

write.csv(data.ni2, file = "nitrit meand dan se.csv")

#################TWO WAY ANOVA#################################
require(DescTools)
install.packages("FSA")
require(FSA)
require(rcompanion)
require(multcompView)
Summarize(Nitrite~ Position,
          data = data)

#histoggram

library(lattice)

histogram(~ Nitrite | Position,
          data=data.ni,
          layout=c(1,3)) 

#uji mann withney kruskal wallis
kruskal.test(Nitrite ~ Position,
             data = data.ni) #hasil tdk signifikan utk nitrite



plot.ni <- ggplot(data.ni2, aes(x=Station, y=mean.ni, fill=Position, ymin=0, ymax=30)) + 
  geom_col(width = .65, position = position_dodge(.8), colour="black")+
  geom_errorbar(aes(ymin=mean.ni-se.ni, ymax=mean.ni+se.ni),width=.2,
                position=position_dodge(.8), color="black")+
  labs(x="Station", y="Nitrite (然)", fill=NULL)+
  scale_fill_manual(values=c("white", "#D1D5DB", "#6B7280"))+
  scale_y_continuous(expand=expansion(0),limits = c(0, 30), breaks =seq(0, 30, 6))+
  
  theme(
    plot.margin = unit(c(0.7, 0.7, 0.7, 0.7), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.ticks.x = element_blank(), legend.position = "none"
)
plot.ni

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}
plot.ni
ggsave("bar_nitrite3.jpg", width=9, height = 5, dpi=700)

###########################################################################
######################### ######### AMMONIA################################
###########################################################################
library(readxl)
library(tidyverse)
library(xlsx)
#buka data
data.a <- read_excel("NPA_1912.xlsx", sheet = "amonia")
head(data.a)
str(data.a)
data.a$Station <- as.factor(data$Station)
data.a$Position <- factor(data$Position, levels = c("Surface", "Middle", "Bottom"))
str(data.a)
fix(data.a)

############visualisasi data#########
qplot(x = Station, y = Amonia, geom = "point", data = data.a) +
  facet_grid(.~Position, labeller = label_both)

ggsave("qplot data amonia.jpg", width = 7, height = 2.5, dpi=1000)
#########summary data#######
data.a2 <- data.a %>%
  group_by(Station, Position) %>%
  summarise(mean.a = mean(Amonia),
            se.a = sd(Amonia)/sqrt(2))

write.csv(data.a2, file = "amonia meand dan se.csv")
#################TWO WAY ANOVA#################################
require(DescTools)
install.packages("FSA")
require(FSA)
require(rcompanion)
require(multcompView)
Summarize(Nitrite~ Position,
          data = data.a)

#histoggram

library(lattice)

histogram(~ Amonia| Position,
          data=data.a,
          layout=c(1,3)) 

#uji mann withney kruskal wallis
kruskal.test(Amonia~ Position,
             data = data.a) #hasil tdk signifikan utk nitrate


plot.a <- ggplot(data.a2, aes(x=Station, y=mean.a, fill=Position, ymin=0, ymax=500)) + 
  geom_col(width = .65, position = position_dodge(.8), colour="black")+
  geom_errorbar(aes(ymin=mean.a-se.a, ymax=mean.a+se.a),width=.2,
                position=position_dodge(.8), color="black")+
  labs(x="Station", y="Ammonia (然)", fill=NULL)+
  scale_fill_manual(values=c("white", "#D1D5DB", "#6B7280"))+
  scale_y_continuous(expand=expansion(0), limits = c(0, 500), breaks =seq(0, 500, 100))+
  
  theme(
    plot.margin = unit(c(0.7, 0.7, 0.7, 0.7), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size=13),
    axis.text.x= element_text(size=13),
    axis.ticks.x = element_blank(),
    legend.position = c(0.5, 0.98),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 13),
    legend.margin = margin(t = 2.7, l = 3.2, r = 3.2, b = 3.2),
    legend.key = element_rect(color = NA, fill = NA),
    legend.direction="horizontal"
  )+
  guides(
    fill = guide_legend(
      keywidth = 0.4,
      keyheight = 0.4,
      default.unit= "cm"))
)

plot.a

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

plot.a
ggsave("bar_amonia3.jpg", width=9, height = 5, dpi=700)

library("ggpubr")

ggarrange(plot.a, plot.ni, plot.n, plot.p, 
          labels = c("a", "b", "c", "d"),
          ncol = 1, nrow = 4)
ggdraw() +  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))
ggsave("bar_NPA1graph3.png", width=10, height = 17, dpi=700) 
