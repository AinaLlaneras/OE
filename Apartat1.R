setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(tidyr)
library(dplyr)

 #### Apartat 1 Flux elèctric òptim #### 

## a) Funció objectiu i variables de la solució òptima 

pg <- read.table("Apartat1_5_PG.txt", header = TRUE)
th <- read.table("Apartat1_5_TH.txt", header = TRUE)
u <- read.table("Apartat1_5_u.txt", header = TRUE)
vud <- read.table("Apartat1_5_vud.txt", header = TRUE)

## b) unitats necesaries per satisfer la demanda dels nodes a cada periode de temps 

df2 <- read.table("Apartat1_2.txt", header=T)

## c) gràfic del perfil de potència i energia de dues unitats generadores al llarg del temps
T <- 1:24
df3 <- read.table("Apartat1_3.txt", header = TRUE)
df3$T <- T
df32 <- pivot_longer(df3, cols = c(PG_NRC3, PG_NRC1), names_to = "Generadors", values_to = "Potencia")

graf1 <- ggplot(data = df32, aes(x = T, y = Potencia, group = Generadors)) 

graf1 +
  geom_line(aes(color=factor(Generadors)),linewidth=1.1) +
  labs(title = "Perfil de potència",
       x = "T",
       y = "MGW",
       color = "Generador") 

## d) quin és l'arc més sobrecarregat? gràfic del seu perfil de potència al llarg del temps 

pot <- matrix(c(1,2,289.897,
1,5,839.888,
2,3,389.582,
2,4,1133.52,
2,5,1035.12,
3,4,733.77,
4,5,472.025,
4,7,1363.83,
4,9,758.595,
5,6,1312.88,
6,11, 1086.39,
6,12,58.8122,
6,13,167.676,
7,8,121.1,
7,9,1242.73,
9,10, 1160.41,
9,14,130.088,
10,11,37.0086,
12,13,34.0122,
13,14,154.888), ncol = 3, byrow = T)

pot[which.max(pot[,3]),]

arc <- read.table("Apartat1_41.txt", header = TRUE)
arc$T <- T

ggplot(data = arc, aes(x = T, y = potencia))+ 
  geom_line(linewidth=1.1) +
  labs(title = "Perfil de potència arc (4,7)",
       x = "T",
       y = "MGW") 

## e) marge de potència disponible té el sistema a cada període per absorbir eventuals desajustos d oferta i o demanda
marges <- read.table("Apartat_42.txt", header = TRUE)

pg2 <- pg[,-which(sapply(pg, sum)==0)]
pg2$T <- T
marges$generador <- colnames(pg)
marges <- marges[-which(sapply(pg, sum)==0),]

potencia_hores <- pivot_longer(data = pg2, cols = 1:4, names_to = "generador", values_to = "mgw")

prova <- left_join(marges[,3:5], potencia_hores, by="generador")
prova[prova$rdown < 0, 2] <- 0

ggplot(data = prova, aes(x = T, y = mgw)) + 
  geom_ribbon(aes(ymin = rdown, ymax = rup), fill = "grey80", alpha = 0.5) +
  geom_line() +
  facet_wrap(~generador, ncol = 2) +
  labs(x="T", 
       y="MGW")+
  geom_hline(data = marges, aes(yintercept = pmax), color = "red", linetype = "dashed") +
  geom_hline(data = marges, aes(yintercept = pmin), color = "blue", linetype = "dashed") 

## f) creus que pot tenir setnit incrementar la capacitat d'alguna unitat generadora? si es així, quina inversió econòmica estaries disposat a fer?

max_min_data <- potencia_hores %>%
  group_by(generador) %>%
  summarize(mgw_max = max(mgw), mgw_min = min(mgw))

