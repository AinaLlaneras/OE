setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(tidyr)
library(dplyr)

 #### Apartat 1 Flux elèctric òptim #### 

## a) Funció objectiu i variables de la solució òptima 

# CostTotal = 955484
P <- read.table("P.txt", header = TRUE)
pg <- read.table("Apartat1_5_PG.txt", header = TRUE)
th <- read.table("Apartat1_5_TH.txt", header = TRUE)
u <- read.table("Apartat1_5_u.txt", header = TRUE)

arcs <- c("()", "(1,5)", "(2,3)", "(2,4)", "(2,5)", "(3,4)", "(4,5)", "(4,7)", "(4,9)", "(5,6)", "(6,11)", "(6,12)", "(6,13)", "(7,8)", "(7,9)", "(9,10)", "(9,14)", "(10,11)", "(12,13)", "(13,14)")
colnames(P) <- arcs
colnames(th) <- paste("Th", 1:14, sep="")

## b) unitats necesaries per satisfer la demanda dels nodes a cada periode de temps 

df2 <- read.table("Apartat1_2.txt", header=T)
colnames(df2) <- "nUnitats"
df2$T <- 1:24
ggplot(data = df2) +
  geom_line(aes(x = T, y = nUnitats, color = "pink"), linewidth=1.5) +
  labs(title = "Unitats enceses cada hora", x = "T", y = "Unitats") +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))

## c) gràfic del perfil de potència i energia de dues unitats generadores al llarg del temps
T <- 1:24
df3 <- read.table("Apartat1_3.txt", header = TRUE)
df3$T <- T
df32 <- pivot_longer(df3, cols = c(PG_NRC3, PG_NRC1), names_to = "Generadors", values_to = "Potencia")

df32$Tprev <- df32$T -1

ggplot(data = df32) + 
  geom_rect(aes(xmin = Tprev, xmax = T, ymin = 0, ymax = Potencia, fill = Generadors, color = Generadors), 
            alpha = 0.7) +
  geom_line(aes(x = (T-0.5), y = Potencia, color = factor(Generadors)), linewidth = 1.1) +
  labs(title = "Perfil de potència i energia dels generadors NRC1 i NRC3", x = "T", y = "MW") +
  theme(plot.title = element_text(hjust = 0.5))

## d) quin és l'arc més sobrecarregat? gràfic del seu perfil de potència al llarg del temps 

# com a definicio de sobrecarrega hem sumat les carregues absolutes que passen per cada arc i diem que el mes carregat es el que te la suma més alta. 

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
                13,14,154.888)
              , ncol = 3, byrow = T)

pot[which.max(pot[,3]),] # arc L=(4,7) més sobrecarregat 

arc <- read.table("Apartat1_41.txt", header = TRUE)
arc$T <- T
arc$Tprev <- T-1

ggplot(data = arc) + 
  geom_rect(aes(xmin = Tprev, xmax = T, ymin = 0, ymax = potencia, color = "black")) +
  geom_line(aes(x = T-0.5, y = potencia))+
  labs(title = "Perfil de potència arc (4,7)", x = "T", y = "MW") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

## e) marge de potència disponible té el sistema a cada període per absorbir eventuals desajustos d oferta i o demanda
marges <- read.table("Apartat_42.txt", header = TRUE)
U <- marges$rup
D <- marges$rdown
M <- marges
# marges de generadors oberts
pg2 <- pg[,-which(sapply(pg, sum)==0)]
pg2$T <- T
marges$generador <- colnames(pg)
marges <- marges[-which(sapply(pg, sum)==0),]
rownames(marges) <- marges$generador

# fem un df en format allargat per poder fer el ggplot b
potencia_hores <- pivot_longer(data = pg2, cols = 1:4, names_to = "generador", values_to = "mgw")

# unim els marges amb les dades de potencia i ajustem els marges per cada temps i generador 
prova <- left_join(marges[,3:5], potencia_hores, by="generador")
prova[prova$rdown < 0, 2] <- 0
prova[,"rdown"] <- prova$mgw-prova$rdown
prova[,"rup"] <- prova$mgw+prova$rup
prova[prova[,"rdown"]<0,"rdown"] <- 0

prova2 <- prova

# ajustem els marges pq on siguin mes grans q pmin i mes petits q pmax 
for(i in 1:nrow(prova)){
  if(prova[i,"mgw"]>0 && prova[i, "rdown"]<marges[prova[i,"generador"],"pmin"]){
    prova2[i, "rdown"] <- marges[prova[i,"generador"],"pmin"]
  } 
  else if(prova[i, "rup"]>=marges[prova[i,"generador"],"pmax"]){
    prova2[i, "rup"] <- marges[prova[i,"generador"],"pmax"]
  }
}
prova2[prova2$generador=="ROB2"& prova2$rup > 40, "rup"] <- 70

# gràfic per cada generador obert
ggplot(data = prova2, aes(x = T, y = mgw)) + 
  geom_ribbon(aes(ymin = rdown, ymax = rup), fill = "tomato", alpha = 0.5) +
  geom_line() +
  facet_wrap(~generador, ncol = ) +
  labs(title = "Marge de potència de cada generador", x="T", y="MW")+
  geom_hline(data = marges, aes(yintercept = pmax), color = "red", linetype = "dashed") +
  geom_hline(data = marges, aes(yintercept = pmin), color = "blue", linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))

  
# ara ho fem per a tots els generadors: capcitat del sistema
total <- prova %>% group_by(T) %>% summarise(potencia = sum(mgw))
up_total <- sum(U)
down_total <- sum(D)

total$up <- total$potencia + up_total#colSums(t(u)*U) # generadors oberts * rampa up
total$down <- total$potencia - down_total#colSums(t(u)*D) # generadors oberts * rampa down
pmax_total <- sum(M$pmax) # suma dels maxims
pmin_total <- min(M$pmin) # minim dels minims

total[total$down<pmin_total,"down"] <- pmin_total

 
m <- c(pmax_total, pmin_total)

ggplot(data = total, aes(x = T, y = potencia)) + 
  geom_ribbon(aes(ymin = down, ymax = up), fill = "tomato", alpha = 0.5) +
  geom_line() +
    labs(title = "Marge de potència del sistema", x="T", y="MW")+
  geom_hline(aes(yintercept = pmax_total), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = pmin_total), color = "blue", linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))

## f) creus que pot tenir setnit incrementar la capacitat d'alguna unitat generadora? si es així, quina inversió econòmica estaries disposat a fer? 

# L unic generador que es veu limitat per la capacitat és el ROB2. La resta sempre estan per sota el màxim. 


