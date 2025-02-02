# TP Statistique DIT
## Pr�nom et Nom : Abdoulaye DIAW

# D�finir le chemin par setwd("Nom du chemin du dossier de TP")

############# Partie 1 ################

# 1 Chargement des donn�es 

library(readxl)
data_TP_DIT1 <- read_excel("C:\\dataTPDIT1.xlsx")

# 2 Le nombre de colonnes et de lignes

dimensions <- dim.data.frame(data_TP_DIT1)
cat("le nombre de lignes et de colonnes est respectivement : ", dimensions)

# 3 La commande qui permet d'afficher les premi�res lignes de la base de donn�es

head(data_TP_DIT1, 5)

############# Partie 2 ################

# Exercice 1

# 1 Calcul de la moyenne, la m�diane, la variance et l'�cart-type des variables �ge, bmi

noms_variables <- c("age", "bmi")
moyennes <- apply(data_TP_DIT1[noms_variables],2,mean)
moyennes
med <- apply(data_TP_DIT1[noms_variables],2,median)
med
ecart_type <- apply(data_TP_DIT1[noms_variables],2,
                    function(x)(sqrt(var(x))))
ecart_type

# 2 le tableau des effectifs de l'�ge

attach(data_TP_DIT1) # pour l'utilisation direct des noms des variables sans le nom de la base
tabl_age <- rbind(sort(unique(age)), tabulate(factor(age)))
rownames(tabl_age) <- c("age", "effectif")
View(tabl_age)

##  En d�duire le mode.

### En regaradnt le tableau, nous notons un mode 41 avec un effectif de 13

# 3 a) le diagramme en batton

# b) repr�sentation graphique

x = tabulate(factor(age))
noms_barres = factor(sort(unique(age)))
barplot(x,col="blue",names.arg=noms_barres)

# 4 Il s'agit de la commande summary

# 5 Regroupement de l'�ge en 7 classes d'amplitude �gale 

data_TP_DIT1$age_rec <- cut(age, 7)

## en d�duire la classe modale

tableau <-  tabulate(data_TP_DIT1$age_rec)
cat("La classe modale est de : ", attributes(data_TP_DIT1$age_rec)$levels[which.max(tableau)])

# 6 

## les frequances 

freq_simple <- tableau/sum(tableau)*100
freq_simple

# Frequences cumulees

freq_cum <- freq_simple  # initialisation
for (i in 1:6) {
  freq_cum[i+1] <- freq_cum[i] + freq_cum[i+1]
}
freq_cum 

# Exercice 2

# 1 tableau de contigence entre sexe et tabac

sexe_tabac <-  table(sexe, tabac)

# 2 a repr�sentation tableau de frequences

frequ_sexe_tabac <-  sexe_tabac*100/sum(sexe_tabac)
frequ_sexe_tabac

# 2 b frequences linge 

frequ_ligne <- apply(frequ_sexe_tabac, 1, sum)
frequ_ligne

### frequences colonnes

frequ_colonne <- apply(frequ_sexe_tabac, 2, sum)
frequ_colonne


# 3 Fiare la repr�sentation graphiqe 

## a age bmi
boxplot(bmi ~ data_TP_DIT1$age_rec,
        col = "purple", border = "black",
        main = "bmi en fonction de l'�ge",
        ylab = "bmi",
        xlab = "age recode")

## b camoriees et graisse

plot(calories, graisses)

