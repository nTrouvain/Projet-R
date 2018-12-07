### PROJET MODELISATION STATISTIQUE ###
###  M. Devaux N. Trouvain ENSC 2A  ###
###           2018 - 2019           ### 

# Modification du working directory
setwd("C:/Users/NATHAN/Desktop/Projet R")

#######################################

### Statistique descriptive ###

#######################################
##### Préambule #####

# On réalise l'ACP à l'aide du package PCAmixdata
require(PCAmixdata)

activation <- as.data.frame(readRDS("activation.Rdata"))
#head(activation)

# Renommage des variables
# (raccourcissement des noms)
var<-c("Sujet","Sexe","Age","Vol","ILH","GFront","GAng",
	 "GOcci","GRol","GTemp","GHipp","DFront","DAng",
	 "DOcci","DRol","DTemp","DHipp")

names(activation)<-var

# Nommage des individus
sujets<-activation$Sujet
rownames(activation)<-sujets

# Supression de activation$Sujet

activation<-subset(activation, select=Sexe:DHipp)

##### ACP Données mixtes #####

split <- splitmix(activation)
quanti <- split$X.quanti 
sexe <- split$X.quali 

res<-PCAmix(X.quanti=quanti, X.quali=sexe,
		rename.level=FALSE, graph=FALSE)
                     
par(mfrow=c(2,2))
plot(res,choice="ind",coloring.ind=sexe$Sexe,label=FALSE,
      posleg="topleft", main="Observations")
plot(res,choice="levels",xlim=c(-1.5,2.5), 
	main="Variables qualitatives")
plot(res,choice="cor",main="Variables quantitatives")
plot(res,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="Variables mixtes")


##### ACP Données quantitatives #####

#Récupération des données quantitatives en fonction du sexe
activationH<-subset(activation, activation$Sexe=="H", 
				select=Age:DHipp)
activationF<-subset(activation, activation$Sexe=="F", 
				select=Age:DHipp)

par(mfrow=c(1,2))
boxplot(activationH, axes = c(1,2))
boxplot(activationF, axes = c(1,1))

par(mfrow=c(1,3))
ACP<-PCAmix(activationH, graph = FALSE)

######################################

#### Regression linéaire multiple ####

######################################




