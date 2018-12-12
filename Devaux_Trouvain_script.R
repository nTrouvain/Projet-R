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
head(activation)

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
activation.acti<-subset(activation, select=GFront:DHipp)
boxplot(activation.acti)
abline(0,0,col=2)
activation.indiv<-subset(activation, TRUE, select=Vol)
boxplot(activation.indiv)

#Récupération des données quantitatives en fonction du sexe
activationH<-subset(activation, activation$Sexe=="H", 
                    select=Age:DHipp)
activationF<-subset(activation, activation$Sexe=="F", 
                    select=Age:DHipp)

sd(activationH$GFront)
sd(activationF$GFront)
boxplot(activationH$GFront, activationF$GFront, names=c("Hommes\n sigma=0,5024824","Femmes\n sigma=0,4542667"), ylab="GFront")
summary(activationH$GFront)
summary(activationF$GFront)

shapiro.test(activationH$GFront)
shapiro.test(activationF$GFront)
t.test(activationH$GFront, activationF$GFront)
var.test(activationH$GFront, activationF$GFront)

##### ACP Données mixtes #####

split <- splitmix(activation)
quanti <- split$X.quanti 
sexe <- split$X.quali 

res<-PCAmix(X.quanti=quanti, X.quali=sexe,
		rename.level=FALSE, graph=FALSE)

round(res$eig,digit=3)

round(res$sqload, digit=3)

barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

#Axes 1-2
par(mfrow=c(2,2))
plot(res,choice="ind",coloring.ind=sexe,label=FALSE,
      posleg="topleft", main="Observations")
plot(res,choice="levels",xlim=c(-1.5,2.5), 
	main="Modalités qualitatives")
plot(res,choice="cor",main="Cercle de corrélation")
plot(res,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="Squared Loadings")

#Axes 1-3
plot(res,axes=c(1,3),choice="ind",coloring.ind=sexe,label=FALSE,
     posleg="topleft", main="Observations")
plot(res,axes=c(1,3),choice="levels",xlim=c(-1.5,2.5), 
     main="Modalités qualitatives")
plot(res,axes=c(1,3),choice="cor",main="Cercle de corrélation")
plot(res,axes=c(1,3),choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="Squared Loadings")

#Axes 1-4
plot(res,axes=c(1,4),choice="ind",coloring.ind=sexe,label=FALSE,
     posleg="topleft", main="Observations")
plot(res,axes=c(1,4),choice="levels",xlim=c(-1.5,2.5), 
     main="Modalités qualitatives")
plot(res,axes=c(1,4),choice="cor",main="Cercle de corrélation")
plot(res,axes=c(1,4),choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="Squared Loadings")


######################################

#### Regression linéaire multiple ####

######################################

# Stockage des variables (raccouricement des noms)
sexe<-activation$Sexe
age <- activation$Age
vol <- activation$Vol
ILH <- activation$ILH
gFront <- activation$GFront
gAng <- activation$GAng
gOcci <- activation$GOcci
gRol <- activation$GRol
gTemp <- activation$GTemp
gHipp <- activation$GHipp
dFront <- activation$DFront
dAng <- activation$DAng
dOcci <- activation$DOcci
dRol <- activation$DRol
dTemp <- activation$DTemp
dHipp <- activation$DHipp

# Données quantitatives uniquement
activation.quanti<-subset(activation, select = Age:DHipp)

# ANOVA GFront~Sexe

res.aov<-lm(GFront~Sexe, activation)
anova(res.aov)
summary(res.aov)

### ANCOVA ###
lin<-lm(gFront~vol+ILH+gFront+gAng+gOcci
		+gRol+gTemp+gHipp+dFront+dAng+dOcci
		+dRol+dTemp+dHipp+sexe)
summary(lin)

par(mfrow=c(1,2))
plot(lin$fitted.values, activation$GFront)
abline(0,1,col=2)
plot(lin$fitted.values,lin$residuals)
abline(0,0,col=2)

# AIC
step(lin)

par(mfrow=c(1,2))
plot(lin$fitted.values, activation$GFront)
abline(0,1,col=2)
plot(lin$fitted.values,lin$residuals)
abline(0,0,col=2)

# Sans le sexe

lin<-lm(gFront~vol+ILH+gFront+gAng+gOcci
        +gRol+gTemp+gHipp+dFront+dAng+dOcci
        +dRol+dTemp+dHipp)
summary(lin)

par(mfrow=c(1,2))
plot(lin$fitted.values, activation$GFront)
abline(0,1,col=2)
plot(lin$fitted.values,lin$residuals)
abline(0,0,col=2)

# AIC
lin<-step(lin)
summary(lin)

par(mfrow=c(1,2))
plot(lin$fitted.values, activation$GFront)
abline(0,1,col=2)
plot(lin$fitted.values,lin$residuals)
abline(0,0,col=2)

#Evaluation de l'influence du ILH sur GFront
lin<-lm(gFront~ILH)
summary(lin)

