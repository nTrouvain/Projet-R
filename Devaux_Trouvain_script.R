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

##### ACP Données mixtes #####

split <- splitmix(activation)
quanti <- split$X.quanti 
sexe <- split$X.quali 

res<-PCAmix(X.quanti=quanti, X.quali=sexe,
		rename.level=FALSE, graph=FALSE)

round(res$eig,digit=3)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

par(mfrow=c(2,2))
plot(res,choice="ind",coloring.ind=sexe$Sexe,label=FALSE,
      posleg="topleft", main="Observations")
plot(res,choice="levels",xlim=c(-1.5,2.5), 
	main="Modalités qualitative")
plot(res,choice="cor",main="Cercle de corrélation")
plot(res,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="Squared Loadings")


#### ANOVA Broca~sexe



#Récupération des données quantitatives en fonction du sexe
activationH<-subset(activation, activation$Sexe=="H", 
				select=Age:DHipp)
activationF<-subset(activation, activation$Sexe=="F", 
				select=Age:DHipp)

#Evaluation de l'influence du sexe sur la variable d'intérêt
shapiro.test(activationH$GFront)
shapiro.test(activationF$GFront)
t.test(activationH$GFront, activationF$GFront)
var.test(activationH$GFront, activationF$GFront)

#Evaluation de l'influence du sexe sur le volume cérébral
plot(activation$Vol~activation$Sexe)
aov(activation$Vol~activation$Sexe)
res<-lm(activation$Vol~activation$Sexe)
anova(res)
summary(res)
plot(res$residuals,res$fitted.values)

#Evaluation de l'influence du sexe sur l'ILH
plot(activation$ILH~activation$Sexe)

# Supression des variables de volume cérébral, age et ILH
# Les données sont trop disparates pour que le boxplot
# soit significatif autrement
activationHbp <- activationH
activationHbp$Vol <- NULL
activationHbp$Age <- NULL
activationHbp$ILH <- NULL
activationFbp <- activationF
activationFbp$Vol <- NULL
activationFbp$Age <- NULL
activationFbp$ILH <- NULL
par(mfrow=c(1,2))
boxplot(activationHbp, axes = c(1,2))
boxplot(activationFbp, axes = c(1,1))

# ACP
par(mfrow=c(1,3))
ACP<-PCAmix(activationH, graph = TRUE)
ACP<-PCAmix(activationHbp, graph = TRUE)

######################################

#### Regression linéaire multiple ####

######################################

# Stockage des variables (raccouricement des noms)

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

lin<-lm(gFront~vol+ILH+gFront+gAng+gOcci
		+gRol+gTemp+gHipp+dFront+dAng+dOcci
		+dRol+dTemp+dHipp)
summary(lin)

anova(lin)

par(mfrow=c(1,2))
plot(lin$fit)

lin2<-step(lin)
plot(lin2$fit)

anova(lin2)

