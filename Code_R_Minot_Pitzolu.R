library(dplyr)
setwd('C:/Users/taminot/OneDrive/Micro-�conomie/BDD')
data <- read.csv('BDD_reduce.csv', sep=";")
data<- data[sample(1:nrow(data),2000000),]
data$DCRANna<- as.numeric(data$DCRAN)# il elimine la corse et il elimine le 0 au debut.
data$COMMna<- as.numeric(data$COMMUNE)
data$change<- ifelse(data$COMMna != data$DCRANna,1,0)
sum(data$change, na.rm=T)
sum(is.na(data$change))
#test <- subset(data, is.na(change)) # ce qu'on vient d'eliminer
data2 <- subset(data, !is.na(change))
data2 <- select(data2,-c(COMMUNE, ARM, DCRAN))

#############################################################################################
#####################               ENCODING DES VARIABLES              ##################### 
#############################################################################################

# Encoding EMPL
data2<- data2 %>%
  mutate(EMPLenc = case_when(
    EMPL == "11" ~ "Appr",
    EMPL == "12" ~ "Interim",
    EMPL == "13" ~ "Emplaide",
    EMPL == "14" ~ "Stage",
    EMPL == "15" ~ "CDD",
    EMPL == "16" ~ "CDI",
    EMPL == "21" ~ "Indip",
    EMPL == "22" ~ "Employeur",
    EMPL == "23" ~ "Aidfam",
    EMPL == "ZZ" ~ "NA",
    TRUE ~ as.character(EMPL)
  ))
data2 <- select(data2,-c(EMPL))
# Encoding CATPC
data2<- data2 %>%
  mutate(CATPCenc = case_when(
    CATPC == "0" ~ "menages",
    CATPC == "1" ~ "communautes",
    CATPC == "2" ~ "habmobiles",
    TRUE ~ as.character(CATPC)
  ))
data2 <- select(data2,-c(CATPC))
# Encoding CS1
data2<- data2 %>%
  mutate(CS1enc = case_when(
    CS1 == "1" ~ "Agriculteur",
    CS1 == "2" ~ "ArtComm",
    CS1 == "3" ~ "Cadres",
    CS1 == "4" ~ "Intermed",
    CS1 == "5" ~ "Employes",
    CS1 == "6" ~ "Ouvr",
    CS1 == "7" ~ "Retraites",
    CS1 == "8" ~ "Autresans",
    TRUE ~ as.character(CS1)
  ))
data2 <- select(data2,-c(CS1))
# Encoding CSM (categorie de la personne de reference du menage)
data2<- data2 %>%
  mutate(CSMenc = case_when(
    CSM == "1" ~ "Agriculteur",
    CSM == "2" ~ "ArtComm",
    CSM == "3" ~ "Cadres",
    CSM == "4" ~ "Intermed",
    CSM == "5" ~ "Employes",
    CSM == "6" ~ "Ouvr",
    CSM == "7" ~ "Retraites",
    CSM == "8" ~ "Autresans",
    CSM == "Z" ~ "HLO",
    TRUE ~ as.character(CSM)
  ))
data2 <- select(data2,-c(CSM))
# Encoding DIPL
data2<- data2 %>%
  mutate(DIPLenc = case_when(
    DIPL == "01" ~ "noprim",
    DIPL == "02" ~ "nocollege",
    DIPL == "03" ~ "college",
    DIPL == "11" ~ "CEP",
    DIPL == "12" ~ "Brevet",
    DIPL == "13" ~ "CAP",
    DIPL == "14" ~ "BAC",
    DIPL == "15" ~ "BACpro",
    DIPL == "16" ~ "BTS",
    DIPL == "17" ~ "Licence",
    DIPL == "18" ~ "Master",
    DIPL == "19" ~ "Doctorat",
    DIPL == "ZZ" ~ "Moins14",
    TRUE ~ as.character(DIPL)
  ))
data2 <- select(data2,-c(DIPL))
# Encoding INAI
data2<- data2 %>%
  mutate(INAIenc = case_when(
    INAI == "1" ~ "Actuel",
    INAI == "2" ~ "Autredep",
    INAI == "3" ~ "Autrereg",
    INAI == "4" ~ "AutreDOM",
    INAI == "5" ~ "AutreTOMCOM",
    INAI == "6" ~ "Etranger",
    TRUE ~ as.character(INAI)
  ))
data2 <- select(data2,-c(INAI))
# Encoding INATC
data2<- data2 %>%
  mutate(Nationalite = case_when(
    INATC == "1" ~ "Francais",
    INATC == "2" ~ "Etranger",
    TRUE ~ as.character(INATC)
  ))
data2 <- select(data2,-c(INATC))
# Encoding IRAN
data2<- data2 %>%
  mutate(ResAnte = case_when(
    IRAN == "0" ~ "Actuel",
    IRAN == "1" ~ "Memlogement",
    IRAN == "2" ~ "Memcom",
    IRAN == "3" ~ "Autrecom",
    IRAN == "4" ~ "Memreg",
    IRAN == "5" ~ "Autrereg",
    IRAN == "6" ~ "AutreDOM",
    IRAN == "7" ~ "AutreTOMCOM",
    IRAN == "8" ~ "EtrEU",
    IRAN == "9" ~ "Etranger",
    IRAN == "Z" ~ "NewNe",
    TRUE ~ as.character(IRAN)
  ))
data2 <- select(data2,-c(IRAN))
# Encoding IRANUU
data2<- data2 %>%
  mutate(Urbain = case_when(
    IRANUU == "1" ~ "Rural",
    IRANUU == "2" ~ "Rural",
    IRANUU == "3" ~ "Urbain",
    IRANUU == "4" ~ "Urbain",
    IRANUU == "5" ~ "Urbain",
    IRANUU == "Z" ~ "NewNe",
    TRUE ~ as.character(IRANUU)
  ))
data2 <- select(data2,-c(IRANUU))
# Encoding LPRM
data2<- data2 %>%
  mutate(Refmen = case_when(
    LPRM == "1" ~ "Reference",
    LPRM == "2" ~ "Conjoint",
    LPRM == "3" ~ "Enfant",
    LPRM == "4" ~ "Pet-enfant",
    LPRM == "5" ~ "Acendant",
    LPRM == "6" ~ "Parent",
    LPRM == "7" ~ "Ami",
    LPRM == "8" ~ "Sousloc",
    LPRM == "9" ~ "Domestique",
    LPRM == "Z" ~ "HLO",
    TRUE ~ as.character(LPRM)
  ))
data2 <- select(data2,-c(LPRM))
# Encoding METRODOM
data2<- data2 %>%
  mutate(METRODOMenc = case_when(
    METRODOM == "M" ~ "Metro",
    METRODOM == "D" ~ "DOM",
    TRUE ~ as.character(METRODOM)
  ))
data2 <- select(data2,-c(METRODOM))
# Encoding MOCO
data2<- data2 %>%
  mutate(Cohabitation = case_when(
    MOCO == "11" ~ "Enfcoup",
    MOCO == "12" ~ "Enfmono",
    MOCO == "21" ~ "Couplesans",
    MOCO == "22" ~ "Coupleavec",
    MOCO == "23" ~ "Adumono",
    MOCO == "31" ~ "horsfam",
    MOCO == "32" ~ "Seule",
    MOCO == "40" ~ "horsmen",
    TRUE ~ as.character(MOCO)
  ))
data2 <- select(data2,-c(MOCO))
# Encoding NA17 (activite eco 17 classes)
data2<- data2 %>%
  mutate(Eco17 = case_when(
    NA17 == "AZ" ~ "Agri",
    NA17 == "C1" ~ "Alimentaire",
    NA17 == "C2" ~ "Raffinage",
    NA17 == "C3" ~ "Electronique",
    NA17 == "C4" ~ "Transprod",
    NA17 == "C5" ~ "Autreprod",
    NA17 == "DE" ~ "Energie",
    NA17 == "FZ" ~ "Construction",
    NA17 == "GZ" ~ "Commerce",
    NA17 == "HZ" ~ "Transports",
    NA17 == "IZ" ~ "HotelRest",
    NA17 == "JZ" ~ "InforComm",
    NA17 == "KZ" ~ "BanqAss",
    NA17 == "LZ" ~ "Immo",
    NA17 == "MN" ~ "Scitech",
    NA17 == "OQ" ~ "Publique",
    NA17 == "RU" ~ "Autreserv",
    NA17 == "ZZ" ~ "NA",
    TRUE ~ as.character(NA17)
  ))
data2 <- select(data2,-c(NA17))
# Encoding NA5 (activite eco 5 classes)
data2<- data2 %>%
  mutate(Eco5 = case_when(
    NA5 == "AZ" ~ "Agri",
    NA5 == "BE" ~ "Industrie",
    NA5 == "FZ" ~ "Construction",
    NA5 == "GU" ~ "Services",
    NA5 == "OQ" ~ "Publique",
    NA5 == "ZZ" ~ "NA",
    TRUE ~ as.character(NA5)
  ))
data2 <- select(data2,-c(NA5))
# Encoding NPERR
data2<- data2 %>%
  mutate(NPERS = case_when(
    NPERR == "1" ~ "1",
    NPERR == "2" ~ "2",
    NPERR == "3" ~ "3",
    NPERR == "4" ~ "4",
    NPERR == "5" ~ "5",
    NPERR == "6" ~ "6",
    NPERR == "Z" ~ "HLO",
    TRUE ~ as.character(NPERR)
  ))
data2 <- select(data2,-c(NPERR))
data2$NPERS<- as.numeric(data2$NPERS)
# Encoding RECH
data2<- data2 %>%
  mutate(RECHenc = case_when(
    RECH == "0" ~ "Pasrech",
    RECH == "1" ~ "Rech1",
    RECH == "2" ~ "Plus1",
    RECH == "9" ~ "Inactif",
    RECH == "Z" ~ "NA",
    TRUE ~ as.character(RECH)
  ))
data2 <- select(data2,-c(RECH))
# Encoding SEXE
data2<- data2 %>%
  mutate(SEXEenc = case_when(
    SEXE == "1" ~ "Hommes",
    SEXE == "2" ~ "Femmes",
    TRUE ~ as.character(SEXE)
  ))
data2 <- select(data2,-c(SEXE))
# Encoding STOCD
data2<- data2 %>%
  mutate(STOCC = case_when(
    STOCD == "0" ~ "LOInocc",
    STOCD == "10" ~ "Propriet",
    STOCD == "21" ~ "LocnoHLM",
    STOCD == "22" ~ "LocHLM",
    STOCD == "23" ~ "Locmeuble",
    STOCD == "30" ~ "Locgratuit",
    STOCD == "ZZ" ~ "HLO",
    TRUE ~ as.character(STOCD)
  ))
data2 <- select(data2,-c(STOCD))
# Encoding TACT
data2<- data2 %>%
  mutate(TACTenc = case_when(
    TACT == "11" ~ "Actif",
    TACT == "12" ~ "Chomeur",
    TACT == "21" ~ "Retraite",
    TACT == "22" ~ "Etudiant",
    TACT == "23" ~ "Moins14",
    TACT == "24" ~ "FHfoyer",
    TACT == "25" ~ "Incatif",
    TRUE ~ as.character(TACT)
  ))
data2 <- select(data2,-c(TACT))
# Encoding TACTM
data2<- data2 %>%
  mutate(TACTMENenc = case_when(
    TACTM == "11" ~ "Actif",
    TACTM == "12" ~ "Chomeur",
    TACTM == "21" ~ "Retraite",
    TACTM == "22" ~ "Etudiant",
    TACTM == "23" ~ "Moins14",
    TACTM == "24" ~ "FHfoyer",
    TACTM == "25" ~ "Incatif",
    TACTM == "YY" ~ "HResPrinc",
    TACTM == "ZZ" ~ "HLO",
    TRUE ~ as.character(TACTM)
  ))
data2 <- select(data2,-c(TACTM))
# Encoding TRANS
data2<- data2 %>%
  mutate(TRANSenc = case_when(
    TRANS == "1" ~ "Pastransp",
    TRANS == "2" ~ "Pied",
    TRANS == "3" ~ "Velo",
    TRANS == "4" ~ "Moto",
    TRANS == "5" ~ "Voiture",
    TRANS == "6" ~ "Transpublic",
    TRANS == "Z" ~ "NA",
    TRUE ~ as.character(TRANS)
  ))
data2 <- select(data2,-c(TRANS))
# Encoding TYPC
data2<- data2 %>%
  mutate(TYPCONSTR = case_when(
    TYPC == "1" ~ "MaiIsole",
    TYPC == "2" ~ "MaiGroup",
    TYPC == "3" ~ "Multipropr",
    TYPC == "4" ~ "Nonhab",
    TYPC == "5" ~ "Provisoire",
    TYPC == "Y" ~ "HResPrinc",
    TYPC == "Z" ~ "HLO",
    TRUE ~ as.character(TYPC)
  ))
data2 <- select(data2,-c(TYPC))
# Encoding TYPL
data2<- data2 %>%
  mutate(TYPLenc = case_when(
    TYPL == "1" ~ "Maison",
    TYPL == "2" ~ "Appart",
    TYPL == "3" ~ "Logfoyer",
    TYPL == "4" ~ "Hotel",
    TYPL == "5" ~ "Habfortune",
    TYPL == "6" ~ "Pieceindip",
    TYPL == "Z" ~ "HLO",
    TRUE ~ as.character(TYPL)
  ))
data2 <- select(data2,-c(TYPL))
# Encoding TYPMR
data2<- data2 %>%
  mutate(TYPMRenc = case_when(
    TYPMR == "11" ~ "Seul",
    TYPMR == "12" ~ "Seul",
    TYPMR == "20" ~ "Plusieurs",
    TYPMR == "31" ~ "Monoparent",
    TYPMR == "32" ~ "Monoparent",
    TYPMR == "41" ~ "Coupleactif",
    TYPMR == "42" ~ "actconjautre",
    TYPMR == "43" ~ "actconjautre",
    TYPMR == "44" ~ "Coupleautre",
    TYPMR == "ZZ" ~ "HLO",
    TRUE ~ as.character(TYPMR)
  ))
data2 <- select(data2,-c(TYPMR))
# Encoding ANEMC
data2<- data2 %>%
  mutate(ANEMCenc = case_when(
    ANEMC == "0" ~ "Moinsde2",
    ANEMC == "1" ~ "2a4ans",
    ANEMC == "2" ~ "5a9ans",
    ANEMC == "3" ~ "10a19ans",
    ANEMC == "4" ~ "20a29ans",
    ANEMC == "5" ~ "30ouplus",
    ANEMC == "9" ~ "LogOrdInocc",
    ANEMC == "Z" ~ "HLO",
    TRUE ~ as.character(ANEMC)
  ))
data2 <- select(data2,-c(ANEMC))
rm(data)

#############################################################################################
#####################                  REGRESSION LOGISTIQUE            ##################### 
#############################################################################################

datamen<- data2[data2$CATPCenc == "menages",] # Selection de la population des menages seulement
datared<- datamen[sample(1:nrow(datamen),1000000),]
datared$AGEREVQ2<- datared$AGEREVQ^2
# Test de differentes references de categorie.
datared$CSMenc <- relevel(factor(datared$CSMenc), ref = "Cadres")
datared$EMPLenc <- relevel(factor(datared$EMPLenc), ref = "CDI")
datared$DIPLenc <- relevel(factor(datared$DIPLenc), ref = "BAC")
datared$INAIenc <- relevel(factor(datared$INAIenc), ref = "Actuel")
datared$Nationalite <- relevel(factor(datared$Nationalite), ref = "Francais")
datared$METRODOMenc <- relevel(factor(datared$METRODOMenc), ref = "Metro")
datared$Cohabitation <- relevel(factor(datared$Cohabitation), ref = "Couplesans")
datared$Eco5 <- relevel(factor(datared$Eco5), ref = "Services")
datared$STOCC <- relevel(factor(datared$STOCC), ref = "Propriet")
datared$TRANSenc <- relevel(factor(datared$TRANSenc), ref = "Voiture")
datared$ANEMCenc <- relevel(factor(datared$ANEMCenc), ref = "Moinsde2")
summary(glm(change~CSMenc+EMPLenc+DIPLenc+INAIenc+Nationalite+METRODOMenc+Cohabitation+Eco5
            +SEXEenc+STOCC+TRANSenc+AGEREVQ+AGEREVQ2+Urbain
            ,data=datared, weights = datared$IPONDI, family=quasibinomial(link="logit")))
#NA pour Eco5 depend du fait que c'est collineaire � EMPLencNA, les deux representent les chomeurs
## Modele logit complet
modl<- glm(change~CSMenc+EMPLenc+DIPLenc+INAIenc+Nationalite+METRODOMenc+Cohabitation+Eco5
           +SEXEenc+STOCC+TRANSenc+AGEREVQ+AGEREVQ2+Urbain
           ,data=datared, weights = datared$IPONDI, family=quasibinomial(link="logit"))


#############################################################################################
#####################                  REGRESSION LINEAIRE              ##################### 
#############################################################################################
library(dplyr)
library(tidyr)

#################################################
#   On importe la base de donn�es
#################################################
df <- data2[data2$CATPCenc == "menages",] # Base de donn�es avec que les m�nages.
nrow=1e6  # On r�duit � 1 millions de lignes
df <- df[sample(1:nrow(df),nrow),] # On r�duit la base de donn�es � 1 million lignes.
df <- df %>% # On met la variable expliqu�e au d�but du dataframe pour plus de clart�.
  relocate(change)
df_init <- df
#################################################
# On s�l�ctionne les variables de notre mod�le
#################################################

### Premier nettoyage des variables ###

df <- select(df, -c('X')) # Le num�ro de ligne n'est pas utile pour le mod�le
df <- select(df, -c('ResAnte','Urbain')) # On supprime ces 2 variables car ce sont des indicateurs apr�s un d�m�nagement.
df <- select(df, -c('DCRANna','COMMna')) # Ce sont les 2 variables qui nous ont permis de cr�er la variable expliqu�e change (variable binaire qui d�termine si le m�nage a d�m�nag� ou non)
df <- select(df, -c('CATPCenc')) # On a s�l�ctionn� que les m�nages donc cette variable vaut toujours m�nage, elle n'a pas d'int�r�t pour notre mod�le, elle vaut toujours la m�me valeur
df <- select(df, -c('ANEMCenc'))
### R�ductions des corr�lations entre les variables en choisissant certaines plut�t que d'autres ###

## Variable sur des informations li�es au lieu de Naissance.
# Nous avons 3 variables : DNAI, INAI et INACT(Nationalite)
# DNAI repr�sente le num�ro de d�partement de naissance.
# INAI est un indicateur du lieu de naissance en comparaison � la r�sidence actuelle.
# INACT (Nationalite) correspond � la nationalit� du m�nage (Fran�ais ou �tranger).
# Nous consid�rons que INAI est plus pertinente car permet de comparer le lieu de naissance au lieu de logement actuel.
# On choisit donc d'exclure DNAI et INACT, et de garder seulement INAI.
df <- select(df, -c('DNAI','Nationalite'))

## Variable AGE ##
# Nous avons les variables AGEMEN8 et AGEREVQ
# Elles repr�sentent l'�ge regroup� de la personne de r�f�rence et Age quinquennnal en ann�es r�volues respectivement.
# Ces deux variables sont par tranche d'�ge.
# On trouve que la variable AGEREVQ est plus int�r�ssante � prendre en compte car c'est l'�ge du m�nage.
# On exclu donc AGEMEN8 pour garder seulement AGEREVQ.
# Cependant la variable AGEREVQ nous semble trop cat�goris� (trop de tranche d'�ge). On pr�f�re donc re cat�goriser
# la variable avec d'autres tranches comme ci dessous.

df$AGEREVQ <- as.character(df$AGEREVQ) # On met en charact�re pour que la variable reste cat�gorielle
df_init$AGEREVQ <- as.character(df_init$AGEREVQ)

df<- df %>% # On modifie les cat�gories de la variable age en r�duisant le nombre de tranche d'�ge.
  mutate(AGEREV = case_when(
    AGEREVQ == "5" ~ "0",
    AGEREVQ == "10" ~ "0",
    AGEREVQ == "30" ~ "25",
    AGEREVQ == "35" ~ "25",
    AGEREVQ == "45" ~ "40",
    AGEREVQ == "50" ~ "40",
    AGEREVQ == "60" ~ "55",
    AGEREVQ == "70" ~ "65",
    AGEREVQ == "75" ~ "65",
    AGEREVQ == "85" ~ "80",
    AGEREVQ == "90" ~ "80",
    AGEREVQ == "95" ~ "80",
    AGEREVQ == "100" ~ "80",
    AGEREVQ == "105" ~ "80",
    AGEREVQ == "110" ~ "80",
    AGEREVQ == "115" ~ "80",
    AGEREVQ == "120" ~ "80",
    TRUE ~ as.character(AGEREVQ)
  ))
# Notre nouvelle variable �ge est AGEREV.
df <- select(df, -c('AGEREVQ','AGEMEN8')) 

## Variable sur des informations li�es � la cat�gorie d'emploi
# Nous avons les variables : CS1, CSM, NA17(Eco17) et NA5(Eco5).
# On a deux variables CS1 et CSM qui r�pr�sentent soit la cat�gorie socioprofessionnell du m�nage soit celle de son r�f�rant.
# Les 2 variables NA17 et NA5 repr�sentant l'activit� �conomique du m�nage regroup� en 17 et 5 activit�s.
# On d�cide de garder CS1 et d'exclure les autres car cela semble la plus int�r�ssante sur la variable de d�m�nagement.
df <- select(df, -c('CSMenc','Eco17','Eco5'))

## Variable sur des informations li�es � la condition d'emploi
# Nous avons la variables : EMPL, TACT et TACTM
# La variable EMPL repr�sente la condition d'emploi (CDI, CDD, etc...)
# Les variables TACT et TACTM repr�sentent le type d'activit� du m�nage et du r�f�rent du m�nage respectivement.
# L'ensemble de ces variables sont fortement corr�l�es � CSM (variable pr�c�dente dans notre analyse). On les
# supprime donc tous pour notre r�gression.
df <- select(df, -c('TACTenc','EMPLenc','TACTMENenc')) 

## Variable sur les liens du m�nage
# Nous avons les variables : LPRM (Refmen) et MOCO (Cohabitation)
# La variable LPRM repr�sente le lien � la personne de r�f�rence du m�nage
# La variable MOCO est le mode de cohabitation du m�nage.
# Ce sont des variables similaires, on choisit d'en prendre une seule pour �viter une corr�lation entre les variables explicatives
# On supprime LPRM (Refmen) et on garde MOCO
df <- select(df,-c('Refmen'))

## Variable sur le type du batiment du logement
# Nous avons les variables TYPC (TYPCONSTR) et TYPL
# La variable TYPC est le type de construction
# La variable TYPL est le type de logement
# Ces variables sont relativement ressemblantes, on d�cide d'en choisir une seule. On supprime TYPC.
df <- select(df,-c('TYPCONSTR'))

## Variable sur le type du m�nage
# Nous avons les variables TYPMR et MOCO (Cohabitation)
# La variable TYPMR est le type de m�nage regroup� (m�nage vivant seul, dont la famille est monoparentale, ect...)
# La variable MOCO (Cohabitation) est le mode de cohabitation.
# On d�cide donc de garder seulement MOCO (Cohabitation).
df <- select(df,-c('TYPMRenc'))

#################################################
# APPLICATION DU MODELE DE REGRESSION LINEAIRE  #
#################################################
## Premi�re application du mod�le ##
reg <- lm(change~.,select(df,-c('IPONDI')), weights = df$IPONDI)
summary(reg)
# On voit que la variable AGE n'est pas tr�s significative. Pourtant, l'�ge semble �tre un facteur 
# d�terminant au d�m�nagement puisqu'une personne ag�e aura potentiellement moins de chance de d�m�nager
# qu'une personne jeune. On peut l'expliquer par le fait que l'�ge est 
# corr�l� aux autres variables. En effet, une personne � la retraire aura par exemple plus de 65ans. Une 
# personne en recherche d'emploi sera majeure etc...
# On d�cide donc de ne pas prendre en compte l'�ge pour ne pas augmenter la variance des coefficients estim�s des
# autres variables du mod�le. # Cependant, il est possible d'�tudier l'impact de l'�ge sur le d�m�nagement en changeant le
# mod�le choisi. Il faudra alors supprimer les variables corr�l�es avec l'�ge afin de voir son impact sur la variable expliqu�e 
df <- select(df,-c('AGEREV'))

# On voit que certaines variables n'expliquent en rien le d�m�nagement. Par exemple, la variable NPERS qui
# repr�sente le nombre de personnes dans le logement n'est pas significative sur la variable expliqu�e (change).
# De m�me pour le sexe du m�nage, on supprime donc la variable SEXE.
df <- select(df,-c('NPERS','SEXEenc'))

## MODELE FINAL ##
print('Les variables de notre mod�le final sont donc : ')
names(select(df,-c('IPONDI')))
reg <- lm(change~.,select(df,-c('IPONDI')), weights = df$IPONDI)
summary(reg)
# Pour afficher le mod�le sous forme d'un tableau comme dans le rapport :
#tab_model(reg, digits = 3)

#################################################
# VARIABLES DETERMINANTES SUR LE DEMENAGEMENT   #
#################################################
variables_determinantes <- function(top=3){ #Fonction qui renvoie le top des variables les plus d�terminantes en valeur absolue.
  coefs <- reg$coefficients[2:length(reg$coefficients)]
  var_top=c()
  for (j in 1:top){
    val_max <- max(abs(coefs),na.rm=T)
    a <- match(val_max,abs(coefs))
    i <- match(val_max,abs(reg$coefficients))
    var_top <- append(var_top,reg$coefficients[i])
    coefs <- coefs[-c(a)]
  }
  return (var_top)
}
variables_determinantes(5)

# La conclusion est faite dans le rapport, on y d�taille les variables explicatives de notre mod�le
# sur la variable d�m�nagement.








