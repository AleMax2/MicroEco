library(dplyr)
data <- read.csv('BDD_reduce.csv', sep=";")
data<- data[sample(1:nrow(data),2000000),]
data$DCRANna<- as.numeric(data$DCRAN)# il elimine la corse, mais il elimine le 0 au debut.
data$COMMna<- as.numeric(data$COMMUNE)
data$change<- ifelse(data$COMMna != data$DCRANna,1,0)
sum(data$change, na.rm=T)
sum(is.na(data$change))
#test <- subset(data, is.na(change)) # ce qu'on vien d'eliminer
data2 <- subset(data, !is.na(change))
data2 <- select(data2,-c(COMMUNE, ARM, DCRAN))
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
    IRANUU == "1" ~ "Ruralememe",
    IRANUU == "2" ~ "Ruralehors",
    IRANUU == "3" ~ "Urbainmeme",
    IRANUU == "4" ~ "Urbainmunit",
    IRANUU == "5" ~ "Urbainhors",
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
# L'age avec les valeurs inferieures as numeric.
# data2$AGEREVQnum<- as.numeric(data2$AGEREVQ)
# Il n'y a pas de nouveaux nees.
#table(data2$Urbain)
# Che variabili usare? Che variabili descrivere? Che variabili usare nel modello logit?
# Exploring CATPCenc
#table(data2$CATPCenc)
#Checking how many HLO
#table(data2$ANEMCenc)
# Checking ho many hors residence principale.
#table(data2$TACTMENenc)
# Starting the modelling
#summary(lm(change~CATPCenc,data=data2))
# Selecting que la population des menages
rm(data)
datamen<- data2[data2$CATPCenc == "menages",]
rm(data2)
# After selecting que la pop des menages on essaye de modeliser.
datared<- datamen[sample(1:nrow(datamen),1000000),]
datared$AGEREVQ2<- datared$AGEREVQ^2
# Testing different reference categories
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
            +NPERS+SEXEenc+STOCC+TRANSenc+ANEMCenc+AGEREVQ+AGEREVQ2
            ,data=datared, weights = datared$IPONDI, family=quasibinomial))
#NA pour Eco5 depend du fait que c'est collineaire à EMPLencNA, les deux representent les chomeurs
## Modele complet
modl<- glm(change~CSMenc+EMPLenc+DIPLenc+INAIenc+Nationalite+METRODOMenc+Cohabitation+Eco5
           +NPERS+SEXEenc+STOCC+TRANSenc+ANEMCenc+AGEREVQ+AGEREVQ2
           ,data=datared, weights = datared$IPONDI, family=quasibinomial)
#A plotter la relation entre l'age et la probabilité de demenager pour montrer le terme au carré
summary(modl)
# To change reference category
# datared$x <- relevel(factor(datared$x, ref = 2)
