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
reg <- lm(change~.,df, weights = df$IPONDI)
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

#################################################
# VARIABLES DETERMINANTES SUR LE DEMENAGEMENT   #
#################################################
max(abs(reg$coefficients[2:(length(reg$coefficients)-1)]),na.rm=T)




#################################################
#   BORUILLON
#################################################

mod<- glm(change~.,data=df, weights = df$IPONDI, family=quasibinomial)
summary(mod)


## Variable sur des informations li�es � la condition de recherche d'emploi
# Nous avons la variable RECH
# La variable RECH repr�sente l'anciennet� de recherche d'emploi.
# df <- select(df, -c('RECHenc'))

# On voit que la variable TACT poss�de des NA dans la r�gression. En effet, la variable TACTencRetraite
# repr�sentant les retrait�s est similaire � une autre variable prise dans la r�gression : CS1encRetraites
# On doit donc en supprimer une, on d�cide donc de supprimer TACT.
df <- select(df, -c('TACTenc'))

## Variable sur le type du batiment du logement
# Nous avons les variables TYPC (TYPCONSTR), TYPL et STOCD (STOCC)
# La variable TYPC est le type de construction
# La variable TYPL est le type de logement
# La variable STOCD est le statut d'occupation d�taill� du logement.
# Ces variables sont relativement ressemblantes, on d�cide d'en choisir une seule.


# data <- read.csv2(file_directory,sep=',',
#                   nrows = nrows,
#                   colClasses = (ACHLR=as.numeric(),
#                                 AGEMEN8 = as.numeric())
#                   )
df <- data2
#df <- select(df, -c('X'))
#df <- df %>%
  #mutate(MOVE=(!(COMMna==DCRANna))*1) ## On cr�e une variable qui vaut 0 si pas d�m�nager et 1 sinon
df <- df %>%
  select(-c('COMMna','DCRANna')) #'METRODOMenc'
df <- df %>% 
  relocate(change) ## Mets la colonne MOVE au d�but
df <- df %>%
  select(-c('METRODOMenc','TACTenc', 'ResAnte','Urbain')) # Cette variable (TACT) est trop corr�l�e � une autre.
                                                # La variable ResAnte (IRAN sur la doc de l'insee) permet de connaitre
                                                # les d�tails du d�m�ngement..  donc � supp osef dans notre modele
                                                # forc�ment elle est parfaitement cor�l�e.
                                                # m�me d�lire pour Urbain, c'est une donn�e que l'on a apr�s le
                                                # le d�m�nagement.

reg <- lm(change~.,df)
max(abs(reg$coefficients[2:length(reg$coefficients)]),na.rm=T)

# for (i in 1:(ncol(df))){
#   u <- unique(df[,i])
#   if (length(u)<=3){
#     print(u)
#     print(i)
#     print(' ')
#   }
# }







df$COMMUNE <- as.character(df$COMMUNE)
df$DCRAN <- as.character(df$DCRAN)
df <- df %>%
  drop_na(COMMUNE,DCRAN)
df <- df %>%
  mutate(MOOVE=(!(COMMUNE==DCRAN))*1)


#### EN MODE BOURIN ####

df$ACHLR <- as.numeric(df$ACHLR)
df$AGEMEN8 <- as.numeric(df$AGEMEN8)
df$ANEMC <- as.numeric(df$ANEMC)
df$CSM <- as.numeric(df$CSM)
df$DIPL <- as.numeric(df$DIPL)

reg <- lm(data=df, MOVE~.) ; summary(reg)


for (i in 1:ncol(df)){
  if (length(unique(df[,i]))==1){
    print(i)
  }
}

summary(lm(change~INAIenc,df_init))$r.squared 
summary(lm(change~DNAI,df_init))$r.squared

BIC(lm(change~INAIenc,df_init))
BIC(lm(change~DNAI,df_init))
