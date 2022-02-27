#############################################################################################
#####################                  REGRESSION LINEAIRE              ##################### 
#############################################################################################
library(dplyr)
library(tidyr)

#################################################
#   On importe la base de données
#################################################
df <- data2[data2$CATPCenc == "menages",] # Base de données avec que les ménages.
nrow=1e6  # On réduit à 1 millions de lignes
df <- df[sample(1:nrow(df),nrow),] # On réduit la base de données à 1 million lignes.
df <- df %>% # On met la variable expliquée au début du dataframe pour plus de clarté.
  relocate(change)
df_init <- df
#################################################
# On séléctionne les variables de notre modèle
#################################################

### Premier nettoyage des variables ###

df <- select(df, -c('X')) # Le numéro de ligne n'est pas utile pour le modèle
df <- select(df, -c('ResAnte','Urbain')) # On supprime ces 2 variables car ce sont des indicateurs après un déménagement.
df <- select(df, -c('DCRANna','COMMna')) # Ce sont les 2 variables qui nous ont permis de créer la variable expliquée change (variable binaire qui détermine si le ménage a déménagé ou non)
df <- select(df, -c('CATPCenc')) # On a séléctionné que les ménages donc cette variable vaut toujours ménage, elle n'a pas d'intérêt pour notre modèle, elle vaut toujours la même valeur
df <- select(df, -c('ANEMCenc'))
### Réductions des corrélations entre les variables en choisissant certaines plutôt que d'autres ###

  ## Variable sur des informations liées au lieu de Naissance.
# Nous avons 3 variables : DNAI, INAI et INACT(Nationalite)
# DNAI représente le numéro de département de naissance.
# INAI est un indicateur du lieu de naissance en comparaison à la résidence actuelle.
# INACT (Nationalite) correspond à la nationalité du ménage (Français ou étranger).
# Nous considérons que INAI est plus pertinente car permet de comparer le lieu de naissance au lieu de logement actuel.
# On choisit donc d'exclure DNAI et INACT, et de garder seulement INAI.
df <- select(df, -c('DNAI','Nationalite'))

  ## Variable AGE ##
# Nous avons les variables AGEMEN8 et AGEREVQ
# Elles représentent l'âge regroupé de la personne de référence et Age quinquennnal en années révolues respectivement.
# Ces deux variables sont par tranche d'âge.
# On trouve que la variable AGEREVQ est plus intéréssante à prendre en compte car c'est l'âge du ménage.
# On exclu donc AGEMEN8 pour garder seulement AGEREVQ.
# Cependant la variable AGEREVQ nous semble trop catégorisé (trop de tranche d'âge). On préfére donc re catégoriser
# la variable avec d'autres tranches comme ci dessous.

df$AGEREVQ <- as.character(df$AGEREVQ) # On met en charactére pour que la variable reste catégorielle
df_init$AGEREVQ <- as.character(df_init$AGEREVQ)

df<- df %>% # On modifie les catégories de la variable age en réduisant le nombre de tranche d'âge.
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
# Notre nouvelle variable âge est AGEREV.
df <- select(df, -c('AGEREVQ','AGEMEN8')) 

  ## Variable sur des informations liées à la catégorie d'emploi
# Nous avons les variables : CS1, CSM, NA17(Eco17) et NA5(Eco5).
# On a deux variables CS1 et CSM qui réprésentent soit la catégorie socioprofessionnell du ménage soit celle de son référant.
# Les 2 variables NA17 et NA5 représentant l'activité économique du ménage regroupé en 17 et 5 activités.
# On décide de garder CS1 et d'exclure les autres car cela semble la plus intéréssante sur la variable de déménagement.
df <- select(df, -c('CSMenc','Eco17','Eco5'))

  ## Variable sur des informations liées à la condition d'emploi
# Nous avons la variables : EMPL, TACT et TACTM
# La variable EMPL représente la condition d'emploi (CDI, CDD, etc...)
# Les variables TACT et TACTM représentent le type d'activité du ménage et du référent du ménage respectivement.
# L'ensemble de ces variables sont fortement corrélées à CSM (variable précédente dans notre analyse). On les
# supprime donc tous pour notre régression.
df <- select(df, -c('TACTenc','EMPLenc','TACTMENenc')) 

  ## Variable sur les liens du ménage
# Nous avons les variables : LPRM (Refmen) et MOCO (Cohabitation)
# La variable LPRM représente le lien à la personne de référence du ménage
# La variable MOCO est le mode de cohabitation du ménage.
# Ce sont des variables similaires, on choisit d'en prendre une seule pour éviter une corrélation entre les variables explicatives
# On supprime LPRM (Refmen) et on garde MOCO
df <- select(df,-c('Refmen'))

  ## Variable sur le type du batiment du logement
# Nous avons les variables TYPC (TYPCONSTR) et TYPL
# La variable TYPC est le type de construction
# La variable TYPL est le type de logement
# Ces variables sont relativement ressemblantes, on décide d'en choisir une seule. On supprime TYPC.
df <- select(df,-c('TYPCONSTR'))
 
  ## Variable sur le type du ménage
# Nous avons les variables TYPMR et MOCO (Cohabitation)
# La variable TYPMR est le type de ménage regroupé (ménage vivant seul, dont la famille est monoparentale, ect...)
# La variable MOCO (Cohabitation) est le mode de cohabitation.
# On décide donc de garder seulement MOCO (Cohabitation).
df <- select(df,-c('TYPMRenc'))

#################################################
# APPLICATION DU MODELE DE REGRESSION LINEAIRE  #
#################################################
## Première application du modèle ##
reg <- lm(change~.,df, weights = df$IPONDI)
summary(reg)
# On voit que la variable AGE n'est pas très significative. Pourtant, l'âge semble être un facteur 
# déterminant au déménagement puisqu'une personne agée aura potentiellement moins de chance de déménager
# qu'une personne jeune. On peut l'expliquer par le fait que l'âge est 
# corrélé aux autres variables. En effet, une personne à la retraire aura par exemple plus de 65ans. Une 
# personne en recherche d'emploi sera majeure etc...
# On décide donc de ne pas prendre en compte l'âge pour ne pas augmenter la variance des coefficients estimés des
# autres variables du modèle. # Cependant, il est possible d'étudier l'impact de l'âge sur le déménagement en changeant le
# modèle choisi. Il faudra alors supprimer les variables corrélées avec l'âge afin de voir son impact sur la variable expliquée 
df <- select(df,-c('AGEREV'))

# On voit que certaines variables n'expliquent en rien le déménagement. Par exemple, la variable NPERS qui
# représente le nombre de personnes dans le logement n'est pas significative sur la variable expliquée (change).
# De même pour le sexe du ménage, on supprime donc la variable SEXE.
df <- select(df,-c('NPERS','SEXEenc'))

## MODELE FINAL ##
print('Les variables de notre modèle final sont donc : ')
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


## Variable sur des informations liées à la condition de recherche d'emploi
# Nous avons la variable RECH
# La variable RECH représente l'ancienneté de recherche d'emploi.
# df <- select(df, -c('RECHenc'))

# On voit que la variable TACT possède des NA dans la régression. En effet, la variable TACTencRetraite
# représentant les retraités est similaire à une autre variable prise dans la régression : CS1encRetraites
# On doit donc en supprimer une, on décide donc de supprimer TACT.
df <- select(df, -c('TACTenc'))

## Variable sur le type du batiment du logement
# Nous avons les variables TYPC (TYPCONSTR), TYPL et STOCD (STOCC)
# La variable TYPC est le type de construction
# La variable TYPL est le type de logement
# La variable STOCD est le statut d'occupation détaillé du logement.
# Ces variables sont relativement ressemblantes, on décide d'en choisir une seule.


# data <- read.csv2(file_directory,sep=',',
#                   nrows = nrows,
#                   colClasses = (ACHLR=as.numeric(),
#                                 AGEMEN8 = as.numeric())
#                   )
df <- data2
#df <- select(df, -c('X'))
#df <- df %>%
  #mutate(MOVE=(!(COMMna==DCRANna))*1) ## On crée une variable qui vaut 0 si pas déménager et 1 sinon
df <- df %>%
  select(-c('COMMna','DCRANna')) #'METRODOMenc'
df <- df %>% 
  relocate(change) ## Mets la colonne MOVE au début
df <- df %>%
  select(-c('METRODOMenc','TACTenc', 'ResAnte','Urbain')) # Cette variable (TACT) est trop corrélée à une autre.
                                                # La variable ResAnte (IRAN sur la doc de l'insee) permet de connaitre
                                                # les détails du déméngement..  donc à supp osef dans notre modele
                                                # forcément elle est parfaitement corélée.
                                                # même délire pour Urbain, c'est une donnée que l'on a après le
                                                # le déménagement.

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
