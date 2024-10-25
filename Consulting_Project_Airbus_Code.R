library(readxl)
library(plm)
library(lmtest)
library(corrplot)
library(ggplot2)
library(dplyr)
library(car)
library(panelvar)
library(stargazer)

must <- read.csv("C:/Users/Boute/Downloads/base.csv", sep = ";")
must2 <- read_excel("C:/Users/Boute/Downloads/MUST_intermediate_data.xlsx")

## base sur laquelle on va trvailler 
must <- merge(must, must2[c("AIRLINE_ID", "YEAR",
                            "LOAD_FACTOR",
                            "MKT_CONCENTRATION",
                            "LH_RATIO",
                            "YIELD",
                            "OPE_PROFIT_RPK",
                            "HHI_REGION",
                            "HHI_GLOBAL")], 
              by = c("AIRLINE_ID","YEAR"), 
              all.x = TRUE)

must$YEAR = as.character(must$YEAR)
## création de cette variable pour différencier nos modèles sur avant-après 2016
must$year_2016 <- ifelse(must$YEAR == "2010" 
                         | must$YEAR == "2011" 
                         | must$YEAR == "2012" 
                         | must$YEAR == "2013" 
                         | must$YEAR == "2014" 
                         | must$YEAR == "2015" 
                         | must$YEAR == "2016"
                         , 0, 1)


must$low_cost <- ifelse(must$LOW_COST_FIN == "LOW_COST_FIN", 1, 0)

## création des dummy selon l'origine
must$EUROPE <- ifelse(must$REGION == "EUROPE", 1, 0)
must$ASIA <- ifelse(must$REGION == "ASIA", 1, 0)
must$NORTH_AMERICA <- ifelse(must$REGION == "NORTH AMERICA", 1, 0)
must$AFRICA <- ifelse(must$REGION == "AFRICA", 1, 0)
must$OCEANIA <- ifelse(must$REGION == "OCEANIA", 1, 0)
must$MIDDLE_EAST <- ifelse(must$REGION == "MIDDLE EAST", 1, 0)
must$SOUTH_AMERICA <- ifelse(must$REGION == "SOUTH AMERICA", 1, 0)

must$Government <- ifelse(must$CTRL_TYPE == "Government", 1, 0)
must$Airline <- ifelse(must$CTRL_TYPE == "Airline", 1, 0)
must$Private_Investor <- ifelse(must$CTRL_TYPE == "Private Investor", 1, 0)

must$FUEL_COSTS_ASK = as.numeric(must$FUEL_COSTS_ASK)
must$l_FUEL_COSTS_ASK = log(must$FUEL_COSTS_ASK)

must$CASK = as.numeric(must$CASK)
must$l_CASK = log(must$CASK)

must$GDPpCAPITA_CURRENT_REGION = as.numeric(must$GDPpCAPITA_CURRENT_REGION)
must$l_GDPpCAPITA_CURRENT_REGION = log(must$GDPpCAPITA_CURRENT_REGION)

must$ASK_m_final = as.numeric(must$ASK_m_final)
must$l_ASK_m_final = log(must$ASK_m_final)

must$UTKT_PRICE = as.numeric(must$UTKT_PRICE)
must$l_UTKT_PRICE = log(must$UTKT_PRICE)

must$MKT_CONCENTRATION = as.numeric(must$MKT_CONCENTRATION)
must$l_MKT_CONCENTRATION = 100 * must$MKT_CONCENTRATION

must$CPI_REGION = as.numeric(must$CPI_REGION)
must$l_CPI_REGION = log(must$CPI_REGION)

must$LH_RATIO = as.numeric(must$LH_RATIO)
must$LH_RATIO <- round(must$LH_RATIO, 4)
must$l_LH_RATIO= 100 * must$LH_RATIO

must$LOAD_FACTOR = as.numeric(must$LOAD_FACTOR)
must$l_LOAD_FACTOR = 100 * must$LOAD_FACTOR

## création de la variable cout autre pour enlever l'effet de fuel
must$cout_autre = must$CASK - must$FUEL_COSTS_ASK
must$l_cout_autre = log(must$cout_autre)

must$l_FUEL_JET_GULF = log(must$FUEL_JET_GULF)

must$l_RPK = log(must$RPKs_m)


## création d'un variable dummy selon la taille en ASK
catégorie = print(boxplot(tapply(must$TOTAL_ASK_m,must$AIRLINE_ID,min)), plot = FALSE)
must$big = ifelse(must$TOTAL_ASK_m >= catégorie$stats[5,1], 1, 0)
must$medium = ifelse(must$TOTAL_ASK_m < catégorie$stats[5,1]
                     & must$TOTAL_ASK_m >= (catégorie$stats[2,1] + catégorie$stats[3,1])/2
                     , 1, 0)
must$small = ifelse(must$TOTAL_ASK_m < (catégorie$stats[2,1] + catégorie$stats[3,1])/2
                    , 1, 0)


## création d'un variable dummy selon % de vol long
must$big_lh = ifelse(must$LH_RATIO >= 0.2, 1, 0)
must$medium_lh = ifelse(must$LH_RATIO < 0.2& must$LH_RATIO > 0 , 1, 0)
must$small_lh = ifelse(must$LH_RATIO == 0 , 1, 0)

must$YIELD = as.numeric(must$YIELD)
must$OPE_PROFIT_RPK = as.numeric(must$OPE_PROFIT_RPK)
must$HHI_REGION = as.numeric(must$HHI_REGION)
must$HHI_GLOBAL = as.numeric(must$HHI_GLOBAL)


## on renomme pour plus de clarté
must$log_UTKT_PRICE = must$l_UTKT_PRICE
must$log_FUEL_JET_GULF = must$l_FUEL_JET_GULF
must$log_COST_management = must$l_cout_autre
must$log_ASK = must$l_ASK_m_final
must$LOAD_FACTOR_100 = must$l_LOAD_FACTOR
must$LH_RATIO_100 = must$l_LH_RATIO
must$MKT_CONCENTRATION_100 = must$l_MKT_CONCENTRATION
must$log_GDPpCAPITA_CURRENT_REGION = must$l_GDPpCAPITA_CURRENT_REGION
must$IS_LOW_COST = must$low_cost
must$BELONGS_to_GOVERNMENT = must$Government
must$BELONGS_to_PRIVATE_I = must$Private_Investor


################################################################################
################################################################################
## étude simple et bivariée

# Calculer la matrice des corrélations
correlation_matrix <- cor(must[c("YIELD","FUEL_COSTS_ASK","FUEL_JET_GULF","CASK",
                                 "UTKT_PRICE","OPE_PROFIT_RPK","ASK_m_final","RPKs_m",
                                 "LOAD_FACTOR","LH_RATIO","HHI_REGION",
                                 "HHI_GLOBAL","GDPpCAPITA_CURRENT_REGION",
                                 "CPI_REGION")])

# Afficher la matrice des corrélations avec corrplot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.5)

rm(correlation_matrix)


# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(YIELD ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
p=plot(mean_x_by_year$YEAR, mean_x_by_year$YIELD, type = "l", 
       xlab = "Année", ylab = "Moyenne du Yield",
       main = "Moyenne du YIELD par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(FUEL_COSTS_ASK ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$FUEL_COSTS_ASK, type = "l", 
     xlab = "Année", ylab = "Moyenne du FUEL_COSTS_ASK",
     main = "Moyenne du FUEL_COSTS_ASK par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(FUEL_JET_GULF ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$FUEL_JET_GULF, type = "l", 
     xlab = "Année", ylab = "Moyenne du FUEL_JET_GULF",
     main = "Moyenne du FUEL_JET_GULF par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(CASK ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$CASK, type = "l", 
     xlab = "Année", ylab = "Moyenne du CASK",
     main = "Moyenne du CASK par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(OPE_PROFIT_RPK ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$OPE_PROFIT_RPK, type = "l", 
     xlab = "Année", ylab = "Moyenne du OPE_PROFIT_RPK",
     main = "Moyenne du OPE_PROFIT_RPK par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(LOAD_FACTOR ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$LOAD_FACTOR, type = "l", 
     xlab = "Année", ylab = "Moyenne du LOAD_FACTOR",
     main = "Moyenne du LOAD_FACTOR par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(UTKT_PRICE ~ YEAR, data = must, FUN = mean)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$UTKT_PRICE, type = "l", 
     xlab = "Année", ylab = "Moyenne du UTKT_PRICE",
     main = "Moyenne du UTKT_PRICE par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(ASK_m_final ~ YEAR, data = must, FUN = sum)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$ASK_m_final, type = "l", 
     xlab = "Année", ylab = "Moyenne du ASK_m_final",
     main = "Moyenne du ASK_m_final par année")

# Calculer la moyenne de x par année
mean_x_by_year <- aggregate(RPKs_m ~ YEAR, data = must, FUN = sum)
# Tracer le graphique
plot(mean_x_by_year$YEAR, mean_x_by_year$RPKs_m, type = "l", 
     xlab = "Année", ylab = "Moyenne du RPKs_m",
     main = "Moyenne du RPKs_m par année")




# Calculer la moyenne de ...par année et par modalités de Z
mean_x_by_year <- aggregate(YIELD ~ YEAR + REGION, data = must, FUN = mean)
# Tracé du graphique en utilisant ggplot2
ggplot(data = mean_x_by_year, aes(x = YEAR, y = YIELD, group = REGION, color = REGION)) +
  geom_line() +
  labs(title = "Moyenne du YIELD",
       x = "Année", y = "Moyenne du YIELD") +
  theme_minimal()

rm(mean_x_by_year)


# Tri des données par YIELD de façon décroissante
must_sorted <- must[order(-must$ASK_m_final), ]
# Sélection des 5 premiers individus
top_5_individuals <- unique(head(must_sorted, 1)$AIRLINE_ID)
# Filtrage des données pour ne garder que les individus sélectionnés
top_5_data <- subset(must, AIRLINE_ID %in% top_5_individuals)

# Tracé de l'évolution de YIELD pour les individus sélectionnés
ggplot(data = top_5_data, aes(x = YEAR, y = FUEL_JET_GULF, group = as.factor(AIRLINE_ID))) +
  geom_line(aes(color = as.factor(AIRLINE_ID))) +
  labs(title = "Évolution de FUEL_JET_GULF pour les individus avec les plus grandes valeurs",
       x = "Année", y = "FUEL_JET_GULF")

# Tracé de l'évolution de FUEL_COSTS_ASK pour les individus sélectionnés
ggplot(data = top_5_data, aes(x = YEAR, y = FUEL_COSTS_ASK, group = as.factor(AIRLINE_ID))) +
  geom_line(aes(color = as.factor(AIRLINE_ID))) +
  labs(title = "Évolution de FUEL_COSTS_ASK pour les individus avec les plus grandes valeurs",
       x = "Année", y = "FUEL_COSTS_ASK")

# Tracé de l'évolution de CASK pour les individus sélectionnés
ggplot(data = top_5_data, aes(x = YEAR, y = CASK, group = as.factor(AIRLINE_ID))) +
  geom_line(aes(color = as.factor(AIRLINE_ID))) +
  labs(title = "Évolution de CASK pour les individus avec les plus grandes valeurs",
       x = "Année", y = "CASK")


rm(must_sorted)
rm(top_5_data)
rm(top_5_individuals)

plot(must$LOAD_FACTOR,must$YIELD)
# Calcul de la droite de régression linéaire
regression <- lm(must$YIELD ~ must$LOAD_FACTOR)
# Ajout de la droite de régression au graphique
abline(regression, col = "red")



################################################################################
################################################################################
## dans tous les modèles à suivre il vous faudra jouer avec les variables et arguments

## OLS effet fixe individuel
OLS = lm(log_UTKT_PRICE ~ 
           lag(log_FUEL_JET_GULF)
         + lag(log_COST_management)
         + lag(log_ASK)
         + lag(LOAD_FACTOR_100)
         + lag(LH_RATIO_100)
         + lag(MKT_CONCENTRATION_100)
         + lag(log_GDPpCAPITA_CURRENT_REGION)
         # log_FUEL_JET_GULF
         # log_FUEL_COSTS_ASK
         # + log_COST_management
         # + log_ASK
         # + LOAD_FACTOR_100
         # + LH_RATIO_100
         # + MKT_CONCENTRATION_100
         # + log_GDPpCAPITA_CURRENT_REGION
         + IS_LOW_COST
         + BELONGS_to_GOVERNMENT
         + BELONGS_to_PRIVATE_I 
         + ASIA
         + NORTH_AMERICA
         + AFRICA
         + OCEANIA
         + MIDDLE_EAST
         + SOUTH_AMERICA
         + AIRLINE_ID
         + (lag(log_FUEL_JET_GULF) * IS_LOW_COST)
         # + (lag(log_FUEL_JET_GULF) * year_2016)
         # + (lag(log_FUEL_JET_GULF) * big_lh)
         # + (lag(log_FUEL_JET_GULF) * medium_lh)
         ,data=must)

## OLS effet fixe temporel
OLS2 = lm(log_UTKT_PRICE ~ 
            #   lag(log_FUEL_JET_GULF)
            # + lag(log_COST_management)
            # + lag(log_ASK)
            # + lag(LOAD_FACTOR_100) 
            # + lag(LH_RATIO_100) 
            # + lag(MKT_CONCENTRATION_100)
            # + lag(log_GDPpCAPITA_CURRENT_REGION)
          log_FUEL_COSTS_ASK
          + log_COST_management
          + log_ASK
          + LOAD_FACTOR_100
          + LH_RATIO_100
          + MKT_CONCENTRATION_100
          + log_GDPpCAPITA_CURRENT_REGION
          + IS_LOW_COST
          + BELONGS_to_GOVERNMENT
          + BELONGS_to_PRIVATE_I 
          + ASIA
          + NORTH_AMERICA
          + AFRICA
          + OCEANIA
          + MIDDLE_EAST
          + SOUTH_AMERICA
          + YEAR
          ,data=must)

## within effet fixe individuel
within = plm(log_UTKT_PRICE ~ 
               lag(log_FUEL_JET_GULF)
             + lag(log_COST_management)
             + lag(log_ASK)
             + lag(LOAD_FACTOR_100)
             + lag(LH_RATIO_100)
             + lag(MKT_CONCENTRATION_100)
             + lag(log_GDPpCAPITA_CURRENT_REGION)
             # log_FUEL_JET_GULF
             # log_FUEL_COSTS_ASK
             # + log_COST_management
             # + log_ASK
             # + LOAD_FACTOR_100
             # + LH_RATIO_100
             # + MKT_CONCENTRATION_100
             # + log_GDPpCAPITA_CURRENT_REGION
             + IS_LOW_COST
             + BELONGS_to_GOVERNMENT
             + BELONGS_to_PRIVATE_I 
             + ASIA
             + NORTH_AMERICA
             + AFRICA
             + OCEANIA
             + MIDDLE_EAST
             + SOUTH_AMERICA
             + (lag(log_FUEL_JET_GULF) * IS_LOW_COST)
             # + (lag(log_FUEL_JET_GULF) * year_2016)
             # + (lag(log_FUEL_JET_GULF) * big_lh)
             # + (lag(log_FUEL_JET_GULF) * medium_lh)
             ,data=must,
             model = "within",
             effect = "individual",
             index = c("AIRLINE_ID","YEAR"))

## within effet fixe temporel
within2 = plm(log_UTKT_PRICE ~ 
                log_FUEL_JET_GULF
              # log_FUEL_COSTS_ASK
              + log_COST_management
              + log_ASK
              + LOAD_FACTOR_100
              + LH_RATIO_100
              + MKT_CONCENTRATION_100
              + log_GDPpCAPITA_CURRENT_REGION
              + IS_LOW_COST
              + BELONGS_to_GOVERNMENT
              + BELONGS_to_PRIVATE_I 
              + ASIA
              + NORTH_AMERICA
              + AFRICA
              + OCEANIA
              + MIDDLE_EAST
              + SOUTH_AMERICA
              ,data=must ,
              model = "within",
              effect = "time",
              index = c("AIRLINE_ID","YEAR"))

## pour l'affichage des résultats
stargazer(OLS, within,
          title = "Results",
          align = TRUE, 
          omit = c("AIRLINE_ID","YEAR", "ASIA","NORTH_AMERICA",
                   "AFRICA","OCEANIA","MIDDLE_EAST","SOUTH_AMERICA"),
          type = "text",
          style = "qje",
          single.row = FALSE, star.cutoffs = c(0.05, 0.01, 0.001),
          star.char = c("*", "**", "***"),
          header = FALSE,
          digits = 3,
          model.names = TRUE,
          omit.stat = "F",
          column.labels = c("Inidividual","Inidividual"),
          add.lines = list(c("Variable omise", "REGION", ""),
                           c("Variable omise", "ID", "ID"),
                           c("","","")))


## pour vérifier les résultats d'une régression

vif(OLS_variation)
#produce residual vs. fitted plot
plot(fitted(OLS_variation), OLS_variation$residuals) +
  abline(0,0)
#create Q-Q plot for residuals
qqnorm(OLS_variation$residuals) +
  qqline(OLS_variation$residuals)
#Create density plot of residuals
plot(density(OLS_variation$residuals))



## système GMM avec PGMM
z1 <- pgmm(log_UTKT_PRICE ~ 
             lag(log_FUEL_JET_GULF,1)
           + lag(log_COST_management,1)
           + lag(log_ASK,1)
           + lag(LOAD_FACTOR_100,1)
           + lag(LH_RATIO_100,1)
           + lag(MKT_CONCENTRATION_100,1)
           + lag(log_GDPpCAPITA_CURRENT_REGION,1)
           + low_cost
           + Government
           + Private_Investor
           + ASIA
           + NORTH_AMERICA
           + AFRICA
           + OCEANIA
           + MIDDLE_EAST
           + SOUTH_AMERICA
           | lag(log_UTKT_PRICE, 1:99)
           # + lag(log_COST_management, 2:99)
           # + lag(log_ASK, 2:99)
           # + lag(MKT_CONCENTRATION_100, 2:99)
           # + lag(LH_RATIO_100, 2:99)
           # + lag(LOAD_FACTOR_100, 2:99)
           # + lag(log_GDPpCAPITA_CURRENT_REGION, 2:99)
           |
             lag(log_COST_management, 2)
           + lag(log_ASK, 2)
           + lag(MKT_CONCENTRATION_100, 2)
           + lag(LH_RATIO_100, 2)
           + lag(LOAD_FACTOR_100, 2)
           + lag(log_GDPpCAPITA_CURRENT_REGION, 2)
           # + lag(log_COST_management, 3)
           # + lag(log_ASK, 3)
           # + lag(MKT_CONCENTRATION_100, 2)
           # + lag(LH_RATIO_100, 3)
           # + lag(LOAD_FACTOR_100, 3)
           # + lag(log_GDPpCAPITA_CURRENT_REGION, 3)
           ,
           data = must, effect = "individual", model = "twosteps",
           collapse = TRUE, 
           robust = TRUE ,
           transformation = "ld" )
summary(z1)



## pvar 

must$l_FUEL_JET_GULF_previous = log(lag(must$FUEL_JET_GULF))
must$l_FUEL_JET_GULF_low_cost = log(must$FUEL_JET_GULF) * must$low_cost
must$l_FUEL_JET_GULF_low_cost
must$l_FUEL_JET_GULF_previous_low_cost = must$l_FUEL_JET_GULF_previous * must$low_cost
must$l_FUEL_JET_GULF_previous_low_cost

Pvar_M0 <- pvargmm(dependent_vars = c("l_UTKT_PRICE", 'l_RPK'),
                   lags = 1,
                   exog_vars = c("l_FUEL_JET_GULF",
                                 "l_cout_autre",
                                 "l_ASK_m_final",
                                 "l_MKT_CONCENTRATION",
                                 "l_LH_RATIO",
                                 "l_GDPpCAPITA_CURRENT_REGION"
                                 
                   ),
                   
                   predet_vars = c("low_cost",
                                   "Government",
                                   "Private_Investor"
                                   
                   ),
                   transformation = "fd",
                   data = must,
                   panel_identifier = c("AIRLINE_ID", "YEAR"),
                   steps = c("twostep"),
                   system_instruments = TRUE,
                   max_instr_dependent_vars = 99,
                   max_instr_predet_vars = 99,
                   min_instr_dependent_vars = 2L,
                   min_instr_predet_vars = 1L,
                   collapse = TRUE,
                   progressbar = TRUE)

Pvar_M1 <- pvargmm(dependent_vars = c("l_UTKT_PRICE", 'l_RPK'),
                   lags = 1,
                   exog_vars = c("l_FUEL_JET_GULF",
                                 "l_cout_autre",
                                 "l_ASK_m_final",
                                 "l_MKT_CONCENTRATION",
                                 "l_LH_RATIO",
                                 "l_GDPpCAPITA_CURRENT_REGION",
                                 "l_FUEL_JET_GULF_low_cost"
                   ),
                   
                   predet_vars = c("low_cost",
                                   "Government",
                                   "Private_Investor"
                                   
                   ),
                   transformation = "fd",
                   data = must,
                   panel_identifier = c("AIRLINE_ID", "YEAR"),
                   steps = c("twostep"),
                   system_instruments = TRUE,
                   max_instr_dependent_vars = 99,
                   max_instr_predet_vars = 99,
                   min_instr_dependent_vars = 2L,
                   min_instr_predet_vars = 1L,
                   collapse = TRUE,
                   progressbar = TRUE)


summary(Pvar_M0)
summary(Pvar_M1)