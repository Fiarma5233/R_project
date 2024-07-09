# instal packages 
install.packages('tidymodels')
install.packages("rsample")
install.packages('dplyr')

 # load libraries
library(tidyverse) # for data manipulation and visualization  
library(tidymodels) # for creation linear & logistic regression  models
library(gridExtra) # for arranging visualiziing
library(Amelia) # for dealing with missing data 
library(psych) # for data visualizations and  descriptives statistics
library(vip) # for visualizing data variable importance in models
library(extrafont) # for data font customization
library(caret) # for additional modelanalysis
library(rsample) # for creating data splits and resamples
library(dplyr)


# import data

# duplicate the data for EDA
eda_data = Apex_Trust_Dataset

colnames(eda_data)

# assign  all the categorical  columns names to an object
cat_cols  <- c(
                "status","credit_history","purpose","savings","employment_duration","installment_rate",
                "other_debtors", "present_residence", "property", "other_installment_plans", 
                "housing", "number_credits", "job" , "people_liable", "telephone", 
                "foreign_worker",  "credit_risk")

#convert columns to a  factor variable
eda_data[,cat_cols] <- lapply(eda_data[,cat_cols],factor)

# rename variable values to something more meaningful for EDA
eda_data = eda_data|> mutate(credit_risk = ifelse(credit_risk == 0, 'bad', 'good'))

eda_data$status = ifelse(eda_data$status == 1 ,'no checking account',
                         ifelse(eda_data$status == 2, '<0 USD',
                                ifelse(eda_data$status == 3, '0 USD >= & <200 USD', '>= 200 USD')))

eda_data$status = factor(eda_data$status, levels = c('no checking account', '<0 USD', '0 USD >= & <200 USD', '>= 200 USD'))

eda_data$credit_risk = as.factor(eda_data$credit_risk)

# check the transformed data type
str(eda_data)

# explorre data
summary(eda_data)

# visualize EDA for categorical variable 
# determine the response variable  distribution 

credit_risk_disk <- ggplot(eda_data, aes(x=credit_risk))+
  geom_bar(with=0.25, fill='darkgreen')+
  theme_minimal()+
  labs(x='Credit Risk',
       y = 'Count',
       title = "Distribution of Response Variable")+
  theme(plot.title = element_text(size = 17, family = 'Arial', hjust = 0.5),
        plot.subtitle = element_text(size = 12, family = 'Arial', hjust = 0.5),
        plot.background = element_rect(fill="#FBFBFB"))

credit_risk_disk

# visualizing distributions of distributions of individual variable by credit risk using histogram
  df2 = Apex_Trust_Dataset |>
    mutate(credit_risk = as.factor(ifelse(credit_risk == 0, 'bad', 'good')))
  
  # status 
  # Création du graphique 'status'
  status <- ggplot(data = df2, aes(status))+ # Définition des données et de l'esthétique (mapping)
    # Géométrie de l'histogramme
    geom_histogram(breaks = seq(0,5, by=1), # Définit les intervalles de barres de l'histogramme
                   col = "black", # Couleur des contours des barres
                   aes(fill= after_stat(count)))+ # Remplissage des barres en fonction du nombre de données
    
    # Facettage en fonction de 'credit_risk'
    facet_wrap(~credit_risk)+ # Crée des sous-graphiques pour chaque niveau de 'credit_risk'
    
    # Échelle de gradient pour le remplissage
    scale_fill_gradient("Count",  low = "lightgreen", high = "darkgreen")+ # Dégradé de couleur pour le remplissage des barres
    
    # Thème du graphique
    theme_minimal() # Thème minimal pour un aspect propre et simple du graphique
  
  
  # Affichage du graphique
  status

  
  # amount
  amount <- ggplot(data= df2, aes(amount))+
    geom_histogram(s