# Projet


Pour rendre le projet, vous devez :

  - Cloner ce repository
  - Copier les fichiers de votre projet dans le repository cloné, notamment :
    - le code source (fichier.Rmd)
    - les données
    - le "rapport", c'est-à-dire, le output du Rmd après un knit (fichier html ou md)
    

Vous pouvez re-knit votre fichier Rmd pour vous assurer que tout fonctionne.

Quand tout est complet, vous pouvez commiter et pousser les changements sur votre repository personnel.

Une version par groupe est suffisante. Assurez que les noms des membres du groupes sont clairement indiqués dans le rapport.


---
title: "Projet Final R"
author: "Éloïse Brochu et Marianne Dion"
date: "`r Sys.Date()`"
output: html_document
---

INTRO

À l’aube d’une nouvelle ère technologique introduite par l’arrivée de l’intelligence artificielle, plusieurs analystes ont évalué une hausse des cas de fraudes au Canada. En effet, selon Statistique Canada, le nombre de fraudes aurait presque doublé durant les dix dernières années. En nombre, cela représente plus de 79 000 personnes touchées en 2012 et environ 150 000 en 2022. https://www.canada.ca/fr/bureau-concurrence/nouvelles/2024/03/la-montee-de-lia--la-fraude-a-lere-numerique.html . Avec la hausse alarmante de ses escroqueries, ma coéquipière, Éloise Brochu et moi, Marianne Dion en sommes venus à nous questionner sur comment le gouvernement canadien pourrait réduire le nombre et l’impact de la fraude dans les sphères sociales, économiques et politiques au Canada? Pour répondre à cette question, nous allons utiliser le dataset  Canadian Anti-Fraud Centre Fraud Reporting System Dataset - Canadian Anti-Fraud Centre Reporting Data afin d’en analyser le recensement des cas de fraudes au Canada. De plus, nous avons divisé notre questionnement premier en 5 autres catégories. Premièrement, nous allons analyser le lien entre les tranches d’âge touchées par la fraude et de quel type s’agit-il afin de cerner quel type de fraude affecte en majorité quel groupe d’âge. Deuxièmement, il sera pertinent de voir s’il y a un changement distinctif des types de fraudes canadiennes selon chaque province afin de déterminer si le problème est similaire sur l’ensemble du pays. Troisièmement, examiner quel type de fraude est le plus coûteux pour l’État en opposition au nombre de victimes occasionnées nous permettra de formuler des conseils au gouvernement canadien sur le plan économique. Quatrièmement, afin d’infirmer ou de confirmer le stéréotype lié à l’âge et au sexe des personnes touchées par la fraude, nous allons comparer la fraude par courriel, qui toucherait principalement les femmes âgées de 70 à 79 ans, avec les autres types de fraudes, tranches d’âge et le sexe . Cette grande analyse nous permettra d’avoir une vue globale de la situation canadienne des victimes de fraudes. Finalement, vu l’aspect bilingue du Canada, nous allons analyser l’impact de cet aspect sur l’étendue des fraudes au Québec afin d’étudier si la langue de correspondance peut jouer sur le nombre de victimes. En somme, le but de notre analyse est d’évaluer l’étendue des fraudes au Canada et de formuler des pistes de solutions et conseils au gouvernement canadien. 

DONNÉE

Pour commencer, l’outil le plus important pour ce projet est la base de données. En effet, le centre du travail consistera d’une analyse poussée sur un jeu de donnée englobant le sujet des cas de fraude au Canada. Notre base d’information provient du site du Gouvernement du Canada. Le « dataset » est : « Ensemble de données du système de déclaration de la fraude du Centre antifraude du Canada – Données de déclaration du Centre antifraude du Canada ».  Il faut d’abords préciser que le format des données est un « dataframe », ce qui est simplement un tableau de données ou chaque colonne peut avoir un type d’information différent comme des chiffres, des lettres, des dates, des signes, etc. Le jeu de donnée contient différentes informations et la première chose à remarquer ce sont les variables et les observations. Alors, il y a 300 171 observations (lignes) et 13 variables (colonnes). D’ailleurs, toutes les variables dans la base de données seront utilisées pour répondre à une variété de questions et d’hypothèses. Celles-ci sont; les numéros d’identification, la date de la plainte, le type de plainte, le pays, la province/l’état, les catégories thématiques sur la fraude et la cybercriminalité, les méthodes de sollicitations, le genre des personnes sollicitées, la langue de correspondance, la tranche d’âge des victimes, le type de plainte, le nombre de victimes et les pertes financières. Finalement, la base de données contient tous les cas de fraude au Canada datant de 2021-01-02 jusqu’en 2024-09-29.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r installation des package}
library(readr)
library(tidyverse)
library(tidymodels)
```


```{r, jeu de données}
chemin_fichier <- "C:/Users/ebrochu/OneDrive/Desktop/43c67af5-e598-4a9b-a484-fe1cb5d775b5.csv"

canada_fraud <- read_csv(chemin_fichier)

glimpse(canada_fraud)
```

On se limite au personnes vivantes et on exclue les entreprises.

```{r, organisation des données}
fraudes_canada <- canada_fraud %>%
  select("Numero d'identification / Number ID", "Date Received / Date recue", "Type de plainte recue", "Pays", "Province/Etat", "Categories thematiques sur la fraude et la cybercriminalite", "Methode de sollicitation", "Genre", "Langue de correspondance", "Victim Age Range / Tranche d'age des victimes", "Type de plainte", "Number of Victims / Nombre de victimes", "Dollar Loss /pertes financieres")

glimpse(fraudes_canada)
```
Avant de tout commencer, il faut renommer les variables pour que notre base de données soit plus organisée et compréhensible.

```{r, organisation de donnée }
fraudes_canada<- fraudes_canada %>%
rename("ID" = "Numero d'identification / Number ID","date_recue"="Date Received / Date recue", "systeme_plainte"= "Type de plainte recue","Type_fraude"="Categories thematiques sur la fraude et la cybercriminalite","Methode_sollicitation"= "Methode de sollicitation","Langue_correspondance"="Langue de correspondance", "Type_plainte"= "Type de plainte", "Nombre_victimes"= "Number of Victims / Nombre de victimes", "pertes_financieres"="Dollar Loss /pertes financieres", "Province"="Province/Etat","victim_age_range"="Victim Age Range / Tranche d'age des victimes") 
```

Visualisation 1 - Analyser le lien entre les tranches d’âge touchées par la fraude et de quel type il s'agit.

```{r, code préalable pour visualisation 1}
fraudes_canada  %>%
  filter(Type_fraude != "Inconnu")%>%
  group_by(Type_fraude)%>%
  summarise(n=n())%>%
  arrange((n))%>%
  filter( n>=2000)
```

```{r, visualisation 1.0}
filtrer_data <- fraudes_canada  %>%
  select(Type_fraude, victim_age_range, Nombre_victimes, Genre)%>%
  filter(
    Type_fraude != "Inconnu", 
    victim_age_range !="'Deceased / Décédé",
    victim_age_range !="'Business / Entreprise",
    Nombre_victimes == 1 ,
    Type_fraude %in% c( "Harponnage","Romance","Urgence (Arrestation, Accident, Hospitalisation, Aide urgente)", "Fraude liée à la vente","Emploi","Enquêteur de la banque","Marchandise contrefaite", "Marchandise", "Investissements", "Service", "Renseignements personnels", "Hameçonnage","Extorsion", "Fraude à l'identité"),
    Genre %in% c( "Autre","Homme", "Femme"))
print(filtrer_data)
 
filtrer_data %>%
  mutate(victim_age_range=fct_relevel(victim_age_range,
                                      "'Not Available / non disponible", "'1 - 9", "'10 - 19", "'20 - 29","'30 - 39", "'40 - 49", "'50 - 59", "'60 - 69", "'70 - 79", "'80 - 89", "'90 - 99", "'100 +" )  )%>%
  ggplot(aes(y= victim_age_range, fill = Genre))+
  geom_bar()+
  labs( title= "Lien entre les tranches d’âge touchées par la fraude, leur sexe",
        subtitle= "et le type de fraude", 
        y= "Tranche d'âge des victimes de fraude",
        x= "Nombre de victimes",
        fill= "Genre")+
  facet_wrap(~Type_fraude, scales = "free_x")
 
```

Visualisation 1: 

Qu’elle est le profil type du canadien ou de la canadienne le/la plus touché(e) au Canada?
Le but de la visualisation 1 était de mettre en relation les types de fraudes dont le Canada est touchée avec les différentes tranches d’âges de la société ainsi que leur sexe, afin de les quantifier. Ainsi, il serait possible de faire un portrait global du type de personne le plus touché par cette attaque au Canada. Dans le cadre de cette analyse, nous avons restreint notre étude au cas de fraudes touchant plus de 2000 canadiens ce qui nous laisse avec les 14 fraudes canadiennes les plus communes : L’emploi, l’enquêteur de la banque, l’extorsion, la fraude à l’identité, la fraude liée à la vente, l’hameçonnage, le harponnage, l’investissement, la marchandise, la marchandise contrefaite, les renseignements personnels, la romance, le service et l’urgence (Arrestation, Accident, Hospitalisation, Aide urgente). De plus, nous avons inclus les cas de fraudes où l’âge de la victime n’était pas disponible pour que cela n’affecte pas trop le nombre de victimes total. Afin d’avoir une plus grande précision sur le nombre de victimes pour chaque tranche d’âge, l’axe des x est indépendant pour chaque facette. 
Ainsi, en se fiant à cette graduation, il est possible de le remarqué que la fraude la plus fréquente est la fraude à l’identité. Celle-ci touche environ 60 000 Canadiens et Canadiennes en tout, et ce, dans un rapport un peu plus grand pour l’homme. La tranche d’âge la plus touchée par ce type de fraude est 30-39 ans avec près de 15 000 victimes. Les personnes les moins touchées sont les gens âgés de 90 à 99 ans et ceux qui ne sont pas du tout touchés sont les personnes de plus de 100 ans et plus ou moins de 9 ans. Les personnes de plus de 90 ans ou 9 ans et moins ont moins tendance à être sur les réseaux sociaux ou sur tous types d’appareil électronique, ce qui réduit leurs chances d’être victimes d’une attaque frauduleuse.
Ainsi, nous croyons que ces résultats pourraient être expliqués par l’augmentation du contact avec le numérique. En effet, la tranche 30-39 est une des périodes de la vie où il y a le plus de responsabilités financières. Ainsi, la hausse des paiements numériques augmente le risque de fraude d’identité . De plus, la grande présence sur les réseaux sociaux où elles ne sont pas toujours au courant du nombre d’informations personnelles mis en ligne. De plus, cette période de la vie est souvent associée à une période de stabilité financière, ce qui peut expliquer pourquoi ces femmes sont la cible première des fraudeurs. Finalement, elles peuvent être la cible pour plusieurs concours liés à la maternité ou de promotion sur certains biens, ce qui vient encore une fois renforcer leur position de cible. 
En ce qui concerne la fraude la moins présente au Canada, deux réponses se présentent. Premièrement,  si nous prenons en compte les valeurs qui ne sont pas reliées à une tranche d’âge, la fraude la moins populaire est celle liée à l’urgence. Celle-ci touche environ 2 100 personnes, majoritairement des femmes. Deuxièmement, si nous ne prenons pas en compte les données non reliées à une tranche d’âge, la fraude la moins répandue est la marchandise contrefaite qui touche un peu plus de 2000 personnes dans un rapport assez égalitaire homme/femme.  
Grâce à cette visualisation, nous en sommes venus à la conclusion que le profil du Canadien le plus touché par la fraude est les femmes de 30 à 39 ans. Cette conclusion est due au fait que la tranche d’âge 30-39 se retrouve dans le top 3 des 3 fraudes les plus présentes soit : la fraude d’identité, le service et les renseignements personnels. 


Visualisation 2 - Analyser s’il y a un changement distinctif des types de fraudes canadiennes selon chaque province afin de déterminer si le problème est similaire sur l’ensemble du pays.

```{r, code préalable visualisation 2}
fraudes_canada <- fraudes_canada %>%
mutate(date_recue = year(date_recue)) 
glimpse(fraudes_canada)
 
fraudes_canada %>%
  select(date_recue)%>%
  count(date_recue)
```

```{r, visualisation 2.0}
fraudes_canada %>%
  select(Province,Methode_sollicitation, Nombre_victimes)%>%
  filter(Methode_sollicitation != "Autre/inconnu",
  Province %in% c("Alberta", "Colombie-Britanique", "Île-du-Prince-Édouard", "Manitoba", "Nouveau-Brunswick", "Nouvelle-Écosse", "Ontario", "Québec", "Saskatchewan", "Terre-Neuve-et-Labrador", "Territoire du Nord-Ouest", "Yukon"),
Methode_sollicitation %in% c("Courrier", "Porte-à-porte/en personne", "Messages texte", "Internet", "Internet et réseaux sociaux", "Courriel", "Appel direct", "Appel direct"),
Nombre_victimes==1) %>%
count(Province,Methode_sollicitation, Nombre_victimes)%>%
  summarise(nbr_victimes_province=n,Province,Methode_sollicitation )%>%
  
  ggplot(aes(y=nbr_victimes_province, x=Province))+
  geom_point()+
  labs(
    title= "Nombre de victimes en fonction de la province",
    subtitle= "selon les 7 plus courantes méthodes de sollicitation",
    x= "Province",
    y= "Nombre de victimes"
  )+
  facet_wrap(~Methode_sollicitation, scales = "free_y")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

Visualisation 2:

Quels sont les types de sollicitation les plus courantes dans chaque province et comment varient-elles par rapport aux autres provinces? Le problème est-il similaire sur l’ensemble du pays?
D’abord, pour mieux comprendre la visualisation, il faut expliquer ce qu’on recherche au départ. La première partie de la question vise à identifier les méthodes de sollicitation les plus fréquemment utilisées par les fraudeurs dans chacune des provinces. La seconde partie vise à comparer les types de fraudes les plus répandus dans une province donnée avec ceux des autres provinces. On pourra ainsi déterminer si ces dernières présentent des différences significatives selon la région. La dernière question vise à savoir si les types et leurs fréquences sont les mêmes dans toutes les provinces ou s’il y a des variations régionales.
Maintenant, en regardant le graphique formé à partir d’un code qui contient toutes les informations désirées, on peut voir ceci. Premièrement, les sept méthodes de sollicitation les plus courantes sont : par appel direct, par courriel, par courrier, par Internet, par les réseaux sociaux, par message texte et en personne (porte-à-porte). Lorsqu’on examine le graphique global, il est facile de constater que, dans chaque méthode, les deux provinces qui se distinguent le plus sont l’Ontario et le Québec, suivies de l’Alberta et de la Colombie-Britannique. Voici, classés en fonction du nombre de victimes décroissant, les types de sollicitations les plus courantes : les appels téléphoniques, avec environ 9000 victimes; les sollicitations sur internet et les réseaux sociaux, avec environ 8000 victimes; les courriels, avec environ 4000 victimes; les messages texte, avec environ 2500 victimes; le porte-à-porte, avec environ 1000 victimes; et enfin, les attrapes à la poste, avec seulement 300 victimes. Il faut souligner que toutes les sollicitations comportent un nombre de victimes dans chaque province, sauf celles par courrier, le Yukon ne compte aucune victime.
Pour terminer, il semble que la fraude par appel direct soit la méthode la plus utilisée par les fraudeurs, ce qui est logique puisque cette méthode permet une interaction directe avec la cible, qu’elle peut être personnalisée en fonction des réactions de celle-ci, ainsi que pour bien d’autres raisons. Ensuite, la province qui se distingue par le nombre le plus élevé de victimes pour toutes les méthodes de sollicitation est l’Ontario, probablement en raison d’une forte densité de population. Les autres provinces comptent moins de victimes pour toutes les méthodes. Enfin, le problème de la fraude n’est pas uniforme sur l’ensemble du pays, certaines provinces, en particulier l’Ontario, étant plus touchées que d’autres.


Visualisation 3 - Quel type de fraude est le plus coûteux pour l’État en opposition au nombre de victimes occasionnées (statistique prédictive -> régression)

```{r, préalable pour la visualisation 3}
filtrer_data_pred <- fraudes_canada %>%
  select(Type_fraude, Genre, victim_age_range, pertes_financieres, Nombre_victimes) %>%
  filter(pertes_financieres != "$0.00",
         Type_fraude %in% c( "Harponnage","Romance","Urgence (Arrestation, Accident, Hospitalisation, Aide urgente)", "Fraude liée à la vente","Emploi","Enquêteur de la banque","Marchandise contrefaite", "Marchandise", "Investissements", "Service", "Renseignements personnels", "Hameçonnage","Extorsion", "Fraude à l'identité"), victim_age_range !="'Deceased / Décédé",
    victim_age_range !="'Business / Entreprise", Genre %in% c( "Autre","Homme", "Femme"))%>%
  mutate(pertes_financieres = as.numeric(gsub("[$,]", "", pertes_financieres)),
         Type_fraude=as.factor(Type_fraude), Genre, victim_age_range)%>%
  filter(pertes_financieres<30000)

print(filtrer_data_pred)
```


```{r, régression linéaire}
fraudes_canada_split <- initial_split(filtrer_data_pred, prop = 0.75)
fraudes_canada_train <- training(fraudes_canada_split)
fraudes_canada_test <- testing(fraudes_canada_split)

fraudes_canada_model <- linear_reg() %>%
  set_engine("lm")
fcm_recipe <-
  recipe(pertes_financieres ~ Type_fraude + victim_age_range + Genre, data = fraudes_canada_train) %>%
  step_dummy(all_nominal_predictors())
fcm_workflow <- workflow() %>%
  add_recipe(fcm_recipe) %>%
  add_model(fraudes_canada_model)
fcm_fit <- fcm_workflow %>%
  fit(data = fraudes_canada_train)
```

```{r}
fcm_fit %>%
  tidy()
```

```{r, prédiction}
fcm_pred <- fcm_fit %>%
  predict(new_data = fraudes_canada_test)%>%
  bind_cols(fraudes_canada_test)

fcm_pred%>%
  select(.pred, pertes_financieres)
```
```{r, r carrée}
metrics(fcm_pred, truth = pertes_financieres, estimate = .pred)
```
```{r, essai}
fcm_pred %>%
  ggplot(aes(x = .pred, y = pertes_financieres))+
  geom_point()
```

Visualisation 3:

Quel type de fraude est le plus coûteux pour l’État en opposition au nombre de victimes occasionnées (statistique prédictive)?
Tout d’abord, l’objectif de la troisième visualisation était de mettre en évidence l’élément clé des données du jeu : les différents types de fraudes. La première partie de la question consistait à identifier le type de fraude qui entraînait les pertes financières les plus élevées pour l’État. Autrement dit, il s’agissait de regrouper le total des pertes causées par chaque abus. On voulait aussi inclure dans le même tableau de résultats le ratio entre le nombre de victimes et les pertes financières. Cela nous aurait permis de connaitre le type de fraude la plus couteuse, et si le nombre de personnes touchées jouait un rôle important dans les pertes financières. La partie de la question sur la statistique prédictive visait quant à elle à nous aider à prévoir quels types de fraudes pourraient encore entraîner des pertes importantes à l’avenir de l’État. Malheureusement, lors de la conception des codes pour la prédiction ci-dessus, nous avons constaté qu’il était impossible de réaliser une prévision statistique avec les variables sélectionnées. 
Au lieu de cela, nous avons développé un code pour voir s’il était encore possible de réaliser une prédiction. Nous avons d’abord divisé les données en deux ensembles : un pour le test (25 %) et un pour l’entraînement (75 %). Après, on a créé un modèle de régression linéaire (“lm” linear model). Ensuite, on a créé une recette pour le modèle, en spécifiant que la variable cible est les pertes financières, qu’on souhaite déterminer selon le type de fraude, l’âge de la victime et le genre de la personne. Par la suite, on a converti toutes les variables nominales en variable indicatrice (dummy variables). Finalement, nous avons élaboré un workflow pour notre modèle en intégrant la recette dans ce dernier. Nous avons converti nos données pour obtenir un tableau structuré qui permet de visualiser les résultats.
Le tableau obtenu est décodé comme suit : l’intercept représente l’ordonnée à l'origine, tandis que l’estimate correspond à la valeur de «b» dans l’équation ax+b, qui sert à déterminer la pente. Le « a » est la pente pour chaque type de fraude et les autres variables définies. Dans ce tableau, les prédictions négatives ont une valeur nulle, car une perte négative n’a pas de sens. 
En ce qui concerne la valeur du coefficient de détermination (r²), elle est à nouveau médiocre, avec un résultat de 0,3. Plus la valeur est proche de 1, meilleure elle est. Les valeurs inférieures à 0,5 sont très faibles. Plusieurs facteurs peuvent expliquer la faible valeur obtenue lors de chacun des essais. La première explication possible serait que notre base de données présente de considérables écarts. Par exemple, dans la variable des pertes financières, on trouve des valeurs allant de 0 à 80 millions de dollars. Ce facteur peut beaucoup influencer la prédiction. Nous n’avons pas non plus considéré l’influence des autres variables sur celles qu’on souhaite prédire. En effet, les pertes financières sont très peu expliquées par le genre, le type de fraude et l’âge. En d’autres termes, il faut se demander quelle catégorie de personnes est la plus susceptible d’être victime de fraudes. Cela nous aidera à identifier la variable qui correspondrait le mieux aux pertes financières afin d’obtenir une bonne prédiction statistique. Ici, ce serait la corrélation entre les pertes et l’âge, mais cela n’améliore toujours pas le coefficient de détermination. 
En résumé, tout ce qui ne fonctionne pas peut être attribué à nos données. Les pertes financières sont trop dispersées, la proportion de pertes par rapport à la «non-perte» est trop élevée. La qualité des données n’est pas assez bonne pour effectuer une prévision statistique des pertes d’argent. Nous aurions dû sélectionner une autre variable pour cette représentation.


Visualisation 4 - Comparer la fraude par courriel, qui toucherait principalement les femmes âgées entre 70 et 79 ans, avec les autres types de fraudes, tranches d’âge et le sexe (vue globale de la situation canadienne).

```{r, visualisation 4.0}
library(dplyr)
dates_data <- data.frame(dates=as.Date(canada_fraud$`Date Received / Date recue`))

dates_data <- dates_data %>%
  mutate(year = format(dates, "%Y"),
         month = format(dates, "%b"),
         month = fct_relevel(month,"Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ),
          month_num = match(month, month.abb))

dates_data %>%
 count(month,year)
  
ggplot(dates_data, aes(x= month_num))+
  geom_density(fill="blue",  alpha=0.5)+
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb)+
  labs( title= "Évolution du nombre de cas fraude sur 1 an",
        subtitle= "de 2021 à 2024", 
        y= "Mois",
        x= "Densité")+
  facet_wrap(~ year, nrow = 4)+
   theme_minimal()
```

Visualisation 4:

Quelle est l’évolution du nombre de cas et de tentative de fraude au courant d’une l’année et ce pour les années 2021 à 2024?
En effectuant une première fois cette visualisation. Nous nous sommes rendu compte que le data frame ne contenait pas les données des mois d’octobre, novembre et décembre en 2024. Ainsi, nous allons effectuer 2 analyses. Premièrement, une analyse des tentatives de fraudes de janvier à décembre entre les années 2021, 2022 et 2023, et une deuxième analyse des tentatives de fraudes de janvier à septembre entre les années 2021,2022,2023 et 2024. Notre hypothèse est que le nombre de cas de fraude va augmenter avec les années et que l’arrivée de plateforme d’intelligence artificielle telle que chatgpt en 2022 va marquer les tendances.
Premièrement, avec une vue d’ensemble, les trois années semblent assez similaires. Il y a une certaine différence au mois d’avril 2023 mais c’est le seul changement majeur. En analysant chaque mois, on peut y faire plusieurs observations. Premièrement, au mois de janvier,  la densité de nombre de fraudes est plus grande, environ 0,17 en 2023 mais est similaire entre 2021 et 2022 à aux alentours de 0,12. Deuxièmement, au mois de février, la densité du nombre de fraudes est plus haute en 2021 soir environ 0,14 et environ 0,08 en 2022 et 2023. Troisièmement, en mars, il y a une décroissance de densité . En effet, en 2021 celle-ci était à l’alentour de 0,14, en 2022 environ 0,125 et, en 2022 environ 0,115. Quatrièmement, au mois d’avril , il y a une perte significative de densité en 2023 soit environ 0,06 à comparer de 0,125 en 2022 et 0,11 en 2021. Cinquièmement, au mois de mai, il y a une légère hausse de densité en 2022 soit environ 0,12 et des valeurs aux alentours de 0,1 pour les années 2023 et 2021. Sixièmement, au mois de juin on peut remarquer une certaine constance , les trois valeurs de densité sont pratiquement identiques pour les trois années, soit 2021,2022 et 2023 et tourne aux alentours de 0,12. Septièmement, au mois de juillet, il y a une légère recrudescence en 2023 avec une densité de 0,2, mais les années 2022 et 2021 sont assez stables aux alentours de la même valeur que le mois de juin.  Huitièmement, aux mois d’août, il y a une forte réduction de densité en 2023 soit 0,09, les deux autres années quant à elle restent toujours stables aux alentours de 0,12. Neuvièmement, au mois de septembre, il y a une légère hausse de densité en 2023 (0,1) et une légère baisse en 2022 (0,095) et 2023 (0,10). Dixièmement, en octobre, les années 2023 et 2022 connaissent une baisse de densité passant respectivement de 0,1 à 0,08 et de 0,095 à 0,085. La densité de 2021 reste toujours stable. Onzièmement, pour le mois de novembre, il y a une forte perte de densité en 2023 soit 0,07. L’effet inverse se fait remarquer pour 2021 et 2022 avec des augmentations les amenant respectivement à des valeurs de densité de 0,10 6 et 0,101. Finalement, pour le mois de décembre, la tendance de l’année 2023 se maintient avec une densité d’environ 0,07. Pour l’année 2022 on remarque une légère recrudescence avec une valeur de 0,12 et pour 2021 on remarque une chute de densité, soit 0,08. Avec l’analyse détaillée de chaque mois, il est plus facile de comprendre le portrait global de cette visualisation. Notre hypothèse que le nombre de fraudes allait augmenter avec les années s’avère erronée. En effet, comme le démontre cette analyse, l’année 2023 subit une décroissance assez stable à comparer à l’année 2022 et 2021 qui est plutôt stable. Cependant, nous avons remarqué que les valeurs de densité de l’année 2021 sont un peu plus hautes que celle de l’année 2022, ce qui montre une légère décroissance des nombres de cas de fraude en 2022.

```{r, visualisation 4.1}
library(dplyr)
dates_data <- data.frame(dates=as.Date(canada_fraud$`Date Received / Date recue`, format="%Y-%m-%d"))
 
dates_data <- dates_data %>%
  mutate(year = format(dates, "%Y"),
         month = format(dates, "%b"),
         month = fct_relevel(month,"Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug" ),
          month_num = match(month, month.abb))%>%
  filter(month_num <=9)
 
dates_data %>%
count(month,year)

ggplot(dates_data, aes(x= month_num))+
  geom_density(fill="blue",  alpha=0.5)+
  scale_x_continuous(
    breaks = 1:9,
    labels = month.abb[1:9])+
  labs( title= "Évolution du nombre de cas fraude sur 1 an",
        subtitle= "de 2021 à 2024",
        x= "Densité",
        y= "Mois")+
  facet_wrap(~ year, nrow = 4)+
   theme_minimal()
```

Après avoir effectué une analyse approfondie de ces 3 années, il est possible d’y comparer l’année 2024 du mois de janvier à septembre. Vu qu’il a été établi qu’il y a une diminution des nombres de cas de fraude de l’année 2021 à 2023. Nous allons seulement comparer l’année 2024 avec l’année 2023. Premièrement, au mois de janvier, la densité de l’année 2024 est grandement inférieure à celle de 2023, soit 0,16 < 0,21. Deuxièmement, en février, la densité de l’année 2024 est toujours inférieure à celle de 2023 et atteint une valeur de 0,15 à comparer à 0,16. Troisièmement, en mars, l’année 2024 continue à diminuer en densité (0,103), ce qui est le contraire pour l’année 2023, qui subit une hausse de densité (0,175). Quatrièmement, en avril, il y a une importante hausse de densité en 2024, atteignant presque le 0,15 et une baisse drastique en 2023 frôlant le 0,1. Cinquièmement, en mai, la densité de 2024 continue d’augmenter avec une valeur de 0,155. L’année 2023 va aussi subir une hausse marquante (0,15). Sixièmement, une tendance descendante s’installe en 2024 pour les mois de juin et juillet avec des valeurs de densité de 0,13 et 0,12. Cette tendance est inverse en 2023 puisque la densité ne cesse d’augmenter durant les mois de juin et juillet, 0,15 et 0,17. Septièmement, pour le mois d’aout, il y a une hausse prononcée de la densité en 2024 soit 0,16 ainsi qu’une baisse marquante en 2023 soit 0,14.Pour conclure, au mois de septembre, les deux années ont des valeurs de densité similaire aux alentours de 0,15. Brièvement, la densité de l’année 2024 était globalement sous la densité de l’année 2023 ce qui laisse présager une diminution du nombre de cas de fraude entre ces deux années. 
Avec cette analyse exhaustive, il est possible d’en conclure que le nombre de cas de fraude à une tendance descendante de l’année 2021 à 2024. Ainsi, notre hypothèse concernant la hausse des nombres de cas de fraude est belle et bien erronée. En ce qui concerne notre hypothèse face à la hausse marquée du nombre de cas lors de l’arrivée de chatgpt en novembre 2022, il y a une hausse de densité assez marquante. Toutefois il n’est pas possible de déterminer si chatgpt ou d’autres plateformes d’intelligence artificielle en sont responsables. Cependant, la tendance descendante du nombre de cas de fraude continue même après leur apparition, ce qui laisse présager qu’ils n’ont peut-être pas d’impact sur le nombre de cas de fraudes canadien. 


Visualisation 5 - Analyser l’impact de cet aspect sur l’étendue des fraudes au Québec afin d’étudier si la langue de correspondance peut jouer sur le nombre de victimes.

```{r, visualisation 5.0}
fraudes_québec <- fraudes_canada %>%
  select(Type_fraude, Province, Nombre_victimes, Langue_correspondance) %>%
  filter(Type_fraude != "Inconnu",
         Type_fraude == c("Harponnage", "Romance","Urgence (Arrestation, Accident, Hospitalisation, Aide urgente)", "Fraude liée à la vente","Emploi", "Enquêteur de la banque", "Marchandise contrefaite", "Marchandise", "Investissements", "Service", "Renseignements personnels", "Hameçonnage","Extorsion", "Fraude à l'identité"),
         Province == c("Québec"),
         Nombre_victimes != "0", 
         Langue_correspondance != "non disponible") %>%
  group_by(Type_fraude, Langue_correspondance) %>%
  summarize(victimes_total = sum(Nombre_victimes), Province = "Québec")

print(fraudes_québec)

ggplot(fraudes_québec, 
       aes(x = Langue_correspondance, y=victimes_total))+
  geom_col()+
  labs( title= "L'Étendue des fraudes selon la langue de correspondance et la quantité de victimes",
        subtitle= "Dans la province du Québec", 
        y= "Nombre de victimes total",
        x= "Langue correspondante",)+
  facet_wrap(~ Type_fraude, scales = "free")
```

Visualisation 5: 

Quels sont les impacts du bilinguisme sur l’étendue des fraudes au Québec selon la langue de correspondance et le nombre de victimes occasionnés?
La visualisation 5 vise à mettre en évidence l’impact des diverses formes de fraudes en fonction de la langue utilisée dans les communications. Elle sert également à mesurer l’influence possible de cette variable sur le nombre de victimes. Les paramètres sont limités aux données de la province de Québec. Autrement dit, elle ne prend en compte que les résultats obtenus pour le Québec, où seules deux langues, soit le français et l’anglais, ont été prises en considération puisque ce sont les deux langues parler par la majorité de la population québécoise. 
Avant d’analyser cette visualisation, il est important de comprendre certains éléments du code. Tout d’abord, nous avons créé un nouveau data frame pour ne garder que celles concernant le Québec. De plus, on a choisi les variables à étudier en fonction du thème abordé : le type de fraude, la province visée, la langue utilisée dans les échanges et le nombre de victimes. Les lignes de code servant à filtrer les données importantes sont identiques aux autres visualisations, puisqu’elles répondent aux mêmes conditions. On regroupe ensuite les informations pour ne conserver que le nombre total de victimes, le type de fraude et la langue de correspondance. À partir de ces informations, on peut créer un graphique et procéder à une analyse approfondie.
Maintenant, quand on regarde la compilation du tableau, il y a quelques points importants à souligner. Tout d’abord, ce qui frappe, c’est que la langue utilisée pour communiquer est plus souvent le français. Cette observation est très logique, puisqu'au Québec, la langue principale est le français. Donc, les fraudeurs ont tendance à privilégier cette langue, peut-être parce qu’ils peuvent ainsi toucher un plus grand auditoire. Une autre information qui se distingue est que la fraude à l’identité est la plus répandue au Québec, touchant plus de 1500 francophones. De plus, le type de sollicitation portant sur les informations personnelles affecte plus de 400 francophones. On compte aussi une fraude sur les investissements, qui toucherait environ 80 francophones. Pour ce qui est du reste, on compte moins de 60 victimes. Dans le reste des fraudes, celle sur les urgences affecte seulement les francophones, il n’y a aucune victime anglophone.
Grâce à cette visualisation, nous avons conclu qu’au Québec, la meilleure victime est une personne qui parle français. Cette conclusion est justifiée, puisque les francophones représentent la grande majorité de la population québécoise.


Recommandations : 
À la lueur des résultats de nos diverses analyses, nous en sommes venus à dresser une liste de recommandations, une pour chaque analyse, pour le gouvernement canadien afin d’assurer une meilleure sécurité et de réduire davantage les cas de fraude. 
1)	Ayant déterminé un portrait global du Canadien le plus touché par la fraude, nous croyons que les femmes âgées de 30 à 39 ans devraient avoir accès à plus de ressources et de renseignent sur la fraude afin de réduire le nombre de victimes. Ceci pourrait être fait par des campagnes de sensibilisations ou de l’information envoyée aux personnes concernées. 
2)	Les 4 provinces les plus touchées par le plus de méthodes de sollicitations de fraudes devraient bénéficier d’une aide supplémentaire, soit financière ou en informations propre aux méthodes de sollicitation, afin d’aider ces provinces à diminuer le nombre de victimes. 
3)	Vu la tendance descendante, nous conseillons de garder les mesures déjà prises pas le gouvernent, car elles ont eu de bon impact dans les 3 dernières années, et croyons que l’ajout des autres recommandations aidera à réduire davantage le nombre de victimes. 
4)	Au Québec, la meilleure victime est un francophone, ce qui est logique parce que la langue la plus courante est le français. Alors, la recommandation serait d’avantage sensibiliser les francophones aux différentes fraudes.
Conclusion : 
Avec les analyses effectuées lors de ce travail, nous en somme venu à la conclusion que le nombre de cas de fraude diminue plus les années augmente, et ce, de 2021 à 2024. De plus, nous avons réussi à brosser un portrait global de la personne typiquement visée par le plus de types de fraude. Ce sont les femmes âgées de 30 à 39 ans, et ce dû à leur situation. Aussi, nous avons établi que certaines provinces canadiennes étaient plus touchées par certains types de sollicitation que d’autres, notamment le Québec, l’Ontario, l’Alberta et la Colombie britannique. Par la suite,  vu le caractère bilingue de notre pays, nous voulions voir si l’aspect linguistique aurait un impact sur le nombre de fraudes au Québec. Ceci s’est avéré peu deux types de fraude, soit la fraude d’identité et la fraude de renseignement personnel. C’est deux types de fraudes ont fait plus de victimes en français quand anglais. Finalement, nous avons fait une prédiction pour voir si la tendance des dernières années allait se maintenir et avons déterminé qu’il n’est pas possible de faire une bonne prédiction avec les variables choisies. 

Cependant, malgré la baisse des attaques frauduleuse depuis l’arrivée de l’intelligence artificielle, nous croyons tout de même que celle-ci pourrait jouer un rôle beaucoup plus important que l’on peut l’imaginer. En effet, notre pensée se divise en deux aspects. Premièrement, l’IA pourrait aider les fraudeurs à développer davantage leurs techniques, les faisant ainsi passer sous le radar. Ainsi, en apparence, les taux de fraudes au Canada sont descendants, mais, en réalité, ce n’est que nous techniques de détection qui ne sont plus à jours. Deuxièmement, l’intelligence artificielle pourrait aider les programmeurs et les responsables de cybersécurités à développer de nouvelles technologies pour rendre les réseaux plus sécuritaires, entrainant ainsi une baisse du taux de fraudes canadien. Il serait pertinent d’analyser ces deux autres pistes dans une prochaine étude afin d’approfondir nos recherches sur le sujet. 


SOURCE:
Scales= free x : https://ggplot2.tidyverse.org/reference/facet_grid.html
(262588213843476 s. d.)
Canada$ date received: https://larmarange.github.io/analyse-R/fusion-de-tables.html
(Anon s. d.-a)
Year= format %Y: https://gist.github.com/zross/7fbbc034459aeff36627
Match: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/match
(Anon s. d.-b)
Scales_x_continuous: https://ggplot2.tidyverse.org/reference/scale_continuous.html

(Anon s. d.-c)
(Anon s. d.-d)

BIBLIOGRAPHIE des sources utilisées non apprises dans le cours 
Modification des dates ( year = format…) : 
262588213843476. s. d. « Formatting Dates in R Reference ». Gist. Consulté 9 décembre 2024 (https://gist.github.com/zross/7fbbc034459aeff36627).
Anon. s. d.-a. « Fusion de tables ». Consulté 9 décembre 2024 (https://larmarange.github.io/analyse-R/fusion-de-tables.html).
Joindre 2 colonnes avec $: 
Anon. s. d.-b. « Lay out Panels in a Grid — Facet_grid ». Consulté 9 décembre 2024 (https://ggplot2.tidyverse.org/reference/facet_grid.html).
Fonction match
Anon. s. d.-c. « match function - RDocumentation ». Consulté 9 décembre 2024 (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/match).
Rendre l'axe des x ou y indépendante: 
Anon. s. d.-d. « Position Scales for Continuous Data (x & y) — Scale_continuous ». Consulté 9 décembre 2024 (https://ggplot2.tidyverse.org/reference/scale_continuous.html).