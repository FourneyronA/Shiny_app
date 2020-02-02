# Partage de l'ensemble des applications shiny réalisées

## 1 - Analyse Spatio-Temporelle 

Application très simples permettant à partir d'un jeu de donnée de visualiser : 
* Les fréquence d'apparition à travers différentes temporalités (Mensuelle, hebdomadaire, journalière) et à travers leurs types. Pour celà nous utilisons un représentation de bar graph pour indiquer le nombre d'événements pour chaque catégorie
* Les concentrations spatiales par temporalité pour permettre d'avoir un autre regard sur le comportement des données. Pour celà nous visualisons une cartographie, découper par un maillage (d'une distance réglable), contenant le nombre d'événements par maille. 
* L'organisation spatiale générale du jeu de donnée, en visualisant les différentes lieux proposant des événements. (avec en détails le nombre d'événement réalisé) 

Le jeu de donnée utilisé viens de OpenAgenda, les champs nécéssaire aux scripts sont : position lat/long, catégories, date, nom du lieu
