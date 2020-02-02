# Partage de l'ensemble des applications shiny réalisées

## 1 - Analyse Spatio-Temporelle 

Application très simple permettant à partir d'un jeu de donnée de visualiser :
* Les fréquences d'apparition à travers différentes temporalités (mensuelle, hebdomadaire, journalière) et à travers leurs types. Pour celà nous utilisons une représentation de bar graph pour indiquer le nombre d'événements pour chaque catégorie
* Les concentrations spatiales par temporalité pour permettre d'avoir un autre regard sur le comportement des données. Pour celà nous visualisons une cartographie, découper par un maillage (d'une distance réglable), contenant le nombre d'événements par maille.
* L'organisation spatiale générale du jeu de donnée, en visualisant les différents lieux proposant des événements. (avec en détails le nombre d'événements réalisé)

Le jeu de donnée utilisé vient d'OpenAgenda, les champs nécessaires aux scripts sont : position lat/long, catégories, date, nom du lieu
