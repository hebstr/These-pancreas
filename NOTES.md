# outcome

## CJP

Principe de parcimonie à appliquer sur le choix et le nombre de variables à inclure dans l'analyse multivariée.
Avec le CJP binaire sur 100 patients, le modèle de régression est déjà sous-dimentionné : pas assez d'observations, donc pas assez d'évenement par variable, donc le modèle s'ajuste trop aux données et est moins généralisable.
Coefficients poussés vers les extrêmes et IC trop larges.

Une analyse multivariée ne peut porter que sur les observations complètes (= aucune donnée manquante sur aucune des variables incluses dans le modèle).
L'ajout d'une nouvelle variable expose à un risque de perte de puissance.

Proposition : au lieu d'un seuil arbitraire de 12 cures qui met au même niveau un patient ayant eu 1 cure qu'un autre en ayant eu 11, modélisation en comptage du nombre total de cures

Alternative à la binarisation du CJP qui induite une perte d'information

**CJP = nombre total de cures reçues**

Analyse réalisable avec un modèle de régression de Poisson (ou plus exactement quasi-Poisson du fait de la distribution hétérogène du nombre de cures, un détail)

La modélisation en CJP binaire répondait à la question : **quels sont les facteurs associés à la réalisation d'un schéma complet de chimio (nombre total >= 12) ?**

La modélisation en comptage du nombre total de cures répond à la question : **quels sont les facteurs associés à un nombre total de cures plus élevé ?**

## CJS

- **SUPPR 16/07** : inverser l'évènement en "nombre de cures total < 12" oui/non
  > Plus de sens au niveau épidémiologique : aucun effet sur les conclusions mais permet une lecture plus naturelle et conventionnelle des estimations, qui vont dans le sens d'un effet protecteur si < 0 (= moins de risque d'avoir un schéma incomplet) et d'un excès de risque si > 1 (risque augmenté d'avoir un schéma incomplet)

# tbls

## tbl_tumor

- meta : discuter présentation recidive_type + incohérences nb sites meta

## tbl_ttt

- adapt_pct : pct adapt à la dernière cure

## analyse multivariable

choix covariables en attente

ne devrait pas être inclus en facteur d'ajustement :

1) Durée d'hospitalisation :

- inversion temporelle : une partie de l'outcome (avoir < 12 cures) est antérieure à la chirurgie (groupe péri-opératoire)
- possiblement sur le chemin causal : complications > hospit prolongée > moins de cures reçues

2) CA19-9 : trop de données manquantes

### survie

Si biais de temps immortel entre groupe 1 et 2, tester modèle naïf + modèle avec landmark en analyse de sensibilité (seuil clinique)

### Incidence cumulée de récidive

Récidive vs décès sans récidive traité comme risque compétitif, depuis la chirurgie : **skip car trop peu de décès**

Régression sur le risque de récidive avec décès sans récidive traité comme évènement compétitif (modèle de Fine Gray) : **skip car trop peu de décès et manque de puissance**
