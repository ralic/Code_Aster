#@ MODIF mecanonline5 Messages  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-

def _(x) : return x

cata_msg = {

1  : _("""
Avec un schéma de type explicite, seule la prédiction TANGENTE est possible
"""),

3 : _("""
 Il n'est pas possible actuellement de calculer des modes de flambement
 (CRIT_FLAM) ou des modes vibratoires (MODE_VIBR) si on utilise la
 méthode continue du contact ou XFEM avec du contact.
"""),


4 : _("""
 Le contact avec méthode continue ou XFEM avec du contact nécessite de réactualiser la matrice tangente
 à chaque itération (REAC_ITER = 1).
"""),

7 : _("""
 Etant donné la présence du mot clé AMOR_ALPHA et / ou AMOR_BETA, 
 on va assembler la matrice d'amortissement globale de Rayleigh, 
 même si ces coefficients sont tous les deux nuls.
 Cette opération engendre un surcoût de calcul.
"""),

9 : _("""
 Pour avoir BETA nul (schéma purement explicite) avec un schéma de Newmark (standard ou HHT),
utilisez DIFF_CENT ou TCHAMWA.
"""),

10 : _("""
 Pour un schéma purement explicite (DIFF_CENT ou TCHAMWA), seule la formulation
en accélération est possible
"""),

11 : _("""
 Pour un schéma de type NEWMARK, seule les formulations en accélération et en déplacement sont possibles
"""),

12 : _("""
 Pour un schéma de type THETA, seule les formulations en vitesse et en déplacement sont possibles
"""),


19 : _("""
Il y plus d'amortissements modaux (AMOR_MODAL) que de modes.
"""),

20 : _("""
On ne trouve pas le champ de déplacement pour Dirichlet différentiel dans le concept <%(k1)s>.
Votre valeur de NUME_DIDI doit etre incorrecte ou le concept n'est pas le bon.

"""),

21 : _("""
  -> Critère de convergence est lache !
  -> Risque & Conseil : La valeur de RESI_GLOB_RELA est supérieure à 10-4.
     Cela peut nuire à la qualité de la solution. Vous ne vérifiez pas l'équilibre de 
     manière rigoureuse.
"""),

22 : _("""
Schéma en dynamique explicite.
Le contact n'est pas possible.
"""),

23 : _("""
Schéma en dynamique explicite.
LIAISON_UNILATER n'est pas possible.
"""),

24 : _("""
Schéma en dynamique explicite.
Les poutres en grandes rotations POU_D_T_GD et POU_D_TGM ne sont utilisables
qu'en faibles rotations.
"""),

25 : _("""
Dynamique non-linéaire
Le pilotage n'est pas possible.
"""),

26 : _("""
Dynamique non-linéaire
La recherche linéaire n'est pas possible.
"""),

27 : _("""
Dynamique non-linéaire avec schéma THETA
Les poutres en grandes rotations POU_D_T_GD et POU_D_TGM sont interdits.
"""),

28 : _("""
Dynamique non-linéaire
La méthode XFEM n'est pas possible.
"""),

44 : _("""
Pour la prédiction de type 'DEPL_CALCULE', il faut obligatoirement:
 - ITER_GLOB_MAXI = 0
 - ARRET = 'NON'
"""),

45 : _("""
Il faut préciser un concept EVOL_NOLI en prédiction de type 'DEPL_CALCULE'
"""),

46 : _("""
  -> La définition des paramètres RHO_MIN et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MIN à RHO_EXCL.
  -> Risque & Conseil :
     RHO_MIN ne doit pas etre compris entre -RHO_EXCL et RHO_EXCL

"""),

47 : _("""
  -> La définition des paramètres RHO_MAX et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MAX à -RHO_EXCL.
  -> Risque & Conseil :
     RHO_MAX ne doit pas etre compris entre -RHO_EXCL et RHO_EXCL

"""),

}
