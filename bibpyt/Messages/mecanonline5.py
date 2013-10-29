# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1  : _(u"""
Avec un schéma de type explicite, seule la prédiction TANGENTE est possible
"""),

2 : _(u"""
A cause des erreurs précédentes, le code s'arrête.
  Vous voulez "poursuivre" un calcul non-linéaire (STAT_NON_LINE ou DYNA_NON_LINE).
  Pour cela, vous précisez un état initial (mot clé ETAT_INIT).
  Pour le calcul du premier pas de temps, le champ des variables internes du début du pas est pris
  dans le concept EVOL_NOLI fourni ou par le champ VARI_ELGA.
  On l'a comparé avec le champ des variables internes créé par le mot-clef COMPORTEMENT, il y a incohérence.
  Vérifiez la cohérence entre le comportement et le champ des variables internes donné dans l'état initial.
"""),


3 : _(u"""
 Il n'est pas possible actuellement de calculer des modes de flambement
 (CRIT_FLAMB) ou des modes vibratoires (MODE_VIBR) si on utilise la
 méthode continue du contact ou XFEM avec du contact.
"""),


4 : _(u"""
 Vous utilisez une méthode de contact (continue ou XFEM) qui nécessite de réactualiser la matrice tangente
 à chaque itération. La réactualisation est donc forcée (REAC_ITER = 1).

  -> Risque & Conseil :
   - Vous pouvez supprimer cette alarme si vous
     renseignez REAC_ITER=1 sous le mot-clé facteur NEWTON.

"""),

5 : _(u"""
 Vous utilisez une méthode de contact (contact discret avec pénalisation ou élément DIS_CHOC)
  qui apporte une contribution à la matrice tangente à chaque itération. La réactualisation est donc forcée (REAC_ITER=1) et ce même si vous utilisez la matrice
 'ELASTIQUE'.

  -> Risque & Conseil :
   - Vous pouvez supprimer cette alarme dans le cas où vous utilisez une matrice 'TANGENTE', pour cela
     renseignez REAC_ITER=1 sous le mot-clé facteur NEWTON.
"""),

6 : _(u"""
 Le calcul des énergies n'est pas disponible avec le mot-clé PROJ_MODAL.
"""),

7 : _(u"""
 Étant donné la présence du mot clé AMOR_ALPHA et / ou AMOR_BETA,
 on va assembler la matrice d'amortissement globale de Rayleigh,
 même si ces coefficients sont tous les deux nuls.
 Cette opération engendre un surcoût de calcul.
"""),

8 : _(u"""
 Le calcul des énergies n'est pas disponible lorsque MATR_DISTRIBUEE='OUI'.
"""),

9 : _(u"""
 Pour avoir BETA nul (schéma purement explicite) avec un schéma de Newmark (standard ou HHT),
utilisez DIFF_CENT ou TCHAMWA.
"""),

10 : _(u"""
 Pour un schéma purement explicite (DIFF_CENT ou TCHAMWA), seule la formulation
en accélération est possible
"""),

11 : _(u"""
 Pour un schéma de type NEWMARK, seules les formulations en accélération et en déplacement sont possibles
"""),

12 : _(u"""
 Pour un schéma de type THETA, seules les formulations en vitesse et en déplacement sont possibles
"""),

13 : _(u"""
 La matrice de masse diagonale (option MASS_DIAG) n'est pas utilisable avec un schéma implicite.
"""),

14 : _(u"""
 Il n'est pas possible actuellement de calculer des modes de flambement
 (CRIT_FLAMB) ou des modes vibratoires (MODE_VIBR) si on utilise la
 méthode discrète du contact avec frottement ou la méthode pénalisée.
"""),

19 : _(u"""
Il y a plus d'amortissements modaux (AMOR_MODAL) que de modes.
"""),

20 : _(u"""
On ne trouve pas le champ de déplacement pour Dirichlet différentiel dans le concept <%(k1)s>.
Votre valeur de NUME_DIDI doit être incorrecte ou le concept n'est pas le bon.

"""),

21 : _(u"""
  -> Critère de convergence est lâche !
  -> Risque & Conseil : La valeur de RESI_GLOB_RELA est supérieure à 10-4.
     Cela peut nuire à la qualité de la solution. Vous ne vérifiez pas l'équilibre de
     manière rigoureuse.
"""),

22 : _(u"""
Schéma en dynamique explicite.
Le contact n'est pas possible.
"""),

23 : _(u"""
Schéma en dynamique explicite.
Les liaisons unilatérales ne sont pas possibles.
"""),

24 : _(u"""
Schéma en dynamique explicite.
Les poutres en grandes rotations POU_D_T_GD et POU_D_TGM ne sont utilisables
qu'en faibles rotations.
"""),

25 : _(u"""
Dynamique non-linéaire
Le pilotage n'est pas possible.
"""),

27 : _(u"""
Dynamique non-linéaire avec schéma THETA
Les poutres en grandes rotations POU_D_T_GD et POU_D_TGM sont interdits.
"""),

28 : _(u"""
Dynamique non-linéaire
La méthode XFEM n'est pas possible.
"""),


29 : _(u"""
Vous faites de la projection modale PROJ_MODAL en explicite.
Il y a %(i1)d  modes dans la structure MODE_MECA.
Le nombre de modes (mot-clef NB_MODE dans PROJ_MODAL) vaut %(i2)d.
On prend donc %(i3)d modes.
"""),

30 : _(u"""
Vous faites de l'amortissement modal (réduit ou non).
Il y a %(i1)d  modes dans la structure MODE_MECA.
Le nombre de modes (mot-clef NB_MODE dans AMOR_MODAL) vaut %(i2)d.
On prend donc %(i3)d modes.
"""),

31 : _(u"""
Vous faites de la projection modale PROJ_MODAL en explicite en reprise.
Il n'y a pas de modes stockés lors du calcul précédent.
On part donc de DEPL/VITE/ACCE généralisés nuls.
"""),


32 : _(u"""
La SD EVOL_NOLI utilisée dans REST_COND_TRAN ne contient pas les
champs généralisés.
Vérifiez qu'il s'agit du même concept que celui utilisé dans le DYNA_NON_LINE,
option PROJ_MODAL et que l'archivage a été fait (mot-clef ARCHIVAGE de DYNA_NON_LINE)

"""),

33 : _(u"""
Dynamique non-linéaire
La méthode IMPLEX n'est pas possible.
"""),

34 : _(u"""
La recherche linéaire est incompatible avec le pilotage de type DDL_IMPO.
"""),

35 : _(u"""
La recherche linéaire de type PILOTAGE nécessite de faire du pilotage (présence du mot-clef facteur PILOTAGE).
"""),

36 : _(u"""
La prédiction de type EXTRAPOL ou DEPL_CALCULE est incompatible avec le pilotage.
"""),

37 : _(u"""
L'usage de ARRET='NON' dans CONVERGENCE est dangereux et doit être utilisé avec précaution car il permet à un calcul de converger
même lorsque l'équilibre n'est pas vérifié.
"""),

43 : _(u"""
  -> Les paramètres RHO_MIN et RHO_MAX sont identiques.
"""),
44 : _(u"""
  -> La définition des paramètres RHO_MIN et RHO_MAX est contradictoire.
     On choisit de prendre RHO_MIN plus petit que RHO_MAX.
"""),


45 : _(u"""
Il faut préciser un concept EVOL_NOLI en prédiction de type 'DEPL_CALCULE'
"""),

46 : _(u"""
  -> La définition des paramètres RHO_MIN et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MIN à RHO_EXCL.
  -> Risque & Conseil :
     RHO_MIN ne doit pas être compris entre -RHO_EXCL et RHO_EXCL

"""),

47 : _(u"""
  -> La définition des paramètres RHO_MAX et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MAX à -RHO_EXCL.
  -> Risque & Conseil :
     RHO_MAX ne doit pas être compris entre -RHO_EXCL et RHO_EXCL

"""),

48 : _(u"""
  Le pilotage est incompatible avec la méthode NEWTON_KRYLOV.
"""),


50 : _(u"""
 Pilotage.
 La composante <%(k1)s> n'a pas été trouvée dans la numérotation.
 Vérifier NOM_CMP dans le mot-clef PILOTAGE.
"""),

51 : _(u"""
 Pour utiliser METHODE='NEWTON_KRYLOV', il faut utiliser une méthode itérative (GCPC, PETSC) sous le mot-clé SOLVEUR.
"""),

53: _(u"""
   Le critère RESI_COMP_RELA est interdit en dynamique. Utilisez un autre critère de convergence
"""),


55 : _(u"""
  Vous utilisez l'indicateur de convergence RESI_REFE_RELA et une modélisation %(k1)s.
  Vous devez renseigner la valeur de référence %(k2)s dans CONVERGENCE.
"""),

56 : _(u"""
  Il n'est pas possible actuellement de calculer des modes vibratoires (MODE_VIBR)
  sur un modèle dont au moins une matrice assemblée (masse ou raideur) est non-symétrique.
"""),
}
