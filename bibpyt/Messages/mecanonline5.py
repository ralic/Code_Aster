#@ MODIF mecanonline5 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE DELMAS J.DELMAS

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
 Vous utilisez une méthode de contact (continue ou XFEM) qui nécessite de réactualiser la matrice tangente
 à chaque itération. La réactualisation est donc forcée (REAC_ITER = 1) et ce même si vous utilisez la matrice
 'ELASTIQUE'.

  -> Risque & Conseil :
   - Vous pouvez supprimer cette alarme dans le cas où vous utilisez une matrice 'TANGENTE', pour cela
     renseignez REAC_ITER=1 sous le mot-clé facteur NEWTON.
 
"""),

5 : _("""
 Vous utilisez une méthode de contact (contact discret avec pénalisation ou élément DIS_CHOC)
  qui apporte une contribution à la matrice tangente à chaque itération. La réactualisation est donc forcée (REAC_ITER=1) et ce même si vous utilisez la matrice
 'ELASTIQUE'.

  -> Risque & Conseil :
   - Vous pouvez supprimer cette alarme dans le cas où vous utilisez une matrice 'TANGENTE', pour cela
     renseignez REAC_ITER=1 sous le mot-clé facteur NEWTON.
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
Il y a plus d'amortissements modaux (AMOR_MODAL) que de modes.
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


29 : _("""
Vous faites de la projection modale PROJ_MODAL en explicite.
Il y a %(i1)d  modes dans la structure MODE_MECA.
Le nombre de modes (mot-clef NB_MODE dans PROJ_MODAL) vaut %(i2)d.
On prend donc %(i3)d modes.
"""),

30 : _("""
Vous faites de l'amortissement modal (réduit ou non).
Il y a %(i1)d  modes dans la structure MODE_MECA.
Le nombre de modes (mot-clef NB_MODE dans AMOR_MODAL) vaut %(i2)d.
On prend donc %(i3)d modes.
"""),

31 : _("""
Vous faites de la projection modale PROJ_MODAL en explicite en reprise.
Il n'y a pas de modes stockés lors du calcul précédent.
On part donc de depl/vite/acce généralisés nuls.
"""),


32 : _("""
La SD evol_noli utilisée dans REST_COND_TRAN ne contient pas les 
champs généralisés.
Verifiez qu'il s'agit du meme concept que celui utilisé dans le DYNA_NON_LINE, 
option PROJ_MODAL et que l'archivage a été fait (mot-clef ARCHIVAGE de DYNA_NON_LINE)

"""),

33 : _("""
Dynamique non-linéaire
La méthode IMPL_EX n'est pas possible.
"""),

34 : _("""
La recherche linéaire est incompatible avec le pilotage de type DDL_IMPO.
"""),

43 : _("""
  -> Les paramètres RHO_MIN et RHO_MAX sont identiques.
"""),
44 : _("""
  -> La définition des paramètres RHO_MIN et RHO_MAX est contradictoire.
     On choisit de prendre RHO_MIN plus petit que RHO_MAX.
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



50 : _("""
 Pilotage.
 La composante <%(k1)s> n'a pas été trouvée dans la numérotation.
 Vérifier NOM_CMP dans le mot-clef PILOTAGE.
"""),
51: _("""
        Probleme programmation, dans rescmp : NBCMPU > nbddlmax  
"""), 
52: _("""
        Probleme programmation, dans rescmp : REEL DEMANDE 
"""),  
53: _("""
        Critere RESI_COMP_RELA interdit en dynamique. Utiliser un autre critere de convergence 
"""),  

54 : _("""
  Vous voulez utiliser l'indicateur de convergence RESI_REFE_RELA et une modélisation 
  GRAD_VARI mais vous n'avez pas renseigné le mot-clé %(k1)s .  
"""),


}
