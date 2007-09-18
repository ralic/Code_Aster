#@ MODIF xfem Messages  DATE 18/09/2007   AUTEUR PELLET J.PELLET 
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


def _(x) : return x

cata_msg={

1: _("""
Pour le DVP : écrasement des valeurs nodales dans xconno.f
Pour l'utilisateur : les fissures X-FEM sont surement trop proches.
                     il faut au minimum 2 mailles entre les fissures.
                     veuillez raffiner le maillage entre les fissures (ou écarter les fissures). 
"""),

2: _("""
 Le nombre de fissures autorisées avec X-FEM est limité à (i1)i
"""),

3: _("""
 Le modele %(k1)s est incompatible avec la methode X-FEM.
 Vérifier qu'il a bien été créé par l'opérateur MODI_MODELE_XFEM. 
"""),

4: _("""
 Il est interdit de melanger dans un modèle les fissures X-FEM avec et sans
 contact. Veuillez rajouter les mots clé CONTACT manquants 
 dans DEFI_FISS_XFEM.
"""),

5: _("""
La valeur du parametre %(k1)s (%(i1)d) de la fissure %(k2)s 
a été changé à 
%(i2)d (valeur maximale de toutes les fissures du modèle)
"""),

6: _("""
DDL_IMPO sur un noeud X-FEM : %(k1)s =  %(r1)f au noeud %(k2)s
"""),

7: _("""
Il y a %(i1)s mailles %(k1)s 
"""),

8: _("""
Le nombre de %(k1)s X-FEM est limité à 10E6. Veuillez reduire la taille du maillage.
"""),

9: _("""
erreur de dvt dans %(k2)s : on a trouvé trop de nouveaux %(k1)s à ajouter.
"""),

10: _("""
On ne peut pas post-traiter de champs aux points de Gauss avec X-FEM sur des éléments
dont le nombre de points de Gauss est différent de 1.
"""),

11: _("""
On a trouvé plus de 2 points de fond de fissure, ce qui est impossible en 2d.
Veuillez revoir la définition des level sets.
"""),

12: _("""
La prise en compte du contact sur les lèvres des fissures X-FEM n'est possible que
sur un maillage linéaire.
Deux solutions : 
- soit passer en approximation linéaire
- soit ne pas prendre en compte le contact (enlever le mot-clé CONTACT de MODI_MODELE_XFEM)
"""),

13: _("""
pb fissure elliptique
"""),

14: _("""
On ne peut pas appliquer un cisaillement 2d sur les lèvres d'une fissure X-FEM.'
"""),

15: _("""
Cette option n'a pas encore été programmée.'
"""),

16: _("""
Il n'y a aucun élément enrichi.
- Si le contact est défini sur les lèvres de la fissure,
la modélisation doit etre 3D_XFEM_CONT ou C_PLAN_XFEM_CONT ou D_PLAN_XFEM_CONT'
- Si le contact n'est pas défini sur les lèvres de la fissure,
la modélisation doit etre 3D ou C_PLAN ou D_PLAN'.
"""),

18: _("""
Dimension de l'espace incorrecte. Le modèle doit etre 2D ou 3D et ne pas comporter
de sous-structures.
"""),

19: _("""
Caractéristique de la SD inconnue. Contactez les développeurs.
"""),

20: _("""
Le mot-clef ORIE_FOND est indispensable en 3D.
"""),

21: _("""
Le mot-clef ORIE_FOND n'est pas nécessaire en 2D.
"""),

22: _("""
Plus d'une occurrence du mot-clef ORIE_FOND.
"""),

23: _("""
Erreur dans le choix de la methode de calcul des level-sets: renseignez FONC_LT/LN ou GROUP_MA_FISS/FOND.
"""),

24: _("""
Erreur de développement : Attribut XFEM non renseigné pour cet élément.
"""),

50: _("""
Le nombre d'aretes coupees par la fissure est superieur au critere de dimensionnement initialement prevu. Contactez les développeurs.
Note DVP: Il faut augmenter le parametre mxar dans la routine xlagsp.
"""),

57: _("""
Aucune maille de fissure n'a été trouvée. Suite des calculs risquée.
"""),

58: _("""
  -> Aucun point du fond de fissure n'a été trouvé !
  -> Risque & Conseil :
     Ce message est normal si vous souhaitiez définir une interface (et non une fissure).
     Si vous souhaitiez définir une fissure, la définition des level sets (Méthode XFEM)
     ne permet pas de trouver de points du fond de fissure à l'intèrieur de la structure.
     Il doit y avoir une erreur lors de la définition de la level set tangente.
     Vérifier la définition des level sets.
"""),

59: _("""
Ne pas utiliser le mot-clef RAYON_ENRI lorsque le fond de fissure est en dehors de la structure.
"""),

60: _("""
Le point initial de fissure n'est pas un point de bord de fissure, bien que la fissure soit débouchante. assurez-vous de la bonne définition de PFON_INI.
"""),


}
