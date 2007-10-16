#@ MODIF xfem Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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
  -> Les fissures X-FEM sont surement trop proches.
     Il faut au minimum 2 mailles entre les fissures.
  -> Risque & Conseil:
     Veuillez raffiner le maillage entre les fissures
     (ou écarter les fissures). 
"""),

2: _("""
  -> Le nombre de fissures autorisées avec X-FEM est limité à (i1)i
  -> Risque & Conseil: 
     Il faut réduire le nombre de fissures introduites.
"""),

3: _("""
  -> Le modèle %(k1)s est incompatible avec la méthode X-FEM.
  -> Risque & Conseil:
     Vérifier qu'il a bien été créé par l'opérateur MODI_MODELE_XFEM. 
"""),

4: _("""
  -> Il est interdit de mélanger dans un modèle les fissures X-FEM 
     avec et sans contact. 
  -> Risque & Conseil:
     Veuillez rajouter les mots clés CONTACT manquants 
     dans DEFI_FISS_XFEM.
"""),

6: _("""
     DDL_IMPO sur un noeud X-FEM : %(k1)s =  %(r1)f au noeud %(k2)s
"""),

7: _("""
     Il y a %(i1)s mailles %(k1)s 
"""),

8: _("""
     Le nombre de %(k1)s X-FEM est limité à 10E6. 
     Risque & Conseil:
     Veuillez réduire la taille du maillage.
"""),

11: _("""
  -> On a trouvé plus de 2 points de fond de fissure, ce qui est impossible en 2d.
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

14: _("""
     On ne peut pas appliquer un cisaillement 2d sur les lèvres d'une fissure X-FEM.
"""),

15: _("""
  -> Cette option n'a pas encore été programmée.
  -> Risque & Conseil:
     Veuillez utiliser un autre chargement (en pression) ou contacter votre
     correspondant.
"""),

16: _("""
  -> Il n'y a aucun élément enrichi.
  -> Risque & Conseil:
     - Si le contact est défini sur les lèvres de la fissure, la modélisation
       doit etre 3D_XFEM_CONT ou C_PLAN_XFEM_CONT ou D_PLAN_XFEM_CONT.
     - Si le contact n'est pas défini sur les lèvres de la fissure,
       la modélisation doit etre 3D ou C_PLAN ou D_PLAN'.
"""),

18: _("""
     Dimension de l'espace incorrecte. 
     Le modèle doit etre 2D ou 3D et ne pas comporter de sous-structures.
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
  -> Erreur dans le choix de la méthode de calcul des level-sets
  -> Risque & Conseil :
     Veuillez renseignez FONC_LT/LN ou GROUP_MA_FISS/FOND.
"""),

25: _("""
     Le frottement n'est pas pris en compte pour l'approche 
     <<Grands glissements avec XFEM>>.
"""),

26: _("""
     L'approche <<Grands glissements avec XFEM>> fonctionne seulement pour le cas 2D.
"""),

27: _("""
     Seulement les mailles QUAD4 sont prises en compte par l'approche
     <<Grands glissements avec XFEM>>.
"""),

57: _("""
  -> Aucune maille de fissure n'a été trouvée. 
  -> Risque & Conseil :
     Suite des calculs risquée.
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
     Ne pas utiliser le mot-clef RAYON_ENRI lorsque le fond de fissure
     est en dehors de la structure.
"""),

60: _("""
  -> Le point initial de fissure n'est pas un point de bord de fissure,
     bien que la fissure soit débouchante
  -> Risque & Conseil:
     Assurez-vous de la bonne définition de PFON_INI.
"""),


}
