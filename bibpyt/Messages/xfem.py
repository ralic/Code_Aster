#@ MODIF xfem Messages  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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
 Le nombre de fissures autorisées avec X-FEM est limité à %(i1)d
"""),

3: _("""
 Le nombre de fissures X-FEM est négatif ou nul. Contactez les développeurs.
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
