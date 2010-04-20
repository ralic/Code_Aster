#@ MODIF contact Messages  DATE 20/04/2010   AUTEUR DESOZA T.DESOZA 
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

cata_msg = {

1 : _("""
Contact méthodes discrètes.
 -> Les méthodes de contact discrètes supposent la symétrie de la matrice obtenue après assemblage.
    Si votre modélisation produit une matrice non-symétrique, on force donc sa symétrie pour résoudre
    le contact.
 -> Risque & Conseil :
    Ce changement peut conduire à des difficultés de convergence dans le processus de Newton mais en
    aucun cas il ne produit des résultats faux.
    
    Si la matrice de rigidité de votre structure est symétrique, vous pouvez ignorer ce qui précède.
    Enfin, il est possible de supprimer l'affichage de cette alarme en renseignant SYME='OUI'
    sous le mot-clé facteur SOLVEUR.
"""),

2 : _("""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le GCP.
Vous pouvez essayer d'augmenter ITER_GCP_MAXI.
La liste des noeuds présentant une interpénétration est donnée ci-dessous.
"""),

3 : _("""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le préconditionneur.
Vous pouvez essayer d'augmenter ITER_PRE_MAXI
"""),

4 : _("""
Contact methode GCP. Le paramètre RESI_ABSO doit etre obligatoirement renseigné.
"""),

7 : _("""
Contact methode GCP. Le pas d'avancement est negatif ; risque de comportement hasardeux de l'algorithme
"""),

9 : _("""
Contact liaison glissiere. Des noeuds se décollent plus que la valeur d'ALARME_JEU:
"""),

13 : _("""
La normale que vous avez prédéfinie (VECT_* = 'FIXE') sur le noeud %(k1)s est colinéaire à la tangente à la maille.
"""),

14 : _("""
La normale que vous avez prédéfinie (VECT_* = 'FIXE') sur la maille %(k1)s est colinéaire à la tangente à la maille.
"""),

15 : _("""
Le vecteur MAIT_FIXE ou ESCL_FIXE est nul !
"""),

16 : _("""
Le vecteur MAIT_VECT_Y ou ESCL_VECT_Y est nul !
"""),

60 : _("""
La maille %(k1)s est de type 'SEG' (poutres) en 3D sans donner la normale pour l'appariement.
Vous devez utilisez l'option NORMALE:
- FIXE: qui décrit une normale constante pour la poutre
- ou VECT_Y: qui décrit une normale par construction d'un repère basé sur la tangente (voir documentation)
"""),

61 : _("""
Le noeud %(k1)s fait partie d'une maille de type 'SEG' (poutres) en 3D sans donner la normale pour l'appariement.
Vous devez utilisez l'option NORMALE:
- FIXE: qui décrit une normale constante pour la poutre
- ou VECT_Y: qui décrit une normale par construction d'un repère basé sur la tangente (voir documentation)
"""),

75 : _("""
Contact méthodes discrètes. Un POI1 ne peut pas etre une maille maitre.
"""),

84 : _("""
Le modèle mélangent des mailles avec des modélisations de dimensions 
différentes (2D avec 3D ou macro-éléments).
A ce moment du fichier de commande, on ne peut dire si ce mélange sera
compatible avec le contact.

"""),

85 : _("""
L'alarme CONTACT_84 se transforment en erreur fatale !
Il ne faut pas que les surfaces de contact mélangent des mailles affectées d'une modélisations planes (D_PLAN, C_PLAN ou AXI)
avec des mailles affectées d'une modélisation 3D. 

"""),

88 : _("""
Ne pas utiliser REAC_INCR=0 avec le frottement.
"""),

93 : _("""
Contact methode VERIF.
 -> Interpénétrations des surfaces.
 
 -> Risque & Conseil :
    Vérifier si le niveau d'interpénétration des surfaces est acceptable dans
    votre problème.
"""),

94 : _("""
Contact méthode continue. La modélisation COQUE_3D n'est pas encore compatible avec la formulation 'CONTINUE'.
"""),

95 : _("""
Contact méthode continue. Les options RACCORD_LINE_QUAD et FOND_FISSURE sont exclusives.
"""),

96 : _("""
Contact méthode continue. Pour l'option SANS_GROUP_NO_FR, il faut que le frottement soit activé.
"""),

97 : _("""
Contact méthode continue. Pour l'option SANS_GROUP_NO et SANS_GROUP_NO_FR, l'intégration aux noeuds est obligatoire.
"""),

98 : _("""
Contact méthode continue. Pour l'option NORMALE = 'MAIT_ESCL' ou NORMALE = 'ESCL', l'intégration aux noeuds est obligatoire.
"""),

99 : _("""
Contact méthode continue. Vos surfaces de contact esclaves comportent des QUAD8 et vous avez demandé l'option NORMALE = 'MAIT_ESCL' ou NORMALE = 'ESCL'
L'intégration au noeuds est incompatible avec cette option.

Conseils : utilisez un autre schéma d'intégration ou bien des QUAD9.

"""),

}
