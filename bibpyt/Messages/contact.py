#@ MODIF contact Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

1 : _(u"""
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

2 : _(u"""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le GCP.
Vous pouvez essayer d'augmenter ITER_GCP_MAXI.
La liste des noeuds présentant une interpénétration est donnée ci-dessous.
"""),

3 : _(u"""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le préconditionneur.
Vous pouvez essayer d'augmenter ITER_PRE_MAXI
"""),

4 : _(u"""
Contact methode GCP. Le paramètre RESI_ABSO doit etre obligatoirement renseigné.
"""),

7 : _(u"""
Contact methode GCP. Le pas d'avancement est negatif ; risque de comportement hasardeux de l'algorithme
"""),

8 : _(u"""
Contact méthodes discrètes.
 -> Il y a des éléments de type QUAD8 sur la surface esclave de contact. Ces éléments produisent des forces nodales négatives aux noeuds sommets.
    Afin de limiter les oscillations des forces et d'empêcher une pénétration intempestive de la surface maître dans la surface esclave, on
    a procédé à des liaisons cinématiques (LIAISON_DDL) entre les noeuds milieux et les noeuds sommets, sur les deux surfaces (maître et esclave).
 -> Risque & Conseil :
    Il est préférable d'utiliser des éléments de type QUAD9. Changer votre maillage ou utiliser la commande MODI_MAILLAGE.
    Ces liaisons supplémentaires peuvent provoquer des incompatibilités avec les conditions limites, ce qui se traduira par un pivot nul dans
    la matrice.
"""),

9 : _(u"""
Contact liaison glissiere. Des noeuds se décollent plus que la valeur d'ALARME_JEU:
"""),

13 : _(u"""
La normale que vous avez prédéfinie (VECT_* = 'FIXE') sur le noeud %(k1)s est colinéaire à la tangente à la maille.
"""),

14 : _(u"""
La normale que vous avez prédéfinie (VECT_* = 'FIXE') sur la maille %(k1)s est colinéaire à la tangente à la maille.
"""),

15 : _(u"""
Le vecteur MAIT_FIXE ou ESCL_FIXE est nul !
"""),

16 : _(u"""
Le vecteur MAIT_VECT_Y ou ESCL_VECT_Y est nul !
"""),

60 : _(u"""
La maille %(k1)s est de type 'SEG' (poutres) en 3D. Pour ces mailles la normale ne peut pas être déterminée automatiquement.
Vous devez utilisez l'option NORMALE :
- FIXE : qui décrit une normale constante pour la poutre
- ou VECT_Y : qui décrit une normale par construction d'un repère basé sur la tangente (voir documentation)
"""),

61 : _(u"""
Le noeud %(k1)s fait partie d'une maille de type 'SEG' (poutres) en 3D. Pour ces mailles la normale ne peut pas être déterminée automatiquement.
Vous devez utilisez l'option NORMALE :
- FIXE : qui décrit une normale constante pour la poutre
- ou VECT_Y: qui décrit une normale par construction d'un repère basé sur la tangente (voir documentation)
"""),


84 : _(u"""
Le modèle mélange des mailles avec des modélisations de dimensions différentes (2D avec 3D ou macro-éléments).
À ce moment du fichier de commandes, on ne peut dire si ce mélange sera compatible avec le contact.
"""),

85 : _(u"""
L'alarme CONTACT_84 se transforme en erreur fatale !
Il ne faut pas que les surfaces de contact mélangent des mailles affectées d'une modélisation plane (D_PLAN, C_PLAN ou AXI)
avec des mailles affectées d'une modélisation 3D. 
"""),

88 : _(u"""
N'utilisez pas REAC_INCR=0 avec le frottement.
"""),

93 : _(u"""
Contact methode VERIF.
 -> Interpénétrations des surfaces.
 
 -> Risque & Conseil :
    Vérifier si le niveau d'interpénétration des surfaces est acceptable dans
    votre problème.
"""),

94 : _(u"""
Contact méthode continue. La modélisation COQUE_3D n'est pas encore compatible avec la formulation 'CONTINUE'.
"""),

95 : _(u"""
Contact méthode continue. Les options RACCORD_LINE_QUAD et FOND_FISSURE sont exclusives.
"""),

96 : _(u"""
Contact méthode continue. La prise en compte d'un contact entre une maille '%(k1)s' et une maille '%(k2)s' n'est pas prévu.

Conseils :
- utilisez une formulation 'DISCRETE'
- contactez votre assistance technique
"""),

97 : _(u"""
Contact méthode continue. Pour l'option SANS_GROUP_NO et SANS_GROUP_NO_FR, l'intégration aux noeuds est obligatoire.
"""),

98 : _(u"""
Contact méthode continue. Pour l'option NORMALE = 'MAIT_ESCL' ou NORMALE = 'ESCL', l'intégration aux noeuds est obligatoire.
"""),

99 : _(u"""
Contact méthode continue. Vos surfaces de contact esclaves comportent des QUAD8 et vous avez demandé l'option NORMALE = 'MAIT_ESCL' ou NORMALE = 'ESCL'
L'intégration aux noeuds est incompatible avec cette option.

Conseil : utilisez un autre schéma d'intégration ou bien des QUAD9.

"""),

}
