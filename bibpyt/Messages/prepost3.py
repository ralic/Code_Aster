#@ MODIF prepost3 Messages  DATE 12/04/2010   AUTEUR SELLENET N.SELLENET 
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

4 : _("""
  le nombre de noeuds selectionnes est superieur au nombre de noeuds du maillage. on va tronquer la liste.
"""),

5 : _("""
 chaine de caracteres trop longues : imprimer moins de champs
"""),

6 : _("""
 type inconnu" %(k1)s "
"""),

7 : _("""
 le maillage  %(k1)s  a deja ete ecrit au format ensight: le contenu du fichier  %(k2)s  sera ecrase.
"""),

8 : _("""
 probleme a l'ouverture du fichier " %(k1)s " pour impression du maillage  %(k2)s  au format ensight
"""),

9 : _("""
 type de base inconnu:  %(k1)s 
"""),

10 : _("""
 soit le fichier n'existe pas, soit c'est une mauvaise version de hdf (utilise par med).
"""),

29 : _("""
 pour LIRE_RESU / FORMAT=IDEAS
 le maillage doit normalement avoir été lu au format IDEAS.
"""),

30 : _("""
 on a trouve plusieurs champs pour le même DATASET
"""),

31 : _("""
 on n'a pas trouvé le numéro d'ordre à l'adresse indiquée
"""),

32 : _("""
 on n'a pas trouvé l'instant à l'adresse indiquée
"""),

33 : _("""
 on n'a pas trouvé la fréquence à l'adresse indiquée
"""),

34 : _("""
 on n'a pas trouvé dans le fichier UNV le type de champ
"""),

35 : _("""
 on n'a pas trouvé dans le fichier UNV le nombre de composantes à lire
"""),

36 : _("""
 on n'a pas trouvé dans le fichier UNV la nature du champ
 (réel ou complexe)
"""),

37 : _("""
 le type de champ demandé est différent du type de champ à lire
"""),

38 : _("""
 le champ demande n'est pas de même nature que le champ à lire
 (réel/complexe)
"""),

39 : _("""
 le mot cle MODELE est obligatoire pour un CHAM_ELEM
"""),

40 : _("""
 pb correspondance noeud IDEAS
"""),

41 : _("""
 le champ de type ELGA n'est pas supporté
"""),

63 : _("""
 on attend 10 ou 12 secteurs
"""),

64 : _("""
 ******* percement tube *******
"""),

65 : _("""
 pour la variable d'acces "noeud_cmp", il faut un nombre pair de valeurs.
"""),

66 : _("""
 le modèle et le maillage introduits ne sont pas cohérents
"""),

67 : _("""
 il faut donner le maillage pour une impression au format "CASTEM".
"""),

68 : _("""
 vous voulez imprimer sur un même fichier le maillage et un champ
 ce qui est incompatible avec le format GMSH
"""),

69 : _("""
 L'impression d'un champ complexe nécessite l'utilisation du mot-clé PARTIE.
 Ce mot-clé permet de choisir la partie du champ à imprimer (réelle ou imaginaire).
"""),

70 : _("""
 Vous avez demandé une impression au format ASTER sans préciser de MAILLAGE.
 Aucune impression ne sera réalisée car IMPR_RESU au format ASTER n'imprime qu'un MAILLAGE.
"""),

72 : _("""
 l'impression avec selection sur des entites topologiques n'a pas de sens au format ensight : les valeurs de tous les noeuds du maillage seront donc imprimees.
"""),

73 : _("""
 l'impression avec selection sur des entites topologiques n'a pas de sens au format castem  : toutes les valeurs sur tout le maillage seront donc imprimees.
"""),

74 : _("""
 Le maillage %(k1)s n'est pas coherent avec le maillage %(k2)s portant le resultat %(k3)s
"""),

75 : _("""
 fichier GIBI créé par SORT FORMAT non supporté dans cette version
"""),

76 : _("""
 version de GIBI non supportée, la lecture peut échouer
"""),

77 : _("""
 fichier GIBI erroné
"""),

78 : _("""
 le fichier maillage GIBI est vide
"""),

79 : _("""
 cette commande ne fait que compléter un résultat composé déjà existant.
 il faut donc que le résultat de la commande :  %(k1)s
 soit identique à l'argument "RESULTAT" :  %(k2)s 
"""),

80 : _("""
 pour un résultat de type " %(k1)s ", on ne traite que l'option ..._NOEU_...
"""),

81 : _("""
 lmat =0
"""),

84 : _("""
 il faut autant de composantes en i et j
"""),

85 : _("""
 il faut autant de composantes que de noeuds
"""),

92 : _("""
 mot clé "TEST_NOOK" non validé avec le mot clé facteur "INTE_SPEC".
"""),

93 : _("""
 la fonction n'existe pas.
"""),

94 : _("""
 il faut définir deux paramètres pour une nappe.
"""),

95 : _("""
 pour le paramètre donné on n'a pas trouvé la fonction.
"""),

96 : _("""
 l'option FORC_NODA_NONL doit être appelée seule
"""),

97 : _("""
 avec l'option FORC_NODA_NONL il faut que le résultat de la commande 
 soit différent de l'argument "RESULTAT" :  %(k1)s 
"""),

98 : _("""
 l'option FORC_NODA_NONL doit être calculée sur un résultat DYNA_TRANS
"""),

}
