#@ MODIF prepost3 Messages  DATE 07/11/2011   AUTEUR COURTOIS M.COURTOIS 
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

4 : _(u"""
  le nombre de noeuds sélectionnés est supérieur au nombre de noeuds du maillage. on va tronquer la liste.
"""),

5 : _(u"""
 chaîne de caractères trop longues : imprimer moins de champs
"""),

6 : _(u"""
 type inconnu" %(k1)s "
"""),

7 : _(u"""
 le maillage  %(k1)s  a déjà été écrit au format ENSIGHT: le contenu du fichier  %(k2)s  sera écrasé.
"""),

8 : _(u"""
 problème a l'ouverture du fichier " %(k1)s " pour impression du maillage  %(k2)s  au format ENSIGHT
"""),

9 : _(u"""
 type de base inconnu:  %(k1)s 
"""),

10 : _(u"""
 soit le fichier n'existe pas, soit c'est une mauvaise version de HDF (utilise par MED).
"""),


31 : _(u"""
 on n'a pas trouvé le numéro d'ordre à l'adresse indiquée
"""),

32 : _(u"""
 on n'a pas trouvé l'instant à l'adresse indiquée
"""),

33 : _(u"""
 on n'a pas trouvé la fréquence à l'adresse indiquée
"""),

34 : _(u"""
 on n'a pas trouvé dans le fichier UNV le type de champ
"""),

35 : _(u"""
 on n'a pas trouvé dans le fichier UNV le nombre de composantes à lire
"""),

36 : _(u"""
 on n'a pas trouvé dans le fichier UNV la nature du champ
 (réel ou complexe)
"""),

37 : _(u"""
 le type de champ demandé est différent du type de champ à lire
"""),

38 : _(u"""
 le champ demande n'est pas de même nature que le champ à lire
 (réel/complexe)
"""),

39 : _(u"""
 le mot clé MODELE est obligatoire pour un CHAM_ELEM
"""),

40 : _(u"""
 Problème correspondance noeud IDEAS
"""),

41 : _(u"""
 le champ de type ELGA n'est pas supporté
"""),

63 : _(u"""
 on attend 10 ou 12 secteurs
"""),

64 : _(u"""
 ******* percement tube *******
"""),

65 : _(u"""
 pour la variable d'accès "NOEUD_CMP", il faut un nombre pair de valeurs.
"""),

66 : _(u"""
 le modèle et le maillage introduits ne sont pas cohérents
"""),

67 : _(u"""
 il faut donner le maillage pour une impression au format "CASTEM".
"""),

68 : _(u"""
 vous voulez imprimer sur un même fichier le maillage et un champ
 ce qui est incompatible avec le format GMSH
"""),

69 : _(u"""
 L'impression d'un champ complexe nécessite l'utilisation du mot-clé PARTIE.
 Ce mot-clé permet de choisir la partie du champ à imprimer (réelle ou imaginaire).
"""),

70 : _(u"""
 Vous avez demandé une impression au format ASTER sans préciser de MAILLAGE.
 Aucune impression ne sera réalisée car IMPR_RESU au format ASTER n'imprime qu'un MAILLAGE.
"""),

73 : _(u"""
 l'impression avec sélection sur des entités topologiques n'a pas de sens au format CASTEM  : toutes les valeurs sur tout le maillage seront donc imprimées.
"""),

74 : _(u"""
 Le maillage %(k1)s n'est pas cohérent avec le maillage %(k2)s portant le résultat %(k3)s
"""),

75 : _(u"""
 fichier GIBI créé par SORT FORMAT non supporté dans cette version
"""),

76 : _(u"""
 version de GIBI non supportée, la lecture peut échouer
"""),

77 : _(u"""
 fichier GIBI erroné
"""),

78 : _(u"""
 le fichier maillage GIBI est vide
"""),

79 : _(u"""
 cette commande ne fait que compléter un résultat composé déjà existant.
 il faut donc que le résultat de la commande :  %(k1)s
 soit identique à l'argument "RESULTAT" :  %(k2)s 
"""),

80 : _(u"""
 pour un résultat de type " %(k1)s ", on ne traite que l'option ..._NOEU_...
"""),

81 : _(u"""
 lmat =0
"""),

84 : _(u"""
 il faut autant de composantes en i et j
"""),

85 : _(u"""
 il faut autant de composantes que de noeuds
"""),

92 : _(u"""
 mot clé "TEST_NOOK" non validé avec le mot clé facteur "INTE_SPEC".
"""),

93 : _(u"""
 la fonction n'existe pas.
"""),

94 : _(u"""
 il faut définir deux paramètres pour une nappe.
"""),

95 : _(u"""
 pour le paramètre donné on n'a pas trouvé la fonction.
"""),


}
