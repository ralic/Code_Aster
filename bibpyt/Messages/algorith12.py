#@ MODIF algorith12 Messages  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
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

2 : _("""
 interface inexistante
 numéro liaison            : %(i1)d
 nom sous-structure        : %(k1)s
 nom MACR_ELEM             : %(k2)s
 nom interface inexistante : %(k3)s
"""),

3 : _("""
 une sous-structure est sans connexion sous-structure -->  %(k1)s
"""),

7 : _("""
 données incompatibles :
 pour les modes mecaniques :  %(k1)s
 il manque l'option        :  %(k2)s
"""),

12 : _("""
 données incompatibles :
 pour les MODE_CORR :  %(k1)s
 il manque le champ :  %(k2)s
"""),

13 : _("""
 données incompatibles :
 pour les mode_corr :  %(k1)s
 pour le champ      :  %(k2)s
 le type n'est pas  %(k3)s
"""),

14 : _("""
 donnees incompatibles :
 pour les statiques :  %(k1)s
 il manque le champ :  %(k2)s
"""),

15 : _("""
 données incompatibles :
 pour les statiques :  %(k1)s
 pour le champ      :  %(k2)s
 le type n'est pas  %(k3)s
"""),

18 : _("""
 on ne sait pas bien traiter l'option de calcul demandée :  %(k1)s
"""),

20 : _("""
 données incompatibles :
 pour les modes mécaniques :  %(k1)s
 pour l'option             :  %(k2)s
 il manque le champ d'ordre  %(i1)d
"""),

21 : _("""
 données incompatibles :
 pour les mode_corr :  %(k1)s
 il manque l'option :  %(k2)s
"""),

22 : _("""
 données incompatibles :
 pour les modes statiques :  %(k1)s
 il manque l'option       :  %(k2)s
"""),

23 : _("""
 arret sur question illicite pour le type de base
 type de base -->  %(k1)s
 question     -->  %(k2)s
"""),

26 : _("""
 arret sur manque argument
 base modale donnée -->  %(k1)s
 interf_dyna donnée -->  %(k2)s
"""),

27 : _("""
 arret sur type de base incorrecte
 base modale donnée -->  %(k1)s
 type  base modale  -->  %(k2)s
 type attendu       -->  %(k3)s
"""),

28 : _("""
 arret sur incohérence données
 base modale donnée         -->  %(k1)s
 INTERF_DYNA correspondante -->  %(k2)s
 INTERF_DYNA donnée         -->  %(k3)s
"""),

29 : _("""
 problème arguments de définition interface
 nom interface donné    %(k1)s
 numero interface donné %(i1)d
"""),

30 : _("""
 arret sur base modale sans INTERF_DYNA
 base modale donnée -->  %(k1)s
"""),

31 : _("""
 arret sur manque arguments
 base modale donnée -->  %(k1)s
 INTERF_DYNA donnée -->  %(k2)s
"""),

38 : _("""
 arret sur problème cohérence interface
"""),

39 : _("""
 arret sur matrice inexistante
 matrice %(k1)s
"""),

40 : _("""
  arret problème de factorisation:
  présence probable de modes de corps rigide
  la methode de Mac-Neal ne fonctionne pas en présence de modes de corps rigide
"""),

41 : _("""
  la taille bloc  : %(i1)d est < HAUTEUR_MAX : %(i2)d
  changez la taille_bloc des profils: %(k1)s
  prenez au moins : %(i3)d
"""),

42 : _("""
 le mot-clé  %(k1)s est incompatible avec le champ %(k2)s
 utilisez 'GROUP_MA' ou 'MAILLE'  pour restreindre le changement de repere
 à certaines mailles. %(k3)s
"""),

43 : _("""
 étude 2D
 angle nautique unique :  %(r1)f
"""),

44 : _("""
 noeud sur l'AXE_Z noeud :  %(k1)s
"""),

49 : _("""
 problème: sous-structure inconnue
 sous-structure -->  %(k1)s
"""),

50 : _("""
 pas de sous-structure dans le squelette
"""),

51 : _("""
 nom de sous-structure non trouvé 
 la sous-structure :  %(k1)s n existe pas  %(k2)s
"""),

53 : _("""
 arret sur pivot nul
 ligne -->  %(i1)d
"""),

55 : _("""
 le MAILLAGE : %(k1)s ne contient pas de GROUP_MA
"""),

56 : _("""
 le GROUP_MA : %(k2)s n'existe pas dans le MAILLAGE : %(k1)s
"""),

57 : _("""
 le MAILLAGE : %(k1)s ne contient pas de GROUP_NO  
"""),

58 : _("""
 le GROUP_NO : %(k2)s n'existe pas dans le MAILLAGE : %(k1)s
"""),

59 : _("""
 nombre de noeuds communs NBNOCO =  %(i1)d
"""),

62 : _("""
 les deux numérotations n'ont pas meme maillage d'origine
  numérotation 1: %(k1)s
  maillage     1: %(k2)s
  numérotation 2: %(k3)s
  maillage     2: %(k4)s
"""),

63 : _("""
 perte d'information sur DDL physique à la conversion de numérotation
 noeud numéro    :  %(i1)d
 type DDL numéro :  %(i2)d
"""),

64 : _("""
 arret sur perte d'information DDL physique
"""),

66 : _("""
 champ inexistant
 CHAMP      :  %(k1)s
 NUME_ORDRE :  %(i1)d
 MODE_MECA  :  %(k2)s
"""),

67 : _("""
 arret sur problème de conditions d'interface
"""),

68 : _("""
 le maillage final n'est pas 3D
 maillage : %(k1)s
"""),

69 : _("""
 l'origine du maillage 1D n'est pas 0
"""),

70 : _("""
 les noeuds du maillage sont confondus
"""),

71 : _("""

 le noeud se trouve en dehors du domaine de définition avec un profil gauche de type EXCLU
 noeud :  %(k1)s
"""),

72 : _("""

 le noeud se trouve en dehors du domaine de definition avec un profil droit de type EXCLU
 noeud :  %(k1)s
"""),

73 : _("""
 problème pour stocker le champ dans le résultat :  %(k1)s
 pour le NUME_ORDRE :  %(i1)d
"""),

74 : _("""
 champ déjà existant
 il sera remplacé par le champ %(k1)s
 pour le NUME_ORDRE  %(i1)d
"""),

76 : _("""
  problème de récuperation CHAMNO
  concept résultat:  %(k1)s
  numéro d'ordre  :  %(i1)d
"""),

77 : _("""
 pas d'interface définie
"""),

78 : _("""
 arret sur interface déjà définie
 mot-clé interface numero  -->  %(i1)d
 interface                 -->  %(k1)s
"""),

79 : _("""
 les deux interfaces n'ont pas le meme nombre de noeuds
 nombre noeuds interface droite -->  %(i1)d
 nombre noeuds interface gauche -->  %(i2)d
"""),

80 : _("""
 les deux interfaces n'ont pas le meme nombre de degrés de liberté
 nombre ddl interface droite -->  %(i1)d
 nombre ddl interface gauche -->  %(i2)d
"""),

81 : _("""
 arret sur base modale ne comportant pas de modes propres
"""),

82 : _("""

 nombre de modes propres demandé supérieur au nombre de modes dynamiques de la base
 nombre de modes demandés       --> %(i1)d
 nombre de modes de la base     --> %(i2)d
 nombre de fréquences douteuses --> %(i3)d
"""),

83 : _("""
 plusieurs champs correspondant à l'acces demandé
 resultat     : %(k1)s
 acces "INST" : %(r1)f
 nombre       : %(i1)d
"""),

84 : _("""
 pas de champ correspondant à un accès demandé
 résultat     :  %(k1)s
 acces "INST" :  %(r1)f
"""),

89 : _("""
 instant de reprise supérieur à la liste des instants
 instant de reprise :  %(r1)f
 instant max        :  %(r2)f
"""),

90 : _("""
 on n'a pas trouvé l'instant
 instant de reprise:  %(r1)f
 pas de temps      :  %(r2)f
 borne min         :  %(r3)f
 borne max         :  %(r4)f
"""),

91 : _("""
 instant final inférieur à la liste des instants
 instant final:  %(r1)f
 instant min  :  %(r2)f
"""),

92 : _("""
 on n'a pas trouvé l'instant
 instant final:  %(r1)f
 pas de temps :  %(r2)f
 borne min    :  %(r3)f
 borne max    :  %(r4)f
"""),

97 : _("""
 données erronées
 pas d'instant de calcul pour l'instant d'archivage :  %(r1)f
"""),

98 : _("""
 données erronées
 plusieurs instants de calcul pour l'instant d'archivage:  %(r1)f
"""),

}
