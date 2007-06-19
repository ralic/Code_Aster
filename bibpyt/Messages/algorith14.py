#@ MODIF algorith14 Messages  DATE 19/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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














3: _("""
 &type interface non supportee en cyclique type interface -->  %(k1)s 
"""),

4: _("""
 arret sur type de resultat non supporte type donne -->  %(k1)s 
  types supportes -->  %(k2)s %(k3)s
"""),






















8: _("""
 manque la deformee modale pour le mode  %(i1)d 
"""),






10: _("""
 le maille noe n'existe pas dans le maillage mail mail= %(k1)s noe= %(k2)s 
"""),

11: _("""
 le noeud n'existe pas dans le maillage maillage= %(k1)s noeud= %(k2)s 
"""),








13: _("""
 & arret sur base modale de type illicitebase modale -->  %(k1)s type -->  %(k2)s 
 type  attendu -->  %(k3)s 
"""),

14: _("""
 arret sur matrice raideur non unique
"""),

15: _("""
 arret sur matrice masse non unique
"""),

16: _("""
 arret sur matrice amortissement non unique en argument
"""),





















21: _("""
 les matrices assemblees n'ont pas la meme numerotation masse= %(k1)s 
 raideur= %(k2)s 
"""),

22: _("""
 les matrices assemblees n'ont pas la meme numerotation amortissement= %(k1)s 
 raideur= %(k2)s 
"""),

23: _("""
 
 les matrices assemblees et la base modalen'ont pas le meme maillage initial
 maillage matrice: %(k1)s 
 maillage base modale: %(k2)s 
"""),

24: _("""
 arret sur probleme coherence mode_meca donne -->  %(k1)s 
  numerotation associee -->  %(k2)s 
  interf_dyna donnee -->  %(k3)s 
  numerotation associee -->  %(k4)s 
"""),

25: _("""
 sous-structure inexistante dans le modele generalise modele generalisee %(k1)s 
 sous-structure %(k2)s 
"""),

26: _("""
 probleme coherence nombre de champs base modale base modale %(k1)s 
 nombre de champs de la base %(i1)d 
 nombre de dgres generalises %(i2)d 
"""),

27: _("""
 le maillage n'est pas un maillage squelette maillage %(k1)s 
"""),

28: _("""
  aucun type d'interface defini pour la sous structure :  %(i1)d 
  pas de mode rigide d'interface  le calcul de masses effectives risque d'etre  imprecis %(i2)d 
"""),








30: _("""
 incoherence detectee dans squelette objet non trouve :  %(k1)s 
"""),






32: _("""
 sd resultat  resultle champ n'existe pas  %(k1)s 
 pour le nume_ordre  %(i1)d 
"""),

33: _("""
 sd resultat  nomresle champ n'a pas ete duplique  %(k1)s 
 pour le nume_ordre  %(i1)d 
"""),






35: _("""
 aucun champ n'est calculedans la structure de donnees  %(k1)s 
"""),

36: _("""
 les numerotations des champs ne coincident pas celui de  %(k1)s  est :  %(k2)s 
 et celui de  %(k3)s 
  est :  %(k4)s 
"""),




























































































50: _("""
 il faut au moins 1 mode !
"""),

51: _("""
 il faut un mode_meca a la 1ere occurence de ritz
"""),









55: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
"""),




















59: _("""
 le champ de "meta_elno_temp"  n'existe pas
  pour le numero d'ordre  %(i1)d 
"""),








61: _("""
 
 le pas de temps du calcul  metallurgique ne correspond pas au pas  de temps du calcul thermique
  numero d'ordre  %(i1)d 
     pas de temps thermique  %(r1)f 
     pas de temps metallurgique  %(r2)f 
"""),

62: _("""
 manque la deformee modale nom_cham  %(k1)s  pour le mode  %(i1)d 
"""),

63: _("""
 donnees incompatibles : pour le mode_stat  :  %(k1)s 
  il manque le champ :  %(k2)s 
"""),

64: _("""
 manque le mode statique nom_cham  %(k1)s  pour le mode  %(i1)d 
"""),








66: _("""
 &taille de bloc insuffisante taille de bloc demandee (kr8): %(r1)f 
 taille de bloc utilisee (kr8): %(r2)f 
"""),








68: _("""
  valeur minimale conseillee :  %(r1)f 
"""),

69: _("""
 non-linearite incompatible avec  la definition du modele generalise
 noeud_1      :  %(k1)s 
 sous_struc_1 :  %(k2)s 
 noeud_2      :  %(k3)s 
 sous_struc_2 :  %(k4)s 
"""),

70: _("""
 &probleme de coherence de nombre de noeuds d'interface
 sous-structure1: %(k1)s 
 interface1: %(k2)s 
 nombre de noeuds interface1: %(i1)d 
 sous-structure2: %(k3)s 
 interface2: %(k4)s 
 nombre de noeuds interface2: %(i2)d 
"""),

71: _("""
 &probleme de coherence des interfaces orientees sous-structure1: %(k1)s 
 interface1: %(k2)s 
 presence composante sur 1: %(k3)s 
 sous-structure2: %(k4)s 
 interface2: %(k5)s 
 composante inexistante sur 2 %(k6)s 
"""),

72: _("""
 &probleme de coherence des interfaces orientees sous-structure2: %(k1)s 
 interface2: %(k2)s 
 presence composante sur 2: %(k3)s 
 sous-structure1: %(k4)s 
 interface1: %(k5)s 
 composante inexistante sur 1 %(k6)s 
"""),

73: _("""
 &sous-structure incompatibles sous-structure 1:: %(k1)s macr_elem associe: %(k2)s 
 numero grandeur sous-jacente: %(i1)d 
 sous-structure 2:: %(k3)s 
 macr_elem associe: %(k4)s 
 numero grandeur sous-jacente: %(i2)d 
"""),

74: _("""
 &arret sur incompatibilite de sous-structure 
"""),

75: _("""
  Erreur développement : code retour 1 dans nmcomp en calculant la matrice tangente
 """),

76: _("""
  Objet &FETI.MONITORING.MPI inexistant !
 """),

77: _("""
 les types des deux matrices sont differents
 type de la matrice de raideur :  %(k1)s 
 type de la matrice de masse   :  %(k2)s 
"""),

78: _("""
 les numerotations des deux matrices sont differentes
 numerotation matrice de raideur :  %(k1)s 
 numerotation matrice de masse   :  %(k2)s 
"""),

79: _("""
 coefficient de conditionnement des lagranges:  %(r1)f 
"""),

80: _("""
 affichage des coeff d'amortissement:
 premier coefficient d'amortissement %(r1)f 
 second coefficient d'amortissement %(r2)f 
"""),

81: _("""
  calcul cyclique:aucun nombre de diametre nodaux demande
"""),

82: _("""
 calcul du nombre de diametres modaux demande impossible
 nombre de diametre demande --> %(i1)d 
"""),

83: _("""
 calcul des modes propres limite a nombre de diametres maximum --> %(i1)d 
"""),

84: _("""
 cyclique: aucun nombre de diametres nodaux licite
"""),

85: _("""
 liste de frequences incompatible avec optionnombre de frequences --> %(i1)d 
 option --> %(k1)s 
"""),

86: _("""
 liste de frequences incompatible avec optionnombre de frequences --> %(i1)d 
 option --> %(k1)s 
"""),

87: _("""
  resolution du probleme generalise complexe
      nombre de modes dynamiques:  %(i1)d 
      nombre de ddl droite:  %(i2)d 
"""),

88: _("""
      nombre de ddl axe:  %(i1)d
             dont cycliques:  %(i2)d 
             dont non cycliques:  %(i3)d 
"""),

89: _("""
      dimensiom max du probleme:  %(i1)d 
"""),

90: _("""
 etude 2dangle nautique unique :  %(r1)f 
"""),

91: _("""
 noeud sur l'axe_z noeud :  %(k1)s 
"""),

92: _("""
 noeud sur l'axe_z noeud :  %(k1)s 
"""),

93: _("""
 arret sur dimension matrice teta incorrecte dimension effective:  %(i1)d 
 dimension en argument:  %(i2)d 
"""),

94: _("""
  erreur  de repetitivite cyclique
"""),

95: _("""
  il manque un ddl sur un noeud  axe type du ddl -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),

96: _("""
  erreur  de repetitivite cyclique
"""),

97: _("""
  il manque un ddl sur un noeud  axe type du ddl -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),

98: _("""
 arret sur probleme de repetitivite cyclique
"""),

99: _("""
 arret sur nombres de noeuds interface non identiques 
 nombre de noeuds interface droite:  %(i1)d 
 nombre de noeuds interface gauche:  %(i2)d 
"""),

}
