#@ MODIF algorith14 Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
 arret sur matrice inexistante matrice %(k1)s 
"""),

2: _("""
 arret sur matrice inexistante matrice %(k1)s 
"""),

3: _("""
 &type interface non supportee en cyclique type interface -->  %(k1)s 
"""),

4: _("""
 arret sur type de resultat non supporte type donne -->  %(k1)s 
  types supportes -->  %(k2)s %(k3)s
"""),

5: _("""
 arret sur matrice inexistante matrice: %(k1)s 
"""),

6: _("""
 arret sur type de resultat non supporte type donne -->  %(k1)s 
  types supportes -->  %(k2)s %(k3)s
"""),

7: _("""
 arret sur matrice inexistante matrice: %(k1)s 
"""),

8: _("""
 manque la deformee modale pour le mode  %(i1)d 
"""),

9: _("""
  -------------------------------- %(k1)s nombre de points pas periode:  %(i1)d 
 coefficient de remontee du pas de temps:  %(r1)f 
 coefficient de division du pas de temps:  %(r2)f 
 coeff determinant dt min (=dt init*coeff):  %(r3)f 
 nombre maximal de reductions du pas:  %(i2)d 
 vitesse minimale variable:  %(k2)s 
"""),

10: _("""
 le maille noe n'existe pas dans le maillage mail mail= %(k1)s noe= %(k2)s 
"""),

11: _("""
 le noeud n'existe pas dans le maillage maillage= %(k1)s noeud= %(k2)s 
"""),

12: _("""
 arret sur maillage non skelette
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

17: _("""
 &donnee de matrice masse et option classique incompatibles matrice masse: %(k1)s 
 option automatiquement mise a %(k2)s 
"""),

18: _("""
 &donnee de matrice raideur et option classique incompatibles matrice raideur: %(k1)s 
 option automatiquement mise a %(k2)s 
"""),

19: _("""
 &base modale ritz et option classique incompatibles
 option automatiquement mise a %(k1)s 
"""),

20: _("""
 &base modale ritz: il faut donner les matrice a projeter
 matrice masse donnee: %(k1)s 
 matrice raideur donnee: %(k2)s 
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

29: _("""
 le maillage n'est pas un maillage squelette maillage %(k1)s 
"""),

30: _("""
 incoherence detectee dans squelette objet non trouve :  %(k1)s 
"""),

31: _("""
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

34: _("""
 incoherence detectee dans squelette objet non trouve :  %(k1)s 
"""),

35: _("""
 aucun champ n'est calculedans la structure de donnees  %(k1)s 
"""),

36: _("""
 les numerotations des champs ne coincident pas celui de  %(k1)s  est :  %(k2)s 
 et celui de  %(k3)s 
  est :  %(k4)s 
"""),

37: _("""
 sous-structure inexistante dans le modele generalise modele generalisee %(k1)s 
 sous-structure %(k2)s 
"""),

38: _("""
 probleme coherence nombre de champs base modale base modale %(k1)s 
 nombre de champs de la base %(i1)d 
 nombre de dgres generalises %(i2)d 
"""),

39: _("""
 le maillage n'est pas un maillage squelette maillage %(k1)s 
"""),

40: _("""
 aucun champ n'est calculedans la structure de donnees  %(k1)s 
"""),

41: _("""
 les numerotations des champs ne coincident pas celui de  %(k1)s  est :  %(k2)s 
 et celui de  %(k3)s est :  %(k4)s 
"""),

42: _("""
 arret sur type de resultat non supporte type donne -->  %(k1)s 
  types supportes -->  %(k2)s %(k3)s
"""),

43: _("""
 arret sur matrice inexistante matrice: %(k1)s 
"""),

44: _("""
 arret sur type de resultat non supporte type donne -->  %(k1)s 
  types supportes -->  %(k2)s %(k3)s
"""),

45: _("""
 arret sur matrice inexistante matrice: %(k1)s 
"""),

46: _("""
 aucun champ n'est calcule dans la structure de donnees  %(k1)s 
"""),

47: _("""
 sous-structure inexistante dans le  modele generalise modele generalisee %(k1)s 
 sous-structure %(k2)s 
"""),

48: _("""
 le maillage n'est pas un maillage  squelette maillage %(k1)s 
"""),

49: _("""
 aucun champ n'est calcule dans la structure de donnees  %(k1)s 
"""),

50: _("""
 il faut au moins 1 mode !
"""),

51: _("""
 il faut un mode_meca a la 1ere occurence de ritz
"""),

52: _("""
 il faut au moins 1 mode !
"""),


55: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
"""),

56: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
"""),

57: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
"""),

58: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
"""),

59: _("""
 le champ de "meta_elno_temp"  n'existe pas
  pour le numero d'ordre  %(i1)d 
"""),

60: _("""
 le champ de "temp" n'existe pas pour le numero d'ordre  %(i1)d 
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

65: _("""
 manque la deformee modale nom_cham  %(k1)s  pour le mode  %(i1)d 
"""),

66: _("""
 &taille de bloc insuffisante taille de bloc demandee (kr8): %(r1)f 
 taille de bloc utilisee (kr8): %(r2)f 
"""),

67: _("""
 champ inexistant champ:  %(k1)s , nume_ordre:  %(i1)d , mode_meca:  %(k2)s 
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

}
