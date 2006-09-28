#@ MODIF prepost4 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
 la methode 'taheri_manson' ne peut pas etre utilisee avec l'option %(k1)s 
"""),

2: _("""
 le nom de la fonction  nappe dsigm(depsi,depsimax) doit etre present sous le mot cle 'taheri_nappe'
"""),

3: _("""
 le nom de la fonctiondsigm(depsi) doit etre present sous le mot cle 'taheri_fonc'
"""),

4: _("""
 la methode 'taheri_mixte' ne peut pas etre utilisee avec l'option %(k1)s 
"""),

5: _("""
 l'option  %(k1)s n'a  pas ete calculee dans la sd  %(k2)s 
"""),

6: _("""
 le champ " %(k1)s " ( %(k2)s ) n'a pas ete note dans la sd  %(k3)s 
"""),

7: _("""
 "tube_neuf" n'a de sens que pour une table d'usure existante
"""),

8: _("""
 angle initial different de -180. degres.
"""),

9: _("""
 les angles ne sont pas croissants.
"""),

10: _("""
 angle final different de 180. degres.
"""),

11: _("""
 rayon mobile obligatoire avec             secteur.
"""),

12: _("""
 rayon obstacle obligatoire avec           secteur.
"""),

13: _("""
 la table usure en sortie est differente de celle en entree
"""),

14: _("""
 le nombre de secteurs en sortie est different de celui en entree
"""),

15: _("""
 probleme extraction pour la table  %(k1)s 
"""),

16: _("""
 attention : le calcul du dommage n'a de sens que pour des dsp en contraintes
"""),

17: _("""
 aucune valeur de moment presente
"""),

18: _("""
 y a un bug: recup frequences
"""),

19: _("""
 il faut au moins un group_ma_radier
"""),

20: _("""
 rigidite de translation non nulle
"""),

21: _("""
 rigidite de rotation non nulle
"""),

22: _("""
 nombres de composantes raideurs et mode differents
"""),

23: _("""
 nombres de group_ma et amor_interne differents
"""),

24: _("""
 nombres de composantes amortissements et mode differents
"""),

25: _("""
 option sief_elga non developpee
"""),

26: _("""
 le type du concept resultat  n'est ni evol_elas, ni evol_noli.
"""),

27: _("""
 vous avez probablement archive l etat initial dans la commande stat_non_line. cela correspond au numero d ordre 0. nous ne tenons pas compte du resultat a ce numero d ordre pour le calcul de de la fatigue.
"""),

28: _("""
 les champs de contraintes aux points de gauss sigm_noeu_depl ou sief_noeu_elga sief_noeu_elga n'ont pas ete calcules.
"""),

29: _("""
 les champs de  contraintes aux points de gauss n'existent pas.
"""),

30: _("""
 le champ simple qui contient les valeurs des contraintes n existe pas.
"""),

31: _("""
 le critere de fatemi et socie est prevu pour fonctionner apres un calcul elastoplastique, son utilisation apres meca_statique n'est pas prevue.
"""),

32: _("""
 le champ de contraintes aux points de gauss sief_elga ou sief_elga_depl n'a pas ete calcule.
"""),

33: _("""
 le champ de deformations aux points de gauss epsi_elga_depl n'a pas ete calcule.
"""),

34: _("""
 les champs de  deformations aux points de gauss n'existent pas.
"""),

35: _("""
 le champ simple qui contient les valeurs des deformations n existe pas.
"""),

36: _("""
 les champs de contraintes aux noeuds sigm_noeu_depl ou sief_noeu_elga n'ont pas ete calcules.
"""),

37: _("""
 les champs de  contraintes aux noeuds n'existent pas.
"""),

38: _("""
 le champ de contraintes aux noeuds sief_noeu_elga n'a pas ete calcule.
"""),

39: _("""
 le champ de deformations aux noeuds epsi_noeu_depl n'a pas ete calcule.
"""),

40: _("""
 le champ de  contraintes aux noeuds n'existe pas.
"""),

41: _("""
 le champ de  deformations aux noeuds n'existe pas.
"""),

42: _("""
 la donnee d'une courbe de wohler est obligatoire
"""),

43: _("""
 la donnee du moment spectral d'ordre 4 est obligatoire pour le comptage des pics de contraintes
"""),

44: _("""
 la valeur du moment spectral d'ordre 0 (lambda_0) est certainement nulle
"""),

45: _("""
 la valeur du moment spectral d'ordre 2 (lambda_2) est nulle
"""),

46: _("""
 la valeur du moment spectral d'ordre 4 (lambda_4) est nulle
"""),

47: _("""
 inst_init plus grand que inst_fin
"""),

48: _("""
 l'histoire de chargement doit avoir meme discretisation pour toutes les composantes
"""),

49: _("""
 loi de dommage non compatible
"""),

50: _("""
 l'histoire de la deformation plastique cumulee doit avoir meme discretisation que l'histoire des contraintes
"""),

51: _("""
 l'histoire de la temperature doit avoir meme discretisation que l'histoire des contraintes
"""),

52: _("""
 methode de comptage inconnue
"""),

53: _("""
 nombre de cycles nul
"""),

54: _("""
 l'utilisation de manson_coffin est reserve a des histoires de chargements en deformations
"""),

55: _("""
 la courbe de manson_coffin doit etre donnee dans defi_materiau
"""),

56: _("""
 les lois de taheri sont reservees pour des chargements en deformations
"""),

57: _("""
  erreur donnees.
"""),

58: _("""
 presence de point(s) que dans un secteur.
"""),

59: _("""
 aucun cercle n'est  circonscrit aux quatre points.
"""),

60: _("""
 le decalage se trouve necessairement cote revetement. le decalage doit etre negatif
"""),

61: _("""
 hors bornes definies dans cesmat ou cmp non affectee.
"""),

62: _("""
 les mailles attachees au noeud traite ne sont pas affectees du meme materiau.
"""),

63: _("""
 pour calculer le dommage max il faut renseigner cisa_plan_crit dans la commande defi_materiau
"""),

64: _("""
 nous ne pouvons  pas recuperer la valeur du parametre a du critere de matake, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

65: _("""
 nous ne pouvons  pas recuperer la valeur du parametre b du critere de matake, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

66: _("""
 nous ne pouvons pas recuperer la valeur du coefficient de passage flexion-torsion, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

67: _("""
 nous ne pouvons  pas recuperer la valeur du parametre a du critere de dang_van_modi_ac, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

68: _("""
 nous ne pouvons  pas recuperer la valeur du parametre b du critere de dang_van_modi_ac, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

69: _("""
 nous ne pouvons  pas recuperer la valeur du coefficient de passage cisaillement-traction, cf. commande:  defi_materiau, operande: cisa_plan_crit.
"""),

70: _("""
 nous ne pouvons  pas recuperer la valeur du parametre a du critere domm_maxi, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

71: _("""
 nous ne pouvons  pas recuperer la valeur du parametre b du critere domm_maxi, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

72: _("""
 nous ne pouvons pas recuperer la valeur du coefficient de passage cisaillement-traction, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

73: _("""
 nous ne pouvons  pas recuperer la valeur du parametre a du critere dang_van_modi_av, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

74: _("""
 nous ne pouvons  pas recuperer la valeur du parametre b du critere dang_van_modi_av, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

75: _("""
 nous ne pouvons  pas recuperer la valeur du parametre a du critere fatemi_socie, de la commande:  defi_materiau, operande: cisa_plan_crit.
"""),

76: _("""
 le champ demande n'est pas prevu
"""),

77: _("""
 nom_cham:  %(k1)s  interdit.
"""),

78: _("""
 methode non programmee pour les hexa, penta et pyram
"""),

79: _("""
 probleme de convergence pour calculer la nouvelle carte de taille
"""),

80: _("""
 bug !
"""),

81: _("""
 lunule, bug !
"""),

82: _("""
 type  %(k1)s  non implante.
"""),

83: _("""
 profondeur > rayon du tube
"""),

84: _("""
 pas d'informations dans le "resu_gene" sur l'option "choc".
"""),

85: _("""
 modele non valide.
"""),

86: _("""
  seuil / v0  > 1 
"""),

87: _("""
  ***** arret du calcul ***** 
"""),

88: _("""
 nno > 27
"""),

89: _("""
 type non traite  %(k1)s 
"""),

90: _("""
 les tables tabl_meca_rev et tabl_meca_mdb n ont pas les memes dimensions
"""),

91: _("""
 les tables n ont pas les memes instants de calculs
"""),

92: _("""
 les tables n ont pas les memes dimensions
"""),

93: _("""
 volume use trop grand pour la modelisation
"""),
}
