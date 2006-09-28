#@ MODIF utilitai3 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 erreur dans la donnee de la s.d.  %(k1)s  a imprimer, il ne s'agit ni d'un matr_elem, ni d'un vect_elem.
"""),

2: _("""
 l utilisation de cette commande n est legitime que si la configuration etudiee est du type "faisceau_trans"
"""),

3: _("""
 erreur dvp_1
"""),

4: _("""
 le nom d'un parametre ne peut pas depasser 16 caracteres
"""),

5: _("""
 le parametre  %(k1)s  n 'existe pas
"""),

6: _("""
 seuls les parametres de types reel, entier ou complexe sont traites
"""),

7: _("""
 erreur dvp_2
"""),

8: _("""
 code retour non nul detecte
"""),

9: _("""
 maillage autre que seg2                                ou poi1 
"""),

10: _("""
 mailles ponctuelles                           plexus poi1 ignorees
"""),

11: _("""
 le format doit etre ideas
"""),

12: _("""
 le maillage doit etre issu d'ideas
"""),

13: _("""
  maillage non issu d'ideas
"""),

14: _("""
 avec le 2414, on ne traite pas les nume_ordre
"""),

15: _("""
 pb lecture du fichier ideas
"""),

16: _("""
 nom_med ? (svp)
"""),

17: _("""
 format  %(k1)s  inconnu.
"""),

18: _("""
 nom_cmp_idem est curieux :  %(k1)s 
"""),

19: _("""
 probleme maillage <-> modele
"""),

20: _("""
 le champ de meta_elno_temp:etat_init(num_init) n'existe pas.
"""),

21: _("""
 maillage et modele incoherents.
"""),

22: _("""
 pour type_resu:'el..' il faut renseigner le mot cle modele.
"""),

23: _("""
 option:  %(k1)s non prevue pour les elements du modele.
"""),

24: _("""
 option=  %(k1)s  incompatible avec type_cham=  %(k2)s 
"""),

25: _("""
 operation=  %(k1)s  seulement type_cham= 'noeu_geom_r' 
"""),

26: _("""
 operation=  %(k1)s  incompatible avec type_cham=  %(k2)s 
"""),

27: _("""
 grandeurs differentes pour : %(k1)s et : %(k2)s 
"""),

28: _("""
 il existe des doublons dans la liste d'instants de rupture
"""),

29: _("""
 il faut donner plus d'un instant de rupture
"""),

30: _("""
 il manque des temperatures  associees aux bases de resultats (mot-cle tempe)
"""),

31: _("""
 le parametre m de weibull doit etre le meme pour toutes les bases resultats !
"""),

32: _("""
 le parametre sigm_refe de weibull doit etre le meme pour toutes les bases resultats !
"""),

33: _("""
 aucun numero d'unite logique n'est associe a  %(k1)s 
"""),

34: _("""
 aucun numero d'unite logique n'est disponible 
"""),

35: _("""
 action inconnue:  %(k1)s 
"""),

36: _("""
 arret de la procedure de recalage : le parametre m est devenu trop petit (m<1) , verifiez vos listes d'instants de rupture
"""),

37: _("""
 les parametres de la nappe ont ete reordonnees.
"""),

38: _("""
 type de fonction non connu (ordonn)
"""),

39: _("""
 points confondus.
"""),

40: _("""
 impossibilite, la maille  %(k1)s  doit etre de type "seg2" ou "seg3"et elle est de type :  %(k2)s 
"""),

41: _("""
 nbma different de nbel
"""),

42: _("""
 le contour n est pas ferme
"""),

43: _("""
 le contour n'est pas ferme
"""),

44: _("""
 nbma different de nbe
"""),

45: _("""
 nj2 different de nj0
"""),

46: _("""
 le groupe de mailles " %(k1)s " n'existe pas.
"""),

47: _("""
 le groupe  %(k1)s  ne contient aucune maille.
"""),

48: _("""
 on ne traite que des problemes 2d.
"""),

49: _("""
 la maille " %(k1)s " n'existe pas.
"""),

50: _("""
 on doit donner un resultat de type "evol_ther" apres le mot-cle "lapl_phi"  du mot-facteur "cara_poutre" dans la commande post_elem pour calculer la constante de torsion.
"""),

51: _("""
 le nombre d'ordres du resultat  %(k1)s  necessaire pour calculer la constante de torsion doit etre egal a 1.
"""),

52: _("""
 on n'arrive pas a recuperer le champ de temperatures du resultat  %(k1)s 
"""),

53: _("""
 la table "cara_geom" n'existe pas.
"""),

54: _("""
 on doit donner un resultat de type "evol_ther" apres le mot-cle "lapl_phi_y"  du mot-facteur "cara_poutre" dans la commande post_elem pour calculer les coefficients de cisaillement et les coordonnees du centre de torsion.
"""),

55: _("""
 on doit donner un resultat de type "evol_ther" apres le mot-cle "lapl_phi_z"  du mot-facteur "cara_poutre" dans la commande post_elem pour calculer les coefficients de cisaillement et les coordonnees du centre de torsion.
"""),

56: _("""
 le nombre d'ordres du resultat  %(k1)s  necessaire pour calculer les coefficients de cisaillement et les coordonnees du centre de torsion doit etre egal a 1.
"""),

57: _("""
 on doit donner un resultat de type "evol_ther" apres le mot-cle "lapl_phi"  du mot-facteur "cara_poutre" dans la commande post_elem pour calculer la constante de gauchissement.
"""),

58: _("""
 le nombre d'ordres du resultat  %(k1)s  necessaire pour calculer la constante de gauchissement doit etre egal a 1.
"""),

59: _("""
 il faut donner le nom d'une table issue d'un premier calcul avec l'option "cara_geom" de  post_elem apres le mot-cle "cara_geom" du mot-facteur "cara_poutre".
"""),

60: _("""
 il faut obligatoirement definir l'option de calcul des caracteristiques de poutre apres le mot-cle "option" du mot-facteur "cara_poutre" de la commande post_elem.
"""),

61: _("""
 l'option  %(k1)s n'est pas admise apres le mot-facteur "cara_poutre".
"""),

62: _("""
 il faut donner le nom d'un resultat de type evol_ther apres le mot-cle lapl_phi du mot-facteur "cara_poutre".
"""),

63: _("""
 il faut donner le nom d'un resultat de type evol_ther apres le mot-cle lapl_phi_y du mot-facteur "cara_poutre".
"""),

64: _("""
 il faut donner le nom d'un resultat de type evol_ther apres le mot-cle lapl_phi_z du mot-facteur "cara_poutre".
"""),

65: _("""
 y a un bug 12
"""),

66: _("""
 y a un bug 13
"""),

67: _("""
 y a un bug 14
"""),

68: _("""
 on attend un concept "mode_meca" ou "evol_elas" ou "evol_ther" ou "dyna_trans" ou "evol_noli"
"""),

69: _("""
 champ de vitesse donne.
"""),

70: _("""
 champ de deplacement donne.
"""),

71: _("""
 option masse coherente.
"""),

72: _("""
 calcul avec masse diagonale
"""),

73: _("""
 type de champ inconnu.
"""),

74: _("""
 le groupe " %(k1)s " ne contient aucune maille.
"""),

75: _("""
 on attend un concept "mode_meca" ou "evol_elas" ou "mult_elas" ou "evol_ther" ou "dyna_trans" ou "evol_noli"
"""),

76: _("""
 pour calculer les indicateurs globaux d'energie, il faut donner un resultat issu de stat_non_line .
"""),

77: _("""
 on attend un resultat de type "evol_noli" .
"""),

78: _("""
 le resultat  %(k1)s  doit comporter la relation de comportement au numero d'ordre  %(k2)s  .
"""),

79: _("""
 le resultat  %(k1)s  doit comporter un champ de variables internes au numero d'ordre  %(k2)s  .
"""),

80: _("""
 impossibilite : le volume du modele traite est nul. 
"""),

81: _("""
 impossibilite : le volume du group_ma  %(k1)s  est nul. 
"""),

82: _("""
 impossibilite : le volume de la maille  %(k1)s  est nul. 
"""),

83: _("""
 erreur: les options de calcul doivent etre identiques pour toutes les occurrences du mot clef facteur
"""),

84: _("""
 on attend un concept "evol_noli"
"""),

85: _("""
 erreur: le champ sief_elga n'existe pas
"""),

86: _("""
 erreur: le champ vari_elga n'existe pas
"""),

87: _("""
 erreur: le champ depl_elno n'existe pas
"""),

88: _("""
 erreur: le champ epsg_elga_depl n'existe pas
"""),

89: _("""
 les 2 nuages : %(k1)s  et  %(k2)s  doivent avoir le meme nombre de coordonnees.
"""),

90: _("""
 les 2 nuages : %(k1)s  et  %(k2)s  doivent avoir la meme grandeur associee.
"""),

91: _("""
 il manque des cmps sur :  %(k1)s 
"""),

92: _("""
 l'interpolation n'est pas encore faite en complexe.
"""),

93: _("""
 seuls les types "reel" et "complexe" sont autorises.
"""),

94: _("""
 est deja memorisee :  %(k1)s 
"""),

95: _("""
 choix=/e/l svp.
"""),

96: _("""
 pour un concept, pas plus de 8 svp.
"""),

97: _("""
 la structure  %(k1)s  apparait plusieurs fois en tant que derivee ?
"""),

98: _("""
 ce parametre de sensibilite est introuvable :  %(k1)s 
"""),

99: _("""
 les types de sensibilite pour l'influence de  %(k1)s  sont incoherents.
"""),
}
