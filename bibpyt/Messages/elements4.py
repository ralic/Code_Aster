#@ MODIF elements4 Messages  DATE 23/04/2007   AUTEUR GNICOLAS G.NICOLAS 
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
  erreur dans le calcul de pres_f 
"""),

2: _("""
 pour l'option "indic_ener", les seules relations admises sont "vmis_isot_line" et "vmis_isot_trac" .
"""),

3: _("""
 pour l'option "indic_seuil", les seules relations admises sont "vmis_isot_line", "vmis_isot_trac"  et "vmis_cine_line" .
"""),

4: _("""
 ! pb rcvala biot_coef  %(k1)s 
"""),

5: _("""
 le vecteur tau1 (direction1 du frottement) est nul. les gradients des level sets sont surement colineaires en ce point.
"""),

6: _("""
 le type du champ de contraintes est incompatible avec l'option :  %(k1)s 
"""),

7: _("""
 pas de contraintes dans pcontgp
"""),

8: _("""
 pas de champ endo_elga dans ptriagp
"""),

9: _("""
 pas de champ vari_elga dans pvarimr
"""),

10: _("""
 pas de champ vari_elga dans pvaripr
"""),

11: _("""
 option non traitee  %(k1)s 
"""),

12: _("""
 nombre total de sous-elements limite a 32.
"""),

13: _("""
 nombre total de points d'intersection limite a 11.
"""),

14: _("""
 type de maille non valide pour le post-traitement des elements xfem
"""),

15: _("""
  deformation :  %(k1)s non implantee sur les elements "pou_d_tgm" : utiliser petit ou reac_geom
"""),

16: _("""
 option "vari_elno_elga" impossible actuellement
"""),

17: _("""
 seuls comportements autorises :"elas" et "vmis_isot_trac"
"""),

18: _("""
 pb de statut de contact
"""),

19: _("""
 lsn non nul sur la surface.
"""),

20: _("""
 pour l'element de poutre " %(k1)s " l'option " %(k2)s " est invalide
"""),

21: _("""
 pour un element de poutre noeuds confondus :  %(k1)s 
"""),

22: _("""
 les poutres a section variable ne sont pas traitees.
"""),

23: _("""
 comp_incr non disponible pour les elements enrichis avec x-fem.
"""),

30: _("""
 option refe_forc_noda plus tard...
"""),

31: _("""
 dfdi mal dimensionnee
"""),

32: _("""
 vous utilisez le mot cle liaison_elem avec l'option coq_pou: l'epaisseur des elements de bord de coque n'a pas ete affectee.
"""),

33: _("""
 l'epaisseur des elements de bord de coque est negative ou nulle.
"""),

34: _("""
 le jacobien est nul.
"""),

35: _("""
 matns() sous-dimensionne
"""),

36: _("""
 pr() sous-dimensionne
"""),

37: _("""
 nive_couche ne peut etre que "moy"
"""),

38: _("""
 option  %(k1)s  non active pour un element de type  %(k2)s 
"""),

39: _("""
 option  %(k1)s  : incompatibilite des deux champs d entree
"""),

40: _("""
 le nombre de ddl est trop grand
"""),

41: _("""
 le nombre de ddl est faux
"""),

42: _("""
 nom de type element inattendu
"""),

43: _("""
 comp. elastique inexistant
"""),

44: _("""
 l'option " %(k1)s " est interdite pour les tuyaux
"""),

45: _("""
 l'option " %(k1)s " en repere local est interdite pour les tuyaux utiliser le repere global
"""),

46: _("""
 le nombre de couches et de secteurs doivent etre superieurs a 0
"""),

47: _("""
 cmp  %(k1)s  non traitee, on abandonne
"""),

48: _("""
 champ  %(k1)s  non traite, on abandonne
"""),

49: _("""
 l'option " %(k1)s " est non prevue
"""),

50: _("""
  angle compris entre 0 et deuxpi
"""),

51: _("""
  nume_sect incorrect
"""),

52: _("""
 mauvaise option
"""),

53: _("""
 ep/r > 0.2 modi_metrique pas adapte
"""),

54: _("""
 ep/r > 0.2 modi_metrique=non pas adapte.
"""),

55: _("""
 alpha est pris egal a 0
"""),

56: _("""
 famille inexistante  %(k1)s 
"""),

57: _("""
 indn = 1 (integration normale) ou                                 indn = 0 (integration reduite) obligatoirement.
"""),

58: _("""
  le code " %(k1)s "                         est non prevue. code doit etre = "gl" ou "lg"
"""),

59: _("""
 nom d'element fini incompatible
"""),

60: _("""
 pb calcul des derivees des fonctions singulieres sur le fond de fissure
"""),

61: _("""
 preconditions non remplies
"""),

62: _("""
  erreur: element non 2d
"""),

63: _("""
  l'option %(k1)s n'est pas disponible pour le comportement %(k2)s 
"""),

64: _("""
  Il est impossible de calculer la normale au noeud %(k1)s de la maille %(k2)s.
  Des aretes doivent etre confondues.
"""),

65: _("""
  Comportement inattendu : %(k1)s.
"""),

66: _("""
  Il est impossible de calculer la contrainte d'arc.
  La normale à l'élément et le vecteur obtenu à partir du mot-clé ANGL_REP sont colinéaires.
"""),

67: _("""
 ! le module de young est nul !
"""),


68: _("""
 Nombre d'itérations internes insuffisant.
"""),

69: _("""
 ! pb recuperation donnee materiau dans thm_liqu %(k1)s !
"""),

70: _("""
 ! pb recuperation donnee materiau dans thm_init %(k1)s !
"""),

71: _("""
 ! pb recuperation donnees materiau dans elas %(k1)s !
"""),


72: _("""
   rcvala ne trouve pas nu, qui est nécessaire pour l'élément MECA_HEXS8  
"""),

73: _("""
   élément MECA_HEXS8:COMP_ELAS non implanté, utiliser COMP_INCR RELATION='ELAS'  
"""),

74: _("""
   attention l'élément MECA_HEXS8 ne fonctionne correctement que sur les parallélipipèdes.
  Sur les elements quelconques on peut obtenir des resultats faux.   
"""),

75: _("""
 la maille de numero:  %(i1)d appartient a plusieurs sous-domaines! %(i2)d 
"""),

76: _("""
 la maille du modele de numero:  %(i1)d appartient a aucun sous-domaine ! %(i2)d 
"""),

77: _("""
 numero de couche  %(i1)d 
  trop grand par rapport au nombre  de couches autorise pour la maille  %(k1)s 
"""),

78: _("""
 ! pb recuperation donnee materiau dans thm_diffu %(k1)s !
"""),






79: _("""
 la loi de comportement n'existe pas pour la  modelisation dktg :  %(k1)s 
"""),

80: _("""
 
 attention : l element de plaque  quad4 defini sur la maille : zk24(iazk24+2) n est pas plan et peut conduire a des resultats faux. 
  distance au plan :  %(r1)f 
"""),

81: _("""
 manque le parametre  %(k1)s pour la maille  %(k2)s 
"""),








83: _("""
 utiliser "stat_non_line"  temperature inf:  %(r1)f   temperature moy:  %(r2)f 
   temperature sup:  %(r3)f 
"""),

84: _("""
 famille non disponible    element de reference  %(k1)s 
                 famille  %(k2)s 
"""),


















88: _("""
 elrefe non disponible    element de reference  %(k1)s 
"""),








90: _("""
 elrefe mal programme maille  %(k1)s  type  %(k2)s  nb noeuds  %(i1)d 
  nb noeuds pour le gano  %(i2)d 
"""),

91: _("""
 ! le calcul de cet estimateur !! ne tient pas compte d'eventuelles ! %(i1)d 
 ! conditions limites non lineaires   ! %(i2)d 
"""),

92: _("""
 la pression doit etre nulle pour la maille  %(k1)s 
"""),


































98: _("""
 la contrainte equivalente est nulle pour la maille  %(k1)s 
"""),






}
