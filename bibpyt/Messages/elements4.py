#@ MODIF elements4 Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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
  erreur dans le calcul de pres_f 
"""),

2 : _("""
 pour l'option "indic_ener", les seules relations admises sont "vmis_isot_line" et "vmis_isot_trac" .
"""),

3 : _("""
 pour l'option "indic_seuil", les seules relations admises sont "vmis_isot_line", "vmis_isot_trac"  et "vmis_cine_line" .
"""),

6 : _("""
 le type du champ de contraintes est incompatible avec l'option :  %(k1)s 
"""),

7 : _("""
 pas de contraintes dans pcontgp
"""),

8 : _("""
 pas de champ endo_elga dans ptriagp
"""),

9 : _("""
 pas de champ vari_elga dans pvarimr
"""),

10 : _("""
 pas de champ vari_elga dans pvaripr
"""),

11 : _("""
 option non traitée  %(k1)s
 """),

12 : _("""
 nombre total de sous-éléments limité a 32.
"""),

13 : _("""
 nombre total de points d'intersection limité a 11.
"""),

15 : _("""
  deformation :  %(k1)s non implantée sur les éléments "pou_d_tgm" : utiliser PETIT ou REAC_GEOM
"""),

16 : _("""
 option "vari_elno_elga" impossible actuellement
"""),

17 : _("""
 seuls comportements autorises :"elas" et "vmis_isot_trac"
"""),

18 : _("""
 pb de statut de contact
"""),

19 : _("""
 lsn non nul sur la surface.
"""),

20 : _("""
 pour l'élément de poutre " %(k1)s " l'option " %(k2)s " est invalide
"""),

21 : _("""
 pour un élément de poutre noeuds confondus :  %(k1)s 
"""),

22 : _("""
 les poutres à section variable ne sont pas traitées.
"""),

23 : _("""
 comp_incr non disponible pour les elements enrichis avec x-fem.
"""),

30 : _("""
 option refe_forc_noda plus tard...
"""),

31 : _("""
 dfdi mal dimensionnée
"""),

32 : _("""
 vous utilisez le mot clé liaison_elem avec l'option coq_pou: l'épaisseur des éléments de bord de coque n'a pas été affectée.
"""),

33 : _("""
 l'epaisseur des éléments de bord de coque est negative ou nulle.
"""),

34 : _("""
 le jacobien est nul.
"""),

35 : _("""
 matns() sous-dimensionné
"""),

36 : _("""
 pr() sous-dimensionne
"""),

37 : _("""
 nive_couche ne peut etre que "moy"
"""),

38 : _("""
 option  %(k1)s  non active pour un élément de type  %(k2)s 
"""),

39 : _("""
 option  %(k1)s  : incompatibilité des deux champs d entrée
"""),

40 : _("""
 le nombre de ddl est trop grand
"""),

41 : _("""
 le nombre de ddl est faux
"""),

42 : _("""
 nom de type élément inattendu
"""),

43 : _("""
 comp. elastique inexistant
"""),

44 : _("""
 l'option " %(k1)s " est interdite pour les tuyaux
"""),

45 : _("""
 l'option " %(k1)s " en repère local est interdite pour les tuyaux : utiliser le repère global
"""),

46 : _("""
 le nombre de couches et de secteurs doivent etre supérieurs a 0
"""),

47 : _("""
 composante  %(k1)s  non traitée, on abandonne
"""),

48 : _("""
 champ  %(k1)s  non traité, on abandonne
"""),

49 : _("""
 l'option " %(k1)s " est non prévue
"""),

51 : _("""
  nume_sect incorrect
"""),

52 : _("""
 mauvaise option
"""),

53 : _("""
 ep/r > 0.2 modi_metrique pas adapté
"""),

54 : _("""
 ep/r > 0.2 modi_metrique=non pas adapté
"""),

55 : _("""
 alpha est pris egal a 0
"""),

56 : _("""
 famille inexistante  %(k1)s 
"""),

57 : _("""
 indn = 1 (intégration normale) ou indn = 0 (integration réduite) obligatoirement.
"""),

58 : _("""
  le code " %(k1)s " est non prévu. code doit etre = "gl" ou "lg"
"""),

59 : _("""
 nom d'élément fini incompatible
"""),

60 : _("""
 pb calcul des derivées des fonctions singulières sur le fond de fissure
"""),

61 : _("""
 préconditions non remplies
"""),

62 : _("""
  erreur: élément non 2d
"""),

63 : _("""
  l'option %(k1)s n'est pas disponible pour le comportement %(k2)s 
"""),

64 : _("""
  Il est impossible de calculer la normale au noeud %(k1)s de la maille %(k2)s.
  Des aretes doivent etre confondues.
"""),

65 : _("""
  Comportement inattendu : %(k1)s.
"""),

66 : _("""
  Il est impossible de calculer la contrainte d'arc.
  La normale à l'élément et le vecteur obtenu à partir du mot-clé ANGL_REP sont colinéaires.
"""),

68 : _("""
 Nombre d'itérations internes insuffisant.
"""),

69 : _("""
 ! pb récuperation donnée matériau dans thm_liqu %(k1)s !
"""),

72 : _("""
   rcvala ne trouve pas nu, qui est nécessaire pour l'élément MECA_HEXS8  
"""),

73 : _("""
   élément MECA_HEXS8:COMP_ELAS non implanté, utiliser COMP_INCR RELATION='ELAS'  
"""),

74 : _("""
  Attention l'élément MECA_HEXS8 ne fonctionne correctement que sur les parallélépipèdes.
  Sur les elements quelconques on peut obtenir des résultats faux.   
"""),

75 : _("""
 la maille de numero:  %(i1)d appartient à plusieurs sous-domaines! %(i2)d 
"""),

76 : _("""
 la maille du modèle de numéro:  %(i1)d n appartient à aucun sous-domaine ! %(i2)d 
"""),

77 : _("""
 numero de couche  %(i1)d 
  trop grand par rapport au nombre de couches autorisé pour la maille  %(k1)s 
"""),

78 : _("""
 ! pb recuperation donnée matériau dans thm_diffu %(k1)s !
"""),

79 : _("""
 la loi de comportement n'existe pas pour la modélisation dktg :  %(k1)s 
"""),

80 : _("""
 
 attention : l élément de plaque quad4 défini sur la maille : %(k1)s n est pas plan et peut conduire a des résultats faux. 
  distance au plan :  %(r1)f 
"""),

81 : _("""
 manque le paramètre  %(k1)s pour la maille  %(k2)s 
"""),

83 : _("""
 utiliser "stat_non_line"  température inf:  %(r1)f   température moy:  %(r2)f 
 température sup:  %(r3)f 
"""),

84 : _("""
 famille non disponible élément de référence  %(k1)s 
 famille  %(k2)s 
"""),

88 : _("""
 elrefe non disponible élément de référence  %(k1)s 
"""),

90 : _("""
 elrefe mal programme maille  %(k1)s  type  %(k2)s  nb noeuds  %(i1)d 
 nb noeuds pour le gano  %(i2)d 
"""),

91 : _("""
 ! le calcul de cet estimateur !! ne tient pas compte d'éventuelles ! %(i1)d 
 ! conditions limites non linéaires   ! %(i2)d 
"""),

92 : _("""
 la pression doit etre nulle pour la maille  %(k1)s 
"""),

98 : _("""
 la contrainte equivalente est nulle pour la maille  %(k1)s 
"""),

}
