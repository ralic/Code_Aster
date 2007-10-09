#@ MODIF algorith13 Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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
 l'entité  %(k1)s  n'est pas possible  %(k2)s 
"""),

2 : _("""
 erreur dans les données  d'observation
 le noeud  %(k1)s n'existe pas dans  %(k2)s 
"""),

3 : _("""
 erreur dans les données d'observation
 le GROUP_NO  %(k1)s n'existe pas dans  %(k2)s 
"""),

4 : _("""
 erreur dans les données d'observation
 la maille  %(k1)s n'existe pas dans  %(k2)s 
"""),

5 : _("""
 erreur dans les données d'observation
 le GROUP_MA  %(k1)s n'existe pas dans  %(k2)s 
"""),

6 : _("""
 erreur dans les données d'observation
 pour "NOM_CHAM"  %(k1)s , il faut renseigner  %(k2)s ou  %(k3)s 
"""),

7 : _("""
 erreur dans les données d'observation
 pour "NOM_CHAM"  %(k1)s , il faut renseigner  %(k2)s et  %(k3)s 
"""),

8 : _("""
 arret sur maillage non squelette
"""),

10 : _("""
 problème de duplication de matrice :  %(k1)s 
"""),

11 : _("""
  arret problème de factorisation
  présence de modes de corps rigide
"""),

13 : _("""
 ICMP dans le désordre
 noeud        =  %(i1)d
 sous-domaine =  %(i2)d 
"""),

14 : _("""
 système (GI)T*GI probablement non inversible:
 %(i1)d pb LAPACK DGETRF:  %(i2)d 
"""),

15 : _("""
 système (GI)T*GI probablement non inversible:
 %(i1)d pb LAPACK DGETRS:  %(i2)d 
"""),

17 : _("""
 arret sur problème base modale sans INTERF_DYNA
 base modale -->  %(k1)s 
"""),

18 : _("""
  GAMDEV(ALPHA) < 0
  GAMDEV(ALPHA) =  %(r1)f 
"""),

26 : _("""
 conflit de nom de groupe de maille dans le squelette
 le nom de groupe               :  %(k1)s 
 provenant de la sous-structure :  %(k2)s 
 et du groupe de maille         :  %(k3)s 
 existe déjà.
 %(k4)s 
"""),

27 : _("""
 nom de groupe non trouvé
 le groupe :  %(k1)s n'existe pas  %(k2)s dans la sous-structure :  %(k3)s 
"""),

28 : _("""
 aucun axe défini
"""),

29 : _("""
 méthode non supportée en  sous-structuration
 méthode demandée   :  %(k1)s 
 méthodes supportées:  %(k2)s 
"""),

30 : _("""
 conditions initiales non supportées en sous-structuration transitoire
"""),

31 : _("""
 calcul non linéaire non supporté en sous-structuration transitoire
"""),

32 : _("""
 RELA_EFFO_DEP non supporté en sous-structuration transitoire
"""),

33 : _("""
 RELA_EFFO_VITE non supporté en sous-structuration transitoire
"""),

34 : _("""
 la liste des amortissements modaux est définie au niveau de l'operateur MACR_ELEM_DYNA
"""),

35 : _("""
 numéro de mode de votre liste inexistant dans les modes utilisés:
 numéro ds votre liste : %(i1)d 
"""),

36 : _("""
 appel erroné
"""),

39 : _("""
 choc mal defini
 la maille définissant le choc  %(k1)s doit etre de type  %(k2)s 
"""),

41 : _("""
 trop de noeuds dans le GROUP_NO  %(k1)s
 noeud utilisé:  %(k2)s 
"""),

44 : _("""
 incompatibilité avec multi APPUI : %(k1)s 
"""),

46 : _("""
 il manque les modes statiques
"""),

47 : _("""
 il manque les modes corrigés
"""),

48 : _("""
 Nombre de modes propres calcules insuffisant.
 Nombre de modes propres de la base limite à : %(i1)d 
"""),

49 : _("""
 sous-structure inexistante dans le modèle généralisé
 modèle généralisé       -->  %(k1)s 
 sous-structure demandée -->  %(k2)s 
"""),

50 : _("""
 sous-structure inexistante dans le modèle-généralisé
 modèle généralisé              -->  %(k1)s 
 numéro sous-structure demandée -->  %(i1)d 
"""),

53 : _("""
  champ inexistant
  mesure    %(k1)s
  nom_cham  %(k2)s 
"""),

56 : _("""
 problème interpolation volumique 3d:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

57 : _("""
 problème interpolation volumique 2d:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

58 : _("""
 problème charge vol2d puis surf3d:
 evol_char: %(k1)s
 instant  : %(r1)f 
"""),

59 : _("""
 problème interpolation surfacique 3d:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

60 : _("""
 problème charge vol3d puis surf2d:
 evol_char: %(k1)s
 instant  : %(r1)f 
"""),

61 : _("""
 problème interpolation surfacique 2d:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

62 : _("""
 problème interpolation pression:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 on ne sait pas extrapoler le champ  %(k2)s 
 de pression par rapport au temps %(k3)s 
 mais seulement l'interpoler %(k4)s 
"""),

63 : _("""
 problème interpolation pression:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
 contacter le support %(k2)s 
"""),

64 : _("""
 interpolation température:
 evol_ther: %(k1)s
 instant  : %(r1)f
 icoret   : %(i1)d 
"""),

68 : _("""
 problème interpolation vitesse:
 evol_char  : %(k1)s
 instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

69 : _("""
 le noeud: %(k1)s  ne peut pas etre TYPL et TYPB
"""),

70 : _("""
 impossible de coder le nombre :  %(i1)d  sur :  %(k1)s 
"""),

71 : _("""
 choix impossible pour INITPR :  %(i1)d 
"""),

74 : _("""
 composante non définie  dans la numérotation :  %(k1)s 
"""),

75 : _("""
 détection d'une sous-structure non connectée
 sous-structure de nom: %(k1)s 
"""),

76 : _("""
 arret sur problème de connexion sous-structure
"""),

78 : _("""
 les intervalles doivent etre croissants
 valeur de la borne precedente :  %(i1)d 
 valeur de la borne            :  %(i2)d 
"""),

79 : _("""
 l'intervalle entre les  deux derniers instants ne sera pas égal au pas courant :  %(i1)d 
 pour l'intervalle  %(i2)d 
"""),

80 : _("""
 le nombre de pas est trop grand :  %(i1)d , pour l'intervalle  %(i2)d 
"""),

81 : _("""
 les valeurs doivent etre croissantes
 valeur précédente :  %(i1)d 
 valeur            :  %(i2)d 
"""),

82 : _("""
 la distance entre les deux derniers réels ne sera pas égale
 au pas courant :  %(r1)f,
 pour l'intervalle  %(i1)d 
"""),

84 : _("""
 modèle amont non défini
"""),

85 : _("""
 champ inexistant
 résultat   : %(k1)s 
 nom_cham   : %(k2)s 
 nume_ordre : %(i1)d 
"""),

86 : _("""
 type de matrice inconnue
 type: %(k1)s 
"""),

91 : _("""
 problème de dimension de la matrice à mutiplier
"""),

92 : _("""
 problème de dimension de la matrice résultat
"""),

99 : _("""
 matrice d'amortissement non créée dans le macro-élément :  %(k1)s 
"""),

}
