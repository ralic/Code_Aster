#@ MODIF algorith13 Messages  DATE 30/05/2007   AUTEUR COURTOIS M.COURTOIS 
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
 erreur fatale l'entite  %(k1)s  n'est pas possible  %(k2)s 
"""),

2: _("""
 erreur dans les donnees  d'observation le noeud  %(k1)s n'existe pas dans  %(k2)s 
"""),

3: _("""
 erreur dans les donnees d'observation le group_no  %(k1)s n'existe pas dans  %(k2)s 
"""),

4: _("""
 erreur dans les donnees  d'observation la maille  %(k1)s n'existe pas dans  %(k2)s 
"""),

5: _("""
 erreur dans les donnees d'observation le group_ma  %(k1)s n'existe pas dans  %(k2)s 
"""),

6: _("""
 erreur dans les donnees  d'observation pour "nom_cham"  %(k1)s il faut renseigner  %(k2)s ou  %(k3)s 
"""),

7: _("""
 erreur dans les donnees  d'observation pour "nom_cham"  %(k1)s  il faut renseigner  %(k2)s et  %(k3)s 
"""),

8: _("""
 arret sur maillage non skelette
"""),








10: _("""
 probleme de duplication de matrice matrice:  %(k1)s 
"""),

11: _("""
  arret probleme de factorisation: presence de modes de corps rigide
"""),








13: _("""
 icmp dans le desordre pour noeud=  %(i1)d  et sous-domaine=  %(i2)d 
"""),

14: _("""
 systeme (gi)t*gi probablement  non inversible:  %(i1)d pb lapack dgetrf:  %(i2)d 
"""),

15: _("""
 systeme (gi)t*gi probablement  non inversible:  %(i1)d pb lapack dgetrs:  %(i2)d 
"""),








17: _("""
 arret sur probleme base modale sans interf_dyna base modale -->  %(k1)s 
"""),

18: _("""
  gamdev(alpha) < 0    gamdev(alpha) =  %(r1)f 
"""),







26: _("""
 conflit de nom de groupe de  maille dans le squelette le nom de groupe :  %(k1)s 
 provenant de la sous-structure :  %(k2)s 
 et du groupe de maille :  %(k3)s 
 existe deja.  %(k4)s 
"""),

27: _("""
 nom de groupe non trouvele groupe :  %(k1)s n existe pas  %(k2)s dans la sous-structure :  %(k3)s 
"""),

28: _("""
 &aucun axe defini
"""),

29: _("""
 methode non supportee en  sous-structuration   methode demandee :  %(k1)s 
    methodes supportees: %(k2)s 
"""),

30: _("""
 conditions initiales non supportees  en sous-structuration transitoire
"""),

31: _("""
 calcul non lineaire non supporte en  sous-structuration transitoire
"""),

32: _("""
 rela_effo_dep non supporte en  sous-structuration transitoire
"""),

33: _("""
 rela_effo_vite non supporte en  sous-structuration transitoire
"""),

34: _("""
 
 la liste des amortissements modaux est  definie au niveau de l'operateur macr_elem_dyna
"""),

35: _("""
 numero de mode de votre liste inexistant dans les modes utilises:
 numero ds votre liste : %(i1)d 
"""),

36: _("""
 appel errone
"""),





39: _("""
 choc mal definila maille definissant le choc  %(k1)s  doit etre de type  %(k2)s 
"""),





41: _("""
 trop de noeuds dans le group_no  %(k1)s   noeud utilise:  %(k2)s 
"""),






44: _("""
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!incompatibilite avec multi appui : %(k1)s 
"""),






46: _("""
 il manque les modes statiques
"""),

47: _("""
 il manque les modes corriges
"""),

48: _("""
 Nombre de modes propres calcules insuffisant.
 Nombre de modes propres de la base limite a : %(i1)d 
"""),

49: _("""
 &sous-structure inexistante dans le modele-generalise modele generalise -->  %(k1)s 
 sous-structure demandee -->  %(k2)s 
"""),

50: _("""
 &sous-structure inexistante dans le modele-generalise modele generalise -->  %(k1)s 
 numero sous-structure demandee -->  %(i1)d 
"""),





53: _("""
  champ inexistant  mesure  %(k1)s  nom_cham  %(k2)s 
"""),




56: _("""
 pb.interpolation volumique 3d:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

57: _("""
 pb.interpolation volumique 2d:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

58: _("""
 pb.charge vol2d puis surf3d:evol_char: %(k1)s instant  : %(r1)f 
"""),

59: _("""
 pb.interpolation surfacique 3d:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

60: _("""
 pb.charge vol3d puis surf2d:evol_char: %(k1)s instant  : %(r1)f 
"""),

61: _("""
 pb.interpolation surfacique 2d:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

62: _("""
 pb. interpolation pression:evol_char  : %(k1)s instant    : %(r1)f 
 on ne sait pas extrapoler le champ  %(k2)s 
 de pression par rapport au temps %(k3)s 
 mais seulement l'interpoler %(k4)s 
"""),

63: _("""
 pb. interpolation pression:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
 contacter les developpeurs %(k2)s 
"""),

64: _("""
 interpolation temperature:evol_ther: %(k1)s instant: %(r1)f icoret: %(i1)d 
"""),






66: _("""
 Force fluide, grappe bloquee   
  - iteration  %(i1)d , z =  %(r1)f    iteration  %(i2)d  , z =  %(r2)f 
    temps de chute compris entre  %(r3)f et  %(r4)f 
"""),



68: _("""
 pb.interpolation vitesse:evol_char  : %(k1)s instant    : %(r1)f 
 code_retour: %(i1)d 
"""),

69: _("""
 le noeud: nomnoe ne peut pas etre  typl  et  typbnomnoe %(k1)s 
"""),

70: _("""
 impossible de coder le nombre :  %(i1)d  sur :  %(k1)s 
"""),

71: _("""
 choix impossiblepour initpr :  %(i1)d 
"""),






74: _("""
 composante non definie  dans la numerotation :  %(k1)s 
"""),

75: _("""
 & detection d'une sous-structure non connectesous-structure de nom: %(k1)s 
"""),

76: _("""
 & arret sur probleme de connexion sous-structure
"""),




78: _("""
 les intervalles doivent etre croissants.   valeur de la borne precedente :  %(i1)d 
    valeur de la borne :  %(i2)d 
"""),

79: _("""
 l'intervalle entre les  deux derniers instants ne sera pas egal
  au pas courant :  %(i1)d 
 , pour l'intervalle  %(i2)d 
"""),

80: _("""
 le nombre de pas est trop grand :  %(i1)d , pour l'intervalle  %(i2)d 
"""),

81: _("""
 les valeurs doivent etre croissantes.  valeur precedente :  %(i1)d 
   valeur :  %(i2)d 
"""),

82: _("""
 la distance entre les  deux derniers reels ne sera pas egal
 au pas courant :  %(r1)f,
 pour l'intervalle  %(i1)d 
"""),




84: _("""
 modele amont non defini
"""),

85: _("""
 champ inexistant resultat  %(k1)s  nom_cham  %(k2)s  nume_ordre  %(i1)d 
"""),

86: _("""
 type de matrice inconnuetype: %(k1)s 
"""),






91: _("""
 &probleme de dimension matrice a mutiplier
"""),

92: _("""
 &probleme de dimension matrice resultat
"""),






99: _("""
 matrice d'amortissement non creeedans le macro-element :  %(k1)s 
"""),

}
