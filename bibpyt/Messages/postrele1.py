#@ MODIF postrele1 Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 

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
 probleme donnees pour l''occurrence  %(i1)d  dans le resultat  %(k1)s 
  pour le nom_cham  %(k2)s 
"""),

2: _("""
 erreur donnees pour la situation numero  %(i1)d 
 on n''a pas pu recuperer le "resu_ther"  correspondant au numero  %(i2)d 
"""),

3: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il y a plusieurs resu_ther %(i3)d 
"""),

4: _("""
 erreur donneessituation numero  %(i1)d maille numero  %(i2)d 
 noeud numero  %(i3)d 
 plusieurs resu_ther %(i4)d 
"""),

5: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 aucun resu_ther %(i3)d 
"""),

6: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

7: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

8: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

9: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

10: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

11: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

12: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

13: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

14: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

15: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

16: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

17: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

18: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

19: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

20: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s  les  %(k3)s 
"""),

21: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

22: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

23: _("""
 probleme pour recuperer dans la table  %(k1)s  intitule  %(k2)s 
  la contrainte  %(k3)s 
  pour l''absc_curv  %(r1)f 
"""),

24: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

25: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

26: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

27: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

28: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

29: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

30: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

31: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

32: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

33: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

34: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque le  %(k1)s 
"""),

35: _("""
 erreur donnees pour la maille  %(i1)d  et le noeud  %(i2)d il manque un  %(k1)s 
"""),

36: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque le resu_ther %(i3)d 
"""),

37: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque resu_ther_moye %(i3)d 
"""),

38: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
"""),

39: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
"""),

40: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
 occurrence  %(i1)d 
"""),

41: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque le resu_ther %(i3)d 
"""),

42: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque resu_ther_moye %(i3)d 
"""),

43: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque le resu_ther %(i3)d 
"""),

44: _("""
 erreur donnees pour la situation numero  %(i1)d sur la maille numero  %(i2)d 
 il manque resu_ther_moye %(i3)d 
"""),

45: _("""
 probleme pour recuperer  dans la table  %(k1)s 
  la temperature pour l''instant  %(r1)f 
  pour l''absc_curv  %(r2)f 
"""),

46: _("""
 probleme pour recuperer  dans la table  %(k1)s 
  la temperature pour l''instant  %(r1)f 
  pour l''absc_curv  %(r2)f 
"""),

47: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
"""),

48: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
"""),

49: _("""
 probleme pour recuperer  dans la table  %(k1)s  la quantite  %(k2)s 
  pour l''instant  %(r1)f 
 occurrence  %(i1)d 
"""),

50: _("""
 changement de reperechamp non traite  %(k1)s option de calcul  %(k2)s 
"""),

51: _("""
 changement de reperechamp non traite  %(k1)s option de calcul  %(k2)s 
"""),

52: _("""
 noeud sur l''axe_z maille :  %(k1)s   noeud :  %(k2)s 
   coordonnees :  %(r1)f 
"""),

53: _("""
 noeud sur l''axe_z maille :  %(k1)s   noeud :  %(k2)s 
   coordonnees :  %(r1)f 
"""),

54: _("""
 les noeuds du maillage  ne sont pas tous dans un meme plan z = cst
 changement de repere non traite %(k1)s 
"""),

55: _("""
 on ne sait pas faire ce post-traitement pour le chemin %(k1)s  en repere  %(k2)s 
"""),

56: _("""
 les noeuds de post-traitement  ne sont pas tous dans un meme plan z = cst
 changement de repere non traite %(k1)s 
"""),

57: _("""
 noeud confondu avec l''origine noeud :  %(k1)s 
"""),

58: _("""
 noeud sur l''axe_z noeud :  %(k1)s 
"""),

59: _("""
 les noeuds du maillage  ne sont pas tous dans un meme plan z = cst
 option trac_nor non traite %(k1)s 
 utiliser l''option  %(k2)s 
"""),

60: _("""
 option non traitee :  %(k1)s post-traitement  %(i1)d 
 les invariants tensoriels ne sont  calcules que pour les options :  %(k2)s 
        %(k3)s 
        %(k4)s 
        %(k5)s 
        %(k6)s 
        %(k7)s 
        %(k8)s 
        %(k9)s 
        %(k10)s 
        %(k11)s 
        %(k12)s 
        %(k13)s 
        %(k14)s 
        %(k15)s 
        %(k16)s 
        %(k17)s 
        %(k18)s 
        %(k19)s 
        %(k20)s 
"""),

61: _("""
 option non traitee :  %(k1)s post-traitement :  %(i1)d 
 les traces normales sont calculees  pour les options :  %(k2)s 
        %(k3)s 
        %(k4)s 
        %(k5)s 
        %(k6)s 
        %(k7)s 
        %(k8)s 
        %(k9)s 
        %(k10)s 
        %(k11)s 
        %(k12)s 
        %(k13)s 
        %(k14)s 
        %(k15)s 
        %(k16)s 
        %(k17)s 
        %(k18)s 
 ou pour les grandeurs %(k19)s 
        %(k20)s 
"""),

62: _("""
 option non traitee :  %(k1)s post-traitement  %(i1)d 
 les traces directionnelles sont  calculees pour les options :  %(k2)s 
        %(k3)s 
        %(k4)s 
        %(k5)s 
        %(k6)s 
        %(k7)s 
        %(k8)s 
        %(k9)s 
        %(k10)s 
        %(k11)s 
        %(k12)s 
        %(k13)s 
        %(k14)s 
        %(k15)s 
        %(k16)s 
        %(k17)s 
        %(k18)s 
        %(k19)s 
        %(k20)s 
 ou pour les grandeurs %(k21)s 
        %(k22)s 
        %(k23)s 
"""),

63: _("""
 trace directionnellepost-traitement  %(i1)d direction nulle  %(k1)s 
"""),

64: _("""
 attentionpost-traitement  %(i1)d 
 seules les composantes du tenseur  des contraintes sont traitees %(k1)s 
"""),

65: _("""
 cmp non traitee  dans un changement de reperepost-traitement  %(i1)d 
"""),

66: _("""
 cmp non traitee dans un  changement de reperepost-traitement  %(i1)d 
"""),

67: _("""
 cmp non traitee dans un  changement de reperepost-traitement  %(i1)d 
"""),

68: _("""
 grandeur non traitee dans  un changement de reperepost-traitement  %(i1)d 
 les changements de reperes sont  possibles pour les grandeurs :  %(k1)s 
      %(k2)s 
 option :  %(k3)s 
      %(k4)s 
 option :  %(k5)s 
                       %(k6)s 
 ou pour les grandeurs %(k7)s 
      %(k8)s 
      %(k9)s 
"""),

69: _("""
 noeud non  connecte a une maillenoeud numero :  %(i1)d de nom :  %(k1)s 
"""),

70: _("""
 noeud non  connecte a une maillenoeud numero :  %(i1)d de nom :  %(k1)s 
"""),

71: _("""
 champ inexistant nom_cham=  %(k1)s  nume_ordre=  %(i1)d 
"""),

72: _("""
 champ inexistant nom_cham=  %(k1)s  nume_ordre=  %(i1)d 
"""),

73: _("""
 champ inexistant nom_cham=  %(k1)s  nume_ordre=  %(i1)d 
"""),

74: _("""
 champ inexistant nom_cham=  %(k1)s  nume_ordre=  %(i1)d 
"""),

75: _("""
 mots cles resultante sommeoccurence de "action" numero :  %(i1)d 
 les listes arguments des mots cles  resultante et moment doivent etre de  meme longueur %(k1)s 
 cette longueur est :  %(i2)d 
 ou  %(i3)d 
"""),

76: _("""
 mots cles resultante sommeoccurence de "action" numero :  %(i1)d 
 la liste arguments du mot cle  point doit etre de longueur  %(i2)d 
 ou  %(i3)d 
"""),

77: _("""
 acces incoherent"action" occurence :  %(i1)d 
 le concept argument de  nom :  %(k1)s 
 et de type :  %(k2)s 
 ne peut pas etre  acceder ni par  %(k3)s 
  ni par  %(k4)s 
"""),

78: _("""
 acces incoherent"action" occurence :  %(i1)d 
 le concept argument de  nom :  %(k1)s 
 et de type :  %(k2)s 
 ne peut pas etre  acceder par  %(k3)s 
"""),

79: _("""
        Plusieurs seismes dans le meme groupe
        Groupe numero %(i1)d 
        Occurence situation %(i2)d  et  %(i3)d 
 
"""),

}
