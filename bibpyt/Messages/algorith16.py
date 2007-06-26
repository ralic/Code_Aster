#@ MODIF algorith16 Messages  DATE 25/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
  ! nombre de pas de calcul       :  %(i1)d 
 ! nombre d''iterations          :  %(i2)d 
 ----------------------------------------------
 
"""),

2: _("""
  infos noeuds de choc, lieu de choc :  %(i1)d  noeud de choc  : %(k1)s 
"""),

3: _("""
 sous-structure : %(k1)s 
"""),

4: _("""
 coordonnees    : x :  %(r1)f
                  y :  %(r2)f 
                  z :  %(r3)f 
"""),

5: _("""
 noeud de choc  : %(k1)s 
"""),

6: _("""
 sous-structure : %(k1)s 
"""),

7: _("""
 coordonnees    : x : %(r1)f
                  y : %(r2)f 
                  z : %(r3)f 
"""),

8: _("""
 amortissement tangent utilise :  %(r1)f 

 origine choc x : %(r2)f 
              y : %(r3)f 
              z : %(r4)f 

 norm_obst sin(alpha) : %(r5)f 
           cos(alpha) : %(r6)f 
           sin(beta)  : %(r7)f 
           cos(beta)  : %(r8)f 

 angl_vrille : sin(gamma) : %(r9)f 
               cos(gamma) : %(r10)f 
"""),

9: _("""
 jeu initial :  %(r1)f 
"""),

10: _("""
 
"""),

11: _("""
  ! le nb max d''iterations  %(i1)d ! est atteint sans converger 
 ! le residu relatif final est  : %(r1)f 
 
"""),

12: _("""
 le nombre d''amortissements reduits est trop grand
 le nombre de modes retenus vaut  %(i1)d 
 et le nombre de coefficients :  %(i2)d 
 on ne garde donc que les  %(i3)d 
   %(k1)s 
 
"""),

13: _("""
 le nombre d''amortissements reduits est insuffisantil en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d 
 on rajoute  %(i3)d 
   %(k1)s 
   %(k2)s 
 
"""),

14: _("""
  pas de temps utilisateur trop grand : %(r1)f 
 pas de temps necessaire pour le calcul: %(r2)f 
 risques de problemes de precision %(k1)s 
 
"""),

15: _("""
  pas de temps utilisateur trop grand : %(r1)f 
 pas de temps necessaire pour le calcul: %(r2)f 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %(k1)s 
 parametres de calcul dans ce cas :    %(k2)s 
 nb de pas de calcul :  %(i1)d 
 
"""),

16: _("""
  pas de temps utilisateur trop grand : %(r1)f 
 pas de temps necessaire pour le calcul: %(r2)f 
"""),

17: _("""
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %(k1)s 
 parametres de calcul dans ce cas :    %(k2)s 
 nb de pas de calcul :  %(i1)d 
 
"""),

18: _("""
 le nombre d''amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d 
 et le nombre de coefficients :  %(i2)d 
 on ne garde donc que les  %(i3)d 
   %(k1)s 
 
"""),

19: _("""
 le nombre d''amortissements reduits est insuffisantil en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d 
 on rajoute  %(i3)d 
   %(k1)s 
   %(k2)s 
 
"""),

20: _("""
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!mode dynamique           :  %(i1)d 
 amortissement trop grand :  %(r1)f 
 amortissement critique   :  %(r2)f 
 problemes de convergence possibles %(k1)s 
 
"""),

21: _("""
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! taux de souplesse negligee : %(r1)f 
"""),

22: _("""
         calcul par superposition modale
 ----------------------------------------------
 ! la base de projection est un %(k1)s 
 ! le nb d''equations est          : %(i1)d 
 ! la methode utilisee est        : %(k2)s 
 ! la base utilisee est           : %(k3)s 
 ! le nb de vecteurs de base est  :  %(i2)d 
"""),

23: _("""
 ! le pas de temps initial est  : %(r1)f 
 ! le nb de pas d''archive est     :  %(i1)d 
"""),

24: _("""
 ! nume_vite_flui                 :  %(i1)d 
 ! vitesse gap                    :  %(r1)f 
 ! le nb de modes de base_flui    :  %(i2)d 
 ! le nb total de modes de la base:  %(i3)d 
 ! le pas de temps initial est    :  %(r2)f 
 ! duree de l''excitation          :  %(r3)f 
"""),

25: _("""
 ! le nb de pas d''archive est     :  %(i1)d 
"""),

26: _("""
 ! le pas de temps du calcul est  : %(r1)f 
 ! le nb de pas de calcul est     :  %(i1)d 
 ! le nb de pas d''archive est     :  %(i2)d 
"""),

27: _("""
 ----------------------------------------------
"""),

28: _("""
 trop d''amortissements modaux   nombre d''amortissement :  %(i1)d 
    nombre de mode :  %(i2)d 
 
"""),

29: _("""
 trop d''amortissements modaux   nombre d''amortissement :  %(i1)d 
    nombre de mode :  %(i2)d 
 
"""),

30: _("""
 amortissement non diagonal on ne sait pas traiter  %(i1)d 
"""),

31: _("""
 il manque des amortissements modaux   nombre d''amortissement :  %(i1)d 
    nombre de mode :  %(i2)d 
 
"""),

32: _("""
 on ne peut pas demander de reponsesecondaire sans la reponse primaire
"""),

33: _("""
  *** analyse spectrale ***-------------------------
 ! la base modale utilisee est :  %(k1)s 
 ! le nombre de vecteurs de base est :  %(i1)d 
 ! regle de combinaison modale :  %(k2)s 
 ! les options de calcul demandees sont :  %(k3)s 
"""),

34: _("""
                                           %(k1)s 
"""),

35: _("""
 ! regle de combinaison des contributions 
  de chaque mouvement d''appui :  %(k1)s 
"""),

36: _("""
 erreur dans les donneesla masse n existe pas dans la table  %(k1)s 
"""),

37: _("""
 erreur dans les donneesla masse n existe pas dans la table  %(k1)s 
"""),

38: _("""
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!mode dynamique           :  %(i1)d 
 amortissement trop grand :  %(r1)f 
 amortissement critique   :  %(r2)f 
 probleme de convergence possible %(k1)s 
 
"""),

39: _("""
          sous-structuration dynamique
        calcul par superposition modale
 ----------------------------------------------
 ! la numerotation utilisee est   :  %(k1)s 
 ! le nb d''equations est          :  %(i1)d 
 ! la methode utilisee est        :  %(k2)s 
 !    - nb de vecteurs dynamiques :  %(i2)d 
 !    - nb de deformees statiques :  %(i3)d 
"""),

40: _("""
 ! le pas de temps initial est  : %(r1)f 
"""),

41: _("""
 ! le pas de temps du calcul est  : %(r1)f 
 ! le nb de pas de calcul est     :  %(i1)d 
"""),

42: _("""
 ! le nb de pas d'archive est     :  %(i1)d 
"""),

43: _("""
 ----------------------------------------------
"""),

44: _("""
 les interfaces de la liaison n''ont pas la meme longueur
  sous-structure 1 -->  %(k1)s 
  interface 1      -->  %(k2)s 
  sous-structure 2 -->  %(k3)s 
  interface 2      -->  %(k4)s 
 
"""),

45: _("""
 conflit dans les vis_a_vis des noeudsle noeud  %(k1)s 
 est le vis-a-vis des noeuds  %(k2)s 
 et  %(k3)s 
 
"""),

46: _("""
 Le critère de vérification ne peut etre relatif dans votre cas,
 la longueur caracteristique de l'interface de la sous-structure etant nulle.
  sous-structure 1 -->  %(k1)s 
  interface 1      -->  %(k2)s 
  sous-structure 2 -->  %(k3)s 
  interface 2      -->  %(k4)s 
 
"""),

47: _("""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s 
  interface 1      -->  %(k2)s 
  sous-structure 2 -->  %(k3)s 
  interface 2      -->  %(k4)s 
 
"""),

48: _("""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s 
  interface 1      -->  %(k2)s 
  sous-structure 2 -->  %(k3)s 
  interface 2      -->  %(k4)s 
 
"""),

49: _("""
 les noeuds des interfaces ne sont pas alignes en vis-a-vis
  sous-structure 1 -->  %(k1)s 
  interface 1      -->  %(k2)s 
  sous-structure 2 -->  %(k3)s 
  interface 2      -->  %(k4)s 
 les noeuds ont ete reordonnes %(k5)s 
 
"""),

50: _("""
 les deux interfaces ont pas meme nombre de noeuds
 nombre noeuds interface droite -->  %(i1)d 
 nombre noeuds interface gauche -->  %(i2)d 
 
"""),

51: _("""
 conflit dans les vis_a_vis des noeudsle noeud  %(k1)s 
 est le vis-a-vis des noeuds  %(k2)s 
 et  %(k3)s 
 
"""),

52: _("""
 axe de symetrie cyclique different de oznumero du couple de noeuds :  %(i1)d 
 noeud droite -->  %(k1)s 
 noeud gauche -->  %(k2)s 
 
"""),

53: _("""
  probleme de rayon droite-gauche differents numero du couple de noeuds :  %(i1)d 
 noeud droite -->  %(k1)s 
 noeud gauche -->  %(k2)s 
 
"""),

54: _("""
  probleme signe angle entre droite et gauche numero du couple de noeuds:  %(i1)d 
 noeud droite -->  %(k1)s 
 noeud gauche -->  %(k2)s 
 
"""),

55: _("""
  probleme valeur angle repetitivite cyclique numero du couple de noeuds:  %(i1)d 
 noeud droite -->  %(k1)s 
 noeud gauche -->  %(k2)s 
 
"""),

56: _("""
  verification repetitivitee: aucune erreur detectee
"""),

57: _("""
 les noeuds des interfaces ne sont pas alignes en vis-a-vis
 les noeuds ont ete reordonnes  
 
"""),

58: _("""
  arret sur probleme repetitivite cyclique tentative de diagnostic:  %(k1)s 
"""),

59: _("""
  Arret sur nombre de secteurs impossible
      - nombre de secteurs --> %(i1)d
 
"""),

60: _(""" 
 VISCOCHABOCHE : erreur d'intégration
  - Essai d'intégration numéro :  %(i1)d
  - Convergence vers une solution non conforme,
  - Incrément de déformation cumulée négative = %(r1)f,
  - Changer la taille d'incrément.
"""),

61: _(""" 
 VISCOCHABOCHE : erreur 
  - Non convergence à l'itération maxi : %(i1)d
  - Convergence régulière mais trop lente,
  - Erreur > %(r1)f
  - Diminuer la taille d'incrément

"""),

62: _(""" 
 VISCOCHABOCHE: ERREUR)
  - Non convergence à l'itération maxi : %(i1)d
  - Convergence régulière
  - Erreur > %(r1)f
  - Diminuer la taille d'incrément
"""),

63: _(""" 
 VISCOCHABOCHE: ERREUR)
  - Non convergence à l'itération maxi : %(i1)d
  - Erreur > %(r1)f
  - Diminuer la taille d'incrément
"""),

64: _(""" 
    NADAI_B, erreur d'intégration
     - Essai d'intégration numéro : %(i1)d
     - Convergence vers une solution non conforme,
     - Incrément de déformation cumulée négative = %(r1)f
     - Changer la taille d'incrément
"""),

65: _(""" 
    NADAI_B : ERREUR 
     - Non convergence a itération maxi : %(i1)d
     - Convergence régulière mais trop lente
     - ERREUR > %(r1)f
     - Diminuer la taille d'incrément
 """),
 
66: _(""" 
    NADAI_B : ERREUR 
     - Non convergence à itération maxi : %(i1)d
     - Convergence irrégulière & erreur > %(r1)f
     - Diminuer la taille d'incrément
 """),
 
67: _(""" 
   NADAI_B : ERREUR
     - Non convergence à itération maxi : %(i1)d
     - ERREUR > %(r1)f
     - Diminuer la taille d'incrément
 """),
 
68: _(""" 
 Arret par manque de temps CPU au numéro d'ordre %(i1)d

   - Temps moyen par incrément de temps : %(r1)f
   - Temps CPU restant :                  %(r2)f

 La base globale est sauvegardée, elle contient les pas archivés avant l'arret 

 """),
 
69: _(""" 
    %(k1)s: ERREUR
      - Non convergence a itération maxi : %(i1)d
      - Convergence régulière mais trop lente
      - ERREUR > %(r1)f
      - Diminuer la taille d'incrément
 """),

70: _(""" 
    %(k1)s: ERREUR
     - Non convergence à itération maxi : %(i1)d
     - Convergence irrégulière & erreur > %(r1)f
     - Diminuer la taille d'incrément 
 """),

71: _(""" 
    %(k1)s: ERREUR
     - Non convergence à itération maxi : %(i1)d
     - ERREUR > %(r1)f
     - Diminuer la taille d'incrément

 """),
 
72: _(""" 
   LMARC : erreur d'intégration
     - Essai d'intégration numéro : %(i1)d
     - Divergence de l'intégration locale
     - Diminuer la taille d'incrément
 """),
 
73: _(""" 
   LMARC : erreur d'intégration
     - Essai d'intégration numéro : %(i1)d
     - Convergence vers une solution non conforme
     - Incrément de déformation cumulée négative : %(r1)f
     - Changer la taille d'incrément
 """),
74: _(""" 
   LMARC : erreur 
     - Non convergence a itération maxi :  %(i1)d
     - Convergence régulière mais trop lente
     - Diminuer la taille d'incrément
 """),
 
75: _(""" 
   LMARC : erreur 
     - Non convergence a itération maxi :  %(i1)d
     - Convergence irrégulière & erreur >  %(r1)f
     - Diminuer la taille d'incrément
 """),
76: _(""" 
   LMARC : erreur 
     - Non convergence a itération maxi :  %(i1)d
     - erreur > %(r1)f
     - Diminuer la taille d'incrément
 """),
 
77: _(""" 
   Arret par manque de temps CPU au numéro d'ordre : %(i1)d
     - Dernier instant archivé :      %(r1)f
     - Numéro d'ordre correspondant : %(i2)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps CPU restant :            %(r3)f
  """),
  
78: _(""" 
   ! Précision du transitoire : %(r1)f
  """),
  
79: _(""" 
   ! Couplage temporel avec NB modes : %(i1)d
  """),

80: _(""" 
 ! Le nombre de lieu(x) de choc est : %(i1)d
  """),

81: _(""" 
 ! Le nombre de dispo anti sismique est : %(i1)d
  """),
  
82: _(""" 
 ! le nombre de lieu(x) de choc avec flambement est : %(i1)d
  """),
  
83: _(""" 
 ! Le nombre de RELA_EFFO_DEPL est : %(i1)d
  """),
  
84: _(""" 
 ! Le nombre de RELA_EFFO_VITE est : %(i1)d
  """),
  
85: _(""" 
 Nature de l'excitation : %(k1)s
  """),
  
86: _(""" 
 Règles de combinaison des réponses directionnelles : %(k1)s
  """),
  
87: _(""" 
   Arret par manque de temps CPU 
     - Instant courant :              %(r1)f
     - Nombre d'appels à ALITMI :     %(i1)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps CPU restant :            %(r3)f
  """),
  
88: _(""" 
   Arret par manque de temps CPU au pas de temps : %(i1)d
     - A l'instant  :                %(r1)f
     - Temps moyen par pas :         %(r2)f
     - Temps CPU restant :           %(r3)f
  """),

}
