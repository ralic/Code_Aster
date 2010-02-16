#@ MODIF edyos Messages  DATE 16/02/2010   AUTEUR GREFFET N.GREFFET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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



#
#  messages d'erreur pour interface Aster/edyos 
# 
#        


#    Ce script python permet d'associer un texte aux numeros d'erreur
#    appeles dans le sous programme errcou.f 

#    Ces messages d'erreur sont issus de la note HI-26/03/007A
#    "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
#    ANNEXE 1: CODES D'ERREURS  (PAGE 70)
#    FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003
#
#    Les numeros des erreurs de ce script correspondent aux numeros de la
#    reference bibliographique 
 

def _(x) : return x

cata_msg={

1 : _("""
      YACS : Emetteur inconnu
"""),

2 : _("""
      YACS : Nom de variable inconnu
"""),

3 : _("""
      YACS : Variable ne devant pas être lue mais écrite
"""),


4 : _("""
      YACS : Type de variable inconnu
"""),

5 : _("""
      YACS : Type de variable différent de celui déclaré
"""),

6 : _("""
      YACS : Mode de dépendance inconnu
"""),

7 : _("""
      YACS : Mode de dépendance différent de celui déclaré
"""),

8 : _("""
      YACS : Requête non autorisée
"""),

9 : _("""
      YACS : Type de déconnection incorrect
"""),

10 : _("""
       YACS : Directive de déconnection incorrecte
"""),

11 : _("""
       YACS : Nom de code inconnu
"""),

12 : _("""
       YACS : Nom d'instance inconnue
"""),

13 : _("""
      YACS : Requête en attente
"""),

14 : _("""
      YACS : Message de service
"""),

15 : _("""
      YACS : Nombre de valeurs transmises nul
"""),

16 : _("""
       YACS : Dimension de tableau récepteur insuffisante
"""),

17 : _("""
      YACS : Blocage
"""),

18 : _("""
      YACS : Arrêt anormal d'une instance
"""),

19 : _("""
      YACS : Coupleur absent...
"""),

20 : _("""
      YACS : Variable ne figurant dans aucun lien
"""),

21 : _("""
      YACS : Nombre de pas de calcul égal à zéro
"""),

22 : _("""
      YACS : Machine non déclarée
"""),

23 : _("""
      YACS : Erreur variable d'environnement COUPLAGE_GROUPE non positionnée
"""),

24 : _("""
=      YACS : Variable d'environnement COUPLAGE_GROUPE inconnue
"""),

25 : _("""
      YACS : Valeur d'info non utilisée
"""),  

26 : _("""
      YACS : Erreur de format dans le fichier de couplage
"""),

27 : _("""
      YACS : Requête annulée à cause du passage en mode normal
"""),

28 : _("""
      YACS : Coupleur en mode d'exécution normal
"""),

29 : _("""
      YACS : Option inconnue
"""),

30 : _("""
      YACS : Valeur d'option incorrecte
"""),

31 : _("""
      YACS : Ecriture d'une variable dont l'effacement est demandé
"""),

32 : _("""
      YACS : Lecture d'une variable incorrectement connectée
"""),

33 : _("""
      YACS : Valeur d'info non utilisée
"""),  

34 : _("""
      YACS : Valeur d'info non utilisée
"""),  

35 : _("""
      YACS : Erreur dans la chaine de déclaration
"""),

36 : _("""
      YACS : Erreur dans le lancement dynamique d'une instance
"""),

37 : _("""
      YACS : Erreur de communication 
"""),

38 : _("""
      YACS : Valeur d'info non utilisée
"""),

39 : _("""
      YACS : Mode d'exécution non défini
"""),    

40 : _("""   
      YACS : Instance déconnectée
"""),


41 : _("""
 Avertissement YACS (gravité faible) :
       Dans le SSP %(k1)s la variable %(k2)s a une valeur différente
       de celle envoyée
"""),

42 : _("""
 Erreur YACS :
       Problème dans le SSP  : %(k1)s
       Pour la variable      : %(k2)s
       A l'itération numéro  : %(i1)d
"""),

43 : _("""   
      Attention, le nombre de palier dépasse 10 => vérifiez les données
"""),

44 : _("""   
      Problème noeud du palier %(i1)d = %(i2)d
"""),

45 : _("""   
      Non convergence du code EDYOS
"""),

46 : _("""   
      Le code EDYOS n'a pas convergé
      Avc le schéma en temps d'Euler, on ne sous-divise pas le pas de temps
      Le calcul s'arrête donc
      Conseil : tester le schéma en temps adaptatif
"""),

47 : _("""   
      Le code EDYOS n'a pas convergé
      Avc le schéma en temps adaptatif, on va tenter de diminuer le pas de temps
"""),

}

