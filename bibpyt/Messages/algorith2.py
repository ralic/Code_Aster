#@ MODIF algorith2 Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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

3 : _("""
 la liste des CHAM_NO n'existe pas
"""),

4 : _("""
 il n'y a aucun CHAM_NO dans la liste
"""),

5 : _("""
 les CHAM_NO n'ont pas tous la meme longueur
"""),

6 : _("""
 il faut définir NOM_CMP
"""),

7 : _("""
 il faut définir 3 angles nautiques.
"""),

8 : _("""
 l'origine doit etre définie par 3 coordonnées.
"""),

9 : _("""
 l axe z est obligatoire en 3d.
"""),

10 : _("""
 pour le 2d, on ne prend que 2 coordonnées pour l'origine.
"""),

11 : _("""
 l axe z est n'a pas de sens en 2d.
"""),

12 : _("""
 le noeud se trouve sur l'axe du repère cylindrique.
 on prend le noeud moyen des centres géometriques.
"""),

13 : _("""
  -> Lors du passage au repère cylindrique, un noeud a été localisé sur l'axe
     du repère cylindrique. Code_Aster utilise dans ce cas le centre de gravité de
     l'élément pour le calcul de la matrice de passage en repère cylindrique.
  -> Risque & Conseil :
     Si ce centre de gravité se trouve également sur l'axe du repère, le calcul
     s'arrete en erreur fatale.
"""),

14 : _("""
 charge non traitée:  %(k1)s
"""),

15 : _("""
 les modélisations autorisées sont 3D et D_PLAN et AXIS
"""),

16 : _("""
 le choix des paramètres ne correspond pas à l'un des modèles CJS
"""),

17 : _("""
 non convergence : essai normales
"""),

18 : _("""
 non convergence : nombre d'itérations maximum atteint
"""),

19 : _("""
 les modélisations autorisées sont 3D et D_PLAN et AXIS
"""),

20 : _("""
 modélisation inconnue
"""),

21 : _("""
  NVI > NVIMAX
"""),

22 : _("""
 vecteur de norme nulle
"""),

23 : _("""
 la maille doit etre de type TETRA4, TETRA10, PENTA6, PENTA15, HEXA8 ou HEXA20.
 or la maille est de type :  %(k1)s .
"""),

24 : _("""
 la maille doit etre de type TETRA4, TETRA10, PENTA6, PENTA15, HEXA8 ou HEXA20.
 ou TRIA3-6 ou QUAD4-8
 or la maille est de type :  %(k1)s .
"""),

25 : _("""
 mauvaise face
"""),

26 : _("""
  %(k1)s  groupe inexistant
"""),

27 : _("""
 maille  %(k1)s  de type  %(k2)s  invalide pour le contact
"""),

28 : _("""
 groupe de mailles de contact invalide
"""),

29 : _("""
 mailles de contact 2d et 3d
"""),

30 : _("""
 trois éléments
"""),

31 : _("""
 deux éléments sur la meme face
"""),

33 : _("""
 pas de maille de référence trouvée
"""),

34 : _("""
 STOP_SINGULIER = DECOUPE nécessite la subdivision automatique du pas de temps (SUBD_PAS)
"""),

35 : _("""
 la méthode  %(k1)s  est inadéquate pour une résolution de type "LDLT"
"""),

36 : _("""
 la méthode  %(k1)s  est inadéquate pour une résolution de type "GCPC"
"""),

37 : _("""
 la methode  %(k1)s  est inadéquate pour une résolution de type "MULT_FRONT"
"""),

38 : _("""
 la méthode  %(k1)s  est inadéquate pour une résolution de type "FETI"
"""),

39 : _("""
 le solveur FETI requiert un concept produit de type SD_FETI en entrée du mot-clé PARTITION
"""),

40 : _("""
 ! nombre de sous-domaines illicite !
"""),

41 : _("""
 en parallèle, il faut au moins un sous-domaine par processeur !
"""),

42 : _("""
 en parallèle, STOGI = OUI obligatoire pour limiter les messages !
"""),

43 : _("""
 pas de calcul sur le critère de Rice disponible
"""),

44 : _("""
 cette commande doit nécessairement avoir le type EVOL_THER.
"""),

45 : _("""
 seuls les champs de fonctions aux noeuds sont évaluables:  %(k1)s
"""),

46 : _("""
 nous traitons les champs de réels et de fonctions: . %(k1)s
"""),

47 : _("""
 le nom symbolique du champ à chercher n'est pas licite. %(k1)s
"""),

48 : _("""
 plusieurs instants correspondent à celui specifié sous AFFE
"""),

49 : _("""
 NUME_FIN inférieur à NUME_INIT
"""),

50 : _("""
 CMP non traitée
"""),

51 : _("""
 il y a plusieurs charges contenant des liaisons unilatérales
"""),

52 : _("""
 débordement tableau (dvlp)
"""),

53 : _("""
 erreur code dans affichage (dvlp)
"""),

54 : _("""
  increment de déformation cumulée (dp) = - %(k1)s
"""),

55 : _("""
 erreur d'intégration
 - essai d(integration  numero  %(k1)s 
 - convergence vers une solution non conforme
 - incrément de déformation cumulée négative = - %(k2)s
 - redécoupage du pas de temps
"""),

56 : _("""
  erreur 
  - non convergence à l'itération maxi  %(k1)s  
  - convergence régulière mais trop lente 
  - erreur >  %(k2)s 
  - redécoupage du pas de temps
"""),

57 : _("""
  erreur
  - non convergence à l'itération maxi  %(k1)s 
  - convergence irrégulière & erreur >  %(k2)s 
  - redécoupage du pas de temps
"""),

58 : _("""
  erreur
  - non convergence à l'itération maxi  %(k1)s 
  - erreur >  %(k2)s 
  - redécoupage du pas de temps
"""),

59 : _("""
  la transformation géométrique est singulière pour la maille : %(k1)s
  (jacobien = 0.)
"""),

60 : _("""
  dérivées secondes non étendues au 3d
"""),

61 : _("""
 les listes des groupes de noeuds à fournir doivent contenir le meme nombre de groupes de noeuds
"""),

62 : _("""
  les listes des groupes de noeuds doivent contenir le meme nombre de noeuds
"""),

63 : _("""
 on n'imprime que des champs réels
"""),

64 : _("""
  %(k1)s cham_no déjà existant
"""),

65 : _("""
 appel erroné a RSEXCH
"""),

66 : _("""
 calcul du transitoire : choc en phase transitoire - pas de solution trouvée.
 utiliser l'option ETAT_STAT = NON
"""),

79 : _("""
 pas de valeurs propres trouvées
"""),

86 : _("""
 il n'y a aucun instant de calcul ('LIST_INST')
"""),

87 : _("""
 liste d'instants non croissante
"""),

88 : _("""
 accès par instant sans évolution ordonnée interdit (INCREMENT)
"""),

89 : _("""
 instant initial introuvable dans la liste d'instants (LIST_INST)
"""),

90 : _("""
 instant final introuvable dans la liste d'instants (LIST_INST)
"""),

91 : _("""
 NUME_INST_INIT plus petit que NUME_FIN avec EVOLUTION: 'RETROGRADE'
"""),

92 : _("""
 NUME_INIT plus grand que NUME_FIN
"""),

94 : _("""
  -> Le numéro d'ordre correspondant à l'instant final de calcul NUME_INST_FIN
     n'appartient pas à la liste des numéros d'ordre.
     Dans ce cas, Aster considère pour numéro d'ordre final, le dernier de
     la liste fournie.
  -> Risque & Conseil :
     Afin d'éviter des pertes de résultats, assurez-vous que le numéro d'ordre
     associé à l'instant NUME_INST_FIN appartienne bien à la liste des numéros
     d'ordre.
"""),

95 : _("""
 accès par instant sans évolution ordonnée interdit (ARCHIVAGE)
"""),

96 : _("""
 impossible d'archiver l'état initial : le concept est réentrant (ARCHIVAGE)
"""),

97 : _("""
 l'archivage va écraser des instants déjà calculés (ARCHIVAGE)
"""),

98 : _("""
 l'archivage va laisser des trous dans la sd EVOL_NOLI (ARCHIVAGE, NUME_INIT)
"""),

99 : _("""
 le nombre de niveau de subdivisions doit etre plus grand que 1 (SUBD_NIVEAU)
"""),

}
