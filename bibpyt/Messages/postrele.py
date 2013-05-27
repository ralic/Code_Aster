# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1 : _(u"""
 création/extension de la table %(k1)s
"""),

2 : _(u"""
 post-traitement numéro :  %(i1)d
 l'instant demandé n'a pas été calculé
 pas de post-traitement
 """),

3 : _(u"""
 post-traitement numéro :  %(i1)d
 aucune maille ne correspond aux critères demandés
 pas de post-traitement
"""),

5 : _(u"""
 il manque le vecteur des composantes "NOCP".
 Contactez le support.
"""),

6 : _(u"""
 chemin nul ou défini en un noeud
"""),

7 : _(u"""
 le nombre de composantes à traiter est limité à 6 pour opération "MOYENNE".
 utiliser "NOM_CMP" avec au plus 6 composantes.
"""),

8 : _(u"""
 initialisation de la table %(k1)s
"""),

9 : _(u"""
 pas de champ trouvé pour l'option %(k1)s
"""),

10 : _(u"""
 paramètre %(k1)s de type %(k2)s
"""),

11 : _(u"""
 on ne traite que les champs complexes
"""),

12 : _(u"""
 tableau de travail limité, réduire le nombre de composantes à traiter
"""),

13 : _(u"""
 plus de 3000 composantes.
 Contactez le support
"""),

14 : _(u"""
 en repère local, on ne traite pas le champ %(k1)s
"""),

15 : _(u"""
 ICOEF trop grand
 Contactez le support
"""),

16 : _(u"""
 problème maillage
 Contactez le support
"""),

17 : _(u"""
 on ne traite que des champs de type "DEPL_R" pour un changement de repère
"""),

18 : _(u"""
 le type de maille %(k1)s n'est pas traité
"""),

19 : _(u"""
 mauvaise définition du chemin, problème de continuité du chemin sur une maille
 diminuer la précision dans l'opérateur INTE_MAIL_(2D/3D)
"""),

20 : _(u"""
 on ne traite pas ce cas
 Contactez le support
"""),

21 : _(u"""
 avec VECT_Y, le groupe de noeuds doit contenir au moins 2 noeuds
"""),

22 : _(u"""
 avec VECT_Y, il faut préciser
   - soit un seul groupe de noeuds
   - soit plusieurs noeuds
"""),

23 : _(u"""
 on ne peut pas mélanger des arcs et des segments.
"""),

24 : _(u"""
 chemin de maille vide
"""),

25 : _(u"""
 contradiction avec INTE_MAIL_2D
"""),

26 : _(u"""
 changement de repère:
 champ non traité %(k1)s
 option de calcul %(k2)s
"""),

27 : _(u"""
 noeud sur l'AXE_Z
 maille      : %(k1)s
 noeud       : %(k2)s
 coordonnées : %(r1)f
"""),

28 : _(u"""
 les noeuds du maillage ne sont pas tous dans un même plan Z = CONSTANTE
 changement de repère non traité
"""),

29 : _(u"""
 on ne sait pas faire ce post-traitement pour le chemin %(k1)s en repère %(k2)s
"""),

30 : _(u"""
 le noeud %(k1)s est confondu avec l'origine
"""),

31 : _(u"""
 le noeud %(k1)s est sur l'AXE_Z
"""),

32 : _(u"""
 les noeuds du maillage ne sont pas tous dans un même plan Z = CSTE
 option TRAC_NOR non traitée
 utiliser l'option TRAC_DIR
"""),

33 : _(u"""
 option non traitée: %(k1)s, post-traitement: %(i1)d
 les invariants tensoriels sont calculés
   pour les options :  %(k2)s
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

34 : _(u"""
 option non traitée: %(k1)s, post-traitement: %(i1)d
 les traces normales sont calculées
   pour les options :  %(k2)s
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

35 : _(u"""
 option non traitée: %(k1)s, post-traitement: %(i1)d
 les traces directionnelles sont calculées
   pour les options :  %(k2)s
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

36 : _(u"""
 trace directionnelle, post-traitement: %(i1)d
 direction nulle, pas de calcul
"""),

37 : _(u"""
 attention post-traitement %(i1)d
 seules les composantes du tenseur des contraintes sont traitées
"""),

38 : _(u"""
 post-traitement %(i1)d
 composante non traitée dans un changement de repère
 Contactez le support
"""),

39 : _(u"""
 post-traitement %(i1)d
 grandeur %(k1)s non traitée dans un changement de repère
 les changements de repère sont possibles
   pour la grandeur %(k2)s  option: %(k3)s
   pour la grandeur %(k4)s  options: %(k5)s %(k6)s
   pour les grandeurs %(k7)s  %(k8)s
"""),

40 : _(u"""
 le noeud numéro %(i1)d n'est pas connecté à la maille de nom %(k1)s
"""),

41 : _(u"""
 champ inexistant NOM_CHAM: %(k1)s  NUME_ORDRE: %(i1)d
"""),

42 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 les listes arguments des mots clés RESULTANTE et MOMENT doivent être de même longueur
 cette longueur doit être de 2 ou 3
"""),

43 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 la liste arguments du mot clé POINT doit être de longueur 2 ou 3
"""),

44 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 on ne peut accéder au RESULTAT de nom %(k1)s et de type %(k2)s par %(k3)s ou par %(k4)s
"""),

45 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 on ne peut accéder au RESULTAT de nom %(k1)s et de type %(k2)s par %(k3)s
"""),

46 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 le NOM_CHAM %(k1)s n'est pas autorisé pour le RESULTAT %(k2)s de type %(k3)s
 ou le NOM_CHAM est autorisé mais aucun champ effectif n'existe.
"""),

47 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 le ou les champs élémentaires mis en jeu est ou sont donnés aux points de gauss
 ce ou ces champs ne sont pas traités.
"""),

48 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 la composante %(k1)s n'est pas présente au catalogue des grandeurs.
"""),

49 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 le maillage de la courbe %(k1)s est différent du maillage du champ à traiter %(k2)s
"""),

50 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 le groupe de noeuds %(k1)s ne fait pas parti du maillage sous-jacent au champ à traiter.
"""),

51 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 le noeud %(k1)s ne fait pas parti du maillage sous-jacent au champ à traiter.
"""),

52 : _(u"""
 on ne traite pas le FORMAT_C %(k1)s
"""),

53 : _(u"""
 NEC trop grand
 Contactez le support
"""),

54 : _(u"""
 occurrence %(i1)d du mot clé facteur ACTION
 Impossible de récupérer les composantes du champ.
"""),

55 : _(u"""
 la composante %(k1)s est en double.
"""),

56 : _(u"""
 la composante %(k1)s n'est pas une composante de %(k2)s
"""),

57 : _(u"""
 la grandeur %(k1)s est inconnue au catalogue.
"""),

59 : _(u"""
 Le contenu de la table n'est pas celui attendu !
 Contactez le support
"""),

60 : _(u"""
 arrêt sur erreurs
 Contactez le support
"""),

61 : _(u"""
 Nombre de cycles admissibles négatif,
 vérifiez la courbe de WOHLER
 contrainte calculée =  %(r1)f    nadm =  %(r2)f

"""),

62 : _(u"""
 Attention lors de la définition de votre liste de noeuds,
 %(i1)d noeud est hors de la matière

"""),

63 : _(u"""
 Attention lors de la définition de votre liste de noeuds,
 %(i1)d noeuds sont hors de la matière

"""),

64 : _(u"""
On cherche à faire une extraction sur des noeuds qui ne font
pas partie du maillage, peut-être créés par PROPA_FISS
"""),

65 : _(u"""
La composante %(k1)s n'existe pas pour le champ de type %(k2)s
du résultat %(k3)s
"""),

66 : _(u"""
Dans le cas d'un champ de type ELEM, l'utilisation des mots clés NOEUD, GROUP_NO
n'a pas de sens, seul le mot clé MAILLE ou GROUP_MA est autorisé.
"""),
}
