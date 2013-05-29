# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

2 : _(u"""
Le champ de contraintes n'a pas été calculé sur tout le modèle.
On ne peut pas calculer l'option %(k1)s pour le numéro d'ordre %(k2)s.
"""),

3: _(u"""
On ne peut pas calculer un indicateur d'erreur spatial à l'instant initial.
L'indicateur d'erreur spatial ne sera pas calculé à cet instant.
"""),

4: _(u"""
Attention : on n'a pas pu récupérer le paramètre thêta dans le résultat %(k1)s.
La valeur prise par défaut pour thêta est 0.57
"""),

5: _(u"""
Attention : récupération d'une valeur de thêta illicite dans le résultat %(k1)s.
thêta doit être compris entre 0 et 1.
"""),

6 : _(u"""
Le calcul de l'indicateur d erreur ne sait pas traiter les charges du type de %(k1)s.
"""),

7 : _(u"""
Le choix %(k1)s apparaît au moins dans 2 charges.
"""),

8 : _(u"""
Problème sur les charges. Consulter la documentation
"""),

9 : _(u"""
 La maille %(k1)s semble être est trop distordue.
 On ne calcule pas l'option %(k2)s sur cette maille.

 Risques & conseils :
   Il faut vérifier votre maillage !
   Vérifiez les messages émis par la commande AFFE_MODELE.
"""),

10 : _(u"""
 Erreur de programmation :
 Le type de maille %(k1)s n'est pas prévu ou est inconnu.
"""),

11 : _(u"""
Impossible de récupérer les paramètres temporels.
"""),

12 : _(u"""
 La face numéro %(i1)d de la maille %(k1)s possède %(i2)d voisins.
 Or, ceci n'est pas prévu : un seul voisin par face est autorisé.
 Cela signifie que la maillage comporte des mailles doubles ou qu'une maille
 de bord est intercalée entre deux mailles volumiques.
 
 Conseils :
   Vérifiez votre maillage.
   Ces mailles doubles ou intercalées ne sont peut-être pas nécessaire au calcul.
   Dans ce cas ne les affectez pas avec le modèle !
"""),

20 : _(u"""
PERM_IN: division par zéro
"""),

21 : _(u"""
La %(k1)s caractéristique est nulle. On risque la division par zéro.
"""),

22: _(u"""
RHO liquide: div par zéro
"""),

23: _(u"""
Vous n'utilisez pas une modélisation HM saturée élastique.
"""),

24 : _(u"""
 le résultat  %(k1)s  doit comporter un champ d'indicateurs d'erreur au numéro
 d'ordre %(k2)s  .
"""),


25: _(u"""
Il faut renseigner le mot-clef COMP_INCR avec ELAS et LIQU_SATU pour calculer l'indicateur d'erreur temporelle.
"""),

28 : _(u"""
Pour le calcul de l'indicateur d'erreur en HM, il faut fournir
les longueur et pression caractéristiques.
Ces valeurs doivent être strictement positives.

-> Conseil :
     N'oubliez pas de renseigner les valeurs sous le mot-clef GRANDEUR_CARA dans AFFE_MODELE.
"""),

31: _(u"""
deltat: division par zéro
"""),

32 : _(u"""
 Dans le programme %(k1)s, impossible de trouver le diamètre
 pour un élément de type %(k2)s
"""),

33 : _(u"""
 Le diamètre de l'élément de type %(k1)s vaut : %(r1)f
"""),

90 : _(u"""
La condition %(k1)s est bizarre.
"""),

91 : _(u"""
On ne sait pas traiter la condition %(k1)s.
"""),

92 : _(u"""
L'option %(k1)s est calculable en dimension 2 uniquement.
"""),

98 : _(u"""
L'option %(k1)s est invalide.
"""),

99 : _(u"""
Erreur de programmation dans %(k1)s
L'option %(k2)s ne correspond pas à une option de calcul d'indicateur d'erreur.
"""),

}
