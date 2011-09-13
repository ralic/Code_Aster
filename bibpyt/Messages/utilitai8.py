#@ MODIF utilitai8 Messages  DATE 13/09/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

1 : _("""
 Rien que des constantes pour une nappe.
 Nombre de fonctions constantes %(i1)d
"""),

2 : _("""
 Paramètres différents.
 fonction %(k1)s de paramètre %(k2)s au lieu de %(k3)s
"""),

3 : _("""
 Le nombre de paramètres  %(i1)d  est différent du nombre de fonctions  %(i2)d
"""),

4 : _("""
 Il n'y a pas un nombre pair de valeurs, "DEFI_FONCTION" occurence  %(i1)d
"""),

5 : _("""
 Les abscisses de la fonction  %(k1)s ont ete réordonnées.
"""),

6 : _("""
 L'ordre des abscisses de la fonction numéro  %(i1)d a ete inversé .
"""),

7 : _("""
 Appel erroné
  archivage numéro :  %(i1)d
  code retour de rsexch :  %(i2)d
"""),

8 : _("""
 Lecture des champs:
"""),

9 : _("""
   Numéro d'ordre :  %(i1)d             inst :  %(r1)g
"""),

10 : _("""
   Le champ 'DEPL' n'est pas présent dans la structure de données résultat.
   Le calcul de l'option %(k1)s n'est pas possible.
"""),

13 : _("""
  Dans la structure de données resultat %(k1)s,
  le champ %(k2)s
"""),

14 : _("""
  ou le champ %(k1)s
"""),

15 : _("""
  n'existe pas.
"""),

16 : _("""
  Pour le numéro d'ordre NUME_ORDRE %(i1)d,
  l'option %(k1)s n'est pas calculée.

  Conseil :
    Vérifiez le nom de la structure de donnée et vérifiez que les champs existent.
    Si le concept n'est pas réentrant les champs ne sont pas cherchés dans %(k2)s.
"""),

17 : _("""
 pas de NUME_ORDRE trouvé pour le numéro  %(i1)d
"""),

18 : _("""
 pas de champs trouvé pour l'instant  %(r1)g
"""),

19 : _("""
 Plusieurs pas de temps trouvés  dans l'intervalle de précision
 autour de l'instant  %(r1)g
 nombre de pas de temps trouvés  %(i1)d
 Conseil : modifier le paramètre PRECISION
"""),

20 : _("""
 Erreur dans les données :
 Le paramètre existe déja:  %(k1)s  dans la table:  %(k2)s
"""),

21 : _("""
 Erreur dans les données
 Le type du paramètre:  %(k1)s
  est différent pour le paramètre:  %(k2)s
  et le paramètre:  %(k3)s
"""),

22 : _("""
  Valeur de M maximale atteinte pour résoudre F(M)=0,
  Conseil : Vérifiez vos listes d'instants de rupture, M maximal admissible =  %(r1)f
"""),

23 : _("""
  Valeur de M minimale atteinte pour résoudre F(M)=0,
  Conseil : Vérifiez vos listes d'instants de rupture, valeur de M =  %(r1)f
"""),

24 : _("""
 Le champ demandé est incompatible avec le type de résultat
  type de résultat : %(k1)s
      nom du champ : %(k2)s
"""),

25 : _("""
 Le nombre d'astérisques pour les noms de fichiers ensight de pression est trop grand.
 Il est limite à 7
 Il y a %(i1)d asterisques.
"""),

26 : _("""
 Appel erroné  résultat :  %(k1)s   archivage numéro :  %(i1)d
   code retour de rsexch :  %(i2)d
   problème champ :  %(k2)s
"""),

27 : _("""
 Appel erroné  résultat :  %(k1)s   archivage numéro :  %(i1)d
   code retour de rsexch :  %(i2)d
   problème champ :  %(k2)s
"""),

28 : _("""
 Fin de fichier dans la lecture des fichiers ensight
"""),

29 : _("""
 Erreur dans la lecture du fichier ensight
"""),

30 : _("""
  problème pour le fichier:  %(k1)s
"""),

31 : _("""
  Option déjà calculée:  option  %(k1)s  NUME_ORDRE  %(i1)d
  On la recalcule car les données peuvent être différentes

"""),

32 : _("""
 L'extrapolation ne peut être faite à gauche (interdit).
"""),

33 : _("""
 L'extrapolation ne peut être faite à droite (interdit).
"""),

34 : _("""
 L'interpolation ne peut être faite car aucun champ de : %(k1)s n'est calcule.
"""),

35 : _("""
 La variable d'accès %(k1)s est invalide pour une interpolation.
"""),

36 : _("""
 Ce nom de champ est interdit : %(k1)s pour une interpolation.
"""),

37 : _("""
 Résultat: %(k1)s nom_cham: %(k2)s  variable d'accès: %(k3)s valeur: %(r1)g

"""),

38 : _("""
 Plusieurs champs correspondant à l'accès demandé pour la sd_resultat  %(k1)s
"""),

39 : _("""
 accès %(k1)s : %(i1)d
"""),

40 : _("""
 accès %(k1)s : %(r1)g
"""),

41 : _("""
 accès %(k1)s  : %(k1)s
"""),

46 : _("""
  nombre : %(i1)d NUME_ORDRE retenus : %(i2)d, %(i3)d
"""),

47 : _("""
 Pas de champ correspondant à un accès demandé pour la sd_resultat  %(k1)s
"""),

48 : _("""
  nombre : %(i1)d NUME_ORDRE retenus (les trois premiers) : %(i2)d, %(i3)d, %(i4)d
"""),

56 : _("""
 pas de champs pour l'accès  %(k1)s de valeur  %(r1)g
"""),

57 : _("""
Erreur utilisateur :
  Plusieurs champs correspondent à l'accès demandé pour la sd_résultat  %(k1)s
  - accès "INST"             : %(r1)19.12e
  - nombre de champs trouvés : %(i1)d
Conseil:
  Reserrer la précision avec le mot clé PRECISION
"""),

58 : _("""
 Pas de champs pour l'accès  %(k1)s de valeur  %(r1)g
"""),

59 : _("""
Erreur utilisateur :
  Plusieurs champs correspondent à l'accès demandé pour la sd_résultat  %(k1)s
  - accès "FREQ"             : %(r1)19.12e
  - nombre de champs trouvés : %(i1)d
Conseil:
  Reserrer la précision avec le mot clé PRECISION
"""),

60 : _("""
 L'intégrale d'un champ sur des éléments de structure
(poutre, plaque, coque, tuyau, poutre multi-fibres) n'est pas programmée.
 Réduisez la zone de calcul par le mot-clé GROUP_MA/MAILLE.
"""),

61 : _("""
 Erreur dans les données pour le champ  %(k1)s
 Aucun noeud ne supporte les composantes
 %(k2)s, %(k3)s, %(k4)s, %(k5)s, ...
"""),

62 : _("""
 Erreur dans les données pour le champ  %(k1)s
 Aucune maille ne supporte les composantes
 %(k2)s, %(k3)s, %(k4)s, %(k5)s, ...
"""),

63 : _("""
 POST_ELEM INTEGRALE : la maille %(i1)d de type %(k1)s ne sait pas
 (ou ne peut pas) calculer le post traitement demandé
Conseil:
   Limiter le post traitement à des GROUP_MA contenant des mailles
de type valide
"""),

64 : _("""
  Vous definissez une charge thermique sur un modele mecanique !
  Le MODELE doit être de type thermique.
"""),

65 : _("""
  Vous definissez une charge mecanique sur un modele thermique !
  Le MODELE doit être de type mécanique.
"""),

}
