# coding=utf-8
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

1 : _(u"""
 Rien que des constantes pour une nappe.
 Nombre de fonctions constantes %(i1)d
"""),

2 : _(u"""
 Paramètres différents.
 fonction %(k1)s de paramètre %(k2)s au lieu de %(k3)s
"""),

3 : _(u"""
 Le nombre de paramètres  %(i1)d  est différent du nombre de fonctions  %(i2)d
"""),

4 : _(u"""
 Il n'y a pas un nombre pair de valeurs, "DEFI_FONCTION" occurrence  %(i1)d
"""),

5 : _(u"""
 Les abscisses de la fonction  %(k1)s ont été réordonnées.
"""),

6 : _(u"""
 L'ordre des abscisses de la fonction numéro  %(i1)d a été inversé .
"""),

7 : _(u"""
 Appel erroné
  archivage numéro :  %(i1)d
  code retour de rsexch :  %(i2)d
"""),

8 : _(u"""
 Lecture des champs:
"""),

9 : _(u"""
   Numéro d'ordre :  %(i1)d             instant :  %(r1)g
"""),

10 : _(u"""
 Le modèle est manquant.

 Conseil :
  Il faut remplir le mot-clé MODELE si la commande utilisée le permet.
"""),






13 : _(u"""
  Dans la structure de données résultat %(k1)s,
  le champ %(k2)s
"""),

14 : _(u"""
  ou le champ %(k1)s
"""),

15 : _(u"""
  n'existe pas.
"""),

16 : _(u"""
  Pour le numéro d'ordre NUME_ORDRE %(i1)d,
  l'option %(k1)s n'est pas calculée.

  Conseil :
    Vérifiez le nom de la structure de donnée et vérifiez que les champs existent.
    Si le concept n'est pas réentrant les champs ne sont pas cherchés dans %(k2)s.
"""),

17 : _(u"""
 pas de NUME_ORDRE trouvé pour le numéro  %(i1)d
"""),

18 : _(u"""
 pas de champs trouvé pour l'instant  %(r1)g
"""),

19 : _(u"""
 Plusieurs pas de temps trouvés  dans l'intervalle de précision
 autour de l'instant  %(r1)g
 nombre de pas de temps trouvés  %(i1)d
 Conseil : modifier le paramètre PRECISION
"""),

20 : _(u"""
 Erreur dans les données :
 Le paramètre existe déjà:  %(k1)s  dans la table:  %(k2)s
"""),

21 : _(u"""
 Erreur dans les données
 Le type du paramètre:  %(k1)s
  est différent pour le paramètre:  %(k2)s
  et le paramètre:  %(k3)s
"""),

22 : _(u"""
  Valeur de M maximale atteinte pour résoudre F(M)=0,
  Conseil : Vérifiez vos listes d'instants de rupture, M maximal admissible =  %(r1)f
"""),

23 : _(u"""
  Valeur de M minimale atteinte pour résoudre F(M)=0,
  Conseil : Vérifiez vos listes d'instants de rupture, valeur de M =  %(r1)f
"""),

24 : _(u"""
 Le champ demandé est incompatible avec le type de résultat
  type de résultat : %(k1)s
      nom du champ : %(k2)s
"""),

25 : _(u"""
 Le nombre d'astérisques pour les noms de fichiers ENSIGHT de pression est trop grand.
 Il est limite à 7
 Il y a %(i1)d astérisques.
"""),

26 : _(u"""
 Appel erroné  résultat :  %(k1)s   archivage numéro :  %(i1)d
   code retour de rsexch :  %(i2)d
   problème champ :  %(k2)s
"""),

27 : _(u"""
 Appel erroné  résultat :  %(k1)s   archivage numéro :  %(i1)d
   code retour de rsexch :  %(i2)d
   problème champ :  %(k2)s
"""),

28 : _(u"""
 Fin de fichier dans la lecture des fichiers ENSIGHT
"""),

29 : _(u"""
 Erreur dans la lecture du fichier ENSIGHT
"""),

30 : _(u"""
  problème pour le fichier:  %(k1)s
"""),

31 : _(u"""
  Option déjà calculée:  option  %(k1)s  NUME_ORDRE  %(i1)d
  On la recalcule car les données peuvent être différentes

"""),

32 : _(u"""
 L'extrapolation ne peut être faite à gauche (interdit).
"""),

33 : _(u"""
 L'extrapolation ne peut être faite à droite (interdit).
"""),

34 : _(u"""
 L'interpolation ne peut être faite car aucun champ de : %(k1)s n'est calcule.
"""),

35 : _(u"""
 La variable d'accès %(k1)s est invalide pour une interpolation.
"""),

36 : _(u"""
 Ce nom de champ est interdit : %(k1)s pour une interpolation.
"""),

37 : _(u"""
 Résultat: %(k1)s NOM_CHAM: %(k2)s  variable d'accès: %(k3)s valeur: %(r1)g

"""),

38 : _(u"""
 Plusieurs champs correspondant à l'accès demandé pour la SD_RESULTAT  %(k1)s
"""),

39 : _(u"""
 accès %(k1)s : %(i1)d
"""),

40 : _(u"""
 accès %(k1)s : %(r1)g
"""),

41 : _(u"""
 accès %(k1)s  : %(k1)s
"""),

46 : _(u"""
  nombre : %(i1)d NUME_ORDRE retenus : %(i2)d, %(i3)d
"""),

47 : _(u"""
 Pas de champ correspondant à un accès demandé pour la SD_RESULTAT  %(k1)s
"""),

48 : _(u"""
  nombre : %(i1)d NUME_ORDRE retenus (les trois premiers) : %(i2)d, %(i3)d, %(i4)d
"""),

56 : _(u"""
 pas de champs pour l'accès  %(k1)s de valeur  %(r1)g
"""),

57 : _(u"""
Erreur utilisateur :
  Plusieurs champs correspondent à l'accès demandé pour la sd_résultat  %(k1)s
  - accès "INST"             : %(r1)19.12e
  - nombre de champs trouvés : %(i1)d
Conseil:
  Resserrer la précision avec le mot clé PRECISION
"""),

58 : _(u"""
 Pas de champs pour l'accès  %(k1)s de valeur  %(r1)g
"""),

59 : _(u"""
Erreur utilisateur :
  Plusieurs champs correspondent à l'accès demandé pour la sd_résultat  %(k1)s
  - accès "FREQ"             : %(r1)19.12e
  - nombre de champs trouvés : %(i1)d
Conseil:
  Resserrer la précision avec le mot clé PRECISION
"""),

60 : _(u"""
 L'intégrale d'un champ sur des éléments de structure
(poutre, plaque, coque, tuyau, poutre multifibre) n'est pas programmée.
 Réduisez la zone de calcul par le mot-clé GROUP_MA/MAILLE.
"""),

61 : _(u"""
 Erreur dans les données pour le champ  %(k1)s
 Aucun noeud ne supporte les composantes
 %(k2)s, %(k3)s, %(k4)s, %(k5)s, ...
"""),

62 : _(u"""
 Erreur dans les données pour le champ  %(k1)s
 Aucune maille ne supporte les composantes
 %(k2)s, %(k3)s, %(k4)s, %(k5)s, ...
"""),

63 : _(u"""
 POST_ELEM INTEGRALE : la maille %(i1)d de type %(k1)s ne sait pas
 (ou ne peut pas) calculer le post-traitement demandé
Conseil:
   Limiter le post-traitement à des GROUP_MA contenant des mailles
de type valide
"""),

64 : _(u"""
  Vous définissez une charge thermique sur un modèle mécanique !
  Le MODELE doit être de type thermique.
"""),

65 : _(u"""
  Vous définissez une charge mécanique sur un modèle thermique !
  Le MODELE doit être de type mécanique.
"""),

66 : _(u"""
  Le MACR_ELEM_DYNA a été créé avec une base modale qui entre-temps a été
  modifiée/enrichie. Le nombre d'équations dans le MACR_ELEM_DYNA ne 
  correspond plus au nombre de vecteurs de projection modale.
  
  -> Conseil: Pour mettre à jour le MACR_ELEM_DYNA, il faut d'abord détruire 
              le concept associé et le récréer ensuite avec la nouvelle base modale.
  
  Nombre d'équations dans le MACR_ELEM_DYNA      = %(i1)d
  Nombre de vecteurs de projection modale        = %(i2)d
"""),

}
