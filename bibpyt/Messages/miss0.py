# coding=utf-8
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

cata_msg={
1: _(u"""
Longueur de LFREQ_LISTE incorrecte.
"""),

2: _(u"""
Longueur de CONTR_LISTE incorrecte.
"""),

3 : _(u"""
Il faut une et une seule couche avec SUBSTRATUM="OUI".
"""),

4 : _(u"""
La définition de la couche numéro %(i1)d est incorrecte :
Il y a %(i2)d matériaux, or NUME_MATE=%(i3)d.
"""),

5 : _(u"""
La numérotation des couches est incorrectes.
"""),

6 : _(u"""
Erreur lors de la copie de fichier pour MISS :
  source      : %(k1)s
  destination : %(k2)s
"""),

7 : _(u"""
Erreur lors de la lecture du fichier de résultat Aster
à la ligne numéro %(i1)d.

Message d'erreur :
%(k1)s
"""),

8 : _(u"""
Les données lues dans le fichier de résultat Aster ne sont pas cohérentes.
La trace ci-dessous doit montrer l'incohérence relevée.

Message d'erreur :
%(k1)s
"""),

9 : _(u"""
Les abscisses de l'accélérogramme '%(k1)s' doivent être à pas constant.
"""),

10 : (u"""
Interpolation des accélérogrammes sur l'intervalle : [%(r1).4f, %(r2).4f]
par pas de %(r3).4f, soit %(i1)d instants.
"""),

11 : _(u"""
Les %(i1)d fréquences du calcul harmonique sont :
    %(k1)s
"""),

12 : _(u"""
Plage de fréquence du calcul harmonique : [%(r1).4f, %(r2).4f]
par pas de %(r3).4f Hz, soit %(i1)d fréquences.
"""),

13 : _(u"""
Plage de fréquence du calcul Miss : [%(r1).4f, %(r2).4f]
par pas de %(r3).4f Hz, soit %(i1)d fréquences.
"""),

14 : _(u"""
Les %(i1)d fréquences du calcul Miss sont :
    %(k1)s
"""),

15 : _(u"""
L'utilisation de MACRO_MISS_3D est déconseillée et maintenant exclue du périmètre
qualifié de Code_Aster.

Certaines fonctionnalités ne sont pas encore disponibles dans CALC_MISS.
Pour celles-ci, il faut encore utiliser MACRO_MISS_3D. Il s'agit :
    - des ondes inclinées,
    - des sources ponctuelles,
    - de l'interaction sol, fluide, structure.
"""),

16 : _(u"""
Dans le cas présent (MODULE='MISS_IMPE' et ISSF='NON'), CALC_MISS répond à votre besoin.
Nous vous conseillons d'utiliser dorénavant CALC_MISS.
"""),

17 : _(u"""
Fournir une plage de fréquence (mot-clé LIST_FREQ) n'est possible que si
TYPE_RESU = 'FICHIER', 'HARM_GENE' ou 'TABLE_CONTROL'.
Dans les autres cas, il est nécessaire d'avoir un pas de fréquences constant
pour le calcul des FFT.
"""),

18 : _(u"""
Le nombre de pas de temps (calculé avec INST_FIN et INST_PAS) n'est pas pair.
Il faut donc corriger ces valeurs pour respecter cette condition.
"""),

19 : _(u"""
Dans le cas présent (DECOMP_IMPE='SANS_PRODUIT'), 
les valeurs lues par les mots-clés MATR_MASS et AMOR_HYST (tous les deux sous MATR_GENE) 
ne sont pas utilisées.
De plus, le fichier UNITE_RESU_MASS ne sera pas crée.
"""),

20 : _(u"""
La matrice d'impédance correspondant à l'instant t = 0 n'est pas définie positive.
La liste de DDL problématique(s) est : %(k1)s
Il faut donc, soit :
- augmenter la valeur du mot-clé INST_PAS,
- diminuer la taille des éléments du maillage de l'interface ISS,
- bloquer ce ou ces DDL.
"""),

21 : _(u"""
Il faut au moins une couche avec EPAIS.
"""),

22 : _(u"""
En interaction sol, structure, fluide (ISSF='OUI'), les mots-clés
GROUP_MA_FLU_STR, GROUP_MA_FLU_SOL, GROUP_MA_SOL_SOL sont tous les trois obligatoires.
"""),

23 : _(u"""Le calcul d'interaction sol, structure, fluide (ISSF='OUI')
n'est pas compatible avec le post-traitement aux points de contrôle
(présence de GROUP_MA_CONTROL).
"""),

}
