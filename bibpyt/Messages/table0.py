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

1 : _(u"""
Erreur dans les données. Le paramètre %(k1)s n'existe pas dans la table.
"""),

2 : _(u"""
Paramètre %(k1)s inexistant dans la table %(k2)s.
"""),

3 : _(u"""
Opération RENOMME. Erreur : %(k1)s
"""),

6 : _(u"""
Le fichier %(k1)s existe déjà, on écrit à la suite.
"""),

7 : _(u"""
Paramètre absent de la table : %(k1)s.
"""),

8 : _(u"""
Paramètres absents de la table (ou de NOM_PARA) : %(k1)s.
"""),

10 : _(u"""
NUME_TABLE=%(i1)d incorrect : il n'y a que %(i2)d blocs de tables dans le fichier.
"""),

11 : _(u"""
Nombre de champs incorrect ligne %(i1)d.
"""),

12 : _(u"""
On attend %(i1)d paramètres.
"""),

13 : _(u"""
On attend %(i1)d champs dans le fichier.
"""),

14 : (u"""
Les listes %(k1)s et %(k2)s doivent avoir le même cardinal.
"""),

15 : _(u"""
Le format de la ligne semble incorrect.
Ligne lue :
    %(k1)s

Il ne satisfait pas l'expression régulière :
    %(k2)s
"""),

16:_(u"""
L'objet %(k1)s à l'instant %(r1)f existe déjà dans la table fournie.
On l'écrase pour le remplacer par le nouveau.
"""),

20 : _(u"""Erreur lors de la construction des n-uplets
"""),

21 : _(u"""La table doit avoir exactement deux paramètres pour une impression au format XMGRACE.
"""),

22 : _(u"""Les cellules ne doivent contenir que des nombres réels
"""),

23 : _(u"""Le paramètre %(k1)s est en double.
"""),

24 : _(u"""Le paramètre %(k1)s existe déjà.
"""),

25 : _(u"""'%(k1)s' n'a pas d'attribut '%(k2)s'.
"""),

27 : _(u"""Les paramètres n'existent pas dans la table : %(k1)s
"""),

28 : _(u"""L'argument '%(k1)s' doit être de type '%(k2)s'.
"""),

29 : _(u"""Valeur incorrecte pour ORDRE : %(k1)s
"""),

30 : _(u"""Les paramètres doivent être les mêmes dans les deux tables pour
faire l'intersection  ou l'union (opérateurs &, |).
"""),

31 : _(u"""Type du paramètre '%(k1)s' non défini.
"""),

32 : _(u"""Type du paramètre '%(k1)s' forcé à '%(k2)s'.
"""),

33 : _(u"""Erreur pour le paramètre '%(k1)s' :
   %(k2)s
"""),

34 : _(u"""La colonne '%(k1)s' est vide.
"""),

35 : _(u"""La table est vide !
"""),

36 : _(u"""La table doit avoir exactement trois paramètres.
"""),

37 : _(u"""
   La table %(k1)s n'existe pas dans le résultat %(k2)s.
"""),

38 : _(u"""Champ %(k1)s inexistant à l'ordre %(i1)d .
"""),

39 : _(u"""
Aucun numéro d'ordre associé à l'accès %(k1)s de valeur %(i1)d
Veuillez vérifier vos données.
"""),

40 : _(u"""
Aucun numéro d'ordre associé à l'accès %(k1)s de valeur %(r1)f
Veuillez vérifier vos données.
"""),

41 : _(u"""
Les mots-clés 'NOEUD' et 'GROUP_NO' ne sont pas autorisés pour
les champs élémentaires (ELNO/ELGA).
"""),

42 : _(u"""
Développement non réalisé pour les champs dont les valeurs sont complexes.
"""),

43 : _(u"""
Lecture des noms de paramètres.
On attend %(i1)d noms et on a lu cette ligne :

%(k1)s

Conseil : Vérifier que le séparateur est correct et le format du fichier
    à lire.
"""),

44 : _(u"""
La table '%(k1)s' est composée de %(i1)d lignes x %(i2)d colonnes.

Son titre est :
%(k2)s
"""),

45 : _(u"""
Erreur de type lors de la lecture de la table :

Exception : %(k1)s

Conseil :
    Cette erreur se produit quand on relit au FORMAT='TABLEAU' une table qui a été
    imprimée au FORMAT='ASTER' car la deuxième ligne contient des types et non des valeurs.
    Si c'est le cas, utilisez LIRE_TABLE au FORMAT='ASTER'.
"""),

46 : _(u"""
Plusieurs numéros d'ordre sont associés à l'accès %(k1)s de valeur %(r1)f


Conseil :
    Vous pouvez modifier la recherche en renseignant le mot clé PRECISION.
"""),
}
