#@ MODIF sdveri Messages  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
 composante 1 de    l objet mael_mass_desc non valide
"""),

2: _("""
 composante 2 de    l objet mael_mass_desc non valide
"""),

3: _("""
 composante 3 de    l objet mael_mass_desc non valide
"""),

4: _("""
 composante 1 de    l objet mael_desc non valide
"""),

5: _("""
 composante 2 de    l objet mael_desc non valide
"""),

6: _("""
 composante 3 de    l objet mael_desc non valide
"""),

7: _("""
 composante 1 de    l objet mael_raid_desc non valide
"""),

8: _("""
 composante 2 de    l objet mael_raid_desc non valide
"""),

9: _("""
 composante 3 de    l objet mael_raid_desc non valide
"""),

10: _("""
  erreur modg.desc comp 1
"""),

11: _("""
  erreur modg.desc comp 2
"""),

12: _("""
  erreur MODG.DESC comp 3
"""),

13: _("""
 composante 5 de  l'objet MODG.LIDF non valide
"""),

14: _("""
 matrice de liaison 1 de   l'objet MODG.LIMA non valide
"""),

15: _("""
 matrice de liaison 2 de   l'objet MODG.LIMA non valide
"""),

16: _("""
 matrice de liaison 3 de   l'objet MODG.LIMA non valide
"""),

17: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' a un LONMAX incorrect.
 LONMAX attendu : %(i1)d
 LONMAX trouvé  : %(i2)d
"""),

18: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 La collection JEVEUX '%(k1)s' n'a pas le bon nombre d'objets (NUTIOC)
"""),

19: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 Le  pointeur de noms JEVEUX '%(k1)s' n'a pas la bonne longueur (NOMUTI)
"""),

20: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' devrait etre de type entier
"""),

21: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' devrait etre de type réel
"""),

22: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' devrait etre de type complexe
"""),

23: _("""
 Erreur de programmation :
 L'objet JEVEUX '%(k1)s' est interdit dans les structures de données de type '%(k2)s'
 On le détruit.
"""),

24: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' devrait etre de type '%(k2)s'.
"""),

25: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 La vérification '%(k1)s' n'est pas programmée.
"""),

26: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet '%(k1)s' est obligatoire dans la SD mais il n'existe pas.
"""),

27: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' a un champ DOCU incorrect.
 Le champ DOCU attendu est : '%(k2)s'
 Le champ DOCU trouvé  est : '%(k3)s'
"""),

28: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 Le type de SD '%(k1)s' n'est pas encore programmé
"""),

29: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' a un type incorrect.
 Le type attendu est : '%(k2)s'
 Le type trouvé  est : '%(k3)s'
"""),

30: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 La Structure de donnée '%(k1)s' de type %(k2)s est obligatoire mais elle est vide
"""),

31: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' a un GENR incorrect.
 GENR attendu est : '%(k2)s'
 GENR trouvé  est : '%(k3)s'
"""),

32: _("""
 Erreur de programmation :
 Vérification d'une structure de donnée :
 L'objet JEVEUX '%(k1)s' a un XOUS incorrect.
 XOUS attendu est : '%(k2)s'
 XOUS trouvé  est : '%(k3)s'
"""),

31: _("""
 Erreur de programmation :
 Concept de type inconnu %(k1)s
"""),

32: _("""
 Erreur de programmation :
 Type de configuration inconnu pour le mot clé FAISCEAU_AXIAL
"""),

33: _("""
 Erreur de programmation :
 Type de spectre incorrect
"""),


}
