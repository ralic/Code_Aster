#@ MODIF sdveri Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
 Impossible d'importer le catalogue de la structure de données '%(k1)s'
"""),

17: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet JEVEUX '%(k1)s' a un LONMAX incorrect.
 LONMAX attendu : %(i1)d
 LONMAX trouvé  : %(i2)d
"""),

18: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 La collection JEVEUX '%(k1)s' n'a pas le bon nombre d'objets (NUTIOC)
"""),

19: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 Le  pointeur de noms JEVEUX '%(k1)s' n'a pas la bonne longueur (NOMUTI)
"""),

20: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet JEVEUX '%(k1)s' devrait etre de type entier
"""),

21: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet JEVEUX '%(k1)s' devrait etre de type réel
"""),

22: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet JEVEUX '%(k1)s' devrait etre de type complexe
"""),

24: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet JEVEUX '%(k1)s' devrait etre de type '%(k2)s'.
"""),

25: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 La vérification '%(k1)s' n'est pas programmée.
"""),

26: _("""
 Erreur de programmation :
 Vérification d'une structure de données :
 L'objet '%(k1)s' est obligatoire dans la SD mais il n'existe pas.
"""),

27: _("""
 Erreur de programmation :
   Vérification d'une structure de données :
   L'objet JEVEUX '%(k1)s' a un champ DOCU incorrect.
   Le champ DOCU attendu est : '%(k2)s'
   Le champ DOCU trouvé  est : '%(k3)s'
"""),


30: _("""
 Erreur de programmation (catalogue des SD) :
   Vérification d'une structure de donnée :
   certains objets JEVEUX sont incorrects :
"""),

31: _("""
      Objet : '%(k1)s'    Message : %(k2)s
"""),

32: _("""
      Fin Message 30
"""),

40: _("""
 Erreur de programmation (catalogue des SD) :
   Vérification d'une structure de donnée :
   Les objets suivants sont interdits dans les SD de type : %(k1)s
"""),

41: _("""
   Objet '%(k1)s'   INTERDIT
"""),

42: _("""
      Fin Message 40
"""),

45: _("""
      Erreur Python : voir traceback ci-dessous
"""),

}
