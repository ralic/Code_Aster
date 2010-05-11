#@ MODIF table0 Messages  DATE 10/05/2010   AUTEUR LEBOUVIER F.LEBOUVIER 
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

1 : _("""
Erreur dans les données. Le paramètre %(k1)s n'existe pas dans la table.
"""),

2 : _("""
Paramètre %(k1)s inexistant dans la table %(k2)s.
"""),

3 : _("""
Opération RENOMME. Erreur : %(k1)s
"""),

6 : _("""
Le fichier %(k1)s existe déjà, on écrit à la suite.
"""),

7 : _("""
Paramètre absent de la table : %(k1)s.
"""),

8 : _("""
Paramètres absents de la table (ou de NOM_PARA) : %(k1)s.
"""),

10 : _("""
NUME_TABLE=%(i1)d incorrect : il n'y a que %(i2)d blocs de tables dans le fichier.
"""),

11 : _("""
Nombre de champs incorrect ligne %(i1)d.
"""),

12 : _("""
On attend %(i1)d paramètres.
"""),

13 : _("""
On a lu %(i1)d champs dans le fichier.
"""),

14 : ("""
Les listes NOM_PARA et VALE doivent avoir le meme cardinal.
"""),

15 : ("""
Les listes DEFA et PARA_NOCI doivent avoir le meme cardinal.
"""),

16:_("""
L'objet %(k1)s à l'instant %(r1)f existe déjà dans la table fournie.
On l'écrase pour le remplacer par le nouveau.
"""),

20 : _("""Erreur lors de la construction des n-uplets
"""),

21 : _("""La table doit avoir exactement deux paramètres pour une impression au format XMGRACE.
"""),

22 : _("""Les cellules ne doivent contenir que des nombres réels
"""),

23 : _("""Le paramètre %(k1)s est en double.
"""),

24 : _("""Le parametre %(k1)s existe déjà.
"""),

25 : _("""(fromfunction) '%(k1)s' n'a pas d'attribut '__call__'.
"""),

26 : _("""(fromfunction) '%(k1)s' n'a pas d'attribut 'nompar'.
"""),

27 : _("""Le (ou les) paramètre(s) n'existe(nt) pas dans la table : %(k1)s
"""),

28 : _("""(fromfunction) L'argument 'const' doit etre de type 'dict'.
"""),

29 : _("""Valeur incorrecte pour ORDRE : %(k1)s
"""),

30 : _("""Les paramètres doivent être les mêmes dans les deux tables pour
faire l'intersection  ou l'union (opérateurs &, |).
"""),

31 : _("""Type du paramètre '%(k1)s' non défini.
"""),

32 : _("""Type du paramètre '%(k1)s' forcé à '%(k2)s'.
"""),

33 : _("""Erreur pour le paramètre '%(k1)s' :
   %(k2)s
"""),

34 : _("""La colonne '%(k1)s' est vide.
"""),

35 : _("""La table est vide !
"""),

36 : _("""La table doit avoir exactement trois paramètres.
"""),

37 : _("""
   La table %(k1)s n'existe pas dans le résultat %(k2)s.
"""),
}
