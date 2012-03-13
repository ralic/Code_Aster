#@ MODIF jeveux1 Messages  DATE 12/03/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
# -*- coding: iso-8859-1 -*-
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""

 L'attribut %(k1)s est non modifiable ou déjà défini.

"""),

2 : _(u"""

 L'attribut %(k1)s est non modifiable ou déjà défini pour un objet simple.

"""),

3 : _(u"""

 L'attribut %(k1)s n'est pas compatible avec la valeur de LONT.

"""),

4 : _(u"""

 L'attribut %(k1)s n'est pas accessible ou non modifiable.

"""),

5 : _(u"""

 Pour une collection contiguë, il faut définit %(k1)s dans l'ordre de création des objets.

"""),

6 : _(u"""

 L'attribut %(k1)s n'est pas modifiable ou déjà défini (attribut LONO non nul).

"""),

7 : _(u"""

 L'attribut %(k1)s est incompatible avec la valeur initiale de LONT.

"""),

8 : _(u"""

 Le premier argument %(k1)s n'est pas du bon type (différent de CHARACTER).

"""),

9 : _(u"""

 L'appel est invalide pour l'objet simple "%(k1)s".

"""),

10 : _(u"""

 Le nom de l'attribut est incompatible avec le genre %(k1)s.

"""),

11 : _(u"""

 La longueur ou la position de la sous chaîne %(k1)s est invalide.

"""),

12 : _(u"""

 L'objet %(k1)s n'est pas de genre "N" répertoire de noms, la requête JENUNO est invalide.

"""),

13 : _(u"""

 Le répertoire de noms %(k1)s contient %(i1)d points d'entrée, la requête JENUNO
 sur le numéro %(i2)d est invalide.

"""),

14 : _(u"""

 La collection %(k1)s ne possède pas de pointeur de noms, la requête JENUNO est invalide.

"""),

15 : _(u"""

 Nom de classe %(k1)s invalide.

"""),

16 : _(u"""

 Nom d'objet attribut %(k1)s invalide.

"""),

17 : _(u"""

 Nom d'attribut %(k1)s invalide.

"""),

18 : _(u"""

 L'impression de l'attribut %(k1)s est invalide. L'objet %(k2)s n'est pas une collection.

"""),

19 : _(u"""

 Le segment de valeurs associé à l'attribut %(k1)s n'est pas accessible en mémoire (adresse nulle).

"""),

20 : _(u"""

 L'accès au répertoire de noms %(k1)s est invalide.

"""),

21 : _(u"""

 L'accès à la collection dispersée %(k1)s n'est pas valide en bloc, il faut y accéder avec un nom ou un
 numéro d'objet de collection.

"""),

22 : _(u"""

 L'objet de la collection %(k1)s contiguë est de longueur nulle.

"""),

27 : _(u"""

 Le paramètre d'accès %(r1)f est invalide, la valeur doit être E ou L.

"""),

28 : _(u"""

 La valeur de l'attribut %(k1)s est invalide, la valeur doit être LONCUM.

"""),

29 : _(u"""

 Cette requête n'est valide que sur une collection contiguë.

"""),

30 : _(u"""

 L'attribut LONCUM n'est valide que sur une collection contiguë.

"""),

31 : _(u"""

 La liste de paramètres de création d'objet est incomplète.

"""),

32 : _(u"""

 La liste de paramètres de création d'objet contient des champs superflus.

"""),

33 : _(u"""

 Le répertoire de noms %(k1)s est saturé, il faut le redimensionner.

"""),

34 : _(u"""

 Le nom %(k1)s est introuvable dans le répertoire de noms %(k2)s.

"""),

35 : _(u"""

 Le nom %(k1)s existe déjà dans le répertoire de noms %(k2)s.

"""),

36 : _(u"""

 Impossible d'insérer le nom %(k1)s dans le répertoire de noms %(k2)s, il y trop de collisions avec
 la fonction de hashage.

"""),

38 : _(u"""

 Un objet de genre N (répertoire de noms) doit être de type K (caractère).

"""),

39 : _(u"""

 Il faut définir la longueur du type caractère, par exemple K8 ou K32.

"""),

40 : _(u"""

 La longueur du type caractère vaut %(i1)d, elle doit être comprise entre 1 et 512 .

"""),

41 : _(u"""

 Pour un objet de genre N (répertoire de noms), la longueur du type caractère
 vaut %(i1)d, elle n'est pas un multiple de 8.

"""),

42 : _(u"""

 Pour un objet de genre N (répertoire de noms), la longueur du type caractère
 vaut %(i1)d, elle ne peut être supérieure à 24.

"""),

43 : _(u"""

 Le type de l'objet %(k1)s est invalide, il peut valoir K, S, I, R, C ou L.

"""),

44 : _(u"""

 Pour une collection nommée, la création d'objet est uniquement autorisée par nom.

"""),

45 : _(u"""

 L'objet de collection %(i1)d existe déjà.

"""),

46 : _(u"""

 Il est impossible de créer l'objet de collection, le répertoire est saturé.

"""),

47 : _(u"""

 L'accès par nom à une collection numérotée est impossible.

"""),

48 : _(u"""

 Une erreur d'écriture de l'attribut %(k1)s au format HDF s'est produite, l'exécution continue.

"""),

49 : _(u"""

 Un écrasement de l'identificateur de l'objet est détecté, sa valeur ne peut pas être nulle.

"""),

50 : _(u"""

 Un écrasement de la classe de l'objet est détecté, sa valeur %(i1)d est invalide.

"""),

51 : _(u"""

 Un écrasement de la classe de l'objet est détecté, sa valeur %(k1)s est invalide.

"""),

52 : _(u"""

  Il est impossible d'accéder au DATASET HDF associé à %(k1)s.

"""),

54 : _(u"""

  Un écrasement amont est détecté, la zone mémoire (adresse %(i1)d) a été utilisée devant l'adresse autorisée %(i1)d.

"""),

55 : _(u"""

  Un écrasement aval est détecté, la zone mémoire (adresse %(i1)d) a été utilisée au-delà de la longueur autorisée.

"""),

56 : _(u"""

  La structure du nom de l'objet est invalide au-delà des 24 premiers caractères, elle vaut %(k1)s.

"""),

57 : _(u"""

  La structure du nom de l'objet est invalide, elle vaut %(k1)s.

"""),

58 : _(u"""

  La structure du nom de l'objet est invalide, le caractère %(k1)s est illicite.

"""),

59 : _(u"""

  L'objet ne possède pas d'image disque (adresse disque nulle).

"""),

60 : _(u"""

  L'objet de type K (chaîne de caractères) est déjà alloué en mémoire, il n'est pas possible de le déplacer sans l'avoir auparavant libéré.

"""),

61 : _(u"""

  L'objet n'est pas en mémoire et ne possède pas d'image disque (adresse disque nulle).

"""),

62 : _(u"""

  La longueur des objets de collection constante n'a pas été définie.

"""),

63 : _(u"""

 L'attribut %(k1)s n'est pas accessible pour cette collection.

"""),

64 : _(u"""

 Le volume des données temporaires (objets de la base Volatile) écrites sur disque (%(r3).2f Mo)
 est plus de %(r1).2f fois supérieur au volume de données lues (%(r2).2f Mo).

"""),

65 : _(u"""

 Le segment de valeurs associé à l'objet %(i1)d de la collection %(k1)s ne possède ni adresse mémoire, ni adresse disque.

"""),


66 : _(u"""

 Le segment de valeurs associé à l'objet simple %(k1)s ne possède ni adresse mémoire, ni adresse disque.

"""),

67 : _(u"""

 La valeur %(i1)d affectée à l'attribut %(k1)s est invalide.

"""),

68 : _(u"""

 L'accès à l'objet simple %(k1)s par la fonction JEXNOM ou JEXNUM est invalide. Il faut que l'objet simple soit de genre répertoire de noms.
 
"""),

69 : _(u"""

 Le nom de répertoire associé à la base Globale est trop long %(k1)s, il comporte %(i1)d caractères, il ne doit pas dépasser 119.
 
"""),

70 : _(u"""

 Le nom de répertoire associé à la base Volatile est trop long %(k1)s, il comporte %(i1)d caractères, il ne doit pas dépasser 119.
 
"""),}
