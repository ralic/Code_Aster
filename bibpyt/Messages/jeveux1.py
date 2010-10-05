#@ MODIF jeveux1 Messages  DATE 29/09/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

 L'attribut %(k1)s est non modifiable ou déjà défini.

"""),

2 : _("""

 L'attribut %(k1)s est non modifiable ou déjà défini pour un objet simple.

"""),

3 : _("""

 L'attribut %(k1)s n'est pas compatible avec la valeur de LONT.

"""),

4 : _("""

 L'attribut %(k1)s n'est pas accessible ou non modifiable.

"""),

5 : _("""

 Pour une collection contigüe, il faut définit %(k1)s dans l'ordre de création des objets.

"""),

6 : _("""

 L'attribut %(k1)s n'est pas modifiable ou déjà défini (attribut LONO non nul).

"""),

7 : _("""

 L'attribut %(k1)s est incompatible avec la valeur initiale de LONT.

"""),

8 : _("""

 Le premier argument %(k1)s n'est pas du bon type (différent de CHARACTER).

"""),

9 : _("""

 L'appel est invalide pour l'objet simple "%(k1)s".

"""),

10 : _("""

 Le nom de l'attribut est incompatible avec le genre %(k1)s.

"""),

11 : _("""

 La longueur ou la position de la sous-chaîne %(k1)s est invalide.

"""),

12 : _("""

 L'objet %(k1)s n'est pas de genre "N" répertoire de noms, la requête JENUNO est invalide.

"""),

13 : _("""

 Le répertoire de noms %(k1)s contient %(i1)d points d'entrée, la requête JENUNO
 sur le numéro %(i2)d est invalide.

"""),

14 : _("""

 La collection %(k1)s ne possède pas de pointeur de noms, la requête JENUNO est invalide.

"""),

15 : _("""

 Nom de classe %(k1)s invalide.

"""),

16 : _("""

 Nom d'objet attribut %(k1)s invalide.

"""),

17 : _("""

 Nom d'attribut %(k1)s invalide.

"""),

18 : _("""

 L'impression de l'attribut %(k1)s est invalide. L'objet %(k2)s n'est pas une collection.

"""),

19 : _("""

 Le segment de valeurs associé à l'attribut %(k1)s n'est pas accessible en mémoire (adresse nulle).

"""),

20 : _("""

 L'accès au répertoire de noms %(k1)s est invalide.

"""),

21 : _("""

 L'accès à la collection dispersée %(k1)s n'est pas valide en bloc, il faut y accèder avec un nom ou un 
 numéro d'objet de collection.

"""),

22 : _("""

 L'objet de la collection %(k1)s contigue est de longueur nulle.

"""),

23 : _("""

 Le type de recherche %(k1)s invalide.
 
"""),

24 : _("""

 La taille des segments de valeurs %(i1)d invalide.
 
"""),

25 : _("""

 La taille de la partition %(r1)f invalide.
 
"""),

26 : _("""

 Le type de parcours de la segmentation mémoire %(r1)f est invalide, les valeurs possibles sont 1, 2, 3 ou 4.
 
"""),

27 : _("""

 Le paramètre d'accès %(r1)f est invalide, la valeur doit être E ou L.
 
"""),

28 : _("""

 La valeur de l'attribut %(k1)s est invalide, la valeur doit être LONCUM.
 
"""),

29 : _("""

 Cette requête n'est valide que sur une collection contigue.
 
"""),

30 : _("""

 L'attribut LONCUM n'est valide que sur une collection contigue.
 
"""),

31 : _("""

 La liste de paramètres de création d'objet est incomplète.
 
"""),

32 : _("""

 La liste de paramètres de création d'objet contient des champs superflus.
 
"""),

33 : _("""

 Le répertoire de noms %(k1)s est saturé, il faut le redimensionner.
 
"""),

34 : _("""

 Le nom %(k1)s est introuvable dans le répertoire de noms %(k2)s.
 
"""),

35 : _("""

 Le nom %(k1)s existe déjà dans le répertoire de noms %(k2)s.
 
"""),

36 : _("""

 Impossible d'insérer le nom %(k1)s dans le répertoire de noms %(k2)s, il y trop de collisions avec
 la fonction de hcoding.
 
"""),

37 : _("""

 La valeur du rapport entre les partitions est invalide, (%r1)f n'est pas comprise entre 0.0 et 1.0.
 
"""),

38 : _("""

 Un objet de genre N (répertoire de noms) doit être de type K (caractère).
 
"""),

39 : _("""

 Il faut définir la longueur du type caractère, par exemple K8 ou K32.
 
"""),

40 : _("""

 La longueur du type caractère vaut %(i1)d, elle doit être comprise entre 1 et 512 .
 
"""),

41 : _("""

 Pour un objet de genre N (répertoire de noms), la longueur du type caractère
 vaut %(i1)d, elle n'est pas un multiple de 8.
 
"""),

42 : _("""

 Pour un objet de genre N (répertoire de noms), la longueur du type caractère
 vaut %(i1)d, elle ne peut être supérieure à 24.
 
"""),

43 : _("""

 Le type de l'objet %(k1)s est invalide, il peut valoir K, S, I, R, C ou L.
 
"""),

44 : _("""

 Pour une collection nommée, la création d'objet est uniquement autorisée par nom.
 
"""),

45 : _("""

 L'objet de collection %(i1)d existe déjà.
 
"""),

46 : _("""

 Il est impossible de créer l'objet de collection, le répertoire est saturé.
 
"""),

47 : _("""

 L'accès par nom à une collection numérotée est impossible.
 
"""),

48 : _("""

 Une erreur d'écriture de l'attribut %(k1)s au format HDF s'est produite, l'exécution continue.
 
"""),

49 : _("""

 Un écrasement de l'identificateur de l'objet est détecté, sa valeur ne peut pas être nulle. 
 
"""),

50 : _("""

 Un écrasement de la classe de l'objet est détecté, sa valeur %(i1)d est invalide. 
 
"""),

51 : _("""

 Un écrasement de la classe de l'objet est détecté, sa valeur %(k1)s est invalide. 
 
"""),

52 : _("""

  Il est impossible d'accèder au dataset hdf associé à %(k1)s. 
 
"""),

53 : _("""

  La zone mémoire à libérer est déjà marquée libre. 
 
"""),

54 : _("""

  Un écrasement amont est détecté, la zone mémoire (adresse %(i1)d) a été utilisée devant l'adresse autorisée %(i1)d. 
 
"""),

55 : _("""

  Un écrasement aval est détecté, la zone mémoire (adresse %(i1)d) a été utilisée au-delà de la longueur autorisée. 
 
"""),

56 : _("""

  La structure du nom de l'objet est invalide au-delà des 24 premiers caractères, elle vaut %(k1)s. 
 
"""),

57 : _("""

  La structure du nom de l'objet est invalide, elle vaut %(k1)s. 
 
"""),

58 : _("""

  La structure du nom de l'objet est invalide, le caractère %(k1)s est illicite. 
 
"""),

59 : _("""

  L'objet ne possède pas d'image disque (adresse disque nulle). 
 
"""),

60 : _("""

  L'objet de type K (chaîne de caractères) est déjà alloué en mémoire, il n'est pas possible de le déplacer sans l'avoir aupravant libéré. 
 
"""),

61 : _("""

  L'objet n'est pas en mémoire et ne possède pas d'image disque (adresse disque nulle). 
 
"""),

62 : _("""

  La longueur des objets de collection constante n'a pas été définie. 
 
"""),

63 : _("""

 L'attribut LONCUM n'est pas accessible pour cette collection.
 
"""),

64 : _("""

 Le volume des données temporaires (objets de la base Volatile) écrites sur disque (%(r3).2f Mo)  
 est plus de %(r1).2f fois supérieur au volume de données lues (%(r2).2f Mo). 
  
"""),

}
