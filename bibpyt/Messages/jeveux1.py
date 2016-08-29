# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
L'attribut %(k1)s est non modifiable ou déjà défini.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    2 : _(u"""
L'attribut %(k1)s est non modifiable ou déjà défini pour un objet simple.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    3 : _(u"""
L'attribut %(k1)s n'est pas compatible avec la valeur de la longueur totale de la collection.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    4 : _(u"""
L'attribut %(k1)s n'est pas accessible ou non modifiable.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    5 : _(u"""
Pour une collection contiguë, il faut définit %(k1)s dans l'ordre de création des objets.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    6 : _(u"""
L'attribut %(k1)s n'est pas modifiable ou déjà défini (attribut non nul).
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    7 : _(u"""
L'attribut %(k1)s est incompatible avec la valeur initiale de la longueur totale.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    8 : _(u"""
Le premier argument %(k1)s n'est pas du bon type.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    9 : _(u"""
L'appel est invalide pour l'objet simple "%(k1)s".
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    10 : _(u"""
Le nom de l'attribut est incompatible avec le genre %(k1)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    11 : _(u"""
La longueur ou la position de la sous chaîne %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    12 : _(u"""
L'objet %(k1)s n'est pas de genre répertoire de noms, la requête est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    13 : _(u"""
Le répertoire de noms %(k1)s contient %(i1)d points d'entrée, la requête 
sur le numéro %(i2)d est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    14 : _(u"""
La collection %(k1)s ne possède pas de pointeur de noms, la requête est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    15 : _(u"""
Nom de classe %(k1)s invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    16 : _(u"""

 Nom d'objet attribut %(k1)s invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    17 : _(u"""
Nom d'attribut %(k1)s invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    18 : _(u"""
L'impression de l'attribut %(k1)s est invalide. L'objet %(k2)s n'est pas une collection.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    19 : _(u"""
Le segment de valeurs associé à l'attribut %(k1)s n'est pas accessible en mémoire (adresse nulle).
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    20 : _(u"""
L'accès au répertoire de noms %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    21 : _(u"""
L'accès à la collection dispersée %(k1)s n'est pas valide en bloc, il faut y accéder avec un nom ou un
 numéro d'objet de collection.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    22 : _(u"""
L'objet de la collection %(k1)s contiguë est de longueur nulle.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    23 : _(u"""
Le nom de l'attribut %(k1)s est invalide pour la requête.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    27 : _(u"""
Le paramètre d'accès %(r1)f est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    28 : _(u"""
La valeur de l'attribut %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    29 : _(u"""
Cette requête n'est valide que sur une collection contiguë.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    30 : _(u"""
L'attribut longueur cumulée n'est valide que sur une collection contiguë.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    31 : _(u"""
La liste de paramètres de création d'objet est incomplète.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    32 : _(u"""
La liste de paramètres de création d'objet contient des champs superflus.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    33 : _(u"""
Le répertoire de noms %(k1)s est saturé, il faut le redimensionner.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    34 : _(u"""
Le nom %(k1)s est introuvable dans le répertoire de noms %(k2)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    35 : _(u"""
Le nom %(k1)s existe déjà dans le répertoire de noms %(k2)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    36 : _(u"""
Impossible d'insérer le nom %(k1)s dans le répertoire de noms %(k2)s, il y trop de collisions avec
 la fonction de hashage.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    38 : _(u"""
Un objet de genre répertoire de noms doit être de type caractère.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    39 : _(u"""
Il faut définir la longueur du type caractère.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    40 : _(u"""
La longueur du type caractère vaut %(i1)d, elle doit être comprise entre 1 et 512 .
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    41 : _(u"""
Pour un objet de genre répertoire de noms, la longueur du type caractère
 vaut %(i1)d, elle n'est pas un multiple de 8.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    42 : _(u"""
Pour un objet de genre répertoire de noms, la longueur du type caractère
 vaut %(i1)d, elle ne peut être supérieure à 24.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    43 : _(u"""
Le type de l'objet %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    44 : _(u"""
Pour une collection nommée, la création d'objet est uniquement autorisée par nom.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    45 : _(u"""
L'objet de collection %(i1)d existe déjà.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    46 : _(u"""
Il est impossible de créer l'objet de collection, le répertoire est saturé.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    47 : _(u"""
L'accès par nom à une collection numérotée est impossible.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    48 : _(u"""
Une erreur d'écriture de l'attribut %(k1)s au format HDF s'est produite, l'exécution continue.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    49 : _(u"""
Un écrasement de l'identificateur de l'objet est détecté, sa valeur ne peut pas être nulle.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    50 : _(u"""
Un écrasement de la classe de l'objet est détecté, sa valeur %(i1)d est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    51 : _(u"""
Un écrasement de la classe de l'objet est détecté, sa valeur %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    52 : _(u"""
Il est impossible d'accéder au DATASET HDF associé à %(k1)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    54 : _(u"""
Un écrasement amont est détecté, la zone mémoire (adresse %(i1)d) a été utilisée devant l'adresse autorisée %(i1)d.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    55 : _(u"""
Un écrasement aval est détecté, la zone mémoire (adresse %(i1)d) a été utilisée
  au-delà de la longueur autorisée.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    56 : _(u"""
 La structure du nom de l'objet est invalide au-delà des 24 premiers caractères,
  elle vaut %(k1)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    57 : _(u"""
La structure du nom de l'objet est invalide, elle vaut %(k1)s.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    58 : _(u"""
La structure du nom de l'objet est invalide, le caractère %(k1)s est illicite.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    59 : _(u"""
L'objet ne possède pas d'image disque (adresse disque nulle).
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    60 : _(u"""

L'objet de type chaîne de caractères est déjà alloué en mémoire, il n'est pas
possible de le déplacer sans l'avoir auparavant libéré.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    61 : _(u"""
L'objet n'est pas en mémoire et ne possède pas d'image disque (adresse disque nulle).
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    62 : _(u"""
La longueur des objets de collection constante n'a pas été définie.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    63 : _(u"""
L'attribut %(k1)s n'est pas accessible pour cette collection.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    64 : _(u"""

 Le volume des données temporaires (objets de la base Volatile) écrites sur disque (%(r3).2f Mo)
 est plus de %(r1).2f fois supérieur au volume de données lues (%(r2).2f Mo).

Risques et conseils :
 Ce déséquilibre n'a pas de conséquence sur les résultats de calcul, il indique simplement que
 certaines structures de données temporaires ont été écrites sur disque et détruites sans avoir
 été relues. C'est le cas lorsque vous utilisez le solveur MUMPS, car certaines structures de
 données sont volontairement déchargées pour maximiser la mémoire lors de la résolution.

"""),

    65 : _(u"""
Le segment de valeurs associé à l'objet %(i1)d de la collection %(k1)s ne possède
 ni adresse mémoire, ni adresse disque.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),


    66 : _(u"""
Le segment de valeurs associé à l'objet simple %(k1)s ne possède ni adresse mémoire,
 ni adresse disque.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    67 : _(u"""
La valeur %(i1)d affectée à l'attribut %(k1)s est invalide.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    68 : _(u"""
L'accès à l'objet simple %(k1)s est invalide.
 Il faut que l'objet simple soit de genre répertoire de noms.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    69 : _(u"""
Le nom de répertoire associé à la base Globale est trop long %(k1)s,
 il comporte %(i1)d caractères, il ne doit pas dépasser 119.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    70 : _(u"""
 Le nom de répertoire associé à la base Volatile est trop long %(k1)s,
 il comporte %(i1)d caractères, il ne doit pas dépasser 119.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    71 : _(u"""
 La mémoire totale de %(r1).2f Mo allouée à l'étude est insuffisante, il est nécessaire
 de disposer d'au moins %(r3).2f Mo uniquement pour démarrer l'exécution.
"""),

    72 : _(u"""
 Il n'est pas possible de modifier la valeur limite totale de l'allocation dynamique JEVEUX.
 La valeur fournie en argument vaut %(r2).2f.
 Actuellement %(r1).2f Mo sont nécessaires au gestionnaire de mémoire.
"""),

    73 : _(u"""
  La mémoire consommée actuellement hors JEVEUX est de %(r1).2f Mo.
  La limite de l'allocation dynamique JEVEUX est fixée à %(r2).2f Mo.
  Cette valeur limite a été réactualisée entre les appels aux différentes commandes
"""),

    74 : _(u"""
  La mémoire consommée actuellement hors JEVEUX est de %(r1).2f Mo.
  La limite de l'allocation dynamique JEVEUX est fixée à %(r2).2f Mo.
  Cette valeur limite a été réactualisée lors de la mise en oeuvre d'un processus de libération
"""),

    75 : _(u"""
 La plate-forme utilisée ne permet pas d'avoir accès à la valeur de VmPeak.
"""),


    77 : _(u"""
 La mémoire demandée au lancement est sous estimée, elle est de %(r2).2f Mo.
 Le pic mémoire utilisée est de %(r1).2f Mo.

"""),

    78 : _(u"""
 La mémoire demandée au lancement est surestimée, elle est de %(r2).2f Mo.
 Le pic mémoire utilisée est de %(r1).2f Mo.

"""),
}
