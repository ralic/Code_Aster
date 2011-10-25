#@ MODIF jeveux Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

2 : _(u"""
 Pointeur de longueur externe interdit maintenant.
"""),

3 : _(u"""
 Pointeur de nom externe interdit maintenant.
"""),

6 : _(u"""
 Erreur de programmation :

  Appel invalide, la marque devient négative
"""),

7 : _(u"""
 Destruction de  %(k1)s
"""),

8 : _(u"""
 La base  %(k1)s  a été constituée avec la version  %(k2)s
 et vous utilisez la version  %(k3)s
"""),

9 : _(u"""
 Suppression de la partition mémoire
"""),

10 : _(u"""
 Erreur de programmation :

 Le nom demandé existe déjà dans la base %(k1)s
"""),

11 : _(u"""
 Erreur lors de la fermeture de la base  %(k1)s
"""),

12 : _(u"""
 Fichier associé à la base  %(k1)s  inexistant
"""),

13 : _(u"""
 Erreur de lecture du 1er bloc de  %(k1)s
"""),

14 : _(u"""
 Erreur lors de la fermeture de  %(k1)s
"""),

15 : _(u"""
 Ecrasement amont, l'objet :< %(k1)s > est peut être écrasé"""),

16 : _(u"""
 Ecrasement aval, l'objet :< %(k1)s > est peut être écrasé
"""),

17 : _(u"""
 Chainage cassé après l'objet :  %(k1)s
"""),

18 : _(u"""
 Le segment de valeurs associé à l'objet : %(k1)s, n'existe pas en mémoire et
 l'objet ne possède pas d'image disque.
"""),

19 : _(u"""
 Le nom d'un objet JEVEUX ne doit pas commencer par un blanc.
"""),

21 : _(u"""

     RE-OUVERTURE DE LA BASE

     NOM DE LA BASE                          :  %(k1)s
     CREEE AVEC LA VERSION                   :  %(k2)s
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i4)d
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i5)d
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i6)d %%
"""),

22 : _(u"""

     FERMETURE DE LA BASE

     NOM DE LA BASE                          :  %(k1)s
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d
     NOMBRE TOTAL D'ACCES EN LECTURE         :  %(i4)d
     VOLUME DES ACCES EN LECTURE             :  %(r1)12.2f Mo.
     NOMBRE TOTAL D'ACCES EN ECRITURE        :  %(i5)d
     VOLUME DES ACCES EN ECRITURE            :  %(r2)12.2f Mo.
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i6)d
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i7)d
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i8)d %%
"""),

23 : _(u"""
     Nom de Collection ou de Répertoire de noms inexistant :  %(k1)s
"""),

24 : _(u"""
     JENONU : Collection ou Répertoire de noms  :  %(k1)s
     Il faut passer par JEXNOM,JEXNUM.
"""),

25 : _(u"""
     Nom de collection ou de répertoire inexistant : >%(k1)s<
"""),

26 : _(u"""
     Objet JEVEUX inexistant dans les bases ouvertes : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

27 : _(u"""
     Objet simple JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

28 : _(u"""
     Collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

29 : _(u"""
     Objet %(i1)d de collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
"""),

30 : _(u"""
     Objet de collection JEVEUX inexistant : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

31 : _(u"""
     Erreur programmeur :
     La routine JUVECA n'a pas prévu de re-dimensionner l'objet :%(k1)s
     de type :%(k2)s
"""),

32 : _(u"""
     Erreur allocation de segment de mémoire de longueur %(i1)d (entiers).
     Mémoire allouée insuffisante. Il est impossible de trouver un espace
     de longueur suffisante dans la segmentation mémoire. Cette erreur
     concerne la mémoire dite "statique".
     Fermeture des bases (glob.*) sur erreur
     Il faut relancer le calcul en augmentant la limite mémoire (paramètre
     -memjeveux_stat sur la ligne de commande).
"""),

33 : _(u"""
  Statistiques mémoire (Mo) : %(r9)9.2f / %(r5)9.2f / %(r2)9.2f (VmPeak / Optimum / Minimum)
"""),

34 : _(u"""
  Statistiques mémoire (Mo) : %(r5)9.2f / %(r2)9.2f (Optimum / Minimum)
"""),

36 : _(u"""
     Le nombre d'enregistrements maximum de la base %(k1)s sera modifié
     de %(i1)d a %(i2)d
"""),

37 : _(u"""
     La valeur du rapport entre partitions ne convient pas,
     la longueur de la partition 1 doit etre au minimum de %(i1)d mots
     soit environ %(i2)d %%
"""),

38 : _(u"""
     Numero d'objet invalide %(i1)d
"""),

39 : _(u"""
     Taille de repertoire demandé trop grande.
     Le maximun est de %(i1)d
     La valeur reclamé est de %(i2)d

"""),

40 : _(u"""
     Erreur écriture de l'enregistrement %(i1)d sur la base : %(k1)s %(i2)d
     code retour WRITDR : %(i3)d
     Erreur probablement provoquée par une taille trop faible du répertoire de travail.
"""),

41 : _(u"""
     Erreur lecture de l'enregistrement %(i1)d sur la base : %(k1)s %(i2)d
     code retour READDR : %(i3)d
"""),

42 : _(u"""
     Fichier saturé, le nombre maximum d'enregistrement %(i1)d de la base %(k1)s est atteint
     il faut relancer le calcul en passant une taille maximum de base sur la ligne de commande
     argument "-max_base" suivi de la valeur en Mo.
"""),

43 : _(u"""
     Erreur d'ouverture du fichier %(k1)s , code retour OPENDR = %(i1)d
"""),

44 : _(u"""
 Taille des segments de valeurs %(i1)d
"""),

45 : _(u"""
 Taille de la partition principale %(r1)g
"""),

47 : _(u"""
 Erreur lors de la relecture d'un enregistrement sur le fichier d'accès direct.
"""),

48 : _(u"""
 Erreur lors de l'écriture d'un enregistrement sur le fichier d'accès direct.
"""),

49 : _(u"""
 Taille de la zone à allouer invalide %(i1)d < 0 .
"""),

50 : _(u"""
 Allocation dynamique impossible.
"""),

51 : _(u"""
 Relecture au format HDF impossible.
"""),

52 : _(u"""
 Erreur de relecture des paramètres du dataset HDF.
"""),

53 : _(u"""
 Relecture au format HDF impossible.
"""),

54 : _(u"""
 Impossible d'ouvrir le fichier HDF %(k1)s.
"""),

55 : _(u"""
 Impossible de fermer le fichier HDF %(k1)s.
"""),

56 : _(u"""
 Fermeture du fichier HDF %(k1)s.
"""),

57 : _(u"""
 Longueur du segment de valeurs à allouer invalide %(i1)d.
  -> Risque & Conseil :
     Une valeur négative peut parfois provenir d'une valeur entière supérieure à la valeur de l'entier maximum sur
     une plate-forme 32 bits (2147483648).
"""),

58 : _(u"""
 Le répertoire est saturé.
"""),

59 : _(u"""
 Le nom demandé existe déjà dans le répertoire %(k1)s.

"""),

60 : _(u"""
 Erreur lors de l'allocation dynamique. Il n'a pas été possible d'allouer
 une zone mémoire de longueur %(i1)d (octets).
 La dernière opération de libération mémoire a permis de récupérer %(i2)d (octets).

"""),

62 : _(u"""
 Erreur lors de l'allocation dynamique. Il n'a pas été possible d'allouer
 une zone mémoire de longueur %(i1)d Mo, on dépasse la limite maximum
 fixée à %(i2)d Mo et on occupe déjà %(i3)d Mo.
 La dernière opération de libération mémoire a permis de récupérer %(i4)d Mo.

"""),

63 : _(u"""

 Critère de destruction du fichier (%(r2).2f %%) associé à la base %(k1)s dépassé %(r1).2f %%
 Nombre d'enregistrements utilisés : %(i1)d
 Volume disque occupé              : %(i2)d Mo.
 Nombre maximum d'enregistrements  : %(i3)d

"""),


64 : _(u"""

 ATTENTION la taille de répertoire de noms atteint %(i1)d pour la base %(k1)s.
 Il sera impossible de l'agrandir.
  -> Conseil :
     Il faut réduire le nombre de concepts sur la base GLOBALE en utilisant
     la commande DETRUIRE.

"""),

65 : _(u"""

 ATTENTION la taille de répertoire de noms atteint %(i1)d pour la base %(k1)s.
 Il sera impossible de l'agrandir.
  -> Conseil :
     Il y a trop d'objets créés sur la base VOLATILE, cela peut provenir d'une
     erreur dans la programmation de la commande.

"""),

66 : _(u"""

 La base au format HDF de nom %(k1)s ne peut être créée.
 La fonction HDFCRF renvoie un code retour : %(i1)d

"""),


67 : _(u"""

 Le nombre d'objets de la collection %(k1)s est inférieur ou égal à 0

"""),


68 : _(u"""

 Le fichier associé à la base demandée %(k1)s n'est pas ouvert.

"""),

69 : _(u"""

 Le nom %(k1)s est deja utilise pour un objet simple.

"""),

70 : _(u"""

 Le type de stockage %(k1)s de la collection est erroné.

"""),

71 : _(u"""

 La longueur variable pour la collection %(k1)s est incompatible avec le genre E.

"""),

72 : _(u"""

 La longueur du type caractère n'est pas valide pour la collection %(k1)s

"""),

73 : _(u"""

 Le nom %(k1)s du pointeur de longueurs est invalide.

"""),

74 : _(u"""

 Le pointeur de longueurs %(k1)s n'a pas été créé dans la bonne base.

"""),

75 : _(u"""

 Le pointeur de longueurs %(k1)s n'est pas de la bonne taille.

"""),

76 : _(u"""

 Le type du pointeur de longueurs %(k1)s n'est pas correct (différent de I).

"""),

77 : _(u"""

 Le nom du répertoire de noms %(k1)s est invalide.

"""),

78 : _(u"""

 Le répertoire de noms %(k1)s n'a pas été créé dans la bonne base.

"""),

79 : _(u"""

 Le répertoire de noms %(k1)s n'est pas de la bonne taille.

"""),

80 : _(u"""

 L'objet %(k1)s n'est pas un répertoire de noms.

"""),

81 : _(u"""

 Le type d'accès %(k1)s est inconnu.

"""),

82 : _(u"""

 Le type d'accès %(k1)s de la collection est erroné.

"""),

83 : _(u"""

 Le nom du pointeur d'accès %(k1)s est invalide.

"""),

84 : _(u"""
 La longueur du nom %(k1)s est invalide (> 24 caractères).

"""),

85 : _(u"""

 Le nom %(k1)s est deja utilise pour une collection.

"""),

86 : _(u"""

 La longueur du type caractère n'est pas définie pour l'objet %(k1)s

"""),

87 : _(u"""

 Un objet de genre répertoire (N) doit être de type caractère (K) %(k1)s

"""),

88 : _(u"""

 La longueur du type caractère %(k1)s n'est pas valide.

"""),

89 : _(u"""

 Un objet de genre répertoire doit être de type K de longueur multiple de 8 %(k1)s.

"""),

90 : _(u"""

 Un objet de genre répertoire doit être de type K de longueur inférieure ou égale à 24 %(k1)s.

"""),

91 : _(u"""

 Le type %(k1)s est invalide.

"""),

92 : _(u"""

 La longueur ou la position de la sous-chaîne %(k1)s est invalide.

"""),

93 : _(u"""

 Les longueurs des sous-chaînes %(k1)s sont différentes.

"""),

94 : _(u"""

 Les sous-chaînes %(k1)s sont identiques.

"""),

95 : _(u"""

 L'appel de JECROC par JEXNOM ou JEXNUM est obligatoire.

"""),

96 : _(u"""

 L'accès par JEXNUM est interdit %(k1)s.

"""),

97 : _(u"""

 Erreur lors de l'appel à JECROC %(k1)s.

"""),

98 : _(u"""

 L'attribut %(k1)s. est uniquement destiné aux collections contigues.

"""),

99 : _(u"""

 L'attribut est incompatible avec le genre %(k1)s.

"""),

}
