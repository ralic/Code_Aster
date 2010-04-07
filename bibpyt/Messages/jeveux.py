#@ MODIF jeveux Messages  DATE 06/04/2010   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
  %(k1)s
"""),

2 : _("""
 Pointeur de longueur externe interdit maintenant.
"""),

3 : _("""
 Pointeur de nom externe interdit maintenant.
"""),

6 : _("""
 Erreur de programmation :
 
  Appel invalide, la marque devient négative
"""),

7 : _("""
 Destruction de  %(k1)s
"""),

8 : _("""
 La base  %(k1)s  a été constituée avec la version  %(k2)s
 et vous utilisez la version  %(k3)s
"""),

9 : _("""
 Suppression de la partition mémoire
"""),

10 : _("""
 Erreur de programmation :

 Le nom demandé existe déjà dans la base %(k1)s
"""),

11 : _("""
 Erreur lors de la fermeture de la base  %(k1)s
"""),

12 : _("""
 Fichier associé à la base  %(k1)s  inexistant
"""),

13 : _("""
 Erreur de lecture du 1er bloc de  %(k1)s
"""),

14 : _("""
 Erreur lors de la fermeture de  %(k1)s
"""),

15 : _("""
 Ecrasement amont, l'objet :< %(k1)s > est peut être écrasé"""),

16 : _("""
 Ecrasement aval, l'objet :< %(k1)s > est peut être écrasé
"""),

17 : _("""
 Chainage cassé après l'objet :  %(k1)s
"""),

18 : _("""
 Le segment de valeurs associé à l'objet : %(k1)s, n'existe pas en mémoire et
 l'objet ne possède pas d'image disque. 
"""),

19 : _("""
 Le nom d'un objet JEVEUX ne doit pas commencer par un blanc.
"""),

21 : _("""

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

22 : _("""

     FERMETURE DE LA BASE

     NOM DE LA BASE                          :  %(k1)s
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d
     NOMBRE TOTAL D'ACCES EN LECTURE         :  %(i4)d
     VOLUME DES ACCES EN LECTURE             :  %(r1).2f Mo.
     NOMBRE TOTAL D'ACCES EN ECRITURE        :  %(i5)d
     VOLUME DES ACCES EN ECRITURE            :  %(r2).2f Mo.
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i6)d
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i7)d
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i8)d %%
"""),

23 : _("""
     Nom de Collection ou de Répertoire de noms inexistant :  %(k1)s
"""),

24 : _("""
     JENONU : Collection ou Répertoire de noms  :  %(k1)s
     Il faut passer par JEXNOM,JEXNUM.
"""),

25 : _("""
     Nom de collection ou de répertoire inexistant : >%(k1)s<
"""),

26 : _("""
     Objet JEVEUX inexistant dans les bases ouvertes : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

27 : _("""
     Objet simple JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

28 : _("""
     Collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

29 : _("""
     Objet %(i1)d de collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
"""),

30 : _("""
     Objet de collection JEVEUX inexistant : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

31 : _("""
     Erreur programmeur :
     La routine JUVECA n'a pas prévu de re-dimensionner l'objet :%(k1)s
     de type :%(k2)s
"""),

32 : _("""
     Erreur allocation de segment de mémoire de longueur %(i1)d (entiers).
     Mémoire allouée insuffisante. Il est impossible de trouver un espace
     de longueur suffisante dans la segmentation mémoire. Cette erreur 
     concerne la mémoire dite "statique".
     Fermeture des bases (glob.*) sur erreur
     Il faut relancer le calcul en augmentant la limite mémoire (paramètre 
     -memjeveux_stat sur la ligne de commande). 
"""),

33 : _("""
     Modification de l'environnement JEVEUX.
     Allocation dynamique des segments de valeurs de taille supérieure
     a %(i1)d (entiers) 
"""),

34 : _("""
     Modification de l'environnement JEVEUX.
     Mode debug positionne a %(i1)d
"""),

36 : _("""  
     Le nombre d'enregistrements maximum de la base %(k1)s sera modifié
     de %(i1)d a %(i2)d
"""),

37 : _("""
     La valeur du rapport entre partitions ne convient pas, 
     la longueur de la partition 1 doit etre au minimum de %(i1)d mots 
     soit environ %(i2)d %%
"""),

38 : _("""
     Numero d'objet invalide %(i1)d 
"""),

39 : _("""
     Taille de repertoire demandé trop grande.
     Le maximun est de %(i1)d
     La valeur reclamé est de %(i2)d 
      
"""),

40 : _("""
     Erreur ériture de l'enregistrement %(i1)s sur la base : %(k1)s %(i2)d
     code retour WRITDR : %(i3)d 
     Erreur probablement provoquée par une taille trop faible du répertoire de travail.
"""),

41 : _("""
     Erreur lecture de l'enregistrement %(i1)d sur la base : %(k1)s %(i2)d
     code retour READDR : %(i3)d 
"""),

42 : _("""
     Fichier saturé, le nombre maximum d'enregistrement %(i1)d de la base %(k1)s est atteint
     il faut relancer le calcul en passant une taille maximum de base sur la ligne de commande 
     argument "-max_base" suivi de la valeur en Mo.
"""),

43 : _("""
     Erreur d'ouverture du fichier %(k1)s , code retour OPENDR = %(i1)d 
"""),

44 : _("""
 Taille des segments de valeurs %(i1)d 
"""),

45 : _("""
 Taille de la partition principale %(r1)g 
"""),

47 : _("""
 Erreur lors de la relecture d'un enregistrement sur le fichier d'accès direct.
"""),

48 : _("""
 Erreur lors de l'écriture d'un enregistrement sur le fichier d'accès direct.
"""),

49 : _("""
 Taille de la zone à allouer invalide %(i1)d < 0 .
"""),

50 : _("""
 Allocation dynamique impossible.
"""),

51 : _("""
 Relecture au format HDF impossible.
"""),

52 : _("""
 Erreur de relecture des paramètres du dataset HDF.
"""),

53 : _("""
 Relecture au format HDF impossible.
"""),

54 : _("""
 Impossible d'ouvrir le fichier HDF %(k1)s. 
"""),

55 : _("""
 Impossible de fermer le fichier HDF %(k1)s. 
"""),

56 : _("""
 Fermeture du fichier HDF %(k1)s. 
"""),

57 : _("""
 Longueur du segment de valeurs à allouer invalide %(i1)d.
"""),

58 : _("""
 Le répertoire est saturé.
"""),

59 : _("""
 Le nom demandé existe déjà dans le répertoire %(k1)s.

"""),

60 : _("""
 Erreur lors de l'allocation dynamique. Il n'a pas été possible d'allouer 
 une zone mémoire de longueur %(i1)d (octets).
 La dernière opération de libération mémoire a permis de récupérer %(i2)d (octets). 

"""),

62 : _("""
 Erreur lors de l'allocation dynamique. Il n'a pas été possible d'allouer 
 une zone mémoire de longueur %(i1)d (octets), on dépasse la limite maximum 
 fixée à %(i2)d (octets) et on occupe déjà %(i3)d (octets).
 La dernière opération de libération mémoire a permis de récupérer %(i4)d (octets). 

"""),

63 : _("""

 Critère de destruction du fichier (%(r2).2f %%) associé à la base %(k1)s dépassé %(r1).2f %%  
 Nombre d'enregistrements utilisés : %(i1)d 
 Volume disque occupé              : %(i2)d Mo.
 Nombre maximum d'enregistrements  : %(i3)d  

"""), 
 

64 : _("""

 ATTENTION la taille de répertoire de noms atteint %(i1)d pour la base %(k1)s.
 Il sera impossible de l'agrandir.  
  -> Conseil :     
     Il faut réduire le nombre de concepts sur la base GLOBALE en utilisant
     la commande DETRUIRE.     

"""), 

65 : _("""

 ATTENTION la taille de répertoire de noms atteint %(i1)d pour la base %(k1)s.
 Il sera impossible de l'agrandir.  
  -> Conseil :     
     Il y a trop d'objets créés sur la base VOLATILE, cela peut provenir d'une
     erreur dans la programmation de la commande.     

"""), 

66 : _("""

 La base au format HDF de nom %(k1)s ne peut être créée.
 La fonction HDFCRF renvoie un code retour : %(i1)d 

"""), 
 

67 : _("""

 Le nombre d'objets de la collection %(k1)s est inférieur ou égal à 0  

"""), 


68 : _("""

 Le fichier associé à la base demandée %(k1)s n'est pas ouvert.  

"""), 

69 : _("""

 Le nom %(k1)s est deja utilise pour un objet simple.

"""), 

70 : _("""

 Le type de stockage %(k1)s de la collection est erroné.

"""), 

71 : _("""

 La longueur variable pour la collection %(k1)s est incompatible avec le genre E.

"""), 

72 : _("""

 La longueur du type caractère n'est pas valide pour la collection %(k1)s 

"""), 

73 : _("""

 Le nom %(k1)s du pointeur de longueurs est invalide.

"""), 

74 : _("""

 Le pointeur de longueurs %(k1)s n'a pas été créé dans la bonne base.

"""), 

75 : _("""

 Le pointeur de longueurs %(k1)s n'est pas de la bonne taille.

"""), 

76 : _("""

 Le type du pointeur de longueurs %(k1)s n'est pas correct (différent de I).

"""), 

77 : _("""

 Le nom du répertoire de noms %(k1)s est invalide.

"""), 

78 : _("""

 Le répertoire de noms %(k1)s n'a pas été créé dans la bonne base.

"""), 

79 : _("""

 Le répertoire de noms %(k1)s n'est pas de la bonne taille.

"""), 

80 : _("""

 L'objet %(k1)s n'est pas un répertoire de noms.

"""), 

81 : _("""

 Le type d'accès %(k1)s est inconnu.

"""), 

82 : _("""

 Le type d'accès %(k1)s de la collection est erroné.

"""), 

83 : _("""

 Le nom du pointeur d'accès %(k1)s est invalide.

"""), 

84 : _("""
 La longueur du nom %(k1)s est invalide (> 24 caractères).

"""),

85 : _("""

 Le nom %(k1)s est deja utilise pour une collection.

"""), 

86 : _("""

 La longueur du type caractère n'est pas définie pour l'objet %(k1)s 

"""), 

87 : _("""

 Un objet de genre répertoire (N) doit être de type caractère (K) %(k1)s 

"""), 

88 : _("""

 La longueur du type caractère %(k1)s n'est pas valide. 

"""), 

89 : _("""

 Un objet de genre répertoire doit être de type K de longueur multiple de 8 %(k1)s. 

"""), 

90 : _("""

 Un objet de genre répertoire doit être de type K de longueur inférieure ou égale à 24 %(k1)s. 

"""), 

91 : _("""

 Le type %(k1)s est invalide.

"""), 

92 : _("""

 La longueur ou la position de la sous-chaîne %(k1)s est invalide.

"""), 

93 : _("""

 Les longueurs des sous-chaînes %(k1)s sont différentes.

"""), 

94 : _("""

 Les sous-chaînes %(k1)s sont identiques.

"""), 

95 : _("""

 L'appel de JECROC par JEXNOM ou JEXNUM est obligatoire.

"""), 

96 : _("""

 L'accès par JEXNUM est interdit %(k1)s.

"""), 

97 : _("""

 Erreur lors de l'appel à JECROC %(k1)s.

"""), 

98 : _("""

 L'attribut %(k1)s. est uniquement destiné aux collections contigues.

"""), 

99 : _("""

 L'attribut est incompatible avec le genre %(k1)s.

"""), 

}
