#@ MODIF jeveux Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
  %(k1)s
"""),

2: _("""
 pointeur de longueur externe interdit maintenant.
"""),

3: _("""
 pointeur de nom externe interdit maintenant.
"""),

4: _("""
 le nombre de bases a gerer est negatif ou nul
"""),

5: _("""
 taille memoire demandee negative ou nulle
"""),

6: _("""
 appel invalide, la marque devient negative
"""),

7: _("""
 destruction de  %(k1)s
"""),

8: _("""
  la base  %(k1)s  a ete constituee avec la version  %(k2)s  et vous utilisez la version  %(k3)s
"""),

9: _("""
 suppression de la partition memoire
"""),

10: _("""
 le nom demande existe deja  dans la base  %(k1)s
"""),

11: _("""
 erreur lors de la fermeture de la base  %(k1)s
"""),

12: _("""
  fichier associe a la base  %(k1)s  inexistant
"""),

13: _("""
  erreur de lecture du 1er bloc de  %(k1)s
"""),

14: _("""
  erreur lors de la fermeture de  %(k1)s
"""),

15: _("""
  Ecrasement amont, l'objet :< %(k1)s > est peut etre écrasé
"""),

16: _("""
  Ecrasement aval, l'objet :< %(k1)s > est peut etre écrasé
"""),

17: _("""
  Chainage casse apres l'objet :  %(k1)s
"""),

18: _("""
  Erreur detectee code retour hpcheck non nul
"""),

19: _("""
 Le nom d'un objet JEVEUX ne doit pas commencer par un blanc.
"""),

21: _("""
     REOUVERTURE DE LA BASE                  :  %(k1)s
     CREEE AVEC LA VERSION                   :  %(k2)s
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i4)d
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i5)d
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i6)d %%
"""),

22: _("""
     NOM DE LA BASE                          :  %(k1)s
     NOMBRE D'ENREGISTREMENTS UTILISES       :  %(i1)d
     NOMBRE D'ENREGISTREMENTS MAXIMUM        :  %(i2)d
     LONGUEUR D'ENREGISTREMENT (OCTETS)      :  %(i3)d
     NOMBRE TOTAL D'ENTREES/SORTIES          :  %(i4)d
     NOMBRE D'IDENTIFICATEURS UTILISES       :  %(i5)d
     TAILLE MAXIMUM DU REPERTOIRE            :  %(i6)d
     POURCENTAGE D'UTILISATION DU REPERTOIRE :  %(i7)d %%
"""),

23: _("""
     Nom de Collection ou de Répertoire de noms inexistant :  %(k1)s
"""),

24: _("""
     JENONU : Collection ou Répertoire de noms  :  %(k1)s
     Il faut passer par JEXNOM,JEXNUM.
"""),

25: _("""
     Nom de collection ou de répertoire inexistant : >%(k1)s<
"""),

26: _("""
     Objet JEVEUX inexistant dans les bases ouvertes : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

27: _("""
     Objet simple JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

28: _("""
     Collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
     le segment de valeurs est introuvable
"""),

29: _("""
     Objet %(i1)d de collection JEVEUX inexistant en mémoire et sur disque : >%(k1)s<
"""),

30: _("""
     Objet de collection JEVEUX inexistant : >%(k1)s<
     l'objet n'a pas été créé ou il a été détruit
"""),

31: _("""
     Erreur programmeur :
     La routine JUVECA n'a pas prévu de redimensionner l'objet :%(k1)s
     de type :%(k2)s
"""),

32: _("""
     Erreur allocation de segment de memoire de longueur %(i1)d (entiers).
     Memoire allouee insuffisante. Fermeture des bases (glob.*) sur erreur
     Il faut relancer le calcul en augmentant la limite memoire.
"""),

33: _("""
     Modification de l'environnement JEVEUX.
     Allocation dynamique des segments de valeurs de taille superieure
     a %(i1)d (entiers) 
"""),

34: _("""
     Modification de l'environnement JEVEUX.
     Mode debug positionne a %(i1)d
"""),

35: _("""
     Le nombre de bases gerables est limite a %(i1)d
"""),

36: _("""  
     Le nombre d'enregistrements maximum de la base %(k1)s sera modifie,
     de %(i1)d a %(i2)d
"""),

37: _("""
     La valeur du rapport entre partitions ne convient pas, 
     la longueur de la partition 1 doit etre au minimum de %(i1)d mots 
     soit environ %(i2)d %%
"""),


38: _("""
     Numero d'objet invalide %(i1)d 
"""),


39: _("""
     Taille de repertoire demandee trop grande.
     Le maximun est de %(i1)d
     La valeur reclamee est de %(i2)d 
      
"""),


40: _("""
     Erreur ecriture de l'enregistrement %(i1)s sur la base : %(k1)s %(i2)d
     code retour WRITDR : %(i3)d 
     Erreur probablement provoquee par une taille trop faible du repertoire de travail.
      
"""),


41: _("""
     Erreur lecture de l'enregistrement %(i1)d sur la base : %(k1)s %(i2)d
     code retour READDR : %(i3)d 
      
"""),


42: _("""
     Fichier sature, le nombre maximum d'enregistrement %(i1)d de la base %(k1)s 
     est atteint, il faut relancer le calcul en modifiant le parametre NMAX_ENRE dans DEBUT 
     ou en passant une taille maximum de base sur la ligne de commande 
     argument "-max_base" suivi de la valeur en Mo.

"""),


43: _("""
     Erreur d'ouverture du fichier %(k1)s , code retour OPENDR = %(i1)d 

      
"""),

44: _("""
 Taille des segments de valeurs %(i1)d 
"""),

45: _("""
 Taille de la partition principale %(r1)g 
"""),

}

