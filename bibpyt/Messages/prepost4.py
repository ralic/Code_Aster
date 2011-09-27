#@ MODIF prepost4 Messages  DATE 26/09/2011   AUTEUR TRAN V-X.TRAN 
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

def _(x) : return x

cata_msg = {

5 : _("""
 l'option  %(k1)s n'a pas été calculée pour la SD  %(k2)s
"""),

6 : _("""
 le champ " %(k1)s " ( %(k2)s ) n'a pas été noté dans la SD  %(k3)s
"""),

7 : _("""
 "TUBE_NEUF" n'a de sens que pour une table d'usure existante
"""),

8 : _("""
 angle initial différent de -180. degrés.
"""),

9 : _("""
 les angles ne sont pas croissants.
"""),

10 : _("""
 angle final différent de 180. degrés.
"""),

11 : _("""
 rayon mobile obligatoire avec secteur.
"""),

12 : _("""
 rayon obstacle obligatoire avec secteur.
"""),

13 : _("""
 la table usure en sortie est differente de celle en entree
"""),

14 : _("""
 le nombre de secteurs en sortie est différent de celui en entree
"""),

15 : _("""
 probleme extraction pour la table  %(k1)s
"""),

17 : _("""
 aucune valeur de moment presente
"""),

18 : _("""
 y a un bug: recup frequences
"""),

19 : _("""
 il faut au moins un group_ma_radier
"""),

20 : _("""
 rigidite de translation non nulle
"""),

21 : _("""
 rigidite de rotation non nulle
"""),

22 : _("""
 nombres de composantes raideurs et mode differents
"""),

23 : _("""
 nombres de group_ma et amor_interne differents
"""),

24 : _("""
 nombres de composantes amortissements et mode differents
"""),

26 : _("""
 le type du concept resultat  n'est ni evol_elas, ni evol_noli.
"""),

27 : _("""
 vous avez probablement archive l etat initial dans la commande stat_non_line. cela correspond au numero d ordre 0. nous ne tenons pas compte du resultat a ce numero d ordre pour le calcul de de la fatigue.
"""),

29 : _("""
 les champs de  contraintes aux points de gauss n'existent pas.
"""),

30 : _("""
 le champ simple qui contient les valeurs des contraintes n existe pas.
"""),

32 : _("""
 le champ de contraintes aux points de gauss  SIEF_ELGA n'a pas été calcule.
"""),

33 : _("""
 le champ de deformations aux points de gauss EPSI_ELGA n'a pas ete calcule.
"""),

34 : _("""
 les champs de  deformations aux points de gauss n'existent pas.
"""),

35 : _("""
 le champ simple qui contient les valeurs des deformations n existe pas.
"""),

38 : _("""
 le champ de contraintes aux noeuds SIEF_NOEU n'a pas été calcule.
"""),

39 : _("""
 le champ de deformations aux noeuds EPSI_NOEU n'a pas ete calcule.
"""),

40 : _("""
 le champ de  contraintes aux noeuds n'existe pas.
"""),

41 : _("""
 le champ de  deformations aux noeuds n'existe pas.
"""),

47 : _("""
 inst_init plus grand que inst_fin
"""),

57 : _("""
  erreur donnees.
"""),

58 : _("""
 présence de point(s) que dans un secteur.
"""),

59 : _("""
 aucun cercle n'est circonscrit aux quatre points.
"""),

60 : _("""
 le décalage se trouve nécessairement coté revêtement
 le décalage doit être négatif
"""),

76 : _("""
 le champ demandé n'est pas prévu
"""),

77 : _("""
 nom_cham:  %(k1)s  interdit.
"""),

82 : _("""
 type  %(k1)s  non implante.
"""),

83 : _("""
 profondeur > rayon du tube
"""),

84 : _("""
 pas d'informations dans le "RESU_GENE" sur l'option "choc".
"""),

85 : _("""
 modele non valide.
"""),

86 : _("""
  seuil / v0  > 1
"""),

87 : _("""
  ***** arret du calcul *****
"""),

89 : _("""
 type non traite  %(k1)s
"""),

90 : _("""
 les tables TABL_MECA_REV et TABL_MECA_MDB n'ont pas les mêmes dimensions
"""),

91 : _("""
 les tables n'ont pas les mêmes instants de calculs
"""),

92 : _("""
 les tables n'ont pas les mêmes dimensions
"""),

93 : _("""
 volume usé trop grand pour la modélisation
"""),

94 : _("""
Elément inconnu.
   Type d'élément Gibi          : %(i1)d
   Nombre de sous-objet         : %(i2)d
   Nombre de sous-référence     : %(i3)d
   Nombre de noeuds par élément : %(i4)d
   Nombre d'éléments            : %(i5)d

La ligne lue dans le fichier doit ressembler à ceci :
%(i1)8d%(i2)8d%(i3)8d%(i4)8d%(i5)8d
"""),

95 : _("""
On a lu un objet dit composé (car type d'élément = 0) qui serait
composé de 0 sous-objet !
"""),

96 : _("""
 Type de concept invalide.
"""),

97 : _("""
 Erreur Utilisateur :
 La maille de peau : %(k1)s ne peut pas être réorientée.
 Car elle est inserrée entre 2 mailles "support" placées de part et d'autre : %(k2)s et %(k3)s.
"""),

}
