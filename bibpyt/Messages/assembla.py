# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1 : _(u"""
  Erreur d'utilisation :
    Pour la méthodes itérative GCPC, on ne peut pas encore utiliser
    de matrice non-symétrique.

  Conseil : Changer de solveur
"""),

3: _(u"""
 Le calcul est séquentiel, on ne peut donc pas utiliser MATR_DISTRIBUEE='OUI'.
 On force MATR_DISTRIBUEE='NON'.
"""),

4: _(u"""
 L'utilisation de MATR_DISTRIBUEE='OUI' nécessite que chaque processeur ait
 au moins 1 degré de liberté qui lui soit alloué.
 Ici, le processeur %(i1)d ne s'est vu attribué aucun ddl.

 Conseil : Modifiez le partitionnement des mailles de votre modèle dans
           AFFE_MODELE/PARTITION/PARALLELISME ou diminuez le nombre de processeurs.
"""),

5 : _(u"""
 modèles discordants
"""),





8 : _(u"""
 le mot-clé :  %(k1)s  est incorrect.
 on attend : "CUMU" ou "ZERO"
"""),

9 : _(u"""
 on ne trouve pas la composante "LAGR" dans la grandeur
"""),

10 : _(u"""
 il est imprévu d avoir la composante "LAGR" au delà de 30
"""),

11 : _(u"""
 on ne peut assembler que des vecteurs réels ou complexes
"""),




13 : _(u"""
 ICHIN = 0
"""),

14 : _(u"""
 ICHIN < -2
"""),

15 : _(u"""
 S => ICHIN=/0
"""),

16 : _(u"""
 action : E/L/S
"""),

18 : _(u"""
 Erreur développeur dans l'assemblage.
 Les vecteurs élémentaires ou les matrices élémentaires sont incohérentes: ils ne portent pas sur le même modèle ou ils ne calculent pas la même option.
"""),

19 : _(u"""
 Erreur développeur dans l'assemblage.
 Les vecteurs élémentaires ou les matrices élémentaires ne contiennent ni sous-structures, ni objet LISTE_RESU.
"""),

20 : _(u"""
  Erreur programmeur :
    lors d'un assemblage, dans la liste des MATR_ELEM (ou VECT_ELEM) que l'on veut
    assembler, on ne trouve aucun resuelem.
"""),

21 : _(u"""
 modèles différents
"""),

24 : _(u"""
 le nombre maximum de composante de la grandeur est nul
"""),

25 : _(u"""
 le nombre d'entiers codes est nul
"""),

26 : _(u"""
 le noeud:  %(k1)s composante:  %(k2)s  est bloqué plusieurs fois.
"""),

27 : _(u"""
 l'entier décrivant la position du premier Lagrange ne peut être égal qu'à +1 ou -1 .
"""),

28 : _(u"""
 le nombre de noeuds effectivement numérotés ne correspond pas au nombre
 de noeuds à numéroter
"""),

29 : _(u"""
  -  aucun LIGREL
"""),

30 : _(u"""
  plusieurs phénomènes
"""),

31 : _(u"""
 les DDL du NUME_DDL ont bougé
"""),





33 : _(u"""
 le .PRNO est construit sur plus que le maillage
"""),

34 : _(u"""
 le .PRNO est de dimension nulle
"""),

35 : _(u"""
 il n y a pas de modèle dans la liste  %(k1)s .NUME.LILI
"""),

36 : _(u"""
 noeud inexistant
"""),

37 : _(u"""
 méthode :  %(k1)s  inconnue.
"""),

38 : _(u"""
 noeud incorrect
"""),

39 : _(u"""
 le phénomène  %(k1)s  n'est pas admis pour la symétrisation des matrices.
 seuls sont admis les phénomènes "MECANIQUE" et "THERMIQUE"
"""),

41 : _(u"""
 le noeud  : %(i1)d  du RESUEL : %(k1)s  du VECT_ELEM  : %(k2)s
 n'a pas d'adresse dans : %(k3)s
"""),

42 : _(u"""
 le noeud  : %(i1)d  du RESUEL : %(k1)s  du VECT_ELEM  : %(k2)s
   a une adresse  : %(i2)d  > NEQUA : %(i3)d
"""),

43 : _(u"""
 NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

44 : _(u"""
 --- VECT_ELEM     : %(k1)s
 --- RESU          : %(k2)s
 --- NOMLI         : %(k3)s
 --- GREL numéro   : %(i1)d
 --- MAILLE numéro : %(i2)d
 --- NNOE par NEMA : %(i3)d
 --- NNOE par NODE : %(i4)d
"""),

45 : _(u"""
Erreur Programmeur ou utilisateur :
-----------------------------------
 Le LIGREL    : %(k1)s  référencé par le noeud supplémentaire. : %(i1)d
 de la maille : %(i2)d  du resuelem  : %(k2)s  du sd_vect_elem : %(k3)s
 n'est pas présent  dans le sd_nume_ddl : %(k4)s

Risques & conseils :
--------------------
 Si vous utilisez la commande MACRO_ELAS_MULT :
   Si %(k5)s est une charge contenant des conditions aux limites dualisées (DDL_IMPO, ...),
   Êtes-vous sur d'avoir indiqué cette charge derrière le mot clé CHAR_MECA_GLOBAL ?
   En effet, il faut indiquer TOUTES les charges dualisées derrière CHAR_MECA_GLOBAL.

 Si vous utilisez directement la commande ASSE_VECTEUR :
   Si %(k5)s est une charge contenant des conditions aux limites dualisées (DDL_IMPO, ...),
   Êtes-vous sur d'avoir indiqué cette charge derrière le mot clé CHARGE
   de la commande CALC_MATR_ELEM/OPTION='RIGI_MECA' ?
"""),

46 : _(u"""
 --- NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

47 : _(u"""
 --- NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

48 : _(u"""
 --- le noeud  : %(i1)d  du RESUEL    : %(k1)s  du VECT_ELEM   : %(k2)s
 --- n'a pas d''adresse  dans la numérotation : %(k3)s
"""),

49 : _(u"""
 --- le noeud  : %(i1)d  du RESUEL    : %(k1)s  du VECT_ELEM   : %(k2)s
 --- a une adresse : %(i2)d   > NEQUA : %(i3)d
"""),

63 : _(u"""
 erreur sur le premier Lagrange d'une LIAISON_DDL
 on a mis 2 fois le premier  Lagrange :  %(i1)d
 derrière le noeud :  %(i2)d
"""),

64 : _(u"""
 erreur sur le  2ème Lagrange d'une LIAISON_DDL
 on a mis 2 fois le 2ème  Lagrange :  %(i1)d
 derrière le noeud :  %(i2)d
"""),

65 : _(u"""
 incohérence dans le dénombrement des ddls
 nombre de ddl a priori    : %(i1)d
 nombre de ddl a posteriori: %(i2)d
"""),

66 : _(u"""
 Il faut vérifier la cohérence des maillages dans les structures de données
 %(k5)s et %(k6)s. Les maillages devraient être les mêmes.

 On trouve au moins deux maillages différents :
  - maillage 1 : %(k1)s
  - maillage 2 : %(k2)s

 Détails :
   Le maillage 1 : %(k1)s est lié au ligrel 1 : %(k3)s
   Le maillage 2 : %(k2)s est lié au ligrel 2 : %(k4)s
"""),



}
