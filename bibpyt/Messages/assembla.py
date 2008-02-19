#@ MODIF assembla Messages  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
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
  Erreur d'utilisation :
    Pour les méthodes itératives GCPC et FETI, on ne peut pas encore utiliser
    de matrice non-symétrique.

  Conseil : Changer de solveur
"""),

4 : _("""
  on ne peut assembler que des matrices réelles ou complexes
"""),

5 : _("""
 modèles discordants
"""),

6 : _("""
 FETI : maille positive avec LIGREL de charge !
"""),

7 : _("""
 FETI : maille négative avec LIGREL de modele !
"""),

8 : _("""
 le motcle :  %(k1)s  est incorrect.
 on attend : "CUMU" ou "ZERO"
"""),

9 : _("""
 on ne trouve pas la composante "LAGR" dans la grandeur
"""),

10 : _("""
 il est imprevu d avoir le cmp "lagr" au dela de 30
"""),

11 : _("""
 on ne peut assembler que des vecteurs réels ou complexes
"""),

12 : _("""
 le maillage  %(k1)s  contient des super-mailles
 pour l'instant, elles sont proscrites avec FETI
"""),

13 : _("""
 ICHIN = 0
"""),

14 : _("""
 ICHIN < -2
"""),

15 : _("""
 S => ICHIN=/0
"""),

16 : _("""
 action : E/L/S
"""),

18 : _("""
 incohérence des MATR_ELEM
"""),

19 : _("""
 MATR_ELEM sans SSS et sans LISTE_RESU
"""),

20 : _("""
  -  aucun LIGREL dans les RESUELEM
"""),

21 : _("""
 modèles différents
"""),

24 : _("""
 le nombre maximum de composante de la grandeur est nul
"""),

25 : _("""
 le nombre d'entiers codes est nul
"""),

26 : _("""
 le noeud:  %(k1)s composante:  %(k2)s  est bloqué plusieurs fois.
"""),

27 : _("""
 l'entier décrivant la position du premier lagrange ne peut etre égal qu'à +1 ou -1 .
"""),

28 : _("""
 le nombre de noeuds effectivement numerotés ne correspond pas au nombre
 de noeuds à numéroter
"""),

29 : _("""
  -  aucun LIGREL
"""),

30 : _("""
  plusieurs phénomènes
"""),

31 : _("""
 les DDL du NUME_DDL ont bougé
"""),

32 : _("""
 phénomène non prévu dans le MOLOC de NUMER2 pour DD
"""),

33 : _("""
 le .PRNO est construit sur plus que le maillage
"""),

34 : _("""
 le .PRNO est de dimension nulle
"""),

35 : _("""
 il n y a pas de modèle dans la liste  %(k1)s .NUME.LILI
"""),

36 : _("""
 noeud inexistant
"""),

37 : _("""
 méthode :  %(k1)s  inconnue.
"""),

38 : _("""
 noeud incorrect
"""),

39 : _("""
 le phénomène  %(k1)s  n'est pas admis pour la symétrisation des matrices.
 seuls sont admis les phénomènes "MECANIQUE" et "THERMIQUE"
"""),

41 : _("""
 le noeud  : %(i1)d  du RESUEL : %(k1)s  du VECT_ELEM  : %(k2)s
 n'a pas d'adresse dans : %(k3)s
"""),

42 : _("""
 le noeud  : %(i1)d  du RESUEL : %(k1)s  du VECT_ELEM  : %(k2)s
   a une adresse  : %(i2)d  > NEQUA : %(i3)d
"""),

43 : _("""
 NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

44 : _("""
 --- VECT_ELEM     : %(k1)s
 --- RESU          : %(k2)s
 --- NOMLI         : %(k3)s
 --- GREL numéro   : %(i1)d
 --- MAILLE numéro : %(i2)d
 --- NNOE par NEMA : %(i3)d
 --- NNOE par NODE : %(i4)d
"""),

45 : _("""
Erreur Programmeur ou utilisateur :
-----------------------------------
 Le sd_ligrel    : %(k1)s  référencé par le noeud supplém. : %(i1)d
 de la maille : %(i2)d  du sd_resuelem  : %(k2)s  du sd_vect_elem : %(k3)s
 n'est pas présent  dans le sd_nume_ddl : %(k4)s

Risques & conseils :
--------------------
 Si vous utilisez la commande MACRO_ELAS_MULT :
   Si %(k5)s est une charge contenant des conditions aux limites dualisées (DDL_IMPO, ...),
   Etes-vous sur d'avoir indiqué cette charge derrière le mot clé CHAR_MECA_GLOBAL ?
   En effet, il faut indiquer TOUTES les charges dualisées derrière CHAR_MECA_GLOBAL.

 Si vous utilisez directement la commande ASSE_VECTEUR :
   Si %(k5)s est une charge contenant des conditions aux limites dualisées (DDL_IMPO, ...),
   Etes-vous sur d'avoir indiqué cette charge derrière le mot clé CHARGE
   de la commande CALC_MATR_ELEM/OPTION='RIGI_MECA' ?
"""),

46 : _("""
 --- NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

47 : _("""
 --- NDDL :  %(i1)d  > NDDL_MAX : %(i2)d
"""),

48 : _("""
 --- le noeud  : %(i1)d  du RESUEL    : %(k1)s  du VECT_ELEM   : %(k2)s
 --- n'a pas d''adresse  dans la numérotation : %(k3)s
"""),

49 : _("""
 --- le noeud  : %(i1)d  du RESUEL    : %(k1)s  du VECT_ELEM   : %(k2)s
 --- a une adresse : %(i2)d   > NEQUA : %(i3)d
"""),

63 : _("""
 erreur sur le premier lagrange d'une LIAISON_DDL
 on a mis 2 fois le premier  lagrange :  %(i1)d
 derrière le noeud :  %(i2)d
"""),

64 : _("""
 erreur sur le  2ème lagrange d'une LIAISON_DDL
 on a mis 2 fois le 2ème  lagrange :  %(i1)d
 derrière le noeud :  %(i2)d
"""),

65 : _("""
 incohérence dans le dénombrement des ddls
 nombre de ddl a priori    : %(i1)d
 nombre de ddl a posteriori: %(i2)d
"""),

66 : _("""
 Problème dans NULILI.F: on a au moins deux maillages différents:
  - maillage 1: %(k1)s
  - maillage 2: %(k2)s
"""),

67 : _("""
 Problème dans NUMERO.F avec FETI: L'objet PROF_CHNO.NUEQ est différent de
 l'identité pour i= %(i1)d on a NUEQ(i)= %(i2)d
"""),

68 : _("""
 Problème dans NUMERO.F avec FETI: Incohérence entre la SD_FETI et le paramètrage
 de l'opérateur. Nombre d'incohérences= %(i1)d
"""),

}
