#@ MODIF assembla Messages  DATE 18/06/2007   AUTEUR BOITEAU O.BOITEAU 
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
  le type  :  %(k1)s   de la matrice est incorrect. on attend : "s"pour une resolution par methode iterative
"""),

2: _("""
 matrice non symetrique pour l'instant proscrite  avec feti
"""),

3: _("""
  le parametre :  %(k1)s  est incorrect. on attend : "cumu" ou "zero" 
"""),

4: _("""
  on ne peut assembler que des matrices reelles ou complexes
"""),

5: _("""
 modeles discordants
"""),

6: _("""
 feti: maille positive avec ligrel de charge !
"""),

7: _("""
 feti: maille negative avec ligrel de modele !
"""),

8: _("""
 le motcle :  %(k1)s  est incorrect. on attend : "cumu" ou "zero" 
"""),

9: _("""
 on ne trouve pas le cmp "lagr" dans la grandeur
"""),

10: _("""
 il est imprevu d avoir le cmp "lagr" au dela de 30
"""),

11: _("""
 on ne peut assembler que des vecteurs reels ou complexes
"""),

12: _("""
 le maillage  %(k1)s  contient des super-mailles pour l'instant, elles sont proscrites avec feti
"""),

13: _("""
 ichin = 0 
"""),

14: _("""
 ichin < -2 
"""),

15: _("""
 s => ichin=/0 
"""),

16: _("""
 action:e/l/s 
"""),

17: _("""
 message vide    
"""),

18: _("""
 incoherence des matr_elem
"""),

19: _("""
 matr_elem sans sss et sans liste_resu
"""),

20: _("""
  -  aucun ligrel dans les resuelem 
"""),

21: _("""
 modeles diff.
"""),

22: _("""
 les valeurs de la matrice  %(k1)s  doivent etre reelles, on ne traite pas encore les matrices non-symetriques complexes.
"""),

23: _("""
 la matrice  %(k1)s  a transformer en matrice non-symetrique doit etre symetrique.
"""),

24: _("""
 le nombre maxi de composante de la grandeur est nul
"""),

25: _("""
 le nombre d"entiers codes est nul
"""),

26: _("""
 le noeud:  %(k1)s composante:  %(k2)s  est bloque plusieurs fois.
"""),

27: _("""
 l'entier decrivant la postion du premier lagrange ne peut etre egal qu'a  +1  ou  -1 .
"""),

28: _("""
 le nombre de noeuds effectivement numerotes ne correspond pas  au nombre de noeuds a numeroter
"""),

29: _("""
  -  aucun ligrel  
"""),

30: _("""
  plusieurs "phenomenes" stop 
"""),

31: _("""
 les ddls du nume_ddl ont bouge.
"""),

32: _("""
 phenomene non prevu dans le moloc de numer2 pour dd
"""),

33: _("""
 le .prno est construit sur plus que le maillage
"""),

34: _("""
 le .prno est de dimension nulle
"""),

35: _("""
 il n y a pas de modele dans la liste  %(k1)s .nume.lili
"""),

36: _("""
 noeud inexistant
"""),

37: _("""
 methode :  %(k1)s  inconnue.
"""),

38: _("""
 noeud incorrect
"""),

39: _("""
 le phenome  %(k1)s  n'est pas admis pour la symetrisation des matrices.seuls sont admis les phenomenes "mecanique" et "thermique" .
"""),

40: _("""
 erreur programmeur : certains type_element ne savent pas calculer les options syme_m?ns_r
"""),

41: _("""
 1--- le noeud  : %(i1)d  du resuel    : %(k1)s    du vect_elem  : %(k2)s 
    n''a pas d''adresse dans  : %(k3)s 
"""),

42: _("""
 2--- le noeud  : %(i1)d  du resuel    : %(k1)s  du vect_elem   : %(k2)s 
   a 1 adresse  : %(i2)d 
  > nequa : %(i3)d 
"""),

43: _("""
 3--- nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),

44: _("""
 4--- vect_elem     : %(k1)s --- resu        : %(k2)s --- nomli       : %(k3)s 
 --- grel numero   : %(i1)d 
 --- maille numero : %(i2)d 
 --- nnoe par nema : %(i3)d 
 --- nnoe par node : %(i4)d 
"""),

45: _("""
 5--- le ligrel : %(k1)s  ref. par le noeud supl.  : %(i1)d 
 --- de la maille : %(i2)d 
  du resuelem  : %(k2)s 
  du vect_elem   : %(k3)s 
 --- n"est pas present  dans la numerotation : %(k4)s 
"""),

46: _("""
 6--- nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),

47: _("""
 7--- nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),

48: _("""
 8--- le noeud  : %(i1)d  du resuel    : %(k1)s  du vect_elem   : %(k2)s 
 --- n''a pas d''adresse  dans la numerotation : %(k3)s 
"""),

49: _("""
 9--- le noeud  : %(i1)d  du resuel    : %(k1)s  du vect_elem   : %(k2)s 
 --- a une adresse : %(i2)d 
  > nequa : %(i3)d 
"""),

50: _("""
 11  nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),






52: _("""
 12  nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),

53: _("""
 13- nddl :  %(i1)d  > nddl_max : %(i2)d 
"""),
































































63: _("""
 erreur sur le   premier lagrange d"une liaison_ddl
 on a mis 2 fois le premier  lagrange :  %(i1)d 
 derrire le noeud :  %(i2)d 
"""),

64: _("""
 erreur sur le   2 eme lagrange d"une liaison_ddl
 on a mis 2 fois le 2eme  lagrange :  %(i1)d 
 derrire le noeud :  %(i2)d 
"""),

65: _("""
 incoherence dans  le denombrement des ddlsnbre de ddl a priori    : %(i1)d 
 nbre de ddl a posteriori: %(i2)d 
"""),
66: _("""
 Problème dans NULILI.F: on a au moins deux maillages différents:
  - maillage 1: %(k1)s
  - maillage 2: %(k2)s
"""),
67: _("""
 Problème dans NUMERO.F avec FETI: L'objet PROF_CHNO.NUEQ est différent de
 l'identité pour i= %(i1)d on a NUEQ(i)= %(i2)d
"""),
68: _("""
 Problème dans NUMERO.F avec FETI: Incohérence entre la SD_FETI et le paramètrage
 de l'opérateur. Nombre d'incohérences= %(i1)d
"""),
}
