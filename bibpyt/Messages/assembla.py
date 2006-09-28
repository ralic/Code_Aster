#@ MODIF assembla Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
}
