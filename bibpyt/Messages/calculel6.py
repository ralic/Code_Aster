#@ MODIF calculel6 Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 

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

cata_msg={
1: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

2: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

3: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

4: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

5: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

6: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

7: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

8: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

9: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

10: _("""
  option inconnue %(k1)s 
"""),

11: _("""
  option inconnue %(k1)s 
"""),

12: _("""
  option inconnue %(k1)s 
"""),

13: _("""
 interpolation deformations  anelastiques : evol_noli: %(k1)s instant: %(r1)f 
 icoret: %(i1)d 
"""),

14: _("""
 interpolation temperature:evol_ther: %(k1)s instant: %(r1)f icoret: %(i1)d 
"""),

15: _("""
  l'element diagonal u( %(i1)d , %(i2)d ) de la factorisation est nul. %(k1)s 
 la solution et les estimations d' erreurs ne peuvent etre calculees. %(k2)s 
"""),

16: _("""
 interpolation temperature:evol_ther: %(k1)s nom symbolique: %(k2)s 
 instant: %(r1)f 
 icoret: %(i1)d 
"""),

17: _("""
 recherche nbre de cmp: erreur:  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

18: _("""
 recherche nbre de cmp: erreur:  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

19: _("""
 recherche nbre de cmp: erreur:  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

20: _("""
 recherche nbre de cmp: erreur: grandeur ligne numero  %(i1)d  de nom  %(k1)s 
 grandeur colonne numero  %(i2)d 
  de nom  %(k2)s 
 grandeur mere numero  %(i3)d 
  de nom  %(k3)s 
"""),

21: _("""
 recherche nbre de cmp: erreur: grandeur %(i1)d a un code inconnu:  %(i2)d 
"""),

22: _("""
 recherche nbre d entiers codes  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

23: _("""
 recherche nbre d entiers codes  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

24: _("""
 recherche nbre d entiers codes  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s 
"""),

25: _("""
 recherche nbre d entiers codes grandeur ligne numero  %(i1)d  de nom  %(k1)s 
 grandeur colonne numero  %(i2)d de nom  %(k2)s 
 grandeur mere numero  %(i3)d de nom  %(k3)s 
"""),

26: _("""
 recherche nbre d entiers codes grandeur %(i1)d a un code inconnu:  %(i2)d 
"""),

27: _("""
 acces impossible  champ :  %(k1)s , nume_ordre :  %(i1)d 
"""),

28: _("""
 acces impossible au mode propre champ :  %(k1)s , nume_ordre :  %(i1)d 
"""),

29: _("""
 acces impossible  champ :  %(k1)s , nume_ordre :  %(i1)d 
"""),

30: _("""
 la derivee lagrangienne du deplacement d'occurrence n  %(i1)d 
 est inexistant dans la sd  %(k1)s 
 derivee par rapport a  %(k2)s 
"""),

31: _("""
 la derivee du deplacement d'occurrence n  %(i1)d 
 est inexistante dans la sd  %(k1)s 
 derivee par rapport a  %(k2)s 
"""),

32: _("""
 la derivee de la deformation d'occurrence n  %(i1)d 
 est inexistante dans la sd  %(k1)s 
 derivee par rapport a  %(k2)s 
"""),

33: _("""
 la derivee de la contrainte d'occurrence n  %(i1)d 
 est inexistante dans la sd  %(k1)s 
 derivee par rapport a  %(k2)s 
"""),

34: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

35: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

36: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

37: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

38: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

39: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

40: _("""
 famille non disponible    type de maille  %(k1)s 
    famille d'integration  %(i1)d 
"""),

41: _("""
 famille non disponible    type de maille  %(k1)s 
"""),

42: _("""
 ! prise en compte de l'erreur !
 ! sur cl de type echange_paroi n'a ! %(i1)d 
 ! pas ete encore implantee          ! %(i2)d 
"""),

43: _("""
 ! le mot cle excit contient !! plusieurs occurences de type flux lineaire ! %(i1)d 
 !   seule la derniere sera prise en compte   ! %(i2)d 
"""),

44: _("""
 ! le mot cle excit contient !! plusieurs occurences de type echange    ! %(i1)d 
 ! seule la derniere sera prise en compte  ! %(i2)d 
"""),

45: _("""
 ! le mot cle excit contient !! plusieurs occurences de type source     ! %(i1)d 
 ! seule la derniere sera prise en compte  ! %(i2)d 
"""),

46: _("""
 ! champ temperature !! vide pour numero ordre ! %(i1)d 
"""),

47: _("""
 ! champ flux_elno_temp !! vide pour numero ordre ! %(i1)d 
"""),

48: _("""
 calcul insensible variable sensible: %(k1)s 
"""),

49: _("""
 erreurs donnees composante inconnue  %(k1)s  pour la grandeur  %(k2)s 
"""),

50: _("""
 erreurs donneescomposante inconnue  %(k1)s  pour la grandeur  %(k2)s 
"""),

51: _("""
 erreurs donnees composante inconnue  %(k1)s 
"""),

52: _("""
 
 variables internes initiales   non coherentes (nb sous-points) avec le comportement  pour la maille  nomail
  nb sous-points "k-1" :  %(i1)d 
  nb sous-points "k" :  %(i2)d 
"""),

53: _("""
 
 variables internes initiales :  pas le nombre de composantes voulu par le comportement  pour la maille  nomail
  attendu par le comportement :  %(i1)d 
  trouve sur la maille :  %(i2)d 
"""),

}
