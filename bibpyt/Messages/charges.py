#@ MODIF charges Messages  DATE 20/10/2009   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
La charge <%(k1)s> a été utilisée plus d'une fois dans EXCIT: il faut la supprimer.
"""),

22 : _("""
La charge <%(k1)s> n'est pas mécanique.
"""),

23 : _("""
La charge <%(k1)s> est de type Dirichlet : 
 elle ne peut pas être suiveuse.
"""),

24 : _("""
La charge <%(k1)s> est de type cinématique (AFFE_CHAR_CINE): 
 elle ne peut pas être différentielle.
"""),

25 : _("""
Il y a plusieurs charges thermiques.
"""),

27 : _("""
La charge <%(k1)s> est de type cinématique (AFFE_CHAR_CINE): 
 elle ne peut pas être pilotée.
"""),

28 : _("""
On ne peut pas piloter la charge <%(k1)s> car c'est une charge fonction du temps
"""),

29 : _("""
La charge thermique <%(k1)s> ne peut pas être pilotée.
"""),

30 : _("""
Il y a plusieurs charges de séchage .
"""),

31 : _("""
La charge de séchage <%(k1)s> ne peut pas être pilotée.
"""),

32 : _("""
Il y a plusieurs charges de déformations anélastiques.
"""),

33 : _("""
La charge de déformations anélastiques <%(k1)s> ne peut pas être pilotée.
"""),

34 : _("""
La charge de type EVOL_CHAR <%(k1)s>  ne peut pas être pilotée.
"""),

35 : _("""
La charge de type ARLEQUIN <%(k1)s>  ne peut pas être pilotée.
"""),

36 : _("""
La charge de type liaison_unilatérale  %(k1)s  ne peut être pilotée.
"""),

37 : _("""
La charge de type contact  %(k1)s  ne peut être pilotée.
"""),

38 : _("""
La charge <%(k1)s> ne peut pas utiliser de fonction multiplicative FONC_MULT
 car elle est pilotée.
"""),

39 : _("""
On ne peut pas piloter en l'absence de forces de type "FIXE_PILO"
"""),

40 : _("""
On ne peut piloter plus d'une charge"
"""),

41 : _("""
Il y a au moins une charge non mécanique : vérifiez le fichier de commandes.
"""),

42 : _("""
On ne peut avoir plus d'une charge de contact"
"""),

}
