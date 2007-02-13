#@ MODIF modelisa8 Messages  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

cata_msg={

1: _("""
 la section de la poutre est nulle
"""),
2: _("""
 l'inertie de la poutre suivant OY est nulle
"""),
3: _("""
 l'inertie de la poutre suivant OZ est nulle
"""),
4: _("""
 La somme des aires des fibres est differente de l'aire de la section de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_AIRE :\n
   - occurence de multifire : %(r1)d \n
   - aire de la poutre      : %(r2)12.5E \n
   - aire des fibres        : %(r3)12.5E \n
   - erreur relative        : %(r4)12.5E
"""),
5: _("""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Y est differente du moment de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_INERTIE :\n
   - occurence de multifire : %(r1)d \n
   - moment d'inertie de la poutre : %(r2)12.5E \n
   - aire d'inertie des fibres     : %(r3)12.5E \n
   - erreur relative               : %(r4)12.5E
"""),
6: _("""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Z est differente du moment de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_INERTIE :\n
   - occurence de multifire : %(r1)d \n
   - moment d'inertie de la poutre : %(r2)12.5E \n
   - aire d'inertie des fibres     : %(r3)12.5E \n
   - erreur relative               : %(r4)12.5E
"""),

7: _("""
 actuellemnt on ne peut mettre que %(k1)s groupes de fibres sur un element 
"""),

8: _("""
 Le groupe de fibre %(k1)s n'a pas ete defini dans DEFI_GEOM_FIBRE
"""),


}
