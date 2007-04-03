#@ MODIF fonct0 Messages  DATE 02/04/2007   AUTEUR COURTOIS M.COURTOIS 
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

1 : _("""
   Le fichier %(k1)s existe déjà, on écrit à la suite.
"""),

2 : _("""
   Il n'y a pas de règles d'interpolation pour LIST_PARA/LIST_RESU,
   LIST_PARA/LIST_RESU ne peut donc apparaitre qu'une seule fois
   et à la première occurence de COURBE.
"""),

3 : _("""
   LIST_PARA et LIST_RESU n'ont pas la meme taille.
"""),

4 : _("""
   FONC_X/FONC_Y ne peuvent pas etre des nappes !
"""),

5 : _("""
   Au format 'TABLEAU', FONC_X/FONC_Y ne peut apparaitre qu'une seule fois
   et à la première occurence de COURBE
"""),

6 : _("""
   Il n'y a pas de règles d'interpolation pour ABSCISSE/ORDONNEE,
   ABSCISSE/ORDONNEE ne peut donc apparaitre qu'une seule fois
   et à la première occurence de COURBE.
"""),

7 : _("""
   ABSCISSE et ORDONNEE n'ont pas la meme taille.
"""),

8 : _("""
   Format inconnu : %(k1)s
"""),

}
