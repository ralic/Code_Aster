#@ MODIF volufini Messages  DATE 06/05/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
  -> type de maille  %(k1)s incorrect
  """),

2 : _("""
  -> le sommet de numero global %(i1)i n appartient pas
  -> a la maille %(i2)i
"""),

3 : _("""
  -> Nombre de voisins %(i1)i trop grand
"""),

4 : _("""
  -> Nombre de sommets communs %(i1)i trop grand
"""),

5 : _("""
  -> Le nombre de mailles %(i1)i est inferieur a 1
"""),
6 : _("""
  -> Le type de voisinage %(k1)s est inconnu
"""),
7 : _("""
  -> Le type de voisinage %(k1)s a une longueur %(i1)i trop grande
"""),

}
