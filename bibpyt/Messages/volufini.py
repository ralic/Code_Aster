#@ MODIF volufini Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

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
8 : _("""
  -> La loi de comportement  %(k1)s est inconnu
"""),
9 : _("""
  -> Le type de modelisation volumes finis (TYPVF)  %(i1)i   est inconnu.
     Vous avez le choix entre :
                          TYPVF=1  => Schema VF a deux points
                          TYPVF=2  => Schema SUDM
                          TYPVF=3  => Schema SUDA
                          TYPVF=4  => Schema SUC
                                                                          
"""),
10 : _("""
  -> Le nom de la modelisation  %(k1)s  est inconnu
"""),
11 : _("""
  -> L'option  %(k1)s est inconnue
"""),
12 : _("""
  -> ELREFE  %(k1)s   inconnue : En 3D et en VF on peut utiliser uniquement des hexaedres
    a 27 DDL et des tetraedres a 27 DDL.
"""),
13 : _("""
  -> l'element %(k1)s et la face  %(i1)i est non plane 
"""),
14 : _("""
  -> Il est possible d'utiliser comme inconnue dans la maille le centre du cercle
     circonscrit (a la place du centre de gravite) ce qui nous permet de retrouver
     un schema volumes finis a deux points. Cependant cette possibilité n'est programme
     que pour des maillages 2D composés de triangles.
"""),
15 : _("""
  -> Le nombre de noeuds de l'element est inconnu en VF. 
     Nous pouvons uttiliser uniquement des mailles quadratiques. Il est donc 
     possible d'avoir 7 noeuds on a alors une maille TRIA7,
     9 noeuds on a alors un QUAD9, etc...
"""),
}
