#@ MODIF fermetur Messages  DATE 07/06/2007   AUTEUR PELLET J.PELLET 
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
  Erreur :
      le solveur "MUMPS" n'est pas installé sur cette machine.
"""),

2: _("""
  Erreur :
      la bibliothèque "MED" n'est pas installée sur cette machine.
"""),

3: _("""
  Erreur :
      la bibliothèque "HDF5" n'est pas installée sur cette machine.
"""),

4: _("""
  Erreur :
      la bibliothèque "ZMAT" n'est pas installée sur cette machine.
"""),

5: _("""
  Erreur de programmation :
      On essaye d'utiliser un operateur (op0xxx) qui n'est pas encore programmé.
"""),

6: _("""
  Erreur de programmation :
      On essaye d'utiliser un operateur (ops0xx) qui n'est pas encore programmé.
"""),


7: _("""
  Erreur :
      le logiciel "SCOTCH" n'est pas installé sur cette machine.
"""),

8: _("""
  Erreur de programmation :
      On essaye d'utiliser une routine de calcul élémentaire (te0xxx)
      qui n'est pas encore programmée.
"""),

9: _("""
  Erreur de programmation :
      On essaye d'utiliser une routine d'initialisation élémentaire (ini0xx)
      qui n'est pas encore programmée.
"""),


}
