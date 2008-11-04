#@ MODIF utils Utilitai  DATE 03/11/2008   AUTEUR PELLET J.PELLET 
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

from sets import Set

"""
Module fournissant quelques fonctions utilitaires.
"""

# ------------------------------------------------------------------------------
def miss_dble(list1, list2):
   """miss = elements de list1 absents de list2
      dble = elements de list2 presents dans list1, fournis plusieurs fois."""
   s_ini = Set(list1)
   inter = s_ini.intersection(list2)
   miss = s_ini.copy()
   dble = Set()
   for p in list2:
      try:
         miss.remove(p)
      except KeyError:
         if Set(p).issubset(s_ini) and p.strip() != '':
            dble.add(p)
   return miss, inter, dble


# ------------------------------------------------------------------------------
if __name__ == '__main__':
   npar = ('X', 'Y',)
   nuti = ('DX', 'DY', 'X', 'X')
   print miss_dble(npar, nuti)

