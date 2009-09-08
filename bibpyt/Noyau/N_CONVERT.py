#@ MODIF N_CONVERT Noyau  DATE 07/09/2009   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
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

"""
   Module de conversion des valeurs saisies par l'utilisateur après vérification.
"""

# -----------------------------------------------------------------------------
def is_int(real):
   """Est-ce que 'real' a une valeur entière ?
   """
   return abs(int(real) - real) < 1.e-12

# -----------------------------------------------------------------------------
class Conversion:
   """Conversion de type.
   """
   def __init__(self, name, typ):
      self.name = name
      self.typ  = typ

   def convert(self, obj):
      """Filtre liste
      """
      in_type = type(obj)
      if in_type not in (list, tuple):
         obj = (obj,)
      
      result = []
      for o in obj:
         result.append(self.function(o))
      
      if in_type not in (list, tuple):
         return result[0]
      else:
         # ne marche pas avec MACR_RECAL qui attend une liste et non un tuple
         return tuple(result)

   def function(self, o):
      raise NotImplementedError, 'cette classe doit être dérivée'

# -----------------------------------------------------------------------------
class TypeConversion(Conversion):
   """Conversion de type
   """
   def __init__(self, typ):
      Conversion.__init__(self, 'type', typ)

# -----------------------------------------------------------------------------
class IntConversion(TypeConversion):
   """Conversion en entier
   """
   def __init__(self):
      TypeConversion.__init__(self, 'I')

   def function(self, o):
      if type(o) is float and is_int(o):
         o = int(o)
      return o

# -----------------------------------------------------------------------------
class FloatConversion(TypeConversion):
   """Conversion de type
   """
   def __init__(self):
      TypeConversion.__init__(self, 'R')

   def function(self, o):
      if type(o) in (int, float, long):
         o = float(o)
      return o

# -----------------------------------------------------------------------------
_convertI = IntConversion()
_convertR = FloatConversion()

def ConversionFactory(name, typ):
   if name == 'type':
      if 'I' in typ:
         return _convertI
      elif 'R' in typ:
         return _convertR
   return None


