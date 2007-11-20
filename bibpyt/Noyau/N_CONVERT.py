#@ MODIF N_CONVERT Noyau  DATE 19/11/2007   AUTEUR COURTOIS M.COURTOIS 
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

"""
   Module de conversion des valeurs saisies par l'utilisateur après vérification.
"""

# -----------------------------------------------------------------------------
class ConversionError(Exception):
   pass

# -----------------------------------------------------------------------------
def is_int(real):
   """Est-ce que 'real' a une valeur entière ?
   """
   return abs(int(real) - real) < 1.e-12

# -----------------------------------------------------------------------------
class TypeConversion:
   """Conversion de type
   """
   def __init__(self, name, typ=None):
      self.name = name
      self.typ = typ

   def convert(self, obj):
      """Filtre liste
      """
      in_type = type(obj)
      is_simple = in_type not in (list, tuple)
      if is_simple:
         obj = (obj,)
      
      result = []
      for o in obj:
         result.append(self.defaut(o))
      
      if is_simple:
         return result[0]
      else:
         return in_type(result)

   def defaut(self, obj):
      """Conversion de obj si c'est possible.
      """
      for type_permis in self.typ:
         # attention si plusieurs types sont permis, l'ordre de self.typ peut influer sur le résultat.
         if type_permis == 'R':
            if type(obj) in (int, float, long):
               return float(obj)
         elif type_permis == 'I':
            if type(obj) in (int, float, long):
               if is_int(obj):
                  return int(obj)
               else:
                  raise ConversionError("%s (%s) ne peut pas être considéré comme entier" % (repr(obj), type(obj)))

      # autres types : pas de conversion, la validation arrêtera si obj est incorrect.
      return obj


