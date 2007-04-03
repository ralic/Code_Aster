#@ MODIF aster_jeveux SD  DATE 02/04/2007   AUTEUR COURTOIS M.COURTOIS 
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
   Interface d'accès aux objets jeveux.
"""

import aster
import pyaster
import Numeric
from Utilitai.Utmess import UTMESS

# -----------------------------------------------------------------------------
# Quelques utilitaires
# correspondance 'type Numeric' / 'type aster'
dict_astyp = { 'D': 'C', 'F': 'C',
               'c': 'K', 'd': 'R', 'f': 'R',
               'i': 'I', 'l': 'I', 's': 'I', 'N': 'I', }

# numérotation des types
dict_numtyp = { 'R'   : 1, 'I'   : 2, 'C'   : 3, 'K8'  : 4,
                'K16' : 5, 'K24' : 6, 'K32' : 7, 'K80' : 8, }

def isNumArray(obj):
   """Dit si `obj` est un tableau numérique.
   """
   return type(obj) == Numeric.ArrayType

def AsVectType(obj):
   """Retourne le type d'objet parmi : I, R, C, K (types des OBJVect) ou None.
   """
   typ = None
   if isNumArray(obj):
      typ = dict_astyp.get(obj.typecode())
   elif type(obj) in (list, tuple):
      if len(obj) > 0:
         vtest = obj[0]
         if   type(vtest) in (int, long):
            typ = 'I'
         elif type(vtest) == float:
            typ = 'R'
         elif type(vtest) == complex:
            typ = 'C'
         elif type(vtest) == str:
            typ = 'K'
   else:
      raise AssertionError, 'Seuls les tableaux numériques et ' \
                            'les lists/tuples sont traités'
   return typ


# -----------------------------------------------------------------------------
class ObjetJeveux(object):
   """Objet jeveux élémentaire :
      - un vecteur     = un ObjetJeveux particulier
      - une collection = un ensemble d'ObjetJeveux
   Accès par défaut en "L"ecture seule.
   """
   def __init__(self, nomj, mode='L', *args, **kwargs):
      """Constructeur
      """
      self.nomj = nomj
      self.mode = mode
      self._addr = None
      self.check_exists()

   def __del__(self):
      """A faire à la destruction de l'objet.
      """
      # se protéger en cas d'erreur
      if self._exists:
         self.libe()

   def check_exists(self):
      """Vérifie que l'objet existe, émet une erreur sinon.
      """
      if aster.jeveux_exists( self.nomj ):
         self._exists = True
      else:
         self._exists = False
         UTMESS('F', 'aster_jeveux', 'Objet inexistant : "%s"' % self.nomj)

   def lock(self, mode):
      """On verrouille l'objet en mémoire (JELOCK) pour conserver l'objet en mémoire.
      Accès en "L"ecture ou "E"criture (mode).
      """
      assert mode in ('E', 'L')
      self._addr = aster.jeveux_lock(self.nomj, self._numtyp, mode)
      #print """--- Marquage   de '%s' : adresse = %d""" % (self.nomj, self._addr)

   def libe(self):
      """Prévenir Jeveux que l'objet peut etre déchargé si besoin.
      """
      #print """--- Libération de '%s'... """ % self.nomj
      aster.jeveux_libe(self.nomj)
      self._addr = None

   def access_value(self):
      """Construit l'accès aux valeurs de l'objet.
      """
      raise NotImplementedError, "à définir dans une classe spécialisée"

   def __repr__(self):
      return '%s(nomj="%s")' % (type(self).__name__, self.nomj)


# -----------------------------------------------------------------------------
class Vecteur(ObjetJeveux):
   """Spécialisation pour les vecteurs.
   """
   def __init__(self, nomj, type, lonmax, lonuti, *args, **kwargs):
      """Initialisation
      """
      super(Vecteur, self).__init__(nomj, *args, **kwargs)
      self.__value = None
      self._type = type
      self._numtyp = dict_numtyp[type]
      self._lonmax = lonmax
      self._lonuti = lonuti
      self.access_value()

   def is_valid_value(self, candidat):
      """Dit si `candidat` est une valeur valide pour le vecteur.
      """
      # se limiter aux vérifications globales, pas de test sur chaque valeur
      return hasattr(candidat, '__len__') and len(candidat) <= self._lonmax


# -----------------------------------------------------------------------------
class VectNum(Vecteur):
   """Spécialisation pour les vecteurs numériques.
   La valeur est accessible par l'attribut `value` (propriété).
   """
   def access_value(self):
      """Construit l'accès aux valeurs de l'objet.
      """
      self.lock(self.mode)
      #self.__value = pyaster.VectJev_AsArray(self.nomj)
      self.__value = aster.VectJev_AsArray(self._numtyp, self._addr, self._lonmax)
   
   def is_valid_value(self, candidat):
      """Dit si `candidat` est une valeur valide pour le vecteur.
      """
      return super(VectNum, self).is_valid_value(candidat) and \
             AsVectType(candidat) == self._type

   # --- propriété value ---
   def get_value(self):
      """Retourne la valeur
      """
      return self.__value

   def set_value(self, new):
      """Affectation de `new`.
      """
      assert self.mode == 'E', "Interdiction d'accéder en écriture"
      assert self.is_valid_value(new), 'Cette valeur ne convient pas '\
                                       'pour un vecteur numérique : %r' % new
      self.libe()  # JEDETR, non ?
      pyaster.VectJev_FromArrayAndType(self.nomj, new, self._type)
      self.access_value()

   value = property(get_value, set_value, None, "Valeur du vecteur")

# -----------------------------------------------------------------------------
class Collection(object):
   """Collection vue que N objets jeveux construits en fonction du besoin.
   """

# -----------------------------------------------------------------------------
if __name__ == '__main__':
   #DIME = AsVI(lonmax=6, )
   #obj = DIME.get()  retourne un objet Vecteur, type entier...
   obj = VectNum('MA       .DIME', 'I', 10, 10, mode='E')
   print obj.value
   obj.value = [9, 7, 4]
   print obj.value
   

