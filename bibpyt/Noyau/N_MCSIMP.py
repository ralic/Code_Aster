#@ MODIF N_MCSIMP Noyau  DATE 03/09/2002   AUTEUR GNICOLAS G.NICOLAS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
#                                                                       
#                                                                       
# ======================================================================
""" 
    Ce module contient la classe MCSIMP qui sert à controler la valeur
    d'un mot-clé simple par rapport à sa définition portée par un objet
    de type ENTITE
"""

import types
from copy import copy

from Noyau.N_ASSD import ASSD,assd
import N_OBJECT

class MCSIMP(N_OBJECT.OBJECT):
   """
   """
   nature = 'MCSIMP'
   def __init__(self,val,definition,nom,parent):
      """
         Attributs :

          - val : valeur du mot clé simple

          - definition

          - nom

          - parent

        Autres attributs :

          - valeur : valeur du mot-clé simple en tenant compte de la valeur par défaut

      """
      self.definition=definition
      self.nom=nom
      self.val = val
      self.parent = parent
      self.valeur = self.GETVAL(self.val)
      if parent :
         self.jdc = self.parent.jdc
         self.niveau = self.parent.niveau
         self.etape = self.parent.etape
      else:
         # Le mot cle simple a été créé sans parent
         self.jdc = None
         self.niveau = None
         self.etape = None
         
   def GETVAL(self,val):
      """ 
          Retourne la valeur effective du mot-clé en fonction
          de la valeur donnée. Defaut si val == None
      """
      if (val is None and hasattr(self.definition,'defaut')) :
        return self.definition.defaut
      else:
        return val

   def get_valeur(self):
      """
          Retourne la "valeur" d'un mot-clé simple.
          Cette valeur est utilisée lors de la création d'un contexte 
          d'évaluation d'expressions à l'aide d'un interpréteur Python
      """
      return self.valeur

   def get_val(self):
      """
          Une autre méthode qui retourne une "autre" valeur du mot clé simple.
          Elle est utilisée par la méthode get_mocle
      """
      return self.valeur

   def accept(self,visitor):
      """
         Cette methode permet de parcourir l'arborescence des objets
         en utilisant le pattern VISITEUR
      """
      visitor.visitMCSIMP(self)

   def copy(self):
    """ Retourne une copie de self """
    objet = self.makeobjet()
    # il faut copier les listes et les tuples mais pas les autres valeurs
    # possibles (réel,SD,...)
    if type(self.valeur) in (types.ListType,types.TupleType):
       objet.valeur = copy(self.valeur)
    else:
       objet.valeur = self.valeur
    objet.val = objet.valeur
    return objet

   def makeobjet(self):
    return self.definition(val = None, nom = self.nom,parent = self.parent)

   def reparent(self,parent):
     """
         Cette methode sert a reinitialiser la parente de l'objet
     """
     self.parent=parent
     self.jdc=parent.jdc
     self.etape=parent.etape

   def get_sd_utilisees(self):
    """ 
        Retourne une liste qui contient la SD utilisée par self si c'est le cas
        ou alors une liste vide
    """
    l=[]
    if type(self.valeur) == types.InstanceType:
      #XXX Est ce différent de isinstance(self.valeur,ASSD) ??
      if issubclass(self.valeur.__class__,ASSD) : l.append(self.valeur)
    elif type(self.valeur) in (types.TupleType,types.ListType):
      for val in self.valeur :
         if type(val) == types.InstanceType:
            if issubclass(val.__class__,ASSD) : l.append(val)
    return l
