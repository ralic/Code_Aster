# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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


""" Ce module contient la classe de definition SIMP
    qui permet de spécifier les caractéristiques des mots clés simples
"""

import types

import N_ENTITE
import N_MCSIMP
from strfunc import ufmt

class SIMP(N_ENTITE.ENTITE):
   """
    Classe pour definir un mot cle simple

    Cette classe a deux attributs de classe

    - class_instance qui indique la classe qui devra etre utilisée
            pour créer l'objet qui servira à controler la conformité d'un
            mot-clé simple avec sa définition

    - label qui indique la nature de l'objet de définition (ici, SIMP)

   """
   class_instance = N_MCSIMP.MCSIMP
   label = 'SIMP'

   def __init__(self,typ,fr="",statut='f',into=None,defaut=None,
                     min=1,max=1,homo=1,position='local',
                     val_min='**',val_max='**',docu="",validators=None,
                     sug=None):
      """
          Un mot-clé simple est caractérisé par les attributs suivants :
          - type : cet attribut est obligatoire et indique le type de valeur attendue
          - fr : chaîne documentaire en français
          - statut : obligatoire ou facultatif ou caché
          - into : valeurs autorisées
          - defaut : valeur par défaut
          - min : nombre minimal de valeurs
          - max : nombre maximal de valeurs
          - homo : ?
          - position : si global, le mot-clé peut-être lu n'importe où dans la commande
          - val_min : valeur minimale autorisée
          - val_max : valeur maximale autorisée
          - docu : ?
          - sug : ?
      """
      N_ENTITE.ENTITE.__init__(self,validators)
      # Initialisation des attributs
      if type(typ) == types.TupleType :
          self.type=typ
      else :
          self.type=(typ,)
      self.fr=fr
      self.statut=statut
      self.into=into
      self.defaut=defaut
      self.min=min
      self.max=max
      self.homo=homo
      self.position = position
      self.val_min=val_min
      self.val_max=val_max
      self.docu = docu
      self.sug = sug

   def verif_cata(self):
      """
          Cette methode sert à valider les attributs de l'objet de définition
          de la classe SIMP
      """
      self.check_min_max()
      self.check_fr()
      self.check_statut()
      self.check_homo()
      self.check_into()
      self.check_position()
      self.check_validators()

   def __call__(self,val,nom,parent=None):
      """
          Construit un objet mot cle simple (MCSIMP) a partir de sa definition (self)
          de sa valeur (val), de son nom (nom) et de son parent dans l arboresence (parent)
      """
      return self.class_instance(nom=nom,definition=self,val=val,parent=parent)
